/*  EXAMPLE 16 - This example encodes real surface observations using the
 * template approach.  Two sub examples are provided: 1) using several
 * Tables B and Table D descriptors, and 2) using a single Table D descriptor
 * that is equivlent to the sequence of descriptors used in the first 
 * sub example.
 *
 * The example uses a shortened input data file.  If timeing tests are desired,
 * a larger input data set is available,
*/

#include <stdio.h>
#include <time.h>
#include <mel_bufr.h>
#include "datsav2_io.h"


#if PROTOTYPE_NEEDED

void error_print_usage(void);

int main(int argc, char **argv)

#else
 
void error_print_usage();

int main( argc, argv )
int   argc;
char* argv[];
 
#endif
{

  extern int BUFR_Debug_Flag, BUFR_Trace_Flag;

  BUFR_Info_t bufr_info;

  DS2_Surface_t *bufr_rec;

  Initialization_Data_t *request;

  FXY_t *rec_fxy,  *fxy_pr; 
  int   *fxy_i; 
  int   *ipr; 
  int    i;
  int   date_flag;
  int   year, centry;
  
  FILE *fp_ds2;
  FILE *pr_fxy_file;
  char  fxy_file[60];
  char  ini_file[60];

  int   num_fxys;

  int rec_count = 0;
  date_flag = 0;

#ifdef DEBUG3
    BUFR_Debug_Flag = 3;
    BUFR_Trace_Flag = 3;
#endif


/*
--  Did we get the correct number of command line parameters?
*/

  if (argc != 2) error_print_usage();
  
  printf("\n *********************************** \n");
  printf(" *           EXAMPLE 16            *\n");
  printf(" *           TEMPLATES             *\n"); 
  printf(" *         (surface data)          *\n");
  printf(" *         --------------          *\n");
  switch ( *argv[1] ) {
    case 'a':
       printf(" *              case a             *\n");
       printf(" *       multiple descriptors      *\n");
       printf(" *         small data file         *\n");
       printf(" *       encodes 6 stations        *\n");
       break;
    case 'b':
       printf(" *              case b             *\n");
       printf(" *    single Table D descriptor    *\n");
       printf(" *         small data file         *\n");
       printf(" *       encodes 6 stations        *\n");
       break;
    case 'c':
       printf(" *              case c             *\n");
       printf(" *    single Table D descriptor    *\n");
       printf(" *          large data file        *\n");
       break;
    default:
       printf("\n <<<<<<<<<< ERROR >>>>>>>>>\n");
       printf("   To execute example12 enter \n");
       printf("  >ex16 {a,b,c}\n\n  where \n\n");
       printf("  a = multiple descriptors, small data file\n");
       printf("  b = single Table D descriptor, small data file\n");
       printf("  c = single Table D descriptor, large data file\n");
       printf("\n <<<<<<<<<< ERROR >>>>>>>>>\n");
       exit(1);
  }
  printf(" *                                 *\n"); 
  printf(" *   BUFR_Define_Dataset           *\n"); 
  printf(" *   FXY_Pack_Dec                  *\n"); 
  printf(" *   BUFR_Put_Array                *\n");   
  printf(" ***********************************\n\n"); 

/*
--  build fxy input file name
 */
   sprintf(fxy_file,"ex16%s.fxy",argv[1]);
   pr_fxy_file = fopen(fxy_file, "r");

   /* read FXYs from fxy input file */

   /* read number of FXYs */
   fscanf(pr_fxy_file, "%d", &num_fxys);
   
   /* allocate memory for FXYs */
   fxy_i = (int *) malloc(sizeof(int) * (uint_t)num_fxys);  /* decimal FXYs as read */
   ipr = fxy_i;
   rec_fxy = (FXY_t *) malloc(sizeof(FXY_t ) * (uint_t)num_fxys);  /* packed FXYs */
   fxy_pr = rec_fxy;
   
   /*  pack FXYS */
   for ( i=0; i< num_fxys; i++){
     fscanf(pr_fxy_file, "%d", ipr);
     *fxy_pr = FXY_Pack_Dec(*ipr);
     printf(" i=%d  fxy=%d  packed = %X\n", i, *ipr++, *fxy_pr++);
   }

/*
--  Retrieve data from extract template.
*/
  /* build ini file name */
  sprintf(ini_file, "ex16%s.ini",argv[1]);
  request = get_ini_data(ini_file);
  dump_ini_data(request);

/*
--  If the requested file exists, open it, else error off.
*/
  if ( !(fp_ds2 = fopen(request->Input_file,"rt")) )
    error_file_not_found("extract",request->Input_file);

/*
--  Initialize the BUFR message structure.
*/

    /* Initialize the BUFR information structure. */

    if( BUFR_Info_Init( &bufr_info ) )
    {
        BUFR_perror( "main" );
        return 1;
    }

  bufr_info.BUFR_MasterTable  = 0;    /* Use WMO Standard */
  bufr_info.BUFR_Edition  = 3;    /* Use WMO Standard */
  bufr_info.OriginatingCenter = 128;   /*NRL Monterey */
  bufr_info.UpdateSequenceNumber = 0; /* 0 == Original Message */
  bufr_info.DataCategory = 2;         /* From Table A */
  bufr_info.DataSubCategory = 0;      /* BUFR message sub-type */
  bufr_info.VersionNumberOfMasterTables = 5;
  bufr_info.VersionNumberOfLocalTables  = 0; /* No local tables */
  bufr_info.MinorLocalVersion          = 0;
  bufr_info.ObservedData                = 1; /* Section 3 flag */

    Set_Flag(NoAuto_FTP);
    Set_Flag(Allow_Dup_Tab_Entry);
    Set_Flag(No_Warn_Dup_Tab_Entry);
    
    if ( BUFR_Init(&bufr_info,  request->BUFR_output_file, ENCODING) ) {

    BUFR_perror("extract");
    return 1;

  } /* end if(BUFR_init) */

/*
  BUFR_Debug(8);
  BUFR_Trace(8);
*/
  while (!feof(fp_ds2)) {
/*
**  Retrieve the next DATSAV2 record from the input stream
**  and decode into usable data.
*/
    bufr_rec = DS2_get_surface(fp_ds2);
/*
**  Does this station meet criteria?  If so, write it.
*/
    if ( bufr_rec != NULL && is_selected(bufr_rec,request) )
    {
	
/*	DS2_dump_surface(bufr_rec);   */
	
#ifdef SCREEN
	fprintf(stderr,"Record %d\r",++rec_count);
#else
        ++rec_count;
#endif
        /* use fist date as typical data for data set */

        year = bufr_rec->Year;
        if ( date_flag == 0 ) {
           printf(" setting date \n");
           centry = year/100;
           year = year - 100 * centry;
           if ( year == 0 ) year = 100;
           centry++;
           printf(" centry =%d   year = %d\n", centry, year);
           bufr_info.Year = year;
           bufr_info.Month = bufr_rec->Month;
           bufr_info.Day = bufr_rec->Day;

           bufr_info.Hour = bufr_rec->Hour;
           bufr_info.Minute = bufr_rec->Minute;
           date_flag = 1;
	}

	if ( BUFR_Put_Array(bufr_rec,DS2_RECORD_LENGTH,
			    DT_FLOAT, rec_fxy,num_fxys) ){

	  (void) fclose( fp_ds2 );
	  BUFR_perror("extract");
	  (void) BUFR_Destroy(1);
	  return 1;

	} /* end if(BUFR_Put_Array) */

        /*************************************************/
        /*             define template           */

        BUFR_Define_Dataset( (FXY_t*) NULL, 0 );  

    } /* end if(bufr_rec) */

  } /* end while(!feof) */
  
  fprintf(stderr,"\n");

  fclose(fp_ds2);

  if (BUFR_Encode(&bufr_info))
    {

      BUFR_perror("extract");
      BUFR_Destroy(1);
      return 1;

    } /* end if(BUFR_Encode) */

  BUFR_Destroy(1);

  fprintf(stderr,"Wrote %d records to %s\n",
	  rec_count,request->BUFR_output_file);

  return 0;

} /* end main() */


#if PROTOTYPE_NEEDED

void error_print_usage(void)

#else
 
void error_print_usage()
 
#endif
{

  printf("\nusage:  extract <template filename>\n\n");
  exit(1);

} /* end error_print_usage() */







