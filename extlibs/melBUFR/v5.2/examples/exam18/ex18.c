/*   The same as example17 but each station is encoded as a data subset using
* a template.  The program is the same except that a call to 
* BUFR_Define_Dataset is made at the end of the first pass through the loop 
* that enters the data into the message.  This is the correct way to encode
* multiple dataset into a single message.
*/
#include <mel_bufr.h>
#include "fill_array2.h"

#if PROTOTYPE_NEEDED

int main(void)

#else

int main( )

#endif
{

  extern int BUFR_Debug_Flag, BUFR_Trace_Flag;

  BUFR_Info_t bufr_info;

  Data_MixVal_t *bufr_rec;

  FXY_t rec_fxy[4];

  int i;

#ifdef DEBUG3
    BUFR_Debug_Flag = 3;
    BUFR_Trace_Flag = 3;
#endif


/*
--  Initialize the BUFR message structure.
*/
  printf("\n ************************************* \n");
  printf(" *            EXAMPLE 18             *\n");
  printf(" *        BUFR_Put_MixArray          *\n"); 
  printf(" *              and                  *\n"); 
  printf(" *      Templates (on the fly)       *\n"); 
  printf(" ************************************* \n");

    if( BUFR_Info_Init( &bufr_info ) )
    {
        BUFR_perror( "main" );
        return 1;
    }

  bufr_info.BUFR_MasterTable  = 0;    /* Use WMO Standard */
  bufr_info.OriginatingCenter = 71;   /* Not assigned -- AFCCC */
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
    
  if ( BUFR_Init(&bufr_info,  "ex18.enc", ENCODING) ) {

    BUFR_perror("extract");
    return 1;

  } /* end if(BUFR_init) */

/*
--   We are encoding a BUFR Surface Report (of sorts).
*/

  rec_fxy[0] = FXY_Pack(3,1,11);
  rec_fxy[1] = FXY_Pack(3,1,12);
  rec_fxy[2] = FXY_Pack(3,1,24);
  rec_fxy[3] = FXY_Pack(0,1,11);

/*
**  Loop for the three data subsets.
**  Malloc for the number of data points needed and call fill_array
**  to file the structure properly.
*/
  for(i = 0; i < 3; i++){
    bufr_rec = (Data_MixVal_t *) malloc(sizeof(Data_MixVal_t) * 9);
    fill_array2(bufr_rec, i);

/*  Call BUFR_Put_MixArray with the data structure, the number of data
    Elements in the structure, the packed FXYs, and the number of fxys */

	if ( BUFR_Put_MixArray(bufr_rec,DS2_RECORD_LENGTH,
			    rec_fxy,4) ){

	  BUFR_perror("BUFR_Put_MixArray");
	  (void) BUFR_Destroy(1);
	  return 1;

	} /* end if(BUFR_Put_MixArray) */

        /*************************************************/
        /*             define template           */

        BUFR_Define_Dataset( (FXY_t*) NULL, 0 );  

  } /* end loop */
  
  fprintf(stderr,"\n");

  bufr_info.Year = 97;
  bufr_info.Month = 8;
  bufr_info.Day = 6;

  bufr_info.Hour = 9;
  bufr_info.Minute = 40;

/*  Encode and write to file the bufr data */

  if (BUFR_Encode(&bufr_info))
    {

      BUFR_perror("extract");
      BUFR_Destroy(1);
      return 1;

    } /* end if(BUFR_Encode) */

  BUFR_Destroy(1);
  return 0;

} /* end main() */
