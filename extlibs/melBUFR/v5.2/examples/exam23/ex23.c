/*
 * EXAMPLE 23: Replication within a data set using mixed floating
 * point data and character strings.  Table C descriptors used to modify
 * some descriptors.
 *
 * Encodes a data set using replication to repeat a data sequence
 * within the data set.  The data set is composed of a latatude, longitude,
 * date, time, a repeated sequence of height, temperature, wind speed,
 * wind direction, and station name.
 */

#include <mel_bufr.h>           /* BUFR library include file */

#if PROTOTYPE_NEEDED

int main(void)

#else
 
int main(  )
 
#endif

{
    BUFR_Info_t bufr_info;  /* Largely Section 1 information. */
    Data_MixVal_t *bufr_rec;
    int   *fxy_i;         /*  array of decimal FXYs  */
    FXY_t *fxy;           /*  array of packed FXYs   */
    int   num_vals;         /*  number of values in array */
    int   i;                /*  loop counter */
    char  fxy_file[40];
    char  encode_file[30];
    int   *ipr;
    FXY_t *xpr;
    FILE  *fd;
    int   NUM_FXYS;         /* number of FXYs          */
    
    printf("\n *************************** \n");
    printf(" *       EXAMPLE 23          *\n");
    printf(" *      Replication          *\n");
    printf(" *     Within a Dataset      *\n");  
    printf(" *  With Character Strings   * \n");
    printf(" *  With Table B descriptors * \n");
    printf(" *                           * \n");
    printf(" *   FXY_Pack_Dec            *\n"); 
    printf(" *   BUFR_Put_MixArray       *\n");   
    printf(" *   BUFR_DefineDataset      *\n");
    printf(" ***************************** \n");
 
    /****  define file names  *****/

    strcpy(fxy_file,"exam23.fxy");
    strcpy(encode_file,"ex23.enc");
    /********************************************************/
    /*             read FXYs                                */
    /********************************************************/

    /* open FXY file */
    fd = fopen(fxy_file,"r");

    /* how many FXYs ?? */
    fscanf(fd,"%d",&NUM_FXYS);
    printf(" NUM_FXYS = %d\n",NUM_FXYS);

    /* allocate memory of FXY input */
    fxy_i = (int *)malloc ( (uint_t)NUM_FXYS * sizeof(int));
    
    /* allocate memory for packed FXYs */
    fxy = (FXY_t *)malloc( (uint_t)NUM_FXYS * sizeof(FXY_t));
    
    /* read FXYs */
    printf(" echo of FXYs read\n");
    ipr = fxy_i;
    for (i=0; i<NUM_FXYS; i++){
      fscanf(fd,"%d",ipr);
      printf(" %06d\n",*ipr++);
      fflush(stdout);
    }
    /* close FXY file */
    fclose(fd);
    

    /************************************************************/
    
    /* Initialize BUFR message.  Read standard tables. */

    /* Initialize the BUFR information structure. */

    if( BUFR_Info_Init( &bufr_info ) )
    {
        BUFR_perror( "main" );
        return 1;
    }

    bufr_info.BUFR_MasterTable            =  0; /* Use WMO standard */
    bufr_info.BUFR_Edition                =  3; /* Use WMO standard */
    bufr_info.OriginatingCenter           = 128; /* NRL Monterey, CA */
    bufr_info.UpdateSequenceNumber        =  0; /* 0 for original message */
    bufr_info.DataCategory                =  0; /* Surface land data */
    bufr_info.DataSubCategory             =  0; /* BUFR message sub-type */
    bufr_info.VersionNumberOfMasterTables = 5;
    bufr_info.VersionNumberOfLocalTables  =  0;
    bufr_info.MinorLocalVersion          = 0;
    bufr_info.Year                        = 95;
    bufr_info.Month                       = 12;
    bufr_info.Day                         = 31;
    bufr_info.Hour                        = 23;
    bufr_info.Minute                      = 59;
    bufr_info.ObservedData                =  1;

    Set_Flag(NoAuto_FTP);
    Set_Flag(Allow_Dup_Tab_Entry);
    Set_Flag(No_Warn_Dup_Tab_Entry);
    
    if( BUFR_Init(  &bufr_info, encode_file, ENCODING ) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }

    /***************  pack FXYs  **************/
    /*  pack replication FXY              */
    /*  using delayed replication, therefore the replication count */
    /*  is in the data array.  The FXY for delayed replication of  */
    /*  5 FXY is 1-05-000.  The 000 indicates delayed replication. */ 
    /*  The delayed replication FXY must be followed by a count    */
    /*  descriptor (0-31-001 or 0-31-002).  This example uses      */
    /*  0-31-001 (see exam5.fxy) (single byte count)               */    
   
    /* pack FXYs for data */
    ipr = fxy_i;
    xpr = fxy;
    for ( i=0; i<NUM_FXYS; i++)
       *xpr++ = FXY_Pack_Dec( *ipr++);

/*
**  Loop on the three data sets.
**  Malloc for the number of data points needed and call fill_array2
**  to file the sturcture properly.
*/
        Set_Compress();

    num_vals = 22;

    for(i = 0; i < 4; i++){
      bufr_rec = (Data_MixVal_t *) malloc(sizeof(Data_MixVal_t) * num_vals);
      fill_array2(bufr_rec, i);
 
      /* echo print data */
      printf(" data set %d\n",  i );
      B_Mix_Print(bufr_rec, num_vals);
    
    /* Enter array of data into bufr message */
    /*  Call BUFR_Put_MixArray with the data structure, the number of data
    *  Elements in the structure, the packed FXYs, and the number of fxys */

      if ( BUFR_Put_MixArray( bufr_rec, num_vals, fxy, NUM_FXYS) ){
        BUFR_perror(" Error on call to BUFR_Put_Array in main");
        return 1;
      }
   /*************************************************/
   /*             define template                   */

      BUFR_Define_Dataset( (FXY_t*) NULL, 0 );

    } /* end loop */


    /* create message  */
    if( BUFR_Encode( &bufr_info) )
    {
        BUFR_perror( "main" );
        BUFR_Destroy(1);
        return 1;
    }

    BUFR_Destroy(1);
    free (fxy);
    free (fxy_i);
    return 0;
}
