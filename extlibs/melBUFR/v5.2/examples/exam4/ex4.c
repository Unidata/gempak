/*
 * EXAMPLE 4: Replication:
 * Create a BUFR message using simple PUT_Array.  Encodes
 * four sets of latitude, longitude, hour, and temperature using an array
 * and the replication FXY.
 */

#include <mel_bufr.h>           /* BUFR library include file */

#if PROTOTYPE_NEEDED

int main( void )

#else

int main(  )

#endif
{
    BUFR_Info_t bufr_info;  /* Largely Section 1 information. */
    float *values;        /* array of data values  */
    int   fxy_i[4];         /*  array of decimal FXYs  */
    FXY_t fxy[5];           /*  array of packed FXYs   */
    int   num_vals;         /*  number of values in array */
    int   i;                /*  loop counter */
    int   num_sets;         /* number of data sets */
    float *fpr;
    FILE  *fd; 
    static int NUM_FXYS = 5;    
    
    printf("\n *************************** \n");
    printf(" *       EXAMPLE 4         *\n");
    printf(" *      Replication        *\n"); 
    printf(" *   FXY_Pack_Dec          *\n");
    printf(" *   BUFR_Put_Array        *\n");   
    printf(" *************************** \n");

    /********************************************************/
    /*             read FXys and data                       */
    /********************************************************/

    /* open FXY file */
    fd = fopen("exam4.fxy","r");
    
    /* read FXYs */
    printf(" echo of FXYs read\n");
    for (i=0; i<NUM_FXYS - 1; i++){
      fscanf(fd,"%d",&fxy_i[i]);
      printf(" %06d\n",fxy_i[i]);
    }
    /* close FXY file */
    fclose(fd);
    
    /* open data file */
    fd = fopen("exam4.dat","r");
    
    /* read number of data sets */
    fscanf(fd,"%d",&num_sets);
    
    /* allocate memory for data sets */
    /* each data set is composed of 4 entries  */
    num_vals = num_sets * 4;
    values = (float *)malloc( sizeof(float) * (uint_t)num_vals);
    fpr = values;
    
    /* read data sets */
    printf(" echo of data read\n");
    for ( i=0; i< num_vals; i++){
       fscanf(fd,"%f",fpr);
       printf(" i=%d  value=%f\n",i,*fpr++);
    }
    
    fclose(fd);
 
    /************************************************************/
    /* encode message */
    
    /* Initialize the BUFR information structure. */

    if( BUFR_Info_Init( &bufr_info ) )
    {
        BUFR_perror( "main" );
        return 1;
    }

    bufr_info.BUFR_MasterTable            =  0; /* Use WMO standard */
    bufr_info.BUFR_Edition            =  3; /* Use WMO standard */
    bufr_info.OriginatingCenter           = 58; /* FNOC Monterey, CA */
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

    /* Initialize BUFR message.  Read standard tables. */

    Set_Flag(NoAuto_FTP);
    Set_Flag(No_Warn_Dup_Tab_Entry);
    
    if( BUFR_Init(  &bufr_info, "ex4.enc", ENCODING  ) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }

    /***************  pack FXYs  **************/
    /*  pack replication FXY              */
    /* the number of FXYs replicated is 4 */
    /* replication count is num_sets      */
    /* not using delayed replication      */
      
    fxy[0] = FXY_Pack(1,4,num_sets);
    
    /* pack FXYs for data */
    for ( i=1; i<NUM_FXYS; i++)
       fxy[i] = FXY_Pack_Dec( fxy_i[i-1] );

    /* Enter array of data into bufr message */
    if ( BUFR_Put_Array( values, num_vals, DT_FLOAT, fxy, NUM_FXYS) ){
      BUFR_perror(" Error on call to BUFR_Put_Array in main");
      return 1;
    }

    /* create message  */
    if( BUFR_Encode(&bufr_info ) )
    {
        BUFR_perror( "main" );
        BUFR_Destroy(1);
        return 1;
    }

    BUFR_Destroy(1);

    return 0;
}
