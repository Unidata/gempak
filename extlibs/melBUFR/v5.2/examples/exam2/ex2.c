/*
 * EXAMPLE 2: Create a BUFR message using simple PUT_Array.  Encodes
 * latitude, longitude, hour, and temperature using an array.
 */

#include <mel_bufr.h>           /* BUFR library include file */

int main()
{
    BUFR_Info_t bufr_info;  /* Largely Section 1 information. */
    float values[4];
    FXY_t fxy[4];
    int   num_vals, num_fxys;

    /* Initialize the BUFR information structure. */
    
    printf("\n *************************** \n");
    printf(" *       EXAMPLE 2         *\n");
    printf(" * Basic message structure *\n"); 
    printf(" *   FXY_Pack              *\n");
    printf(" *   BUFR_Put_Array        *\n");    
    printf(" *************************** \n");


    if( BUFR_Info_Init( &bufr_info ) )
    {
        BUFR_perror( "main" );
        return 1;
    }

    bufr_info.BUFR_MasterTable            =  0; /* Use WMO standard */
    bufr_info.OriginatingCenter           = 58; /* FNOC Monterey, CA */
    bufr_info.UpdateSequenceNumber        =  0; /* 0 for original message */
    bufr_info.DataCategory                =  0; /* Surface land data */
    bufr_info.DataSubCategory             =  0; /* BUFR message sub-type */
    bufr_info.VersionNumberOfMasterTables =  11;
    bufr_info.VersionNumberOfLocalTables  =  0;
    bufr_info.Century                     = 19;
    bufr_info.MinorLocalVersion           = 0;
    bufr_info.Year                        = 95;
    bufr_info.Month                       = 12;
    bufr_info.Day                         = 31;
    bufr_info.Hour                        = 23;
    bufr_info.Minute                      = 59;
    bufr_info.ObservedData                =  1;


    /* Initialize BUFR message.  Read standard tables. */

    Set_Flag(NoAuto_FTP);
    Set_Flag(Allow_Dup_Tab_Entry);
    Set_Flag(No_Warn_Dup_Tab_Entry);
    
    if( BUFR_Init(  &bufr_info, "ex2.enc", ENCODING  ) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }

    num_vals = 4;
    num_fxys = 4;

 /* set variables in data array */
    values[0] = 35.5;
    values[1] = 120.3;
    values[2] = 13.0;
    values[3] = 295.2;

    /* Define fxy array  */

    fxy[0] = FXY_Pack(0,5,2);
    fxy[1] = FXY_Pack(0,6,2);
    fxy[2] = FXY_Pack(0,4,4);
    fxy[3] = FXY_Pack(0,12,4);

    /* Enter array of data into bufr message */
    if ( BUFR_Put_Array( values, num_vals, DT_FLOAT, fxy, num_fxys) ){
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
