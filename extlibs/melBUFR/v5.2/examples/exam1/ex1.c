/*
 * EXAMPLE 1: Create a BUFR message using simple put values.  Encodes
 * latitude, longitude, hour, and temperature.
 * 
 */

#include <mel_bufr.h>           /* BUFR library include file */

#if PROTOTYPE_NEEDED

int main( void )

#else

int main(  )

#endif

{
    BUFR_Info_t bufr_info;  /* Largely Section 1 information. */
    float lat,lon,hour,temp;
    FXY_t fxy;

    char trash_str[30];

    strcpy(trash_str,"This is a test string");
    /* set variables */
    lat = 35.5;
    lon = 120.3;
    hour = 13.0;
    temp = 295.2;
    
    printf("\n *************************** \n");
    printf(" *       EXAMPLE 1         *\n");
    printf(" * Basic message structure *\n");    
    printf(" *************************** \n");

    /**************************************************************/
    /* Initialize BUFR information structure with default values. */
    /**************************************************************/

    if( BUFR_Info_Init( &bufr_info ) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }

    bufr_info.BUFR_MasterTable            =  0; /* Use WMO standard */
    bufr_info.BUFR_Edition            =  3; /* Use WMO standard */
    bufr_info.OriginatingCenter           = 58; /* FNOC Monterey, CA */
    bufr_info.UpdateSequenceNumber        =  0; /* 0 for original message */
    bufr_info.DataCategory                =  0; /* Surface land data */
    bufr_info.DataSubCategory             =  0; /* BUFR message sub-type */
    bufr_info.VersionNumberOfMasterTables = 11;
    bufr_info.MinorLocalVersion           = 0;
    bufr_info.VersionNumberOfLocalTables  =  0;
    bufr_info.Century                      = 19;
    bufr_info.Year                        = 95;
    bufr_info.Month                       = 12;
    bufr_info.Day                         = 31;
    bufr_info.Hour                        = 23;
    bufr_info.Minute                      = 59;
    bufr_info.ObservedData                =  1;

    /**************************************************************/
    /*      Initialize BUFR message.  Read standard tables.       */
    /**************************************************************/

    Set_Flag(NoAuto_FTP);
    Set_Flag(Allow_Dup_Tab_Entry);
    Set_Flag(No_Warn_Dup_Tab_Entry);
    
    if( BUFR_Init( &bufr_info, "ex1.enc", ENCODE) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }

    /**************************************************************/
    /*         Enter data into message                            */
    /**************************************************************/

    /* Put latitude (course accuracy) FXY = 0-05-002 */
    /* pack FXY value */
    fxy = FXY_Pack(0,5,2);
    if( BUFR_Put_Value(fxy, (double)lat ) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }
    /* Put longitude (course accuracy) FXY = 0-06-002 */
    /* pack FXY value */
    fxy = FXY_Pack(0,6,2);
    if( BUFR_Put_Value( fxy, (double)lon ) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }
    /* Put hour FXY = 0-04-004 */
    /* pack FXY value */
    fxy = FXY_Pack(0,4,4);
    if( BUFR_Put_Value( fxy, (double)hour ) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }
    /* Put dry bulb temperature at 2m  FXY = 0-12-004 */
    /* pack FXY value */
    fxy = FXY_Pack(0,12,4);
    if( BUFR_Put_Value(fxy, (double)temp ) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }

    /**************************************************************/
    /*                     create message                         */
    /**************************************************************/
    if( BUFR_Encode( &bufr_info) )
    {
        BUFR_perror( "main" );
        BUFR_Destroy(1);
        return 1;
    }

    /**************************************************************/
    /*                         Free memory                        */
    /**************************************************************/
    BUFR_Destroy(1);

    return 0;
}
