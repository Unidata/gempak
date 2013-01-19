/*
 * EXAMPLE 11: Change data width for a FXY using BUFR_Change_DataWidth. 
 * The data width in the BUFR tables for 0-22-39 is given as 12 bits.  
 * This data width is to small and needs be set to 14 bits inorder to hold
 * negative numbers as small as -5 meters.
 */

#include <mel_bufr.h>           /* BUFR library include file */

#if PROTOTYPE_NEEDED

int main( void )

#else

int main( )

#endif
{
    BUFR_Info_t bufr_info;  /* Largely Section 1 information. */

#define NUM_DATA  4
#define NUM_FXYS  3

    /* data elements or hour( 0-04-004) and residual tidal elevation (0-22-039) */
    static float data[NUM_DATA] = { 12., -2.3, 22., 1.5 };

    float lat, lon;

    printf("\n ***************************\n");
    printf(" *        EXAMPLE 11       *\n");
    printf(" *     Change datawidth    *\n"); 
    printf(" *           using         *\n"); 
    printf(" *  BUFR_Change_DataWidth  *\n");   
    printf(" *                         * \n"); 
    printf(" *   FXY_Pack_Dec          *\n"); 
    printf(" *   BUFR_Put_Value        *\n");   
    printf(" ***************************\n"); 

    /* Initialize BUFR message.  Read standard tables. */

    /* Initialize the BUFR information structure. */

    if( BUFR_Info_Init( &bufr_info ) )
    {
        BUFR_perror( "main" );
        return 1;
    }

    bufr_info.BUFR_MasterTable            =  0; /* Use WMO standard */
    bufr_info.BUFR_Edition            =  3; /* Use WMO standard */
    bufr_info.OriginatingCenter           =128; /* NRL Monterey, CA */
    bufr_info.UpdateSequenceNumber        =  0; /* 0 for original message */
    bufr_info.DataCategory                =  2; /* Surface land data */
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
    
    if( BUFR_Init( &bufr_info,"ex11.enc", ENCODING  ) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }

    /*
     * Use delayed replication, along with descriptor 0-12-001 to add an
     * entire array of temperature values.
     */

      lat = 45.;
      lon = 75.;

    BUFR_Put_Value(FXY_Pack (0, 5, 2), (double)lat); 
    BUFR_Put_Value(FXY_Pack (0, 6, 2), (double)lon);

    /* put 1 hour */
    BUFR_Put_Value(FXY_Pack (0, 4, 4), (double)data[0]);
   
    /* change data width */
    BUFR_Change_DataWidth( 2);

    /* put residual tidal elevation (0-22-039) */
    BUFR_Put_Value( FXY_Pack (0, 22,39), (double)data[1]);

    /* cancel data width change */
    BUFR_Change_DataWidth( 0);

    /* repeat for second pair */
    BUFR_Put_Value( FXY_Pack (0, 4, 4), (double)data[2]);
    BUFR_Change_DataWidth( 2);
    BUFR_Put_Value( FXY_Pack (0, 22,39), (double)data[3]);
    BUFR_Change_DataWidth( 0);

    if( BUFR_Encode(&bufr_info ) )
    {
        BUFR_perror( "main" );
        BUFR_Destroy(1);
        return 1;
    }

    BUFR_Destroy(1);

    return 0;
}
