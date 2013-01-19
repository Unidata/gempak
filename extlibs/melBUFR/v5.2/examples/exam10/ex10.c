/*
 * EXAMPLE 10: Adding A generic text string using BUFR_Put_String.  This 
 * function uses the Table B descriptor 2-05-YYY to add a generic text
 * string to a BUFR message.  A maximum of 255 characters can be added per
 * occurance of the descriptor.  The function checks the length of the text 
 * string and determines the number of 2-05-yyy descriptors required to enter
 * the text string.
 * 
 * Please note that BUFR_Put_Array can not be used to enter this descriptor.
 * BUFR_Put_Array will be modified in the furture to allow this.
 */

#include <mel_bufr.h>           /* BUFR library include file */

#if PROTOTYPE_NEEDED

int main( void )

#else

int main( )

#endif
{
    BUFR_Info_t bufr_info;  /* Largely Section 1 information. */
    float values[4];        /* array of data values  */
    int   fxy_i[4];         /*  array of decimal FXYs  */
    FXY_t fxy[4];           /*  array of packed FXYs   */
    int   num_vals;         /*  number of values in array */
    int   num_fxys;         /* number of FXYs          */
    int   i;                /*  loop counter */
    char  tstr[100];

    printf("\n *************************** \n");
    printf(" *       EXAMPLE 10        *\n");
    printf(" *   Generic text string   *\n"); 
    printf(" *  using BUFR_Put_String  *\n");   
    printf(" *                         *\n"); 
    printf(" *   FXY_Pack_Dec          *\n"); 
    printf(" *   BUFR_Put_Array        *\n");   
    printf(" ***************************\n"); 

    if( BUFR_Info_Init( &bufr_info ) )
    {
        BUFR_perror( "main" );
        return 1;
    }

    bufr_info.BUFR_MasterTable            =  0; /* Use WMO standard */
    bufr_info.BUFR_Edition            =  3; /* Use WMO standard */
    bufr_info.OriginatingCenter           =128; /* NRL Monterey, CA */
    bufr_info.UpdateSequenceNumber        =  0; /* 0 for original message */
    bufr_info.DataCategory                =  0; /* Surface land data */
    bufr_info.DataSubCategory             =  0; /* BUFR message sub-type */
    bufr_info.VersionNumberOfMasterTables =  5;
    bufr_info.VersionNumberOfLocalTables  =  0;
    bufr_info.MinorLocalVersion           =  0;
    bufr_info.Year                        = 95;
    bufr_info.Month                       = 12;
    bufr_info.Day                         = 31;
    bufr_info.Hour                        = 23;
    bufr_info.Minute                      = 59;
    bufr_info.ObservedData                =  1;
 
    Set_Flag(NoAuto_FTP);
    Set_Flag(Allow_Dup_Tab_Entry);
    Set_Flag(No_Warn_Dup_Tab_Entry);
    
    /* Initialize BUFR message.  Read standard tables. */

    if( BUFR_Init( &bufr_info, "ex10.enc", ENCODING ) )
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

/*  define aircraft registration number  */
    strcpy(tstr,"This is comment text string added to the Example 3 message.");

    /* Define decimal FXY array  */

    fxy_i[0] = 5002;
    fxy_i[1] = 6002;
    fxy_i[2] = 4004;
    fxy_i[3] = 12004;

    /*  pack FXYs using FXY_Pack_Dec */
    
    for ( i=0; i<num_fxys; i++)
       fxy[i] = FXY_Pack_Dec( fxy_i[i] );

/*  Enter a generic character string into message   */
     BUFR_Put_String(tstr);

    /* Enter array of data into bufr message */
    if ( BUFR_Put_Array(values, num_vals, DT_FLOAT, fxy, num_fxys) ){
      BUFR_perror(" Error on call to BUFR_Put_Array in main");
      return 1;
    }

   /* create message  */
    if( BUFR_Encode( &bufr_info ) )
    {
        BUFR_perror( "main" );
        BUFR_Destroy(1);
        return 1;
    }

    BUFR_Destroy(1);

    return 0;
}
