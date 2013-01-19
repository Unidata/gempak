/*
 * BUFR_Info_Verify - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Info_Verify(): Verify BUFR information structure prior to encoding
 * a BUFR message.
 */
/*
 * CHANGE LOG
 *
 * 102097 LAH: Added double casts
 * 022498 LAH:  Added prints to bufr_log file.
 *
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_Info_Verify( BUFR_Info_t BI )

#else

int BUFR_Info_Verify( BI )
BUFR_Info_t BI;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    static int monthday[2][12] =
    {
	{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
	{ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
    };

    Table0_t t0v;

    int is_leap;

    char buf[128];

    /* Verify values. */

    t0v = Table0_Value( BI.OriginatingCenter );

    if( t0v.name == NULL )
    {
        sprintf( buf, "Originating center (%d) is not in Table 0",
            BI.OriginatingCenter );
        BUFR_Err_Set( "BUFR_Info_Verify", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Info_Verify", buf);
        return 1;
    }

    /* 102097 LAH: Added double cast  */
    /* Generating Center no longer supported */
    if ( (double) BI.GeneratingCenter != GENERATING_CENTER_FLAG) {
       fprintf(stderr, " Generating Center option no longer supported\n");
       fprintf(stderr, "   generating center ID = %d\n", BI.GeneratingCenter);
    }
/*
    if( (double) BI.GeneratingCenter != BUFR_MISSING_VALUE )
    {
        t0v = Table0_Value( BI.GeneratingCenter );

        if( t0v.name == NULL )
        {
            sprintf( buf, "Generating center (%d) is not in Table 0",
                BI.GeneratingCenter );
            BUFR_Err_Set( "BUFR_Info_Verify", buf );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Info_Verify", buf);
            return 1;
        }
    }
*/

    /* 102097 LAH: Added double cast  */
    if( (double) BI.DataCategory == BUFR_MISSING_VALUE )
    {
        BUFR_Err_Set( "BUFR_Info_Verify",
            "Data category has not been specified" );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Info_Verify",
            "Data category has not been specified");
        return 1;
    }

    if( TableA_Value( BI.DataCategory ) == NULL )
    {
        sprintf( buf, "Invalid data category (%d)", BI.DataCategory );
        BUFR_Err_Set( "BUFR_Info_Verify", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Info_Verify", buf);
        return 1;
    }

    /*
     * Check the date and time.  BUFR_Info_Init() initializes these to
     * MISSING_VAL.  If they're all set this way, it means that the calling
     * routine either doesn't know or forgot to set them.  In any event, print
     * a warning and return.
     */

    /* 102097 LAH: Added double cast  */
    if( (double) BI.Year == BUFR_MISSING_VALUE && 
        (double) BI.Month == BUFR_MISSING_VALUE &&
        (double) BI.Day  == BUFR_MISSING_VALUE &&
        (double) BI.Hour  == BUFR_MISSING_VALUE &&
        (double) BI.Minute == BUFR_MISSING_VALUE )
    {
        fprintf( stderr, "BUFR_Info_Verify(): WARNING, the date and time " );
        fprintf( stderr, "are not set in the BUFR_Info_t structure\n" );
        fflush( stderr );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Info_Verify", 
            "WARNING, the date and time" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
            "are not set in the BUFR_Info_t structure");
        return 0;
    }

    if( BI.Year < 1 || BI.Year > 100 )
    {
        sprintf( buf, "BUFR_Info_Verify(): Invalid year (%d)", BI.Year );
        BUFR_Err_Set( "BUFR_Info_Verify", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Info_Verify", buf);
        return 1;
    }

    if( BI.Month < 0 || BI.Month > 12 )
    {
        sprintf( buf, "BUFR_Info_Verify(): Invalid month (%d)", BI.Month );
        BUFR_Err_Set( "BUFR_Info_Verify", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Info_Verify", buf);
        return 1;
    }

    /* Year is < 100 so there's no need to check centuries. */

    is_leap = ( (BI.Year % 4) == 0 ? 1 : 0 );

    if( BI.Day < 0 || BI.Day > monthday[is_leap][BI.Month-1] )
    {
        sprintf( buf, "BUFR_Info_Verify(): Invalid day (%d)", BI.Day );
        BUFR_Err_Set( "BUFR_Info_Verify", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Info_Verify", buf);
        return 1;
    }

    if( BI.Hour < 0 || BI.Hour > 23 )
    {
        sprintf( buf, "BUFR_Info_Verify(): Invalid hour (%d)", BI.Hour );
        BUFR_Err_Set( "BUFR_Info_Verify", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Info_Verify", buf);
        return 1;
    }

    if( BI.Minute < 0 || BI.Minute > 59 )
    {
        sprintf( buf, "Invalid minute (%d)", BI.Minute );
        BUFR_Err_Set( "BUFR_Info_Verify", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Info_Verify", buf);
        return 1;
    }

    return 0;
}
