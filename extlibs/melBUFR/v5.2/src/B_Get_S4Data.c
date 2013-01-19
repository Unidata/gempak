/*
 * BUFR_Get_S4Data - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Get_S4Data - Get data from Section 4's bit stream.
 * Return 0 on error, otherwise the number of bits of data gotten.
 *
 * NOTE: Since this function can be called any number of times,
 * the number of bits (NOT bytes) must be specified so that the
 * data is properly appended.
 *
 * CHANGE LOG
 *
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

int BUFR_Get_S4Data( char* Data, int DataBits )

#else

int BUFR_Get_S4Data( Data, DataBits )
char*       Data;       /* Section 4 data                   */
int         DataBits;   /* Size of Section 4 data, IN BITS! */

#endif
{

    int bits_assigned;

    int      i;
    uchar_t* dp;
    int      num_bytes;
    int      bits_remaining;
    int m_flag;  /* missing value flag, required for function call but not */
                 /* used by this function */

    EncVal_t ev;
    double   d;

    if( Data == NULL )
    {
        BUFR_Err_Set( "BUFR_Get_S4Data", "NULL pointer for data" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
            "BUFR_Get_S4Data: NULL pointer for data");
        return 0;
    }

    if( DataBits < 1 )
    {
        BUFR_Err_Set( "BUFR_Get_S4Data", "data length < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
            "BUFR_Get_S4Data: data length < 1");
        return 0;
    }

    if( BUFR_ProcType() == TYPE_ENCODE )
    {
        BUFR_Err_Set( "BUFR_Get_S4Data",
            "Decoding function called while encoding data" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
            "BUFR_Get_S4Data: ", 
            "Decoding function called while encoding data");
        return 0;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            BUFR_Err_Set( "BUFR_Get_S4Data",
                "Can't mix value-based and raw processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                "BUFR_Get_S4Data: ", 
                "Can't mix value-based and raw processing methods.");
             break;

        case METHOD_RAW:
            break;

        case METHOD_TEMPLATE:
            BUFR_Err_Set( "BUFR_Get_S4Data",
                "Can't mix template and raw processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                "BUFR_Get_S4Data: ", 
                "Can't mix template and raw processing methods.");
            return 0;

        /*
         * If the processing method hasn't been set, then this is the first
         * BUFR_Get function to be called. no get.  Set the process flag.
         */

        case METHOD_UNKNOWN:
        default:
            BUFR_Msg.ProcFlag = TYPE_DECODE;
            BUFR_Msg.MethFlag = METHOD_RAW;
    }

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Geting Section 4 Data\n" );
#endif

    /* Get each complete byte from the Section 4 bit stream. */

    num_bytes = DataBits / BITS_IN_BYTE;

    EncVal_Init( &ev );

    bits_assigned = 0;

    for( i=0, dp=(uchar_t*)Data; i < num_bytes; i++, dp++ )
    {
        if( BitStream_Get( &BUFR_Msg.Section4_Data, &ev, BITS_IN_BYTE ) )
        {
            BUFR_Err_Log( "BUFR_Get_S4Data" );
            return bits_assigned;
        } else if( EncVal_Get( &d, ev, 0, 0, &m_flag))
        {
            BUFR_Err_Log( "BUFR_Get_S4Data" );
            return bits_assigned;
        }

        *dp = (uchar_t) d;
        EncVal_Destroy( ev );
        bits_assigned += BITS_IN_BYTE;
    }

    bits_remaining = DataBits % BITS_IN_BYTE;

    if( bits_remaining )
    {
        /* Get trailing bits */

        EncVal_Init( &ev );
        ev.nbits = bits_remaining;

        if( BitStream_Get( &BUFR_Msg.Section4_Data, &ev, BITS_IN_BYTE ) )
        {
            BUFR_Err_Log( "BUFR_Get_S4Data" );
            return bits_assigned;
        } else if( EncVal_Get( &d, ev, 0, 0, &m_flag) )
        {
            BUFR_Err_Log( "BUFR_Get_S4Data" );
            return bits_assigned;
        }

        *dp = (uchar_t) d;
        EncVal_Destroy( ev );
        bits_assigned += bits_remaining;
    }

    return bits_assigned;
}
