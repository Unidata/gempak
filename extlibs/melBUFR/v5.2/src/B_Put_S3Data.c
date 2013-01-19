/*
 * BUFR_Put_S3Data - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Put_S3Data - Add data to Section 3's bit stream.
 * Return 1 on error, else 0.
 *
 * NOTE: Since this function can be called any number of times,
 * the number of bits (NOT bytes) must be specified so that the
 * data is properly appended.
 *
 * CHANGE LOG
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

int BUFR_Put_S3Data( char* Data, int DataBits )

#else

int BUFR_Put_S3Data( Data, DataBits )
char*       Data;       /* Section 3 data                   */
int         DataBits;   /* Size of Section 3 data, IN BITS! */

#endif
{

    int      i;
    uchar_t* dp;
    int      num_bytes;
    int      bits_remaining;

    EncVal_t ev;
    uchar_t  uc;

    if( Data == NULL )
    {
        BUFR_Err_Set( "BUFR_Put_S3Data", "NULL pointer for data" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
           "BUFR_Put_S3Data: NULL pointer for data");
        return 1;
    }

    if( DataBits < 1 )
    {
        BUFR_Err_Set( "BUFR_Put_S3Data", "data length < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
           "BUFR_Put_S3Data: data length < 1");
        return 1;
    }

    if( BUFR_ProcType() == TYPE_DECODE )
    {
        BUFR_Err_Set( "BUFR_Put_S3Data",
           "Encoding function called while decoding data" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
           "BUFR_Put_S3Data: Encoding function called while decoding data");
        return 1;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            BUFR_Err_Set( "BUFR_Put_S3Data",
                "Can't mix value-based and raw processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                "BUFR_Put_S3Data: ", 
                "Can't mix value-based and raw processing methods.");
            break;

        case METHOD_RAW:
            break;

        case METHOD_TEMPLATE:
            BUFR_Err_Set( "BUFR_Put_S3Data",
                "Can't mix template and raw processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
                "BUFR_Put_S3Data: ", 
                "Can't mix template and raw processing methods.");
            return 1;

        /*
         * If the processing method hasn't been set, then this is the first
         * BUFR_Put function to be called. no get.  Set the process flag.
         */

        case METHOD_UNKNOWN:
        default:
            BUFR_Msg.ProcFlag = TYPE_ENCODE;
            BUFR_Msg.MethFlag = METHOD_RAW;
    }

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Adding Section 3 Data\n" );
#endif

    /* Add each complete byte to the Section 3 bit stream. */

    num_bytes = DataBits / BITS_IN_BYTE;

    ev.nbits = BITS_IN_BYTE;

    for( i=0, dp=(uchar_t*)Data; i < num_bytes; i++, dp++ )
    {
        ev.value = (HexStr_t) dp;

        if( BitStream_Put( &BUFR_Msg.Section3_Data, ev ) )
        {
            BUFR_Err_Log( "BUFR_Put_S3Data" );
            return 1;
        }
    }

    bits_remaining = DataBits % BITS_IN_BYTE;

    if( bits_remaining != 0 )
    {
        /* Add trailing bits */

        uc = (uchar_t) *dp;

        /* Trailing bits need to be shifted right. */

        uc >>= (BITS_IN_BYTE - bits_remaining);

        ev.value = &uc;
        ev.nbits = bits_remaining;

        if( BitStream_Put( &BUFR_Msg.Section3_Data, ev ) )
        {
            BUFR_Err_Log( "BUFR_Put_S3Data" );
            return 1;
        }
    }

    return 0;
}
