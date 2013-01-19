/*
 * BUFR_Get_OptionalData - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Get_OptionalData - Get optional data from Section 2's bit stream
 * and store the given number of bytes in 'data.'  Return the number of
 * bytes assigned.
 * 
 *  CHANGE LOG
 * 
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_Get_OptionalData( char* data, int data_length )

#else

int BUFR_Get_OptionalData( data, data_length )
char*       data;
int         data_length;

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    int      i;
    uchar_t* dp;
    EncVal_t ev;
    double   d;
    int m_flag;  /* missing value flag, required for function call but not */
                  /* used by this function */
    
    int bytes_assigned;

    if( data == NULL )
    {
        BUFR_Err_Set( "BUFR_Get_OptionalData", "NULL pointer for data" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
           "BUFR_Get_OptionalData: NULL pointer for data");
        return 0;
    }

    if( data_length < 1 )
    {
        BUFR_Err_Set( "BUFR_Get_OptionalData", "data length < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
            "BUFR_Get_OptionalData: data length < 1");
        return 0;
    }

    if( BUFR_ProcType() == TYPE_ENCODE )
    {
        BUFR_Err_Set( "BUFR_Get_OptionalData",
            "Decoding function called while encoding data" );
        fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
           "BUFR_Get_OptionalData: ", 
           "Decoding function called while encoding data");
        return 0;
    }

    if( Int3ToInt( BUFR_Msg.Section2.length ) == 0 )
    {
        BUFR_Err_Set( "BUFR_Get_OptionalData",
            "No optional data in BUFR message" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
           "BUFR_Get_OptionalData: No optional data in BUFR message");
        return 0;
    }

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Getting Optional Data\n" );
#endif

    /* Get each byte from the Section 2 bit stream. */

    ev.value = (HexStr_t) NULL;

    bytes_assigned = 0;

    for( i=0, dp=(uchar_t*)data; i < data_length; i++, dp++ )
    {
        ev.value = (HexStr_t) dp;

        if( BitStream_Get( &BUFR_Msg.Section2_Data, &ev, BITS_IN_BYTE ) )
        {
            BUFR_Err_Log( "BUFR_Get_OptionalData" );
            break;
        } else if( EncVal_Get( &d, ev, 0, 0, &m_flag ) )
        {
            BUFR_Err_Log( "BUFR_Get_OptionalData" );
            break;
        }

        *dp = (uchar_t) d;

        EncVal_Destroy( ev );

        bytes_assigned++;
    }

    return bytes_assigned;
}
