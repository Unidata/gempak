/*
 * BUFR_Put_OptionalData - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Put_OptionalData - Add optional data to Section 2's bit stream.
 * Return 1 on error, else 0.
 * 
 * CHNAGE LOG
 * 
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

int BUFR_Put_OptionalData( char* data, int data_length )

#else

int BUFR_Put_OptionalData( data, data_length )
char*       data;
int         data_length;

#endif
{

    BUFR_Msg_t* BM;

    BitStream_t* bs;

    int      i;
    uchar_t* dp;
    EncVal_t ev;

    BM = &BUFR_Msg;

    if( data == NULL )
    {
        BUFR_Err_Set( "BUFR_Put_OptionalData", "NULL pointer for data" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
           "BUFR_Put_OptionalData: NULL pointer for data");
        return 1;
    }

    if( data_length < 1 )
    {
        BUFR_Err_Set( "BUFR_Put_OptionalData", "data length < 1" );
        fprintf(BUFR_Cntl.bufr_log,"%s\n", 
           "BUFR_Put_OptionalData: data length < 1 ");
       return 1;
    }

    if( BUFR_ProcType() == TYPE_DECODE )
    {
       BUFR_Err_Set( "BUFR_Put_OptionalData",
            "Encoding function called while decoding data" );
       fprintf(BUFR_Cntl.bufr_log,"%s%s\n", 
           "BUFR_Put_OptionalData: ", 
           "Encoding function called while decoding data");
        return 1;
    }

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        printf( "Adding Optional Data\n" );
#endif

    /* Add each byte to the Section 2 bit stream. */

    bs = &BM->Section2_Data;

    ev.value = (HexStr_t) data;
    ev.nbits = BITS_IN_BYTE;

    for( i=0, dp=(uchar_t*)data; i < data_length; i++, dp++ )
    {
        ev.value = (HexStr_t) dp;

        if( BitStream_Put( bs, ev ) )
            return 1;
    }

    /*
     * Set optional data flag bit (highest bit) in Section 1.
     */

    BM->Section1.flags |= (1 << (BITS_IN_BYTE-1));

    return 0;
}
