/*
 * FXYY_List_get - VERSION: %I%  %E% %T%
 * 
 * extracts the FXY list from Section 3
 * 
 * Changes
 * 111601 added m_flag to EncVal_Get.  not used by this function.
 */
#include <mel_bufr.h>
#if PROTOTYPE_NEEDED

int FXY_List_Get(FXY_t **fxy_array)

#else

int FXY_List_Get( fxy_array)
FXY_t **fxy_array;    /* Holds unexpanded S3 FXY values. */

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;
    unsigned char  *bp_save;

    int i, m_flag;  
    EncVal_t  enc_val;
    double    d;
    BitStream_t* S3BS;  /* Section 3 bit stream (FXY values)     */

    int          fxy_array_len;
    FXY_t* array;
    /* Compute number of (unexpanded) FXY values in Section 3. */

    S3BS = &BUFR_Msg.Section3_Data;
    /* save pointer to start of bit string to set pointer before exiting */
    bp_save = S3BS->bp;

    /* 100897 LAH: Added int cast */
    fxy_array_len = S3BS->size / (int )sizeof(short);

    /* Create an FXY List from the Section 3 bit stream. */

    /* 100897 ALH: added uint_t cast */
    array = (FXY_t*) malloc( ((uint_t) fxy_array_len) * sizeof(FXY_t) );

    if( array == NULL )
    {
        BUFR_Err_Set( "FXY_List_Get", "Can't create FXY array for Section 3" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Decode", 
            "Can't create FXY array for Section 3" );
        return 1;
    }

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
        fprintf(BUFR_Cntl.bufr_log, 
	    "Reading FXY values from Section 3 bit stream\n" );
#endif

    for( i=0; i < fxy_array_len; i++ )
    {
        if( BitStream_Get( S3BS, &enc_val, sizeof(short)*BITS_IN_BYTE ) )
        {
            BUFR_Err_Log( "BUFR_Decode" );
            free( (void*) fxy_array );
            return 0;
        }  else if( EncVal_Get( &d, enc_val, 0, 0, &m_flag ) ){
            BUFR_Err_Log( "BUFR_Decode" );
            return 0;
        } else {
            array[i] = (FXY_t) d;
/*            printf(" i=%d   %s  %d\n", i, FXY_String(array[i]), array[i]);  */
            EncVal_Destroy(enc_val);
        }
     }
     
     /* reset pointer & counters to start of bit stream */
     S3BS->byte_num = 0;
     S3BS->bit_num = 0;
     S3BS->bp = bp_save;

     *fxy_array = array;
     return fxy_array_len;
}
