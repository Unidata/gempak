/*
 * BUFR_Cancel_AF - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Cancel_AF - Cancel the most recently defined associated field.
 * Return 1 on error, else 0.
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int BUFR_Cancel_AF( void )

#else

int BUFR_Cancel_AF()

#endif
{

    BUFR_Msg_t* BM;

    FXY_t AF_fxy;   /* 02-04-000 */

    BM = &BUFR_Msg;

    /* Remove most recent AF in linked list of associated fields. */

    if( AF_List_Remove( BM->af_list ) )
    {
        BUFR_Err_Log( "BUFR_Cancel_AF" );
        return 1;
    }

    /* Add AF cancellation operator to data list. */

    AF_fxy = FXY_Pack( 2, 4, 0 );

    if( DataList_Put( BM->data_list, &AF_fxy, 1, (EncVal_t*)NULL, 0 ) )
    {
        BUFR_Err_Log( "BUFR_Cancel_AF" );
        return 1;
    }

    return 0;
}
