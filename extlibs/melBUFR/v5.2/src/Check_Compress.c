/*
 * Check_Compress - VERSION: %I%  %E% %T%
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Check_Compress( void )
#else

int Check_Compress()
#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    BUFR_Msg_t* BM;

    int ret_num;

    BM = &BUFR_Msg;

    ret_num = 0;

    if(BM->Compress_Flag == 1) ret_num = 1;

    return ret_num;
}
