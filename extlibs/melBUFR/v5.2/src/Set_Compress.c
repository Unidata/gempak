/*
 * Set_Compress - VERSION: %I%  %E% %T%
 */
 
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void Set_Compress( void )
#else

void Set_Compress()
#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    BUFR_Msg_t* BM;

    BM = &BUFR_Msg;

    BM->Compress_Flag = 1;
    BM->Section3.flags  = (uchar_t) (1 << (BITS_IN_BYTE-2)); 
}
