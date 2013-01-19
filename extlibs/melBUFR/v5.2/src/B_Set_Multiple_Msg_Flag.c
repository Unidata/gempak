/*
 * 102097 - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
 
#if PROTOTYPE_NEEDED

/* 102097 LAH: Added void to function argument */
void BUFR_Set_Multiple_Msg_Flag(void)
 
#else
 
void BUFR_Set_Multiple_Msg_Flag( )
 
#endif
{
    extern BUFR_Msg_t BUFR_Msg;

    BUFR_Msg_t* BM;


    BM = &BUFR_Msg;

    BM->Multiple_Msg_Flag = 2;
}
