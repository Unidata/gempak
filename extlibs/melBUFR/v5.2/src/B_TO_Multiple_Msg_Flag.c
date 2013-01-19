/*
 * BUFR_Turn_Off_Multiple_Msg_Flag - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
 
#if PROTOTYPE_NEEDED
 
void BUFR_Turn_Off_Multiple_Msg_Flag(void)
 
#else
 
void BUFR_Turn_Off_Multiple_Msg_Flag( )
 
#endif
{

    BUFR_Msg_t* BM;


    BM = &BUFR_Msg;

    BM->Multiple_Msg_Flag = 0;
}
