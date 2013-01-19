/*
 * Get_UpdateSeqNum  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_UpdateSeqNum (void)

#else

int Get_UpdateSeqNum ()

#endif
{
   int num;

   num = BUFR_Msg.Info.UpdateSequenceNumber;

   return (num);
}
