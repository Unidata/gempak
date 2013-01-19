/*
 * Get_HourMinute - Version %I% %E% %T%
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_HourMinute(void)

#else

int Get_HourMinute()

#endif
{
  int num;

  num = BUFR_Msg.Info.Hour*100 + BUFR_Msg.Info.Minute;
  return(num);
}
