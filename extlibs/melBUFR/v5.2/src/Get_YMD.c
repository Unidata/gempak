/*
 * Get_YMD  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;

#if PROTOTYPE_NEEDED

int Get_YMD (Date_Format_t flag)

#else

int Get_YMD (flag)
Date_Format_t flag;

#endif
{
   int num;

   switch(flag)
   {
     case YYYYMMDD:
        num = BUFR_Msg.Info.Century*100 +  BUFR_Msg.Info.Year;
        num = num*10000 + BUFR_Msg.Info.Month*100 + BUFR_Msg.Info.Day;
	break;
     case MMDDYYYY:
        num = BUFR_Msg.Info.Month*100 +  BUFR_Msg.Info.Day;
        num = num*10000 + BUFR_Msg.Info.Century*100 + BUFR_Msg.Info.Year;
	break;
     case DDMMYYYY:
        num = BUFR_Msg.Info.Day*100 +  BUFR_Msg.Info.Month;
        num = num*10000 + BUFR_Msg.Info.Century*100 + BUFR_Msg.Info.Year;
	break;
     default:
        BUFR_Err_Set("Get_YMD"," bad flag argument");
        return BUFR_ERROR;
   }
   return (num);
}
