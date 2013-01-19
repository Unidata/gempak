/*
 * Set_Date - VERSION: %I%  %E% %T%
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Set_Date( BUFR_Info_t* BI, int date, Date_Format_t flag)

#else

int Set_Date( BI, date, flag)
BUFR_Info_t* BI;
int date;
Date_Format_t flag;
#endif
{
    int mm, dd, yy, cc, rem;
    if( BI == NULL )
    {
        BUFR_Err_Set( "BUFR_Info_Init", "NULL BUFR_Info_t pointer" );
        return 1;
    }

    switch (flag)
    {
       case YYYYMMDD:
         cc = date/1000000;
         rem = date - cc*1000000;
         yy = rem/10000;
         rem = rem - yy*10000;
         mm = rem/100;
         dd = rem - mm*100;
         break;
       case MMDDYYYY:
         mm = date/1000000;
         rem = date - mm*1000000;
         dd = rem/10000;
         rem = rem - dd*10000;
         cc = rem/100;
         yy = rem - cc*100;
         break;
       case DDMMYYYY:
         dd = date/1000000;
         rem = date - dd*1000000;
         mm = rem/10000;
         rem = rem - mm*10000;
         cc = rem/100;
         yy = rem - cc*100;
         break;
      default:
         BUFR_Err_Set("Set_Date"," bad date flag argument");
         return BUFR_ERROR;
    }


    BI->Year = yy;
    BI->Century = cc;
    BI->Month = mm;
    BI->Day = dd;
    
    return 0;
}
