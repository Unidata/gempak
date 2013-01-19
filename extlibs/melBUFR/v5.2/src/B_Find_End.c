/*
 * BUFR_Find_End - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Find_End - Find out if the file pointer is within two bytes of the eof
 *  Return a 1 if at or near the end, otherwise a 0.
 *
 *  Valerie Pastor,  SAIC,  May 28, 1997
 */
/*
 * CHANGE LOG
 *
 * 100997 LAH: Added int cast
 * 101097 LAH: Added void to function prototype 
 *
 */

#include <mel_bufr.h>
 
#if PROTOTYPE_NEEDED
 
int BUFR_Find_End( void )
 
#else
 
int BUFR_Find_End(  )
 
#endif
{
    int current_num, num_left, ret;

    extern BUFR_Msg_t   BUFR_Msg;
    extern int BUFR_Size_of_File;

    BUFR_Msg_t* BM;
 
    BM = &BUFR_Msg;

    ret = 0;

    /* 100997 LAH: Added int cast */
    current_num = (int) ftell(BM->FilePtr);

    num_left = BUFR_Size_of_File - current_num;

    if(num_left <= 34) ret = 1;

    return ret;
}
