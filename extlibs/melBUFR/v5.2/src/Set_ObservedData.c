/*
 * Set_ObservedData  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Set_ObservedData ( BUFR_Info_t* BI, int flag )

#else

int Set_ObservedData ( BI, flag)
BUFR_Info_t* BI;
int flag;

#endif
{

    BI->ObservedData = flag;

    return (0);
}
