/*
 * Set_UpdateSequence  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Set_UpdateSequence ( BUFR_Info_t* BI, int sequence_num )

#else

int Set_UpdateSequence ( BI, sequence_num)
BUFR_Info_t* BI;
int sequence_num;

#endif
{

    BI->UpdateSequenceNumber = sequence_num;

    return (0);
}
