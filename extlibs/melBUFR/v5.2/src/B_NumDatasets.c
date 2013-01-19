/*
 * BUFR_NumDatasets - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

/* Return the number of data subsets in the BUFR message. */

#if PROTOTYPE_NEEDED

int BUFR_NumDatasets( void )

#else

int BUFR_NumDatasets()

#endif

{
    extern BUFR_Msg_t BUFR_Msg;

    int num_ds = 0;

    if( BUFR_ProcType() == TYPE_DECODE )
    {
       /* convert from Int2 structure to Int */
        num_ds = Int2ToInt(BUFR_Msg.Section3.data_subsets);
    }
    else                /* TYPE_ENCODE */
    {
        num_ds = BUFR_Msg.subset_index - 1;
    }

    return num_ds;
}
