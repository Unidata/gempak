/*
 * TableD_Match - VERSION: %I%  %E% %T%
 */
/*
 * TableD_Match - Search Table D's list of sequences for a sequence
 * which matches the given FXY value.  Return a NULL pointer if one
 * cannot be found.
 */

#include <mel_bufr.h>
extern TableD_t* TableD;

#if PROTOTYPE_NEEDED

TableD_Sequence_t* TableD_Match( FXY_t FXYval )

#else

TableD_Sequence_t* TableD_Match( FXYval )
FXY_t FXYval;

#endif
{

    TableD_Sequence_t* DS;

    for( DS=TableD->head->next; DS != TableD->tail; DS=DS->next )
    {
        if( DS->head->fxy_value == FXYval )
            return DS;
    }

    return (TableD_Sequence_t*) NULL;
}
