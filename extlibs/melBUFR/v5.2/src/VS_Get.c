/*
 * ValStack_Get - VERSION: %I%  %E% %T%
 */
/*
 * ValStack_Get - Get top-most entry value off of a value stack.
 * Return BAD_VAL on error, else value.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

int ValStack_Get( ValStack_t* VS )

#else

int ValStack_Get( VS )
ValStack_t* VS;

#endif
{
    if( VS == NULL )
    {
        BUFR_Err_Set( "ValStack_Get", "NULL ValStack_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "ValStack_Get", 
	     "NULL ValStack_t pointer" );
        return BAD_VAL;
    }

    if( ValStack_IsEmpty( VS ) )
        return BAD_VAL;
    else
        return VS->head->next->val;
}
