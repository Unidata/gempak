/*
 * ValStack_IsEmpty - VERSION: %I%  %E% %T%
 */
/*
 * ValStack_IsEmpty - Return 1 if value stack is empty, else 0.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

int ValStack_IsEmpty( ValStack_t* VS )

#else

int ValStack_IsEmpty( VS )
ValStack_t* VS;

#endif
{
    if( VS == NULL )
    {
        BUFR_Err_Set( "ValStack_IsEmpty", "NULL ValStack_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "ValStack_IsEmpty", 
	     "NULL ValStack_t pointer" );
        return BAD_VAL;
    }
    else
        return VS->head->next == VS->tail;
}
