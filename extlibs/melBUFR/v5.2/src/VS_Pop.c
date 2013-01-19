/*
 * ValStack_Pop - VERSION: %I%  %E% %T%
 */
/*
 * ValStack_Pop - Pop top-most entry off of a value stack.
 * Return 1 on error, else 0.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

int ValStack_Pop( ValStack_t* VS )

#else

int ValStack_Pop( VS )
ValStack_t* VS;

#endif
{
    ValEntry_t *TopVE, *NextVE;

    if( VS == NULL )
    {
        BUFR_Err_Set( "ValStack_Pop", "NULL ValStack_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "ValStack_Pop", 
	     "NULL ValStack_t pointer" );
        return 1;
    }

    TopVE  = VS->head->next;
    NextVE = TopVE->next;

    if( TopVE == VS->tail )     /* Stack is empty. */
        return 1;

    free( (void*) TopVE );

    VS->head->next = NextVE;

    return 0;
}
