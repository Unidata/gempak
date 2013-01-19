/*
 * ValStack_Push - VERSION: %I%  %E% %T%
 */
/*
 * ValStack_Push - Push a value onto a value stack.
 * Return 1 on error, otherwise 0.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

int ValStack_Push( ValStack_t* VS, int Value )

#else

int ValStack_Push( VS, Value )
ValStack_t* VS;
int         Value;

#endif
{
    ValEntry_t *NewVE;

    if( VS == NULL )
    {
        BUFR_Err_Set( "ValStack_Push", "NULL ValStack_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "ValStack_Push", 
	     "NULL ValStack_t pointer" );
        return 1;
    }

    if( (NewVE = (ValEntry_t*) malloc( sizeof(ValEntry_t) )) == NULL )
    {
        BUFR_Err_Set( "ValStack_Push", "Can't allocate new entry" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "ValStack_Push", 
	     "Can't allocate new entry" );
        return 1;
    }

    /* Insert new entry onto the top of the stack. */

    NewVE->val  = Value;
    NewVE->next = VS->head->next;

    VS->head->next = NewVE;

    return 0;
}
