/*
 * ValStack_Init - VERSION: %I%  %E% %T%
 */
/*
 * ValStack_Init - Initialize a value stack.  Return 1 on error, else 0.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

int ValStack_Init( ValStack_t* VS )

#else

int ValStack_Init( VS )
ValStack_t* VS;

#endif
{

    if( VS == NULL )
    {
        BUFR_Err_Set( "ValStack_Init", "NULL ValStack_t pointer" );
        return 1;
    }

    if( (VS->head=(ValEntry_t*) malloc( sizeof(ValEntry_t) )) == NULL )
    {
        BUFR_Err_Set( "ValStack_Init", "Can't allocate head" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "ValStack_Init", 
	     "Can't allocate head" );
        return 1;
    }

    if( (VS->tail=(ValEntry_t*) malloc( sizeof(ValEntry_t) )) == NULL )
    {
        BUFR_Err_Set( "ValStack_Init", "Can't allocate tail" );
        free( (void*) VS->head );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "ValStack_Init", 
	     "Can't allocate tail" );
        return 1;
    }

    VS->head->val  = 0;
    VS->head->next = VS->tail;

    VS->tail->val  = 0;
    VS->tail->next = (ValEntry_t*) NULL;

    return 0;
}
