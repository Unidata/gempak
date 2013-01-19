/*
 * FXY_List_Init - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
/*
 * CHANGE LOG
 * 12/11/97  LAH:  Initialize new elements in FXY_List_t
 * 022498 LAH:  Added prints to bufr_log file.
 */
#if PROTOTYPE_NEEDED

FXY_List_t* FXY_List_Init( void )

#else

FXY_List_t* FXY_List_Init()

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    FXY_List_t* FL;
    FXY_Entry_t *h, *t;

    if( (FL=(FXY_List_t*) malloc( sizeof(FXY_List_t) )) == NULL )
    {
        BUFR_Err_Set( "FXY_List_Init", "Can't create FXY_List" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Init", 
           "Can't create FXY_List" );
        return NULL;
    }

    if( (h = (FXY_Entry_t*) malloc( sizeof(FXY_Entry_t) )) == NULL )
    {
        BUFR_Err_Set( "FXY_List_Init", "Can't allocate head" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Init", 
           "Can't allocate head" );
        return NULL;
    }

    if( (t = (FXY_Entry_t*) malloc( sizeof(FXY_Entry_t) )) == NULL )
    {
        BUFR_Err_Set( "FXY_List_Init", "Can't allocate tail" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Init", 
           "Can't allocate tail" );
        free( (void*) h );
        return NULL;
    }

    FL->head = h;
    FL->tail = t;

    FL->head->fxy  = (FXY_t) NO_FXY_VAL;
    FL->head->next = FL->tail;

    FL->tail->fxy  = (FXY_t) NO_FXY_VAL;
    FL->tail->next = (FXY_Entry_t*) NULL;
    
    /* 12/11/97  LAH:  Initialize new elements in FXY_List_t  */
    FL->last = FL->head;
    FL->last->next = FL->tail;
    
    FL->num_fxys = 0;
    
    return FL;
}
