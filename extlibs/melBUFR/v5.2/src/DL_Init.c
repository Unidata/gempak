/*
 * DataList_Init - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

/* Initializes the data list and sets all of the defaults and the
   pointers to NULL */
   
/*
 * CHANGER LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */
 
#if PROTOTYPE_NEEDED

int DataList_Init( DataList_t* DL )

#else

int DataList_Init( DL )
DataList_t* DL;

#endif
{
    DataEntry_t *h, *t;
    extern BUFR_Cntl_t BUFR_Cntl;

    if( DL == NULL )
    {
        BUFR_Err_Set( "DataList_Init", "NULL DataList_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Init", 
            "NULL DataList_t pointer" );
        return 1;
    }

    if( (h = (DataEntry_t*) malloc( sizeof(DataEntry_t) )) == NULL )
    {
        BUFR_Err_Set( "DataList_Init", "Can't allocate head" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Init", 
            "Can't allocate head" );
        return 1;
    }

    if( (t = (DataEntry_t*) malloc( sizeof(DataEntry_t) )) == NULL )
    {
        BUFR_Err_Set( "DataList_Init", "Can't allocate tail" );
        free( (void*) h );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "DataList_Init", 
            "Can't allocate tail" );
        return 1;
    }

    DL->head = h;
    DL->tail = t;

    DL->head->fxy_list  = NULL;
    DL->head->num_fxys  = 0;
    DL->head->value     = NULL;
    DL->head->next      = DL->tail;

    /* Copy head to tail and set tail to point to nowhere. */

    *DL->tail = *DL->head;

    DL->tail->next = (DataEntry_t*) NULL;

    return 0;
}
