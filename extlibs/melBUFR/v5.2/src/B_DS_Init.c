/***********************************************************************
* B_DS_Init VERSION: %I%  %E% %T%
*
* PURPOSE:  initializes BUFR data set linked list
***********************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include "mel_bufr.h"


#if PROTOTYPE_NEEDED

int BUFR_DataSet_Init(BUFR_DataSet_t* DS)

#else

int BUFR_DataSet_Init( DS)
BUFR_DataSet_t* DS;

#endif
{
    BUFR_Val_t *h, *t;

    if( DS== NULL )
    {
        BUFR_Err_Set("BUFR_DataSet_Init", "File index pointer = NULL");
    }

    if( (h = (BUFR_Val_t*) malloc( sizeof(BUFR_Val_t) )) == NULL )
    {
        BUFR_Err_Set( "BUFR_DataSet_Init", "Can't allocate head");
    }

    if( (t = (BUFR_Val_t*) malloc( sizeof(BUFR_Val_t) )) == NULL )
    {
        free( (void*) h );
        BUFR_Err_Set( "BUFR_DataSet_Init", "Can't allocate tail");
    }

    (void*) memset( (void*) h, 0, sizeof(BUFR_Val_t));
    (void*) memset( (void*) t, 0, sizeof(BUFR_Val_t));
    
    DS->head = h;
    DS->tail = t;

    DS->head->next  = DS->tail;

    DS->tail->next = (BUFR_Val_t*) NULL;

    DS->last = DS->head;
    DS->count = 0;
    return 0;
}
