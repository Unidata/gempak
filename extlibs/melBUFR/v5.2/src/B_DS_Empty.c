/***********************************************************************
*  BUFR_DataSet_Empty.c   VERSION: %I%  %E% %T%
*
* PURPOSE:  Frees memory associate with data set entry and removes
            entries from Data Set linked list,  until linked list is empty
*
* WRITTEN BY: Louis Hembree
*             NRL Monterey
*
* Change LOG:
*
* ARGUMENTS
*  INPUT:
*    list = linked list of names
*  OUTPUT:
*    NONE
*
*  RETURN VALUES:
*     NONE
***********************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include "mel_bufr.h"

#ifdef PROTOTYPE_NEEDED
int BUFR_DataSet_Empty(BUFR_DataSet_t *DS)
#else
int BUFR_DataSet_Empty(DS)
BUFR_DataSet_t *DS;
#endif 
{
   BUFR_Val_t *ni_entry;
   BUFR_Val_t *next;
   
   next = DS->head->next;
   
   while ( next != DS->tail )
   {
       ni_entry = next;
       next = ni_entry->next;
       if ( ni_entry->AF != NULL) free(ni_entry->AF);
       if ( ni_entry->AF_sig != NULL) free(ni_entry->AF_sig);
       if ( ni_entry->Val_Type == DT_STRING )
           free (ni_entry->Val.string);
       free(ni_entry);
   }
   DS->head->next=DS->tail;
   DS->last = DS->head;
   DS->count = 0;
   return(0);
}
