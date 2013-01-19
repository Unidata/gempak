/***********************************************************************
*  BUFR_DataSet_FreeFree.c   VERSION: %I%  %E% %T%
*
* PURPOSE:  Frees memory associated with Data Set linked list
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
/*#include <malloc.h> S. Chiswell/Unidata commented out, use stdlib.h */
#include "mel_bufr.h"

#ifdef PROTOTYPE_NEEDED
int BUFR_DataSet_Free(BUFR_DataSet_t *DS)
#else
int BUFR_DataSet_Free(DS)
BUFR_DataSet_t *DS;
#endif 
{
  
   BUFR_DataSet_Empty(DS);
   free(DS->head);
   free(DS->tail);
   DS->last = (BUFR_Val_t *)NULL;
   return(0);
}
