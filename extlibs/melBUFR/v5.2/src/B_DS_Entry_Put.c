/***********************************************************************
* BUFR_DataSetEntryPut.c: VERSION: %I%  %E% %T%
*
* Purpose - Add parameter Val entry to data set linked list.
* Return 1 on error, else 0.
*
* WRITTEN BY: Louis Hembree
*             NRL Monterey
*
* Change LOG:
*
* ARGUMENTS
*  INPUT:
*      
*  OUTPUT:
*
*  RETURN VALUES:
*     o = no error
*    -1 = error
***********************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include "mel_bufr.h"

#if PROTOTYPE_NEEDED

int BUFR_DataSetEntryPut( BUFR_DataSet_t* DS, BUFR_Val_t entry, int IgnoreAFs)

#else

int BUFR_DataSetEntryPut(DS, entry, IgnoreAFs)
BUFR_DataSet_t* DS;
BUFR_Val_t entry;
int IgnoreAFs;
#endif
{
   extern BUFR_Cntl_t BUFR_Cntl;
   BUFR_Val_t* new;
   BUFR_Val_t* last;
   int  n;
   char *cpr;
    
   /* allocate memory for new entry */
   new = (BUFR_Val_t *)malloc(sizeof(BUFR_Val_t));
   (void *) memset( (void *)new,  0,  sizeof(BUFR_Val_t));
    
   /* copy information into new entry */
    
   /* copy Associated fields */
   if( IgnoreAFs == 0 && entry.num_AFs > 0 )
   {
       /* Copy associated field data. */

       n = entry.num_AFs * (int) sizeof(double);

       if( (new->AF=(double*)malloc( (uint_t) n )) == NULL )
       {
           BUFR_Err_Set( "BUFR_DataSetEntryPut",
                "Can't allocate data for associated fields" );
           return -1;
       } else {
            /* 100897 LAH: Added uint_t cast */
            (void) memcpy( (void*)new->AF, (void*)entry.AF, (uint_t) n );
       }
	
       /* Copy associated field significances. */

       n = entry.num_AFs * (int) sizeof(int);
       if( (new->AF_sig=(int*)malloc( (uint_t) n )) == NULL )
       {
          BUFR_Err_Set( "BUFR_DataSetEntryPut",
            "Can't allocate data for associated fields significance");
          fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_DataSetEntryPut:", 
            "Can't allocate data for associated fields significance");
            free( (void*) new->AF );
           return -1;
        } else {
           (void) memcpy( (void*)new->AF_sig, (void*)entry.AF_sig, (uint_t)n );
        }
        new->num_AFs = entry.num_AFs;
    }
    else
    {
         new->AF      = (double*) NULL;
         new->AF_sig  = (int*) NULL;
         new->num_AFs = 0;
    }

    /* copy data */
    new->FXY_Val = entry.FXY_Val;
    new->Val_Type = entry.Val_Type;
    new->missing_flag = entry.missing_flag;
    if ( entry.Val_Type == DT_STRING )
    {
        cpr = (char *)malloc( (strlen(entry.Val.string) +1 ) * sizeof(char) );
        strcpy(cpr, entry.Val.string);
        new->Val.string = cpr;
    } else {
        new->Val = entry.Val;
    }

    last = DS->last;
    new->next = last->next;
    DS->last = new;
    last->next = new;
    
    DS->count++;
    
    return 0;
}
