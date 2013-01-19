/*
 * AF_List_Remove - VERSION: %I%  %E% %T%
 */
/*
 * AF_List_Remove() - Remove the last entry in a linked-list of associated
 * fields.
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         	AF_List_Remove
 
  DESCRIPTION:         	Remove the last entry in a linked-list of associated
			fields.
 
  CLASSIFICATION:      	UNCLASSIFIED
 
  RESTRICTIONS:        	NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
				
 
  CURRENT PROGRAMMER:  	V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
		int AF_List_Remove( AF_List_t* AL )	
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
        AL           AF_List_t*    In/Out    Pointer to Associated Fields list
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  List doesn't exist    Write error message
                        Return with error
  List is empty		Write error message
                        Return with error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set          Puts the BUFR error message into a buffer
  AF_List_Last          Finds the last data element in the list
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  AE         	AF_Entry_t*     Pointer to the entry to remove 
  last_AE       AF_Entry_t*     Pointer to the last entry before AE
 
  METHOD:
	Check to see if the Associated Fields list exists
        Check to see if there are list items 
	Loop to find the entry before AE
	Perform free to free the memory for the list item
	Take out the list item

 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    022498 LAH Added bufr_log prints
     
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int AF_List_Remove( AF_List_t* AL )

#else

int AF_List_Remove( AL )
AF_List_t* AL;

#endif
{
    AF_Entry_t* AE;         /* AF_Entry to remove      */
    AF_Entry_t* last_AE;    /* AF_Entry before AE */

    if( AL == NULL )
    {
        BUFR_Err_Set( "AF_List_Remove", "NULL AF_List_t pointer" );
	fprintf(BUFR_Cntl.bufr_log, 
	     "AF_List_Remove: NULL AF_List_t pointer\n");
        return 1;
    }

    if( (AE = AF_List_Last( AL )) == NULL )
    {
        BUFR_Err_Set( "AF_List_Remove", "AF_List_t is empty" );
	fprintf(BUFR_Cntl.bufr_log, 
	     "AF_List_Remove: AF_List_t is empty\n");
        return 1;
    }

    /* Find entry before AE */

    for( last_AE=AL->head; last_AE->next != AE; last_AE=last_AE->next )
        ;

    free( (void*) AE );

    last_AE->next = AL->tail;

    AL->head->next = AL->tail;

    return 0;
}
