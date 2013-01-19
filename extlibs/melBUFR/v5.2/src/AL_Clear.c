/*
 * AF_List_Clear - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         AF_List_Clear
 
  DESCRIPTION:         Clears entries for the Associated Fields list
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:   J. R. Atkin 
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	void  AF_List_Clear(  AF_List_t* AL )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
	AL	     AF_List_t	   in/out    Pointer to the associated fields
					     list
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  free		Frees memory for a list
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
 
  METHOD:
		Loop for all list members
		    Free the memory for each list member
		End loop
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

void AF_List_Clear( AF_List_t* AL )

#else

void AF_List_Clear( AL )
AF_List_t* AL;

#endif
{
    AF_Entry_t *AE, *next_AE;

    if( AL == NULL )
        return;

    for( AE=AL->head->next; AE != AL->tail; AE=next_AE )
    {
        next_AE = AE->next;
        memset( (char*)AE, 0, sizeof(AF_Entry_t));
        free( (void*) AE );
    }
}
