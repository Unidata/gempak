/*
 * AF_List_Destroy - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         AF_List_Destroy
 
  DESCRIPTION:         Destroys the Associated Fields List
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
				
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	void AF_List_Destroy( AF_List_t* AL )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
	AL	     AF_List_t*	   In/Out    Associated Fields List pointer
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  AF_List_Clear		Clears each list element from memory
  free		Clears the list from memory
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
 
  METHOD:
		Perform AF_List_Clear to clear list elements
		Perform free to clear the list head
		Perform free to clear the list tail
 
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

void AF_List_Destroy( AF_List_t* AL )

#else

void AF_List_Destroy( AL )
AF_List_t* AL;

#endif
{
    if( AL == NULL )
        return;

    AF_List_Clear( AL );

    free( (void*) AL->head );
    free( (void*) AL->tail );
}
