/*
 * AF_List_Put - VERSION: %I%  %E% %T%
 */
/*
 * AF_List_Put() - Add a descriptor to a linked-list of associated fields.
 * Return 1 on error, else 0.
 */

#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         AF_List_Put
 
  DESCRIPTION:         Add a descriptor to a linked-list of associated fields.
			Return 1 on error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int AF_List_Put( AF_List_t* AL, int nBits, int Significance )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
 	AL	     AF_List_t*	   In/Out    Pointer to Associated Fields list
      nBits		int	     In	     Number of bits in the field
   Significance		int	     In	     Significance descriptor
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  List doesn't exist	Write error message
			Return with error
  nBits are < zero  	Write error message
			Return with error
  No memory         	Write error message
			Return with error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set		Puts the BUFR error message into a buffer
  malloc		      Mallocs memory
  AF_List_Last		Finds the last data element in the list
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  newAE		AF_Entry_t*	Pointer to the new associated field
  lastAE	AF_Entry_t*	Pointer to the last associated field
 
  METHOD:
	Check to see if the Associated Fields list exists
	Check to see if the number of bits sent is < zero
	Perform  malloc to create memory space for the list item
	Put the data into the new list item
	Perform AF_List_Last to find the last entry
	Insert the new entry before the tail
 
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

int AF_List_Put( AF_List_t* AL, int nBits, int Significance )

#else

int AF_List_Put( AL, nBits, Significance )
AF_List_t* AL;
int        nBits;
int        Significance;

#endif
{
    AF_Entry_t* newAE;
    AF_Entry_t* lastAE;

 
   if( AL == NULL )
    {
        BUFR_Err_Set( "AF_List_Put", "NULL AF_List_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,
	    "AF_List_Put: NULL AF_List_t pointer\n");
        return 1;
    }

    if( nBits < 0 )
    {
        BUFR_Err_Set( "AF_List_Put", "Bit width < 0" );
        fprintf(BUFR_Cntl.bufr_log,
	    "AF_List_Put: Bit width < 0\n");
        return 1;
    }

    /* Create new data entry */

    if( (newAE = (AF_Entry_t*) malloc( sizeof(AF_Entry_t) )) == NULL )
    {
        BUFR_Err_Set( "AF_List_Put", "Can't allocate new entry" );
        fprintf(BUFR_Cntl.bufr_log,
	    "AF_List_Put: Can't allocate new entry\n");
        return 1;
    }
    else
        (void) memset( (char*) newAE, 0, sizeof(AF_Entry_t) );

    newAE->nbits = nBits;
    newAE->sig   = Significance;

    /* Everything went OK.  Insert new entry before tail. */

    lastAE = AF_List_Last( AL );

    newAE->next  = lastAE->next;
    lastAE->next = newAE;

    return 0;
}
