/*
 * AF_List_Init - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         AF_List_Init
 
  DESCRIPTION:         Initializes the Associated Fields list
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
				
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int AF_List_Init( AF_List_t* AL)
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
	AL	     AF_List_t*	   in/out    Pointer to Associated Fields list
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  No AF_List_t* pointer	   Set an error message, return a one
  No memory avail.	   Set an error message, return a one
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  h		AF_Entry_t*	head of list
  t		AF_Entry_t*	tail of list
 
  METHOD:
	Perform malloc to request memory for the head
	Perform malloc to request memory for the tail
	Set head data to zero
	Point head to tail
	Set tail to point to Null
 
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

int AF_List_Init( AF_List_t* AL)

#else

int AF_List_Init( AL)
AF_List_t* AL;

#endif
{
    AF_Entry_t *h, *t;

    if( AL== NULL )
    {
        BUFR_Err_Set( "AF_List_Init", "NULL AF_List_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,
	     "AF_List_Init: NULL AF_List_t pointer\n" );
        return 1;
    }

    if( (h = (AF_Entry_t*) malloc( sizeof(AF_Entry_t) )) == NULL )
    {
        BUFR_Err_Set( "AF_List_Init", "Can't allocate head" );
        fprintf(BUFR_Cntl.bufr_log, "AF_List_Init: Can't allocate head\n" );
        return 1;
    }

    if( (t = (AF_Entry_t*) malloc( sizeof(AF_Entry_t) )) == NULL )
    {
        BUFR_Err_Set( "AF_List_Init", "Can't allocate tail" );
        free( (void*) h );
        fprintf(BUFR_Cntl.bufr_log, "AF_List_Init: Can't allocate tail\n" );
        return 1;
    }

    AL->head = h;
    AL->tail = t;

    AL->head->nbits = 0;
    AL->head->sig   = 0;
    AL->head->next  = AL->tail;

    /* Copy head to tail and set tail to point to nowhere. */

    *AL->tail = *AL->head;

    AL->tail->next = (AF_Entry_t*) NULL;

    return 0;
}
