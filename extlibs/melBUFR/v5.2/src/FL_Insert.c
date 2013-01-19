/*
 * FXY_List_Insert - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         FXY_List_Insert

  DESCRIPTION:         Insert an FXY_Entry_t between the given one and
			the next one.  Return 1 on error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int FXY_List_Insert( FXY_Entry_t* FE, FXY_t FXY_Val )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  FE		     FXY_Entry_t   In/Out	List entry item
  FXY_Val	     FXY_t	   In	        FXY value to insert
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  NULL entry pointer	Write error message to the buffer
			Return with an error
  Bad malloc        	Write error message to the buffer
			Return with an error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set		Places an error message into a buffer
  malloc		Requests array memory
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  newFE		FXY_Entry_t	New FXY entry structure
  next		FXY_Entry_t	Next FXY entry structure
 
  METHOD:
	If the FXY entry is empty then
	  Perform BUFR_Err_Set to place an error message into a buffer
	  Return with an error
	Endif
	If there is an error while performing malloc then
	  Perform BUFR_Err_Set to place an error message into a buffer
	  Return with an error
	Else
	  Place the new FXY information into the new FXY entry
	Endif
	Insert new entry before next
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    12/15/97 LAH:  Modified to allow updateing of modified FXY_List 
                   structure.  Included adding input argument.
    02/24/98 LAH:  Added prints to bufr_log file.
    10/08/98 VLP:  changed Main_list to MAIN_List in second
		   function definition
  
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int FXY_List_Insert( FXY_Entry_t* FE, FXY_t FXY_Val,  FXY_List_t *MAIN_List )

#else

int FXY_List_Insert( FE, FXY_Val, MAIN_List )
FXY_Entry_t* FE;
FXY_t        FXY_Val;   /* FXY value to insert */
FXY_List_t   *MAIN_List; /* pointer to main list structure */

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    FXY_Entry_t* newFE;
    FXY_Entry_t* nextFE;

    if( FE == NULL )
    {
        BUFR_Err_Set( "FXY_List_Insert", "NULL FXY_Entry_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Insert", 
            "NULL FXY_Entry_t pointer" );
        return 1;
    }

    /* Create new FXY entry */

    if( (newFE = (FXY_Entry_t*) malloc( sizeof(FXY_Entry_t) )) == NULL )
    {
        BUFR_Err_Set( "FXY_List_Insert", "Can't allocate new entry" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Insert", 
            "Can't allocate new entry" );
        return 1;
    }
    else
    {
        newFE->fxy  = FXY_Val;
        newFE->next = (FXY_Entry_t*) NULL;
    }

    /* Everything went OK.  Insert new entry before next. */

    nextFE = FE->next;

    FE->next    = newFE;
    newFE->next = nextFE;
    
    /* 12/15/97 LAH: Following code added to update modified FXY_List */
    /*               structure */
    MAIN_List->num_fxys++;
    if ( MAIN_List->last == FE )
         MAIN_List->last = newFE;
	 
    return 0;
}
