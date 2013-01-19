/*
 * FXY_List_Put - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         FXY_List_Put
 
  DESCRIPTION:         Add an FXY value to the end of a linked-list of FXYs.
			Return 1 on error, else 0.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int FXY_List_Put( FXY_List_t* FL, FXY_t FXY_Val )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  FL		      FXY_List_t   In/Out	Pointer to the FXY list
  FXY_Val	      FXY_t	   In		FXY descriptor to add to list
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  No FXY list		Write error message to buffer
			Return with error 
  No memory available	Write error message to buffer
			Return with error:Q
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set		Writes error message to a buffer
  malloc		Gets memory for an array
  FXY_List_Last		Finds the last entry in a FXY list
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  newFE		FXY_Entry_T	New list entry
  lastFE	FXY_Entry_T	Last list entry
 
  METHOD:
	If the FXY list is NULL then
	  Perform BUFR_Err_Set to write an error message to a buffer
	  Return with error
	Endif
	If accessing memory via Performing malloc is an error then
	  Perform BUFR_Err_Set to write an error message to a buffer
	  Return with error
	Else
	  File in the new list entry
	Endif
	Perform FXY_List_Last to find the last FXY entry
	Place the new entry after the last entry and before the tail
	  
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    12/11/97 LAH: Added code to update new elements in FXY_List_t 
                  structure 
    02/24/98 LAH:  Added prints to bufr_log file.
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int FXY_List_Put( FXY_List_t* FL, FXY_t FXY_Val )

#else

int FXY_List_Put( FL, FXY_Val )
FXY_List_t*   FL;
FXY_t         FXY_Val;  /* FXY value to add */

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    FXY_Entry_t* newFE;
    FXY_Entry_t* lastFE;

    if( FL == NULL )
    {
        BUFR_Err_Set( "FXY_List_Put", "NULL FXY_List_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Put", 
            "NULL FXY_List_t pointer" );
        return 1;
    }

    /* Create new FXY entry */

    if( (newFE = (FXY_Entry_t*) malloc( sizeof(FXY_Entry_t) )) == NULL )
    {
        BUFR_Err_Set( "FXY_List_Put", "Can't allocate new entry" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Put", 
            "Can't allocate new entry" );
        return 1;
    } else
    {
        newFE->fxy  = FXY_Val;
        newFE->next = (FXY_Entry_t*) NULL;
    }

    /* Everything went OK.  Insert new entry before tail. */
 
    /*  12/11/97 LAH: Replaced call to FXY_List_Last with reference to */
    /*                new last pointer in FXY_List_t structure          */
    /*    lastFE = FXY_List_Last( FL ); */
    lastFE = FL->last;
    
    newFE->next  = lastFE->next;
    lastFE->next = newFE;

    /*  12/11/97 LAH: Update new elements in FXY_List_t structure */
    FL->last = newFE;
    FL->num_fxys++;
    
    return 0;
}
