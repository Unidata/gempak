/*
 * FXY_List_Prev - VERSION: %I%  %E% %T%
 */
/*
 * FXY_List_Prev() - Return the address of the entry before the given one in
 * the FXY_List_t.  Return NULL on error.
 */

#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         FXY_List_Prev
 
  DESCRIPTION:         Return the address of the entry before the given one in
			the FXY_List_t.  Return NULL on error.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	FXY_Entry_t* FXY_List_Prev( FXY_List_t* FL, FXY_Entry_t* FE )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  FL		     FXY_List_t	    In		FXY list to search
  FE		     FXY_Entry_t    In		FXY entry to search on
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  No FXY list		Write error message to buffer
			Return with error
  No FXY entry		Write error message to buffer
                        Return with error
  FXY is not in list	Write error message to buffer
                        Return with error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set		Writes error message to buffer
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  prevFE	FXY_Entry_t	Pointer to previous FXY entry
 
  METHOD:
	If there is not a FXY list then
	  Perform BUFR_Err_Set to write error message to buffer
	  Return with an error
	Endif
	If there is not a FXY list entry then
	  Perform BUFR_Err_Set to write error message to buffer
	  Return with an error
	Endif
	Loop on the list until the previous pointer does equal the
		FXY entry
	  if the previous entry equal the tail of the list
	    Perform BUFR_Err_Set to write the error message to a buffer
	    Return with an error
	  Endif
	Endloop
	Return with the previous entry
 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    022498 LAH:  Added prints to bufr_log file.
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

FXY_Entry_t* FXY_List_Prev( FXY_List_t* FL, FXY_Entry_t* FE )

#else

FXY_Entry_t* FXY_List_Prev( FL, FE )
FXY_List_t*  FL;
FXY_Entry_t* FE;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    FXY_Entry_t *prevFE;

    if( FL == NULL )
    {
        BUFR_Err_Set( "FXY_List_Prev", "NULL FXY_List_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Prev", 
            "NULL FXY_List_t pointer" );
        return NULL;
    }

    if( FE == NULL )
    {
        BUFR_Err_Set( "FXY_List_Prev", "NULL FXY_Entry_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Prev", 
            "NULL FXY_Entry_t pointer" );
        return NULL;
    }

    /* Find the FXY entry before FE. */

    for( prevFE=FL->head; prevFE->next != FE; prevFE=prevFE->next )
    {
        if( prevFE == FL->tail )
        {
            BUFR_Err_Set( "FXY_List_Prev",
                "The given FXY entry does not appear in the FXY list" );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Prev", 
                "The given FXY entry does not appear in the FXY list" );
            return NULL;
        }
    }
    return prevFE;
}
