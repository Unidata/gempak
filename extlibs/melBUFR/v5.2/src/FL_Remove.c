/*
 * FXY_List_Remove - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         FXY_List_Remove
 
  DESCRIPTION:         Remove the given entry from an FXY_List_t.
			Return NULL on error, otherwise the address of 
			the previous entry.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:     J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	FXY_Entry_t* FXY_List_Remove( FXY_List_t* FL, FXY_Entry_t* FE )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  FL		     FXY_List_t	   In/Out	FXY list
  FE		     FXY_Entry_t   In		FXY entry
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  No FXY list           Write error message to buffer
                        Return with error
  No FXY entry          Write error message to buffer
                        Return with error
  FXY is not in list    Write error message to buffer
                        Return with error

 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Err_Set          Writes error message to buffer
  free		Frees memory
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  lastFE	FXY_Entry_g	Pointer to last FXY entry in list
 
  METHOD:
        If there is not a FXY list then
          Perform BUFR_Err_Set to write error message to buffer
          Return with an error
        Endif
        If there is not a FXY list entry then
          Perform BUFR_Err_Set to write error message to buffer
          Return with an error
        Endif
        Loop on the list to find the FXY entry before FE
          if the last entry equal the tail of the list
            Perform BUFR_Err_Set to write the error message to a buffer
            Return with an error
          Endif
        Endloop
	Set the previous list entry to the next entry to remove an entry
	Update num_fxys counter
	If entry removed is equal to the last entry pointer
	   reset last entry pointer
	Perform free to free up memory

 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    12/11/97 LAH: Modified to accodimate changes in FXY_List_t structure
                 Incorporated a counter and pointer to last entry
    02/24/98 LAH:  Added prints to bufr_log file.
  
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

FXY_Entry_t* FXY_List_Remove( FXY_List_t* FL, FXY_Entry_t* FE )

#else

FXY_Entry_t* FXY_List_Remove( FL, FE )
FXY_List_t*  FL;
FXY_Entry_t* FE;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    FXY_Entry_t *lastFE;

    if( FL == NULL )
    {
        BUFR_Err_Set( "FXY_List_Remove", "NULL FXY_List_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Remove", 
            "NULL FXY_List_t pointer" );
        return NULL;
    }

    if( FE == NULL )
    {
        BUFR_Err_Set( "FXY_List_Remove", "NULL FXY_Entry_t pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Remove", 
            "NULL FXY_Entry_t pointer" );
        return NULL;
    }

    /* Find the FXY entry before FE. */

    for( lastFE=FL->head; lastFE->next != FE; lastFE=lastFE->next )
    {
        if( lastFE == FL->tail )
        {
            BUFR_Err_Set( "FXY_List_Remove",
                "The given FXY entry does not appear in the FXY list" );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_List_Remove", 
                "The given FXY entry does not appear in the FXY list" );
            return NULL;
        }
    }

    /* Everything went OK.  Set lastFE to point to FE->next and free 'FE'. */

    lastFE->next = FE->next;
    
    /* 12/11/97 LAH: decrease coun of FXYs in linked list */
    FL->num_fxys--;
    
    /* 12/11/97 LAH: Update pointer to last entry in list */
    if ( FL->last == FE)
        FL->last = lastFE;
	
    free( (void*) FE );

    return lastFE;
}
