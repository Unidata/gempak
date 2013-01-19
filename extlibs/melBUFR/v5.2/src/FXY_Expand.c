/*
 * FXY_Expand - VERSION: %I%  %E% %T%
 */
/*
 * FXY_Expand() - Expand a Table D (sequence) descriptor into an allocated
 * array of FXY_t values.  Return 0 on error, else the number of expanded FXY
 * values.
 *
 * NOTE: It is the calling routine's responsibility to free the memory
 * allocated for the array of FXY_t values.
 */

#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         FXY_Expand
 
  DESCRIPTION:          Expand a Table D (sequence) descriptor into an 
			allocated array of FXY_t values.  Return 0 on error, 
			else the number of expanded FXY values.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int FXY_Expand( FXY_t SeqFXY, FXY_t** ExpFXYs )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  SeqFXY		FXY_t	   In	     FXY sequence to expand
  ExpFXYs		FXY_t	   Out	     List of expanded FXYs
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  FXY not a Table D	Write error message to buffer
			Return with an error
  The ExpFXYs is a	Write error message to buffer
  NULL array pointer    Return with an error 
  Bad list expand	Write error message to buffer
                        Return with an error
  Bad malloc 		Write error message to buffer
			Destroy the list
                        Return with an error
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  FXY_IsTableD		Returns a true if a FXY descriptor is from Table D
  BUFR_Err_Set		Write an error message to buffer
  FXY_List_Expand	Expands a sequence into an array of FXY values
  FXY_List_Size		Returns the number of elements in a FXY list
  malloc		Allocates memory for an array
  FXY_List_Destroy	Destroys a list and deallocates it memory
  BUFR_Err_Log		Write an error message to an error log
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  FL		FXY_List_t	FXY list structure
  fe		FXY_Entry_t	FXY element structure
  ExpArray	FXY_t		Expression array
  ea		FXY_t		Expression array
  numExp	int		Number of expressions
 
  METHOD:
	If the FXY sequence is not a Table D sequence then
	  Perform BUFR_Err_Set to write an error message to buffer
	  Return with error
	Endif
	If the Expanded FXY list is not initialized then
	  Perform BUFR_Err_Set to write an error message to buffer
	  Return with error
	Endif
	If Performing FXY_List_Expand results in an error then
	  Perform BUFR_Err_Set to write an error message to buffer
	  Return with error
	Endif
	Perform FXY_List_Size to find the size of the list
        If accessing memory via Performing malloc is an error then
          Perform BUFR_Err_Set to write an error message to a buffer
	  Perform FXY_List_Destroy to destroy the list
          Return with error
        Endif
	Loop on the list of FXYs from FXY_List_Expand
	  Move FXYs to a list of FXYs
	End loop
	Perform FXY_List_Destroy 

 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
     100897 LAH: Added uint_t cast

..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED

int FXY_Expand( FXY_t SeqFXY, FXY_t** ExpFXYs )

#else

int FXY_Expand( SeqFXY, ExpFXYs )
FXY_t   SeqFXY;     /* Sequence to be expanded. */
FXY_t** ExpFXYs;    /* Pointer to pointer for expanded array. */

#endif
{
    FXY_List_t*  FL;
    FXY_Entry_t* fe;

    FXY_t* ExpArray;
    FXY_t* ea;
    int    numExp;

    if( !FXY_IsTableD( SeqFXY ) )
    {
        BUFR_Err_Set( "FXY_Expand", "Non-Table D FXY value given" );
        return 0;
    }

    if( ExpFXYs == NULL )
    {
        BUFR_Err_Set( "FXY_Expand", "NULL FXY array pointer given" );
        return 0;
    }

    if( (FL = FXY_List_Expand( &SeqFXY, 1 )) == NULL )
    {
        BUFR_Err_Log( "FXY_Expand" );
        return 0;
    }

    /* Allocate array of FXY_t values */

    numExp = FXY_List_Size( FL );

    /* 100897 LAH: Added uint_t cast */
    if( (ExpArray = (FXY_t*) malloc( (uint_t)numExp * sizeof(FXY_t) )) == NULL )
    {
        BUFR_Err_Log( "FXY_Expand" );
        FXY_List_Destroy( FL );
        return 0;
    }

    for( fe=FL->head->next, ea=ExpArray; fe != FL->tail; fe=fe->next, ea++ )
        *ea = fe->fxy;

    FXY_List_Destroy( FL );

    *ExpFXYs = ExpArray;

    return numExp;
}
