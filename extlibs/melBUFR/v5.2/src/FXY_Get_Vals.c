/*
 * FXY_Get_Values - VERSION: %I%  %E% %T%
 */
/*
 * FXY_Get_Vaulues() - Get Scale, Reference Value, and DataWidth for the given
 * FXY value.  If any of the pointers S, RV, or DW are NULL, it means that
 * the calling process isn't interested in that particular value.
 */

#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         FXY_Get_Values
 
  DESCRIPTION:         Get Scale, Reference Value, and DataWidth for the given
			FXY value.  If any of the pointers S, RV, or DW 
			are NULL, it means that the calling process isn't 
			interested in that particular value.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    J.R. Akin
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int FXY_Get_Values( FXY_t FXY_Val, int* S, int* RV, int* DW )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  FXY_Val	     FXY_t	   In		FXY value
  S		     Int	   Out		Scale value
  RV		     Int	   Out		Reference value
  DW		     Int	   Out		Data width value
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  FXY not a Table B     Write error message to buffer
                        Return with an error
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  TableB_Get		Returns a Table B descriptor for a given FXY
  ValStack_IsEmpty	Checks to see if a stack for a given pointer is empty
  ValStack_Get		Gets a piece of data off of the top of a given pointer
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg      BUFR_Msg_t      External BUFR message structure
  BM      	BUFR_Msg_t      BUFR message structure
  descriptor    Descriptor_t    Table B descriptor for a specific FXY
  scale		int		Scaling value
  ref_val	int		Reference value
  dw		int		Data width value
  buf           char            Error message string

  METHOD:
        If the FXY sequence is not a Table B sequence then
          Perform BUFR_Err_Set to write an error message to buffer
          Return with error
        Else
	  Set scale, ref_val, and dw with values from descriptor structure
	Endif
	If the Scale Stack is not empty then
	  Add to scale the result from Performing ValStack_Get
	Endif
	If the Data Width Stack is not empty then
	  Add to dw the result from Performing ValStack_Get
	Endif
 
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

int FXY_Get_Values( FXY_t FXY_Val, int* S, int* RV, int* DW )

#else

int FXY_Get_Values( FXY_Val, S, RV, DW )
FXY_t       FXY_Val;
int         *S, *RV, *DW;

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    BUFR_Msg_t* BM;

    Descriptor_t* descriptor;
    int           scale, ref_val, dw;
    char          buf[256];

    BM = &BUFR_Msg;

    if( (descriptor=TableB_Get( FXY_Val )) == NULL )
    {
        sprintf( buf, "Unknown descriptor (%s)", FXY_String(FXY_Val) );
        BUFR_Err_Set( "FXY_Get_Values", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_Get_Values", buf);
        return 1;
    }
    else
    {
        scale   = descriptor->scale;
        ref_val = descriptor->RefValStack.head->next->val;
        dw      = descriptor->data_width;
    }

    if( !ValStack_IsEmpty( &BM->ScaleStack ) )
        scale += ValStack_Get( &BM->ScaleStack );

    if( !ValStack_IsEmpty( &BM->DataWidthStack ) )
        dw += ValStack_Get( &BM->DataWidthStack );

    if( S != NULL )
        *S = scale;

    if( RV != NULL )
        *RV = ref_val;

    if( DW != NULL )
        *DW = dw;

    return 0;
}
