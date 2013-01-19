/*
 * FXY_IsLocal - VERSION: %I%  %E% %T%
 */
/*
 * FXY_IsLocal() - Returns a zero is the FXY value is from a local table  
 * else it returns a 1.
 */

#include <mel_bufr.h>

/*
..............START PROLOGUE....................................
 
  MODULE NAME:         FXY_IsLocal
 
  DESCRIPTION:         Returns a zero is the FXY value is from a local table
			else it returns a 1.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:    V.L. Pastor
 
  CURRENT PROGRAMMER:  V.L. Pastor
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	int FXY_IsLocal( FXY_t FXY_Val )
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  FXY_Val	     FXY_t	   In		FXY value
 
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
  descriptor    Descriptor_t    Table B descriptor for a specific FXY
  buf           char            Error message string
  ret_val	int		returning value 0 or 1

  METHOD:
        If the FXY sequence is not a Table B sequence then
          Perform BUFR_Err_Set to write an error message to buffer
          Return with error
        Else
	  Check local table indicator
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

int FXY_IsLocal( FXY_t FXY_Val)

#else

int FXY_IsLocal( FXY_Val )
FXY_t       FXY_Val;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;

    Descriptor_t* descriptor;
    int ret_val;
    char          buf[256];

    ret_val = 1;

    if( (descriptor=TableB_Get( FXY_Val )) == NULL )
    {
        sprintf( buf, "Unknown descriptor (%s)", FXY_String(FXY_Val) );
        BUFR_Err_Set( "FXY_Get_Values", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_Get_Values", buf);
        return -1;
    } else
    {
        if(descriptor->local_flag == 0) ret_val = 0;
    }

    return ret_val;
}
