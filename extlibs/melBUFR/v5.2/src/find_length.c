/*
 * find_length - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
/*
..............START PROLOGUE....................................
 
  SCCS IDENTIFICATION: 
 
  CONFIGURATION IDENTIFICATION:
 
  MODULE NAME:         find_length
 
  DESCRIPTION:          find_length find the length of a BUFR message
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:     VALERIE PASTOR, FEBRUARY 97
 
  CURRENT PROGRAMMER:  VALERIE PASTOR, FEBRUARY 97
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
      find_length(length)
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  length	        int         OUT         length of the BUFR message
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  Int3ToInt		Takes a 3 byte character word and returns a number
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg	BUFR_Msg_t	BUFR messages structure
  BM		BUFR_Msg_t	Pointer to a BUFR messages structure
 
  METHOD:
	Set pointer to BUFR structure
	Perform Int3ToInt to get the total length of the message
 
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
 
void find_length( int *length  )
 
#else
 
void find_length(length )
int *length;
 
#endif
{

    extern BUFR_Msg_t BUFR_Msg;
    BUFR_Msg_t* BM;
 
    BM = &BUFR_Msg;
 
 
      *length = Int3ToInt( BM->Section0.message_length);
 

}
