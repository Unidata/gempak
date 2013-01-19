/*
 * mess_write - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
/*
..............START PROLOGUE....................................
 
  SCCS IDENTIFICATION: 
 
  CONFIGURATION IDENTIFICATION:
 
  MODULE NAME:         mess_write
 
  DESCRIPTION:          mess_write writes a bufr header to a given file and
                        prints the same header to screen.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:     VALERIE PASTOR, FEBRUARY 97
 
  CURRENT PROGRAMMER:  VALERIE PASTOR, FEBRUARY 97
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
      mess_write(fp2, mess_num, length)
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  fp2			FILE	    IN		File pointer
  mess_num		int	    IN		Number of messages written
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
	Write header data to a file
	Print header data
 
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
 
int mess_write(FILE *fp2, int mess_num, int *length  )
 
#else
 
int mess_write(fp2, mess_num, length )
FILE *fp2; 
int mess_num;
int *length;
 
#endif
{

    extern BUFR_Msg_t BUFR_Msg;
    BUFR_Msg_t* BM;
 
    BM = &BUFR_Msg;
 
    /* Process section 0. */
 
      *length = Int3ToInt( BM->Section0.message_length);
 
    /* Process section 1. */

      fprintf(fp2,"%04d %d %d %02d%02d%02d%02d%02d %d %d %d\n",
	mess_num, (int)BM->Section1.originating_center,
        BM->Info.GeneratingCenter,
	BM->Section1.year,  BM->Section1.month, BM->Section1.day, 
	BM->Section1.hour, BM->Section1.minute,  
	BM->Section1.data_category, BM->Section1.data_sub_category, *length);

      printf("%04d %d %d %02d%02d%02d%02d%02d %d %d %d\n",
	mess_num, (int)BM->Section1.originating_center,
        BM->Info.GeneratingCenter,
	BM->Section1.year,  BM->Section1.month, BM->Section1.day, 
	BM->Section1.hour, BM->Section1.minute,  
	BM->Section1.data_category, BM->Section1.data_sub_category, *length); 
	
	BUFR_Msg.MsgStatus = BUFR_EOM;

	return 0;
}
