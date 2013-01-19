/*
 * write_msg - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
/*
..............START PROLOGUE....................................
 
  SCCS IDENTIFICATION: 
 
  CONFIGURATION IDENTIFICATION:
 
  MODULE NAME:         write_msg
 
  DESCRIPTION:          write_msg writes a BUFR message to a file  
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:     VALERIE PASTOR, FEBRUARY 97
 
  CURRENT PROGRAMMER:  VALERIE PASTOR, FEBRUARY 97
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
      write_msg(fp, fp2, length, big_length)
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  fp			FILE	    IN		Input file file pointer
  fp2			FILE	    IN		Output file file pointer
  length	        int         IN          length of the BUFR message
  big_length		int	    IN		Total length of messages
						previous to this one
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  body		char		Pointer to a BUFR message
 
  METHOD:
	Perform malloc to set aside enough memory for the message
	Perform fseek to find the beginning of the message
	Read the message into the buffer, body
	Write the message out to the user designated file
	Perform free to free the pointer
 
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
 
int write_msg( FILE *fp, FILE *fp2, int length, int big_length  )
 
#else
 
int write_msg(fp, fp2, length, big_length )
FILE *fp;
FILE *fp2;
int length;
int big_length;
 
#endif
{
    char *body;

    body = (char *) malloc((uint_t)length);
    fseek(fp, (long)big_length, SEEK_SET);
    fread(body, 1, (uint_t)length, fp);
    fwrite(body, 1, (uint_t)length, fp2);
    fseek(fp, (long)(big_length+length), SEEK_SET);

    free(body);

    return 0;
}
