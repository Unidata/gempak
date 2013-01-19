/*
 * Loop_T_Data_Type - VERSION: %I%  %E% %T%
 */
/* 092997 LAH: Added mel_bufr.h & string.h includes */
#include <mel_bufr.h> 
#include <string.h>

#include <stdio.h>
#include <stdlib.h>

#ifndef _WIN32
#include <unistd.h>
#endif

/* 092997 LAH: Removed typedef for Int3_t  */
/* typedef struct {unsigned char val[3];} Int3_t; */
/*
..............START PROLOGUE....................................
 
  SCCS IDENTIFICATION: 
 
  CONFIGURATION IDENTIFICATION:
 
  MODULE NAME:         Loop_T_Data_Type
 
  DESCRIPTION:          Loop_T_Data_Type loops through a file and checks to see
                        what kind of data the file contains.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:     VALERIE PASTOR, JANUARY 97
 
  CURRENT PROGRAMMER:  VALERIE PASTOR, JANUARY 97
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
      Loop_T_Data_Type(fp, File_Type, offset)
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  fp			FILE	    IN		File pointer
  File_Type             Char        IN          File to be looked for
  offset		Int	    OUT		Offset of data
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         TYPE    DESCRIPTION
  ----         ----    -----------
  fp2           FILE    Duplicate File pointer
  message_length
		Int3_t  message length
  c		char	single character variable
  id		char	array for end of file variable
  i,j           int     Loop variables
  s0p		char	pointer to the message length
  DONE		int	flag for when the while loop is done
  som		int	size of the message in octets
  sr0		int	return indicator from fread
  length	int	length of the message
  start		int	loop starting value
 
  METHOD:
	Set variable sizes, fill arrays, and point pointers
	Loop
	  Loop on 100000 octets
		If while reading a character there is and EOF then exit routine
		Else If the character does not equal the first character
			of the File_Type then continue
 
		If while reading a character there is and EOF then exit routine
		Else If the character does not equal the second character
			of the File_Type then continue
 
		If while reading a character there is and EOF then exit routine
		Else If the character does not equal the third character
			of the File_Type then continue
 
		If while reading a character there is and EOF then exit routine
		Else If the character does not equal the fourth character
			of the File_Type then continue

		Set offset to loop value

		Read the message length
		If there is an EOF  return
		Use Int3ToInt to convert message_length to an integer
		Calculate where the 7777s are

		Use fseek to move the file pointer to the end of the message
		Read the four characters that should be the end of message

		Use strncmp to compare the id vs 7777
		If this is not the end of the message then
			Write warning message
			Set the start to the current values of i
			break
		Endif
		Return with success
	  End 100000 loop
	  If i > 99999 set DONE to 1
	End endless loop
	return

 
  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values
 
  COMPILER DEPENDENCES:  CC
 
  COMPILER OPTIONS:      -O
 
  MAKEFILE:
 
  RECORD OF CHANGES:
    INITIAL INSTALLATION:
    092997 LAH: Added mel_bufr.h & string.h includes
                Removed typedef for Int3_t 
    100997 LAH: Added int, uint_t, long and char casts
..............END PROLOGUE...................................... */
 
#if PROTOTYPE_NEEDED
 
int Loop_T_Data_Type(FILE *fp, char* File_Type, int *offset )
 
#else
 
int Loop_T_Data_Type(fp, File_Type, offset)
char* File_Type ;  /* The type to be looked for*/
FILE *fp;
int *offset;
#endif
{
/*    extern BUFR_Msg_t BUFR_Msg; */
    FILE *fp2;
    Int3_t  message_length;     /* Total length of BUFR message */
    int i,j;
    int c;
    char id[5];
    char *s0p;
    int DONE = 0;
    int som;  /* size of message */
    int sr0, length;
    int start = 0;

    som = sizeof(message_length);
    s0p = (char*) &message_length;

    fp2 = fp;

    /*
     * Search for the characters  which signal the
     * beginning of a BUFR or GRIB message.
     */

    while(!DONE){ 
      for ( i = start; i<= 100000; i++)
      {
          /* 100997  LAH: Added char cast */
          if( (c = getc( fp )) == EOF )
              return 0;
          else if( (char) c != File_Type[0] )
              continue;
   
          /* 100997  LAH: Added char cast */
          if( (c = getc( fp )) == EOF )
              return 0;
          else if( (char) c != File_Type[1])
              continue;
   
          /* 100997  LAH: Added char cast */
          if( (c = getc( fp )) == EOF )
              return 0;
          else if( (char) c != File_Type[2])
              continue;
          /* 100997  LAH: Added char cast */
          if( (c = getc( fp )) == EOF )
              return 0;
          else if( (char) c != File_Type[3])
              continue;

	  *offset = i;

/* Find the length of the message */
          /* 100997 LAH:  Added int and uint_t casts */
          sr0 = (int) fread( s0p, 1, (uint_t) som, fp );
          if(sr0 == EOF) return 0;
 	  length = Int3ToInt(message_length);
/*  Calculate when the end of message indicator (7777) should be */
          length = length - som - 8;
 
         /* 101097  LAH: Added uint_t cast */
          fseek(fp2, (long) length, SEEK_CUR);
      /* Read section 5. */
          for(j = 0; j < 4; j++){
            /* 100997  LAH: Added char cast */
            if( (c =  getc( fp2 )) == EOF )
                return 0;
	    id[j] = (char) c;
	  }
 
          if( strncmp( "7777", id, 4 ) != 0 )
          {
              fprintf( stderr,"WARNING: Section 5 ID is %s instead of 7777 %d\n",
                  id, strncmp( id, "7777", 4 ) );
	      start = i;
	      break;
	  } 

    	  return 1;
      }
    if(i > 99999) DONE = 1;
    }
      return 0;
}
