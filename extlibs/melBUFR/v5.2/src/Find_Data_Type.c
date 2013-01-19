/*
 * Find_Data_Type - VERSION: %I%  %E% %T%
 */
/* 092997  LAH:  Adderd mel_bufr.h include*/
#include <mel_bufr.h> 

#include <stdio.h>
/*
..............START PROLOGUE....................................

  SCCS IDENTIFICATION: 

  CONFIGURATION IDENTIFICATION:

  MODULE NAME:         Find_Data_Type

  DESCRIPTION:    	Find_Data_Type opens a given file and checks to see 
			what kind of data the file contains.

  CLASSIFICATION:      UNCLASSIFIED

  RESTRICTIONS:        NONE

  ORIGINAL PROGRAMMER, DATE:     VALERIE PASTOR, JANUARY 97

  CURRENT PROGRAMMER:  VALERIE PASTOR, JANUARY 97

  LIBRARIES OF RESIDENCE:

  USAGE:
      Find_Data_Type(File_Name, offset)

  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  File_Name		Char	    IN		File to be opened
  offset		Int	    OUT		Place in file where data starts

  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Open ERROR          	Write error message and return to calling routine

  ADDITIONAL COMMENTS:

..............MAINTENANCE SECTION...............................

  MODULES CALLED:

  NAME         		DESCRIPTION
  ----         		-----------
  Loop_T_Data_Type	Reads file looking for a given data type

  LOCAL VARIABLES AND STRUCTURES:

  NAME         TYPE    DESCRIPTION
  ----         ----    -----------
  fp		FILE	File pointer
  file_type	char	Array of current file types (BUFR and GRIB)
  i		int	Loop variable
  out_flag	int	Output return flag
  offs		int	Offset of file

  METHOD:
	Fill file_type array
	Open the given file
	If there is a problem opening the file then
		print error message
		return -1
	Endif
	
	Loop on number of data tye (Currently 2)
	  If Loop_T_Data_Type returns a success then
	        Set the offset to the returned offset
		Set the out_flag to the loop value
		Exit the loop
	  Endif
	  Rewind the file
	End loop

	Close the file

	Return the out_flag

  INCLUDE FILES:
    NAME                DESCRIPTION
  --------------     -------------------------------------------------
   mel_bufr.h        Defines bufr values 

  COMPILER DEPENDENCES:  CC

  COMPILER OPTIONS:      -O

  MAKEFILE:

  RECORD OF CHANGES:
    INITIAL INSTALLATION:

    092997  LAH: Added include for mel_bufr.h 

..............END PROLOGUE...................................... */

#if PROTOTYPE_NEEDED
 
int Find_Data_Type(char* File_Name, int *offset )
 
#else
 
int Find_Data_Type(File_Name, offset)
char* File_Name ;  /* The file to be opened */
int *offset;
#endif
{
/* Find_Data_Type returns the data type of the file.  */
/*  1 - BUFR, 2 - GRIB, 0 - UNKNOWN, -1 open error */
    FILE *fp;
    char file_type[2][5];
    int i;
    int out_flag = 0;
    int offs;

    strcpy(file_type[0], "BUFR");
    strcpy(file_type[1], "GRIB");

    if( (fp = fopen( File_Name, "r" )) == NULL )
    {
        printf( "%s: Can't open file for reading",File_Name);
        return -1;
    }

   for(i = 1; i < 3; i++)
   {
     if(Loop_T_Data_Type(fp, file_type[i-1], &offs))
     {
       *offset = offs;
       out_flag = i;
       break;
     }
     rewind(fp);
     out_flag = 0;
   }
   fclose(fp);
   return out_flag;
}
