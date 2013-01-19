/*
 * Num_Messages - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;
/*
..............START PROLOGUE....................................
 
  SCCS IDENTIFICATION: 
 
  CONFIGURATION IDENTIFICATION:
 
  MODULE NAME:         Num_Messages
 
  DESCRIPTION:          Num_Messages opens a file and checks how many 
                        messages are in it.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:     VALERIE PASTOR, JANUARY 97
 
  CURRENT PROGRAMMER:  VALERIE PASTOR, JANUARY 97
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
      Num_Messages(File_Name)
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  File_Name             Char        IN          File name 
 
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
  fp            FILE    File pointer
  s0p		char	pointer to section 0
  s5p		char	pointer to section 5
  s0l		int	size of section 0
  s5l		int	size of section 5
  sr0		int	return indicator from fread
  length	int	length of the message
  num_of_mess	int	number of messages counter
  seven		char	Array of 7s
  file_type	char	Array of file type to look for
  offset	int	Number of octets between a message end and a beginning
			or between the start of a file and a message beginning
  real_length	int	The total of previous message lengths and their offsets
 
  METHOD:
	Set variable sizes, fill arrays, and point pointers
	Loop through file
	  Perform Loop_T_Data_Type to find offset to where the BUFR data 
		begins in the file
	  Perform fseek to position file to beginning of data
	  Read section 0
	  If the size of the message equals EOF or does not equal the
		expected size of the message then return number of messages
	  Perform Int3ToInt to convert the length to an integer form
	  Calculate the real_length of the message (length + offset)
	  Calculate using length the placement of the end of the message
	  Perform fseek to skip down to the end of the message
	  Read section 5
	  If the size of the message does not equal the expected size then
		print warning message
		return number of messages - 1
	  If the section 5 string does not equal 7777 then
		print warning message
		return number of messages - 1
	If while reading a character there is an EOF then exit routine
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
    092997  LAH:  Removed definition of unused variable c
    100997  LAH:  Added return 0 at end to eliminate warnning message
                  Added int, size_t, and long casts
    022498 LAH:  Added prints to bufr_log file.
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED
 
int Num_Messages( char* File_Name )
 
#else
 
int Num_Messages( File_Name )
char* File_Name;
 
#endif
{

    BUFR_Msg_t* BM;

    FILE *fp;
    char *s0p, *s5p;
    char seven[5];
    int   s0l, s5l, sr0;
    int length;
    int num_of_mess = 0;
    char file_type[2][5];
    int offset;
    int real_length;

    strcpy(file_type[0], "BUFR");

#ifdef _WIN32
    if( (fp = fopen( File_Name, "rb" )) == NULL )
#else
    if( (fp = fopen( File_Name, "r" )) == NULL )
#endif
    {
        printf( "%s: Can't open file for reading",File_Name);
        fprintf(BUFR_Cntl.bufr_log,
	     "%s: Can't open file for reading",File_Name);
    	fclose(fp);
        return -1;
    }
    real_length = 0;

    strcpy(seven, "7777");
    BM = &BUFR_Msg;

    s0p = (char*) &BM->Section0;
    s5p = (char*) &BM->Section5;
    s0l = sizeof( Section0_t );         /* Should be  8 */
    s5l = sizeof( Section5_t );         /* Should be  4 */
    length = 0;

 
    for(;;){
/* find the beginning of the BUFR data */
      Loop_T_Data_Type(fp, file_type[0], &offset);
      /* 100997 LAH: Added long & int casts */
      fseek(fp, (long)(offset+real_length), (int)SEEK_SET);
    /* Read section 0. */
      /* 100997 LAH: Added int & size_t cast */
      sr0 = (int) fread( s0p, 1, (size_t) s0l, fp );
      if(sr0 == EOF || sr0 != s0l) {
    	fclose(fp);
	return num_of_mess;
      } 
      num_of_mess++;
      if( sr0 != s0l )
      {
          fprintf(BUFR_Cntl.bufr_log,
	      "Num_Messages   Can't read Section 0 %d", num_of_mess );
          BUFR_Err_Set( "Num_Messages", "Can't read Section 0" );
    	  fclose(fp);
          return 1;
      }
      length = Int3ToInt( BM->Section0.message_length);
      real_length += length + offset;
      length = length - s0l - s5l;

      /* 100997 LAH: Added long & int casts */
      fseek(fp, (long) length, (int) SEEK_CUR);
 
      /* Read section 5. */
 
      /* 100997 LAH: Added size_t cast */
      if( fread( s5p, 1, (size_t) s5l, fp ) != (size_t) s5l )
      {
          BUFR_Err_Set( "Num_Messages", "Can't read Section 5" );
          fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "Num_Messages", 
	      "Can't read Section 5" );
    	  fclose(fp);
          return num_of_mess - 1;
      }
 
      if( strncmp( BM->Section5.id, seven, 4 ) != 0 )
      {
          fprintf( stderr, "WARNING: Section 5 ID is %s instead of 7777 \n",
              BM->Section5.id );
          fprintf(BUFR_Cntl.bufr_log,
	      "WARNING: Section 5 ID is %s instead of 7777 \n",
              BM->Section5.id );
    	  fclose(fp);
          return num_of_mess - 1;
   
/*****************************************************************************
        sprintf( errbuf, "File \"%s\" is not a BUFR file", BM->FileName );
        BUFR_Err_Set( "Num_Messages", errbuf );
        return 1;
******************************************************************************/
      }
    } /* endless for loop */
    /* this return is needed to avoid a warning message */
    return 0;
}
