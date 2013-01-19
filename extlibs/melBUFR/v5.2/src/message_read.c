/*
 * message_read - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
/*
..............START PROLOGUE....................................
 
  SCCS IDENTIFICATION: 
 
  CONFIGURATION IDENTIFICATION:
 
  MODULE NAME:         message_read
 
  DESCRIPTION:          message_read loops through a file of messages,
			reads each message, and calls mess_write to write
			the header.
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:     VALERIE PASTOR, FEBRUARY 97
 
  CURRENT PROGRAMMER:  VALERIE PASTOR, FEBRUARY 97
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
	message_read( fp, fp2, File_Name, mess_num, num_files )

 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  fp			FILE	    IN		File pointer
  fp2			FILE	    IN		Second File pointer
  File_Name             Char        IN          Name of file
  mess_num		Int	    IN		Number of messages in a file
  num_files		Int	    IN		Total number of messages
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
  Could not malloc	Write error message and exit function
  space
  Could not initialize  Write error message and exit function
  BUFR message struct
  Could not get         Write error message, nullify pointers, close
  decoded value         files, and exit the function
  Could not open a file	Write an error message and exit function

 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  BUFR_Info_Init	Initializes the BUFR message structure
  BUFR_Init		Reads data into the BUFR message structure
  mess_write		Writes the header message to a file
  BUFR_Destroy		Frees pointers
  BUFR_Close		Closes the open BUFR file
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         TYPE    		DESCRIPTION
  ----         ----    		-----------
  BInfo		BUFR_Msg_t	BUFR message structure
  fp3           FILE    	File pointer
  length	int		size of message
  i           	int     	Loop variable
  body		char		array for total message
  p_size	int		array for the lengths of messages
 
  METHOD:
	Perform malloc to set p_size
	If there was a malloc error then
	  Print error message
	  Exit function
	Endif
	Loop on the number of BUFR messages in a file
	  Perform BUFR_Info_Init to initialize the BUFR message structure
	  Perform BUFR_Init to fill the BUFR message structure
	  Perform mess_write to read and write header information
	End Loop
	Open file
	Loop on number of messages
	  read a message
	  write a message
	End Loop
	Close the file
	Perform BUFR_Destroy to free pointers
	  
 
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
 
int message_read( FILE *fp, FILE *fp2, char* File_Name, int mess_num,
	int num_files )
 
#else
 
int message_read( fp, fp2, File_Name, mess_num, num_files )
FILE *fp;
FILE *fp2;
char* File_Name;
int mess_num;
int num_files;
 
#endif
{

    BUFR_Info_t BInfo;
 
    FILE *fp3;
    int length;
    int i;
    char *body;
    int *p_size;

    p_size = malloc((uint_t)(mess_num+1) * sizeof(int));
    if(!p_size){
	printf("Allocation Error \n");
	return -1;
    }


    for(i = 0; i < mess_num; i++){
    /*
     * Initialize the BUFR message structure and store information about
     * the message in "BUFR_Msg."
     */
    if ( BUFR_Info_Init(&BInfo) )
    {
       printf(" >>>> Could not initilize BUFR info structure >>>>>\n");
    }
    if( BUFR_Init( &BInfo, File_Name, DECODING ) )
    {
        BUFR_perror( "message read" );
        return 1;
    }

      mess_write(fp2, (i+num_files), &length);
    /* Process section 0. */
 
      p_size[i] = length;
 
    }
/*    BUFR_Destroy(1); */

#ifdef _WIN32
    if( (fp3 = fopen( File_Name, "rb" )) == NULL )
#else
    if( (fp3 = fopen( File_Name, "r" )) == NULL )
#endif
    {
        printf( "%s: Can't open file for reading",File_Name);
        return -1;
    }

    for(i = 0; i < mess_num; i++){
        length = p_size[i];
        body = (char *) malloc((uint_t)length);
        fread(body, 1, length, fp3);
        fwrite(body, 1, length, fp);
        free(body);
    }

      free(p_size);
      fclose(fp3);
      BUFR_Close();
      BUFR_Destroy(1); 
      return 0;
}
