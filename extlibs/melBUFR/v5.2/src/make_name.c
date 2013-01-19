/*
 * make_name - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
/*
..............START PROLOGUE....................................
 
  SCCS IDENTIFICATION: 
 
  CONFIGURATION IDENTIFICATION:
 
  MODULE NAME:         make_name
 
  DESCRIPTION:          make_name creates the file name for a decated message
 
  CLASSIFICATION:      UNCLASSIFIED
 
  RESTRICTIONS:        NONE
 
  ORIGINAL PROGRAMMER, DATE:     VALERIE PASTOR, MARCH 97
 
  CURRENT PROGRAMMER:  VALERIE PASTOR, MARCH 97
 
  LIBRARIES OF RESIDENCE:
 
  USAGE:
      make_name(fp, buffer, iyn, real_length, num_mess, length, offset)
 
  PARAMETERS:
     NAME               TYPE        USAGE           DESCRIPTION
  -------------      ----------    -------   -------------------------
  fp			FILE	    IN		file pointer to BUFR message
  buffer		char	    OUT		computed file name
  iyn			char	    IN		flag on how to create the
						file name
  real_length		int	    IN		Length of file previous to
						this message
  num_mess		int	    IN		Number of messages read
  length	        int         OUT         length of the BUFR message
  offset		int	    OUT		length between messages
 
  ERROR CONDITIONS:
    CONDITION              ACTION
  -------------------   ----------------------------------------------
 
  ADDITIONAL COMMENTS:
 
..............MAINTENANCE SECTION...............................
 
  MODULES CALLED:
 
  NAME                  DESCRIPTION
  ----                  -----------
  Loop_T_Data_Type	Find beginning of message
  Int3ToInt		Takes a 3 byte character word and returns a number
 
  LOCAL VARIABLES AND STRUCTURES:
 
  NAME         	TYPE    	DESCRIPTION
  ----         	----    	-----------
  BUFR_Msg	BUFR_Msg_t	BUFR messages structure
  BM		BUFR_Msg_t	Pointer to a BUFR messages structure
 
  METHOD:
	Set pointer to BUFR structure
        Set variable sizes, fill arrays, and point pointers
	Perform Look_T_Data_Type to find the message offset
	Perform fseek to read section 0 and section 1
	Perform Int3ToInt to get the total length of the message
	Rewind to beginning of the message
	If the user wants a file name contructed then
	  construct a file name
	Else
	  Use the users file name
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
 
..............END PROLOGUE...................................... */
#if PROTOTYPE_NEEDED
 
int make_name( FILE *fp, char *buffer, char iyn, int real_length, 
		int num_mess, int *length, int *offset  )
 
#else
 
int make_name(fp, buffer, iyn, real_length, num_mess, length, offset )
FILE *fp;
char *buffer;
char iyn;
int real_length;
int num_mess;
int *length;
int *offset;
 
#endif
{

    extern BUFR_Msg_t BUFR_Msg;
    BUFR_Msg_t* BM;
    char *s0p, *s1p;
    int   s0l, s1l, sr0, sr1;
/*    int   back_length, ll; */
    int ll;
    char file_type[2][5];
    int off;

    BM = &BUFR_Msg;
 
    strcpy(file_type[0], "BUFR");
    s0p = (char*) &BM->Section0;
    s1p = (char*) &BM->Section1;
    s0l = sizeof( Section0_t );         /* Should be  8 */
    s1l = sizeof( Section1_t );         /* Should be  4 */

/* get the messages offset */
    Loop_T_Data_Type(fp, file_type[0], &off);
/*  seek to the beginning of the message */
    fseek(fp, (off+real_length), SEEK_SET); 
/*  read section 1 and 2 */
    sr0 = fread( s0p, 1, s0l, fp );
    sr1 = fread( s1p, 1, s1l, fp );
    ll = Int3ToInt( BM->Section0.message_length);
    *length = ll;
    *offset = off;
/*    back_length = -(s0l + s1l); */
/* seek to the end of the message */
    fseek(fp, (off+real_length), SEEK_SET); 

/*  create file name */
    if(iyn == 'n'){
      sprintf(buffer,"B%02d%02d%02d%02d%02d%02d%02d_%d", 
	  (int)BM->Section1.originating_center, 
	  BM->Section1.year,  BM->Section1.month, BM->Section1.day,
          BM->Section1.hour, BM->Section1.minute, BM->Section1.data_category,
	  num_mess);

     }
   return 0;
}
