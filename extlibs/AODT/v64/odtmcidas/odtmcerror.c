/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

int mcidas_qmessage( int, int, char *, char *); 
int mcidas_msg( int, int, char *,char * ); 

int mcidas_qmessage(int errorcode,int inint1,char *instrng, char *message) 
/* Subroutine to load McIDAS specified diagnostic or error message into character string
   for output within APPL
   Inputs : errorcode : code for error (negative) or diagnostic (positive) message
            init1     : integer value for message
	    instrng   : character string for message (e.g. file name)
   Outputs: message   : error message string (diagnostic messages are stored for later output)
   Return : 0 : o.k.
*/
{
  int iok;
  char *retstrng; 

  retstrng=(char *)calloc((size_t)5000,(size_t)sizeof(char)); 
  iok=mcidas_msg(errorcode,inint1,instrng,retstrng);

  if(errorcode<0) {
    /* error message, return string */
    strcpy(message,retstrng);  
    message[strlen(retstrng)]='\0';
  } else {
    /* diagnostic message, store value for in diagnostics string */
    strcat(diagnostics,retstrng);
    message[0]="\0";
  }

  free(retstrng);
  retstrng=NULL;
  return 0;
}

int mcidas_msg(int value,int intin,char *charin,char *retstring) 
/* Output information message and continue program.
*/
{
  char *strng;  

  strng=(char *)calloc((size_t)5000,(size_t)sizeof(char)); 
  switch(value) {
    case -11: sprintf(strng,"Error reading image file %s\n",charin);
             break;
    case -12: sprintf(strng,"Error accessing image file %s\n",charin);
             break;
    case -13: sprintf(strng,"Bad navigation in image file %s\n",charin);
             break;
    case -14: sprintf(strng,"Line/Element mismatch error in image file %s\n",charin);
             break;
    case -15: sprintf(strng,"Multiple bands in image file %s\n",charin);
             break;
    case -16: sprintf(strng,"Latitude/Longitude conversion error in image file %s\n",charin);
             break;
    case -17: sprintf(strng,"Data read off edge of image in image file %s\n",charin);
             break;
    case -21: sprintf(strng,"Invalid storm center location... point not on planet\n");
             break;
    case -22: sprintf(strng,"Error setting up navigation with cursor position\n");
             break;
    case -23: sprintf(strng,"Bad navigation in image file %s\n",charin);
             break;
    case -31: sprintf(strng,"Error reading from existing history file %s\n",charin);
             break;
    case -32: sprintf(strng,"Error opening history file %s\n",charin);
             break;
    case -91: sprintf(strng,"Error inidializing McIDAS environment\n");
             break;
    case -92: sprintf(strng,"Error with mouse button entry during scene override\n");
             break;

    case 12: sprintf(strng,"Utilizing image data file %s\n",charin);
             break;
    case 31: sprintf(strng,"User accepted scene type\n");
             break;
    case 32: sprintf(strng,"User modified scene type\n");
             break;
    case 102: sprintf(strng,"Successfully completed graph\n");
             break;
    case 121: sprintf(strng,"OVER and AUTO keywords cannot be used at same time\nDisabling override ability\n");
             break;
    default: sprintf(strng,"Invalid Information Code %d\n",value);
             break;
  }

  strcat(retstring,strng);

  free(strng);
  strng=NULL;

  return 0;
}
