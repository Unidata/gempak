/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

int extern_qmessage( int, int, char *,char * );  
int extern_msg(int, int, char [], char []);  

int extern_qmessage(int errorcode,int inint1,char *instrng, char *message)  
/* Subroutine to load External specified diagnostic or error message into character string
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
  float infloat1=0.0,infloat2=0.0;

  retstrng=(char *)calloc((size_t)5000,(size_t)sizeof(char)); 
  iok=extern_msg(errorcode,inint1,instrng,retstrng);

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

int extern_msg(int value,int intin,char *charin,char *retstring)  
/* Output information message and continue program.
*/
{
  char *strng; 

  strng=(char *)calloc((size_t)5000,(size_t)sizeof(char)); 
  switch(value) {
    case 15: sprintf(strng,"Utilizing SST GRIB file %s\n",charin);
             break;
    case 16: sprintf(strng,"Utilizing topography file %s\n",charin);
             break;
    case 81: sprintf(strng,"SST value of %4.1fC at cursor location\n",(float)intin/10.0);
             break;
    case 111: sprintf(strng,"Error accessing SST GRIB file - No TIE Model estimate available\n");
             break;
    case 112: sprintf(strng,"Memory error while reading SST GRIB file - No TIE Model estimate available\n");
             break;
    case 113: sprintf(strng,"Corrupted SST GRIB file - No TIE Model estimate available\n");
             break;
    case 114: sprintf(strng,"Error reading SST GRIB file - No TIE Model estimate available\n");
             break;
    case 115: sprintf(strng,"Invalid SST value - No TIE Model estimate available\n");
             break;
    default: sprintf(strng,"Invalid Information Code %d\n",value);
             break;
  }

  strcat(retstring,strng);

  free(strng);
  strng=NULL;

  return 0;
}
