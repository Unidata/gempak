#include "geminc.h"
#include "gemprm.h"


void cst_padString ( const char *inString, char fillChar, int fillDir, 
		int outLength, char *outString )
/************************************************************************
 * cst_padString							*
 *									*
 * This subroutine fills the input string with the given fill character	* 
 * if the length of the input string is less than the given output	*
 * length, starting from the beginning if the fill direction is 0 and 	*
 * from the end if fill direction is 1. However, if the length of the	*
 * input string is greater than or equal to the given output length, 	*
 * the first "outLength" characters are copied into the output string.	*
 * The ending NULL character does not count against "outLength".	*
 *									*
 * cst_padString ( inString, fillChar, fillDir, outLength, outString )	*
 *									*
 * Input parameters:							*
 *	*inString	char		Input string			*
 *	fillChar	char		Filling character 		*
 *	fillDir		int		Filling direction	 	*
 *	outLength	int		Length for output string	*
 *									*
 * Output parameters:							*
 *	*outString	char		Output string			*
 **									*
 * Log:									*
 * J. Wu/SAIC	 08/04		initial coding				*
 ***********************************************************************/
{

    int		ii, inLength, fillLength;
/*---------------------------------------------------------------------*/
        
    /*
     *   Find lenth of the input string and length for padding.
     */
    inLength   = strlen ( inString );
    fillLength = outLength - inLength;
        

    /*
     *   Fill the output string up to "outLength", filling with
     *   padding character if necessary.
     */
    if ( fillLength <= 0 ) {
        strncpy ( outString, inString, outLength );
    }
    else {
	if ( fillDir == 0 ) {
	    for ( ii = 0; ii < fillLength; ii++ ) {
	        outString[ii] = fillChar;
	    }
            
	    outString[fillLength] = '\0';
	    	    
	    strcat ( outString, inString );
	}
	else {
	    
	    strcpy ( outString, inString );

	    for ( ii = inLength; ii < outLength; ii++ ) {
	        outString[ii] = fillChar;
	    }	
	}	
    }            
    
    /* 
     *  Be sure to end the string.
     */
    outString[outLength] = '\0';	    

}
