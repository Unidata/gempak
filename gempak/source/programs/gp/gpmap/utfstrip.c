#include "geminc.h"
#include "gemprm.h"

void utf_strip ( unsigned char *buf, long byts, long *tot, int *iret )
/************************************************************************
 * utf_strip								*
 *									*
 * This function strips off the header in a UTF file.  This function 	*
 * receives a buffer of data from a file only.  The buffer must be	*
 * filled before the function is executed.				*
 *									*
 * utf_strip ( buf, byts, tot, iret )					*
 *									*
 * Input parameters:							*
 *	*buf		unsigned char	Pointer to the buffer		*
 *	byts		long		Size of the buffer		*
 *									*
 * Output parameters:							*
 *	*tot		long		Size of the buffer after strip	*
 *	*iret		int		Return code			*
 *					 -5 = buffer is empty		*
 *					 -6 = buffer is already stripped*
 *					-13 = invalid UTF file		*
 **									*
 * Log:									*
 * D. Keiser/GSC	 7/96		Created				*
 * D. Keiser/GSC	12/96		General clean up		*
 * T. Piper/GSC		10/98		Prolog update			*
 * S. Jacobs/NCEP	12/10		Remove check for ZCZC		*
 ***********************************************************************/
{
    unsigned char 	*cp;
/*---------------------------------------------------------------------*/
    *iret = 0;
	
    if ( byts == 0 )
	*iret = -5;
    /*
     * This check is not needed for files created on the CCS. There
     * is no ZCZC at the beginning of the file.
     *
    else if ( *buf != 90 && *(buf + 1) != 67 &&
				*(buf + 2) != 90 && *(buf + 3) != 67 ) {
	*iret = -13;
    	if ( *buf == 193 )
	    *iret = -6;
    }
    */
    else {
	cp  = buf;
	*tot = 0;

	while ( *buf != 193 )
	    buf += 1;

	while ( ( *tot < byts ) && ( *buf != 131 ) ) {
	    if ( ( *buf == 16 ) && ( *(buf + 1) == 16 ) ) {
		buf += 2;
		*cp++ = 16;
		*tot += 0x0001;
	    }
	    else if ( (*buf == 16) && ( *(buf + 1) == 12 ) ) {
		buf += 2;
		*cp++ = 131;
		*tot += 0x0001;
	    }
	    else {
		*cp++ = *buf++;
		*tot += 0x0001;
	    }
	}
    }

}
