#include "utfcmn.h"

void uclear ( int *iret )
/************************************************************************
 * uclear								*
 *									*
 * This subroutine clears the UTF file by reintializing the output      *
 * buffer.								*
 *									*
 * uclear  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Safford/GSC	11/96	Initial coding				*
 * S. Jacobs/NCEP	 8/97	Changed to use memset to clear the array*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Return if no file has been opened.
 */
	if  ( ! opnfil )  return ;

/*
 *      Reinitialize the outbuf and numout to 0.
 */
	memset ( outbuf, '\0', MXAFOS );
        numout = 0;

}
