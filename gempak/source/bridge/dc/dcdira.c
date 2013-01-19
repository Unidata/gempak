#include "geminc.h"
#include "gemprm.h"
#include "mkdirs_open.h"

void dc_dira ( char *filnam, int *iret )
/************************************************************************
 * dc_dira								*
 *									*
 * This routine is an interface for creating directory paths		*
 *									*
 * dc_dira  ( filnam, iret )						*
 *									*
 * Input parameters:							*
 *	*filnam		char		file name, including path	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *						0 if access is ok	*
 *						-1 on error		*
 *									*
 **									*
 * Log:									*
 * S. Chiswell/Unidata	11/02						*
 ***********************************************************************/
{
	char	newfil[512];
	int	ier;

/*---------------------------------------------------------------------*/

	*iret = 0;

	/*
	 * make sure directories to file exist, and create as needed
 	 */

	css_envr ( filnam, newfil, &ier);
	*iret = diraccess(newfil,  (R_OK | W_OK), !0);

}
