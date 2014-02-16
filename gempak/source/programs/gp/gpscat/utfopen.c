#include "geminc.h"
#include "gemprm.h"

void utf_open ( char *filnam, int *fptr, int *iret )
/************************************************************************
 * utf_open								*
 *									*
 * This function opens a UTF file.					*
 *									*
 * utf_open ( filnam, fptr, iret )					*
 *									*
 * Input parameters:							*
 *	*filnam		char		UTF filename			*
 *									*
 * Output parameters:							*
 *	*fptr		int		File pointer			*
 *	*iret		int		Return code			*
 *					 -1 = error opening UTF file	*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96						*
 * D. Keiser/GSC	 1/97		Fix comments			*
 ***********************************************************************/
{
    int			ier;
    FILE		*fp;
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
**  Open the UTF file.
*/
    fp = cfl_ropn(filnam, " ", &ier);
    *fptr = (long) fp;
    if ( ier != 0 ) {
	*iret = -1;
    }

}
