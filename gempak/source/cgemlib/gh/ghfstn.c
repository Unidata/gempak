#include "geminc.h"
#include "gemprm.h"
#include "ghcmn.h"

void gh_fstn ( char *locnam, char *xid, char *xstate, int *srchtyp,
		int *maxlen, int *nret, char *info, int *iret )
/************************************************************************
 * gh_fstn								*
 * 									*
 * This function is a wrapper for the Fortran routines in the GH library*
 * to call CLO_FINDSTN. Therefore, the calling sequence is the same as	*
 * CLO_FINDSTN except that all integers are converted to pointers.	*
 * 									*
 * gh_fstn ( locnam,  xid, xstate, srchtyp, maxlen, nret, info, iret )	*
 * 									*
 * Input parameters:							*
 *	*locnam		char		Data location name		*
 *	*xid		char		Station name or substring	*
 *	*xstate		char		State name (optional)		*
 *	*srchtyp	int		Search type			*
 *					= 1 - EXACT			*
 *					= 2 - FIRST			*
 *					= 3 - INDEX			*
 *	*maxlen		int		Max length of info string	*
 * 									*
 * Output parameters:							*
 * 	*nret		int		Number of stations returned	*
 * 	*info		char		String w/ station information	*
 * 	*iret		int		Return code			*
 * 					= 0 - normal			*
 * 					= >0 - >maxret stations availble*
 * 					= -2 - unable to match station	*
 ** 									*
 * Log:									*
 * S. Jacobs/NCEP	 2/05						*
 ***********************************************************************/
{

    clo_findstn ( locnam, xid, xstate, *srchtyp,
	   	  *maxlen, nret, info, iret );

}
