#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

void clo_bstag ( char *tag, int *iret )
/************************************************************************
 * clo_bstag								*
 *									*
 * This function sets the tag name and tag values on which to search.	*
 *									*
 * clo_bstag ( tag, iret )						*
 *									*
 * Input parameters:							*
 *	*tag		char	Tag name				*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 6/01						*
 ***********************************************************************/
{
int	ier;
char	*lptr, *rptr;
size_t	nchr;
/*---------------------------------------------------------------------*/

    *iret = 0;

    tBndName[0] = '\0';
    tBndData[0] = '\0';

    lptr = strchr(tag,'<');
    rptr = strchr(tag,'>');

    if ( lptr != (char *)NULL && rptr != (char *)NULL && rptr > lptr )  {

	nchr = (size_t)(rptr-lptr) - 1;
        strncpy ( tBndName, (char *)(lptr+1), nchr );
	tBndName[nchr] = '\0';

        cst_gtag ( tBndName, tag, "DEFAULT", tBndData, &ier );

    }

    return;

}
