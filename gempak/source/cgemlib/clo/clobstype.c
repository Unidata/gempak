#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern CLO_t	clo;

void clo_bstype ( char *name, int *iret )
/************************************************************************
 * clo_bstype								*
 *									*
 * This function sets which bounds type is to be accessed and resets	*
 * the appropriate pointers to start a search.				*
 *									*
 * clo_bstype ( name, iret )						*
 *									*
 * Input parameters:							*
 *	*name		char	Bound name				*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *				< 0 - bounds file open error return	*
 *					or illegal bounds name		*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 4/01	Created					*
 ***********************************************************************/
{
int	which, ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    which = clo_which ( name );

    if ( which < 0  ||  clo_qformat(name) != 1 )  {
	*iret = -1;
	return;
    }

    /*
     *  Sort the bounds info structure by longitude for searching purposes.
     */
    clo_sortbnd ( name, BND_MXLON, &ier );

    whichBnd = which;

    boundBnd = 0;

    bndsptBnd = -1;

    tBndName[0] = '\0';
    tBndData[0] = '\0';

    if ( fpBnd != (FILE *)NULL )  {

	/*
	 *  Bounds data file already open; close it.
	 */
	cfl_clos ( fpBnd, &ier );

    }

    /*
     *  Open the bounds file.
     */
    clo_bofile ( clo.loc[whichBnd].bnd.filename, &ier );
    if ( fpBnd == NULL  ||  ier != 0 )  {
        *iret = ier;
    }

}
