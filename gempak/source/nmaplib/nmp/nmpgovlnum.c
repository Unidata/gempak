#include "nmpcmn.h"

void nmp_govlnum ( int *ovlnum, int *iret )
/************************************************************************
 * nmp_govlnum                                                          *
 *                                                                      *
 * This function gets the number of overlays available.                 *
 *									*
 * void nmp_govlnum ( ovlnum, iret )					*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	*ovlnum		int		The number of overlays 		*
 *      *iret           int             Return code                     *
 *                                       = 0  - OK                      *
 *                                       = -2 - table not read          *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            11/00   Created                                 *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
    *iret = 0;

    *ovlnum = overlay[0].novl;
    if (overlay[0].novl <= 0) {
	*iret = -2;
	*ovlnum = IMISSD;
    }

}

/*=====================================================================*/	    
