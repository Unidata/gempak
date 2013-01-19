#include "crgcmn.h"


void crg_ggnhl ( char grptyp, int *high_grpnum, int *low_grpnum, int *iret )
/************************************************************************
 * crg_ggnhl                                                          	*
 *                                                                      *
 * This function returns the largest and the smallest group number to 	*
 * be used for the specified group. 					*
 *                                                                      *
 * crg_ggnhl ( grptyp, high_grpnum, low_grpnum, iret ) 			*
 *                                                                      *
 * Input parameters:                                                    *
 * 	grptyp		char		group type			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*high_grpnum	int		the largest group number	*	
 *      *low_grpnum    	int             the smallest group number       *
 *      *iret           int             Return code (always 0)          *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		04/02       					*
 * E. Safford/SAIC	04/02	verify ioffset is valid (positive value)*
 ***********************************************************************/
{
int ii, nn, max_grpnum, min_grpnum, found;
/*---------------------------------------------------------------------*/

    *iret = 0;

    nn = 0;
    found = 0;
    max_grpnum = 0;
    min_grpnum = MAX_EDITABLE_ELEMS;

    for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++ ) {
        if ( range[ii].grptyp == grptyp && range[ii].ioffset >= 0 ) {
	    nn = range[ii].grpnum;
	    if ( nn > max_grpnum ) max_grpnum = nn;
	    if ( nn < min_grpnum ) min_grpnum = nn;
	
	    found++;
	}
    }

    if (found > 0) {
        *high_grpnum = max_grpnum;
        *low_grpnum = min_grpnum;
    }
    else {
        *high_grpnum = 0;
        *low_grpnum = 0;
    }

}
