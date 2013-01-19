#include "crgcmn.h"

void crg_gginx ( char grptyp, int grpnum, int nexp, int *inxarry, 
						int *nelm, int *iret )
/************************************************************************
 * crg_gginx                                                          	*
 *                                                                      *
 * This function returns an array of indices to the range record of all *
 * the elements that belong to the specified group. 			*
 *                                                                      *
 * crg_gginx ( grptyp, grpnum, nexp, inxarry, nelm, iret ) 		*
 *                                                                      *
 * Input parameters:                                                    *
 *	grptyp		char   Group type				*
 *	grpnum		int    Group number				*
 *	nexp		int    number of element to be expected		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*inxarry	int    index array to the range records		*
 * 	*nelm		int    number of elements in the array		*
 *      *iret           int    Return code (always 0)          		*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            4/98						*
 * E. Safford/SAIC	04/02	do not return deleted elements		*
 ***********************************************************************/
{
int ii, n;
/*---------------------------------------------------------------------*/

    *iret = 0;
  
    n = 0;
    for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++ ) {
	
	if ( range[ii].grptyp == grptyp && range[ii].grpnum == grpnum && 
		range[ii].ioffset >= 0 ) {
	    inxarry[n] = ii;
	    n++;
	}

	if ( n >= nexp ) 
		break;

    }

    *nelm = n;

}
