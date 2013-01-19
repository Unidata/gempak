#include "crgcmn.h"


void crg_ggnel ( char grptyp, int grpnum, int *nelm, int *iret )
/************************************************************************
 * crg_ggnel                                                          	*
 *                                                                      *
 * This function returns the total number of elements in the specified  *
 * group.								*
 *                                                                      *
 * crg_ggnel ( grptyp, grpnum, nelm, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	grptyp		char		Group type			*
 *	grpnum		int		Group number			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*nelm		int		number of elements in the group	*
 *      *iret           int             Return code (always 0)          *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI         4/98						*
 * E. Safford/SAIC	04/02	verify offset is good (positive value)  *
 ***********************************************************************/
{
int ii, n;
/*---------------------------------------------------------------------*/

    *iret = 0;
  
    n = 0;
    for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++ ) {
	if ( range[ii].grptyp == grptyp &&
		range[ii].grpnum == grpnum && range[ii].ioffset >= 0 ) {
	    n++;
	}
    }

    *nelm = n;

}
