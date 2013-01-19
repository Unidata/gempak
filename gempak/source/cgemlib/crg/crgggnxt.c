#include "crgcmn.h"


void crg_ggnxt ( char grptyp, int *grpnum, int *iret )
/************************************************************************
 * crg_ggnxt                                                          	*
 *                                                                      *
 * This function returns the next group number to be used for the       *
 * specified group. 							*
 *                                                                      *
 * crg_ggnxt ( grptyp, grpnum, iret ) 					*
 *                                                                      *
 * Input parameters:                                                    *
 * 	grptyp		char		group type			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*grpnum		int		next group number		*
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI         4/98						*
 * M. Li/SAIC	      4/02	Call crg_ggnhl				*
 ***********************************************************************/
{
int high_grpnum, low_grpnum, ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    crg_ggnhl (grptyp, &high_grpnum, &low_grpnum, &ier);

    high_grpnum++;
    *grpnum = high_grpnum ;

}
