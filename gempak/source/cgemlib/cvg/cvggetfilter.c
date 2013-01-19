#include "cvgcmn.h"

void cvg_getfilter ( int *filnum, filter_t *filter, int *iret )
/************************************************************************
 * cvg_getfilter							*
 *									*
 * This function gets the display filters.	 			*
 *									*
 * cvg_getfilter ( filnum, filter, iret )				*
 *									*
 * Input parameters:							*
 *	None								*
 *									*
 * Output parameters:							*
 *	*filnum		int		number of filters		*
 *	*filter		filter_t	filter array			*
 *	*iret		int		Return code			*
 *					 0 = Normal  			*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04   initial coding         			*
 * M. Li/SAIC		08/04	Added a check for no filter		*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    if ( numFilter <= 0 ) {
	strcpy ( filter[0], "ALL" );
    	*filnum = 1;
    }
    else {
    	*filnum = numFilter;
    
    	for ( ii = 0; ii < MAX_FILTER_NUM; ii++ ) {
	    strcpy ( filter[ii], dsplyFilter[ii] );
        }     
    }

}
