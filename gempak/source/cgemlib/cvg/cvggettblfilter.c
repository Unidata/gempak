#include "cvgcmn.h"

void cvg_gettblfilter ( int *filnum, filter_t *filter, int *iret )
/************************************************************************
 * cvg_gettblfilter							*
 *									*
 * This function gets all display filters loaded from the filter table.	*
 *									*
 * cvg_gettblfilter ( filnum, filter, iret )				*
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
 * J. Wu/SAIC		06/06   initial coding         			*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    *filnum = nTblFilter;
	    
    for ( ii = 0; ii < nTblFilter; ii++ ) {
	strcpy ( filter[ii], tblFilter[ii] );
    }

}
