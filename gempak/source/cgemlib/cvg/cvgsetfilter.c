#include "cvgcmn.h"

void cvg_setfilter ( char *filter, int *iret )
/************************************************************************
 * cvg_setfilter							*
 *									*
 * This function sets new dsiplay filters.				*
 *									*
 * cvg_setfilter ( filter, iret )					*
 *									*
 * Input parameters:							*
 *	*filter		char		filter string			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 0 = Normal  			*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/04   initial coding         			*
 * M. Li/SAIC		08/04	Added a check for no filter		*
 ***********************************************************************/
{
    int		last, ii;
    char	*ptr;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *  Wipe out the previous filter settings first.
     */
    numFilter = 0;
    for ( ii = 0; ii < MAX_FILTER_NUM; ii++ ) {
	dsplyFilter[ii][0] = '\0';
    }	    

    
    /*
     *  Set the new filter settings.
     */
    ptr = strtok ( filter, ";" );
    last = DSPLY_FILTER_SZ - 1;
    while ( (ptr != (char *)NULL) && ( numFilter < MAX_FILTER_NUM ) ) {
	
	strncpy ( dsplyFilter[numFilter], ptr, (size_t)last );
	dsplyFilter[numFilter][last] = '\0';

	numFilter++;
	
	ptr = strtok ( NULL, ";" );
		
    }	

    /*
     * If no filter is found, set to ALL.
     */
    if ( numFilter == 0 ) {
	strcpy ( dsplyFilter[0], "ALL" );
	numFilter = 1;
    }

}
