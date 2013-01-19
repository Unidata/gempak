#include "dg.h"

void dg_cola ( int *mcnt, float *x, float *y, float *v, int *keep,
               int *iret ) 
/************************************************************************
 * dg_cola								*
 *									*
 * This subroutine discards all rows of the triple array x/y/v where 	*
 * KEEP is false.							*
 *									*
 * dg_cola ( mcnt, x, y, v, keep, iret )				*
 *									*
 * Input and Output parameters:						*
 *	*mcnt		int		Total # of elements in arrays	*
 *	*x		float		Grid columns			*
 *	*y		float		Grid rows			*
 *	*v		float		Grid values			*
 *	*keep		int		Storage for internal flags	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 **									*
 * Log:									*
 * D. Keiser/GSC	11/95						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int itotal, if1, if2, iarrix;
/*----------------------------------------------------------------------*/
    *iret = 0 ;
    itotal = *mcnt;
    if1 = 1;
    if2 = 0;

    while ( ( if1 + if2 ) <= (*mcnt) ) {
	/*
	 * If point flagged for removal, move array up one position.
	 */
	if ( keep[if1-1] == G_FALSE ) {
	    for ( iarrix = if1 - 1; iarrix < (*mcnt) - 1; iarrix++ ) {
	        x[iarrix]    = x[iarrix+1];
	        y[iarrix]    = y[iarrix+1];
	        v[iarrix]    = v[iarrix+1];
	        keep[iarrix] = keep[iarrix+1];
	    }

	    /*
	     * Update count and last entry. 
	     */
	    keep[(*mcnt)-1] = G_FALSE;
	    itotal--;

	    /*
	     * Increment number of points kept.
	     */
	    if2++;
	} else {
	    /*
	     * Increment number of points not removed. 
	     */
	    if1++;
	}
    }

    /*
     * Reset the total number of extrema.
     */
    *mcnt = itotal;

    return;
}
