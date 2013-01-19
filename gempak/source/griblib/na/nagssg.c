#include "na.h"

void na_gssg ( const int *igxold, const int *ksbx, const int *ksby,
               float *grid, int *igx, int *igy, int *iret )
/************************************************************************
 * na_gssg								*
 *									*
 * This subroutine subsets a grid using the subset coordinates		*
 * in KSBX and KSBY.  The new grid dimensions are returned in IGX	*
 * and IGY.								*
 *									*
 * na_gssg ( igxold, ksbx, ksby, grid, igx, igy, iret )			*
 *									*
 * Input parameters:							*
 *	*igxold  	const int	Maximum grid size    		*
 *	*ksbx		const int	Subset X coordinates		*
 *	*ksby		const int	Subset Y coordinates		*
 *									*
 * Input and output parameters:						*
 *	*grid		float		Grid of data			*
 *	*igx		int		New X grid dimension		*
 *	*igy		int		New Y grid dimension		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = Normal			*
 *									*
 **									*
 * Log:									*
 * K. Brill/NMC		 4/95						*
 * L. Sager/NMC		 8/95	Added coding to wrap grid boundary	*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 * R. Tian/SAIC		 7/06	Recoded from Fortran			*
 ************************************************************************/
{
    int io, i, j, k, indx;
/*----------------------------------------------------------------------*/
    *iret = 0;

    io = 0;
    for ( j = ksby[0]; j <= ksby[1]; j++ ) {
	for ( i = ksbx[0]; i <= ksbx[1]; i++ ) {
	    k = i;
	    if ( k > *igxold ) k -= *igxold;
	    indx = ( j - 1 ) * (*igx) + k - 1;
	    grid[io++] = grid[indx];
	}
    }

    *igx = ksbx[1] - ksbx[0] + 1;
    *igy = ksby[1] - ksby[0] + 1;

    return;
}
