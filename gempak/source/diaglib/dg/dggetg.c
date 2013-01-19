#include "dg.h"

void dg_getg ( const int *num, float **grid, int *kxd, int *kyd,
	       int *ksub1, int *ksub2, int *iret )
/************************************************************************
 * dg_getg                                                              *
 *                                                                      *
 * This subroutine returns the grid for a given	grid number.		*
 *                                                                      *
 * dg_getg ( num, grid, kxd, kyd, ksub1, ksub2, iret )			*
 *                                                                      *
 * Input parameters:                                                   	*
 *	*num		const int	Grid number			*
 *                                                                      *
 * Output parameters:                                                   *
 *	**grid		float		Grid returned			*
 *	*kxd		int		Grid X dimension		*
 *	*kyd		int		Gird Y dimension		*
 *	*ksub1		int		Starting point			*
 *	*ksub2		int		Ending point			*
 *      *iret           int             Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         10/05						*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    *grid = _dggrid.dgg[(*num)-1].grid;
    *kxd = _dgfile.kxd;
    *kyd = _dgfile.kyd;
    *ksub1 = _dgarea.ksub1;
    *ksub2 = _dgarea.ksub2;

    return;
}
