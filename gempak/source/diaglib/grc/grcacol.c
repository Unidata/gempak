#include "geminc.h"
#include "gemprm.h"

void grc_acol ( int *kxin, int *kyin, float *grid, int *kxout,
               int *kyout, int *iret )
/************************************************************************
 * grc_acol                                                             *
 *                                                                      *
 * This subroutine adds a column of data to a grid.                     *
 *                                                                      *
 * This is mainly used for adding a column to grids on cylindrical      *
 * projections. The data should be a global grid which does not         *
 * completely wrap around the earth, but is only one grid point short.  *
 * This routine will then repeat the first column of data as the last   *
 * column in order to achieve a complete wrap.                          *
 *                                                                      *
 * grc_acol  ( *kxin, *kyin, grid, kxout, kyout, iret )                 *
 *                                                                      *
 * Input parameters:                                                    *
 *      *kxin           const int       Number of input points in x dir *
 *      *kyin           const int       Number of input points in y dir *
 *                                                                      *
 * Input and output parameters:                                         *
 *      *grid           float           Grid of data                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *kxout          int             Number of output points in x dir*
 *      *kyout          int             Number of output points in y dir*
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * K. Brill/NMC         03/93                                           *
 * S. Jacobs/EAI        11/93           Adapted from GR_RARG            *
 * D.W.Plummer/NCEP	12/05		Translated from FORTRAN		*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/

    gr_acol ( kxin, kyin, grid, kxout, kyout, iret );

    return;
}
