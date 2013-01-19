#include "fortran_wrappers.h"

void cgd_rdat ( const int *iacss, const char *gdattm1, const char *gdattm2,
                const int *level1, const int *level2, const int *ivcord,
		const char *parm, float *grid, int *igx, int *igy,
		int *ighdr, int *iret )
/************************************************************************
 * cgd_rdat                                                             *
 *                                                                      *
 * This subroutine reads the requested grid from a grid file.           *
 *                                                                      *
 * cgd_rdat ( iacss, gdattm1, gdattm2, level1, level2, ivcord, parm,	*
 *            grid, igx, igy, ighdr, iret )				* 
 *                                                                      *
 * Input parameters:                                                    *
 *      iacss           int		Grid access number              *
 *      *gdattm1	const char	GEMPAK times                    *
 *      *gdattm2	const char	GEMPAK times                    *
 *      level1		int		Vertical levels                 *
 *      level2		int		Vertical levels                 *
 *      ivcord          int		Vertical coordinate             *
 *                                        0 = NONE                      *
 *                                        1 = PRES                      *
 *                                        2 = THTA                      *
 *                                        3 = HGHT                      *
 *      *parm		const char	Parameter name                  *
 *                                                                      *
 * Output parameters:                                                   *
 *      *grid		floata		Grid data                       *
 *      *igx		int		Number of horizontal points     *
 *      *igy		int		Number of vertical points       *
 *      *ighdr		int		Grid header                     *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                       -4 = file not open             *
 *                                       -6 = read/write error          *
 *                                      -12 = grid does not exist       *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          2/06	C wrapper of GD_RDAT			*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Call gd_rdatw
     */
    gd_rdatw ( iacss, gdattm1, gdattm2, level1, level2, ivcord, parm,
               grid, igx, igy, ighdr, iret );

    return;
}
