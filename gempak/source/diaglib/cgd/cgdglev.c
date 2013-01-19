#include "fortran_wrappers.h"

void cgd_glev ( const int *iacss, const char *gdattm1, const char *gdattm2,
	        const int *ivcord, const int *maxlev, int *levarr1,
		int *levarr2, int *nlev, int *iret )
/************************************************************************
 * cgd_glev								*
 *									*
 * This subroutine returns all the levels present in a grid file for	*
 * a given date and vertical coordinate.  The levels returned are	*
 * not sorted.								*
 *									*
 * cgd_glev ( iacss, gdattm1, gdattm2, ivcord, maxlev, levarr1, levarr2,*
 *            nlev, iret )						*
 *									*
 * Input parameters:							*
 *	*iacss		const int	Grid access number		*
 *	*gdattm1	const char	GEMPAK times			*
 *	*gdattm2	const char	GEMPAK times			*
 *	*ivcord		const int	Vertical coordinate		*
 *	*maxlev		const int	Maximum number of levels	*
 *									*
 * Output parameters:							*
 *	*levarr1	int		Levels found			*
 *	*levarr2	int		Levels found			*
 *	*nlev		int		Number of levels found		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -4 = file not open		*
 *					 -6 = read/write error		*
 **									*
 * Log:									*
 * R. Tian/SAIC		 3/06	C wrapper of GD_GLEV			*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    gd_glevw ( iacss, gdattm1, gdattm2, ivcord, maxlev, levarr1, levarr2,
               nlev, iret );

    return;
}
