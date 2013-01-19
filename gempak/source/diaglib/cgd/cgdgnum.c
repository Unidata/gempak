#include "fortran_wrappers.h"

void cgd_gnum ( const int *iacss, const char *gdattm1, const char *gdattm2,
	      	const int *level1, const int *level2, const int *ivcord,
		const char *parm, int *ignum, int *iret )
/************************************************************************
 * cgd_gnum								*
 *									*
 * This subroutine gets the grid number for the requested grid.		*
 *									*
 * cgd_gnum  ( iacss, gdattm1, gdattm2, level1, level2, ivcord, parm,	*
 *             ignum, iret )						*
 *									*
 * Input parameters:							*
 *	*iacss 		const int	File acess number		*
 *	*gdattm1	const char	GEMPAK times			*
 *	*gdattm2	const char	GEMPAK times			*
 *	*level1		const int	Vertical levels			*
 *	*level2		const int	Vertical levels			*
 *	*ivcord		const int	Vertical coordinate		*
 *					  0 = NONE			*
 *					  1 = PRES			*
 *					  2 = THTA			*
 *					  3 = HGHT			*
 *	*parm		const char	Parameter name			*
 *									*
 * Output parameters:							*
 *	*ignum		int		Grid number			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -4 = file not open		*
 *					 -6 = read/write error		*
 *					-12 = grid does not exist	*
 **									*
 * Log:									*
 * R. Tian/SAIC          3/06						*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    gd_gnumw ( iacss, gdattm1, gdattm2, level1, level2, ivcord, parm,
    	       ignum, iret );

    return;
}
