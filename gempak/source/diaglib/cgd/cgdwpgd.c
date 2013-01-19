#include "fortran_wrappers.h"

void cgd_wpgd ( const int *iacss, const float *grid, const int *igx,
		const int *igy, const int *ighdr, const char *gdattm1,
		const char *gdattm2, const int *level1, const int *level2,
		const int *ivcord, const char *parm, const int *rewrit,
		const int *ipktyp, const int *nbits, int *iret )
/************************************************************************
 * cgd_wpgd								*
 *									*
 * This subroutine packs an input grid of real values and writes it	*
 * to a grid file.  IPKTYP should be one of the following parameter	*
 * names from GEMPRM.PRM:						*
 *									*
 *         MDGNON        No grid packing				*
 *         MDGGRB        Pack in GEMPAK GRIB format given nbits		*
 *         MDGDEC        Pack in GEMPAK GRIB format given precision	*
 *         MDGDIF        Pack in GEMPAK DIF format given nbits		*
 *									*
 * If the packing type is MDGNON, the real data will be stored as if	*
 * GD_WDAT were called.  If MDGGRB or MDGDIF is specified, the		*
 * number of bits given in NBITS will be used to store the data.	*
 * For packing type MDGDEC, NBITS is the precision.  The grid data	*
 * is multiplied by 10 ** NBITS and rounded to the nearest integer.	*
 * The actual number of bits used to store the data is the minimum	*
 * number required to store the resulting integers.			*
 *									*
 * cgd_wpgd  ( iacss, grid, igx, igy, ighdr, gdattm1, gdattm2, level1,	*
 *	       level2, ivcord,parm, rewrit, ipktyp, nbits, iret )	*
 *									*
 * Input parameters:							*
 *	*iacss 		const int	Grid access number		*
 *	*grid		const float	Grid data			*
 *	*igx		const int	Number of horizontal points	*
 *	*igy		const int	Number of vertical points 	*
 *	*ighdr		const int	Grid header			*
 *	*gdattm1	const char	GEMPAK times			*
 *	*gdattm2	const char	GEMPAK times			*
 *	*level1		const int	Vertical levels			*
 *	*level2		const int	Vertical levels			*
 *	*ivcord		const int	Vertical coordinate		*
 *				  	   0 = NONE			*
 *				  	   1 = PRES			*
 *					   2 = THTA			*
 *					   3 = HGHT			*
 *	*parm		const char	Parameter name			*
 *	*rewrit		const int	Flag to replace existing grid	*
 *	*ipktyp		const int	Packing type			*
 *	*nbits		const int	Number of bits / precision	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -4 = file not open		*
 *					 -5 = no write access		*
 *					 -6 = read/ write error		*
 *					 -9 = invalid grid size		*
 *					-10 = grid already exists	*
 *					-11 = grid file is full		*
 **									*
 * Log:									*
 * R. Tian/SAIC          3/06						*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    gd_wpgdw ( iacss, grid, igx, igy, ighdr, gdattm1, gdattm2, level1,
    	       level2, ivcord, parm, rewrit, ipktyp, nbits, iret );

    return;
}
