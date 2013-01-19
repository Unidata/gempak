#include "fortran_wrappers.h"

void cgd_wppg ( const int *iacss, const int *igrid, const int *lengrd,
                const int *igx, const int *igy, const int *ighdr,
                const char *gdattm1, const char *gdattm2,
		const int *level1, const int *level2, const int *ivcord,
                const char *parm, const int *rewrit, const int *ipktyp,
		const int *nbits, const int *misflg, const float *ref,
                const float *scale, const float *difmin, int *iret )
/************************************************************************
 * cgd_wppg 								*
 *									*
 * This subroutine writes a grid that is already packed to a grid	*
 * file.  IPKTYP should be one of the following parameter names:	*
 *         MDGGRB        Packed in GEMPAK GRIB format			*
 *                         REF    = minimum value			*
 *                         SCALE  = 2 ** N				*
 *         MDGNMC        Packed in NMC format				*
 *                         REF    = average value			*
 *                         SCALE  = 1 / 2 ** N				*
 *         MDGDIF        Packed in GEMPAK DIF format			*
 *                         REF    = first non-missing point in grid	*
 *                         SCALE  = scaling term for differences	*
 *                         DIFMIN = minimum value of difference field	*
 *									*
 * cgd_wppg ( iacss, igrid, lengrd, igx, igy, ighdr, gdattm1, gdattm2,	*
 *            level1, level2, ivcord, parm, rewrit, ipktyp, nbits,	*
 *            misflg, ref, scale, difmin, iret )			*
 *									*
 * Input parameters:							*
 *	*iacss 		const int	Grid access number		*
 *	*igrid		const int	Packed grid data		*
 *	*lengrd		const int	Number of 32-bit words in grid	*
 *	*igx		const int	Number of horizontal points	*
 *	*igy		const int	Number of vertical points 	*
 *	*ighdr		const int	Grid header			*
 *	*gdattm1   	const char	GEMPAK times			*
 *	*gdattm2   	const char	GEMPAK times			*
 *	*level1    	const int	Vertical levels			*
 *	*level2    	const int	Vertical levels			*
 *	*ivcord		const int	Vertical coordinate		*
 *				  	   0 = NONE			*
 *				  	   1 = PRES			*
 *					   2 = THTA			*
 *					   3 = HGHT			*
 *	*parm		const char	Parameter name			*
 *	*rewrit		const int	Flag to replace existing grid	*
 *	*ipktyp		const int	Packing type			*
 *	*nbits		const int	Number of bits 			*
 *	*misflg		const int	Missing data flag		*
 *	*ref		const float	Reference value			*
 *	*scale		const float	Scaling factor			*
 *	*difmin		const float	DIF reference value		*
 *									*
 * Output parameters:							*
 *	*IRET		int		Return code			*
 *					  0 = normal return		*
 *					 -4 = file not open		*
 *					 -5 = no write access		*
 *					 -6 = read/ write error		*
 *					 -9 = invalid grid size		*
 *					-10 = grid already exists	*
 *					-11 = grid file is full		*
 **									*
 * Log:									*
 * R. Tian/SAIC          8/06						*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/
    gd_wppgw ( iacss, igrid, lengrd, igx, igy, ighdr, gdattm1, gdattm2,
               level1, level2, ivcord, parm, rewrit, ipktyp, nbits,
               misflg, ref, scale, difmin, iret );

    return;
}
