#include "geminc.h"
#include "gemprm.h"

void grc_mbn2 ( const float *deltan, const int *iebnds, const float *dbnds,
                const float *rnvblk, float *anlblk, int *iret )
/************************************************************************
 * grc_mbn2								*
 *									*
 * This subroutine makes a general Barnes analysis block.  The analysis	*
 * block created is LLNANL words long.  All the bounds must be entered	*
 * in the order:  lower left latitude; lower left longitude; upper	*
 * right latitude; upper right longitude.				*
 *									*
 * grc_mbn2 ( deltan, iebnds, dbnds, rnvblk, anlblk, iret )		*
 *									*
 * Input parameters:							*
 *	*deltan		const float	Station spacing			*
 *	*iebnds		const int	Extended bounds in grid units	*
 *	*dbnds		const float	Data area bounds		*
 *	*rnvblk		const float	Navigation block		*
 *									*
 * Output parameters:							*
 *	*anlblk		float		Analysis block			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -6 = invalid navigation	*
 **									*
 * Log:									*
 * M. desJardins/NMC	 4/91						*
 * K. Brill/NMC		05/91	Add rather than subtract IEXTND to KX,KY*
 * 				Corrected input lat/lon in GSMPRJ for	*
 *				the extend region. Change IER1 to IER in*
 *				summing rc's from setting extend rgn.	*
 * K. Brill/NMC		02/92	Use LLNNAV and LLNANL in documentation	*
 * T. Piper/GSC		11/98	Updated prolog				*
 * R. Tian/SAIC		07/06	Translated from Fortran			*
 ************************************************************************/
{
/*----------------------------------------------------------------------*/

    gr_mbn2 ( deltan, iebnds, dbnds, rnvblk, anlblk, iret );

    return;
}
