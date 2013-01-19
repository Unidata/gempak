#include "dg.h"

void dg_scal ( const char *parm, const int *num, int *iret )
/************************************************************************
 * dg_scal								*
 *									*
 * This subroutine scales grids to store them in MKS units.  This	*
 * is necessary for MIXR, SMXR and PSYM since the standard GEMPAK	*
 * units are not MKS units.  Whenever these parameters are returned	*
 * directly to the user, they will be rescaled to standard GEMPAK	*
 * units.								*
 *									*
 * dg_scal ( parm, num, iret )						*
 *									*
 * Input parameters:							*
 *	*parm		const char	Parameter name			*
 *	*num		const int	Number of internal grid		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *				  	  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 5/88						*
 * M. desJardins/NMC	 3/92	Add check for MIXS and SMXS		*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * T. Piper/GSC		11/98	Updated prolog				*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float scale;
    int gidx, i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check that grid number is valid.
     */
    if ( *num <= 0 ) return;

    /*
     * Get scaling factor.
     */
    if ( ( strcmp ( parm, "MIXR" ) == 0 ) ||
         ( strcmp ( parm, "SMXR" ) == 0 ) ||
	 ( strcmp ( parm, "MIXS" ) == 0 ) ||
	 ( strcmp ( parm, "SMXS" ) == 0 ) ) {
	scale = .001;
    } else if ( strcmp ( parm, "PSYM" ) == 0 ) {
	scale = 100.;
    } else {
	return;
    }

    /*
     * Scale the data.
     */
    gidx = (*num) - 1;
    for ( i = _dgarea.ksub1; i <= _dgarea.ksub2; i++ ) {
	if ( ! ERMISS ( _dggrid.dgg[gidx].grid[i-1] ) ) {
	    _dggrid.dgg[gidx].grid[i-1] *= scale;
	}
    }

    return;
}
