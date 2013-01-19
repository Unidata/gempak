#include "lyrdiag.h"

void dl_setl ( int lflag, float *rlev, int *nlev, int *iret )
/************************************************************************
 * dl_setl								*
 *									*
 * This subroutine sets the layer/level data to LYRLVS in               *
 * dl_variables_prv.h.							*
 *									*
 * dl_setl  ( lflag, rlev, nlev, iret )					*
 *									*
 * Input parameters:							*
 *	lflag		int		Layer function flag		*
 *	*rlav [nlev]	float		Vertical level array		*
 *									*
 * Input and output parameters:						*
 *	*nlev		int		Number of levels/layers		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * T. Lee/SAIC		03/05						*
 * S. Gilbert/NCEP	12/05	Translation from Fortran		*
 ************************************************************************/
{
	int		i;
/*----------------------------------------------------------------------*/
	*iret = 0;

	if  ( *nlev <= 0 )  {
	    *iret = -5;
	    return;
	}

	if ( lflag != 0 ) {
	    *nlev = *nlev - 1;
	    for ( i = 0; i < *nlev; i++ ) {
		_lyrdiag.lyrlvs1[ i ] = rint ( rlev [ i ] );
		_lyrdiag.lyrlvs2[ i ] = rint ( rlev [ i + 1 ] );
	    }
	}
	else {
	    for ( i = 0; i < *nlev; i++ ) {
		_lyrdiag.lyrlvs1[ i ] = rint ( rlev [ i ] );
		_lyrdiag.lyrlvs2[ i ] = -1;
	    }
	}

	return;
}
