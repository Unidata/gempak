#define DE_GLOBAL
#include "lyrdiag.h"
#undef DE_GLOBAL

void dl_init  ( int *iret )
/************************************************************************
 * dl_init								*
 *									*
 * This function initializes the layer diagnostics package global	*
 * variables declared in dl_variables_prv.h.  This is a one-time        *
 * initialization included in DG_INTL.                          	*
 *									*
 * dl_init  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * T. Lee/SAIC		 3/05						*
 * S. Gilbert/NCEP	12/05	Translation from Fortran		* 
 ************************************************************************/
{
	int		j;

/*----------------------------------------------------------------------*/
	*iret  = 0;

        /*
         *	Initialize layer diagnostic package.
         */
	_lyrdiag.lyrnid = 0;

	for ( j = 0; j < LLMXLV ; j++ ) {
	    _lyrdiag.lyrlvs1[ j ] = 0;
	    _lyrdiag.lyrlvs2[ j ] = 0;
	}

	return;
}
