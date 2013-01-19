#include "ensdiag.h"

void de_rset ( int *iret )
/************************************************************************
 * de_rset								*
 *									*
 * This subroutine resets variables in DGCMN.CMN.  It is called before	*
 * the end of an ensemble function subroutine. This subroutine must be	*
 * called before DG_UDIG.						*
 *									*
 * de_rset ( iret )							*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * T. Lee/SAIC		 1/05						*
 * R. Tian/SAIC		12/05		Added aftrbr, removed igtmsv	*
 * R. Tian/SAIC		 1/06		Translated from Fortran		*
 ************************************************************************/
{
    int zero;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    /*
     * Reset variables in DGCMN.CMN.
     */
    dg_snfl ( &_ensdiag.ndxens,
              _ensdiag.tmplsv,
	      _ensdiag.gpthsv,
	      _ensdiag.flnmsv,
	      _ensdiag.gdtmsv1,
	      _ensdiag.gdtmsv2,
	      &zero,
	      iret );
    dg_cset ( "INGDTM", _ensdiag.igtmsv, iret );

    return;
}
