#include "ensdiag.h"

void de_save ( const int *indx, int *iret )
/************************************************************************
 * de_save								*
 *									*
 * This subroutine save variables in DGCMN.CMN.  It is called before	*
 * the an ensemble function subroutine. 				*
 *									*
 * de_save ( indx, iret )						*
 *									*
 * Input parameters:							*
 *	*indx		const int	Index points to current ensemble*
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
    int mbrnum;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Save variables in DGCMN.CMN.
     */
    dg_gnfl ( indx, _ensdiag.tmplsv, _ensdiag.gpthsv, _ensdiag.flnmsv,
              _ensdiag.gdtmsv1, _ensdiag.gdtmsv2, &mbrnum, iret );
    dg_cget ( "INGDTM", _ensdiag.igtmsv, iret );

    return;
}
