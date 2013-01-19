#include "ensdiag.h"

void de_mset  ( const int *imem, int *iret )
/************************************************************************
 * de_mset								*
 *									*
 * This subroutine initializes DGCMN elements for ensemble member 	*
 * number imem.								*
 *									*
 * de_mset ( imem, iret )						*
 *									*
 * Input parameters:							*
 *	*imem		const int	Ensemble member number		*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * T. Lee/SAIC		 1/05						*
 * R. Tian/SAIC		12/05		Added aftrbr, removed ingdtm	*
 * R. Tian/SAIC		 1/06		Translated from Fortran		*
 ************************************************************************/
{
    int mbrnum;
/*----------------------------------------------------------------------*/
    *iret = 0;
    mbrnum = (*imem) + 1;

    /*
     * Initialize GDFILE common block elements in DGCMN.
     */
    dg_snfl ( &_ensdiag.ndxens,
              _ensdiag.etmplt[*imem],
	      _ensdiag.enspth[*imem],
	      _ensdiag.ensfnm[*imem],
	      "",
	      "",
	      &mbrnum,
	      iret );
    dg_cset ( "INGDTM", _ensdiag.etimes[*imem], iret ); 

    return;
}
