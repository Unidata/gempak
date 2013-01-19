#include "dg.h"

void dg_esub ( const int *n1, const int *n2, const int *n3,
               const int *n4, int *iret )
/************************************************************************
 * dg_esub								*
 *									*
 * This subroutine frees all internal grids owned by the current	*
 * subroutine identified by ISUBID, assigns ownership of N1, N2, N3,	*
 * and N4 to the subroutine identified by ISUBID-1, and decrements	*
 * ISUBID.								*
 *									*
 * Four input grid numbers are required for the case when vector layer	*
 * grids are passed to the calling subroutine.				*
 *									*
 * Always set Nx=0 for unneeded grid numbers, where x represents 1, 2,	*
 * 3, or 4.  If Nx = 0 for all x, then the action of this subroutine	*
 * is to assign all grids owned by ISUBID to ISUBID-1 and decrement	*
 * ISUBID.								*
 *									*
 * dg_esub ( n1, n2, n3, n4, iret )					*
 *									*
 * Input parameters:							*
 *	*n1		int		Internal grid number 1		*
 *	*n2		int		Internal grid number 2		*
 *	*n3		int		Internal grid number 3		*
 *	*n4		int		Internal grid number 4		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-36 = negative ISUBID		*
 **									*
 * Log:									*
 * K. Brill/HPC		12/01						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int not0, gnum, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    if ( *n1 != 0 || *n2 != 0 || *n3 != 0 || *n4 != 0 ) {
        not0 = G_TRUE;
    } else {
        not0 = G_FALSE;
    }

    if ( not0 == G_TRUE ) {
        for ( gnum = 1; gnum <= _dggrid.maxdgg; gnum++ ) {
            if ( _dggrid.iusesv[gnum-1] == _dggrid.isubid ) {
	        if ( gnum == *n1 || gnum == *n2 ||
	             gnum == *n3 || gnum == *n4 ||
		    _dggrid.savflg[gnum-1] == G_TRUE ) {
	            _dggrid.iusesv[gnum-1] = _dggrid.isubid - 1;
	        } else {
	            dg_frig ( &gnum, &ier );
                }
            }
        }
    } else {
        /*
	 * All input grid numbers are zero.
	 */
        for ( gnum = 1; gnum <= _dggrid.maxdgg; gnum++ ) {
	    if ( _dggrid.dgg[gnum-1].grid && _dggrid.savflg[gnum-1] == G_FALSE )		 dg_frig ( &gnum, &ier );
	}
    }

    _dggrid.isubid--;
    if ( _dggrid.isubid < 0 ) {
	*iret = -36;
    }

    return;
}
