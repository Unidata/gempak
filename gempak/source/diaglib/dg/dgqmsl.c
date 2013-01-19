#include "dg.h"

void dg_qmsl ( int *ixmscl, int *iymscl, float *gddx, float *gddy,
	       int *iret )
/************************************************************************
 * dg_qmsl								*
 *									*
 * This subroutine retrieves the grid number for map scale factors and	*
 * grid spacing in x, y.						*
 *									*
 * dg_qmsl ( ixmscl, iymscl, gddx, gddy, iret )				*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*ixmscl		int		ixmscl in mapscl.h		*
 *	*iymscl		int		iymscl in mapscl.h		*
 *	*gddx		float		gddx in mapscl.h		*
 *	*gddy		float		gddy in mapscl.h		*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 **									*
 * Log:									*
 * R. Tian/SAIC		 3/06						*
 ************************************************************************/
{
    int nval;
/*----------------------------------------------------------------------*/
    *iret = 0;

    nval = 1;
    dg_iget ( "IXMSCL", &nval, ixmscl, iret );
    dg_iget ( "IYMSCL", &nval, iymscl, iret );
    dg_fget ( "GDDX", &nval, gddx, iret );
    dg_fget ( "GDDY", &nval, gddy, iret );

    return;
}
