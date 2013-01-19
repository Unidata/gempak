#include "dg.h"

void dg_qbnd ( int *jgxmin, int *jgxmax, int *jgymin, int *jgymax,
	       int *iret )
/************************************************************************
 * dg_qbnd								*
 *									*
 * This subroutine retrieves the full area bounds.			*
 *									*
 * dg_qbnd ( jgxmin, jgxmax, jgymin, jgymax iret )			*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*jgxmin		int		jgxmin in dgarea.h		*	
 *	*jgxmax		int		jgxmax in dgarea.h		*
 *	*jgymin		int		jgymin in dgarea.h		*
 *	*jgymax		int		jgymax in dgarea.h		*
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
    dg_iget ( "JGXMIN", &nval, jgxmin, iret );
    dg_iget ( "JGXMAX", &nval, jgxmax, iret );
    dg_iget ( "JGYMIN", &nval, jgymin, iret );
    dg_iget ( "JGYMAX", &nval, jgymax, iret );

    return;
}
