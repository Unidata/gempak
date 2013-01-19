#include "dg.h"

void dg_nend ( int *iret )
/************************************************************************
 * dg_nend                                                              *
 *                                                                      *
 * This subroutine makes sure all grid files are closed.		*
 *                                                                      *
 * dg_nend ( iret )							*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04                                           *
 * R. Tian/SAIC		 3/05	Removed setting nucode to false		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int minus1;
/*----------------------------------------------------------------------*/
    *iret = 0;
    minus1 = -1;

    gd_clos ( &minus1, iret );

    return;
}
