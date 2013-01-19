#include "dg.h"

void dg_lncr ( int *iret )
/************************************************************************
 * dg_lncr                                                              *
 *                                                                      *
 * This function reads in the raw land-sea array.			*
 *                                                                      *
 * dg_lncr ( iret )							*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 7/98						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char buff[81];
    static int dglncr = G_FALSE;
    int n, ier;
    FILE *fp;
/*----------------------------------------------------------------------*/
    *iret = 0;

    if ( dglncr == G_TRUE ) return;

    fp = cfl_tbop ( "landsea.tbl", "grid", &ier );
    if ( ier != 0 ) {
	er_wmsg ( "FL", &ier, " ", &ier, strlen("CFL"), strlen(" ") );
	*iret = -1;
	return;
    }

    n = 0;
    while ( n < 8135 ) {
	cfl_trln ( fp, sizeof(buff), buff, &ier );
	sscanf ( buff, "%x %x %x %x %x %x",
	    &_dglndc.ls[n],   &_dglndc.ls[n+1], &_dglndc.ls[n+2],
	    &_dglndc.ls[n+3], &_dglndc.ls[n+4], &_dglndc.ls[n+5] );
	n += 6;
    }

    dglncr = G_TRUE;

    return;
}
