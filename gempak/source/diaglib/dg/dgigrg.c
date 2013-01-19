#include "dg.h"

void dg_igrg ( const int *np, const float *ri, const float *rj,
               float *rir, float *rjr, int *iret)
/************************************************************************
 * dg_igrg                                                              *
 *                                                                      *
 * This subroutine transforms internal navigation (RI, RJ) grid		*
 * relative points to reference navigation (RIR, RJR) points.		*
 *                                                                      *
 * dg_igrg ( np, ri, rj, rir, rjr, iret )                               *
 *                                                                      *
 * Input parameters:							*
 *	*np		const int	Number of points		*
 *	*ri		const float	Internal I grid points		*
 *	*rj		const flaot	Internal J grid points		*
 * Output parameters:                                                   *
 *	*rir		float		Reference I grid points		*
 *	*rjr		float		Reference J grid points		*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/02                                           *
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char gprj[5];
    float ag1, ag2, ag3, aglt1, agln1, aglt2, agln2;
    int mx, my, navsz, nc, ier, ier2;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Transform from 'G' to 'M' using current internal grid navigation
     */
    gtrans ( sys_G, sys_M, (int *)np, (float *)ri, (float *)rj, rir, rjr,
             &ier, strlen(sys_G), strlen(sys_M) );
    if ( ier != 0 ) {
	er_wmsg ( "GEMPLT", &ier, " ", &ier2, strlen("GEMPLT"), strlen(" ") );
	*iret = ier;
	return;
    }

    /*
     * Save current internal grid navigation
     */
    gqgprj ( gprj, &ag1, &ag2, &ag3, &mx, &my, &aglt1, &agln1,
	     &aglt2, &agln2, &ier, sizeof(gprj) );
    gprj[4] = '\0';
    cst_lstr ( gprj, &nc, &ier );
    gprj[nc] = '\0';
    if ( ier != 0 ) {
	er_wmsg ( "GEMPLT", &ier, " ", &ier2, strlen("GEMPLT"), strlen(" ") );
	*iret = ier;
	return;
    }

    /*
     * Set the reference grid navigation in GPLT.
     */
    navsz = 13;
    grc_snav ( &navsz, _dgsubg.refnav, &ier );
    if ( ier != 0 ) {
	er_wmsg ( "GEMPLT", &ier, " ", &ier2, strlen("GEMPLT"), strlen(" ") );
	*iret = ier;
	return;
    }

    /*
     * Transform from 'M' to 'G' using reference grid navigation
     */
    gtrans ( sys_M, sys_G, (int *)np, rir, rjr, rir, rjr, &ier,
             strlen(sys_M), strlen(sys_G) );
    if ( ier != 0 ) {
	er_wmsg ( "GEMPLT", &ier, " ", &ier2, strlen("GEMPLT"), strlen(" ") );
	*iret = ier;
	return;
    }

    /*
     * Reset the internal grid navigation in GPLT.
     */
    gsgprj ( gprj, &ag1, &ag2, &ag3, &mx, &my, &aglt1, &agln1,
	     &aglt2, &agln2, &ier, strlen(gprj) );
    if ( ier != 0 ) {
	er_wmsg ( "GEMPLT", &ier, " ", &ier2, strlen("GEMPLT"), strlen(" ") );
	*iret = ier;
	return;
    }

    return;
}
