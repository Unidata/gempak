#include "dg.h"

void dg_tadc ( int *iret )
/************************************************************************
 * dg_tadc                                                              *
 *                                                                      *
 * This subroutine determines if an added column is required for the	*
 * transfer navigation.							*
 *                                                                      *
 * This subroutine sets the adcltg and gwrptg flags in the HINTRP block *
 * of DGCMN.CMN								*
 *                                                                      *
 * The transfer navigation is assumed to be set in GPLT.		*
 *                                                                      *
 * dg_tadc ( iret ) 		 		                        *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * K. Brill/HPC		 3/04	Created from DG_ADCL			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 * K. Brill/HPC         11/11   Remove check for exceeding LLMXTG	*
 ************************************************************************/
{
    char gprj[5];
    float rgx[2], rgy[2];
    int mx, my, two, ier, ier2;
/*----------------------------------------------------------------------*/
    *iret   = 0;
    two = 2;

    _hintrp.gwrptg = G_FALSE;
    _hintrp.adcltg = G_FALSE;
    grc_rnav ( _hintrp.tfrnav, gprj, &mx, &my, &ier );
    if ( strcmp ( gprj, "MER" ) == 0 || strcmp ( gprj, "CED" ) == 0 ) {
/*	if ( ( my * (mx+1) ) > LLMXTG ) return; */
	rgx[0] = 1.;
	rgy[0] = 1.;
	rgx[1] = (float)( mx + 1 );
	rgy[1] = 1.;
	gtrans ( sys_G, sys_M, &two, rgx, rgy, rgx, rgy, &ier,
	         strlen(sys_G), strlen(sys_M) );
	if ( G_ABS ( rgy[0] - rgy[1] ) < 0.005 ||
	    ( G_ABS ( rgy[0] + 180. ) < 0.005 &&
	      G_ABS ( rgy[1] - 180. ) < 0.005 ) ) {
	    mx += 1;
	    gsgprj ( gprj, &_hintrp.tfrnav[10], &_hintrp.tfrnav[11],
		&_hintrp.tfrnav[12], &mx, &my, &_hintrp.tfrnav[6],
		&_hintrp.tfrnav[7], &_hintrp.tfrnav[8], &_hintrp.tfrnav[7],
		&ier, strlen(gprj) );
	    if ( ier != 0 ) {
		er_wmsg ( "GEMPLT", &ier, " ", &ier2,
		    strlen("GEMPLT"), strlen(" " ) );
		*iret = -7;
	    }
	    _hintrp.adcltg = G_TRUE;
	    _hintrp.gwrptg = G_TRUE;
	    return;
	} else {
	    _hintrp.adcltg = G_FALSE;
	}
    }

    if ( ( strcmp ( gprj, "MER" ) == 0 ) || ( strcmp ( gprj, "MCD" ) == 0 ) ||
	 ( strcmp ( gprj, "CED" ) == 0 ) ) {
	/*
	 * Set GWRAPG flag for globe wrapping grid.
	 */
	rgx[0] = 1.;
	rgy[0] = 1.;
	rgx[1] = (float)mx;
	rgy[1] = 1.;
	gtrans ( sys_G, sys_M, &two, rgx, rgy, rgx, rgy, &ier,
	         strlen(sys_G), strlen(sys_M) );
	if ( G_ABS ( rgy[0] - rgy[1] ) < 0.005 ||
	    ( G_ABS ( rgy[0] + 180. ) < 0.005 &&
	      G_ABS ( rgy[1] - 180. ) < 0.005 ) )  _hintrp.gwrptg = G_TRUE;
    }

    return;
}
