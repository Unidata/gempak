#include "dg.h"

void dg_adcl ( int *iret )
/************************************************************************
 * dg_adcl                                                              *
 *                                                                      *
 * This subroutine adds a column to a grid.                             *
 *                                                                      *
 * dg_adcl ( iret )  				                        *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         10/03                                           *
 * K. Brill/HPC		 2/04	Initialize gwrapg and addcol		*
 * R. Tian/SAIC		 5/04	Removed check for addcol		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 * K. Brill/HPC         11/11   Remove check for exceeding LLMXTG	*
 ************************************************************************/
{
    float rgx[2], rgy[2];
    int np, ier, ier2;
/*----------------------------------------------------------------------*/
    *iret   = 0;

    _dgsubg.gwrapg = G_FALSE;
    _dgfile.addcol = G_FALSE;
    grc_rnav ( _dgsubg.refnav, _dgfile.cprj, &_dgfile.kxd, &_dgfile.kyd, &ier );
    if ( ( strcmp ( _dgfile.cprj, "MER" ) == 0 ) || 
         ( strcmp ( _dgfile.cprj, "CED" ) == 0 ) ) {
/*	if ( ( _dgfile.kyd * (_dgfile.kxd+1) ) > LLMXTG ) return; */
	rgx[0] = 1.;
	rgy[0] = 1.;
	rgx[1] = (float)( _dgfile.kxd + 1 );
	rgy[1] = 1.;
	np = 2;
	gtrans ( sys_G, sys_M, &np, rgx, rgy, rgx, rgy, &ier,
	    strlen(sys_G), strlen(sys_M) );
	if ( G_ABS ( rgy[0] - rgy[1] ) < 0.01 ||
	    ( G_ABS ( rgy[0] + 180. ) < 0.01 &&
	      G_ABS ( rgy[1] - 180. ) < 0.01 ) ) {
	    _dgfile.kxd += 1;
	    _dgfile.kxyd = _dgfile.kxd * _dgfile.kyd;
	    _dggrid.maxdgg = NDGRD;
	    gsgprj ( _dgfile.cprj, &_dgsubg.refnav[10], &_dgsubg.refnav[11], 
	        &_dgsubg.refnav[12], &_dgfile.kxd, &_dgfile.kyd,
		&_dgsubg.refnav[6], &_dgsubg.refnav[7], &_dgsubg.refnav[8],
		&_dgsubg.refnav[7], &ier, strlen(_dgfile.cprj) );
	    if ( ier != 0 ) {
		er_wmsg ( "GEMPLT", &ier, " ", &ier2,
		    strlen("GEMPLT"), strlen(" ") );
		*iret = -7;
	    }
	    _dgfile.addcol = G_TRUE;

	    /*
	     * Free all existing grids since grid size is changed.
	     */
	    dg_fall ( &ier );
	}
    } else {
	_dgfile.addcol = G_FALSE;
    }

    if ( ( strcmp ( _dgfile.cprj, "MER" ) == 0 ) ||
         ( strcmp ( _dgfile.cprj, "MCD" ) == 0 ) ||
    	 ( strcmp ( _dgfile.cprj, "CED" ) == 0 ) ) {
	/*
	 * Set GWRAPG flag for globe wrapping grid.
	 */
	rgx[0] = 1.;
	rgy[0] = 1.;
	rgx[1] = (float)_dgfile.kxd;
	rgy[1] = 1.;
	np = 2;
	gtrans ( sys_G, sys_M, &np, rgx, rgy, rgx, rgy, &ier,
	    strlen(sys_G), strlen(sys_M) );
	if ( G_ABS ( rgy[0] - rgy[1] ) < 0.01 ||
	    ( G_ABS ( rgy[0] + 180. ) < 0.01 &&
	      G_ABS ( rgy[1] - 180. ) < 0.01 ) ) {
	    _dgsubg.gwrapg = G_TRUE;
	}
    }

    return;
}
