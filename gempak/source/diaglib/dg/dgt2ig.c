#include "dg.h"

void dg_t2ig ( const float *tfnav, const int *ighd, float *tgrid,
               float *grid, int *igx, int *igy, int *iret )  
/************************************************************************
 * dg_t2ig                                                              *
 *                                                                      *
 * This subroutine is used to remap input transfer grid to output	*
 * internal grid.							*
 *                                                                      *
 * dg_t2ig ( tfnav, ighd, tgrid, grid, igx, igy, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*tfnav		const float	Input grid navagiation		*
 *	*ighd		const int	Input grid header		*
 *	*tgrid		float		Input grid data			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*grid		float		Output grid data		*
 *	*igx		int		Number of horizontal points	*
 *	*igy		int		Number of vertical points   	*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 5/04						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char proj[5];
    int same, xflag, yflag;
    int navsz, num, i, j, k, jbase, ier, ier1, ier2;
/*----------------------------------------------------------------------*/
    *iret = 0;
    _hintrp.didint = G_FALSE;
    navsz = LLNNAV;

    /*
     * Read transfer navigation.
     */
    grc_rnav ( tfnav, proj, igx, igy, &ier );

    /*
     * Compare transfer navigation with internal navigation.
     */
    grc_cnav ( (float *)tfnav, _dgfile.snav, &navsz, &same, &ier );
    if ( same == G_TRUE ) {
	for ( k = 0; k < (*igx) * (*igy); k++ ) {
	    grid[k] = tgrid[k];
	}
	return;
    }

    /*
     * Compare transfer navigation with reference navigation.
     */
    grc_cnav ( (float *)tfnav, _dgsubg.refnav, &navsz, &same, &ier );
    if ( same == G_TRUE ) {
	/*
	 * If ADDCOL is true, add a column of data.
	 */
	if ( _dgfile.addcol == G_TRUE ) {
	    grc_acol ( igx, igy, tgrid, igx, igy, &ier );
	}

	/*
	 * Rearrange this grid and extract the subset if the DGSUBG
	 * flag is TRUE.
	 */
	if ( _dgsubg.dgsubg == G_TRUE ) {
	    /*
	     * Rearrange the grid if necessary.
	     */
	    if ( _dgsubg.gwrapg == G_TRUE ) {
		grc_dorg ( igx, igy, &_dgsubg.ishift, tgrid, &ier );
	    }

	    /*
	     * Extract the subset.
	     */
	    k = 0;
	    for ( j = _dgsubg.jsgymn; j <=_dgsubg.jsgymx;
	          j += _dgsubg.jsgysk ) {
		jbase = ( j - 1 ) * (*igx);
		for ( i = _dgsubg.jsgxmn; i <= _dgsubg.jsgxmx;
		      i +=_dgsubg.jsgxsk ) {
		    grid[k++] = tgrid[jbase + i - 1];
		}
	    }
	    *igx = _dgfile.kxd;
	    *igy = _dgfile.kyd;
	} else {
	    for ( k = 0; k < (*igx) * (*igy); k++ ) { 
		grid[k] = tgrid[k];
	    }
	}

	return;
    }

    /*
     * Compare transfer navigation with saved transfer navigation.
     */
    grc_cnav ( (float *)tfnav, _hintrp.tfrnav, &navsz, &same, &ier );
    dg_cndg ( "TG_IGRXIG", &_hintrp.igrxig, &xflag, &ier );
    dg_cndg ( "TG_IGRYIG", &_hintrp.igryig, &yflag, &ier );
    if ( same == G_FALSE || xflag == G_FALSE || yflag == G_FALSE ) {
	for ( k = 0; k < navsz; k++ ) {
	    _hintrp.tfrnav[k] = tfnav[k];
	}
	_hintrp.adcltg = G_FALSE;
	_hintrp.gwrptg = G_FALSE;

	if ( G_DIFF(_dgfile.snav[1], _hintrp.tfrnav[1]) &&
	    G_ABS ( _dgfile.snav[10] - _hintrp.tfrnav[10] ) < 1.E-5 &&
	    G_ABS ( _dgfile.snav[11] - _hintrp.tfrnav[11] ) < 1.E-5 &&
	    G_ABS ( _dgfile.snav[12] - _hintrp.tfrnav[12] ) < 1.E-5 ) {
	    _hintrp.wndrot = G_FALSE;
	} else {
	    _hintrp.wndrot = G_TRUE;
	}

	/*
	 * Transform the lat/lon of all points on internal grid to
	 * grid relative positions on transfer grid.
	 */
	dg_ltln ( &ier );

	/*
	 * Convert lat/lons to degrees.
	 */
	for ( k = 0; k < _dgfile.kxyd; k++ ) {
	    if ( ! ERMISS ( _dggrid.dgg[_dgfile.idglat-1].grid[k] ) )
		_dggrid.dgg[_dgfile.idglat-1].grid[k] *= RTD;
	    if ( ! ERMISS ( _dggrid.dgg[_dgfile.idglon-1].grid[k] ) )
		_dggrid.dgg[_dgfile.idglon-1].grid[k] *= RTD;
	}
	grc_snav ( &navsz, _hintrp.tfrnav, &ier );
	dg_tadc ( &ier );
	gtrans ( sys_M, sys_G, &_dgfile.kxyd,
	    _dggrid.dgg[_dgfile.idglat-1].grid,
	    _dggrid.dgg[_dgfile.idglon-1].grid,
	    _dggrid.dgg[_hintrp.igrxig-1].grid,
	    _dggrid.dgg[_hintrp.igryig-1].grid, &ier,
	    strlen(sys_M), strlen(sys_G) );
	grc_snav ( &navsz, _dgfile.snav, &ier );

	/*
	 * Convert lat/lons to radians.
	 */
	for ( k = 0; k < _dgfile.kxyd; k++ ) {
	    if ( ! ERMISS ( _dggrid.dgg[_dgfile.idglat-1].grid[k] ) )
		_dggrid.dgg[_dgfile.idglat-1].grid[k] *= DTR;
	    if ( ! ERMISS ( _dggrid.dgg[_dgfile.idglon-1].grid[k] ) )
		_dggrid.dgg[_dgfile.idglon-1].grid[k] *= DTR;
	}
    }

    /*
     * If ADCLTG is true, add a column of data.
     */
    if ( _hintrp.adcltg == G_TRUE ) {
	grc_acol ( igx, igy, tgrid, igx, igy, &ier );
    }

    /*
     * Remap the transfer grid to internal grid.
     */
    if ( ighd[1] == 1 )  {
	/*
	 * Transfer grid is a grid of direction. Convert to radians
	 * if the direction is degrees.
	 */
	for ( k = 0; k < (*igx) * (*igy); k++ ) {
	    if ( ! ERMISS ( tgrid[k] ) ) {
		tgrid[k] *= DTR;
	    }
	}

	/*
	 * Get a temporary storage from the internal grids.
	 */
	dg_nxts ( &num, &ier );

	/*
	 * Remap sines and cosines of directions.
	 */
	dg_g2gs ( &ighd[0], &_hintrp.gwrptg, igx, igy, tgrid,
	    &_dgfile.kxd, &_dgfile.kyd, _dggrid.dgg[_hintrp.igrxig-1].grid,
	    _dggrid.dgg[_hintrp.igryig-1].grid, grid, &ier1 );
	dg_g2gc ( &ighd[0], &_hintrp.gwrptg, igx, igy, tgrid,
	    &_dgfile.kxd, &_dgfile.kyd, _dggrid.dgg[_hintrp.igrxig-1].grid,
	    _dggrid.dgg[_hintrp.igryig-1].grid, _dggrid.dgg[num-1].grid, &ier2 );
	if ( ier1 != 0 || ier2 != 0 ) {
	    *iret = -68;
	    return;
	}

	/*
	 * Compute directions from sines and cosines.
	 */
	for ( k = 0; k < _dgfile.kxyd; k++ ) {
	    if ( ERMISS ( grid[k] ) || ERMISS ( _dggrid.dgg[num-1].grid[k] ) ) {
		grid[k] = RMISSD;
	    } else {
		grid[k] = atan2 ( grid[k], _dggrid.dgg[num-1].grid[k] ) + TWOPI;
		if ( grid[k] >= TWOPI ) grid[k] -= TWOPI;
	    }
	}

	/*
	 * Convert radians to degrees.
	 */
	for ( k = 0; k < _dgfile.kxyd; k++ ) {
	    if ( ! ERMISS ( grid[k] ) ) grid[k] *= RTD;
	}

	/*
	 * Free temporary storage from the internal grids.
	 */
	dg_frig ( &num, &ier );
    }
    else {
	/*
	 * Transfer grid is not a grid of direction.
	 */
	dg_g2gi ( &ighd[0], &_hintrp.gwrptg, igx, igy, tgrid,
	    &_dgfile.kxd, &_dgfile.kyd, _dggrid.dgg[_hintrp.igrxig-1].grid,
	    _dggrid.dgg[_hintrp.igryig-1].grid, grid, &ier );
	if ( ier != 0 ) {
	    *iret = -68;
	    return;
	}
    }

    *igx = _dgfile.kxd;
    *igy = _dgfile.kyd;
    _hintrp.didint = G_TRUE;

    return;
}
