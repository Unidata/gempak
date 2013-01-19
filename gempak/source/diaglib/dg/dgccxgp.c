#include "dg.h"

void dgc_cxgp ( const char *uipnts, const char *ijskip, const int *npmx,
                int *np, float *rgx, float *rgy, float *rlat, float *rlon,
	        int *iret )
/************************************************************************
 * dgc_cxgp                                                             *
 *                                                                      *
 * This subroutine translates the user input for a grid point or for	*
 * the end points of a cross-section line through a grid into actual	*
 * grid point(s), x and y coordinates, and latitude and longitude.  	*
 *                                                                      *
 * dgc_cxgp ( uipnts, ijskip, npmx, np, rgx, rgy, rlat, rlon, iret )   	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *uipnts		const char	User input for grid point       *
 *	*ijskip		const char	User input for grid skipping	*
 *	*npmx		const int	Max allowed value for NP	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*np		int		Number of output points		*
 *      *rgx		float		X grid point                    *
 *      *rgy		float		Y grid point                    *
 *      *rlat		float		Latitude                        *
 *      *rlon		float		Longitude                       *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -45 = blank uipnts	        *
 *                                      -46 = invalid grid point	*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/02						*	
 * m.gamazaychikov/SAIC 11/02		added calls to ER_WMSG          *
 * K. Brill/HPC		12/02		Added IJSKIP			*
 * R. Tian/SAIC		 2/06		Recoded from Fortran		*
 * S. Gilbert/NCEP	 5/07		Added maxgrid to dgc_subg	*
 * S. Gilbert/NCEP	 8/07		Set maxgrid to IMISSD		*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    char *ui, **cxstns, gprj[5], gpoint[LLMXLN+1], slt[2][21], sln[2][21];
    float rx[2], ry[2], rlt[2], rln[2], agln2, rltmin, rlnmin, rltmax,
        rlnmax, dlatll, dlonll, dlatur, dlonur, rgxmin, rgymin, rgxmax,
	rgymax, angle2;
    int isgpnt, maxgrid;
    int isize, nc, mx, my, nums, igxmin, igymin, igxmax, igymax, ix1, iy1,
        ix2, iy2, one, i, ier, ierr;
/*----------------------------------------------------------------------*/
    *iret = 0;
    one = 1;

/*
 * Make sure uipnts is not blank
 */
    G_MALLOC ( ui, char, strlen(uipnts) + 1, "dgc_cxgp - ui" );
    if ( ui == NULL ) {
        *iret = -73;
	return;
    }
    cst_rmbl ( (char *)uipnts, ui, &isize, &ier );
    if ( isize == 0 ) {
	*iret = -45;
	er_wmsg ( "DG", iret, " ", &ierr, strlen("DG"), strlen(" ") );
	return;
    }

/*
 * Set the reference grid navigation in GPLT.
 */
    cst_itos ( (int *)&_dgsubg.refnav[1], 1, &nc, gprj, &ier );
    cst_rmbl ( gprj, gprj, &nc, &ier );
    mx = G_NINT ( _dgsubg.refnav[4] );
    my = G_NINT ( _dgsubg.refnav[5] );
    if ( _dgfile.addcol == G_TRUE ) {
	mx += 1;
	agln2 = _dgsubg.refnav[7];
    } else {
	agln2 = _dgsubg.refnav[9];
    }
    gsgprj ( gprj, &_dgsubg.refnav[10], &_dgsubg.refnav[11], &_dgsubg.refnav[12],
	&mx, &my, &_dgsubg.refnav[6], &_dgsubg.refnav[7], &_dgsubg.refnav[8],
	&agln2, &ier, strlen(gprj) );
    if ( ier != 0 ) {
	er_wmsg ( "GEMPLT", &ier, " ", &ierr, strlen("GEMPLT"), strlen(" ") );
	*iret = ier;
	return;
    }

/*
 * Check uipnts is GPOINT or CXSTNS and transform
 */
    if ( strchr ( ui, '>' ) ) {
	isgpnt = G_FALSE;
	cxstns = (char **)cmm_malloc2d ( 2, LLMXLN+1, sizeof(char), &ier );
	if ( ier != 0 ) {
	    *iret = -73;
	    return;
	}
	cst_clst ( ui, '>', " ", 2, LLMXLN, cxstns, &nums, &ier );
	for ( i = 0; i < 2; i++ ) {
	    grc_ploc ( cxstns[i], &rx[i], &ry[i], &rlt[i], &rln[i], &ier );
	    if ( ier != 0 ) {
		er_wmsg ( "GR", &ier, " ", &ierr, strlen("GR"), strlen(" ") );
		*iret = -46;
		cmm_free2d ( (void **)cxstns, &ier );
		return;
	    }
	}
	cmm_free2d ( (void **)cxstns, &ier );
    } else {
	isgpnt = G_TRUE;
	grc_ploc ( ui, &rx[0], &ry[0], &rlt[0], &rln[0], &ier );
	if ( ier != 0 ) {
	    er_wmsg ( "GR", &ier, " ", &ierr, strlen("GR"), strlen(" ") );
	    *iret = -46;
	    return;
	}
    }
    G_FREE ( ui, char );

/*
 * Define sub-grid area
 */
    if ( _dgsubg.gwrapg == G_TRUE ) {
	if ( isgpnt == G_TRUE ) {
	    rltmin = rlt[0];
	    rlnmin = rln[0];
	    rltmax = rlt[0];
	    rlnmax = rln[0];
	} else {
	    rltmin = G_MIN ( rlt[0], rlt[1] );
	    rlnmin = G_MIN ( rln[0], rln[1] );
	    rltmax = G_MAX ( rlt[0], rlt[1] );
	    rlnmax = G_MAX ( rln[0], rln[1] );
	}

	/*
	 * Take care of the sub-grid area across the date-line
	 */
	if ( ( rlnmax - rlnmin ) > 180. ) {
	    dlatll = rltmin;
	    dlonll = rlnmax;
	    dlatur = rltmax;
	    dlonur = rlnmin;
	} else {
	    dlatll = rltmin;
	    dlonll = rlnmin;
	    dlatur = rltmax;
	    dlonur = rlnmax;
	}

	/*
	 * Extend the sub-grid area bounds by 5 degrees if possible
	 */
	dlatll -= 5.;
	if ( dlatll < -90. ) dlatll = -90.;
	dlonll -= 5.;
	if ( dlonll < -180. ) dlonll += 360.;
	dlatur += 5.;
	if ( dlatur > 90. ) dlatur = 90.;
	dlonur += 5.;
	if ( dlonur > 180 ) dlonur -= 360.;
    } else {
	if ( isgpnt == G_TRUE ) {
	    igxmin = (int)( rx[0] );
	    igymin = (int)( ry[0] );
	    igxmax = G_NINT ( rx[0] );
	    igymax = G_NINT ( ry[0] );
	} else {
	    igxmin = (int)( G_MIN ( rx[0], rx[1] ) );
	    igymin = (int)( G_MIN ( ry[0], ry[1] ) );
	    igxmax = G_NINT ( G_MAX ( rx[0], rx[1] ) );
	    igymax = G_NINT ( G_MAX ( ry[0], ry[1] ) );
	}

	/*
	 * Extend the sub-grid area bounds by 2 grid index units if possible
	 */
	for ( i = 0; i < 2; i++ ) {
	    if ( igxmin > 1 ) igxmin--;
	    if ( igymin > 1 ) igymin--;
	    if ( igxmax < _dgfile.kxd ) igxmax++;
	    if ( igymax < _dgfile.kyd ) igymax++;
	}
	rgxmin = igxmin;
	rgymin = igymin;
	rgxmax = igxmax;
	rgymax = igymax;

	gtrans ( sys_G, sys_M, &one, &rgxmin, &rgymin, &dlatll, &dlonll, &ier,
	         strlen(sys_G), strlen(sys_M) );
	gtrans ( sys_G, sys_M, &one, &rgxmax, &rgymax, &dlatur, &dlonur, &ier,
	         strlen(sys_G), strlen(sys_M) );
    }

    /*
     * *Set internal sub-grid navigation
     */
    gsmprj ( gprj, &_dgsubg.refnav[10], &_dgsubg.refnav[11], &_dgsubg.refnav[12],
	&dlatll, &dlonll, &dlatur, &dlonur, &ier, strlen(gprj) );

    /*
     * IF set sub-grid navigation fail, change center longitude
     */
    if ( ier != 0 ) {
	angle2 = _dgsubg.refnav[11] + 180.;
	if ( angle2 >= 360. ) angle2 -= 360.;
	gsmprj ( gprj, &_dgsubg.refnav[10], &angle2, &_dgsubg.refnav[12],
	    &dlatll, &dlonll, &dlatur, &dlonur, &ier, strlen(gprj) );
    }
    
    /*
     * IF set sub-grid navigation still fail, something is wrong
     */
    if ( ier != 0 ) {
	er_wmsg ( "GEMPLT", &ier, " ", &ierr, strlen("GEMPLT"), strlen(" ") );
	*iret = ier;
	return;
    }

    /*
     * Set up the sub-grid
     */
    maxgrid = IMISSD;
    dgc_subg ( ijskip, &maxgrid, &ix1, &iy1, &ix2, &iy2, &ier );
    if ( ier != 0 ) {
	er_wmsg ( "DG", &ier, " ", &ierr, strlen("DG"), strlen(" ") );
	*iret = ier;
	return;
    }

    /*
     * Translate user input into actual point(s) relative to sub-grid
     */
    if ( isgpnt == G_TRUE ) {
	cst_rlch ( rlt[0], 6, slt[0], &ier );
	cst_rlch ( rln[0], 6, sln[0], &ier );
	strcpy ( gpoint, slt[0] );
	strcat ( gpoint, ";" );
	strcat ( gpoint, sln[0] );
	grc_ploc ( gpoint, rgx, rgy, rlat, rlon, &ier );
	if ( ier != 0 ) {
	    er_wmsg ( "GR", &ier, " ", &ierr, strlen("GR"), strlen(" ") );
	    *iret = -46;
	    return;
	}
    } else {
	for ( i = 0; i < 2; i++ ) {
	    cst_rlch ( rlt[i], 6, slt[i], &ier );
	    cst_rlch ( rln[i], 6, sln[i], &ier );
	}
	strcpy ( gpoint, slt[0] );
	strcat ( gpoint, ";" );
	strcat ( gpoint, sln[0] );
	strcat ( gpoint, ">" );
	strcat ( gpoint, slt[1] );
	strcat ( gpoint, ";" );
	strcat ( gpoint, sln[1] );
	grc_plin ( gpoint, npmx, np, rgx, rgy, rlat, rlon, &ier );
	if ( ier != 0 ) {
	    er_wmsg ( "GR", &ier, " ", &ierr, strlen("GR"), strlen(" ") );
	    *iret = -46;
	    return;
	}
    }

    return;
}
