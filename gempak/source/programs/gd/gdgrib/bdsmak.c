#include "gdgrib.h"

void bds_mak ( const int *igx, const int *igy, const char *precsn,
               float *grid, int *nbyts, unsigned char *cbds, int *ip10sc,
	       int *misval, int *iret )
/************************************************************************
 * bds_mak								*
 *									*
 * This subroutine makes the GRIB BDS from an input grid of data.	*
 *									*
 * PRECSN contains the user input for the required precision.  It may	*
 * be entered as B/n, where n is the power of 2 to which data is to be	*
 * rounded, or D/r, where r is the number of decimal significant digits	*
 * to be preserved.							*
 *									*
 * bds_mak ( igx, igy, precsn, grid, nbyts, cbds, ip10sc, misval, iret )*
 *									*
 * Input parameters:							*
 *	*igx		const int	Number of points in x dir	*
 *	*igy		const int	Number of points in y dir	*
 *	*precsn		const char	Precision parm			*
 *									*
 * Input and output parameters:						*
 *	*grid		float		Input: gridded data values	*
 *					Output: values * 10 ** IP10SC	*
 *	*nbyts		int		Input:  Max # of bytes for BDS	*
 *					Output: # of bytes in BDS	*
 * Output parameters:							*
 *	*cbds		unsigned char	Binary data section		*
 *	*ip10sc		int		Power of 10 scaling (for PDS)	*
 *	*misval		int		Flag for missing data on grid	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-21 = BDS section too long	*
 *					-24 = invalid binary scaling	*
 *					-25 = cannot compute reference  *
 *					-26 = BDS array size too small  *
 *					-27 = nbit calculation failed	*
 *					-28 = all data is missing	*
 **									*
 * Log:									*
 * K. Brill/HPC		08/99						*	
 * K. Brill/HPC		 9/99	Check for all missing data		*
 * K. Brill/HPC		 3/00	Write out warning for default precision *
 * K. Brill/HPC		 5/00	Allow bit maps for constant grids	*
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 ************************************************************************/
{
    char cprc[17], part[2][9], *cdp[2];
    unsigned char cref[4];
    int ibyts[3], kxky, mxln, icntms, nbits, num0, ip2scl, num, iprec,
        ibase, n, nb, ival, ncdat, ilen, lendat, ii, jj, ier;
    float qmin, qmax, prec, rmn, factor;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *misval = G_FALSE;
    ibase = 256;
    kxky = (*igx) * (*igy);
    mxln = *nbyts - 11;

    /*
     * Read through the grid finding the minimum and maximum values.
     */
    qmin = 1.0E+31;
    qmax = -1.0E+31;
    icntms = 0;
    for ( ii = 0; ii < kxky; ii++ ) {
	if ( ! ERMISS ( grid[ii] ) ) {
	    if ( grid[ii] < qmin )  qmin = grid[ii];
	    if ( grid[ii] > qmax )  qmax = grid[ii];
	} else {
	    icntms++;
	}
    }
    if ( icntms == kxky ) {
	*iret = -28;
	return;
    }

    if ( G_DIFF ( qmax, qmin ) ) {
	/*
	 * Constant field.
	 */
	nbits = 0;
	*ip10sc = 0;
	num0 = 8;
	ip2scl = 0;
	*nbyts = 12;
	cbds[11] = (unsigned char) (0);
	if ( icntms > 0 ) {
	    *misval = G_TRUE;
	} else {
	    *misval = G_FALSE;
	}
    } else {
	/*
	 * Use the precision to compute the number of bits, the power
	 * of 10 scaling, and the rounded minimum.
	 */
	cst_lcuc ( (char *)precsn, cprc, &ier );
	for ( ii = 0; ii < 2; ii++ ) cdp[ii] = part[ii];
	cst_clst ( cprc, '/', "", 2, 8, cdp, &num, &ier );
	if ( part[0][0] == 'D' ) {
	    cst_crnm ( part[1], &prec, &ier );
	    if ( ERMISS ( prec ) ) prec = 4.0;
	} else if ( part[0][0] == 'B' ) {
	    cst_numb ( part[1], &iprec, &ier );
	    if ( iprec == IMISSD ) {
		prec = 5.0;
	    } else {
		if ( iprec > 0 ) {
		    prec = (float) ( 50 + iprec );
		} else {
		    prec = (float) ( iprec );
		}
	    }
	} else {
	    printf ( " WARNING -- Default precision D/5 used.\n" );
	    prec = 5.0;
	}

	bds_nbt ( &qmin, &qmax, &prec, &nbits, ip10sc, &rmn, &ier );
	if ( ier != 0 ) {
	    *iret = -27;
	    return;
	}

	/*
	 * Apply power of 10 scaling.
	 */
	factor = pow ( 10., *ip10sc );
	qmin = rmn * factor;
	qmax = qmax * factor;
	for ( ii = 0; ii < kxky; ii++ ) {
	    if ( ! ERMISS ( grid[ii] ) ) {
		grid[ii] *= factor;
	    }
	}

	/*
	 * Get the lenth for the BDS.
	 */
	n = nbits * ( kxky - icntms );
	ncdat = n / 8;
	if ( ( n % 8 ) != 0 ) ncdat++;
	num0 = ncdat * 8 - n;
	ilen = ncdat + 11;
	if ( ( ilen % 2 ) != 0 ) {
	    ilen++;
	    num0 += 8;
	    ncdat++;
	}
	if ( ilen > *nbyts ) {
	    *iret = -26;
	    return;
	}
	*nbyts = ilen;
	if ( icntms > 0 ) {
	    *misval = G_TRUE;
	} else {
	    *misval = G_FALSE;
	}

	/*
	 * Initialize entire BDS.
	 */
	for ( ii = 0; ii < *nbyts; ii++ ) {
	    cbds[ii] = (unsigned char) (0);
	}

	/*
	 * Pack the scaled gridded data.
	 */
	lendat = mxln;
	bds_pgb ( grid, igx, igy, &qmin, &qmax, &nbits, &lendat,
	          &cbds[11], &ip2scl, iret );
	if ( *iret != 0 ) return;
    }

    /*
     * Now set the BDS header byte values.
     */
    jj = 0;
    nb = 3;
    gdigit ( nbyts, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
        *iret = -21;
        return;
    } else {
        for ( ii = 2; ii >=0; ii-- ) {
            cbds[jj++] = (unsigned char)( ibyts[ii] );
	}
    }

    /*
     * Set # of unused bits in octet 4.
     */
    cbds[jj++] = (unsigned char)( num0 );

    /*
     * Set binary scale factor in octets 5-6.
     */
    ival = G_ABS ( ip2scl );
    nb = 2;
    gdigit ( &ival, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
        *iret = -24;
        return;
    } else {
        if ( ip2scl < 0 ) ibyts[1] += 128;
	for ( ii = 1; ii >= 0; ii-- ) {
	    cbds[jj++] = (unsigned char)( ibyts[ii] );
	}
    }

    /*
     * Compute and load the reference value.
     */
    bds_ibm ( &qmin, cref, &ier );
    if ( ier != 0 ) {
        *iret = -25;
        return;
    } else {
        for ( ii = 0; ii < 4; ii++ ) {
	    cbds[jj++] = cref[ii];
	}
    }

    /*
     * Load in the number of bits used to pack data.
     */
    cbds[jj++] = (unsigned char)( nbits );

    return;
}
