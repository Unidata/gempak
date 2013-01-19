#include "df.h"

void df_hilo  ( const int *hi, const int *lo, int *iret )
/************************************************************************
 * df_hilo								*
 *									*
 * This subroutine finds the relative extrema over a grid.		*
 *									*
 * df_hilo ( hi, lo, iret )						*
 *									*
 * Input parameters:							*
 *	*hi		const int	Flag for finding highs		*
 *	*lo		const int	Flag for finding lows		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * K. Brill/NMC          5/93   					*
 * D. Keiser/GSC	 7/95	Changed DC_HILO to DG_HILO		*
 * D. Keiser/GSC	10/95	Add call to write error message after 	*
 *				call to DG_HILO				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * T. Piper/GSC		11/98	Updated prolog				*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num1, num2, no, nx, kxd, kyd, kxyd, ksub1, ksub2;
    int krad, nmax, nmin, ixmx, iymx, ivmx, ixmn, iymn, ivmn, intflg,
        nummx, nummn, ii, i, j, nxi,nxj, nxv, index, ier, ier2, izero;
    float *gnum1, *gnum2, *gno, *gnx, fzero;
    char namout[5];
/*----------------------------------------------------------------------*/
    *iret = 0;
    izero = 0;
    fzero = 0.0;

    dg_ssub ( iret );
    strcpy ( namout, "HILO" );

    /*
     * Get the grid to be searched from the stack.
     */
    dg_gets ( &num1, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &num1, &gnum1, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Get the radius value.
     */
    dg_gets ( &num2, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &num2, &gnum2, &kxd, &kyd, &ksub1, &ksub2, iret );
    krad = G_NINT ( gnum2[0] );

    /*
     * Get a new grid number for the output highs and lows.
     */
    dg_nxts ( &no, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &no, &gno, &kxd, &kyd, &ksub1, &ksub2, iret );

    kxyd = kxd * kyd;
    nmax = kxyd / 6;
    nmin = nmax;

    /*
     * Store the HILO information in the NX grid.
     */
    dg_nxts ( &nx, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &nx, &gnx, &kxd, &kyd, &ksub1, &ksub2, iret );

    ixmx = 1;
    iymx = 1 + nmax;
    ivmx = iymx + nmax;
    ixmn = ivmx + nmin;
    iymn = ixmn + nmin;
    ivmn = iymn + nmin;
    if ( *hi == G_FALSE ) nmax = 0;
    if ( *lo == G_FALSE ) nmin = 0;

    intflg = G_FALSE;
    dg_hilo ( gnum1, &kxd, &kyd, &krad, &intflg, &nmax, &nmin, 
        &fzero, &fzero, &fzero, &fzero,
	&nummx, &gnx[ixmx-1], &gnx[iymx-1], &gnx[ivmx-1],
	&nummn, &gnx[ixmn-1], &gnx[iymn-1], &gnx[ivmn-1], iret );
    if ( *iret < 0 )  return;

    /*
     * Write out error message regarding internal buffers in DG_HILO.
     */
    if ( *iret != 0 ) {
        er_wmsg ( "DG", iret, " ", &ier2, strlen("DG"), strlen(" ") );
    }

    /*
     * Set all grid values to missing.
     */
    for ( ii = ksub1; ii <= ksub2; ii++ ) {
        gno[ii-1] = RMISSD;
    }

    if ( *hi == G_TRUE ) {
        if ( *lo == G_FALSE ) strcpy ( namout, "HIGH" );

	/*
	 * Put the maxima on the grid.
	 */
	for ( ii = 1, nxi = ixmx-1, nxj = iymx-1, nxv = ivmx-1; 
	      ii <= nummx; 
	      ii++, nxi++, nxj++, nxv++ ) {
    	    i = G_NINT ( gnx[nxi] );
    	    j = G_NINT ( gnx[nxj] );
    	    index = ( j - 1 ) * kxd + i;
    	    gno[index-1] = gnx[nxv];
        }
    }

    if ( *lo == G_TRUE ) {
        if ( *hi == G_FALSE ) strcpy ( namout, "LOWS" );

	/*
	 *Put the minima on the grid.
	 */
        for ( ii = 1, nxi = ixmn-1, nxj = iymn-1, nxv = ivmn-1;
	      ii <= nummn;
	      ii++, nxi++, nxj++, nxv++ ) {
    	    i = G_NINT ( gnx[nxi] );
    	    j = G_NINT ( gnx[nxj] );
    	    index = ( j - 1 ) * kxd + i;
    	    gno[index-1] = gnx[nxv];
        }
    }

    /*
     * Get a name of the form NAMOUT //S and update header;
     * update stack.
     */
    dg_updh ( namout, &no, &num1, &izero, iret );
    dg_puts ( &no, iret );
    dg_esub ( &no, &izero, &izero, &izero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
