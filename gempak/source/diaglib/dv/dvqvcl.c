#include "dv.h"

void dv_qvcl ( int *iret )
/************************************************************************
 * dv_qvcl								*
 *									*
 * This subroutine computes the layer Q-vector using a scalar (usually	*
 * THTA) and a vector (usually GEO) in the formula:			*
 *									*
 *  QVCL ( S, V ) = ( 1/( D(S)/DP ) ) *					*
 * 		    [ ( DOT ( DVDX (V), GRAD (S) ) ),			*
 *                    ( DOT ( DVDY (V), GRAD (S) ) ) ]			*
 *									*
 *  where the derivates of u, v and S use layer average values of the	*
 *        scalar quantities; and D(S)/DP is the layer difference of 	*
 *        S divided by the pressure difference.				*
 *									*
 * A scale factor of 9 or 10 is appropriate.				*
 *									*
 * Note: This function is valid only for isobaric coordinates.		*
 *									*
 * dv_qvcl  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 3/94						*
 * S. Jacobs/NMC	 5/95	Updated header for documentation	*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB, DG_ESUB; CHK iret & RTRN	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		i, ier, kxd, kyd, ksub1, ksub2, zero=0;

        int             nthta1, nthta2, ndthdp, nthavg;
        float           *grthta1, *grthta2, *grdthdp, *grthavg;

        int             nlavu, nlavv, nu, nv;

        float           dp;

	char	        gfun[13], gvect[13], gnam[13], pdum[13];
        char		time1[21], time2[21];
        char            errst[1024];
	int		level1, level2, ignum, ivcord, tmp;
        int             nqx, nqy, nqvecx, nqvecy;
        float           *grqx, *grqy, *grqvecx, *grqvecy;

/*-----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	The scalar is on the top of the stack.
         *	Get the information for the scalar grid. If the vertical
         *	coordinate is NOT 'PRES', then return.
         */
	dg_tops ( gfun, &ignum, time1, time2, &level1, &level2, 
                         &ivcord, pdum, iret );
	if  ( ivcord != 1 ) {
	    *iret = -24;
            tmp = -1;
	    dg_merr  ( "", "", "", &tmp, &tmp, &ivcord, errst, &ier );
            dg_cset ( "ERRST", errst, &ier );
	    return;
	}

        /*
         *	Get the scalar field at both levels.
         */
	dg_getl ( &nthta1, &nthta2, iret );
	if ( *iret != 0 ) return;

        /*
         *	Get two output grids.
         */
	dg_nxts ( &ndthdp, iret );
	if ( *iret != 0 ) return;
	dg_nxts ( &nthavg, iret );
	if ( *iret != 0 ) return;

        /*
         *	Compute the difference in pressure over the layer. Then
         *	calculate D(THETA)/DP and the average THTA for the layer.
         */
	dp = (float) ( level1 - level2 );

        dg_getg ( &nthta1, &grthta1, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nthta2, &grthta2, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &ndthdp, &grdthdp, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nthavg, &grthavg, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if  ( ERMISS ( grthta1[i] ) || ERMISS ( grthta2[i] ) )  {
		grdthdp[i] = RMISSD;
		grthavg[i] = RMISSD;
            }
	    else {
		grdthdp[i] = ( grthta1[i] - grthta2[i] ) / dp;
		grthavg[i] = ( grthta1[i] + grthta2[i] ) / 2.0;
            }
	}

        /*
         *	The vector is now on the top of the stack. Get the data and
         *	compute the VLAV ( V ) and get the result.
         */
	dg_topv ( gvect, &nu, &nv, time1, time2, &level1, &level2, 
                         &ivcord, pdum, iret );
	dv_vlav ( iret );
	if  ( *iret != 0 )  return;
	dg_getv  ( &nlavu, &nlavv, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the vector and the scalar on the stack.
         */
	dg_putv ( &nlavu, &nlavv, iret );
	dg_puts ( &nthavg, iret );

        /*
         *	Calculate the Q-vector.
         */
	dv_qvec ( iret );
	if ( *iret != 0 ) return;
	dg_getv  ( &nqvecx, &nqvecy, iret );

        /*
         *	Get an output vector.
         */
	dg_nxtv ( &nqx, &nqy, iret );
	if ( *iret != 0 ) return;

        /*
         *	Multiply the Q-vector by the stability.
         */
        dg_getg ( &nqx, &grqx, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nqy, &grqy, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nqvecx, &grqvecx, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nqvecy, &grqvecy, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if  ( ERMISS ( grdthdp[i] ) || ERMISS ( grqvecx[i] ) ||
     +		  ERMISS ( grqvecy[i] ) )  {
		grqx[i] = RMISSD;
		grqy[i] = RMISSD;
            }
	    else {
		grqx[i] = ( -1 / grdthdp[i] ) * grqvecx[i];
		grqy[i] = ( -1 / grdthdp[i] ) * grqvecy[i];
            }
	}

        /*
         *	Load the layer Q-vector into the stack.
         */
	dg_mnam  ( "QVCL", gfun, gvect, gnam, &ier );
	dg_upvg  ( time1, time2, &level1, &level2, &ivcord, &zero, 
                   gnam, &nqx, &nqy, iret );
	dg_putv  ( &nqx, &nqy, iret );
	dg_esub  ( &nqx, &nqy, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
