#include "dv.h"

void dv_msfc ( int *iret )
/************************************************************************
 * dv_msfc								*
 *									*
 * This subroutine computes angular momentum surfaces for a cross	*
 * section.								*
 *									*
 *	MSFC ( V ) = NORM ( V ) + CORL * DIST				*
 *									*
 * dv_msfc ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/SSAI	10/91						*
 * K. Brill/NMC		 4/93	Modified distance calculation.		*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * T. Lee/GSC		 5/96	Moved IGDPT outside DO loop		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB, DG_ESUB; CHK iret & RTRN	*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				by using internal grids for scl fctrs	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             ier, nval, i, ksub1, ksub2, zero=0;
        int             kxd, kyd, ixg, iyg;
	int		nnrm, nf, numout, ixmscl, iymscl;
	float		*grnrm, *grnf, *grout, *grxms, *gryms;
        float           orgxpt, orgypt, xg, yg, rmavx, rmavy, dd, gddx, gddy;
        int             iorg, jorg, indor, ijcorn;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Check to see if the origin from which to compute distance has
         *	been set.
         */
        nval = 1;
        dg_fget ( "ORGXPT", &nval, &orgxpt, iret );
        dg_fget ( "ORGYPT", &nval, &orgypt, iret );

	if ( ERMISS ( orgxpt ) || ERMISS ( orgypt ) ) {
	    *iret = -35;
	    return;
	}

        /*
         *	Get the component of the wind normal to the plane of the
         *	cross section.
         */
	dv_norm ( iret );
	if  ( *iret != 0 )  return;
	dg_gets ( &nnrm, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Get the coriolis force at every grid point.
         */
	dg_nxts ( &nf, iret );
	if  ( *iret != 0 )  return;
	df_corl ( &nf, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute the mapscale factors.
         */
	dg_mscl ( iret );
	if  ( *iret != 0 )  return;

        /*
         *	Get the next internal grid number.
         */
	dg_nxts ( &numout, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Calculate the distance to each grid point, and calculate the
         *	values for the M-surface.
         */
        dg_getg ( &nf, &grnf, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nnrm, &grnrm, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numout, &grout, &kxd, &kyd, &ksub1, &ksub2, iret );

	dg_qmsl ( &ixmscl, &iymscl, &gddx, &gddy, iret );
        dg_getg ( &ixmscl, &grxms, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &iymscl, &gryms, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {

            /*
             *	    Find grid x, y index values.
             */
	    ixg =  (i+1) % kxd;
	    if ( ixg == 0 ) ixg = kxd;
	    iyg = (i+1) / kxd + 1;
	    if ( ixg == kxd ) iyg = iyg - 1;
	    xg = ixg;
	    yg = iyg;

            /*
             *	    Compute average scale factor.
             */
	    rmavx = grxms[i];
	    rmavy = gryms[i];
	    iorg = rint( orgxpt );
	    jorg = rint( orgypt );
	    indor = ( jorg - 1 ) * kxd + iorg;
	    if ( (iorg >= 1) && (iorg <= kxd) &&
     		 (jorg >= 1) && (jorg <= kyd) ) {
		ijcorn = ( jorg - 1 ) * kxd +  ixg;
		rmavx = .5 * ( grxms[ijcorn-1] + grxms[indor-1] );
	        rmavy = .5 * ( gryms[ijcorn-1] + gryms[i] );
	    }
	    xg = ( xg - orgxpt ) * gddx / rmavx;
	    yg = ( yg - orgypt ) * gddy / rmavy;
	    dd = sqrt ( xg * xg + yg * yg );
	    if ( ERMISS ( grnrm[i] ) || ERMISS ( grnf[i] ) )
		grout[i] = RMISSD;
	    else
	        grout[i] = grnrm[i] + grnf[i] * dd;
	    
	}

        /*
         *	Make a name of the form 'MSFC' and update header; update stack
         */
	dg_updh ( "MSFC", &numout, &zero, &zero, iret );
	dg_puts ( &numout, iret );
	dg_esub ( &numout, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
