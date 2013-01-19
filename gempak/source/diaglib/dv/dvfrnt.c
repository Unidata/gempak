#include "dv.h"

void dv_frnt ( int *iret )
/************************************************************************
 * dv_frnt								*
 *									*
 * This subroutine computes the frontogenesis function from THTA and	*
 * the wind.  The following equation is used:				*
 *									*
 *     FRNT ( THTA, V ) = CONV * 1/2 * MAG ( GRAD (THTA) ) *		*
 *                        ( DEF * COS (2 * BETA) - DIV )		*
 *									*
 *                        Where: CONV = unit conversion factor		*
 *                                    = 1.08E4 * 1.E5			*
 *                               BETA = ASIN ( (-DDX (THTA) * COS (PSI)	*
 *                                         - DDY (THTA) * SIN (PSI))/   *
 *                                      MAG ( GRAD (THTA) ) )		*
 *                               PSI  = 1/2 ATAN2 ( SHR / STR )		*
 *									*
 *									*
 * dv_frnt  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. Goodman/RDS	11/85						*
 * G. Huffman/GSC	 9/88	GEMPAK4 version 			*
 * G. Huffman/GSC	 4/89	Correct BETA (second term)		*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC		10/89   Subsetting				*
 * K. Brill/NMC       	 9/90   Check for bad argument int ASIN		*
 * T. Lee/GSC		 4/96   Single dimension for dgg; Optimize	*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 *				internal grid use			*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             i, kxd, kyd, ksub1, ksub2, ier, zero=0;
	char	        gdum[13], pdum[13], time1[21], time2[21];
	int		level1, level2, ivcord, nstr, nshr;
        int             nddx, nddy, ndef, ndiv;
        float           *grddx, *grddy, *grdef, *grdiv;
        int             natn2, ndmag, nfrnt;
        float           *gratn2, *grdmag, *grfrnt;
        float           ddx, ddy, dmag, def, atn2, div, cnst;
        float           psi, argbet, argcos;

	int		nu, nv, nthta;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the THTA and wind fields and define the output grid.
         */
	dg_gets  ( &nthta, iret );
	if  ( *iret != 0 ) return;
	dg_getv  ( &nu, &nv, iret );
	if  ( *iret != 0 ) return;	
	dg_nxts  ( &nfrnt, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put THTA on the stack and compute GRAD.  Read the grid numbers
         *	of the result; the x-component is DDX (THTA).  Take MAG and
         *	get the result.
         */
	dg_puts  ( &nthta, iret );
	if  ( *iret != 0 ) return;
	dv_grad  ( iret );
	if  ( *iret != 0 ) return;
	dg_topv  ( gdum, &nddx, &nddy, time1, time2, &level1, 
     			  &level2, &ivcord, pdum, iret );
	if  ( *iret != 0 ) return;

	dv_mag  ( iret );
	if  ( *iret != 0 ) return;
	dg_gets  ( &ndmag, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put V on the stack, compute STR, put V on the stack, compute
         *	SHR, compute ATN2 ( SHR / STR ) and get the answer.  Using
         *	ATN2 returns the correct angle past the valid range of ATAN.
         */
	dg_putv  ( &nu, &nv, iret );
	if  ( *iret != 0 ) return;
	dv_str  ( iret );
	if  ( *iret != 0 ) return;
	dg_tops  ( gdum, &nstr, time1, time2, &level1, &level2, 
                          &ivcord, pdum, iret );
	if  ( *iret != 0 ) return;

	dg_putv  ( &nu, &nv, iret );
	if  ( *iret != 0 ) return;
	dv_shr  ( iret );
	if  ( *iret != 0 ) return;
	dg_tops  ( gdum, &nshr, time1, time2, &level1, &level2,
                          &ivcord, pdum, iret );
	if  ( *iret != 0 ) return;

	df_atn2  ( iret );
	if  ( *iret != 0 ) return;
	dg_gets  ( &natn2, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put STR and SHR on the stack, compute magnitude, i.e., DEF.
         */
	dg_putv  ( &nshr, &nstr, iret );
	if  ( *iret != 0 ) return;
	dv_mag   ( iret );
	if  ( *iret != 0 ) return;
	dg_gets ( &ndef, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Put V on the stack, compute DIV, and get the result.
         */
	dg_putv  ( &nu, &nv, iret );
	if  ( *iret != 0 ) return;
	dv_div  ( iret );
	if  ( *iret != 0 ) return;
	dg_gets  ( &ndiv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Do the calculation . . .
         *
         *	The result is in units of Kelvin / 100 Kilometers / 3 hours,
         *	and 3 hours             = 1.08E4 seconds
         *	    100 kilometers      = 1.00E5 meters
         *	    analytic multiplier = .5
         */
	cnst = .5 * 1.08E4 * 1.00E5;

        dg_getg ( &nddx, &grddx, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nddy, &grddy, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &ndef, &grdef, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &ndiv, &grdiv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &natn2, &gratn2, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &ndmag, &grdmag, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nfrnt, &grfrnt, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    ddx  = grddx[i];
	    ddy  = grddy[i];
	    dmag = grdmag[i];
	    def  = grdef[i];
	    atn2 = gratn2[i];
	    div  = grdiv[i];
	    if  ( ERMISS (ddx ) || ERMISS (ddy ) ||
     		  ERMISS (dmag) || G_DIFFT(dmag, 0.0F, GDIFFD) ||
     		  ERMISS (def ) || ERMISS (atn2) || ERMISS (div ) )
		grfrnt[i] = RMISSD;
	    else {

                /*
                 *	Compute PSI, then compute the argument of BETA.
                 *	The (-) on DDX and DDY is not needed, since
                 *	COS ( ASIN (-xx) ) = COS ( -ASIN (xx) )
                 *			   = COS ( +ASIN (xx) )
                 */
		psi = 0.5 * atn2;
		argbet = ( ddx * cos (psi) + ddy * sin (psi) ) / dmag;

                /*
                 *	Compute the argument of the COS in FRNT, then FRNT.
                 */
		if ( (argbet >= -1.) && (argbet <= 1.) ) {
		    argcos = 2. * asin ( argbet );
		    grfrnt[i] = cnst * dmag *
     			 	     ( def * cos ( argcos ) - div );
                }
	        else
	            grfrnt[i] = RMISSD;
		
	    }
	}

        /*
         *	Make a name of the form 'FRNT'//THTA//u and update header;
         *	update the stack.
         */
	dg_updh  ( "FRNT", &nfrnt, &nthta, &nu, iret );
	dg_puts  ( &nfrnt, iret );
	dg_esub  ( &nfrnt, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
