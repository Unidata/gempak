#include "dv.h"

void dv_geo  ( int *iret )
/************************************************************************
 * dv_geo								*
 *									*
 * This subroutine computes the geostrophic wind:			*
 *									*
 *     GEO ( S )  = [ - DDY (S) * const / CORL, DDX (S) * const / CORL ]*
 *									*
 *                  Where: const    S    vert.coord.			*
 *                         ------  ----  -----------			*
 *                         GRAVTY  ZMSL     none			*
 *                         GRAVTY  HGHT     PRES			*
 *                           1     PSYM     THTA			*
 *                         100/RO  PRES     HGHT			*
 *                         ------  ----  -----------			*
 *                         RO = PD_DDEN ( PRES, TMPC )			*
 *									*
 * GEO generates a vector grid.						*
 *									*
 * dv_geo  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS, plus		*
 *					 -9 = ... calling sequence error*
 *					-11 = ... must be a vector	*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * M. desJardins/GSFC	 9/88	GEMPAK4 Version				*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/GSFC	 7/89	Added PA subroutines			*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC		10/89   Subsetting				*
 * K. Brill/NMC         11/90   Pass grid number to DF_CORL		*
 * K. BRILL/NMC		12/92	Check for too small Coriolis parm	*
 * L. Sager/NMC          4/93   Change to UGEO and VGEO			*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * D. Keiser/GSC	 8/95	Increased Coriolis parm			*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96	Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB, DG_ESUB; CHK iret & RTRN	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
C************************************************************************/
{
	char	        grid[13], parm[13], time1[21], time2[21];
	int		level1, level2, ivcord;

        char            errst[1024];
        int             i, ier, zero=0, tmp, nval, kxd, kyd, ksub1, ksub2;
        int             kxyd, idlun, ndx, ndy, ignum;
        int             num1, nf, ncnst, nvecu, nvecv;
        float           *grnum1, *grnf, *grcnst, *grvecu,  *grvecv;
        float           testit, cnst, dens;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Read the information from the top of the stack.
         */
	dg_tops  ( grid, &ignum, time1, time2, &level1, &level2, 
                          &ivcord, parm, iret );
	if  ( *iret != 0 )  return;

        /*
         *	If the name of the vector to be computed is "GEO", replace the
         *	top of the stack with the proper dependent "height" field.
         *	Otherwise, assume that the user has done it.
         */
	if  ( strncmp( grid, "GEO",3) == 0 )  {
	    if  ( ivcord == 0 )
		dg_rpls  ( "ZMSL", &zero, &ier );
	    else if  ( ivcord == 1 )
		dg_rpls  ( "HGHT", &zero, &ier );
	    else if  ( ivcord == 2 )
		dg_rpls  ( "PSYM", &zero, &ier );
	    else if  ( ivcord == 3 )
		dg_rpls  ( "PRES", &zero, &ier );
	    else {
		*iret = -24;
                tmp = -1;
		dg_merr  ( "", "", "", &tmp, &tmp, &ivcord, errst, &ier );
                dg_cset ( "ERRST", errst, &ier);
		return;
	    }

        /*
         *	Else, assume that the user did it.
         */
	}

        /*
         *	Read in the scalar grid.
         */
	dg_gets  ( &num1, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the scalar field on the stack and compute the y derivative.
         *	Here and following, always get the result after the computation.
         */
	dg_puts  ( &num1, iret );
	df_ddy  ( iret );
	if  ( *iret != 0 )  return;
	dg_gets  ( &ndy, iret );

        /*
         *	Put the scalar back on the stack and compute the x derivative.
         */
	dg_puts  ( &num1, iret );
	df_ddx  ( iret );
	if  ( *iret != 0 )  return;
	dg_gets  ( &ndx, iret );

        /*
         *	Compute the coriolis grid.
         */
	dg_nxts  ( &nf, iret );
	if  ( *iret != 0 )  return;
	df_corl  ( &nf, iret );
	if  ( *iret != 0 )  return;

        /*
         *   Set near equatorial ( latitude 4 degrees N/S ) Coriolis to missing.
         */
        dg_getg ( &nf, &grnf, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if ( ! ERMISS ( grnf[i] ) ) {
		testit = fabs ( grnf[i] );
		if ( testit < 1.25E-05 )
		    grnf[i] = RMISSD;
	    }
	}

        /*
         *	Divide the y derivative by the coriolis (note order of puts).
         */
	dg_puts  (  &nf, iret );
	dg_puts  ( &ndy, iret );
	if  ( *iret != 0 )  return;
	df_quo  ( iret );
	if ( *iret != 0 )  return;
	dg_gets  ( &nvecu, iret );

        /*
         *	Divide the x derivative by the coriolis.
         */
	dg_puts  (  &nf, iret );
	dg_puts  ( &ndx, iret );
	if ( *iret != 0 )  return;
	df_quo  ( iret );
	if( *iret != 0 )  return;
	dg_gets  ( &nvecv, iret );

        /*
         *    Compute the numerical "constant" grid for the vertical coordinate.
         */
	dg_nxts  ( &ncnst, iret );
	if  ( *iret != 0 )  return;
        dg_getg ( &ncnst, &grcnst, &kxd, &kyd, &ksub1, &ksub2, iret);

        /*
         *	NONE, PRES: the constant is gravity.
         */
 	if  ( (ivcord == 0 ) || ( ivcord == 1 ) )  {
	    cnst = GRAVTY;
	    dg_real  ( &cnst, &ncnst, iret );
        }

        /*
         *	    THTA: no additional constant.
         */
	else if  ( ivcord == 2 )  {
	    cnst = 1.;
	    dg_real  ( &cnst, &ncnst, iret );
        }

        /*
         *	    HGHT: the 100 converts mb to Pascals, and 1/DDEN is needed.
         */
	else if  ( ivcord == 3 )  {
	    dg_temp  ( time1, time2, &level1, &level2, &ivcord, 
                              "TMPC", &ncnst, iret );
	    if  ( *iret != 0 )  return;

            dg_getg ( &num1, &grnum1, &kxd, &kyd, &ksub1, &ksub2, iret );

	    kxyd = kxd * kyd;
	    pd_dden  ( grnum1, grcnst, &kxyd, grcnst, &ier );

	    for ( i = ksub1 - 1; i < ksub2; i++ ) {
		dens = grcnst[i];
		if  ( ERMISS ( dens ) || G_DIFFT( dens, 0.0F, GDIFFD) )
		    grcnst[i] = RMISSD;
		else
		    grcnst[i] = 100. / dens;
	    }
	}

        /*
         *	Now, compute the components of the geostrophic wind.
         */
        dg_getg ( &nvecu, &grvecu, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nvecv, &grvecv, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {

            /*
             *	    Compute the u-component of the geostrophic wind.
             */
	    if  ( ERMISS ( grvecu[i] ) || ERMISS ( grcnst[i] ) )
		grvecu[i] = RMISSD;
	    else
		grvecu[i] = - grvecu[i] * grcnst[i];

            /*
             *	    Compute the v-component of the geostrophic wind.
             */
	    if  ( ERMISS ( grvecv[i] ) || ERMISS ( grcnst[i] ) )
		grvecv[i] = RMISSD;
	    else
		grvecv[i] =   grvecv[i] * grcnst[i];
	}

        /*
         *	Update both grid headers.  Use wind type as parameter name.
         */
        dg_iget ( "IDLUN", &nval, &idlun, iret );
	dg_upvg  ( time1, time2, &level1, &level2, &ivcord, &idlun, 
                          "GEO", &nvecu, &nvecv, &ier );

        /*
         *	Update stack.
         */
	dg_putv  ( &nvecu, &nvecv, iret );
	dg_esub  ( &nvecu, &nvecv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
