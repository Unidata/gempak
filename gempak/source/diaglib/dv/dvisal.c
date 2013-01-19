#include "dv.h"

void dv_isal  ( int *iret )
/************************************************************************
 * dv_isal								*
 *									*
 * This subroutine computes the isallobaric wind:			*
 *									*
 *     ISAL ( S ) = [ - DDT ( v (GEO(S)) ) / CORL,			*
 *                      DDT ( u (GEO(S)) ) / CORL ]			*
 *									*
 * The "DDT" is done explicitly, since DF_DDT only operates on grids	*
 * read directly from the file.   ISAL generates a vector field.	*
 *									*
 * dv_isal  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS, plus		*
 *					 -9 = ... calling sequence error*
 *					-11 = ... must be a vector	*
 *					-18 = TIME ... must be a range	*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * G. Huffman/GSC	 9/88	GEMPAK4 version				*
 * K. Brill		 8/89   Subsetting				*
 * K. Brill             10/89   Subsetting				*
 * K. Brill		11/89   Call TG_DIFF instead of TI_DIFF		*
 * K. Brill		11/90   Pass grid number into DF_CORL		*
 * L. Sager/NMC          4/93   Change to UISAL and VISAL               *
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB, DG_ESUB; CHK iret & RTRN	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		i, ier, nval, kxd, kyd, ksub1, ksub2, zero=0, tmp;
        char            errst[1024];
	char    	grid[13], parm[13], time1[21], time2[21];
	int		level1, level2, ignum, ivcord, mins, idlun;
	int		num1, num2, ndelu, ndelv, nf;
        float           *grdelu, *grdelv, *grf;
        float           sec;

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
         *	If the name of the vector to be computed is "ISAL", replace the
         *	top of the stack with the proper dependent "height" field.
         *	Otherwise, assume that the user has done it.
         */
	if  ( strncmp (grid, "ISAL", 4) == 0 )  {
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
         *	Read in the scalar grid at the two times.
         */
	dg_gett  ( &num1, &num2, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the earlier scalar field on the stack and compute the
         *	geostrophic wind.  Leave it for subtraction.
         */
	dg_puts  ( &num2, iret );
	dv_geo  ( iret );
	if  ( *iret != 0 )  return;

        /*
         *	Put the later scalar field on the stack and compute the
         *	geostrophic wind.  Leave it for subtraction.
         */
	dg_puts  ( &num1, iret );
	dv_geo  ( iret );
	if  ( *iret != 0 )  return;

        /*
         *	Subtract earlier from later (the order left above) and get
         *	the answer.  Indices are in the order [v,u] because the
         *	u (GEO) difference contributes to v (ISAL), and v (GEO)
         *	to u (ISAL).
         */
	dv_vsub  ( iret );
	if  ( *iret != 0 )  return;
	dg_getv  ( &ndelv, &ndelu, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Convert the date/time range into seconds; compute CORL
         *	and get the answer.
         */
	tg_diff  ( time1, time2, &mins, &ier, strlen(time1), strlen(time2) );
	if  ( mins == 0 )
	    sec = 1.;
	else
	    sec = mins * 60.;
	
	dg_nxts  ( &nf, iret );
	if  ( *iret != 0 )  return;
	df_corl  ( &nf, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Do the computation.
         */
        dg_getg( &nf, &grf, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &ndelu, &grdelu, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &ndelv, &grdelv, &kxd, &kyd, &ksub1, &ksub2, iret );

	for (i = ksub1 - 1; i < ksub2; i++ ) {

            /*
             *	    U-component.
             */
	    if  ( ERMISS ( grdelu[i] ) || ERMISS ( grf[i] ) )
		grdelu[i] = RMISSD;
	    else
		grdelu[i] = - grdelu[i] / ( grf[i] * sec );

            /*
             *	    V-component.
             */
	    if  ( ERMISS ( grdelv[i] ) || ERMISS ( grf[i] ) )
		grdelv[i] = RMISSD;
	    else
		grdelv[i] = grdelv[i] / ( grf[i] * sec );

	}

        /*
         *	Update both grid headers.  Use wind type as parameter name.
         *	Update the stack.
         */
        nval = 1;
        dg_iget ( "IDLUN", &nval, &idlun, iret );

	dg_upvg  ( time1, time2, &level1, &level2, &ivcord, &idlun, 
                          "ISAL", &ndelu, &ndelv, &ier );
	dg_putv  ( &ndelu, &ndelv, iret );
	dg_esub  ( &ndelu, &ndelv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
