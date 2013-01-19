#include "dv.h"

void dv_pvor ( int *iret )
/************************************************************************
 * dv_pvor								*
 *									*
 * This subroutine computes the potential vorticity in a layer from 	*
 * scalar (thermal and pressure) and vector (wind) fields:		*
 *									*
 *     PVOR ( s, V ) = - GRAVTY * AVOR ( VLAV (V) ) * LDF ( CTA ) /	*
 *                  ( 100 * LDF ( PRES ) )				* 
 *								  	*
 *                  Where: 100 converts millibars to Pascals		*
 *			   CTA can be THTA, THTE, or THES		*
 *								  	*
 * PVOR works on a layer in THTA or PRES coordinates.  In the case of	*
 * PRES, the vorticity of the layer averaged wind is corrected by 	*
 * the addition of ( k cross partial V w.r.t. CTA ) dot grad (CTA).	*
 * PVOR generates a scalar grid.					*
 *									*
 * dv_pvor ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * J. Nielsen/MIT	 1/88	Check for thin layers			*
 * G. Huffman/GSC	 9/88	GEMPAK4 version				*
 * K. Brill/GSC		 8/89	Added calculations for PRES coord	*
 * K. Brill/GSC		10/89	Subsetting				*
 * K. Brill/NMC		 4/93	Changed for levels not in expected order*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * T. Lee/GSC		 4/96   Single dimension for dgg; Add a scalar	*
 *				argument and multiply gravity		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * T. Lee/GSC		 5/96	Eliminated thin layers check; Fixed	*
 *				comments and error message calls	*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int		i, ier, nval, kxd, kyd, ksub1, ksub2, zero=0;
        int             tmp, idlun;
	int		npvor, nth1, nth2;
        float           *grpvor, *grnth1, *grnth2;
        int             nu1, nv1, nu2, nv2;
        float           *gru1, *grv1, *gru2, *grv2;
        int             navu, navv, navth, ndthdx, ndthdy;
        float           *grnavu, *grnavv, *grnavth, *grthdx, *grthdy;

	char	        gp[13], gfunc[13], parm[13], time1[21], time2[21];
     	char		gdum[13], pdum[13];
	int		level1, level2, idum, ivcord;
        float           au, av, ath, dpi, du, dv, dth, vorcor, dthta;

        int             np1, np2, ndp, navor;
        float           *grp1, *grp2, *grndp;
        float           p1, p2, dtdp;

        char            errst[1024];

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Read the name of the wind at the top of the stack to get levels.
         */
	dg_tops  ( gfunc, &idum, time1, time2, &level1, &level2,
                          &ivcord, parm, iret );
	if  ( *iret != 0 )  return;

	if ( ivcord == 1 ) {

          /*
           *        Get the grid numbers for THETA at the top and bottom.
           */
          dg_getl ( &nth1, &nth2, iret );
          if ( *iret != 0 ) return;

          /*
           *        Get the grid numbers for the winds at the top and bottom 
           *        (LEVEL (1) is the top).
           */
	  dg_gtvl ( &nu1, &nv1, &nu2, &nv2, iret );
	  if ( *iret != 0 ) return;

          /*
           *	  Get three new grids.
           */
	  dg_nxtv ( &navu, &navv, iret );
	  if ( *iret != 0 ) return;
	  dg_nxts ( &navth, iret );
	  if ( *iret != 0 ) return;

          /*
           *	  Put the average wind into NAVU and NAVV.
           *	  
           *	  Put average THETA in NAVTH.
           */
          dg_getg ( &nu1, &gru1, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &nv1, &grv1, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &nu2, &gru2, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &nv2, &grv2, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &nth1, &grnth1, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &nth2, &grnth2, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &navu, &grnavu, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &navv, &grnavv, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &navth, &grnavth, &kxd, &kyd, &ksub1, &ksub2, iret );

	  for ( i = ksub1 - 1; i < ksub2; i++ ) {
	      if ( ERMISS ( gru1[i] ) || ERMISS ( grv1[i] ) || 
     		   ERMISS ( gru2[i] ) || ERMISS ( grv2[i] ) || 
     		   ERMISS ( grnth1[i] ) || ERMISS ( grnth2[i] ) )  {
                grnavu[i]  = RMISSD;
	        grnavv[i]  = RMISSD;
	        grnavth[i] = RMISSD;
              }
	      else {
	        au = .5 * ( gru1[i] + gru2[i] );
                av = .5 * ( grv1[i] + grv2[i] );
	        grnavu[i] = au;
	        grnavv[i] = av;
	        ath = .5 * ( grnth1[i] + grnth2[i] );
	        grnavth[i] = ath;
              }
	  }

          /*
           *	  Compute the vorticity in the layer.
           */
	  dg_putv ( &navu, &navv, iret );
	  if ( *iret != 0 ) return;
	  dv_avor ( iret );
	  if ( *iret != 0 ) return;
	  dg_gets ( &npvor, iret );
	  if ( *iret != 0 ) return;

          /*
           *	  Compute GRAD ( THETA ) in the layer.
           */
	  dg_puts ( &navth, iret );
	  if ( *iret != 0 ) return;
	  dv_grad ( iret );
	  if ( *iret != 0 ) return;
	  dg_getv ( &ndthdx, &ndthdy, iret );
	  if ( *iret != 0 ) return;

          /*
           *	  Compute (1/dp), converting to Pascals and changing sign.
           */
	  dpi = -1. / ( (float)( level1 - level2 ) * 100. );

          /*
           *	  Compute the potential vorticity on p.
           */
          dg_getg ( &npvor, &grpvor, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &ndthdx, &grthdx, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &ndthdy, &grthdy, &kxd, &kyd, &ksub1, &ksub2, iret );

	  for ( i = ksub1 - 1; i < ksub2; i++ ) {

              /*
               *      Do not recheck winds and theta--just check DGG(*,NAVTH).
               */
	      if ( ERMISS ( grpvor[i] ) || ERMISS ( grthdx[i] ) ||
     	 	   ERMISS ( grthdy[i] ) || ERMISS ( grnavth[i] ) ) 
	        grpvor[i] = RMISSD;
	      else {
	        du = gru1[i] - gru2[i];
	        dv = grv1[i] - grv2[i];
	        dth = grnth1[i] - grnth2[i];
	        vorcor = ( du * grthdy[i] - dv * grthdx[i] ) / dth;
	        grpvor[i] = GRAVTY * ( grpvor[i] + vorcor ) * dth * dpi;
	      }
	  }
        }
        else if ( ivcord == 2 ) {

          /*
           *	Get grid numbers for isentropic pressure and winds. 
           */
	  dg_getl ( &np1, &np2, iret );
	  if ( *iret != 0 ) return;
	  dg_nxts ( &ndp, iret );
	  if ( *iret != 0 ) return;
	  dg_nxtv ( &navu, &navv, iret );
	  if ( *iret != 0 ) return;
	  dg_tops ( gdum, &navor, time1, time2, &level1, &level2, 
                           &ivcord, pdum, iret );
	  if ( *iret != 0 ) return;
	  dg_gtvl ( &nu1, &nv1, &nu2, &nv2, iret );
	  if ( *iret != 0 ) return;

          /*
           *        Average the wind in the layer, and compute LDF of THETA
           *	  and pressure.
           */
          dg_getg ( &nu1, &gru1, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &nv1, &grv1, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &nu2, &gru2, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &nv2, &grv2, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &np1, &grp1, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &np2, &grp2, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &ndp, &grndp, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &navu, &grnavu, &kxd, &kyd, &ksub1, &ksub2, iret );
          dg_getg ( &navv, &grnavv, &kxd, &kyd, &ksub1, &ksub2, iret );

	  for ( i = ksub1 - 1; i < ksub2; i ++ ){
	      if ( ERMISS ( gru1[i] ) || ERMISS ( grv1[i] ) ||
     		   ERMISS ( grp1[i] ) || ERMISS ( gru2[i] ) ||
     		   ERMISS ( grv2[i] ) || ERMISS ( grp2[i] ) ) {
		grnavu[i]  = RMISSD;
		grnavv[i]  = RMISSD;
		grndp[i]   = RMISSD;
              }
              else {
		au = .5 * ( gru1[i] + gru2[i] );
		av = .5 * ( grv1[i] + grv2[i] );
		p1 = grp1[i];
		p2 = grp2[i];
		grnavu[i] = au;
		grnavv[i] = av;
		grndp[i]  = p1 - p2;
              }
          }
	  dthta = level1 - level2;

          /*
           *	Compute absolute vorticity from the average, and read the
           *	grid number of the result.
           */
	  dg_putv ( &navu, &navv, iret);
	  if ( *iret != 0 ) return;
	  dv_avor  ( iret );
	  if ( *iret != 0 ) return;
	  dg_gets ( &npvor, iret );
	  if ( *iret != 0 ) return;

          /*
           *        Compute the result.
           */
          dg_getg ( &npvor, &grpvor, &kxd, &kyd, &ksub1, &ksub2, iret );

	  for ( i = ksub1 - 1; i < ksub2; i++ ) {
	      if ( ERMISS ( grndp[i] ) || ( G_DIFFT(grndp[i], 0.0F, GDIFFD) ) )
		grpvor[i] = RMISSD;
	      else {

                /*
                 *	Compute PVOR. Millibars * 100 = Pascals.
                 */
		dtdp = dthta / grndp[i];
		if ( ! ERMISS ( grpvor[i] ) )
		    grpvor[i] = -.01 * GRAVTY * dtdp * grpvor[i];
	      }
	  }

        }
	else {
	    *iret = -24;
            tmp = -1;
	    dg_merr  ( "", "", "", &tmp, &tmp, &ivcord, errst, &ier );
            dg_cset ( "ERRST", errst, &ier );
	    return;
	}

        /*
         *	Make a name of the form 'PVOR' // V and update header; update
         *	stack.
         */
        dg_mnam  ( "PVOR", gfunc, "", gp, iret );
        nval = 1;
        dg_iget ( "IDLUN", &nval, &idlun, iret);
        dg_upsg  ( time1, time2, &level1, &level2, &ivcord, &idlun, 
                          gp, &npvor, iret );
        dg_puts  ( &npvor, iret );
	dg_esub  ( &npvor, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
