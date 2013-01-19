#include "df.h"
#include "../dg/dg.h"

#ifdef UNDERSCORE
#define pd_cape	pd_cape_
#define pc_intp pc_intp_
#endif

void pd_cape ( float *, float *, float *, float *, float *, float *,
		float *, float *);
void pc_intp ( float *, float[], float[], int *, int[], int[], float[], int *);

void df_prcl ( char *parm, int *iret )
/************************************************************************
 * df_prcl								*
 *									*
 * This subroutine checks for alternate ways to compute parcel		*
 * stability parameters.						*
 *									*
 * Supported Calculations:						*
 * CAPE ( P_SFC, Z_SFC, T_SFC, TD_SFC )					*
 * CINS ( P_SFC, Z_SFC, T_SFC, TD_SFC )					*
 *									*
 * df_prcl ( parm, iret )						*
 *									*
 * Input parameters:							*
 *	*parm		Character 					*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0 = normal return		*
 *					-19 = Unknown operator		*
 *									*
 **									*
 * Log:									*
 * S. Chiswell/Unidata	10/02	Wrote for CAPE				*
 * S. Chiswell/Unidata	 5/06	Added CINS				*
************************************************************************/
{
    char  pdum[13], time1[21], time2[21], tname[13], errst[128];
    int kxd, kyd, ksub1, ksub2;
    int nt1, level1, level2, ivcord;
    int nsfcp, nz1, nsfct, nsfcd, nplcl, nthta_bot, nout;
    int nthte, npbot, nzbot, nthbot, nthpbot, nneg;
    float *gsfcp, *gz1, *gsfct, *gsfcd, *gplcl, *gthta_bot, *gout;
    float *gthte, *gpbot, *gzbot, *gthbot, *gthpbot, *gneg;
    int nval, itvcord, lev1[LLMXLV], lev2[LLMXLV], nlvl;
    int i, j, np, itmp, fidx, ier, ier1;
    float ftmp, tlcl, plcl, ptop, ztop, thetapcl_top, theta_top;
    float sumpos, sumneg;
    float adata[3], bdata[3], outdat[3];
    int intflg[3], angflg[3];
    const int zero = 0, one = 1;
/*----------------------------------------------------------------------*/
    *iret = 0;
    if ( ( strcmp(parm,"CAPE") != 0 ) &&
	 ( strcmp(parm,"CINS") != 0 ) )
	{
	*iret = -19;
	return;
	}

    dg_ssub ( iret );

    /*
     * Get time for grid calculations
     */
    dg_tops ( tname, &nt1, time1, time2, &level1, &level2, &ivcord, pdum,
              iret );
    if ( *iret != 0 ) return;

    /*
     * Get the four grids from the stack for the base of integration.
     */

    dg_gets ( &nsfcp, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &nz1, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &nsfct, iret );
    if ( *iret != 0 ) return;
    dg_gets ( &nsfcd, iret );
    if ( *iret != 0 ) return;

    /*
     * Find pressure levels in grid file at this time
     */
    itvcord = 1;
    nval = LLMXLV;
    dgc_glev ( &one, time1, time2, &itvcord, &nval, lev1, lev2, &nlvl,
                       iret );

    /*
     * Only want levels, no layers
     */
    i = 0;
    while ( i < nlvl ) {
	if ( lev2[i] != -1 ) {
           if ( i < nlvl - 1 ) {
		lev1[i] = lev1[nlvl - 1];
		lev2[i] = lev2[nlvl - 1];
           }
           nlvl--;
        }
	else {
	    i++;
	}
    }

    /*
     * Sort pressure levels
     */
    for ( i=0; i < nlvl - 1; i++ )
	for ( j = i+1; j < nlvl; j++) {
	    if ( lev1[i] < lev1[j] ) {
		itmp = lev1[i];
		lev1[i] = lev1[j];
		lev1[j] = itmp;
	        /* Note: don't have to swap lev2 which is always -1 */
	    }
	}

    /*
     * Calculate PLCL
     */
    dg_puts ( &nsfcd, iret );
    dg_puts ( &nsfct, iret );
    dg_puts ( &nsfcp, iret );
    df_plcl ( iret );
    if ( *iret != 0 ) return;

    dg_gets ( &nplcl, iret );

    /*
     * Calculate THTA for the dry adiabat below LCL
     */
    dg_puts ( &nsfcp, iret );
    dg_puts ( &nsfct, iret );
    df_thta ( iret );
    if ( *iret != 0 ) return;

    dg_gets ( &nthta_bot, iret );

    /*
     * Get output grid for cape sum
     */
    dg_nxts ( &nout, iret);
    if ( *iret != 0 ) return;

    /*
     * Get intermediate computation grids
     */
    dg_nxts ( &nthte, iret);
    if ( *iret != 0 ) return;
    dg_nxts ( &npbot, iret);
    if ( *iret != 0 ) return;
    dg_nxts ( &nzbot, iret);
    if ( *iret != 0 ) return;
    dg_nxts ( &nthbot, iret);
    if ( *iret != 0 ) return;
    dg_nxts ( &nthpbot, iret);
    if ( *iret != 0 ) return;
    dg_nxts ( &nneg, iret);
    if ( *iret != 0 ) return;

    /*
     * Grid number to grid
     */
    dg_getg ( &nsfcp, &gsfcp, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nz1, &gz1, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nsfct, &gsfct, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nsfcd, &gsfcd, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nplcl, &gplcl, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nthte, &gthte, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nthta_bot, &gthta_bot, &kxd, &kyd, &ksub1, &ksub2, iret );

    dg_getg ( &npbot, &gpbot, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nzbot, &gzbot, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nthbot, &gthbot, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nthpbot, &gthpbot, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nout, &gout, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nneg, &gneg, &kxd, &kyd, &ksub1, &ksub2, iret );

    for ( fidx = ksub1; fidx <= ksub2; fidx++) {
	i = fidx - 1;
	pd_tlcl ( &gsfct[i], &gsfcd[i], &one, &ftmp, &ier );
	pd_tmkc ( &ftmp, &one, &tlcl, &ier );

	/*
	 * Calculate THTE along the moist adiabat above LCL
	 */
	pd_thte ( &gplcl[i], &tlcl, &tlcl, &one, &gthte[i], &ier);

	gpbot[i] = gsfcp[i];
	gzbot[i] = gz1[i];
	gthbot[i] = gthta_bot[i];
	gthpbot[i] = gthbot[i];

	gout[i] = 0.0;
	gneg[i] = 0.0;
    }

    /*
     * Loop through each level of pressure data in file
     */
    for ( i=0; i < nlvl; i++ ) {
	/* reuse nsfct */
	dg_gtmp ( time1, time2, &lev1[i], &lev2[i], &itvcord,
		"TMPC", &nsfct, &ier);

	dg_vcrd ( time1, time2, &lev1[i], &lev2[i], &itvcord,
		"HGHT", &nz1, &ier1);

	if ( ( ier == 0 ) && ( ier1 == 0 ) ) {
	    for ( fidx = ksub1; fidx <= ksub2; fidx++ ){
		j = fidx - 1;
		/*
		 * Consider pressure level if above lower parcel bound
		 * and hght is above the lower parcel bound
		 * NB: Found a few cases in where topography was not
		 * consistent with pressure!
		 */

		if ( ( gpbot[j] > (float)lev1[i] ) &&
		     ( gzbot[j] < gz1[j] ) ) {
		    ztop = gz1[j];
		    plcl = gplcl[j];
		    ptop = (float)lev1[i];

		    pd_thta ( &gsfct[j], &ptop, &one, &theta_top, &ier);

		    sumpos = 0.0;
		    sumneg = 0.0;

		    if ( ( plcl > ptop ) && ( plcl < gpbot[j] ) ) {
			/*
			 * If LCL is in this layer, need to split calculation
			 * for THTA and THTE of the parcel
			 */
			adata[0] = gpbot[j];
			bdata[0] = ptop;
			adata[1] = gzbot[j];
			bdata[1] = ztop;
			adata[2] = gthbot[j];
			bdata[2] = theta_top;

			intflg[0] = G_TRUE;
			intflg[1] = G_TRUE;
			intflg[2] = G_TRUE;

			angflg[0] = G_FALSE;
			angflg[1] = G_FALSE;
			angflg[2] = G_FALSE;

			outdat[0] = adata[0];
			outdat[1] = adata[1];
			outdat[2] = adata[2];

			np = 3;
			pc_intp ( &plcl, adata, bdata, &np, intflg, angflg,
				outdat, &ier);
			if ( ier != 0 ) {
			    er_wmsg ( "PC", &ier, " ", &ier1, 
				strlen("PC"), strlen(" ") );
			}

			thetapcl_top = gthta_bot[j];
			pd_cape ( &gzbot[j], &outdat[1], &gthbot[j],
				&outdat[2], &gthpbot[j], &thetapcl_top,
				&sumpos, &sumneg);

			gthpbot[j] = thetapcl_top;

			pd_tmst ( &gthte[j], &ptop, &one, &ftmp, &ier);
			pd_tmkc ( &ftmp, &one, &thetapcl_top, &ier );
			ftmp = thetapcl_top;
		        pd_thta ( &ftmp, &ptop, &one, &thetapcl_top, &ier);

			pd_cape ( &outdat[1], &ztop, &outdat[2],
				&theta_top, &gthpbot[j], &thetapcl_top,
				&sumpos, &sumneg);
			
		    }
		    else {
			if ( plcl < ptop ) {
			    /*
			     * Below LCL, parcel follows dry adiabat (const thta)
			     */
			    thetapcl_top = gthta_bot[j];
			}
			else {
			    /*
			     * Above LCL, parcel follows moist adiabat (const thte)
			     */
			    pd_tmst ( &gthte[j], &ptop, &one, &ftmp, &ier);
			    pd_tmkc ( &ftmp, &one, &thetapcl_top, &ier );
			    ftmp = thetapcl_top;
			    pd_thta ( &ftmp, &ptop, &one, &thetapcl_top, &ier);
			}

			pd_cape ( &gzbot[j], &ztop, &gthbot[j], &theta_top,
				&gthpbot[j], &thetapcl_top,
				&sumpos, &sumneg);
		    }

		    gpbot[j] = ptop;
		    gzbot[j] = ztop;
		    gthbot[j] = theta_top;
		    gthpbot[j] = thetapcl_top;

		    if ( strcmp(parm, "CAPE") == 0 ) {
			gout[j] = gout[j] + sumpos;
		    }
		    else if ( strcmp(parm, "CINS" ) == 0 ) {
			if ( sumpos > 0.0 ) {
			    gout[j] = gout[j] + gneg[j];
			    gneg[j] = sumneg;
			}
			else {
			    gneg[j] = gneg[j] + sumneg;
			}
		    }
		    else {
			*iret = -19;
			return;
		    }
		}
	    }
	}
	else {
	    printf("Missing TMPC or HGHT for %d [%d,%d]\n",lev1[i],ier,ier1);
	}
    }


    dg_updh ( parm, &nout, &nsfcp, &zero, iret );
    dg_puts ( &nout, iret );
    dg_esub ( &nout, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;


}
