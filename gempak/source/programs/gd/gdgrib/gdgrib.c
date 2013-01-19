#include "gdgrib.h"

#define MXLPDS	64			/* Maximum PDS length */
#define MXLGDS	128			/* Maximum GDS length */

int main ( void )
/************************************************************************
 * PROGRAM GDGRIB							*
 *									*
 * This programs computes a grid diagnostic and adds it to a GRIB	*
 * file.								*
 *									*
 **									*
 * Log:									*
 * K. Brill/HPC          8/99   Created from GDDIAG			*
 * K. Brill/HPC		11/99	Fix final error message output		*
 * K. Brill/HPC		 2/00	Added "GRIB message written." output	*
 * K. Brill/HPC		 2/00	Added CPYFIL				*
 * K. Brill/HPC		 3/00	Pass NCNTR to GDGWMO; add NAVCHG	*
 * K. Brill/HPC		10/02	LLMXGD*15 -> LLMXGD*6 for grdo		*
 * T. Lee/SAIC		 7/03	Bypass diagnosis for existing grids	*
 * R. Tian/SAIC		 1/04	Added nuflg to DG_INTL call		*
 * R. Tian/SAIC		 2/04	Removed nuflg from DG_INTL call		*
 * R. Tian/SAIC		 4/05	Changed for time/file mgmt		*
 * S. Gilbert/NCEP	 6/05	Added call to GD_RDAT before DG_GRID	*
 *				to be able to read grids > LLMXGD	*
 * T. Lee/SAIC		12/05	Optimized arrays dimension		*
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    char time1[21], time2[21], timfnd[42], firstm[21], lasttm[21],
        pfunc[81], parm[13], cdd[3], chhmm[5], maxg[LLMXLN], anlyss[LLMXLN],
	tmpfil[LLMXLN];
    unsigned char *cbds=NULL, *cbms=NULL, chdr[22], csc0[9], csc5[5],
	cgds[MXLGDS], cpds[MXLPDS];
    float *grid=NULL, anlblk[LLNANL], rnvblk[LLNNAV];
    int respnd, done, proces, chngnv, coladd, gottm, navchg, makbms;
    int ibyts[3], entry, maxgrd, igx, igy, level1, level2, ivcord,
        nbbms, nbgds, nbbds, nbpds, ip10sc, ibase, anlsz, navsz, iperr,
	igpds, ncntr, itot, idx, nb, ii, iret, ier;
    GDGRIB_input ui;
    FILE *gbfptr;
    int ifile=0, mode=1;
/*---------------------------------------------------------------------*/
/*
 *  Initialize TAE.
 */
    ip_init ( &respnd, &iperr );
    if ( iperr == 0 ) {
	ip_idnt ( "GDGRIB", &ier, strlen("GDGRIB") );
/*
 *  Initialize GEMPLT.
 */
	gg_init ( &mode, &ier );
	if ( ier == 0 ) {
/*
 *  Initialize grid library common area grdcmn.cmn
 */
	    gd_init ( &ier );
/*
 *  Initialize the DG library.
 */
	    dg_intl ( &iperr );
	    done  = G_FALSE;
	}
	else {
	    done = G_TRUE;	    
	}
    }
     else {
	done = G_TRUE;
    }

/*
 * Main loop to read in user parameters and compute diagnostics.
 */
    while ( done == G_FALSE ) {
/*
 * Set flag to indicate processing will be done.
 */
	proces = G_TRUE;

/*
 * Read in the variables from the user interface.
 */
	gdguin ( &ui, &iperr );
	if ( iperr != 0 ) break;

/*
 * Open the output GRIB file.
 */
	gbfptr = cfl_aopn ( ui.gbfile, &ier );
	if ( ier != 0 ) {
	    proces = G_FALSE;
	    printf ( "Grib file open failed.\n" );
	}

/*
 * Process GDFILE.
 */
	if ( proces == G_TRUE ) {
	    if ( strlen(ui.cpyfil) > 0 || ( strlen(ui.proj) > 0 &&
	         strlen(ui.kxky) > 0 && strlen(ui.gdarea) > 0 ) ) {
/*
 * Use user specified navigation. Create a temporary grid
 * file for automatic grid interplation.
 */
	        navchg = G_TRUE;
	        strcpy ( maxg, "10" );
	        strcpy ( anlyss, "4/2;2;2;2" );
		sprintf ( tmpfil, "tmpgrd.%d", ++ifile );
	        gdgcfl ( tmpfil, ui.proj, ui.gdarea, ui.kxky, maxg, ui.cpyfil,
	            anlyss, &iret, strlen(tmpfil), strlen(ui.proj),
	            strlen(ui.gdarea), strlen(ui.kxky), strlen(maxg),
		    strlen(ui.cpyfil), strlen(anlyss) );
	        dgc_nfil ( ui.gdfile, tmpfil, &iret );
	        if ( iret == 0 ) remove ( tmpfil );
	    } else {
/*
 * Use input grid navigation.
 */
	        navchg = G_FALSE;
	        dgc_nfil ( ui.gdfile, "", &iret );
	    }
	    if ( iret != 0 ) {
	        er_wmsg ( "DG", &iret, ui.gdfile, &ier, 2, strlen(ui.gdfile) );
	        proces = G_FALSE;
	    }
	}

/*
 * Process GDATIM.
 */
	if ( proces == G_TRUE ) {
	    dgc_ndtm ( ui.gdatim, &iret );
	    if ( iret != 0 ) {
	        er_wmsg ( "DG", &iret, ui.gdatim, &ier, 2, strlen(ui.gdatim) );
	        proces = G_FALSE;
	    }
	}

/*
 * Get time.
 */
	if ( proces == G_TRUE ) {
	    chngnv = G_TRUE;
	    coladd = G_FALSE;
	    dgc_ntim ( &chngnv, &coladd, time1, time2, &gottm, &iret );
	    if ( iret != 0 ) {
	        er_wmsg ( "DG", &iret, " ", &ier, 2, 1 );
	    }
	    if ( gottm == G_FALSE || iperr != 0 ) proces = G_FALSE;
	    if ( strlen ( time2 ) > 0 ) {
	        strcpy ( timfnd, time1 );
	        strcat ( timfnd, time2 );
	    } else {
	        strcpy ( timfnd, time1 );
	    }
	}
	entry = 1;
	dgc_qdtm ( &entry, firstm, lasttm, &iperr );
	anlsz = 0;
	navsz = LLNNAV;
	dg_qref ( &anlsz, &navsz, anlblk, rnvblk, &maxgrd, &iperr );

/*
 * Read the grid directly from the file.
 */
	if ( proces == G_TRUE ) {
	    dg_kxky ( &igx, &igy, &iret );
	    G_MALLOC ( grid, float, igx * igy, "gdgrib - grid" );
	    if ( grid == NULL ) {
		break;
	    }
	    dgc_grid  ( timfnd, ui.glevel, ui.gvcord, ui.gfunc, pfunc, grid,
	        &igx, &igy, time1, time2, &level1, &level2, &ivcord, parm,
		&iret );
	    if ( iret != 0 ) {
	        er_wmsg ( "DG", &iret , pfunc, &ier, 2, strlen(pfunc) );
		proces = G_FALSE;
	    }
	}

/*
 * First generate the GDS.
 */
	if ( proces == G_TRUE ) {
	    nbgds = MXLGDS;
	    gds_mak ( &navchg, rnvblk, &navsz, &nbgds, cgds, &iret );
	    if ( iret != 0 ) {
	        er_wmsg ( "GDGRIB", &iret, " ", &ier, 6, 1 );
	        proces = G_FALSE;
	    }
	}

/*
 * Second, generate the BDS.
 */
	if ( proces == G_TRUE ) {
	    nbbds = igx * igy * 4 + 14;
	    G_MALLOC ( cbds, unsigned char, nbbds, "gdgrib - cbds"); 
	    if ( cbds == NULL ) {
	        break;
	    }
	    bds_mak ( &igx, &igy, ui.precsn, grid, &nbbds, cbds, &ip10sc,
	        &makbms, &iret );
	    if ( iret != 0 ) {
	        er_wmsg ( "GDGRIB", &iret, " ", &ier, 6, 1 );
	        proces = G_FALSE;
	    }
	}

/*
 * Third, generate the BMS, if needed.
 */
	nbbms = 0;
	if ( proces == G_TRUE && makbms == G_TRUE ) {
	    nbbms = igx * igy / 8 + 8;
	    G_MALLOC ( cbms, unsigned char, nbbms, "gdgrib - cbms" );
	    if ( cbms == NULL ) {
	        break;
	    }
	    bms_mak ( grid, &igx, &igy, &nbbms, cbms, &iret );
	    if ( iret != 0 ) {
		er_wmsg ( "GDGRIB", &iret, " ", &ier, 6, 1 );
		proces = G_FALSE;
	    }
	}

/*
 * Now generate the PDS.
 */
	if ( proces == G_TRUE ) {
	    nbpds = MXLPDS;
	    igpds = 255;
	    gdgpds ( ui.pdsval, ui.vercen, rnvblk, parm, &ivcord, &level1,
	        &level2, &makbms, &ip10sc, lasttm, time1, time2, ui.gbtbls,
		&igpds, &nbpds, cpds, cdd, chhmm, &iret );
	    if ( iret != 0 ) {
	        er_wmsg ( "GDGRIB", &iret, " ", &ier, 6, 1 );
	        proces = G_FALSE;
	    }
	}

/*
 * Finally make the WMO header.
 */
	if ( proces == G_TRUE ) {
	    ncntr = cpds[4];
	    gdgwmo ( ui.wmohdr, &ncntr, cdd, chhmm, chdr, &iret );
	    if ( iret < 0 ) {
	        er_wmsg ( "GDGRIB", &iret, " ", &ier, 6, 1 );
	        proces = G_FALSE;
	    } else if ( iret > 0 ) {
	        er_wmsg ( "GDGRIB", &iret, " ", &ier, 6, 1 );
	    }
	}

/*
 * Write the grid to the file.
 */
	if ( proces == G_TRUE ) {
/*
 * Make section 0.
 */
	    itot = 8 + nbpds + nbgds + nbbms + nbbds + 4;
	    csc0[0] = 'G';
	    csc0[1] = 'R';
	    csc0[2] = 'I';
	    csc0[3] = 'B';
	    idx = 4;
	    nb = 3;
            ibase = 256;
	    gdigit ( &itot, &ibase, &nb, ibyts, &iret );
	    if ( iret != 0 ) {
	        iret = -8;
	        er_wmsg ( "GDGRIB", &iret, " ", &ier, 6, 1 );
	        proces = G_FALSE;
	    }
	    for ( ii = 2; ii >= 0; ii-- ) {
	        csc0[idx++] = (unsigned char)( ibyts[ii] );
	    }
	    csc0[idx++] = (unsigned char)(1);
	    strcpy ( (char *)csc5, "7777" );
 
/*
 * Write out all sections, as needed.
 */
	    if ( strlen((char *)chdr) > 0 && proces == G_TRUE ) {
	        cfl_writ ( gbfptr, 21, chdr, &iret );
	    }
	    if ( iret == 0 && proces == G_TRUE ) {
	        cfl_writ ( gbfptr, 8, csc0, &iret );
	    }
	    if ( iret == 0 && proces == G_TRUE ) {
	        cfl_writ ( gbfptr, nbpds, cpds, &iret );
	    }
	    if ( iret == 0 && proces == G_TRUE ) {
	        cfl_writ ( gbfptr, nbgds, cgds, &iret );
	    }
	    if ( iret == 0 && makbms == G_TRUE && proces == G_TRUE ) {
	        cfl_writ ( gbfptr, nbbms, cbms, &iret );
	    }
	    if ( iret == 0 && proces == G_TRUE ) {
	        cfl_writ ( gbfptr, nbbds, cbds, &iret );
	    }
	    if ( iret == 0 && proces == G_TRUE ) {
	        cfl_writ ( gbfptr, 4, csc5, &iret );
	    }
	    if ( iret == 0 && proces == G_TRUE ) {
	        printf ( " GRIB message written.\n" );
	    }
	}

/*
 * free space.
 */
	G_FREE ( grid, float );
	G_FREE ( cbds, unsigned char );
	G_FREE ( cbms, unsigned char );

/*
 * Close grib file.
 */
        if ( gbfptr ) cfl_clos ( gbfptr, &ier );

/*
 * Prompt for next diagnostic to be done.
 */
	ip_dynm ( &done, &ier );
    }

/*
 * Print general error messages if necessary.
 */
    if ( iperr != 0 ) er_wmsg ( "GDGRIB", &iperr, " ", &ier, 6, 1 );
    ip_exit ( &ier );

    return 0;
}
