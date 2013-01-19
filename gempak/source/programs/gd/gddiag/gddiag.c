#include "gddiag.h"

int main ( void )
/************************************************************************
 * PROGRAM GDDIAG							*
 *									*
 * This programs computes a grid diagnostic and adds it to a grid	*
 * file.								*
 *									*
 **									*
 * Log:									*
 * M. Goodman/RDS	11/85						*
 * M. desJardins/GSFC	 8/88	GEMPAK4					*
 * K. Brill/GSC          3/90   Changed call sequence for GDGOPN	*
 * K. Brill/NMC          8/90   Replaced GDGOPN with DG_OFIL		*
 * S. Jacobs/EAI         2/94   Added COLADD flag to DG_OFIL            *
 * L. Williams/EAI	 7/94	Removed call to GDGUPD			*
 * K. Tyle/GSC		 8/96	Added FL_MFIL to search for file type	*
 * S. Maxwell/GSC        7/97   Increased input character length        *
 * S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL 		*
 * T. Lee/GSC		 7/01	Processed multiple files		*
 * T. Lee/SAIC		10/01	Called DG_CLAL				*
 * K. Brill/HPC		 4/03	CALL DG_INTL				*
 * R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
 * R. Tian/SAIC          2/04   Removed nuflg from DG_INTL call         *
 * M. Li/SAIC		04/04	Added new parameter grdhdr		*
 * R. Tian/SAIC		 1/05	Time/file mgmt, file create, vector grid*
 * R. Tian/SAIC          9/06   Recoded from Fortran                    *
 * S. Gilbert/NCEP      10/06   Added another ER_WMSG after GDGCFL      *
 * R. Tian/SAIC		10/06	Fixed existing program after error	*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    char fname[LLMXLN], time1[42], time2[42], trange[37], timfnd[37],
         grdtyp[LLMXLN], pfunc[81], parm[13], parm2[13], gname[13];
    int respnd, done, proces, gottm, both, chngnv, coladd;
    int level1, level2, ivcord, igx, igy, iarhdr[2], ihzrmp, idrct;
    int mode, two, ntms, nn, iperr, ier, ier1, ier2;
    long flen;
    float *grid, *grid2;
    GDDIAG_input ui;
/*---------------------------------------------------------------------*/
    mode = 1;
    two = 2;
    grid = NULL;
    grid2 = NULL;
    both = G_FALSE;
    chngnv = G_TRUE;
    coladd = G_FALSE;

/*
 *  Initialize TAE.
 */
    ip_init ( &respnd, &iperr );
    if ( iperr == 0 ) {
	ip_idnt ( "GDDIAG", &ier, strlen("GDDIAG") );
/*
 *  Initialize GEMPLT.
 */
	gg_init ( &mode, &ier );
	if ( ier == 0 ) {
/*
 *  Initialize grid library common area grdcmn.cmn.
 */
	    gd_init ( &iperr );

/*
 *  Initialize the DG library.
 */
	    dg_intl ( &iperr );
	    done = G_FALSE;
	}
	else {
	    done = G_TRUE;
	}
    }
    else {
	done = G_TRUE;
    }

/*
 *  Main loop to read in TAE parameters and compute diagnostics.
 */
    while ( done == G_FALSE ) {
/*
 * Set flag to indicate processing will be done.
 */
	proces = G_TRUE;

/*
 * Read in the variables from the TAE.
 */
	gdginp ( &ui, &iperr );

/*
 * Exit if there is an error.
 */
	if ( iperr != 0 ) {
	    done = G_TRUE;
	} else {
/*
 * If 'gdoutf' not exist, create it. 
 */
	    cfl_inqr ( ui.gdoutf, NULL, &flen, fname, &iperr );
	    if ( iperr != 0 ) {
		gdgcfl ( ui.gdoutf, ui.proj, ui.gdarea, ui.kxky, ui.maxgrd,
		    ui.cpyfil, ui.anlyss, &iperr,
		    strlen(ui.gdoutf), strlen(ui.proj), strlen(ui.gdarea),
		    strlen(ui.kxky), strlen(ui.maxgrd), strlen(ui.cpyfil),
		    strlen(ui.anlyss) );
		if ( iperr != 0 ) {
		    er_wmsg ( "GDCFIL", &iperr, " ", &ier,
		        strlen("GDCFIL"), strlen(" ") );
                    iperr = -5;
		    er_wmsg ( "GDDIAG", &iperr, " ", &ier,
		        strlen("GDDIAG"), strlen(" ") );
		    proces = G_FALSE;
		}
	    }

/*
 * Process the GDFILE and GDOUTF input.
 */
	    if ( proces == G_TRUE ) {
		dgc_nfil ( ui.gdfile, ui.gdoutf, &iperr );
		if ( iperr != 0 ) {
		    er_wmsg ( "DG", &iperr, " ", &ier,
		        strlen("DG"), strlen(" ") );
		    proces = G_FALSE;
		}
	    }

/*
 * Process the GDATTIM input.
 */
	    if ( proces == G_TRUE ) {
		dgc_ndtm ( ui.gdatim, &iperr );
		if ( iperr != 0 ) {
		    er_wmsg ( "DG", &iperr, " ", &ier,
		        strlen("DG"), strlen(" ") );
		    proces = G_FALSE;
		} else {
		    dgc_qtms ( &two, &both, time1, &ntms, trange, &iperr );
		    if ( iperr != 0 || ntms > 1 ) proces = G_FALSE;
		}
	    }

/*
 * Get the time to process.
 */
	    if ( proces == G_TRUE ) {
		dgc_ntim ( &chngnv, &coladd, time1, time2, &gottm, &iperr );
		if ( iperr != 0 || gottm == G_FALSE ) {
		    er_wmsg ( "DG", &iperr, " ", &ier,
		        strlen("DG"), strlen(" ") );
		    proces = G_FALSE;
		} else {
		    if ( strlen(time2) > 0 ) {
			sprintf ( timfnd, "%s:%s", time1, time2 );
		    } else {
			strcpy ( timfnd, time1 );
		    }
		}
	    }

/*
 * Compute the new grid.
 */
	    cst_lcuc ( ui.grdtyp, grdtyp, &iperr );
	    if ( proces == G_TRUE ) {
		if ( grdtyp[0] == 'S' || grdtyp[0] == '\0' ) {
/*
 * Allocate space for grid.
 */
		    dg_kxky ( &igx, &igy, &iperr );
		    G_MALLOC ( grid, float, igx * igy, "GDDIAG - grid");
		    if ( grid == NULL ) {
			iperr = -73;
			er_wmsg ( "DG", &iperr, " ", &ier,
                            strlen("DG"), strlen(" ") );
			proces = G_FALSE;
		    }

/*
 * Compute the scalar diagnostic grid.
 */
		    if ( proces == G_TRUE ) {	
			dgc_grid ( timfnd, ui.glevel, ui.gvcord, ui.gfunc,
			    pfunc, grid, &igx, &igy, time1, time2, &level1,
			    &level2, &ivcord, parm, &iperr );
			if ( iperr != 0 ) {
			    er_wmsg ( "DG", &iperr, pfunc, &ier,
			        strlen("DG"), strlen(pfunc) );
			    proces = G_FALSE;
			} else {
/*
 * Write the grid to the file.
 */
			    cst_ilst ( ui.grdhdr, '/', IMISSD, 2, iarhdr, &nn,
			        &iperr );
		            ihzrmp = iarhdr[0];
		            idrct  = iarhdr[1];
			    gdgwrt ( grid, time1, time2, &level1, &level2,
			        &ivcord, parm, ui.grdnam, ui.gpack, &ihzrmp,
				&idrct, &iperr );
		            if ( iperr != 0 ) proces = G_FALSE;
			}
		    }

/*
 * Free grid space.
 */
		    G_FREE(grid, float);
		} else if ( grdtyp[0] == 'V' ) {
/*
 * Allocate space for grid.
 */
		    dg_kxky ( &igx, &igy, &iperr );
		    G_MALLOC ( grid,  float, igx * igy, "GDDIAG:  grid"); 
		    G_MALLOC ( grid2, float, igx * igy, "GDDIAG:  grid2");
		    if ( grid == NULL || grid2 == NULL ) {
			iperr = -73;
			er_wmsg ( "DG", &iperr, " ", &ier,
                            strlen("DG"), strlen(" ") );
			proces = G_FALSE;
		    }

/*
 * Compute the vector diagnostic grid.
 */
		    if ( proces == G_TRUE ) {
		        dgc_vecr ( timfnd, ui.glevel, ui.gvcord, ui.gfunc,
			    pfunc, grid, grid2, &igx, &igy, time1, time2,
			    &level1, &level2, &ivcord, parm, parm2, &iperr );
		        if ( iperr != 0 ) {
			    er_wmsg ( "DG", &iperr, pfunc, &ier,
			        strlen("DG"), strlen(pfunc) );
			    proces = G_FALSE;
			} else {
/*
 * Write the grid to the file.
 */
			    cst_ilst ( ui.grdhdr, '/', IMISSD, 2, iarhdr, &nn,
			        &iperr);
		            ihzrmp = iarhdr[0];
		            idrct  = iarhdr[1];
			    strcpy ( gname, "U" );
			    strcat ( gname, ui.grdnam );
		            gdgwrt ( grid, time1, time2, &level1, &level2,
			        &ivcord, parm, gname, ui.gpack, &ihzrmp,
				&idrct, &ier1 );
			    strcpy ( gname, "V" );
			    strcat ( gname, ui.grdnam );
		            gdgwrt ( grid2, time1, time2, &level1, &level2,
			        &ivcord, parm2, gname, ui.gpack, &ihzrmp,
				&idrct, &ier2 );
		            if ( ier1 != 0 || ier2 != 0 ) proces = G_FALSE;
		        }
		    }

/*
 * Free grid space.
 */
		    G_FREE(grid, float);
		    G_FREE(grid2, float);
		} else {
/*
 * Invalid 'grdtyp' input.
 */
		    iperr = -14;
		    er_wmsg ( "GDDIAG", &iperr, " ", &ier,
		        strlen("GDDIAG"), strlen(" ") );
		    proces = G_FALSE;
		}
	    }

/*
 * Prompt for next diagnostic to be done.
 */
	    ip_dynm ( &done, &iperr );
	}
    }

/*
 * Print general error messages if necessary.
 */
    if ( iperr != 0 ) er_wmsg ( "GDDIAG", &iperr, " ", &ier, 6, 1 );
    ip_exit ( &iperr );

    return 0;
}
