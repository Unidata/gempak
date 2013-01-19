#include "gdlist.h"

int main ( void )
/************************************************************************
 * GDLIST								*
 *									*
 * This program lists grid data.					*
 *									*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 4/85						*
 * M. desJardins/GSFC	 4/86	Eliminated GR_DISP			*
 * M. desJardins/GSFC	 5/86	Changed IP_OUTT to IN_OUTT		*
 * M. desJardins/GSFC	 6/88	Final GEMPAK4 version			*
 * M. desJardins/GSFC	11/89	Changed GR_FILE to DG_OFIL		*
 * K. Brill/GSC         12/89   Added call to DG_AREA			*
 * J. Whistler/SSAI	 5/91	Changed output*10 to output*48		*
 * S. Jacobs/EAI         2/94   Added COLADD flag to DG_OFIL            *
 * S. Jacobs/NMC         3/94   Added satellite display routines        *
 * L. Williams/EAI       3/94   Clean up declarations of user input	*
 *				variables				*
 * L. Williams/EAI	 7/94	Removed call to GDLUPD			*
 * D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
 * K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
 *				use filnam in calls to GDLDSP & GDLDTA	*
 * S. Maxwell/GSC        7/97   Increased input character length        *
 * T. Lee/GSC		11/00	Changed calling sequence of GR_FIXA	*
 * S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL 		*
 * T. Lee/GSC		 7/01	Processed multiple files;Added time loop*
 * K. Brill/HPC		 4/03	CALL DG_INTL				*
 * R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
 * R. Tian/SAIC          2/04   Removed nuflg from DG_INTL call         *
 * R. Tian/SAIC         11/04   Changes for time/file mngmnt            *
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 * S. Gilbert/NCEP	 5/07	Added IMISSD to dgc_subg		*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 * K. Brill/HPC         08/09   Do not allow add column for GAREA=GRID;	*
 *				re-initialize DG if GAREA changes	*
 ***********************************************************************/
{
    char time1[21], time2[21], garout[73], satfil[LLMXLN+1], prjout[73],
         timfnd[42], parm[13], pfunc[73], outfil[LLMXLN+1];
    char garprior[73];
    int chngnv=G_TRUE, coladd=G_TRUE, respnd, done, proces, drpflg,
	first, gottm, termflg, fileflg;
    int iscale, iscalv, level1, level2, ivcord, ix1, iy1, ix2, iy2,
        kx, ky;
    int mode=1, zero=0, iperr, ier, irr, iret, maxgrid;
    float *grid, rmin, rmax;
    GDLIST_input ui;
/*---------------------------------------------------------------------*/
/*
 *  Initialize TAE.
 */
    ip_init ( &respnd, &iperr );
    if ( iperr == 0 ) {
	ip_idnt ( "GDLIST", &ier, strlen("GDLIST") );
/*
 *  Initialize GEMPLT.
 *  Note that GEMPLT is only used to translate grid coordinates.
 */
	gg_init ( &mode, &ier );
	if ( ier == 0 ) {
/*
 *  Initialize grid library common area grdcmn.cmn
 */
	    gd_init  ( &ier );
/*
 *  Initialize the DG library.
 */
	    dg_intl ( &ier );
	    done = G_FALSE;
	}
	else {
	    done = G_TRUE;
	}
    }
     else {
	done = G_TRUE;
    }
    strcpy ( garprior, " " );

/*
 * Main loop to read in TAE parameters and list data.
 */
    while ( done == G_FALSE ) {
/*
 * Set flag to indicate processing will be done.
 */
	proces = G_TRUE;

/*
 * Read in the variables from the TAE.
 */
	gdlinp ( &ui, &iperr );

/*
 * Exit if there is an error.
 */
	if ( iperr != 0 ) {
	    done = G_TRUE;
	} else {
/*
 * Check if GAREA == "grid", if so, then set coladd=G_FALSE to NOT add
 * a column to globe wrapping grids.
 */
	    cst_lcuc ( ui.garea, ui.garea, &ier );
	    if ( strncmp ( ui.garea, "GRID", 4 ) == 0 ) {
	        coladd = G_FALSE;
	    } else {
	        coladd = G_TRUE;
	    }
/*
 * Re-initialize the diagnostics if GAREA changes.
 */
	    if ( strcmp ( ui.garea, garprior ) != 0 ) {
		dg_intl ( &ier );
	    }
    	    strcpy ( garprior, ui.garea );
/*
 * Process the GDFILE input.
 */
	    dgc_nfil ( ui.gdfile, "", &ier );
	    if ( ier != 0 ) {
		er_wmsg ( "DG", &ier, " ", &irr, strlen("DG"), strlen(" ") );
		proces = G_FALSE;
	    }

/*
 * Process the GDATTIM input; setup the time server.
 */
	    dgc_ndtm ( ui.gdatim, &ier );
	    if ( ier != 0 ) {
		er_wmsg ( "DG", &ier, ui.gdatim, &irr,
		          strlen("DG"), strlen(ui.gdatim) );
	    	proces = G_FALSE;
	    }

/*
 * Set up the graphics device.
 */
	    gg_sdev ( "GN", &ier, strlen("GN") );
	    if ( ier != 0 ) proces = G_FALSE;

/*
 * Set the attributes that do not vary within the time loop.
 */
	    in_scal ( ui.scale, &iscale, &iscalv, &iret, strlen(ui.scale) );

/*
 * Loop over times.
 */
	    gottm = proces;
	    first = G_TRUE;

	    while ( gottm == G_TRUE ) {
/*
 * Get the next time to process from time server.
 */
		dgc_ntim ( &chngnv, &coladd, time1, time2, &gottm, &ier );
		if ( ier == 0 && gottm == G_TRUE ) {
		    proces = G_TRUE;
		} else {
		    proces = G_FALSE;
		}
		if ( ier != 0 ) {
		    ier = 2;
		    er_wmsg ( "GDLIST", &ier, time1, &irr,
		              strlen("GDLIST"), strlen(time1) );
		}
		if ( strlen(time2) > 0 ) {
		    sprintf ( timfnd, "%s:%s", time1, time2 );
		} else {
		    strcpy ( timfnd, time1 );
		}

/*
 * Set the map projection and grahpics area.
 */
		if ( proces == G_TRUE ) {
		    cst_lcuc ( ui.proj, ui.proj, &ier );
		    if ( strncmp ( ui.proj, "SAT", 3 ) != 0 &&
		         strncmp ( ui.proj, "RAD", 3 ) != 0 ) {
			dgc_fixa ( ui.garea, ui.proj, garout, prjout, &ier );
		    } else {
			strcpy ( prjout, ui.proj );
			strcpy ( garout, ui.garea );
		    }
		    strcpy ( satfil, " " );
		    gg_maps ( prjout, garout, satfil, &drpflg, &iret,
		        strlen(prjout), strlen(garout), strlen(satfil) );
		    if ( iret != 0 ) proces = G_FALSE;
		}

/*
 * Setup the grid subset that covers the graphics area.
 */
		if ( proces == G_TRUE ) {
                    maxgrid = IMISSD;
		    dgc_subg ( "N", &maxgrid, &ix1, &iy1, &ix2, &iy2, &iret );
		    if ( iret != 0 ) {
			er_wmsg ( "DG", &iret, " ", &ier,
			          strlen("DG"), strlen(" ") );
			proces = G_FALSE;
		    }
		}

/*
 * Allocate space for grid.
 */
		if ( proces == G_TRUE ) {
		    dg_kxky ( &kx, &ky, &iret );
		    G_MALLOC ( grid, float, kx * ky, "gdlist - grid"); 
		    if ( grid == NULL ) {
			iret = -73;
			er_wmsg ( "DG", &iret, " ", &ier,
			          strlen("DG"), strlen(" ") );
		        proces = G_FALSE;
		    }
	 	}

/*
 * Compute the requested grid.
 */
		if ( proces == G_TRUE ) {
		    dgc_grid ( timfnd, ui.glevel, ui.gvcord, ui.gfunc, pfunc,
		        grid, &kx, &ky, time1, time2, &level1, &level2, &ivcord,
			parm, &iret );
		    if ( iret != 0 ) {
			er_wmsg ( "DG", &iret, pfunc, &ier,
			          strlen("DG"), strlen(pfunc) );
			proces = G_FALSE;
		    }
		}

/*
 * Compute the scaling factor and scale the grid data.
 */
		if ( proces == G_TRUE ) {
		    grc_sscl ( &iscale, &kx, &ky, &ix1, &iy1, &ix2, &iy2,
		               grid, &rmin, &rmax, &iret );

/*
 * Get the output units.
 */
		    inc_outt ( ui.output, "GDLIST.fil", &termflg, &fileflg,
			       outfil, &iret);

/*
 * Give user a chance to exit.
 */
		    if ( first == G_TRUE ) {
			gdldsp ( ui.gdfile, time1, time2, &level1, &level2,
			    &ivcord, parm, ui.garea, &iscale, &rmin, &rmax,
			    &termflg, &fileflg, &iret );

/*
 * Stop looping if user requests exist.
 */
			if ( iret != 0 ) {
			    proces = G_FALSE;
		 	    gottm = G_FALSE;
                        }
			first = G_FALSE;
		    }
		}

/*
 * List data. 
 */
		if ( proces == G_TRUE ) {
		    gdldta ( ui.gdfile, time1, time2, &level1, &level2, &ivcord,
		        parm, grid, &kx, &ky, ui.garea, &ix1, &iy1, &ix2, &iy2,
			&iscale, &termflg, &fileflg, outfil, &iret );
		}

/*
 * Free grid space.
 */
		if ( proces == G_TRUE ) {
	            G_FREE ( grid, float );
		}
	    }

/*
 * Prompt for next listing to be done.
 */
	    ip_dynm ( &done, &ier );
	}
    }

/*
 * Print general error messages if necessary.
 */
    if ( iperr != 0 ) er_wmsg ( "GDLIST", &iperr, " ", &ier, 6, 1 );

/*
 * Exit from GEMPLT and the interface.
 */
    gendp ( &zero, &iret );
    ip_exit ( &iret );

    return 0;
}
