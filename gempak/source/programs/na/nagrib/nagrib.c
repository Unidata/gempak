#include "nagrib.h"

int main ( void )
/************************************************************************
 * NAGRIB								*
 *									*
 * This program will convert GRIB grid data to GEMPAK grid data.	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/EAI	 7/93						*
 * S. Jacobs/EAI	10/93	Added igx & igy to GB_GPBD call		*
 * S. Jacobs/EAI	11/93	Changed GDS info to real numbers	*
 * S. Jacobs/EAI	12/93	Send edition and center number		*
 *				   from PDS to NA_RHDR			*
 * L. Williaims/EAI	 7/94	Removed call to NAGUPD			*
 * K. Brill/NMC		 4/95	Changes for areal subsetting		*
 * S. Jacobs/NMC	 5/95	Added subset flag to NAGCOG		*
 * L. Sager/NMC		 8/95	Passed x grid size to NAGSSG		*
 * K. Brill/NMC		12/95	CALL GB_GBDH; irltv = TRUE_C		*
 *				& nbits > 1 if not repacking		*
 * D.W.Plummer/NCEP	 4/96	Added ensemble processing and		*
 * 				GBTBLS and GBDIAG capabilities		*
 * K. Brill/NMC		 5/96	Added fill for staggered grids  	*
 * D.W.Plummer/NCEP	 8/96	Temp ECMWF processing fix		*
 * S. Maxwell/GSC        7/97   Increased input character length	*
 * K. Brill/EMC		 9/98	Add fill for stggrd grd typ 203 	*
 * S. Jacobs/NCEP	 1/99	Added check for grid too large		*
 * S. Jacobs/NCEP	 1/99	Increased grid array size		*
 * D.W.Plummer/NCEP	 3/00	Added subarea option to CPYFIL and	*
 *				added call to GD_GENI; clean up luns	*
 * S. Jacobs/NCEP	 2/01	Fixed max number of grids to MMHDRS-1	*
 * D.W.Plummer/NCEP	 5/01	Add PDSEXT parameter (YES or NO)	*
 * Piper/CDB;Brill/HPC	 1/02	Trap IGX*IGY>MXGRIB; make MXGRIB bigger *
 *				Add common NOSOF to prevent SOF		*
 * K. Brill/HPC		11/02	Use LLMXTG to set MXGRIB parameter	*
 * M. Li/SAIC		04/04	Added ihzrmp, and idrct			*
 * S. Gilbert/NCEP      12/04   Moved some NA routines from local dir   *
 *                              to NA_ in gemlib/na, since they will be *
 *                              used by both nagrib and nagrib2.        *
 * m.gamazaychikov/SAIC	09/05	Added overwr to CS of NAGINP, GD_WPPG,	*
 *				and GD_WPGD				*
 * R. Tian/SAIC		 8/06	Recoded from Fortran			*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 * F. Y. Yen/NCEP	07/08	Fix grid & grdhv dimensions in G_MALLOCs*	
 * M. Pyle/EMC		 9/08	Set flag for grd typ 205        	*
 ***********************************************************************/
{
    int ighdr[LLGDHD], jtime[3], jlevel[2], ksbx[2], ksby[2],
        igdlst[MMHDRS], idbtmp[MMHDRS], jaccm, jvcord, jparm, jgrdnm,
	jcodtbl, jcenter, jisgds, jisbms, jpdse, ictlen, level1, level2,
	ivcord, iuscal, ihzrmp, idrct, igrdnm, igx, igy, jx, jy, igdfln,
	igxold, modknt, length, ibflag, i2scl, nbits, lengrd, iedtn, kkx,
	kky, iqx, iqy, inx, iny, kkmsg, lenfil, lenidx, knt, kntmsg;
    int respnd, done, proces, cont, lstflg, prttl, gsflag, trape0,
        subset, diaflg, dialst, fill, savhv, fil203, termflg, fileflg,
	irltv, nrmflg, misflg;
    int nfps, maxg, num, ii, iperr, ier, iret, ierr;
    float rmsval, gdsarr[10], rnvblk[LLNNAV], bltln[4], rnvtst[LLNNAV],
        ref, scale, difmin;
    float *grid, *grdhv;
    char outfil[LLMXLN], cpyf[2][81], gbdarr[2][73], *cdp[4], gbdsec[73],
         gbdlst[73], gdotmp[73], gbdtmp[73], tbls[4][73], ctmp[33],
	 cpds[14], gdattm1[21], gdattm2[21], parm[13], tmpprm[11],
	 cprj[25], cprjt[25];
    NAGRIB_input ui;
    FILE *fps[MMFILE], *fp;
    int *igrid=NULL, ipktyp=MDGGRB, mode=1, navsz=LLNNAV;
/*----------------------------------------------------------------------*/
/*
 *  Initialize TAE.
 */
    ip_init ( &respnd, &iperr );
    if ( iperr == 0 ) {
	ip_idnt ( "NAGRIB", &ier, strlen("NAGRIB") );
/*
 *  Initialize GEMPLT.
 */
	gg_init ( &mode, &ier );
	if ( iperr == 0 ) {

/*
 *  Initialize grid library common area grdcmn.cmn.
 */
	    gd_init  ( &ier );

/*
 *  Initialize the NA common block.
 */
	    na_init ( &ier );
	    done   = G_FALSE;
	}
	else {
	    done = G_TRUE;
	}
    } else {
	done = G_TRUE;
    }

/*
 *  Process grids until the user is finished.
 */
    while ( done == G_FALSE ) {
	for ( ii = 0; ii < LLGDHD; ii++ ) ighdr[ii] = 0;

/*
 *  Get the user input.
 */
	naginp ( &ui, &iperr );
	if ( iperr != 0 ) {
	    done   = G_TRUE;
	    proces = G_FALSE;
	} else {
	    proces = G_TRUE;
	}

	igdfln = 0;
	lstflg = G_FALSE;
	fill   = G_FALSE;
	fil203 = G_FALSE;
	savhv  = G_FALSE;

	if ( proces == G_TRUE ) {

/*
 * Set the output devices.
 */
	    inc_outt ( ui.output, "NAGRIB.fil", &termflg, &fileflg,
	        outfil, &ier );
	    nfps = 0;
	    if ( termflg == G_TRUE ) {
	        fps[nfps++] = stdout;
	    }
	    if ( fileflg == G_TRUE ) {
	        fp = cfl_wopn ( outfil, &ier );
	        fps[nfps++] = fp;
	    }

/*
 * Set the maximum number of grids from the user input.
 */
	    cst_numb ( ui.maxgrd, &maxg, &ier );
	    if ( maxg > MMHDRS-1 ) {
		for ( ii = 0; ii < nfps; ii++ ) {
		    fprintf ( fps[ii], " WARNING : Resetting MAXGRD value"
		        " to maximum limit (%d)\n", MMHDRS-1 );
		}
		maxg = MMHDRS - 1;
	    }

/*
 * Split CPYFIL using '|' to get optional subarea.
 * Override GAREA if necessary.
 */
	    for ( ii = 0; ii < 2; ii++ ) cdp[ii] = cpyf[ii];
	    cst_clst ( ui.cpyfil, '|', "", 2, 80, cdp, &num, &ier );
	    strcpy ( ui.cpyfil, cpyf[0] );
	    if ( cpyf[1][0] != '\0' ) strcpy ( ui.garea, cpyf[1] );

/*
 * Setup GBDIAG information.
 */
	    for ( ii = 0; ii < 2; ii++ ) cdp[ii] = gbdarr[ii];
	    cst_clst ( ui.gbdiag, '|', "", 2, 72, cdp, &num, &ier );
	    cst_lcuc ( gbdarr[0], gbdsec, &ier );
	    strcpy ( gbdlst, gbdarr[1] );
	    if ( gbdsec[0] == '\0' ) {
		for ( ii = 0; ii < MMHDRS; ii++ ) igdlst[ii] = 0;
		dialst = G_FALSE;
	    } else if ( gbdlst[0] == '\0' ) {
		for ( ii = 0; ii < maxg; ii++ ) igdlst[ii] = ii + 1;
		dialst = G_TRUE;
	    } else {
		cst_ilst ( gbdlst, ';', 0, maxg, idbtmp, &num, &ier );
		for ( ii = 0; ii < MMHDRS; ii++ ) igdlst[ii] = 0;
		for ( ii = 0; ii < num; ii++ ) {
		    if ( idbtmp[ii] > 0 && idbtmp[ii] <= maxg ) {
			igdlst[idbtmp[ii]-1] = idbtmp[ii];
		    }
		}
		dialst = G_TRUE;
	    }

/*
 * Setup GBTBLS information
 */
	    for ( ii = 0; ii < 4; ii++ ) cdp[ii] = tbls[ii];
	    cst_clst ( ui.gbtbls, ';', "?", 4, 72, cdp, &num, &ier );

/*
 * Open the GRIB file.
 */
	    lenfil = strlen ( ui.gbfile );
	    lenidx = strlen ( ui.indxfl );
	    gb_open ( ui.gbfile, &lenfil, ui.indxfl, &lenidx, &iret );
	    if ( iret != 0 ) {
		er_wmsg ( "NAGRIB", &iret, " ", &ier,
		          strlen("NAGRIB"), strlen(" ") );
		proces = G_FALSE;
	    }
	    cst_lcuc ( ui.gdoutf, gdotmp, &iret );
	    lstflg = G_FALSE;
	    if ( strcmp ( gdotmp, "LIST" ) == 0 ) lstflg = G_TRUE;

/*
 * Read in grids to be processed until the end of file.
 */
	    trape0 = G_FALSE;
	    knt = 0;
	    kntmsg = 0;
	    while ( proces == G_TRUE && knt < maxg ) {

/*
 * First set GBDIAG info on or off.
 */
		if ( igdlst[kntmsg++] != 0 ) {
		    strcpy ( gbdtmp, gbdsec );
		    diaflg = G_FALSE;
		} else {
		    gbdtmp[0] = '\0';
		    diaflg = G_FALSE;
		}
		gb_diag ( gbdtmp, &iret );

/*
 * Set the start byte of the next GRIB message. If the
 * end of the file has been reached stop processing.
 */
		gb_next ( &iedtn, &iret );
		
/*
 * Check the GRIB edition. Currently, only set the
 * return code to +5. Decide later whether or not to
 * decode GRIB 0.
 */
		if ( iedtn == 0 ) {
		    if ( trape0 == G_FALSE ) {
			er_wmsg ( "NAGRIB", &iret, " ", &ier,
			          strlen("NAGRIB"), strlen(" ") );
			trape0 = G_TRUE;
		    }
		}

/*
 * Determine if the program should continue or not.
 */
		if ( iret < 0 ) {
		    proces = G_FALSE;
		} else if ( iret > 0 ) {
		    proces = G_TRUE;
		    cont   = G_FALSE;
		} else {
		    proces = G_TRUE;
		    cont   = G_TRUE;
		}

/*
 * If there is a next grid, continue processing.
 */
		if ( proces == G_TRUE ) {

/*
 * Get the PDS information from the GRIB file.
 */
		    if ( cont == G_TRUE ) {
			gb_gpds ( jtime, &jaccm, jlevel, &jvcord, &jparm,
			    &jgrdnm, &jcodtbl, &jcenter, &jisgds, &jisbms, 
			    &jpdse, ctmp, &ictlen, &iret );
			if ( iret != 0 ) {
			    er_wmsg ( "NAGRIB", &iret, " ", &ier,
			              strlen( "NAGRIB"), strlen(" ") );
			    cont = G_FALSE;
			}
		    }

/*
 * Convert the GRIB info to GEMPAK header info.
 */
		    if ( cont == G_TRUE ) {

/*
 * Read the GRIB decoding tables.
 */
			na_rtbl ( &iedtn, &jcenter, &jcodtbl, tbls[0],
			          tbls[1], tbls[2], tbls[3], &iret );
			if ( iret != 0 ) cont = G_FALSE;
		    }

		    if ( cont == G_TRUE ) {
			strncpy ( cpds, ctmp, ictlen );
			cpds[ictlen] = '\0';
			na_rhdr ( jtime, &jaccm, &jlevel[0], &jlevel[1],
			    &jvcord, &jparm, &jcodtbl, &ui.pdsext, gdattm1,
			    gdattm2, &level1, &level2, &ivcord, parm, &iuscal,
			    &rmsval, &jpdse, cpds, &ihzrmp, &idrct, &iret );
			ighdr[0] = ihzrmp;
			ighdr[1] = idrct;
			if ( iret != 0 ) {
			    if ( iret == 2 ) {
				cst_inch ( jvcord, tmpprm, &ier );
			    } else if ( iret == 3 ) {
				cst_inch ( jparm, tmpprm, &ier );
			    } else {
				tmpprm[0] = '\0';
			    }
			    er_wmsg ( "NAGRIB", &iret, tmpprm, &ier,
			              strlen("NAGRIB"), strlen(tmpprm) );
			    cont = G_FALSE;
			}
		    }

/*
 * If the GDS is in the GRIB message, read the information.
 */
		    for ( ii = 0; ii < 10; ii++ ) gdsarr[ii] = 0.;
		    irltv  = G_FALSE;
		    nrmflg = G_TRUE;
		    if ( cont == G_TRUE && jisgds != G_FALSE ) {
			gb_ggds ( gdsarr, &irltv, &nrmflg, &iret );
			if ( iret != 0 ) {
			    er_wmsg ( "NAGRIB", &iret, " ", &ier,
			        strlen("NAGRIG"), strlen(" ") );
			    cont = G_FALSE;
			}
		    }

/*
 * Check for European Center grid. Force processing
 * to be normal until flags get set properly in preprocessing.
 */
		    if ( jcenter == 98 ) nrmflg = G_FALSE;

/*
 * Check for staggered grid.
 */
		    if ( (int)gdsarr[0] == 201 ) {
			fill = G_TRUE;
			nrmflg = G_TRUE;
		    } else if ( (int)gdsarr[0] == 203 ) {
			fill = G_TRUE;
			fil203 = G_TRUE;
			nrmflg = G_TRUE;
		    } else if ( (int)gdsarr[0] == 205 ) {
			nrmflg = G_TRUE;
		    }

/*
 * Create/open the GEMPAK grid file, if necessary.
 * Determine if the user only wants a LIST of the grids.
 */
		    if ( cont == G_TRUE ) {
			if ( lstflg == G_FALSE && igdfln == 0 ) {
			    na_gcog ( ui.gdoutf, ui.proj, ui.grdarea,
			        ui.kxky, ui.cpyfil, &fill, ui.garea,
				gdsarr, &maxg, &igdfln, rnvblk, &igrdnm,
				cprj, &igx, &igy, &jx, &jy, ksbx, ksby,
				&subset, &iret );
			    igxold= (int)rnvblk[4];
			    if ( fill == G_TRUE ) igxold = 999999;
			    if ( iret != 0 ) {
				er_wmsg ( "NAGRIB", &iret, " ", &ier,
				          strlen("NAGRIB"), strlen(" ") );
				cont   = G_FALSE;
				proces = G_FALSE;
			    }
			}
		    }

/*
 * List the header information, or write the data to the GEMPAK file.
 */
		    if ( cont == G_TRUE ) {
			if ( lstflg == G_TRUE ) {
			    if ( dialst == G_FALSE ||
			         ( dialst == G_TRUE && diaflg == G_TRUE ) ) {
				/*
				 * List the header.
				 */
				knt++;
				prttl = G_FALSE;
				modknt = kntmsg % 100;
				if ( knt == 1 || modknt == 1 || modknt == 51 )
				    prttl = G_TRUE;
				if ( nfps != 0 ) {
				    grc_wnmc ( fps, &nfps, &prttl, &kntmsg,
				        gdattm1, gdattm2, &level1, &level2,
					&ivcord, parm, &jparm, &jvcord,
					&jgrdnm, &ier );
			        }
			    }
			} else {

/*
 * Determine if the grid is to be processed.
 */
			    gsflag = G_FALSE;
			    if ( igrdnm == jgrdnm ) {
				gsflag = G_TRUE;
			    } else if ( jisgds != G_FALSE ) {
				na_ggds ( gdsarr, cprjt, &kkx, &kky, bltln,
				    rnvtst, &ier );
				grc_cnav ( rnvblk, rnvtst, &navsz, &gsflag,
				    &ier );
			    }

/*
 * If it is OK, process the grid.
 */
			    if ( gsflag == G_TRUE ) {
				gb_gbdh ( &length, &ibflag, &i2scl, &ref,
				    &nbits, &iret );
				if ( ( jisbms == G_FALSE ) &&
				     ( nrmflg == G_TRUE ) &&
				     ( nbits > 1 ) &&
				     ( subset == G_FALSE ) &&
				     ( fill == G_FALSE ) &&
				     ( ( irltv == G_TRUE ) ||
				       ( strcmp ( cprj, "CED" ) == 0 ) ||
				       ( strcmp ( cprj, "MER" ) == 0 ) ) ) {

/*
 * Read packed data from GRIB file and write to the GEMPAK file.
 */
				    G_MALLOC ( igrid, int, igx * igy, "nagrib - igrid");
				    gb_gpbd ( &igx, &igy, &iuscal, &ref,
					&scale, &nbits, igrid, &lengrd, &iret );
				    if ( iret != 0 ) {
					er_wmsg ( "NAGRIB", &iret, " ", &ier,
					    strlen("NAGRIB"), strlen(" ") );
					cont = G_FALSE;
				    }

				    if ( cont == G_TRUE ) {
					misflg = G_FALSE;
					difmin = 0.0;
					cgd_wppg ( &igdfln, igrid, &lengrd,
					    &igx, &igy, ighdr, gdattm1, gdattm2,
					    &level1, &level2, &ivcord, parm,
					    &ui.overwr, &ipktyp, &nbits,
					    &misflg, &ref, &scale, &difmin,
					    &ierr );
					if ( ierr == 0 ) {
					    knt++;
					    prttl = G_FALSE;
					    modknt = kntmsg % 100;      
					    if ( knt == 1 || modknt == 1 ||
						modknt == 51 ) prttl = G_TRUE;
					    grc_wnmc ( fps, &nfps, &prttl,
						&kntmsg, gdattm1, gdattm2,
						&level1, &level2, &ivcord,
						parm, &jparm, &jvcord, &jgrdnm,
						&ier );
					} else {
					    er_wmsg ( "GD", &ierr, " ", &ier,
					        strlen("GD"), strlen(" ") );
					    if ( ierr == -11 ) proces = G_FALSE;
					}
				    }
				    G_FREE ( igrid, int );
				} else {

/*
 * Read and unpack data from GRIB file and write to the GEMPAK file.
 */
				    if ( fill == G_TRUE ) {
					iqx = jx;
					iqy = jy;
				    } else {
					iqx = igx;
					iqy = igy;
				    }
				    G_MALLOC ( grid, float, igx * igy, "nagrib - grid" );
				    G_MALLOC ( grdhv, float, igx * igy, "nagrib - grdhv");
				    gb_gubd ( &iqx, &iqy, &iuscal, &jisbms,
				        &rmsval, &nrmflg, &irltv, &nbits, grid,
					&iret );
				    if ( iret != 0 ) {
					er_wmsg ( "NAGRIB", &iret, " ", &ier,
					    strlen("NAGRIB"), strlen(" ") );
					cont = G_FALSE;
				    }
				    if ( fill == G_TRUE && cont == G_TRUE ) {
					if ( fil203 == G_TRUE )
					    nagcut ( grid, &igx, &igy, parm,
						grid, &ier );
					nagfil ( grid, &igx, &igy, parm, grid,
					    grdhv, &ier );
				    }
				    inx = igx;
				    iny = igy;
				    if ( subset == G_TRUE && cont == G_TRUE ) {
					na_gssg ( &igxold, ksbx, ksby, grid,
					    &inx, &iny, &iret );
					if ( fill == G_TRUE &&
					     savhv == G_FALSE ) {
					    inx = igx;
					    iny = igy;
					    na_gssg ( &igxold, ksbx, ksby,
					        grdhv, &inx, &iny, &iret );
					}
				    }
				    if ( cont == G_TRUE ) {
					cgd_wpgd ( &igdfln, grid, &inx, &iny,
					    ighdr, gdattm1, gdattm2, &level1,
					    &level2, &ivcord, parm, &ui.overwr,
					    &ipktyp, &nbits, &ierr );
					if ( ierr == 0 ) {
					    knt++;
					    prttl = G_FALSE;
					    modknt = kntmsg % 100;      
					    if ( knt == 1 || modknt == 1 ||
						 modknt == 51 ) prttl = G_TRUE;
					    grc_wnmc ( fps, &nfps, &prttl,
						&kntmsg, gdattm1, gdattm2,
						&level1, &level2, &ivcord,
						parm, &jparm, &jvcord, &jgrdnm,
						&ier );
					} else {
					    er_wmsg ( "GD", &ierr, " ", &ier,
					        strlen("GD"), strlen(" ") );
					    if ( ierr == -11 ) proces = G_FALSE;
					}
				    }
				    if ( cont == G_TRUE && fill == G_TRUE &&
					savhv == G_FALSE &&
				        ( jtime[2] == 100000 ||
					  jtime[2] == 0 ) ) {
					savhv = G_TRUE;
					level1 = 0;
					level2 = -1;
					ivcord = 0;
					strcpy ( parm, "HV10" );
					cgd_wpgd ( &igdfln, grdhv, &inx, &iny,
					    ighdr, gdattm1, gdattm2, &level1,
					    &level2, &ivcord, parm, &ui.overwr,
					    &ipktyp, &nbits, &ierr );
					if ( ierr == 0 ) {
					    kkmsg = 0;
					    prttl = G_FALSE;
					    if ( knt == 1 ) prttl = G_TRUE;
					    grc_wnmc ( fps, &nfps, &prttl,
						&kkmsg, gdattm1, gdattm2,
						&level1, &level2, &ivcord,
						parm, &jparm, &jvcord, &jgrdnm,
						&ier );
					} else {
					    er_wmsg ( "GD", &ierr, " ", &ier,
					        strlen("GD"), strlen(" ") );
					}
				    }
				    G_FREE ( grid, float );
				    G_FREE ( grdhv, float );
				}
			    }
			}
		    }
		}
	    }

/*
 * Write the output information to the luns.
 */
	    if ( nfps > 0 ) {
		for ( ii = 0; ii < nfps; ii++ ) {
		    if ( proces == G_TRUE ) {
		        fprintf ( fps[ii], "\n\n     %5d"
			    " GRIB messages were read or scanned from the"
			    " GRIB file:\n", kntmsg );
		    } else {
		        fprintf ( fps[ii], "\n\n     %5d"
			    " GRIB messages were read or scanned from the"
			    " GRIB file:\n", G_MAX(0,kntmsg-1) );
		    }
		    fprintf ( fps[ii], "     %-72.72s\n\n", ui.gbfile );
		    if ( lstflg == G_FALSE ) {
		        fprintf ( fps[ii], "     %5d"
			    " grids were written to the GEMPAK file:\n"
			    "     %-72.72s\n\n", knt, ui.gdoutf );
		    }
		}
	    }

/*
 * Update parameter values, display grid info and close the files.
 */
	    if ( igdfln != 0 ) {
		gd_clos ( &igdfln, &ier );
	   	if ( nfps != 0 )  grc_geni ( ui.gdoutf, fps, &nfps, &ier );
	    }
	    gb_clos ( &ier );

/*
 * Close file output 
 */
	    if ( fileflg == G_TRUE ) cfl_clos ( fp, &ier );
	}

/*
 * Call dynamic tutor.
 */
	ip_dynm ( &done, &ier );
    }

/*
 * Final error messages.
 */
    if ( iperr != 0 ) er_wmsg ( "NAGRIB", &iperr, " ", &ier,
        strlen("NAGRIB"), strlen(" ") );
    ip_exit ( &iret );

    return 0;
}
