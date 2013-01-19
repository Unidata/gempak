#include "ensdiag.h"

void de_scan ( const int *nina, int *iret )
/************************************************************************
 * de_scan								*
 *									*
 * This subroutine scans the ALLARG array in DECMN.CMN to determine 	*
 * which GDFILE entry has the required ensemble specification.  The 	*
 * allarg array is filled in an ENS_ function subroutine by calling	*
 * ST_CLST to parse out the input arguments before calling DE_SCAN.	*
 * Having found the ensemble, DE_SCAN builds the list of file names	*
 * or paths and templates to process and the time stamp to use for	*
 * each one.								*
 *									*
 * de_scan ( nian, iret )						*
 *									*
 * Input and Output parameters:						*
 *	*nina		const int	Number of input argument	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = more than one ens. file 	*
 *					 -2 = file, template not exist	*
 *					 -3 = invalid forecast time	*
 *					 -5 = no ensemble file referred	*
 *					 -7 = too many ensemble members	*
 *					-11 = Invalid weight entry	*	
 *					-12 = Sum of weights > 100.	*
 *					-13 = Sum of weights < 100.	*
 *					-14 = Ensemble has zero members	* 
 **									*
 * Log:									*
 * T. Lee/SAIC		01/05						*
 * T. Lee/SAIC		06/05	Added weighting factors			*
 * R. Tian/SAIC		12/05	Translated from Fortran			*
 * m.gamazaychikov/SAIC 05/06   Added idtmch flag to CTB_DTGET CS       *
 * T. Piper/SAIC	0806	Added G_DIFF				*
 * T. Piper/SAIC        04/07   Modified for cfl_scnt CSC               *
 * F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to CTB_DTGET CSC*
 ***********************************************************************/
{
    char path[MXFLSZ], fpath[MXFLSZ], lmem[MXFLSZ], alias[MXFLSZ],
	 newfil[MXFLSZ], outfl[MXFLSZ], tmplt[MXTMPL], tplate[MXTMPL],
	 cycle[MXTMPL], dum[MXTMPL];
    char rplc[49], strmis[30], ingdtm[41], fff[4], wtmp[11], cent[3];
    char etm1[DTTMSZ], etm2[DTTMSZ], itm[DTTMSZ], ltm1[DTTMSZ], ltm2[DTTMSZ],
	 tfirst[DTTMSZ], tlast[DTTMSZ], vtm1[DTTMSZ], vtm2[DTTMSZ];
    char *def = " ", *starp, *chrptr;
    char **entry, **lmemf=NULL;
    struct dirent **dnlist=NULL;
    float wgtsum, wgt, wtm, wresid, wtprm, ewgts[MXMBRS];
    long lens;
    int nfpn, ifpn[NGDFLS], memap[MXMBRS], nen, num, istar,
        nfile, nf, last;
    int ic, is, iif, ir, ino, ihb, mnb, intrvl, iha, mna, mstrct, idtmch,
	kneg, kmem;
    int ii, jj, kk, ll, ier, icode, len;
/*----------------------------------------------------------------------*/
    *iret = 0;
    nfpn = 0;
    _ensdiag.ndxens = 0;

/*
 * Initialize file position numbers.
 */
    for ( ii = 0; ii < NGDFLS; ii++ ) {
        ifpn[ii] = 0;
    }

/*
 * Get user input of file position numbers.
 */
    for ( ii = 0; ii < *nina; ii++ ) {
	de_pfpn ( _ensdiag.allarg[ii], ifpn, &nfpn, iret );
    }

/*
 * Check if each ensemble diagnostic only points to one ensemble file.
 */
    for ( ii = 0; ii < nfpn; ii++ ) {
	if ( ifpn[ii] != _ensdiag.ndxens ) {
	    if ( _ensdiag.ensspc[ifpn[ii]-1][0] != '\0' ) {
		if (  _ensdiag.ndxens == 0 ) {
		     _ensdiag.ndxens = ifpn[ii];
		} else {
		    *iret = -1;
		    return;
		}
	     }
	}
    }

/*
 * If no appropriate file position number is found in the arguments 
 * (ndxens = 0), return.
 */
    if (  _ensdiag.ndxens == 0 ) {
	*iret = -5;
	return;
    }

/*
 * Initialize ensemble member arrays.
 */
    for ( ii = 0; ii < MXMBRS; ii++ ) {
	 _ensdiag.etmplt[ii][0] = '\0';
	 _ensdiag.enspth[ii][0] = '\0';
	 _ensdiag.ensfnm[ii][0] = '\0';
	 _ensdiag.etimes[ii][0] = '\0';
    }

/*
 * Get the governing valid time stamp.
 */
    dgc_qdtm ( &_ensdiag.ndxens, tfirst, tlast, &ier );
    dg_cget ( "INGDTM", ingdtm, &ier );
    grc_gtim ( ingdtm, tfirst, tlast, ltm1, ltm2, &ier );
    ctg_vald ( ltm1, vtm1, &ier );
    chrptr = strchr ( vtm1, 'V' );
    if ( chrptr ) *chrptr = '\0';
    if ( ltm2[0] != '\0' ) {
        ctg_vald ( ltm2, vtm2, &ier );
	chrptr = strchr ( vtm2, 'V' );
	if ( chrptr ) *chrptr = '\0';
    } else {
        vtm2[0] = '\0';
    }

/*
 * Build the list of ensemble member file names or paths and 
 * templates.  The entries inside {} may be file names or aliases 
 * with attached cycle times following |. 
 */
    strcpy ( lmem, _ensdiag.ensspc[_ensdiag.ndxens-1] );
    entry = (char **)cmm_malloc2d ( MAXENT, MXFLSZ, sizeof(char), &ier );
    cst_clst ( lmem, ',', def, MAXENT, MXFLSZ, entry, &nen, &ier );
    _ensdiag.nummbr = 0;
    wgtsum = 0.;
    for ( ii = 0; ii < nen; ii++ ) {
	if ( strchr ( entry[ii], '%' ) ) {
	    lmemf = (char **)cmm_malloc2d ( 2, MXFLSZ, sizeof(char), &ier );
	    cst_rlch ( RMISSD, 1, strmis, &ier );
	    cst_clst ( entry[ii], '%', strmis, 2, MXFLSZ, lmemf, &num, &ier );
	    cst_crnm ( lmemf[0], &wgt, &ier );
	    strcpy ( lmem, lmemf[1] );
	    cmm_free2d ( (void **)lmemf, &ier );

	    if ( ERMISS ( wgt ) || wgt < 0.0F || wgt > 100.0F ) {
		*iret = -11;
		cmm_free2d ( (void **)entry, &ier );
		return;
	    }
	    ewgts[ii] = wgt;
	    wgtsum += wgt;
	    if ( wgtsum > 100.0F ) {
		*iret = -12;
		cmm_free2d ( (void **)entry, &ier );
		return;
	    }
	} else {
	    strcpy ( lmem, entry[ii] );
	    ewgts[ii] = -1.0;
	}

	cfl_inqr ( lmem, NULL, &lens, newfil, &ier );
	if ( ier == 0 ) {
	    memap[_ensdiag.nummbr] = ii;
	    strcpy ( _ensdiag.ensfnm[_ensdiag.nummbr], newfil );
            _ensdiag.etmplt[_ensdiag.nummbr][0] = '\0';
            _ensdiag.enspth[_ensdiag.nummbr][0] = '\0'; 
            strcpy ( _ensdiag.etimes[_ensdiag.nummbr], ingdtm );
	    _ensdiag.nummbr += 1;
	    if (  _ensdiag.nummbr > MXMBRS - 1 ) {
	        *iret = -7;
		cmm_free2d ( (void **)entry, &ier );
		return;
	    }
	} else {
	    lmemf = (char **)cmm_malloc2d ( 2, MXFLSZ, sizeof(char), &ier );
	    cst_clst ( lmem, '|', def, 2, MXFLSZ, lmemf, &num, &ier );
	    strcpy ( alias, lmemf[0] );
	    strcpy ( cycle, lmemf[1] );
	    cmm_free2d ( (void **)lmemf, &ier );
	    ctb_dtget ( alias, path, tmplt, &ic, &is, &iif, &ir, &intrvl,
	        &ino, &ihb, &mnb, &iha, &mna, &mstrct, &idtmch, &ier );
	    cfl_inqr ( path, NULL, &lens, fpath, &ier );
	    if ( ier != 0 ) {
		cmm_free2d ( (void **)entry, &ier );
		icode = -1;
		er_wmsg ( "FL", &icode, lmem, &ier, strlen("FL"), strlen(lmem) );
		*iret = -2;
		return;
	    }

	    strcpy ( tplate, tmplt );
	    if ( strcmp ( cycle, def ) != 0 ) {
	        cti_stan ( cycle, "YYMMDD/HHNN", dum, &ier );
                if ( strstr ( tmplt, "YYYYMMDD" ) ) {
                    strcpy ( rplc, "YY" );
                    strncat ( rplc, dum, 6 );
                    rplc[8] = '\0';
                    cst_rpst ( tmplt, "YYYYMMDD", rplc, tmplt, &ier );
                } else {
                    cst_ncpy ( rplc, dum, 6, &ier );
                    cst_rpst ( tmplt, "YYMMDD", rplc, tmplt, &ier );
                }
                cst_ncpy ( rplc, &dum[7], 2, &ier );
                cst_rpst ( tmplt, "HH", rplc, tmplt, &ier );
	    }
	    cfl_scnt ( fpath, tmplt, -1, &dnlist, &nfile, &ier );
	    if ( nfile == 0 ) {
		icode = -1;
		er_wmsg ( "FL", &icode, lmem, &ier, strlen("FL"), strlen(lmem) );
		*iret = -2;
		return;
	    }
	    strcpy ( outfl, dnlist[0]->d_name );
	    for (ll=0; ll<nfile;ll++){
		free ( dnlist[ll] );
	    }
	    if (dnlist != NULL ) free(dnlist);

/*
 * Extract the YYYYMMDDHH information from the name of
 * the file. Subtiitute into the template to create a
 * working template (wtmp in the form of YYYYMMDDHH).
 */
	    cfl_mdat ( outfl, tplate, "YYMMDD/HH00", itm, &ier );
	    strcpy ( &itm[9], "00" );
	    if ( strstr ( tplate, "YYYY" ) ) {
		cti_ccnt ( itm, cent, &ier );
		strcpy ( wtmp, cent );
		strncpy ( &wtmp[2], itm, 6 );
		strncpy ( &wtmp[8], &itm[7], 2 );
		wtmp[10] = '\0';
	    } else {
		strncpy ( wtmp, itm, 6 );
		strncpy ( &wtmp[6], &itm[7], 2 );
		wtmp[8] = '\0';
	    }

/*
 * Use the valid time and initial time to construct the
 * forecast time.
 */
            ctg_vi2f ( vtm1, itm, etm1, &ier );
            if ( ier != 0 ) {
		*iret = -3;
		cmm_free2d ( (void **)entry, &ier );
		return;
	    }

/*
 * Get the FFF part of the date-time stamp.
 */
	    chrptr = strchr ( etm1, 'F' );
	    if ( chrptr ) {
		cst_ncpy ( fff, chrptr+1, 3, &ier );
	    } else {
		strcpy ( fff, "000" );	
	    }

            if ( vtm2[0] != '\0' ) {
                ctg_vi2f ( vtm2, itm, etm2, &ier );
                if ( ier != 0 ) {
		    *iret = -3;
		    cmm_free2d ( (void **)entry, &ier );
		    return;
		}
            } else {
                etm2[0] = '\0';
            }

/*
 * Check single v.s. multiple forecast files.
 */
	    starp = strchr ( tplate, '*' );
            if ( ! starp ) {
                memap[_ensdiag.nummbr] = ii;
                _ensdiag.ensfnm[_ensdiag.nummbr][0] = '\0';
                strcpy ( _ensdiag.enspth[_ensdiag.nummbr], fpath );
                strcpy ( _ensdiag.etmplt[_ensdiag.nummbr], tplate );
		if ( etm2[0] == '\0' ) {
		    strcpy ( _ensdiag.etimes[_ensdiag.nummbr], etm1 );
		} else {
		    strcpy ( _ensdiag.etimes[_ensdiag.nummbr], etm1 );
		    strcat ( _ensdiag.etimes[_ensdiag.nummbr], ":" );
		    strcat ( _ensdiag.etimes[_ensdiag.nummbr], etm2 );
		}
                _ensdiag.nummbr += 1;
                if (  _ensdiag.nummbr > MXMBRS - 1 ) {
                    *iret = -7;
		    cmm_free2d ( (void **)entry, &ier );
		    return;
	        }
	    } else {
/*
 * If there is an "*" in the template, this is an
 * ensemble with multiple members. List all the files
 * and set the individual member to the DECMN.CMN.
 */
		if ( strstr ( tplate, "YYYYMMDDHH" ) ) {
		    cst_rpst ( tplate, "YYYYMMDDHH", wtmp, tmplt, &ier );
		} else {
		    cst_rpst ( tplate, "YYMMDDHH", wtmp, tmplt, &ier );
		}

/*
 * If FF or FFF is in the template, replace with the
 * forecast hour string for the scan.
 */
		if ( strstr ( tplate, "FFF" ) ) {
		    cst_rpst ( tmplt, "FFF", fff, tmplt, &ier );
		} else if ( strstr ( tplate, "FF" ) ) {
		    cst_rpst ( tmplt, "FF", &fff[1], tmplt, &ier );
		}

                cfl_scnt ( fpath, tmplt, 1, &dnlist, &nf, &ier );

/*
 * Replace "*" in the template with number names. Note
 * that "*" may be embedded in the template.
 */
		if ( *(starp+1) == '\0' ) {
		    last = G_TRUE;
		} else {
		    last = G_FALSE;
		}

	        istar = (int)( starp - tplate );
                for ( jj = 0; jj < nf; jj++ ) {
		    if ( last == G_TRUE ) {
			strncpy ( tmplt, tplate, istar );
			strcpy ( &tmplt[istar], &dnlist[jj]->d_name[istar] );
		    } else {
			len = strlen(dnlist[jj]->d_name) - strlen(tplate) + 1;
			strncpy ( tmplt, tplate, istar );
			strncpy ( &tmplt[istar], &dnlist[jj]->d_name[istar], len );
			strcpy ( &tmplt[istar+len], &tplate[istar+1] );
		    }
		    free(dnlist[jj]);			
                    memap[_ensdiag.nummbr] = ii;
                    _ensdiag.ensfnm[_ensdiag.nummbr][0] = '\0';
                    strcpy ( _ensdiag.etmplt[_ensdiag.nummbr], tmplt );
                    strcpy ( _ensdiag.enspth[_ensdiag.nummbr], fpath );
		    if ( etm2[0] == '\0' ) {
		        strcpy ( _ensdiag.etimes[_ensdiag.nummbr], etm1 );
		    } else {
		       strcpy ( _ensdiag.etimes[_ensdiag.nummbr], etm1 );
		       strcat ( _ensdiag.etimes[_ensdiag.nummbr], ":" );
		       strcat ( _ensdiag.etimes[_ensdiag.nummbr], etm2 );
		    }
                    _ensdiag.nummbr += 1;
                    if (  _ensdiag.nummbr > MXMBRS - 1 ) {
                        *iret = -7;
		        cmm_free2d ( (void **)entry, &ier );
		        return;
                    }
                }
		if (dnlist != NULL ) free(dnlist);
            }
        }
    }
    cmm_free2d ( (void **)entry, &ier );

/*
 * Write out the number of members as user information:
 */
    cst_inch ( _ensdiag.nummbr, dum, &ier );
    icode = 4;
    er_wmsg ( "DE", &icode, dum, &ier, strlen("DE"), strlen(dum) );

/*
 * Compute the individual member weights.
 */
    if ( G_DIFF(wgtsum, 0.0F) && _ensdiag.nummbr > 0 ) {

/*
 * If no weights are given, use equal weighting for all members.
 */
	wtm = 1. / _ensdiag.nummbr;
        for ( ii = 0; ii <  _ensdiag.nummbr; ii++ ) {
            _ensdiag.enswts[ii] = wtm;
        }
    } else {
	wresid = 100.0F - wgtsum;
	if ( wresid < 0 ) wresid = 0.0F;

/*
 * Count the number of -1 weights in the ewgts array.
 */
	kneg = 0;
        for ( ii = 0; ii <  nen; ii++ ) {
            if ( ewgts[ii] < 0.0F ) kneg += 1;
        }
	if ( kneg > 0 ) {
	    wtprm = wresid / kneg;
	    for ( ii = 0; ii <  nen; ii++ ) {
		if ( ewgts[ii] < 0.0F )  ewgts[ii] = wtprm;
	    }
	} else {
/*
 * If total weight less than 100. Return.
 */
	    if ( wresid > 0.0F ) {
		*iret = -13;
		return;
	    }
	}

/*
 * Now we are ready to assign weights to individual members
 * since we have the weight values for each entry in { }.
 */
	for ( ii = 0; ii <  nen; ii++ ) {
/*
 * Count # of members with memap .eq. i, let that number
 * be kmem.
 */
	    kmem = 0;
	    for ( kk = 0; kk < _ensdiag.nummbr; kk++ ) {
		if ( memap[kk] == ii )  kmem += 1;
	    }
	    if ( kmem == 0 ) {
		*iret = -14;
		return;
	    }

/*
 * Compute the individual member weight.
 */
	    wtm = ewgts[ii] / kmem;
	    for ( kk = 0; kk <  _ensdiag.nummbr; kk++ ) {
		if ( memap[kk] == ii ) {
		    _ensdiag.enswts[kk] = wtm / 100.0F;
		}
	    }
	}
    }

/*
 * Save DGCMN information that will be restored by DE_RSET after
 * Ensemble diagnostic.
 */
    de_save ( &_ensdiag.ndxens, iret );

    return;
}
