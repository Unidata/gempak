#include "dg.h"

void dg_chck ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret )
/************************************************************************
 * dg_chck								*
 *									*
 * This subroutine checks for alternate ways to compute grids.		*
 *									*
 * dg_chck ( time1, time2, level1, level2, ivcord, parm, num, iret )	*
 *									*
 * Input parameters:							*
 * 	*time1		const char	Date/time			*
 * 	*time2		const char	Date/time			*
 *	*level1		const int	Level				*
 *	*level2		const int	Level				*
 *	*ivcord		const int	Vertical coordinate		*
 *	*parm		const char	Parameter name			*
 *									*
 * Input and output parameters:                                         *
 *	*num		const int	Location of grid		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					-10 = internal grid list full	*
 *					-12 = grid must be a scalar	*
 *					-16 = map proj is invalid	*
 *					-20 = stack is full		*
 *					-21 = stack is empty		*
 *					-22 = TIME is invalid		*
 *					-23 = LEVEL is invalid		*
 *					-24 = IVCORD is invalid		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 3/88	Added STHA,STHE,SMXR			*
 * M. desJardins/GSFC	 5/88	Fixed scaling				*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/GSFC	 1/89	Fixed computation of STHE		*
 * M. desJardins/GSFC	 4/89	Error message changes			*
 * M. desJardins/GSFC	 7/89	GEMPAK 5				*
 * M. desJardins/GSFC	 8/89	PA to PD subroutines			*
 * K. Brill/GSC		 9/89   Added call to DG_PRES			*
 * M. desJardins/GSFC	 2/90	Added DG_MIXR, comp of TVRx, THTV	*
 * M. desJardins/GSFC	 7/90	Corrected computation of THTV		*
 * K. Brill/NMC         11/90   Pass grid number to DF_CORL, XVAL, YVAL	*
 * K. Brill/NMC		 8/91	Add precip alternates			*
 * K. Brill/NMC		 2/92	Check IRET before doing temp or precip	*
 * M. desJardins/NMC	 3/92	Major clean up to change temp, vertical	*
 *				coordinate, mixing ratio calculations;	*
 *				eliminate unused scale terms		*
 * K. Brill/NMC		10/92	Add  PR_PRFT			        *
 * M. desJardins/NMC	11/92	DG_TMPC-->DG_DWPT in computation of THTE*
 * K. Brill/NMC		02/93	Check for S and C for precipitation	*
 * L. Saqer/NMC		 6/93	Move precipitation to end 		*
 * M. desJardins/NMC	 4/94	Combine precipitation calls		*
 * L. Williams/EAI	 8/94	Added call to DG_CWBT and check for     *
 *				mapscale factor				*
 * D. Keiser/GSC	 6/95	Added stability index calculations 	*
 * S. Jacobs/NCEP	10/95	Added checks for UWND and VWND		*
 * K. Tyle/GSC		11/95	Added THES 				*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC		 5/96	Moved IGDPT outside do-loop		*
 * D.W.Plummer/NCEP	 7/98	Added land-sea mask grid processing	*
 * D. McCann/AWC	 4/01	Added BETA				*
 * S. Jacobs/NCEP	 3/02	Added HEAT				*
 * K. Brill/HPC		 5/02	Get MAP SCL FCTRS & LAT/LON from int grd*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * T. Lee/SAIC		 6/03	Added LHAN, MHAN and HHAN		*
 * T. Lee/SAIC		10/05	Added DF_IGPT, DF_JGPT			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 * C. Melick/SPC        08/11   Added THWK, THWC, THWF                  *
 * K. Brill/WPC		04/13	Check WIZAHGNRL types to call dg_prft	*
 ************************************************************************/
{
    char p[14], pp[14];
    float value;
    int ntd, np, nt, nt1, nt2, nr, ntf, nu, nv, nun, nvn, nt500, ntd850,
        nt850, nt700, ntd700, nu850, nv850, nun850, nvn850, nd850, ns850,
	nu500, nv500, nun500, nvn500, nd500, ns500, nmx;
    int level11, level12, level21, level22, itype, ichk, lev1, lev2, ivc, i, ier;
/*----------------------------------------------------------------------*/
    *iret = -7;

    /*
     * First check to see whether input is a number, but not equal
     * to 'E' (char to int conversion of 'E' appears as a reasonable
     * number).  Note: the new ST_CRNM eliminates E by itself.
     */
    cst_crnm ( (char *)parm, &value, &ier );
    if ( ier == 0 ) {
	dg_real ( &value, num, iret );
    }

    /*
     * Check for the current vertical coordinate.
     */
    if ( *iret != 0 ) {
	dg_vcrd ( time1, time2, level1, level2, ivcord, parm, num, iret );
    }

    /*
     * Check alternate names for variables.
     */
    if ( *iret == 0 ) {
    /*
     ************************ TEMPERATURE *******************************
     */
    } else if ( ( strcmp ( parm, "TEMP" ) == 0 ) ||
	        ( strncmp ( parm, "TMP", 3 ) == 0 ) ) {
	dg_temp ( time1, time2, level1, level2, ivcord, parm, num, iret );

    /*
     ************************ DEWPOINT TEMPERATURE **********************
     */
    } else if ( strncmp ( parm, "DWP", 3 ) == 0 ) {
	dg_dwpt ( time1, time2, level1, level2, ivcord, parm, num, iret );

    /*
     *********************** WETBULB TEMPERATURE ************************
     */
    } else if ( strncmp ( parm, "TMW", 3 ) == 0 ) {
	dg_cwbt ( time1, time2, level1, level2, ivcord, parm, num, iret );

    /*
     ************************ SPECIAL TEMPERATURES **********************
     */
    } else if ( ( strcmp ( parm, "TMAX" ) == 0 ) ||
                ( strcmp ( parm, "TMIN" ) == 0 ) ||
		( strncmp ( parm, "SST", 3 ) == 0 ) ||
		( strncmp ( parm, "TMX", 3 ) == 0 ) ||
		( strncmp ( parm, "TMN", 3 ) == 0 ) ) {
	dg_gtmp ( time1, time2, level1, level2, ivcord, parm, num, iret );

    /*
     ************************ MIXING RATIO ******************************
     */
    } else if ( ( strcmp ( parm, "MIXR" ) == 0 ) ||
                ( strcmp ( parm, "RMIX" ) == 0 ) ||
		( strcmp ( parm, "MIXS" ) == 0 ) ||
		( strcmp ( parm, "SMIX" ) == 0 ) ||
		( strcmp ( parm, "SMXR" ) == 0 ) ||
		( strcmp ( parm, "SMXS" ) == 0 ) ) {
	dg_mxnt ( time1, time2, level1, level2, ivcord, parm, num, iret );

	/*
	 * Try alternate computations for MIXR and RMIX.
	 */
	if ( ( ( strcmp ( parm, "MIXR" ) == 0 ) ||
	       ( strcmp ( parm, "RMIX" ) == 0 ) ) && ( *iret != 0 ) ) {
	    /*
	     * Try to compute from dewpoint.
	     */
	    strcpy ( p, "DWPC" );
	    dg_nxts ( &ntd, &ier );
	    dg_dwpt ( time1, time2, level1, level2, ivcord, p, &ntd, iret );
	    if ( *iret == 0 ) {
		strcpy ( p, "PRES" );
		dg_nxts ( &np, &ier );
		dg_vcrd ( time1, time2, level1, level2, ivcord, p, &np, iret );
		if ( *iret == 0 ) {
		    pd_mixr ( _dggrid.dgg[ntd-1].grid, _dggrid.dgg[np-1].grid,
		       &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
		}
		dg_frig ( &np, &ier );
	    }
	    dg_frig ( &ntd, &ier );

	/*
	 * Try alternate computations for MIXS and SMIX.
	 */
	} else if ( ( ( strcmp ( parm, "MIXS" ) == 0 ) ||
	              ( strcmp ( parm, "SMIX" ) == 0 ) ) && ( *iret != 0 ) ) {
	    /*
	     * Try to compute from temperature.
	     */
	    strcpy ( p, "TMPC" );
	    dg_nxts ( &ntd, &ier );
	    dg_temp ( time1, time2, level1, level2, ivcord, p, &ntd, iret );
	    if ( *iret == 0 ) {
		strcpy ( p, "PRES" );
		dg_nxts ( &np, &ier );
		dg_vcrd ( time1, time2, level1, level2, ivcord, p, &np, iret );
		if ( *iret == 0 ) {
		    pd_mixr ( _dggrid.dgg[ntd-1].grid, _dggrid.dgg[np-1].grid,
		        &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
		}
		dg_frig ( &np, &ier );
	    }
	    dg_frig ( &ntd, &ier );

	/*
	 * Try alternate computations for SMXR.
	 */
	} else if ( ( strcmp ( parm, "SMXR" ) == 0 ) && ( *iret != 0 ) ) {
	    strcpy ( p, "DWPC" );
	    dg_nxts ( &ntd, &ier );
	    dg_dwpt ( time1, time2, level1, level2, ivcord, p, &ntd, iret );
	    if ( *iret == 0 ) {
		strcpy ( p, "PALT" );
		dg_nxts ( &np, &ier );
		dg_grdr ( time1, time2, level1, level2, ivcord, p, &np, iret );
		if ( *iret == 0 ) {
		    pd_mixr ( _dggrid.dgg[ntd-1].grid, _dggrid.dgg[np-1].grid,
		        &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
		}
		dg_frig ( &np, &ier );
	    }
	    dg_frig ( &ntd, &ier );

	/*
	 * Try alternate computations for SMXS.
	 */
	} else if ( ( strcmp ( parm, "SMXS" ) == 0 ) && ( *iret != 0 ) ) {
	    strcpy ( p, "TMPC" );
	    dg_nxts ( &ntd, &ier );
	    dg_temp ( time1, time2, level1, level2, ivcord, p, &ntd, iret );
	    if ( *iret == 0 ) {
		strcpy ( p, "PALT" );
		dg_nxts ( &np, &ier );
		dg_grdr ( time1, time2, level1, level2, ivcord, p, &np, iret );
		if ( *iret == 0 ) {
		    pd_mixr ( _dggrid.dgg[ntd-1].grid, _dggrid.dgg[np-1].grid,
		        &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
		}
		dg_frig ( &np, &ier );
	    }
	    dg_frig ( &ntd, &ier );
	}

    /*
     *********************** POTENTIAL TEMPERATURE **********************
     */
    } else if ( ( strncmp ( parm, "THT", 3 ) == 0 ) &&
	        ( strcmp ( parm, "THTE" ) != 0 ) &&
		( strcmp ( parm, "THTV" ) != 0 ) ) {
	dg_gtmp ( time1, time2, level1, level2, ivcord, parm, num, iret );

	if ( *iret != 0 ) {
	    strcpy ( p, "TMPC" );
	    dg_gtmp ( time1, time2, level1, level2, ivcord, p, num, iret );
	    if ( *iret != 0 ) return;
	    strcpy ( p, "PRES" );
	    dg_nxts ( &np, &ier );
	    dg_vcrd ( time1, time2, level1, level2, ivcord, p, &np, iret );
	    if ( *iret == 0 ) {
	        pd_thta ( _dggrid.dgg[(*num)-1].grid, _dggrid.dgg[np-1].grid,
	                  &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
	        dg_cdeg ( "THTA", parm, num, iret );
	    }
	    dg_frig ( &np, &ier );
	}
    } else if ( ( strncmp ( parm, "STH", 3 ) == 0 ) &&
	        ( strcmp ( parm, "STHE" ) != 0 ) ) {
	dg_gtmp ( time1, time2, level1, level2, ivcord, parm, num, iret );

	if ( *iret != 0 ) {
	    strcpy ( p, "TMPC" );
	    dg_gtmp ( time1, time2, level1, level2, ivcord, p, num, iret );
	    if ( *iret != 0 ) return;
	    strcpy ( p, "PALT" );
	    dg_nxts ( &np, &ier );
	    dg_grdr ( time1, time2, level1, level2, ivcord, p, &np, iret );
	    if ( *iret == 0 ) {
	        pd_thta ( _dggrid.dgg[(*num)-1].grid, _dggrid.dgg[np-1].grid,
	                  &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
	        dg_cdeg ( "STHA", parm, num, iret );
	    }
	    dg_frig ( &np, &ier );
	}

    /*
     ******************* EQUIVALENT POTENTIAL TEMPERATURE ***************
     */
    } else if ( strcmp ( parm, "THTE" ) == 0 ) {
	strcpy ( p, "TMPC" );
	dg_nxts ( &nt, &ier );
	dg_temp ( time1, time2, level1, level2, ivcord, p, &nt, iret );
	if ( *iret == 0 ) {
	    strcpy ( p, "DWPC" );
	    dg_nxts ( &ntd, &ier );
	    dg_dwpt ( time1, time2, level1, level2, ivcord, p, &ntd, iret );
	    if ( *iret == 0 ) {
	        strcpy ( p, "PRES" );
		dg_nxts ( &np, &ier );
		dg_vcrd ( time1, time2, level1, level2, ivcord, p, &np, iret );
		if ( *iret == 0 ) {
		    pd_thte ( _dggrid.dgg[np-1].grid, _dggrid.dgg[nt-1].grid,
		              _dggrid.dgg[ntd-1].grid, &_dgfile.kxyd, 
		              _dggrid.dgg[(*num)-1].grid, &ier );
		}
		dg_frig ( &np, &ier );
	    }
	    dg_frig ( &ntd, &ier );
	}
	dg_frig ( &nt, &ier );
    } else if ( strcmp ( parm, "THES" ) == 0 ) {
	strcpy ( p, "TMPC" );
	dg_nxts ( &nt, &ier );
	dg_temp ( time1, time2, level1, level2, ivcord, p, &nt, iret );
	if ( *iret == 0 ) {
	    strcpy ( p, "PRES" );
	    dg_nxts ( &np, &ier );
	    dg_vcrd ( time1, time2, level1, level2, ivcord, p, &np, iret );
	    if ( *iret == 0 ) {
		pd_thte ( _dggrid.dgg[np-1].grid, _dggrid.dgg[nt-1].grid,
		  	  _dggrid.dgg[nt-1].grid, &_dgfile.kxyd,
		  	  _dggrid.dgg[(*num)-1].grid, &ier );
	    }
	    dg_frig ( &np, &ier );
	}
	dg_frig ( &nt, &ier );
    } else if ( strcmp ( parm, "STHE" ) == 0 ) {
	strcpy ( p, "TMPC" );
	dg_nxts ( &nt, &ier );
	dg_temp ( time1, time2, level1, level2, ivcord, p, &nt, iret );
	if ( *iret == 0 ) {
	    strcpy ( p, "DWPC" );
	    dg_nxts ( &ntd, &ier );
	    dg_dwpt ( time1, time2, level1, level2, ivcord, p, &ntd, iret );
	    if ( *iret == 0 ) {
		strcpy ( p, "PALT" );
		dg_nxts ( &np, &ier );
		dg_grdr ( time1, time2, level1, level2, ivcord, p, &np, iret );
		if ( *iret == 0 ) {
		    pd_thte ( _dggrid.dgg[np-1].grid, _dggrid.dgg[nt-1].grid,
		  	      _dggrid.dgg[ntd-1].grid, &_dgfile.kxyd,
		  	      _dggrid.dgg[(*num)-1].grid, &ier );
		}
		dg_frig ( &np, &ier );
	    }
	    dg_frig ( &ntd, &ier );
	}
	dg_frig ( &nt, &ier );
    /*
     ******************* WETBULB POTENTIAL TEMPERATURE ***************
     */
    } else if ( strncmp ( parm, "THW", 3 ) == 0 ) {
        if ( ( strcmp ( parm, "THWK") != 0 ) && ( strcmp ( parm, "THWF") != 0 ) && 
           ( strcmp ( parm, "THWC") != 0 ) ) {
           return;
        }
	strcpy ( p, "TMPC" );
	dg_nxts ( &nt, &ier );
	dg_temp ( time1, time2, level1, level2, ivcord, p, &nt, iret );
	if ( *iret == 0 ) {
	    strcpy ( p, "DWPC" );
	    dg_nxts ( &ntd, &ier );
	    dg_dwpt ( time1, time2, level1, level2, ivcord, p, &ntd, iret );
	    if ( *iret == 0 ) {
	        strcpy ( p, "PRES" );
		dg_nxts ( &np, &ier );
		dg_vcrd ( time1, time2, level1, level2, ivcord, p, &np, iret );
		if ( *iret == 0 ) {
		    pd_thwc ( _dggrid.dgg[np-1].grid, _dggrid.dgg[nt-1].grid,
		              _dggrid.dgg[ntd-1].grid, &_dgfile.kxyd, 
		              _dggrid.dgg[(*num)-1].grid, &ier );
                    if  ( strcmp (parm, "THWK") == 0 ) { 
	                 pd_tmck ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		                   _dggrid.dgg[(*num)-1].grid, &ier);
                    } else if  ( strcmp (parm, "THWF") == 0 ) {
                         pd_tmcf ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
                                   _dggrid.dgg[(*num)-1].grid, &ier);
                    }
		}
		dg_frig ( &np, &ier );
	    }
	    dg_frig ( &ntd, &ier );
	}
	dg_frig ( &nt, &ier );
    /*
     **************** RELATIVE HUMIDITY *********************************
     */
    } else if ( strcmp ( parm, "RELH" ) == 0 ) {
	strcpy ( p, "TMPC" );
	dg_nxts ( &nt, &ier );
	dg_temp ( time1, time2, level1, level2, ivcord, p, &nt, iret );
	if ( *iret == 0 ) {
	    strcpy ( p, "DWPC" );
	    dg_nxts ( &ntd, &ier );
	    dg_dwpt ( time1, time2, level1, level2, ivcord, p, &ntd, iret );
	    if ( *iret == 0 ) {
		pd_relh ( _dggrid.dgg[nt-1].grid, _dggrid.dgg[ntd-1].grid,
	    		  &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
	    }
	    dg_frig ( &ntd, &ier );
	}
	dg_frig ( &nt, &ier );
    /*
     *************** HAINES INDICES *************************************
     */
    } else if ( strncmp ( &parm[1], "HAN", 3 ) == 0 ) {
	if ( strcmp ( parm, "LHAN" ) == 0 ) {
	    level11 = 850;
	    level21 = 950;
	    itype = 1;
	} else if ( strcmp ( parm, "MHAN" ) == 0 ) {
	    level11 = 850;
	    level21 = 700;
	    itype = 2;
	} else if ( strcmp ( parm, "HHAN" ) == 0 ) {
	    level11 = 700;
	    level21 = 500;
	    itype = 3;
	}
	level12 = -1;
	level22 = -1;
	strcpy ( p, "TMPC" );
	dg_nxts ( &nt1, &ier );
	dg_temp ( time1, time2, &level11, &level12, ivcord, p, &nt1, iret );
	if ( *iret == 0 ) {
	    dg_nxts ( &nt2, &ier );
	    dg_temp ( time1, time2, &level21, &level22, ivcord, p, &nt2, iret );
	    if ( *iret == 0 ) {
		strcpy ( p, "DWPC" );
		dg_nxts ( &ntd, &ier );
		dg_dwpt ( time1, time2, &level11, &level12, ivcord, p, &ntd,
		          iret );
		if ( *iret == 0 ) {
		    pd_hans ( _dggrid.dgg[nt1-1].grid, _dggrid.dgg[nt2-1].grid,	
		  	      _dggrid.dgg[ntd-1].grid, &_dgfile.kxyd, &itype,
		  	      _dggrid.dgg[(*num)-1].grid, &ier );
		}
		dg_frig ( &ntd, &ier );
	    }
	    dg_frig ( &nt2, &ier );
	}
	dg_frig ( &nt1, &ier );
    /*
     *********  VIRTUAL TEMPERATURE & VIRTUAL POTENTIAL TEMPERATURE *****
     */
    } else if ( ( strncmp ( parm, "TVR", 3 ) == 0 ) ||
		( strcmp ( parm, "THTV" ) == 0 ) ) {
	if ( strcmp ( parm, "THTV" ) == 0 ) {
	    strcpy ( p, "TVRC" );
	} else {
	    strcpy ( p, parm );
	}
	dg_gtmp  ( time1, time2, level1, level2, ivcord, p, num, iret );

	/*
	 * If not found, try to compute from TMPK, MIXR.
	 */
	if ( *iret != 0 ) {
	    strcpy ( pp, "TMPK" );
	    dg_temp ( time1, time2, level1, level2, ivcord, pp, num, iret );
	    if ( *iret == 0 ) {
		strcpy ( pp, "MIXR" );
		dg_nxts ( &nmx, &ier );
		dg_mxnt ( time1, time2, level1, level2, ivcord, pp, &nmx, iret );
		if ( *iret != 0 ) {
		    strcpy ( pp, "DWPC" );
		    dg_nxts ( &ntd, &ier );
		    dg_dwpt ( time1, time2, level1, level2, ivcord, pp, &ntd,
		              iret );
		    if ( *iret == 0 ) {
			strcpy ( pp, "PRES" );
			dg_nxts ( &np, &ier );
			dg_vcrd ( time1, time2, level1, level2, ivcord, pp, &np,
			          iret );
			if ( *iret == 0 ) {
			    pd_mixr ( _dggrid.dgg[ntd-1].grid,
			              _dggrid.dgg[np-1].grid, &_dgfile.kxyd,
				      _dggrid.dgg[nmx-1].grid, &ier);
			}
			dg_frig ( &np, &ier );
		    }
		    dg_frig ( &ntd, &ier );
		}
		if ( *iret == 0 ) {
		    pd_tvrk ( _dggrid.dgg[(*num)-1].grid,
		              _dggrid.dgg[nmx-1].grid, &_dgfile.kxyd,
			      _dggrid.dgg[(*num)-1].grid, &ier );
		    dg_cdeg ( "TVRK", p, num, iret );
		}
		dg_frig ( &nmx, &ier );
	    }
	}

	if ( ( *iret == 0 ) && ( strcmp ( parm, "THTV" ) == 0 ) ) {
	    strcpy ( p, "PRES" );
	    dg_nxts ( &np, &ier );
	    dg_vcrd ( time1, time2, level1, level2, ivcord, p, &np, iret );
	    if ( *iret == 0 ) {
		pd_thta ( _dggrid.dgg[(*num)-1].grid, _dggrid.dgg[np-1].grid,
		    &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
	    }
	    dg_frig ( &np, &ier );
	}
    /*
     ******************* HEAT INDEX *************************************
     */
    } else if ( strcmp ( parm, "HEAT" ) == 0 ) {
	strcpy ( p, "TMPC" );
	dg_nxts ( &nt, &ier );
	dg_temp ( time1, time2, level1, level2, ivcord, p, &nt, iret );
	if ( *iret == 0 ) {
	    strcpy ( p, "DWPC" );
	    dg_nxts ( &ntd, &ier );
	    dg_dwpt  ( time1, time2, level1, level2, ivcord, p, &ntd, iret );
	    if ( *iret == 0 ) {
		dg_nxts ( &nr, iret );
		pd_relh ( _dggrid.dgg[nt-1].grid, _dggrid.dgg[ntd-1].grid,
	    	          &_dgfile.kxyd, _dggrid.dgg[nr-1].grid, &ier );
		strcpy ( p, "TMPF" );
		dg_nxts ( &ntf, &ier );
		dg_temp ( time1, time2, level1, level2, ivcord, p, &ntf, iret );
		if ( *iret == 0 ) {
		    pd_heat ( _dggrid.dgg[ntf-1].grid, _dggrid.dgg[nr-1].grid,
	    	              &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
		}
		dg_frig ( &nr, &ier );
		dg_frig ( &ntf, &ier );
	    }
	    dg_frig ( &ntd, &ier );
	}
	dg_frig ( &nt, &ier );
    /*
     ******************* WINDS ******************************************
     */
    } else if ( ( strcmp ( parm, "SPED" ) == 0 ) ||
	      ( strcmp ( parm, "DRCT" ) == 0 ) ||
	      ( strcmp ( parm, "UWND" ) == 0 ) ||
	      ( strcmp ( parm, "VWND" ) == 0 ) ) {
	dg_gobs ( time1, time2, level1, level2, ivcord, &nu, &nv, iret );
	if ( *iret != 0 )  return;

	dg_nxtv ( &nun, &nvn, iret );
	if ( *iret != 0 )  return;

	dg_nrel ( _dggrid.dgg[nu-1].grid, _dggrid.dgg[nv-1].grid,
		  _dggrid.dgg[nun-1].grid, _dggrid.dgg[nvn-1].grid, iret );
	dg_frig ( &nu, &ier );
	dg_frig ( &nv, &ier );

	if ( *iret == 0 ) {
	    if ( strcmp ( parm, "DRCT" ) == 0 ) {
	        pd_drct ( _dggrid.dgg[nun-1].grid, _dggrid.dgg[nvn-1].grid,
		          &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
		dg_frig ( &nun, &ier );
		dg_frig ( &nvn, &ier );
	    }

	    if ( strcmp ( parm, "SPED" ) == 0 ) {
	        pd_sped ( _dggrid.dgg[nun-1].grid, _dggrid.dgg[nvn-1].grid,
		          &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, &ier );
		dg_frig ( &nun, &ier );
		dg_frig ( &nvn, &ier );
	    }
 
	    if ( strcmp ( parm, "UWND" ) == 0 ) {
		dg_frig ( num, &ier );
		dg_frig ( &nvn, &ier );
	        *num = nun;
	    }

	    if ( strcmp ( parm, "VWND" ) == 0 ) {
		dg_frig ( num, &ier );
		dg_frig ( &nun, &ier );
	        *num = nvn;
	    }
	}
    /*
     ************************* SNOW *************************************
     */
    } else if ( strcmp ( parm, "SNOW" ) == 0 ) {
	strcpy ( p, "SNDM" );
	dg_grdr ( time1, time2, level1, level2, ivcord, p, num, iret );
	if ( *iret == 0 ) {
	    for ( i = 0; i < _dgfile.kxyd; i++ ) {
		if ( ! ERMISS ( _dggrid.dgg[(*num)-1].grid[i] ) ) {
		    _dggrid.dgg[(*num)-1].grid[i] *= 1000.;
		}
	    }
	}
    } else if ( strcmp ( parm, "SNDM" ) == 0 ) {
	strcpy ( p, "SNOW" );
	dg_grdr ( time1, time2, level1, level2, ivcord, p, num, iret );
	if ( *iret == 0 ) {
	    for ( i = 0; i < _dgfile.kxyd; i++ ) {
	        if ( ! ERMISS ( _dggrid.dgg[(*num)-1].grid[i] ) ) {
		    _dggrid.dgg[(*num)-1].grid[i] *= .001;
	        }
	    }
        }

    /*
     ********************* CROSS TOTALS INDEX ***************************
     */
    } else if ( strcmp ( parm, "CTOT" ) == 0 ) {
	/*
	 * Compute the needed temperature and dewpoint values
	 */
	strcpy ( p, "TMPC" );
	lev1 = 500;
	lev2 = -1;
	ivc = 1;
	dg_nxts ( &nt500, &ier );
	dg_temp ( time1, time2, &lev1, &lev2, &ivc, p, &nt500, iret );
	if ( *iret == 0 ) {
	    strcpy ( p, "DWPC" );
	    lev1 = 850;
	    dg_nxts ( &ntd850, &ier );
	    dg_dwpt ( time1, time2, &lev1, &lev2, &ivc, p, &ntd850, iret );
	    if ( *iret == 0 ) {
		/*
	 	* Compute the cross totals index
	 	*/
		pd_ctot ( _dggrid.dgg[nt500-1].grid, _dggrid.dgg[ntd850-1].grid,
	    	          &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, iret );
	    }
	    dg_frig ( &ntd850, &ier );
	}
	dg_frig ( &nt500, &ier );
    /*
     ********************** VERTICAL TOTALS INDEX ***********************
     */
    } else if ( strcmp ( parm, "VTOT" ) == 0 ) {
	/*
	 * Compute the needed temperature values
	 */
	strcpy ( p, "TMPC" );
	lev1 = 500;
	lev2 = -1;
	ivc = 1;
	dg_nxts ( &nt500, &ier );
	dg_temp ( time1, time2, &lev1, &lev2, &ivc, p, &nt500, iret );
	if ( *iret == 0 ) {
	    lev1 = 850;
	    dg_nxts ( &nt850, &ier );
	    dg_temp ( time1, time2, &lev1, &lev2, &ivc, p, &nt850, iret );
	    if ( *iret == 0 ) {
		/*
	 	* Compute the vertical totals index
	 	*/
		pd_vtot ( _dggrid.dgg[nt500-1].grid, _dggrid.dgg[nt850-1].grid,
	    	          &_dgfile.kxyd, _dggrid.dgg[(*num)-1].grid, iret );
	    }
	    dg_frig ( &nt850, &ier );
	}
	dg_frig ( &nt500, &ier );
    /*
     *************************** K INDEX ********************************
     */
    } else if ( strcmp ( parm, "KINX" ) == 0 ) {
	/*
	 * Compute the needed temperature and dewpoint values
	 */
	strcpy ( p, "TMPC" );
	lev1 = 850;
	lev2 = -1;
	ivc = 1;
	dg_nxts ( &nt850, &ier );
	dg_temp ( time1, time2, &lev1, &lev2, &ivc, p, &nt850, iret );
	if ( *iret == 0 ) {
	    lev1 = 500;
	    dg_nxts ( &nt500, &ier );
	    dg_temp ( time1, time2, &lev1, &lev2, &ivc, p, &nt500, iret );
	    if ( *iret == 0 ) {
		lev1 = 700;
		dg_nxts ( &nt700, &ier );
		dg_temp ( time1, time2, &lev1, &lev2, &ivc, p, &nt700, iret );
		if ( *iret == 0 ) {
		    strcpy ( p, "DWPC" );
		    lev1 = 850;
		    dg_nxts ( &ntd850, &ier );
		    dg_dwpt ( time1, time2, &lev1, &lev2, &ivc, p, &ntd850,
		              iret );
		    if ( *iret == 0 ) {
			lev1 = 700;
			dg_nxts ( &ntd700, &ier );
			dg_dwpt ( time1, time2, &lev1, &lev2, &ivc, p, &ntd700,
			          iret );
			if ( *iret == 0 ) {
			    /*
	 		     * Compute the K index
	 		     */
			    pd_kinx ( _dggrid.dgg[nt850-1].grid,
			    	      _dggrid.dgg[nt700-1].grid,
		  		      _dggrid.dgg[nt500-1].grid,
				      _dggrid.dgg[ntd850-1].grid,
		  		      _dggrid.dgg[ntd700-1].grid,
				      &_dgfile.kxyd,
		  	              _dggrid.dgg[(*num)-1].grid, iret );
			}
			dg_frig ( &ntd700, &ier );
		    }
		    dg_frig ( &ntd850, &ier );
		}
		dg_frig ( &nt700, &ier );
	    }
	    dg_frig ( &nt500, &ier );
	}	    	
	dg_frig ( &nt850, &ier );
    /*
     *************************** TOTAL TOTALS INDEX *********************
     */
    } else if ( strcmp ( parm, "TOTL" ) == 0 ) {
	/*
	 * Compute needed temperature and dewpoint values
	 */
	strcpy( p, "TMPC" );
	lev1 = 850;
	lev2 = -1;
	ivc = 1;
	dg_nxts ( &nt850, &ier );
	dg_temp ( time1, time2, &lev1, &lev2, &ivc, p, &nt850, iret );
	if ( *iret == 0 ) {
	    lev1 = 500;
	    dg_nxts ( &nt500, &ier );
	    dg_temp ( time1, time2, &lev1, &lev2, &ivc, p, &nt500, iret );
	    if ( *iret == 0 ) {
		strcpy ( p, "DWPC" );
		lev1 = 850;
		dg_nxts ( &ntd850, &ier );
		dg_dwpt ( time1, time2, &lev1, &lev2, &ivc, p, &ntd850, iret );
		if ( *iret == 0 ) {
		    /*
	 	     * Compute the total totals index
	 	     */
		    pd_totl ( _dggrid.dgg[nt850-1].grid,
		              _dggrid.dgg[ntd850-1].grid,
		  	      _dggrid.dgg[nt500-1].grid, &_dgfile.kxyd,
		  	      _dggrid.dgg[(*num)-1].grid, iret );
		}
		dg_frig ( &ntd850, &ier );
	    }
	    dg_frig ( &nt500, &ier );
	}
	dg_frig ( &nt850, &ier );
    /*
     ************************** SWEAT INDEX *****************************
     */
    } else if ( strcmp ( parm, "SWET" ) == 0 ) {
	/*
	 * Compute needed temperature and dewpoint values
	 */
	strcpy ( p, "TMPC" );
	lev1 = 850;
	lev2 = -1;
	ivc = 1;
	dg_nxts ( &nt850, &ier );
	dg_temp ( time1, time2, &lev1, &lev2, &ivc, p, &nt850, iret );

	lev1 = 500;
	dg_nxts ( &nt500, &ier );
	dg_temp ( time1, time2, &lev1, &lev2, &ivc, p, &nt500, iret );

	strcpy ( p, "DWPC" );
	lev1 = 850;
	dg_nxts ( &ntd850, &ier );
	dg_dwpt ( time1, time2, &lev1, &lev2, &ivc, p, &ntd850, iret );

	/*
	 * Compute needed wind speed and direction
	 */
	lev1 = 850;
	dg_gobs ( time1, time2, &lev1, &lev2, &ivc, &nu850, &nv850, iret );

	dg_nxtv ( &nun850, &nvn850, &ier );
	dg_nrel ( _dggrid.dgg[nu850-1].grid, _dggrid.dgg[nv850-1].grid,
	          _dggrid.dgg[nun850-1].grid, _dggrid.dgg[nvn850-1].grid, iret );
	dg_frig ( &nu850, &ier );
	dg_frig ( &nv850, &ier );

	dg_nxts ( &nd850, &ier );
	pd_drct ( _dggrid.dgg[nun850-1].grid, _dggrid.dgg[nvn850-1].grid,
	          &_dgfile.kxyd, _dggrid.dgg[nd850-1].grid, iret );

	dg_nxts ( &ns850, &ier );
	pd_sped ( _dggrid.dgg[nun850-1].grid, _dggrid.dgg[nvn850-1].grid,
	    &_dgfile.kxyd, _dggrid.dgg[ns850-1].grid, iret );
	dg_frig ( &nun850, &ier );
	dg_frig ( &nvn850, &ier );

	lev1 = 500 ;
	dg_gobs ( time1, time2, &lev1, &lev2, &ivc, &nu500, &nv500, iret );

	dg_nxtv ( &nun500, &nvn500, &ier );
	dg_nrel ( _dggrid.dgg[nu500-1].grid, _dggrid.dgg[nv500-1].grid,
	          _dggrid.dgg[nun500-1].grid, _dggrid.dgg[nvn500-1].grid, iret );
	dg_frig ( &nu500, &ier );
	dg_frig ( &nv500, &ier );

	dg_nxts ( &nd500, &ier );
	pd_drct ( _dggrid.dgg[nun500-1].grid, _dggrid.dgg[nvn500-1].grid,
	    &_dgfile.kxyd, _dggrid.dgg[nd500-1].grid, iret );

	dg_nxts ( &ns500, &ier );
	pd_sped ( _dggrid.dgg[nun500-1].grid, _dggrid.dgg[nvn500-1].grid,
	    &_dgfile.kxyd, _dggrid.dgg[ns500-1].grid, iret );
	dg_frig ( &nun500, &ier );
	dg_frig ( &nvn500, &ier );

	/*
	 * Compute the sweat index
	 */
	pd_swet ( _dggrid.dgg[nt850-1].grid, _dggrid.dgg[ntd850-1].grid,
		  _dggrid.dgg[nt500-1].grid, _dggrid.dgg[ns850-1].grid,
		  _dggrid.dgg[ns500-1].grid, _dggrid.dgg[nd850-1].grid,
		  _dggrid.dgg[nd500-1].grid, &_dgfile.kxyd,
		  _dggrid.dgg[(*num)-1].grid, iret );
	dg_frig ( &nt850, &ier );
	dg_frig ( &nt500, &ier );
	dg_frig ( &ntd850, &ier );
	dg_frig ( &ns850, &ier );
	dg_frig ( &ns500, &ier );
	dg_frig ( &nd850, &ier );
	dg_frig ( &nd500, &ier );
    /*
     ********************** LATITUDE / LONGITUDE ************************
     */
    } else if ( strcmp ( parm, "LATR" ) == 0 ) {
	/*
	 * Check if navigation parameters have been computed.
	 */
	dg_ltln ( iret );
	if ( *iret != 0 )  return;

	/*
	 * Fill the grid with latitudes.
	 */
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    _dggrid.dgg[(*num)-1].grid[i] = _dggrid.dgg[_dgfile.idglat-1].grid[i];
	}
    } else if ( strcmp ( parm, "LONR" ) == 0 ) {
	/*
	 * Check if navigation parameters have been computed.
	 */
	dg_ltln ( iret );
	if ( *iret != 0 )  return;

	/*
	 * Fill the grid with longitudes.
	 */
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    _dggrid.dgg[(*num)-1].grid[i] = _dggrid.dgg[_dgfile.idglon-1].grid[i];
	}

    /*
     ************************* CORIOLIS FORCE ***************************
     */
    } else if ( (strcmp ( parm, "CORL" ) == 0 ) ||
                (strcmp ( parm, "F" ) == 0 ) ) {
	/*
	 * Compute Coriolis force with result on stack.
	 */
	df_corl ( num, iret );
	if ( *iret != 0 )  return;

    /*
     ************************* BETA EFFECT ******************************
     */
    } else if ( strcmp ( parm, "BETA" ) == 0 ) {
	/*
	 * Compute Beta effect with result on stack.
	 */
	df_beta ( num, iret );
	if ( *iret != 0 )  return;

    /*
     ************************* MAPSCALE FACTOR **************************
     */
    } else if (strcmp ( parm, "MSFX" ) == 0 ) {
	/*
	 * Compute x Mapscale factor
	 */
	dg_mscl ( iret );
	if ( *iret != 0 ) return;

	/*
	 * Fill the grid with Mapscale parameters.
	 */
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    _dggrid.dgg[(*num)-1].grid[i] = _dggrid.dgg[_mapscl.ixmscl-1].grid[i];
	}
    } else if (strcmp ( parm, "MSFY" ) == 0 ) {
	/*
	 * Compute y Mapscale factor
	 */
	dg_mscl ( iret );
	if ( *iret != 0 ) return;

	/*
	 * Fill the grid with mapscale parameters.
	 */
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    _dggrid.dgg[(*num)-1].grid[i] = _dggrid.dgg[_mapscl.iymscl-1].grid[i];
	}

    /*
     *********** XVAL/YVAL  get values of X/Y or R/THETA in graph coords.
     */
    } else if ( strcmp ( parm, "XVAL" ) == 0 ) {
	/*
	 * Compute the x value of the grids.
	 */
	df_xval ( num, iret );
	if ( *iret != 0 )  return;
    } else if ( strcmp ( parm, "YVAL" ) == 0 ) {
	/*	
	 * Compute the y value of the grid.
	 */
	df_yval ( num, iret );
	if ( *iret != 0 )  return;

    /*
     ************ IGPT/JGPT  get values of reference grid indices.
     */
    } else if ( strcmp ( parm, "IGPT" ) == 0 ) {
	/*
	 * Compute the x value of the grids.
	 */
	df_igpt ( num, iret );
	if ( *iret != 0 )  return;
    } else if ( strcmp ( parm, "JGPT" ) == 0 ) {
	/*
	 * Compute the y value of the grid.
	 */
	df_jgpt ( num, iret );
	if ( *iret != 0 )  return;

    /*
     ******************* GRIDS CONTAINING CONSTANTS *********************
     */
    } else if ( strcmp ( parm, "PI" ) == 0 ) {
	value = PI;
	dg_real ( &value, num, iret );
    } else if ( strcmp ( parm, "DTR" ) == 0 ) {
	value = DTR;
	dg_real ( &value, num, iret );
    } else if ( strcmp ( parm, "RTD" ) == 0 ) {
	value = RTD;
	dg_real ( &value, num, iret );
    } else if ( ( strcmp ( parm, "GRAVTY" ) == 0 ) ||
	   	( strcmp ( parm, "GRAVITY" ) == 0 ) ) {
	value = GRAVTY;
	dg_real ( &value, num, iret );
    } else if ( strcmp ( parm, "KAPPA" ) == 0 ) {
	value = RKAPPA;
	dg_real ( &value, num, iret );
    } else if ( strcmp ( parm,  "E" ) == 0 ) {
	value = 2.71828182;
	dg_real ( &value, num, iret );

    /*
     ********************** LAND-SEA MASK *******************************
     */
    } else if ( strcmp ( parm, "SEA" ) == 0 ||
                strcmp ( parm, "LAND" ) == 0 ) {
	if ( strcmp ( parm, "SEA" ) == 0 )  ichk = 1;
	if ( strcmp ( parm, "LAND" ) == 0 ) ichk = 0;

	/*
	 * Check if land-sea grid has been set
	 */
	dg_lncx ( iret );
	if ( *iret != 0 )  return;

	/*
	 * Set to missing the grid land points
	 */
	for ( i = 0; i < _dgfile.kxyd; i++ ) {
	    if ( (int)_dggrid.dgg[_dglndc.lndsea-1].grid[i] == ichk ) {
		_dggrid.dgg[(*num)-1].grid[i] = RMISSD;
	    } else {
		_dggrid.dgg[(*num)-1].grid[i] = 1;
	    }
	}

    /*
     ********************** PRECIPITATION *******************************
     */
    } else if ( parm[0] == 'P' || parm[0] == 'S' || parm[0] == 'C' ||
		parm[0] == 'W' || parm[0] == 'I' || parm[0] == 'Z' ||
		parm[0] == 'A' || parm[0] == 'H' || parm[0] == 'G' ||
                parm[0] == 'N' || parm[0] == 'R' || parm[0] == 'L' ) {
	dg_prft ( time1, time2, level1, level2, ivcord, parm, num, iret );

    /*
     ********************** END *****************************************
     */
    }

    /*
     * Update internal grid information.
     */
    if ( *iret == 0 ) {
	dg_upsg ( time1, time2, level1, level2, ivcord, &_dgfile.idlun,
	    parm, num, iret );
    }

    return;
}
