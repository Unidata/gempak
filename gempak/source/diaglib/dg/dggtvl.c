#include "dg.h"

void dg_gtvl ( int *igu1, int *igv1, int *igu2, int *igv2, int *iret )
/************************************************************************
 * dg_gtvl								*
 *									*
 * This subroutine gets the next operand on the stack for a layer	*
 * computation.  The operand must be a vector; otherwise, an error is	*
 * returned.  The internal grid numbers of the u and v components are	*
 * returned for the two levels.  Note the styles of operand names that	*
 * are permitted.							*
 *									*
 * dg_gtvl ( igu1, igv1, igu2, igv2, iret )				*
 *									*
 * Output parameters:							*
 *	*igu1		int		Grid number for level1 u	*
 *	*gv1		int		Grid number for level1 v	*
 *	*gu2		int		Grid number for level2 u	*
 *	*gv2		int		Grid number for level2 v	*
 *	*ret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					 -9 = calling sequence error	*
 *					-10 = internal grid list full	*
 *					-11 = must be a vector		*
 *					-12 = must be a scalar		*
 *					-13 = must be from grid file	*
 *					-16 = map projection invalid	*
 *					-17 = LEVEL must be a layer	*
 *					-18 = TIME must be a range	*
 *					-20 = stack is full		*
 *					-21 = stack is empty		*
 *					-22 = TIME is invalid		*
 *					-23 = LEVEL is invalid		*
 *					-24 = IVCORD is invalid		*
 *					-26 = layer of layers invalid	*
 *					-27 = time range layer invalid	*
 **									*
 * Log:									*
 * G. Huffman/GSC	 9/88	Adapted from DG_GETV, DG_GETL, DV_OBS	*
 * M. desJardins/GSFC	 5/89	Eliminate vector syntax with /		*
 * K. Brill/GSC		 1/90	Write error message if not 2 levels	*
 * M. desJardins/NMC	 3/92	Check for WND, WIND			*
 * K. Brill/NMC		 4/93	CALL DG_FNDV & fixes for reading vectors*
 *				of the form Uxxx, Vxxx			*
 * M. desJardins/NMC	 8/93	Eliminate duplicate names		*
 * K. Brill/HPC		12/01	CALL DG_SSUB and DG_ESUB		*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char time1[21], time2[21], gvect[14], parm[14], gvec[14], depend[5];
    int ignumu, ignumv, level1, level2, lobs1, lobs2, ivcord, num[2],
        igu[2], igv[2], minus1, zero, i, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    minus1 = -1;
    zero = 0;
    dg_ssub ( iret );

    igu[0] = 0;
    igv[0] = 0;
    igu[1] = 0;
    igv[1] = 0;

    /*
     * Get grid information.
     */
    dg_topv  ( gvect, &ignumu, &ignumv, time1, time2, &level1, &level2,
        &ivcord, parm, iret );
    if ( *iret != 0 ) return;

    /*
     * Check to see if grid has already been computed; if so,
     * this is an error.
     */
    if ( _dgstck.stack[_dgstck.itop][0] == '\0' ) {
	*iret = -13;
	strcpy ( _dgerr.errst, parm );
	return;
    }

    /*
     * Check that there are two levels.
     */
    if ( ( level1 == -1 ) || ( level2 == -1 ) || ( level1 == level2 ) ) {
	*iret = -17;
	dg_merr ( "", "", "", &level1, &level2, &minus1, _dgerr.errst, &ier );
	return;
    }

    /*
     * Check to see if the vector is already in internal grids.
     */
    lobs1 = level1;
    lobs2 = -1;
    strcpy ( gvec, gvect );
    dg_fndv ( time1, time2, &lobs1, &lobs2, &ivcord, gvec,
              &igu[0], &igv[0], &ier );
    strcpy ( gvec, gvect );
    lobs1 = level2;
    dg_fndv ( time1, time2, &lobs1, &lobs2, &ivcord, gvec,
              &igu[1], &igv[1], &ier );

    /*
     * GEO family . . . Winds which are actually calculated from DEPEND,
     *			 the dependent vertical variable.
     */
    if ( ( strcmp ( gvect, "GEO" ) == 0 ) ||
         ( strcmp ( gvect, "AGE" ) == 0 ) ||
	 ( strcmp ( gvect, "THRM" ) == 0 ) ||
	 ( strcmp ( gvect, "ISAL" ) == 0 ) ) {
	/*
	 * Set the value of DEPEND.
	 */
	if ( ivcord == 1 ) {
	    strcpy ( depend, "HGHT" );
	} else if ( ivcord == 2 ) {
	    strcpy ( depend, "PSYM" );
	} else if ( ivcord == 3 ) {
	    strcpy ( depend, "PRES" );
	} else if ( ivcord == 0 ) {
	    *iret = -24;
	    strcpy ( _dgerr.errst, "%NONE" );
	    return;
	}

	/*
	 * Replace the vector name with the appropriate scalar
	 * name and get the grids at the two levels.
	 */
	dg_rpls ( depend, &zero, iret );
	if ( *iret != 0 ) return;
	dg_getl ( &num[0], &num[1], iret );
	if ( *iret != 0 ) return;

	/*
	 * Put level I on the stack, invoke the correct wind
	 * routine, and get the answer.  THRM and ISAL invoke
	 * errors because they need multiple reads.
	 */
	for ( i = 0; i < 2; i++ ) {
	    if ( igu[i] == 0 ) {
		dg_puts ( &num[i], iret );
		if ( *iret != 0 ) return;

		if ( strcmp ( gvect, "GEO" ) == 0 ) {
		    dv_geo ( iret );
		} else if ( strcmp ( gvect, "AGE" ) == 0 ) {
		    dv_age ( iret );
		} else if ( ( strcmp ( gvect, "THRM" ) == 0 ) ||
			    ( strcmp ( gvect, "THRML" ) == 0 ) ) {
		    *iret = -26;
		    return;
		} else if ( strcmp ( gvect, "ISAL" ) == 0 ) {
		    *iret = -27;
		    return;
		}

		dg_getv ( &igu[i], &igv[i], iret );
		if ( *iret != 0 ) return;
	    }
	}

	/*
	 * OBS . . . The observed wind field with parameter conversions
	 * 	    as needed.
	 */
    } else if ( ( strcmp ( gvect, "OBS" ) == 0 ) ||
                ( strcmp ( gvect, "WND" ) == 0 ) ||
		( strcmp ( gvect, "WIND" ) == 0 ) ) {
	for ( i = 0; i < 2; i++ ) {
	    if ( igu[i] == 0 ) {
		/*
		 * Set a single level and get wind grid numbers.
		 */
		if ( i == 0 ) {
		    lobs1 = level1;
		} else if ( i == 1 ) {
		    lobs1 = level2;
		}
		lobs2 = -1;
		dg_gobs ( time1, time2, &lobs1, &lobs2, &ivcord,
		          &igu[i], &igv[i], iret );
		if ( *iret != 0 ) return;
	    }
	}

	/*
	 * Decrement ITOP, since it isn't done in DG_GOBS.
	 */
	_dgstck.itop--;
    } else {
	/*
	 * Try to read the vector components from the file.
	 */
	lobs2 = -1;
	for ( i = 0; i < 2; i++ ) {
	    if ( igu[i] == 0 ) {
	        if ( i == 0 ) {
		    lobs1 = level1;
		} else if ( i == 1 ) {
		    lobs1 = level2;
		}
		strcpy ( parm, "U" );
		strcat ( parm, gvect );
		dg_rgrd ( time1, time2, &lobs1, &lobs2, &ivcord, parm,
			  &igu[i], &ier );
		if ( ier == 0 ) {
		    strcpy ( parm, "V" );
		    strcat ( parm, gvect );
		    dg_rgrd ( time1, time2, &lobs1, &lobs2, &ivcord, parm,
			      &igv[i], &ier );
		}
	    }
	}
	_dgstck.itop--;
    }

    /*
     * In all cases ITOP has been decremented by 1.  Load the four grid
     * numbers and return.
     */
    *igu1 = igu[0];
    *igv1 = igv[0];
    *igu2 = igu[1];
    *igv2 = igv[1];
    if ( *igu1 == 0 || *igu2 == 0 || *igv1 == 0 || *igv2 == 0 ) *iret = -11;
   
    dg_esub ( igu1, igv1, igu2, igv2, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
