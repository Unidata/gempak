#include "na.h"

void na_rtbl ( const int *ieditn, const int *icenter, const int *icodtbl, 
               const char *wtbl, const char *ntbl, const char *vtbl,
	       const char *ctbl, int *iret )
/************************************************************************
 * na_rtbl								*
 *									*
 * This routine will read all the necessary GRIB tables and save the	*
 * contents to common block variables.					*
 *									*
 * na_rtbl ( ieditn, icenter, icodtbl, wtbl, ntbl, vtbl, ctbl, iret )	*
 *									*
 * Input parameters:							*
 *	*ieditn		const int	GRIB edition number		*
 *	*icenter	const int	Originating center number	*
 *	*icodtbl	const int	Code table version number	*
 *	*wtbl 		const char	GRIB table for WMO		*
 *	*ntbl 		const char	GRIB table for orig center	*
 *	*vtbl 		const char	GRIB table for vertical coord	*
 *	*ctbl 		const char	GRIB table for centers		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					    0 = normal			*
 *					  -13 = Cannot open vcord table	*
 *					  -14 = Cannot open parm table	*
 *					  -19 = Cannot open cent table	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/EAI	12/93						*
 * D. Keiser/GSC	12/95		Changed FL_TOPN to FL_TBOP	*
 * D.W.Plummer/NCEP	 2/96		Added GBTBLS option processing	*
 * T. Piper/GSC		11/98		Updated prolog			*
 * H. Zeng/SAIC		01/05		Re-initialize some variables	*
 * R. Tian/SAIC		07/06		Recoded from Fortran		*
 * S. Gilbert/NCEP	01/07		Removed error check             *
 ************************************************************************/
{
    char centtb[73], vcrdtb[73], edtn[4], vers[4];
    char parmt1[73], parmt2[73], buffer[133];
    char tmcnam[65], tmcabb[17];
    char tmvcnam[33], tmvunit[21], tmvcord[5];
    int num, len, tmvscal, minus1, i, ier;
    FILE *centfp, *vcrdfp;
/*----------------------------------------------------------------------*/
    *iret = 0;
    minus1 = -1;

    cst_inch ( *ieditn, edtn, &ier );
    cst_inch ( *icodtbl, vers, &ier );

    /*
     * Check for originating center table.
     */
    if ( ctbl[0] == '?' ) {
    	strcpy ( centtb, "cntrgrib" );
	strcat ( centtb, edtn );
	strcat ( centtb, ".tbl" );
    } else {
	strcpy ( centtb, ctbl );
    }

    if ( _nacent.meditn != *ieditn ||
         strcmp ( _nacent.cenfil, centtb) != 0 ) {
	/*
	 * Set the center table filename based on the GRIB
	 * edition number. but first reinitialize character strings
	 * mcnam and mcabb to blank.
	 */
	for ( i = 0; i < MAXNUM; i++ ) {
	    _nacent.mcnam[i][0] = '\0';
	    _nacent.mcabb[i][0] = '\0';
	}

	printf ( "Changing center table to %s\n", centtb );

	centfp = cfl_tbop ( centtb, "grid", iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "NA", &minus1, centtb, &ier,
	              strlen("NA"), strlen(centtb) );
	    *iret = -19;
	    return;
	}

	tmcnam[64] = '\0';
	while ( !feof ( centfp ) ) {
	    cfl_trln ( centfp, sizeof(buffer), buffer, &ier );
	    if ( ier != 0 ) break;

            sscanf ( buffer, "%d %64c %s", &num, tmcnam, tmcabb );
            cst_lstr ( tmcnam, &len, &ier );
            tmcnam[len] ='\0';
            strcpy ( _nacent.mcnam[num-1], tmcnam );
            cst_uclc ( tmcabb, _nacent.mcabb[num-1], &ier );
	}
	cfl_clos ( centfp, &ier );
    }
    strcpy ( _nacent.cenfil, centtb );

    if ( vtbl[0] == '?' ) {
    	strcpy ( vcrdtb, "vcrdgrib" );
	strcat ( vcrdtb, edtn );
	strcat ( vcrdtb, ".tbl" );
    } else {
	strcpy ( vcrdtb, vtbl );
    }

    if ( _nacent. meditn != *ieditn ||
         strcmp ( vcrdtb, _navcrd.vrtfil ) != 0 ) {
	/*
	 * Set the vertical coordinate table filename based on the
	 * GRIB edition number or an input filename. but first
	 * reinitialize character strings mvcnam, mvunit, mvcord and
	 * mvscal.
	 */
	for ( i = 0; i < MAXNUM; i++ ) {
	    _navcrd.mvcnam[i][0] = '\0';
	    _navcrd.mvunit[i][0] = '\0';
	    _navcrd.mvcord[i][0] = '\0';
	    _navcrd.mvscal[i] = IMISSD;
	}

	printf ( "Changing vertical coord table to %s\n", vcrdtb );

	vcrdfp = cfl_tbop ( vcrdtb, "grid", iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "NA", &minus1, vcrdtb, &ier,
	              strlen("NA"), strlen(vcrdtb) );
	    *iret = -13;
	    return;
	}

	tmvcnam[32] = '\0';
	tmvunit[20] = '\0';
	while ( !feof ( vcrdfp ) ) {
	    cfl_trln ( vcrdfp, sizeof(buffer), buffer, &ier );
	    if ( ier != 0 ) break;

	    /*
            sscanf ( buffer, "%d %32c  %20c %s %d", &num, tmvcnam, tmvunit,
	             tmvcord, &tmvscal );
	    */
	    sscanf ( &buffer[0],  "%d",   &num );
	    sscanf ( &buffer[4],  "%32c", tmvcnam );
	    sscanf ( &buffer[38], "%20c", tmvunit );
	    sscanf ( &buffer[59], "%s",   tmvcord );
	    sscanf ( &buffer[68], "%d",  &tmvscal );

            cst_lstr ( tmvcnam, &len, &ier );
            tmvcnam[len] ='\0';
            strcpy ( _navcrd.mvcnam[num-1], tmvcnam );
            cst_lstr ( tmvunit, &len, &ier );
            tmvunit[len] ='\0';
            strcpy ( _navcrd.mvunit[num-1], tmvunit );
	    strcpy ( _navcrd.mvcord[num-1], tmvcord );
	    _navcrd.mvscal[num-1] = tmvscal;
	}
	cfl_clos ( vcrdfp, &ier );
    }
    strcpy ( _navcrd.vrtfil, vcrdtb );
    _nacent.meditn = *ieditn;

    /*
     * If the parameter table is the same version as used in the
     * previous message, return.
     */
    if ( wtbl[0] == '?' ) {
    	strcpy ( parmt1, "wmogrib" );
	strcat ( parmt1, vers );
	strcat ( parmt1, ".tbl" );
    } else {
	strcpy ( parmt1, wtbl );
    }
    if ( ntbl[0] == '?' ) {
    	strcpy ( parmt2, _nacent.mcabb[(*icenter)-1] );
	strcat ( parmt2, "grib" );
	strcat ( parmt2, vers );
	strcat ( parmt2, ".tbl" );
    } else {
	strcpy ( parmt2, ntbl );
    }
 
    if ( *icodtbl != _naparm.mcodtbl ||
	 strcmp ( parmt1, _naparm.wmofil ) != 0 ||
	 strcmp ( parmt2, _naparm.prmfil ) != 0 ) {
	/*
	 * reinitialize integers mcodtbl, mcenter to IMISSD and 
	 * character strings wmofil, prmfil to blank.
	 */
	_naparm.mcodtbl = IMISSD;
	_naparm.mcenter = IMISSD;
	_naparm.wmofil[0] = '\0';
	_naparm.prmfil[0] = '\0';

	printf ( "Changing WMO parameter table to %s\n", parmt1 );

	na_rprm ( parmt1, iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "NA", &minus1, parmt1, &ier,
	              strlen("NA"), strlen(parmt1) );
	    return;
	}
	strcpy ( _naparm.wmofil, parmt1 );

	printf ( "Changing center parameter table to %s\n", parmt2 );

	na_rprm ( parmt2, &ier );
	
	strcpy ( _naparm.prmfil, parmt2 );
	_naparm.mcodtbl = *icodtbl;
	_naparm.mcenter = *icenter;
    }

    /*
     * Check for a new center identifier.
     */
    if ( *icenter != _naparm.mcenter ||
         strcmp ( parmt2, _naparm.prmfil ) != 0 ) {
	/*
	 * reinitialize integer mcenter to IMISSD and 
	 * character string prmfil to blank.
	 */
	_naparm.mcenter = IMISSD;
	_naparm.prmfil[0] = '\0';

	printf ( "Changing center parameter table to %s\n", parmt2 );

	na_rprm ( parmt2, iret );
	if ( *iret != 0 ) {
	    er_wmsg ( "NA", &minus1, parmt2, &ier,
	              strlen("NA"), strlen(parmt2) );
	}
	strcpy ( _naparm.prmfil, parmt2 );
	_naparm.mcenter = *icenter;
    }

    return;
}
