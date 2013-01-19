#include "na.h"

void na_rhdr ( const int *jtime, const int *jaccm, const int *jlevel1,
               const int *jlevel2, const int *jvcord, const int *jparm,
               const int *icodtbl, const int *pdsext, char *gdattm1,
	       char *gdattm2, int *level1, int *level2, int *ivcord,
	       char *parm, int *iscal, float *rmsval, int *ispds,
               char *cpds, int *ihzrmp, int *idrct, int *iret )
/************************************************************************
 * na_rhdr								*
 *									*
 * This routine will convert the GRIB Product Definition Section info	*
 * to values needed to construct a GEMPAK header.			*
 *									*
 * na_rhdr ( jtime, jaccm, jlevel1, jlevel2, jvcord, jparm, icodtbl,	*
 *           pdsext, gdattm1, gdattm2, level1, level2, ivcord, parm,	*
 *           iscal, rmsval, ispds, cpds, ihzrmp, idrct, iret )		*
 *									*
 * Input parameters:							*
 *	*jtime		const int	Model run date, time, forecast	*
 *	*jaccm		const int	Accumulation time		*
 *	*jlevel1	const int	Level				*
 *	*jlevel2	const int	Level				*
 *	*jvcord		const int	Vertical coordinate		*
 *	*jparm		const int	Parameter number		*
 *	*icodtbl	const int	Code table number		*
 *	*pdsext		const int	Flag to add PDS extension	*
 *									*
 * Output parameters:							*
 *	*gdattm1	char		GEMPAK date/time		*
 *	*gdattm2	char		GEMPAK date/time		*
 *	*level1		int		GEMPAK level info		*
 *	*level2		int		GEMPAK level info		*
 *	*ivcord		int		GEMPAK vertical coordinate	*
 *	*parm		char		GEMPAK parameter name		*
 *	*iscal		int		Scale conversion value		*
 *	*rmsval		float		Missing value for grid		*
 *	*ispds		int						*
 *	*cpds		char						*
 *	*ihzrmp		int		Horizontal remapping flag	*
 *	*idrct		int		Directional flag		*
 *	*iret		int		Return code			*
 *					   +3 = No valid parm found	*
 *					   +2 = No valid vert coord	*
 *					  -13 = Cannot open vcord table	*
 *					  -14 = Cannot open parm table	*
 *					  -17 = Error in date/time	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/EAI	 7/93						*
 * S. Jacobs/EAI	12/93		Copied from NAGHDR		*
 *					   Eliminated reading table	*
 * T. Piper/GSC		11/98		Updated prolog			*
 * D.W.Plummer/NCEP	 4/01		Disregard PDS extension suffix	*
 * D.W.Plummer/NCEP	 5/01		Add option for PDS extension	*
 * T. Lee/SAIC		 1/04		Handled precip accm. >= 100 hrs	*
 * M. Li/SAIC		 4/04		Add ihzrmp, and idrct		*
 * R. Tian/SAIC		 7/06		Recoded from Fortran		*
 ************************************************************************/
{
    char caccm[11], stmp[4], schr, *idash;
    int lenp, len, i, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Set the GEMPAK date/time and forecast time. Also set the
     * accumulation time string.
     */
    gdattm1[0] = '\0';
    gdattm2[0] = '\0';
    ctg_itoc ( jtime, gdattm1, &ier );
    if ( ier != 0 ) {
	*iret = -17;
	return;
    }
    if ( *jaccm == IMISSD ) {
	strcpy ( caccm, "XX" );
    } else {
	cst_inch ( *jaccm, caccm, &ier );
    }

    /*
     * Set the vertical coordinate and the levels.
     */
    na_levl ( jvcord, jlevel1, jlevel2, ivcord, level1, level2, &ier );

    /*
     * If there are no valid values, return.
     */
    if ( ier != 0 ) {
	*iret = +2;
	return;
    }

    /*
     * Set the parameter.
     *
     * If there is no valid value, return.
     */
    if ( _naparm.mparms[(*jparm)-1][0] == '\0' ) {
	*iret = +3;
	return;
    }
    strcpy ( parm, _naparm.mparms[(*jparm)-1] );
    *iscal  = _naparm.mpscal[(*jparm)-1];
    *rmsval = _naparm.rmssvl[(*jparm)-1];
    *ihzrmp = _naparm.mhzrmp[(*jparm)-1];
    *idrct  = _naparm.mdrct [(*jparm)-1];

    /*
     * Put any special parameters here...
     */
    if ( ( *jparm == 1 ) && ( *jvcord == 102 ) ) strcpy ( parm, "PMSL" );

    /*
     * Adjust for PDS extension information, if any.
     */
    if ( *ispds == -1 ) {
	strcpy ( parm, cpds );
    } else if ( *ispds == 1 ) {
	/*
	 * Add PDS extension if pdsext is true.
	 */
	if ( *pdsext == G_TRUE ) {
	    cst_lstr ( parm, &lenp, &ier );
	    strcpy ( &parm[lenp], cpds );
	}
    }

    /*
     * If the parameter is an accumulation or difference, add
     * the length of time to the parameter name.
     */
    idash = strstr ( parm, "--" );
    if ( ( *jaccm >= 0 ) && ( *jaccm < 10 ) ) {
        stmp[0] = '0';
	stmp[1] = caccm[0];
	stmp[2] = '\0';
    } else if ( *jaccm >= 10 && *jaccm < 100 ) {
	strncpy ( stmp, caccm, 2 );
	stmp[2] = '\0';
    } else if ( *jaccm >= 100 && *jaccm < 1000 ) {
	cst_lstr ( caccm, &lenp, &ier );
	strncpy ( stmp, &caccm[lenp-3], 3 );
	stmp[3] = '\0';
    } else {
	cst_lstr ( caccm, &lenp, &ier );
	strncpy ( stmp, &caccm[lenp-2], 2 );
	stmp[2] = '\0';
    }
    cst_lstr ( parm, &lenp, &ier );
    if ( idash != NULL ) {
	if ( *jaccm < 100 ) {
	    for ( i = 0; i < 2; i++ ) *(idash+i) = stmp[i];
	} else {
	    if ( (int)( idash - parm ) == 1 ) {
		schr = parm[lenp-1];
		strcpy ( &parm[1], stmp );
		len = strlen(parm);
		parm[len] = schr;
		parm[len+1] = '\0';
	    } else {
	    	strcpy ( idash, stmp );
	    }
	}
    } else {
	if ( *jaccm > 0 ) {
	    if ( lenp <= 10 )  strcpy ( &parm[lenp], stmp );
	}
    }

    return;
}
