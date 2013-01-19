#include "na.h"

void na_rprm ( const char *prmtbl, int *iret )
/************************************************************************
 * na_rprm								*
 *									*
 * This routine will read all the necessary GRIB tables and save the	*
 * contents to common block variables.					*
 *									*
 * na_rprm ( prmtbl, iret )						*
 *									*
 * Input parameters:							*
 *	*prmtbl		const char	Parameter table filename	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  -14 = Cannot open parm table	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/EAI	12/93						*
 * D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
 * M. Li/SAIC		04/04	Added mhzrmp, and mdrct			*
 * H. Zeng/SAIC		01/05	Initialize character strings		*
 * R. Tian/SAIC		07/06	Recoded from Fortran			*
 * D.W.Plummer/NCEP	04/07	Add nin checking for sscanf		*
 ************************************************************************/
{
    char buffer[133], tmptbl[133];
    char tmprnam[33], tmpunit[21], tmparms[13];
    int tmpscal, tmhzrmp, tmdrct;
    float trmssvl;
    int istart, iend, iprm, len, i, minus1, ier, nin;
    FILE *fp;
/*----------------------------------------------------------------------*/
    *iret = 0;
    minus1 = -1;

    /*
     * Open the parameter table.
     */
    fp = cfl_tbop ( (char *)prmtbl, "grid", iret );
    if ( *iret != 0 ) {
	er_wmsg ( "NA", &minus1, (char *)prmtbl, &ier,
	          strlen("NA"), strlen(prmtbl) );
	*iret = -14;
	return;
    }

    /*
     * initialize some character strings.
     */
    cst_uclc ( (char *)prmtbl, tmptbl, &ier);
    if ( strncmp ( tmptbl, "wmo", 3 ) == 0 ) {
	istart = 1;
	iend   = 127;
    } else {
	istart = 128;
	iend   = MAXNUM;
    }

    for ( i = istart - 1; i < iend; i++ ) {
	_naparm.mprnam[i][0] = '\0';
	_naparm.mpunit[i][0] = '\0';
	_naparm.mparms[i][0] = '\0';

	_naparm.mpscal[i] = IMISSD;
	_naparm.mhzrmp[i] = IMISSD;
	_naparm.mdrct[i]  = IMISSD;
	_naparm.rmssvl[i] = RMISSD;
    }

    /*
     * Read each line from the table and save the
     * information in the common block variables.
     */
    tmprnam[32] = '\0';
    tmpunit[20] = '\0';
    while ( ! feof ( fp ) ) {
    	cfl_trln ( fp, sizeof(buffer), buffer, &ier );
	if ( ier != 0 ) break;

	nin = sscanf ( buffer, "%d %32c  %20c %s %d %f %d %d",
	         &iprm, tmprnam, tmpunit, tmparms, &tmpscal,
		 &trmssvl, &tmhzrmp, &tmdrct );
	if ( nin < 7 )  tmhzrmp = 0;
	if ( nin < 8 )  tmdrct  = 0;
        cst_lstr ( tmprnam, &len, &ier );
	tmprnam[len] = '\0';
	cst_lstr ( tmpunit, &len, &ier );
	tmpunit[len] = '\0';

	strcpy ( _naparm.mprnam[iprm-1], tmprnam );
	strcpy ( _naparm.mpunit[iprm-1], tmpunit );
	strcpy ( _naparm.mparms[iprm-1], tmparms );
	_naparm.mpscal[iprm-1] = tmpscal;
	_naparm.rmssvl[iprm-1] = trmssvl;
	_naparm.mhzrmp[iprm-1] = tmhzrmp;
	_naparm.mdrct [iprm-1] = tmdrct;
    }
    cfl_clos ( fp, &ier );

    return;
}
