#include "dg.h"

void dgc_glev ( const int *intry, const char *gdattm1, const char *gdattm2,
                const int *ivcord, const int *maxlev, int *levarr1,
	        int *levarr2, int *nlev, int *iret )
/************************************************************************
 * dgc_glev                                                             *
 *                                                                      *
 * This subroutine returns all the levels present in a grid file for    *
 * a given date and vertical coordinate.  The levels returned are       *
 * not sorted.                                                          *
 *                                                                      *
 * dgc_glev ( intry, gdattm1, gdattm2, ivcord, maxlev, levarr1, levarr2,*
 *            nlev, iret )    						*
 *                                                                      *
 * Input parameters:                                                    *
 *      *intry		const int	GDFILE entry number (usually 1) *
 *      *gdattm1	const char	GEMPAK times                    *
 *      *gdattm2	const char	GEMPAK times                    *
 *      *ivcord		const int	Vertical coordinate             *
 *      *maxlev		const int	Maximum number of levels        *
 *                                                                      *
 * Output parameters:                                                   *
 *      *levarr1	int		Levels found                    *
 *      *levarr2	int		Levels found                    *
 *      *nlev		int		Number of levels found          *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -55 = file open failed		*
 *                                      -66 = cannot get levels		*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          1/04                                           *
 * R. Tian/SAIC		 2/04	Modified to use new GD file management	*
 * K. Brill/HPC		 3/04	Use tlast(1) in template only if input	*
 *				gdattm is ' '; CALL FL_MNAM; clean up	*
 * R. Tian/SAIC		 4/04	Added INTRY in calling sequence		*
 * T. Lee/SAIC		 2/05	Initialized nlev = 0			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float adum1, adum2;
    char time1[21], time2[21], filnam[MXFLSZ+1], tmpnam[MXFLSZ+1];
    int ifn, mxgd, zero, i, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    filnam[0] = '\0';
    *nlev = 0;
    zero = 0;

    if ( (*intry) <= 0 || (*intry) > NGDFLS ) {
	i = 0;
    } else {
	i = (*intry) - 1;
    }

    if ( gdattm1[0] == '\0' ) {
	strcpy ( time1, _dgfile.tlast[i] );
	time2[0] = '\0';
    } else {
	strcpy ( time1, gdattm1 );
	strcpy ( time2, gdattm2 );
    }

    if ( _nfile.ntmplt[i][0] != '\0' ) {
	/*
	 * This entry is a template
	 */
	cfl_mnam ( time1, _nfile.ntmplt[i], tmpnam, &ier );
	strcpy ( filnam, _nfile.gflpth[i] );
	strcat ( filnam, "/" );
	strcat ( filnam, tmpnam );
    } else {
	/*
	 * This entry is an actual file
	 */
	strcpy ( filnam, _nfile.crtfnm[i] );
    }

    /*
     * Open the required file and get the levels.
     */
    gd_open ( filnam, &_nfile.outflg[i], &zero, &zero, &ifn, &adum1, &adum2,
              &mxgd, &ier, strlen(filnam) );
    if ( ier != 0 ) {
	*iret = -55;
	return;
    }
    cgd_glev ( &ifn, time1, time2, ivcord, maxlev, levarr1, levarr2, nlev, &ier );
    if ( ier != 0 ) {
	*iret = -66;
	return;
    }

    return;
}
