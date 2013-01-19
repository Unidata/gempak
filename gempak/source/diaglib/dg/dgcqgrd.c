#include "dg.h"

void dgc_qgrd ( const char *time1, const char *time2, const int *level1,
	        const int *level2, const int *ivcord, const char *parm,
	        int *exist, int *iret )
/************************************************************************
 * dgc_qgrd                                                             *
 *                                                                      *
 * This subroutine determines whether a grid exists in the output file. *
 *                                                                      *
 * dgc_qgrd ( time1, time2, level1, level2, ivcord, parm, exist, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *time1		const char	GEMPAK times                    *
 *      *time2		const char	GEMPAK times                    *
 *      *level1		const int	GEMPAK grid levle               *
 *      *level2		const int	GEMPAK grid levle               *
 *      *ivcord		const int	Vertical coordinate             *
 *      *parm		const char	GEMPAK parameter name           *
 *                                                                      *
 * Output parameters:                                                   *
 *	*exist		int		Existence of a grid		*
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -60 = grid file open failed     *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04                                           *
 * R. Tian/SAIC		 3/06	Recoded from Fortran			*
 ************************************************************************/
{
    char filnam[MXFLSZ+1], fullnm[MXFLSZ+1];
    float adum1, adum2;
    int iout, ifn, mxgd, ign, zero, i, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    *exist = G_FALSE;
    for ( i = 0; i < NGDFLS; i++ ) {
	if ( _nfile.outflg[i] == G_TRUE ) {
	    iout = i;
	    *exist = G_TRUE;
	    break;
	}
    }
    if ( exist == G_FALSE ) return;

    /*
     * Query the existence of the grid
     */
    if ( _nfile.ntmplt[iout][0] != '\0' ) {
	/*
	 * This entry is a template
	 */
	cfl_mnam ( (char *)time1, _nfile.ntmplt[iout], filnam, &ier );
	strcpy ( fullnm, _nfile.gflpth[iout] );
	strcat ( fullnm, "/" );
	strcat ( fullnm, filnam );
    } else {
	/*
	 * This entry is an actual file
	 */
	strcpy ( fullnm, _nfile.crtfnm[iout] );
    }
    gd_open ( fullnm, &_nfile.outflg[iout], &zero, &zero, &ifn, &adum1, &adum2,
              &mxgd, &ier, strlen(fullnm) );
    if ( ier != 0 ) {
	*iret = -60;
	return;
    }
    cgd_gnum ( &ifn, time1, time2, level1, level2, ivcord, parm, &ign, &ier );
    *exist = ( ier == 0 ) ? G_TRUE : G_FALSE;

    return;
}
