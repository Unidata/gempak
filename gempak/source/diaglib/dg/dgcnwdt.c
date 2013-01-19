#include "dg.h"

void dgc_nwdt ( const float *grid, const char *time1, const char *time2,
	        const int *level1, const int *level2, const int *ivcord,
	        const char *parm, const int *igdhdr, const char *gpack,
	        const int *rplc, int *iret )
/************************************************************************
 * dgc_nwdt                                                             *
 *                                                                      *
 * This subroutine writes a packed grid.				*
 *                                                                      *
 * dgc_nwdt ( grid, time1, time2, level1, level2, ivcord, parm, igdhdr,	*
 *            gpack, rplc, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*grid		const float	Grid to write out		*
 *      *time1		const char	GEMPAK times                    *
 *      *time2		const char	GEMPAK times                    *
 *      *level1		const int	GEMPAK grid levle               *
 *      *level2		const int	GEMPAK grid levle               *
 *      *ivcord          const int	Vertical coordinate             *
 *      *parm            const char	GEMPAK parameter name           *
 *	*igdhdr		const int	Grid header			*
 *	*gpack		const char	Grid packing from user input	*
 *	*rplc		const int	Flag to replace existing grid	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 *                                      -61 = grid file open failed     *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04                                           *
 * R. Tian/SAIC		 1/05		Changed LLNANV to LLNNAV	*
 * R. Tian/SAIC		 5/05		Added outflg checking		*
 * D.W.Plummer/NCEP	 5/05		Add calls to ER_WMSG		*
 * R. Tian/SAIC		 3/06		Recoded from Fortran		*
 ************************************************************************/
{
    char filnam[MXFLSZ+1], fullnm[MXFLSZ+1];
    float rnvblk[LLNNAV], adum1;
    int same;
    int iout, ifn, mxgd, ipktyp, nbits, navsz, zero, i, ier, ierr;
/*----------------------------------------------------------------------*/
    *iret = 0;
    navsz = LLNNAV;
    zero = 0;

    iout = -1;
    for ( i = 0; i < NGDFLS; i++ ) {
	if ( _nfile.outflg[i] == G_TRUE ) {
	    iout = i;
	    break;
	}
    }
    if ( iout == -1 ) {
	*iret = -61;
	return;
    }

    /*
     * Open the output file
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

    gd_open ( fullnm , &_nfile.outflg[iout], &zero, &navsz, &ifn, &adum1,
              rnvblk, &mxgd, &ier, strlen(fullnm) );
    if ( ier != 0 ) {
	er_wmsg ( "GD", &ier, " ", &ierr, strlen("GD"), strlen(" ") );
	*iret = -61;
	return;
    }

    /*
     * Compare the returned navigation with the internal navigation
     */
    grc_cnav ( rnvblk, _dgfile.snav, &navsz, &same, &ier );
    if ( ier != 0 ) {
	er_wmsg ( "GR", &ier, " ", &ierr, strlen("GD"), strlen(" ") );
    }
    if ( same == G_FALSE ) {
	*iret = -64;
	return;
    }

    /*
     * Write the packed grid
     */
    grc_pack ( gpack, &ipktyp, &nbits, &ier );
    cgd_wpgd ( &ifn, grid, &_dgfile.kxd, &_dgfile.kyd, igdhdr, time1, time2,
        level1, level2, ivcord, parm, rplc, &ipktyp, &nbits, &ier );
    if ( ier != 0 ) {
	er_wmsg ( "GD", &ier, " ", &ierr, strlen("GD"), strlen(" ") );
	*iret = -65;
	return;
    }

    return;
}
