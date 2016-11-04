#include "dg.h"

void dgc_inxt ( const int *chngnv, const int *coladd, const char *time1,
		const char *time2, int *iret )
/************************************************************************
 * dgc_inxt                                                             *
 *                                                                      *
 * This subroutine initialize processing for the given time.            *
 *                                                                      *
 * dgc_inxt ( chngnv, coladd, time1, time2, iret )			            *
 *                                                                      *
 * Input parameters:                                                    *
 *      *chngnv		const int	Flag to change navigation               *
 *	*coladd		const int	Flag to add a column of data	            *
 *	*time1		const char	The given time(1)		                    *
 *	*time2		const char	The given time(2)		                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                                 *
 *                                        0 = normal return             *
 *					-50 = navigation not same	                        *
 *					-57 = grid file open failed	                        *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		12/04						                        *
 * R. Tian/SAIC		 2/06	Recoded from Fortran			            *
 * S. Gilbert/NCEP	 9/15	Added check for AWIPSDB			            *
 ************************************************************************/
{
	char **parts=NULL;
	char filnam[MXFLSZ+1];
	float adum, rnvblk[LLNNAV];
	int navsz, ifn, mxgd, same, zero, ier, ii, nparts;
	/*----------------------------------------------------------------------*/
	*iret = 0;
	zero = 0;
	navsz = LLNNAV;

	for ( ii = 0; ii < NGDFLS; ii++ ) {
		if ( _nfile.outflg[ii] == G_TRUE && *coladd == G_TRUE ) {
			*iret = -59;
			return;
		}
	}

	/*
	 * Get navigation from the navagation reference entry
	 */
	if ( _nfile.ntmplt[_nfile.irefnv][0] != '\0' ) {
		cfl_mnam ( (char *)time1, _nfile.ntmplt[_nfile.irefnv], filnam, &ier );
		strcpy ( _nfile.crtfnm[_nfile.irefnv], _nfile.gflpth[_nfile.irefnv] );
		strcat ( _nfile.crtfnm[_nfile.irefnv], "/" );
		strcat ( _nfile.crtfnm[_nfile.irefnv], filnam );
	}
	else if ( strncmp( _nfile.crtfnm[_nfile.irefnv], "AWIPSDB", 7) == 0 ) {
		/*  Read from AWIPS Data Stores  */
		parts = (char **)cmm_malloc2d ( 3, MXFLSZ, sizeof(char), &ier );
		cst_clst ( _nfile.crtfnm[_nfile.irefnv], '/', " ", 3, MXFLSZ, parts, &nparts, &ier );
		strcpy ( _nfile.crtfnm[_nfile.irefnv], parts[0] );
		strcat ( _nfile.crtfnm[_nfile.irefnv], "/" );
		strcat ( _nfile.crtfnm[_nfile.irefnv], parts[1] );
		strcat ( _nfile.crtfnm[_nfile.irefnv], "/" );
		strcat ( _nfile.crtfnm[_nfile.irefnv], time1 );
		cmm_free2d( (void **)parts, &ier );
	}
	gd_open ( _nfile.crtfnm[_nfile.irefnv], &_nfile.outflg[_nfile.irefnv],
			&zero, &navsz, &ifn, &adum, rnvblk, &mxgd, &ier,
			strlen(_nfile.crtfnm[_nfile.irefnv]) );
	if  ( ier != 0 ) {
		*iret = -57;
		return;
	}

	/*
	 * Set navagation.
	 */
	grc_cnav ( rnvblk, _dgsubg.refnav, &navsz, &same, &ier );
	if ( G_DIFFT(_dgsubg.refnav[1], 0.0F, 1e-22) ||
			( same == G_FALSE && *chngnv == G_TRUE ) ) {
		for ( ii = 0; ii < LLNNAV; ii++ ) {
			_dgsubg.refnav[ii] = rnvblk[ii];
		}
		grc_snav ( &navsz, _dgsubg.refnav, &ier );
		if ( *coladd == G_TRUE ) dg_adcl ( &ier );

		/*
		 * Free all existing grids since navigation is changed.
		 */
		dg_fall ( &ier );

		for ( ii = 0; ii < LLNNAV; ii++ ) {
			_dgfile.snav[ii] = _dgsubg.refnav[ii];
		}

		/*
		 * Set the DGCMN navigation related elements.
		 */
		dg_snav ( _dgsubg.refnav, &ier );

		/*
		 * Set the DGCMN DGAREA block, dgset, and igdlst.
		 */
		_dgarea.kgxmin = 1;
		_dgarea.kgxmax = _dgfile.kxd;
		_dgarea.kgymin = 1;
		_dgarea.kgymax = _dgfile.kyd;
		_dgarea.kextnd = 0;
		_dgarea.jgxmin = 1;
		_dgarea.jgxmax = _dgfile.kxd;
		_dgarea.jgymin = 1;
		_dgarea.jgymax = _dgfile.kyd;
		_dgarea.ksub1 = 1;
		_dgarea.ksub2 = _dgfile.kxyd;
		_dgfile.dgset = G_TRUE;
		_dggrid.idglst = 0;
		_dggrid.maxdgg = NDGRD;
	} else if ( same == G_FALSE ) {
		*iret = -50;
		return;
	}

	_dgfile.idlun = 1;

	return;
}
