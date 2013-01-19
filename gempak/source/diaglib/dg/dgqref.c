#include "dg.h"

void dg_qref ( const int *mxanl, const int *mxnav, float *bkanl,
               float *bknav, int *mxgrd, int *iret )
/************************************************************************
 * dg_qref								*
 *									*
 * This subroutine queries the reference grid navigation and analysis	*
 * block, as well as the maximum number of grids. It can only be called	*
 * after either DG_NTIM or DG_INXT is called.				*
 *									*
 * dg_qref ( mxanl, mxnav, bkanl, bknav, mxgrd, iret )			*
 *									*
 * Input parameters:							*
 *	*mxanl		const int	Max analysis queried		*
 *	*mxnav		const int	Max navigation queried		*
 *									*
 * Output parameters:							*
 *	*bkanl		float		Analysis block data array	*
 *	*bknav		float		Navigation block data array	*
 *	*mxgrd		int		Max number of grids		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-54 = grid file open failed	*
 **									*
 * Log:									*
 * R. Tian/SAIC          3/05						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float rnav[LLNNAV], ranl[LLNANL];
    int igdfln, anlsz, navsz, i, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    anlsz = LLNANL;
    navsz = LLNNAV;

    gd_open ( _nfile.crtfnm[_nfile.irefnv], &_nfile.outflg[_nfile.irefnv],
              &anlsz, &navsz, &igdfln, ranl, rnav, mxgrd, &ier,
	      strlen(_nfile.crtfnm[_nfile.irefnv]) );
    if ( ier != 0 ) {
	*iret = -54;
	return;
    }

    if ( (*mxanl) > 0 ) {
	for ( i = 0; i < G_MIN ( (*mxanl), LLNANL ); i++ ) {
	    bkanl[i] = ranl[i];
	}
    }

    if ( (*mxnav) > 0 ) {
	for ( i = 0; i < G_MIN ( (*mxnav), LLNNAV ); i++ ) {
	    bknav[i] = rnav[i];
	}
    }

    return;
}
