#include "dg.h"

void dg_snav ( const float *rnav, int *iret )
/************************************************************************
 * dg_snav								*
 *									*
 * This subroutine sets the DGCMN grid navigation related elements for	*
 * the navigation in the input navigation block.			*
 * 									*
 * dg_snav ( rnav, iret )						*
 *									*
 * Input parameters:							*
 *	*rnav		const float	Navigation block		*
 *									*
 * Output parameters:							*
 * 	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/HPC		02/04						*
 * R. Tian/SAIC		05/04		Added call to DG_CONE		*
 * R. Tian/SAIC		 2/06		Recoded from Fortran		*
 ************************************************************************/
{
    float ag1, ag2, ag3;
    int nc, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    cst_itos ( (int *)(&rnav[1]), 1, &nc, _dgfile.cprj, &ier );
    cst_rmbl ( _dgfile.cprj, _dgfile.cprj, &nc, &ier );
    _dgfile.kxd = G_NINT ( rnav[4] );
    _dgfile.kyd = G_NINT ( rnav[5] );
    _dgfile.kxyd = _dgfile.kxd * _dgfile.kyd;
    ag1 = rnav[10];
    ag2 = rnav[11];
    ag3 = rnav[12];

    /*
     * Set the constant of the cone for various projections (code
     * duplicated from UPDCON.FOR in GEMPLT).
     */
    _dgfile.anglr1 = ag1 * DTR;
    _dgfile.anglr2 = ag2 * DTR;
    _dgfile.anglr3 = ag3 * DTR;
    dg_cone ( _dgfile.cprj, &_dgfile.anglr1, &_dgfile.anglr3, 
              &_dgfile.concon, iret );

    /*
     * Set lat/lon,  map scale factor, and rotation matrix
     * internal grid pointers to zero.
     */
    _dgfile.idglat = 0;
    _dgfile.idglon = 0;
    _mapscl.ixmscl = 0;
    _mapscl.iymscl = 0;
    _mapscl.ixmsdy = 0;
    _mapscl.iymsdx = 0;
    _dgrtwd.irtcos = 0;
    _dgrtwd.irtsin = 0;
    _dglndc.lndsea = 0;

    /*
     * Initialize orientation angle.
     */
    _dgovec.ornang = RMISSD;

    /*
     * Initialize the origin for M calculation.
     */
    _dgorig.orglat = RMISSD;
    _dgorig.orglon = RMISSD;
    _dgorig.orgxpt = RMISSD;
    _dgorig.orgypt = RMISSD;

    return;
}
