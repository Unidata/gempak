#include "dg.h"

void dg_onav ( const float *rnvblk, int *iret )
/************************************************************************
 * dg_onav                                                              *
 *                                                                      *
 * This subroutine sets the output grid navigation block		*
 *                                                                      *
 * dg_onav ( rnvblk, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      *rnvblk		const float	New navigation block            *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP	07/05		Modified from DG_INXT		*
 * R. Tian/SAIC		 2/06		Recoded from Fortran		*
 ************************************************************************/
{
    int same;
    int navsz, i, ier;
/*----------------------------------------------------------------------*/
    *iret   = 0;

    /*
     * Set navagation.
     */
    navsz = LLNNAV;
    grc_cnav ( (float *)rnvblk, _dgsubg.refnav, &navsz, &same, &ier );
    if ( same == G_FALSE ) {
	for ( i = 0; i < LLNNAV; i++ ) {
	    _dgsubg.refnav[i] = rnvblk[i];
	}
	grc_snav ( &navsz, _dgsubg.refnav, &ier );

        /*
	 * Free all existing grids since navigation is changed.
	 */
	dg_fall ( &ier );

	for ( i = 0; i < LLNNAV; i++ ) {
	    _dgfile.snav[i] = _dgsubg.refnav[i];
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
	_dgarea.jgxmin = 1;;
	_dgarea.jgxmax = _dgfile.kxd;
	_dgarea.jgymin = 1;
	_dgarea.jgymax = _dgfile.kyd;
	_dgarea.ksub1 = 1;
	_dgarea.ksub2 = _dgfile.kxyd;
	_dgfile.dgset = G_TRUE;
	_dggrid.idglst = 0;
	_dggrid.maxdgg = NDGRD;
     }

    _dgfile.idlun = 1;

    return;
}
