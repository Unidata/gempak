#include "dg.h"

void dg_robs ( const int *iflno, const char *gdtime1, const char *gdtime2,
               const int *level1, const int *level2, const int *ivcord,
	       float *grid1, float *grid2, int *igx, int *igy, int *iret )
/************************************************************************
 * dg_robs								*
 *									*
 * This subroutine retrieves grid relative observed wind components	*
 * from a grid file.  The grid components must be stored as UREL	*
 * and VREL.								*
 *									*
 * dg_robs ( iflno, gdtime1, gdtime2, level1, level2, ivcord, grid1,	*
 *           grid2, igx, igy, iret )					*
 *									*
 * Input parameters:							*
 *	*iflno		const int	Grid file number		*
 *	*gdtime1	const char	Grid time			*
 *	*gdtime2	const char	Grid time			*
 *	*level1		const int	Grid level			*
 *	*ivcord		const int	Grid vertical coordinate	*
 *									*
 * Output parameters:							*
 *	*grid1		float		U-component grid		*
 *	*grid2		float		V-component grid		*
 *	*igx		int		Number of points in x dir	*
 *	*igy		int		Number of points in y dir	*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 *					  -3 = wind unavailable		*
 **									*
 * Log:									*
 * R. Tian/SAIC		 1/04	Modified from GR_ROBS			*
 * R. Tian/SAIC		 5/04	Added call to DG_TRIG			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int jghdr[LLGDHD], igx1, igy1, ier1, ier2;
/*----------------------------------------------------------------------*/
    *iret = -3;

    /*
     * Read in the UREL and VREL components.
     */
    if ( _nfile.nucode == G_TRUE ) {
	dgc_nrdt ( iflno, gdtime1, gdtime2, level1, level2, ivcord,
	    "UREL", grid1, &igx1, &igy1, jghdr, &ier1 );
	if ( ier1 == 0 ) {
	    dgc_nrdt ( iflno, gdtime1, gdtime2, level1, level2,
	        ivcord, "VREL", grid2, igx, igy, jghdr, &ier2 );

	    /*
	     * Check to see if both grids are the same size.
	     */
	    if ( ( ier2 == 0 ) && ( igx1 == *igx ) && ( igy1 == *igy ) ) {
		*iret = 0;
	    }
	}

	/*
	 * Rotate wind components.
	 */
	if ( *iret == 0 ) {
	    dg_trig ( grid1, grid2, iret );
	}
    } else {
#if 0
	dg_rdat ( iflno, gdtime1, gdtime2, level1, level2, ivcord,
	    "UREL", grid1, &igx1, &igy1, jghdr, &ier1 );
	if ( ier1 == 0 ) {
	    dg_rdat ( iflno, gdtime1, gdtime2, level1, level2, ivcord,
	        "VREL", grid2, igx, igy, jghdr, &ier2 );

	    /*
	     * Check to see if both grids are the same size.
	     */
	    if ( ( ier2 == 0 ) && ( igx1 == *igx ) && ( igy1 == *igy ) ) {
		*iret = 0;
	    }
	}
#endif
    }

    return;
}
