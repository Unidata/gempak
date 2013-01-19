#include "dg.h"

void dg_azst ( const int *ilat, const int *ilon, int *iazst, int *iret )
/************************************************************************
 * dg_azst								*
 *									*
 * This subroutine will calculate the azimuth angle at each grid point	*
 * measured from north to the great circle arc. The great circle arc	*
 * joins the grid point	to the latitude,longitude location specified at	*
 * the corresponding point in the internal grid numbers ILAT and ILON.	*
 *									*
 * The case where grid point equals storm point is flagged so that	*
 * radial/tangential winds are set to zero in calling programs DV_RAD,	*
 * DV_MRAD, DV_TNG, and DV_MTNG.					*
 *									*
 * dg_azst ( ilat, ilon, iazst, iret )					*
 *									*
 * Input parameters:							*
 *	*ilat		const int	Grid number for latitude array	*
 *	*ilon		const int	Grid number for longitude array	*
 *									*
 * Output parameters:							*
 *	*iazst		int		Grid number for azimuth angle	*
 *					array				*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 6/91						*
 * K. Brill/NMC		 4/95	Assure 0 or 180 when arc is a meridian	*
 * K. Tyle/GSC		 9/95	Check for when grid pt = storm pt	*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC		 5/96	Moved IGDPT outside do-loop		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				by using internal grids for lat/lon	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 * K. Brill/HPC		 3/09	Handle cases in which c1 > PI or 	*
 * 				c1 < -PI				*
 ************************************************************************/
{
    float a, b, c1, cosa, cosb, sinb, cosc, sinc, denom, acosin, diff;
    int i, im1;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Get a grid number for the azimuth angles.
     */
    dg_ltln ( iret );
    if ( *iret != 0 ) return;
    dg_nxts ( iazst, iret );
    if ( *iret != 0 ) return;

    for ( i = _dgarea.ksub1; i <= _dgarea.ksub2; i++ ) {
        im1 = i - 1;
	if ( ERMISS ( _dggrid.dgg[(*ilat)-1].grid[im1] ) ||
	     ERMISS ( _dggrid.dgg[(*ilon)-1].grid[im1] ) ||
	     ERMISS ( _dggrid.dgg[_dgfile.idglon-1].grid[im1] ) ||
	     ERMISS ( _dggrid.dgg[_dgfile.idglat-1].grid[im1] ) ) {
	    _dggrid.dgg[(*iazst)-1].grid[im1] = RMISSD;
	} else {
	    /*
	     * Finding the x and y component of the vector from
	     * the storm center to the grid point using an oblique
	     * spherical triangle.
	     */
	    a = ( PI / 2. ) - ( _dggrid.dgg[(*ilat)-1].grid[im1] * DTR );
	    b = ( PI / 2. ) -  _dggrid.dgg[_dgfile.idglat-1].grid[im1];
	    c1 = _dggrid.dgg[_dgfile.idglon-1].grid[im1] -
	       ( _dggrid.dgg[(*ilon)-1].grid[im1] * DTR );
	    if ( c1 > PI ) {
		c1 = c1 - ( 2 * PI );
	    }
	    if ( c1 < -PI ) {
		c1 = c1 + ( 2 * PI );
	    }
	    if ( G_ABS (b) < 1.E-05 ) c1 = 0.0;
	    if ( G_ABS (c1) > 1.E-05 ) {
		cosa = cos ( a );
		cosb = cos ( b );
		sinb = sin ( b );
		cosc = ( cosa * cosb ) + ( sin ( a ) * sinb * cos ( c1 ) );
		sinc = pow ( 1 - cosc  * cosc, .5 );
		denom = sinb * sinc;
		if ( !G_DIFFT(denom, 0.0F, GDIFFD) ) {
		    acosin = ( cosa - ( cosb * cosc ) ) / denom;

		    /*
		     * Check to see if truncation error gives us an
		     * Arccos argument outside valid range.
		     */
		    if ( ( acosin > 1 ) || ( acosin < -1 ) ) {
			_dggrid.dgg[(*iazst)-1].grid[im1] = 0.;
		    } else {
			_dggrid.dgg[(*iazst)-1].grid[im1] = acos ( acosin );
		    }

		    /*
		     * Check to see what side the the grid point is on
		     * relative to the storm.  If the grid point is left
		     * of the storm, the angle will be subtracted from
		     * 2PI, since ARCCOS only ranges from 0 to PI, which
		     * never give us a wind with a westerly component.
		     */
		    if (  c1 > 0  ) {
			_dggrid.dgg[(*iazst)-1].grid[im1] =  ( 2 * PI ) -
			    _dggrid.dgg[(*iazst)-1].grid[im1];
		    }
		} else {
		    _dggrid.dgg[(*iazst)-1].grid[im1] = 0.0;
		}
	    } else {
		diff = a - b;
		if ( diff > 1.E-05 ) {
		    _dggrid.dgg[(*iazst)-1].grid[im1] = PI;
		} else if ( diff < -1.E-05 ) {
		    _dggrid.dgg[(*iazst)-1].grid[im1] = 0.;
		/*
		 * Flag case when grid point = storm point.
		 */
		} else {
		    _dggrid.dgg[(*iazst)-1].grid[im1] = 99.;
		}
	    }
	}
    }

    return;
}
