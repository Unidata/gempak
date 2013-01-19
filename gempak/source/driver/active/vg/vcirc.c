#include "vgcmn.h"

void vcirc ( float *xcent, float *ycent, float *xcumpt, 
				float *ycumpt, int *np, int *iret )
/************************************************************************
 * vcirc								*
 *									*
 * This subroutine draws a circle to the device by constructing a VG    *
 * record for the circle and writing the record to the file at proper   *
 * location.	        						*
 *									*
 * vcirc ( xcent, ycent, xcumpt, ycumpt, np, iret )			*
 *									*
 * Input parameters:							*
 *      *xcent           float		x center latitude coordinate    *
 *      *ycent           float		y center longitude coordinate   *
 *      *xcumpt          float		x circumference latitude coord. *
 *      *ycumpt          float		y circumference longitude coord.*
 *	*np		 int		Number of points on the circle  *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					0   = normal           	        *
 *					-28 = invalid number of points	*
 **									*
 * Log:									*
 * A. Hardy/GSC         11/98   Created                         	*
 * J. Wu/GSC		12/00	Rewrote for new cvg_writelm()		*
 * J. Wu/GSC		02/01	Replaced cvg_writelm() with cvg_write() *
 * 				to avoid overhead file open/close 	*
 * S. Jacobs/NCEP	12/03	Changed to write constant num of pts (2)*
 * S. Danz/AWC		07/06	Update to new cvg_writeD() function     *
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		ii, recsize, start;
    int		npx, iquad, lquad[4], filled, ier;
    float       rlat, rlon, dlon, dist, dir;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    start = -1;
    filled = 0;

    if ( *np < 0 || *np > MAXPTS ) {
	*iret = -28;
	return;
    }

/*
 *  Initialize quadrant detection.
 */
    for ( iquad = 0; iquad < 4; iquad++ ) {
	lquad[iquad] = 0;
    }

/*
 *  Initialize a circle element.
 */
    el.hdr.vg_type = CIRCLE_ELM;
    el.hdr.vg_class = CLASS_CIRCLE;
    
    cvg_initelm( &el );
    
/* 
 *  Load the colors and points into the VG record(s).
 */
    el.hdr.maj_col = kcolr;
    el.hdr.min_col = kcolr;

/*
 *  Finding the record size and the range distance.
 */
    recsize = ( (sizeof(float) * 2 * 2) + sizeof(VG_HdrStruct) + 
		 sizeof(LineInfo) );
    npx = 1;
    clo_dist ( xcent, ycent, &npx, xcumpt, ycumpt, &dist, &ier );

/*
 *  Finding the max and min latitudes and longitudes.
 */
    el.hdr.range_min_lon = 180.0F;
    el.hdr.range_min_lat = 90.0F;
    el.hdr.range_max_lon = -180.0F;
    el.hdr.range_max_lat = -90.0F;
    for ( ii = 0; ii < 360; ii += 10 ){
	dir = (float)ii;
	clo_dltln ( xcent, ycent, &dist, &dir, &rlat, &rlon, &ier );
	dlon = rlon;
	if ( rlon - *ycent > 180.0F ) {
	    dlon = rlon - 360.0F;
	} else if ( *ycent - rlon > 180.0F ) {
	    dlon = rlon + 360.0F;
	}
	if ( dlon < el.hdr.range_min_lon ) el.hdr.range_min_lon = dlon;
	if ( rlat < el.hdr.range_min_lat ) el.hdr.range_min_lat = rlat;
	if ( dlon > el.hdr.range_max_lon ) el.hdr.range_max_lon = dlon;
	if ( rlat > el.hdr.range_max_lat ) el.hdr.range_max_lat = rlat;

	if ( rlon >=    0.0F && rlon <  90.0F ) lquad[0] = 1;
	if ( rlon >=   90.0F && rlon < 180.0F ) lquad[1] = 1;
	if ( rlon >= -180.0F && rlon < -90.0F ) lquad[2] = 1;
	if ( rlon >=  -90.0F && rlon <   0.0F ) lquad[3] = 1;
    }

    if ( lquad[0] == 1 && lquad[1] == 1 && lquad[2] == 1 && lquad[3] == 1 ) {
	el.hdr.range_min_lon = -180.0F;
	el.hdr.range_max_lon =  180.0F;
	if ( *xcent >= 0.0F ) {
	    el.hdr.range_min_lat =   0.0F;
	    el.hdr.range_max_lat =  90.0F;
	} 
	else {
	    el.hdr.range_min_lat = -90.0F;
	    el.hdr.range_max_lat =   0.0F;
	}
    } 

/* 
 *  Save the number of points and other settings information. 
 */
    el.elem.cir.info.numpts = 2;
    el.elem.cir.info.lintyp = kltyp;
    el.elem.cir.info.lthw = klthw;
    el.elem.cir.info.width = klwid;
    el.elem.cir.info.lwhw = klwhw;
    el.elem.cir.data.latlon[0] = *xcent;
    el.elem.cir.data.latlon[2] = *ycent;
    el.elem.cir.data.latlon[1] = *xcumpt;
    el.elem.cir.data.latlon[3] = *ycumpt;
    el.hdr.filled = (char)filled;
    el.hdr.recsz = recsize;    
    el.hdr.grptyp = kgtyp;
    el.hdr.grpnum = kgnum;

/* 
 *  Write element (append to the end of file if start = -1) 
 */
    cvg_writeD( &el, start, el.hdr.recsz, flun, iret );
}
