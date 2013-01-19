#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

void poly_cvgfw ( char *fname, int *np, float *lat, float *lon, int *lintyp, 
		  int *filtyp, int *majcol, int *iret );

/*
 *  Public functions
 */
void poly_cvgfw ( char *fname, int *np, float *lat, float *lon, 
		  int *lintyp, int *filtyp, int *majcol, int *iret )
/************************************************************************
 * cgr_cvgfw								*
 *                                                                      *
 * This is a wrapper function to create VGF files for polygons.		*
 *                                                                      *
 * poly_cvgfw ( fname, np, lat, lon, lintyp, filtyp, majcol, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *  *fname    	char	VG file name					*
 *  *np    	int     Number of points on a polygon			*
 *  *lat[np]	float	Latitude					*
 *  *lon[np]	float	Longitude					*
 *  *lintyp	int	Line type					*
 *  *filtyp	int	Fill type					*
 *  *majcol	int	Major color					*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret    	int    	return code					*
 *			   +5 = no raw polygons				*
 *			   +6 = no warning polygons			*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/SAIC		05/08	initial coding; based on cvgcrall.c	*
 ***********************************************************************/
{   
    int		ii, start, loc, ier, close, smth, delete;
    int		grptyp, grpnum;
    VG_DBStruct	el;    
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    start = -1;      /* Indicating write to the end of file */


    /* 
     * Fill the header structure.
     */
    
    el.hdr.vg_class = CLASS_LINES;
    el.hdr.vg_type = (char)LINE_ELM;
    el.elem.lin.info.lintyp  = *lintyp;
    el.elem.lin.info.lthw = 0;
    el.elem.lin.info.width = 2;
    el.elem.lin.info.lwhw = 0;
    close = 1;
    el.hdr.closed = (char)close;
    smth = delete = 0;
    el.hdr.filled = (char)(*filtyp);
    el.hdr.delete = (char)delete;
    el.hdr.smooth = (char)smth;
    
    el.elem.lin.info.numpts = *np;
    el.hdr.recsz = (int)( (sizeof(float) * (size_t)(2 * (*np))) + 
		   sizeof(VG_HdrStruct) + sizeof(LineInfo) );


    /* 
     * Return if no warning polygons.
     */
    if ( *np <= 0 || *np > MAXPTS ) {
	cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
        *iret = +6;
	return;
    }

    for ( ii = 0; ii < *np; ii++ ) {
	el.elem.lin.latlon[ii]    = lat[ii];
	el.elem.lin.latlon[ii+(*np)] = lon[ii];
    }
    
    /* 
     * Fill the line element.
     */
    
    cvg_initelm ( &el );
    el.hdr.maj_col = *majcol;
    el.hdr.min_col = 3;
    grptyp = grpnum = 0;
    el.hdr.grptyp = (char)grptyp;
    el.hdr.grpnum = (char)grpnum;

    /*
     *  Find the maximum and minimum range.
     */
    el.hdr.range_min_lon = lon[0];
    el.hdr.range_min_lat = lat[0];
    el.hdr.range_max_lon = lon[0];
    el.hdr.range_max_lat = lat[0];

    for ( ii = 0; ii < *np; ii++ ) {
        if ( el.hdr.range_min_lon > lon[ii] )
             el.hdr.range_min_lon = lon[ii];
        if ( el.hdr.range_min_lat > lat[ii] )
             el.hdr.range_min_lat = lat[ii];
        if ( el.hdr.range_max_lon < lon[ii] )
             el.hdr.range_max_lon = lon[ii];
        if ( el.hdr.range_max_lat < lat[ii] )
             el.hdr.range_max_lat = lat[ii];
    }   

    cvg_writefD( &el, start, el.hdr.recsz, fname, &loc, &ier ); 	
    if ( ier < 0 ) *iret = +5;
}
