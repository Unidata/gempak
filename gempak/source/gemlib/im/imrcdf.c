#include "geminc.h"
#include "gemprm.h"

#include "netcdf.h"

void im_rcdf ( char imgfil[], int *kx, int *ky, int *isorc, int *itype, 
		int *idate, int *itime, int *iproj, float *clat, 
		float *clon, float *xmin, float *xmax, float *ymin, 
		float *ymax, int *iret )
/************************************************************************
 * im_rcdf								*
 *									*
 * This subroutine opens a NetCDF file and reads the header information *
 * and returns it to the calling function.                              *
 *                                                                      *
 * im_rcdf ( imgfil, kx, ky, isorc, itype, idate, itime, iproj,         *
 *	  clat, clon, xmin, xmax, ymin, ymax, iret )                    *
 *                                                                      *
 * Input parameters:                                                    *
 *	imgfil[]	char		Name of image file              *
 *                                                                      *
 * Output parameters:                                                   *
 *	*kx		int		x dimension of image		*
 *     	*ky		int		y dimension of image		*
 *	*isorc		int						*
 *	*itype		int		type of channel			*
 * 	*idate		int		date				*
 *	*itime		int		time				*
 *	*iproj		int		projection type			*
 *	*clat		float		central latitude		*
 * 	*clon		float		central longitude		*
 *	*xmin		float		x - minimum for image		*
 * 	*xmax	 	float		x - maximum for image		*
 *	*ymin 		float		y - minimum for image		*
 *	*ymax		float		y - maximum for image		*
 *	*iret 		int		return value			*
 *                                                                      *
 *                                                                      *
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * R. Curtis/EAI  	 8/00   Updated and implemented in GEMPAK       *
 * T. Piper/GSC		 9/00	Modified for Lambert Conformal		*
 * A. Hardy/GSC		 7/01   Modified to use the channel table       *
 ***********************************************************************/
{
	int		ncid, iy, im, id, ih, in, is;
	size_t		lenp, jx, jy;
	double		dsec;
	time_t		isec;
	struct tm	*tarr;
	char		chanl[81];
/*---------------------------------------------------------------------*/
	*iret = 0;
/*
 *	Open the NetCDF file.
 */
	nc_open ( imgfil, NC_NOWRITE, &ncid );
/*
 *	Get the x and y dimensions of the image.
 */
	nc_inq_dimlen ( ncid, 0, &jy );
	nc_inq_dimlen ( ncid, 1, &jx );
	*kx = (int) jx;
	*ky = (int) jy;
/*
 *	Get the date/time. It is stored as the number of seconds
 *	since 00:00:00 UTC, January 1, 1970.
 */
	nc_get_var1_double ( ncid, 1, 0, &dsec );
	isec = (time_t) dsec;
	tarr = gmtime ( &isec );
	iy = (*tarr).tm_year + 1900;
	im = (*tarr).tm_mon + 1;
	id = (*tarr).tm_mday;
	ih = (*tarr).tm_hour;
	in = (*tarr).tm_min;
	is = (*tarr).tm_sec;
	*idate = iy * 10000 + im * 100 + id;
	*itime = ih * 10000 + in * 100 + is;
/*
 *	Get the channel information.
 */
	nc_inq_attlen ( ncid, NC_GLOBAL, "channel", &lenp );
	nc_get_att_text ( ncid, NC_GLOBAL, "channel", chanl );
	chanl[lenp] = CHNULL;
/*
 *       ITYPE  Channel String(s)
 *      -----------------------------
 *          1   VIS
 *          2   3.9 micron (IR2)
 *          4   6.7 micron (WV)
 *          8   11  micron (IR)
 *         16   12  micron (IR5)
 */
	im_chtb ( chanl, itype, iret, strlen(chanl) );
/*
 *	Get the projection and central lat/lon.
 */
	nc_get_att_int ( ncid, NC_GLOBAL, "projIndex", iproj );
	nc_get_att_float ( ncid, NC_GLOBAL, "centralLat", clat );
	nc_get_att_float ( ncid, NC_GLOBAL, "centralLon", clon );
/*
 *	If the center longitude is east of -100, assume that the image
 *	is from GOES-8. Otherwise, assume that it is from GOES-9.
 */
	if  ( *clon > -100.0 )  {
	    *isorc = 70;
	}
	else {
	    *isorc = 72;
	}
/*
 *	Get the corner lat/lons. The values are for the upper left
 *	and lower right corners.
 */
	nc_get_att_float ( ncid, NC_GLOBAL, "xMin",   xmin );
	nc_get_att_float ( ncid, NC_GLOBAL, "xMax",   xmax );
	nc_get_att_float ( ncid, NC_GLOBAL, "yMin",   ymin );
	nc_get_att_float ( ncid, NC_GLOBAL, "yMax",   ymax );
/*
 *	Close the NetCDF file.
 */
 	nc_close ( ncid );
}
