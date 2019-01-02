#include "geminc.h"
#include "gemprm.h"
#include "netcdf.h"

void im_rcdf4 ( char imgfil[], int area[64], int nav[640], int *isorc, int *itype, 
		int *idate, int *itime, int *imdpth, float *clat, 
		float *clon, int *ixmin, int *ixmax, int *iymin, 
		int *iymax, int *iret )
/************************************************************************
 * im_rcdf4								*
 *									*
 * This subroutine opens a NetCDF file and reads the header information *
 * and returns it to the calling function.                              *
 *                                                                      *
 * im_rcdf4 ( imgfil, area[64], nav[640], isorc, itype, idate, itime,   *
 * iproj, clat, clon, ixmin, ixmax, iymin, iymax, iret )                *
 *                                                                      *
 * Input parameters:                                                    *
 *	imgfil[]	char		Name of image file              *
 *                                                                      *
 * Output parameters:                                                   *
 *      area		int		MCIDAS AREA header              *		
 *     	nav		int		MCIDAS NAV header		*
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
 * S. Guan/NCEP          6/15   Created                                 *
 * M. James/UCAR         1/19   Account for GOES-16 and GOES-17		*
 ***********************************************************************/
{
	int		ncid, xy_id, ncret, i;
        int             iy, im, id, ih, in, is;
        size_t          jx, jy;
	char		chanl[81];
        char            sdate_time[23], stmp[10], units[4];
        double          chanltype, bit_depth;
        float           x_scale, y_scale, x_offset, y_offset, sat_lon;
        double          dsec;
        time_t          isec;
        struct tm       *tarr;
        int             leap;
        int             mthday[12];
        float           aa;
/*---------------------------------------------------------------------*/
	*iret = 0;
        leap = 0;
/*      days of each month   */
        mthday[0] = 31;
        mthday[1] = 28;       
        mthday[2] = 31;
        mthday[3] = 30;
        mthday[4] = 31;
        mthday[5] = 30;
        mthday[6] = 31;
        mthday[7] = 31;
        mthday[8] = 30;
        mthday[9] = 31;
        mthday[10] = 30;
        mthday[11] = 31;
/*
 *	Open the NetCDF file.
 */
	nc_open ( imgfil, NC_NOWRITE, &ncid );
/*
 *      Get the x and y dimensions of the image.
 */
        nc_inq_dimlen ( ncid, 0, &jy );
        nc_inq_dimlen ( ncid, 1, &jx );
/*
 *	Get the date/time. 
 */
        ncret = nc_get_att_text ( ncid, NC_GLOBAL, "start_date_time", sdate_time );
        if ( ncret ==  0) {
            sdate_time[13] = '\0';
            strncpy(stmp, sdate_time, 4);
            stmp[4] = '\0';
            iy = atoi(stmp);
            if ( ( ((iy % 4) == 0) && (iy % 100) != 0 ) || ((iy % 400) == 0)) leap = 1;  
            if ( leap ) mthday[1] = 29; 
            strncpy(stmp, sdate_time, 7);       
            stmp[7] = '\0';
            area[3] = atoi(&stmp[2]);
            id = atoi(&stmp[4]);
            for (i = 0; i < 12; i++) {
               if ( ( id > 0 ) && ( id - mthday[i] ) < 0 ) break; 
               id = id - mthday[i];
            }
            im = i + 1;
       	    *idate = iy * 10000 + im * 100 + id;
	    *itime = atoi(&sdate_time[7]);
        } else {
             ncret = nc_get_att_text ( ncid, NC_GLOBAL, "time_coverage_end", sdate_time );
             strncpy(stmp, sdate_time, 4);
             stmp[4] = '\0';
             iy = atoi(stmp);
             strncpy(stmp, &sdate_time[5], 2);           
             stmp[2] = '\0';
             im = atoi(stmp);
             strncpy(stmp, &sdate_time[8], 2);
             stmp[2] = '\0';
             id = atoi(stmp);
             strncpy(stmp, &sdate_time[11], 2);
             stmp[2] = '\0';
             ih = atoi(stmp);
             strncpy(stmp, &sdate_time[14], 2);
             stmp[2] = '\0';
             in = atoi(stmp);
             strncpy(stmp, &sdate_time[17], 2);
             stmp[2] = '\0';
             is = atoi(stmp);
             *idate = iy * 10000 + im * 100 + id;
             *itime = ih * 10000 + in * 100 + is;
        } 
/*
 *	Get the channel information.
 */
        nc_get_att_text ( ncid, NC_GLOBAL, "satellite_id", chanl );
        ncret = nc_get_att_double ( ncid, NC_GLOBAL, "channel_id", &chanltype );
        if ( ncret == 0 ) {
           *itype = (int) pow(2, (int) chanltype - 1);
        }
        else {
           *itype = 1;
        }
        nc_get_att_double ( ncid, NC_GLOBAL, "bit_depth", &bit_depth );

        *imdpth =  ( int ) bit_depth;  

/*
 *	Get the projection and central lat/lon.
 */
        nc_get_att_double ( ncid, NC_GLOBAL, "product_center_latitude", clat );
        nc_get_att_double ( ncid, NC_GLOBAL, "product_center_longitude", clon );
        ncret = nc_get_att_float ( ncid, NC_GLOBAL, "satellite_longitude", &sat_lon);
        if ( ncret != 0 ) {
            nc_inq_varid (ncid, "goes_imager_projection", &xy_id);
            nc_get_att_float ( ncid, xy_id, "longitude_of_projection_origin", &sat_lon );
        }

/*
 *      Check for GOES-16/17 satellite_lon written by ldm-alchemy:
 *        * 188 is used for GOES-S(17)
 *	  see: https://gitlab.ssec.wisc.edu/mug/mcidasv/commit/74e299db9965350e29b52f30f50c486ed55da3d3#9f5abd1215318eaffaef36ef1e69c81a940ce18b_1227_1255
 *
 *	If not GOES-16/17, and the center longitude is east of -100, assume that the image
 *	is from GOES-8. Otherwise, assume that it is from GOES-9.
 *
 */
	if ( sat_lon == -137.0 ) {
	    *isorc = 188;
        } else if ( sat_lon == -75.0 ) {
            *isorc = 186;
	} else if ( sat_lon  > 100.0 )  {
	    *isorc = 86;
	} else {
	    *isorc = 70;
	}
/*
 * *      prepare area parameters
 * */
        area[0] = 0;
        area[1] = 4;
        area[2] = 200;
        area[4] = *itime;
        ncret = nc_get_att_int ( ncid, NC_GLOBAL, "tile_row_offset", &area[5] );
        if ( ncret != 0) area[5] = 0;
        ncret = nc_get_att_int ( ncid, NC_GLOBAL, "tile_column_offset", &area[6] );
        if ( ncret != 0) area[6] = 0;
        area[8] = (int) jy;
        area[9] = (int) jx;
        area[11] = 1;
        area[12] = 1;
        nav[0] = '4EOG';
        nav[1] =  area[3];
        nav[2] =  area[4];
        nav[4] =  area[3];
        nav[5] =  area[4];


/*
 *	Get the corner lat/lons. The values are for the upper left
 *	and lower right corners.
 */
        *iymax = area[8];
        *ixmax = area[9];
        *ixmin = 0;
        *iymin = 0;

/*      Get scale and offset   */
        nc_inq_varid (ncid, "y", &xy_id);
        nc_get_att_float ( ncid, xy_id, "scale_factor", &y_scale );
        nc_get_att_float ( ncid, xy_id, "add_offset",   &y_offset );
        nc_inq_varid (ncid, "x", &xy_id);
        nc_get_att_float ( ncid, xy_id, "scale_factor", &x_scale );
        nc_get_att_float ( ncid, xy_id, "add_offset",   &x_offset );
        ncret = nc_get_att_text ( ncid, xy_id,  "units", units);
        units[3] ='\0';
        if ( strcmp("rad", units) == 0 ) {
           y_scale = y_scale * 1000000;
           y_offset = y_offset * 1000000;
           x_scale = x_scale * 1000000;
           x_offset = x_offset * 1000000;  
        }
        nc_get_att_float ( ncid, NC_GLOBAL, "satellite_longitude", &sat_lon);
        
        nav[39] = (int) y_scale;
        nav[40] = (int) y_offset;
        nav[41] = (int) x_scale; 
        nav[42] = (int) x_offset;
        nav[43] = (int) (1000*sat_lon);
/*
 *	Close the NetCDF file.
 */
 	nc_close ( ncid );
}
