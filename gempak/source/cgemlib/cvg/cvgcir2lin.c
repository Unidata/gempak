#include "cvgcmn.h"
#include "pgprm.h"
#include "drwids.h"

void cvg_cir2lin ( VG_DBStruct *el_cir, int res, VG_DBStruct *el_lin, 
		   int *iret )
/************************************************************************
 * cvg_cir2lin								*
 *									*
 * This function converts a circle element to a line element.		*
 *									*
 * cvg_cir2lin (el_cir, res, el_lin, iret)	                        *
 *									*
 * Input parameters:							*
 *	*el_cir		VG_DBStruct 	input circle element		*
 *	res		int		angular distance between two	*
 *					points on the circle(resolution)*
 * Output parameters:							*
 *	*el_lin		VG_DBStruct	output line element		*
 *	*iret		int		return value			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA   	02/03	initial coding				*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{  
    int    npts, index, nn, ier;
    double arc;
    float  center_x, center_y, first_x, first_y, radius, ini_dir;
    float  dir, r_x, r_y, dev_x[2], dev_y[2];
/*---------------------------------------------------------------------*/  

    *iret = 0;

    /*
     * Calculate the # of points on the circle based on resolution.
     */
    npts = ( (360 % res) == 0 ) ? (360 / res) : ((360 / res) + 1);

    /*
     * Copy the LineInfo structure from circle to line.
     */
    el_lin->elem.lin.info = el_cir->elem.cir.info;
    el_lin->elem.lin.info.numpts = npts;

    /*
     * Obtain circle center, radius and direction between two points.
     * All in device coordinates.
     */
    nn = 2;
    gtrans (sys_M, sys_D, &nn, 
            &(el_cir->elem.cir.data.latlon[0]),
            &(el_cir->elem.cir.data.latlon[2]),
            dev_x, dev_y, &ier,
	    strlen(sys_M), strlen(sys_D)        );

    center_x = dev_x[0];
    center_y = dev_y[0];
    first_x  = dev_x[1];
    first_y  = dev_y[1];
  
    radius = (float)
	     sqrt ( (first_x - center_x) * (first_x - center_x) +
                    (first_y - center_y) * (first_y - center_y)   );

    arc = atan ( (double)(first_y - center_y) / 
			 (first_x - center_x)   );
    if ( (first_x - center_x) < 0 ) {
         arc += PI;
    }
    ini_dir = (float) arc / (2 * PI) * 360;
    ini_dir = (ini_dir + 360) > 360 ? ini_dir : (ini_dir + 360);

    /*
     * Construct latlon data for line element.
     */

    el_lin->elem.lin.latlon[0]    = el_cir->elem.cir.data.latlon[1];
    el_lin->elem.lin.latlon[npts] = el_cir->elem.cir.data.latlon[3];

    for ( index = 1; index < npts; index++ ) {

	    dir = ini_dir + index * res;
            dir = (dir > 360) ? (dir - 360) : dir;

	    r_x = (float)
		   radius * cos ( dir / 360 * (2*PI) ) + center_x;
	    r_y = (float)
		   radius * sin ( dir / 360 * (2*PI) ) + center_y;

            nn = 1;
            gtrans (sys_D, sys_M, &nn, 
		    &r_x, &r_y,
                    &(el_lin->elem.lin.latlon[index]),
                    &(el_lin->elem.lin.latlon[index+npts]),
                    &ier, strlen(sys_D), strlen(sys_M)      );

    }

    /*
     * Fill header info. for line element.
     */
    el_lin->hdr.delete    = 0;
    el_lin->hdr.vg_type   = LINE_ELM;
    el_lin->hdr.vg_class  = CLASS_LINES;
    el_lin->hdr.filled    = 0;
    el_lin->hdr.closed    = 1;
    el_lin->hdr.smooth    = 0;
    el_lin->hdr.version   = 0;
    el_lin->hdr.grptyp    = el_cir->hdr.grptyp;    
    el_lin->hdr.grpnum    = el_cir->hdr.grpnum;
    el_lin->hdr.maj_col   = el_cir->hdr.maj_col;    
    el_lin->hdr.min_col   = el_cir->hdr.min_col;

    el_lin->hdr.recsz     = (sizeof(float) * 2 * npts) +
			     sizeof(LineInfo) +
                      	     sizeof(VG_HdrStruct); 

    el_lin->hdr.range_min_lat = el_cir->hdr.range_min_lat;   
    el_lin->hdr.range_min_lon = el_cir->hdr.range_min_lon;   
    el_lin->hdr.range_max_lat = el_cir->hdr.range_max_lat;   
    el_lin->hdr.range_max_lon = el_cir->hdr.range_max_lon;   

}

/*=====================================================================*/


