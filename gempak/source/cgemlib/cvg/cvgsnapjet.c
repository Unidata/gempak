#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"

#ifndef MAXOUT
#define MAXOUT          50000
#endif

void cvg_snapjet ( VG_DBStruct *jet_in, VG_DBStruct *jet_out, int *iret )
/************************************************************************
 * cvg_snapjet                                                          *
 *                                                                      *
 * This function moves the location of each wind barb, text, and hash	*
 * mark to the closest point on smoothed/unsmoothed jet line, and	*
 * rotates the wind barb, text, and hash mark at that point.		*
 * The input and output can be the memory address.			*
 *									*
 * Since this function calls gtrans(), it requires GPLT coordinate	*
 * transform functions to be available.					*
 *                                                                      *
 * cvg_snapjet ( jet_in, jet_out, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*jet_in			VG_DBStruct	Input jet element	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*jet_out		VG_DBStruct	Output jet element	*
 *	*iret			int		Return code		*
 *                                 		  0 = Normal            *
 *                                		 -1 = Error             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		11/03	Initial coding                          *
 * R. Tian/SAIC		11/03	Changed call cgr_dang to clo_direct to	*
 *				avoid DEVICE dependency.		*
 * D.W.Plummer/NCEP	02/04	Chg rotate comp from sys_M to sys_D	*
 *                       	Also, chg cgr_range calls to gtrans	*
 * D.W.Plummer/NCEP	02/04	Adjust text based on barb not jet line	*
 * J. Wu/SAIC           02/04   adjust text offset                      *
 * D.W.Plummer/NCEP	02/04	Use sys_N coords when snapping		*
 * J. Wu/SAIC           02/04   set text offset on # of lines in FL text*
 * J. Wu/SAIC           03/04   remove the adjustment on text offset	*
 ***********************************************************************/
{
    VG_DBStruct tmpjet;

    float line_lat[MAXPTS], line_lon[MAXPTS];
    float barb_lat[MAX_JETPTS], barb_lon[MAX_JETPTS];
    float hash_lat[MAX_JETPTS], hash_lon[MAX_JETPTS];
    float line_x[MAXPTS], line_y[MAXPTS];
    float barb_x[MAX_JETPTS], barb_y[MAX_JETPTS];
    float hash_x[MAX_JETPTS], hash_y[MAX_JETPTS];
    float maxlat, minlat;
    int numlpt, numbpt, numtpt, numhpt;

    int nearest, next;
    float distance, nx, ny;

    int istrt, iend, maxpts, nout;
    float dens, crvscl, xout[MAXOUT], yout[MAXOUT];
    float xout_n[MAXOUT], yout_n[MAXOUT];

    float angle[MAX_JETPTS], angle1, angle2;

    int i, point1, point2, ier;
/*---------------------------------------------------------------------*/

    *iret = 0;
    maxlat = -FLT_MAX;
    minlat = FLT_MAX;

    /*
     * Check for correct JET element.
     */
    if ( jet_in->hdr.vg_type != JET_ELM ) {
        *iret = -1;
	return;
    }

    /*
     * In order to work properly no matter if the input and output are the
     * same address in memory, copy the input to a local variable.
     */
    memcpy ( &tmpjet, jet_in, sizeof ( VG_DBStruct ) );

    /*
     * Copy the jet line points, and transform to DEVICE coordinate.
     * If the direction flag is negative, swap the points.
     */
    numlpt = tmpjet.elem.jet.line.spl.info.numpts;
    for ( i = 0; i < numlpt; i++ ) {
        if ( tmpjet.elem.jet.line.spl.info.spldir < 0 ) {
            line_lat[i] = tmpjet.elem.jet.line.spl.latlon[numlpt-1-i];
	    line_lon[i] = tmpjet.elem.jet.line.spl.latlon[2*numlpt-1-i];
	}
	else {
            line_lat[i] = tmpjet.elem.jet.line.spl.latlon[i];
	    line_lon[i] = tmpjet.elem.jet.line.spl.latlon[numlpt+i];
	}
	maxlat = G_MAX ( maxlat, line_lat[i] );
	minlat = G_MIN ( minlat, line_lat[i] );
    }
    gtrans ( sys_M, sys_D, &numlpt, line_lat, line_lon,
	line_x, line_y, &ier, strlen(sys_M), strlen(sys_D) );

    /*
     * Copy the wind barb points, and transform to DEVICE coordiante.
     */
    numbpt = tmpjet.elem.jet.nbarb;
    if ( numbpt > 0 ) {
        for ( i = 0; i < numbpt; i++ ) {
            barb_lat[i] = tmpjet.elem.jet.barb[i].wnd.data.latlon[0];
	    barb_lon[i] = tmpjet.elem.jet.barb[i].wnd.data.latlon[1];
        }
	gtrans ( sys_M, sys_D, &numbpt, barb_lat, barb_lon,
		barb_x, barb_y, &ier, strlen(sys_M), strlen(sys_D) );
    }

    /*
     * Copy the hash points, and transform to DEVICE coordinate.
     */
    numhpt = tmpjet.elem.jet.nhash;
    if ( numhpt > 0 ) {
        for ( i = 0; i < numhpt; i++ ) {
            hash_lat[i] = tmpjet.elem.jet.hash[i].wnd.data.latlon[0];
            hash_lon[i] = tmpjet.elem.jet.hash[i].wnd.data.latlon[1];
        }
	gtrans ( sys_M, sys_D, &numhpt, hash_lat, hash_lon,
		hash_x, hash_y, &ier, strlen(sys_M), strlen(sys_D) );
    }

    /*
     * Apply the smoothing factor to the line.
     */
    switch ( (int)(tmpjet.hdr.smooth) ) {
        case 0:
	    for ( i = 0; i < numlpt; i++ ) {
	        xout[i] = line_x[i];
		yout[i] = line_y[i];
	    }
	    nout = numlpt;
        break;

	case 1:
        case 2:
	    istrt = 0;
	    iend = MAXOUT;
	    maxpts = MAXOUT;
	    if ( (int)(tmpjet.hdr.smooth) == 1 ) {
	        dens = 1.0;
	    }
	    else if ( (int)(tmpjet.hdr.smooth) == 2 ) {
	        dens = 5.0;
	    }
	    gqcvsc( &crvscl, &ier );  /* get device curve scaling factor */
	    if ( crvscl <= 0. )  crvscl = 25.0;

	    /*
	     * Perform parametric curve fit.
	     */
            cv_prmt ( &numlpt, line_x, line_y, &dens, &maxpts, &crvscl, 
	              &istrt, &iend, &nout, xout, yout, &ier );    

	    gtrans ( sys_D, sys_N, &nout, xout, yout, xout_n, yout_n,
		    &ier, strlen(sys_D), strlen(sys_N) );

        break;

	default:
	    ier = -1; 
	break;
    }

    if ( ier != 0 ) {
	*iret = -1;
        return;
    }

    /*
     * Snap the wind barb points.
     */
    if ( numbpt > 0 ) {

        for ( i = 0; i < numbpt; i++ ) {

            cgr_segdist ( &nout, xout, yout, &barb_x[i], &barb_y[i],
	                  &distance, &nearest, &next, &nx, &ny, &ier );

	    point1 = G_MAX(nearest-1,0); point2 = point1 + 1;
	    angle1 = RTD*(float)atan2((double)(yout_n[point1]-yout_n[point2]),
	    			      (double)(xout_n[point1]-xout_n[point2]));

	    point2 = G_MIN(nearest+1,(nout-1)); point1 = point2 - 1;
	    angle2 = RTD*(float)atan2((double)(yout_n[point1]-yout_n[point2]),
	    			      (double)(xout_n[point1]-xout_n[point2]));

            barb_x[i] = nx; barb_y[i] = ny;
	    angle[i] = 0.5 * ( angle1 + angle2 ) - 
	               ( G_ABS (angle1 - angle2) > 180.0 ? 180.0 : 0.0 );
            angle[i] += angle[i] < 0.0 ? 360.0 : 0.0;
            angle[i] -= angle[i] > 360.0 ? 360.0 : 0.0;

            angle[i] = -angle[i] + 90.0F;

	    gp_draz ( &(angle[i]), &(barb_lat[i]), &(barb_lon[i]), 
		      &(angle[i]), &ier );

        }

	gtrans ( sys_D, sys_M, &numbpt, barb_x, barb_y, barb_lat, barb_lon, 
		&ier, strlen(sys_D), strlen(sys_M) );

        for ( i = 0; i < numbpt; i++ ) {
            tmpjet.elem.jet.barb[i].wnd.data.latlon[0] = barb_lat[i];
	    tmpjet.elem.jet.barb[i].wnd.data.latlon[1] = barb_lon[i];
            tmpjet.elem.jet.barb[i].wnd.data.spddir[1] = angle[i]; 
        }

    }

    /*
     * Snap the text points.
     */
    numtpt = numbpt;
    if ( numtpt > 0 ) {
        for ( i = 0; i < numtpt; i++ ) {
            tmpjet.elem.jet.barb[i].spt.info.lat = barb_lat[i];
	    tmpjet.elem.jet.barb[i].spt.info.lon = barb_lon[i];
	    tmpjet.elem.jet.barb[i].spt.info.rotn = 270.0 - angle[i];
	    if ( tmpjet.elem.jet.barb[i].spt.info.rotn < 0.0 ) {
	        tmpjet.elem.jet.barb[i].spt.info.rotn += 360.0;
	    }
	    /*
	     * Relative to North.
	     */
	    tmpjet.elem.jet.barb[i].spt.info.rotn += 1000.0F; 

        }
    }

    /*
     * Snap the hash points.
     */
    if ( numhpt > 0 ) {
        for ( i = 0; i < numhpt; i++ ) {

            cgr_segdist ( &nout, xout, yout, &hash_x[i], &hash_y[i],
	                  &distance, &nearest, &next, &nx, &ny, &ier );

	    point1 = G_MAX(nearest-1,0); point2 = point1 + 1;
	    angle1 = RTD*(float)atan2((double)(yout_n[point1]-yout_n[point2]),
	    			      (double)(xout_n[point1]-xout_n[point2]));

	    point2 = G_MIN(nearest+1,(nout-1)); point1 = point2 - 1;
	    angle2 = RTD*(float)atan2((double)(yout_n[point1]-yout_n[point2]),
	    			      (double)(xout_n[point1]-xout_n[point2]));

            hash_x[i] = nx; hash_y[i] = ny;
	    angle[i] = 0.5 * ( angle1 + angle2 ) - 
	               ( G_ABS (angle1 - angle2) > 180.0 ? 180.0 : 0.0 );
            angle[i] += angle[i] < 0.0 ? 360.0 : 0.0;
            angle[i] -= angle[i] > 360.0 ? 360.0 : 0.0;

            angle[i] = -angle[i] + 90.0F;

	    gp_draz ( &(angle[i]), &(hash_lat[i]), &(hash_lon[i]), 
		      &(angle[i]), &ier );

        }

	gtrans ( sys_D, sys_M, &numhpt, hash_x, hash_y, hash_lat, hash_lon, 
		&ier, strlen(sys_D), strlen(sys_M) );

        for ( i = 0; i < numhpt; i++ ) {
            tmpjet.elem.jet.hash[i].wnd.data.latlon[0] = hash_lat[i];
	    tmpjet.elem.jet.hash[i].wnd.data.latlon[1] = hash_lon[i];
            tmpjet.elem.jet.hash[i].wnd.data.spddir[1] = angle[i]; 
        }
    }

    memcpy ( jet_out, &tmpjet, sizeof ( VG_DBStruct ) );
}
