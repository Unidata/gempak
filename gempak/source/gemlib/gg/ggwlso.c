#include "geminc.h"
#include "gemprm.h"
#include "gpc.h"
#include "proto_gpc.h"

void gg_wlso ( int *npts, int nfips[], int *mxv, int *nunion, int *nv,
	       float xp[], float yp[], float *xc, float *yc,
	       float *area, int *iret )
/************************************************************************
 * gg_wlso                                                              *
 *                                                                      *
 * This function obtains the union of counties given in the list of FIPS*
 * codes if *npts (the number of FIPS codes) is greater than 0.  It also*
 * returns the coordinates, centroid and area of the first non-hole	*
 * polygon in the union.  If *npts is equal to 0, then it returns the	*
 * same information for the next non-hole polygon.			*
 *									*
 * gg_wlso ( npts, nfips, mxv, nunion, nv, xp, yp, xc, yc, area, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	*npts		int	No. of FIPS codes in nfips		*
 *	nfips[]		int	FIPS codes of counties to perform union *
 *	*mxv		int	Maximum number of expected vertices	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*nunion		int	Number of non-hole polygons in union	*
 *	*nv		int	Number of vertices in non-hole polygon	*
 *	xp[]		float	x coordinates of the non-hole polygon	* 
 *	yp[]		float	y coordinates of the non-hole polygon	* 
 *	*xc		float   x centroid of non-hole polygon		*
 *	*yc		float	y centroid of non-hole polygon		*
 *	*area		float	area of the non-hole polygon		*
 *				  > 0 for counter_clockwise		*
 *				  < 0 for clockwise			*
 *	*iret		int	Return code				*
 *				 =   2 failure to obtain centroid/area  *
 *				 =   1 No more non-hole polygons	*
 *				 =   0 Normal				*
 *				 = -21 more than *mxv vertices		*
 *				 = -22 failure to obtain union (bounds	*
 *				       file error or bad tag name/value)*
 *				 = -23 Too many counties		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * F. J. Yen/NCEP	 5/05                                           *
 * F. J. Yen/NCEP	 7/05	Changed iret for 0 polygons returned and*
 *				added centroid and area of union (CSC)	*
 ***********************************************************************/

{
    int			im, ier, nc, nholes, ii, ji, iretn;
    int			nfnd; 
    static int		ind;
    char		bounds[40], name[40], cfips[8], cstfips[2000];
    static gpc_polygon	union_poly;
/*---------------------------------------------------------------------*/

    *iret  = 0;
    ier    = 0;

    if ( *npts > 333 ) {
	*iret = -23;
	return;
    }
    if ( *npts > 0 ) {
	ind = -1;
    }
    if ( ind == -1 ) {
        /* 
         * Initalize the gpc polygon structure variables 
         */
        union_poly.num_contours	= 0;
        union_poly.hole		= (int *)NULL;
        union_poly.contour	= (gpc_vertex_list *)NULL;
        /* 
         * Query the bounds dimensions 
         */
        strcpy (bounds, "WBCMZ_BNDS");
        strcpy (name, "<FIPS>");
        cstfips [0] = '\0';
        for (im = 0; im < *npts; im++) {
	    cst_inch ( nfips[im], cfips, &ier );
	    cst_lstr ( cfips, &nc, &ier );
	    strcat ( cstfips, cfips );
	    strcat ( cstfips, ";" );
        }
	clo_init ( &ier );
	/*
 	 *  Invoke clo_blasso to get polygon structure of the union of
 	 *  the polygons degsinated by cstfips.
 	 */
        clo_blasso ( bounds, name, npts, cstfips, &union_poly, &iretn );
        if ( iretn != -1 ) {
	    if ( union_poly.num_contours == 0 ) {
		*iret = 1;
		return;
            }
            else {
                /*
                 * Determine the number of polygons that are holes and
	         * obtain the vertices if non-hole.
                 */
                nholes = 0;
		nfnd = 0;
                for ( ii = 0; ii < union_poly.num_contours; ii++ ) {
                    if ( union_poly.hole[ii] == 1 ) {
		        nholes++;
		    }
		    else {
			/*
			 *   Non-hole, so get vertices
			 */
			if ( union_poly.contour[ii].num_vertices <= *mxv ) {
			    if ( nfnd == 0 ) {
			         gpc_gvlist (&union_poly.contour[ii], nv,
				       xp, yp, &ier );
			         ind = ii;
			         nfnd = 1;
			     }
			}
			else {
			    *iret = -21;
			    ind = ii;
			    return;
			}
		    }
                }
		*nunion = union_poly.num_contours - nholes;
		if ( nfnd == 0 ) {
		    *iret = 1;
		}
    		if ( *iret == 0 ) {
    		    /*
     		     *  Get centroid and area of the non-hole polygon
     		     */
		    cgr_centroid ( xp, yp, nv, xc, yc, area, &ier );
		    if ( ier != 0 ) {
	    		*iret = 2;
		    }
    		}
	    }
	}
	else {
	    *iret = -22;
	}
    }
    else {
	nfnd = 0;
	ind++;
        if ( ind >= 0 && ind < union_poly.num_contours ) {
            for ( ji = ind; ji < union_poly.num_contours && nfnd == 0; ji++ ) {
                if ( union_poly.hole [ji] == 0 ) {
		    nfnd = 1;
		    if ( union_poly.contour[ji].num_vertices <= *mxv ) {
                        gpc_gvlist (&union_poly.contour[ji], nv,
                                    xp, yp, &ier );
                        ind = ji;
		    }
		    else {
		        ind = ji;
		        *iret = -21;
		        return;
		    }
                }
            }
            if ( nfnd == 0 ) *iret = 1;
        }
        else {
            *iret = 1;
        }
        if ( *iret == 0 ) {
            /*
             *  Get centroid and area of the non-hole polygon
             */
	    cgr_centroid ( xp, yp, nv, xc, yc, area, &ier );
	    if ( ier != 0 ) {
	        *iret = 2;
	    }
        }
    }
}
