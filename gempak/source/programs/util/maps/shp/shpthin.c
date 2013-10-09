#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

/*
 * private function
 */
static float distance ( float lat1, float lon1, float lat2, float lon2 );

/************************************************************************
 * shpthin.c                                                            *
 *                                                                      *
 * This module contains the functions to remove data points.            *
 *                                                                      *
 * CONTENTS:                                                            *
 *      shp_thin()       removes data points.                           *
 *      distance()       computes distance between two points.		* 
 ***********************************************************************/

/*=====================================================================*/

void shp_thin ( shp_part *oneprt, float threshold, int *iret ) 
/************************************************************************
 * shp_thin								*
 *                                                                      *
 * This function removes data points from map files based on a		*
 * threshold index. The index is a product of angular change and point	*
 * separation in each triplet of points.				*
 *                                                                      *
 * shp_thin ( oneprt, threshold, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*oneprt		shp_part	Shape part  			*
 *	threshold	float		Threshold			*
 *									*
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04		Modified from perl 'mapthin'	*
 * X. Guo/CWS            9/11           Checked intersection before     *
 *                                      reduce point                    *
 ***********************************************************************/
{
    float prvlat, prvlon, curlat, curlon, nxtlat, nxtlon, latest, lonest;
    float       xp[2], yp[2], xp1[2], yp1[2];
    float       xm, ym;
    float       x1, y1, x2, y2, z;
    int *valid, prvpt, curpt, nxtpt, np, delpts,intrsct,ier,i;
/*---------------------------------------------------------------------*/
    *iret = 0;
    delpts = 0;

    if ( ( valid = shp_mnew ( oneprt->numpts * sizeof(int) ) ) 
        == NULL ) {
        fprintf ( stderr, "Memory allocation failed.\n" );
        exit ( -1 );
    };

    for ( curpt = 0; curpt < oneprt->numpts; curpt++ ) {
        valid[curpt] = G_TRUE;
    }

    for ( curpt = 1; curpt < oneprt->numpts - 1; curpt++ ) {
	prvpt = curpt - 1;
	while ( prvpt > 0 && ! valid[prvpt] ) {
	    prvpt--;
	}
	nxtpt = curpt + 1;

	prvlat = oneprt->pty[prvpt];
	prvlon = oneprt->ptx[prvpt];
	curlat = oneprt->pty[curpt];
	curlon = oneprt->ptx[curpt];
	nxtlat = oneprt->pty[nxtpt];
	nxtlon = oneprt->ptx[nxtpt];
        x1 = prvlon-curlon;
        y1 = prvlat-curlat;
        x2 = curlon-nxtlon;
        y2 = curlat-nxtlat;
        z = x1*y2 - x2*y1;

        if ( (z * z ) > (threshold*0.005) ) continue;

	if ( distance ( prvlat, prvlon, curlat, curlon ) < threshold ) {
            xp[0] = prvlon;
            yp[0] = prvlat;
    
            xp[1] = nxtlon;
            yp[1] = nxtlat;
            for ( i = 0; i < oneprt->numpts ; i ++ ) {
                if ( i == oneprt->numpts - 1 ) {
                    xp1[0] = oneprt->ptx[i];
                    yp1[0] = oneprt->pty[i];

                    xp1[1] = oneprt->ptx[0];
                    yp1[1] = oneprt->pty[0];
                }
                else {
                    xp1[0] = oneprt->ptx[i];
                    yp1[0] = oneprt->pty[i];

                    xp1[1] = oneprt->ptx[i+1];
                    yp1[1] = oneprt->pty[i+1];
                }
                cgr_segint ( sys_M, xp, yp, sys_M, xp1, yp1,
                 sys_M, &xm, &ym, &intrsct, &ier );
                if ( intrsct == 1 ) break;
            }
            if ( intrsct == 0 ) {
                valid[curpt] = G_FALSE;
                delpts++;
            }
	}
    }

    if ( delpts > 0 ) {
        for ( np = 0, curpt = 0; curpt < oneprt->numpts; 
	      curpt++, np++ ) {
	    if ( ! valid[curpt] ) {
		/*
		 * Search for the next valid point.
		 */
		while ( curpt < oneprt->numpts && ! valid[curpt] ) {
		    curpt++;
		}
	    }
	    oneprt->ptx[np] = oneprt->ptx[curpt];
	    oneprt->pty[np] = oneprt->pty[curpt];
	}
        oneprt->numpts = np;

	/*
	 * Re-compute the max/min.
	 */
	oneprt->maxlat = -90.0;
	oneprt->minlat = 90.0;
	oneprt->maxlon = -180.0;
	oneprt->minlon = 180.0;
	for ( np = 0; np < oneprt->numpts; np++ ) {
	    oneprt->maxlat = oneprt->maxlat > oneprt->pty[np] ?
	                     oneprt->maxlat : oneprt->pty[np];
	    oneprt->minlat = oneprt->minlat < oneprt->pty[np] ?
	                     oneprt->minlat : oneprt->pty[np];
	    oneprt->maxlon = oneprt->maxlon > oneprt->ptx[np] ?
	                     oneprt->maxlon : oneprt->ptx[np];
	    oneprt->minlon = oneprt->minlon < oneprt->ptx[np] ?
	                     oneprt->minlon : oneprt->ptx[np];
	}
    }

    shp_mfree ( valid );
}

static float distance ( float lat1, float lon1, float lat2, float lon2 )
/************************************************************************
 * distance								*
 *                                                                      *
 * This function computes the distance between two points.		*
 *                                                                      *
 * static float distance ( lat1, lon1, lat2, lon2 )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	lat1		float		Point 1 latitude		*
 *	lon1		float		Point 1 longitude		*
 *	lat2		float		Point 2 latitude		*
 *	lon2		float		Point 2 longitude		*
 *									*
 * Output parameters:                                                   *
 *	distance	static float	Distance			*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04		Modified from perl 'mapthin'	*
 ***********************************************************************/
{
    float latdif, londif;
/*---------------------------------------------------------------------*/
    latdif = lat2 - lat1;
    londif = lon2 - lon1;

    /*
     * Simple square root is used to calculate distance for two reasons:
     * 1. two points are assumed to be close enough.
     * 2. only estimated distance is needed.
     */
    return sqrt ( latdif * latdif + londif * londif );
}
