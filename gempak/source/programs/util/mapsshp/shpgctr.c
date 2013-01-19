#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"
	
void shp_gctr ( shp_record *onerec, float *xctr, float *yctr, int *iret )
/************************************************************************
 * shp_gctr                                                             *
 *                                                                      *
 * This function computes the centroid of a shape record. If the shape	*
 * record has more than one data part ( polygon ), the polygon with the *
 * biggest area is returned.						*
 *                                                                      *
 * shp_gctr ( onerec, xctr, yctr, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec		shp_record	One shape record		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*xctr     	float 		x coordinate of the centroid    *    
 *	*yctr     	float 		y coordinate of the centroid    *    
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 *                                       -1 = Failed                    *
 **                                                                     *
 * R. Tian/SAIC		 6/04						*
 ***********************************************************************/
{
    shp_part *curprt;
    float *ptx, *pty, xc, yc, area, tmparea;
    int numpts, iprt, ipts, one, inout, ier;
/*---------------------------------------------------------------------*/
    *iret = 1;
    one = 1;
    tmparea = 0.;

    for ( curprt = onerec->shpart, iprt = 0; iprt < onerec->numprt;
          iprt++, curprt = curprt->nxtprt ) {

        numpts = curprt->numpts;
        ptx = shp_mnew ( numpts * sizeof(float) );
        pty = shp_mnew ( numpts * sizeof(float) );
        for ( ipts = 0; ipts < numpts; ipts++ ) {
            ptx[ipts] = curprt->ptx[ipts];
            pty[ipts] = curprt->pty[ipts];
        }
        cgr_centroid ( pty, ptx, &numpts, &yc, &xc, &area, &ier );

        /*
         * cgr_inpoly() seems not working well aroung south pole.
         */
        if ( maptyp == MAP_IEDG ) {
            inout = 1;
        } else {
            cgr_inpoly ( sys_M, &one, &yc, &xc, sys_M, &numpts,
                         pty, ptx, &inout, &ier );
        }
        shp_mfree ( ptx );
        shp_mfree ( pty );

	if ( area > tmparea && inout == 1 ) {
	    tmparea = area;
	    *xctr = xc;
	    *yctr = yc;
	    *iret = 0;
	}
    }
}
