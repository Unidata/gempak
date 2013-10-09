#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

#define MAP_PTS 4999	/* Maximum number of points per segment
				required by SSFGSF program */

void shp_cmap ( shp_part *prtlst, char *mapnam, float ratio, int *iret )
/************************************************************************
 * shp_cmap                                                             *
 *                                                                      *
 * This function creates a map file in Sequential Standard Format (SSF).*
 *                                                                      *
 * shp_cmap ( prtlst, mapnam, ratio, iret )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *prtlst         shp_part    	Data part list head   		*
 *	*mapnam		char		Output file name       		*
 *	ratio		float		Reduce point ratio		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 ***********************************************************************/
{
    shp_part *curprt, *oneprt, outprt;
    int tltpts, rempts, nummax;
    FILE *mapfp;
    int ipts, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    mapfp = cfl_wopn ( mapnam, &ier );
    if ( ier != 0 ) {
        fprintf ( stderr, "File %s open failed.\n", mapnam );
	exit ( -1 );
    }

    curprt = prtlst;
    while ( curprt ) {
	/*
	 * Make a copy of curprt, because we do not want shp_thin
	 * to modify the original data. The original data may be
	 * used to create other resolution maps.
	 */
        oneprt = shp_mnew ( SHP_PRTSZ );
        oneprt->numpts = curprt->numpts;
        oneprt->ptx = shp_mnew ( oneprt->numpts * sizeof(float) );
        oneprt->pty = shp_mnew ( oneprt->numpts * sizeof(float) );
        for ( ipts = 0; ipts < oneprt->numpts; ipts++ ) {
            oneprt->ptx[ipts] = curprt->ptx[ipts];
            oneprt->pty[ipts] = curprt->pty[ipts];
        }
        
        /*
	 * Reduce number of points.
	 */
        shp_thin ( oneprt, ratio, &ier );

	/*
	 * Output one part.
	 */
        if ( oneprt->numpts > MAP_PTS ) {
            tltpts = oneprt->numpts;
	    nummax = (int)( tltpts / MAP_PTS );
	    rempts = tltpts - nummax * MAP_PTS;

	    for ( ipts = 0; ipts < nummax; ipts++ ) {
	        outprt.numpts = MAP_PTS;
	        outprt.ptx = &(oneprt->ptx[ipts*MAP_PTS]);
	        outprt.pty = &(oneprt->pty[ipts*MAP_PTS]);

	        shp_wprt ( mapfp, &outprt, &ier );
	    }

	    if ( rempts > 0 ) {
	        outprt.numpts = rempts;
	        outprt.ptx = &(oneprt->ptx[nummax*MAP_PTS]);
	        outprt.pty = &(oneprt->pty[nummax*MAP_PTS]);

	        shp_wprt ( mapfp, &outprt, &ier );
	    }
        } else {
	    outprt.numpts = oneprt->numpts;
	    outprt.ptx = oneprt->ptx;
	    outprt.pty = oneprt->pty;
	    shp_wprt ( mapfp, &outprt, &ier );
	}

        shp_mfree ( oneprt->ptx );
        shp_mfree ( oneprt->pty );
        shp_mfree ( oneprt );

	curprt = curprt->nxtprt;
    }
    
    cfl_clos ( mapfp, &ier );
}
