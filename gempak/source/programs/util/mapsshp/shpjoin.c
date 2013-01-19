#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_join ( shp_part **prtlstp, int *numprt, int *iret )
/************************************************************************
 * shp_join                                                             *
 *                                                                      *
 * This function joins segments of a map line together into a single	*
 * line. 								*
 *                                                                      *
 * shp_join ( prtlstp, numprt, iret )         				*
 *                                                                      *
 * Input parameters:                                                    *
 *      **prtlstp       shp_part    	Part list head address		*
 *	*numprt		int		Number of parts			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Modified from perl 'JoinLines'  *
 ***********************************************************************/
{
    shp_part *prtlst, *curprt, *tstprt, *newprt, *tmpprt;
    float nearlt, nearln, farlat, farlon;
    int nparts, ni, nj, ipts;
/*---------------------------------------------------------------------*/
    *iret = 0;

    nparts = *numprt;
    prtlst = *prtlstp;
    for ( curprt = prtlst, ni = 0; ni < nparts; 
          ni++, curprt = curprt->nxtprt ) {
	nearlt = curprt->pty[0];
	nearln = curprt->ptx[0];
	farlat = curprt->pty[curprt->numpts-1];
	farlon = curprt->ptx[curprt->numpts-1];

	if ( G_DIFF(nearlt, farlat) && G_DIFF(nearln, farlon) ) {
/*
 * Do not join a closed polygon.
 */
	    continue;
	}
/*
 * Try to join curprt with all parts after.
 */
	for ( tstprt = curprt->nxtprt, nj = ni + 1; nj < nparts;
	      nj++, tstprt = tstprt->nxtprt ) {
	    newprt = NULL;
/*
 * Test head of curprt.
 */
	    if ( G_DIFF(nearlt, tstprt->pty[tstprt->numpts-1]) &&
	         G_DIFF(nearln, tstprt->ptx[tstprt->numpts-1]) ) {
/*
 * Join curprt to the end of tstprt.
 */
		 newprt = shp_mnew ( SHP_PRTSZ );
		 newprt->prvprt = NULL;
		 newprt->nxtprt = NULL;
		 newprt->numpts = curprt->numpts + tstprt->numpts - 1;
		 newprt->maxlat = curprt->maxlat > tstprt->maxlat ?
		                  curprt->maxlat : tstprt->maxlat;
		 newprt->minlat = curprt->minlat < tstprt->minlat ?
		                  curprt->minlat : tstprt->minlat;
		 newprt->maxlon = curprt->maxlon > tstprt->maxlon ?
		                  curprt->maxlon : tstprt->maxlon;
		 newprt->minlon = curprt->minlon < tstprt->minlon ?
		                  curprt->minlon : tstprt->minlon;
	         newprt->ptx = shp_mnew ( newprt->numpts * sizeof(float) );
	         newprt->pty = shp_mnew ( newprt->numpts * sizeof(float) );

		 for ( ipts = 0; ipts < tstprt->numpts -1; ipts++ ) {
		     newprt->ptx[ipts] = tstprt->ptx[ipts];
		     newprt->pty[ipts] = tstprt->pty[ipts];
		 }
		 for ( ipts = 0; ipts < curprt->numpts; ipts++ ) {
		     newprt->ptx[tstprt->numpts-1+ipts] = curprt->ptx[ipts];
		     newprt->pty[tstprt->numpts-1+ipts] = curprt->pty[ipts];
		 }
	     } else if ( G_DIFF(nearlt, tstprt->pty[0]) &&
	                 G_DIFF(nearln, tstprt->ptx[0]) ) {
/*
 * Join curprt to the head of tstprt.
 */
		 newprt = shp_mnew ( SHP_PRTSZ );
		 newprt->prvprt = NULL;
		 newprt->nxtprt = NULL;
		 newprt->numpts = curprt->numpts + tstprt->numpts - 1;
		 newprt->maxlat = curprt->maxlat > tstprt->maxlat ?
		                  curprt->maxlat : tstprt->maxlat;
		 newprt->minlat = curprt->minlat < tstprt->minlat ?
		                  curprt->minlat : tstprt->minlat;
		 newprt->maxlon = curprt->maxlon > tstprt->maxlon ?
		                  curprt->maxlon : tstprt->maxlon;
		 newprt->minlon = curprt->minlon < tstprt->minlon ?
		                  curprt->minlon : tstprt->minlon;
	         newprt->ptx = shp_mnew ( newprt->numpts * sizeof(float) );
	         newprt->pty = shp_mnew ( newprt->numpts * sizeof(float) );

		 for ( ipts = 0; ipts < curprt->numpts; ipts++ ) {
		     newprt->ptx[ipts] = curprt->ptx[curprt->numpts-1-ipts];
		     newprt->pty[ipts] = curprt->pty[curprt->numpts-1-ipts];
		 }
		 for ( ipts = 1; ipts < tstprt->numpts; ipts++ ) {
		     newprt->ptx[curprt->numpts+ipts-1] = tstprt->ptx[ipts];
		     newprt->pty[curprt->numpts+ipts-1] = tstprt->pty[ipts];
	         }
	     } else if ( G_DIFF(farlat, tstprt->pty[tstprt->numpts-1]) &&
	                 G_DIFF(farlon, tstprt->ptx[tstprt->numpts-1]) ) {
/*
 * Join curprt to the end of tstprt.
 */
		 newprt = shp_mnew ( SHP_PRTSZ );
		 newprt->prvprt = NULL;
		 newprt->nxtprt = NULL;
		 newprt->numpts = curprt->numpts + tstprt->numpts - 1;
		 newprt->maxlat = curprt->maxlat > tstprt->maxlat ?
		                  curprt->maxlat : tstprt->maxlat;
		 newprt->minlat = curprt->minlat < tstprt->minlat ?
		                  curprt->minlat : tstprt->minlat;
		 newprt->maxlon = curprt->maxlon > tstprt->maxlon ?
		                  curprt->maxlon : tstprt->maxlon;
		 newprt->minlon = curprt->minlon < tstprt->minlon ?
		                  curprt->minlon : tstprt->minlon;
	         newprt->ptx = shp_mnew ( newprt->numpts * sizeof(float) );
	         newprt->pty = shp_mnew ( newprt->numpts * sizeof(float) );

		 for ( ipts = 0; ipts < curprt->numpts - 1; ipts++ ) {
		     newprt->ptx[ipts] = curprt->ptx[ipts];
		     newprt->pty[ipts] = curprt->pty[ipts];
		 }
		 for ( ipts = 0; ipts < tstprt->numpts; ipts++ ) {
		     newprt->ptx[curprt->numpts-1+ipts] = 
		         tstprt->ptx[tstprt->numpts-1-ipts];
		     newprt->pty[curprt->numpts-1+ipts] =
		         tstprt->pty[tstprt->numpts-1-ipts];
		 }
	     } else if ( G_DIFF(farlat, tstprt->pty[0]) &&
	                 G_DIFF(farlon, tstprt->ptx[0]) ) {
/*
 * Join curprt to the head of tstprt.
 */
	         newprt = shp_mnew ( SHP_PRTSZ );
		 newprt->prvprt = NULL;
		 newprt->nxtprt = NULL;
		 newprt->numpts = curprt->numpts + tstprt->numpts - 1;
		 newprt->maxlat = curprt->maxlat > tstprt->maxlat ?
		                  curprt->maxlat : tstprt->maxlat;
		 newprt->minlat = curprt->minlat < tstprt->minlat ?
		                  curprt->minlat : tstprt->minlat;
		 newprt->maxlon = curprt->maxlon > tstprt->maxlon ?
		                  curprt->maxlon : tstprt->maxlon;
		 newprt->minlon = curprt->minlon < tstprt->minlon ?
		                  curprt->minlon : tstprt->minlon;
	         newprt->ptx = shp_mnew ( newprt->numpts * sizeof(float) );
	         newprt->pty = shp_mnew ( newprt->numpts * sizeof(float) );

		 for ( ipts = 0; ipts < curprt->numpts; ipts++ ) {
		     newprt->ptx[ipts] = curprt->ptx[ipts];
		     newprt->pty[ipts] = curprt->pty[ipts];
		 }
		 for ( ipts = 1; ipts < tstprt->numpts; ipts++ ) {
		     newprt->ptx[curprt->numpts+ipts-1] = tstprt->ptx[ipts];
		     newprt->pty[curprt->numpts+ipts-1] = tstprt->pty[ipts];
		 }
	     }

	     if ( newprt != NULL ) {
/*
 * Replace curprt with newprt.
 */
	        if ( curprt->prvprt == NULL ) {
	            prtlst = newprt;
	        } else {
	            curprt->prvprt->nxtprt = newprt;
		    newprt->prvprt = curprt->prvprt;
	        }
	        if ( curprt->nxtprt != NULL ) {
	            curprt->nxtprt->prvprt = newprt;
	            newprt->nxtprt = curprt->nxtprt;
	        }
	        tmpprt = curprt;
	        curprt = newprt;
	        nearlt = curprt->pty[0];
	        nearln = curprt->ptx[0];
	        farlat = curprt->pty[curprt->numpts-1];
	        farlon = curprt->ptx[curprt->numpts-1];
	        shp_mfree ( tmpprt->ptx );
	        shp_mfree ( tmpprt->pty );
	        shp_mfree ( tmpprt );
/*
 * Remove tstprt.
 */
	        if ( tstprt->prvprt != NULL ) {
	            tstprt->prvprt->nxtprt = tstprt->nxtprt;
	        }
	        if ( tstprt->nxtprt != NULL ) {
	            tstprt->nxtprt->prvprt = tstprt->prvprt;
	        }
	        tmpprt = tstprt;
	        tstprt = tstprt->prvprt;
	        nj--;
	        shp_mfree ( tmpprt->ptx );
	        shp_mfree ( tmpprt->pty );
	        shp_mfree ( tmpprt );
	        nparts--;
	    }
	}
    }

    *prtlstp = prtlst;
    *numprt = nparts;
}
