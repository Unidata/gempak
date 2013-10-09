#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

#define near(val1,val2,tol) ( ( (val1) <= (val2) + (tol) ) && \
                              ( (val1) >= (val2) - (tol) ) )

void shp_strip ( shp_part **prtlstp, int *numprt, float tol, int *iret )
/************************************************************************
 * shp_strip                                                            *
 *                                                                      *
 * This function strips overlapped map lines.				*
 *                                                                      *
 * shp_strip ( prtlstp, numprt, tol, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      **prtlstp	shp_part    	Part list			*
 *	*numprt		int		Total number of parts		*
 *	tol		float		Overlap tolerance		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Modified from perl 'mapstrip'   *
 ***********************************************************************/
{
    shp_part *newhead, *prtlst, *curprt, *mchprt, *newprt, *tmpprt;
    float curlat, curlon, prvlat, prvlon;
    float mchlat, mchlon, prvmlt, prvmln, nxtmlt, nxtmln;
    float maxlat, minlat, maxlon, minlon;
    float *xbuf, *ybuf;
    int *valid, allvalid, crtnew;
    int tltprt, ncur, nmch, ipts, jpts, npts, curtst, mchtst;
/*---------------------------------------------------------------------*/
    *iret = 0;

    tltprt = *numprt;
    newhead = *prtlstp;
    prtlst = *prtlstp;
    for ( curprt = prtlst, ncur = 0; ncur < *numprt; 
          ncur++, curprt = curprt->nxtprt ) {
	/*
	 * First assume all points are valid.
	 */
	valid = shp_mnew ( curprt->numpts * sizeof(int) );
	for ( ipts = 0; ipts < curprt->numpts; ipts++ ) {
	    valid[ipts] = G_TRUE;
	}
	allvalid = G_TRUE;

	/*
	 * Try to match curprt with all parts ahead of it.
	 */
	for ( mchprt = prtlst, nmch = 0; nmch < ncur;
	      nmch++, mchprt = mchprt->nxtprt ) {
	    if ( mchprt->minlat > curprt->maxlat ||
	         curprt->minlat > mchprt->maxlat ||
		 mchprt->minlon > curprt->maxlon ||
		 curprt->minlon > mchprt->maxlon ) {
		/*
		 * These two parts have no overlapped points.
		 */
	        continue;
	    }

	    /*
	     * Loop over each point in curprt.
	     */
	    for ( ipts = 0; ipts < curprt->numpts; ipts++ ) {
		if ( ! valid[ipts] ) {
		    /*
		     * Not necessary to test an invalid point.
		     */
		    continue;
		}

	        curlat = curprt->pty[ipts];
		curlon = curprt->ptx[ipts];
		if ( ipts == 0 ) {
		    prvlat = RMISSD;
		    prvlon = RMISSD;
		} else {
		    prvlat = curprt->pty[ipts-1];
		    prvlon = curprt->ptx[ipts-1];
		}

		/*
		 * Test matching of one point in curprt with
		 * every point in mchprt.
		 */
		for ( jpts = 1; jpts < mchprt->numpts - 1; jpts++ ) {
		    mchlat = mchprt->pty[jpts];
		    mchlon = mchprt->ptx[jpts];
		    prvmlt = mchprt->pty[jpts-1];
		    prvmln = mchprt->ptx[jpts-1];
		    nxtmlt = mchprt->pty[jpts+1];
		    nxtmln = mchprt->ptx[jpts+1];

		    if ( near ( curlat, mchlat, tol ) &&
		         near ( curlon, mchlon, tol ) ) {
		        /*
		         * Find a match point, checking for
			 * previous and next point.
		         */
		        if ( near ( prvlat, prvmlt, tol ) &&
			     near ( prvlon, prvmln, tol ) ) {
			   /*
			    * Previous point also matches, continue to
			    * test matching in the same direction.
			    */
			    curtst = ipts + 1;
			    mchtst = jpts + 1;
			    while ( curtst < curprt->numpts       &&
			        mchtst < mchprt->numpts           &&
				near ( curprt->pty[curtst],
				       mchprt->pty[mchtst], tol ) &&
				near ( curprt->ptx[curtst],
				       mchprt->ptx[mchtst], tol ) ) {

				allvalid = G_FALSE;
			        valid[curtst-1] = G_FALSE;
				curtst++;
				mchtst++;
			    }
		        }

		        if ( near ( prvlat, nxtmlt, tol ) &&
			     near ( prvlon, nxtmln, tol ) ) {
			   /*
			    * next point also matches, continue to
			    * test matching in the reverse direction.
			    */
			    curtst = ipts + 1;
			    mchtst = jpts - 1;
			    while ( curtst < curprt->numpts       &&
			        mchtst >= 0                       &&
				near ( curprt->pty[curtst],
				       mchprt->pty[mchtst], tol ) &&
				near ( curprt->ptx[curtst],
				       mchprt->ptx[mchtst], tol ) ) {

				allvalid = G_FALSE;
			        valid[curtst-1] = G_FALSE;
				curtst++;
				mchtst--;
			    }
		        }
		    }
		}
	    }
	}

        if ( ! allvalid ) {
	    xbuf = shp_mnew ( curprt->numpts * sizeof(float) );
	    ybuf = shp_mnew ( curprt->numpts * sizeof(float) );
	    crtnew = G_TRUE;
	    npts = 0;
	    for ( ipts = 0; ipts < curprt->numpts; ipts++ ) {
	        if ( valid[ipts] ) {
		    if ( crtnew ) {
		        maxlat = -90.0;
		        minlat = 90.0;
		        maxlon = -180.0;
		        minlon = 180.0;
		        crtnew = G_FALSE;
		    }
		    maxlat = maxlat > curprt->pty[ipts] ? maxlat :
		        curprt->pty[ipts];
		    minlat = minlat < curprt->pty[ipts] ? minlat :
		        curprt->pty[ipts];
		    maxlon = maxlon > curprt->ptx[ipts] ? maxlon :
		        curprt->ptx[ipts];
		    minlon = minlon < curprt->ptx[ipts] ? minlon :
		        curprt->ptx[ipts];

		    xbuf[npts] = curprt->ptx[ipts];
		    ybuf[npts] = curprt->pty[ipts];
		    npts++;
	        } else {
		    if ( npts > 1 ) {
		        /*
		         * There are enough points to
		         * create a new part.
		         */
		        newprt = shp_mnew ( SHP_PRTSZ );
			newprt->prtflg = PRT_OUT;
			newprt->numpts = npts;
			newprt->prvprt = NULL;
			newprt->nxtprt = NULL;
			newprt->maxlat = maxlat;
			newprt->minlat = minlat;
			newprt->maxlon = maxlon;
			newprt->minlon = minlon;
			newprt->ptx = shp_mnew ( newprt->numpts *
			                         sizeof(float) );
			newprt->pty = shp_mnew ( newprt->numpts *
			                         sizeof(float) );
			for ( jpts = 0; jpts < npts; jpts++ ) {
			    newprt->ptx[jpts] = xbuf[jpts];
			    newprt->pty[jpts] = ybuf[jpts];
			}

			/*
			 * Insert newprt ahead of newhead.
			 */
			newprt->nxtprt = newhead;
			newhead->prvprt = newprt;
			newhead = newprt;
			tltprt++;
		    }
		    crtnew = G_TRUE;
		    npts = 0;
	        }
	    }

	    /*
	     * All the remaining points are valid.
	     */
	    if ( npts > 1 ) {
	        /*
	         * There are enouth points to create a new part.
		 */
		newprt = shp_mnew ( SHP_PRTSZ );
		newprt->prtflg = PRT_OUT;
		newprt->numpts = npts;
		newprt->prvprt = NULL;
		newprt->nxtprt = NULL;
		newprt->maxlat = maxlat;
		newprt->minlat = minlat;
		newprt->maxlon = maxlon;
		newprt->minlon = minlon;
		newprt->ptx = shp_mnew ( newprt->numpts *
		                         sizeof(float) );
		newprt->pty = shp_mnew ( newprt->numpts *
		                          sizeof(float) );
		for ( jpts = 0; jpts < npts; jpts++ ) {
		    newprt->ptx[jpts] = xbuf[jpts];
		    newprt->pty[jpts] = ybuf[jpts];
		}

		/*
		 * Insert newprt ahead of newhead.
		 */
		newprt->nxtprt = newhead;
		newhead->prvprt = newprt;
		newhead = newprt;
		tltprt++;
	    }

	    /*
	     * curprt has been stripped, mark it as PRT_DEL.
	     */
	    curprt->prtflg = PRT_DEL;
	    shp_mfree ( xbuf );
	    shp_mfree ( ybuf );
        }
	shp_mfree ( valid );

	/*
        if ( ( ncur % 100 ) == 0 ) {
            printf ( "Stripped part: %d\n", ncur );
        }
	*/
    }

    /*
     * Remove those parts marked as PRT_DEL from part list.
     */
    for ( curprt = newhead, ipts = 0; ipts < tltprt; 
          ipts++, curprt = curprt->nxtprt ) {

        if ( curprt->prtflg == PRT_DEL ) {
	    if ( curprt->prvprt != NULL ) {
	        curprt->prvprt->nxtprt = curprt->nxtprt;
	    } else {
	        newhead = curprt->nxtprt;
		if ( newhead != NULL ) {
		    newhead->prvprt = NULL;
		}
	    }

	    if ( curprt->nxtprt != NULL ) {
	        curprt->nxtprt->prvprt = curprt->prvprt;
	    }

	    tmpprt = curprt;
	    curprt = curprt->prvprt;
	    ipts--;
	    shp_mfree ( tmpprt->ptx );
	    shp_mfree ( tmpprt->pty );
	    shp_mfree ( tmpprt );
	    tltprt--;
	}
    }

    *prtlstp = newhead;
    *numprt = tltprt;
}
