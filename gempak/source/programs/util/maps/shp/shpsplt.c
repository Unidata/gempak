#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_splt ( shp_record *shprec, int numrec, int *iret )
/************************************************************************
 * shp_splt                                                             *
 *                                                                      *
 * This function splits a data part if the part has more than MAXOUT	*
 * number of points, or if it crosses the international dateline.	*
 *                                                                      *
 * shp_splt ( shprec, numrec, iret )                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *shprec         shp_record      Shape record     		*
 *	numrec		int		Number of records		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 ***********************************************************************/
{
    shp_record *currec;
    shp_part *curprt, **nxtprtp, *savprt, *newprt;
    int npp, rec, prt, pts, kk;
    int tltpts, rempts, nummax;
    float *xout, *yout, xsave, ysave;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * If the number of points of a part exceeds MAXOUT, break it
     * into separate parts.
     */
    for ( rec = 0, currec = shprec;  rec < numrec;
	  rec++, currec = currec->nxtrec ) {
        for ( curprt = currec->shpart, prt = 0; prt < currec->numprt; 
              prt++, curprt = curprt->nxtprt ) {

            if ( curprt->numpts > MAXOUT ) {
                tltpts = curprt->numpts;
	        nummax = (int)( tltpts / MAXOUT );
	        rempts = tltpts - nummax * MAXOUT;

	        /*
	         * The new current part.
	         */
	        curprt->numpts = MAXOUT;
	        savprt = curprt->nxtprt;
	        nxtprtp = &(curprt->nxtprt);

	        /*
	         * New parts with MAXOUT points.
	         */
	        for ( kk = 1; kk < nummax; kk++ ) {
	            newprt = shp_mnew ( SHP_PRTSZ );
	            newprt->numpts = MAXOUT;
		    newprt->ptx = shp_mnew ( MAXOUT * sizeof(float) );
		    newprt->pty = shp_mnew ( MAXOUT * sizeof(float) );
		    for ( pts = 0; pts < MAXOUT; pts++ ) {
		        newprt->ptx[pts] = curprt->ptx[pts+MAXOUT*kk];
		        newprt->pty[pts] = curprt->pty[pts+MAXOUT*kk];
		    }

		    *nxtprtp = newprt;
		    nxtprtp = &(newprt->nxtprt);
		    currec->numprt += 1;
	        }

	        /*
	         * New part with the remaining points.
	         */
	        if ( rempts > 0 ) {
	            newprt = shp_mnew ( SHP_PRTSZ );
		    newprt->numpts = rempts; 
		    newprt->ptx = shp_mnew ( rempts * sizeof(float) );
		    newprt->pty = shp_mnew ( rempts * sizeof(float) );
		    for ( pts = 0; pts < rempts; pts++ ) {
		        newprt->ptx[pts] = curprt->ptx[pts+MAXOUT*nummax];
		        newprt->pty[pts] = curprt->pty[pts+MAXOUT*nummax];
		    }

		    *nxtprtp = newprt;
		    nxtprtp = &(newprt->nxtprt);
		    currec->numprt += 1;
	        }

	        /*
	         * Link to the rest of parts.
	         */
	        *nxtprtp = savprt;
            }
        }
    }

    /*
     * If the part crosses the international dateline, split it.
     */
    for ( rec = 0, currec = shprec;  rec < numrec;
	  rec++, currec = currec->nxtrec ) {
        for ( curprt = currec->shpart,  prt= 0; prt < currec->numprt; 
              prt++, curprt = curprt->nxtprt ) {

            tltpts = curprt->numpts;
	    xout = curprt->ptx;
	    yout = curprt->pty;

	    for ( npp = 0, kk = 0; kk < tltpts; kk++, npp++ ) {

	        /*
	         * If the point extends beyond the top of the cartesian
	         * world, wrap it to the opposite hemisphere.
	         */
	        if ( yout[npp] > 90.0F ) {
	            yout[npp] = 180.0F - yout[npp];
		    xout[npp] += 180.0F;
	        } else if ( yout[npp] < -90.0F ) {
		    yout[npp] = -180.0F - yout[npp];
		    xout[npp] += 180.0F;
	        }

	        /*
	         * If the point extends across the dateline out of the
	         * cartesian world, wrap it to the other side of the
	         * dateline.
	         */
	        if ( xout[npp] > 180.0F ) {
	            xout[npp] -= 360.0F;
	        }
	        if ( xout[npp] < -180.0F ) {
	            xout[npp] += 360.0F;
	        }

	        /*
	         * If there are any existing points, we can check for 
	         * lines that extend over the dateline.
	         */
	        if ( npp > 0 ) {
	            if ( xout[npp-1] <= -90.0F && xout[npp] > 90.0F ) {
		        xsave = xout[npp];
		        ysave = yout[npp];
		        xout[npp] = -180.0F;
		        yout[npp] = (xout[npp] - xout[npp-1]) /
		                    (xsave - 360.0F - xout[npp-1]) *
				    (yout[npp] - yout[npp-1]) + yout[npp-1];
		        npp += 1;

		        /*
		         * The new current part.
		         */
		        curprt->numpts = npp;

		        /*
		         * Create a new part for the rest of the points.
		         */
		        newprt = shp_mnew ( SHP_PRTSZ );
		        newprt->numpts = tltpts - kk + 1; 
		        newprt->ptx = shp_mnew ( newprt->numpts * 
			              sizeof(float) );
		        newprt->pty = shp_mnew ( newprt->numpts * 
			              sizeof(float) );
		        newprt->ptx[0] = 180.F;
		        newprt->pty[0] = yout[npp-1];
		        newprt->ptx[1] = xsave;
		        newprt->pty[1] = ysave;
		        for ( pts = 2; pts < newprt->numpts; pts++ ) {
		            newprt->ptx[pts] = curprt->ptx[pts+kk-1];
		            newprt->pty[pts] = curprt->pty[pts+kk-1];
		        }

		        /*
		         * Re-link the list.
		         */
		        savprt = curprt->nxtprt;
		        curprt->nxtprt = newprt;
		        newprt->nxtprt = savprt;
		        currec->numprt += 1;

		        /*
		         * Finished the current part because we split it.
		         */
		        break;
	            } else if ( xout[npp-1] > 90.0F && 
		                xout[npp] <= -90.0F ) {
	                xsave = xout[npp];
		        ysave = yout[npp];
		        xout[npp] = 180.0F;
		        yout[npp] = (xout[npp] - xout[npp-1]) /
		    		    (xsave + 360.0F - xout[npp-1]) *
				    (yout[npp] - yout[npp-1]) + yout[npp-1];
		        npp += 1;

		        /*
		         * The new current part.
		         */
		        curprt->numpts = npp;

		        /*
		         * Create a new part for the rest of the points.
		         */
		        newprt = shp_mnew ( SHP_PRTSZ );
		        newprt->numpts = tltpts - kk + 1; 
		        newprt->ptx = shp_mnew ( newprt->numpts *
			              sizeof(float) );
		        newprt->pty = shp_mnew ( newprt->numpts *
			              sizeof(float) );
		        newprt->ptx[0] = -180.F;
		        newprt->pty[0] = yout[npp-1];
		        newprt->ptx[1] = xsave;
		        newprt->pty[1] = ysave;
		        for ( pts = 2; pts < newprt->numpts; pts++ ) {
		            newprt->ptx[pts] = curprt->ptx[pts+kk-1];
			    newprt->pty[pts] = curprt->pty[pts+kk-1];
		        }

		        /*
		         * Re-link the list.
		         */
		        savprt = curprt->nxtprt;
		        curprt->nxtprt = newprt;
		        newprt->nxtprt = savprt;
		        currec->numprt += 1;

		        /*
		         * Finished the current part because we split it.
		         */
		        break;
	            }
	        }
	    }
        }
    }
}
