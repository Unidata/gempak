#include "geminc.h"
#include "gemprm.h"
#include "proto_uka.h"
#include "cascmn.h"


void uka_jtsp ( int tmppt, int idxtyp[], float indx[], float tlvl[], 
		int *iret )
/************************************************************************
 * uka_jtsp								*
 *                                                                      *
 * This function fills out the wind speed values and flight levels 	*
 * for the hash marks. 							*
 *                                                                      *
 * uka_jtsp ( tmppt, idxtyp, indx, tlvl, iret )				* 
 *                                                                      *
 * Input parameters:                                                    *
 *	tmppt		int		Number of points on the jet	*
 *	idxtyp[]	int		Array of point types		*
 *					   0 = core line point		*
 *					   1 = wind barb point		*
 *					   2 = hash point		*
 * Input and Output parameters:                                         *
 *	indx[]		float		Array of wind speeds ( m s-1 )  *
 *	tlvl[]		float		Array of flight level ( m )	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        01/02   Created					*
 * A. Hardy/SAIC         3/02   Set jet core wind speeds to SIGRLMS	*
 * M. Li/SAIC		 5/04	Copied from sig_jtsp			*
 * M. Li/SAIC		 7/04	Added tlvl, rewrote for hash vals	*
 * D. Kidwell/NCEP	 8/04	Changed error handling for hash speed   *
 ***********************************************************************/
{

    int		ij, ii, jj, nwnd, jwi[MAXPTS], jhi[MAXPTS];
    int		n1, n2, nn, kk, nhp;
    float       minspd, speed, change, wndspd; 
    float       lower, w80kt, lowm20, tmpval; 

/*---------------------------------------------------------------------*/
    *iret   = 0;
    tmpval  = 89.0F;
    minspd  = pr_knms ( &tmpval );
    tmpval  = 20.0F;
    change  = pr_knms ( &tmpval );
    w80kt   = 4 * change;
	    
   /*
    * Get the indices of the barb points. 
    */

    nwnd = 0;
    for ( ij = 0; ij < tmppt; ij++ ) {
       	if ( idxtyp[ij] == 1 ) {
	    jwi[nwnd++] = ij;
	}
    }

   /* 
    * Assign wind speed values to hash marks.
    */
        
    if ( nwnd > 1 ) {
	for ( ii = 0; ii < nwnd - 1; ii++ ) {
	    
	   /*
	    * Determine which barb has the higher speed, and 
	    * assign wind speed to hash marks.
	    */

	    nhp = 0;
	    n1 = jwi[ii];
	    n2 = jwi[ii+1];
	    if ( indx[n1] < ( indx[n2] - 0.1F ) ) {
		lower  = indx[n1];
		speed  = indx[n2];
		lowm20 = lower - change - 1.0F;
		for ( jj = n2; jj >= n1; jj-- ) {
                    if ( idxtyp[jj] == 2 ) {
                    	wndspd = speed - change;
                    	if ( wndspd < w80kt ) {
                            wndspd = w80kt;
                    	}
                    	if ( wndspd <= lowm20 ) {
                            wndspd = speed;
                    	}
                    	indx[jj] = wndspd;
                    	speed = wndspd;
                    	jhi[nhp++] = jj;
                    }
            	}
	    }
	    else if ( indx[n1] > ( indx[n2] + 0.1F ) ) {
		lower  = indx[n2];
                speed  = indx[n1];
		lowm20 = lower - change - 1.0F;
                for ( jj = n1; jj <= n2; jj++ ) {
                    if ( idxtyp[jj] == 2 ) {
                        wndspd = speed - change;
                        if ( wndspd < w80kt ) {
                            wndspd = w80kt;
                        }
                        if ( wndspd <= lowm20 ) {
                            wndspd = speed;
                        }
                        indx[jj] = wndspd;
                        speed = wndspd;
                        jhi[nhp++] = jj;
                    }
                }
	    }
	    else {

		/*
		 * If barb speeds are equal, midpoint hash is assigned
		 * the minimum speed.
		 */

		speed  = indx[n1];
		for ( jj = n1; jj <= n2; jj++ ) {
                    if ( idxtyp[jj] == 2 ) {
                    	wndspd = speed - change;
                    	if ( wndspd < w80kt ) {
                            wndspd = w80kt;
                    	}
                    	indx[jj] = wndspd;
                    	speed = wndspd;
                    	jhi[nhp++] = jj;
                    }
            	}

		speed  = indx[n2];
		for ( jj = n2; jj >= n1; jj-- ) {
                    if ( idxtyp[jj] == 2 ) {
                    	wndspd = speed - change;
                    	if ( wndspd < w80kt ) {
                            wndspd = w80kt;
                    	}
			if ( wndspd > indx[jj] ) {
                    	    indx[jj] = wndspd;
			}
                    	speed = wndspd;
                    }
            	}
	    }
	}  /* for loop */
    }

   /*
    * Assign wind speed to hash marks in the end segments.
    */

    if ( nwnd > 0 ) {

	if ( jwi[0] > 0 ) {
	    speed = indx[jwi[0]];
	    for ( jj = jwi[0]; jj >= 0; jj-- ) {  
	    	if ( idxtyp[jj] == 2 ) { 
	    	    wndspd = speed - change;
		    if ( wndspd <= minspd ) {
		        wndspd = speed;
		    }
	    	    indx[jj] = wndspd;
            	    speed = wndspd;
	        }
	    }
	}

	if ( jwi[nwnd-1] < tmppt ) {
	    kk = jwi[nwnd-1];
            speed = indx[kk];
            for ( jj = kk; jj < tmppt; jj++ ) {
                if ( idxtyp[jj] == 2 ) {
                    wndspd = speed - change;
                    if ( wndspd <= minspd ) {
                        wndspd = speed;
                    }
                    indx[jj] = wndspd;
                    speed = wndspd;
                }
            }
        }

    }


   /*
    * Assign flight level to hashes, using level of nearest barb.
    */

    if ( nwnd > 1 ) {
        for ( ii = 0; ii < nwnd - 1; ii++ ) {

	    nhp = 0;
	    n1 = jwi[ii];
            n2 = jwi[ii+1];
	    for ( jj = n1; jj <= n2; jj++ ) {
		if ( idxtyp[jj] == 2 ) {
                    jhi[nhp++] = jj;
                }
            }

           /*
	    * Assign flight level to hash marks.
            */

	    if ( nhp == 1 ) {
		tlvl[jhi[0]] = G_MAX ( tlvl[n1], tlvl[n2] );
	    }
	    else if ( nhp > 1 ) {
		for ( jj = 0; jj < nhp/2; jj++ ) {
                    tlvl[jhi[jj]] = tlvl[n1];
                }

		if ( ( nhp % 2 ) != 0 ) {
		    nn = nhp/2 + 1;
		    tlvl[jhi[nhp/2]] = G_MAX ( tlvl[n1], tlvl[n2] );
		}
		else {
		    nn = nhp/2;
		}
		for ( jj = nn; jj < nhp; jj++ ) {
                    tlvl[jhi[jj]] = tlvl[n2];
                }
	    }
	}
    }

   /*
    * Assign flight level to hash marks at the end segments.
    */

    if ( nwnd > 0 ) {

        if ( jwi[0] > 0 ) {
            for ( jj = jwi[0]; jj >= 0; jj-- ) {
                if ( idxtyp[jj] == 2 ) {
		    tlvl[jj] = tlvl[jwi[0]]; 
                }
            }
        }

        if ( jwi[nwnd-1] < tmppt ) {
	    for ( jj = jwi[nwnd-1]; jj < tmppt; jj++ ) {
                if ( idxtyp[jj] == 2 ) {
                    tlvl[jj] = tlvl[jwi[nwnd-1]];
                }
            }
        }
    }
   
}
