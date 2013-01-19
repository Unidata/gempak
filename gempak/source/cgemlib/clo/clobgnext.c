#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

extern  CLO_t           clo;

void clo_bgnext ( int *minpts, int *maxpts, float *filt, int *npts, 
		  float *lat, float *lon, int *iret )
/************************************************************************
 * clo_bgnext                                                           *
 *                                                                      *
 * This function returns the next bound area.				*
 *                                                                      *
 * clo_bgnext ( minpts, maxpts, filt, npts, lat, lon, iret )            *
 *                                                                      *
 * Input parameters:                                                    *
 *      *minpts         int     Min number of points allowed returned   *
 *      *maxpts         int     Max number of points allowed returned   *
 *      *filt           float   Filter factor-reduce pts if necessary	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *npts           int     Number of points returned               *
 *      *lat            float   Latitudes                               *
 *      *lon            float   Longitudes                              *
 *      *iret           int     Return code                             *
 *                              = 0  - normal                           *
 *                              = -1 - end of data                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      4/01   Created                                 *
 * D.W.Plummer/NCEP      6/01   Added call to clo_bqtag			*
 * D.W.Plummer/NCEP      9/02   Bug fix for npBnd > maxpts		*
 ***********************************************************************/
{
int	np, np1, np2, ier;
long	strec;
float	*x1, *y1, *x2, *y2, filter;
Bnd_t   *bptr;

/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Set a pointer to the proper bounds structure.
     */
    bptr = &(clo.loc[whichBnd].bnd);

    /*
     *  The global variable bndsptBnd always points to the last-accessed
     *  bounds part. It is initialized to -1.
     */

    bndsptBnd++;

    /*
     *  The bounds structure is assumed to have been sorted by longitude
     *  in the function clo_stype, so perform a loop with if tests 
     *  searching for valid bounds areas. Also consider the minimum
     *  number of points requested (prior to filtering).
     */
    while ( boundBnd < bptr->nbnd  &&
	    bptr->bound[boundBnd].maxlon >= lonllBnd )  {

      if ( bptr->bound[boundBnd].minlon <= lonurBnd  &&
           bptr->bound[boundBnd].minlat <= laturBnd  &&
           bptr->bound[boundBnd].maxlat >= latllBnd )  {

       if ( clo_bqtag( bptr->bound[boundBnd].info ) )  {

	while ( bndsptBnd < bptr->bound[boundBnd].nparts )  {

	  np = bptr->bound[boundBnd].bndspt[bndsptBnd].npts;
	  if ( bptr->bound[boundBnd].bndspt[bndsptBnd].minlon <= lonurBnd  &&
	       bptr->bound[boundBnd].bndspt[bndsptBnd].minlat <= laturBnd  &&
	       bptr->bound[boundBnd].bndspt[bndsptBnd].maxlat >= latllBnd  &&
	       bptr->bound[boundBnd].bndspt[bndsptBnd].maxlon >= lonllBnd  &&
	       np > *minpts )  {

	    /*
	     *  Found valid bound part.  Read the record and check the
	     *  number of points; reduce the number of points if necessary.
	     */
	    *npts = np;
	    strec = bptr->bound[boundBnd].bndspt[bndsptBnd].strec;

	    clo_brdrec ( strec, &ier );

	    if ( ier == 0 )  {

	      if ( npBnd <= *maxpts  &&  G_DIFF(*filt, 0.0F) )  {

		*npts = npBnd;
		memmove ( lat, xpBnd, (size_t)(*npts)*sizeof(lat[0]) );
		memmove ( lon, ypBnd, (size_t)(*npts)*sizeof(lon[0]) );
	
	      }
	      else  {

		/*
		 *  Reduce the number of points using CV_RDUC.
		 *  First, allocate some working space.
		 */

		x1 = (float *)malloc((size_t)npBnd * sizeof(float));
		y1 = (float *)malloc((size_t)npBnd * sizeof(float));
		x2 = (float *)malloc((size_t)npBnd * sizeof(float));
		y2 = (float *)malloc((size_t)npBnd * sizeof(float));

		gtrans ( sys_M, sys_D, &npBnd, xpBnd, ypBnd,
			 x1, y1, &ier, strlen(sys_M), strlen(sys_D) );
		np1 = npBnd;

		filter = *filt;
		if ( G_DIFF(filter, 0.0F) )  filter = 0.05F;

		cv_rduc ( &np1, x1, y1, &filter, &np2, x2, y2, &ier );

		if ( np2 > *maxpts )  {

		  memmove ( x1, x2, (size_t)np2*sizeof(x1[0]) );
		  memmove ( y1, y2, (size_t)np2*sizeof(y1[0]) );
		  np1 = np2;

		  /*
		   *  If filter is non-zero, use that value.
		   *  Otherwise, use a small value (0.1) to reduce.
		   */
		  while ( np1 > *maxpts )  {

		    filter += 0.05F;
		    cv_rduc ( &np1, x1, y1, &filter, &np2, x2, y2, &ier );

		    if ( np2 > *maxpts )  {
		      memmove ( x1, x2, (size_t)np2*sizeof(x1[0]) );
		      memmove ( y1, y2, (size_t)np2*sizeof(y1[0]) );
		    }
		    np1 = np2;

		  }

		}

		gtrans ( sys_D, sys_M, &np2, x2, y2,
			 xpBnd, ypBnd, &ier, strlen(sys_D), strlen(sys_M) );
		npBnd = np2;

		*npts = npBnd;
                memmove ( lat, xpBnd, (size_t)(*npts)*sizeof(lat[0]) );
                memmove ( lon, ypBnd, (size_t)(*npts)*sizeof(lon[0]) );
	
		free ( y2 );
		free ( x2 );
		free ( y1 );
		free ( x1 );
		
	      }

	      /*
	       *  Found and accessed a bound; ready to return.
	       */

	      return;

	    }

	  }

          bndsptBnd++;

	}

       }

      }

      bndsptBnd = 0;
      boundBnd++;

    }

    *iret = -1;

}
