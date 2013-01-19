#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"

#define	SC(xxxx)	( G_NINT ( (xxxx) * scale ) * 2 )

#define	EXTRAforPOLY	8

void cgr_inpoly ( char *syspt, int *npts, float *xpt, float *ypt, 
		  char *syspl, int *npls, float *xpl, float *ypl, 
		  int *inout, int *iret )
/************************************************************************
 * cgr_inpoly								*
 *									*
 * This function accepts a point(s) and polygon vertices and determines *
 * whether the point(s) is(are) internal or external to the polygon.	*
 *									*
 * THE POLYGON MAY BE OPEN OR CLOSED.					*
 *									*
 * cgr_inpoly ( syspt, npts, xpt, ypt, syspl, npls, xpl, ypl, 		*
 * 		inout, iret)						*
 *									*
 * Input parameters:							*
 *	*syspt	char		Input points coordinate system		*
 *	*npts	int		Number of points to test		*
 *	*xpt	float[]		X array  of test points			*
 *	*ypt	float[]		Y array  of test points			*
 *	*syspl char		Input points coordinate system		*
 *	*npls	int		Number of vertices in the polygon	*
 *	*xpl	float[] 	Polygon X vertices  			*
 *	*ypl	float[] 	Polygon Y vertices  			*
 *									*
 * Output parameters:							*
 *	*inout	int[]	Array of in or out results			*
 *  			0  =  Vertex lies outside of polygon		*
 *  			1  =  Vertex lies inside of polygon		*
 *	*iret	int	Return code					*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 6/97	Created					*
 * D.W.Plummer/NCEP	10/97	Updated for county watch		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*	
 * T. Piper/GSC		10/98	Prolog update				*
 * D.W.Plummer/NCEP	 2/99	Allow open polygons.			*
 * D.W.Plummer/NCEP	 6/99	Added edge checking & M coord scale	*
 * A. Hardy/GSC         11/00   renamed coord. system output device	*
 * D.W.Plummer/NCEP	 1/04	Added call to CGR_RANGE for cyl proj chk*
 * D.W.Plummer/NCEP	 4/04	Added 2nd parameter in call to G_FREE.	*
 * D.W.Plummer/NCEP	 6/05	Removed specific code for sys_M coords	*
 * 				which rounded to nearest 100th of degree*
 ***********************************************************************/
{
int 	i, ii, k, np, lt, npoly, ier, qpoly=G_TRUE, nout;
float	t, q;
int 	xmin, ymin, xmax, ymax;
int	*ixpt, *iypt, *ixpl, *iypl;
float	*xpt_L, *ypt_L, *xpl_L, *ypl_L;
float	*fx, *fy;
float   scale = RADIUS;
int	incrx[] = { 1, -2,  0,  2 };
int	incry[] = { 1, -2,  2, -2 };
int 	ir, roll;
float	xll, yll, xur, yur;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Temp space for points/polygon in sys_L coordinates.
     */
    np = G_MAX( *npts, *npls ) + 1 + EXTRAforPOLY;
    G_MALLOC ( xpt_L, float, np, "CGR_INPOLY - xpt_L" );
    G_MALLOC ( ypt_L, float, np, "CGR_INPOLY - ypt_L" );
    G_MALLOC ( xpl_L, float, np, "CGR_INPOLY - xpl_L" );
    G_MALLOC ( ypl_L, float, np, "CGR_INPOLY - ypl_L" );
    G_MALLOC (    fx, float, np, "CGR_INPOLY -    fx" );
    G_MALLOC (    fy, float, np, "CGR_INPOLY -    fy" );

    /*
     * Temp space for points/polygon in scaled, integer sys_N coordinates.
     */
    G_MALLOC ( ixpt, int, np, "CGR_INPOLY - ixpt" );
    G_MALLOC ( iypt, int, np, "CGR_INPOLY - iypt" );
    G_MALLOC ( ixpl, int, np, "CGR_INPOLY - ixpl" );
    G_MALLOC ( iypl, int, np, "CGR_INPOLY - iypl" );

    /*
     *  First transform to sys_L (linear) coordinates and check for
     *  wrap-around condition for cylindrical coordinates.
     */
    gtrans ( syspt, sys_L, npts, xpt, ypt, xpt_L, ypt_L, &ier,
	    strlen(syspt), strlen(sys_L) );

    cgr_range ( syspl, npls, xpl, ypl, &qpoly, sys_L, &roll, &nout, 
	   xpl_L, ypl_L, &xll, &yll, &xur, &yur, &ier );

    /*
     *  If 'roll' = 0, then proceed normally.
     *  If 'roll' > 0, then coordinate system is cylindrical and polygon
     *  spans the divide; must make 'roll' number of checks against
     *  the 'rolled out' polygon.
     */
    for ( ir = 0; ir <= roll; ir++ )  {

	if ( ir > 0 )  {
	    /*
	     * For subsequent repeats, add full-earth increment (TWOPI)
	     * to X-coordinate (longitude), and test again.
	     */
	    for ( ii = 0; ii < *npts; ii++ )  xpt_L[ii] += TWOPI;
	}

	/*
	 * Now convert from sys_L (linear) to sys_N (normalized) 
	 * coordinates and then scale.
	 */
    	gtrans( sys_L, sys_N, npts, xpt_L, ypt_L, fx, fy, &ier, 
		strlen(sys_L), strlen(sys_N) );
    	for ( i = 0; i < *npts; i++ )  {
		ixpt[i] = SC ( fx[i] );
		iypt[i] = SC ( fy[i] );
    	}

    	gtrans( sys_L, sys_N, npls, xpl_L, ypl_L, fx, fy, &ier, 
		strlen(sys_L), strlen(sys_N) );
    	for ( i = 0; i < *npls; i++ )  {
		ixpl[i] = SC ( fx[i] );
		iypl[i] = SC ( fy[i] );
    	}

        if ( ixpl[0] != ixpl[(*npls)-1] || iypl[0] != iypl[(*npls)-1] )  {
	    /*
	     *  Polygon open; close it and increase number of points.
	     */
	    ixpl[*npls] = ixpl[0];
	    iypl[*npls] = iypl[0];
	    npoly = (*npls) + 1;
        }
        else  {
	    /*
	     *  Polygon closed; set number of points to input.
	     */
	    npoly = *npls;
        }

        if ( ir == 0 )  {

          /*
           *  Initialize inout array and compute min/max bounds of polygon
           */
          for ( np = 0; np < *npts; np++ )  inout[np] = 0;
          xmin = INT_MAX; xmax = -INT_MAX;
          ymin = INT_MAX; ymax = -INT_MAX;
          for ( i = 0; i < npoly; i++ )  {
            xmin = G_MIN ( ixpl[i], xmin );
            ymin = G_MIN ( iypl[i], ymin );
            xmax = G_MAX ( ixpl[i], xmax );
            ymax = G_MAX ( iypl[i], ymax );
          }
          xmin = xmin - 1; xmax = xmax + 1;
          ymin = ymin - 1; ymax = ymax + 1;

        }

        /*
         *  This loop results in complete edge checking.  If edge checking
         *  does not matter, then this loop should only do k=1.
         */
        for ( k = 0; k < 4; k++ )  {

          for ( i = 0; i < npoly; i++ )  {
	    ixpl[i] = ixpl[i] + incrx[k];
	    iypl[i] = iypl[i] + incry[k];
          }

          for ( np = 0; np < *npts; np++ )  {

	    if ( inout[np] == 0 )  {

              if ( ixpt[np] < xmin || ixpt[np] > xmax || 
	           iypt[np] < ymin || iypt[np] > ymax )  {
		    inout[np] = 0;
	      }
	      else  {

                lt = 0;

                for ( i = 0 ; i < npoly-1 ; i++ ) {

	          if ( iypl[i] > iypt[np]  &&  iypl[i+1] > iypt[np] )  continue;
	          if ( iypl[i] < iypt[np]  &&  iypl[i+1] < iypt[np] )  continue;
	          if ( ixpl[i] < ixpt[np]  &&  ixpl[i+1] < ixpt[np] )  continue;

	          t = (float)(iypt[np] - iypl[i]) / (float)(iypl[i+1] - iypl[i]);

	          q = t * (float)(ixpl[i+1] - ixpl[i]) + (float)ixpl[i];

	          if ( (float)ixpt[np] < q )  lt++;

                }

	        inout[np] = !(lt%2 == 0);

              }

            }

          }

	}

    }

    /*
     * Free all the temporary space.
     */
    G_FREE ( iypl, int );
    G_FREE ( ixpl, int );
    G_FREE ( iypt, int );
    G_FREE ( ixpt, int );

    G_FREE ( fy, float );
    G_FREE ( fx, float );
    G_FREE ( ypl_L, float );
    G_FREE ( xpl_L, float );
    G_FREE ( ypt_L, float );
    G_FREE ( xpt_L, float );

}
