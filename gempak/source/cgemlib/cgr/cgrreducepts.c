#include "geminc.h"
#include "gemprm.h"

#define		ALG_ANGLE	(1)	/* Algorithm based on the angle */

#define		ALG_SIZE	(2)	/* Algorithm based on the amount of 
					       increase in size */
#define		ALG_PCT_DIST	(3)	/* Algorithm based on the pecentage of 
					       increase in size & the distance
					       to the original point */
#define		ALG_KEEP_CONCAV	(4)	/* Algorithm like ALG_PCT_DIST but
					       without the pre-selection of
					       concave areas */

float       	_radius = RADIUS*M2NM;  /* The Earth's radius in nautical miles*/

static	char	inputCoordSys[8];


/*
 *  Private functions
 */
static void _reducePts1 ( int reduceNum, int nin, float *xin, float *yin,
        	int *reduceFlg, int *nout, float *xout, float *yout, int *iret );

static void _reducePts2 ( int reduceNum, float reducePct, float reduceDst,
		int nin, float *xin, float *yin, int *reduceFlg, int *nout, 
		float *xout, float *yout, int *orig, int *iret );

static void _removeOnePt ( float reducePct, float reduceDst, 
	    int index, int nin, float *xin, float *yin,
            int *redFlg, int *flag, float *sdiff, float *xb, float *yb, 
	    float *xa, float *ya, int *iret );
	
static void _findReplacePts ( float maxDst, int index, int nin, float *xin,  
                 float *yin,  float *xb, float *yb, float *xa, 
		 float *ya, int *iret );

static void _reducePts3 ( float incrPct, float incrPctOrig, float incrDst, 
	        char *prefix, int nin, float *xin, float *yin, 
        	int *reduceFlg, int *nout, float *xout, float *yout, 
		int *orig, int *iret );

static void _reducePts4 ( float incrPct, float incrDst, char *prefix,
		int nin, float *xin, float *yin, 
        	int *reduceFlg, int *nout, float *xout, float *yout, 
		int *orig, int *iret );

/*
 *  Public functions
 */
void cgr_reducePts ( char *opts, int nin, float *xin, 
		float *yin, int *reduceFlg, int *nout, float *xout, 
		float *yout, int *orig, int *iret )
/************************************************************************
 * cgr_reducePts							*
 *                                                                      *
 * This function takes as input a closed polygon in device coordinates 	*
 * and the desired options for reduction. If the polygon is not closed,	*
 * the results are undefined.  Points will be removed until the desired	*
 * reduction has been achieved.	 Three algorithms are available	and all	*
 * of them are desogned to preserve the basic shape of the polygon as	*
 * much as possible.							*  
 *									*
 * The caller should choose from one of the three algorithms available 	*
 * and pass it through parameter "opts" using tag "alg_choice", e.g.	*
 * "<alg_choice>1</alg_choice>.						*
 *									*
 * ALG_ANGLE	(1)	- an algorithm based on the angle.  The one(s)	*
 *			  close to 180 degrees is (are) removed first.	*
 * ALG_SIZE	(2)	- an algorithm based on the amount of increase	*
 *			  in size. The one(s) resulting in the least 	*
 *			  size expansion is (are) removed first.	*
 * ALG_PCT_DIST	(3)	- an algorithm based on the pecentage of 	*
 *			  increase in size & the distance to the 	*
 *			  original point.  See _reducePts3 for details.	*
 *									*
 * Depending on the choice of algorithm, other reduction parameters	*
 * should be provided with the corresponding tags:			*
 *									*
 * "reduce_num" - number of points desired after reduction.  Required 	*
 *		for ALG_ANGLE and ALG_SIZE.				* 
 * "incr_pct" - maximum percentage allowed for the increase in size	*
 *		when a point is removed.  Required for ALG_PCT_DIST.	*
 * "incr_dst" - maximum distance allowed between a new point and the	*
 *		point to be removed.  Required for ALG_PCT_DIST.	*
 * "format_prefix"   - the prefix string used to determine if the points*
 *		can be represented on 3 lines of text (65 characters  	*
 *              each) The choice is "FROM" or "BOUNDED BY". Required for*
 *              ALG_PCT_DIST.						*
 *                                                                      *
 * Note: it is the caller's responsibilty to make sure the size of the	*
 *       output array is no less than the size of the input array.	*
 *                                                                      *
 * cgr_reducePts ( opts, nin, xin, yin, reduceFlg, nout, xout, yout, 	*
 * 		   orig, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *  *opts    	char    options for algorithm choice & reduction params	*
 *   nin    	int     number of input points				*
 *  *xin  	float	X-coordinates of input points			*
 *  *yin  	float	Y-coordinates of input points			*
 *  *reduceFlg	int     Flags to indicate if a point is reduce-able	*
 *                                                                      *
 * Output parameters:                                                   *
 *  *nout    	int     number of output points				*
 *  *xout  	float	X-coordinates of output points			*
 *  *yout  	float	Y-coordinates of output points			*
 *  *orig    	int     If a point is an original point			*
 *  *iret    	int    	return code					*
 *                      	 2 - partial success - still cannot be	*
 *                                   be represented on 3 lines but no	*
 *                                   reduction is allowed anymore based	*
 *                                   on the given criteria.		*
 *                      	 1 - reduce_num >= npts, no reduction	*
 *                      	-1 - bad reduceNum value (<3)		*
 *                      	-2 - bad npts value (<3)		*
 *                      	-3 - bad algorith choice (<=0 or >4)	*
 *                      	-4 - reduce_pct <= 0 or reduce_dst <= 0,*
 *                      	     no reduction possible.		*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		09/04	initial coding				*
 * J. Wu/SAIC		01/06	add algorithm to check size difference	*
 * J. Wu/SAIC		10/06	add new algorithm for size + dist check	*
 * J. Wu/SAIC		10/06	default reducePct & reduceDst to 0	*
 * E. Safford/SAIC	01/07	add 4th algorithm choice		*
 * J. Wu/SAIC		01/07	change reducePct & reduceDst to float	*
 * B. Yin/SAIC          04/07   skip incr_pct_orig for the 4th algorithm*
 * T. Lee/SAIC		06/08	use ALG_SIZE for warning polygons	*
 ***********************************************************************/
{
    int    	ii, algChoice, reduceNum, ier;
    char	tmpstr[256], value[32], redu_prefix[32];    
    float    	reducePct, reducePctOrig, reduceDst;
/*---------------------------------------------------------------------*/

    *iret = 0;
    
    /*
     *  Fill the output array with original inputs. 
     */
    *nout = nin;    

    for ( ii = 0; ii < *nout; ii++ ) {
	xout[ii] = xin[ii];
	yout[ii] = yin[ii];
        if ( orig != NULL ) orig[ii] = G_TRUE;
    }
    
    
    /*
     *  No reduction if total number of points < 3.
     */
    if ( nin < 3 ) {
        *iret = -2;
	return;
    }
        

    /*
     * Parse the option string to retrieve the algorithm choice,  the
     * desired number of points after reduction,  the maximum percentage
     * of size increase allowed,  and the maximum distance allowed for a
     * new point away from the point to be removed.
     */
    strcpy( inputCoordSys, sys_D );
    cst_gtag ( "coord_sys", opts, "", value, &ier );

    if( strlen( value ) > 0 ) {
	strcpy( inputCoordSys, value );
    }
    strcpy( value, "" );

    cst_uclc ( opts, tmpstr, &ier );
    cst_gtag ( "alg_choice", tmpstr, "", value, &ier );
    
    algChoice = atoi( value );
   


    /*
     *  No reduction if the algorithm does not exist.
     */
    if ( algChoice <= 0 || algChoice > ALG_KEEP_CONCAV ) {
        *iret = -3;
        return;
    }

    if ( algChoice == ALG_ANGLE || algChoice == ALG_SIZE ) {
	cst_gtag ( "reduce_num", tmpstr, "0", value, &ier );        
        reduceNum = atoi( value );        
        
	if ( reduceNum < 3 ) {
            *iret = -1;
	}
	else if ( reduceNum >= nin ) {
            *iret = 1;
        }

	if ( algChoice == ALG_SIZE ) {
            cst_gtag ( "incr_pct", tmpstr, "", value, &ier );
            cst_crnm ( value, &reducePct, &ier );

            if ( ier == 0 ) {
                cst_gtag ( "incr_dst", tmpstr, "", value, &ier );
                cst_crnm ( value, &reduceDst, &ier );
	    }
        }

        if ( ier != 0 ) {
            *iret = -4;
        }


    }
    else if ( algChoice == ALG_KEEP_CONCAV ) {
        cst_gtag ( "incr_pct", tmpstr, "", value, &ier );
        cst_crnm ( value, &reducePct, &ier );

        if ( ier == 0 ) {
            cst_gtag ( "incr_dst", tmpstr, "", value, &ier );
            cst_crnm ( value, &reduceDst, &ier );
        }

        if ( ier != 0 || reducePct <= 0.0F || reduceDst <= 0.0F ) {
            *iret = -4;
        }

        cst_gtag ( "format_prefix", tmpstr, "", value, &ier );
        cst_lcuc ( value, redu_prefix, &ier );
    }
    else {
	cst_gtag ( "incr_pct", tmpstr, "", value, &ier );        
        cst_crnm ( value, &reducePct, &ier );
        
	if ( ier == 0 ) {
	    cst_gtag ( "incr_dst", tmpstr, "", value, &ier );        
            cst_crnm ( value, &reduceDst, &ier );
	
	    if ( ier == 0 ) {
	        cst_gtag ( "incr_pct_orig", tmpstr, "", value, &ier );        
                cst_crnm ( value, &reducePctOrig, &ier );
            }
        }
        
        if ( ier != 0 || reducePct <= 0.0F || reduceDst <= 0.0F || reducePctOrig <= 0.0F ) {
            *iret = -4;	    
	}

	cst_gtag ( "format_prefix", tmpstr, "", value, &ier );        
        cst_lcuc ( value, redu_prefix, &ier );
    }
        

    /*
     *  No reduction for any bad inputs for reduction parameters.
     */
    if ( *iret != 0 ) {
        return;
    }
                        
    
    /*
     *  Reduce points using the chosen algorithm.
     */
    if ( algChoice == ALG_ANGLE ) {
        _reducePts1 ( reduceNum, nin, xin, yin, reduceFlg,
        	      nout, xout, yout, iret );
    }    
    else if ( algChoice == ALG_SIZE ) {
	_reducePts2 ( reduceNum, reducePct,  reduceDst,
		      nin, xin, yin, reduceFlg, 
	              nout, xout, yout, orig, iret );    
    }
    else if ( algChoice == ALG_PCT_DIST ) {        
        _reducePts3 ( reducePct, reducePctOrig, reduceDst, redu_prefix, 
		      nin, xin, yin, reduceFlg, 
        	      nout, xout, yout, orig, iret );
    }
    else {        
        _reducePts4 ( reducePct, reduceDst, redu_prefix, 
		      nin, xin, yin, reduceFlg, 
        	      nout, xout, yout, orig, iret );
    }       
}

/*=====================================================================*/

static void _reducePts1 ( int reduceNum, int nin, float *xin, float *yin,
        	int *reduceFlg, int *nout, float *xout, float *yout, int *iret )
/************************************************************************
 * _reducePts1								*
 *                                                                      *
 * This function examines the angle at each point. The one(s) closest to*
 * 180 degree is(are) removed, if the reduceFlg is either NULL or TRUE  *
 * for that point.                                       		*
 *                                                                      *
 * cgr_reducePts ( reduceNum, nin, xin, yin, reduceFlg, 		*
 *					nout, xout, yout, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *   reduceNum	int     desired number of points after reduction	*
 *   nin    	int     number of input points				*
 *  *xin  	float	X-coordinates of input points			*
 *  *yin  	float	Y-coordinates of input points			*
 *  *reduceFlg	int	reduce flags for each point or NULL		*
 *                                                                      *
 * Output parameters:                                                   *
 *  *nout    	int     number of output points				*
 *  *xout  	float	X-coordinates of output points			*
 *  *yout  	float	Y-coordinates of output points			*
 *  *iret    	int    	return code					*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		09/04	initial coding				*
 * J. Wu/SAIC		01/06	remove conversion of radian to degree	*
 * E. Safford/SAIC	10/06	add reduceFlg param; make point removal	*
 *				 conditional based on reduceFlg 	*
 ***********************************************************************/
{
    int    	ii, mindangindx;
    float	dang, mindang;
    double	ang1, ang2;
    Boolean	done, anyReduceable;
/*---------------------------------------------------------------------*/

    *iret = 0;
    done  = FALSE; 
        
    /*
     *  Reduce points by examining the angle at each point. 
     *  The one closest to 180 degree is removed. Repeat until
     *  the desired reduced point has been achived. 
     */
    *nout = nin;
    while ( *nout > reduceNum && !done )  {

        mindang = PI;
      
	anyReduceable = FALSE;  

	for ( ii = 0; ii < *nout; ii++ )  {

	    if ( reduceFlg == (int *)NULL || reduceFlg[ii] == G_TRUE ) {

		anyReduceable = TRUE;
		
	        if ( ii == (*nout-1) )  {
	            ang1 = atan2( (double)(yout[0]-yout[ii]), 
		                  (double)(xout[0]-xout[ii]) );
	        }
	        else  {
	            ang1 = atan2( (double)(yout[ii+1]-yout[ii]), 
		                  (double)(xout[ii+1]-xout[ii]) );
	        }
	    
	        if ( ii == 0 )  {
	            ang2 = atan2( (double)(yout[*nout-1]-yout[ii]), 
		                  (double)(xout[*nout-1]-xout[ii]) );
	        }
	        else  {
	            ang2 = atan2( (double)(yout[ii-1]-yout[ii]), 
		       	          (double)(xout[ii-1]-xout[ii]) );
	        }


	        dang = G_ABS( PI - G_ABS((float)(ang1-ang2)) );
	  
	        if ( dang < mindang )  {
	            mindang = G_MIN ( mindang, dang );
	            mindangindx = ii;
	        }

	    }

        }

	if ( !anyReduceable ) {
	    done = TRUE;
	}
       
	if( !done ) { 
            for ( ii = mindangindx; ii < (*nout-1); ii++ )  {
	        xout[ii] = xout[ii+1];
	        yout[ii] = yout[ii+1];
            }
	
	    (*nout) -= 1;
        }
    }

}

/*=====================================================================*/

static void _reducePts2 ( int reduceNum, float reducePct, float reduceDst,
	int nin, float *xin, float *yin, int *reduceFlg, int *nout, 
	float *xout, float *yout, int *orig, int *iret )
/************************************************************************
 * _reducePts2								*
 *                                                                      *
 * This function examines the size difference between polygons before 	*
 * and after the removal of a point.  The one resulting in the least 	*
 * expansion of the polygon will be removed until the desired reduction *
 * has been achived.  Note that	the removal of a point should be done 	*
 * in a way that always results in the expansion of the polygon.	* 
 *                                                                      *
 * _reducePts2 ( reduceNum, nin, xin, yin, reduceFlg, nout, xout, yout,	*
 *               orig, ret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *   reduceNum	int     desired number of points after reduction	*
 *   nin    	int     number of input points				*
 *  *xin  	float	X-coordinates of input points			*
 *  *yin  	float	Y-coordinates of input points			*
 *  *reduceFlg	int     Flags to indicate if a point is reduce-able	*
 *                                                                      *
 * Output parameters:                                                   *
 *  *nout    	int     number of output points				*
 *  *xout  	float	X-coordinates of output points			*
 *  *yout  	float	Y-coordinates of output points			*
 *  *orig    	int     If a point is an original point			*
 *  *iret    	int    	return code					*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		01/06	initial coding				*
 * T. Lee/SAIC		06/08	used for warning polygons		*
 ***********************************************************************/
{
    int    	ii, rflag, ptRemove, ptFlag, done, ia, ib, ier;
    float	sizeDiff, xa, ya, xb, yb;
    float	sdf, xaf, yaf, xbf, ybf;
/*---------------------------------------------------------------------*/

    *iret = 0;
    
    /*
     *  Reduce points by examining the size difference of the polygons
     *  before and after the removal of a point. The one resulting in 
     *  the least size expansion is removed. Repeat until the desired
     *  the desired reduction has been achived.
     *
     *  The point will be removed as following:
     *  
     *  a. if the point P lies inside of the polygon, simply remove it.
     *  b. if the point P lies outside of the polygon, the points 
     *     immediately before (Pb) and after (Pa) it will be adjusted 
     *     include P. 
     */   
    *nout = nin;    
    done = G_FALSE;	
        
    while ( done == G_FALSE && *nout > reduceNum )  {
			     	
	/*
	  *  Find the point to be removed.
	  */
	sizeDiff = 1.0e10;
	ptRemove = -1;
	ptFlag   = -1;
	for ( ii = 0; ii < *nout; ii++ ) {	    
	    if ( reduceFlg == (int *)NULL || reduceFlg[ii] == G_TRUE ) {
		_removeOnePt ( 	reducePct, reduceDst, ii, *nout,
				xout, yout, reduceFlg, &rflag, 
	                       	&sdf, &xbf, &ybf, &xaf, &yaf, &ier);
	    
	        if ( rflag >= 0 && sdf < sizeDiff ) {
		    sizeDiff = sdf;
		    ptRemove = ii; 
		    ptFlag   = rflag; 
		        
		    if ( rflag == 1 ) {
			xb = xbf;
			yb = ybf;
			xa = xaf;
			ya = yaf;    
		    }        
	        }
            }
	}   
	
	/*
	  *  Remove the point.
	  */
	if ( ptRemove < 0 ) {
	    done = G_TRUE;	    
	}
	else {
            	    	    
	    if ( ptFlag == 1 ) {
	        ib = ( ( ptRemove - 1 ) + (*nout) ) % (*nout);
		ia = ( ptRemove + 1) % (*nout);

		xout[ib] = xb;
	        yout[ib] = yb;	        
	        xout[ia] = xa;
	        yout[ia] = ya;	        
	        
		if ( orig != NULL ) {
		    orig[ib] = G_FALSE;
		    orig[ia] = G_FALSE;
		}
	    }
	    	    	    	    
	    for ( ii = ptRemove; ii < (*nout-1); ii++ )  {
	        xout[ii] = xout[ii+1];
	        yout[ii] = yout[ii+1];
	        
		if ( reduceFlg != NULL ) reduceFlg[ii] = reduceFlg[ii+1];
		
		if ( orig != NULL ) orig[ii] = orig[ii+1];
	    }        	     	    
	    
	    (*nout) -= 1;	    
	}		    	
    }  /* End of while loop */    
}

/*=====================================================================*/

static void _removeOnePt ( float reducePct, float reduceDst,
                   int index, int nin, float *xin, float *yin,
                   int *redFlg, int *rmflag, float *sdiff, float *xb, 
	           float *yb, float *xa, float *ya, int *iret )
/************************************************************************
 * _removeOnePt								*
 *                                                                      *
 * This function finds the size difference between polygons before and	*
 * after the removal of a given point P.  If P is outside of the polygon*
 * without P,  then the replacement points for the points before and	*
 * after P are calculated.  The removal of a point should not increase	*
 * the size of polygon by "reducePct" and the replacement points should *
 * be within "reduceDst" of the origianl points				*
 *                                                                      *
 *  _removeOnePt ( reducePct, reduceDst, index, nin, xin, yin, rmflag,	*
 *                 sdiff, xb, yb, xa, ya, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *   reducePct	float   maximum increase in size allowed		*
 *   reduceDst	float   maximum distance allowed away from the polygon	*
 *   index    	int     index of the point P to be removed		*
 *   nin    	int     number of input points				*
 *  *xin  	float	X-coordinates of input points			*
 *  *yin  	float	Y-coordinates of input points			*
 *  *redFlg    	int     Flags to indicate if a point is reduce-able	*
 *                                                                      *
 * Output parameters:                                                   *
 *  *rmflag  	int	flag to indicate how to remove the point P	*
 *                       0 - P is inside of the original polygon	*
 *                       1 - P is outside & replacement points available*
 *                      -1 - P is outside & no replacement points avail.*
 *                      -2 - size increase exceeds reducePct		*
 *  *sdiff  	float	size difference between polygons with/without P	*
 *  *xb  	float	X-coord of the point to replace point before P	*
 *  *yb 	float	Y-coord of the point to replace point before P	*
 *  *xa  	float	X-coord of the point to replace point after P	*
 *  *ya  	float	Y-coord of the point to replace point after P	*
 *  *iret    	int    	return code					*
 *                       0 - normal                               	*
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		01/06	initial coding				*
 * D.W.Plummer/NCEP	07/06	Add G_ABS to sdiff calculation		*
 * J. Wu/SAIC		10/06	limit size increase and distance	*
 * J. Wu/SAIC		10/06	allow 0% size increase			*
 * J. Wu/SAIC		01/07	change reducePct & reduceDst to float	*
 * D.W.Plummer/NCEP	04/07	snap new convex pts prior to size calc	*
 ***********************************************************************/
{
    int    	ii, np, one, inout[1], fa, fb, ib, ia, ier;
    float	*xtmp, *ytmp, oldArea, newArea, xct, yct;    
    float	*lats, *lons, snapLat, snapLon;
/*---------------------------------------------------------------------*/

    *iret = 0;
    *sdiff = 1.0e10;
    *rmflag = -1;
    *xb = RMISSD;
    *yb = RMISSD;
    *xa = RMISSD;
    *ya = RMISSD;
    
    np = nin - 1;
    
    G_MALLOC ( xtmp, float, nin, "_removeOnePt: xtmp" );
    G_MALLOC ( ytmp, float, nin, "_removeOnePt: ytmp" );    
    
    /*
     *  First, remove point "index".
     */
    for ( ii = 0; ii < index; ii++ ) {
        xtmp[ii] = xin[ii];
	ytmp[ii] = yin[ii];
    }
    
    for ( ii = index; ii < np; ii++ ) {
        xtmp[ii] = xin[ii + 1];
	ytmp[ii] = yin[ii + 1];
    }
	    
    /*
     *  Check point "index" is inside or outside of the new polygon.
     */
    one = 1;
    cgr_inpoly ( sys_D, &one, &xin[index], &yin[index], 
		 sys_D, &np, xtmp, ytmp, inout, &ier );
	                    	        

    /*
     *  If point "index" is outside, replace the point before and after 
     *  the point "index" - if both of them are reduce-able AND the
     *  replacement points are available.
     */
    ib = ( (index - 1) + np ) % np;
    ia = index % np;
        
    fb = ( (index - 1) + nin ) % (nin);
    fa = (index + 1) % nin;

    if ( inout[0] == G_TRUE ) {
        *rmflag = 0; 
    }
    else if ( redFlg == (int *)NULL || ( redFlg[fb] && redFlg[fa] ) ) {	
	
	_findReplacePts ( reduceDst, index, nin, xin, yin, 
	                  xb, yb, xa, ya, &ier );	
     					
	if ( ier == 0 ) {
	    xtmp [ ib ] = *xb;
	    ytmp [ ib ] = *yb;
	    xtmp [ ia ] = *xa;	        
            ytmp [ ia ] = *ya;	        

	    G_MALLOC ( lats, float, np, "_removeOnePt : Error allocating lats" );
	    G_MALLOC ( lons, float, np, "_removeOnePt : Error allocating lons" );

	    gtrans ( sys_D, sys_M, &np, xtmp, ytmp, lats, lons, &ier, 
			strlen(sys_D), strlen(sys_M) );

	    clo_snapPt ( ib, lats[ib], lons[ib], ib, np, lats, lons, G_TRUE, 0.0, 
			&snapLat, &snapLon, &ier );
	    lats[ib] = snapLat;
	    lons[ib] = snapLon;

	    clo_snapPt ( ia, lats[ia], lons[ia], ia, np, lats, lons, G_TRUE, 0.0, 
			&snapLat, &snapLon, &ier );
	    lats[ia] = snapLat;
	    lons[ia] = snapLon;

	    gtrans ( sys_M, sys_D, &np, lats, lons, xtmp, ytmp, &ier, 
			strlen(sys_M), strlen(sys_D) );

	    G_FREE ( lats, float );
	    G_FREE ( lons, float );
	        
	    *rmflag = 1;
	}
    }
	    
    /*
     *   Now find the size difference between the new polygon and  
     *   the original one.
     */
    if ( *rmflag >= 0 ) {

        cgr_centroid ( xin, yin, &nin, &xct, &yct, &oldArea, &ier );
	
        cgr_centroid ( xtmp, ytmp, &np, &xct, &yct, &newArea, &ier );
	
	*sdiff = G_ABS ( newArea - oldArea );
        	
	if ( reducePct >= 0.0F && (*sdiff/oldArea * 100) > reducePct ) {
	    *rmflag = -2;      
	}        	
    }
    
    G_FREE ( xtmp, float );
    G_FREE ( ytmp, float );    
}

/*=====================================================================*/

static void _findReplacePts ( float maxDst, int index, int nin, float *xin,  
                 float *yin,  float *xb, float *yb, float *xa, 
		 float *ya, int *iret )
/************************************************************************
 * _findReplacePts							*
 *                                                                      *
 * This function finds the replacement points for a given point P in a 	*
 * closed polygon.  The replacement points are found by intersecting	*
 * segments PaaPa and PbbPb with the line parallel to segment PaPb and 	*
 * through point P (Pa is the point after P and Pb is the point before 	*
 * P.  Paa is the point after Pa & Pbb is the point before Pb. 		*
 *                                                    			*
 * _findReplacePts ( index, nin, xin, yin, xb, yb, xa, ya, iret)	*
 *                                                                      *
 * Input parameters:                                                    *
 *   maxDst	float   maximum distance allowed away from the polygon 	*
 *   index    	int     index of the point to be removed		*
 *   nin    	int     number of input points				*
 *  *xin  	float	X-coordinates of input points			*
 *  *yin  	float	Y-coordinates of input points			*
 *                                                                      *
 * Output parameters:                                                   *
 *  *xb  	float	X-coord of the replacement point for Pb		*
 *  *yb 	float	Y-coord of the replacement point for Pb		*
 *  *xa  	float	X-coord of the replacement point for Pa		*
 *  *ya  	float	Y-coord of the replacement point for Pa		*
 *  *iret    	int    	return code					*
 *                      	 0 - normal				*
 *                      	-1 - no replacement points found	*
 *                      	-2 - replacement points found but the	* 
 *                                   distance is greater than maxDst	*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		01/06	initial coding				*
 * J. Wu/SAIC		10/06	limit the distance between the new 	*
 *                              new points and the original points	*
 * J. Wu/SAIC		01/07	change maxDSt to float			*
 ***********************************************************************/
{
    int    	ier, ia, iaa, ib, ibb, intrsct;
    int		one = 1;
    float	xp[2], yp[2], xpa[2], ypa[2], xpb[2], ypb[2]; 
    float	xm, ym, xn, yn; 
    float	latP1[1], lonP1[1], latP2[1], lonP2[1], dist[1]; 
/*---------------------------------------------------------------------*/

    *iret = 0;
    	
    /*
     *  Definition:
     *    P   - point to be eliminated
     *    Pb  - point before P;    Pbb - point before Pb
     *    Pa  - point after P;     Paa - point after Pa
     *    L   - line through P and parallel to line Pb-Pa 
     *    M   - intersetion point of line Pb-Pbb with P-Pa
     *    N   - intersetion point of line Pa-Paa with P-Pb
     *    O1  - intersetion point of line Pb-Pbb with line L 
     *    O2  - intersetion point of line Pa-Paa with line L 
     * 
     *    After replacing Pb with O1 and Pa with O2, the area covered
     *    by triangle Pb-P-Pa must be fully contained within the 
     *    trapezoid Pbb-O1-O2-Paa. SO M cannot falls on segment P-Pa 
     *    AND N cannot falls on segment P-Pb. 
     * 
     *    Note: Do not judge the intersection from the value of 
     *    "intrsct" here since we are using two random points to
     *    represent a line.  If two segments intersect after 
     *    extended as lines, cgr_segint will return the intersection
     *    point but set "intrsct" as false since the point is not 
     *    within two segment's bounds. 
     */	        			    
	        
    /*
     * Segment Pb-Pbb
     */
    ib  = ((index - 1) + nin) % nin;
    ibb = ((index - 2) + nin) % nin;
    
    xpb[0] = xin[ib];
    ypb[0] = yin[ib];
		
    xpb[1] = xin[ibb];
    ypb[1] = yin[ibb];

    
    /*
     * Segment Paa-Pa
     */
    ia  = (index + 1) % nin;
    iaa = (index + 2) % nin;
	
    xpa[0] = xin[ia];
    ypa[0] = yin[ia];
		
    xpa[1] = xin[iaa];
    ypa[1] = yin[iaa];
                
		
    /* 
     *   M - intersection of Pb-Pbb with P-Pa - falls on P-Pa?
     */
    xp[0] = xin[ index ];
    yp[0] = yin[ index ];
    
    xp[1] = xin[ia];  
    yp[1] = yin[ia];  
    cgr_segint ( sys_D, xpb, ypb, sys_D, xp, yp, 
		 sys_D, &xm, &ym, &intrsct, &ier );		
           
    if ( (xm - RMISSD) > 0.0001 )  { /* intersect */
	
	if ( xm >= G_MIN(xp[0], xp[1]) && xm <= G_MAX(xp[0], xp[1]) &&
             ym >= G_MIN(yp[0], yp[1]) && ym <= G_MAX(yp[0], yp[1]) ) {
	
	    *iret = -1;
	    return;
        }
    }
             
    
    /* 
     *   N - intersection of Pa-Paa with P-Pb - falls on P-Pb?
     */
    xp[1] = xin[ib];  
    yp[1] = yin[ib];  
    cgr_segint ( sys_D, xpa, ypa, sys_D, xp, yp, 
		 sys_D, &xn, &yn, &intrsct, &ier );		
         
    if ( (xn - RMISSD) > 0.0001 )  {  /* intersect */
	
	if ( xn >= G_MIN(xp[0], xp[1]) && xn <= G_MAX(xp[0], xp[1]) &&
             yn >= G_MIN(yp[0], yp[1]) && yn <= G_MAX(yp[0], yp[1]) ) {
             	
	    *iret = -1;
	    return;
        }
    }
        
    
    /*
     *   O1 - intersection of Pb-Pbb with line L 
     *   O2 - intersection of Pa-Paa with line L 
     */	        			
    xp[1] = 0.0;
    yp[1] = yp[0] - ( ( yin[ia] - yin[ib] ) /
		      ( xin[ia] - xin[ib] ) ) * xp[0];
			        		    
    cgr_segint ( sys_D, xp, yp, sys_D, xpb, ypb, 
		 sys_D, xb, yb, &intrsct, &ier );		
    
    if ( (*xb -RMISSD) < 0.0001 ) {    /* no intersection */
	*iret = -1;
	 return;
    }
    
    cgr_segint ( sys_D, xp, yp, sys_D, xpa, ypa, 
		 sys_D, xa, ya, &intrsct, &ier );

    if (  (*xa -RMISSD) < 0.0001 ) {   /* no intersection */
	*iret = -1;
	 return;
    }

    /*
     *   Pb->O1 and Pa->O2 should not exceed maxDst. 
     */	        			
    if ( maxDst > 0 ) {
        
        gtrans ( inputCoordSys, sys_M, &one, xb, yb, 
		 latP1, lonP1, &ier, strlen(sys_D), strlen(sys_M) );
        gtrans ( inputCoordSys, sys_M, &one, xpb, ypb, 
		 latP2, lonP2, &ier, strlen(sys_D), strlen(sys_M) );
		 
        clo_dist ( &latP1[0], &lonP1[0], &one, latP2, lonP2, dist, &ier );
	        
	if ( ier != 0 || (dist[0] * M2NM ) > maxDst ) {
	    *iret = -2;
	    return;
	}
	
	gtrans ( inputCoordSys, sys_M, &one, xa, ya, 
		 latP1, lonP1, &ier, strlen(sys_D), strlen(sys_M) );
        gtrans ( inputCoordSys, sys_M, &one, xpa, ypa, 
		 latP2, lonP2, &ier, strlen(sys_D), strlen(sys_M) );
		 
        clo_dist ( &latP1[0], &lonP1[0], &one, latP2, lonP2, dist, &ier );
	
	if ( ier != 0 || (dist[0] * M2NM ) > maxDst ) {
	    *iret = -2;
	}
    }
}

/*=====================================================================*/
static void _reducePts3 ( float incrPct, float incrPctOrig, float incrDst, 
	        char *prefix, int nin, float *xin, float *yin, 
        	int *reduceFlg, int *nout, float *xout, float *yout, 
		int *orig, int *iret )
/************************************************************************
 * _reducePts3								*
 *                                                                      *
 * This routine reduces the number of points in a polygon to allow it to*
 * be represented on three 65-character lines of text.	It tries to 	*
 * remove allowable points, one at a time, based on the impact their 	*
 * individule removal would have on the size of the polygon.  		*
 * Specifically, remove points that increase the size of the polygon the*
 * least,  while not increasing the overall size of the polygon 	*
 * by "incrPct" (per individual point removal), not increasing the 	*
 * overall size of the polygon by "incrPctOrig" (per cumulative point 	*
 * removal compared with the original size of the polygon), and not 	*
 * allowing any new points to be "incrDst" distance from the original 	*
 * polygon points. "reducePct" refers to the areal percentage increase 	*
 * when a single point is removed from the polygon. Point reduction 	*
 * continues until the polygon can be represented on three 65-character	*
 * lines of text, or no more points can be removed under the above 	*
 * criteria.								*
 *                                                                      *
 * _reducePts3 ( reducePct, reducePctOrig, reduceDst, nin, xin, yin, 	*
 * 		 reduceFlg, nout, xout, yout, orig, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *   incrPct	float   maximum pct of size increase allowed		*
 *   incrPctOrig float  maximum pct of overall size increase allowed	*
 *   incrDst	float   maximum distance allowed away from the polygon 	*
 *  *prefix	char    prefix string - "FROM" or "BOUNDED BY" 		*
 *   nin    	int     number of input points				*
 *  *xin  	float	X-coordinates of input points			*
 *  *yin  	float	Y-coordinates of input points			*
 *  *reduceFlg	int     Flags to indicate if a point is reduce-able	*
 *                                                                      *
 * Output parameters:                                                   *
 *  *nout    	int     number of output points				*
 *  *xout  	float	X-coordinates of output points			*
 *  *yout  	float	Y-coordinates of output points			*
 *  *orig    	int     If a point is an original point			*
 *  *iret    	int    	return code					*
 *			0 - normal, points can be formatted on 3 lines	*
 *			2 - points cannot be formatted on 3 lines but no*
 *			    reduction possible based on given criteria	*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/06	initial coding				*
 * D.W.Plummer/NCEP	12/06	Accept concave points immediately	*
 * J. Wu/SAIC		01/07	change incrPct & incrDst to float	*
 * D.W.Plummer/NCEP	03/07	Rm concave preference, etc.		*
 ***********************************************************************/
{
    int    	ii, *rflag, ptRemove, ptFlag, done, ia, ib, ier;
    int		np, canBeFormatted;
    float	sizeDiff, sumSizeDiff, origArea, xa, ya, xb, yb;
    float	*sdf, *xaf, *yaf, *xbf, *ybf;
    float	*xlat, *ylon, slat, slon, xct, yct;
/*---------------------------------------------------------------------*/

    *iret = 0;
   
    /*
     *  Reduce points by examining the size difference of the polygons
     *  before and after the removal of a point. The one resulting in 
     *  the least size expansion is removed while the maximum percentage of
     *  size increase before and after the removal of a point cannot exceed. 
     *  "incrPct".  Repeat until the desired reduction has been achieved.  
     *
     *  The point will be removed as following:
     *  
     *  a. if the point P lies inside of the polygon, simply remove it.
     *  b. if the point P lies outside of the polygon, the points 
     *     immediately before (Pb) and after (Pa) it will be adjusted 
     *     to include P. But a new point can not be "incrDst" away 
     *	   from the point to be replaced.
     */   
    *nout = nin;    
    done = G_FALSE;	
        
    np = nin;
    G_MALLOC (  xlat, float, np + 1, "_reducePts3:  xlat" );
    G_MALLOC (  ylon, float, np + 1, "_reducePts3:  ylon" );
    G_MALLOC ( rflag,   int, np + 1, "_reducePts3: rflag" );
    G_MALLOC (   sdf, float, np + 1, "_reducePts3:   sdf" );
    G_MALLOC (   xaf, float, np + 1, "_reducePts3:   xaf" );
    G_MALLOC (   yaf, float, np + 1, "_reducePts3:   yaf" );
    G_MALLOC (   xbf, float, np + 1, "_reducePts3:   xbf" );
    G_MALLOC (   ybf, float, np + 1, "_reducePts3:   ybf" );

    gtrans ( inputCoordSys, sys_M, &np, xout, yout, xlat, ylon,
                 &ier, strlen(inputCoordSys), strlen(sys_M) );
    
    canBeFormatted = cgr_canBeFormatted ( np, xlat, ylon, prefix );     

    if ( canBeFormatted == G_FALSE )  {

      /*
       * Compute area of original polygon using D coordinates
       */
      cgr_centroid ( xout, yout, &np, &xct, &yct, &origArea, &ier );
        
      /*
       * Compute the potentials for all points.
       */
      for ( ii = 0; ii < *nout; ii++ ) {	    
	orig[ii] = G_TRUE;
	if ( reduceFlg == (int *)NULL || reduceFlg[ii] == G_TRUE ) {
	     _removeOnePt ( incrPct, incrDst, ii, *nout, xout, yout, reduceFlg, 
		&rflag[ii], &sdf[ii], &xbf[ii], &ybf[ii], &xaf[ii], &yaf[ii], &ier);
        }
      }

      sumSizeDiff = 0;
      while ( done == G_FALSE && canBeFormatted == G_FALSE )  {
			     	
	/*
	 *  Find the point to be removed.
	 */
	sizeDiff = 1.0e10;
	ptRemove = -1;
	ptFlag   = -1;
	for ( ii = 0; ii < *nout; ii++ ) {	    
	    if ( reduceFlg == (int *)NULL || reduceFlg[ii] == G_TRUE ) {
	        if ( rflag[ii] >= 0 && sdf[ii] < sizeDiff ) {
		    /*
		     * Also check here to see if removing this point results in an 
		     * expansion beyond the specified polygon by 3000 sq nm.
		     */
	/*
	 * Check to see if any 'spillage' outside of a given area has occurred. We must not
	 * allow any part of the total 'spillage' to exceed 3K sq nm. 
	 * (This area may be input via the function (...); if NULL then bypass this step altogether.)
	 *
	 * Algorithm:
	 *
	 * If overall increase to the original polygon is greater than 3K sq nm, 
	 * then start doing some checking for spillage...
	 * If no 'spillage', then we can accumulate another total increase of
	 * 3K sq nm before having to perform this check again. 
	 * If 'spillage' has occurred (possibly in several areas), then we must start checking the total
	 * increase against the maximum 'spillage' size.
	 * Never perform these expensive checks unless we have to!
	 * If a point has been determined to increase the 'spillage' too much, then
	 * that point must somehow be marked to not be considered in future possible removals.
	 */

		    sizeDiff = sdf[ii];
		    ptRemove = ii; 
		    ptFlag   = rflag[ii]; 
		    if ( rflag[ii] == 1 ) {
			xb = xbf[ii];
			yb = ybf[ii];
			xa = xaf[ii];
			ya = yaf[ii];
		    }        
	        }
            }
	}   

	/*
	 * Compute overall size increase. Units are square D coord pixels.
	 */
        sumSizeDiff += sizeDiff;
	
	/*
	 * Check if the total size differential exceeds that requested 
	 * (in terms of percentage of original polygon size).
	 */
	if ( ptRemove < 0 || ( 100*sumSizeDiff/origArea >= incrPctOrig ) ) {
	    done = G_TRUE;	    
	}
	else {

	    /*
	     * Remove point.
	     *
	     * If convex removal, adjust points on either side with new values.
	     */
	    if ( ptFlag == 1 ) {
	        ib = ( ( ptRemove - 1 ) + (*nout) ) % (*nout);
		ia = ( ptRemove + 1) % (*nout);

		xout[ib] = xb;
	        yout[ib] = yb;	        
	        xout[ia] = xa;
	        yout[ia] = ya;	        
	        
		if ( orig != NULL ) {
		    orig[ib] = G_FALSE;
		    orig[ia] = G_FALSE;
		}			        
	    }

	    /*
	     * Shift all the arrays one position starting with the removed point.
	     */
	    for ( ii = ptRemove; ii < (*nout-1); ii++ )  {
	        xout[ii] = xout[ii+1];
	        yout[ii] = yout[ii+1];
	        xlat[ii] = xlat[ii+1];
	        ylon[ii] = ylon[ii+1];
	        rflag[ii] = rflag[ii+1];
	        sdf[ii] = sdf[ii+1];
	        xbf[ii] = xbf[ii+1];
	        ybf[ii] = ybf[ii+1];
	        xaf[ii] = xaf[ii+1];
	        yaf[ii] = yaf[ii+1];
	        
		if ( reduceFlg != NULL ) reduceFlg[ii] = reduceFlg[ii+1];
		
		if ( orig != NULL ) orig[ii] = orig[ii+1];
	    }        	     	    

	    (*nout) -= 1;	    
	    ptRemove = ( ptRemove + (*nout) ) % (*nout);

	    /*
	     *  New points should be snapped so cgr_canBeFormatted can
	     *  check on the same FROM line as in af_fmt2xml.
	     *  Only need to do this if convex removal.
	     */
	    if ( ptFlag == 1 )  {
	        np = 1;
	        ii = ptRemove;
                gtrans ( inputCoordSys, sys_M, &np, 
		    &(xout[ii]), &(yout[ii]), &(xlat[ii]), &(ylon[ii]),
                    &ier, strlen(inputCoordSys), strlen(sys_M) );
	        ii = ( ( ptRemove - 1 ) + (*nout) ) % (*nout);
                gtrans ( inputCoordSys, sys_M, &np, 
		    &(xout[ii]), &(yout[ii]), &(xlat[ii]), &(ylon[ii]),
                    &ier, strlen(inputCoordSys), strlen(sys_M) );

	        ii = ptRemove;
	        if ( orig != (int *)NULL && orig[ ii ] == G_FALSE && 
	             reduceFlg != (int *)NULL && reduceFlg[ ii ] == G_TRUE ) {

	            clo_snapPt( ii, xlat[ ii ], ylon[ ii ], ii, *nout, 
			    xlat, ylon, True, 3.0F, &slat, &slon, &ier );
	            xlat[ ii ] = slat;
	            ylon[ ii ] = slon;
	        }
	        ii = ( ( ptRemove - 1 ) + (*nout) ) % (*nout);
	        if ( orig != (int *)NULL && orig[ ii ] == G_FALSE && 
	             reduceFlg != (int *)NULL && reduceFlg[ ii ] == G_TRUE ) {

	            clo_snapPt( ii, xlat[ ii ], ylon[ ii ], ii, *nout, 
			    xlat, ylon, True, 3.0F, &slat, &slon, &ier );
	            xlat[ ii ] = slat;
	            ylon[ ii ] = slon;
	        }
	    }
        
	    /*
	     *  Check if the new polygon can be formatted on 3 lines.
	     */
	    np = *nout;
	    canBeFormatted = cgr_canBeFormatted ( np, xlat, ylon, prefix );     
       	
	    if ( canBeFormatted == G_FALSE )  {

	        /*
	         * Calculate the new potentials for the adjacent points.
	         * The two adjacent points need to be recalculated for all removals,
	         * however for convex removals the four adjacent points need to
	         * be recalculated (2 on either side).
	         */
	        ii = ( ( ptRemove - 1 ) + (*nout) ) % (*nout);
	        if ( reduceFlg == (int *)NULL || reduceFlg[ii] == G_TRUE ) {
	            _removeOnePt ( incrPct, incrDst, ii, *nout, xout, yout, reduceFlg, 
			    &rflag[ii], &sdf[ii], &xbf[ii], &ybf[ii], &xaf[ii], &yaf[ii], &ier);
                }
	        ii = ptRemove;
	        if ( reduceFlg == (int *)NULL || reduceFlg[ii] == G_TRUE ) {
	            _removeOnePt ( incrPct, incrDst, ii, *nout, xout, yout, reduceFlg, 
			    &rflag[ii], &sdf[ii], &xbf[ii], &ybf[ii], &xaf[ii], &yaf[ii], &ier);
                }
	        if ( ptFlag == 1 ) {
	            ii = ( ( ptRemove - 2 ) + (*nout) ) % (*nout);
	            if ( reduceFlg == (int *)NULL || reduceFlg[ii] == G_TRUE ) {
	                _removeOnePt ( incrPct, incrDst, ii, *nout, xout, yout, reduceFlg, 
			    &rflag[ii], &sdf[ii], &xbf[ii], &ybf[ii], &xaf[ii], &yaf[ii], &ier);
                    }
	            ii = ( ( ptRemove + 1 ) + (*nout) ) % (*nout);
	            if ( reduceFlg == (int *)NULL || reduceFlg[ii] == G_TRUE ) {
	                _removeOnePt ( incrPct, incrDst, ii, *nout, xout, yout, reduceFlg, 
			    &rflag[ii], &sdf[ii], &xbf[ii], &ybf[ii], &xaf[ii], &yaf[ii], &ier);
                    }
	        }
	    }
	}
      }  /* End of while loop */ 
    }
        
    
    
    G_FREE (  xlat, float );
    G_FREE (  ylon, float );
    G_FREE ( rflag,   int );
    G_FREE (   sdf, float );
    G_FREE (   xaf, float );
    G_FREE (   yaf, float );
    G_FREE (   xbf, float );
    G_FREE (   ybf, float );
    
    /*
     *   Flag a partial success - points still cannot be represented on 3 lines     
     *   of text but no reduction possible based on the given criteria.
     */
    if ( canBeFormatted == G_FALSE ) {
        *iret = 2;
    }   

}

/*=====================================================================*/

static void _reducePts4 ( float incrPct, float incrDst, char *prefix,
		int nin, float *xin, float *yin, 
        	int *reduceFlg, int *nout, float *xout, float *yout, 
		int *orig, int *iret )
/************************************************************************
 * _reducePts4								*
 *                                                                      *
 * This routine reduces the number of points in a polygon to allow it to*
 * be represented on three 65-character lines of text.	It tries to 	*
 * remove allowable points, one at a time, based on the impact their 	*
 * individule removal would have on the size of the polygon.  		*
 * Specifically, remove points that increase the size of the polygon the*
 * least,  while not increasing the overall size of the polygon 	*
 * by "incrPct" and not allowing any new points to be "incrDst" 	*
 * distance from the original polygon points. "reducePct" refers to the *
 * areal percentage increase when a single point is removed from the 	*
 * polygon. Point reduction continues until the polygon can be 		*
 * represented on three 65-character lines of text, or no more points 	*
 * can be removed under the above criteria.				*
 *                                                                      *
 * _reducePts4 ( reducePct, reduceDst, nin, xin, yin, reduceFlg, 	*
 *               nout, xout, yout, orig, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *   incrPct	float     maximum percentage of size increase allowed	*
 *   incrDst	float     maximum distance allowed away from the polygon*
 *  *prefix	char    prefix string - "FROM" or "BOUNDED BY" 		*
 *   nin    	int     number of input points				*
 *  *xin  	float	X-coordinates of input points			*
 *  *yin  	float	Y-coordinates of input points			*
 *  *reduceFlg	int     Flags to indicate if a point is reduce-able	*
 *                                                                      *
 * Output parameters:                                                   *
 *  *nout    	int     number of output points				*
 *  *xout  	float	X-coordinates of output points			*
 *  *yout  	float	Y-coordinates of output points			*
 *  *orig    	int     If a point is an original point			*
 *  *iret    	int    	return code					*
 *			0 - normal, points can be formatted on 3 lines	*
 *			2 - points cannot be formatted on 3 lines but no*
 *			    reduction possible based on given criteria	*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	01/07	copied _reducePts3, removed concave 	*
 *				  preference				*
 * J. Wu/SAIC		01/07	change incrPct & incrDst to float	*
 ***********************************************************************/
{
    int    	ii, jj, rflag, ptRemove, ptFlag, done, ia, ib, ier;
    int		np, canBeFormatted;
    float	sizeDiff, xa, ya, xb, yb;
    float	sdf, xaf, yaf, xbf, ybf;
    float	*xlat, *ylon, slat, slon;
/*---------------------------------------------------------------------*/

    *iret = 0;
   
    /*
     *  Reduce points by examining the size difference of the polygons
     *  before and after the removal of a point. The one resulting in 
     *  the least size expansion is removed while the maximum percentage of
     *  size increase before and after the removal of a point cannot exceed. 
     *  "incrPct".  Repeat until the desired reduction has been achived.  
     *
     *  The point will be removed as following:
     *  
     *  a. if the point P lies inside of the polygon, simply remove it.
     *  b. if the point P lies outside of the polygon, the points 
     *     immediately before (Pb) and after (Pa) it will be adjusted 
     *     to include P. But a new point can not be "incrDst" away 
     *	   from the point to be replaced.
     */   
    *nout = nin;    
    done = G_FALSE;	
        
    np = nin;
    G_MALLOC ( xlat, float, np + 1, "_reducePts3: xlat" );
    G_MALLOC ( ylon, float, np + 1, "_reducePts3: ylon" );

    gtrans ( inputCoordSys, sys_M, &np, xout, yout, xlat, ylon,
                 &ier, strlen(inputCoordSys), strlen(sys_M) );
    
    canBeFormatted = cgr_canBeFormatted ( np, xlat, ylon, prefix );     

    while ( done == G_FALSE && canBeFormatted == G_FALSE )  {
			     	
	/*
	 *  Find the point to be removed.
	 */
	sizeDiff = 1.0e10;
	ptRemove = -1;
	ptFlag   = -1;
	for ( ii = 0; ii < *nout; ii++ ) {	    
	    if ( reduceFlg == (int *)NULL || reduceFlg[ii] == G_TRUE ) {
	        _removeOnePt ( incrPct, incrDst, 
		               ii, *nout, xout, yout, reduceFlg, &rflag, 
	                       &sdf, &xbf, &ybf, &xaf, &yaf, &ier);
	 
	        if ( rflag >= 0 && sdf < sizeDiff ) {
		    sizeDiff = sdf;
		    ptRemove = ii; 
		    ptFlag   = rflag; 
		        
		    if ( rflag == 1 ) {
			xb = xbf;
			yb = ybf;
			xa = xaf;
			ya = yaf;    
		    }        
	        }
            }
	}   
	
	/*
	  *  Remove the point.
	  */
	if ( ptRemove < 0 ) {
	    done = G_TRUE;	    
	}
	else {
            	    	    
	    if ( ptFlag == 1 ) {
	        ib = ( ( ptRemove - 1 ) + (*nout) ) % (*nout);
		ia = ( ptRemove + 1) % (*nout);

		xout[ib] = xb;
	        yout[ib] = yb;	        
	        xout[ia] = xa;
	        yout[ia] = ya;	        
	        
		if ( orig != NULL ) {
		    orig[ib] = G_FALSE;
		    orig[ia] = G_FALSE;
		}			        
	    }
	    	    	    	    
	    for ( ii = ptRemove; ii < (*nout-1); ii++ )  {
	        xout[ii] = xout[ii+1];
	        yout[ii] = yout[ii+1];
	        
		if ( reduceFlg != NULL ) reduceFlg[ii] = reduceFlg[ii+1];
		
		if ( orig != NULL ) orig[ii] = orig[ii+1];
	    }        	     	    
	    
	    (*nout) -= 1;	    
	}		    	
	
	
	/*
	 *  Check if the new polygon can be formatted on 3 lines.
	 *  New points should be snapped so cgr_canBeFormatted can
	 *  check on the same FROM line as in af_fmt2xml.
	 */
	np = *nout;
        gtrans ( inputCoordSys, sys_M, &np, xout, yout, xlat, ylon,
                 &ier, strlen(inputCoordSys), strlen(sys_M) );
        
	for ( jj = 0; jj < np; jj++ ) {
	    if ( orig != (int *)NULL && orig[ jj ] == G_FALSE && 
		 reduceFlg != (int *)NULL && reduceFlg[ jj ] == G_TRUE ) {
		    
		clo_snapPt( jj, xlat[ jj ], ylon[ jj ], jj, np, 
			        xlat, ylon, True, 3.0F, 
				&slat, &slon, &ier );
		    
		xlat[ jj ] = slat;
		ylon[ jj ] = slon;
	    }
	}
    		
	canBeFormatted = cgr_canBeFormatted ( np, xlat, ylon, prefix );     
       	
    }  /* End of while loop */ 
    
    
    G_FREE ( xlat, float );
    G_FREE ( ylon, float );
    
    
    /*
     *   Flag a partial success - points still cannot be represented on 3 lines     
     *   of text but no reduction possible based on the given criteria.
     */
    if ( canBeFormatted == G_FALSE ) {
        *iret = 2;
    }   
   

}

/*=====================================================================*/
