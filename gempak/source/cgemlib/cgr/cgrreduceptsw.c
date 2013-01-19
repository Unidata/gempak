#include "geminc.h"
#include "gemprm.h"

/*
 *  Public functions
 */
void cgr_reduceptsw ( char *opts, int *nin, float *xin, 
		float *yin, int *reduceFlg, int *nout, float *xout, 
		float *yout, int *orig, int *iret )
/************************************************************************
 * cgr_reduceptsw							*
 *                                                                      *
 * This is a wrapper function for cgr_reducePts to remove points on a	*
 * polygon.  If the polygon is not closed, the results are undefined.	*
 * Points will be removed until the desired reduction has been achieved.*
 * Three algorithms are available and all of them are desogned to 	*
 * preserve the basic shape of the polygon as much as possible.		*  
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
 * cgr_reducepts ( opts, nin, xin, yin, reduceFlg, nout, xout, yout, 	*
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
 * T. Lee/SAIC		04/08	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;
    cgr_reducePts ( opts, *nin, xin, yin, reduceFlg, nout, xout,
		    yout, orig, iret );
}
