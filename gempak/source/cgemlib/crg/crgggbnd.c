#include "crgcmn.h"


void crg_ggbnd ( char grptyp, int grpnum, float *rleft, 
		float *rright, float *rtop, float *rbottom, int *iret )
/************************************************************************
 * crg_ggbnd                                                          	*
 *                                                                      *
 * This function returns the bounds to the range record of all 		*
 * the elements that belong to the specified group. 			*
 *                                                                      *
 * crg_ggbnd ( grptyp, grpnum, rleft, rright, rtop, rbottom, iret ) 	*
 *                                                                      *
 * Input parameters:                                                    *
 *	grptyp		char	Group type				*
 *	grpnum		int	Group number				*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*rleft		float	Left bound				*
 * 	*rright		float	Right bound				*
 * 	*rtop		float	Top bound				*
 * 	*rbottom	float	Bottom bound				*
 *      *iret           int     Return code                    		*
 *				=  0 - found element in group		*
 *			        = -1 - no element in group		*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		 4/98	Created					*
 * T. Piper/GSC		10/98	Prolog update				*
 ***********************************************************************/
{
int   ii, ifirst;
float rl, rr, rt, rb;
/*---------------------------------------------------------------------*/
    *iret = 0;
  
    ifirst = 1;

    for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++ ) {

	if ( range[ii].grptyp == grptyp &&
		range[ii].grpnum == grpnum ) {

	    if ( ifirst ) {

    		rl = range[ii].rleft;
    		rr = range[ii].rright;
    		rt = range[ii].rtop;
    		rb = range[ii].rbottom;

		ifirst = 0;
	    }
	    else {

	    	if (range[ii].rleft < rl)
		    rl = range[ii].rleft;

	    	if (range[ii].rright > rr)
		    rr = range[ii].rright;
 
	    	if (range[ii].rbottom < rb)
		    rb = range[ii].rbottom;

	    	if (range[ii].rtop > rt)
		    rt = range[ii].rtop;

	    }
	
	}
    }

    if ( ifirst ) { /* no element belong to the group */

	*rleft   = 0.0F;
	*rright  = 0.0F;
	*rtop    = 0.0F;
	*rbottom = 0.0F;

	*iret    = -1;
    }
    else {
	*rleft   = rl;
	*rright  = rr;
	*rtop    = rt;
	*rbottom = rb;
    }

}
