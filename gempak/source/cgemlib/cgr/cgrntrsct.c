#include "geminc.h" 
#include "gemprm.h"

int cgr_ntrsct ( float allx, float ally, float aurx, float aury, 
		float bllx, float blly, float burx, float bury, int *iret )
/************************************************************************
 * cgr_ntrsct								*
 *									*
 * This function receives the corners of two bounding boxes and 	*
 * determines if these two bounding boxes intersect each other in 	*
 * any way.								*
 *									*
 * int cgr_ntrsct (allx, ally, aurx, aury, bllx, blly, burx, bury, iret)*
 *									*
 * Input parameters:							*
 *	allx	float	Lower left x-coordinate				*
 *	ally	float	Lower left y-coordinate				*
 *	aurx	float	Upper right x-coordinate			*
 *	aury	float	Upper right y-coordinate			*
 *	bllx	float	Lower left x-coordinate				*
 *	blly	float	Lower left y-coordinate				*
 *	burx	float	Upper right x-coordinate			*
 *	bury	float	Upper right y-coordinate			*
 *									*
 * Output parameters:							*
 *	*iret	   int 	Return code					*
 *	cgr_ntrsct int	Return code					*
 **									*
 * Log:									*
 * E.Wehner/EAi		11/96	Created					*
 * T. Piper/GSC		10/98	Prolog update				*
 ***********************************************************************/
{

    *iret = 0;

    /* A box lower left point is in B box */
    if (  ( (allx <= burx) && (allx >= bllx) ) && 
	   ( (ally <= bury) && (ally >= blly) ) )
    {
        return 1;
    }

    /* A box upper right point is in B box */
    if (  ( (aurx <= burx) && (aurx >= bllx) ) && 
	   ( (aury <= bury) && (aury >= blly) ) )
    {
        return 1;
    }

    /* A box upper left point is in B box */
    if (  ( (allx <= burx) && (allx >= bllx) ) && 
	   ( (aury <= bury) && (aury >= blly) ) )
    {
        return 1;
    }

    /* A box lower right point is in B box */
    if (  ( (aurx <= burx) && (aurx >= bllx) ) && 
	   ( (ally <= bury) && (ally >= blly) ) )
    {
        return 1;
    }


    /* A box lower left point is in B box */
    if (  ( (bllx <= aurx) && (bllx >= allx) ) && 
	   ( (blly <= aury) && (blly >= ally) ) )
    {
        return 1;
    }

    /* A box upper right point is in B box */
    if (  ( (burx <= aurx) && (burx >= allx) ) && 
	   ( (bury <= aury) && (bury >= ally) ) )
    {
        return 1;
    }

    /* A box upper left point is in B box */
    if (  ( (bllx <= aurx) && (bllx >= allx) ) && 
	   ( (bury <= aury) && (bury >= ally) ) )
    {
        return 1;
    }

    /* A box lower right point is in B box */
    if (  ( (burx <= aurx) && (burx >= allx) ) && 
	   ( (blly <= aury) && (blly >= ally) ) )
    {
        return 1;
    }


    if ( ((bury > aury) && (blly <= aury) ) &&
	  (( bllx <= aurx) && (burx >= allx) ) )
    {
	return 1;
    }

    if ( ((aury > bury) && (ally <= bury) ) &&
	  (( allx <= burx) && (aurx >= bllx) ) )
    {
	return 1;
    }


    return 0;
}
