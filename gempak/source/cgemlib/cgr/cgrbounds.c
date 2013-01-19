#include "geminc.h"
#include "gemprm.h"

int cgr_bounds ( int ptx, int pty, int spanx, int spany, int ileft, 
					int irght, int ibot, int itop )
/************************************************************************
 * cgr_bounds								*
 *									*
 * This function determines whether a point falls within the boundary	*
 * of a box that is defined by the input parameters.			*
 *									*
 * int cgr_bounds ( ptx, pty, spanx, spany, ileft, irght, ibot, itop )	*
 *									*
 * Input parameters:							*
 *	ptx		int		X of point to check		*
 *	pty		int		Y of point to check		*
 *	spanx		int		Direction of X span		*
 *	spany		int		Direction of Y span		*
 *	ileft		int		Left margin			*
 *	irght		int		Right margin			*
 *	ibot		int		Bottom margin			*
 *	itop		int		Top margin			*
 *									*
 * Output parameters:							*
 *	cgr_bounds	int		Return value			*
 *					  1 = in bounds			*
 *					  2 = out of bounds		*
 **									*
 * Log:									*
 * E. Wehner/EAI	10/96	Created					*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * E. Safford/GSC	10/00	removed cgr.h include			*
 ***********************************************************************/
{
    if ( ( (spanx * (ptx-ileft) ) >= 0) &&
	 ( (spanx * (irght - ptx)) >= 0) &&
	 ( (spany * (pty - ibot)) >= 0) &&
	 ( (spany * (itop - pty)) >= 0)
	)
    {
        return 1;
    }
    else
    {
        return 0;
    }

}
