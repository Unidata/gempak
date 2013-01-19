#include "xwcmn.h"

void xsincr ( Boolean incr_value )
/************************************************************************
 * xsincr 								*
 *									*
 *  This routine sets the incr_pxmCnt flag to the input value.  A value *
 *  of TRUE (which is the initial value for the flag) allows the number *
 *  of pixmaps and current pixmap counters to be incremented on calls   *
 *  to xsplot.  FALSE blocks xsplot from incrementing those counters.   *
 *									*
 * xsincr   ( incr_value )						*
 *									*
 * Input parameters:							*
 *	incr_value	Boolean		new value for incr_pxmCnt flag  *
 *									*
 * Output parameters:							*
 *	NONE								*
 **									*
 * Log:									*
 * E. Safford/GSC	02/99	initial coding                    	*
 ***********************************************************************/
{
    gemwindow[current_window].incr_pxmCnt = incr_value;
}
