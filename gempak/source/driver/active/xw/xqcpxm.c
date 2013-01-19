#include "xwcmn.h"

void xqcpxm ( int *npxms, int *curpxm )
/************************************************************************
 * xqcpxm								*
 *									*
 * This function querries and the number of pixmaps of current window   *
 * and the index of current pixmap.     				*
 *									*
 * xqcpxm( npxms, curpxm )						*
 *									*
 * Input/Output parameters:						*
 *	*npxms		int		# of pixmaps of current window	*
 *	*curpxm		int		index to the current pixmap	*
 *									*
 * Output parameters:							*
 *	None.								*
 **									*
 * Log:									*
 * C. Lin/EAI	         5/96						*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 ***********************************************************************/
{
    Window_str	*cwin;
/*---------------------------------------------------------------------*/

    cwin  = &(gemwindow[current_window]);

    *npxms   = cwin->npxms;
    *curpxm = cwin->curpxm[cwin->curr_loop];
}
