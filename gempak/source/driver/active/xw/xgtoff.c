#include "xwcmn.h"

void xgtoff ( int *xoff, int *yoff, int *iret)
/************************************************************************
 * xgtoff								*
 *									*
 * This subroutine gets the current X and Y offsets.			*
 *									*
 * xgtoff (xoff, yoff, iret)						*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*xoff 		int 		X offset			*
 *	*yoff 		int		Y offset			*
 *	*iret		int		Return code			*
 *					 G_NORMAL = what could go wrong?*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		06/00	initial coding				*
 ***********************************************************************/
{
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    cwin  = &(gemwindow[current_window]);
    cloop = &(cwin->loop[cwin->curr_loop]);

    *xoff = cloop->xoffset; 
    *yoff = cloop->yoffset; 
}
