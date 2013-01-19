#include "xwcmn.h"

void xxflsh ( int *raise, int *iret )
/************************************************************************
 * xxflsh								*
 *									*
 * This subroutine flushes the buffers in the X window. It should only	*
 * be called from HEPLOT or its equivalent.				*
 *									*
 * xxflsh ( raise, iret )						*
 *									*
 * Input parameters:							*
 *	*raise		int		Raise flag 			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 4/96	Copied from XW driver; removed popping	*
 *				of window				*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		01/00	cwin->pixmaps -> cwin->pxms[curr_loop]	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    XFlush ( gemdisplay );

}
