#include "xwcmn.h"

void xstanm ( int *iret )
/************************************************************************
 * xstanm								*
 *									*
 * This subroutine sets up the pixmap pointer to the beginning of the	*
 * 	pixmap array and stops animation when necessary.		*
 *									*
 * xstanm  ( iret )							*
 *									*
 * Input parameters:							*
 *			None						*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * C. Lin/EAI	         7/94	Multi-window & Multi-pixmap		*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	added pixmap cleanup			*
 * E. Safford/GSC	08/00	undo cleanup				*
 ***********************************************************************/
{
    Window_str	*cwin;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    cwin  = &(gemwindow[current_window]);

    /* 
     * if it is in the looping status, 
     * stop the loop 
     */

    if ( LOOP ) LOOP = 0;

    /*
     * starts counting the number of pixmaps to be created
     */

    cwin->npxms = 1;

    /*
     * sets the pointer to the beginning
     */

    cwin->curpxm[cwin->curr_loop] = 0;

}
