#include "xwcmn.h"

void xsdatt ( int *iunit, char *filnam, int *lenf, int *itype, 
		float *xsize, float *ysize, int *ixsize, int *iysize, 
		int *isxsiz, int *isysiz, int *ixoff, int *iyoff, 
		int *ncurwn, int *iret )
/************************************************************************
 * xsdatt								*
 *									*
 * This subroutine opens the graphics window and sets the initial	*
 * graphics context along with basic window attributes.			*
 *									*
 * xsdatt ( iunit, filnam, lenf, itype, xsize, ysize, ixsize, iysize, 	*
 *		isxsiz, isysiz, ixoff, iyoff, ncurwn, iret )		*
 *									*
 * Input parameters:							*
 *	*iunit		int		Type of output device		*
 *					  For XW:			*
 *					    1 = GEMPAK window		*
 *					    2 = Motif window		*
 *	*filnam		char		Name of output window		*
 *	*lenf		int		Length of window name		*
 *	*itype		int		Device type (color, bw, etc.)	*
 *	*xsize		float		X size in pixels		*
 *	*ysize		float		Y size in pixels		*
 *									*
 * Output parameters:							*
 *	*ixsize		int		device X size in pixels		*
 *	*iysize		int		device Y size in pixels		*
 *      *isxsiz         int             screen X size in pixels         *
 *      *isysiz         int             screen Y size in pixels         *
 *      *ixoff          int             screen X offset         	*
 *      *iyoff          int             screen Y offset         	*
 *	*ncurwn		int		Current window number		*
 *	*iret		int		Return code			*
 *					  G_NORMAL = normal return	*
 *					  G_NIWNAM = invalid window name*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/96						*
 * S. Jacobs/NCEP	 4/96	Added check for iunit			*
 * S. Jacobs/NCEP	 5/96	Added new global variables for queries	*
 * C. Lin/EAI            6/97   Added screen size and offset in calling *
 *				sequence               			*
 *                              set the size info when iunit = 2        *
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 ***********************************************************************/
{
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * Set the global output and color scheme types.
     */
    kctype = *itype;
    kunit  = *iunit;

    if  ( *iunit != 1 ) {
	*iret = G_NEWWIN;
    }

    xselwin ( filnam, lenf, *xsize, *ysize, ixsize, iysize, iret );

    cwin   = &gemwindow[current_window];
    cloop  = &(cwin->loop[cwin->curr_loop]);

    *ncurwn = current_window;
    *isxsiz = cwin->width;
    *isysiz = cwin->height;
    *ixoff  = cloop->xoffset;
    *iyoff  = cloop->yoffset;
}
