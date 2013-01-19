#include "xwcmn.h"

void xqdatt ( int *iunit, char *filnam, int *lenf, int *itype, 
		float *xsize, float *ysize, int *ncurwn, int *iret )
/************************************************************************
 * xqdatt								*
 *									*
 * This subroutine queries the device attributes.			*
 *									*
 * xqdatt ( iunit, filnam, lenf, itype, xsize, ysize, ncurwn, iret )	*
 *									*
 * Output parameters:							*
 *	*iunit		int		Type of output device		*
 *					  For XW:			*
 *					    1 = GEMPAK window		*
 *					    2 = Motif window		*
 *	*filnam		char		Name of output window		*
 *	*lenf		int		Length of window name		*
 *	*itype		int		Device type (color, bw, etc.)	*
 *	*xsize		float		X size in pixels		*
 *	*ysize		float		Y size in pixels		*
 *	*ncurwn		int		Current window number		*
 *	*iret		int		Return code			*
 *					  G_NORMAL = normal return	*
 *					  G_NIWNAM = invalid window name*
 **									*
 * Log:									*
 * S. Jacobs/NCEP        5/96  Copied from XSDATT			*
 * C. Lin/EAI	         6/97  Return pixmap size instead of window size*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 ***********************************************************************/
{
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    cwin   = &gemwindow[current_window];
    cloop  = &(cwin->loop[cwin->curr_loop]);

    *iunit  = kunit;

    strcpy ( filnam, cwin->name );
    *lenf   = strlen (cwin->name);

    *itype  = kctype;

    *xsize  = (float) cloop->pxm_wdth;
    *ysize  = (float) cloop->pxm_hght;

    *ncurwn = current_window;
}
