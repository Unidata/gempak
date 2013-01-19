#include "xwcmn.h"

void gfdatt ( int *iunit, char *filnam, int *lenf, int *itype, 
		float *xsize, float *ysize, int *ixsize, int *iysize, 
		int *isxsiz, int *isysiz, int *ixoff, int *iyoff, 
		int *ncurwn, int *iret )
/************************************************************************
 * gfdatt								*
 *									*
 * This subroutine opens the graphics window and sets the initial	*
 * graphics context along with basic window attributes.			*
 *									*
 * gfdatt ( iunit, filnam, lenf, itype, xsize, ysize, ixsize, iysize, 	*
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
 * A. Hardy/GSC		 2/01   Copied from the XW driver and added new *
 *                              file name check  			*
 * A. Hardy/GSC		 3/01   Added CHNULL to end of filnam		*
 * T. Piper/SAIC	02/08	Renamed from gf/xsdatt to gfdatt	*
 ***********************************************************************/
{
    Window_str	*cwin;
    winloop_t	*cloop;
    char	tmpfil[133];
    int         numwn;    
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    filnam[*lenf] = CHNULL;

/*
 * Set the global output and color scheme types.
 */
    kctype = *itype;
    kunit  = *iunit;

    if  ( *iunit != 1 ) {
	*iret = G_NEWWIN;
    }

/*
 *  Set the temporary filename.
 */
    strncpy ( tmpfil, gemwindow[current_window].name, *lenf );
    tmpfil[*lenf] = CHNULL;

/*
 *  If the passed in filename is different from the global filename,
 *  change the name after closing the old file.
 */

    if  ( strcmp ( filnam, tmpfil ) != 0 )
    {
        gfclosp ( ixsize, iysize, &numwn, iret );
    }

    gfselwin ( filnam, lenf, *xsize, *ysize, ixsize, iysize, iret );

    cwin   = &gemwindow[current_window];
    cloop  = &(cwin->loop[cwin->curr_loop]);

    *ncurwn = current_window;
    *isxsiz = cwin->width;
    *isysiz = cwin->height;
    *ixoff  = cloop->xoffset;
    *iyoff  = cloop->yoffset;

}
