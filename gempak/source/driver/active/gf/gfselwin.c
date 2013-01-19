#include "xwcmn.h"

void gfselwin ( char win_name[], int *len, float xsize, float ysize, 
				int *ixsize, int *iysize, int *iret )
/************************************************************************
 * gfselwin								*
 *									*
 * This program selects the window by the window name. If the input	*
 *	window name is NULL, the default window name will be used.	*
 *									*
 * gfselwin ( win_name, len, xsize, ysize, ixsize, iysize, iret )	*
 *									*
 * Input parameters:							*
 *	win_name[]	char		Window Name			*
 *	*len		int		length of the window name	*
 *      xsize		float		Right edge of window            *
 *      ysize		float		Bottom edge of window           *
 *									*
 * Output parameters:							*
 *      *ixsize		int		Right edge of window            *
 *      *iysize		int		Bottom edge of window           *
 *	*iret		int		Return code			*
 *			G_NORMAL = normal return (no size change)	*
 *			G_NWSIZE = new window size 			*
 *			G_NEWWIN = new window was openned		*
 *			G_NWINDW = maximum # of windows opend		*
 *			G_NIWNAM = invalid window name			*
 **									*
 * Log:									*
 * C. Lin/EAI	         7/94	Multi-window & Multi-pixmap		*
 * D. Himes/COMET        7/94	Set return codes and foreground color   *
 *                              when active window changes              *
 * C. Lin/EAI		 2/95   Error handling				*
 * M. Linda/GSC		 3/96	Removed DEFAULT_WNAME and related	*
 * C. Lin/EAI		 6/97   Return pixmap size instead of window    *
 *				size as device size			*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * T. Piper/SAIC	02/08	Copied from xw/xselwin; renamed		*
 ***********************************************************************/
{
    int		ii, ierr, itype;
    char	window_set;	/* flag: 0 = window not set, 1 = window set */

    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/
/*
 * Check the window name.
 */
    if ( ( *len > WNAME_LEN ) || ( *len <= 0 ) ) {
	*iret = G_NIWNAM;
	return;
    }

    win_name[*len] = '\0';

/*
 * Check if requested window is already open 
 * or if it already exists.
 */
    window_set = 0;
    for ( ii = 0; ii < MAX_WINDOW; ii++ ) {
	cwin   = &gemwindow[ii];
	cloop  = &(cwin->loop[cwin->curr_loop]);

	if ( strcmp (cwin->name, win_name ) == 0 ) {

/*
 * Requested window exists.
 */
	    *ixsize = cloop->pxm_wdth; 
	    *iysize = cloop->pxm_hght;
	    window_set = 1;

	    if ( ii == current_window ) {

/*
 * Requested window is the current window.
 */
		*iret = G_NORMAL;
		return;
	    }

/*
 * Requested window is another existing window.
 */
	    current_window = ii;
	    *iret = G_NWSIZE;
	    break;
	}
    }

    if ( window_set == 0 ) {

/*
 * Requested window does not exist.  Looking for an unused window.
 */
	for ( ii = 0; ii < MAX_WINDOW; ii++ ) {
	    if ( gemwindow[ii].name[0] == '\0' ) {
		gfopenw (win_name, ii, xsize, ysize, ixsize, iysize, &ierr);
		window_set = 1;
		current_window = ii;
		*iret = G_NEWWIN;
		break;
	    }
	}
    }

    if ( window_set == 0 ) {

/*
 *Unused window was not found.  Too many windows are open.
 */
	*iret = G_NWINDW;
    } else {

/*
 * Unused window was found.  Initializing the new window.
 */
	itype = ierr = 0;
	xscolr(&itype, &ifrcol, &ierr);   
    }

    return;
}
