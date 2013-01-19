#include "xwcmn.h"

void xslwin ( char winnam[], int *len, int *ixsize, int *iysize, 
		int *isxsiz, int *isysiz, int *ixo, int *iyo, 
		int *ncurwn, int *iret )
/************************************************************************
 * xslwin								*
 *									*
 * This program selects the current window by the window name. If the	*
 * input window name is NULL, the current window name will popped.	*
 *									*
 * xslwin ( winnam, len, ixsize, iysize, isxsiz, isysiz, ixo, iyo,	*
 *					ncurwn, iret )			*
 *									*
 * Input parameters:							*
 *	winnam[]	char		Window Name			*
 *	*len		int		length of the window name	*
 *									*
 * Output parameters:							*
 *      *ixsize		int		Pixmap width of window          *
 *      *iysize		int		Pixmap height of window         *
 *      *isxsiz		int		width of window          	*
 *      *isysiz		int		height of window         	*
 *      *ixo		int		window x offset 		*
 *      *iyo		int		window y offset			*
 *      *ncurwn		int		Current window number		*
 *	*iret		int		Return code			*
 *			G_NORMAL = normal return (no size change)	*
 *			G_NWSIZE = new window size 			*
 *			G_NEWWIN = new window was openned		*
 *			G_NWINDW = maximum # of windows opend		*
 *			G_NIWNAM = invalid window name			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 5/96	Copied from XSELWIN			*
 * C. Lin/EAI	 	 6/97	Return pixmap size instead of window	*
 *				add screen size and offset in calling   *
 *					sequence			*
 *				modified the logic			*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 ***********************************************************************/
{
    int		ii, done;

    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    /*
     * Preset the return code.
     */
    *iret = G_NIWNAM;

    /*
     * Check the window name length.
     */
    if ( ( *len > WNAME_LEN ) || ( *len < 0 ) ) return;

    /*
     * If the requested window name is blank return the current
     * window information.
     */
    if  ( *len == 0 ) {
	cwin  = &(gemwindow[current_window]); 
	cloop = &(cwin->loop[cwin->curr_loop]);

	strcpy ( winnam, cwin->name );
	*ixsize = cloop->pxm_wdth;
	*iysize = cloop->pxm_hght;
	*isxsiz = cwin->width;
	*isysiz = cwin->height;
	*ixo    = cloop->xoffset;
	*iyo    = cloop->yoffset;
	*ncurwn = current_window;
	*iret   = G_NORMAL;
	return;
    }

    winnam[*len] = '\0';

    /*
     * Check if requested window already exists.
     */
    done = G_FALSE;
    ii    = 0;
    while ( ( ii < MAX_WINDOW ) && ( ! done ) ) {
	if ( strcmp ( gemwindow[ii].name, winnam ) == 0 ) {

	    cwin  = &(gemwindow[ii]); 
	    cloop = &(cwin->loop[cwin->curr_loop]);

	    *ixsize = cloop->pxm_wdth; 
	    *iysize = cloop->pxm_hght;
	    *isxsiz = cwin->width;
	    *isysiz = cwin->height;
	    *ixo    = cloop->xoffset;
	    *iyo    = cloop->yoffset;
	    *ncurwn = ii;
	    done    = G_TRUE;

	    if ( ii == current_window ) {

		/*
		 * Requested window is the current window.
		 */
		*iret   = G_NORMAL;
	    }
	    else {

		/*
		 * Requested window is another existing window.
		 */
		current_window = ii;
		*iret = G_NWSIZE;
	    }
	}

	ii++;
    }
}
