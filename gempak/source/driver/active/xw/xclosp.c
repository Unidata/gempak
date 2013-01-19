#include "xwcmn.h"

void xclosp ( int *ixsize, int *iysize, int *ncurwn, int *iret )
/************************************************************************
 * xclosp								*
 *									*
 * This subroutine closes the current window for the X windows device	*
 * driver.								*
 *									*
 * xclosp ( ixsize, iysize, ncurwn, iret )				*
 *									*
 * Output parameters:							*
 *	*ixsize		int		X size in pixels		*
 *	*iysize		int		Y size in pixels		*
 *	*ncurwn		int		Current window number		*
 *	*iret		int		Return code			*
 *              			G_NORMAL     --- successful.    *
 **									*
 * Log:									*
 * S. Jacobs/NMC	 8/94	From xendd				*
 * C. Lin/EAI		 2/95	add break				*
 * S. Jacobs/NCEP	 4/96	Added ncurwn				*
 * S. Jacobs/NCEP	 4/96	Changed to allow closing of the 1st	*
 *				window, but not the only window		*
 ***********************************************************************/
{
	Window		gwin; 
	GC		gemgc; 
	int		i, knt;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Check the number of windows currently open. If there is only
 *	one, return with an error.
 */
	knt = 0;
	for ( i = 0; i < MAX_WINDOW; i++ ) {
	    if  ( gemwindow[i].name[0] != '\0' ) {
		knt++;
	    }
	}

	if  ( knt == 1 )  {
	    *iret = G_NOCLOS;
	    return;
	}

/*
 *	Close the selected window.
 */
	gwin  = gemwindow[current_window].window;
	gemgc = gemwindow[current_window].gc;

/*
 *	Release the graphics context.
 */
	XFreeGC ( gemdisplay, gemgc );

/*
 *	Destroy the graphics window.
 */
	XDestroyWindow ( gemdisplay, gwin ); 

	gemwindow[current_window].name[0] = '\0';

/*
 *	Choose the next current window.
 */
	current_window = 0;
	for ( i = 0; i < MAX_WINDOW; i++ ) {
	    if  ( gemwindow[i].name[0] != '\0' ) {
		current_window = i;
		break;
	    }
	}

/*
 *	Return the info about the current window.
 */
	*ixsize = gemwindow[current_window].width;
	*iysize = gemwindow[current_window].height;
	*ncurwn = current_window;

}
