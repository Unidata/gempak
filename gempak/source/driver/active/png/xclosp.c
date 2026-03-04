#include "geminc.h"
#include "gemprm.h"
#include "xwcmn.h"


void xclosp ( int *ixsize, int *iysize, int *ncurwn, int *iret )
/************************************************************************
 * xclosp								*
 *									*
 * This subroutine closes the current window for the GF windows device	*
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
 * A. Hardy/GSC		 2/01	Copied from XW driver; modified to      *
 *                              write to file without leaving driver    *
 ***********************************************************************/
{
	Window		gwin; 
	GC		gemgc; 
	int		j;
	Window_str      *cwin;
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	if (gemwindow[current_window].name[0] != '\0') {
           /*
            *  Write the pixmaps to the PNG files.
            */
            cwin  = &(gemwindow[current_window]);
            for ( j = 0; j < cwin->npxms; j++ )
            {
                cwin->curpxm[cwin->curr_loop] = j;
                xwrpng ( iret );
            }

           /*
            *	Close the current window.
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
            *	Return the info about the current window.
            */
   	        *ixsize = gemwindow[current_window].width;
                *iysize = gemwindow[current_window].height;
	        *ncurwn = cwin->curpxm[cwin->curr_loop];

	}

}
