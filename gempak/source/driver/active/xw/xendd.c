#include "xwcmn.h"

void xendd ( int *iret )
/************************************************************************
 * xendd								*
 *									*
 * This subroutine closes the all the windows for the X windows device	*
 * driver.								*
 *									*
 * xendd ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91	C call for X device driver		*
 * M. desJardins/NMC	01/92	GEMPAK 5.1				*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	Multi-window, Multi-pixmap		*
 ***********************************************************************/
{
	Window		gwin; 
	GC		gemgc; 
	int		i;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	for ( i = 0; i < MAX_WINDOW; i++ ) {

	    if  ( gemwindow[i].name[0] != '\0' ) {

		gwin  = gemwindow[i].window;
		gemgc = gemwindow[i].gc;

		/*
 		 * Release the graphics context.
 		 */
		XFreeGC ( gemdisplay, gemgc );

		/*
 		 * Destroy the graphics window.
 		 */
		XDestroyWindow ( gemdisplay, gwin ); 

	    }

	}

	/*
 	 * Close the connection to the X server.
 	 */
	XCloseDisplay ( gemdisplay ); 

}
