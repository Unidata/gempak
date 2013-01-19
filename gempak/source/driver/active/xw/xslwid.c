#include "xwcmn.h"

void xslwid ( int *ilwid, int *iret )
/************************************************************************
 * xslwid								*
 *									*
 * This subroutine sets the hardware line width for the XW driver.	*
 *									*
 * xslwid ( ilwid, iret )						*
 *									*
 * Input parameters:							*
 *	*ilwid		int		Line width			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91	C call for XW device driver		*
 * M. desJardins/NMC	12/91	GEMPAK 5.1				*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	multi-window, multi-pixmap		*
 * S. Jacobs/NCEP	10/97	Changed cap_style from CapButt to	*
 *				CapRound				*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	/*
 	 * Check for valid line width.
 	 */

	if  ( *ilwid < 0 )  return;

	/*
 	 * Set the line attributes.
 	 */

	line_width = *ilwid;

	line_style = LineSolid;
	cap_style  = CapRound;
	join_style = JoinRound;

	XSetLineAttributes ( gemdisplay, 
			gemwindow[current_window].gc, 
			line_width, line_style,
			cap_style, join_style ); 

}
