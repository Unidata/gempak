#include "geminc.h"
#include "cpgcmn.h"

void vshowmap ( Cardinal xse, Cardinal yse, int xorig, int yorig, GC gc, 
						Pixmap *pmap, Widget wid )
/************************************************************************
 * vshowmap 								*
 *									*
 * This function displays a pixmap into an X window screen after the	*
 * screen has been cleared.						*
 *									*
 * void vshowmap (xse, yse, xorig, yorig, gc, pmap, wid )		*
 *									*
 * Input parameters:							*
 * 	xse	Cardinal	X coordinate size			*
 *	yse	Cardinal	Y coordinate size			*
 * 	xorig	int		X coordinate origin			*
 * 	yorig	int		Y coordinate origin			*
 *	gc	GC		Graphics Context			*
 * 	*pmap	Pixmap		Pixel map containing data for display	*
 *									*
 * Output parameters:							*
 *	wid	Widget		Widget to display pixmap in		*
 **									*
 * Log:									*
 *	E. Wehner/EAi	6/96	Created				 	*
 ***********************************************************************/
{
    XCopyArea(XtDisplay(wid), *pmap, XtWindow(wid), gc, xorig, 
		yorig, xse, yse, 1, 1);
}
