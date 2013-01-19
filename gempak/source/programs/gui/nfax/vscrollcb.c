#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"


extern Widget draw_area;
extern GC gc;      /* graphics context declaration */
extern Cardinal bitmapX;
extern Cardinal bitmapY;
extern int xorg;
extern int yorg;
extern Pixmap pmap;
extern int raster_up;

/* ARGSUSED */
void vvert_scrollCB ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * vvert_scrollCB							*
 *									*
 * This is the callback function for vertical scrolling in fax view	*
 * applications.							*
 *									*
 * void vvert_scrollCB ( w, clnt, call )				*
 *									*
 * Input parameters:							*
 *	 w		Widget		Widget activated the callback	*
 *	clnt		XtPointer	Data sent along with activation	*
 *	call		XtPointer	Info about the widget		*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	6/96		Created				*
 * T. Piper/GSC		3/01	Fixed IRIX6 compiler warnings		*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    Arg al[10];
    Cardinal ac;
    int value;
    ac  = 0;
    XtSetArg(al[ac], XmNvalue, &value); ac++;
    
    XtGetValues(w, al, ac);

    yorg = value;
 
    if (raster_up)
        vshowmap(bitmapX, bitmapY, xorg, yorg, gc, &pmap, draw_area);
}

/*=====================================================================*/
/* ARGSUSED */
void vhoriz_scrollCB ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * VHORIZ_SCROLLCB							*
 *									*
 * This is the callback function for horizontal scrolling in fax view	*
 * applications.							*
 *									*
 * VHORIZ_SCROLLCB  ( w, clnt, call )					*
 *									*
 * Input parameters:							*
 *	W		Widget		Widget activated the callback	*
 *	clnt		XtPointer	Data sent along with activation	*
 *	call		XtPointer	Info about the widget		*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	6/96		Created				*
 ***********************************************************************/
{
    Arg al[10];
    Cardinal ac;
    int value;
    ac  = 0;
    XtSetArg(al[ac], XmNvalue, &value); ac++;
    
    XtGetValues(w, al, ac);

    xorg = value;

    if (raster_up)
        vshowmap(bitmapX, bitmapY, xorg, yorg, gc, &pmap, draw_area);
}
