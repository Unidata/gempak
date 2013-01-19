/************************************************************************
*       drawingw.c                                                 	*
*                                                                  	*
*   Module to create the drawing area widget			   	*
*                                                                  	*
*   Log:                                                           	*
*   Chien Lin/EAI      02/93                                       	*
*   S. Wang/GSC		1/97  add new functions                    	*
*   S. Wang/GSC		3/97  Corrected resizing problem	   	*
*   I. Durham/GSC	5/98  Changed call for underscore	   	*
*   J. Wu/GSC		5/01  free XmString	   			*
************************************************************************/
#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "Nxm.h"
#include "xwcmn.h"
#define GEM_COLORS 33


extern	Widget	colrbar_frame, model_toplevel;
int	resize_flag;


void Expose_Callback	( Widget, XtPointer, XmDrawingAreaCallbackStruct* );
void getColorByPointer	( Widget, XtPointer, XEvent* );
void Resize_Callback	( Widget, XtPointer, XmDrawingAreaCallbackStruct* );
void Reload_OK_Callback ( void );
void setPointerFocus	( Widget, XtPointer, XEvent* );
unsigned long GetColorFromWindow ( Drawable win, int x, int y );

/***********************************************************************/


void create_drawingW ( Widget parent )
/************************************************************************
 *	create_drawingW()						*
 *									*
 *      This subroutione creates the drawing area			*
 *									*
 *      Input parameters:						*
 *      parent  Wiget           parent widget id			*
 *									*
 *	C Lin/EAI							*
 ***********************************************************************/
{

        DrawingW = XtVaCreateManagedWidget("drawing_widget",
                               	xmDrawingAreaWidgetClass, parent,
				XmNtopAttachment,   XmATTACH_WIDGET,
				XmNtopWidget,       legend_frame,
				XmNbottomAttachment,XmATTACH_WIDGET,
				XmNbottomWidget,    colrbar_frame,
				XmNleftAttachment,  XmATTACH_FORM,
				XmNrightAttachment, XmATTACH_FORM,
				XmNresizable,       TRUE,
				NULL);

        XtAddCallback(DrawingW,XmNexposeCallback, (XtCallbackProc)Expose_Callback, NULL);
        XtAddCallback(DrawingW,XmNresizeCallback, (XtCallbackProc)Resize_Callback, NULL);

        XtAddEventHandler(DrawingW, ButtonReleaseMask, FALSE ,
                (XtEventHandler)getColorByPointer, NULL);

        XtAddEventHandler(DrawingW, EnterWindowMask, FALSE ,
                (XtEventHandler)setPointerFocus, NULL);

}

/*=========================================================================*/
/* ARGSUSED */
void Expose_Callback ( Widget w, XtPointer clnt, 
				XmDrawingAreaCallbackStruct *call )
/************************************************************************
 *	Expose_Callback()						*
 *									*
 *      This function takes care of expose event			*
 *									*
 *	S. Wang/GSC	01/97						*
 * E. Safford/GSC	08/00	update for new xwcmn.h			*
 ***********************************************************************/
{
XEvent	*event;
/*---------------------------------------------------------------------*/

	event = call->event;
	
	if (event->xexpose.count == 0 )
		xmexpo(event);

}

/*=====================================================================*/
/* ARGSUSED */
void Resize_Callback ( Widget drawing_a, XtPointer clnt, 
					XmDrawingAreaCallbackStruct *cbs )
/************************************************************************
* T. Piper/SAIC         07/03   replaced gemdisplay with XtDisplay()    *
************************************************************************/
{
Arg             args[10];
Cardinal	argcnt;
int             iret;
Widget          message, button;
Dimension       current_width;
Dimension       current_height;
XmString	xmstr;

/*------------------------------------------------------------------------*/

/*      get current window size         */

        XtVaGetValues(DrawingW, 
			XmNwidth,  &current_width,
			XmNheight, &current_height,
			NULL);

	XClearArea(XtDisplay(DrawingW), cbs->window,
                                0, 0, 0, 0, False);

        XCopyArea (XtDisplay(DrawingW),
                   gemwindow[0].pxms[0][0], XtWindow(DrawingW),
                        gemwindow[0].gc, 0, 0, 
			(Cardinal)current_width,
			(Cardinal)current_height, 0, 0);

        XFlush(XtDisplay(DrawingW));

        if (load_flag && GroupLoadFlag) {
        	argcnt = 0;
        	xmstr = XmStringCreateLocalized("Reload to the new size window?");
		XtSetArg(args[argcnt], XmNmessageString, xmstr);
		argcnt++;
		
                XtSetArg(args[argcnt], XmNnoResize, True);
                argcnt++;
                XtSetArg(args[argcnt], XmNdialogStyle,
                        XmDIALOG_SYSTEM_MODAL);
                argcnt++;

        	message = XmCreateQuestionDialog(drawing_a, "Reload ", 
						args, argcnt);
        	
		XtAddCallback(message, XmNokCallback, (XtCallbackProc)Reload_OK_Callback, NULL);

        	button = XmMessageBoxGetChild(message, XmDIALOG_HELP_BUTTON);

        	XmStringFree( xmstr );		
        	XtManageChild(message);
        	XtUnmanageChild(button);

        }
	else {
		gclear(&iret);
	}

        if (load_flag && ViewFrame) {
		gclear(&iret);
                PixmapData.old_pixmap_no = 1;
                load_frame(ViewFrame);
	}

}

/*=====================================================================*/

void Reload_OK_Callback ( void )
{
        reload_group();
}
	
/*=====================================================================*/
/* ARGSUSED */
void getColorByPointer ( Widget w, XtPointer clnt, XEvent *call )
/************************************************************************
 * T. Piper/SAIC	07/03	Added xqnclr				*
 ***********************************************************************/
{
int           cbank=GraphCid, ii, iret, ncolors, x, y, indx;
XEvent        colorevent;
unsigned long color;
/*---------------------------------------------------------------------*/
        switch ( call->xbutton.button ) {

          case Button2:

		x = call->xbutton.x;
               	y = call->xbutton.y;

		color = GetColorFromWindow( XtWindow(DrawingW), x, y);

		indx = -1;
		xqnclr(&cbank, &ncolors, &iret);
		for ( ii = 0; ii < ncolors; ii++) {
		    if ( pixels[ii] == color ) {
			indx = ii;
			break;
		    }
		}

		colorevent.xbutton.button = Button1; 
		NxmPopupColorEdit(w, indx, &colorevent);
                break;

        }
}

/*=====================================================================*/
/* ARGSUSED */
void setPointerFocus ( Widget w, XtPointer clnt, XEvent *event )
{

    switch( event->type ) {

	case EnterNotify:
            XmProcessTraversal(DrawingW, XmTRAVERSE_CURRENT);
    }
}

/*=====================================================================*/

void toggle_menuWindow ( void )
{

	OpenModel = 1;

	if ( XtIsManaged( group_select_toplevel ) &
	     XtIsManaged( model_toplevel )  ) {
	    XtUnmanageChild( group_select_toplevel );
	    XtUnmanageChild( model_toplevel );
	}
	else {
	    XtManageChild( group_select_toplevel );
	    XtManageChild( model_toplevel );
	}
}

/*=====================================================================*/

unsigned long GetColorFromWindow ( Drawable win, int x, int y )
/************************************************************************
* T. Piper/SAIC         07/03   replaced gemdisplay with XtDisplay()    *
************************************************************************/
{
XImage  *image;
unsigned long color;

/*---------------------------------------------------------------------*/

        image = XGetImage(XtDisplay(DrawingW), win,
               		x,y, 1,1, AllPlanes, ZPixmap);

	color = XGetPixel(image, 0, 0);

	XDestroyImage( image );
	return( color );
}
