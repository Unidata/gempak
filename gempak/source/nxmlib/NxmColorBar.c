#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmInit.h"
#include "NxmColorEdit.h"
#include "proto_xw.h"


WidgetList NXMcolrCells;          /* widgets the color cells in colorbar */
Widget     _activeColLabel;       /* title string of color bar */
int	   _toggleColors[2][MAXCOLORS]; /* states for toggling the colors */

Pixel	   _white, _black; 

/*
 *  Private functions
 */
void NxmUndisplayCbColorcell ( int index );

/************************************************************************
 * NxmColorBar.c							*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

void NxmColorBarCreate ( Widget parent, int ncolors, 
			Pixel color_pixels[], Boolean show_active_color )
/************************************************************************
 * NxmColorBarCreate                                                    *
 *                                                                      *
 * This function creates a color bar widget consisted of a              *
 * string( "Active colors/Colors" ) and a set of color cells.           *
 * If the parameter show_active_color is set to False, the string will  *
 * not be displayed.                                                    *
 * The user can click left mouse button on a color cell to get a popup  *
 * color editing window. The right mouse button will toggle on/off the  *
 * selected color and holding CTRL key while clicking the right mouse   *
 * button will blink the selected color.                                *
 *                                                                      *
 * void NxmColorBarCreate(parent, ncolors, color_pixels,                *
 *      show_active_color )                                             *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent            Widget   parent widget ID                         *
 *  ncolors           int      number of color cells                    *
 *  color_pixels[]    Pixel    array of index of each color             *
 *  show_active_color Boolean  show/not_show title string               *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       	05/94                                           *
 * C. Lin/EAI       	11/94 	set topShadowColor and bottomShadowColor*
 *			      	for each color block to prevent Motif   *
 *			      	creating new dynamic colors for the     *
 *				shadow color				*
 * S. Wang/GSC	    	11/97   replace XStoreColor() with gscrgb()	*
 * I. Durham/GSC     	05/98   changed underscore decl. to an include	*
 * E. Safford/GSC	01/99	clean up				*
 * T. Piper/SAIC	05/03	replaced XAllocNamedColor w/xsncolr	*
 * T. Piper/SAIC	11/05	changed ii to long			*
 ***********************************************************************/
{
int	iret;
long	ii;
char	name[3];
Widget  rowcol;

/*---------------------------------------------------------------------*/


    if ( !NXMisInitialized ) {
        NxmInitialize( parent );
    }

    xsncolr("White", &_white, &iret);
    xsncolr("Black", &_black, &iret);

    NXMcolrCells = (WidgetList) XtMalloc( (size_t)ncolors * sizeof(Widget));

    rowcol = XtVaCreateManagedWidget("colrbarRc",
		xmRowColumnWidgetClass, 	parent,
        	XmNorientation, 		XmHORIZONTAL,
                NULL);

    if (show_active_color) {
        _activeColLabel = XtVaCreateManagedWidget("Active Colors:",
                xmLabelWidgetClass, 		rowcol,
                NULL);
    }
    else {
        _activeColLabel = XtVaCreateManagedWidget("",
                xmLabelWidgetClass, 		rowcol,
                NULL);
    }

    for(ii=0; ii<ncolors; ii++) {
	sprintf(name, "%2ld", ii);
        NXMcolrCells[ii] = XtVaCreateWidget(name,
                xmLabelWidgetClass, 		rowcol,
        	XmNbackground,        		color_pixels[ii],
		XmNbottomShadowColor, 		color_pixels[ii],
		XmNtopShadowColor,    		color_pixels[ii],
                NULL);

	_toggleColors[0][ii] = 0;
	_toggleColors[1][ii] = 0;
	
        XtAddEventHandler(NXMcolrCells[ii], 
	        ButtonReleaseMask, 		FALSE ,
                (XtEventHandler)NxmPopupColorEdit, (XtPointer)ii);
    }

    if (show_active_color) {
        XtManageChild(NXMcolrCells[0]);
    }
    else {
	XtManageChildren(NXMcolrCells, (Cardinal)ncolors);
    }

}

/*=====================================================================*/
/* ARGSUSED */
void NxmPopupColorEdit ( Widget w, long color, XEvent *event )
/************************************************************************
 * NxmPopupColorEdit                                                    *
 *                                                                      *
 * This function is the callback that displays the color edit popup     *
 * window.                  						*
 *                                                                      *
 * void NxmPopupColorEdit( w, color, event )                       *
 *                                                                      *
 * Input parameters:                                                    *
 *  w		Widget	color widget that generated the callback        *
 *  color	long	color index number	                        *
 *  *event	XEvent	event type					*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NULL                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       	05/94                                           *
 * E. Safford/GSC	01/99	clean up & add documentation		*
 * S. Jacobs/NCEP	 9/02	Added check for 8 bit visual		*
 * T. Piper/SAIC	07/04	Replaced ratio with COLR_SCAL		*
 ***********************************************************************/
{
XColor  xcolor;
int     icolr, red, green, blue, iret, xdpth;
/*---------------------------------------------------------------------*/

    xdpth = DefaultDepth(XtDisplay(w), DefaultScreen(XtDisplay(w)));

    if  ( xdpth == 8 )  {

	switch ( event->xbutton.button ) {

	  case Button1: 		/* popup the color editing */

	    NxmColorSetCurrent( (int)color );
	    XtVaSetValues(NXMcolrCells[color],
			XmNforeground, 		_black, 
			NULL);

            XtManageChild( NXMcolrEditWindow );

	    break;

	  case Button3:

	    if ( event->xbutton.state == 1028 ) {

                if ( color != 0 ) {
                    if ( _toggleColors[1][color] == 0 ) {
                        _toggleColors[1][color] = 1;
                        NxmColorBlinkSet( (int)color, 0 );
                    }
                    else {
                        _toggleColors[1][color] = 0;
                        NxmColorBlinkSet( (int)color, 1);
                    }
                }

		break;
	    }

	    if ( color != 0 ) {
		if ( _toggleColors[0][color] == 0 ) {

		    _toggleColors[0][color] = 1;

        	    xcolor.pixel = NXMcolrEditPixels[0];
		    xcolor.flags = DoRed | DoBlue | DoGreen;
        	    XQueryColor(XtDisplay(w), NXMcmap, &xcolor);
        	    xcolor.pixel = NXMcolrEditPixels[color];
					
		    icolr = (int)color;
                    red   = xcolor.red/COLR_SCAL;
                    green = xcolor.green/COLR_SCAL;
                    blue  = xcolor.blue/COLR_SCAL;
                    gscrgb( &icolr, &red, &green, &blue, &iret );

		    XtVaSetValues(NXMcolrCells[color],
			   	XmNforeground, 		_white, 
				NULL);
		}
		else {
		    _toggleColors[0][color] = 0;
		    NxmColorSetCurrent( (int)color );
		    XtVaSetValues(NXMcolrCells[color],
				XmNforeground, 		_black, 
				NULL);
		}
			
	    }

	    break;

	}  /* end switch */

    } /* end visual depth check */
}

/*=====================================================================*/

void NxmColorbarReset ( int type )
/************************************************************************
 * NxmColorBarReset                                                     *
 *                                                                      *
 * This function resets the display of the color bar. Different title   *
 * string will be displayed based on the input parameter.               *
 *                                                                      *
 * void NxmColorBarReset( type )                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *  type            int   0 --- title string = "Active Colors:"         *
 *                        1 --- title string = "Colors:"                *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       	05/94                                           *
 * E. Safford/GSC	01/99	clean up				*
 * S. Law/GSC		07/00	added XmStringFree calls		*
 ***********************************************************************/
{
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    if (type == 0) {
	xmstr = XmStringCreateLocalized("Active Colors:");
        XtVaSetValues( _activeColLabel, 
		       XmNlabelString, xmstr,
		       NULL);
	XmStringFree (xmstr);

	if ( NXMcolrCells != NULL ) {
	    XtUnmanageChildren( NXMcolrCells, NXMtotalEditColor );
	    XtManageChild( NXMcolrCells[0]);
	}
    }
    else {
	xmstr = XmStringCreateLocalized("Colors:");
        XtVaSetValues( _activeColLabel, 
		       XmNlabelString, xmstr,
		       NULL);
	XmStringFree (xmstr);

	if ( NXMcolrCells != NULL ) {
	    XtManageChildren( NXMcolrCells, NXMtotalEditColor );
	}
    }
}

/*=====================================================================*/

void NxmColorBarReload ( int ncolors, Pixel colorl[] )
/************************************************************************
 * NxmColorBarReload                                                    *
 *                                                                      *
 * This function reloads the colors in the color bar using the original *
 * colors.								*
 *                                                                      *
 * void NxmColorBarReload ( ncolors, colorl )                   	*
 *                                                                      *
 * Input parameters:                                                    *
 *	ncolors		int	number of colors			*
 *	colorl[]	Pixel	color pixels				*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *   None                                                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	01/99	initial coding                          *
 * E. Safford/GSC	01/99	add reset of bg color and clean up      *
 * T. Piper/SAIC	11/05	removed unused parameter Parent		*
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
/*
 *  reset the background color
 */
    NxmColorSetCurrent(0);

/*
 *  reset the color cells to the original table values
 */
    for(ii=0; ii<ncolors; ii++) {
	XtVaSetValues (NXMcolrCells[ii],
       		XmNbackground,        	colorl[ii],
		XmNbottomShadowColor, 	colorl[ii],
		XmNtopShadowColor,    	colorl[ii],
                NULL);

	_toggleColors[0][ii] = 0;
	_toggleColors[1][ii] = 0;
    }
}

/*=====================================================================*/

void NxmColorbarSetBlack ( void )
/************************************************************************
 * NxmColorbarSetBlack                                                  *
 *                                                                      *
 * This function set the label color of each color cell to be black.    *
 *                                                                      *
 * void NxmColorbarSetBlack()                                           *
 *                                                                      *
 * Input parameters:                                                    *
 *                      NULL                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       	05/94                                           *
 * E. Safford/GSC	01/99	clean up				*
 ***********************************************************************/
{
Cardinal     ii;
/*---------------------------------------------------------------------*/

    for ( ii = 1; ii < NXMtotalEditColor; ii++) {

        XtVaSetValues(NXMcolrCells[ii], 
		XmNforeground, 		_black, 
		NULL);
    }
}

/*=====================================================================*/

void NxmDisplayCbColorcell ( int index )
/************************************************************************
 * NxmDisplayCbColorcell                                                *
 *                                                                      *
 * This function displays the selected color cell in color bar area.    *
 *                                                                      *
 * void NxmDisplayCbColorcell( index )                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *  index            int   index of the selected color                  *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 ***********************************************************************/
{
    XtManageChild(NXMcolrCells[index]);
}

/*=====================================================================*/

void NxmUndisplayCbColorcell ( int index )
/************************************************************************
 * NxmUndisplayCbColorcell                                              *
 *                                                                      *
 * This function make the selected color cell not shown in color bar.   *
 *                                                                      *
 * void NxmUndisplayCbColorcell( index )                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  index            int   index of the selected color                  *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 ***********************************************************************/
{
    XtUnmanageChild(NXMcolrCells[index]);
}
