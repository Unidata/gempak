#include "geminc.h"
#include "gemprm.h"
#include "xwcmn.h"
#include "Nxm.h"
#include "NxmInit.h"
#include "NxmColorEdit.h"


/*
 *  Private functions
 */
void _NxmCPgetDefaultColor ( Widget w, long which, XEvent *event );
/************************************************************************
 * NxmColorPalette.c							*
 *									*
 * CONTENTS:								*
 *	NxmColorPalettCreate						*
 *	_NxmCPgetDefaultColor						*
 ***********************************************************************/


void NxmColorPalettCreate ( Widget parent, int ncolors )
/************************************************************************
 * NxmColorPalettCreate                                                 *
 * 									*
 * This function creates a color editing panel.				*
 *                                                                      *
 * NxmColorPalettCreate ( parent, ncolors )				*
 *									*
 * Input parameters:							*
 *	parent		Widget						*
 *	ncolors		int						*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * C. Lin/EAI       11/94 set the topShadowColor and bottomShadowColor  *
 *                      for each color block to prevent Motif create    *
 *                      new dynamic colors for the shadow color,        *
 *                      thereore save significant amount of  color      *
 *                      resources.                                      *
 * S. Wang/GSC	    11/97 repace XStoreColor() with gscrgb()		*
 * I. Durham/GSC     5/98 changed underscore decl. to an include	*
 * S. Law/GSC		07/00	added XmStringFree call			*
 * T. Piper/SAIC	11/05	change ii to long			*
 ***********************************************************************/
{
long	ii;
char    name[5];
XmString xmstr;
Widget  bb, rc, button;

/*---------------------------------------------------------------------*/

	bb = XtVaCreateManagedWidget("_colorpalettBb",
                xmBulletinBoardWidgetClass, parent,
                NULL);

	xmstr = XmStringCreateLocalized("Color Palette");
	XtVaCreateManagedWidget("_colrpalettLb",
                xmLabelWidgetClass, bb,
        	XmNlabelString, xmstr,
		XmNnumColumns, 4,
		XmNx,          65,
		XmNy,          10,
                NULL);
	XmStringFree (xmstr);

        rc = XtVaCreateManagedWidget("_colorpalettRc",
                xmRowColumnWidgetClass, bb,
        	XmNorientation,         XmHORIZONTAL,
        	XmNpacking,             XmPACK_COLUMN,
		XmNx,                   20,
		XmNy,                   40,
		XmNnumColumns,          4,
		NULL);

	for(ii=1; ii<ncolors; ii++) {

		sprintf(name, "%2ld", ii);
		button = XtVaCreateManagedWidget(name,
                        xmLabelWidgetClass, rc,
                        XmNbackground,        NXMcolrEditPixels[ii],
			XmNtopShadowColor,    NXMcolrEditPixels[ii],
			XmNbottomShadowColor, NXMcolrEditPixels[ii],
                        NULL);

		XtAddEventHandler(button,
                     	ButtonReleaseMask, FALSE ,
                     	(XtEventHandler)(_NxmCPgetDefaultColor), 
			(XtPointer)ii);
        }
}

/*=====================================================================*/
/* ARGSUSED */
void _NxmCPgetDefaultColor ( Widget w, long which, XEvent *event )
/************************************************************************
 * _NxmCPgetDefaultColor						*
 *									*
 * _NxmCPgetDefaultColor ( w, which, event )				*
 *									*
 * Input parameters:							*
 *	w		Widget						*
 *	which		long						*
 *	*event		XEvent						*
 **									*
 * T. Piper/SAIC	07/04	Replaced ratio with COLR_SCAL		*
 ***********************************************************************/
{
    XColor	color;
    int		icolr, iret, red, green, blue;
/*---------------------------------------------------------------------*/

    if ( event->xbutton.button == Button1 ) { 

	color.pixel  = NXMcolrEditPixels[which];
	color.flags  = DoRed | DoGreen | DoBlue;
        XQueryColor(gemdisplay, gemmap, &color);

	NXMcurrentColor.red   = color.red;
	NXMcurrentColor.green = color.green;
	NXMcurrentColor.blue  = color.blue;

	icolr = NXMcurColIndex;
	red = color.red/COLR_SCAL;
	green = color.green/COLR_SCAL;
	blue = color.blue/COLR_SCAL;

	gscrgb( &icolr, &red, &green, &blue, &iret );

	NxmColorSetSliders();
    }
}
