#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmInit.h"


Cardinal NXMtotalEditColor;
int    NXMcurColIndex;
XColor NXMcurrentColor;
Pixel  NXMcolrEditPixels[MAXCOLORS];

Widget NXMcolrEditWindow; 
Widget NXMcolorCubeW; 
Widget NXMcolorCubeLabel;
Widget NXMcolorSliderWgts[3];

float  NXMtabRed[MAXCOLORS], NXMtabGreen[MAXCOLORS], NXMtabBlue[MAXCOLORS];

/************************************************************************
 * NxmColorEdit.c							*
 *									*
 * CONTENTS:								*
 * NxmColorEditPopupCreate						*
 * NxmColorSetCurrent							*
 * NxmColorSetSliders							*
 ***********************************************************************/

Widget NxmColorEditPopupCreate ( Widget parent, char *popup_name, 
				 Pixel colr_pixels[], char *colrname_file, 
				 int ncolors )
/************************************************************************
 * NxmColorEditPopupCreate						*
 *									*
 * Widget NxmColorEditPopupCreate ( parent,popup_name,colr_pixels, 	*
 *					colrname_file, ncolors )	*
 *									*
 **									*
 ***********************************************************************/
{
Widget popup, pane, button; 
int    ii;

XmString xmstr;

/*--------------------------------------------------------------------*/

	if ( !NXMisInitialized )
		NxmInitialize( parent );

	for ( ii=0; ii<ncolors; ii++) 
	    NXMcolrEditPixels[ii] = colr_pixels[ii];

	NXMtabRed[0]   = 0.0F;
	NXMtabGreen[0] = 0.0F;
	NXMtabBlue[0]  = 0.0F;

	NXMtotalEditColor = (Cardinal)ncolors;
	NXMcurColIndex    = 0;

	NXMcurColIndex = 0;
	NXMcurrentColor.red   = (Dimension)(NXMtabRed[NXMcurColIndex] * 65535.0F);
        NXMcurrentColor.green = (Dimension)(NXMtabGreen[NXMcurColIndex] * 65535.0F);
        NXMcurrentColor.blue  = (Dimension)(NXMtabBlue[NXMcurColIndex] * 65535.0F);
	NXMcurrentColor.pixel = NXMcolrEditPixels[NXMcurColIndex];
	NXMcurrentColor.flags = DoRed | DoGreen | DoBlue;

        popup = XmCreateBulletinBoardDialog(parent,
                popup_name, NULL, 0);

	xmstr = XmStringCreateLocalized("Color Editing");
	XtVaSetValues(popup, 
		XmNnoResize, True, 
		XmNwidth, 300, 
		XmNdialogTitle, xmstr, 
		NULL);
	XmStringFree (xmstr);

        pane = XtVaCreateManagedWidget("color_pane",
                xmPanedWindowWidgetClass, popup,
		XmNsashWidth,             1,
		XmNsashHeight,            1,
                NULL);
	
	NxmColorEditSlidersCreate(pane);
	NxmColorPalettCreate(pane, ncolors);
	NxmColorNamelistsCreate(pane, colrname_file);
	NxmColorTablePanelCreate(pane);

	button = XtVaCreateManagedWidget("Close",
                xmPushButtonWidgetClass, pane,
                NULL);

        XtAddCallback(button, XmNactivateCallback,
                (XtCallbackProc)NxmClose_popupCb, 
		popup);

	NXMcolrEditWindow = popup;

	return( popup );
}

/*=====================================================================*/

void NxmColorSetCurrent ( int index )
/************************************************************************
 * NxmColorSetCurrent							*
 *									*
 * NxmColorSetCurrent (  index )					*
 *									*
 * Input parameters:							*
 *	index		int						*
 **									*
 ***********************************************************************/
{
char	name[5];
int	icolr, red, green, blue, iret;
XmString xmstr;

/*---------------------------------------------------------------------*/

	NXMcurColIndex = index;

	NXMcurrentColor.red   = (Dimension)(NXMtabRed[index]*65535.0F);
	NXMcurrentColor.green = (Dimension)(NXMtabGreen[index]*65535.0F);
	NXMcurrentColor.blue  = (Dimension)(NXMtabBlue[index]*65535.0F);
	NXMcurrentColor.pixel = NXMcolrEditPixels[index];

	red   = (int)(NXMtabRed[index]*255.0F);
        green = (int)(NXMtabGreen[index]*255.0F);
        blue  = (int)(NXMtabBlue[index]*255.0F);
	icolr = index;

	gscrgb( &icolr, &red, &green, &blue, &iret );

	XtVaSetValues(NXMcolorCubeW, 
		XmNbackground, NXMcurrentColor.pixel,
		NULL);

	sprintf(name, "%2d", index);
	xmstr = XmStringCreateLocalized(name);
        XtVaSetValues(NXMcolorCubeLabel, 
		XmNlabelString, xmstr,
		NULL);
	XmStringFree (xmstr);

	NxmColorSetSliders();

}

/*=====================================================================*/

void NxmColorSetSliders ( void )
/************************************************************************
 * NxmColorSetSliders		                                        *
 *                                                                      *
 * NxmColorSetSliders ( )						*
 *                                                                      *
 **                                                                     *
 ***********************************************************************/
{
int red, green, blue;

/*---------------------------------------------------------------------*/

	red   = (int)((float)NXMcurrentColor.red/655.35F);
	green = (int)((float)NXMcurrentColor.green/655.35F);
	blue  = (int)((float)NXMcurrentColor.blue/655.35F);

	XtVaSetValues(NXMcolorSliderWgts[0],
		XmNvalue, red,
		NULL);

	XtVaSetValues(NXMcolorSliderWgts[1],
		XmNvalue, green,
		NULL);

	XtVaSetValues(NXMcolorSliderWgts[2],
		XmNvalue, blue,
		NULL);

}
