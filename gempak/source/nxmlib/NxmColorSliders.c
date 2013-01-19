#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmInit.h"
#include "NxmColorEdit.h"

void   _NxmColorSet ( Widget, XtPointer, XtPointer );
Widget NxmMakeColorSlider ( Widget parent, char *name, int value,
			    void (*callback)(Widget, long,
			     XmScaleCallbackStruct *), long calldata );
void   NxmColorSliderMovedCallback ( Widget, long which, 
				XmScaleCallbackStruct *cbs );

/************************************************************************
 * NxmColorSliders.c							*
 *									*
 * CONTENTS:								*
 *	NxmColorEditSlidersCreate					*
 *	_NxmColorSet							*
 *	NxmMakeColorSlider						*
 *	NxmColorSliderMovedCallback					*
 ***********************************************************************/

void NxmColorEditSlidersCreate ( Widget parent )
/************************************************************************
 * NxmColorEditSlidersCreate						*
 *                                                                      *
 * This function creates color editing sliders.                         *
 *                                                                      *
 * NxmColorEditSlidersCreate ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget						*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       05/94                                               *
 * C. Lin/EAI       11/94 set the topShadowColor and bottomShadowColor  *
 *                      for the "current_color_cube" to prevent Motif   *
 *                      create new dynamic colors for the shadow color, *
 *                      thereore save significant amount of  color      *
 *                      resources.                                      *
 * S. Wang/GSC	    11/97 replace XStoreColor() with gscrgb()		*
 * I. Durham/GSC     5/98 changed underscore decl. to and include	*
 * S. Law/GSC		07/00	added XmStringFree calls		*
 ***********************************************************************/
{
Widget form, button, rc;
int    red, green, blue;
char   name[5];
XmString xmstr;

/*----------------------------------------------------------------------*/

        form = XtVaCreateManagedWidget("_form",
		xmFormWidgetClass, parent, 
                NULL);

	sprintf(name, "%2d", NXMcurColIndex);

	xmstr = XmStringCreateLocalized(name);
        NXMcolorCubeLabel = XtVaCreateManagedWidget("color_cube_l", 
		xmLabelWidgetClass, form,
		XmNtopAttachment,   XmATTACH_POSITION,
		XmNtopPosition,     20,
		XmNleftAttachment,  XmATTACH_POSITION,
		XmNleftPosition,    17,
		XmNlabelString,     xmstr,
                NULL);
	XmStringFree (xmstr);

	xmstr = XmStringCreateLocalized("");
        NXMcolorCubeW = XtVaCreateManagedWidget("color_cube", 
		xmLabelWidgetClass, form,
		XmNtopAttachment,   XmATTACH_POSITION,
		XmNtopPosition,     35,
		XmNleftAttachment,  XmATTACH_POSITION,
		XmNleftPosition,    10,
		XmNwidth,           80,
		XmNheight,          80,
		XmNbackground,       NXMcolrEditPixels[NXMcurColIndex],
		XmNtopShadowColor,   NXMcolrEditPixels[NXMcurColIndex],
		XmNbottomShadowColor,NXMcolrEditPixels[NXMcurColIndex],
		XmNlabelString,     xmstr,
                NULL);
	XmStringFree (xmstr);

	xmstr = XmStringCreateLocalized(" Save Color");
        button = XtVaCreateManagedWidget("color_save", 
		xmPushButtonWidgetClass, form,
		XmNtopAttachment,        XmATTACH_POSITION,
		XmNtopPosition,          80,
		XmNleftAttachment,       XmATTACH_POSITION,
		XmNleftPosition,         5,
		XmNlabelString,          xmstr,
                NULL);
	XmStringFree (xmstr);

  	XtAddCallback(button, XmNactivateCallback, _NxmColorSet, NULL);

  	rc = XtVaCreateManagedWidget("slider",
		xmRowColumnWidgetClass, form,
		XmNtopAttachment,       XmATTACH_POSITION,
		XmNtopPosition,         10,
		XmNleftAttachment,      XmATTACH_POSITION,
		XmNleftPosition,        50,
                NULL );

        red   = (int)((float)NXMcurrentColor.red/655.35F);
        green = (int)((float)NXMcurrentColor.green/655.35F);
        blue  = (int)((float)NXMcurrentColor.blue/655.35F);

  	NXMcolorSliderWgts[0] = NxmMakeColorSlider(rc, "red", red,
                             NxmColorSliderMovedCallback, 0);

  	NXMcolorSliderWgts[1] = NxmMakeColorSlider(rc, "green", green,
                             NxmColorSliderMovedCallback, 1);

  	NXMcolorSliderWgts[2] = NxmMakeColorSlider(rc, "blue",  blue,
                            NxmColorSliderMovedCallback, 2);

}

/*=====================================================================*/
/* ARGSUSED */
void _NxmColorSet ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * _NxmColorSet			                                        *
 *                                                                      *
 * _NxmColorSet ( w, clnt, cbs )			                *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget                                          *
 *      clnt	XtPointer                                       *
 *      cbs		XtPointer					*
 **                                                                     *
 ***********************************************************************/
{

        NXMtabRed[NXMcurColIndex]   = (float)NXMcurrentColor.red/65535.0F;
        NXMtabGreen[NXMcurColIndex] = (float)NXMcurrentColor.green/65535.0F;
        NXMtabBlue[NXMcurColIndex] = (float)NXMcurrentColor.blue/65535.0F;

}

/*======================================================================*/

Widget NxmMakeColorSlider ( Widget parent, char *name, int value, 
			void (*callback)(Widget, long, XmScaleCallbackStruct *),
			long calldata )
/************************************************************************
 * NxmMakeColorSlider                                          		*
 *                                                                      *
 * Widget NxmMakeColorSlider ( parent, name, value, callback, calldata )*
 *                                                                      *
 * Input parameters:                                                    *
 *      parent          Widget                                          *
 *      *name		char						*
 *	value		int                                             *
 *      *callback()	void						*
 *	calldata	int						*
 * Output parameters:							*
 *	NxmMakeColorSlider	Widget					*
 **                                                                     *
 ***********************************************************************/
{
Widget  w;
XmString xmstr;

/*------------------------------------------------------------------------*/

        xmstr = XmStringCreateLocalized(name);
  	w = XtVaCreateManagedWidget(name, 
		xmScaleWidgetClass, parent,
		XmNminimum, 0,
		XmNmaximum, 100,
		XmNdecimalPoints, 2,
		XmNorientation,XmHORIZONTAL,
		XmNtitleString, xmstr,
		XmNshowValue, True,
		XmNscaleWidth, 120,
		XmNscaleHeight, 20,
		XmNvalue, value,
                NULL);
	XmStringFree (xmstr);

  	XtAddCallback(w, XmNvalueChangedCallback, 
		(XtCallbackProc)callback, (XtPointer)calldata);
  	XtAddCallback(w, XmNdragCallback, 
		(XtCallbackProc)callback, (XtPointer)calldata);

  	return (w);
}

/*=====================================================================*/
/* ARGSUSED */
void NxmColorSliderMovedCallback ( Widget w, long which, 
				XmScaleCallbackStruct *cbs )
/************************************************************************
 * NxmColorSliderMovedCallback						*
 *									*
 * NxmColorSliderMovedCallback ( w, which, cbs )			*
 *									*
 * Input parameters:							*
 *	w		Widget						*
 *	which		long						*
 *	*cbs	XmScaleCallbackStruct				*
 **									*
 * T. Piper/SAIC	07/04	Replaced ratio with COLR_SCAL		*
 ***********************************************************************/
{
int     icolr, iret, red, green, blue;

/*---------------------------------------------------------------------*/

	switch ( which ) {
	    case 0 : /* red slider */
  		NXMcurrentColor.red =  (unsigned short)((float)cbs->value*655.35F);
		break;
	
	    case 1 : /* green slider */
  		NXMcurrentColor.green = (unsigned short)((float)cbs->value*655.35F);
		break;

	    case 2 : /* blue slider */
  		NXMcurrentColor.blue =  (unsigned short)((float)cbs->value*655.35F);
		break;
	}
	

	icolr = NXMcurColIndex;
	red = NXMcurrentColor.red/COLR_SCAL;
	green = NXMcurrentColor.green/COLR_SCAL;
	blue = NXMcurrentColor.blue/COLR_SCAL;
	gscrgb( &icolr, &red, &green, &blue, &iret );

}
