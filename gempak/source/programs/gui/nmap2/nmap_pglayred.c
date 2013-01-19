#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"

static Widget	   _pglayredWin;
static Widget	   _nameLbl, _colorTgl, _colorPickW, _fillTgl;

static Boolean     _color, _fill;
static int         _colorPick, _layerIdx;      

/*
 *  private callback functions
 */
void pglayred_ctlBtnCb ( Widget, long, XtPointer );
void pglayred_colorCb  ( Widget, XtPointer, XtPointer );
void pglayred_tglbCb   ( Widget, long, XtPointer );

/*
 *  private functions
 */

/************************************************************************
 * nmap_pglayred.c							*
 *									*
 * This module defines a layer display popup window for VGF "Layering"  *
 * capability.                                                          *
 *									*
 * CONTENTS:								*
 *	pglayred_create()	create the layer display window	        *
 *	pglayred_popup()	pop up the layer display window	        *
 *	pglayred_popdown()	pop down the layer display window	*
 *	pglayred_isUp()		query if the window is up 		*
 *									*
 *	pglayred_ctlBtnCb()	callback for control buttons 		*
 *      pglayred_tglbCb()       callback for toggle buttons             *
 *      pglayred_colorCb()      callback for color push buttons         *
 *									*
 ***********************************************************************/

/*=====================================================================*/

Widget pglayred_create ( Widget parent )
/************************************************************************
 * pglayred_create							*
 *									*
 * This function creates the layer display window.		        *
 *									*
 * Widget pglayred_create(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * pglayred_create	Widget  ID of the layer display window	        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	     02/02	initial coding                          *
 ***********************************************************************/
{
    Widget	pane, layer_rc, form;
    int		nn, loff = 10;
    char	*btnstrs[] = {"Accept", "Cancel"};
/*---------------------------------------------------------------------*/

    /*
     * create dialog shell
     */
    _pglayredWin = XmCreateFormDialog(parent, "pglayred_popup",
				     NULL, 0);
    XtVaSetValues(_pglayredWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pglayredWin),
		  XmNtitle, "Layer Display",
		  NULL);
    
    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("pglayred_pane",
			    xmPanedWindowWidgetClass, _pglayredWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    
    /*
     * create the main rowcolumn area
     */
    layer_rc = XtVaCreateWidget ("",
			    xmRowColumnWidgetClass,	pane,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_COLUMN,
			    NULL);

    /*
     * create the buttons for each layer.
     */
    _nameLbl = XtVaCreateManagedWidget ("DISPLAY FOR:",
				     xmLabelWidgetClass,	layer_rc,
                         	     NULL);  

    form = XtVaCreateWidget("form",
		     xmFormWidgetClass,     layer_rc,
		     NULL);

    _colorTgl = XtVaCreateManagedWidget("Mono Color",
		     xmToggleButtonGadgetClass, form,
                     XmNleftAttachment,         XmATTACH_FORM,
                     XmNtopAttachment,          XmATTACH_FORM,
                     XmNtraversalOn,            FALSE,
		     XmNindicatorType,          XmN_OF_MANY,
		     NULL);

    XtAddCallback(_colorTgl, XmNvalueChangedCallback,
		  (XtCallbackProc)pglayred_tglbCb, (XtPointer)0);

    _colorPickW	= XtVaCreateManagedWidget(" ",
		     xmPushButtonWidgetClass,	form,
                     XmNleftAttachment,         XmATTACH_WIDGET,
                     XmNleftWidget,             _colorTgl,
                     XmNleftOffset,             loff,
                     XmNtopAttachment,          XmATTACH_FORM,
                     XmNtraversalOn,            FALSE,
		     XmNwidth,			25,
		     XmNheight,			20,
		     NULL);

    XtAddCallback(_colorPickW, XmNactivateCallback, 
    		  (XtCallbackProc)pglayred_colorCb, NULL);

    XtManageChild(form);

    _fillTgl = XtVaCreateManagedWidget("Fill",
		     xmToggleButtonGadgetClass, layer_rc,
                     XmNtraversalOn,            FALSE,
		     XmNindicatorType,          XmN_OF_MANY,
		     NULL);

    XtAddCallback(_fillTgl, XmNvalueChangedCallback,
		  (XtCallbackProc)pglayred_tglbCb, (XtPointer)1);

    XtManageChild(layer_rc);


    /*
     * create control buttons
     */
    nn = XtNumber(btnstrs);
    NxmCtlBtn_create(pane, 0, "pglayred_ctlBtn", nn, btnstrs, 
			(XtCallbackProc)pglayred_ctlBtnCb, NULL);


    XtManageChild(pane);

    return(_pglayredWin);
    
}

/*=====================================================================*/

void pglayred_popup ( int layer )
/************************************************************************
 * pglayred_popup							*
 *									*
 * This function pops up the layer display window and set the display	*
 * attribute for the particular layer.                                  *
 *									*
 * void pglayred_popup( layer )						*
 *									*
 * Input parameters:                                                    *
 *        layer         int     current layer index			*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		02/02	initial coding				*
 ***********************************************************************/
{
    char       label[50];
    XmString   xmstr; 
    Pixel      col_pxl;  
/*---------------------------------------------------------------------*/

    if ( XtIsManaged (_pglayredWin) && _layerIdx == layer )  return;


    pglayred_popdown ();

    _layerIdx = layer;

    sprintf ( label, "%s:", pglayer_getName(layer) );
    xmstr = XmStringCreateLocalized ( label );  
    XtVaSetValues( _nameLbl,
                   XmNlabelString,          xmstr,
                   NULL); 
    XmStringFree(xmstr);          

    _color = pglayer_getDsplClr ( layer );
    XmToggleButtonSetState (_colorTgl, !_color, FALSE); 

    _colorPick = pglayer_getMonoClr(layer);
    col_pxl = NxmColrP_getColorPixel( _colorPick );
    XtVaSetValues(_colorPickW,
	          XmNbackground,	col_pxl,
	          XmNtopShadowColor,	col_pxl,
	          XmNbottomShadowColor,	col_pxl,
	          NULL);

    _fill = pglayer_getFill ( layer );
    XmToggleButtonSetState (_fillTgl, _fill, FALSE); 
      
    XtManageChild (_pglayredWin);
  
}

/*=====================================================================*/

void pglayred_popdown ( void ) 
/************************************************************************
 * pglayred_popdown							*
 *									*
 * This function pops down the layer display window.			*
 *									*
 * void pglayred_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		02/02	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_pglayredWin)) {
        NxmClrW_popdown();
	XtUnmanageChild (_pglayredWin);

    }
}

/*=====================================================================*/

Boolean pglayred_isUp ( void ) 
/************************************************************************
 * pglayred_isUp							*
 *									*
 * This function queries whether the layer display window is up.	*
 *									*
 * Boolean pglayred_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:                                                   *
 * pglayred_isUp	Boolean		True -- up,	False -- down   *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		02/02	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	return (XtIsManaged (_pglayredWin));
}

/*=====================================================================*/

/* ARGSUSED */
void pglayred_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pglayred_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of layer display *
 * window.						                *
 *									*
 * void pglayred_ctlBtnCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          02/02   initial coding                          *
 * E. Safford/SAIC	03/02	add pglayrw_refresh()			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    switch(which) {
      case 0:	/* Accept */
        pglayer_setDsplClr ( _layerIdx, _color );
        pglayer_setMonoClr ( _layerIdx, _colorPick );
        pglayer_setFill    ( _layerIdx, _fill      );
        pglayrw_updtSettings ( _layerIdx );
        pglayred_popdown ();

 	pglayrw_refresh();

	break;

      case 1:	/* Cancel */
        pglayred_popdown ();
 
	break;

      default:

	break;

    } /* the end of switch(which) */

}

/*=====================================================================*/

/* ARGSUSED */
void pglayred_colorCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pglayred_colorCb							*
 *                                                                      *
 * Callback function for layer display window color picker.             *
 *                                                                      *
 * void pglayred_colorCb(w, clnt, call)				*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  clnt   XtPointer  used to hold color value                   *
 *  call          XtPointer  callback data struct     			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI     02/02        initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    clnt = (XtPointer) &_colorPick;
    NxmClrW_popup (w, clnt, call);

}

/*=====================================================================*/

/* ARGSUSED */
void pglayred_tglbCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pglayred_tglbCb                                                      *
 *                                                                      *
 * Callback function for layer display toggle buttons.     		*
 *                                                                      *
 * void pglayred_tglbCb(w, which, call)                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long       button index                               *
 *  call          XtPointer  callback data struct     			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI     02/02        initial coding                          *
 ***********************************************************************/
{
XmToggleButtonCallbackStruct 
                  *cbs = (XmToggleButtonCallbackStruct *)call;
/*---------------------------------------------------------------------*/
 
    switch(which) {
      case 0:	/* Mono Color */
        _color = !(cbs->set);

	break;

      case 1:	/* Fill */
        _fill = cbs->set;

	break;

      default:

	break;

    } /* the end of switch(which) */

}
