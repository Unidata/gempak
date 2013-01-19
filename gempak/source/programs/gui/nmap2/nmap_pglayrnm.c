#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"

static Widget	   _pglayrnmWin;
static Widget	   _newNmTxt;

static int         _layerIdx;      

/*
 *  private callback functions
 */
void pglayrnm_ctlBtnCb ( Widget, long, XtPointer );

/*
 *  private functions
 */

/************************************************************************
 * nmap_pglayrnm.c							*
 *									*
 * This module defines a layer name popup window for VGF "Layering"     *
 * capability.                                                          *
 *									*
 * CONTENTS:								*
 *	pglayrnm_create()	create the layer name window	        *
 *	pglayrnm_popup()	pop up the layer name window	        *
 *	pglayrnm_popdown()	pop down the layer name window	        *
 *	pglayrnm_isUp()		query if the window is up 		*
 *									*
 *	pglayrnm_ctlBtnCb()	callback for control buttons 		*
 *									*
 ***********************************************************************/

/*=====================================================================*/

Widget pglayrnm_create ( Widget parent )
/************************************************************************
 * pglayrnm_create							*
 *									*
 * This function creates the layer name window.		                *
 *									*
 * Widget pglayrnm_create(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * pglayrnm_create	Widget  ID of the layer name window	        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	     	02/02	initial coding                          *
 * E. Safford/SAIC	03/02	limit text entry to 8 chars		*
 * B. Yin/SAIC		09/05	change text field widget to text widget	*
 ***********************************************************************/
{
    Widget	pane, layer_form;
    int		nn;
    char	*btnstrs[] = {"Accept", "Cancel"};
/*---------------------------------------------------------------------*/

    /*
     * create dialog shell
     */
    _pglayrnmWin = XmCreateFormDialog(parent, "pglayrnm_popup",
				     NULL, 0);
    XtVaSetValues(_pglayrnmWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pglayrnmWin),
		  XmNtitle, "Layer Name",
		  NULL);
    
    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("pglayrnm_pane",
			    xmPanedWindowWidgetClass, _pglayrnmWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    
    /*
     * create the main form area
     */
    layer_form = XtVaCreateWidget ("",
			    xmFormWidgetClass,	    pane,
			    NULL);

    /*
     * create the contents for form.
     */ 
    _newNmTxt= XtVaCreateManagedWidget ("new_name", 
		     xmTextWidgetClass,    layer_form,
		     XmNcolumns,		10,
		     XmNrows,			1,
                     XmNmaxLength,              8,
                     XmNmarginWidth,            5,
                     XmNleftAttachment,         XmATTACH_FORM,
		     XmNleftOffset,		10,
                     XmNtopAttachment,          XmATTACH_FORM,
		     NULL);

    XtManageChild(layer_form);

    /*
     * create control buttons
     */
    nn = XtNumber(btnstrs);
    NxmCtlBtn_create(pane, 0, "pglayrnm_ctlBtn", nn, btnstrs, 
			(XtCallbackProc)pglayrnm_ctlBtnCb, NULL);


    XtManageChild(pane);

    return(_pglayrnmWin);
    
}

/*=====================================================================*/

void pglayrnm_popup ( int layer )
/************************************************************************
 * pglayrnm_popup							*
 *									*
 * This function pops up the layer name window and set a new name for   *
 * the particular layer.                                                *
 *									*
 * void pglayrnm_popup( layer )						*
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
 * B. Yin/SAIC		09/05	change text field widget to text widget	*
 ***********************************************************************/
{
    char       value[100]; 
/*---------------------------------------------------------------------*/

    if ( pglayrnm_isUp() && _layerIdx == layer )  return;


    if (!pglayrnm_isUp() ) {
        XtManageChild (_pglayrnmWin);
    }

    _layerIdx = layer;

    sprintf ( value, "%s", pglayer_getName(layer) );
    XmTextSetString (_newNmTxt, value);
    XmTextSetHighlight (_newNmTxt,  0,
                        XmTextGetLastPosition(_newNmTxt),
                        XmHIGHLIGHT_SELECTED );
    XmTextSetSelection (_newNmTxt,  0,
                        XmTextGetLastPosition(_newNmTxt),
                        CurrentTime );
      
}

/*=====================================================================*/

void pglayrnm_popdown ( void ) 
/************************************************************************
 * pglayrnm_popdown							*
 *									*
 * This function pops down the layer name window.			*
 *									*
 * void pglayrnm_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		02/02	initial coding				*
 * B. Yin/SAIC		09/05	change text field widget to text widget	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_pglayrnmWin)) {
        XmTextSetHighlight (_newNmTxt,  0,
                            XmTextGetLastPosition(_newNmTxt),
                            XmHIGHLIGHT_NORMAL );
        XmTextSetString (_newNmTxt, "");
	XtUnmanageChild (_pglayrnmWin);

    }
}

/*=====================================================================*/

Boolean pglayrnm_isUp ( void ) 
/************************************************************************
 * pglayrnm_isUp							*
 *									*
 * This function queries whether the layer name window is up.	        *
 *									*
 * Boolean pglayrnm_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:                                                   *
 * pglayrnm_isUp	Boolean		True -- up,	False -- down   *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		02/02	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	return (XtIsManaged (_pglayrnmWin));
}

/*=====================================================================*/

/* ARGSUSED */
void pglayrnm_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pglayrnm_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of layer name    *
 * window.						                *
 *									*
 * void pglayrnm_ctlBtnCb (wid, which, call)				*
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
 * B. Yin/SAIC		09/05	change text field widget to text widget	*
 ***********************************************************************/
{
    char    *new_name;
/*---------------------------------------------------------------------*/

    switch(which) {
      case 0:	/* Accept */
        new_name = XmTextGetString (_newNmTxt);
        if ( new_name != NULL && strcmp(new_name, "") != 0 ) {
             pglayer_setName ( _layerIdx, new_name );
             pglayrw_updtSettings ( _layerIdx );
        }
        XtFree (new_name);
        pglayrnm_popdown ();

	break;

      case 1:	/* Cancel */
        pglayrnm_popdown ();
 
	break;

      default:

	break;

    } /* the end of switch(which) */

}
