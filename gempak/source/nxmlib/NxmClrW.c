#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"


static Widget    	_colrPalW = NULL;
static Widget    	_triggerW;
static NxmColrP_t   	*_txtColorInfo;
static int          	*_colrVal;	

/*
 *  Private callback functions
 */
void NxmClrW_exitCb ( Widget, XtPointer, XtPointer );
void NxmClrW_selEh  ( Widget, XtPointer, XEvent *event, Boolean *ctd );


/************************************************************************
 * NxmClrW.c	  							*
 *                                                                      *
 * This file contains the subroutines necessary to build and operate    *
 * a generic color selection popup window.				*
 *                                                                      *
 * CONTENTS:            						*
 *    NxmClrW_create      - creates the color picker window		*
 *    NxmClrW_popup       - manages the color picker window		*
 *    NxmClrW_popdown     - unmanages the color picker window		*
 *    NxmClrW_popdown2    - selectively popdown the color window	*
 *									*
 *    NxmClrW_selEh       - Event Handler for color button              *
 *    NxmClrW_exitCb      - callback for color window exit              *
 *									*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/97	Initial coding - removed from NxmTxtA.c *
 *				 and made a separate module        	*
 ***********************************************************************/

/*=====================================================================*/

void NxmClrW_create ( Widget parent_w )
/************************************************************************
 * NxmClrW_create							*
 *                                                                      *
 * This function creates color selection popup window.                 	*
 *                                                                      *
 * void NxmClrW_create  ( parent_w )                     		*
 *                                                                      *
 * Input parameters:                                                    *
 *      parent_w	 Widget		Parent widget			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC 	10/97		Initial coding			*
 * W. Li/EAI		10/98		Removed "OK" button		*
 * S. Jacobs/NCEP	 2/00	Changed location of color palette	*
 ***********************************************************************/
{
    Widget	frame, pane, colr_rc;
    char	*btnstr[] = {"Cancel"};
    XmString	ttl_string;
/*---------------------------------------------------------------------*/
    if (_colrPalW == NULL) { 
        _colrPalW = XmCreateFormDialog(parent_w, "_colrPalW", NULL, 0);  

        ttl_string = XmStringCreateLocalized("Color Palette");
        XtVaSetValues(_colrPalW, 
		XmNnoResize,	  	    True,
	   	XmNdialogTitle,   	    ttl_string,
		XmNdefaultPosition,	    False,
		XmNx,                       0,
                XmNy,                       800,
		NULL);
        XmStringFree(ttl_string);

        pane  = XtVaCreateManagedWidget ("pane",
		xmPanedWindowWidgetClass,   _colrPalW,
		XmNsashWidth,		    1,
		XmNsashHeight,		    1,
		NULL);

        colr_rc = XtVaCreateManagedWidget ("colr_rc",
		xmRowColumnWidgetClass,     pane,
		XmNorientation,		    XmVERTICAL,
		NULL);


        frame = XtVaCreateWidget ("frame", 
		xmFrameWidgetClass, 	    colr_rc, 
		XmNtopAttachment,	    XmATTACH_FORM,
		XmNtopOffset,		    3,
		XmNleftAttachment,	    XmATTACH_FORM, 
		NULL);


        _txtColorInfo = NxmColrP_create (frame, 4, 1, (XtEventHandler)NxmClrW_selEh); 
   
        XtManageChild (frame);
    

        NxmCtlBtn_create (pane, 1, "ctlBtns", XtNumber(btnstr), 
			 btnstr, (XtCallbackProc)NxmClrW_exitCb, NULL);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void NxmClrW_popup ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * NxmClrW_popup                                                        *
 *                                                                      *
 * This function initiates the color selection window.                  *
 *									*
 * void NxmClrW_popup ( wdgt, clnt, call )                     		*
 *                                                                      *
 * Input parameters:                                                    *
 *      wdgt            Widget          Widget that activated callback  *
 *      clnt		XtPointer	client data			*
 *      call		XtPointer       callback data    		*
 *                                                                      *
 * Output Parameters:							*
 *	none                                                            *
 *									*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/97	Initial coding				*
 * C. Lin/EAI           04/98	Add _newColor initialization		*
 * T. Piper/SAIC	04/06	Changed *_colrVal to int from long	*
 ***********************************************************************/
{
    _triggerW = wdgt;
    _colrVal  = (int *)clnt;
    NxmColrP_setColor (_txtColorInfo, *_colrVal); 
    XtManageChild (_colrPalW);
}

/*=====================================================================*/

void NxmClrW_popdown ( void )
/************************************************************************
 * NxmClrW_popdown 							*
 *                                                                      *
 * This function unmanages the color selection popup window            	*
 *                                                                      *
 * void NxmClrW_popdown ()	 					*
 *                                                                      *
 * Input Parameters:							*
 *									*
 * Output Parameters:							*
 *     None								*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC 	10/97	Initial coding	       			*
 ***********************************************************************/
{
    if (XtIsManaged (_colrPalW)) {
        XtUnmanageChild (_colrPalW);
    }
}

/*=====================================================================*/

void NxmClrW_popdown2 ( Widget wid )
/************************************************************************
 * NxmClrW_popdown2							*
 *                                                                      *
 * This function selectively unmanages the color selection popup window *
 * if the passed in widget equals the current triggering widget.        *
 *                                                                      *
 * void NxmClrW_popdown2 ( wid )	 				*
 *                                                                      *
 * Input Parameters:							*
 *      wid         Widget          passed in widget pointer		*
 *									*
 * Output Parameters:							*
 *     None								*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		11/04	Initial coding				*
 ***********************************************************************/
{
    if ( wid == _triggerW ) {
         NxmClrW_popdown ( );      
    }
}

/*=====================================================================*/
/* ARGSUSED */
void NxmClrW_selEh( Widget wdgt, XtPointer clnt, XEvent *event, Boolean *ctd )
/************************************************************************
 * NxmClrW_selEh                                                        *
 *                                                                      *
 * This is the event handler function for all color button selections.  *
 *                                                                      *
 * void NxmClrW_selEh  ( wdgt, clnt, event, ctd )    	        	*
 *                                                                      *
 * Input parameters:                                                    *
 *      wdgt            Widget          Widget that activated callback  *
 *      clnt		XtPointer       User selected color		*
 *      *event       	XEvent		Structure of info   	 	*
 *	*ctd		Boolean		Continue to dispatch		*
 *                                                                      *
 * Output parameters:                                                   *
 *	none								*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/97	Initial coding				*
 * W. Li/EAI		10/98	Removed "OK" button			*
 ***********************************************************************/
{    
/*---------------------------------------------------------------------*/
    if (event->xbutton.button == Button1) {
       *_colrVal = (long)clnt;
        XtVaSetValues (_triggerW,
	    	XmNbackground,		NxmColrP_getColorPixel(*_colrVal),
		XmNtopShadowColor,	NxmColrP_getColorPixel(*_colrVal),
		XmNbottomShadowColor,	NxmColrP_getColorPixel(*_colrVal),
		NULL);
        XtUnmanageChild(_colrPalW); 
        NxmColrP_setColor (_txtColorInfo, *_colrVal);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void NxmClrW_exitCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * NxmClrW_exitCb                                                       *
 *                                                                      *
 * This is the call back function for exiting the color picker, for     *
 * both the ok and cancel buttons.                                      *
 *                                                                      *
 * NxmClrW_exitCb  ( wdgt, clnt, call )                        		*
 *                                                                      *
 * Input parameters:                                                    *
 *      wdgt            Widget          Widget that activated callback  *
 *      clnt		XtPointer	N/A				*
 *      call		XtPointer	N/A				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/97	Initial coding				*
 * S. Law/GSC		03/98	Added call to _clrOkF			*
 * W. Li/EAI		10/98	Removed "OK" button			*
 ***********************************************************************/
{
    XtUnmanageChild(_colrPalW); 
} 
