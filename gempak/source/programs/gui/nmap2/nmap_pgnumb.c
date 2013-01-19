#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmTxt.h"
#include "vgstruct.h"
#include "drwids.h"
#include "hints.h"
#include "proto_nmaplib.h"


#define _one 1
#define	 FORM_WIDTH 150
#define	 FORM_HEIGH 350
#define	 MAX_SETTING 20

static Widget		_numbEditW, _numbTxtW, _numbRowColW;
static Widget      	_ctlFrom; 
static Widget		_ctlBtns[2];

static Widget		_subRowColW;
static int		_numbValu = 1;


/*
 *	Private Callback functions
 */

void pgnumb_arrowCb 	( Widget, long, XtPointer );
void pgnumb_desExitBtCb ( Widget, XtPointer, XtPointer );
void pgnumb_setPbCb 	( Widget, long, XtPointer );
void pgnumb_valueTxtCb 	( Widget, XtPointer, XtPointer );


/************************************************************************
 * nmap_pgnumb.c							*
 *									*
 * This file contains the subroutines necessary to build and operate	*
 * the number edit popup.						*
 *									*
 * CONTENTS:								*
 *	pgnumb_create		- creates the number edit window	*
 *	pgnumb_popup		- manages the number edit window	*
 *	pgnumb_popdown		- unmanages the number edit window	*
 *									*
 *	pgnumb_setNumb		- sets the  number values       	*
 *	pgnumb_updateBtns	- sets the deselect button sensitive	*
 *	pgnumb_getNumb		- returns the new number value		*
 *	pgnumb_isUp		- return status of number edit window	*
 *									*
 *	pgnumb_arrowCb		- call back function for arrow buttont	*
 *	pgnumb_desExitBtCb	- call back function for de/exit button	*
 *	pgnumb_valueTxtCb	- call back function for text widget	*
 *	pgnumb_setPbCb		- call back function for setting widget *
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		12/98	Initial coding.  Moved from NxmTxtA.c	*
 * W. Li/EAI		03/99	Removed change type and call back	*
 ***********************************************************************/


/*=====================================================================*/

void pgnumb_create ( Widget parent_w )
/************************************************************************
 * pgnumb_create							*
 *									*
 * This function creates the number dialog box for the edit number	*
 *									*
 * void pgnumb_create (parent_w)					*
 *									*
 * Input parameters:							*
 *	parent_w	Widget	Parent widget				*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		12/98	Initial coding, Moved from NxmTxtA.c	*
 * E. Safford/GSC	12/98	modify startup position of window	*
 * W. Li/EAI		03/99	added value setting for Inc/Dec		*
 ***********************************************************************/
{
    Widget      form,		sub_form,	pane;   
    Widget	arrow_up,	arrow_down,	frame[2];	
    XmString  	title_string;
/*---------------------------------------------------------------------*/

    _numbEditW = XmCreateFormDialog(parent_w, "numb_edit", NULL, 0);
    title_string = XmStringCreateLocalized ("Inc/Dec Editor");

    XtVaSetValues(_numbEditW, 
		XmNdefaultPosition,		FALSE,
		XmNx,				8,
		XmNy,				650,
		XmNdialogTitle,			title_string,
		XmNautoUnmanage,		False, 	
		XmNnoResize,                    True, 
		NULL);
    XmStringFree (title_string);

    form    =  XtVaCreateWidget ("form",
		xmFormWidgetClass,          	_numbEditW,   
		XmNtopAttachment,               XmATTACH_FORM,
                XmNtopOffset,                   10,
		XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  5,
		XmNrightAttachment,             XmATTACH_FORM,
                XmNrightOffset,                 5,
		XmNbottomAttachment,            XmATTACH_FORM,
                XmNbottomOffset,                15,

	  	NULL);
	
    frame[0]  =  XtVaCreateWidget ("frame[0]", 
                xmFrameWidgetClass,             form,
                NULL);

    pane    =  XtVaCreateWidget ("pane",
		xmPanedWindowWidgetClass,   	frame[0],
		XmNsashWidth,	 	    	1,
		XmNsashHeight,		    	1,	
		NULL);

    frame[1]  =  XtVaCreateWidget ("frame[1]", 
                xmFrameWidgetClass,             pane,
                NULL);

    sub_form = XtVaCreateWidget ("sub_form",
		xmFormWidgetClass,          	frame[1],
	  	NULL);	

    /*
     *  create row column widget for  input
     */

    _numbRowColW = XtVaCreateManagedWidget("_numbRowColW",
                xmRowColumnWidgetClass,		sub_form,
                XmNorientation,			XmHORIZONTAL,
		XmNpacking,			XmPACK_TIGHT,
		XmNnumColumns,			2,		
                NULL);

    /*
     *  create row column widget for arrows and text input
     */


    _subRowColW = XtVaCreateManagedWidget("_subRowColW",
            xmRowColumnWidgetClass,		_numbRowColW,
            XmNorientation,			XmHORIZONTAL,
	    XmNpacking,				XmPACK_TIGHT,
	    XmNnumColumns,			3,		
            NULL);

    /*
     *    number text field
     */
 

    arrow_up = XtVaCreateManagedWidget("numb_arrowup",
                xmArrowButtonWidgetClass,   	_subRowColW,
                XmNarrowDirection,          	XmARROW_UP,
                NULL);	

    XtAddCallback(arrow_up, XmNactivateCallback,
                   (XtCallbackProc)pgnumb_arrowCb, (XtPointer)0);


    arrow_down = XtVaCreateManagedWidget("dataw_arrowdown",
                xmArrowButtonWidgetClass,   	_subRowColW,
                XmNarrowDirection,          	XmARROW_DOWN,
                NULL);
    XtAddCallback(arrow_down, XmNactivateCallback,
                   (XtCallbackProc)pgnumb_arrowCb, (XtPointer)1);


    _numbTxtW = XtVaCreateManagedWidget ("_numbTxtW",
		xmTextFieldWidgetClass,	    	_subRowColW,
		XmNcolumns,		    	3,
		XmNvalue, 		    	"1",
		XmNcursorPositionVisible,   	True,
		NULL);

    XtAddCallback(_numbTxtW, XmNvalueChangedCallback, 
                  (XtCallbackProc)pgnumb_valueTxtCb, NULL);


    /*
     *  deselect & Exit  buttons
     */


    _ctlFrom  = XtVaCreateManagedWidget( "_ctlFrom",
        	 xmFormWidgetClass,             pane,
        	 XmNleftAttachment,             XmATTACH_FORM,
        	 XmNrightAttachment,            XmATTACH_FORM,		
        	 NULL);

    _ctlBtns[0] = XtVaCreateManagedWidget("Deselect",
             xmPushButtonWidgetClass,  		_ctlFrom,
             XmNleftAttachment,              	XmATTACH_FORM,
             XmNrightAttachment,            	XmATTACH_FORM,
	     XmNtopAttachment,              	XmATTACH_FORM,
             XmNtopOffset,                   	10,
             NULL);

     XtAddCallback(_ctlBtns[0], XmNactivateCallback,
		   (XtCallbackProc)pgnumb_desExitBtCb, (XtPointer)0);


    _ctlBtns[1] = XtVaCreateManagedWidget("EXIT",
            xmPushButtonWidgetClass,  		_ctlFrom, 
	    XmNtopAttachment,       		XmATTACH_WIDGET,
	    XmNtopWidget,           		_ctlBtns[0],
            XmNtopOffset,                   	10,
            XmNleftAttachment,              	XmATTACH_FORM,
            XmNrightAttachment,             	XmATTACH_FORM,
            NULL);

    XtAddCallback(_ctlBtns[1], XmNactivateCallback,
		  (XtCallbackProc)pgnumb_desExitBtCb, (XtPointer)1);

    XtManageChild (sub_form);
    XtManageChild (frame[0]);
    XtManageChild (frame[1]);
    XtManageChild (form); 
    XtManageChild (pane); 
 
}

/*=====================================================================*/

void pgnumb_popup ( void )
/************************************************************************
 * pgnumb_popup                                                        	*
 *									*
 * This function popup the number edit window only			*
 *									*
 * void pgnumb_popup ()							*
 *									*
 * Input Parameters:							*
 *	none								*
 * Output Parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		12/98	Initial coding 				*
 * E. Safford/GSC	12/98 	update for renamed _updateBtns function *
 * W. Li/EAI		03/99	added keyboard focus to text field	*
 * M. Li/SAIC		01/04	Added a check for deselect state	*
 ***********************************************************************/
{
    Widget		drawingw;
    Boolean		dselbtn;
/*---------------------------------------------------------------------*/
    /*
     * set key board focus to text field 
     */

    drawingw = (Widget)mcanvw_getDrawingW();
    XtSetKeyboardFocus(drawingw, _numbTxtW);

    XmTextSetInsertionPosition(_numbTxtW, 
  	XmTextGetLastPosition (_numbTxtW));

    dselbtn = (pghdlb_elemSelected() > 0) ? True : False;
    pgnumb_updateBtns ( dselbtn );

    XtManageChild(_numbEditW);
    XtManageChild(_numbTxtW); 
}

/*=====================================================================*/
/* ARGSUSED */
void pgnumb_arrowCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pgnumb_arrowCb                                                 	*
 *                                                                      *
 * This is the callback function for number arrow button.          	*
 *                                                                      *
 *                                                                      *
 * void pgnumb_arrowCb (w, which, call )                          	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w       	Widget      widget ID                          		*
 *  which   	long         widget button                               *
 *  call	XtPointer   resource                                    *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NULL                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI      11/98      Initial coding 				*
 * W. Li/EAI      03/99      added real value and excluded "0"		*
 ***********************************************************************/
{
    int		new_numb;
    char	*txt; 
/*---------------------------------------------------------------------*/


    XtVaGetValues (_numbTxtW, XmNvalue, &txt, NULL);
    new_numb = atoi(txt);

    switch( which ) {
            case 0:                         /* arrow up */
		    if (new_numb == -1) {
			new_numb = 0;
		    }
                    new_numb++;
                    break;

            case 1:                         /* arrow down */
		    if (new_numb == 1) {
			new_numb = 0;
		    }
		    new_numb--;
                    break;
    }

    pgnumb_setNumb(new_numb);
}

/*=====================================================================*/

void pgnumb_setNumb ( int new_numb )
/************************************************************************
 * pgnumb_setNumb                                                      	*
 *                                                                      *
 * This function sets the number value.                                 *
 *                                                                      *
 * void pgnumb_setNumb ( new_numb )                                   	*
 *                                                                      *
 * Input Parameters:                                                    *
 *	new_numb		int	new number value		*
 *                                                                      *
 * Output Parameters:                                                   *
 *     None                                                             *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	11/98	Initial coding	 				*
 * W. Li/EAI	03/99	added position of insertion cursor		*
 ***********************************************************************/
{
    char	txtstr[5];
/*---------------------------------------------------------------------*/

	sprintf (txtstr, "%i", new_numb);
	if (new_numb == 0)
	    return;

	XmTextFieldSetString (_numbTxtW, txtstr);

	XmTextSetInsertionPosition(_numbTxtW, 
  	    XmTextGetLastPosition (_numbTxtW));

	_numbValu = new_numb;
}

/*=====================================================================*/

void pgnumb_popdown ( void )
/************************************************************************
 * pgnumb_popdown							*
 *									*
 * This function unmanages the number popup window			*
 *									*
 * void pgnumb_popdown ()						*
 *									*
 * Input Parameters:							*
 *		None							*
 *									*
 * Output Parameters:							*
 *		None							*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI	11/98	Initial coding	 				*
 * W. Li/EAI	03/99	added keyboard focus to text field		*
 * J. Wu/SAIC	09/01	add parentheses around assignment when used as	*
 *			truth value					*
 ***********************************************************************/
{
    Widget	drawingw;
/*---------------------------------------------------------------------*/
    if ((drawingw = (Widget)mcanvw_getDrawingW())){
        XtSetKeyboardFocus(drawingw, NULL);
    }

    if (XtIsManaged (_numbEditW))
	XtUnmanageChild (_numbEditW);
}

/*=====================================================================*/

void pgnumb_getNumb ( int *new_numb )
/************************************************************************
 * pgnumb_getNumb                                                  	*
 *                                                                      *
 * This function gets the new number value.                          	*
 *                                                                      *
 * void pgnumb_getNumb ( new_numb )                   	             	*
 *                                                                      *
 * Input Parameters:                                                    *
 *     None                                                             *
 * Output Parameters:                                                   *
 *     *new_numb	int 	new number value			*
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		12/98		Initial coding			*
 ***********************************************************************/
{
    *new_numb = _numbValu;
}

/*=====================================================================*/
/* ARGSUSED */
void pgnumb_desExitBtCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgnumb_desExitBtCb                                                 	*
 *                                                                      *
 * Callback for the deselect/exit buttons on the number edit windows	*
 *                                                                      *
 * void pgnumb_desExitBtCb ( w, clnt, call)                    	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget          Calling widget                          *
 *  clnt	XtPointer       Point to client data			*
 *  call	XtPointer       callback		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		11/98	Initial coding				*
 * E. Safford/GSC	12/98	update to use renamed _updateBtns func  *
 * W. Li/EAI		01/99	added call to pgpalw_unmanageObjPal	*
 * H. Zeng/EAI          05/00   added pgnumb_popdown()                  *
 ***********************************************************************/
{      

    if ((long)clnt == 0) {  
	pghdlb_deselectAll();
	pgnumb_updateBtns ( FALSE );
    }
    else if ((long)clnt == 1){ 
	pgedit_multiEditCb( NULL, (XtPointer)_one, NULL);

        if( pgnumb_isUp() ) {
          pgnumb_popdown();
        }
 
	pgpalw_setCurBtns (FUNC_SELECT, CLASS_ANY, -1);
	pgpalw_unmanageObjPal();
	pgpalw_setupOper ();
    }
}

/*=====================================================================*/

Boolean pgnumb_isUp ( void )
/************************************************************************
 * pgnumb_isUp 								*
 *                                                                      *
 * This function returns the current status of the number edit window. 	*
 *                                                                      *
 * Boolean pgnumb_isUp ( )		                                *
 *                                                                      *
 * Input Parameters:                                                    *
 * Output Parameters:                                                   *
 * pgnumb_isUp		Boolean		True if the window is active	*
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	12/98							*
 ***********************************************************************/
{
    return ( XtIsManaged(_numbEditW) );
}

/*=====================================================================*/

void pgnumb_updateBtns ( Boolean btn_sensitive )
/************************************************************************
 * pgnumb_updateBtns 	                                                *
 *                                                                      *
 *  This function set the deselect button on the number edit windows	*
 *                                                                      *
 * void pgnumb_updateBtns ( btn_sensitive)		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   	btn_sensitive	Boolean	    					*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		11/98	Initial coding				*
 * E. Safford/GSC	12/98	rename to updateBtns & rename params	*
 * E. Safford/GSC	12/98	change to MMHINT_EXIT               	*
 ***********************************************************************/
{
    XtSetSensitive(_ctlBtns[0], btn_sensitive);

    if ( btn_sensitive ) {
	mbotw_mouseSet( LMHINT_TOGGLE, MMHINT_APPLY);
    }
    else {
 	mbotw_mouseSet( LMHINT_TOGGLE, MMHINT_EXIT);   
    }

}
/*=====================================================================*/
/* ARGSUSED */
void pgnumb_valueTxtCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgnumb_valueTxtCb							*
 *                                                                      *
 * Callback for text field widget.                             		*
 *                                                                      *
 * void pgnumb_valueTxtCb( w, clnt, call)                  	*
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          Widget ID                           *
 *   clnt    XtPointer	    client data				*
 *   call     XtPointer	    callback struct 	                *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	03/99							*
 ***********************************************************************/
{
char		*s;
int		numb_value;
/*---------------------------------------------------------------------*/

	s = XmTextGetString(_numbTxtW);
	numb_value = (int)atoi(s);
	_numbValu = numb_value;
	XtFree(s);

}

/*=====================================================================*/
/* ARGSUSED */
void pgnumb_setPbCb ( Widget w, long clnt, XtPointer call)
/************************************************************************
 * pgnumb_setPbCb							*
 *                                                                      *
 * Callback for setting push button widget.                             *
 *                                                                      *
 * void pgnumb_setPbCb( w, clnt, call)                  	*
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          Widget ID                           *
 *   clnt    int		    client data				*
 *   call     XtPointer callback struct                *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	03/99							*
 ***********************************************************************/
{
    int		numb_value;
/*---------------------------------------------------------------------*/

    numb_value = (int)clnt - 9;
    if (numb_value != 0) {
       pgnumb_setNumb (numb_value);
    }
}
