#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "proto_vf.h"

#define OUTLOOK_NCOL	30
#define OUTLOOK_MSG	"Outlook Message"
#define SFCPRG_NCOL	80
#define SFCPRG_MSG	"Surface Prog Message"
#define QPF_NCOL	70
#define QPF_MSG		"QPF Prog Message"
#define HCNTRK_NCOL     80	
#define HCNTRK_MSG	"HCN Track Message"
#define XRAINF_NCOL	70
#define XRAINF_MSG	"Excessive Rainfall Potential Outlook"
#define WXD_NCOL	30
#define WXD_MSG		"WXD Message"
#define WATCH_NCOL	80
#define WATCH_MSG	"Severe Weather Watch"
#define CCFPRD_NCOL	80
#define CCFPRD_MSG	"Collective Convection Forecast Message"
#define WTCHCNL_NCOL	30
#define WTCHCNL_MSG	"Cancel Watch Message"

static Widget  	    _pgprdWin;
static Widget  	    _pgprdSvWin;	/* save popup window */
static Widget       _ctrlForm;
static WidgetList   _ctrlBtns;
static Widget  	    _pgprdTxt;
static Widget  	    _pgprdScroll;
static Widget  	    _fnameTxtW;		/* file name text input widget */

static int  	_iObj;			/* Current Object ID 	*/


/*
 *  private callback functions
 */
void pgprd_ctlBtnCb  ( Widget, long, XtPointer );
void pgprd_svCtlBtnCb( Widget, long, XtPointer );
void pgprd_OlkOkCb   ( Widget, XtPointer, XtPointer );
void pgprd_OlkCnclCb ( Widget, XtPointer, XtPointer );

/************************************************************************
 * nmap_pgprd.c                                                         *
 *                                                                      *
 * This module defines an product popup window.      			*
 *                                                                      *
 * CONTENTS:                                                            *
 *      pgprd_create()        create the product popup window		*
 * 	pgprd_createPopSv()   create the product save popup window	*
 *      pgprd_popup()         pop up the product window			*
 *      pgprd_popdown()       pop down the product window		*
 *   	pgprd_clear()	      clear the product text window		*
 *      pgprd_putstr()	      put string to product text display	*
 *	pgprd_getTxtSiz()     queries the size of the text		*
 *      pgprd_update()	      update product				*
 *                                                                      *
 *      pgprd_isUp()          query if the product window is up 	*
 *                                                                      *
 *      pgprd_ctlBtnCb()      callback for control buttons 		*
 *      pgprd_svCtlBtnCb()    callback for product save to text file	*
 *      pgprd_OlkOkCb()       callback for OK on OUTLOOK generation	*
 *      pgprd_OlkCnclCb()     callback for CANCEL on OUTLOOK generation	*
 ***********************************************************************/

/*=====================================================================*/

Widget pgprd_create ( Widget parent )
/************************************************************************
 * pgprd_create                                                    	*
 *                                                                      *
 * This function creates the product popup window.  			*
 *                                                                      *
 * Widget pgprd_create(parent)                                		*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent      Widget      parent widget               		*
 *                                                                      *
 * Output parameters:                                             	*
 * pgprd_create	Widget	Widget ID of the product popup window		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI     	04/98                                           *
 * E. Safford/GSC	12/98	Add scroll bar policy to placate aix4   *
 * S. Law/GSC		05/99	Added _pgprdScroll			*
 ***********************************************************************/
{
    int		nn;
    Widget	pane;
    char	*btnstrs[] = {"Update", "Save", "Close"};
    Arg		args[6];
/*---------------------------------------------------------------------*/

    /*
     * create dialog shell
     */
    _pgprdWin = XmCreateFormDialog(parent, "pgprd_popup", NULL, 0);
    XtVaSetValues(_pgprdWin, 
		  XmNnoResize,		TRUE, 
		  XmNdefaultPosition,	FALSE, 
		  NULL);
    XtVaSetValues(XtParent(_pgprdWin), XmNtitle, OUTLOOK_MSG, NULL);

    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("pgprd_pane",
			    xmPanedWindowWidgetClass, _pgprdWin,
			    XmNsashWidth,	1,
			    XmNsashHeight,	1,
			    NULL);

    /*
     * message text area 
     */
    nn = 0;
    XtSetArg(args[nn], XmNscrollingPolicy,		XmAUTOMATIC);	nn++;
    XtSetArg(args[nn], XmNscrollBarDisplayPolicy,	XmSTATIC);	nn++;
    XtSetArg(args[nn], XmNeditable,			True);		nn++;
    XtSetArg(args[nn], XmNcolumns,			SFCPRG_NCOL);	nn++;
    XtSetArg(args[nn], XmNrows,				20);		nn++;
    XtSetArg(args[nn], XmNeditMode,			XmMULTI_LINE_EDIT); nn++;
    _pgprdTxt = XmCreateScrolledText(pane, "pgprd_text", args, nn);

    XtVaGetValues (XtParent (_pgprdTxt), XmNverticalScrollBar, 
		   &_pgprdScroll, NULL);
    XtVaSetValues (_pgprdScroll, XmNprocessingDirection, XmMAX_ON_BOTTOM, NULL);

    XtManageChild (_pgprdTxt);

    /*
     * create control buttons
     */
    nn = XtNumber(btnstrs);
    _ctrlBtns = (WidgetList)XtMalloc(nn*sizeof(Widget));
    _ctrlForm = NxmCtlBtn_create(pane, 1, "pgprd_ctlBtn", nn,
				 btnstrs, (XtCallbackProc)pgprd_ctlBtnCb, _ctrlBtns);

    XtManageChild(pane);

    pgprd_createPopSv( pane );

    return(_pgprdWin);
}

/*=====================================================================*/

Widget pgprd_createPopSv ( Widget parent )
/************************************************************************
 * pgprd_createPopSv                                                    *
 *                                                                      *
 * This function creates the save popup window.  			*
 *                                                                      *
 * Widget pgprd_createPopSv(parent)                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent      Widget      parent widget               		*
 *                                                                      *
 * Output parameters:                                             	*
 * pgprd_createPopSv	Widget    Widget ID of the save popup window	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI     04/98                                                 *
 ***********************************************************************/
{
int    n;
Widget pane;
char   *btnstrs[] = {"OK", "Close"};
/*---------------------------------------------------------------------*/

        /*
         * create dialog shell
         */
        _pgprdSvWin = XmCreateFormDialog(parent, "pgprd_popupSave",
                        NULL, 0);
        XtVaSetValues(_pgprdSvWin, 
			XmNnoResize,        True, 
			XmNdefaultPosition, False, 
			NULL);
        XtVaSetValues(XtParent(_pgprdSvWin),
                        XmNtitle, "Product Save",
                        NULL);

        /*
         * create a parent pane widget
         */
        pane = XtVaCreateWidget("pgprd_svpane",
                        xmPanedWindowWidgetClass, _pgprdSvWin,
                        XmNsashWidth,             1,
                        XmNsashHeight,            1,
                        NULL);

        /*
         * file name area 
         */
	NxmTxtIn_create(pane, "File Name", 50, &_fnameTxtW);

        /*
         * create control buttons
         */
	n = XtNumber(btnstrs);
        NxmCtlBtn_create(pane, 1, "pgprd_svctlBtn", n,
                         btnstrs, (XtCallbackProc)pgprd_svCtlBtnCb, NULL);

        XtManageChild(pane);

        return(_pgprdSvWin);

}

/*=====================================================================*/

void pgprd_popup ( void )
/************************************************************************
 * pgprd_popup                                                          * 
 *                                                                      *
 * This function pops up the product popup window.                      *
 *                                                                      *
 * void pgprd_popup()                                                   *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI     	04/98                                           * 
 * F. J. Yen/NCEP 	06/98 	Added QPF obj                           *
 * D.W.Plummer/NCEP	 8/98	Added HCNTRK obj                        *
 * D.W.Plummer/NCEP	 8/98	Added GGCNTR obj                        * 
 * F. J. Yen/NCEP 	09/98 	Added XRAINF obj                        *
 * F. J. Yen/NCEP 	01/99 	Added WXD obj                           * 
 * D.W.Plummer/NCEP	 3/99	Added WATCH obj                         *	
 * S. Law/GSC		04/99	Changed WATCH look, removed _iObjSave   *
 * H. Zeng/EAI          10/99   Removed GGCNTR obj                      *
 * S. Law/GSC		03/00	Added CCF product                       *
 * A. Hardy/GSC		05/00   Added watch cancel product              *
 * A. Hardy/GSC		05/00   Changed HCNTRK text box defaults        *
 * A. Hardy/GSC		05/00   Increased the rows in HCNTRK to 2       *
 * F. J. Yen/NCEP	05/01	Increased the rows in HCNTRK to 5       *
 * A. Hardy/NCEP	 6/04   Changed watch editable window to FALSE	*
 * G. Grosshans/SPC	06/06	XmNrows from 20 to 53 in obj_outlook    *
 *				to assist fcstrs in QC of outlooks      *
 ***********************************************************************/
{
    short	ii, ncol, nbtns;
    char	msg[80], button0[10] = "Update", button2[10] = "Close";
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    if ( pgwfmt_isUp() )  {
	_iObj = OBJ_WATCHFMT;
    }
    else  {
	_iObj = pgpalw_getCurObjId();
    }

    pgprd_popdown ();

    if (pgwfmt_isUp ()) {
	_iObj = OBJ_WATCHFMT;
    }
    else {
	_iObj = pgpalw_getCurObjId();
    }

    nbtns = XtNumber (_ctrlBtns);
    for (ii = 0; ii < nbtns; ii++) {
	XtUnmanageChild (_ctrlBtns[ii]);
    }

    XtUnmanageChild(_ctrlForm);
    XtUnmanageChild(_pgprdTxt);
    XtUnmanageChild(XtParent(_pgprdTxt));

    XtVaSetValues(_pgprdTxt, XmNeditable, TRUE, 
    			     XmNrows, 20, 
			     NULL);
    if ( _iObj == OBJ_OUTLOOK )  {
	ncol = OUTLOOK_NCOL;
	strcpy( msg, OUTLOOK_MSG );
    XtVaSetValues(_pgprdTxt, XmNeditable, TRUE, 
    		      XmNrows, 53, NULL);
    }
    else if ( _iObj == OBJ_SFCPRG )  {
	ncol = SFCPRG_NCOL;
	strcpy( msg, SFCPRG_MSG );
    }
    else if ( _iObj == OBJ_QPF )  {
	ncol = QPF_NCOL;
	strcpy( msg, QPF_MSG );
    }
    else if ( _iObj == OBJ_HCNTRK )  {
	ncol = HCNTRK_NCOL;
	strcpy( msg, HCNTRK_MSG );
        XtVaSetValues(_pgprdTxt, XmNeditable, FALSE, 
    				 XmNrows, 5, 
				 NULL);
    }
    else if ( _iObj == OBJ_XRAINF )  {
	ncol = XRAINF_NCOL;
	strcpy( msg, XRAINF_MSG );
    }
    else if ( _iObj == OBJ_WXD )  {
	ncol = WXD_NCOL;
	strcpy( msg, WXD_MSG );
    }
    else if ( _iObj == OBJ_WATCHFMT )  {
	ncol = WATCH_NCOL;
	strcpy (msg, WATCH_MSG);
	strcpy (button0, "Re-edit");
	strcpy (button2, "Cancel");
        XtVaSetValues(_pgprdTxt, XmNeditable, FALSE, 
    			     XmNrows, 20, 
			     NULL);
    }
    else if ( _iObj == OBJ_CCFPRD )  {
	ncol = CCFPRD_NCOL;
	strcpy( msg, CCFPRD_MSG );
    }
    else if ( _iObj == OBJ_WTCHCNL )  {
	ncol = WTCHCNL_NCOL;
	strcpy( msg, WTCHCNL_MSG );
    }

    XtVaSetValues( _pgprdTxt, XmNcolumns, ncol, NULL);
    XtVaSetValues(XtParent(_pgprdWin), XmNtitle, msg, NULL);

    xmstr = XmStringCreateLocalized (button0);
    XtVaSetValues (_ctrlBtns[0], XmNlabelString, xmstr, NULL);
    XmStringFree (xmstr);

    xmstr = XmStringCreateLocalized (button2);
    XtVaSetValues (_ctrlBtns[2], XmNlabelString, xmstr, NULL);
    XmStringFree (xmstr);

    XtManageChild(_pgprdTxt);
    XtManageChild(XtParent(_pgprdTxt));

    for (ii = 0; ii < nbtns; ii++) {
	XtManageChild (_ctrlBtns[ii]);
    }

    XtManageChild(_ctrlForm);

    XtManageChild(_pgprdWin);
 
    pgprd_update();

}

/*=====================================================================*/

void pgprd_popdown ( void ) 
/************************************************************************
 * pgprd_popdown                                                    	*
 *                                                                      *
 * This function pops down the product popup window. 			*
 *                                                                      *
 * void pgprd_popdown()                     				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		04/98						*
 * S. Law/GSC		04/99	add _pgprdSvWin popdown			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_pgprdWin)) {
	XtUnmanageChild(_pgprdWin);
    }

    if (XtIsManaged (_pgprdSvWin)) {
	XtUnmanageChild(_pgprdSvWin);
    }
}

/*=====================================================================*/

void pgprd_clear ( void )
/************************************************************************
 * pgprd_clear                                                    	*
 *                                                                      *
 * This function clears the product message text. 			*
 *                                                                      *
 * void pgprd_clear( )                     				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI	 4/98							*
 ***********************************************************************/
{
char text[5];
/*---------------------------------------------------------------------*/

	text[0] = '\0';
	XmTextSetString(_pgprdTxt, text);

}

/*=====================================================================*/

void pgprd_putstr ( char *str, int *iret )
/************************************************************************
 * pgprd_putstr								*
 *                                                                      *
 * This function writes a string to the product text display.		*
 *                                                                      *
 * void pgprd_putstr ( str, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  *str	char		String to put to text display		*
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret	int		Return code				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 4/98						*
 ***********************************************************************/
{
XmTextPosition  textpos;
/*---------------------------------------------------------------------*/

    *iret = 0;

    textpos = XmTextGetLastPosition(_pgprdTxt);
    XmTextInsert(_pgprdTxt, textpos, str);

}

/*=====================================================================*/

void pgprd_update ( void )
/************************************************************************
 * pgprd_update								*
 *                                                                      *
 * This function updates the text display.				*
 *                                                                      *
 * void pgprd_update ( )						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                             	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 5/98						*
 * F. J. Yen/NCEP	 6/98	Added QPF obj				*
 * D.W.Plummer/NCEP	 8/98	Added HCNTRK obj			*
 * D.W.Plummer/NCEP	 8/98	Added GGCNTR obj			*
 * F. J. Yen/NCEP	 9/98	Added XRAINF obj			*
 * F. J. Yen/NCEP	 1/99	Added WXD obj				*
 * D.W.Plummer/NCEP	 3/99	Added WATCH obj				*
 * S. Law/GSC		05/99	Added scrollbar adjustment		*
 * H. Zeng/EAI          10/99   Removed GGCNTR obj                      *
 * S. Law/GSC		03/00	Added CCF product			*
 * J. Wu/GSC		05/01	add check for unexpected OUTLOOK ele.	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 ***********************************************************************/
{
    int	iret, value, slider, incr, page, ier;
    char	msgs[256], mesg[256];
/*---------------------------------------------------------------------*/

    if ( _iObj == OBJ_OUTLOOK )  {
	pgolk_check( sizeof(msgs)/sizeof(msgs[0]), msgs, &iret );
	if ( iret > 0 ) {		       
            strcpy(mesg, "WARNING:\n"); 
            strcat(mesg, msgs);	    
	    NxmConfirm_show(_pgprdWin, mesg, (XtCallbackProc)pgprd_OlkOkCb, 
				(XtCallbackProc)pgprd_OlkCnclCb, 
				(XtPointer)NULL, &ier );
	}
	else {
           pgolk_update( cvg_getworkfile(), &iret );
	}
    }
    else if ( _iObj == OBJ_SFCPRG ) {
	pgsfp_update( cvg_getworkfile(), &iret );
    }
    else if ( _iObj == OBJ_QPF ) {
	pgqpf_update( cvg_getworkfile(), &iret );
    }
    else if ( _iObj == OBJ_HCNTRK ) {
	pgtrk_update( cvg_getworkfile(), &iret );
    }
    else if ( _iObj == OBJ_XRAINF ) {
	pgxrain_update( cvg_getworkfile(), &iret );
    }
    else if ( _iObj == OBJ_WXD ) {
	pgwxd_update( cvg_getworkfile(), &iret );
    }
    else if ( _iObj == OBJ_WATCHFMT ) {
	pgwfmt_update( &iret );
    }
    else if ( _iObj == OBJ_CCFPRD ) {
	pgccfp_update( cvg_getworkfile(), &iret );
    }

    XmScrollBarGetValues (_pgprdScroll, &value, &slider, &incr, &page);
    value = 0;
    XmScrollBarSetValues (_pgprdScroll, value, slider, incr, page, TRUE);
}

/*=====================================================================*/

Boolean pgprd_isUp ( void ) 
/************************************************************************
 * pgprd_isUp                                                    	*
 *                                                                      *
 * This function queries whether the product window is up. 		*
 *                                                                      *
 * Boolean pgprd_isUp()                     				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *  pgprd_isUp	Boolean      True -- up, False -- down                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/98  						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	return ( XtIsManaged(_pgprdWin) );

}

/*=====================================================================*/

int pgprd_getTxtSiz ( void )
/************************************************************************
 * pgprd_getTxtSiz                                                 	*
 *                                                                      *
 * This function gets the size of the text in _pgprdTxt text field.     *
 *                                                                      *
 * int pgprd_getTxtSiz( void )		                             	*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:							*
 *			NONE						*
 *                                                                      *
 * Return parameters:                                                   *
 *	pgprd_getTxtSiz	int	the size of the text                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		09/06	initial coding				*
 ***********************************************************************/
{
    int		size;
    char	*text;
/*---------------------------------------------------------------------*/

    text = XmTextGetString(_pgprdTxt);
    size = strlen (text);
    XtFree(text);

    return ( size );

}

/*=====================================================================*/

/* ARGSUSED */
void pgprd_ctlBtnCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * pgprd_ctlBtnCb                                                  	*
 *                                                                      *
 * Callback function for control buttons at the bottom of product   	*
 * popup window.                 					*
 *                                                                      *
 * void pgprd_ctlBtnCb(w, which, cbs)                             	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long       which button                               *
 *  cbs          XtPointer  not used                                   *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/98                                           *
 * C. Lin/EAI      	06/98   use XtFree                              *
 * F. J. Yen/NCEP  	06/98   Aded QPF obj	                        *
 * D.W.Plummer/NCEP	 8/98	Added HCNTRK obj			*
 * D.W.Plummer/NCEP	 8/98	Added GGCNTR obj			*
 * F. J. Yen/NCEP  	09/98	Added XRAINF obj	                *
 * F. J. Yen/NCEP  	01/99   Added WXD obj	                        *
 * F. J. Yen/NCEP       03/99   Unmanaged pgprdSvWin if managed.        *
 * F. J. Yen/NCEP       03/99   Used cvg_getfname and new pgwxd_getfname*
 * D.W.Plummer/NCEP	 3/99	Added WATCHFMT obj			*
 * S. Law/GSC		04/99	Changed effects when WATCH		*
 * S. Schotz/NCEP	 9/99	Changed Hurr. track file to original	*
 * H. Zeng/EAI          09/99   Added pgofmt_popdown() for "CLOSE"      *
 *                              button                                  *
 * H. Zeng/EAI          10/99   Removed GGCNTR obj                      *
 * S. Law/GSC		03/00	Added CCF product			*
 * D.W.Plummer/NCEP	 3/00	Have CCF prod get its filename		*
 * D.W.Plummer/NCEP	 5/01	Split out OBJ_SFCPRG from case statemnt	*
 * G. Grosshans/SPC	04/02	Added help information in XmNtitle     	*
 *				for Outlook generation for ammendments	*
 * H. Zeng/SAIC		04/07	added call to pgofmt_bulkProcessEnd()	*
 ***********************************************************************/
{
    int		iret;
    char	*text, fname[256];
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:     /* UPDATE */
	if (_iObj == OBJ_WATCHFMT) {
	    pghdlb_deselectAll ();
	    pgprd_popdown ();
	}
	else {
	    pgprd_update();
	}
	break;

      case 1:     /* SAVE */
	/*
	 * create the default file name
	 */
	text = XmTextGetString(_pgprdTxt);

	switch (_iObj) {
	  case OBJ_QPF: 
	  case OBJ_XRAINF:
	    /*
	     *  Use generic product generation filename.
	     */
	    cvg_getfname(fname, &iret);
	    break;

	  case OBJ_SFCPRG:
	    pgsfp_getfname(fname, &iret);
	    break;

	  case OBJ_CCFPRD:
	    pgccfp_getfname(fname, &iret);
	    break;

	  case OBJ_OUTLOOK:
            XtVaSetValues(XtParent(_pgprdSvWin),
                        XmNtitle, "Product Save.   If AMD then DDHHMM = original OTLK time.",
                        NULL);
	    pgolk_getfname(text, fname, &iret);
	    break;

	  case OBJ_HCNTRK:
	    pgtrk_getfname(text, fname, &iret);
	    break;

	  case OBJ_WXD:
	    pgwxd_getfname(fname, &iret);
	    break;

	  case OBJ_WATCHFMT:
	    pgwfmt_getfname(fname, &iret);
	    break;
	}

	XtFree(text);

	XtVaSetValues (_fnameTxtW, 
		       XmNvalue,	fname, 
		       XmNeditable,	(_iObj != OBJ_WATCHFMT),
		       NULL);

	XtManageChild(_pgprdSvWin);

	break;

      case 2:     /* CLOSE */
	pgprd_popdown ();

	if (_iObj == OBJ_WATCHFMT) {
	    pgwfmt_popdown ();
	}

        if (_iObj == OBJ_OUTLOOK) {
	    pgofmt_bulkProcessEnd();
            pgofmt_popdown();
        }

	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgprd_svCtlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgprd_svCtlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of product save	*
 * popup window.							*
 *									*
 * void pgprd_svCtlBtnCb (wid, which, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		which button			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		04/98						*
 * C. Lin/EAI		06/98   use XtFree				*
 * W. Li/EAI		05/99	added call to pgprd_popdown()		*
 * S. Law/GSC		05/99	added vgf file save for OBJ_WATCHFMT	*
 * H. Zeng/EAI          12/99   added call to pgwfmt_formatSave()       *
 * D. Plummer/NCEP	03/00	added OBJ_CCFPRD			*
 * A. Hardy/GSC		03/00   added calls to create SPC txt files     *
 * A. Hardy/GSC		03/00   added VFWWCL call			*
 * S. Law/GSC		03/00	added invalid filename check		*
 * A. Hardy/GSC		04/00   added VFWAWN and VFWOUI calls		*
 * A. Hardy/GSC		05/00   added VFWSEL and VFWPWN calls		*
 * J. Wu/GSC		12/00   Replaced cvg_writ() with cvg_writelm()	*
 * J. Wu/GSC		12/00   Replaced cvg_writelm() with cvg_writef()*
 * D.W.Plummer/NCEP	 5/01	added call to pgaofmt_update		*
 * J. Wu/SAIC		09/01	add parentheses around assignment when 	*
 *				used as truth value			*
 * D.W.Plummer/NCEP	12/01	chg pgaofmt_update to pgolk_ptsprod	*
 * D.W.Plummer/NCEP	 3/02	rm pgolk_ptsprod			*
 * E. Safford/SAIC	05/02	param change for pgwfmt_getcontWtch()	*
 * R. Tian/SAIC		05/02	removed vfwwcl				*
 * H. Zeng/EAI          06/02   calculated "issue time" at the very end *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * A. Hardy/NCEP	 2/04   Added call to pglist_createWtchLst 	*
 * H. Zeng/SAIC		09/04	Added call to vfwrep			*
 * T. Piper/SAIC	07/05	Removed calls to vfwawn and vfwpwn	*
 * F. J. Yen/NCEP	 8/05	Process error codes from vfwrep		*
 * S. Danz/AWC		07/06   Update to new cvg_write[f]() parameter  *
 * H. Zeng/SAIC		09/06	save new format surface prog text mess. *
 ***********************************************************************/
{
    int		iret, start, loc, cur_layer, el_layer, orecsz, nrecsz;
    int		flag, ier, err_cd;
    long        ini_siz, curpos, wrk_size;
    char	*text, *fname, *new_fname;
    char	*cntstr, *ptr, *txt_ptr, tblnam[72];
    char        time_str[20], tmp_file[FILE_FULLSZ];
    Boolean	more;    
    FILE	*fp, *wrkfp, *outfp;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
    
    start = -1;  /* Indicating write to the end of file */
    switch(which) {

      case 0:     /* OK */

	fname = XmTextFieldGetString (_fnameTxtW);
	text  = XmTextGetString (_pgprdTxt);

	if (_iObj == OBJ_WATCHFMT) {
	    pgwfmt_popdown ();

            /*
             * Save "issue time", "issue status" and "file name"
             * at this point.
             */
            mcanvw_setCursor(CURS_BUSY);
            pgwfmt_formatSave (FALSE);
            iret = pgwatch_restore ();  
            mcanvw_setCursor(CURS_DEFAULT);

            /*
             * Insert "issue time" and "valid time" into Weather
             * Watch text file at this point.
             */
            pgwfmt_getIssTmStr ( time_str );
            cst_rpst ( text, "XX XX XXXX XXXX", time_str, text, &iret );
            cst_rpst ( text, "XX XX XXXX XXXX", time_str, text, &iret );

            /*
             * Save Weather Watch text file.
             */
	    fp = fopen (fname, "w");
	    if (fp == (FILE *) NULL) {
	        NxmWarn_show (wid, "Invalid filename\n");
	        return;
	    }
	    iret = fputs (text, fp);
	    fclose (fp);
	    if ( text ) XtFree(text);

            /*
             * Do some other things specifically for Watch.
             */
	    if ((text = strchr (fname, '.'))) {
		*text = '\0';
	    }

	    cvg_rdrec (cvg_getworkfile(), (pgactv_getElmLoc ()), &el, &iret);

	    strcat (fname, ".vgf");
	    cvg_crvgf (fname, &iret);
            cvg_writef( &el, start, el.hdr.recsz, fname, FALSE, &loc, &iret);

            /*
             * Create List (county) element from watch.
             */

	     pglist_createWtchLst ( &el, fname );

	    /*
	     *  Create text watch format products (SAW and SEV).
	     */
	    if ((text = strchr (fname, '.'))) {
		*text = '\0';
	    }
	    strcat( fname, ".txt" );

	    /*
	     *  Creating the SPC text products - SAW, SEV, WCL,
	     *  AWN, OUI, SEL and PWN.
	     */

	     vfgttxt ( fname, &iret );
	     if ( iret == 0 ){
	         vfwsaw  ( &iret );
	         vfwsev  ( &iret );
	         vfwoui  ( &iret );

		/*
		 * Getting continuing watch numbers.
		 */
		 pgwfmt_getcontWtch(&cntstr);

		 vfwsel ( cntstr, &iret );
		 vfwrep ( &iret );
		 if ( iret != 0 ) {
		     strcpy ( tblnam, "woudef.tbl" );
		     if ( (iret & 1) == 1 ) {
			 err_cd = 4;
			 er_wmsg ("CTB", &err_cd, tblnam, &ier,
					strlen("CTB"), strlen(tblnam));
			 NxmErr_update();
		     }
		     if ( (iret & 2) == 2 ) {
			 err_cd = 5;
			 er_wmsg ("CTB", &err_cd, tblnam, &ier,
					strlen("CTB"), strlen(tblnam));
			 NxmErr_update();
		     } 
		     if ( (iret & 4) == 4 ) {
			 err_cd = 6;
			 er_wmsg ("CTB", &err_cd, tblnam, &ier,
					strlen("CTB"), strlen(tblnam));
			 NxmErr_update();
		     } 
		     if ( (iret & 8) == 8 ) {
			 err_cd = 7;
			 er_wmsg ("CTB", &err_cd, tblnam, &ier,
					strlen("CTB"), strlen(tblnam));
			 NxmErr_update();
		     } 
		     if ( (iret & 16) == 16 ) {
			 err_cd = -25;
			 er_wmsg ("GG", &err_cd, NULL, &ier,
					strlen("GG"), 0 );
			 NxmErr_update();
		     } 
		     if ( (iret & 32) == 32 ) {
			 err_cd = -24;
			 er_wmsg ("GG", &err_cd, NULL, &ier,
					strlen("GG"), 0 );
			 NxmErr_update();
		     } 
		 }

		 if ( cntstr )  free(cntstr);
	     }
	}
        else if (_iObj == OBJ_OUTLOOK) {

            if( pgofmt_isUp() ) pgofmt_popdown();

            /*
             * Save Outlook Message text file.
             */
	    fp = fopen (fname, "w");
	    if (fp == (FILE *) NULL) {
	        NxmWarn_show (wid, "Invalid filename\n");
	        return;
	    }
	    iret = fputs (text, fp);
	    fclose (fp);
	    if ( text ) XtFree(text);

            /*
             * Do some other things specifically for Outlook.
             */
	    if ((text = strchr (fname, '.'))) {
		*text = '\0';
	    }
	    strcat( fname, ".vgf" );

            /*
             * Save the vgf elements of the current file into fname.
             */
            cur_layer = pglayer_getCurLayer ();

	    cvg_crvgf (fname, &ier);
	    cfl_inqr (fname, NULL, &ini_siz, tmp_file, &ier);
	    cvg_open (fname, G_TRUE, &outfp, &ier);        
	    cfl_seek (outfp, ini_siz, SEEK_SET, &ier);

	    more = TRUE;
	    cfl_inqr (cvg_getworkfile(), NULL, &wrk_size, tmp_file, &ier);
	    cvg_open (cvg_getworkfile(), G_FALSE, &wrkfp, &ier);
       	
	    curpos = sizeof(el.hdr) + sizeof(el.elem.fhed);
	    while ( ( more ) && ( curpos < wrk_size ) ) {

	        el_layer = crg_getLayer (curpos);
	        cvg_rdhdr (cvg_getworkfile(), wrkfp, curpos, (int)wrk_size, 
			   &el, &flag, &ier);
	    
	        if ( ier == 0 && el.hdr.recsz > 0 ) { 
	        
		    orecsz = el.hdr.recsz;
	            cvg_rdele (&el, curpos, el.hdr.recsz, wrkfp, &ier);

	            if ( ier != 0 ) {
	                more = G_FALSE;
	            }

	            /*
	             * Skip non-current-layer elements, deleted elements,  
		     * and file-head element.
 	             */
	            if ( ( more ) && ( el.hdr.delete == 0 ) &&
		         ( el_layer == cur_layer ) &&
	    	         ( el.hdr.vg_type != FILEHEAD_ELM ) )  {

	                 nrecsz = el.hdr.recsz;		       
		         cvg_write (&el, -2, nrecsz, outfp, FALSE, &ier);
		    }	                       
	            curpos += orecsz;  /* re-position in WORK_FILE */
	        }	    
	    
	        else {
		    more = G_FALSE;
	        }
	    	    
            }  /* End of "while" loop */
	       
	    /* 
             * Close both files.
             */
	    cvg_clos (wrkfp, &ier);
	    cvg_clos (outfp, &ier);

	    /*
	     * If bulk processing flag is on, continue to the next layer
	     * for new outlook message.
	     */
	    if ( pgofmt_bulkProcessOn() )  pgofmt_bulkProcessNext();

        }
	else if ( _iObj == OBJ_SFCPRG ) {

            /*
             * First save product text file.
             */
	    fp = fopen (fname, "w");
	    if (fp == (FILE *) NULL) {
	        NxmWarn_show (wid, "Invalid filename\n");
	        return;
	    }
	    iret = fputs (text, fp);
	    fclose (fp);
	    if ( text ) XtFree(text);

	    /*
	     * Secondly save the new format text message 
	     * into a new file.
             */
	    G_MALLOC(new_fname, char, strlen(fname)+15, 
		     "new_fname malloc failure");
	    strcpy ( new_fname, fname );
            ptr = strstr ( new_fname, ".dat" );
	    sprintf ( ptr, "_hires.dat" );

	    fp = fopen (new_fname, "w");
	    if (fp == (FILE *) NULL) {
	        NxmWarn_show (wid, "Invalid filename\n");
	        return;
	    }
	    txt_ptr = pgsfp_getNewFormatTxt ();
	    iret = fputs (txt_ptr, fp);
	    fclose (fp);
	    G_FREE(new_fname, char);

        }
        else {

            /*
             * Save product text file.
             */
	    fp = fopen (fname, "w");
	    if (fp == (FILE *) NULL) {
	        NxmWarn_show (wid, "Invalid filename\n");
	        return;
	    }
	    iret = fputs (text, fp);
	    fclose (fp);
	    if ( text ) XtFree(text);

        }

	if ( fname != NULL ) XtFree(fname);

	break;

      case 1:     /* CLOSE */

        if (_iObj == OBJ_OUTLOOK)  pgofmt_bulkProcessEnd();

	break;

    }

    pgprd_popdown ();
}

/*=====================================================================*/
/* ARGSUSED */
void pgprd_OlkOkCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgprd_OlkOkCb							*
 *									*
 * Callback function for OK the generation of OUTLOOK.			*
 *									*
 * void pgprd_OlkOkCb (wid, clnt, cbs)					*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	client data			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		5/01	create					*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 ***********************************************************************/
{
    int		iret;
/*---------------------------------------------------------------------*/

    /* 
     * OK -> generate/update the OUTLOOK information           
     */
    pgolk_update( cvg_getworkfile(), &iret );

}

/*=====================================================================*/
/* ARGSUSED */
void pgprd_OlkCnclCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgprd_OlkCnclCb							*
 *									*
 * Callback function for "CANCEL" the generation of OUTLOOK. 		*
 *									*
 * void pgprd_OlkCnclCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	client data			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		 5/01	create					*
 * H. Zeng/SAIC		04/07	added call to pgofmt_bulkProcessEnd	*
 ***********************************************************************/
{
      
    /* 
     * CANCLE -> close window and return to PG pallette 
     */
    pgofmt_bulkProcessEnd (); 
    if ( XtIsManaged (_pgprdWin) ) {
    
	XtUnmanageChild (_pgprdWin);
    }
	     
}

/*=====================================================================*/
