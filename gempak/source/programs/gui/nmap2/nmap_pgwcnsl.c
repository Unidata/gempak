#include "geminc.h"
#include "gemprm.h"
#include "proto_vf.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "pgcmn.h"

static  char    _watchcan[6];

static	Widget	_mainForm;
static	Widget	_wholeWatchPane;
static	Widget	_wholeWatchText;
static	Widget	_dividedWatchPane;


/*
 * private functions -- callback
 */
void pgwcnsl_ctlBtnCb ( Widget, long, XtPointer );
void pgwcnsl_watchCb ( Widget, XtPointer, XtPointer );

/*
 * private functions
 */
void pgwcnsl_createCNL ( void );


/************************************************************************
 * nmap_pgwcnsl.c							*
 *									*
 * This module defines everything for watch cancel text file formatting.*
 *									*
 * CONTENTS:								*
 *	pgwcnsl_create()	creates the popup window		*
 *	pgwcnsl_popup()		manages the popup window		*
 *	pgwcnsl_popdown()	unmanages the popup window		*
 *									*
 *	pgwcnsl_isUp()		query whether the window is up		*
 *									*
 *	pgwcnsl_watchCb()	callback for watch widget		*
 *	pgwcnsl_ctlBtnCb()	callback for control buttons		*
 *									*
 *	pgwcnsl_createCNL()	generates the watch cancel text files	*
 ***********************************************************************/

/*=====================================================================*/

void pgwcnsl_create ( Widget parent )
/************************************************************************
 * pgwcnsl_create							*
 *									*
 * This function creates the watch cancel selection box.		*
 *									*
 * void pgwcnsl_create (parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		05/00	initial coding				*
 * T. Piper/GSC		07/01	freed flentry				*
 * J. Wu/SAIC		05/02	verify the input to watch number	*
 * E. Safford/SAIC	05/05	free fontlist				*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    int		voff = 5, hoff = 3;
    long	ii, nn;
    int		wnum_size = 5;
    char	*ctlstrs[] = {"FORMAT", "CANCEL"};
    char	fontname[] = "-adobe-courier-bold-r-normal-*-*-120-*-*-m-*-*-*";

    Widget	pane, label1, button, rc1; 
    XmString	xmstr;
    Display	*dsp;
    XmFontListEntry flentry;
    XmFontList	fontlist;
/*---------------------------------------------------------------------*/
/*
* Create main form
*/
    _mainForm = XmCreateFormDialog (parent, "cnsl_format", NULL, 0);
    xmstr = XmStringCreateLocalized("WATCH CANCEL");

    XtVaSetValues(_mainForm,
		  XmNnoResize,		TRUE,
		  XmNautoUnmanage,	FALSE,
		  XmNdialogTitle,	xmstr,
		  NULL);

    XmStringFree(xmstr);

    dsp = XtDisplay (_mainForm);
    flentry  = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry); 
    pane = (Widget) XtVaCreateManagedWidget ("cnsl_pane",
			xmPanedWindowWidgetClass, 	_mainForm,
			XmNsashWidth,			1,
			XmNsashHeight,	 		1,
			XmNleftAttachment,  		XmATTACH_FORM,
			XmNrightAttachment, 		XmATTACH_FORM,
			NULL);

    XmFontListFree( fontlist );

/**************************
 * whole watch pane area
 **************************/
    _wholeWatchPane = 
	(Widget) XtVaCreateManagedWidget ("cnsl_wholeform",
					  xmFormWidgetClass,	pane,
					  XmNverticalSpacing,	voff,
					  NULL);

    xmstr = XmStringCreateLocalized("Cancel Watch ");
    label1 = 
	(Widget) XtVaCreateManagedWidget ("cnsl_wholelabel1",
					  xmLabelWidgetClass, _wholeWatchPane,
					  XmNlabelString, xmstr,
					  NULL);
    XmStringFree(xmstr);

    _wholeWatchText = 
	(Widget) XtVaCreateManagedWidget ("cnsl_wholetext",
					  xmTextWidgetClass, _wholeWatchPane,
					  XmNcolumns,	     wnum_size,
					  XmNeditable,	     TRUE,
                                          XmNcursorPositionVisible,  
                                                             TRUE,
                                          XmNtraversalOn,    TRUE,
					  XmNleftAttachment, XmATTACH_WIDGET,
					  XmNleftWidget,     label1,
					  NULL);
    XtAddCallback(_wholeWatchText, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_wholeWatchText, XmNactivateCallback, 
			pgwcnsl_watchCb, (XtPointer) NULL);
    XtAddCallback (_wholeWatchText, XmNlosingFocusCallback, 
			pgwcnsl_watchCb, (XtPointer) NULL);

/**************************
 * divided watch pane area
 **************************/
    _dividedWatchPane = 
	(Widget) XtVaCreateManagedWidget ("cnsl_dividedform",
					  xmFormWidgetClass,	pane,
					  XmNverticalSpacing,	voff,
					  NULL);

    XtUnmanageChild (_dividedWatchPane);

/**************************
 * main pane area
 **************************/
    _watchcan[0] = '\0' ;
    nn = XtNumber (ctlstrs) * 100;
    rc1 = 
	(Widget) XtVaCreateManagedWidget ("cnsl_mainform",
					  xmFormWidgetClass,	pane,
					  XmNverticalSpacing,	voff,
					  XmNfractionBase,	nn,
					  NULL);

/*
 * control buttons
 */
    nn = XtNumber (ctlstrs);
    for (ii = 0; ii < nn; ii++) {
	button = XtVaCreateManagedWidget 
	    (ctlstrs[ii], xmPushButtonWidgetClass, rc1, 
	     XmNtopAttachment,		XmATTACH_WIDGET,
	     XmNtopWidget,		rc1,
	     XmNleftAttachment,		XmATTACH_POSITION,
	     XmNleftPosition,		((ii * 100) + hoff),
	     XmNrightAttachment,	XmATTACH_POSITION,
	     XmNrightPosition,		(((ii + 1) * 100) - hoff),
	     NULL);

	XtAddCallback (button, XmNactivateCallback, (XtCallbackProc)pgwcnsl_ctlBtnCb, 
		       (XtPointer) ii); 
	XtUnmanageChild (_mainForm);
    }
}

/*=====================================================================*/

void pgwcnsl_popup ( void )
/************************************************************************
 * pgwcnsl_popup							*
 *									*
 * This function manages the CNL popup.					*
 *									*
 * void pgwcnsl_popup ()						*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		05/00	initial coding				*
 ***********************************************************************/
{
    pgwcnsl_popdown ();
    XtSetSensitive (_mainForm, TRUE);
    XtManageChild (_mainForm);
}

/*=====================================================================*/

void pgwcnsl_popdown ( void )
/************************************************************************
 * pgwcnsl_popdown							*
 *									*
 * This function unmanages the CNL popup.				*
 *									*
 * void pgwcnsl_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		05/00	initial coding				*
 ***********************************************************************/
{
    XtUnmanageChild (_mainForm);
}

/*=====================================================================*/

Boolean pgwcnsl_isUp ( void )
/************************************************************************
 * pgwcnsl_isUp								*
 *									*
 * This function returns a boolean value specifying whether the WSM   	*
 * dialog is managed or not.						*
 *									*
 * Boolean pgwcnsl_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 *	pgwcnsl_isUp		Boolean		Is/is not managed	*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		05/00	Copied from pgsigw_isUp			*
 ***********************************************************************/
{
    return (XtIsManaged (_mainForm));
}

/*=====================================================================*/
/* ARGSUSED */
void pgwcnsl_watchCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgwcnsl_watchCb							*
 *									*
 * Callback function for the watch text widget.				*
 *									*
 * void pgwcnsl_watchCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	client data			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		05/00	initial coding				*
 * A. Hardy/NCEP	05/04	changed watch number check 1000->10000  *
 ***********************************************************************/
{
    int		wnum, lens, ier;
    char	*ptxt, tmpstr[6];
/*---------------------------------------------------------------------*/

    wnum = 0;
    ptxt = XmTextGetString (_wholeWatchText);

    sscanf (ptxt, "%d", &wnum);

    XtFree (ptxt);
    sprintf (tmpstr, "%04d", wnum);
    cst_lstr (tmpstr, &lens, &ier);
    if ( ( lens <= 4 ) && ( ( wnum > 0) && ( wnum < 10000 ) ) ) {
       strcpy ( _watchcan, tmpstr);
    }
    else {
       strcpy ( _watchcan, "0000");
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwcnsl_ctlBtnCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgwcnsl_ctlBtnCb							*
 *									*
 * Callback function for the control buttons.				*
 *									*
 * void pgwcnsl_ctlBtnCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		int		which button			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		05/00	initial coding				*
 ***********************************************************************/
{
    switch (clnt) {
      case 0:	/* FORMAT */

        pgwcnsl_createCNL ();
	break;

      case 1:	/* CANCEL */
	pgwcnsl_popdown ();
	break;
    }
}

/*=====================================================================*/

void pgwcnsl_createCNL ( void )
/************************************************************************
 * pgwcnsl_createCNL							*
 *									*
 * This function creates the actual watch cancel text messages.		*
 *									*
 * void pgwcnsl_createCNL ()						*
 *									*
 * Input parameters:							*
 *	None								*
 * Output parameters:							*
 *	None								*
 **									*
 * Log:									*
 * A. Hardy/GSC		05/00	initial coding				*
 * J. Wu/SAIC		08/01	fix the unmatched type warning		*
 ***********************************************************************/
{
    int         iret;
    char	fname[130], newname[130];
    char	warning[30];
/*---------------------------------------------------------------------*/

    iret = 0;

    strcpy ( newname, "ww");
    strcat ( newname, _watchcan);
    strcat ( newname, ".txt" );
    strcpy ( fname, newname);

    vfgttxt ( fname, &iret );

    if ( iret == 0 ) {
        vfcnsaw ( &iret );
        vfcnsel ( &iret );

	XtUnmanageChild (_mainForm);
    }
    else {
	sprintf(warning, "Watch text file not found.");

	NxmWarn_show (_mainForm, warning); 
    }
}
