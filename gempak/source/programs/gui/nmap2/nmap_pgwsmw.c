#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "pgcmn.h"

#define NL 	       "\n"	/* end-of-line for intermediate text product */
#define EOL	       "\n"	/* end-of-line for final text product	     */

#define MAXWATCHLEN	( 1024 )		/* max length of watch text */
#define MAXENHLEN	( 15000)
#define MAXFROMLEN	( 256 )
#define MAXSTLEN	(  80 )
#define MAXWFOLEN	(  80 )
#define FCSTRNAMLEN     (  17 )

#define EXPIRE_TB	(   0 )
#define DISCUSS_TB	(   1 )

#define ITIME_RND	(   5 )		/* minutes to round issue time */
#define ETIME_INTV	(  60 )		/* minutes between issue and end time */
#define ETIME_RND	(  15 )		/* minutes to round end time */

#define FORECASTERS_TBL	"forecasters.tbl" /* table w/forecaster names */

#define CHG_YR	2001
#define CHG_MO  11
#define CHG_DA  7
#define CHG_TM  1200

#define OLDWMO  1
#define NEWWMO  2

#define CNAM_LEN  256
#define WBCMZ_TBL "MZ_CNTY"

static	int	_nFcstr;
static	char	_fcstr[MAXNOPT][FCSTRNAMLEN];
static	struct	optMenuStrc	_fcstrStrc;

static	int	_nStatus;
static	char	_status[MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_statusStrc;

static	char	_statesInc[MAXSTLEN];
static	char	_statesAll[MAXSTLEN];
static	char	_adjareaInc[MAXSTLEN];
static	char	_aWFO[MAXWFOLEN];
static  char    _wtchNam[FILE_FULLSZ];

static  char	_savePrvWatchText[MAXWATCHLEN];

static	int	_currDate    = 0;
static	int	_currDiscuss = 0;
static	int	_currEtime   = 0;
static	int	_currExpire  = 0;
static	int	_currItime   = 0;
static	int	_currWatch   = 0;

static	Boolean	_showExpire  = FALSE;
static	Boolean	_showDiscuss = FALSE;
static	Boolean	_hasStatus   = TRUE;

static	Widget	_mainForm;
static	Widget	_wholeWatchPane;
static	Widget	_wholeWatchText;
static	Widget	_dividedWatchPane;
static	Widget	_dividedWatchText;
static	Widget	_statusLineText;
static	Widget	_expireTb;
static	Widget	_expireText;
static	Widget	_discussTb;
static	Widget	_discussText;
static	Widget	_etimeText;
static	Widget	_saveForm;
static	Widget	_saveWatchText;
static	Widget	_saveFileText;

static	char	_itimeText[10];


/*
 * private functions -- callback
 */
static void pgwsmw_allCapsCb (Widget, XtPointer, XtPointer );
static void pgwsmw_ctlBtnCb  (Widget, long, XtPointer );
static void pgwsmw_svBtnCb   (Widget, long, XtPointer );
static void pgwsmw_timeCb    (Widget, int*, XtPointer );
static void pgwsmw_toggleCb  (Widget, long, XtPointer );
static void pgwsmw_watchCb   (Widget, int*, XtPointer );

/*
 * private functions
 */
static void pgwsmw_createWSM (int maxlen, char watch_text[] );
static void pgwsmw_getFrom   (int grpnum, char *fromline, int *iret );
static void pgwsmw_rdInfo    (int *iret );
static void pgwsmw_wsmCnty ( char *wsm,    char *cday, 
                             char *chour,  char *ctype, 
			     int  iwnum,   char *enh_wsm, int *iret );
static void pgwsmw_getWtchNam ( void );


/************************************************************************
 * nmap_pgwsmw.c							*
 *									*
 * This module defines everything for watch status message formatting.	*
 *									*
 * CONTENTS:								*
 *	pgwsmw_create()		creates the popup window		*
 *	pgwsmw_popup()		manages the popup window		*
 *	pgwsmw_popdown()	unmanages the popup window		*
 *									*
 *	pgwsmw_isUp()		query whether the window is up		*
 *									*
 *	pgwsmw_setWatch()	sets the selected watches information	*
 *									*
 *	pgwsmw_selectEh()	event handler for element selection	*
 *									*
 *	pgwsmw_toggleCb()	callback for toggle buttons		*
 *	pgwsmw_watchCb()	callback for watch widget		*
 *	pgwsmw_timeCb()		callback for time text widgets		*
 *	pgwsmw_ctlBtnCb()	callback for control buttons		*
 *	pgwsmw_svBtnCb()	callback for save buttons		*
 *	pgwsmw_allCapsCb()	convert text message to all caps	*
 *									*
 *	pgwsmw_rdInfo()		reads the info table			*
 *	pgwsmw_createWSM()	generates the watch status message	*
 *	pgwsmw_getFrom()	generates the from line			*
 ***********************************************************************/

/*=====================================================================*/

void pgwsmw_create ( Widget parent )
/************************************************************************
 * pgwsmw_create							*
 *									*
 * This function creates an international sigmets attribute selection	*
 * box.									*
 *									*
 * void pgwsmw_create (parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 * H. Zeng/EAI          04/00   added forecaster pulldown menu          *
 * D.W.Plummer/NCEP	 4/01	Changed name of widget; add word wrap	*
 * E. Safford/GSC	04/01	make msg larger font & all caps, cleanup*
 * T. Piper/GSC		 7/01	Freed flentry				*
 * J. Wu/SAIC		05/02	verify ref. #/exp. time/issue time	*
 * A. Hardy/NCEP	 6/04   Changed status editable window to FALSE	*
 * H. Zeng/SAIC		07/04	added a vertical scrollbar for WSM text	*
 * G. Grosshans/SPC	10/04   Changed status editable window to TRUE	*
 * E. Safford/SAIC	05/05	free fontlist				*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    int		voff = 5, hoff = 3, ier;
    int		line_len = 66, wnum_size  = 4, time_size = 6;
    long	ii, nn;
    char	*ctlstrs[] = {"FORMAT", "CANCEL"};
    char	*svstrs[]  = {"SAVE", "CANCEL"};
    char	fontname[] = "-adobe-courier-bold-r-normal-*-*-120-*-*-m-*-*-*";
    char	font18[]   = "-adobe-courier-bold-r-normal-*-*-180-*-*-m-*-*-*";

    char	*statstrs[] = {"RIGHT\0", "LEFT\0", "NORTH\0", "SOUTH\0", 
			      "EAST\0", "WEST\0", "NORTHEAST\0", "SOUTHEAST\0",
			      "NORTHWEST\0", "SOUTHWEST\0", "NORTH AND EAST\0",
			      "SOUTH AND EAST\0", "NORTH AND WEST\0", 
			      "SOUTH AND WEST\0"};

    Widget	pane, form, label1, label2, button, rc1, rc2, scrolled_w;
    XmString	xmstr;
    Display	*dsp;
    XmFontListEntry flentry;
    XmFontList	fontlist;
    Arg		args[15];
    Cardinal	cnt;
/*---------------------------------------------------------------------*/

    pgwsmw_rdInfo (&ier);

    _mainForm = XmCreateFormDialog (parent, "wsmw_format", NULL, 0);

    xmstr = XmStringCreateLocalized("WATCH STATUS");
    XtVaSetValues(_mainForm,
	 	XmNnoResize,			TRUE,
		XmNautoUnmanage,		FALSE,
		XmNdialogTitle,			xmstr,
		NULL);

    XmStringFree(xmstr);


    dsp = XtDisplay (_mainForm);
    flentry = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);
    pane = (Widget) XtVaCreateManagedWidget ("wsmw_pane",
		xmPanedWindowWidgetClass, 	_mainForm,
		XmNsashWidth,			1,
		XmNsashHeight,	 		1,
		XmNleftAttachment,  		XmATTACH_FORM,
		XmNrightAttachment, 		XmATTACH_FORM,
		NULL);

    XmFontListFree( fontlist );

/*
 * whole watch pane area
 */
    _wholeWatchPane = (Widget) XtVaCreateManagedWidget ("wsmw_wholeform",
		xmFormWidgetClass,		pane,
		XmNverticalSpacing,		voff,
		NULL);

    xmstr = XmStringCreateLocalized("WATCH");
    label1 = (Widget) XtVaCreateManagedWidget ("wsmw_wholelabel1",
	  	xmLabelWidgetClass, 		_wholeWatchPane,
		XmNlabelString, 		xmstr,
		NULL);

    XmStringFree(xmstr);


    _wholeWatchText = (Widget) XtVaCreateManagedWidget ("wsmw_wholetext",
		xmTextWidgetClass, 		_wholeWatchPane,
		XmNcolumns,	     		wnum_size,
		XmNeditable,	     		FALSE,
                XmNcursorPositionVisible,  	FALSE,
                XmNtraversalOn,    		FALSE,
	  	XmNleftAttachment, 		XmATTACH_WIDGET,
	  	XmNleftWidget,     		label1,
	  	NULL);

    xmstr = XmStringCreateLocalized("CONTINUES ACROSS ENTIRE AREA");
    label2 = (Widget) XtVaCreateManagedWidget ("wsmw_wholelabel2",
		xmLabelWidgetClass, 		_wholeWatchPane,
		XmNlabelString,    		xmstr,
		XmNleftAttachment, 		XmATTACH_WIDGET,
		XmNleftWidget,     		_wholeWatchText,
		NULL);

    XmStringFree(xmstr);

/*
 * divided watch pane area
 */
    _dividedWatchPane = (Widget) XtVaCreateManagedWidget ("wsmw_dividedform",
	  	xmFormWidgetClass,		pane,
	  	XmNverticalSpacing,		voff,
	  	NULL);

    xmstr = XmStringCreateLocalized("WATCH");
    label1 =  (Widget) XtVaCreateManagedWidget ("wsmw_dividedlabel1",
	  	xmLabelWidgetClass,		_dividedWatchPane,
		XmNlabelString,    		xmstr,
		NULL);

    XmStringFree(xmstr);


    _dividedWatchText = 
	(Widget) XtVaCreateManagedWidget ("wsmw_dividedtext",
		xmTextWidgetClass, 		_dividedWatchPane,
		XmNcolumns,	     		wnum_size,
		XmNeditable,	     		FALSE,
		XmNleftAttachment, 		XmATTACH_WIDGET,
		XmNleftWidget,     		label1,
		NULL);

    _statusStrc.current = 0;
    _nStatus = XtNumber (statstrs);
    pgutls_createOptionMenu (_dividedWatchPane, _nStatus, 
			     (XtPointer)&_statusStrc.current, 
			     "CONTINUES TO THE", NULL, &_statusStrc.form,
			     &_statusStrc.label, &_statusStrc.menu, 
			     _statusStrc.pb, statstrs);

    for (ii = 0; ii < _nStatus; ii++) {
	strcpy (_status[ii], statstrs[ii]);
    }

    XtVaSetValues (_statusStrc.form, 
		XmNleftAttachment,		XmATTACH_WIDGET, 
		XmNleftWidget,			_dividedWatchText,
		NULL);

    xmstr = XmStringCreateLocalized("OF THE LINE: ");
    label2 = (Widget) XtVaCreateManagedWidget ("wsmw_statuslabel2",
	  	xmLabelWidgetClass,		_dividedWatchPane,
		XmNlabelString,    		xmstr,
		XmNtopAttachment,  		XmATTACH_WIDGET,
		XmNtopWidget,      		_dividedWatchText,
		NULL);

    XmStringFree(xmstr);


    _statusLineText = (Widget) XtVaCreateManagedWidget ("wsmw_statusLinetext",
		xmTextWidgetClass, 		_dividedWatchPane,
		XmNeditable,       		FALSE,
		XmNtopAttachment,  		XmATTACH_WIDGET,
		XmNtopWidget,      		_dividedWatchText,
		XmNleftAttachment, 		XmATTACH_WIDGET,
		XmNleftWidget,     		label2,
		XmNrightAttachment, 		XmATTACH_FORM,
		NULL);


    XtUnmanageChild (_dividedWatchPane);

/*
 * main pane area
 */
    nn = XtNumber (ctlstrs) * 100;
    form = (Widget) XtVaCreateManagedWidget ("wsmw_mainform",
		xmFormWidgetClass,		pane,
		XmNverticalSpacing,		voff,
		XmNfractionBase,		nn,
		NULL);

/*
 * Mesoscale discussion reference
 */
    rc2  = XtVaCreateManagedWidget ("wsmw_discuss_rowcol",
	    	xmRowColumnWidgetClass,		form,
		XmNorientation,			XmHORIZONTAL,
		XmNpacking,			XmPACK_TIGHT,
		XmNtopAttachment,		XmATTACH_FORM,
		NULL);

    xmstr = XmStringCreateLocalized("REFERENCE MESOSCALE DISCUSSION #");
    _discussTb = XtVaCreateManagedWidget ("wsmw_discuss_tb",
	      	xmToggleButtonWidgetClass, 	rc2,
		XmNlabelString,			xmstr,
                XmNtraversalOn,   		FALSE,
		NULL);

    XmStringFree(xmstr);

    XtAddCallback (_discussTb, XmNvalueChangedCallback, (XtCallbackProc)pgwsmw_toggleCb, 
		   (XtPointer) DISCUSS_TB); 


    _discussText = 
	(Widget) XtVaCreateManagedWidget ("wsmw_discuss_text",
		xmTextWidgetClass, 		rc2,
		XmNcolumns,	     		wnum_size,
		NULL);

    XtAddCallback(_discussText, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_discussText, XmNactivateCallback, 
		   (XtCallbackProc)pgwsmw_watchCb, (XtPointer) &_currDiscuss);
    XtAddCallback (_discussText, XmNlosingFocusCallback, 
		   (XtCallbackProc)pgwsmw_watchCb, (XtPointer) &_currDiscuss);

/*
 * Watch expiration toggle
 */
    rc1  = XtVaCreateManagedWidget ("wsmw_expire_rowcol",
	    	xmRowColumnWidgetClass,		form,
		XmNorientation,			XmHORIZONTAL,
		XmNpacking,			XmPACK_TIGHT,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			rc2,
		NULL);

    xmstr = XmStringCreateLocalized("FINAL STATUS - EXPIRATION TIME");
    _expireTb = XtVaCreateManagedWidget ("wsmw_expire_tb",
		xmToggleButtonWidgetClass, 	rc1,
		XmNlabelString,			xmstr,
                XmNtraversalOn,        		FALSE,
		NULL);

    XmStringFree(xmstr);

    XtAddCallback (_expireTb, XmNvalueChangedCallback, 
    		   (XtCallbackProc)pgwsmw_toggleCb, (XtPointer) EXPIRE_TB); 


    _expireText = 
	(Widget) XtVaCreateManagedWidget ("wsmw_expire_text",
		xmTextWidgetClass, 		rc1,
		XmNcolumns,	     		time_size,
		NULL);

    XtAddCallback(_expireText, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_expireText, XmNactivateCallback, 
		   (XtCallbackProc)pgwsmw_timeCb, (XtPointer) &_currExpire);
    XtAddCallback (_expireText, XmNlosingFocusCallback, 
		   (XtCallbackProc)pgwsmw_timeCb, (XtPointer) &_currExpire);

/*
 * Issue and expiration times
 */
    rc2  = XtVaCreateManagedWidget ("wsmw_times_rowcol",
		xmRowColumnWidgetClass,		form,
		XmNorientation,			XmHORIZONTAL,
		XmNpacking,			XmPACK_TIGHT,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			rc1,
		NULL);

    xmstr = XmStringCreateLocalized("STATUS VALID UNTIL");
    label1 = (Widget) XtVaCreateManagedWidget ("wsmw_etime_label",
		xmLabelWidgetClass, 		rc2,
		XmNlabelString, 		xmstr,
		NULL);

    XmStringFree(xmstr);


    _etimeText = (Widget) XtVaCreateManagedWidget ("wsmw_etime_text",
		xmTextWidgetClass, 		rc2,
		XmNcolumns,	     		time_size,
		NULL);

    XtAddCallback(_etimeText, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_etimeText, XmNactivateCallback, 		
    		   (XtCallbackProc)pgwsmw_timeCb, (XtPointer)&_currEtime);
    XtAddCallback (_etimeText, XmNlosingFocusCallback, 
		   (XtCallbackProc)pgwsmw_timeCb, (XtPointer)&_currEtime);

/*
 * Create Forecaster menu
 */
    _fcstrStrc.current = 0;
    pgutls_createOptionMenu (form, 
    			     _nFcstr, (XtPointer)&_fcstrStrc.current, 
			     "FORECASTER:", NULL, &_fcstrStrc.form, 
			     &_fcstrStrc.label, &_fcstrStrc.menu, 
			     _fcstrStrc.pb, NULL);

    for (ii = 0; ii < _nFcstr; ii++) {
	xmstr = XmStringCreateLocalized (_fcstr[ii]);
	XtVaSetValues (_fcstrStrc.pb[ii], XmNlabelString, xmstr, NULL);
	XmStringFree(xmstr);
    }

    XtVaSetValues (_fcstrStrc.form, 
	   	XmNtopAttachment,		XmATTACH_WIDGET, 
	   	XmNtopWidget,			rc2,
	   	NULL);

/*
 * control buttons
 */
    nn = XtNumber (ctlstrs);
    for (ii = 0; ii < nn; ii++) {
	button = XtVaCreateManagedWidget ( ctlstrs[ii], 
 	  	xmPushButtonWidgetClass, 	form, 
	     	XmNtopAttachment,		XmATTACH_WIDGET,
	     	XmNtopWidget,			_fcstrStrc.form,
	     	XmNleftAttachment,		XmATTACH_POSITION,
	     	XmNleftPosition,		((ii * 100) + hoff),
	     	XmNrightAttachment,		XmATTACH_POSITION,
	     	XmNrightPosition,	     	(((ii + 1) * 100) - hoff),
	     	NULL);

	XtAddCallback (button, 
		XmNactivateCallback, 	       (XtCallbackProc)pgwsmw_ctlBtnCb, 
		(XtPointer) ii); 
    }

/*
 * save popup
 */
    _saveForm = XmCreateFormDialog (parent, "wsmw_format", NULL, 0);
    xmstr = XmStringCreateLocalized("WATCH STATUS SAVE");

    nn = XtNumber (svstrs) * 100;
    XtVaSetValues(_saveForm,
		  XmNnoResize,			TRUE,
		  XmNautoUnmanage,		FALSE,
		  XmNdialogTitle,		xmstr,
		  XmNverticalSpacing,		voff,
		  XmNfractionBase,		nn,
		  NULL);

    XmStringFree(xmstr);

/*
 * create the scrolled widnow for WSM text widget.
 */
    scrolled_w = XtVaCreateManagedWidget ("scrolled_w",
                  xmScrolledWindowWidgetClass,    _saveForm,
                  XmNscrollingPolicy,             XmAPPLICATION_DEFINED,
                  XmNscrollBarDisplayPolicy,      XmSTATIC,
                  XmNshadowThickness,             0,
		  XmNtopAttachment,               XmATTACH_FORM,
		  XmNleftAttachment,	          XmATTACH_FORM,
		  XmNrightAttachment,		  XmATTACH_FORM, 
                  NULL );

/*
 *  Multi-line text widget for status message 
 */
    flentry = XmFontListEntryLoad (dsp, font18, XmFONT_IS_FONT, "TAG2");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);

    cnt = 0;
    XtSetArg ( args[cnt], XmNeditable,        TRUE              ); cnt++;
    XtSetArg ( args[cnt], XmNeditMode,        XmMULTI_LINE_EDIT ); cnt++;
    XtSetArg ( args[cnt], XmNwordWrap,        TRUE              ); cnt++;
    XtSetArg ( args[cnt], XmNrows,            20                ); cnt++;
    XtSetArg ( args[cnt], XmNcolumns,         line_len+3        ); cnt++;
    XtSetArg ( args[cnt], XmNfontList,	      fontlist          ); cnt++;

    _saveWatchText = XmCreateText ( scrolled_w, 
    			"wsmw_savewatchtext", args, cnt );

    XmFontListFree( fontlist );
    
    XtAddCallback ( _saveWatchText, 
    			XmNmodifyVerifyCallback,	pgwsmw_allCapsCb,
			NULL);

    XtManageChild ( _saveWatchText );


    _saveFileText = 
	(Widget) XtVaCreateManagedWidget ("wsmw_savefiletext",
		xmTextWidgetClass, 		_saveForm,
		XmNtopAttachment,  		XmATTACH_WIDGET,
		XmNtopWidget,	     		scrolled_w,
		XmNleftAttachment, 		XmATTACH_FORM,
		XmNrightAttachment, 		XmATTACH_FORM,
		NULL);

/*
 * save control buttons
 */
    nn = XtNumber (svstrs);
    for (ii = 0; ii < nn; ii++) {
	button = XtVaCreateManagedWidget ( svstrs[ii], 
		xmPushButtonWidgetClass, 	_saveForm, 
	     	XmNtopAttachment,		XmATTACH_WIDGET,
	     	XmNtopWidget,			_saveFileText,
	     	XmNleftAttachment,		XmATTACH_POSITION,
	     	XmNleftPosition,		((ii * 100) + hoff),
	     	XmNrightAttachment,		XmATTACH_POSITION,
	     	XmNrightPosition,	     	(((ii + 1) * 100) - hoff),
	     	NULL);

	XtAddCallback (button, 
		XmNactivateCallback, 	(XtCallbackProc)pgwsmw_svBtnCb,
		(XtPointer) ii); 
    }
}

/*=====================================================================*/

void pgwsmw_popup ( VG_DBStruct *el, XtCallbackProc callback )
/************************************************************************
 * pgwsmw_popup								*
 *									*
 * This function manages the WSM popup.					*
 *									*
 * void pgwsmw_popup (el, callback)					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	current element			*
 *	callback	XtCallbackProc	edit callback structure		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 ***********************************************************************/
{
    pgwsmw_popdown ();
    XtSetSensitive (_mainForm, FALSE);
    XtManageChild (_mainForm);
}

/*=====================================================================*/

void pgwsmw_popdown ( void )
/************************************************************************
 * pgwsmw_popdown							*
 *									*
 * This function unmanages the WSM popup.				*
 *									*
 * void pgwsmw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 * E. Safford/GSC	12/99	move pgwsm_setWatch into if conditions  *
 ***********************************************************************/
{
    if (XtIsManaged (_mainForm)) {
	XtUnmanageChild (_mainForm);
        pgwsmw_setWatch (NULL);
    }

    if (XtIsManaged (_saveForm)) {
	XtUnmanageChild (_saveForm);
        pgwsmw_setWatch (NULL);
    }
}

/*=====================================================================*/

Boolean pgwsmw_isUp ( void )
/************************************************************************
 * pgwsmw_isUp								*
 *									*
 * This function returns a boolean value specifying whether the WSM   	*
 * dialog is managed or not.						*
 *									*
 * Boolean pgwsmw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 *	pgwsmw_isUp		Boolean		Is/is not managed	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	Copied from pgsigw_isUp			*
 ***********************************************************************/
{
    return (XtIsManaged (_mainForm));
}

/*=====================================================================*/

void pgwsmw_setWatch ( VG_DBStruct *el )
/************************************************************************
 * pgwsmw_setWatch							*
 *									*
 * This function sets information about the selected watch element.	*
 *									*
 * void pgwsmw_setWatch ( el )						*
 *									*
 * Input parameters:							*
 *	*el	VG_DBStruct	current element				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	initial coding				*
 * D.W.Plummer/NCEP	 8/00	change call to clo_ routine		*
 * D.W.Plummer/NCEP	01/01	added "..." to end of WFO string	*
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * G. Grosshans/SPC	06/03	Change expiration time to H+40		*
 * A. Hardy/NCEP	 3/04   changed COUNTY to WBCMZ_TBL		*
 * R. Tian/SAIC		 7/04	always toggle off mesoscale discussion	*
 * A. Hardy/NCEP	12/04   reset _showDiscuss boolean		*
 ***********************************************************************/
{
    int		ii, nn, eday, etime, ier, tarry[5], plusmin, remainder;
    int		one=1, nret, len;
    char        wfo[10];
    char	wnum[10], estr[10], temp_str[80], fmline[MAXFROMLEN];
    struct tm	*utctime;
    time_t	tp;
    WatchBoxInfo *wbx;
/*---------------------------------------------------------------------*/

    if (el == NULL) {
	XtSetSensitive (_mainForm, FALSE);
	
    	pghdlb_deselectAll ();  
    }
    else {
	XtSetSensitive (_mainForm, TRUE);

	wbx = &(el->elem.wbx.info);

	_currWatch = wbx->w_number;
	sprintf (wnum, "%4.4d", _currWatch);
	wbx->wsm_meso[0] = '\0';
	XmToggleButtonSetState ( _discussTb, FALSE, FALSE );
	_showDiscuss = False;

	_hasStatus = (Boolean)((wbx->w_issued == WATCH_WITHLINE));

/*
 *  Line may have been deleted... check "from" line length
 */
	if ( _hasStatus )  {
	    pgwsmw_getFrom (el->hdr.grpnum, fmline, &ier);
	    if ( fmline[0] == '\0' )  _hasStatus = FALSE;
	}

	if (_hasStatus) {

/*
 *  Watch status has been issued previously -- 
 *  present information appropriate for issued status
 */
	    XtUnmanageChild (_wholeWatchPane);
	    XtManageChild (_dividedWatchPane);
	    XmTextSetString (_dividedWatchText, wnum);

	    XmTextSetString (_statusLineText, fmline);
	}
	else {

/*
 *  Watch status has not been issued previously -- 
 *  present information appropriate for unissued status
 */
	    XtUnmanageChild (_dividedWatchPane);
	    XtManageChild (_wholeWatchPane);
	    XmTextSetString (_wholeWatchText, wnum);
	}

/*
 *  save "states included" string
 */
	strcpy ( _statesAll, wbx->w_states );
	wbc_mzrm ( wbx->w_states, _statesInc, &len, &ier );
	strcpy ( _adjareaInc, wbx->w_adjarea );

/*
 *  process time information
 */
	sscanf (wbx->w_exp_t, "%*d/%d/%*d/%d", &eday, &etime);
	sprintf (estr, "%2.2d%4.4d", eday, etime);
	XmTextSetString (_expireText, estr);
	XmToggleButtonSetState (_expireTb, FALSE, TRUE);

	XmTextSetString (_discussText, wbx->wsm_meso);
	XmToggleButtonSetState (_discussTb, 
				(strlen (wbx->wsm_meso) > (size_t)0), TRUE);

/* 
 * set up current times
 */
	tp = time (NULL);
	utctime = gmtime (&tp);

	tarry[0] = utctime->tm_year + 1900;
	tarry[1] = utctime->tm_mon + 1;
	tarry[2] = utctime->tm_mday;
	tarry[3] = utctime->tm_hour;
	tarry[4] = utctime->tm_min;

	_currDate = ((tarry[0] % 100) * 10000) + (tarry[1] * 100) + tarry[2];

	if (tarry[4] != 0) {
	    remainder = tarry[4] % ITIME_RND;
	    plusmin = ITIME_RND - remainder;
	    ti_addm (tarry, &plusmin, tarry, &ier);
	}

	_currItime = (tarry[2] * 10000) + (tarry[3] * 100) + tarry[4];

	if (tarry[4] <= 40) {
	    plusmin = (40 - tarry[4]) + 60;
	}
	else  {
	    plusmin = (60 - tarry[4]) + 40;
	}
	ti_addm (tarry, &plusmin, tarry, &ier);

	_currEtime = (tarry[2] * 10000) + (tarry[3] * 100) + tarry[4];

	sprintf (temp_str, "%6.6d", _currItime);
	strcpy (_itimeText, temp_str);
	sprintf (temp_str, "%6.6d", _currEtime);
	XmTextSetString (_etimeText, temp_str);

/*
 *  create WFO string from county information
 */
	_aWFO[0] = '\0';
	nn = wbx->numcnty;
	for ( ii = 0; ii < nn; ii++ )  {
	    clo_tclosest( WBCMZ_TBL, wbx->cn_ltln[ii],
			wbx->cn_ltln[ii+nn], one, &ier );
	    clo_tgparm( WBCMZ_TBL, 10, one, ";", &nret, wfo, &ier );
	    if ( strstr( _aWFO, wfo ) == 0 )  {
		sprintf( temp_str, "...%s", wfo );
		strcat ( _aWFO, temp_str );
	    }
	}
	strcat ( _aWFO, "..." );
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwsmw_selectEh ( Widget wid, XtPointer clnt, XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgwsmw_selectEh							*
 *									*
 * Callback function for selecting watch boxes.				*
 *									*
 * void pgwsmw_selectEh (wid, clnt, event, ctdr )			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	client data				*
 *	*event	XEvent		event callback structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/99	copied from pgwfmt_selectCb		*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 ***********************************************************************/
{
/*
 * No action if watch box is already selected
 */
    if (event->xbutton.button == Button1) {
	return;
    }

    pgwsmw_setWatch (NULL);
    mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT); 
    mbotw_mouseSet (LMHINT_SELECT, MMHINT_NOACTION);
}

/*=====================================================================*/
/* ARGSUSED */
static void pgwsmw_toggleCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgwsmw_toggleCb							*
 *									*
 * Callback function for the toggle buttons.				*
 *									*
 * static void pgwsmw_toggleCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		which toggle			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 ***********************************************************************/
{
    switch (clnt) {
      case EXPIRE_TB:
	_showExpire = XmToggleButtonGetState (wid);
	break;

      case DISCUSS_TB:
	_showDiscuss = XmToggleButtonGetState (wid);
	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgwsmw_watchCb ( Widget wid, int *clnt, XtPointer cbs )
/************************************************************************
 * pgwsmw_watchCb							*
 *									*
 * Callback function for the watch text widget.				*
 *									*
 * static void pgwsmw_watchCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   *clnt		int		save variable			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 ***********************************************************************/
{
    int		wnum;
    char	*ptxt, tmpstr[10];
/*---------------------------------------------------------------------*/
    ptxt = XmTextGetString (wid);
    sscanf (ptxt, "%d", &wnum);
    XtFree (ptxt);

    if (0 < wnum && wnum < 10000) {
	*clnt = wnum;
    }

    sprintf (tmpstr, "%4.4d", *clnt);
    XmTextSetString (wid, tmpstr);
}

/*=====================================================================*/
/* ARGSUSED */
static void pgwsmw_timeCb ( Widget wid, int *clnt, XtPointer cbs )
/************************************************************************
 * pgwsmw_timeCb							*
 *									*
 * Callback function for the time buttons.				*
 *									*
 * static void pgwsmw_timeCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   *clnt		int		save variable			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 ***********************************************************************/
{
    int		dtg, day, hour, minute;
    char	*ptxt, tmpstr[10];
/*---------------------------------------------------------------------*/
    ptxt = XmTextGetString (wid);
    sscanf (ptxt, "%d", &dtg);
    XtFree (ptxt);

    day = dtg / 10000;
    hour = ((int) (dtg / 100)) % 100;
    minute = dtg % 100;

    if ((0 <= day    && day <= 31) &&
	(0 <= hour   && hour <= 23) &&
	(0 <= minute && minute <= 59)) {

/*
 * if no day was given, use current day
 */
	if (day == 0) {
	    day = _currDate % 100;
	}

	dtg = (day * 10000) + (hour * 100) + minute;

	*clnt = dtg;
    }
    sprintf (tmpstr, "%6.6d", *clnt);
    XmTextSetString (wid, tmpstr);
}

/*=====================================================================*/
/* ARGSUSED */
static void pgwsmw_ctlBtnCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgwsmw_ctlBtnCb							*
 *									*
 * Callback function for the control buttons.				*
 *									*
 * static void pgwsmw_ctlBtnCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		which button			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 * H. Zeng/SAIC		07/04	added creation of enhanced WSM message	*
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * F. J. Yen/NCEP	01/06	Fix UGC expiration time for final prod.	*
 * J. Wu/SAIC		04/06	added para newLineStr in cst_wrap 	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    char	  watch_text[MAXWATCHLEN], save_file[30];
    char	  cday[3], chour[5], ctype[5], enh_wsm[MAXENHLEN];
    char	  blank[2]={' '}, *txtstr=NULL;
    float	  llx, lly, urx, ury;
    int		  line_len = 66, loc, iwnum, ier;
    VG_DBStruct	  el;
    WatchBoxInfo  *wbx;
/*---------------------------------------------------------------------*/

    switch (clnt) {
      case 0:	/* FORMAT */

/*
 * Create WSM message and save it to a local global.
 */
	pgwsmw_createWSM (MAXWATCHLEN, watch_text);

	cst_wrap ( watch_text, blank, &line_len, NL,
			(char *)NULL, watch_text, &ier );

	strcpy (_savePrvWatchText, watch_text);

/*
 * Create enhanced WSM message.
 */
	pgutls_prepNew (-1, &el, &llx, &lly, &urx, &ury, &ier);
	wbx = &(el.elem.wbx.info);

	strcpy (wbx->wsm_iss_t, _itimeText);

	if (_showExpire) {

/*
 *  Final Status--use watch expiration time 
 */
	    txtstr = XmTextGetString (_expireText);
    	}
    	else  {

/*
 *  Not final status--use "valid until time"
 */
	    txtstr = XmTextGetString (_etimeText);
    	}

	strcpy (wbx->wsm_exp_t, txtstr);
	XtFree (txtstr);

	txtstr = XmTextGetString (_discussText);
	strcpy (wbx->wsm_meso, txtstr);
	XtFree (txtstr);

	txtstr = XmTextGetString (_statusLineText);
	strcpy (wbx->wsm_from, txtstr);
	XtFree (txtstr);

	strcpy (wbx->wsm_fcstr, _fcstr[_fcstrStrc.current]);

	pgvgf_saveNewElm (cvg_getworkfile(), sys_D, &el, 0, NULL, NULL,
			  TRUE, &loc, &ier);

/* 
 * Create enhanced WSM.
 */
	cst_ncpy ( cday,  wbx->wsm_exp_t, 2, &ier );
	cst_ncpy ( chour,  wbx->wsm_exp_t+2, 4, &ier );

	iwnum = wbx->w_number;

        switch ( wbx->w_type) {

	case TRWWTCH:
	        cst_ncpy ( ctype, "WS", 2, &ier);
		break;
        case TORWTCH:
	        cst_ncpy ( ctype, "WT", 2, &ier);
		break;
        }

	pgwsmw_getWtchNam ( );

	pglist_wrtLstWtch (_wtchNam, &loc );

        pgwsmw_wsmCnty ( watch_text, cday, chour, 
			 ctype,  iwnum, enh_wsm, &ier );

	pgutls_redraw (loc, &el, &ier);
        pgundo_newStep();
	pgundo_storeThisLoc (loc, UNDO_ADD, &ier);
        pgundo_endStep();

/*
 * Display the enhanced WSM on GUI.
 */
	cst_wrap ( enh_wsm, blank, &line_len, NL, (char *)NULL, enh_wsm, &ier );

	XmTextSetString (_saveWatchText, enh_wsm);

	sprintf (save_file, "WSMenh_%4.4d_%6.6d.txt", _currWatch, _currDate);
	XmTextSetString (_saveFileText, save_file);

	XtUnmanageChild (_mainForm);
	XtManageChild (_saveForm);
	break;

      case 1:	/* CANCEL */
	pgwsmw_popdown ();
	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgwsmw_svBtnCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgwsmw_svBtnCb							*
 *									*
 * Callback function for the control buttons.				*
 *									*
 * static void pgwsmw_svBtnCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		which button			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 * S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 * M. Li/GSC		08/00	removed pgwsmw_createWSM		*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * H. Zeng/EAI          12/00   modified for the undo new design        *
 * D.W.Plummer/NCEP	 4/01	added call to cst_wrap trim to LINE_LEN	*
 * D.W.Plummer/NCEP	 4/01	replace NL with EOL in final text file	*
 * M. Li/GSC		 5/01	assigned output for XmTextGetString	*
 * m.gamazaychikov/SAIC 11/02	check for null filename			* 
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * A. Hardy/NCEP	 2/04   added enhanced WSM county logic		*
 * H. Zeng/SAIC		07/04	added watch no and type info		*
 * H. Zeng/SAIC		07/04	moved out enhanced WSM creation		*
 * A. Hardy/NCEP	11/04   Store txt chngs in gui in old WSM       *
 ***********************************************************************/
{
    int		ier, ierr;
    char	*save_file, *cptr, *str, mesg[128], wsmfnm[128];
    char	errStr[FILE_FULLSZ], enh[MAXENHLEN]; 
    char	wsm[MAXWATCHLEN]; 
    int         ipos, ibeg, iend;
    FILE	*fp;
/*---------------------------------------------------------------------*/

    switch (clnt) {
      case 0:	/* SAVE */
	save_file = XmTextGetString (_saveFileText);

        if(!*save_file) {
          sprintf(mesg,"BLANK filename is invalid\n");
          strcat(mesg,"  Enter VALID filename\n");
          NxmWarn_show(wid, mesg);
          break;
        }
      
	str = XmTextGetString(_saveWatchText);
	strcpy ( enh, str );
	XtFree(str);

	cptr = strstr ( enh, NL );
	while ( cptr != (char *)NULL )  {
	    cst_rpst ( cptr, NL, EOL, cptr, &ier );
	    cptr = strstr ( cptr+strlen(EOL), NL );
	}

/*
 * Save the enhanced watch status message to text file.
 */
	fp = cfl_wopn (save_file, &ier);
	if ( ier == 0 ) {
	    fputs ( enh, fp );
	    fclose (fp);
	}
	else {
	    sprintf( errStr, "%s", save_file );
	    er_wmsg ( "CFL", &ier, errStr, &ierr, 3, strlen(errStr) );
	    NxmErr_update( );
	}

/*
 * Create the original WSM file name.
 */
	cst_rpst ( save_file, "WSMenh", "WSM", wsmfnm, &ier );

	XtFree (save_file);

/*
 * Save the original watch status message to text file.
 */
	strcpy (wsm, _savePrvWatchText);

/*
 * The next 5 lines allow for any text changes made in the enhanced
 * WSM window gui to be reflected in the original WSM when it
 * is written to the text file.
 */
	ibeg = 0;
	cst_lstr ( enh, &iend, &ier );
	cst_srch ( ibeg, iend, "&&", enh, &ipos, &ier );
        cst_ncpy (wsm, enh, ipos+2, &ier);
	cst_rpst ( wsm, "&&", "NNNN", wsm, &ier );

	cptr = strstr ( wsm, NL );
	while ( cptr != (char *)NULL )  {
	    cst_rpst ( cptr, NL, EOL, cptr, &ier );
	    cptr = strstr ( cptr+strlen(EOL), NL );
	}

	fp = cfl_wopn (wsmfnm, &ier);
	if ( ier == 0 ) {
	    fputs ( wsm, fp );
	    fclose (fp);
	}
	else {
	    sprintf( errStr, "%s", wsmfnm );
	    er_wmsg ( "CFL", &ier, errStr, &ierr, 3, strlen(errStr) );
	    NxmErr_update( );
	}

	pgwsmw_popdown ();
	break;

      case 1:	/* CANCEL */
	pgwsmw_popdown ();
	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgwsmw_allCapsCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgwsmw_allCapsCb							*
 *									*
 * Callback function for converting text message to all caps.		*
 *									*
 * static void pgwsmw_allCapsCb (wid, clnt, call)			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	client data				*
 *	call	XtPointer	callback data 				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/GSC	04/01	intial coding              		*
 ***********************************************************************/
{
int	len;
XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call;
/*---------------------------------------------------------------------*/

    if ( cbs->text->ptr == NULL) {
        return;
    }

    for ( len=0; len < cbs->text->length; len++ ) {
	if ( islower ((unsigned long)cbs->text->ptr[len])) {
	    cbs->text->ptr[len] = (char)toupper( (int)cbs->text->ptr[len]);
	}
    }
}

/*=====================================================================*/

static void pgwsmw_rdInfo ( int *iret )
/************************************************************************
 * pgwsmw_rdInfo							*
 *									*
 * This function reads the forecasters table.			        *
 * The table is read in only once.					*
 *									*
 * static void pgwsmw_rdInfo (iret)					*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*iret	int		Return value				*
 *				 -1 - Unable to open table		*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	11/99	Created					*
 * R. Curtis/EAI        05/00   reads from modified forecasters table   *
 * T. Piper/SAIC	12/01	close file				*
 ***********************************************************************/
{
    char	buff[80], name[17], fnm[20];
    int		ier;
    FILE	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;
    _nFcstr = 0;

/*
 *  Open the forecasters table. If not found, return an error.
 */
    strcpy(fnm, FORECASTERS_TBL);
    fp = cfl_tbop(fnm, "config", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }

/*
 *  Scan table line-by-line.
 */
    while ( !feof(fp) )  {

        cfl_trln(fp, sizeof(buff), buff, &ier);

        if ( ier == 0 )  {

/*
 *  Clear memory and scan in forecasters name
 */
            memset(name, '\0', 17);
            sscanf(buff, "%s", name);

/*
 *  Process entry for forecaster name.
 */
	        memset(_fcstr[_nFcstr], '\0', 17);
                strcpy ( _fcstr[_nFcstr], name);
                _nFcstr++;
        }
    }
    cfl_clos(fp, &ier);
}

/*=====================================================================*/

static void pgwsmw_createWSM ( int maxlen, char watch_text[] )
/************************************************************************
 * pgwsmw_createWSM							*
 *									*
 * This function creates the actual watch status message.		*
 *									*
 * static void pgwsmw_createWSM (maxlen, watch_text)			*
 *									*
 * Input parameters:							*
 *	maxlen		int	maximum length of watch_text		*
 *									*
 * Output parameters:							*
 *	watch_text[]	char	watch text				*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 * D.W.Plummer/NCEP	12/99	formalize true watch status message	*
 * H. Zeng/EAI          04/00   changed date format to mm/dd/yy         *
 * D.W.Plummer/NCEP	 4/01	use NL as EOL marker; chg to EOL later	*
 * G. Grosshans		 5/01 	Removed # from mesoscale text string	*
 * D.W.Plummer/NCEP	 7/01	Increased some char array sizes		*
 * A. Hardy/SAIC	10/01   Added check for using NEW WMO header	*
 * A. Hardy/SAIC	11/01   Fixed check for using NEW WMO header	*
 * T. Piper/SAIC	12/01	freed ptext				*
 * D.W.Plummer/NCEP	10/02	added grt lakes & cstl waters to states	*
 * G. Grosshans/SPC	06/03	Change expiration time for final status	*
 *				to be the same as watch end time	*
 * A. Hardy/NCEP	11/03   Removed XtFree from if (_showExpire)    *
 * T. Piper/SAIC	1/06	Properly wrap ATTN line			*
 * J. Wu/SAIC		04/06	added para newLineStr in cst_wrap 	*
 ***********************************************************************/
{
    int		line_len = 66, len, iyy, iyyyy, idd, imm, ihm, fmtflg, ier;
    char	*ptxt, tmpstr[512], from[MAXFROMLEN];
    char	issue_t[20], expire_t[20];
    char	states[MAXSTLEN], *cptr, result[MAXSTLEN];
    char	adjarea[MAXSTLEN];
    char	state[4], area[4];
/*---------------------------------------------------------------------*/

    watch_text[0] = '\0';

/*
 * Set the WMO header flag.
 */
    iyy = _currDate / 10000;
    ti_yy24 ( &iyy, &iyyyy, &ier );
    imm = ( _currDate / 100 ) % 100;
    idd = _currDate % 100;
    ihm = _currDate % 10000;
    fmtflg = OLDWMO;

    if ( iyyyy > CHG_YR ) {
        fmtflg = NEWWMO;
    }
    else if ( iyyyy == CHG_YR ) {
	if ( imm > CHG_MO ) {
            fmtflg = NEWWMO;
        }
	else if ( imm == CHG_MO ) {
	    if ( idd > CHG_DA ) {
                fmtflg = NEWWMO;
	    }
	    else if ( idd == CHG_DA ) {
	        if ( ihm >= CHG_TM ) {
		    fmtflg = NEWWMO;
	        }
	    }
	}
    }

/*
 * Get issue and expiration times
 */
    strcpy (issue_t, _itimeText);

    ptxt = XmTextGetString (_etimeText);
    strcpy (expire_t, ptxt);
    XtFree(ptxt);

/*
 * Form prefix
 */
    if ( fmtflg == OLDWMO ) {
	sprintf (tmpstr, "WWUS08 KMKC %s", issue_t);
	strcat (tmpstr, NL );
	strcat (watch_text, tmpstr);
	sprintf (tmpstr, "MKC WW-A %s", issue_t);
	strcat (tmpstr, NL );
	strcat (watch_text, tmpstr);
    }
    else {
        sprintf (tmpstr, "WOUS20 KWNS %s", issue_t);
        strcat (tmpstr, NL );
        strcat (watch_text, tmpstr);
        sprintf (tmpstr, "WWASPC" );
        strcat (tmpstr, NL );
        strcat (watch_text, tmpstr);
        sprintf (tmpstr, "SPC WW-A %s", issue_t);
        strcat (tmpstr, NL );
        strcat (watch_text, tmpstr);
    }

/*
 * Form state/zone & adjacent area combinations plus date/time
 */
    strcpy ( states, _statesInc );
    cptr = (char *)cst_split ( states, '|', sizeof(result), result, &ier );
    cst_rxbl ( result, result, &len, &ier );
    cptr = (char *)cst_split ( result, ' ', sizeof(state), state, &ier );
    while ( cptr != (char *)NULL )  {
        sprintf (tmpstr, "%sZ000-", state);
        strcat ( watch_text, tmpstr );
        cptr = (char *)cst_split ( cptr, ' ', sizeof(state), state, &ier );
    }
    cst_lstr ( state, &len, &ier );
    if ( len > 0 )  {
        sprintf (tmpstr, "%sZ000-", state);
        strcat ( watch_text, tmpstr );
    }

    strcpy ( adjarea, _adjareaInc );
    cptr = (char *)cst_split ( adjarea, '|', sizeof(result), result, &ier );
    cst_rxbl ( result, result, &len, &ier );
    cptr = (char *)cst_split ( result, ' ', sizeof(area), area, &ier );
    while ( cptr != (char *)NULL )  {
        sprintf (tmpstr, "%sZ000-", area);
        strcat ( watch_text, tmpstr );
        cptr = (char *)cst_split ( cptr, ' ', sizeof(area), area, &ier );
    }
    cst_lstr ( area, &len, &ier );
    if ( len > 0 )  {
        sprintf (tmpstr, "%sZ000-", area);
        strcat ( watch_text, tmpstr );
    }

    if (_showExpire) {
	ptxt = XmTextGetString (_expireText);
	sprintf (tmpstr, "%s-", ptxt );
	XtFree (ptxt);
    } 
    else  {
        sprintf ( tmpstr, "%s-", expire_t );
    }
    strcat (tmpstr, NL );
    strcat (tmpstr, NL );
    strcat ( watch_text, tmpstr );

/*
 * Get watch number
 */
    sprintf (tmpstr, "STATUS REPORT ON WW %d", _currWatch);
    strcat (tmpstr, NL );
    strcat (tmpstr, NL );
    strcat (watch_text, tmpstr);

/*
 * Get status line
 */
    if ( _hasStatus )  {
	ptxt = XmTextGetString (_statusLineText);
	if ( ptxt[0] == CHNULL )  strcpy ( from, "NULL" );
	else strcpy ( from , ptxt );
	sprintf ( tmpstr, 
		  "SEVERE WEATHER THREAT CONTINUES %s OF A LINE FROM %s.",
		  _status[_statusStrc.current], from );
        strcat (tmpstr, NL );
        strcat (tmpstr, NL );
	XtFree (ptxt);
	strcat ( watch_text, tmpstr );
    }
    else  {
	sprintf ( tmpstr, "THE SEVERE WEATHER THREAT CONTINUES ACROSS THE ENTIRE WATCH AREA." );
        strcat (tmpstr, NL );
        strcat (tmpstr, NL );
	strcat ( watch_text, tmpstr );
    }

/*
 * Get watch expiration
 */
    if (_showExpire) {
	ptxt = XmTextGetString (_expireText);
	sprintf (tmpstr, "WW %d WILL BE ALLOWED TO EXPIRE AT %sZ.", _currWatch, ptxt );
        strcat (tmpstr, NL );
        strcat (tmpstr, NL );
	XtFree (ptxt);
	strcat ( watch_text, tmpstr );
    }

/*
 * Get Mesoscale discussion number
 */
    if (_showDiscuss) {
	ptxt = XmTextGetString (_discussText);
	sprintf ( tmpstr, "FOR ADDITIONAL INFORMATION SEE MESOSCALE DISCUSSION %s", ptxt );
        strcat (tmpstr, NL );
        strcat (tmpstr, NL );
	XtFree (ptxt);
        strcat ( watch_text, tmpstr );
    }

/*
 * Get forecaster and ending NNNN
 */
    sprintf (tmpstr, "..%s..%02d/%02d/%02d", 
		_fcstr[_fcstrStrc.current],
		(int)((_currDate/100)%100),
		(int)(_currDate%100),
		(int)(_currDate/10000)    );

    strcat (watch_text, tmpstr );
    strcat (watch_text, NL );
    strcat (watch_text, NL );

/*
 * Add WFO attention string
 */
    sprintf (tmpstr, "ATTN...WFO%s", _aWFO);
    cst_wrap( tmpstr, "...", &line_len, "\n", (char *)NULL, tmpstr, &ier);
    strcat (watch_text, tmpstr );
    strcat (watch_text, NL );
    strcat (watch_text, NL );

/*
 * End with NNNN
 */
    strcat (watch_text, "NNNN" );
    strcat (watch_text, NL );
}

/*=====================================================================*/

static void pgwsmw_getFrom ( int grpnum, char *fromline, int *iret )
/************************************************************************
 * pgwsmw_getFrom 							*
 *									*
 * This function generates a "from" line from the WSM lines.		*
 *									*
 * static void pgwsmw_getFrom ( grpnum, fromline, iret )		*
 *									*
 * Input parameters:							*
 *	grpnum		int	watch group number within VGF file	*
 *									*
 * Output parameters:							*
 *	*fromline   	char	"from" line				*
 *	*iret		int	status return				*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	12/99						*
 * D.W.Plummer/NCEP	 7/01	Chg SIGTYP_LINE to IMISSD in clo_from	*
 * M. Li/SAIC           04/02   replaced crg_ggnxt with crg_ggnhl       *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 ***********************************************************************/
{
    int		ne, ng, nelm, npts, fpos, nlines, ier, inxarry[10];
    int		totgrp, lowgrp; 
    char	vg_class, vg_type, tmpstr[80];
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    *iret = 0;
    fromline[0] = '\0'; /*CHNULL*/

    crg_ggnhl( GRPTYP_WATCH, &totgrp, &lowgrp, &ier );

    nlines = 0;
    for ( ng = lowgrp; ng <= totgrp; ng++ )  {

	crg_gginx( GRPTYP_WATCH, ng, sizeof(inxarry)/sizeof(inxarry[0]),
                   inxarry, &nelm, &ier );
	if ( nelm == 0 )  return;

	for ( ne = 0; ne < nelm; ne++ )  {

	    crg_gtyp( inxarry[ne], &vg_class, &vg_type, &ier );
            crg_goffset( inxarry[ne], &fpos, &ier );

	    if ( fpos > 0 &&
		 (int)vg_class == CLASS_LINES &&
		 (int)vg_type == SPLN_ELM )  {
		cvg_rdrec( cvg_getworkfile(), fpos, &el, &ier );

		if ( el.hdr.grpnum == grpnum )  {

		    npts = el.elem.wsm.info.numpts;
		    clo_from (WSM_ELM, IMISSD, npts, 0,
				&(el.elem.wsm.latlon[0]), 
				&(el.elem.wsm.latlon[npts]),
				sizeof(tmpstr), tmpstr, &ier );

		    if ((strlen (fromline) + strlen (tmpstr)) < (size_t)MAXFROMLEN) {
			if (nlines > 0) strcat (fromline, " AND ");
			strcat (fromline, tmpstr);
			nlines++;
		    }
		}
	    }
	}
    }
}

/*=====================================================================*/

static void pgwsmw_wsmCnty ( char *wsm,    char *cday, 
                             char *chour,  char *ctype,
			     int  iwnum,   char *enh_wsm, int *iret )
/************************************************************************
 * pgwsmw_wsmCnty 							*
 *									*
 * This function creates and formats the current active counties and 	*
 * it is appended to the current WSM to create a new enhanced WSM.	*
 *									*
 * static pgwsmw_wsmCnty (enhfnm, wsm, cday, chour, ctype, iwnum, iret)	*
 *									*
 * Input parameters:							*
 *	*wsm 		char	Original WSM text information		*
 *	*cday 		char	current day				*
 *	*chour 		char	current hour				*
 *      *ctype		char	watch type				*
 *      iwnum		int	watch number				*
 *									*
 * Output parameters:							*
 *	*enh_wsm	char	enhanced WSM message			*
 *	*iret		int	status return				*
 *				   -2 - could not read element		*
 *									*
 **									*
 * Log:									*
 * A. Hardy/NCEP	 2/04	Initial coding				*
 * A. Hardy/NCEP	 4/04   added cst_sort to sort the List elm	*
 * A. Hardy/NCEP	 4/04	Changed COUNTY to WBCMZ_TBL; wbc_mzhv	*
 * A. Hardy/NCEP	 4/04	Fixed sorting of List element		*
 * A. Hardy/NCEP	 6/04	Removed ':' from 'FOLLOWING' line	*
 * H. Zeng/SAIC		07/04	added watch no&type into message	*
 * H. Zeng/SAIC		07/04	modified passed-in arguments		*
 * A. Hardy/NCEP	 8/04	added ADD_WSM_WS_WT check		*
 * A. Hardy/NCEP	 9/04	initilized variable 'hold'		*
 * G. Grosshans/SPC	 3/05	Added status guidance wording		*
 * G. Grosshans/SPC	11/05	Updated guidance wording for WCN	*
 ***********************************************************************/
{
    int           vtime[5], endtm[5], ugcln, vtecln, lenc;
    char          **ugc_arr, **cnam_arr, **st_arr, **ind_arr;
    char	  prdcod[2], actn[4], offid[5], 
    		  phen[3], sigcd[2], etn[5], cntystr[10000];
    char	  **lstcnty;

    char	  outfil[FILE_FULLSZ], info[256], tmpvar[80]; 
    char          hold[128], prefs_tag[15], hold2[256];
    int           ii, more, ier, num, flag, nret, maxlen, snum;
    int           leng, len, isort, lns;
    long          size, curpos, cursiz;
    Boolean       good, hvmz, useln;
    FILE	  *ofp;
    VG_DBStruct	  el;
    ListData      *lst;
/*---------------------------------------------------------------------*/
        *iret = 0;
        more  = G_TRUE;
	good  = G_FALSE;
        curpos = sizeof(el.hdr) + sizeof(el.elem.fhed);
        hold2[0] = '\0';
        strcat (hold2, "THE WATCH STATUS MESSAGE IS FOR GUIDANCE PURPOSES ONLY.  PLEASE\n");
        strcat (hold2, "REFER TO WATCH COUNTY NOTIFICATION STATEMENTS FOR OFFICIAL\n");
        strcat (hold2, "INFORMATION ON COUNTIES...INDEPENDENT CITIES AND MARINE ZONES\n");
        strcat (hold2, "CLEARED FROM SEVERE THUNDERSTORM AND TORNADO WATCHES.\n");
        strcat (hold2, "$$\n\n");

        cfl_inqr (_wtchNam, NULL, &size, outfil, &ier);
	cvg_open (outfil, G_FALSE, &ofp, &ier);
        cfl_seek(ofp, curpos, 0, &ier);

        while ( ( more ) && ( curpos < size ) ) {

            cvg_rdhdr (outfil, ofp, (int)curpos, (int)size, &el, &flag, &ier);

            if ( ier == 0 && el.hdr.recsz > 0 ) {

                cvg_rdele (&el, (int)curpos, el.hdr.recsz, ofp, &ier);

                if ( ier != 0 ) {
                    *iret = -2;
                    more = G_FALSE;
                }

/*
 * Skip deleted elements, file-head element, and watch box.
 */
                if ( ( more ) && ( el.hdr.delete == 0 ) &&
                    ( el.hdr.vg_type != FILEHEAD_ELM ) )  {

		  if ( el.hdr.vg_type == LIST_ELM)   {
		    lst = &(el.elem.lst.data);
                      more = G_FALSE;
		    if ( lst->nitems > 0 ) {
			good = G_TRUE;
		    }
		  }
	       }
	   }
	   cursiz = curpos + el.hdr.recsz;
	   curpos = cursiz;
	}

	if ( good ) {

/*
 * Create the WSM with the active counties attached.
 * Initialize VTEC parameters.
 */
            ugcln = 1;
	    vtecln = 0;
	    lenc = sizeof ( cntystr );
	    cntystr[0] = '\0';
            strcpy ( prdcod, " " );
            strcpy ( actn, " " );
            strcpy ( offid, " " );
            strcpy ( phen, " " );
            strcpy ( sigcd, " " );
            strcpy ( etn, " " );

            for ( ii = 0; ii < 5; ii++ ) {
                vtime[ii] = 0;
                endtm[ii] = 0;
            }

/*
 * Check for marine zones, set flag if one was found.
 */
	    wbc_mzhv ( _statesAll, &hvmz, &ier );

/*
 * Create memory for arrays.
 */
            num = lst->nitems;
            ugc_arr = (char **)malloc(num * sizeof(char *));
            cnam_arr = (char **)malloc(num * sizeof(char *));
            st_arr = (char **)malloc(num * sizeof(char *));
            ind_arr = (char **)malloc(num * sizeof(char *));
	    lstcnty = (char **)malloc(num * sizeof(char *));

/*
 * Store list items in local array.
 */
            for ( ii = 0; ii < num; ii++ ) {
		lstcnty[ii] = (char *)malloc((257) * sizeof(char));
                cst_numb ( lst->item[ii], &snum, &ier);
	        clo_findnum ( WBCMZ_TBL, snum, maxlen, &nret, info, &ier );
	        cst_gtag ( "STNM", info, " ", tmpvar, &ier );
		cst_lstr ( tmpvar, &lns, &ier );
		if  (lns == 4 ){
		    sprintf( lstcnty[ii], "0%s", tmpvar);
		}
		else {
		   strcpy ( lstcnty[ii], tmpvar);
		}
	    }

/*
 * Sort the local copy of FIPS numbers.
 */
	    isort = 1;
	    cst_sort ( isort, &num, lstcnty, &num, lstcnty, &ier );

	    leng = 33;
            for ( ii = 0; ii < num; ii++ ) {
                cst_numb ( lstcnty[ii], &snum, &ier);
	        clo_findnum ( WBCMZ_TBL, snum, maxlen, &nret, info, &ier );


                ugc_arr[ii] = (char *)malloc((leng+1) * sizeof(char));
	        cst_gtag ( "STID", info, " ", tmpvar, &ier );
                len = strlen( tmpvar );
                cst_ncpy( ugc_arr[ii], tmpvar, len, &ier );

                cnam_arr[ii] = (char *)malloc((CNAM_LEN+1) * sizeof(char));
	        cst_gtag ( "NAME", info, " ", tmpvar, &ier );
                len = strlen( tmpvar );
                cst_ncpy( cnam_arr[ii], tmpvar, len, &ier );

/*
 * If have marine zones, then find the full name of the 
 * zone. Check for 'Z' in the id array. If it is a county 
 * station id, do not check name.
 */
	        if ( hvmz ) {
	            if (ugc_arr[ii][2] == 'Z') {
                        ctb_mzgnm ( cnam_arr[ii], cnam_arr[ii], &ier );
		        if ( ier < 0 ) {
		            printf("Do not have %s full name.\n", ugc_arr[ii]);
		        }
	            }
	        }

                st_arr[ii] = (char *)malloc((leng+1) * sizeof(char));
	        cst_gtag ( "ST", info,   "99999", tmpvar, &ier );
                len = strlen( tmpvar );
                cst_ncpy( st_arr[ii], tmpvar, len, &ier);

                ind_arr[ii] = (char *)malloc((leng+1) * sizeof(char));
                cst_ncpy( ind_arr[ii], " ", leng, &ier );
            }

	    wbc_dcty ( ugc_arr, cnam_arr, st_arr, &num, cday, chour, &lenc, 
		       &ugcln, &vtecln, prdcod, actn, offid, phen, sigcd, 
		       etn, vtime, endtm, ind_arr, cntystr, &ier );

/*
 * Free memory for arrays.
 */
            for( ii = 0; ii < num; ii++ ) {
                free( ugc_arr[ii] );
                free( cnam_arr[ii] );
                free( st_arr[ii] );
                free( ind_arr[ii] );
                free( lstcnty[ii] );
            }

            if ( ugc_arr ) {
                free( (char **) ugc_arr );
                free( (char **) cnam_arr );
                free( (char **) st_arr );
                free( (char **) ind_arr );
                free( (char **) lstcnty);
	    }

	    cst_rpst ( wsm, "NNNN", "&&", wsm, &ier );

/* 
 * Check if the following line should be placed in the enhanced
 * WSM text.
 */ 
            hold[0] = '\0';
	    strcpy (prefs_tag, "ADD_WSM_WS_WT");
	    ctb_pfbool (prefs_tag, &useln, &ier );

	    if ( useln == TRUE ) {
	        sprintf(hold, "\nSTATUS REPORT FOR %s %d \n", ctype, iwnum);
	    }

            strcat (hold, "\nSEVERE WEATHER THREAT CONTINUES FOR THE FOLLOWING AREAS \n\n");

/*
 * compose enhanced WSM message.
 */
	    strcpy (enh_wsm, wsm);
	    strcat (enh_wsm, hold);
	    strcat (enh_wsm, cntystr);
	    strcat (enh_wsm, hold2);
	}
}

/*=====================================================================*/

static void pgwsmw_getWtchNam ( void )
/************************************************************************
 * pgwsmw_getWtchNam							*
 *                                                                      *
 * This function returns the active watch file wwxxxx.txt file name.	*
 *                                                                      *
 * void static pgwsmw_getWtchNam ( )                                  	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	*fname		char		VG file name to write element	*
 *									*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 2/04		initial code			*
 ***********************************************************************/
{
    sprintf (_wtchNam, "ww%4.4d.vgf", _currWatch);
}
