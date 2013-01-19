#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "vgstruct.h"
#include "hints.h"
#include "drwids.h"
#include "pgprm.h"
#include "proto_xw.h"

static struct incInfo   _statesInc[MXSTATES];
static CwaIncInfo	_cwasInc[MXCWAS];

#define INC_COL		7	/* Number of button columns in includes		*/
#define	CWA_COL		5	/* Number of cwa btn columns in includes	*/
#define SPECS_LABEL	" %-7s  %-9s %-15s %-15s   ", "LAT", "LON", "Anchor Relative", "VOR Relative"
#define COUNTY_LABEL	"%-8s%-6s%-18s%-13s%-11s%-6s", "UGC", "State", "County Name", "Lat/Long", "Cnty Fips", "WFO   "
#define BNDS_FILE	"WBCMZ_BNDS"
#define SPECS		0
#define CNTY		1

static int	_whichLabel;

static Widget  	_pgwlstWin;
static Widget  	_pgwList;
static Widget	_pgwLabel;
static Widget   _statesForm;
static Widget   _ancptsForm;
static Widget   _wfoLabel;
static Widget	_stateRC;
static Widget	_cwaRC;
static Widget	_inoutRC;
static Widget     _ctlForm;
static WidgetList _ctlBtns;
static WidgetList _ctlkPbs;
static Widget	_clusterRc;
static Widget   _warning;
static Widget   _ancPtsBtn;
static Widget   _ancPtsLbl;

static Boolean	_showFlg, _showCnty, _showInfo, _showQC;

static	char	_wfoList[MAX_WFO_LEN];

/*
 *  Static Local Global variables describing watch box
 */
static int	_styleWbx, _clstStatus = 1, _ctlkStatus = 0;
static char     *_ctlBtnStr[] = { "Create", "Add/Del", "Clear" };

/*
 *  Inactive anchor point list&index
 */
static char     _inactiveAncLst[5][5];
static int      _ancLstIdx;

struct	_list_info
{
	float   lat;
	float   lon;
	int	cy_fips;
	char    desc[32];
	char    state[3]; 
	char    st_fips[8];
	char    wfo[10];
};

/*
 *  private callback functions
 */
void pgwlst_ancBtnCb ( Widget, long, XtPointer );
void pgwlst_clstOptCb( Widget, long, XtPointer );
void pgwlst_ctlkOptCb( Widget, long, XtPointer );
void pgwlst_dspOptCb ( Widget, long, XtPointer );
void pgwlst_prodBtnCb( Widget, long, XtPointer );
void pgwlst_statesCb ( Widget, long, XtPointer );
void pgwlst_cwasCb   ( Widget, long, XtPointer );
void pgwlst_Warn_cb  ( Widget, XtPointer, XtPointer );
void pgwlst_toggleCntyEh ( Widget, XtPointer, XEvent*, Boolean* );
void pgwlst_tglAncPtsEh  ( Widget, XtPointer, XEvent*, Boolean* );

/*
 *  private functions
 */
void pgwlst_createWarnBox ( Widget wdgt );
void pgwlst_loadCnty ( void );
void pgwlst_loadInfo ( void );
void pgwlst_loadQC   ( void );
void pgwlst_loadWFO  ( void );
void pgwlst_updAncLst ( char *sysin, int np, float *x,
                           float *y, int iv, char *disdir, 
			   char *stn, int *iret );

/************************************************************************
 * nmap_pgwlst.c							*
 *									*
 * This module defines a watch county list editing popup window for	*
 * product generation.							*
 *									*
 * CONTENTS:								*
 *	pgwlst_create()		create the watch list editing window	*
 *	pgwlst_popup()		pop up the watch list editing window	*
 *	pgwlst_popdown()	pop down the watch list editing window	*
 *	pgwlst_update()		updates the county list			*
 *	pgwlst_setShowFlg()	sets local global _showFlg		*
 *	pgwlst_setShowInfo()	sets local global _showInfo		*
 *	pgwlst_setShowCnty()	sets local global _showCnty		*
 *	pgwlst_setShowQC()	sets local global _showQC		*
 *	pgwlst_clear()		clears the county list			*
 *      pgwlst_createWarnBox()  create the warning pop up box		*
 *      pgwlst_popupWarn()      pop up the warning box 			*
 *      pgwlst_setBtnSen()      sets the sensitivity of the push buttons*
 *      pgwlst_setPanesSen()    sets the sensitivity of the 3 panes     *
 *      pgwlst_getStateBtns()   get structure for state button widgets  *
 *	pgwlst_getCwaBtns()	get structure for cwa btn widgets	*
 *	pgwlst_getStatesForm()	get widget id for _statesForm		*
 *									*
 *	pgwlst_isUp()		query if the list window is up 		*
 *									*
 *	pgwlst_dspOptCb()	callback for display option buttons	*
 *      pgwlst_clstOptCb()      callback for clustering option buttons  *
 *	pgwlst_ctlkOptCb()	callback for county lock opt. btns	*
 *	pgwlst_cmOptCb()	callback for county management		*
 *      pgwlst_ancBtnCb()       callback for "Toggle Anchor Pts" btn    *
 *      pgwlst_prodBtnCb()      callback for "Generate Products" btns   *
 *      pgwlst_statesCb()       callback for state toggle buttons       *
 *	pgwlst_cwasCb()		callback for CWA toggle buttons		*
 *	pgwlst_toggleCntyEh()	event handler for toggling counties	*
 *      pgwlst_tglAncPtsEh ()   event handler for toggling anchor pts   * 
 *      pgwlst_Warn_cb()        callback for the warning pop up		*
 *									*
 *	pgwlst_loadInfo()	load current specifications		*
 *	pgwlst_loadCnty()	load current county (hotlist) info	* 
 *	pgwlst_loadQC()		load "QC Counties" list			*
 *	pgwlst_updAncLst()	updates the anchor list			*
 *	pgwlst_updVorLst()	updates the vor list			*
 *      pgwlst_getClstStatus()  get clustering status                   *
 *	pgwlst_getCtlkStatus()  get county lock status			*
 *      pgwlst_getInaAncPts ()  get inactive anchor pts list            *
 ***********************************************************************/

/*=====================================================================*/

Widget pgwlst_create ( Widget parent )
/************************************************************************
 * pgwlst_create							*
 *									*
 * This function creates the county list popup window.			*
 *									*
 * Widget pgwlst_create(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * pgwlst_create	Widget  ID of the county list popup window	*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		10/97						*
 * C. Lin/EAI		01/98	add formatting area			*
 * I. Durham/GSC	05/98	changed underscore decl. to an include	*
 * D.W.Plummer/NCEP	12/98	added calls to setShowCnty and Info	*
 * S. Law/GSC		12/98	moved formatting to pgwfmt_create	*
 * D.W.Plummer/NCEP	 4/99	add initialization of _whichLabel	*
 * S. Law/GSC		02/00	added add/delete counties		*
 * H. Zeng/EAI          08/00   made btnstr locally global              *
 * H. Zeng/EAI          01/01   added "States Include" area             *
 * H. Zeng/EAI          02/01   added "Clustering" options              *
 * H. Zeng/EAI          06/01   added "Toggle Anchor Pts" button        *
 * H. Zeng/EAI          11/01   added product pane                      *
 * H. Zeng/EAI          12/01   unmanage certain buttons                *
 * M. Li/SAIC		06/02	added WCC/WCL				*
 * H. Zeng/XTRIA	06/03   added "County Lock" radio buttons	*
 * H. Zeng/XTRIA	07/03	added _ctlkPbs variable			*
 * J. Wu/SAIC		12/03	add space between Wfo and States lables	*
 * E. Safford/SAIC	05/05	free clst_pbs WidgetList		*
 * H. Zeng/SAIC		05/05	added "QC Counties" toggle btn		*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 * H. Zeng/SAIC		11/05	toggled "QC counties" btn on		*
 * H. Zeng/SAIC		01/05	added CWA include line			*
 * H. Zeng/SAIC		02/06	toggled "Specifications" btn on		*
 ***********************************************************************/
{
    Widget pane, optrc, form, button, label, prod_form;
    Widget cnty_labl, prev_wid, clst_labl;
    Widget ctlk_labl, rc2, row_col;
    WidgetList clst_pbs;
    int    toff = 5, loff = 2, nrow, nitems;
    long   ii, nn;
    char   labelstr[128];
    char   *dspopts[] = {"Specifications", "QC Counties", "County List"},
           *clst_str[] = { "Off", "On" },
           *ctlk_str[] = { "Off", "On" },
           *prodbtns[] = {"Generate WCC/WCL", "Generate WCL", "Watch Format"};
    char   fontname[]  = 
                    "-adobe-courier-bold-r-normal-*-*-120-*-*-m-*-*-*";
    Display	*dsp;
    XmFontListEntry flentry;
    XmFontList	fontlist;
/*---------------------------------------------------------------------*/
/*
 * create dialog shell
 */
    _pgwlstWin = XmCreateFormDialog(parent, "pgwlst_popup",
				    NULL, 0);
    XtVaSetValues(_pgwlstWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pgwlstWin),
		  XmNtitle, "Watch Specifications and County List",
		  NULL);
    
/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("pgwlst_pane",
			    xmPanedWindowWidgetClass, _pgwlstWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    
/*
 * create display options 
 */
    optrc = XtVaCreateWidget("pgwlst_optrc",
			     xmRowColumnWidgetClass, pane,
			     XmNorientation,    XmHORIZONTAL,
			     XmNpacking,       	XmPACK_COLUMN,
			     XmNspacing,       	40,
			     XmNradioBehavior,	True,
			     NULL);

    nn = XtNumber(dspopts);
    for ( ii = 0; ii < nn; ii++ )  {
	button = XtVaCreateManagedWidget (dspopts[ii],
					  xmToggleButtonWidgetClass, 
					  optrc, 
                                          XmNtraversalOn,     FALSE,
                                          NULL);

	if ( ii == 0 )  {
	    XmToggleButtonSetState (button, True, False);
	}

	XtAddCallback (button, XmNarmCallback, 
		       (XtCallbackProc)pgwlst_dspOptCb, (XtPointer) ii);
    }

    XtManageChild(optrc);
    
/*
 * create county list area 
 */
    form = XtVaCreateWidget("form",
			    xmFormWidgetClass,	pane,
			    XmNheight,		130,
			    NULL);

/*
 * create the county list label 
 */
    _pgwLabel = XtVaCreateManagedWidget("pgwlst_label",
					xmLabelWidgetClass,	form,
					XmNtopAttachment,	XmATTACH_FORM,
					XmNmarginWidth,		0,
					NULL);
    sprintf (labelstr, COUNTY_LABEL);
    NxmLabel_setStr (_pgwLabel, labelstr);
    _whichLabel = CNTY;
    
/*
 * create the county list 
 */
    _pgwList = XmCreateScrolledList(form, "pgwlst_list", NULL, 0);
    XtVaSetValues(XtParent(_pgwList),
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		_pgwLabel,
		  XmNtopOffset,		5,
		  XmNleftAttachment,	XmATTACH_OPPOSITE_WIDGET,
		  XmNleftWidget,	_pgwLabel,
		  XmNrightAttachment,	XmATTACH_FORM,
		  XmNbottomAttachment,	XmATTACH_FORM,
		  XmNscrollingPolicy,	XmAPPLICATION_DEFINED,
		  NULL);

    XtVaSetValues(_pgwList,
		  XmNscrollBarDisplayPolicy,	XmSTATIC,
		  XmNitemCount,			0,
		  XmNvisibleItemCount,		6,
		  NULL);
    
    XtManageChild(_pgwList);
    XtManageChild(form);

/*
 * Create county management buttons
 */
    _ctlForm  = (Widget)XtVaCreateManagedWidget ("_cnty_mgmt_formW",
                xmFormWidgetClass,      pane,
                NULL);

    cnty_labl = XtVaCreateManagedWidget("Counties:",
			    xmLabelWidgetClass,	_ctlForm,
			    XmNtopAttachment,	XmATTACH_FORM,
                            XmNtopOffset,       3,
			    XmNleftAttachment,  XmATTACH_FORM,
			    NULL);

    prev_wid = cnty_labl;
    _ctlBtns = (WidgetList)XtMalloc(XtNumber(_ctlBtnStr) * sizeof(Widget));
    nn = XtNumber (_ctlBtnStr);

    for ( ii = 0; ii < nn; ii++ )  {
            _ctlBtns[ii] = XtVaCreateManagedWidget(_ctlBtnStr[ii],
			       xmPushButtonWidgetClass,	_ctlForm,
			       XmNtopAttachment,	XmATTACH_FORM,
			       XmNleftAttachment,       XmATTACH_WIDGET,
                               XmNleftWidget,           prev_wid,
                               XmNleftOffset,           loff,
			       NULL);

            XtAddCallback (_ctlBtns[ii], XmNactivateCallback,
                        (XtCallbackProc)pgwlst_cmOptCb, (XtPointer)ii);
            prev_wid = _ctlBtns[ii];
    }

    clst_labl = XtVaCreateManagedWidget ("Clustering:",
			   xmLabelWidgetClass, _ctlForm,
			   XmNtopAttachment,   XmATTACH_FORM,
                           XmNtopOffset,       3,
			   XmNleftAttachment,  XmATTACH_WIDGET,
                           XmNleftWidget,      prev_wid,
                           XmNleftOffset,      loff+3,
			   NULL );

    _clusterRc = XtVaCreateManagedWidget ("clst_rowcol",
				xmRowColumnWidgetClass, _ctlForm,
				XmNradioBehavior,       TRUE,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				XmNleftAttachment,      XmATTACH_WIDGET,
				XmNleftWidget,          clst_labl,
				NULL );

    nn = XtNumber ( clst_str );
    clst_pbs = (WidgetList)XtMalloc( (size_t)nn * sizeof(Widget));
    for ( ii = 0; ii < nn; ii++ )  {
	clst_pbs[ii] = XtVaCreateManagedWidget (clst_str[ii],
			    xmToggleButtonWidgetClass, _clusterRc,
			    XmNtraversalOn,            FALSE,
			    XmNset,                    (ii == _clstStatus),
			    NULL );
      
	XtAddCallback ( clst_pbs[ii], XmNarmCallback,
			(XtCallbackProc)pgwlst_clstOptCb, (XtPointer)ii);

    }

    XtFree( (XtPointer) clst_pbs );

/*
 * Create "County Lock" radio button group.
 */
    ctlk_labl = XtVaCreateManagedWidget ("County Lock:",
			   xmLabelWidgetClass, _ctlForm,
			   XmNtopAttachment,   XmATTACH_WIDGET,
			   XmNtopWidget,       cnty_labl,
                           XmNtopOffset,       10,
			   XmNleftAttachment,  XmATTACH_FORM,
			   NULL );

    rc2 = XtVaCreateManagedWidget ("ctlk_rowcol",
		           xmRowColumnWidgetClass, _ctlForm,
			   XmNradioBehavior,       TRUE,
			   XmNorientation,         XmHORIZONTAL,
			   XmNpacking,             XmPACK_TIGHT,
			   XmNtopAttachment,       XmATTACH_OPPOSITE_WIDGET,
			   XmNtopWidget,           ctlk_labl,
			   XmNtopOffset,	   -3,
			   XmNleftAttachment,      XmATTACH_WIDGET,
			   XmNleftWidget,          ctlk_labl,
		           NULL );

    nn = XtNumber ( ctlk_str );
    _ctlkPbs = (WidgetList)XtMalloc( (size_t)nn * sizeof(Widget));
    for ( ii = 0; ii < nn; ii++ )  {
	_ctlkPbs[ii] = XtVaCreateManagedWidget (ctlk_str[ii],
			    xmToggleButtonWidgetClass, rc2,
			    XmNtraversalOn,            FALSE,
			    XmNset,                    (ii == _ctlkStatus),
			    NULL );
      
	XtAddCallback ( _ctlkPbs[ii], XmNarmCallback,
			(XtCallbackProc)pgwlst_ctlkOptCb, (XtPointer)ii);

    }

/*
 * Create "WFOs:", "States Included:" and "CWAs:" area.
 */
    _statesForm = XtVaCreateWidget("form",
			           xmFormWidgetClass,	pane,
			           NULL );

/*
 * create "WFOs:" label 
 */
    _wfoLabel = XtVaCreateManagedWidget ("wfolabel",
				     xmLabelWidgetClass,	_statesForm,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff,
				     NULL);
    NxmLabel_setStr(_wfoLabel, "WFOs: ");

/*
 * create "States:" label 
 */
    label = XtVaCreateManagedWidget ("label",
				     xmLabelWidgetClass,	_statesForm,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff+35,
				     NULL);
    NxmLabel_setStr(label, "States:");


/*
 * create state button array 
 */
    nrow = (MXSTATES % INC_COL) ? 
                 (MXSTATES / INC_COL) + 1 : MXSTATES / INC_COL;
    _stateRC = XtVaCreateManagedWidget("incrowcol",
				 xmRowColumnWidgetClass,    _statesForm,
				 XmNtopAttachment,	    XmATTACH_FORM,
				 XmNleftAttachment,	    XmATTACH_WIDGET,
                                 XmNleftWidget,             label,
				 XmNtopOffset,		    toff+28,
                                 XmNleftOffset,             15,
				 XmNrowColumnType,	    XmWORK_AREA,
				 XmNradioAlwaysOne,	    FALSE,
				 XmNorientation,	    XmHORIZONTAL,
				 XmNpacking,		    XmPACK_COLUMN,
				 XmNnumColumns,		    nrow,
				 NULL);

    for (ii = 0; ii < MXSTATES; ii++) {
	_statesInc[ii].wid = XtVaCreateManagedWidget ("ST", 
					      xmToggleButtonWidgetClass,
					      _stateRC,
					      XmNuserData,	_statesInc,
                                              XmNtraversalOn,   FALSE,
					      NULL);

	XtAddCallback (_statesInc[ii].wid, XmNvalueChangedCallback,
		       (XtCallbackProc) pgwlst_statesCb,
		       (XtPointer) ii);
    }

/*
 * create "CWAs:" label 
 */
    label = XtVaCreateManagedWidget ("label",
				     xmLabelWidgetClass,	_statesForm,
				     XmNtopAttachment,		XmATTACH_WIDGET,
				     XmNtopWidget,		_stateRC,
				     XmNtopOffset,		15,
				     NULL);
    NxmLabel_setStr(label, "CWAs:");

/*
 * create "IN,OUT" label 
 */
    _inoutRC = XtVaCreateManagedWidget("in_out_rc",
				 xmRowColumnWidgetClass,    _statesForm,
				 XmNtopAttachment,	    XmATTACH_WIDGET,
				 XmNtopWidget,		    _stateRC,
				 XmNtopOffset,		    0,
				 XmNleftAttachment,	    XmATTACH_WIDGET,
                                 XmNleftWidget,             label,
				 XmNleftOffset,		    0,
				 XmNrowColumnType,	    XmWORK_AREA,
				 XmNorientation,	    XmVERTICAL,
				 XmNpacking,		    XmPACK_COLUMN,
				 XmNspacing,		    13,
				 NULL);

    dsp = XtDisplay (_statesForm);
    flentry = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);

    label = XtVaCreateManagedWidget ("label_in",
				     xmLabelWidgetClass,    _inoutRC,
			             XmNfontList,	    fontlist,
				     NULL);
    NxmLabel_setStr(label, "IN");

    label = XtVaCreateManagedWidget ("label_out",
				     xmLabelWidgetClass,    _inoutRC,
			             XmNfontList,	    fontlist,
				     NULL);
    NxmLabel_setStr(label, "OUT");

/*
 * create CWA button array 
 */
    nrow = (MXCWAS % CWA_COL) ? 
                 (MXCWAS / CWA_COL) + 1 : MXCWAS / CWA_COL;
    _cwaRC = XtVaCreateWidget("cwa_rowcol",
				 xmRowColumnWidgetClass,    _statesForm,
				 XmNtopAttachment,	    XmATTACH_WIDGET,
			         XmNtopWidget,		    _stateRC,
			         XmNtopOffset,		    -10,
				 XmNleftAttachment,	    XmATTACH_WIDGET,
                                 XmNleftWidget,             _inoutRC,
			         XmNleftOffset,		    0,
				 XmNrowColumnType,	    XmWORK_AREA,
				 XmNorientation,	    XmHORIZONTAL,
				 XmNpacking,		    XmPACK_COLUMN,
				 XmNnumColumns,		    nrow,
				 NULL);

    for (ii = 0; ii < MXCWAS; ii++) {
	_cwasInc[ii].form_wid = XtVaCreateWidget ("form_wid", 
				 xmFormWidgetClass, _cwaRC,
				 NULL);

        row_col = XtVaCreateWidget("rowcol",
				 xmRowColumnWidgetClass,    
							_cwasInc[ii].form_wid,
				 XmNtopAttachment,	    XmATTACH_FORM,
				 XmNleftAttachment,	    XmATTACH_FORM,
				 XmNrowColumnType,	    XmWORK_AREA,
				 XmNorientation,	    XmVERTICAL,
				 XmNpacking,		    XmPACK_TIGHT,
				 XmNradioBehavior,	    TRUE,
				 XmNradioAlwaysOne,	    TRUE,
				 NULL);

        _cwasInc[ii].in_btn = XtVaCreateManagedWidget (" ", 
				 xmToggleButtonWidgetClass, row_col,
                                 XmNtraversalOn,            FALSE,
			         XmNset,                    FALSE,
				 NULL);

	XtAddCallback (_cwasInc[ii].in_btn, XmNdisarmCallback,
		       (XtCallbackProc) pgwlst_cwasCb,
		       (XtPointer) ii);

	_cwasInc[ii].out_btn = XtVaCreateManagedWidget (" ", 
				 xmToggleButtonWidgetClass, row_col,
                                 XmNtraversalOn,            FALSE,
			         XmNset,                    FALSE,
				 NULL);

	XtAddCallback (_cwasInc[ii].out_btn, XmNdisarmCallback,
		       (XtCallbackProc) pgwlst_cwasCb,
		       (XtPointer) ii);

        _cwasInc[ii].lbl_wid = XtVaCreateManagedWidget ("label",
				 xmLabelWidgetClass,    _cwasInc[ii].form_wid,
				 XmNtopAttachment,	    XmATTACH_FORM,
				 XmNtopOffset,		    20,
				 XmNleftAttachment,	    XmATTACH_WIDGET,
				 XmNleftWidget,		    row_col,
				 XmNleftOffset,		    0,
				 XmNalignment,		XmALIGNMENT_BEGINNING,
				 XmNfontList,		    fontlist,
				 NULL);
        NxmLabel_setStr(_cwasInc[ii].lbl_wid, "WFO ");

        XtManageChild ( row_col );
        XtManageChild ( _cwasInc[ii].form_wid );

    }

    XtManageChild ( _cwaRC );
    XtManageChild ( _statesForm );
    XmFontListFree(fontlist);

/*
 * Create "Toggle Anchor Points" area.
 */
    _ancptsForm = XtVaCreateManagedWidget("form",
			           xmFormWidgetClass,	pane,
			           NULL );

/*
 * create "Toggle Anchor Points" button.
 */
    _ancPtsBtn = XtVaCreateManagedWidget("Toggle Anchor Pts",
		       xmPushButtonWidgetClass,	_ancptsForm,
		       XmNtopAttachment,	XmATTACH_FORM,
                       XmNtopOffset,            3,
		       XmNleftAttachment,       XmATTACH_FORM,
		       NULL);

    XtAddCallback (_ancPtsBtn, XmNactivateCallback,
                (XtCallbackProc)pgwlst_ancBtnCb, (XtPointer)NULL );

/*
 * Create "Inactive" label.
 */
    _ancPtsLbl = XtVaCreateManagedWidget ("Inactive:",
			    xmLabelWidgetClass,	    _ancptsForm,
			    XmNtopAttachment,	    XmATTACH_FORM,
			    XmNtopOffset,	    8,
                            XmNleftAttachment,      XmATTACH_WIDGET,
                            XmNleftWidget,          _ancPtsBtn,
                            XmNleftOffset,          10,
			    NULL);

/*
 * Create "Generate Product" area.
 */
    nitems = XtNumber( prodbtns );
    prod_form = XtVaCreateManagedWidget("form",
			           xmFormWidgetClass,	pane,
			           XmNfractionBase,     (nitems * 100),
			           NULL );

    for ( ii = 0; ii < nitems; ii++ )  {

	button = XtVaCreateManagedWidget ( prodbtns[ii], 
			xmPushButtonWidgetClass, prod_form,
			XmNheight,               25,
			XmNwidth,                100,
                        XmNleftAttachment,       XmATTACH_POSITION,
			XmNleftPosition,         (ii * 100),
			XmNrightAttachment,      XmATTACH_POSITION,
			XmNrightPosition,        ((ii + 1) * 100),
			NULL );
 
        if ( ii == 1 || ii == 2 ) {
             XtUnmanageChild (button);
        }  
	XtAddCallback ( button, XmNactivateCallback,
			(XtCallbackProc)pgwlst_prodBtnCb, (XtPointer)ii );

    }

    XtManageChild (pane);

    pgwlst_setShowFlg (FALSE);

    pgwlst_setShowInfo (TRUE);
    pgwlst_setShowCnty (FALSE);
    pgwlst_setShowQC   (FALSE);
    pgwlst_createWarnBox (_pgwlstWin );

    return(_pgwlstWin);
}

/*=====================================================================*/

void pgwlst_popup ( Boolean reset )
/************************************************************************
 * pgwlst_popup								*
 *									*
 * This function pops up the county list popup window and show the	*
 * county list associated with the watch.				*
 *									*
 * void pgwlst_popup ( reset )						*
 *									*
 * Input parameters:							*
 *	reset		Boolean     clear watch list or not             *
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		10/97						*
 * H. Zeng/EAI          08/00       add a new parameter                 *
 * H. Zeng/EAI          01/01       added unmanaging of _statesForm     *
 * H. Zeng/EAI          06/01       modified to add inactive list       *
 * H. Zeng/XTRIA	06/03	    added check to county lock status	*
 * H. Zeng/XTRIA	07/03	    set County Lock button to "Off"	*
 * J. Wu/SAIC		12/03	    remove '\n' from "Inactive:" str 	*
 * H. Zeng/SAIC		06/05	    added call to pgwlst_loadQC()	*
 * H. Zeng/SAIC		01/06	    added call to pgwatch_loadStCwa()	*
 ***********************************************************************/
{
    int	  ii, nitem;
    char  ina_ancpts[25], ancpts_lbl[40];
    XmString	xmstr;
/*---------------------------------------------------------------------*/
/*
 * Set "County Lock" button to "Off".
 */
    XmToggleButtonSetState ( _ctlkPbs[0], TRUE, TRUE );
    pgwlst_ctlkOptCb ( NULL, 0, NULL );
    

    if(reset) {
      XtVaGetValues(_pgwList, XmNitemCount, &nitem, NULL);
      if (nitem != 0) {
	   XmListDeleteAllItems(_pgwList);
      }
      pgwlst_setBtnSen(FALSE);
      if( XtIsManaged(_statesForm) ) {
          XtUnmanageChild(_statesForm);
      }
    }
    else {

      if ( !pgwlst_getCtlkStatus () ) {
         pgwlst_setBtnSen(TRUE);
      }
      else {
         pgwlst_setBtnSen(FALSE);
      }
     
      pgwlst_loadInfo();
      pgwlst_loadCnty();
      pgwlst_loadQC();
      pgwlst_loadWFO();
      pgwatch_loadStCwa();

    }

/*
 * Reset the inactive anchor point list
 */
    for (ii = 0; ii < 5; ii++ ) {
         _inactiveAncLst[ii][0] = '\0';
    }
    _ancLstIdx = 0;

/*
 * Update the inactive anchor points label
 */ 
    pgwlst_getInaAncPts (ina_ancpts);
    sprintf ( ancpts_lbl, "Inactive: %s", ina_ancpts );

    xmstr = XmStringCreateLocalized ( ancpts_lbl );
    XtVaSetValues( _ancPtsLbl, XmNlabelString, xmstr, NULL );
    XmStringFree( xmstr ); 

/*
 * Make "Toggle Anchor Pts" button sensitive.
 */
    if ( !pgwlst_getCtlkStatus ()   &&
	 !XtIsSensitive(_ancPtsBtn)    ) {
	XtSetSensitive (_ancPtsBtn, TRUE);
    }

    XtManageChild(_pgwlstWin);
}

/*=====================================================================*/

void pgwlst_popdown ( void ) 
/************************************************************************
 * pgwlst_popdown							*
 *									*
 * This function pops down watch list editing window.			*
 *									*
 * void pgwlst_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		10/97						*
 * S. Law/GSC		03/00	removed call to mcanvw_disarmDynamic	*
 ***********************************************************************/
{
    if (XtIsManaged(_pgwlstWin)) {
	XtUnmanageChild(_pgwlstWin);

	XtSetSensitive (_ctlForm, TRUE);
    }
}

/*=====================================================================*/

void pgwlst_update ( VG_DBStruct *el ) 
/************************************************************************
 * pgwlst_update                                                    	*
 *                                                                      *
 * This function updates the county list if the list window is up. 	*
 *                                                                      *
 * void pgwlst_update( el )                     			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el	VG_DBStruct	Pointer to WBOX element			*
 *									*
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      True -- up,	False -- down                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		10/97  						*
 * D.W.Plummer/NCEP	 6/98	Added style and shape information	*
 * G. Krueger/EAI	 6/98	Uniform status hints			*
 * G. Krueger/EAI       10/98   Using table for hints                   *
 * D.W.Plummer/NCEP	11/98	Changes for editing pgram watch edges	*
 * S. Law/GSC            1/99   Moved XtIsManaged check			*
 * D.W.Plummer/NCEP	 1/99	Remove clo_cntyinpoly 			*
 * D.W.Plummer/NCEP	 4/99	Remove saved vertex information		*
 * D.W.Plummer/NCEP	 4/99	Remove call to pgwatch_save		*
 * H. Zeng/SAIC		07/05	added call to pgwlst_loadQC()		*
 * H. Zeng/SAIC		01/06	added call to pgwatch_loadStCwa()	*
 ***********************************************************************/
{
	if ( _showFlg )  {

		_styleWbx = el->elem.wbx.info.w_style;

		if ( XtIsManaged(_pgwlstWin) )  {
	            pgwlst_loadInfo();
	            pgwlst_loadCnty();
                    pgwlst_loadQC();
	            pgwlst_loadWFO();
		    pgwatch_loadStCwa();
		}
	}
	else { 
	
		if ( XtIsManaged(_pgwlstWin) ) {
		    pgwlst_clear();
		}
	}

	mbotw_mouseSet(LMHINT_MOVEPOINT, MMHINT_DONE);
}

/*=====================================================================*/

void pgwlst_setShowFlg ( Boolean what )
/************************************************************************
 * pgwlst_setShowFlg                                                    *
 *                                                                      *
 * This function sets the local global _showFlg.			*
 *                                                                      *
 * void pgwlst_setShowFlg (what)                     			*
 *                                                                      *
 * Input parameters:                                                    *
 *	what	Boolean		Set flag to this			*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 2/98						*
 ***********************************************************************/
{
    _showFlg = what;
}

/*=====================================================================*/

void pgwlst_setShowCnty ( Boolean what )
/************************************************************************
 * pgwlst_setShowCnty                                                   *
 *                                                                      *
 * This function sets the local global _showCnty.			*
 *                                                                      *
 * void pgwlst_setShowCnty (what)                     			*
 *                                                                      *
 * Input parameters:                                                    *
 *	what	Boolean		Set flag to this			*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	12/98						*
 ***********************************************************************/
{
    _showCnty = what;
}

/*=====================================================================*/

void pgwlst_setShowInfo ( Boolean what )
/************************************************************************
 * pgwlst_setShowInfo                                                   *
 *                                                                      *
 * This function sets the local global _showInfo.			*
 *                                                                      *
 * void pgwlst_setShowInfo (what)                     			*
 *                                                                      *
 * Input parameters:                                                    *
 *	what	Boolean		Set flag to this			*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	12/98						*
 ***********************************************************************/
{
    _showInfo = what;
}

/*=====================================================================*/

void pgwlst_setShowQC ( Boolean what )
/************************************************************************
 * pgwlst_setShowQC	                                                *
 *                                                                      *
 * This function sets the local global _showQC.				*
 *                                                                      *
 * void pgwlst_setShowQC (what)                     			*
 *                                                                      *
 * Input parameters:                                                    *
 *	what	Boolean		Set flag to this			*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		05/05	initial coding				*
 ***********************************************************************/
{
    _showQC = what;
}

/*=====================================================================*/

void pgwlst_clear ( void )
/************************************************************************
 * pgwlst_clear                                                    	*
 *                                                                      *
 * This function clears the county list if the list window is up. 	*
 *                                                                      *
 * void pgwlst_clear( )                     				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 2/98						*
 * J. Wu/GSC		 5/01	free XmString				*
 * E. Safford/GSC	06/01	fix error in xmstr use			*
 ***********************************************************************/
{
int		nlines;
XmString	xmstr;
/*---------------------------------------------------------------------*/

	if ( XtIsManaged(_pgwlstWin) ) {

	    XmListDeleteAllItems(_pgwList);

	    nlines = 1;
	    xmstr = XmStringCreateLocalized("No counties included in watch area.");

	    XtVaSetValues(_pgwList,
			XmNitemCount,		nlines,
			XmNitems,		&xmstr,
			NULL);
	    XmStringFree ( xmstr );
	}
        	
	pgwlst_setShowFlg( FALSE );
}

/*=====================================================================*/

Boolean pgwlst_isUp ( void ) 
/************************************************************************
 * pgwlst_isUp                                                    	*
 *                                                                      *
 * This function queries whether the county list window is up. 		*
 *                                                                      *
 * Boolean pgwlst_isUp()                     				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * pgwlst_isUp		Boolean       True -- up, False -- down         *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      10/97  						*
 ***********************************************************************/
{
    return ( XtIsManaged(_pgwlstWin) );
}

/*=====================================================================*/
/* ARGSUSED */
void pgwlst_dspOptCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pgwlst_dspOptCb                                                  	*
 *                                                                      *
 * Callback function for display option radio buttons.    		*
 *                                                                      *
 * void pgwlst_dspOptCb(w, which, call)                             	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        which button                               *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		01/98                                           *
 * S. Law/GSC		11/98	renamed to fit convention		*
 * S. Law/GSC		12/98	rewrote to use spec/county		*
 * H. Zeng/SAIC		05/05	added "QC Counties" case		*
 ***********************************************************************/
{
    switch(which) {

      case 0:	/* SPECIFICATIONS */

	pgwlst_setShowInfo (TRUE);
	pgwlst_setShowCnty (FALSE);
        pgwlst_setShowQC   (FALSE);
        pgwlst_loadInfo ();

	break;

      case 1:	/* QC Counties */

	pgwlst_setShowInfo (FALSE);
	pgwlst_setShowCnty (FALSE);
        pgwlst_setShowQC   (TRUE );
        pgwlst_loadQC ();

	break;

      case 2:	/* COUNTY LIST */

	pgwlst_setShowInfo (FALSE);
	pgwlst_setShowCnty (TRUE);
        pgwlst_setShowQC   (FALSE);
        pgwlst_loadCnty ();

	break;

    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwlst_clstOptCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgwlst_clstOptCb                                                     *
 *                                                                      *
 * Callback function for the clusteing radio buttons.		        *
 *                                                                      *
 * void pgwlst_clstOptCb (wid, which, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs               XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI		02/01	Initial coding				*
 ***********************************************************************/
{
    _clstStatus = (int)which;
}

/*=====================================================================*/
/* ARGSUSED */
void pgwlst_ctlkOptCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgwlst_ctlkOptCb                                                     *
 *                                                                      *
 * Callback function for the county lock radio buttons.		        *
 *                                                                      *
 * void pgwlst_ctlkOptCb (wid, which, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   cbs               XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA	06/03	Initial coding				*
 * H. Zeng/XTRIA	07/03	added call to XmToggleButtonSetState()	*
 * H. Zeng/SAIC		01/06	added operations on _cwaRC		*
 ***********************************************************************/
{
  int	nn, ii;
/*---------------------------------------------------------------------*/
	_ctlkStatus = (int)which;

/*
 * Force Button "which" to be selected on GUI to fix a certain
 * problem of radio button group on linux 2.4
 */
        XmToggleButtonSetState ( _ctlkPbs[which], TRUE, TRUE );      

        nn = XtNumber (_ctlBtnStr);
	switch ( _ctlkStatus ) {

	   case 0:
                for ( ii = 0; ii < nn; ii++ ) {
		    XtSetSensitive ( _ctlBtns[ii], TRUE);
		}
		XtSetSensitive ( _stateRC,   TRUE); 
		XtSetSensitive ( _cwaRC,     TRUE); 
		XtSetSensitive ( _inoutRC,   TRUE); 
		XtSetSensitive ( _ancPtsBtn, TRUE);
		XtSetSensitive ( _clusterRc, TRUE);

		break;

	   case 1:
                for ( ii = 0; ii < nn; ii++ ) {
		    XtSetSensitive ( _ctlBtns[ii], FALSE);
		}
		XtSetSensitive ( _stateRC,   FALSE); 
		XtSetSensitive ( _cwaRC,     FALSE); 
		XtSetSensitive ( _inoutRC,   FALSE); 
		XtSetSensitive ( _ancPtsBtn, FALSE);
		XtSetSensitive ( _clusterRc, FALSE);

		break;
	}
}

/*=====================================================================*/
/* ARGSUSED */
void pgwlst_cmOptCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwlst_cmOptCb							*
 *									*
 * Callback function for county management buttons.			*
 *									*
 * void pgwlst_cmOptCb (wid, which, call)				*
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
 * D.W.Plummer/NCEP	 1/00						*
 * S. Law/GSC		02/00	added add/delete counties		*
 * D.W.Plummer/NCEP	 2/00	cleanup; call pgwatch_restore directly	*
 * H. Zeng/EAI          03/00   added confirmation OK callback          *
 * H. Zeng/EAI          04/00   changed cursor name                     *
 * E. Safford/GSC	11/00	don't order restore when starting       *
 *				    county add/delete			*
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters          *
 * H. Zeng/EAI          01/01   added unmanaging of _statesForm         *
 * H. Zeng/EAI          06/01   added default choice                    *
 * H. Zeng/EAI          11/01   added confirmation OK for WCC           *
 * E. Safford/SAIC	12/01	add unmanage/manage to resize wfo label *
 * J. Wu/SAIC		06/02	unmanage state buttons when clear cnty  *
 * H. Zeng/XTRIA	06/03   changed "Toggle Anchor Pts" sensitivity *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * H. Zeng/SAIC		07/05	added call to pgwlst_loadQC()		*
 * H. Zeng/SAIC		11/05	added call to pgwlst_loadWFO for case 4 *
 * H. Zeng/SAIC		01/06	added call to pgwatch_clrNumCwas()	*
 ***********************************************************************/
{
    int		location, ier;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    location = pgactv_getElmLoc();
    cvg_rdrec(cvg_getworkfile(), location, &el, &ier);

    pgwatch_save (&el);

    switch(which) {
      case 0:	/* CREATE COUNTIES */

	pgwatch_clrNumCwas ();
  	pgwatch_redocnty   (); 

	break;

      case 1:	/* ADD/DELETE COUNTIES */

	XtSetSensitive (_ctlForm,   FALSE);
        XtSetSensitive (_ancPtsBtn, FALSE);

	mcanvw_disarmDynamic();
	mcanvw_setPressFunc ((XtEventHandler)&pgwlst_toggleCntyEh, CURS_POINT_SELECT);
	mbotw_mouseSet (LMHINT_TOGGLE, MMHINT_DONE);
	
	break;

      case 2:	/* CLEAR COUNTIES */

	pgwatch_clrNumCwas ();
	pgwatch_clrcnty    ();

	break;

      case 3:	/* CONFIRMATION OK CALLBACK WATCH FORMAT */

	pgwatch_redocnty ( );
        pgwfmt_setWatch (&el);

	break;

      case 4:	/* CONFIRMATION OK CALLBACK for WCC */

	pgwatch_clrNumCwas ();
	pgwatch_redocnty   ();
        pgwlst_loadWFO     ();
        pgwfmt_setWCC      (&el);
	pgwfmt_popupWCC    ();

	break;

      default:

        break;

    } /* the end of switch(which) */


    pgwlst_loadCnty ();
    pgwlst_loadQC ();
    pgwlst_loadWFO ();
    pgwatch_loadStCwa ();


    if (which != 1) {		/* case 1 above */
        pgwatch_restore ();
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwlst_ancBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwlst_ancBtnCb							*
 *									*
 * Callback function for "Toggle Anchor Pts" button.			*
 *									*
 * void pgwlst_ancBtnCb (wid, which, call)				*
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
 * H. Zeng/EAI          06/01   initial coding                          *
 * H. Zeng/XTRIA	06/03   added _ctlForm greying out		*
 ***********************************************************************/
{
    XtSetSensitive (_ancPtsBtn, FALSE);
    XtSetSensitive (_ctlForm,   FALSE);
    mcanvw_disarmDynamic();
    mcanvw_setPressFunc ((XtEventHandler)&pgwlst_tglAncPtsEh, CURS_POINT_SELECT);
    mbotw_mouseSet (LMHINT_TOGGLE, MMHINT_DONE);
}

/*=====================================================================*/
/* ARGSUSED */
void pgwlst_prodBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwlst_prodBtnCb							*
 *									*
 * Callback function for "Generate Products" button.			*
 *									*
 * void pgwlst_prodBtnCb (wid, which, call)				*
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
 * H. Zeng/EAI          11/01   initial coding                          *
 * H. Zeng/EAI          12/01   modified to use pgwfmt_WCCselectCb      *
 * H. Zeng/EAI          12/01   modified to remove event handler        *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * H. Zeng/SAIC		11/05	called pgwfmt_popupWCC() after          *
 *				pgwfmt_setWCC()				*
 ***********************************************************************/
{
    int   index, location, iret;
    long  data;
    char  cfm_mesg[] = "Watch has no counties!\n"
                       "Set default counties?" ;
    Widget draw_w;
    VG_DBStruct	el, *el_pt;
/*---------------------------------------------------------------------*/
/*
 * Get currently selected Watch Element.
 */
    if ( pghdlb_elemSelected() < 1 ) {
	 el_pt = NULL;
    }
    else {
        pghdlb_getNextIndex(-1, &index, &location, &iret);
	cvg_rdrec (cvg_getworkfile(), location, &el, &iret);
        el_pt = &el;
    }

    switch(which) {

      case 0:	/* Generate WCC */
        if ( el_pt != NULL ) {
             if (el_pt->elem.wbx.info.numcnty == 0) {
	         draw_w = (Widget)mcanvw_getDrawingW();
                 data = 4;
	         NxmConfirm_show(draw_w, cfm_mesg, 
                         (XtCallbackProc)pgwlst_cmOptCb,
                         NULL, (XtPointer)data, &iret   ); 
	     }
             else {
                 pgwfmt_setWCC( el_pt );
	         pgwfmt_popupWCC();
             }
        }
        else {
             pgwfmt_setWCC( el_pt );
	     pgwfmt_popupWCC();
        }

	break;

      case 1:	/* Generate WCL */

	break;

      case 2:	/* Watch Format */

	break;

    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwlst_statesCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwlst_statesCb							*
 *									*
 * Callback function for state toggle buttons.			        *
 *									*
 * void pgwlst_statesCb (wid, which, event)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	call	XtPointer	event callback structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/01   initial coding                          *
 * H. Zeng/SAIC		07/05	added call to pgwlst_loadQC()		*
 * H. Zeng/SAIC		01/06	added call to pgwatch_loadStCwa()	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    XtPointer		userdata;
    struct incInfo	*incstruct;
    int                 np, el_location, loco, grp_typ, ier;
    float               *xpts, *ypts, llx, lly, urx, ury;
    VG_DBStruct         el;
/*---------------------------------------------------------------------*/

    XtVaGetValues (wid, XmNuserData, &userdata, NULL);
    incstruct = (struct incInfo *)userdata;
    incstruct[which].include = XmToggleButtonGetState (wid);
    XtUnmanageChild (incstruct[which].wid);

/*
 * Generate new watch element based on the new state info.
 */
    pgactv_getDevPts (&np, &xpts, &ypts);
    el_location = pgactv_getElmLoc();
    pgutls_prepNew (el_location, &el, &llx, &lly, &urx, &ury, &ier);

    pgundo_newStep();
    pgundo_storeThisLoc(el_location, UNDO_DEL, &ier);

    grp_typ = 0;
    if( incstruct[which].include == FALSE ) {   
        pgwatch_rmvGrpCnty (grp_typ, incstruct[which].name, &el);
    }
    else {

/*
 * No codes here since state buttons have no chance to be
 * turned on.
 */
    }

    pgvgf_saveNewElm (NULL, sys_D, &el, np, xpts, ypts, TRUE, &loco, &ier);
    pgundo_storeThisLoc(loco, UNDO_ADD, &ier);
    pgundo_endStep();

    pgutls_redraw (loco, &el, &ier);
    pgwlst_loadCnty();
    pgwlst_loadQC();
    pgwlst_loadWFO();
    pgwatch_loadStCwa();
}

/*=====================================================================*/

IncInfo *pgwlst_getStateBtns ( void )
/************************************************************************
 * pgwlst_getStateBtns                                                  *
 *                                                                      *
 * This function returns a structure for state button widgets.          *
 *                                                                      *
 * IncInfo* pgwlst_getStateBtns( )					*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 * pgwlst_getStateBtns 	IncInfo* Structure for state button widgets	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/01   initial coding                          *
 ***********************************************************************/
{
    return(_statesInc);
}

/*=====================================================================*/
/* ARGSUSED */
void pgwlst_cwasCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwlst_cwasCb							*
 *									*
 * Callback function for CWA radio buttons.			        *
 *									*
 * void pgwlst_cwasCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	call	XtPointer	callback structure			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		01/06	initial coding				*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * H. Zeng/SAIC		01/07	added call to pgwatch_save()		*
 ***********************************************************************/
{
    int                 np, el_location, loco, grp_typ, ier;
    float               *xpts, *ypts, llx, lly, urx, ury;
    VG_DBStruct         el;
/*---------------------------------------------------------------------*/
/*
 * Do nothing for the following two cases.
 */
    if ( wid == _cwasInc[which].in_btn  && 
	 _cwasInc[which].state ==  1       )  return;
    if ( wid == _cwasInc[which].out_btn && 
	 _cwasInc[which].state == -1       )  return;

/*
 * Generate new watch element based on the new CWA info.
 */
    pgactv_getDevPts (&np, &xpts, &ypts);
    el_location = pgactv_getElmLoc();
    pgutls_prepNew (el_location, &el, &llx, &lly, &urx, &ury, &ier);

    pgundo_newStep();
    pgundo_storeThisLoc(el_location, UNDO_DEL, &ier);

    if ( wid == _cwasInc[which].in_btn ) {

/*
 * include all the counties belong to this WFO into current 
 * watch county list.
 */
      grp_typ = 1;
      pgwatch_addGrpCnty ( grp_typ, _cwasInc[which].name, &el);
    }
    else if ( wid == _cwasInc[which].out_btn ) {

/*
 * exclude all the counties belong to this WFO out of  current 
 * watch county list.
 */
      grp_typ = 1;
      pgwatch_rmvGrpCnty ( grp_typ, _cwasInc[which].name, &el);
    }

    pgvgf_saveNewElm (NULL, sys_D, &el, np, xpts, ypts, TRUE, &loco, &ier);
    pgundo_storeThisLoc(loco, UNDO_ADD, &ier);
    pgundo_endStep();

    pgutls_redraw (loco, &el, &ier);
    pgwlst_loadCnty();
    pgwlst_loadQC();
    pgwlst_loadWFO();
    pgwatch_loadStCwa();
    pgwatch_save ( &el );
}

/*=====================================================================*/

CwaIncInfo *pgwlst_getCwaBtns ( void )
/************************************************************************
 * pgwlst_getCwaBtns                                                    *
 *                                                                      *
 * This function returns a structure for cwa button widgets.            *
 *                                                                      *
 * CwaIncInfo* pgwlst_getCwaBtns( )					*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 * pgwlst_getCwaBtns 	CwaIncInfo* Structure for cwa button widgets	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/06   initial coding                          *
 ***********************************************************************/
{
    return(_cwasInc);
}

/*=====================================================================*/

Widget pgwlst_getStatesForm ( void )
/************************************************************************
 * pgwlst_getStatesForm                                                 *
 *                                                                      *
 * This function returns the widget id for _statesForm.		        *
 *                                                                      *
 * Widget  pgwlst_getStatesForm( )					*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 *			 NONE						*
 * Return parameters:							*
 * pgwlst_getStatesForm	 Widget   widget id for _statesForm		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC          06/05   initial coding                         *
 ***********************************************************************/
{
    return(_statesForm);
}

/*=====================================================================*/

Widget pgwlst_getInoutRC ( void )
/************************************************************************
 * pgwlst_getInoutRC                                                    *
 *                                                                      *
 * This function returns the widget id for _inoutRC.		        *
 *                                                                      *
 * Widget pgwlst_getInoutRC ( )						*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 *			 NONE						*
 * Return parameters:							*
 * pgwlst_getInoutRC	 Widget   widget id for _inoutRC		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC          01/06   initial coding                          *
 ***********************************************************************/
{
    return(_inoutRC);
}

/*=====================================================================*/
/* ARGSUSED */
void pgwlst_toggleCntyEh ( Widget wid, XtPointer clnt, 
					XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgwlst_toggleCntyEh							*
 *									*
 * Callback function for county management buttons.			*
 *									*
 * void pgwlst_toggleCntyEh (wid, clnt, event, ctdr)			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	not used				*
 *	*event	XEvent		button event				*
 *									*
 * Output parameters:							*
 *	*ctdr	Boolean		continue to dispatch return flag	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		02/00	initial coding				*
 * S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 * E. Safford/GSC	04/00	add offset to xx, yy locations		*
 * H. Zeng/EAI      	04/00   changed cursor name                     *
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * E. Safford/GSC	11/00	add undo to county changes    		*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * D.W.Plummer/NCEP	12/01	chgs for seq chg to pgwatch_editcnty	*
 * D.W.Plummer/NCEP	10/02	add call to pgwatch_save		*
 * H. Zeng/XTRIA	06/03   changed "Toggle Anchor Pts" sensitivity *
 * A. Hardy/NCEP	 3/04   changed CNTY_BNDS to BNDS_FILE		*
 * H. Zeng/SAIC		07/05	added call to pgwlst_loadQC()		*
 * H. Zeng/SAIC		01/06	added call to pgwatch_loadStCwa()	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		nloc = 1, np, ier, loco, xoff, yoff, el_location;
    float	xx, yy, lat, lon, llx, lly, urx, ury, *xpts, *ypts;
    char	strout[128], info[256], data[12];
    int		fips, ihot=0;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    if (event->xbutton.button == Button1) {

	xgtoff (&xoff, &yoff, &ier);
	xx = (float) (event->xbutton.x + xoff);
	yy = (float) (event->xbutton.y + yoff);

	gtrans (sys_D, sys_M, &nloc, &xx, &yy, &lat, &lon, 
		&ier, strlen(sys_D), strlen(sys_M) );

	clo_tqbnd ( BNDS_FILE, lat, lon, strout, &ier );

	if ( strcmp ( strout, "-" ) != 0 )  {

            clo_bginfo ( BNDS_FILE, ihot, info, &ier );
            cst_gtag ( "FIPS", info, "99999", data, &ier );
            cst_numb ( data, &fips, &ier );

	pgactv_getDevPts (&np, &xpts, &ypts);
	el_location = pgactv_getElmLoc();
	pgutls_prepNew (el_location, &el, &llx, &lly, &urx, &ury, &ier);

	pgundo_newStep();
	pgundo_storeThisLoc(el_location, UNDO_DEL, &ier);

	    pgwatch_editcnty (3, &el, nloc, &fips);

	pgvgf_saveNewElm (NULL, sys_D, &el, np, xpts, ypts, TRUE, &loco, &ier);
	pgundo_storeThisLoc(loco, UNDO_ADD, &ier);
        pgundo_endStep();
	pgutls_redraw (loco, &el, &ier);

	pgwlst_loadCnty ();
        pgwlst_loadQC ();
	pgwlst_loadWFO ();
        pgwatch_loadStCwa ();

	pgwatch_save ( &el );

	}
    }
    else {
	XtSetSensitive (_ctlForm,   TRUE);
        XtSetSensitive (_ancPtsBtn, TRUE);

	mcanvw_disarmDynamic();
	mcanvw_setPressFunc ((XtEventHandler)&pgevt_selectHdl, CURS_DEFAULT); 
	mbotw_mouseSet (LMHINT_MOVEPOINT, MMHINT_DONE);
    }

    *ctdr = TRUE;
}

/*=====================================================================*/
/* ARGSUSED */
void pgwlst_tglAncPtsEh ( Widget wid, XtPointer clnt, 
					XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgwlst_tglAncPtsEh							*
 *									*
 * Callback function for "Toggle Anchor Pts" button.			*
 *									*
 * void pgwlst_tglAncPtsEh (wid, clnt, event, ctdr)			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	not used				*
 *	*event	XEvent		button event				*
 *									*
 * Output parameters:							*
 *	*ctdr	Boolean		continue to dispatch return flag	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          06/01   initial coding                          *
 * H. Zeng/XTRIA	06/03   changed _ctlForm sensitivity		*
 * H. Zeng/SAIC		01/06	added call to pgwatch_clrNumCwas()	*
 * H. Zentg/SAIC	02/06	added manage & unmanage of _ancPtsLbl   *
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		nloc = 1, xoff, yoff, el_location;
    int         nid, ii, jj, prev, shape, np, loco, iret;
    char        id[5], ina_ancpts[25], ancpts_lbl[40];
    float	xx, yy, lat, lon, llx, lly, urx, ury;
    float       *plat, *plon;
    Boolean     done;
    XmString    xmstr;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    if (event->xbutton.button == Button1) {

/*
 * Obtain the nearest anchor point
 */
	xgtoff (&xoff, &yoff, &iret);
	xx = (float) (event->xbutton.x + xoff);
	yy = (float) (event->xbutton.y + yoff);

	gtrans (sys_D, sys_M, &nloc, &xx, &yy, &lat, &lon, 
		&iret, strlen(sys_D), strlen(sys_M) );
	clo_tclosest ( "ANCHOR", lat, lon, 1, &iret );
	clo_tgid ( "ANCHOR", 1, sizeof(id), &nid, id, &iret );
	id[3] = '\0';

/*
 * Check the above anchor point against current inactive anchor
 * point list.
 */
        done = FALSE;
        ii = _ancLstIdx;
        for (jj = 0; jj < 5; jj++) {
	   if (strcmp(_inactiveAncLst[ii], id) == 0) {
               _inactiveAncLst[ii][0] = '\0';
               done = TRUE;
           }
           else if (done) {
	       prev = (ii-1) < 0 ? (ii+4) : (ii-1);
               strcpy (_inactiveAncLst[prev], _inactiveAncLst[ii]);
               _inactiveAncLst[ii][0] = '\0';
           }

           ii = (ii+1) >= 5 ? (ii-4) : (ii+1); 
        }

        if (done) {

/*
 * Anchor point already on the list, remove it and _ancLstIdx
 * take one step back.
 */
	   _ancLstIdx = (_ancLstIdx-1) < 0 ? 
                        (_ancLstIdx+4) : (_ancLstIdx-1);
        }
        else {

/*
 * Anchor point not on the list. add it and _ancLstIdx take
 * one step forward.
 */
	   strcpy ( _inactiveAncLst[_ancLstIdx], id );
           _ancLstIdx = (_ancLstIdx+1) >=  5 ?
	                (_ancLstIdx-4) : (_ancLstIdx+1);
        }

/*
 * Update the inactive anchor points label
 */ 
        pgwlst_getInaAncPts (ina_ancpts);
        sprintf(ancpts_lbl, "Inactive: %s\n", ina_ancpts);
        xmstr = XmStringCreateLocalized ( ancpts_lbl );
        XtUnmanageChild ( _ancPtsLbl );
        XtVaSetValues( _ancPtsLbl, XmNlabelString, xmstr, NULL );
        XtManageChild ( _ancPtsLbl );
        XmStringFree( xmstr ); 

    }
    else {
        pgwlst_getInaAncPts (ina_ancpts);

/*
 * Take all modifications into effect. Start to generate new 
 * watch element.
 */
	el_location = pgactv_getElmLoc();
	pgutls_prepNew (el_location, &el, &llx, &lly, &urx, &ury, &iret);

	pgundo_newStep();
	pgundo_storeThisLoc(el_location, UNDO_DEL, &iret);
   
/*
 * Adjust ("snap") the points after anchor pts info. change.
 */
        pgwpts_setSnap ( 1 ) ;
        shape = el.elem.wbx.info.w_shape;
        np    = el.elem.wbx.info.numpts;
	plat  = el.elem.wbx.latlon;
	plon  = &(el.elem.wbx.latlon[np]);
        pgwpts_get ( 0, shape, plat, plon, plat, plon, &iret );
        pgwpts_setSnap ( 0 ) ;

/*
 *  Save updated anchor info. into watch element.
 */
        pgwbxw_getAnchor ( 0, el.elem.wbx.info.w_a0id,
            &(el.elem.wbx.info.w_a0lt), &(el.elem.wbx.info.w_a0ln),
            &(el.elem.wbx.info.w_a0dis), el.elem.wbx.info.w_a0dir,
            &iret );

        pgwbxw_getAnchor ( 1, el.elem.wbx.info.w_a1id,
            &(el.elem.wbx.info.w_a1lt), &(el.elem.wbx.info.w_a1ln),
            &(el.elem.wbx.info.w_a1dis), el.elem.wbx.info.w_a1dir,
            &iret );

/*
 *  Wipe the county list
 */
	el.elem.wbx.info.numcnty = 0;


	pgvgf_saveNewElm (NULL, sys_M, &el, 0, NULL, NULL, TRUE, &loco, &iret);
	pgundo_storeThisLoc(loco, UNDO_ADD, &iret);
        pgundo_endStep();
	pgutls_redraw (loco, &el, &iret);
	pgwatch_clrNumCwas ();
        pgwbxw_setWlst (loco, TRUE);

/*
 * Disable current press event handler and make "Toggle Anchor
 * Pts" back sensitive.
 */
	mcanvw_disarmDynamic();
	mcanvw_setPressFunc ((XtEventHandler)&pgevt_selectHdl, CURS_DEFAULT);
	mbotw_mouseSet (LMHINT_MOVEPOINT, MMHINT_DONE);
	XtSetSensitive (_ancPtsBtn, TRUE);
        XtSetSensitive (_ctlForm,   TRUE);

    }

    *ctdr = TRUE;
}

/*=====================================================================*/

void pgwlst_loadInfo ( void ) 
/************************************************************************
 * pgwlst_loadInfo							*
 *									*
 * This function loads the county info into the display.		*
 *									*
 * void pgwlst_loadInfo ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		10/97						*
 * D.W.Plummer/NCEP	11/98	Changes for editing pgram watch edges	*
 * D.W.Plummer/NCEP	12/98	reordered listing of info and counties	*
 * E. Safford/GSC	12/98	modify to placate aix4 and clean up	*
 * S. Law/GSC		01/99	cleanup and added label switching	*
 * D.W.Plummer/NCEP	 4/99	rewrite for common watch attr access	*
 * D.W.Plummer/NCEP	 4/99	add check for _whichLabel		*
 * D.W.Plummer/NCEP	 6/99	add half-width in nm units		*
 * A. Hardy/GSC		 5/00   added a warning for missing Anchor pts. *
 * T. Lee/GSC		10/00	changed statute to nautical miles	*
 * A. Hardy/GSC         11/00   removed coordinate system declaration   *
 * J. Wu/GSC		 5/01	free XmStrings				*
 * J. Wu/SAIC		12/03	remove '\n' from "ORIENT" & "AREA" str	*
 * H. Zeng/SAIC		07/05	modified to use "SPECS"			*
 ***********************************************************************/
{
    int		ii, nitem, ier;
    int		ihafwsm, ihafwnm, iarea, npoly;
    char	str[128];
    char	labelstr[128];
    int		hwsm, hwnm;
    int		shape;
    char	shapes[][5]={"NS", "EW", "ESOL"};
    int         ipos;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    if (_showInfo) {

        XtVaGetValues(_pgwList, XmNitemCount, &nitem, NULL);

        if (nitem != 0) {
	    XmListDeleteAllItems(_pgwList);
        }

	if ( _whichLabel != SPECS )  {
	    sprintf (labelstr, SPECS_LABEL);
	    NxmLabel_setStr (_pgwLabel, labelstr);
            _whichLabel = SPECS;
	}

	if ( _styleWbx == WBC )  {

/*
 *  Output vertex information
 */
	    npoly = 6;
	    for ( ii = 0; ii < npoly; ii++ )  {

    	      pgwatch_gvert( ii, str );
	      xmstr = XmStringCreateLocalized(str);
	      XmListAddItemUnselected (_pgwList, xmstr, 0);
	      XmStringFree ( xmstr );
	    }
	}
	else if ( _styleWbx == PGRAM )  {

/*
 *  Endpoint information (ref watch corner points)
 */
    	    pgwatch_gvert( 0, str );
	    xmstr = XmStringCreateLocalized(str);
	    XmListAddItemUnselected (_pgwList, xmstr, 0);
	    XmStringFree ( xmstr );

/*
 *  Endpoint information (ref VOR points)
 */
    	    pgwatch_gvert( 4, str );
	    xmstr = XmStringCreateLocalized(str);
	    XmListAddItemUnselected (_pgwList, xmstr, 0);
	    XmStringFree ( xmstr );

/*
 *  Check for missing Anchor point in box, popup a warning.
 */
	    cst_srch ( 0, (int)strlen(str), "---", str, &ipos, &ier);

            if( (ipos >= 0 )  ) {
                 pgwlst_popupWarn ( );
            } 

/*
 *  Output orientation, half-width and area information
 */
	    pgwatch_gattr( &hwsm, &hwnm, &shape, &iarea );

	    ihafwsm = hwsm;
	    ihafwnm = hwnm;

	    sprintf(str, "ORIENT: %s - HALF WIDTH: %d sm (%d nm)",
			shapes[shape-1], ihafwsm, ihafwnm );

	    xmstr = XmStringCreateLocalized(str);
	    XmListAddItemUnselected (_pgwList, xmstr, 0);
	    XmStringFree ( xmstr );

	    sprintf(str, "AREA (sq nautical miles): %d", iarea );

	    xmstr = XmStringCreateLocalized(str);
	    XmListAddItemUnselected (_pgwList, xmstr, 0);
	    XmStringFree ( xmstr );
	}
    }
}

/*=====================================================================*/

void pgwlst_loadCnty ( void ) 
/************************************************************************
 * pgwlst_loadCnty							*
 *									*
 * This function loads the county list into the display.		*
 *									*
 * void pgwlst_loadCnty ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/00						*
 * A. Hardy/GSC         11/00   removed coordinate system declaration   *
 * D.W.Plummer/NCEP	 4/01	Added WFO list to end of county list	*
 * D.W.Plummer/NCEP	 5/01	Removed WFO list from end of county list*
 * J. Wu/GSC        	 5/01   free XmString				*
 * H. Zeng/SAIC		06/05	changed variable 'i' to 'ii'		*
 ***********************************************************************/
{
    int		ii, ncnty, nitem;
    char	str[128];
    char	labelstr[128];
    XmString	xmstr;

/*---------------------------------------------------------------------*/

    if ( _showCnty )  {

        XtVaGetValues(_pgwList, XmNitemCount, &nitem, NULL);

        if (nitem != 0) {
	    XmListDeleteAllItems(_pgwList);
        }

	pgwatch_gcnty( 0, &ncnty, str );

        if ( ncnty == 0 )  {
	    pgwlst_clear ();
	    return;
        }

	if ( _whichLabel != CNTY )  {
	    sprintf (labelstr, COUNTY_LABEL);
	    NxmLabel_setStr (_pgwLabel, labelstr);
            _whichLabel = CNTY;
	}

	for ( ii = 0; ii < ncnty; ii++ )  {

	    pgwatch_gcnty( ii, &ncnty, str );

	    xmstr = XmStringCreateLocalized(str);
	    XmListAddItemUnselected (_pgwList, xmstr, 0);
	    XmStringFree ( xmstr );
	    
	}
    }
}

/*=====================================================================*/

void pgwlst_loadQC ( void ) 
/************************************************************************
 * pgwlst_loadQC							*
 *									*
 * This function loads the "QC Counties" info into the display.		*
 *									*
 * void pgwlst_loadQC ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		05/05	initial coding				*
 ***********************************************************************/
{
    int		ii, nitem, n_inactv, n_actv;
    char	labelstr[128], str[128];
    char	**inactv_cnty=NULL, **actv_cnty=NULL;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    if (_showQC) {

        XtVaGetValues(_pgwList, XmNitemCount, &nitem, NULL);

        if (nitem != 0) {
	    XmListDeleteAllItems(_pgwList);
        }

	if ( _whichLabel != CNTY )  {
	    sprintf (labelstr, COUNTY_LABEL);
	    NxmLabel_setStr (_pgwLabel, labelstr);
            _whichLabel = CNTY;
	}

/*
 * Call pgwatch_gQCcnty() to get all info.
 */
	pgwatch_gQCcnty ( &n_inactv, &inactv_cnty, &n_actv, &actv_cnty );

/*
 *  List all inactive counties.
 */
	sprintf(str, 
	   "======== Inactive counties  INSIDE the watch area ========");
	xmstr = XmStringCreateLocalized(str);
	XmListAddItemUnselected (_pgwList, xmstr, 0);
	XmStringFree ( xmstr );

        if ( n_inactv == 0 ) {

	  sprintf(str, "NONE...NONE...NONE" );
	  xmstr = XmStringCreateLocalized(str);
	  XmListAddItemUnselected (_pgwList, xmstr, 0);
	  XmStringFree ( xmstr );
        }
        else {

	  for ( ii = 0; ii < n_inactv; ii++ )  {

	    xmstr = XmStringCreateLocalized(inactv_cnty[ii]);
	    XmListAddItemUnselected (_pgwList, xmstr, 0);
	    XmStringFree ( xmstr );
	  }
        }

	sprintf(str, " " );
	xmstr = XmStringCreateLocalized(str);
	XmListAddItemUnselected (_pgwList, xmstr, 0);
	XmStringFree ( xmstr );

/*
 *  List all active counties.
 */
	sprintf(str, 
	   "========   Active counties OUTSIDE the watch area ========");
	xmstr = XmStringCreateLocalized(str);
	XmListAddItemUnselected (_pgwList, xmstr, 0);
	XmStringFree ( xmstr );

        if ( n_actv == 0 ) {

	  sprintf(str, "NONE...NONE...NONE" );
	  xmstr = XmStringCreateLocalized(str);
	  XmListAddItemUnselected (_pgwList, xmstr, 0);
	  XmStringFree ( xmstr );
        }
        else {

	  for ( ii = 0; ii < n_actv; ii++ )  {

	    xmstr = XmStringCreateLocalized(actv_cnty[ii]);
	    XmListAddItemUnselected (_pgwList, xmstr, 0);
	    XmStringFree ( xmstr );
	  }
        }

/*
 * Free actv_cnty and inactv_cnty memory blocks.
 */
        for ( ii = 0; ii < n_actv; ii++ )  free( actv_cnty[ii] );
        free( actv_cnty );   

        for ( ii = 0; ii < n_inactv; ii++ )  free( inactv_cnty[ii] );
        free( inactv_cnty );        

    } /* the end of if (_showQC)... */
}

/*=====================================================================*/

void pgwlst_updAncLst ( char *sysin, int np, float *x, float *y, 
			int iv, char *disdir, char *stn, int *iret )
/************************************************************************
 * pgwlst_updAncLst                                                    	*
 *                                                                      *
 * This function updates the anchor list if the list window is up. 	*
 *                                                                      *
 * void pgwlst_updAncLst( sysin, np, x, y, iv, disdir, stn, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*sysin	char		Input coordinate system			*
 *	np	int		Number of points			*
 *	*x	float		Array of x coordinates			*
 *	*y	float		Array of y coordinates			*
 *	iv	int		Pointer to array element to process	*
 *									*
 * Output parameters:                                                   *
 *	*disdir	char		Returned distance and direction string	*
 *	*stn	char		Returned station string			*
 *	*iret	int		Return value				*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	11/97	Create					*
 * S. Law/GSC		11/98	renamed to make clearer			*
 * D.W.Plummer/NCEP	12/98	Changed calling sequence of clo_closest	*
 * 				and how clo_tgid is called		*
 * D.W.Plummer/NCEP	 1/99	Changed clo_ancinpoly to clo_tinpoly	*
 * D.W.Plummer/NCEP	 1/99	Bug fixes - core dump when npt == 0	*
 * 				   - send closed poly to clo_tinpoly	*
 * D.W.Plummer/NCEP	 4/99	Changed zero value of cmpdir to "-"	*
 * D.W.Plummer/NCEP	 5/99	Replaced distance and compass code	*
 * M. Li/GSC		10/99	Modified clo_direct, clo_dist, and 	*
 *				clo_compass codes			*
 * M. Li/GSC		10/99	Added multi-point cal. to clo_dist	*
 * A. Hardy/GSC         01/00   Changed calls for clo_tinpoly, clo_tgid *
 *                              clo_tgltln                              *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * D.W.Plummer/NCEP	 6/05	Apply LLSC rounding prior to clo_tinpoly*
 ***********************************************************************/
{
int	i, numpts, npt, idist, index, nclose, ier, icmp, npx;
float	plat, plon, vlat, vlon, dir, dist;
float	alat[50], alon[50], xx[10], yy[10];
char	cmpdir[4];
char	id[20], ids[450];
#define LLSC(xxxx)      ( (float)G_NINT( (xxxx) * 100.0F ) / 100.0F )
/*---------------------------------------------------------------------*/

	*iret = 0;

	npt = 1;
	gtrans( sysin, sys_M, &npt, &(x[iv]), &(y[iv]), 
		&plat, &plon, &ier, strlen(sysin), strlen(sys_M) );

/*
 *  Only consider anchor points that are INSIDE watch
 */
	for ( i = 0; i < np; i++ )  {
	    xx[i] = LLSC ( x[i] );
	    yy[i] = LLSC ( y[i] );
	}
        numpts = np;
	if ( !G_DIFF(x[i], x[np-1]) || !G_DIFF(y[i], y[np-1]) )  {
	    xx[np] = xx[0];
	    yy[np] = yy[0];
            numpts = np + 1;
	}
        clo_tinpoly( "ANCHOR", sys_M, numpts, xx, yy, &ier );
	clo_tgltln( "ANCHOR", sizeof(alat)/sizeof(float), 
	        &npt, alat, alon, &ier );
	clo_tgid( "ANCHOR", sizeof(alat)/sizeof(float), sizeof(ids), 
		&npt, ids, &ier );

	if ( npt == 0 )  {
	    strcpy( disdir, "--- ---" );
	    strcpy( stn, "---" );
	}
	else {
	    nclose = 1;
            clo_closest( alat, alon, npt, plat, plon, nclose, &index, &ier );
	    strcpy( id, strtok( ids, ";" ) );
	    i = 0;
	    while ( i < index )  {
	        strcpy( id, strtok( NULL, ";" ) );
		i++;
	    }

	    vlat = alat[index];	
	    vlon = alon[index];
	    npx = 1;
	    clo_dist ( &plat, &plon, &npx, &vlat, &vlon, &dist, &ier );

/*
 *  Distance must be in statute miles, rounded to nearest 5
 */
	    idist = G_NINT( ( dist * M2SM ) / 5.0F ) * 5;

	    if ( idist == 0 )  {
	        strcpy( cmpdir, "-" );
	    }
	    else  {

	        clo_direct ( &plat, &plon, &vlat, &vlon, &dir, &ier );

	        clo_compass ( &dir, cmpdir, &icmp, &ier );

	    }

	    sprintf( disdir, "%-3d %-3s", idist, cmpdir );
	    sprintf( stn, "%s", id );
	}
}

/*=====================================================================*/

void pgwlst_updVorLst ( char *sysin, int np, float *x, float *y, 
			int iv, char *disdir, char *stn, int *iret )
/************************************************************************
 * pgwlst_updVorLst                                                    	*
 *                                                                      *
 * This function updates the vor list if the list window is up. 	*
 *                                                                      *
 * void pgwlst_updVorLst( sysin, np, x, y, iv, disdir, stn, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*sysin	char							*
 *	np	int	number of points				*
 *	*x	float	x coordinate					*
 *	*y	float	y coordinate					*
 *	iv	int	Vertice number to check				*
 *									*
 * Output parameters:                                                   *
 *	*disdir	char		Returned distance and direction string	*
 *	*stn	char		Returned station string			*
 *	*iret	int		Return value				*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	11/97	Create					*
 * S. Law/GSC		11/98	renamed to fit convention		*
 * D.W.Plummer/NCEP	12/98	rename clo_tclsst to clo_tclosest	*
 * 				and changed length of char array id	*
 * D.W.Plummer/NCEP	 4/99	Changed zero value of cmpdir to "-"	*
 * D.W.Plummer/NCEP	 5/99	Replaced distance and compass code	*
 * M. Li/GSC		10/99	Modified clo_direct, clo_dist, and 	*
 *				clo_compass codes.			*
 * M. Li/GSC		10/99	Added multi-point cal. to clo_dist	*
 * A. Hardy/GSC         01/00   Changed call for clo_tclosets, clo_tgltn*
 *                              clo_tgid                                *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 ***********************************************************************/
{
int	npt, idist, nclose, ier, icmp, npx;
float	plat, plon, vlat, vlon, dir, dist;
char	cmpdir[4];
char	id[20], idx[20];
/*---------------------------------------------------------------------*/

	*iret = 0;

	npt = 1;
	gtrans( sysin, sys_M, &npt, &(x[iv]), &(y[iv]), 
		&plat, &plon, &ier, strlen(sysin), strlen(sys_M) );

/*
 *  Consider all VOR points
 */
        nclose = 1;
        clo_tclosest( "VOR", plat, plon, nclose, &ier);
	clo_tgltln( "VOR", 1, &npt, &vlat, &vlon, &ier );
	npx =1;
	clo_dist ( &plat, &plon, &npx, &vlat, &vlon, &dist, &ier );

/*
 *  Distance must be in nautical miles
 */
	idist = G_NINT( dist * M2NM );

	if ( idist == 0 )  {
	    strcpy( cmpdir, "-" );
	}
	else  {

	    clo_direct ( &plat, &plon, &vlat, &vlon, &dir, &ier );

	    clo_compass ( &dir, cmpdir, &icmp, &ier );
	}

	clo_tgid( "VOR", 1, sizeof(idx), &npt, idx, &ier );
        strcpy( id, strtok( idx, ";" ) );

	sprintf( disdir, "%-3d %-3s", idist, cmpdir );
	sprintf( stn, "%s", id );
}

/*=====================================================================*/

void pgwlst_createWarnBox ( Widget wdgt )
/************************************************************************
 * pgwlst_createWarnBox							*
 *									*
 * This function creates the warning dialog box.			*
 *									*
 * void pgwlst_createWarnBox( wdgt )					*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		ID of widget 				*
 *									*
 * Return parameters:							*
 *	    NONE							*
 *									*
 **									*
 * Log: 								*
 * A. Hardy/GSC		 5/00	Modified from NxmWarn.c			*
 * T. Piper/SAIC	06/06	Fix "too many files in HOME directory"	*
 *				by setting XmNsymbolPixmap to NULL	*
 ***********************************************************************/
{
    Arg		args[5];
    Cardinal	argcnt;
    char        mesg[128];
    XmString	title, xmwarning;
    Widget	child;
/*---------------------------------------------------------------------*/

    title = XmStringCreateLocalized("Watch Creation Warning");
    sprintf ( mesg, "Anchor point not found in watch box. \n "); 
    strcat ( mesg, "Resize watch to include an Anchor point."); 
    xmwarning = XmStringCreateLtoR(mesg, XmFONTLIST_DEFAULT_TAG);

    argcnt = 0;
    XtSetArg(args[argcnt], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); argcnt++;
    XtSetArg(args[argcnt], XmNdialogTitle, title); argcnt++;
    XtSetArg(args[argcnt], XmNmessageAlignment, XmALIGNMENT_CENTER); argcnt++;
    XtSetArg(args[argcnt], XmNmessageString, xmwarning); argcnt++;
    XtSetArg(args[argcnt], XmNsymbolPixmap, NULL); argcnt++;

    _warning = XmCreateWarningDialog( wdgt, "Warning", args, argcnt);

    XtAddCallback(_warning, XmNokCallback, pgwlst_Warn_cb, NULL);

    XmStringFree(title);
    XmStringFree(xmwarning);
    child = XmMessageBoxGetChild(_warning, XmDIALOG_HELP_BUTTON);
    XtUnmanageChild(child);

    child = XmMessageBoxGetChild(_warning, XmDIALOG_CANCEL_BUTTON);
    XtUnmanageChild(child);

}

/*=====================================================================*/

void pgwlst_popupWarn ( void )
/************************************************************************
 * pgwlst_popupWarn							*
 *									*
 * This function pops up the warning dialog box.			*
 *									*
 * void pgwlst_popupWarn ( )	 					*
 *									*
 * Input parameters:							*
 *									*
 * Return parameters:							*
 *	    NONE							*
 *									*
 **									*
 * Log: 								*
 * A. Hardy/GSC		 5/00	Created					*
 ***********************************************************************/
{
    if ( !XtIsManaged (_warning) ) {
	XtManageChild(_warning);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwlst_Warn_cb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgwlst_Warn_cb							*
 *									*
 * This function destroys the warning dialog.				*
 *									*
 * void pgwlst_Warn_cb( w, clnt, call )	 				*
 *									*
 * Input parameters:							*
 *	w	Widget		ID of widget to be destroyed		*
 *	clnt	XtPointer	Data from NxmWarn_show			*
 *	call	XtPointer	Data from the widget			*
 *									*
 * Return parameters:							*
 *	    NONE							*
 *									*
 **									*
 * Log: 								*
 * A. Hardy/GSC		 5/00	Created					*
 ***********************************************************************/
{
    XtUnmanageChild(w);
}

/*=====================================================================*/

void pgwlst_setBtnSen ( Boolean sensitive )
/************************************************************************
 * pgwlst_setBtnSen							*
 *									*
 * This function sets sensitive/insensitive the push buttons on the     *
 * bottom of the county list popup window.                              *
 *									*
 * void pgwlst_setBtnSen(sensitive)					*
 *									*
 * Input parameters:							*
 *        sensitive     Boolean     set sensitive or insensitive        *
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          08/00       initial coding                      *
 ***********************************************************************/
{
    int	  nbutton, ii;
/*---------------------------------------------------------------------*/

    if( XtIsManaged(_pgwlstWin) ) {
	nbutton = XtNumber (_ctlBtnStr);
	for ( ii = 0; ii < nbutton; ii++ )  {
	    XtSetSensitive (_ctlBtns[ii], (int)sensitive); 
	} 
    }
}

/*=====================================================================*/

void pgwlst_setPanesSen ( Boolean sensitive )
/************************************************************************
 * pgwlst_setPanesSen							*
 *									*
 * This function sets sensitive/insensitive the three panes on the      *
 * bottom of the county list popup window.                              *
 *									*
 * void pgwlst_setPanesSen(sensitive)					*
 *									*
 * Input parameters:							*
 *        sensitive     Boolean     set sensitive or insensitive        *
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          12/01       initial coding                      *
 ***********************************************************************/
{
    XtSetSensitive (_ctlForm,    (int)sensitive);
    XtSetSensitive (_statesForm, (int)sensitive);
    XtSetSensitive (_ancptsForm, (int)sensitive); 
}

/*=====================================================================*/

int pgwlst_getClstStatus ( void )
/************************************************************************
 * pgwlst_getClstStatus						        *
 *									*
 * This function returns a int value specifying the current clustering  *
 * status.                                                              *
 *									*
 * int pgwlst_getClstStatus( )					        *
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *                              NONE                                    *
 * Return value:                                                        *
 * pgwlst_getClstStatus	        int	  current clustering status     *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	      02/01     initial coding				*
 ***********************************************************************/
{
   return( _clstStatus );
}

/*=====================================================================*/

Boolean pgwlst_getCtlkStatus ( void )
/************************************************************************
 * pgwlst_getCtlkStatus						        *
 *									*
 * This function returns a Boolean value specifying the current county  *
 * lock status.								*
 *									*
 * int pgwlst_getCtlkStatus( )					        *
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *                              NONE                                    *
 * Return value:                                                        *
 * pgwlst_getCtlkStatus	        Boolean	  current county lock status	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	      06/013    initial coding				*
 ***********************************************************************/
{
   return( (Boolean)((_ctlkStatus == 1) ? TRUE : FALSE) );
}

/*=====================================================================*/

void pgwlst_getInaAncPts ( char *ina_ancpts )
/************************************************************************
 * pgwlst_getInaAncPts			                                *
 *									*
 * This function returns a string specifying the current inactive       *
 * anchor point list.                                                   *
 *									*
 * void pgwlst_getInaAncPts(ina_ancpts)					*
 *									*
 * Input parameters:							*
 *                              NONE                                    *
 * Output parameters:							*
 * ina_ancpts                   char*     inactive anchor pts list      *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	      06/01     initial coding				*
 ***********************************************************************/
{
    int		ii, jj;
/*---------------------------------------------------------------------*/

    ina_ancpts[0] = '\0';
 
/*
 *  Gather all anchor pts from inactive anchor pts list.
 */
    ii = _ancLstIdx;
    for (jj = 0; jj < 5; jj++)  {
	if (_inactiveAncLst[ii][0] != '\0')  {
	    strcat (ina_ancpts, _inactiveAncLst[ii]);
            strcat (ina_ancpts, " ");
	}
        ii = (ii+1) >=5 ? (ii-4) : (ii+1);
    }
}

/*=====================================================================*/

void pgwlst_loadWFO ( void ) 
/************************************************************************
 * pgwlst_loadWFO							*
 *									*
 * This function loads the WFO list into it's label area.		*
 *									*
 * void pgwlst_loadWFO ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 5/01						*
 * D.W.Plummer/NCEP	12/01	Save WFO list into global vrbl		*
 * E. Safford/SAIC	06/02	use MAX_WFO_LEN				*
 * T. Piper/SAIC	12/03	Cleaned up error handling for WFO list	*
 * J. Wu/SAIC           12/03   split long list of wfos into 2 lines    *
 ***********************************************************************/
{
    int		ii, ier, err_code, ncnty, len, split_point = 54;
    char	str[128], wfo[16], wfolist[MAX_WFO_LEN + 22];
    XmString    xmstr;

/*---------------------------------------------------------------------*/

    pgwatch_gcnty( 0, &ncnty, str );

    wfolist[0] = '\0';
    _wfoList[0] = '\0';
    
    if ( ncnty != 0 )  {

        for ( ii = 0 ; ii < ncnty ; ii++ )  {

	    pgwatch_gcnty( ii, &ncnty, str );

	    sscanf ( str, "%*s %*s %*s %*s %*s %*s %s", wfo );

/*
 *  Add WFO to WFO list, if WFO list string is long enough.
 */
	    if ( strstr ( _wfoList, wfo ) == (char *)NULL ) {
	        if ( ( strlen(_wfoList) + strlen(wfo) + 3 + 1 ) <= 
	               sizeof(_wfoList)/sizeof(_wfoList[0]) )  {
	            strcat ( _wfoList, "..." );
	            strcat ( _wfoList, wfo );
                }
                else if ( strstr ( wfolist, wfo ) == (char *)NULL ) {
		    strcat ( wfolist, wfo );
 		    strcat ( wfolist, "." );
                    err_code = 6;
		    cst_lcuc ( wfo, wfo, &ier );
                    er_wmsg ( "pgen", &err_code, wfo, &ier,
                              strlen("pgen"), strlen(wfo) );
                    NxmErr_update();
		}
            }
        }
    }
    else  {
	strcpy ( _wfoList, "...NONE" );
    }

/*
 *  Load string "wfolist" to WFO label area. If it is too long,
 *  split it into 2 lines and adjust the alignment.
 */
    strcpy ( wfolist, "WFOs: " );
    strcat ( wfolist, _wfoList );
    len = strlen( wfolist );
    if ( len > split_point ) {
        wfolist[split_point] = '\n';		
        wfolist[split_point+1] = '\0';		
        strcat ( wfolist, "             " );
        strcat ( wfolist, &_wfoList[split_point - 6] );
    }
     
    xmstr = XmStringCreateLtoR ( wfolist, XmFONTLIST_DEFAULT_TAG );
    XtVaSetValues ( _wfoLabel,
    		XmNalignment, XmALIGNMENT_BEGINNING,
    		XmNlabelString, xmstr, 
		NULL );
    XmStringFree ( xmstr );

}

/*=====================================================================*/

void pgwlst_getWFO ( char *wfolist ) 
/************************************************************************
 * pgwlst_getWFO							*
 *									*
 * This function gets the WFO list.					*
 *									*
 * void pgwlst_getWFO ( wfolist )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * wfolist              char*     WFO list				*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	12/01						*
 ***********************************************************************/
{
    strcpy ( wfolist, _wfoList );
}
