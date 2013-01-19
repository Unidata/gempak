#include "geminc.h"
#include "gemprm.h"
#include "proto_vf.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"

#define MAX_NEARBY_WFO	25
#define WFO_COL		4	/* Number of button columns for nearby WFOs  */
#define NL             "\n"     /* end-of-line for intermediate text product */
#define EOL            "\n"     /* end-of-line for final text product        */

static struct incInfo   _nearbyWfos[MAX_NEARBY_WFO];

#define	WATCHINFO_TBL	"nmap_pgw.tbl"
#define FORECASTERS_TBL "forecasters.tbl"
#define PHONE_TBL       "WCCphone.tbl"

#define TYPE_INT	0
#define TYPE_FLOAT	1
#define TYPE_CHAR	2

#define	NINFO	 8	/* Number of different elements listed here	*/
#define	HAIL	 0	/* Hail elements				*/
#define	GUST	 1	/* Maximum wind gust elements			*/
#define	TOPS	 2	/* Maximum tops elements			*/
#define	SMVD	 3	/* Storm Motion vector (dir) elements		*/
#define	SMVS	 4	/* Storm Motion vector (spd) elements		*/
#define FCST	 5	/* Forecaster's name				*/
#define AREA	 6	/* Adjacent areas				*/
#define EXPT     7      /* Expiration Time                              */
#define WNUM	98	/* Watch number(done separately)		*/
#define RWNM	99	/* Replacement watch (done separately)		*/
#define CNTN	99	/* Continuing watches (done separately)		*/

#define	MXELE	50	/* Max elements per info type			*/

#define LSTR	80	/* Size of a long string */
#define SSTR	 8	/* Size of a short string */

#define MAX_ISSUE	    2
#define MAX_SVR		    2
#define WTYP_CHOICE         2
#define TZONE_CHOICE        8
#define TEXT_FIELD         -1
#define EXPIRATION_TIME    -2
#define ISSUE_DEFAULT       1
#define WTYP_DEFAULT       -1           /* All buttons are off          */
#define SVRTY_DEFAULT       0
#define TZONE_DEFAULT      -1           /* All buttons are off          */

#define MAX_CNTN_LEN	(126)		/* continuing watch list len    */

#define	MAX_WFO_NUM	(500)		/* max WFO numbers		*/



typedef struct watchinfo_t {
    char	range[MXELE][MXCHR];	/* Element range		*/
    int		nelems;			/* Number of elements in range	*/
    char	defelem[MXCHR];		/* Default Element		*/
    int		min, max;		/* Minimum and maximum values	*/
    int		format;			/* Format type			*/
} WatchInfo_t;

static WatchInfo_t	_wfInfo[NINFO], _phoneInfo, _passcodeInfo;

struct dtgW {
    Widget	ymd;
    Widget	hhmm;
};

static struct dtgW 	_eTime;	        /* exp. time for format window   */
static struct dtgW      _wWCCTimes[2];  /* expiration time for WCC popup */

struct  listInfo
{
        char    cnty[32];
        char    st[4];
        char    info[LSTR];
};

static struct incInfo	_adjAreaInc[MXELE];
static int		_nAdjArea;

static char		*_mznchck[] = { "AM", "AN", "GM", "PH", "PK",
                                        "PM", "PS", "PZ" }; 

#define NMZNCH  	8 /* number of marine zones to check */

static Widget		_pgwfmtWin, _pgwfmtWCCWin, _WCLWin;
static Widget		_formatForm, _formatWCCForm;
static Widget           _svfileWCCForm, _newWCCText, _svfileWCCText;
static Widget		_WCLTextW;
static Widget		_saveBtn;
static Widget           _applyBtn;
static WidgetList	_issueW, _svrtyW, _wtypW, _wtypWCCW, _tzoneW;

static Widget		_wnumTxtW, _hailTxtW, _gustTxtW, _fcstTxtW;
static Widget		_topsTxtW, _smvdTxtW, _smvsTxtW, _rwnmTxtW;
static Widget		_cntnTxtW, _fcstWCCTxtW;
static Widget		_statesLblW, _phoneOptW, _preIdOptW;
static Widget           _phonePbW[MXELE], _preIdPbW[10];
static Widget		_replTxtW;

static Widget		_propWfoLb, _repWfoLb, _nearbyWfoRC, _wfoForm;

static int		_issueOpt = ISSUE_DEFAULT;
static int              _wtypOpt  =  WTYP_DEFAULT;
static int              _wtypWCCOpt= WTYP_DEFAULT;
static int		_svrtyOpt = SVRTY_DEFAULT;
static int              _tzoneOpt = TZONE_DEFAULT;

static char		blank[2]={' '};
static char		_fName[12];
static char             _curPhoneNo[MXCHR];
static char		_curPasscode[MXCHR];
static char		_curPreId[MXCHR];
static char		_WCLfilnam[FILE_FULLSZ];
static int		_validWnum = -1;
static int		_fcstOpt = -1;

static char 		_statlist[LSTR];
static char 		_adjlist[LSTR];
static int              _curTimeArry[5];

static int		_nNearbyWfo;

static	char	*_watchtypes[]={"SEVERE THUNDERSTORM", "TORNADO"};

static VG_DBStruct	_elWatch;
/*
 *  private callback functions
 */
void pgwfmt_ctlBtnCb (    Widget, long, XtPointer );
void pgwfmt_phonePbCb(    Widget, long, XmAnyCallbackStruct*);
void pgwfmt_preIdPbCb(    Widget, long, XmAnyCallbackStruct*);
void pgwfmt_WCCctlBtnCb(  Widget, long, XtPointer );
void pgwfmt_WCCfilePbCb(  Widget, long, XtPointer );
void pgwfmt_includesCb (  Widget, long, XEvent* );
void pgwfmt_menuTextCb (  Widget, long, XtPointer );
void pgwfmt_ymdTextCb  (  Widget, long, XtPointer );
void pgwfmt_WCCymdTextCb( Widget, long, XtPointer );
void pgwfmt_issueCb	( Widget, long, XtPointer );
void pgwfmt_wtypCb	( Widget, long, XtPointer );
void pgwfmt_svrtyCb	( Widget, long, XtPointer );
void pgwfmt_tzoneCb	( Widget, long, XtPointer );
void pgwfmt_wtypWCCCb	( Widget, long, XtPointer );
void pgwfmt_cfmCb	( Widget, XtPointer, XtPointer );
void pgwfmt_doContinue  ( Boolean overw_flag, Boolean dura_flag );
void pgwfmt_rwnmUpdateCb(Widget, XtPointer, XtPointer );
static void pgwfmt_WCLctlBtnCb( Widget, long, XtPointer );
static void pgwfmt_nearbyWfoCb( Widget, long, XEvent* );

/*
 *  private functions
 */
static void pgwfmt_popupWCL( void );
static void pgwfmt_popdownWCCsave ( void );
void pgwfmt_adjareaInc ( char *includes );
Widget pgwfmt_createIncludes ( Widget parent, char *labelstr, int ncol, 
			int nbtns, struct incInfo *incstruct );
Widget pgwfmt_createMenuText ( Widget parent, char *labelstr, int ncol, 
			int textoff, int info_type, Widget *textwid,
                        Widget *btnwid );
Widget pgwfmt_createOptArea ( Widget parent, char *labelstr, int lbl_spc,
                        int nopt, char opts[][20], WidgetList optw,
			int opt_spc, int nrow, XtPointer optvalp,
	       		XtCallbackProc callback );
void pgwfmt_getTime ( char *itime, char *etime, int *diff );
void pgwfmt_rdInfo ( int *iret );
void pgwfmt_setCurTime ( void );
void pgwfmt_unsetAdjareaBtns ( void );
void pgwfmt_loadWfos ( void );


/************************************************************************
 * nmap_pgwfmt.c							*
 *									*
 * This module defines a watch county list editing popup window for	*
 * product generation.  It also defines the windows that construct the  *
 * WCC and WCL text files.						*
 *									*
 * CONTENTS:								*
 *	pgwfmt_create()		create the watch box formatting window	*
 *      pgwfmt_createWCC()      creates WCC popup window                *
 *      pgwfmt_createWCL()      creates the save WCL window		*
 *									*
 *	pgwfmt_popup()		pop up the watch box formatting window	*
 *      pgwfmt_popupWCC()       pop up the WCC window                   *
 *      pgwfmt_popupWCL()       pop up the WCL window                   *
 *	pgwfmt_popdown()	pop down the watch box format window	*
 *      pgwfmt_popdownWCC()     pop down the WCC window                 *
 *      pgwfmt_popdownWCCsave() pop down the WCC file save window       *
 *      pgwfmt_popdownWCL()     pop down the WCL window                 *
 *	pgwfmt_setWatch()	sets the watch element information	*
 *      pgwfmt_setWCC()         sets the watch ele. info. for WCC       *
 *	pgwfmt_update()		update the watch format text area	*
 *      pgwfmt_formatSave()     save the watch format info.             *
 *									*
 *	pgwfmt_isUp()		query if the window is up 		*
 *	pgwfmt_getfname()	query watch text filename		*
 *									*
 *	pgwfmt_issueCb()	callback for the option buttons		*
 *	pgwfmt_wtypCb()		callback for the option buttons		*
 *	pgwfmt_svrty()		callback for the option buttons		*
 *	pgwfmt_tzoneCb()	callback for the option buttons		*
 *	pgwfmt_wtypWCCCb()	callback for the option buttons		*
 *	pgwfmt_menuTextCb()	callback for the menu text widgets	*
 *	pgwfmt_includesCb()	callback for the include button widgets	*
 *      pgwfmt_phonePbCb()      callback for WCC phone # opt. menu.     * 
 *      pgwfmt_preIdPbCb()      callback for preliminary id selection   * 
 *	pgwfmt_ctlBtnCb()	callback for control buttons 		*
 *      pgwfmt_WCCctlBtnCb()    callback for WCC ctl buttons            *
 *      pgwfmt_WCCfilePbCb()    callback for WCC Save window ctl btns   *
 *	pgwfmt_selectEh()	event handler for selecting watch boxes	*
 *      pgwfmt_WCCselectEh()    callback for watch boxes when WCC is on *
 *      pgwfmt_ymdTextCb        callback for ini&exp time text widgets  *
 *      pgwfmt_WCCymdTextCb     callback for WCC exp. time text widget  *
 *      pgwfmt_WCLctlBtnCb()    callback for WCL ctl buttons            *
 *	pgwfmt_cfmCb()		callback for confirming watch overwrite	*
 *      pgwfmt_nearbyWfoCb()	callback for nearby WFO button		*
 *	pgwfmt_rwnmUpdateCb()	callback for Rep. Watch Nm. text widget *
 *									*
 *	pgwfmt_createOptArea()	creates an option radio box		*
 *	pgwfmt_createMenuText()	creates a text widget with a menu	*
 *	pgwfmt_createIncludes()	creates a label widget and check boxes	*
 *	pgwfmt_rdInfo()		reads the watch formatting info table	*
 *	pgwfmt_setCurTime()	sets the default time			*
 *	pgwfmt_getTime()	gets the current times			*
 *	pgwfmt_unsetAdjareaBtns() unset the adjacent area buttons	*
 *      pgwfmt_adjareaInc()     returns the adjacent area string        *
 *      pgwfmt_doContinue()	func. for "Continue" btn of _pgwfmtWin  *
 *	pgwfmt_loadWfos()	load proposed,replaced,nearby WFO lists *
 ***********************************************************************/

/*=====================================================================*/

Widget pgwfmt_create ( Widget parent )
/************************************************************************
 * pgwfmt_create							*
 *									*
 * This function creates the watch box formatting popup window.		*
 *									*
 * Widget pgwfmt_create(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgwfmt_create	Widget	Widget ID of the county list popup 	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		12/98	moved formatting from pgwlst_create	*
 * D.W.Plummer/NCEP	 3/99	Added call to read watch information	*
 * S. Law/GSC		03/99	added pulldown menus and broke up time	*
 * S. Law/GSC		04/99	added states/adjArea includes		*
 * H. Zeng/EAI          12/99   added a pulldown menu for exp. time     *
 * H. Zeng/EAI          12/99   modified to add more functionalities    *
 * A. Hardy/GSC		 5/00   added continuing watch numbers          *
 * E. Safford/GSC	12/00	fix compiler type mismatch warning	*
 * H. Zeng/EAI          01/01   removed "States Include" area           *
 * J. Wu/GSC		05/01	add a label to list state names		*
 * H. Zeng/EAI          06/01   added a pulldown menu for ini. time     *
 * H. Zeng/EAI          07/01   restrict # of chars on time textboxes   *
 * H. Zeng/EAI          11/01   added check to *rdInfo() return value   *
 * T. Piper/SAIC	11/01	Freed btnw				*
 * E. Safford/SAIC	05/02	add text window sz limits & verif Cbs   *
 * H. Zeng/EAI          05/02   removed initial time input              *
 * M. Li/SAIC		12/02	radio box -> check Box			*
 * M. Li/SAIC		07/03	widen _rwnmTxtW & _cntnTxtW		*
 * A. Hardy/NCEP	 3/04   removed 'adjacent areas' widget		*
 * T. Piper/SAIC	10/05	declared ii long			*
 * T. Piper/SAIC	07/06	Use XmTextSetMaxLength vs. XmNmaxLength	*
 ***********************************************************************/
{
    Widget	pane, wnumrc, vtimrc, rc1, rc2, rc3;
    Widget	hailform, gustform, topsform, smvdform, smvsform, fcstform;
    Widget	isopt_form, wtyp_form, svrty_form, tzone_form, rwnmrc;
    Widget	cntnform;
    WidgetList	btnw;

    int		nn, iret, toff = 10, loff = 5, txsize = 5;
    long	ii;
    char	issopts[][20] = {"Test", "Active"};
    char	wtypopts[][20] = {"Svr T'storm", "Tornado"};
    char	svrtyopts[][20] = {"Normal", "PDS"};
    char	tzoneopts[][20] = {"EST", "CST", "MST", "PST", 
			           "EDT", "CDT", "MDT", "PDT"};

    char	*btnstrs[] = {"Continue", "Apply", "Cancel"};
/*---------------------------------------------------------------------*/

    /*
     *  Read in watch format base information.
     */
    pgwfmt_rdInfo( &iret );
    if ( iret != 0 ) {
         fprintf(stderr, 
                 "Error in reading watch format related tables.\n");
         exit(1);
    }

    /*
     * create dialog shell
     */
    _pgwfmtWin = XmCreateFormDialog(parent, "pgwfmt_popup",
				    NULL, 0);
    XtVaSetValues(_pgwfmtWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pgwfmtWin),
		  XmNtitle, "Format Watch",
		  NULL);
    
    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("pgwfmt_pane",
			    xmPanedWindowWidgetClass, _pgwfmtWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    
    /*
     * create FORMATTING area
     */
    _formatForm = XtVaCreateWidget("form",
				   xmFormWidgetClass,      pane,
				   NULL);
    
    /*
     * issuing status
     */
    nn = XtNumber(issopts);
    _issueW = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    isopt_form = pgwfmt_createOptArea (_formatForm, "Issuing Status", 
				       0, nn, issopts, _issueW, 0, 
				       1, (XtPointer)&_issueOpt,
					(XtCallbackProc)pgwfmt_issueCb);

    /*
     * watch number
     */
    wnumrc = (Widget)NxmTxtIn_create(_formatForm, "Watch Number", txsize,
				     &_wnumTxtW);
    XtVaSetValues( wnumrc, 
		   XmNtopAttachment,       	XmATTACH_WIDGET,
		   XmNtopWidget,           	isopt_form,
		   NULL );

    ii = WNUM;
    XtVaSetValues( _wnumTxtW,  
    		   XmNuserData, 		ii, 
  		   XmNmaxLength,		4, 
		   NULL );

    ii = TEXT_FIELD ;
    XtAddCallback( _wnumTxtW, XmNlosingFocusCallback, 
    		   (XtCallbackProc) pgwfmt_menuTextCb,
		   (XtPointer) ii );

    XtAddCallback( _wnumTxtW, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc) pgutls_vrfyPosIntCb,
		   NULL );



		   
    /*
     *  valid time
     */
    vtimrc = XtVaCreateWidget ("vtimrc",
			       xmRowColumnWidgetClass,	_formatForm,
			       XmNorientation,		XmHORIZONTAL,
			       XmNpacking,		XmPACK_TIGHT,
			       NULL);

    rc1 = XtVaCreateWidget ("vtimlabel",
			    xmRowColumnWidgetClass,	vtimrc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_COLUMN,
			    NULL);

    rc2 = XtVaCreateWidget ("vtim_ymd",
			    xmRowColumnWidgetClass,	vtimrc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_COLUMN,
			    NULL);

    rc3 = XtVaCreateWidget ("vtim_hhmm",
			    xmRowColumnWidgetClass,	vtimrc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_COLUMN,
                            XmNadjustLast,              FALSE,
                            XmNadjustMargin,            FALSE,
			    NULL);

    nn = 8;
    XtVaCreateManagedWidget (" ",
			     xmLabelWidgetClass,	rc1,
			     XmNmarginHeight,		nn,
			     NULL, 0);

    XtVaCreateManagedWidget ("YYYYMMDD",
			     xmLabelWidgetClass,	rc2,
			     NULL, 0);

    XtVaCreateManagedWidget ("HHMM",
			     xmLabelWidgetClass,	rc3,
			     NULL, 0);

    XtVaCreateManagedWidget ("Expiration Time",
			     xmLabelWidgetClass,	rc1,
			     XmNmarginHeight,		nn,
                             XmNmarginWidth,              0,
			     NULL, 0);

    /*
     * YYYYMMDD field for exp. time.
     */
    _eTime.ymd = 
	    XtVaCreateManagedWidget("ymd", 
                                    xmTextWidgetClass, rc2,
				    XmNcolumns,               8, 
                                    XmNmaxLength,             8,
                                    NULL);

    XtAddCallback (_eTime.ymd,   XmNlosingFocusCallback,
		       (XtCallbackProc) pgwfmt_ymdTextCb,
		   (XtPointer)NULL                        );

        /*
         *  Create "HHMM" boxes for expiration time.
         */
    pgwfmt_createMenuText (  rc3, 
			     NULL, 4, 0,
			     EXPT, &(_eTime.hhmm),
                             NULL );
    XmTextSetMaxLength(_eTime.hhmm, 4);


    XtVaSetValues (vtimrc,
		   XmNtopAttachment,	XmATTACH_WIDGET,
		   XmNtopWidget,	wnumrc,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    XtManageChild (rc1);
    XtManageChild (rc2);
    XtManageChild (rc3);
    XtManageChild (vtimrc);

    /*
     * watch type
     */
    nn = XtNumber(wtypopts);
    _wtypW = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    wtyp_form = pgwfmt_createOptArea (_formatForm, "Watch Type", 0, 
				      XtNumber(wtypopts), wtypopts, _wtypW, 
				      0, 1, (XtPointer)&_wtypOpt,
				      (XtCallbackProc)pgwfmt_wtypCb);
    XtVaSetValues(wtyp_form, 
		  XmNtopAttachment,       XmATTACH_WIDGET,
		  XmNtopWidget,           vtimrc,
		  NULL);
    
    /*
     * severity
     */
    nn = XtNumber(svrtyopts);
    _svrtyW = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    svrty_form = pgwfmt_createOptArea (_formatForm, "Severity", 0,  
				       nn, svrtyopts,
                                       _svrtyW, 0, 1, (XtPointer)&_svrtyOpt,
				       (XtCallbackProc)pgwfmt_svrtyCb);
    XtVaSetValues(svrty_form, 
		  XmNtopAttachment,       XmATTACH_WIDGET,
		  XmNtopWidget,           wtyp_form,
		  NULL);
    
    /*
     * time zone
     */
    nn = XtNumber(tzoneopts);
    _tzoneW = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    tzone_form = pgwfmt_createOptArea (_formatForm, 
				       "Primary Watch Time Zone", 0,  
				       nn, tzoneopts, _tzoneW, 
				       0, 2, (XtPointer)&_tzoneOpt,
				       (XtCallbackProc)pgwfmt_tzoneCb);
    XtVaSetValues(tzone_form, 
		  XmNtopAttachment,       XmATTACH_WIDGET,
		  XmNtopWidget,           svrty_form,
		  NULL);
    
    /* 
     * hail size
     */
    hailform = pgwfmt_createMenuText (_formatForm, 
				      "Max Hail Size (in.): ", txsize, loff,
				      HAIL, &_hailTxtW, NULL );

    XtVaSetValues( hailform, 
		   XmNtopAttachment,		XmATTACH_WIDGET,
		   XmNtopWidget,		tzone_form,
		   XmNtopOffset,		toff,
		   NULL );

    XmTextSetMaxLength(_hailTxtW, 3);
  
    XtAddCallback( _hailTxtW, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc) pgutls_vrfyPosFltCb,
		   NULL );
  

    /*
     * wind gusts 
     */
    gustform = pgwfmt_createMenuText (_formatForm, 
				      "Max Wind Gusts (kts): ", txsize, loff,
				      GUST, &_gustTxtW, NULL );

    XtVaSetValues(gustform, 
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		hailform,
		  XmNtopOffset,		toff,
		  NULL);

    XmTextSetMaxLength(_gustTxtW, 3);

    XtAddCallback( _gustTxtW, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc) pgutls_vrfyPosIntCb,
		   NULL );


    /*
     * max tops 
     */
    topsform = pgwfmt_createMenuText (_formatForm, 
				      "Max Tops (hundreds of feet): ", txsize, 
				      loff, TOPS, &_topsTxtW, NULL );

    XtVaSetValues(topsform, 
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		gustform,
		  XmNtopOffset,		toff,
		  NULL);
    
    XmTextSetMaxLength(_topsTxtW, 3);

    XtAddCallback( _topsTxtW, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc) pgutls_vrfyPosIntCb,
		   NULL );

    /*
     * mean motion vector 
     */
    smvdform = pgwfmt_createMenuText (_formatForm, 
				      "Mean Storm Motion Vector(deg): ", 
				      txsize, loff, SMVD, &_smvdTxtW, NULL );

    XtVaSetValues(smvdform, 
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		topsform,
		  XmNtopOffset,		toff,
		  NULL);

    XmTextSetMaxLength(_smvdTxtW, 3);

    XtAddCallback( _smvdTxtW, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc) pgutls_vrfyPosIntCb,
		   NULL );


    smvsform = pgwfmt_createMenuText (_formatForm, "(kts): ", txsize, loff,
				      SMVS, &_smvsTxtW, NULL );

    XtVaSetValues(smvsform, 
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		topsform,
		  XmNtopOffset,		toff,
		  XmNleftAttachment,	XmATTACH_WIDGET,
		  XmNleftWidget,	smvdform,
		  NULL);

    XmTextSetMaxLength(_smvsTxtW, 3);

    XtAddCallback( _smvsTxtW, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc) pgutls_vrfyPosIntCb,
		   NULL );


    /*
     * states included 
     */
    _statesLblW = XtVaCreateManagedWidget ("incstate",
				     xmLabelWidgetClass,	_formatForm,
				     XmNtopAttachment,		XmATTACH_WIDGET,
		  		     XmNtopWidget,		smvsform,
				     XmNtopOffset,		5,
				     NULL);

    NxmLabel_setStr(_statesLblW, "States Included:");

    XtManageChild (_statesLblW);

    /*
     * replacement watch number 
     */
    rwnmrc = (Widget)NxmTxtIn_create(_formatForm, 
				      "Replaced Watches", 
				      (txsize * 7), &_rwnmTxtW);

    XtVaSetValues (rwnmrc, 
		   XmNtopAttachment,	XmATTACH_WIDGET,
		   XmNtopWidget,	_statesLblW,
		   XmNtopOffset,	toff,
		   NULL);

    ii = RWNM;
    XtVaSetValues( _rwnmTxtW, 
		   XmNvalue,			"0000", 
		   XmNuserData,			ii,
		   XmNmaxLength,		MAX_REPLW_LEN-1,
		   NULL );

    ii = TEXT_FIELD ;
    XtAddCallback (_rwnmTxtW, XmNlosingFocusCallback,
		   (XtCallbackProc) pgwfmt_menuTextCb,
		   (XtPointer) ii);

    XtAddCallback( _rwnmTxtW, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc) pgutls_vrfyPosIntBlkCb,
		   NULL );

    /*
     * continuing watch numbers 
     */
    cntnform = (Widget)NxmTxtIn_create(_formatForm, 
				      "Continuing Watches", 
				      (txsize * 7), &_cntnTxtW);
    XtVaSetValues (cntnform, 
		   XmNtopAttachment,	XmATTACH_WIDGET,
		   XmNtopWidget,	rwnmrc,
		   XmNtopOffset,	toff,
		   NULL);

    ii = CNTN;
    XtVaSetValues( _cntnTxtW, 
		   XmNvalue,			"0000", 
		   XmNuserData,			ii,
		   XmNmaxLength,		MAX_CNTN_LEN-1,
		   NULL );

    ii = TEXT_FIELD ;
    XtAddCallback( _cntnTxtW, XmNlosingFocusCallback,
		   (XtCallbackProc) pgwfmt_menuTextCb,
		   (XtPointer) ii );

    XtAddCallback( _cntnTxtW, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc) pgutls_vrfyPosIntBlkCb,
		   NULL );


    /*
     * forecaster
     */
    fcstform = pgwfmt_createMenuText (_formatForm, 
				      "Forecaster: ", MXCHR, 
				      loff, FCST, &_fcstTxtW, NULL );

    XtVaSetValues( fcstform, 
		   XmNtopAttachment,		XmATTACH_WIDGET,
		   XmNtopWidget,		cntnform,
		   XmNtopOffset,		toff,
		   NULL );

    XmTextSetMaxLength(_fcstTxtW, MAX_FCSTR_LEN -1);

    XtManageChild (_formatForm);
    XtSetSensitive (_formatForm, False);



    /*
     * create control buttons
     */
    nn = XtNumber(btnstrs);
    btnw = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    NxmCtlBtn_create(pane, 1, "pgwfmt_ctlBtn", nn,
		     btnstrs, (XtCallbackProc)pgwfmt_ctlBtnCb, btnw);
    _saveBtn = btnw[0];
    _applyBtn = btnw[1];
    XtFree((XtPointer)btnw);

    XtSetSensitive (_saveBtn, False);
    XtSetSensitive (_applyBtn, False);
    
    XtManageChild(pane);

    return(_pgwfmtWin);
    
}

/*=====================================================================*/

Widget pgwfmt_createWCC ( Widget parent )
/************************************************************************
 * pgwfmt_createWCC							*
 *									*
 * This function creates the Watch Conference Call popup window.	*
 *									*
 * Widget pgwfmt_createWCC(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgwfmt_createWCC	Widget	Widget ID of the WCC popup 	        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          11/01   initial coding                          *
 * T. Piper/SAIC	12/01	freed btnw, flentry			*
 * M. Li/SAIC		06/02	Added Preliminary ID widget            	*
 * E. Safford/SAIC	06/02	line up widgets, add ":" to all labels  *
 * H. Zeng/EAI          07/02   changed parameter in cst_ncpy()         *
 * M. Li/SAIC		12/02	Add callback to pgwfmt_createOptArea	*
 * M. Li/SAIC		06/03	Added replacement watch widget		*
 * E. Safford/SAIC	05/05	free fontlist				*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 * H. Zeng/SAIC		11/05	added new callback for _replTxtW	*
 ***********************************************************************/
{
    Widget	pane, vtimrc, rc1, rc2, rc3, rc4, button;
    Widget	preId_form, fcst_form, wtyp_form, phone_form;
    Widget	phone_label, phone_menubar, preId_label, preId_menubar;
    Widget	replrc, label;
    WidgetList	btnw;

    int		toff = 10, loff = 36, nrow, ier;
    long	ii, nn;
    char	wtypopts[][20] = {"Svr T'storm", "Tornado"};
    char	*btnstrs[] = {"Format WCC/WCL", "Cancel"},
                *save_strs[] = {"Save", "Cancel"},
		*preId_list[] = 
		     {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J"};
    char	fontname[] = 
                     "-adobe-courier-bold-r-normal-*-*-120-*-*-m-*-*-*";
    XmString	xmstr;
    Display	*dsp;
    XmFontListEntry flentry;
    XmFontList	fontlist;
/*---------------------------------------------------------------------*/

    /*
     * create dialog shell
     */
    _pgwfmtWCCWin = XmCreateFormDialog(parent, "pgwfmt_WCCpopup",
				    NULL, 0);
    XtVaSetValues(_pgwfmtWCCWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pgwfmtWCCWin),
		  XmNtitle, "Watch Coordination",
		  NULL);
    
    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("pgwfmt_WCCpane",
			    xmPanedWindowWidgetClass, _pgwfmtWCCWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    
    /*
     * create FORMATTING area
     */
    _formatWCCForm = XtVaCreateWidget("form",
				   xmFormWidgetClass,      pane,
				   NULL);
    /*
     * Create preliminary Id list
     */
    preId_form = (Widget)XtVaCreateManagedWidget ("preId_form",
                 xmFormWidgetClass,    _formatWCCForm,
                 XmNleftAttachment,    XmATTACH_FORM,
                 XmNleftOffset,        6,
                 NULL);

    preId_label  = XtVaCreateManagedWidget ("Preliminary ID:",
                 xmLabelGadgetClass,   preId_form,
                 XmNleftAttachment,    XmATTACH_FORM,
                 XmNtopAttachment,     XmATTACH_FORM,
                 XmNtopOffset,         8,
                 NULL);

    preId_menubar  = XmCreatePulldownMenu(preId_form, "PreId", NULL, 0);
    _preIdOptW = XmCreateOptionMenu(preId_form, "preId", NULL, 0);

    nn = XtNumber(preId_list);
    for (ii=0; ii < nn; ii++) {
        xmstr = XmStringCreateLocalized (preId_list[ii]);
        _preIdPbW[ii] = XtVaCreateManagedWidget (" ",
                 xmPushButtonWidgetClass,       preId_menubar,
                 XmNlabelString,                xmstr,
                 NULL);
        XmStringFree (xmstr);

	XtAddCallback( _preIdPbW[ii], XmNactivateCallback,
		(XtCallbackProc)pgwfmt_preIdPbCb, (XtPointer) ii);
    }

    xmstr = XmStringCreateLocalized ("");
    XtVaSetValues (_preIdOptW,
                XmNlabelString,                 xmstr,
                XmNsubMenuId,                   preId_menubar,
                XmNmenuHistory,                 _preIdPbW[0],
                XmNleftAttachment,              XmATTACH_WIDGET,
                XmNleftWidget,                  preId_label,
                XmNleftOffset,                  8,
                NULL);
    XmStringFree (xmstr);
    XtManageChild (_preIdOptW);

    XtVaSetValues (preId_form,
                  XmNleftAttachment,   XmATTACH_FORM,
                  XmNleftOffset,       6,
                  NULL);
    XtManageChild (preId_form);

    /*
     *  Initialize value of _curPreId[]
     */
    cst_ncpy ( _curPreId, "A", sizeof(_curPreId)-1, &ier );
   


    /*
     * watch type
     */
    nn = XtNumber(wtypopts);
    _wtypWCCW = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    wtyp_form = pgwfmt_createOptArea (_formatWCCForm, "Watch Type:", 30, 
				  XtNumber(wtypopts), wtypopts, _wtypWCCW, 
				  0, 1, (XtPointer)&_wtypWCCOpt,
					(XtCallbackProc)pgwfmt_wtypWCCCb);
    for ( ii = 0; ii < nn; ii++ ) {
         XtVaSetValues(_wtypWCCW[ii], 
		    XmNhighlightThickness,    0,
		    NULL);
    }

    XtVaSetValues(wtyp_form, 
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        preId_form,
                   XmNleftAttachment,   XmATTACH_FORM,
                   XmNleftOffset,       6, 
		   NULL);

    /*
     *  expiration time
     */
    vtimrc = XtVaCreateWidget ("vtimrc",
			       xmRowColumnWidgetClass,	_formatWCCForm,
                               XmNleftAttachment,    	XmATTACH_FORM,
			       XmNorientation,		XmHORIZONTAL,
			       XmNpacking,		XmPACK_TIGHT,
			       NULL);

    rc1 = XtVaCreateWidget ("vtimlabel",
			    xmRowColumnWidgetClass,	vtimrc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_COLUMN,
			    NULL);

    rc2 = XtVaCreateWidget ("vtim_ymd",
			    xmRowColumnWidgetClass,	vtimrc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_COLUMN,
			    NULL);

    rc3 = XtVaCreateWidget ("vtim_hhmm",
			    xmRowColumnWidgetClass,	vtimrc,
			    XmNorientation,		XmVERTICAL,
			    XmNpacking,			XmPACK_COLUMN,
                            XmNadjustLast,              FALSE,
                            XmNadjustMargin,            FALSE,
			    NULL);

    nn = 8;
    XtVaCreateManagedWidget ("Expiration Time:",
			     xmLabelWidgetClass,	rc1,
			     XmNmarginHeight,		nn,
			     NULL, 0);


    for (ii = 1; ii < 2; ii++) {
	_wWCCTimes[ii].ymd = 
	    XtVaCreateManagedWidget("ymd", 
                                    xmTextWidgetClass, rc2,
				    XmNcolumns,               8, 
                                    XmNmaxLength,             8,
                                    NULL);

        XtAddCallback (_wWCCTimes[ii].ymd, XmNlosingFocusCallback,
		       (XtCallbackProc)    pgwfmt_WCCymdTextCb,
		       (XtPointer) ii                              );

    }

    /*
     *  Create "HHMM" boxes for initial&expiration time.
     */   
    pgwfmt_createMenuText (  rc3, 
			     NULL, 4, 0,
			     EXPT, &(_wWCCTimes[1].hhmm),
                             NULL );
    XmTextSetMaxLength(_wWCCTimes[1].hhmm, 4);

    XtVaSetValues (vtimrc,
		   XmNtopAttachment,	XmATTACH_WIDGET,
		   XmNtopWidget,	wtyp_form,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    XtManageChild (rc1);
    XtManageChild (rc2);
    XtManageChild (rc3);
    XtManageChild (vtimrc); 

    /*
     * Create phone number list
     */
    phone_form = (Widget)XtVaCreateManagedWidget ("phone_form",
		 xmFormWidgetClass,    _formatWCCForm,
                 XmNleftAttachment,    XmATTACH_FORM,
                 XmNleftOffset,        6,
	 	 NULL);
 
    phone_label  = XtVaCreateManagedWidget ("Phone Number:",
		 xmLabelGadgetClass,   phone_form,
		 XmNleftAttachment,    XmATTACH_FORM,
                 XmNtopAttachment,     XmATTACH_FORM,
                 XmNtopOffset,         8,
		 NULL); 

    phone_menubar  = XmCreatePulldownMenu(phone_form, "Phone", NULL, 0);
    _phoneOptW = XmCreateOptionMenu(phone_form, "phone", NULL, 0);

    for (ii=0; ii < _phoneInfo.nelems; ii++) {
        xmstr = XmStringCreateLocalized (_phoneInfo.range[ii]);
        _phonePbW[ii] = XtVaCreateManagedWidget (" ",
		 xmPushButtonWidgetClass,	phone_menubar,
		 XmNlabelString,		xmstr,
		 NULL);
        XmStringFree (xmstr);
       
        XtAddCallback(_phonePbW[ii], XmNactivateCallback,
		 (XtCallbackProc)pgwfmt_phonePbCb, (XtPointer) ii);

    }

    xmstr = XmStringCreateLocalized ("");
    XtVaSetValues (_phoneOptW, 
		XmNlabelString,			xmstr,	
		XmNsubMenuId,			phone_menubar,
		XmNmenuHistory,			_phonePbW[0], 
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			phone_label,
                XmNleftOffset,                  1,
		NULL);
    XmStringFree (xmstr);
    XtManageChild (_phoneOptW);

    XtVaSetValues (phone_form,
		   XmNtopAttachment,	XmATTACH_WIDGET,
		   XmNtopWidget,	vtimrc,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);
    XtManageChild (phone_form); 

    /*
     * forecaster
     */
    fcst_form = pgwfmt_createMenuText (_formatWCCForm, 
				      "Forecaster: ", MXCHR, 
				      loff, FCST, &_fcstWCCTxtW, NULL );

    XtVaSetValues(fcst_form, 
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		phone_form,
		  XmNtopOffset,		toff,
                  XmNleftAttachment,    XmATTACH_FORM,
                  XmNleftOffset,        6,
		  NULL);


    /*
     * Replaced watches
     */
    replrc = (Widget)NxmTxtIn_create(_formatWCCForm,
                                      "Replacement Watch Numbers",
                                      20, &_replTxtW);

    XtVaSetValues (replrc,
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        fcst_form,
                   XmNtopOffset,        toff,
                   NULL);

    XtAddCallback( _replTxtW, XmNmodifyVerifyCallback,
                   (XtCallbackProc) pgutls_vrfyPosIntBlkCb,
                   NULL );
    XtAddCallback( _replTxtW, XmNvalueChangedCallback,
                   (XtCallbackProc) pgwfmt_rwnmUpdateCb,
                   NULL );

    XtManageChild (_formatWCCForm);


    /*
     * create WFO area
     */
    _wfoForm = XtVaCreateWidget("wfo_form",
				xmFormWidgetClass,      pane,
				NULL);
    /*
     * Create Proposed WFOs label.
     */
    _propWfoLb = (Widget)XtVaCreateManagedWidget ("prop_wfo_label",
                 xmLabelWidgetClass,   _wfoForm,
		 XmNtopAttachment,     XmATTACH_FORM,
                 XmNleftAttachment,    XmATTACH_FORM,
                 XmNleftOffset,        4,
                 NULL);
    NxmLabel_setStr(_propWfoLb, "Proposed WFOs: ");

    _repWfoLb = (Widget)XtVaCreateManagedWidget ("rep_wfo_label",
                 xmLabelWidgetClass,   _wfoForm,
                 XmNleftAttachment,    XmATTACH_FORM,
                 XmNleftOffset,        4,
		 XmNtopAttachment,     XmATTACH_WIDGET,
		 XmNtopWidget,	       _propWfoLb,
		 XmNtopOffset,	       5,
                 NULL);
    NxmLabel_setStr(_repWfoLb, "Replaced  WFOs: ");


    /*
     * create "Nearby WFOs:" label 
     */
    label = XtVaCreateManagedWidget ("label",
				     xmLabelWidgetClass,   _wfoForm,
				     XmNleftAttachment,    XmATTACH_FORM,
				     XmNleftOffset,        4,
				     XmNtopAttachment,	   XmATTACH_WIDGET,
				     XmNtopWidget,	   _repWfoLb,
				     XmNtopOffset,	   5,
				     NULL);
    NxmLabel_setStr(label, "Nearby      WFOs:");

    /*
     * create button array 
     */
    nrow = (MAX_NEARBY_WFO % WFO_COL) ? 
                 (MAX_NEARBY_WFO / WFO_COL) + 1 : MAX_NEARBY_WFO / WFO_COL;
    _nearbyWfoRC = XtVaCreateManagedWidget("incrowcol",
				 xmRowColumnWidgetClass,    _wfoForm,
				 XmNtopAttachment,	    XmATTACH_OPPOSITE_WIDGET,
				 XmNtopWidget,		    label,
				 XmNleftAttachment,	    XmATTACH_WIDGET,
                                 XmNleftWidget,             label,
                                 XmNleftOffset,             5,
				 XmNrowColumnType,	    XmWORK_AREA,
				 XmNradioAlwaysOne,	    FALSE,
				 XmNorientation,	    XmHORIZONTAL,
				 XmNpacking,		    XmPACK_COLUMN,
				 XmNnumColumns,		    nrow,
				 NULL);

    for (ii = 0; ii < MAX_NEARBY_WFO; ii++) {
	_nearbyWfos[ii].wid = XtVaCreateManagedWidget ("WFO", 
				 xmToggleButtonWidgetClass,
				 _nearbyWfoRC,
                                 XmNtraversalOn,   FALSE,
				 NULL);

	XtAddCallback (_nearbyWfos[ii].wid, XmNvalueChangedCallback,
		       (XtCallbackProc) pgwfmt_nearbyWfoCb,
		       (XtPointer) ii);
    }

    XtManageChild (_wfoForm);


    /*
     * create control buttons
     */
    nn = XtNumber(btnstrs);
    btnw = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    NxmCtlBtn_create(pane, 1, "pgwfmt_ctlBtn", nn,
		     btnstrs, (XtCallbackProc)pgwfmt_WCCctlBtnCb, btnw);
    XtFree((XtPointer)btnw);    

    XtManageChild(pane);


    /**************************
     * WCC save file window
     **************************/
    _svfileWCCForm = XmCreateFormDialog (parent, "WCC_svfile", NULL, 0);

    dsp = XtDisplay (_svfileWCCForm);
    flentry = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);

    xmstr = XmStringCreateLocalized("WCC Save");
    XtVaSetValues(_svfileWCCForm,
		  XmNnoResize,		TRUE,
		  XmNautoUnmanage,	FALSE,
		  XmNdialogTitle,	xmstr,
		  NULL);

    XmStringFree(xmstr);

    _newWCCText = 
	(Widget) XtVaCreateManagedWidget ("svfile_text",
			     xmTextWidgetClass,	_svfileWCCForm,
			     XmNeditMode,	XmMULTI_LINE_EDIT,
			     XmNcolumns,	80,
			     XmNrows,		25,
			     XmNfontList,	fontlist,
			     XmNtopAttachment,	XmATTACH_FORM,
			     XmNtopOffset,	15,
			     XmNleftAttachment,	XmATTACH_FORM,
			     XmNrightAttachment,XmATTACH_FORM,
			     XmNscrollVertical, TRUE,
			     NULL);

    _svfileWCCText = 
	(Widget) XtVaCreateManagedWidget ("svfile_text",
			     xmTextWidgetClass,	_svfileWCCForm,
			     XmNeditable,	FALSE,
			     XmNcolumns,	80,
			     XmNfontList,	fontlist,
			     XmNtopAttachment,	XmATTACH_WIDGET,
			     XmNtopWidget,	_newWCCText,
			     XmNtopOffset,	15,
			     XmNleftAttachment,	XmATTACH_FORM,
			     XmNrightAttachment,XmATTACH_FORM,
			     NULL);

    rc4 = XtVaCreateManagedWidget ("svfile_rc",
			     xmRowColumnWidgetClass, _svfileWCCForm,
			     XmNorientation,	     XmHORIZONTAL,
			     XmNtopAttachment,	     XmATTACH_WIDGET,
			     XmNtopWidget,	     _svfileWCCText,
			     XmNleftAttachment,	     XmATTACH_FORM,
			     XmNrightAttachment,     XmATTACH_FORM,
			     NULL);

    nn = XtNumber (save_strs);
    for (ii = 0; ii < nn; ii++) {
	button = XtVaCreateManagedWidget (save_strs[ii],
					  xmPushButtonWidgetClass, 
					  rc4, NULL);       
	XtAddCallback (button, XmNactivateCallback, 
		       (XtCallbackProc)pgwfmt_WCCfilePbCb, (XtPointer) ii);

    }

    XmFontListFree( fontlist );
    return(_pgwfmtWCCWin);
    
}

/*=====================================================================*/

void pgwfmt_createWCL ( Widget parent )
/************************************************************************
 * pgwfmt_createWCL     						*
 *									*
 * Creates the Save WCL window.                             		*
 *									*
 * void pgwfmt_createWCL ( parent )                         		*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			None						*
 **									*
 * Log:									*
 * E. Safford/SAIC	06/02   initial coding                          *
 * E. Safford/SAIC	05/05	free fontlist				*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
char		*save_strs[] = {"Save", "Cancel"};
char		fontname[] = 
                	"-adobe-courier-bold-r-normal-*-*-120-*-*-m-*-*-*";
long		ii, nn;
Widget		rc, button;

XmString	xmstr;
Display		*dsp;
XmFontListEntry flentry;
XmFontList	fontlist;
Arg		args[12];    
/*---------------------------------------------------------------------*/

    _WCLWin = XmCreateFormDialog (parent, "Edit WCL", NULL, 0);
    XtVaSetValues( _WCLWin,
    		XmNdialogStyle,		XmDIALOG_APPLICATION_MODAL,
		NULL);

    dsp = XtDisplay (_WCLWin);
    flentry = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);
  
    xmstr = XmStringCreateLocalized("Save WCL");
    XtVaSetValues(_WCLWin,
		XmNnoResize,		TRUE,
		XmNautoUnmanage,	FALSE,
		XmNdialogTitle,		xmstr,
		NULL);

    XmStringFree(xmstr);

    ii=0;
    XtSetArg( args[ii], XmNeditMode,		XmMULTI_LINE_EDIT ); ii++,
    XtSetArg( args[ii], XmNcolumns,		               80 ); ii++;
    XtSetArg( args[ii], XmNrows,			       20 ); ii++;
    XtSetArg( args[ii], XmNfontList,			 fontlist ); ii++;
    XtSetArg( args[ii], XmNtopAttachment,	    XmATTACH_FORM ); ii++;
    XtSetArg( args[ii], XmNtopOffset,			       15 ); ii++;
    XtSetArg( args[ii], XmNleftAttachment,	    XmATTACH_FORM ); ii++;
    XtSetArg( args[ii], XmNrightAttachment,	    XmATTACH_FORM ); ii++;

    _WCLTextW = XmCreateScrolledText (_WCLWin, "svfile_text", args, ii);
    XtManageChild( _WCLTextW );

    rc = XtVaCreateManagedWidget ("svfile_rc",
		xmRowColumnWidgetClass, _WCLWin,
		XmNorientation,	     	XmHORIZONTAL,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,	    	_WCLTextW,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,     XmATTACH_FORM,
		NULL);
  
    nn = XtNumber (save_strs);

    for (ii = 0; ii < nn; ii++) {
	button = XtVaCreateManagedWidget (save_strs[ii],
		xmPushButtonWidgetClass, rc, 
		NULL);       

  	XtAddCallback (button, XmNactivateCallback, 
	    	(XtCallbackProc)pgwfmt_WCLctlBtnCb, (XtPointer) ii);
    }
  
    XmFontListFree( fontlist );

}

/*=====================================================================*/

void pgwfmt_popup ( void )
/************************************************************************
 * pgwfmt_popup								*
 *									*
 * This function pops up the watch box formatting popup window.		*
 *									*
 * void pgwfmt_popup()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    XtManageChild (_pgwfmtWin);
}

/*=====================================================================*/

void pgwfmt_popupWCC ( void )
/************************************************************************
 * pgwfmt_popupWCC							*
 *									*
 * This function pops up the WCC popup window.		                *
 *									*
 * void pgwfmt_popupWCC()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          11/01   initial coding                          *
 * H. Zeng/EAI          12/01   modified to add pgwlst_setPanesSen      *
 * M. Li/SAIC		11/03	Set _replTxtW to blank			*
 * H. Zeng/SAIC		11/05	managed _pgwfmtWCCWin at the very end	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    /*
     * Register new event handler with button press.
     */
    mcanvw_setPressFunc((XtEventHandler)&pgwfmt_WCCselectEh, CURS_DEFAULT); 
    mbotw_mouseSet(LMHINT_NOACTION, MMHINT_DONE);

    /*
     * Set the bottom three panes of Watch List Window insensitive.
     */
    pgwlst_setPanesSen(FALSE);

    XtManageChild (_pgwfmtWCCWin);

}

/*=====================================================================*/

static void pgwfmt_popupWCL ( void )
/************************************************************************
 * pgwfmt_popupWCL							*
 *									*
 * This function pops up the WCL window.		                *
 *									*
 * void pgwfmt_popupWCL()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	06/02	initial coding				*
 * H. Zeng/EAI          07/02   changed parameter in cst_ncpy()         *
 * H. Zeng/EAI          07/02   modified "etime" string                 *
 * D.W.Plummer/NCEP	10/02	use _elWatch to store watch element	*
 * m.gamazaychikov/SAIC	12/04	set el to newly updated watch element	*
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * F. J. Yen/NCEP	02/06	Increased size of WCL_text;added er_wmsg*
 * J. Wu/SAIC		04/06	added para newLineStr for cst_wrap 	*
 * T. Piper/SAIC	07/06	Use XmTextGetString to retrieve strings	*
 ***********************************************************************/
{
    char	*ptr, *ytwo, wtype[30], etime[12], fcstr[50];
    char	WCL_text[12400];
    int		ii, line_len = 66, ncyfip, *icyfip, ier;
    int		ier1, err_code;
    char	max_str[6] = {"12400"};
    VG_DBStruct	*el;
/*---------------------------------------------------------------------*/

    el = (VG_DBStruct*) pgwatch_getCurElm();

    cst_ncpy ( wtype, _watchtypes[_wtypWCCOpt], sizeof(wtype)-1, &ier );

    /*
     *  Widget format is YYYYMMDD, we need YYMMDD/HHMM in etime
     */
    ptr = XmTextGetString(_wWCCTimes[1].ymd);
    if ( strlen(ptr) > (size_t)2 ) {
        ytwo = ptr + 2;
    }
    cst_ncpy ( etime, ytwo, sizeof(etime)-1, &ier );
    strcat   ( etime, "/");
    if ( ptr ) XtFree (ptr);

    ptr = XmTextGetString(_wWCCTimes[1].hhmm);  
    if ( strlen(etime) + strlen(ptr) < sizeof(etime) ) {
        strcat (etime, ptr);
    }
    if ( ptr ) XtFree (ptr);


    /*
     *  Get the forecaster name.
     */
    ptr = XmTextGetString( _fcstWCCTxtW );
    cst_ncpy ( fcstr, ptr, sizeof(fcstr)-1, &ier ); 
    if ( ptr ) XtFree (ptr);

    /*
     *  Build an array of county fips codes.
     */
    ncyfip = el->elem.wbx.info.numcnty;
    icyfip = (int *) malloc( sizeof(int) * (size_t)ncyfip );

    for (ii=0; ii<ncyfip; ii++) {
	icyfip[ii] = el->elem.wbx.info.cn_fips[ii];
    }

    /*
     *  Send in the required information to the vf library to generate
     *  the watch county list.
     */
    vfsval( wtype, etime, fcstr, ncyfip, icyfip, &ier );


    /*
     *  Use "WCL_report" as a default name so we have a value
     *  in case the vfwwcl call goes south.  Then call vfwwcl to generate
     *  the watch county list text.
     */
    strcpy (_WCLfilnam, "WCL_report");
    vfwwcl ( _curPreId, _WCLfilnam, WCL_text, &ier1 );
    if ( ier1 == -2 ) {
	err_code = 12;
	er_wmsg ( "pgen", &err_code, max_str, &ier,
                        strlen("pgen"), strlen(max_str) );
        NxmErr_update();
    }
    
    /*
     *  Display the WCL_text to the WCL window.
     */
    cst_wrap ( WCL_text, blank, &line_len, NL, (char *)NULL, WCL_text, &ier ); 
    XmTextSetString (_WCLTextW, WCL_text);

    XtManageChild (_WCLWin);


    if ( icyfip ) free (icyfip);

}

/*=====================================================================*/

void pgwfmt_popdown ( void ) 
/************************************************************************
 * pgwfmt_popdown							*
 *									*
 * This function pops down watch box formatting window.			*
 *									*
 * void pgwfmt_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_pgwfmtWin)) {
	XtUnmanageChild (_pgwfmtWin);

	pgwfmt_setWatch (NULL);
    }
}

/*=====================================================================*/

void pgwfmt_popdownWCC ( void ) 
/************************************************************************
 * pgwfmt_popdownWCC							*
 *									*
 * This function pops down WCC window.			                *
 *									*
 * void pgwfmt_popdownWCC ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          11/01   initial coding                          *
 * H. Zeng/EAI          12/01   modified to use pgwlst_setPanesSen      *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    pgwfmt_popdownWCCsave();

    if (XtIsManaged (_pgwfmtWCCWin)) {
	XtUnmanageChild (_pgwfmtWCCWin);
    }

    /*
     * Set the bottom three panes of Watch List Window back sensitive.
     */
    pgwlst_setPanesSen(TRUE);

}

/*=====================================================================*/

static void pgwfmt_popdownWCCsave ( void ) 
/************************************************************************
 * pgwfmt_popdownWCCsave						*
 *									*
 * This function pops down WCC file save window.	                *
 *									*
 * static void pgwfmt_popdownWCCsave ()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC      06/02   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( XtIsManaged(_svfileWCCForm) ) {
         XtUnmanageChild(_svfileWCCForm);
    }

}

/*=====================================================================*/

void pgwfmt_popdownWCL ( void ) 
/************************************************************************
 * pgwfmt_popdownWCL							*
 *									*
 * This function pops down WCL window.			                *
 *									*
 * void pgwfmt_popdownWCL ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/SAIC      06/02   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( XtIsManaged(_WCLWin) ) {
        XtUnmanageChild(_WCLWin);
    }

}

/*=====================================================================*/

void pgwfmt_setWatch ( VG_DBStruct *el )
/************************************************************************
 * pgwfmt_setWatch							*
 *									*
 * This function sets the watch information for the selected element.	*
 *									*
 * void pgwfmt_setWatch (el)						*
 *									*
 * Input parameters:							*
 *      *el             VG_DBStruct     Watch element                   *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		04/99	added setup features			*
 * H. Zeng/EAI          12/99   added new parameter                     *
 * S. Law/GSC		12/99	renamed	from pgwfmt_setSensitive	*
 * H. Zeng/EAI          01/00   correct "States Included" button problem*
 * H. Zeng/EAI          02/00   set watch type default to none          *
 * A. Hardy/GSC		05/00   added continuing watch defaults		*
 * M. Li/GSC		05/00	removed searching for ww file		*
 * H. Zeng/EAI          01/01   removed state info. on the window       *
 * D.W.Plummer/NCEP     05/01   Added setting of adj areas		*
 * J. Wu/GSC		05/01	update list of state names		*
 * H. Zeng/EAI          06/01   added call to pgwfmt_setCurTime()       *
 * H. Zeng/EAI          07/01   removed unused variables                *
 * D.W.Plummer/NCEP	07/01   added continuing watch num generation	*
 * E. Safford/SAIC	05/02	wipe _hailTxtW before assignment	*
 * H. Zeng/EAI          05/02   removed initial time input              *
 * M. Li/SAIC           12/02   radio box -> check Box                  *
 * M. Li/SAIC		07/02	Added Replacement Watch Number		*
 * B. Yin/SAIC		03/04	Changed css_gtim calling sequences	*
 * A. Hardy/NCEP	03/04	removed 'adjacent areas'		*
 * A. Hardy/NCEP	04/04	Added fill in default info from WCC	*
 * A. Hardy/NCEP	05/04	Add check for valid watch type; removed *
 *				unused var. rtext			*
 * A. Hardy/NCEP	06/04	Added repl watch no. to issued watch	*
 * A. Hardy/NCEP	07/04	Added check for coastal marine zones	*
 * H. Zeng/SAIC		01/05	added more coastal state checks		*
 * T. Piper/SAIC	07/06	Removed duplicate initializations	*
 * T. Piper/SAIC	07/06	Use XmTextSetString to set strings	*
 ***********************************************************************/
{
    int  inum, ii, nn, adjust_min, tarry[5], icntnw[20], ncntnw, iret;
    int	 itype, nhot, ier;
    int  wtyp_map[] = { TRWWTCH, TORWTCH };
    int  year, month, day;
    char *ptr, statelst[LSTR], stinc[LSTR];
    char ptext[64], ftext[MAX_FCSTR_LEN];
    char tzoneopts[][20] = {"EST", "CST", "MST", "PST", 
			    "EDT", "CDT", "MDT", "PDT"};
    char chhmm[10], cymd[10], now[20], tstr[16], prefs_tag[11];
    int	ilens, ipos;
    Boolean addmz;
/*---------------------------------------------------------------------*/

/*
 * el may point to NULL.
 */
    if (el != NULL) {

/*
 * Make set current time the first thing no matter we are 
 * editing new or old watch element.
 */
        pgwfmt_setCurTime ();
 

        if (el->elem.wbx.info.w_number != IMISSD) {

/* 
 * Restore watch format info. from passed in watch element
 */
            _issueOpt = el->elem.wbx.info.w_istat;
	    for (ii = 0; ii < MAX_ISSUE; ii++) {
		if ( ii == _issueOpt )
	    	    XmToggleButtonSetState(_issueW[_issueOpt], True, False);
		else
		    XmToggleButtonSetState(_issueW[ii], False, False);
	    }
	
	    sprintf(ptext, "%04d", el->elem.wbx.info.w_number);
            XmTextSetString(_wnumTxtW, ptext);

            strcpy(ptext, el->elem.wbx.info.w_exp_t);
            ptr = strtok (ptext, "/" ) ;
            sscanf(ptr, "%d", &month);
            ptr = strtok(NULL, "/");
            sscanf(ptr, "%d", &day);
            ptr = strtok(NULL, "/");
            sscanf(ptr, "%d", &year);

            sprintf(cymd, "%04d%02d%02d", year, month, day);
            XmTextSetString( _eTime.ymd, cymd );

            ptr = strtok(NULL, "/");
            XmTextSetString( _eTime.hhmm, ptr );
            _wtypOpt = WTYP_DEFAULT;
            for(ii = 0; ii < WTYP_CHOICE; ii++) {
              if( wtyp_map[ii] == el->elem.wbx.info.w_type) {
                _wtypOpt = ii;
              }
            }

            if (_wtypOpt == WTYP_DEFAULT) {
              for(ii = 0; ii < WTYP_CHOICE; ii++ ) {
                 XmToggleButtonSetState(_wtypW[ii], False, False);
              }
            }
            else {
	      for(ii = 0; ii < WTYP_CHOICE; ii++ ) {
		 if ( ii == _wtypOpt )
                     XmToggleButtonSetState(_wtypW[ii], True, True);
		 else
		     XmToggleButtonSetState(_wtypW[ii], False, False);
              }

            }

            _svrtyOpt = el->elem.wbx.info.w_severity;
	    for (ii = 0; ii < MAX_SVR; ii++ ) {
                 if ( ii == _svrtyOpt)
                     XmToggleButtonSetState(_svrtyW[_svrtyOpt], True, False);
                 else
                     XmToggleButtonSetState(_svrtyW[ii], False, False);
            }

            if (el->elem.wbx.info.w_timezone[0] == '\0') {
               _tzoneOpt = TZONE_DEFAULT;
            }
            else {
               nn = XtNumber(tzoneopts);
               for(ii = 0; ii < nn; ii++) {
                  if( strcmp(el->elem.wbx.info.w_timezone, 
                          tzoneopts[ii] ) == 0 ) {
                     _tzoneOpt = ii;
                  }
               }
            }

            if (_tzoneOpt == TZONE_DEFAULT) {
               for(ii = 0; ii < TZONE_CHOICE; ii++ ) {
                  XmToggleButtonSetState(_tzoneW[ii], False, False);
               }
            }
            else {
	      for (ii = 0; ii < TZONE_CHOICE; ii++ ) {
                 if ( ii == _tzoneOpt)
                     XmToggleButtonSetState(_tzoneW[ii], True, True);
                 else
                     XmToggleButtonSetState(_tzoneW[ii], False, False);
              }

            }


            XmTextSetString(_hailTxtW, el->elem.wbx.info.w_hailsz);
            XmTextSetString(_gustTxtW, el->elem.wbx.info.w_windg);
            XmTextSetString(_topsTxtW, el->elem.wbx.info.w_tops);
            XmTextSetString(_smvdTxtW, el->elem.wbx.info.w_msmv_d);
            XmTextSetString(_smvsTxtW, el->elem.wbx.info.w_msmv_s);

/*
 * Set replacement watch numbers.
 */

            XmTextSetString(_rwnmTxtW, el->elem.wbx.info.w_replw);

/*
 * Set adjacent areas.
 */

	    itype = 1;
	    css_gtim ( &itype, now, &ier );
	    gg_wcur ( now, icntnw, &ncntnw, &ier, strlen(now) );
	    strcpy (ptext, "0000");
	    if ( ncntnw > 0 )  ptext[0] = '\0';
	    for ( ii = 0; ii < ncntnw; ii++ )  {
		sprintf ( tstr, "%d", icntnw[ii] );
		if ( strlen(ptext) + strlen(tstr) + 1 < 
				(size_t)(MAX_CNTN_LEN -1) ) {
		    strcat ( ptext, tstr ); strcat ( ptext, " " );
		}
	    }
            ptr = ptext;
            XmTextSetString(_cntnTxtW, ptr);

	    cst_ncpy ( ftext, el->elem.wbx.info.w_fcstr, 
	    			MAX_FCSTR_LEN-1, &ier );
            XmTextSetString( _fcstTxtW, ftext );
	} 

/* 
 * if watch number is IMISSD 
 */
        else {

/*
 * Set default information
 */
	   inum = 0;
	   el->elem.wbx.info.w_number = inum;
	   strcpy(ptext, "");
	   XmTextSetString(_wnumTxtW, ptext);

/*
 * Set expiration time based on current time.
 */

           adjust_min = _curTimeArry[4] % 30;
           if (adjust_min != 0) {
               adjust_min = 30 - adjust_min;
           }
	   ti_addm (_curTimeArry, &adjust_min, tarry, &iret);
 
           adjust_min = 7 * 60;
           ti_addm (tarry, &adjust_min, tarry, &iret);
           if (tarry[4] == 30) {
	       adjust_min = 30;
	       ti_addm (tarry, &adjust_min, tarry, &iret);
           }
	   cst_lstr (_elWatch.elem.wbx.info.w_exp_t, &ilens, &ier);
	   if ( ilens == 0 ) {
               sprintf (cymd, "%04d%02d%02d", tarry[0], tarry[1], tarry[2] );
               sprintf (chhmm, "%02d%02d", tarry[3], tarry[4]);
	   }
	   else {
               sprintf (cymd,  "%.8s",_elWatch.elem.wbx.info.w_exp_t); 
               sprintf (chhmm, "%.4s",_elWatch.elem.wbx.info.w_exp_t+9); 
	   }

           XtVaSetValues (_eTime.ymd,  XmNvalue, cymd,  NULL);
           XtVaSetValues (_eTime.hhmm, XmNvalue, chhmm, NULL);
            
/*
 * Set adjacent areas, if any.
 */
	    strcpy ( _adjlist, "\0");

            ipos = -1;
            for ( ii = 0; ii < NMZNCH; ii++){
	        if( strstr(el->elem.wbx.info.w_states, _mznchck[ii] )
						 != NULL ) {
	            ipos = ii;
	            break;
	        }
            }
/* Determine if the watch touches a coastal state or a Great
 * Lake. If marine zones are not included, then add the Great
 * Lake id or CW (if touching a coastal state) to the 'States
 * Included' string.
 */
	    strcpy (prefs_tag, "ADD_MARZON");
	    ctb_pfbool (prefs_tag, &addmz, &ier );

	    if (  addmz == TRUE ) {
                if ( ipos >= 0 ) {
	            strcpy ( el->elem.wbx.info.w_adjarea, "CW" );
                    strcpy(_adjlist, el->elem.wbx.info.w_adjarea);
                }
                else {
	            strcpy ( el->elem.wbx.info.w_adjarea, " " );
                    strcpy(_adjlist, el->elem.wbx.info.w_adjarea);
                }
	    }
	    else {

	      clo_binpoly ("ADJ_CSTL", el->elem.wbx.info.numpts,
			   el->elem.wbx.latlon,
			   &(el->elem.wbx.latlon[el->elem.wbx.info.numpts]),
			   &ier );
	      nhot = clo_qnhot ();

	      if ( nhot > 0 ) {

                 strcpy(el->elem.wbx.info.w_adjarea, "CW");
	      }
	      else if ( nhot == 0 ) {

		if ( strcmp(el->elem.wbx.info.w_adjarea, "CW") == 0 )  {

		  strcpy (el->elem.wbx.info.w_adjarea, " ");
		}
		else {

		  strcat (el->elem.wbx.info.w_adjarea, " ");
		}


              }

              strcpy(_adjlist, el->elem.wbx.info.w_adjarea);

	    }

/*
 *  Set default information for other text fields 
 *  and radio boxes.
 */
           _issueOpt = ISSUE_DEFAULT;
	   for (ii = 0; ii < MAX_ISSUE; ii++) {
                if ( ii == _issueOpt )
                    XmToggleButtonSetState(_issueW[_issueOpt], True, False);
                else
                    XmToggleButtonSetState(_issueW[ii], False, False);
            }

/*
 * Check for forecaster name. If missing, then set both
 * watch type boxes to false.
 */

            _wtypOpt  =  WTYP_DEFAULT;
	    cst_lstr (_elWatch.elem.wbx.info.w_fcstr, &ilens, &ier);
	    if ( ilens > 0 ) {
                for(ii = 0; ii < WTYP_CHOICE; ii++) {
                    if( ii == _elWatch.elem.wbx.info.w_type) {
                        _wtypOpt = ii;
                    }
                }
	   }

            if (_wtypOpt == WTYP_DEFAULT) {
              for(ii = 0; ii < WTYP_CHOICE; ii++ ) {
                 XmToggleButtonSetState(_wtypW[ii], False, False);
              }
            }
            else {
	      for(ii = 0; ii < WTYP_CHOICE; ii++ ) {
		 if ( ii == _wtypOpt )
                     XmToggleButtonSetState(_wtypW[ii], True, True);
		 else
		     XmToggleButtonSetState(_wtypW[ii], False, False);
              }

            }

           _svrtyOpt = SVRTY_DEFAULT;
	   for (ii = 0; ii < MAX_SVR; ii++ ) {
                 if ( ii == _svrtyOpt)
                     XmToggleButtonSetState(_svrtyW[_svrtyOpt], True, True);
                 else
                     XmToggleButtonSetState(_svrtyW[ii], False, False);
            }


           _tzoneOpt = TZONE_DEFAULT;
           for(ii = 0; ii < TZONE_CHOICE; ii++ ) {
           XmToggleButtonSetState(_tzoneW[ii], False, False);
           }

	   XmTextSetString(_hailTxtW, _wfInfo[0].defelem);
	   XmTextSetString(_gustTxtW, _wfInfo[1].defelem);
	   XmTextSetString(_topsTxtW, _wfInfo[2].defelem);
	   XmTextSetString(_smvdTxtW, _wfInfo[3].defelem);
	   XmTextSetString(_smvsTxtW, _wfInfo[4].defelem);

	   cst_lstr (_elWatch.elem.wbx.info.w_fcstr, &ilens, &ier);
           if ( ilens != 0 ) { 
               XmTextSetString(_fcstTxtW, _elWatch.elem.wbx.info.w_fcstr);
	   }
	   else {
               XmTextSetString(_fcstTxtW, "");
	   }
	    itype = 1;
	    css_gtim ( &itype, now, &ier );
	    gg_wcur ( now, icntnw, &ncntnw, &ier, strlen(now) );
	    strcpy (ptext, "0000");
	    if ( ncntnw > 0 )  ptext[0] = '\0';
	    for ( ii = 0; ii < ncntnw; ii++ )  {
		sprintf ( tstr, "%d", icntnw[ii] );
		if ( strlen(ptext) + strlen(tstr) + 1 < 
				(size_t)(MAX_CNTN_LEN -1) ) {
		    strcat ( ptext, tstr ); strcat ( ptext, " " );
		}
	    }

	   XmTextSetString(_cntnTxtW, ptext);

        } /* the end of else */

	cst_lstr (_elWatch.elem.wbx.info.w_replw, &ilens, &ier);
	if ( ilens != 0 ) {
            XmTextSetString(_rwnmTxtW, 
	                _elWatch.elem.wbx.info.w_replw);
	}
	else {
            XmTextSetString(_rwnmTxtW , "0000");
	}

/*
 *  Update state name label.
 */
	pgwatch_gstate( stinc );
	strcpy(statelst, "States Included: ");
        strcat(statelst, stinc);
	strcat(statelst, _adjlist);
        NxmLabel_setStr(_statesLblW, statelst);
	
	strcpy(_statlist, stinc);
	
	XtSetSensitive (_formatForm, TRUE);
	XtSetSensitive (_saveBtn, TRUE);
	XtSetSensitive (_applyBtn, TRUE);
    } 
    else {
       
        NxmLabel_setStr(_statesLblW, "States Included: ");
	
	XtSetSensitive (_formatForm, FALSE);
	XtSetSensitive (_saveBtn, FALSE);
	XtSetSensitive (_applyBtn, FALSE);

	pghdlb_deselectAll ();
    }
}

/*=====================================================================*/

void pgwfmt_setWCC ( VG_DBStruct *el )
/************************************************************************
 * pgwfmt_setWCC							*
 *									*
 * This function set the watch information for the selected element on	*
 * WCC window.                                                          *
 *									*
 * void pgwfmt_setWCC (el)						*
 *									*
 * Input parameters:							*
 *      *el             VG_DBStruct     Watch element                   *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          11/01   initial coding                          *
 * D.W.Plummer/NCEP	10/02	save watch element to _elWatch		*
 * M. Li/SAIC           12/02   radio box -> check Box                  *
 * H. Zeng/SAIC		11/05	added call to pgwfmt_loadWfos()		*
 * T. Piper/SAIC        07/06   Use XmTextSetString to set strings      *
 ***********************************************************************/
{
    int  ii, adjust_min, tarry[5], iret;
    int  wtyp_map[] = { TRWWTCH, TORWTCH };
    int  year, month, day;
    char *ptr;
    char ptext[64], chhmm[10], cymd[10];
/*---------------------------------------------------------------------*/

    /*
     * el may point to NULL.
     */
    _elWatch = *el;
    if (el != NULL) {

        /*
         * Make set current time the first thing no matter we are 
         * editing new or old watch element.
         */
        pgwfmt_setCurTime ();
 

        if (el->elem.wbx.info.w_number != IMISSD) {

	    /* 
             * Restore watch format info. from passed in watch element
             */
            strcpy(ptext, el->elem.wbx.info.w_exp_t);
            ptr = strtok (ptext, "/" ) ;
            sscanf(ptr, "%d", &month);
            ptr = strtok(NULL, "/");
            sscanf(ptr, "%d", &day);
            ptr = strtok(NULL, "/");
            sscanf(ptr, "%d", &year);

            sprintf(cymd, "%04d%02d%02d", year, month, day);
            XmTextSetString( _wWCCTimes[1].ymd, cymd );

            ptr = strtok(NULL, "/");
            XmTextSetString( _wWCCTimes[1].hhmm, ptr );

            _wtypWCCOpt = WTYP_DEFAULT;
            for(ii = 0; ii < WTYP_CHOICE; ii++) {
              if(wtyp_map[ii] == el->elem.wbx.info.w_type) {
                _wtypWCCOpt = ii;
              }
            }

            if (_wtypWCCOpt == WTYP_DEFAULT) {
              for(ii = 0; ii < WTYP_CHOICE; ii++ ) {
                 XmToggleButtonSetState(_wtypWCCW[ii], False, False);
              }
            }
            else {
	      for(ii = 0; ii < WTYP_CHOICE; ii++ ) {
                 if ( ii == _wtypWCCOpt)
                     XmToggleButtonSetState(_wtypWCCW[ii], True, True);
                 else
                     XmToggleButtonSetState(_wtypWCCW[ii], False, False);
              }
            }

            XtVaSetValues (_phoneOptW, 
		        XmNmenuHistory,		_phonePbW[0], 
		        NULL);
            pgwfmt_phonePbCb (NULL, 0, NULL);

            XmTextSetString(_fcstWCCTxtW,
                                 el->elem.wbx.info.w_fcstr);

	} 

	/* 
	 * if watch number is IMISSD, set default information.
	 */
        else {

           /*
            * Set expiration time based on current time.
            */
           adjust_min = _curTimeArry[4] % 30;
           if (adjust_min != 0) {
               adjust_min = 30 - adjust_min;
           }
	   ti_addm (_curTimeArry, &adjust_min, tarry, &iret);
 
           adjust_min = 7 * 60;
           ti_addm (tarry, &adjust_min, tarry, &iret);
           if (tarry[4] == 30) {
	       adjust_min = 30;
	       ti_addm (tarry, &adjust_min, tarry, &iret);
           }

           sprintf (cymd, "%04d%02d%02d", tarry[0], tarry[1], tarry[2] );
           sprintf (chhmm, "%02d%02d", tarry[3], tarry[4]);

           XtVaSetValues (_wWCCTimes[1].ymd,  XmNvalue, cymd,  NULL);
           XtVaSetValues (_wWCCTimes[1].hhmm, XmNvalue, chhmm, NULL);
            
           /*
            *  Set default information for watch type,phone # and forcaster.
            */
           _wtypWCCOpt  =  WTYP_DEFAULT;
           for(ii = 0; ii < WTYP_CHOICE; ii++ ) {
               XmToggleButtonSetState(_wtypWCCW[ii], False, False);
           }

           XtVaSetValues (_phoneOptW, 
		       XmNmenuHistory,		_phonePbW[0], 
		       NULL);
           pgwfmt_phonePbCb (NULL, 0, NULL);

           XtVaSetValues (_fcstWCCTxtW, 
                          XmNvalue,  "",                NULL);   

        } /* the end of else */
	
	XtSetSensitive (_formatWCCForm, TRUE);

        /*
         * No matter the watch number is valid or not, 
         * Set "Replacement Watch Numbers" text field blank
         * and load all three WFO lists for Watch Coordination 
         * window.
         */
        XmTextSetString(_replTxtW, "");
        pgwfmt_loadWfos ( );

	XtSetSensitive (_wfoForm, TRUE);

    }
    else {
	XtSetSensitive (_formatWCCForm, FALSE);
	XtSetSensitive (_wfoForm,       FALSE);
	pghdlb_deselectAll ();

    }

}

/*=====================================================================*/

void pgwfmt_update ( int *iret )
/************************************************************************
 * pgwfmt_update                                                        *
 *                                                                      *
 * This function creates the text for a watch file.			*
 *                                                                      *
 * void pgwfmt_update ( iret)                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret   int             Return value                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      4/99   Created                                 *
 * D.W.Plummer/NCEP      4/99   Added identifying string on each line	*
 * S. Law/GSC		 5/99	Added check for multiple replacements	*
 * D.W.Plummer/NCEP      6/99   Added units to output			*
 * H. Zeng/EAI          12/99   Added call to pgwfmt_adjareaInc()       *
 * M. Li/GSC		 5/00	Added sscanf - wnum			*
 * H. Zeng/EAI          01/01   get state info. from pgwatch_gstate()   *
 * H. Zeng/EAI          07/01   changed the type of variable "ymd"      *
 * D.W.Plummer/NCEP	12/01	globalize watch types array		*
 * T. Piper/SAIC	12/01	freed XmTextGetString			*
 * E. Safford/SAIC	05/02	add checks to XtFrees			*
 * H. Zeng/EAI          05/02   changed iss.&val. time to unknown       *
 * T. Piper/SAIC        07/06   Use XmTextGetString to retrieve strings *
***********************************************************************/
{
    int		ii, nn, irwnm, ier, wnum, ncActive, ignore;
    char	str[LSTR], str0[LSTR], str1[LSTR];
    char	stranc[LSTR], strvor[LSTR], stinc[LSTR];
    char	strtmp[LSTR], *ptxt1, *ptr, *tmp1, *tmp2;
    char	disanc[SSTR], diranc[SSTR], stnanc[SSTR];
    char	disvor[SSTR], dirvor[SSTR], stnvor[SSTR];
    int		hwsm, hwnm, area;
    int		shape;
    char	shapes[][5]={"NS", "EW", "ESOL"};
    float	lat, lon;

    char	*issopts[] = {"TEST", "ACTIVE"};
    char	*severity[]={"NORMAL", "PDS"};
    char	*tzoneopts[] = {"EST", "CST", "MST", "PST",
				    "EDT", "CDT", "MDT", "PDT"};
    int		ilst[10];
    unsigned    ymd;
/*---------------------------------------------------------------------*/

    *iret = 0;

    pgprd_clear();

    /*
     *  Watch number
     */
    sscanf(tmp1 = XmTextGetString(_wnumTxtW), "%d", &wnum);
    if ( tmp1 ) XtFree(tmp1);
    sprintf(str, "%-20s%04d\n", "WATCH NUMBER:", wnum );
    pgprd_putstr( str, &ier );

    /*
     *  Severe thunderstorm / tornado
     */
    sprintf(str, "%-20s%s\n", "TYPE:", _watchtypes[_wtypOpt] );
    pgprd_putstr( str, &ier );

    /*
     *  PDS / Normal
     */
    sprintf(str, "%-20s%s\n", "PDS/Normal:", severity[_svrtyOpt] );
    pgprd_putstr( str, &ier );

    /*
     *  Issue (current) time (MM DD YYYY HHMM)
     */
    sprintf(str, "%-20sXX XX XXXX XXXX\n", "ISSUE TIME:" );
    pgprd_putstr( str, &ier );

    /*
     *  Valid time and expiration time (MM DD YYYY HHMM)
     */
    sprintf(str, "%-20sXX XX XXXX XXXX\n", "VALID TIME:" );
    pgprd_putstr( str, &ier );

    ptr = XmTextGetString(_eTime.ymd);
    sscanf(ptr, "%d", &ymd);  
    if ( ptr ) XtFree (ptr);
    ptr = XmTextGetString(_eTime.hhmm);  
    sprintf(str, "%-20s%02d %02d %04d %s\n", "EXPIRATION TIME:",
	    (ymd%10000)/100, (ymd%100), (ymd/10000), ptr );
    if ( ptr ) XtFree (ptr);
    pgprd_putstr( str, &ier );

    /*
     *  Endpoint information (wrt watch anchor and VOR points)
     *  (make sure hyphen is in 13th column per SPC specs)
     */
    pgwatch_gvert( 0, str0 );
    sscanf( str0, "%s %s %s %s %s %s %s %s",
           str, str, disanc, diranc, stnanc, disvor, dirvor, stnvor );
    sprintf( strtmp, "%s %s %s", disanc, diranc, stnanc );
    sprintf( stranc, "%-20s%-12s- ", "ENDPOINT (ANC,sm):", strtmp );
    sprintf( strtmp, "%s %s %s", disvor, dirvor, stnvor );
    sprintf( strvor, "%-20s%-12s- ", "ENDPOINT (VOR,nm):", strtmp );

    pgwatch_gvert( 4, str1 );
    sscanf( str1, "%s %s %s %s %s %s %s %s",
           str, str, disanc, diranc, stnanc, disvor, dirvor, stnvor );
    sprintf( strtmp, "%s %s %s", disanc, diranc, stnanc );
    strcat( stranc, strtmp );
    strcat( stranc, "\n" );
    sprintf( strtmp, "%s %s %s", disvor, dirvor, stnvor );
    strcat( strvor, strtmp );
    strcat( strvor, "\n" );

    pgprd_putstr( stranc, &ier );
    pgprd_putstr( strvor, &ier );

    /*
     *  Orientation and half-width info (ref watch corner points)
     */
    pgwatch_gattr( &hwsm, &hwnm, &shape, &area );
    sprintf( str, "%-20s%d %s\n", "ATTRIB (ANC,sm):", hwsm, shapes[shape-1] );
    pgprd_putstr( str, &ier );

    /*
     *  Orientation and half-width info (ref VOR corner points)
     */
    sprintf( str, "%-20s%d %s\n", "ATTRIB (VOR,nm):", hwnm, shapes[shape-1] );
    pgprd_putstr( str, &ier );

    /*
     *  4-watch corner points
     */
    pgwatch_gvert( 1, str0 );
    sscanf( str0, "%f %f", &lat, &lon );
    sprintf( str, "%-20s%.2f %.2f\n", "WATCH CORNER POINT:", lat, lon );
    pgprd_putstr( str, &ier );
    pgwatch_gvert( 3, str0 );
    sscanf( str0, "%f %f", &lat, &lon );
    sprintf( str, "%-20s%.2f %.2f\n", "WATCH CORNER POINT:", lat, lon );
    pgprd_putstr( str, &ier );
    pgwatch_gvert( 5, str0 );
    sscanf( str0, "%f %f", &lat, &lon );
    sprintf( str, "%-20s%.2f %.2f\n", "WATCH CORNER POINT:", lat, lon );
    pgprd_putstr( str, &ier );
    pgwatch_gvert( 7, str0 );
    sscanf( str0, "%f %f", &lat, &lon );
    sprintf( str, "%-20s%.2f %.2f\n", "WATCH CORNER POINT:", lat, lon );
    pgprd_putstr( str, &ier );

    /*
     *  Maximum hail size (in)
     */
    sprintf(str, "%-20s%s\n", "HAIL SIZE (in):", 
		tmp1 = XmTextGetString(_hailTxtW) );
    if ( tmp1 ) XtFree(tmp1);
    pgprd_putstr( str, &ier );

    /*
     *  Maximum thunderstorm wind gusts (kts)
     */
    sprintf(str, "%-20s%s\n", "MAX GUSTS (kts):", 
		tmp1 = XmTextGetString(_gustTxtW) );
    if ( tmp1 ) XtFree(tmp1);
    pgprd_putstr( str, &ier );

    /*
     *  Maximum thunderstorm tops (ft/100)
     */
    sprintf(str, "%-20s%s\n", "MAX TOPS (100s ft):", 
		tmp1 = XmTextGetString(_topsTxtW) );
    if ( tmp1 ) XtFree(tmp1);
    pgprd_putstr( str, &ier );

    /*
     *  Mean storm motion vector (degkts)
     */
    sprintf(str, "%-20s%s %s\n", "MOTION (deg,kts):", 
	    tmp1 = XmTextGetString(_smvdTxtW),
	    tmp2 = XmTextGetString(_smvsTxtW) );
    if ( tmp1 ) XtFree(tmp1);
    if ( tmp2 ) XtFree(tmp2);
    pgprd_putstr( str, &ier );

    /*
     *  Primary watch time zone
     */
    sprintf(str, "%-20s%s\n", "TIME ZONE:", tzoneopts[_tzoneOpt] );
    pgprd_putstr( str, &ier );

    /*
     *  Replacement watch numbers
     */
    ptxt1 = XmTextGetString(_rwnmTxtW);
    irwnm = 0;
    if ( ptxt1 != NULL )  sscanf (ptxt1 , "%d", &irwnm );
    if ( irwnm == 0 )  {
        sprintf(str, "%-20s%s\n", "REPL WATCH NUMBER:", "NONE" );
    }
    else  {
        sprintf(str, "%-20s", "REPL WATCH NUMBER:" );
	
	cst_rxbl( ptxt1, ptxt1, &nn, &ier );
	cst_ilst( ptxt1, ' ', 0, sizeof(ilst)/sizeof(int), ilst, &nn, &ier );

	for ( ii = 0; ii < nn; ii++ )  {
	    if ( ilst[ii] != 0 )  {
	        sprintf( str0, "%04d ", ilst[ii] );
	        strcat ( str, str0 );
	    }
	}

	strcat (str, "\n");
    }
    pgprd_putstr (str, &ier);
    if ( ptxt1 ) XtFree (ptxt1);

    /*
     *  States included
     */
    pgwatch_gstate( stinc );
    strcat(stinc, _adjlist);
    sprintf( str, "%-20s%s\n", "STATES INCLUDED:", stinc );
    pgprd_putstr( str, &ier );

    /*
     *  "ACTIVE" or "TEST"
     */
    sprintf(str, "%-20s%s\n", "STATUS:", issopts[_issueOpt] );
    pgprd_putstr( str, &ier );

    /*
     *  Forecaster name
     */
    sprintf(str, "%-20s%s\n", 
	    "FORECASTER:", tmp1 = XmTextGetString(_fcstTxtW) );
    if ( tmp1 ) XtFree(tmp1);
    pgprd_putstr( str, &ier );

    /*
     *  Area of the watch (sq nm)
     */
    sprintf(str, "%-20s%d\n", "WATCH AREA (sq nm):", area );
    pgprd_putstr( str, &ier );

    /*
     *  County listing
     */
    sprintf( str, 
	    "UGC    State County Name       Lat/Long    CntyFips  WFO\n");
    pgprd_putstr( str, &ier );

    pgwatch_gcnty(0, &ncActive, str);
    for (ii = 0; ii < ncActive; ii++)  {
        pgwatch_gcnty(ii, &ignore, str);
	sscanf (str, "%s %s", str0, str1);
	/*
	 *  Check state against "States Included" string.
	 */
	if (strstr (stinc, str1) != (char *)NULL)  {
            strcat (str, "\n");
            pgprd_putstr (str, &ier);
	}
    }

    /*
     *  Generate filename and save it local globally.
     */
    sprintf( _fName, "ww%04d.txt", wnum );

    return;

}

/*=====================================================================*/

void pgwfmt_formatSave ( Boolean firstime )
/************************************************************************
 * pgwfmt_formatSave							*
 *									*
 * This function saves the watch format info. into the watch element.	*
 *									*
 * void  pgwfmt_formatSave(firstime)					*
 *									*
 * Input parameters:							*
 *	firstime	Boolean	whether it is the first time of saving  *
 *				the info.				*
 *									*
 * Output parameters:							*
 *			NONE				                *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		12/99	initial coding				*
 * H. Zeng/EAI		01/00	modified for new w_states string	*
 * S. Law/EAI		01/00	removed parameter from pgwatch_editcnty	*
 * A. Hardy/GSC		05/00   added continuing watch information      *
 * M. Li/GSC		05/00	checked for watch # and forecaster	*
 * D.W.Plummer/NCEP	 8/00	change call to clo_ routine		*
 * H. Zeng/EAI          01/01   removed states stuff                    *
 * H. Zeng/EAI          06/01   modified for new time format            *
 * H. Zeng/EAI          07/01   changed the type of variable "ymd"      *
 * E. Safford/SAIC	05/02	correct length & comment out w_cntnw    *
 * E. Safford/SAIC	05/02	add checks to XtFrees			*
 * H. Zeng/EAI          05/02   calculate "issue time" at the very end  *
 * H. Zeng/SAIC		01/05	saved _adjlist into watch element	*
 * T. Piper/SAIC        07/06   Use XmTextGetString to retrieve strings *
***********************************************************************/
{
    int		wnum;
    size_t	ii, jj, kk;
    int		wtyp_map[] = { TRWWTCH, TORWTCH };
    char	*ptext;
    char	*tzoneopts[] = {"EST", "CST", "MST", "PST",
				"EDT", "CDT", "MDT", "PDT"};

/*  keep this for the moment -- see commented out usage below */
/*  char	w_cntnw[MAX_CNTN_LEN]; */

    VG_DBStruct	*el;
    unsigned    ymd;
/*---------------------------------------------------------------------*/

    el = (VG_DBStruct*) pgwatch_getCurElm();

    if( firstime ) {

        el->elem.wbx.info.w_istat = _issueOpt;

        ptext = XmTextGetString(_wnumTxtW);
        sscanf (ptext, "%d", &wnum);
	jj = kk = 0;
	for (ii = 0; ii < strlen(ptext); ii++) {
	    if( isalpha((unsigned long)ptext[ii]) ) kk++;
	    if( isspace((unsigned long)ptext[ii]) ) jj++;
	}
	if (wnum < 1 || wnum > 9999 || kk > (size_t)0 
	    || jj >= strlen(ptext) ) {
	    _validWnum = -1;
	    wnum = 0;
	} else 
	    _validWnum = 1;
	if ( ptext ) XtFree (ptext);      
        el->elem.wbx.info.w_number = wnum;

        ptext = XmTextGetString( _eTime.ymd );
        sscanf(ptext, "%d", &ymd);
	if ( ptext ) XtFree (ptext); 

	sprintf(el->elem.wbx.info.w_exp_t,"%02d", (ymd%10000)/100 );
        sprintf( (el->elem.wbx.info.w_exp_t+2), "/");
	sprintf( (el->elem.wbx.info.w_exp_t+3), "%02d", (ymd%100) );
        sprintf( (el->elem.wbx.info.w_exp_t+5), "/");
	sprintf( (el->elem.wbx.info.w_exp_t+6),"%04d", (ymd/10000) );
        sprintf( (el->elem.wbx.info.w_exp_t+10), "/");

        ptext = XmTextGetString( _eTime.hhmm );
	sprintf( (el->elem.wbx.info.w_exp_t+11), "%s", ptext );
        sprintf( (el->elem.wbx.info.w_exp_t+15), "/");
	if ( ptext ) XtFree (ptext);

        if (_wtypOpt != WTYP_DEFAULT) {
          el->elem.wbx.info.w_type = wtyp_map[_wtypOpt];
        }
          
        el->elem.wbx.info.w_severity = _svrtyOpt;

        if (_tzoneOpt != TZONE_DEFAULT) {
           strcpy(el->elem.wbx.info.w_timezone, 
                                 tzoneopts[_tzoneOpt] );
        }

        ptext = XmTextGetString(_hailTxtW);
	sprintf(el->elem.wbx.info.w_hailsz,"%s", ptext );
        if (ptext) XtFree (ptext);

        ptext = XmTextGetString(_gustTxtW);
	sprintf(el->elem.wbx.info.w_windg,"%s", ptext );
        if ( ptext ) XtFree (ptext);

        ptext = XmTextGetString(_topsTxtW);
	sprintf(el->elem.wbx.info.w_tops, "%s", ptext );
        if ( ptext ) XtFree (ptext);

        ptext = XmTextGetString(_smvdTxtW);
	sprintf(el->elem.wbx.info.w_msmv_d, "%s", ptext );
        if ( ptext ) XtFree (ptext);

        ptext = XmTextGetString(_smvsTxtW);
	sprintf(el->elem.wbx.info.w_msmv_s, "%s", ptext );
        if ( ptext ) XtFree (ptext);

        ptext = XmTextGetString( _rwnmTxtW );
	sprintf(el->elem.wbx.info.w_replw, "%s", ptext );
        if ( ptext ) XtFree (ptext);

        sprintf(el->elem.wbx.info.w_adjarea, "%s", _adjlist);
    
       /* 
        * The continuing watch info. is currently not saved to 
        * the text file. 
	*
	* Comment out until we figure out if this will be used again
        */
/*
        ptext = XmTextGetString( _cntnTxtW );
	sprintf(w_cntnw, "%s", ptext );
        if ( ptext ) XtFree (ptext);
*/
        ptext = XmTextGetString( _fcstTxtW );
	sprintf(el->elem.wbx.info.w_fcstr, "%s", ptext );
	jj = 0;
	for (ii = 0; ii < strlen(ptext); ii++)
	    if ( isalnum((unsigned long)ptext[ii]) ) jj++;
        if ( ptext ) XtFree (ptext);
	
	if ( jj > (size_t)0 ) 
	    _fcstOpt = 1;
	else
	    _fcstOpt = -1;

    } /* the end of if */
    else {

        /*
         * For the second time, save "w_file" and "issued"
         */
	strcpy(el->elem.wbx.info.w_file, _fName);
       
        el->elem.wbx.info.w_issued = 1;

        /*
         * Calculate "issue time" at the very end.
         */
        pgwfmt_setCurTime();

	sprintf(el->elem.wbx.info.w_iss_t,"%02d", _curTimeArry[1] );
        sprintf( (el->elem.wbx.info.w_iss_t+2), "/");
	sprintf( (el->elem.wbx.info.w_iss_t+3), "%02d", _curTimeArry[2] );
        sprintf( (el->elem.wbx.info.w_iss_t+5), "/");
	sprintf( (el->elem.wbx.info.w_iss_t+6),"%04d", _curTimeArry[0] );
        sprintf( (el->elem.wbx.info.w_iss_t+10), "/");
	sprintf( (el->elem.wbx.info.w_iss_t+11), "%02d", _curTimeArry[3] );
	sprintf( (el->elem.wbx.info.w_iss_t+13), "%02d", _curTimeArry[4] );
        sprintf( (el->elem.wbx.info.w_iss_t+15), "/");

    } /* the end of else */

}

/*=====================================================================*/

Boolean pgwfmt_isUp ( void ) 
/************************************************************************
 * pgwfmt_isUp								*
 *									*
 * This function queries whether the watch box formatting window is up.	*
 *									*
 * Boolean pgwfmt_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgwfmt_isUp		Boolean	     True -- up,  False -- down		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/99	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	return (XtIsManaged (_pgwfmtWin));
}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_issueCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwfmt_issueCb							*
 *									*
 * Callback function for issue radio buttons.				*
 *									*
 * void pgwfmt_issueCb (wid, which, call)				*
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
 * T. Piper/SAIC	12/02	created  from pgwfmt_optCb		*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/
	_issueOpt = (int)which;
	for (ii = 0; ii < MAX_ISSUE; ii++) {
	    XmToggleButtonSetState(_issueW[ii], FALSE, FALSE);
    	}
}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_wtypCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwfmt_wtypCb                                                        *
 *                                                                      *
 * Callback function for watch type option radio buttons.               *
 *                                                                      *
 * void pgwfmt_wtypCb (wid, which, call)                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long             which button                            *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        12/02   created  from pgwfmt_optCb              *
 ***********************************************************************/
{
    int         ii;
/*---------------------------------------------------------------------*/
	_wtypOpt = (int)which;
	for (ii = 0; ii < WTYP_CHOICE; ii++) {
            XmToggleButtonSetState(_wtypW[ii], FALSE, FALSE);
        }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_svrtyCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwfmt_svrtyCb                                                       *
 *                                                                      *
 * Callback function for severity option radio buttons.                 *
 *                                                                      *
 * void pgwfmt_svrtyCb (wid, which, call)                               *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long             which button                            *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        12/02   created  from pgwfmt_optCb              *
 ***********************************************************************/
{
    int         ii;
/*---------------------------------------------------------------------*/
        _svrtyOpt = (int)which;
	for (ii = 0; ii < MAX_SVR; ii++) {
            XmToggleButtonSetState(_svrtyW[ii], FALSE, FALSE);
        }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_tzoneCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwfmt_tzoneCb                                                       *
 *                                                                      *
 * Callback function for time zone option radio buttons.                *
 *                                                                      *
 * void pgwfmt_tzoneCb (wid, which, call)                               *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long             which button                            *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        12/02   created  from pgwfmt_optCb              *
 ***********************************************************************/
{
    int         ii;
/*---------------------------------------------------------------------*/
	_tzoneOpt = (int)which;
	for (ii = 0; ii < TZONE_CHOICE; ii++) {
            XmToggleButtonSetState(_tzoneW[ii], FALSE, FALSE);
        }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_wtypWCCCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwfmt_wtypWCCCb                                                     *
 *                                                                      *
 * Callback function for WCC watch type option radio buttons.           *
 *                                                                      *
 * void pgwfmt_wtypWCCCb (wid, which, call)                             *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long             which button                            *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        12/02   created  from pgwfmt_optCb              *
 ***********************************************************************/
{
    int         ii;
/*---------------------------------------------------------------------*/
	_wtypWCCOpt = (int)which;
	for (ii = 0; ii < WTYP_CHOICE; ii++) {
            XmToggleButtonSetState(_wtypWCCW[ii], FALSE, FALSE);
        }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_menuTextCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwfmt_menuTextCb							*
 *									*
 * Callback function for general option radio buttons.			*
 *									*
 * void pgwfmt_menuTextCb (wid, which, call)				*
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
 * S. Law/GSC		03/99	initial coding				*
 * H. Zeng/EAI          12/99   added auto-check for MM/DD/YYYY entries *
 * A. Hardy/GSC		05/99   added CNTN to textfield callback        *
 * H. Zeng/EAI          06/01   modified for initial time option menu   *
 * H. Zeng/EAI          07/01   put limit on typed-in ini&exp time      *
 * H. Zeng/EAI          07/01   added more checks for ini&exp time      *
 * H. Zeng/EAI          11/01   modified to deal with WCC text widget   *
 * T. Piper/SAIC	12/01	added a missing XtFree(ptext)		*
 * E. Safford/SAIC	05/02	mod assignment from menu item (default) *
 * E. Safford/SAIC	05/02	add checks to XtFrees			*
 * H. Zeng/EAI          05/02   removed initial time input              *
 * H. Zeng/EAI          07/02   modified exp. time losing focus callback*
 ***********************************************************************/
{
    char	*ptext=NULL, text[10];
    float	min, max, tnum;
    int         expHhmm, iniHhmm, etime[5], tintv, iret;
    int		type, format;
    XmString	xmstr;
    XtPointer	userdata;
    Widget	*twid;
/*---------------------------------------------------------------------*/

    /*
     * textfield callback
     */

    switch(which) {    

      case TEXT_FIELD :

	XtVaGetValues (wid, 
		       XmNvalue,	&ptext, 
		       XmNuserData,	&userdata, 
		       NULL);

	sscanf (ptext, "%f", &tnum);

	if ( ptext ) XtFree (ptext);

	type = (long)userdata;

	if (type == WNUM || type == RWNM || type == CNTN) {
	    min    = 0.0F;
	    max    = 9999.0F;
	    format = TYPE_INT;
	}
	else {
	    min    = (float)_wfInfo[type].min;
	    max    = (float)_wfInfo[type].max;
	    format = _wfInfo[type].format;
	}

	if (min <= max) {
	    tnum = (tnum < min) ? min : (tnum > max) ? max : -999.0F;

	    if (tnum >= 0.0F) {
		switch (format) {
		  case TYPE_INT:
		    sprintf (text, "%d", (int) tnum);
		    XtVaSetValues (wid, XmNvalue, text, NULL);
		    break;
		  case TYPE_FLOAT:
		    sprintf (text, "%.2f", tnum);
		    XtVaSetValues (wid, XmNvalue, text, NULL);
		    break;
		}
	    }
	}

        break;

    /*
     * expiration time textfield losing focus callback
     */

      case EXPIRATION_TIME :

	XtVaGetValues (wid, 
		       XmNvalue,	&ptext,
		       NULL);

	if ( sscanf (ptext, "%d", &expHhmm) != 1 ) {
             expHhmm = 0;
        }
	if ( ptext ) XtFree (ptext);

        if ( expHhmm > 2359 ) {
             expHhmm = 2359;
        }
        else if ( expHhmm < 0 || (expHhmm % 100) > 59 ) {
             expHhmm = 0;
        }
 
        sprintf( text, "%04d", expHhmm );
        XmTextSetString(wid, text);

        /* 
         * Compare Exp. HHMM value with Ini. HHMM value and set the
         * YYYYMMDD value accordingly.
         */
        iniHhmm = _curTimeArry[3] * 100 + _curTimeArry[4];
        if( expHhmm < iniHhmm ) {

            tintv = 1440;
	    ti_addm(_curTimeArry, &tintv, etime, &iret);
            sprintf(text, "%04d%02d%02d", etime[0], etime[1], etime[2] );
        }
        else {
            sprintf(text, "%04d%02d%02d", _curTimeArry[0], 
                                          _curTimeArry[1], 
                                          _curTimeArry[2]  );
        }

        if ( wid == _eTime.hhmm ) {

           XmTextSetString(_eTime.ymd, text);
        }
        else if ( wid == _wWCCTimes[1].hhmm ) {

           XmTextSetString(_wWCCTimes[1].ymd, text);
        }

        break;

    /*
     * menu item callback
     */
    default :

  	XtVaGetValues (wid, 
		       XmNlabelString,	&xmstr, 
		       XmNuserData,	&userdata, 
		       NULL);
	if ( ptext != NULL ) XtFree(ptext);
	XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &ptext);
	XmStringFree (xmstr);

	twid = (Widget *) userdata;
        XmTextSetString(*twid, "");
        XmTextSetString(*twid, ptext);

        if( *twid == _eTime.hhmm || *twid == _wWCCTimes[1].hhmm ) {

            /*
             *  Check MM/DD/YYYY entries of expiration time boxes
             */
	    sscanf (  ptext, "%d", &expHhmm  );
	    if ( ptext ) XtFree (ptext);

            iniHhmm = _curTimeArry[3] * 100 + _curTimeArry[4];
            if( expHhmm < iniHhmm ) {

                tintv = 1440;
	        ti_addm(_curTimeArry, &tintv, etime, &iret);

                sprintf(text, "%04d%02d%02d", etime[0],etime[1],etime[2] );
	    }
            else {
                sprintf(text, "%04d%02d%02d", _curTimeArry[0], 
                                              _curTimeArry[1], 
                                              _curTimeArry[2]  );
            }

            if ( *twid == _eTime.hhmm ) {

                 XmTextSetString(_eTime.ymd, text);
            }
            else if ( *twid == _wWCCTimes[1].hhmm ) {

                 XmTextSetString(_wWCCTimes[1].ymd, text);
            }

        }

        break;    


    } /* the end of switch */

}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_ymdTextCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwfmt_ymdTextCb							*
 *									*
 * Callback function for "YYYYMMDD" text widgets.			*
 *									*
 * void pgwfmt_ymdTextCb (wid, which, call)				*
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
 * H. Zeng/EAI          07/01   initial coding                          *
 * E. Safford/SAIC	05/02	add checks to XtFrees			*
 * H. Zeng/EAI          05/02   removed initial time input              *
 ***********************************************************************/
{
    char	*ptext, *timestr, result_time[15], cymd[10];
    int         time_array[5], iret;
/*---------------------------------------------------------------------*/

    /*
     *  "YYYYMMDD" textfield losing focus callback.
     */
    XtVaGetValues (wid, 
		   XmNvalue,	&ptext, 
		   NULL);
    sprintf ( result_time, "%s", ptext );
    if ( ptext ) XtFree (ptext);
    strcat (result_time, "/0000");

    timestr = &(result_time[2]);
    ti_ctoi( timestr, time_array, &iret, strlen(timestr) );

    if (iret < 0) {
            sprintf(cymd, "%04d%02d%02d", _curTimeArry[0], 
                    _curTimeArry[1], _curTimeArry[2]       );
            XmTextSetString( _eTime.ymd, cymd );
    }

}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_WCCymdTextCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwfmt_WCCymdTextCb							*
 *									*
 * Callback function for "YYYYMMDD" text widgets.			*
 *									*
 * void pgwfmt_WCCymdTextCb (wid, which, call)				*
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
 * E. Safford/SAIC	05/02	add checks to XtFrees			*
 ***********************************************************************/
{
    char	*ptext, *timestr, result_time[15], cymd[10];
    int         time_array[5], iret;
/*---------------------------------------------------------------------*/

    /*
     *  "YYYYMMDD" textfield losing focus callback.
     */
    XtVaGetValues (wid, 
		   XmNvalue,	&ptext, 
		   NULL);
    sprintf ( result_time, "%s", ptext );
    if ( ptext ) XtFree (ptext);
    strcat (result_time, "/0000");

    timestr = &(result_time[2]);
    ti_ctoi( timestr, time_array, &iret, strlen(timestr) );

    if (iret < 0) {
            sprintf(cymd, "%04d%02d%02d", _curTimeArry[0], 
                    _curTimeArry[1], _curTimeArry[2]       );
            XmTextSetString( _wWCCTimes[which].ymd, cymd );
    }

}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_includesCb ( Widget wid, long which, XEvent *event )
/************************************************************************
 * pgwfmt_includesCb							*
 *									*
 * Callback function for general include buttons.			*
 *									*
 * void pgwfmt_includesCb (wid, which, event)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	*event	XEvent		event callback structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/99	initial coding				*
 ***********************************************************************/
{
    XtPointer		userdata;
    struct incInfo	*incstruct;
/*---------------------------------------------------------------------*/

    XtVaGetValues (wid, XmNuserData, &userdata, NULL);
    incstruct = (struct incInfo *)userdata;
    incstruct[which].include = XmToggleButtonGetState (wid);
}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwfmt_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of county list	*
 * popup window.							*
 *									*
 * void pgwfmt_ctlBtnCb (wid, which, call)				*
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
 * C. Lin/EAI		12/97						*
 * S. Law/GSC		12/98	added call to pgwbxw_setDspLabel	*
 * S. Law/GSC		01/99	moved from pgwlst_ctlBtnCb		*
 * S. Law/GSC		03/99	added duration check			*
 * S. Law/GSC		04/99	changed popups/popdowns			*
 * H. Zeng/EAI          12/99   modified the watch duration error mesg. *
 * H. Zeng/EAI          12/99   modified for "Apply" button             *
 * H. Zeng/EAI          02/00   added warning message                   *
 * H. Zeng/EAI          04/00   changed cursor name                     *
 * M. Li/GSC		05/00	changed the place of type/time check;	*
 * 				added checks for watch # and forecaster	*
 * A. Hardy/GSC		05/00   added a warning for missing anchor pts. *
 * M. Li/SAIC		07/03	added a warning for the same watch	*
 *				number in repl. & continu. watches	*
 * R. Tian/SAIC		07/04	added a warning for watch in repl. and	*
 *				a confirm for watch already exists.	*
 * H. Zeng/SAIC		12/04	added call to pgwfmt_doContinue()	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* Continue */

        pgwfmt_doContinue ( FALSE, FALSE );
	break;

      case 1:	/* Apply */
        mcanvw_setCursor(CURS_BUSY);

        pgwfmt_formatSave (TRUE);
        pgwatch_restore (); 

        mcanvw_setCursor(CURS_DEFAULT); 
	break;

      case 2:	/* Cancel */
	pgwfmt_popdown ();
	pgprd_popdown ();

	pgwbxw_setDspLabel (0);
	break;

    } /* the end of switch(which) */

}

/*=====================================================================*/

void pgwfmt_doContinue ( Boolean overw_flag, Boolean dura_flag )
/************************************************************************
 * pgwfmt_doContinue							*
 *									*
 * Things to do for "OK" control button at the bottom of watch format	*
 * popup window.							*
 *									*
 * void pgwfmt_doContinue (overw_flag, dura_flag)			*
 *									*
 * Input parameters:							*
 *	overw_flag	Boolean	    confirmation of overwrite		*
 *	dura_flag	Boolean	    confirmation of duration		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		12/04	initial coding				*
 * T. Piper/SAIC	10/05	declared data long			*
 ***********************************************************************/
{
    char	etime[20], itime[20];
    char        mesg[512], mesg1[512], meswarn[128];
    char        str0[LSTR], *ptxt1, tmpstr[20];
    int		diff, ier, ipos, ii, jj, kk, nn, nn2, irwnm, icntn;
    int		ilst[10], ilst2[10], icwnum, cwinrl, cwincn, cwxist;
    long	data, flen;
/*---------------------------------------------------------------------*/

    if ( overw_flag && dura_flag ) {

         pgprd_popup ();
         return;
    }

    if ( overw_flag && !dura_flag ) {

         pgwfmt_getTime (itime, etime, &diff);

         if ( diff < (2 * 60) || (8 * 60) < diff ) {

	     /*
	      * If not between 2 and 8 hours, prompt
	      */
             sprintf(mesg, 
                     "The watch duration is %-2d hours - Is this correct?",
                     (diff+30)/60  );
             data = 1;
	     NxmConfirm_show(_pgwfmtWin, mesg, 
			     (XtCallbackProc)pgwfmt_cfmCb, NULL, 
			     (XtPointer) data, &ier);
         }
         else {

             pgprd_popup ();
         }

         return;

    }


    /*
     * Check for the same watch number in the replacement
     * and continuing lists.
     */
    nn = 0;
    nn2 = 0;
    ptxt1 = XmTextGetString(_rwnmTxtW);
    irwnm = 0;
    if ( ptxt1 != NULL )  sscanf (ptxt1 , "%d", &irwnm );
    if ( irwnm > 0 )  {
         cst_rxbl( ptxt1, ptxt1, &nn, &ier );
         cst_ilst( ptxt1, ' ', 0, sizeof(ilst)/sizeof(int), ilst, &nn, &ier );
    }
    if ( ptxt1 ) XtFree (ptxt1);

    ptxt1 = XmTextGetString(_cntnTxtW);
    icntn = 0;
    if ( ptxt1 != NULL )  sscanf (ptxt1 , "%d", &icntn);
    if ( icntn > 0 )  {
         cst_rxbl( ptxt1, ptxt1, &nn2, &ier );
         cst_ilst( ptxt1, ' ', 0, sizeof(ilst2)/sizeof(int), ilst2, &nn2, &ier );
    }
    if ( ptxt1 ) XtFree (ptxt1);

    kk = 0;
    if ( nn > 0 && nn2 > 0 ) {
         sprintf(mesg1, "The same watch number appears in both the \n");
         strcat (mesg1, "Replacement and Continuing Watch lists.\n");

         for ( ii = 0; ii < nn; ii++ ) {

               for ( jj = 0; jj < nn2; jj++) {

                    if ( ilst[ii] == ilst2[jj] && ilst[ii] != 0 ) {
                        sprintf(tmpstr, "Watch number %04d\n", ilst[ii]);
                        strcat(mesg1, tmpstr);
                        kk++;
                    }
	       }
	 }
    }

    icwnum = -1;
    cwinrl = G_FALSE;
    cwincn = G_FALSE;
    cwxist = G_FALSE;
    ptxt1 = XmTextGetString(_wnumTxtW);
    if ( ptxt1 != NULL )  sscanf (ptxt1 , "%d", &icwnum);
    if ( ptxt1 ) XtFree (ptxt1);

    if ( icwnum > 0 ) {
	 sprintf ( tmpstr, "ww%04d.vgf", icwnum );
	 cfl_inqr ( tmpstr, NULL, &flen, tmpstr, &ier ); 
	 if ( ier == 0 ) { cwxist = G_TRUE; }

	 if ( nn > 0 ) {
	        for ( ii = 0; ii < nn; ii++ ) {
	            if ( ilst[ii] == icwnum ) {
		        cwinrl = G_TRUE;
		    }
	        }
	 }

	 if ( nn2 > 0 ) {
	        for ( ii = 0; ii < nn2; ii++ ) {
	            if ( ilst2[ii] == icwnum ) {
		        cwincn = G_TRUE;
		    }
	        }
	 }

    }

    /*
     * Check the anchor point positions inside the watch.
     */
    pgwatch_gvert ( 0, str0);
    cst_srch ( 0, (int)strlen(str0), "---", str0, &ipos, &ier);

    /*
     * get valid time 
     */
    pgwfmt_getTime (itime, etime, &diff);


    /*
     * Do a series of check before continuing.
     */
    if ( ipos >= 0 ) {

	 sprintf( meswarn, 
		  "Anchor point not found in watch box. \n ");
	 strcat ( meswarn, "Resize watch to include an Anchor point.\n");
	 NxmWarn_show(_pgwfmtWin, meswarn );

    }
    else if ( kk > 0 ) {

	 NxmWarn_show(_pgwfmtWin, mesg1);

    } 
    else if ( cwinrl == G_TRUE ) {

	 sprintf ( mesg, "Watch Number %d in replacement list.",
		         icwnum );
	 NxmWarn_show(_pgwfmtWin, mesg);

    } 
    else if ( cwincn == G_TRUE ) {

	 sprintf ( mesg, "Watch Number %d in continuing watch list.",
		         icwnum );
	 NxmWarn_show(_pgwfmtWin, mesg);

    } 
    else {

         mcanvw_setCursor(CURS_BUSY);

         pgwfmt_formatSave (TRUE);
         ier = pgwatch_restore ();
  
         mcanvw_setCursor(CURS_DEFAULT);

         if( _wtypOpt == -1 || _tzoneOpt == -1 || 
             _validWnum == -1 || _fcstOpt == -1 ) {

	     sprintf(mesg,"The following problems have been identified:\n");
	     if( _wtypOpt == -1 )
	         strcat(mesg,"    Watch type is not selected.\n");
	     if( _tzoneOpt == -1 )
	         strcat(mesg,"    Time zone is not selected.\n");
	     if( _fcstOpt == -1 )
	         strcat(mesg,"    Forecaster name is not selected.\n");
	     if( _validWnum == -1 )
	         strcat(mesg,"    Watch number is invalid.\n");

             NxmWarn_show(_pgwfmtWin, mesg);

	 }
         else if ( cwxist == G_TRUE ) {

	     sprintf ( mesg, "Watch %d already exists. Overwrite?",
		             icwnum );
             data = 0;
	     NxmConfirm_show ( _pgwfmtWin, mesg, pgwfmt_cfmCb, NULL, 
                               (XtPointer) data, &ier );
         }
         else if ( diff < (2 * 60) || (8 * 60) < diff ) {

	     /*
	      * If not between 2 and 8 hours, prompt
	      */
             sprintf(mesg, 
                     "The watch duration is %-2d hours - Is this correct?",
                     (diff+30)/60  );
             data = 1;
	     NxmConfirm_show(_pgwfmtWin, mesg, 
			     (XtCallbackProc)pgwfmt_cfmCb, NULL, 
			     (XtPointer) data, &ier);

         }
         else {

             pgprd_popup ();

         }
    }


}

/*=====================================================================*/

/* ARGSUSED */
void pgwfmt_preIdPbCb ( Widget wid, long which, XmAnyCallbackStruct *cds )
/************************************************************************
 * pgwfmt_preIdPbCb							*
 *									*
 * This is the callback function for "preliminary watch id menu buttons	*
 * on the WCC window.                                                   *
 *									*
 * void pgwfmt_preIdPbCb ( wid, which, cds )			        *
 *									*
 * Input parameters:							*
 *	wid		Widget			widget ID		*
 *	which	        long			client data		*
 *	*cds	        XmAnyCallbackStruct	callback struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/SAIC      06/02   initial coding                          *
 * H. Zeng/EAI          07/02   changed parameter in cst_ncpy()         *
 ***********************************************************************/
{
    XmString	xmstr;
    char	*text;
    int		ier;
/*---------------------------------------------------------------------*/

    XtVaGetValues(_preIdPbW[which],
    		  XmNlabelString,                 &xmstr,
	          NULL);

    XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &text); 
    XmStringFree(xmstr);

    cst_ncpy ( _curPreId, text, sizeof(_curPreId)-1, &ier );
}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_phonePbCb ( Widget wid, long which, XmAnyCallbackStruct *cds )
/************************************************************************
 * pgwfmt_phonePbCb							*
 *									*
 * This is the callback function for "Phone Number" option menu buttons	*
 * of WCC window.                                                       *
 *									*
 * void pgwfmt_phonePbCb ( wid, which, cds )			        *
 *									*
 * Input parameters:							*
 *	wid		Widget			widget ID		*
 *	which	        long			client data		*
 *	*cds	        XmAnyCallbackStruct	callback struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          11/01   initial coding                          *
 * H. Zeng/XTRIA	06/03	added pass code info.			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    strcpy (_curPhoneNo,  _phoneInfo.range[which]);
    strcpy (_curPasscode, _passcodeInfo.range[which]);

}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_WCCctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwfmt_WCCctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of WCC popup	*
 * window.							        *
 *									*
 * void pgwfmt_WCCctlBtnCb (wid, which, call)				*
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
 * D.W.Plummer/NCEP	12/01	generate WCC text product		*
 * H. Zeng/EAI          12/01   added for actions for "Cancel"          *
 * T. Piper/SAIC	12/01	freed XmTextGetString			*
 * E. Safford/SAIC	05/02	add checks to XtFrees			*
 * E. Safford/SAIC	06/02	use MAX_WFO_LEN       			*
 * G. Grosshans/SPC	05/03	changed meet-me call time to current    *
 *				time plus 4 minutes			*
 * M. Li/SAIC		06/03	Added replacement watches		*
 * M. Li/SAIC		07/03	Removed duplicate wfo ids		*
 * M. Li/SAIC		07/03	Modify the format of wfolist		*
 * B. Yin/SAIC		03/04	Changed css_gtim calling sequences	*
 * A. Hardy/NCEP	04/04	Save type, time, fcstr and replace nos. *
 * A. Hardy/NCEP	05/04	Set defaults for stored wbx.info elms   *
 * H. Zeng/SAIC		11/05	added nearby wfo list info		*
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * T. Piper/SAIC	01/06	Properly wrap wfo lists			*
 * J. Wu/SAIC		04/06	added para newLineStr for cst_wrap 	*
 * T. Piper/SAIC	06/06	Increase wfo malloc to 4 * sizeof(char)	*
 * F. J. Yen/NCEP	03/07	Invoked er_wmsg if # counties > MAX_CNTY*
 ***********************************************************************/
{
    int	    fcst_info, itype, imind, irwnm, line_len = 66, ier, ierr;
    size_t  ii, jj;
    int	    yy, mm, dd, hh, nn, kk, idattim[5], imeetme[5];
    char    tstr[1024], textWCC[2048], fnameWCC[20], mesg[1024];
    char    *cptr, dattim[20], meetme[20], exptime[20], *tmp;
    int	    iaddm[] = { 6, 7, 3, 4, 5 }, ilst[10];
    char    month[][12] = { "JANUARY", "FEBRUARY", "MARCH", "APRIL",
                            "MAY", "JUNE", "JULY", "AUGUST",
                            "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER" };
    char    wfolist[MAX_WFO_LEN], wfolst[MAX_WFO_LEN], str[LSTR], str0[LSTR];
    char    wfostr[MAX_WFO_LEN], wfotmp[MAX_WFO_LEN];
    char    nby_wfo_lst[MAX_WFO_LEN];
    char    *ptext, *ptr, **wfo;
    char    my_replw[LSTR];
    char    fcstr[20];
    VG_DBStruct *el;
/*---------------------------------------------------------------------*/

    strcpy (my_replw, "\0");
    switch(which) {

      case 0:	/* Format WCC */

        /*
         * Check "Forecaster" textfield info.
         */
        ptext = XmTextGetString( _fcstWCCTxtW );
        if ( ptext == NULL ) { 
             fcst_info = 0;
        }    
        else {
	     jj = 0;
	     for (ii = 0; ii < strlen(ptext); ii++) {
	          if ( isalnum((unsigned long)ptext[ii]) ) jj++;
             }

             if ( jj > (size_t)0 ) {
                  fcst_info = 1;
	     }
             else {
                  fcst_info = 0;
             }
  
             if ( ptext ) {
		strcpy ( fcstr, ptext);
		XtFree (ptext);
	     }
        }
	
        /*
         * Popup Warning Window if some info. is missing.
         */
	if( _wtypWCCOpt == WTYP_DEFAULT || fcst_info == 0 ) {
	    sprintf(mesg,"The following problems have been identified:\n");
	    if( _wtypWCCOpt == WTYP_DEFAULT )
		strcat(mesg,"Watch type is not selected.\n");
	    if( fcst_info == 0 )
		strcat(mesg,"Forecaster name is blank or invalid.\n");
            NxmWarn_show(_pgwfmtWCCWin, mesg);

        }
        else {

	/*
	 *  Get UTC time.
	 */
	itype = 1;
	css_gtim ( &itype, dattim, &ier );
	sscanf ( dattim, "%2d%2d%2d/%2d%2d", &yy, &mm, &dd, &hh, &nn );

	textWCC[0] = '\0';

	/*
	sprintf(tstr, "NXXX80 KNCF %2d%02d%02d", dd, hh, nn );
	strcat ( tstr, NL );
	strcat ( textWCC, tstr );

	sprintf(tstr, "NIMNAT");
	strcat ( tstr, NL );
	strcat ( tstr, NL );
	strcat ( textWCC, tstr );
	*/

	/*
	 *  "Meet me" time should be rounded up to the next 5 minute interval
	 *  if minutes%5 is 0, 1 or 2; otw round up to the next 5 after.
	 */
	ti_ctoi ( dattim, idattim, &ier, strlen(dattim) );
	/*
	imind = 4 - ( nn % 5 );
	*/
	imind = 3;
	ti_addm ( idattim, &(iaddm[imind]), imeetme, &ier );
	ti_itoc ( imeetme, meetme, &ier, sizeof(meetme) );

        cptr = XmTextGetString (_wWCCTimes[1].ymd);
	strcpy ( exptime, cptr );
        if ( cptr ) XtFree (cptr);
	strcat ( exptime, "/" );
        cptr = XmTextGetString (_wWCCTimes[1].hhmm);
	strcat ( exptime, cptr );
        if ( cptr ) XtFree (cptr);
	ti_ctoi ( exptime, idattim, &ier, strlen(exptime) );

	sprintf(tstr, "THERE WILL BE A MEET-ME CONFERENCE CALL WITH THE STORM PREDICTION CENTER AT %02d%02d UTC %d %s %4d TO COORDINATE A POTENTIAL %s WATCH VALID THROUGH %02d%02d UTC %d %s %4d.", 
	    imeetme[3], imeetme[4], imeetme[2], month[imeetme[1]-1], imeetme[0],
	        _watchtypes[_wtypWCCOpt],
	    idattim[3], idattim[4], idattim[2], month[idattim[1]-1], idattim[0]);
	strcat ( tstr, NL );
	strcat ( tstr, NL );
	strcat ( textWCC, tstr );

	/*
	 *  WFO list
	 */
	sprintf(tstr, "THE FOLLOWING NATIONAL WEATHER SERVICE FORECAST OFFICES ARE NEEDED ON THE CONFERENCE CALL:");
	strcat ( tstr, NL );
	strcat ( tstr, NL );
	strcat ( textWCC, tstr );

	pgwlst_getWFO ( wfolist );
	cst_wrap( wfolist, "...", &line_len, "\n", (char *)NULL, tstr, &ier);
	strncpy( tstr, "   ", 3);
	strcat ( tstr, NL );
	strcat ( tstr, NL );
	strcat ( textWCC, tstr );

	/*
         * Check "Replacement Watch" textfield info.
         */
	ptext = XmTextGetString( _replTxtW );
	irwnm = 0;
	wfolst[0] = '\0';
	if ( ptext != NULL )  sscanf (ptext , "%d", &irwnm );
	if ( irwnm > 0 ) {
	    sprintf(tstr, "WATCHES PROPOSED FOR REPLACEMENT ARE: ");
	    strcat ( textWCC, tstr );

	    cst_rxbl( ptext, ptext, &nn, &ier );
	    cst_ilst( ptext, ' ', 0, sizeof(ilst)/sizeof(int), ilst, &nn, &ier );

	    str[0] = CHNULL;
	    for ( ii = 0; ii < (size_t)nn; ii++ )  {
		if ( ilst[ii] != 0 )  {
		    sprintf( str0, "%04d ", ilst[ii] );
		    strcat ( str, str0 );
		}
	    }
	    strcpy(my_replw, str);

	    sprintf(tstr, "%s", str);
	    strcat ( tstr, NL );
	    strcat ( tstr, NL );
	    strcat ( textWCC, tstr );

	    sprintf(tstr, "ADDITIONAL NATIONAL WEATHER SERVICE FORECAST OFFICES NEEDED ON THE CONFERENCE CALL AS A RESULT OF REPLACEMENT ARE:");
	    strcat ( tstr, NL );
	    strcat ( tstr, NL );
	    strcat ( textWCC, tstr );

	    sprintf(tstr, "%s", ptext);
	    gg_wwfo ( tstr, wfolst, &ier, strlen(tstr), sizeof(wfolst) );

	    if ( ier == 8 || ier == 7 ) {
	        er_wmsg ( "gg", &ier, NULL, &ierr, strlen("gg"), 0 );
                NxmErr_update();
	    }

	    if ( strlen(wfolst) > (size_t)3 ) {
		cst_wrap( wfolst, "...", &line_len, "\n", (char *)NULL, tstr, &ier);
		strncpy( tstr, "   ", 3);
		strcat ( tstr, NL );
		strcat ( tstr, NL );
		strcat ( textWCC, tstr );
	    }

	    el = (VG_DBStruct*) pgwatch_getCurElm();
	    sprintf(el->elem.wbx.info.w_replw, "%s", str );
	    if ( ptext ) XtFree (ptext);
	}

	/*
	 * Nearby WFO list.
	 */
	nby_wfo_lst[0] = '\0';
	for ( kk = 0; kk < _nNearbyWfo; kk++ )  {

	    if ( _nearbyWfos[kk].include == TRUE ) {
		strcat (nby_wfo_lst, "...");
		strcat (nby_wfo_lst, _nearbyWfos[kk].name);
	    }
	}

	if ( nby_wfo_lst[0] != '\0' ) {

	    strcpy (tstr, "THE FOLLOWING NWS WFOS NEAR THE PROPOSED ");
	    strcat (tstr, "WATCH AREA ARE BEING REQUESTED TO PARTICIPATE ");
	    strcat (tstr, "ON THE CONFERENCE CALL:");
	    strcat (tstr, NL );
            strcat (tstr, NL );
            strcat (textWCC, tstr );
	   
	    cst_wrap(nby_wfo_lst, "...", &line_len, "\n", (char *)NULL, tstr, &ier);
	    strncpy( tstr, "   ", 3);
	    strcat ( tstr, NL );
            strcat ( tstr, NL );
            strcat ( textWCC, tstr );
	}

	/*
	 *  Phone number
	 */
	sprintf(tstr, "PHONE NUMBER: %s", _curPhoneNo );
	strcat ( tstr, NL );
	strcat ( textWCC, tstr );

	sprintf(tstr, "PASSWORD:     %s", _curPasscode);
	strcat ( tstr, NL );
	strcat ( tstr, NL );
	strcat ( textWCC, tstr );

	sprintf(tstr, "IF PASSWORD IS NOT AVAILABLE, CONTACT SPC LEAD FORECASTER.");
	strcat ( tstr, NL );
	strcat ( tstr, NL );
	strcat ( textWCC, tstr );

	/*
	 *  ATTN line
	 */
	sprintf(tstr, "ATTN...WFO%s...WNAW...WNAR", wfolist );
	if ( strlen(wfolst) > (size_t)3 ) {
	    strcpy(wfostr, wfolst); 
	    cptr = strstr ( wfostr, "..." );
	    while ( cptr != (char *)NULL )  {
		cst_rpst ( wfostr, "...", " ", wfostr, &ier ); 
		cptr = strstr ( cptr+1, "..." );
	    }

	    cst_ldsp ( wfostr, wfostr, &nn, &ier );

	    /*
	     * Remove duplicate wfo ids.
	     */
	    wfo = (char **) malloc(sizeof(char *) * MAX_WFO_NUM); 
	    for ( ii = 0; ii < (size_t)MAX_WFO_NUM; ii++ ) 
		wfo[ii] = (char *) malloc(4 * sizeof(char));

	    cst_clst (wfostr, ' ', " ", MAX_WFO_NUM, MAX_REPLW_LEN, wfo, &nn, &ier);

	    if ( nn > 0 ) {
		wfotmp[0] = '\0'; 
	 	for (ii = 0; ii < (size_t)nn; ii++) {
		    ptr = strstr( wfolist, wfo[ii] );
		    if ( ! ptr ) { 
			strcat ( wfotmp, "..." );
                	strcat ( wfotmp, wfo[ii] );
		    }
		}
	    }

	    if ( strlen(wfotmp) > (size_t)3 ) strcat ( tstr, wfotmp ); 

	    for ( ii = 0; ii < (size_t)MAX_WFO_NUM; ii++ ) {
                free ( wfo[ii] ); 
	    }
	    free ( wfo );

	}
        if ( nby_wfo_lst[0] != '\0' )  strcat(tstr, nby_wfo_lst); 
	strcat ( tstr, NL );
	strcat ( tstr, NL );
	cst_wrap(tstr, "...", &line_len, NL, (char *)NULL, tstr, &ier);
	strcat ( textWCC, tstr );

	/*
	 *  Forecaster
	 */
	sprintf(tstr, "%s", tmp = XmTextGetString(_fcstWCCTxtW) );
	if ( tmp ) XtFree(tmp);
	strcat ( tstr, NL );
	strcat ( textWCC, tstr );

	cst_wrap ( textWCC, blank, &line_len, NL, (char *)NULL, textWCC, &ier );

        /*
         *  Replace temporary NL w/ official EOL.
         */
        cptr = strstr ( textWCC, NL );
        while ( cptr != (char *)NULL )  {
            cst_rpst ( cptr, NL, EOL, cptr, &ier );
            cptr = strstr ( cptr+strlen(EOL), NL );
        }

	    XmTextSetString (_newWCCText, textWCC);

	strcpy( fnameWCC, "KNCFNIMNAT" );
	    XmTextSetString (_svfileWCCText, fnameWCC);

            XtManageChild( _svfileWCCForm );

	}

       /*
        * Save Forecaster, watch type, expire time and replacement
	* number to the watch element.
	*/
	_elWatch.elem.wbx.info.w_type =_wtypWCCOpt;
	strcpy ( _elWatch.elem.wbx.info.w_exp_t, exptime);
	strcpy ( _elWatch.elem.wbx.info.w_fcstr, fcstr);
	strcpy ( _elWatch.elem.wbx.info.w_replw,  my_replw);

	break;

      case 1:	/* Cancel */   
	_elWatch.elem.wbx.info.w_type = WTYP_DEFAULT;
	strcpy ( _elWatch.elem.wbx.info.w_exp_t, " " );
	strcpy ( _elWatch.elem.wbx.info.w_fcstr, " " );
	strcpy ( _elWatch.elem.wbx.info.w_replw, " " );

        pgwfmt_popdownWCC();    
	mcanvw_setPressFunc((XtEventHandler)&pgevt_selectHdl, CURS_DEFAULT); 
	mbotw_mouseSet(LMHINT_MOVEPOINT, MMHINT_DONE);   

	break;

    } /* the end of switch(which) */

}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_WCCfilePbCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgwfmt_WCCfilePbCb							*
 *									*
 * Callback function for the control buttons of WCC save window.	*
 *									*
 * void pgwfmt_WCCfilePbCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		which button			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          11/01   initial coding                          *
 * D.W.Plummer/NCEP	12/01	add code to create launch script	*
 * H. Zeng/EAI          12/01   added more actions for "Save" button    *
 * T. Piper/SAIC	12/01	freed XmTextGetString			*
 * E. Safford/SAIC	05/02	add checks to XtFrees			*
 * E. Safford/SAIC	06/02	add check strncpy to wfolist[]		* 
 * E. Safford/SAIC	06/02	start WCL generation after save		* 
 * H. Zeng/SAIC		11/05	used do/while to eliminate EOLs		*
 ***********************************************************************/
{
    int		ier;
    char	*fnameWCC, *textWCC, dbl_eol[12];
    FILE	*fp;
    char	*cptr, *cptr2, *cptr3, *tmp, fnameLaunch[32];
    char	wfolist[MAX_WFO_LEN], comm[128], text[2048];
/*---------------------------------------------------------------------*/

    switch (clnt) {
      case 0:		/* Issue */
	fnameWCC = XmTextGetString (_svfileWCCText);
	fp = fopen (fnameWCC, "w");
	if ( fnameWCC ) XtFree (fnameWCC);

	textWCC = XmTextGetString (_newWCCText);
	cst_lcuc ( textWCC, textWCC, &ier );
	fputs (textWCC, fp);
	if ( textWCC ) XtFree (textWCC);

	fclose (fp);

        pgwfmt_popdownWCCsave();
   

	mcanvw_setPressFunc((XtEventHandler)&pgevt_selectHdl, CURS_DEFAULT); 
	mbotw_mouseSet(LMHINT_MOVEPOINT, MMHINT_DONE);   

	/*
	 *  Now save launch script.
	 */
	strcpy ( fnameLaunch, tmp = XmTextGetString(_svfileWCCText) );
	if ( tmp ) XtFree(tmp);
	strcpy ( text, tmp = XmTextGetString(_newWCCText) );
	if ( tmp ) XtFree(tmp);
	cst_lcuc ( text, text, &ier );

	strcat ( fnameLaunch, ".launch" );

	cptr = strstr ( text, "ATTN...WFO." );

	if ( cptr != (char *)NULL )  {

          sprintf(dbl_eol, "%s%s", EOL, EOL);

          do {
            cptr2 = strstr ( cptr, EOL);
	    cptr3 = strstr ( cptr, dbl_eol );
            cst_rpst ( cptr, EOL, "", cptr, &ier );
          }
          while ( cptr2 != cptr3 ); /* the condition that we are not reaching
				       the end of WFO list. */

          cptr2 = strstr ( cptr, EOL);
	  cptr2[0] = CHNULL;

	  wfolist[0] = '\0';
	  cptr = strstr ( cptr, "WFO." );
	  cptr += 3;
	  while ( cptr[0] == '.' )  cptr++;
	  while ( cptr[0] != CHNULL )  {
	      while ( cptr[0] != CHNULL && cptr[0] != '.' )  {
		  if ( strlen(wfolist) + 1 < 
		       sizeof(wfolist)/sizeof(wfolist[0]) - 1 ) {
		       strncat ( wfolist, cptr, 1 );
		  }
		cptr++;
	      }
	      strcat ( wfolist, "," );
	      while ( cptr[0] != CHNULL && cptr[0] == '.' )  {
		cptr++;
	      }
	  }
	  wfolist[strlen(wfolist)-1] = '\0';
	}
	else  {
	    strcpy ( wfolist, "NONE" );
	}

	strcpy ( comm, "#!/bin/csh\nlaunch_prod text_" );
	strcat ( comm, wfolist );
	strcat ( comm, " KNCFNIMNAT\n" );

	fp = fopen ( fnameLaunch, "w" );
	fputs ( comm, fp );
	fclose ( fp );

	sprintf( comm, "chmod 755 %s", fnameLaunch );
	system ( comm );

	/*
 	 *  Start the WCL window
	 */
	pgwfmt_popupWCL();

	break;

      case 1:		/* Cancel */
        if ( XtIsManaged(_svfileWCCForm) ) {
             XtUnmanageChild(_svfileWCCForm);
        }

	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_selectEh ( Widget wid, XtPointer clnt, XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgwfmt_selectEh							*
 *									*
 * Callback function for selecting watch boxes.				*
 *									*
 * void pgwfmt_selectEh (wid, clnt, event, ctdr )			*
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
 * S. Law/GSC		01/99	initial coding				*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    /*
     * No action if watch box is already selected
     */
    if (event->xbutton.button == Button1) {
	return;
    }

    pgwfmt_setWatch (NULL);
    mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT); 
    mbotw_mouseSet (LMHINT_SELECT, MMHINT_NOACTION);
    
}

/*=====================================================================*/
/* ARGSUSED */
void pgwfmt_WCCselectEh ( Widget wid, XtPointer clnt, XEvent *event, 
							Boolean *ctdr )
/************************************************************************
 * pgwfmt_WCCselectEh							*
 *									*
 * Callback function for selecting watch boxes when WCC window is on.	*
 *									*
 * void pgwfmt_WCCselectEh (wid, clnt, event, ctdr )			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer		client data				*
 *	*event	XEvent		event callback structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          12/01   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    /*
     * No action if watch box is already selected
     */
    if (event->xbutton.button == Button1) {
	return;
    }
    else if (event->xbutton.button == Button2) {
	pgevt_unsetOper (TRUE);
	if (pgpalw_getCurClassId() > 0) {
	    mbotw_mouseSet(LMHINT_SELECT, MMHINT_NOACTION);
	}
    }
    
}

/*=====================================================================*/

/* ARGSUSED */
static void pgwfmt_WCLctlBtnCb( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgwfmt_WCLctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of WCL popup	*
 * window.  Button 0, or Save writes the contents of the WCL window to  *
 * the designated WCL file.						*
 *									*
 * static void pgwfmt_WCLctlBtnCb (wid, which, call)			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	longt		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/SAIC	06/02	initial coding                          *
 ************************************************************************/
{
    FILE	*fp;
    int		ier, ierr;
    char	*textWCL, errStr[FILE_FULLSZ];
/*---------------------------------------------------------------------*/

    switch( which ) {

        case 0:
	    fp = cfl_wopn (_WCLfilnam, &ier);

	    if ( fp ) {
	        textWCL = XmTextGetString (_WCLTextW);
	        cst_lcuc ( textWCL, textWCL, &ier );

	        if ( textWCL ) {
	            fputs (textWCL, fp);
	            XtFree (textWCL);
	        }

	        fclose (fp);
	    }
	    else {
	        sprintf( errStr, "%s", _WCLfilnam );
	        er_wmsg ( "CFL", &ier, errStr, &ierr, 3, strlen(errStr) );
	        NxmErr_update( );
	    }

	    break;

	case 1:
	default:
	    break;
    }	

    pgwfmt_popdownWCL();
    pgwfmt_popdownWCC();

}

/*=====================================================================*/

Widget pgwfmt_createOptArea ( Widget parent, char *labelstr, 
			      int lbl_spc, int nopt, char opts[][20], 
			      WidgetList optw, int opt_spc, int nrow, 
			      XtPointer optvalp, XtCallbackProc callback )
/************************************************************************
 * pgwfmt_createOptArea							*
 *									*
 * This function creates a check box selection area for options.	*
 *									*
 * Widget pgwfmt_createOptArea ( parent, labelstr, lbl_spc, nopt, opts,	*
 *				optw, opt_spc, nrow, optvalp)		*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *	*labelstr	char	label for the selection area		*
 *	lbl_spc		int	spacing between the label and option box*
 *	nopt		int	number of options			*
 *	opts[][20]	char	option names				*
 *	optw		WidgetList option button widgets		*
 *	opt_spc		int	spacing between the options		*
 *	nrow		int	number of rows				*
 *	optvalp		XtPointer pointer to the current option		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgwfmt_createOptArea Widget	Widget ID of a form container widget 	*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		01/98						*
 * S. Law/GSC		01/99	moved from pgwlst_createOptArea		*
 * E. Safford/GSC	12/00	change optvalp type to XtPointer 	*
 * T. Piper		12/02	add call back				*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    Widget	form, label, optrc, button;
    int		*optval, pos;
    long	ii;
/*---------------------------------------------------------------------*/

    optval = (int *) optvalp;

    /*
     * create a form container 
     */
    form = XtVaCreateWidget("form", xmFormWidgetClass, parent, NULL);

    /*
     * create label 
     */
    if (nrow > 0) {
	pos = 20/nrow;
    }
    else {
	pos = 20;
    }

    label = XtVaCreateManagedWidget("label",
				    xmLabelWidgetClass,	form,
				    XmNtopAttachment,	XmATTACH_POSITION,
				    XmNtopPosition,	pos,
				    NULL);
    NxmLabel_setStr(label, labelstr);

    /*
     * create option check box 
     */
    if (lbl_spc == 0) {
	lbl_spc = 10;
    }

    if (opt_spc == 0) {
	opt_spc = 10;
    }

    optrc = XtVaCreateWidget("optrc",
			     xmRowColumnWidgetClass,	form,
			     XmNorientation,		XmHORIZONTAL,
			     XmNpacking,		XmPACK_COLUMN,
			     XmNnumColumns,		nrow,
			     XmNspacing,		opt_spc,
			     XmNradioBehavior,		False,
			     XmNtopAttachment,		XmATTACH_FORM,
			     XmNleftAttachment,		XmATTACH_WIDGET,
			     XmNleftWidget,		label,
			     XmNleftOffset,		lbl_spc,
			     NULL);

    for (ii = 0; ii < nopt; ii++) {

	button = XtVaCreateManagedWidget(opts[ii],
					 xmToggleButtonWidgetClass, optrc,
					 XmNuserData, optvalp,
					 NULL);

	XtAddCallback (button, XmNarmCallback,
		       (XtCallbackProc)callback,
		       (XtPointer)ii);

	if (ii == *optval) { 
	    XmToggleButtonSetState (button, True, True);
	}

	if (optw) optw[ii] = button;
    }

    XtManageChild (optrc);

    XtManageChild (form);

    return (form);
}

/*=====================================================================*/
 
Widget pgwfmt_createMenuText ( Widget parent, char *labelstr, int ncol, 
			       int textoff, int info_type, 
			       Widget *textwid, Widget *btnwid )
/************************************************************************
 * pgwfmt_createMenuText						*
 *									*
 * Creates a labeled text field widget with a pulldown menu.		*
 *									*
 * Widget pgwfmt_createMenuText ( parent, labelstr, ncol, textoff, 	*
 *					info_type, textwid, btnwid )	*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *	*labelstr	char	label string				*
 *	ncol		int	number of text columns			*
 *	textoff		int	offset between text and label		*
 *	info_type	int	type of information			*
 *									*
 * Output parameters:							*
 *	*textwid	Widget	text field widget			*
 *      *btnwid         Widget  option menu button widget               *
 *									*
 * Return parameters:							*
 * pgwfmt_createMenuText Widget	Widget ID of the form widget 		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/99	initial coding				*
 * H. Zeng/EAI          12/99   minor changes for exp. time "HHMM" box  *
 * H. Zeng/EAI          06/01   minor changes for ini. time "HHMM" box  *
 * M. Li/SAIC           12/01   arrow.xbm -> menu_arrow.xbm             *
 * H. Zeng/EAI          06/02   removed initial time input              *
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    Widget	form, label, menub, cascade, menu, button;
    int		iret, toff = 5;
    XmString	xmstr;
    Pixel	fg, bg;
    long	ii, ignore;
    char	filename[256];
    static Pixmap	menu_pxm;
    static Boolean	first = TRUE;
/*---------------------------------------------------------------------*/

    /*
     * create a row column container
     */
    form = XtVaCreateWidget("form",
			    xmFormWidgetClass,	parent,
			    XmNnumColumns,	1,
			    XmNorientation,	XmHORIZONTAL,
			    XmNradioBehavior,	FALSE,
			    XmNpacking,		XmPACK_TIGHT,
                            XmNborderWidth,     0,
                            XmNmarginHeight,    0,
                            XmNmarginWidth,     0,
			    NULL );

    /*
     * create label 
     */

    if ( info_type != EXPT ) {
         label = XtVaCreateManagedWidget ("tmlabel",
				    xmLabelWidgetClass,	form,
				    XmNtopAttachment,	XmATTACH_FORM,
				    XmNtopOffset,	toff,
				    NULL);
         NxmLabel_setStr(label, labelstr);
    }

    /*
     * create text field 
     */
    *textwid = XtVaCreateManagedWidget ("tmtext", 
					xmTextWidgetClass, form,
					XmNcolumns,		ncol,
                                        XmNleftAttachment,      XmATTACH_FORM,
                                        XmNuserData,            info_type,
					NULL);

    if( info_type != EXPT ) {
         XtVaSetValues(*textwid,
                       XmNleftAttachment,    XmATTACH_WIDGET,
                       XmNleftWidget,        label,
		       XmNleftOffset,	     textoff,
                       NULL  );
    
    }

    if ( (info_type != FCST) && (info_type !=EXPT) ) {
	XtVaSetValues (*textwid,  XmNvalue, _wfInfo[info_type].defelem, NULL);
    }


    if ( info_type == EXPT ) {
       ii = EXPIRATION_TIME ;
       XtAddCallback (*textwid, XmNlosingFocusCallback,
		   (XtCallbackProc) pgwfmt_menuTextCb,
		   (XtPointer) ii);
    }
    else {
       ii = TEXT_FIELD ;
       XtAddCallback (*textwid, XmNactivateCallback,
		   (XtCallbackProc) pgwfmt_menuTextCb,
		   (XtPointer) ii);
    }


    /*
     * create menu
     */
    menub = XmCreateMenuBar (form, "tmbar", NULL, 0);

    XtVaSetValues (menub, 
		   XmNleftAttachment,		XmATTACH_WIDGET,
		   XmNleftWidget,		*textwid,
		   XmNleftOffset,		0,
		   XmNmarginHeight,		0,
		   XmNmarginWidth,		0,
		   XmNborderWidth,		0,
		   XmNwidth,			5,
		   XmNhighlightThickness,	1,
		   XmNshadowThickness,		1,
		   NULL);

    menu  = XmCreatePulldownMenu (menub, "tmmenu", NULL, 0);

    cascade = XtVaCreateManagedWidget ("tmcascade", 
				       xmCascadeButtonWidgetClass, menub,
				       XmNsubMenuId, menu, 
				       NULL);

    if (first) {
	first = FALSE;

	XtVaGetValues (parent,
		       XmNforeground,	&fg,
		       XmNbackground, 	&bg,
		       NULL);

	cfl_inqr ("menu_arrow.xbm", "$NAWIPS/icons/nmap", &ignore, filename, &iret);

	if (iret == 0) {
	    menu_pxm = XmGetPixmap (XtScreen (parent), filename, fg, bg);
	}
	else {
	    menu_pxm = XmUNSPECIFIED_PIXMAP;
	}
    }

    if (menu_pxm == (Pixmap)XmUNSPECIFIED_PIXMAP) {
	xmstr = XmStringCreateLocalized ("\\/");

	XtVaSetValues (cascade,
		       XmNlabelString, xmstr,
		       NULL);

	XmStringFree (xmstr);
    }
    else {
	XtVaSetValues (cascade, 
		       XmNlabelType,		XmPIXMAP,
		       XmNlabelPixmap,		menu_pxm,
		       XmNmarginHeight,		0,
		       XmNmarginWidth,		0,
		       NULL);
    }

 
    for (ii = 0; ii < _wfInfo[info_type].nelems; ii++) {
	xmstr = XmStringCreateLocalized (_wfInfo[info_type].range[ii]);
	button = XtVaCreateManagedWidget ("tmbutton", 
					  xmPushButtonWidgetClass, menu, 
					  XmNlabelString,	xmstr,
					  XmNuserData,		*textwid,
					  NULL);
        if ( btnwid != NULL ) {
             *(btnwid+ii) = button;
        }

	XmStringFree (xmstr);

	XtAddCallback (button, XmNactivateCallback,
		       (XtCallbackProc) pgwfmt_menuTextCb,
		       (XtPointer) ii);

	
    }

    XtManageChild(menub);
    XtManageChild(form);
    return (form);
}

/*=====================================================================*/

Widget pgwfmt_createIncludes ( Widget parent, char *labelstr, int ncol, 
				int nbtns, struct incInfo *incstruct )
/************************************************************************
 * pgwfmt_createIncludes						*
 *									*
 * Creates a label widget with check box buttons.			*
 *									*
 * Widget pgwfmt_createIncludes (parent, labelstr, ncol, nbtns,		*
 *					incstruct)			*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *	*labelstr	char	label string				*
 *	ncol		int	number of buttons per row		*
 *	nbtns		int	offset between text and label		*
 *									*
 * Output parameters:							*
 *	*incstruct	struct incInfo	include information structure	*
 *									*
 * Return parameters:							*
 *	pgwfmt_createIncludes  Widget	Widget	ID of the form widget	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/99	initial coding				*
 * T. Piper/SAIC	10/05	declared ii  long			*
 ***********************************************************************/
{
    Widget	form, label, rowcol;
    int		nrow, toff = 5;
    long	ii; 
/*---------------------------------------------------------------------*/

    /*
     * create a row column container
     */
    form = XtVaCreateWidget("incform",
			    xmFormWidgetClass,	parent,
			    NULL );

    /*
     * create label 
     */
    label = XtVaCreateManagedWidget ("inclabel",
				     xmLabelWidgetClass,	form,
				     XmNtopAttachment,		XmATTACH_FORM,
				     XmNtopOffset,		toff,
				     NULL);
    NxmLabel_setStr(label, labelstr);


    /*
     * create button array 
     */
    nrow = (nbtns % ncol) ? (nbtns / ncol) + 1 : nbtns / ncol;
    rowcol = XtVaCreateManagedWidget ("incrowcol",
				      xmRowColumnWidgetClass,	form,
				      XmNtopAttachment,		XmATTACH_WIDGET,
				      XmNtopWidget,		label,
				      XmNrowColumnType,		XmWORK_AREA,
				      XmNradioAlwaysOne,	FALSE,
				      XmNorientation,		XmHORIZONTAL,
				      XmNpacking,		XmPACK_COLUMN,
				      XmNnumColumns,		nrow,
				      NULL);

    for (ii = 0; ii < nbtns; ii++) {
	incstruct[ii].wid = XtVaCreateWidget ("incbtn", 
					      xmToggleButtonWidgetClass,
					      rowcol,
					      XmNuserData,	incstruct,
					      NULL);

	XtAddCallback (incstruct[ii].wid, XmNvalueChangedCallback,
		       (XtCallbackProc) pgwfmt_includesCb,
		       (XtPointer) ii);
    }



    XtManageChild(form);
    return (form);
}

/*=====================================================================*/

void pgwfmt_rdInfo ( int *iret )
/************************************************************************
 * pgwfmt_rdInfo							*
 *									*
 * This function reads the forecasters table and the watch format       *
 * information table.  Each table is read in only once.			*
 *									*
 * void pgwfmt_rdInfo( iret )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret	int		Return value				*
 *                               -1 - Unable to open table      	*
 *				 -2 - Information missing		*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 3/99	Created					*
 * S. Law/GSC		03/99	added min/max inits and changed _wfInfo	*
 * D.W.Plummer/NCEP	 4/99	added adjacent area info processing	*
 * H. Zeng/EAI          12/99   added expiration time "HHMM" info.      *
 * R. Curtis/EAI         5/00   reads from modified forecasters table   *
 * H. Zeng/EAI          06/01   added initial time "HHMM" info.         *
 * H. Zeng/EAI          11/01   added info. from WCCphone.tbl           *
 * H. Zeng/EAI          06/02   removed initial time input              *
 * H. Zeng/XTRIA	06/03	added pass code info.			*
 * S. Jacobs/NCEP	 9/09	Increased number of forecasters to 50	*
 ***********************************************************************/
{
    int		ii, ityp, jtyp, pieces, ier, ier2;
    char	buff[256], fnm[32], cat[20], type[12], str[128]; 
    char	name[17],  number[256], code[256], *iptr;
    FILE    	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;


    /*
     *  Initialize the information structures.
     */
    for (ii = 0; ii < NINFO; ii++)  {
        strcpy ( _wfInfo[ii].defelem, "-" );
        _wfInfo[ii].nelems = 0;
    }

    strcpy ( _phoneInfo.defelem, "-" );
    _phoneInfo.nelems = 0;

    strcpy ( _passcodeInfo.defelem, "-" );
    _passcodeInfo.nelems = 0;


    /*
     *  Open the phone table. If not found, return an error.
     */
    strcpy(fnm, PHONE_TBL);
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

	if ( ier == 0 ) {

	    /*
             *  Clear memory and scan in phone no. and pass code.
             */
            memset(number, '\0', 256);
            memset(code,   '\0', 256);

	    pieces = sscanf(buff, "%s %s", number, code);
            if ( pieces == 2 ) {

		cst_ncpy ( _phoneInfo.range[_phoneInfo.nelems], number,
			   15, &ier2 );
		_phoneInfo.nelems++;

		cst_ncpy ( _passcodeInfo.range[_passcodeInfo.nelems], code,
			   15, &ier2 );
		_passcodeInfo.nelems++;
            }
	    else if ( pieces == 1 ) {

		cst_ncpy ( _phoneInfo.range[_phoneInfo.nelems], number,
			   15, &ier2 );
		_phoneInfo.nelems++;

		strcpy ( _passcodeInfo.range[_passcodeInfo.nelems], "-" );
		_passcodeInfo.nelems++;
            }
            else {

		*iret = -2;
		return;
            }

	}

    } /* the end of while( !feof...  */
    cfl_clos(fp, &ier);


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

	if ( ier == 0 ) {

	    /*
             *  Clear memory and scan in forecaster name
             */
            memset(name, '\0', 17);
	    sscanf(buff, "%s", name);

        	/*
		 *  Process entry for forecaster name.
		 */
		strcpy ( _wfInfo[FCST].range[_wfInfo[FCST].nelems], name );
		_wfInfo[FCST].nelems++;

	}

    } /* the end of while( !feof...  */
    cfl_clos(fp, &ier);


    /*
     *  Open the information table. If not found, return an error.
     */
    strcpy(fnm, WATCHINFO_TBL);
    fp = cfl_tbop(fnm, "nmap", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }


    /*
     * Set minimum and maximum values and the format type
     */
    _wfInfo[HAIL].min    = 0;
    _wfInfo[HAIL].max    = 5;
    _wfInfo[HAIL].format = TYPE_FLOAT;

    _wfInfo[GUST].min    = 40;
    _wfInfo[GUST].max    = 140;
    _wfInfo[GUST].format = TYPE_INT;

    _wfInfo[TOPS].min    = 200;
    _wfInfo[TOPS].max    = 800;
    _wfInfo[TOPS].format = TYPE_INT;

    _wfInfo[SMVD].min    = 0;
    _wfInfo[SMVD].max    = 360;
    _wfInfo[SMVD].format = TYPE_INT;

    _wfInfo[SMVS].min    = 0;
    _wfInfo[SMVS].max    = 70;
    _wfInfo[SMVS].format = TYPE_INT;

    _wfInfo[FCST].min    = 100;
    _wfInfo[FCST].max    = 0;
    _wfInfo[FCST].format = TYPE_CHAR;

    _wfInfo[AREA].min    = 100;
    _wfInfo[AREA].max    = 0;
    _wfInfo[AREA].format = TYPE_CHAR;

    _wfInfo[EXPT].min    = 0;
    _wfInfo[EXPT].max    = 2300;
    _wfInfo[EXPT].format = TYPE_INT;

    /*
     *  Scan table line-by-line.
     */
    while ( !feof(fp) )  {

	cfl_trln(fp, sizeof(buff), buff, &ier);

	if ( ier == 0 )  {

	    sscanf ( buff, "%s %s %s", cat, type, str );

	    if ( strcmp ( cat, "EXPIRATIONTIME" ) == 0 )  {

		/*
		 *  Process entry for expiration time.
		 */
		strcpy ( _wfInfo[EXPT].range[_wfInfo[EXPT].nelems], type );
		_wfInfo[EXPT].nelems++;

	    }
	    else if ( strcmp ( cat, "ADJ_AREA" ) == 0 )  {

		/*
		 *  Process entry for adjacent area.
		 */
		strcpy ( _wfInfo[AREA].range[_wfInfo[AREA].nelems], type );
		_wfInfo[AREA].nelems++;

	    }
	    else  {

		/*
		 *  Process generic type entries.
		 */
		ityp = IMISSD;
		if ( strcmp(cat,"HAIL") == 0 )  ityp = HAIL;
		if ( strcmp(cat,"GUST") == 0 )  ityp = GUST;
		if ( strcmp(cat,"TOPS") == 0 )  ityp = TOPS;
		if ( strcmp(cat,"SMVD") == 0 )  ityp = SMVD;
		if ( strcmp(cat,"SMVS") == 0 )  ityp = SMVS;

		if ( ityp != IMISSD )  {

		    if ( strcmp(type,"RANGE") == 0 )  {

		      /*
		       *  Process range list.
		       */
		      iptr = strtok( str, ";" );
		      while ( iptr != (char)NULL )  {
			jtyp = _wfInfo[ityp].nelems;
			strcpy(_wfInfo[ityp].range[jtyp],iptr);
		        _wfInfo[ityp].nelems++;
		        iptr = strtok( NULL, ";" );
		      }

		    }
		    else if ( strcmp(type,"DEFAULT") == 0 )  {
		      /*
		       *  Process default value.
		       */
		      strcpy ( _wfInfo[ityp].defelem, str );
		    }

		} /* the end of if ( ityp != IMISSD ) */

	    }

	} /* the end of if( ier == 0 */ 

    } /* the end of while ( !feof(fp)... */
    cfl_clos(fp, &ier);

}

/*=====================================================================*/

void pgwfmt_setCurTime ( void )
/************************************************************************
 * pgwfmt_setCurTime							*
 *									*
 * Sets the local global time array _curTimeArry and initial time       *
 * option menu.                                                         *
 *									*
 * void pgwfmt_setCurTime ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/99	initial coding				*
 * H. Zeng/EAI          06/01   modified for new initial time design    *
 * H. Zeng/EAI          06/02   removed initial time input              *
 ***********************************************************************/
{
    int         adjust_min, iret;
    struct tm	*utctime;
    time_t	tp;
/*---------------------------------------------------------------------*/

    tp = time (NULL);
    utctime = gmtime (&tp);

    _curTimeArry[0] = utctime->tm_year + 1900;
    _curTimeArry[1] = utctime->tm_mon + 1;
    _curTimeArry[2] = utctime->tm_mday;
    _curTimeArry[3] = utctime->tm_hour;
    _curTimeArry[4] = utctime->tm_min;

    /* 
     * Modified the system time to make it a reference initial time.
     */
    adjust_min = 5;
    ti_addm (_curTimeArry, &adjust_min, _curTimeArry, &iret);
    adjust_min = _curTimeArry[4] % 5;
    if (adjust_min <= 2) {
        ti_subm (_curTimeArry, &adjust_min, _curTimeArry, &iret);
    }
    else {
        adjust_min = 5 - adjust_min;
        ti_addm (_curTimeArry, &adjust_min, _curTimeArry, &iret);
    }

}

/*=====================================================================*/

void pgwfmt_getIssTmStr ( char *time_str )
/************************************************************************
 * pgwfmt_getIssTmStr							*
 *									*
 * Gets the "ISSUE TIME" string based on the current reference initial  *
 * time for Weather Watch text file.                                    *
 *									*
 * void pgwfmt_getIssTmStr ( time_str )			                *
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	time_str	char*   "ISSUE TIME" string			*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          06/02   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    sprintf( time_str, "%02d %02d %04d %02d%02d", _curTimeArry[1],
                                                  _curTimeArry[2],
                                                  _curTimeArry[0], 
                                                  _curTimeArry[3],
                                                  _curTimeArry[4]  );

}

/*=====================================================================*/

void pgwfmt_getTime ( char *itime, char *etime, int *diff )
/************************************************************************
 * pgwfmt_getTime							*
 *									*
 * Gets the current initial and expiration times and their diffence.	*
 *									*
 * void pgwfmt_getTime ( itime, etime, diff )				*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*itime	char[]		current intial time			*
 *	*etime	char[]		current expiration time			*
 *	*diff	int		the diffence of the two in minutes	*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/99	initial coding				*
 * T. Piper/GSC		12/99   fixed y2k problem			*
 * E. Safford/SAIC	05/02	add checks to XtFrees			*
 * H. Zeng/EAI          06/02   used reference initial time             *
 ***********************************************************************/
{
    int		iret;
    char	*temp;
/*---------------------------------------------------------------------*/

    sprintf( itime, "%04d%02d%02d/%02d%02d", _curTimeArry[0],
                                             _curTimeArry[1],
                                             _curTimeArry[2], 
                                             _curTimeArry[3],
                                             _curTimeArry[4]  );

    temp = XmTextGetString (_eTime.ymd);
    strcpy (etime, temp);
    if ( temp ) XtFree (temp);
    strcat (etime, "/");
    temp = XmTextGetString (_eTime.hhmm);
    strcat (etime, temp);
    if ( temp ) XtFree (temp);
    ti_diff (etime, itime, diff, &iret, strlen (etime), strlen (itime));

}

/*=====================================================================*/

void pgwfmt_getfname ( char *fname, int *iret )
/************************************************************************
 * pgwfmt_getfname                                                      *
 *                                                                      *
 * This function returns the filename of the watch text file.		*
 *                                                                      *
 * void pgwfmt_getfname ( fname, iret )                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *fname  char            Filename				*
 *      *iret   int             Return value                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      4/99   Created                                 *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = 0;

    strcpy( fname, _fName );
    
    return;

}

/*=====================================================================*/

void pgwfmt_unsetAdjareaBtns ( void )
/************************************************************************
 * pgwfmt_unsetAdjareaBtns						*
 *									*
 * This function unsets the current adjacent area buttons.              *
 *									*
 * void pgwfmt_unsetAdjareaBtns ()					*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/01   initial coding                          *
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    /*
     * unset current adjacent area buttons
     */
    for (ii = 0; ii < _nAdjArea; ii++) {
	XtVaSetValues (_adjAreaInc[ii].wid, XmNset, FALSE, NULL);
	_adjAreaInc[ii].include = FALSE;
    }


}

/*=====================================================================*/

void pgwfmt_adjareaInc ( char *includes )
/************************************************************************
 * pgwfmt_adjareaInc							*
 *									*
 * This function returns the current adjacent area string.		*
 *									*
 * void pgwfmt_adjareaInc (includes)					*
 *									*
 * Input parameters:                                                    *
 *			NONE						*
 *									*
 * Output parameters:                                                   *
 *	*includes	char		Adjacent area string	        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          12/99   initial coding                          *
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    includes[0] = '\0';
 
    /*
     *  Gather all adjacent area abbreviations.
     */
    for (ii = 0; ii < _nAdjArea; ii++)  {
	if (_adjAreaInc[ii].include)  {
	    strcat (includes, _adjAreaInc[ii].name);
            strcat (includes, " ");
	}
    }
 
    return;

}

/*=====================================================================*/

void pgwfmt_getcontWtch ( char **cntstr )
/************************************************************************
 * pgwfmt_getcontWtch							*
 *									*
 * This function returns the continuing watches string.			*
 *									*
 * The calling function is required to free the cntstr.          	*
 *									*
 * void pgwfmt_getcontWtch (cntstr)					*
 *									*
 * Input parameters:                                                    *
 *			NONE						*
 *									*
 * Output parameters:                                                   *
 *	**cntstr	char		Continuing watches string	*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		 5/00	Initial coding				*
 * E. Safford/SAIC	05/02	param change cntstr ** instead of *	*
 ***********************************************************************/
{
	int	nn, ier, icntn;
    	char 	*ptxt1;
/*---------------------------------------------------------------------*/

    /*
     *  Continuing watch string
     */
    ptxt1 = XmTextGetString(_cntnTxtW);
    icntn = 0;
    if ( ptxt1 != NULL )  sscanf (ptxt1 , "%d", &icntn );

    if ( icntn == 0 )  {
	*cntstr = ( char *) malloc ( (3) * sizeof(char) );
        sprintf(*cntstr, "%s\n", " " );
    }
    else  {
	
	cst_rxbl( ptxt1, ptxt1, &nn, &ier );

	*cntstr = ( char *) malloc ( (strlen(ptxt1) + 2) * sizeof(char) );
	strcpy (*cntstr, ptxt1);
	strcat (*cntstr, "\n");
    }
    
    if ( ptxt1 ) XtFree (ptxt1);

    return;

}

/*=====================================================================*/

/* ARGSUSED */
void pgwfmt_cfmCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgwfmt_cfmCb								*
 *									*
 * Callback function for confirm popup when watch already exists.	*
 *									*
 * void pgwfmt_cfmCb (wid, data, call)					*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	which confirmation			*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * R. Tian/SAIC		 7/04	initial coding                          *
 * H. Zeng/SAIC		12/04	overhauled for pgwfmt_doContinue()	*
 ************************************************************************/
{
    int which;
/*---------------------------------------------------------------------*/
 
    which = (long)clnt;

    switch ( which ) {

      case 0:	/* Overwrite Confirmation */

        pgwfmt_doContinue ( TRUE, FALSE );
	break;

      case 1:	/* Duration Confirmaion */

        pgwfmt_doContinue ( TRUE, TRUE  );
	break;

    } /* the end of switch(which) */
   
}

/*=====================================================================*/

/* ARGSUSED */
void pgwfmt_rwnmUpdateCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgwfmt_rwnmUpdateCb							*
 *									*
 * Callback function when "Replacement Watch Numbers" text widget loses	*
 * focus.								*
 *									*
 * void pgwfmt_rwnmUpdateCb (wid, clnt, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	not used				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		11/05	initial coding				*
 ************************************************************************/
{
/*---------------------------------------------------------------------*/
 
    /*
     * Update the Proposed WFOs, Replaced WFOs and Nearby WFOs lists.
     */
    pgwfmt_loadWfos ( );
   
}

/*=====================================================================*/

/* ARGSUSED */
static void pgwfmt_nearbyWfoCb ( Widget wid, long which, XEvent *event )
/************************************************************************
 * pgwfmt_nearbyWfoCb							*
 *									*
 * Callback function for nearby WFO toggle buttons.			*
 *									*
 * void pgwfmt_nearbyWfoCb (wid, which, event)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	*event	XEvent		event callback structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC         11/05   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _nearbyWfos[which].include = XmToggleButtonGetState (wid);
}

/*=====================================================================*/

void pgwfmt_loadWfos ( void )
/************************************************************************
 * pgwlst_loadWfos							*
 *									*
 * This function loads "Proposed WFOs", "Replaced WFOs" and "Nearby     *
 * WFOs" for Watch Coordination window when the window pops up.         *
 *									*
 * void pgwfmt_loadWfos ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC         11/05   initial coding                          *
 * F. J. Yen/NCEP	03/07	Invoked er_wmsg if # counties > MAX_CNTY*
 ***********************************************************************/
{
    char      wfo_list1[MAX_WFO_LEN], label_str[MAX_WFO_LEN+50];
    char      *ptext, tag_val[128], info[256], err_str[10];
    char      wfo_arry[MAX_WFO_NUM][12], wfo_list2[MAX_WFO_LEN];
    int       ier, ierr, len, shape, np, nwfo, nrow, ii;
    int	      split_pt, err_code, ignore;
    float     dist, *plat, *plon;
    XmString  xmstr;
/*---------------------------------------------------------------------*/

    if ( XtIsManaged(_wfoForm) )  XtUnmanageChild( _wfoForm );

    /*
     * Construct proposed WFOs label str.
     */
    pgwlst_getWFO (wfo_list1);

    strcpy ( label_str, "Proposed WFOs: ");
    strcat ( label_str, wfo_list1 );

    split_pt = 57;
    len = strlen( label_str );
    if ( len > split_pt ) {

        label_str[split_pt] =   '\n';		
        label_str[split_pt+1] = '\0';		
        strcat ( label_str, "      "     );
        strcat ( label_str, &(wfo_list1[split_pt - 15]) );
    }

    XtUnmanageChild (_propWfoLb);
    xmstr = XmStringCreateLtoR ( label_str, XmFONTLIST_DEFAULT_TAG );
    XtVaSetValues ( _propWfoLb,
    		    XmNalignment, XmALIGNMENT_BEGINNING,
    		    XmNlabelString, xmstr, 
		    NULL );
    XmStringFree ( xmstr );
    XtManageChild   (_propWfoLb); 

    /* 
     * Construct replaced WFOs label str.
     */
    wfo_list2[0] = '\0';
    ptext = NULL;

    ptext = XmTextGetString( _replTxtW );
    if ( ptext != NULL && ptext[0] != '\0' ) {  

        gg_wwfo (ptext, wfo_list2, &ier, strlen(ptext), sizeof(wfo_list2) );

	if ( ier == 8 || ier == 7 ) {
	    er_wmsg ( "gg", &ier, NULL, &ierr, strlen("gg"), 0 );
            NxmErr_update();
	}

        if ( ier!=0 || wfo_list2[0] == '\0' || 
	       strcmp(wfo_list2, "...") == 0 )   {

            strcpy (wfo_list2, "...NONE");
        }
    }
    else {

        strcpy (wfo_list2, "...NONE");
    }

    strcpy ( label_str, "Replaced  WFOs: ");
    strcat ( label_str, wfo_list2 );

    split_pt = 58;
    len = strlen( label_str );
    if ( len > split_pt ) {

        label_str[split_pt] =   '\n';		
        label_str[split_pt+1] = '\0';		
        strcat ( label_str, "      "     );
        strcat ( label_str, &(wfo_list2[split_pt - 16]) );
    }

    XtUnmanageChild (_repWfoLb);
    xmstr = XmStringCreateLtoR ( label_str, XmFONTLIST_DEFAULT_TAG );
    XtVaSetValues ( _repWfoLb,
    		    XmNalignment, XmALIGNMENT_BEGINNING,
    		    XmNlabelString, xmstr, 
		    NULL );
    XmStringFree ( xmstr );
    XtManageChild   (_repWfoLb); 

    if ( ptext != NULL )  XtFree ( ptext );

    /* 
     * Construct nearby WFOs label str.
     */
    ctb_pfstr ( "NEARBY_WFO_DIST", tag_val, &ier );
    if ( ier == 0 ) {
      if ( sscanf ( tag_val, "%f", &dist ) != 1 )  dist = 0.0F;
    }
    else {
      dist = 0.0F;
    }

    shape = _elWatch.elem.wbx.info.w_shape;
    np    = _elWatch.elem.wbx.info.numpts;
    G_MALLOC (plat, float, np, "pgwfmt_loadWfos plat");
    G_MALLOC (plon, float, np, "pgwfmt_loadWfos plon");

    for ( ii = 0; ii < np; ii++ ) {
      plat[ii] = _elWatch.elem.wbx.latlon[ii];
      plon[ii] = _elWatch.elem.wbx.latlon[np+ii];
    }

    pgwpts_expand ( dist, shape, plat, plon, &ier );
   
    /*
     * Get the WFO list from the polygon.
     */
    clo_binpoly( "CWA_BNDS", np, plat, plon, &ier );
    nwfo = clo_qnhot ();

    for (ii = 0; ii < MAX_WFO_NUM; ii++) wfo_arry[ii][0] = '\0';

    for ( ii = 0; ii < nwfo; ii++ )  {
      clo_bginfo ( "CWA_BNDS", ii, info, &ier );
      cst_gtag ( "WFO", info, "NONE", wfo_arry[ii], &ier );
    }

    /*
     * Remove all the WFOs that appear on proposed WFO & replaced WFO lists.
     */
    for ( ii = 0; ii < nwfo; ii++ )  {

      if ( strcasecmp(wfo_arry[ii], "NONE") == 0 )  continue;

      if ( strstr(wfo_list1, wfo_arry[ii]) != NULL || 
	   strstr(wfo_list2, wfo_arry[ii]) != NULL    ) {

	   strcpy ( wfo_arry[ii], "NONE" );
      }
    }

    /*
     * Free spaces for plat&plon.
     */
    G_FREE ( plat, float );
    G_FREE ( plon, float );


    /*
     * Unmanage the wrapping rowcolumn widget.
     * Unmanage all nearby WFO buttons on Watch Coordination window. 
     * Prepare for new nearby WFO buttons.
     */
    if( XtIsManaged(_nearbyWfoRC) )  XtUnmanageChild( _nearbyWfoRC );

    for (ii = 0; ii < MAX_NEARBY_WFO; ii++) {

	XtVaSetValues (_nearbyWfos[ii].wid, XmNset, FALSE, NULL);
        if( XtIsManaged(_nearbyWfos[ii].wid) ) {
	    XtUnmanageChild (_nearbyWfos[ii].wid);
        }
	_nearbyWfos[ii].include = FALSE;
    }

    _nNearbyWfo = 0;

    /*
     * Construct new nearby WFO buttons.
     */
    for ( ii = 0; ii < nwfo; ii++ )  {

      if ( strcasecmp(wfo_arry[ii], "NONE") == 0 )  continue;

      if ( _nNearbyWfo >= MAX_NEARBY_WFO ) {
        err_code = 9;
        sprintf (err_str, "%d", _nNearbyWfo);
        er_wmsg ("pgen", &err_code, err_str, &ignore, 4, strlen(err_str));
	NxmErr_update( );

        break;
      }

      strcpy (_nearbyWfos[_nNearbyWfo].name, wfo_arry[ii]);
      xmstr = XmStringCreateLocalized (_nearbyWfos[_nNearbyWfo].name);
      XtVaSetValues (_nearbyWfos[_nNearbyWfo].wid, 
		     XmNlabelString,	xmstr,
		     XmNset,		FALSE,
		     NULL);

      XmStringFree (xmstr);
      XtManageChild(_nearbyWfos[_nNearbyWfo].wid);
      _nNearbyWfo++;
    }

    /*
     * Set # of row for nearby WFO buttons.
     */
    if (_nNearbyWfo > 0) {
	nrow = (_nNearbyWfo % WFO_COL) ? (_nNearbyWfo / WFO_COL) + 1 : 
	        _nNearbyWfo / WFO_COL;

	XtVaSetValues (_nearbyWfoRC, 
		       XmNnumColumns, nrow, 
		       NULL);
    }

    /*
     * Manage the outer rowcolumn & form widget.
     */
    if ( _nNearbyWfo > 0 &&
	 ! XtIsManaged(_nearbyWfoRC) )  XtManageChild( _nearbyWfoRC );
    if ( ! XtIsManaged(_wfoForm) )      XtManageChild( _wfoForm );

}

/*=====================================================================*/
