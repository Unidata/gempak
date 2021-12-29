#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "pgcmn.h"

#define NL             "\n"     /* end-of-line for intermediate text product */
#define EOL            "\r\r\n" /* end-of-line for final text product        */

#define MAX_SEQNUM	300
#define MAXNSIGMET	  5
#define CIRCFACT	  4

#define	INTL_TBL	"sigmetinfo.tbl"
#define STIME_INTV	5		/* minutes to round start time */
#define ETIME_INTV	(4 * 60)	/* minutes between start and end time*/
#define FCSTTIM_INTV	360		/* additional minutes for forecast time*/

#define NO_VAL		"-9999"

#define	MAXNAREA		20
static	int			_nArea[MAXNSIGMET];
static	char			_area[MAXNSIGMET][MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_areaStrc;

#define MAXNIDS			20
static	int			_nIds[MAXNSIGMET];
static	char			_ids[MAXNSIGMET][MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_idsStrc;

#define MAXNPHEN		20
static	int			_nPhen[MAXNSIGMET];
static	char			_phen[MAXNSIGMET][MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_phenStrc;

static	int			_nPhen2[MAXNSIGMET];
static	char			_phen2[MAXNSIGMET][MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_phen2Strc;

static  int	_volcIdx;

#define MAXNSPD		20
static	int	_nSpd;
static	char	_spd[MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_spdStrc;

#define MAXNDIR		16
static	int	_nDir;
static	char	_dir[MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_dirStrc;

#define MAXNTREND	10
static	int	_nTrend[MAXNSIGMET];
static	char	_trend[MAXNSIGMET][MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_trendStrc;

#define MAXNREM		10
static	int	_nRem[MAXNSIGMET];
static	char	_rem[MAXNSIGMET][MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_remStrc;

#define MAXNFIR         20
#define MAXFIRSZ	32
static	int	_nFIR;
static	char	_FIR[MAXNOPT][MAXTBLNAME];
static	char	_FIRinfo[MAXNOPT][MAXTBLNAME];

static	struct	optMenuStrc	_solStrc;
static  struct  optMenuStrc     _levelInfoOp0,  _levelInfoOp1,
                                _levelInfoOp2;

static	Widget	_mainForm;
static	Widget	_seqnumText;
static	Widget	_fromText;
static	Widget	_fromScrollW;
static	Widget	_timesText[2];
static  Widget  _phennamText;
static  Widget  _phenMenuB;
static  Widget  _phenLatLon[2];
static  Widget  _phLatLab;
static  Widget  _phLonLab;
static  Widget  _freetxtLab;
static  Widget  _freetxtText;
static	Widget	_editAttrPb;
static	Widget	_creationForm;
static	Widget	_basicEditForm;
static	Widget	_intlEditForm;
static  Widget  _firstPhenomLabel;
static  Widget  _extendedAttribForm;
static  Widget  _basicAttribForm;
static  Widget  _basicAttrib2Form;
static  Widget  _basicAttrib3Form;
static  Widget  _intlEdit2Form;
static	Widget	_statusPb[3];
static	Widget	_ctlBtnRc;
static	Widget	_newsigText;
static	Widget	_svfileForm;
static	Widget	_svfileText;
static	Widget	_colorPb;
static	Widget	_typePb[3];
static	Widget	_distText;
static	Widget	_distForm;
static	Widget	_movePb[2];
static	Widget	_presLab;
static	Widget	_presHPA;
static	Widget	_maxwndLab;
static	Widget	_maxWinds;
static	Widget	_levelInfoText0;
static  Widget  _levelInfoText1;
static	Widget	_formatPb[3];
/*
static	Widget	_stateForm;
static	Widget	_stateList;
*/
static	int	_vgType		= SIGINTL_ELM;
static	int	_subType	= SIGTYP_AREA;
static	int	_origSubType	= SIGTYP_AREA;
static	int	_vgLoc		= 2;
static	int	_attrColor	= 4;
static	float	_currDist	= 10.0F;
static	int	_currStatus	= 0;
static	int	_currSequence	= 0;
static	int	_currPres;
static	int	_currMaxWind;
static	char	_currLatLon[2][MAX_LATSTR];
static	int	_currTimes[2];
static	int	_currMove	= 0;
static  int     _fmtflag        = 0;
static 	int	_currPts	= 1;
static	float	_currLat[MAX_SEQNUM];
static	float	_currLon[MAX_SEQNUM];
static	char	_currPhenam[MAX_PHENMSTR];
static	char	_currFreeText[MAX_FTEXTSTR];
static  char	_currST[151] = "";
static  char	_currFIR[64] = "";
static  int     _AFOSflag       = G_FALSE;
static	int 	_maxType	= 3;
static	int	one		= 1;

static	XtCallbackProc _editCbFunc;	


/*
 *  private functions -- callback
 */
void    pgsigw_arrowCb	    ( Widget, long, XtPointer );
void    pgsigw_ctlPbCb      ( Widget, long, XtPointer );
void    pgsigw_formatCb	    ( Widget, long, XtPointer );
void    pgsigw_distanceCb   ( Widget, XtPointer, XtPointer );
void    pgsigw_editPbCb     ( Widget, XtPointer, XtPointer );
void    pgsigw_levelInfo0Cb ( Widget, XtPointer, XtPointer );
void    pgsigw_levelInfo1Cb ( Widget, XtPointer, XtPointer );
void    pgsigw_levelInfo2Cb ( Widget, XtPointer, XtPointer );
void    pgsigw_maxwndCb     ( Widget, XtPointer, XtPointer );
void    pgsigw_phen2PbCb    ( Widget, long, XtPointer );
void    pgsigw_phenllCb	    ( Widget, long, XtPointer );
void    pgsigw_phennamCb    ( Widget, XtPointer, XtPointer );
void    pgsigw_phenPbCb     ( Widget, long, XtPointer );
void    pgsigw_presCb	    ( Widget, XtPointer, XtPointer );
void    pgsigw_sequenceCb   ( Widget, XtPointer, XtPointer );
void    pgsigw_statusTbCb   ( Widget, long, XtPointer );
void    pgsigw_svfilePbCb   ( Widget, long, XtPointer );
void    pgsigw_timesCb 	    ( Widget, long, XtPointer );
void    pgsigw_typeCb	    ( Widget, long, XtPointer );
void    pgsigw_validTimeCb  ( Widget, long, XtPointer );
void    pgsigw_volcPbCb     ( Widget, long, XtPointer );

/*
 * private functions -- query
 */
void    pgsigw_getFname ( char *fname );
void    pgsigw_getSIGMET ( char *sigmet );

/*
 * private functions -- action
 */
void    pgsigw_fillStrArray ( int max_strarr, int max_string, char origstr[],
				int *nstrarr, char strarr[][MAXTBLNAME] );
void    pgsigw_rdInfo ( int *iret );
void    pgsigw_setFIRs ( int np, float *ilat, float *ilon );
void    pgsigw_setState ( int np, float *ilat, float *ilon );
void    pgsigw_setTable ( void );
void    pgsigw_setVgInfo ( void );
void    pgsigw_setAFOSflg ( int *iret );


/************************************************************************
 * nmap_pgsigw.c							*
 *									*
 * This module defines everything for SIGMETs formatting.		*
 *									*
 * CONTENTS:								*
 *	pgsigw_create()		creates the popup window		*
 *	pgsigw_popup()		manages the popup window		*
 *	pgsigw_popdown()	unmanages the popup window		*
 *	pgsigw_setAttr()	sets the attibutes			*
 *	pgsigw_setFrom()	sets the from line			*
 *									*
 *	pgsigw_getAttr()	get the current attributes		*
 *	pgsigw_isUp()		query whether the window is up		*
 *	pgsigw_getSubType()	returns the subtype			*
 *	pgsigw_getStrings()	gets the area and id strings		*
 *									*
 *	pgsigw_areaPbCb()	callback for area menu buttons		*
 *	pgsigw_idsPbCb()	callback for id menu buttons		*
 *	pgsigw_arrowCb()	callback for arrow buttons		*
 *	pgsigw_typeCb()		callback for type toggle buttons	*
 *	pgsigw_solCb()		callback for SOL menu buttons		*
 *	pgsigw_distanceCb()	callback for distance text		*
 *      pgsigw_sequenceCb()     callback for sequence text              *
 *	pgsigw_editPbCb()	callback for edit attribute button	*
 *	pgsigw_statusTbCb()	callback for status buttons		*
 *	pgsigw_phenllCb()	callback for phenomenon lat/lon widgets	*
 *	pgsigw_timesCb()	callback for time text widgets		*
 *      pgsigw_validTimeCb()    callback for start plus push buttons    *
 *	pgsigw_phenPbCb()	callback for phenomenon menu buttons	*
 *      pgsigw_phen2PbCb()      callback for second phenom. buttons.    *
 *	pgsigw_volcPbCb()	callback for volcanic ash menu buttons	*
 *	pgsigw_ctlPbCb()	callback for control buttons		*
 *	pgsigw_svfilePbCb()	callback for save file control buttons	*
 *      pgsigw_formatCb()       callback for latitudes and longitudes   *
 *      pgsigw_levelInfo0Cb()   callback for level info. option menu 0  *
 *      pgsigw_levelInfo1Cb()   callback for level info. option menu 1  *
 *      pgsigw_levelInfo2Cb()   callback for level info. option menu 2  *
 *      pgsigw_phennamCb()	callback for entered phenomenon name	*
 *	pgsigw_presCb()		callback for pressure			*
 *	pgsigw_maxwndCb()	callback for max winds.			*
 *									*
 *	pgsigw_getSIGMET()	gets the new SIGMET string		*
 *	pgsigw_getFname()	gets the default file name		*
 *	pgsigw_getState()	gets the States list			*
 *	pgsigw_getFIRs() 	gets the FIR list   			*
 *									*
 *	pgsigw_rdInfo()		reads the intrnl SIGMET info table	*
 *	pgsigw_fillStrArray()	fills an array of strings		*
 *	pgsigw_setVgInfo()	sets various info based on _vgType	*
 *	pgsigw_setFIRs()	sets the FIR buttons			*
 *	pgsigw_setTable		sets the CES table values		*
 *	pgsigw_setState()	sets the States list			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	10/01	Changed format variables for ordinate   *
 * F. J. Yen/NCEP	 1/04	Increased dimension of _formatPb for VOR*
 * J. Lewis/AWC		05/07	Removd function pgsigw_getFIRstr	*
 * J. Lewis/AWC		05/07	Removd Widget _FIRtbW			*
 * J. Lewis/AWC		05/07	Added _currFIR				*
 * J. Lewis/AWC		05/07	Changed call sequence for pgsigw_setFIRs*
 * J. Lewis/AWC		05/07	Added function pgsigw_getFIRs()		*
 * J. Lewis/AWC         09/07   Changed OUTLKTIM_INTV to FCSTTIM_INTV   *
 * J. Lewis/AWC         09/07   Changed _fmtflag default                 *
 ***********************************************************************/

/*=====================================================================*/

void pgsigw_create ( Widget parent )
/************************************************************************
 * pgsigw_create							*
 *									*
 * This function creates an international sigmets attribute selection	*
 * box.									*
 *									*
 * void pgsigw_create (parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	initial coding				*
 * E. Safford/GSC	08/99	make prev sigm font match from line,    *
 *				  leave edit vtn unmgd, remove apply,	*
 *				  add sash width & height of 1		*
 * S. Law/GSC		09/99	changed to use _currDist for _distText	*
 * S. Law/GSC		09/99	added more widgets and cleanedup	*
 * H. Zeng/EAI          10/99   fixed a small prob. in sigmet edit box  *
 * H. Zeng/EAI          11/99   added start plus push buttons           *
 * A. Hardy/GSC         12/99   added lat/lon conversion buttons        *
 * H. Zeng/EAI          01/00   changed "Tops" to "LEVEL INFO"          *
 * S. Law/GSC		04/00	pgsigw_setOptMenu -> pgutls_setOptMenu	*
 * M. Li/GSC		05/00	added State Included widget		*
 * M. Li/GSC		05/00	Removed State Included widget		*
 * F. J. Yen/NCEP	08/00	Added volcanic ash			*
 * F. J. Yen/NCEP	09/00	Added tropical cyclone			*
 * H. Zeng/EAI          10/00   Rearranged attribute ordering           *
 * H. Zeng/EAI          11/00   Removed "Level Info" label              *
 * E. Safford/GSC	12/00	Add XtCallbackProc cast			*
 * D.W.Plummer/NCEP	10/01	Changed format variables for ordinate	*
 * M. Li/SAIC		12/01	arrow.xbm -> menu_arrow.xbm		*
 * T. Piper/SAIC	12/01	freed flentry				*
 * J. Wu/SAIC		05/02	verify width/sequence/from/to fields	*
 * H. Zeng/XTRIA        11/02   modified for NxmVolcano.c               *
 * M. Li/SAIC           12/02   radio box -> check box                  *
 * F. J. Yen/NCEP	 1/04	Added VOR to format_strs		*
 * E. Safford/SAIC	05/05	free fontlist				*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 * J. Lewis/AWC		05/07	removed FIR buttons from intl. edit pane*
 * J. Lewis/AWC		05/07	removed forecaster menu option		*
 ***********************************************************************/
{
    int		itemnum, mm, ier, toff = 15, boff = 8, voff = 5;
    long	ii, nn;
    char	start_dist[10],lbstr[20];
    char	*format_strs[] = {"new (prepend dir)", "old (postpend dir)",
				  "VOR"};
    char	*status_labels[] = {"New/Update", "Amend", "Cancel"};
    char	*ctlstrs[] = {"Save", "Apply", "Cancel"};
    char	*save_strs[] = {"Save", "Cancel"};
    char	*typstr[] = {"Area", "Line", "Isolated"};
    char	*solstr[] = {"ESOL", "NOF", "SOF", "EOF", "WOF"};
    char	*movestr[] = {"STNRY", "MVG"};
    char        *levelstr0[] = {"-none-", "FCST", "TOPS"};
    char        *levelstr1[] = {"TO", "ABV", "BLW", "BTN"};
    char        *levelstr2[] = {"-none-", "AND"};
    char	fontname[] = "-adobe-courier-bold-r-normal-*-*-120-*-*-m-*-*-*";
    Widget	label, pane, form1, form2, form3;
    Widget	rc1, rc2, button, button1, arrow_up, arrow_down;
    Widget	format1, phenom_form, phenom_label;
    XmString	xmstr;
    Display	*dsp;
    XmFontListEntry flentry;
    XmFontList	fontlist;
/*---------------------------------------------------------------------*/

    pgsigw_rdInfo (&ier);

    pgsigw_setAFOSflg ( &ier );

    _mainForm = XmCreateFormDialog (parent, "sigw_edit", NULL, 0);
    xmstr = XmStringCreateLocalized("SIGMET Edit");

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
    pane = (Widget) XtVaCreateManagedWidget ("sigw_pane",
			xmPanedWindowWidgetClass, 	_mainForm,
			XmNsashWidth,			1,
			XmNsashHeight,	 		1,
			XmNleftAttachment,  		XmATTACH_FORM,
			XmNrightAttachment, 		XmATTACH_FORM,
			NULL);
   
/**************************
 * creation pane area
 **************************/
    _creationForm = 
	(Widget) XtVaCreateManagedWidget ("sigw_createform",
					  xmFormWidgetClass,	pane,
					  XmNverticalSpacing,	voff,
					  NULL);


/*
 * subtype buttons
 */
    rc1  = XtVaCreateManagedWidget ("sigw_rowcol",
				    xmRowColumnWidgetClass,	_creationForm,
				    XmNradioBehavior,	False,
				    XmNorientation,	XmHORIZONTAL,
				    XmNpacking,		XmPACK_TIGHT,
				    NULL); 

    nn = XtNumber (typstr);
    _maxType = nn;
    for (ii = 0; ii < nn; ii++) {
	_typePb[ii] = XtVaCreateManagedWidget (typstr[ii],
					       xmToggleButtonWidgetClass, rc1,
					       XmNhighlightThickness, 0,
					       NULL);

	XtAddCallback (_typePb[ii], XmNarmCallback, 
		       (XtCallbackProc)pgsigw_typeCb, (XtPointer) ii);

	if (ii == SIGTYP_LINE) {
	    mm = XtNumber (solstr);
	    _solStrc.current = SIGLINE_ESOL;
	    pgutls_createOptionMenu (rc1, mm, (XtPointer)&_solStrc.current, 
				     "", NULL, &_solStrc.form, 
				     &_solStrc.label, &_solStrc.menu, 
				     _solStrc.pb, solstr);
	}
    }

/*
 * distance number
 */
    _distForm = 
	(Widget) XtVaCreateManagedWidget ("distnum_form",
					  xmFormWidgetClass,   _creationForm,
					  XmNleftAttachment,   XmATTACH_WIDGET,
					  XmNleftWidget,       rc1,
					  XmNbottomAttachment, XmATTACH_FORM,
					  NULL);
 
    label  = XtVaCreateManagedWidget ("Width:",
				      xmLabelWidgetClass,	_distForm,
				      XmNleftAttachment,	XmATTACH_FORM,
				      XmNbottomAttachment,      XmATTACH_FORM,
				      XmNbottomOffset,		boff,
				      NULL); 

    sprintf (start_dist, "%-6.2f", _currDist);
    _distText = 
	(Widget) XtVaCreateManagedWidget ("distance_text",
					  xmTextWidgetClass,	_distForm,
					  XmNcolumns,		5,
					  XmNvalue,		start_dist,
					  XmNleftAttachment,	XmATTACH_WIDGET,
					  XmNleftWidget,	label,
					  NULL);

/*
    XtAddCallback(_distText, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosFltCb, NULL);
*/
    XtAddCallback (_distText, XmNactivateCallback, 
		   (XtCallbackProc)pgsigw_distanceCb, (XtPointer) NULL);
    XtAddCallback (_distText, XmNlosingFocusCallback, 
		   (XtCallbackProc)pgsigw_distanceCb, (XtPointer) NULL);

/*
 * create color widget
 */
    form1 = XtVaCreateManagedWidget("sigclrfrm",
				     xmFormWidgetClass,	 _creationForm,
				     XmNleftAttachment,   XmATTACH_WIDGET,
				     XmNleftWidget,	  _distForm,
				     XmNbottomAttachment, XmATTACH_FORM,
				     NULL);

    label = XtVaCreateManagedWidget("Color:   ",
				    xmLabelWidgetClass,	form1,
				    XmNleftAttachment,	XmATTACH_FORM,
				    XmNbottomAttachment,XmATTACH_FORM,
				    XmNbottomOffset,	boff,
				    NULL);

    _colorPb = XtVaCreateManagedWidget(" ",
				      xmPushButtonWidgetClass,	form1,
				      XmNleftAttachment, XmATTACH_WIDGET,
				      XmNleftWidget,	 label,
				      XmNwidth,		 25,
				      XmNheight,	 20,
				      NULL);

    XtAddCallback(_colorPb, XmNactivateCallback, NxmClrW_popup, 
		  &_attrColor);


/**************************
 * basic edit pane area
 **************************/
    _basicEditForm = 
	(Widget) XtVaCreateManagedWidget ("sigw_beditform",
					  xmFormWidgetClass,	pane,
					  XmNverticalSpacing,	voff,
					  NULL);

/*
 * Create area menu
 */
    _areaStrc.current = 0;
    pgutls_createOptionMenu (_basicEditForm, MAXNOPT, 
			     (XtPointer)&_areaStrc.current, 
			     "Area:", NULL, &_areaStrc.form, 
			     &_areaStrc.label, &_areaStrc.menu, 
			     _areaStrc.pb, NULL);
 
    XtVaSetValues (_areaStrc.form, XmNleftAttachment, XmATTACH_FORM, NULL);

/*
 * Create id menu
 */
    _idsStrc.current = 0;
    pgutls_createOptionMenu (_basicEditForm, MAXNOPT, 
			     (XtPointer)&_idsStrc.current, 
			     "ID:", NULL, &_idsStrc.form, &_idsStrc.label,
			     &_idsStrc.menu, _idsStrc.pb, NULL);
 
    XtVaSetValues (_idsStrc.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	_areaStrc.form,
		   NULL);

/*
 * sequence number
 */
    form1 = 
	(Widget) XtVaCreateManagedWidget ("seqnum_form",
					  xmFormWidgetClass,  _basicEditForm,
					  XmNleftAttachment,  XmATTACH_WIDGET,
					  XmNleftWidget,      _idsStrc.form,
					  NULL);
 
    label  = XtVaCreateManagedWidget ("Sequence:",
				      xmLabelWidgetClass,	form1,
				      XmNleftAttachment,	XmATTACH_FORM,
				      NULL); 

    _seqnumText = 
	(Widget) XtVaCreateManagedWidget ("sequence_text",
					  xmTextWidgetClass,	form1,
					  XmNcolumns,		3,
					  XmNeditable,		TRUE,
					  XmNvalue,		"0",
					  XmNleftAttachment,	XmATTACH_WIDGET,
					  XmNleftWidget,	label,
					  NULL);

    XtAddCallback(_seqnumText, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_seqnumText,       XmNlosingFocusCallback, 
		   (XtCallbackProc)pgsigw_sequenceCb, (XtPointer) NULL);

    arrow_up = XtVaCreateManagedWidget("sigw_arrowup",
				       xmArrowButtonWidgetClass, form1,
				       XmNarrowDirection,   XmARROW_UP,
				       XmNtopAttachment,    XmATTACH_FORM,
				       XmNbottomAttachment, XmATTACH_FORM,
				       XmNleftAttachment,   XmATTACH_WIDGET,
				       XmNleftWidget,	    _seqnumText,
				       NULL);

    XtAddCallback (arrow_up, XmNactivateCallback, 
		   (XtCallbackProc)pgsigw_arrowCb, (XtPointer) 1);

    arrow_down = XtVaCreateManagedWidget("sigw_arrowdown",
					 xmArrowButtonWidgetClass, form1,
					 XmNarrowDirection,   XmARROW_DOWN,
					 XmNtopAttachment,    XmATTACH_FORM,
					 XmNbottomAttachment, XmATTACH_FORM,
					 XmNleftAttachment,   XmATTACH_WIDGET,
					 XmNleftWidget,       arrow_up,
					 NULL);

    XtAddCallback (arrow_down, XmNactivateCallback, 
		   (XtCallbackProc)pgsigw_arrowCb, (XtPointer) -1);


/*
 * latitude/longitude decimal or minutes
 */
    format1  = XtVaCreateManagedWidget ("format_form",
				   xmRowColumnWidgetClass,   _basicEditForm,	
				   XmNradioBehavior,	TRUE,
				   XmNorientation,	XmHORIZONTAL,
				   XmNpacking,		XmPACK_TIGHT,
				   XmNtopAttachment,	XmATTACH_WIDGET,
				   XmNtopWidget,        _idsStrc.form,
				   XmNleftAttachment,	XmATTACH_FORM,
				   NULL); 
    nn = XtNumber (format_strs);
    for (ii = 0; ii < nn; ii++) {
	_formatPb[ii] = XtVaCreateManagedWidget (format_strs[ii],
					       xmToggleButtonWidgetClass, format1,
					       XmNset, (_fmtflag == ii),
					       NULL);

    XtAddCallback (_formatPb[ii], XmNarmCallback, 
		   (XtCallbackProc)pgsigw_formatCb, (XtPointer) ii);
    }


/*
 * from line
 */
    label  = XtVaCreateManagedWidget ("From Line:",
				      xmLabelWidgetClass,	_basicEditForm,
				      XmNtopAttachment,	        XmATTACH_WIDGET,
				      XmNtopWidget,		_areaStrc.form,
				      NULL); 

    _fromScrollW = 
	(Widget) XtVaCreateManagedWidget ("from_scroll",
					  xmScrolledWindowWidgetClass,	
					  _basicEditForm,
					  XmNscrollingPolicy,	XmAUTOMATIC,
					  XmNheight,		50,
					  XmNtopAttachment,    XmATTACH_WIDGET,
					  XmNtopWidget,		label,
					  XmNtopOffset,		toff,
					  XmNbottomAttachment,	XmATTACH_FORM,
					  XmNleftAttachment,	XmATTACH_FORM,
					  XmNrightAttachment,	XmATTACH_FORM,
					  NULL);

/*  This is the new label widget option... */ 
     _fromText = 
        (Widget) XtVaCreateManagedWidget ("from_line",
					xmLabelWidgetClass,	_fromScrollW,
					XmNfontList,          fontlist,
					NULL);

/*
 * edit button
 */
    xmstr = XmStringCreateLocalized ("Edit Attributes");
    _editAttrPb = 
	(Widget) XtVaCreateManagedWidget ("edit_attr",
					  xmPushButtonWidgetClass, pane,
					  XmNlabelString,	xmstr,	
					  NULL);
    XmStringFree (xmstr);

    XtAddCallback (_editAttrPb, XmNactivateCallback, 
		   (XtCallbackProc)pgsigw_editPbCb, (XtPointer) NULL);

    XtUnmanageChild (_editAttrPb);


/****************************
 * States included		
 ***************************/
/*
    _stateForm = 
	(Widget) XtVaCreateManagedWidget ("sigw_stateform",
					  xmFormWidgetClass,	pane,
					  XmNverticalSpacing,	voff,
					  NULL);

    labelST = XtVaCreateManagedWidget ("States Included:",
                                xmLabelWidgetClass, _stateForm,
				XmNleftAttachment, XmATTACH_FORM,
                                NULL );

    xmstr = XmStringCreateLocalized(_currST);
    _stateList =  XtVaCreateManagedWidget (_currST,
                            	xmLabelWidgetClass,     _stateForm,
                                XmNtopAttachment,       XmATTACH_WIDGET,
				XmNtopWidget, 		labelST,
				XmNlabelString,           xmstr,
                                NULL);

    XmStringFree(xmstr);

    XtUnmanageChild(_stateForm);
*/
/*******************************
 * international edit pane area
 *******************************/
    _intlEditForm = 
	(Widget) XtVaCreateWidget ("sigw_ieditform",
				    xmFormWidgetClass,	pane,
				    XmNverticalSpacing,	voff,
				    NULL);

/*
 * status buttons
 */
    rc2 = XtVaCreateManagedWidget ("status_rc",
				   xmRowColumnWidgetClass, _intlEditForm,
				   XmNorientation,	XmHORIZONTAL,
				   XmNradioBehavior,	TRUE,
				   XmNpacking,		XmPACK_TIGHT,
				   XmNtopAttachment,	XmATTACH_WIDGET,
				   XmNtopWidget,	form1,
				   XmNleftAttachment,	XmATTACH_FORM,
				   NULL);

    nn = XtNumber (status_labels);
    for (ii = 0; ii < nn; ii++) {
	_statusPb[ii] = XtVaCreateManagedWidget (status_labels[ii],
						xmToggleButtonWidgetClass, rc2,
						XmNset,	(_currStatus == ii),
						NULL);

	XtAddCallback (_statusPb[ii], XmNarmCallback, 
		       (XtCallbackProc)pgsigw_statusTbCb, (XtPointer) ii);
    }

/*
 * valid times
 */
    form2 = 
	(Widget) XtVaCreateManagedWidget ("vtime_form",
					  xmFormWidgetClass, _intlEditForm,
					  XmNtopAttachment,   XmATTACH_WIDGET,
					  XmNtopWidget,	      rc2,
					  XmNleftAttachment,  XmATTACH_FORM,
					  NULL);

    xmstr = XmStringCreateLtoR("Valid\nfrom:", XmFONTLIST_DEFAULT_TAG);

    label  = XtVaCreateManagedWidget ("Valid\nfrom:",
				      xmLabelWidgetClass,  form2,
				      XmNleftAttachment,   XmATTACH_FORM,
                                      XmNlabelString,      xmstr,
				      NULL);
    XmStringFree(xmstr);

    _timesText[0]  = XtVaCreateManagedWidget ("start_text",
				     xmTextWidgetClass,   form2,
				     XmNcolumns,	  6,
				     XmNleftAttachment,   XmATTACH_WIDGET,
				     XmNleftWidget,	  label,
				     NULL); 

    XtAddCallback(_timesText[0], XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_timesText[0], XmNactivateCallback, 
		   (XtCallbackProc)pgsigw_timesCb, (XtPointer) 0);
    XtAddCallback (_timesText[0], XmNlosingFocusCallback, 
		   (XtCallbackProc)pgsigw_timesCb, (XtPointer) 0);

    label  = XtVaCreateManagedWidget ("to:",
				      xmLabelWidgetClass,  form2,
				      XmNleftAttachment,   XmATTACH_WIDGET,
				      XmNleftWidget,	   _timesText[0],
				      NULL); 

    _timesText[1]  = XtVaCreateManagedWidget ("end_text",
				     xmTextWidgetClass,   form2,
				     XmNcolumns,	  6,
				     XmNleftAttachment,   XmATTACH_WIDGET,
				     XmNleftWidget,	  label,
				     NULL); 

    XtAddCallback(_timesText[1], XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback (_timesText[1], XmNactivateCallback, 
		   (XtCallbackProc)pgsigw_timesCb, (XtPointer) 1);
    XtAddCallback (_timesText[1], XmNlosingFocusCallback, 
		   (XtCallbackProc)pgsigw_timesCb, (XtPointer) 1);

    xmstr = XmStringCreateLtoR("Start\nplus:", XmFONTLIST_DEFAULT_TAG);

    label  = XtVaCreateManagedWidget ("Start\nplus:",
				      xmLabelWidgetClass,  form2,
				      XmNleftAttachment,   XmATTACH_WIDGET,
				      XmNleftWidget,	   _timesText[1],
                                      XmNleftOffset,       5,
                                      XmNlabelString,      xmstr,
				      NULL);

    XmStringFree(xmstr);
  
    button = XtVaCreateManagedWidget ("4 hrs",
			       xmPushButtonWidgetClass, form2,
                               XmNtopAttachment,        XmATTACH_FORM,
                               XmNtopOffset,            4,
                               XmNleftAttachment,       XmATTACH_WIDGET,
                               XmNleftWidget,           label,
                               NULL   );

    XtAddCallback (button, XmNarmCallback, 
		       (XtCallbackProc)pgsigw_validTimeCb, (XtPointer)4);

    button1 = XtVaCreateManagedWidget ("6 hrs",
			       xmPushButtonWidgetClass, form2,
                               XmNtopAttachment,        XmATTACH_FORM,
                               XmNtopOffset,            4,
                               XmNleftAttachment,       XmATTACH_WIDGET,
                               XmNleftWidget,           button,
                               XmNleftOffset,           3,
                               NULL   );

    XtAddCallback (button1, XmNarmCallback, 
		       (XtCallbackProc)pgsigw_validTimeCb, (XtPointer)6);


/*
 * Create phenomenon menu
 */
    _phenStrc.current = 0;
    pgutls_createOptionMenu (_intlEditForm, MAXNOPT, 
			     (XtPointer)&_phenStrc.current, 
			     "Phenom:", (XtCallbackProc)pgsigw_phenPbCb, &_phenStrc.form,
			     &_phenStrc.label, &_phenStrc.menu, 
			     _phenStrc.pb, NULL);
 
    XtVaSetValues (_phenStrc.form, 
		   XmNtopAttachment,	XmATTACH_WIDGET, 
		   XmNtopWidget,	form2,
		   NULL);


/*******************************
 * extended attributes pane area
 *******************************/
    _extendedAttribForm = 
	(Widget) XtVaCreateWidget ("ext_attrib_form",
				    xmFormWidgetClass,	pane,
				    XmNverticalSpacing,	voff,
				    NULL);

/*
 * Phenomenon name and location area
 */
    xmstr = XmStringCreateLtoR("Select/Enter\nPhenom Name:", 
                               XmFONTLIST_DEFAULT_TAG );
    phenom_label = XtVaCreateManagedWidget ("Select/Enter\nPhenom Name:",
			       xmLabelWidgetClass,  _extendedAttribForm,
			       XmNtopAttachment,    XmATTACH_FORM,
                               XmNlabelString,      xmstr,
			       NULL); 
    XmStringFree(xmstr);

/*
 * create a form container for text menu
 */
    phenom_form = XtVaCreateWidget("form",
			    xmFormWidgetClass,	_extendedAttribForm,
                            XmNmarginWidth,     0,
			    NULL );

/*
 * create text field 
 */
    _phennamText = XtVaCreateManagedWidget ("phen_text",
			      xmTextWidgetClass,  phenom_form,
			      XmNcolumns,	  MAX_PHENMSTR,
			      XmNtopAttachment,	  XmATTACH_FORM,
			      XmNleftAttachment,  XmATTACH_FORM,
			      XmNleftOffset,	  0,
			      XmNmaxLength,	  MAX_PHENMSTR-1,
			      NULL); 

    _volcIdx = -1;
    XtVaSetValues (_phennamText, 
                   XmNvalue,                   "\0", 
                   XmNeditable,                TRUE,
                   XmNcursorPositionVisible,   TRUE,
                   NULL );

/*
 * create menu
 */
    _phenMenuB = NxmVolcano_menuCreate(phenom_form, _phennamText, 
                                       (XtCallbackProc)pgsigw_volcPbCb );


    XtVaSetValues (phenom_form, 
                   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                   XmNtopWidget,        phenom_label,
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	phenom_label,
                   XmNleftOffset,       0,
		   NULL);
    XtManageChild(phenom_form);

/*
 * Phenom Lat&Lon:
 */
    xmstr = XmStringCreateLtoR("Phenom\n  Lat:",XmFONTLIST_DEFAULT_TAG);

    _phLatLab  = XtVaCreateManagedWidget ("Phenom\n  Lat:",
				      xmLabelWidgetClass,  _extendedAttribForm,
				      XmNtopAttachment,	   XmATTACH_WIDGET,
				      XmNtopWidget,	   phenom_label,
				      XmNtopOffset,	   10,
                                      XmNlabelString,      xmstr,
				      NULL); 
    XmStringFree(xmstr);

    _phenLatLon[0] = XtVaCreateManagedWidget ("phen_lat",
				     xmTextWidgetClass,   _extendedAttribForm,
				     XmNcolumns,	  6,
				     XmNtopAttachment,	  XmATTACH_WIDGET,
				     XmNtopWidget,	  phenom_label,
				     XmNtopOffset,	  10,
				     XmNleftAttachment,   XmATTACH_WIDGET,
				     XmNleftWidget,	  _phLatLab,
				     XmNmaxLength,	  MAX_LATSTR-2,
				     NULL); 
    XtAddCallback (_phenLatLon[0], XmNactivateCallback,
		   (XtCallbackProc)pgsigw_phenllCb, (XtPointer) 0);
    XtAddCallback (_phenLatLon[0], XmNlosingFocusCallback,
		   (XtCallbackProc)pgsigw_phenllCb, (XtPointer) 0);

    xmstr = XmStringCreateLtoR("Phenom\n  Lon:",XmFONTLIST_DEFAULT_TAG);

    _phLonLab  = XtVaCreateManagedWidget ("Phenom\n  Lon:",
				      xmLabelWidgetClass,  _extendedAttribForm,
				      XmNtopAttachment,	   XmATTACH_WIDGET,
				      XmNtopWidget,	   phenom_label,
				      XmNtopOffset,	   10,
				      XmNleftAttachment,   XmATTACH_WIDGET,
				      XmNleftWidget,	   _phenLatLon[0],
				      XmNleftOffset,	   12,
                                      XmNlabelString,      xmstr,
				      NULL); 
    XmStringFree(xmstr);

    _phenLatLon[1] = XtVaCreateManagedWidget ("phen_lon",
				     xmTextWidgetClass,   _extendedAttribForm,
				     XmNcolumns,	  7,
				     XmNtopAttachment,	  XmATTACH_WIDGET,
				     XmNtopWidget,	  phenom_label,
				     XmNtopOffset,	  10,
				     XmNleftAttachment,   XmATTACH_WIDGET,
				     XmNleftWidget,	  _phLonLab,
				     XmNmaxLength,	  MAX_LONSTR-1,
				     NULL); 
    XtAddCallback (_phenLatLon[1], XmNactivateCallback,
		   (XtCallbackProc)pgsigw_phenllCb, (XtPointer) 1);
    XtAddCallback (_phenLatLon[1], XmNlosingFocusCallback,
		   (XtCallbackProc)pgsigw_phenllCb, (XtPointer) 1);

/*
 * Pressure
 */

    xmstr = XmStringCreateLtoR("Pressure\n  HPA:",XmFONTLIST_DEFAULT_TAG);

    _presLab = XtVaCreateManagedWidget ("Pres_Lab",
				      xmLabelWidgetClass,  _extendedAttribForm,
                                      XmNtopAttachment,    XmATTACH_WIDGET,
                                      XmNtopWidget,        phenom_label,
                                      XmNtopOffset,        10,
                                      XmNleftAttachment,   XmATTACH_WIDGET,
                                      XmNleftWidget,       _phenLatLon[1],
				      XmNleftOffset,	   12,
                                      XmNlabelString,      xmstr,
                                      NULL);
    XmStringFree(xmstr);

    _presHPA = XtVaCreateManagedWidget ("Pressure",
                                     xmTextWidgetClass,   _extendedAttribForm,
                                     XmNcolumns,          6,
                                     XmNtopAttachment,    XmATTACH_WIDGET,
                                     XmNtopWidget,        phenom_label,
                                     XmNtopOffset,        10,
                                     XmNleftAttachment,   XmATTACH_WIDGET,
                                     XmNleftWidget,       _presLab,
                                     XmNmaxLength,        5,
                                     NULL);

    XtAddCallback (_presHPA, XmNactivateCallback, 
		   (XtCallbackProc)pgsigw_presCb, (XtPointer) NULL);
    XtAddCallback (_presHPA, XmNlosingFocusCallback, 
		   (XtCallbackProc)pgsigw_presCb, (XtPointer) NULL);
/*
 * Max winds
 */
    xmstr = XmStringCreateLtoR("Max\nWinds:",XmFONTLIST_DEFAULT_TAG);

    _maxwndLab = XtVaCreateManagedWidget ("MaxWnds_Lab",
				      xmLabelWidgetClass,  _extendedAttribForm,
                                      XmNtopAttachment,    XmATTACH_WIDGET,
                                      XmNtopWidget,        phenom_label,
                                      XmNtopOffset,        10,
                                      XmNleftAttachment,   XmATTACH_WIDGET,
                                      XmNleftWidget,       _presHPA,
				      XmNleftOffset,	   12,
                                      XmNlabelString,      xmstr,
                                      NULL);
    XmStringFree(xmstr);

    _maxWinds = XtVaCreateManagedWidget ("Max_Winds",
                                     xmTextWidgetClass,   _extendedAttribForm,
                                     XmNcolumns,          6,
                                     XmNtopAttachment,    XmATTACH_WIDGET,
                                     XmNtopWidget,        phenom_label,
                                     XmNtopOffset,        10,
                                     XmNleftAttachment,   XmATTACH_WIDGET,
                                     XmNleftWidget,       _maxwndLab,
                                     XmNmaxLength,        5,
				     NULL);

    XtAddCallback (_maxWinds, XmNactivateCallback, 
		   (XtCallbackProc)pgsigw_maxwndCb, (XtPointer) NULL);
    XtAddCallback (_maxWinds, XmNlosingFocusCallback, 
		   (XtCallbackProc)pgsigw_maxwndCb, (XtPointer) NULL);
	

/*******************************
 * basic attributes pane area
 *******************************/
    _basicAttribForm = 
	(Widget) XtVaCreateWidget ("basic_attrib_form",
				    xmFormWidgetClass,	pane,
				    XmNverticalSpacing,	voff,
				    NULL);

/*
 * First Phenomenon Label
 */
    _firstPhenomLabel = XtVaCreateManagedWidget ("First Phenom Attributes:",
				      xmLabelWidgetClass,  _basicAttribForm,
				      XmNtopAttachment,	   XmATTACH_FORM,
				      XmNleftAttachment,   XmATTACH_FORM,
				      NULL); 

/*
 * movement widgets
 */
    form2 = 
	(Widget) XtVaCreateManagedWidget ("sigw_moveform",
					  xmFormWidgetClass, _basicAttribForm,
					  XmNtopAttachment,   XmATTACH_WIDGET,
                                          XmNtopWidget,      _firstPhenomLabel,
                                          XmNleftAttachment,  XmATTACH_FORM,
					  NULL);

    label  = XtVaCreateManagedWidget ("Movement:",
				      xmLabelWidgetClass,  form2,
				      XmNleftAttachment,   XmATTACH_FORM,
				      NULL); 

    rc1  = XtVaCreateManagedWidget ("sigw_mvrowcol",
				    xmRowColumnWidgetClass,	form2,
				    XmNradioBehavior,	TRUE,
				    XmNorientation,	XmHORIZONTAL,
				    XmNpacking,		XmPACK_TIGHT,
				    XmNleftAttachment,  XmATTACH_WIDGET,
				    XmNleftWidget,      label,
				    NULL); 

    nn = XtNumber (movestr);
    for (ii = 0; ii < nn; ii++) {
	_movePb[ii] = XtVaCreateManagedWidget (movestr[ii],
					       xmToggleButtonWidgetClass, rc1,
					       XmNhighlightThickness, 0,
					       XmNset,	(_currMove == ii),
					       XmNuserData, &_currMove,
					       NULL);

	XtAddCallback (_movePb[ii], XmNarmCallback, 
		       (XtCallbackProc)pgutls_optPbCb, (XtPointer) ii);
    }

    _spdStrc.current = 0;
    pgutls_createOptionMenu (form2, MAXNOPT, (XtPointer)&_spdStrc.current, 
			     "Speed:", NULL, &_spdStrc.form, 
			     &_spdStrc.label, &_spdStrc.menu, 
			     _spdStrc.pb, NULL);
 
    XtVaSetValues (_spdStrc.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	rc1,
		   NULL);

    pgutls_setOptMenu ("", _spd, _nSpd, &_spdStrc);

    _dirStrc.current = 0;

    strcpy(lbstr, "Direction\ntoward:");
    pgutls_createOptionMenu (form2, MAXNOPT, (XtPointer)&_dirStrc.current, 
			     lbstr,   NULL, 
                             &_dirStrc.form, &_dirStrc.label, 
                             &_dirStrc.menu, _dirStrc.pb, NULL);
 
    XtVaSetValues (_dirStrc.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
		   XmNleftWidget,	_spdStrc.form,
		   NULL);

    pgutls_setOptMenu ("", _dir, _nDir, &_dirStrc);

/*
 * Create trend menu
 */
    _trendStrc.current = 0;
    pgutls_createOptionMenu (_basicAttribForm, MAXNOPT, 
			     (XtPointer)&_trendStrc.current, 
			     "Trend:", NULL, &_trendStrc.form, 
			     &_trendStrc.label, &_trendStrc.menu, 
			     _trendStrc.pb, NULL);
 
    XtVaSetValues (_trendStrc.form, 
		   XmNtopAttachment,	XmATTACH_WIDGET, 
		   XmNtopWidget,	form2,
                   XmNtopOffset,        0,
		   NULL);


/*******************************
 * basic attributes 2 pane area
 *******************************/
    _basicAttrib2Form = 
	(Widget) XtVaCreateWidget ("basic_attrib_form",
				    xmFormWidgetClass,	pane,
				    XmNverticalSpacing,	0,
				    NULL);

/*
 * Create second phenomenon menu
 */
    _phen2Strc.current = 0;

    pgutls_createOptionMenu (_basicAttrib2Form, MAXNOPT, 
			     (XtPointer)&_phen2Strc.current, 
			     "Second Phenom:", (XtCallbackProc)pgsigw_phen2PbCb,
			     &_phen2Strc.form, &_phen2Strc.label,
			     &_phen2Strc.menu, _phen2Strc.pb, NULL);
 
    XtVaSetValues (_phen2Strc.form, 
		   XmNtopAttachment,	XmATTACH_FORM, 
	           XmNleftAttachment,   XmATTACH_FORM,
		   NULL);	


/*******************************
 * basic attributes 3 pane area
 *******************************/
    _basicAttrib3Form = 
	(Widget) XtVaCreateWidget ("basic_attrib_form",
				    xmFormWidgetClass,	pane,
				    XmNverticalSpacing,	voff,
				    NULL);

/*
 * level information menus and texts
 */
    form1 = 
	(Widget) XtVaCreateManagedWidget ("sigw_textform",
					  xmFormWidgetClass,  _basicAttrib3Form,
					  XmNtopAttachment,   XmATTACH_FORM,
                                          XmNtopOffset,       0,
					  NULL);

    label  = XtVaCreateManagedWidget ("Level Info:",
				      xmLabelWidgetClass,  form1,
				      XmNtopAttachment,	   XmATTACH_FORM,
				      XmNleftAttachment,   XmATTACH_FORM,
				      NULL); 

    form3 = 
	(Widget) XtVaCreateManagedWidget ("",
					  xmFormWidgetClass,  form1,
					  XmNtopAttachment,   XmATTACH_FORM,
					  XmNleftAttachment,  XmATTACH_WIDGET,
                                          XmNleftWidget,      label,
					  NULL);

    itemnum = XtNumber(levelstr0);

    pgutls_createOptionMenu (form3, itemnum, (XtPointer)&_levelInfoOp0.current, 
			     NULL, 
                             pgsigw_levelInfo0Cb, &_levelInfoOp0.form, 
                             &_levelInfoOp0.label,&_levelInfoOp0.menu, 
                             _levelInfoOp0.pb, levelstr0);
 
    XtVaSetValues (_levelInfoOp0.form, 
		   XmNleftAttachment,	XmATTACH_FORM, 
                   XmNleftOffset,       0,
		   NULL);

    itemnum = XtNumber(levelstr1);

    pgutls_createOptionMenu (form3, itemnum, 
			     (XtPointer)&_levelInfoOp1.current, NULL, 
                             pgsigw_levelInfo1Cb, &_levelInfoOp1.form, 
                             &_levelInfoOp1.label,&_levelInfoOp1.menu, 
                             _levelInfoOp1.pb, levelstr1);
 
    XtVaSetValues (_levelInfoOp1.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
                   XmNleftWidget,       _levelInfoOp0.form,
                   XmNleftOffset,       0,
		   NULL);

    _levelInfoText0 = XtVaCreateManagedWidget ("sigw_leveinfotext",
					 xmTextFieldWidgetClass, 
			                 (XtPointer)form3,
                                         XmNcolumns,             8,
					 XmNleftAttachment,      XmATTACH_WIDGET,
					 XmNleftWidget,          _levelInfoOp1.form,
					 NULL); 

    itemnum = XtNumber(levelstr2);

    pgutls_createOptionMenu (form3, itemnum, 
			     (XtPointer)&_levelInfoOp2.current, NULL, 
                             pgsigw_levelInfo2Cb, &_levelInfoOp2.form, 
                             &_levelInfoOp2.label,&_levelInfoOp2.menu, 
                             _levelInfoOp2.pb, levelstr2);
 
    XtVaSetValues (_levelInfoOp2.form, 
		   XmNleftAttachment,	XmATTACH_WIDGET, 
                   XmNleftWidget,       _levelInfoText0,
		   NULL);

   _levelInfoText1 = XtVaCreateManagedWidget ("sigw_leveinfotext",
					 xmTextFieldWidgetClass,  form3,
                                         XmNcolumns,              8,
					 XmNleftAttachment,       XmATTACH_WIDGET,
					 XmNleftWidget,           _levelInfoOp2.form,
					 NULL); 


/*******************************
 * international edit 2 pane area
 *******************************/
    _intlEdit2Form = 
	(Widget) XtVaCreateWidget ("sigw_iedit2form",
				    xmFormWidgetClass,	pane,
				    XmNverticalSpacing,	voff,
				    NULL);

/*
 * Create remarks menu
 */
    _remStrc.current = 0;
    pgutls_createOptionMenu (_intlEdit2Form, MAXNOPT, 
			     (XtPointer)&_remStrc.current, 
			     "Remarks:", NULL, &_remStrc.form, 
			     &_remStrc.label, &_remStrc.menu, 
			     _remStrc.pb, NULL);
 
    XtVaSetValues (_remStrc.form, 
		   XmNtopAttachment,	XmATTACH_FORM,
                   XmNleftAttachment,   XmATTACH_FORM,
		   NULL);

/*
 * free text for volcano ash
 */
    form3 = 
	(Widget) XtVaCreateManagedWidget ("sigw_moveform",
					  xmFormWidgetClass, _intlEdit2Form,
					  XmNtopAttachment,   XmATTACH_WIDGET,
					  XmNtopWidget,	      _remStrc.form,
					  NULL);

    _freetxtLab  = XtVaCreateManagedWidget ("Free text:",
				      xmLabelWidgetClass,  form3,
				      XmNtopAttachment,	   XmATTACH_FORM,
				      NULL); 

    _freetxtText = XtVaCreateManagedWidget ("free_text",
				     xmTextWidgetClass,   form3,
				     XmNcolumns,	  50,
				     XmNtopAttachment,	  XmATTACH_FORM,
				     XmNleftAttachment,   XmATTACH_WIDGET,
				     XmNleftWidget,	  _freetxtLab,
				     XmNeditMode,	  XmMULTI_LINE_EDIT,
				     XmNrows,		  3,
				     XmNmaxLength,	  MAX_FTEXTSTR-1,
				     XmNwordWrap,	  TRUE,
				     XmNscrollVertical,	  TRUE,
				     NULL); 

/*
 * control buttons
 */
    _ctlBtnRc = XtVaCreateManagedWidget ("control_rc",
					 xmRowColumnWidgetClass, pane,
					 XmNorientation,	XmHORIZONTAL,
					 XmNpacking,		XmPACK_COLUMN,
					 NULL);

    nn = XtNumber (ctlstrs);
    for (ii = 0; ii < nn; ii++) {
	button = XtVaCreateManagedWidget (ctlstrs[ii],
					  xmPushButtonWidgetClass, 
					  _ctlBtnRc, NULL);

	XtAddCallback (button, XmNactivateCallback, 
		       (XtCallbackProc)pgsigw_ctlPbCb, (XtPointer) ii);
    }


/**************************
 * Save file window
 **************************/
    _svfileForm = XmCreateFormDialog (parent, "sigw_svfile", NULL, 0);
    xmstr = XmStringCreateLocalized("SIGMET Save");

    XtVaSetValues(_svfileForm,
		  XmNnoResize,		TRUE,
		  XmNautoUnmanage,	FALSE,
		  XmNdialogTitle,	xmstr,
		  NULL);

    XmStringFree(xmstr);

    _newsigText = 
	(Widget) XtVaCreateManagedWidget ("svfile_text",
					  xmTextWidgetClass,	_svfileForm,
					  XmNeditMode,	XmMULTI_LINE_EDIT,
					  XmNcolumns,		80,
					  XmNrows,		20,
					  XmNfontList,		fontlist,
					  XmNtopAttachment,	XmATTACH_FORM,
					  XmNtopOffset,		toff,
					  XmNleftAttachment,	XmATTACH_FORM,
					  XmNrightAttachment,	XmATTACH_FORM,
					  NULL);

    _svfileText = 
	(Widget) XtVaCreateManagedWidget ("svfile_text",
					  xmTextWidgetClass,	_svfileForm,
					  XmNeditable,		FALSE,
					  XmNcolumns,		80,
					  XmNfontList,		fontlist,
					  XmNtopAttachment,	XmATTACH_WIDGET,
					  XmNtopWidget,		_newsigText,
					  XmNtopOffset,		toff,
					  XmNleftAttachment,	XmATTACH_FORM,
					  XmNrightAttachment,	XmATTACH_FORM,
					  NULL);

    rc1 = XtVaCreateManagedWidget ("svfile_rc",
				   xmRowColumnWidgetClass, _svfileForm,
				   XmNorientation,	XmHORIZONTAL,
				   XmNtopAttachment,	XmATTACH_WIDGET,
				   XmNtopWidget,	_svfileText,
				   XmNleftAttachment,	XmATTACH_FORM,
				   XmNrightAttachment,	XmATTACH_FORM,
				   NULL);

    nn = XtNumber (save_strs);
    for (ii = 0; ii < nn; ii++) {
	button = XtVaCreateManagedWidget (save_strs[ii],
					  xmPushButtonWidgetClass, 
					  rc1, NULL);

	XtAddCallback (button, XmNactivateCallback, 
		       (XtCallbackProc)pgsigw_svfilePbCb, (XtPointer) ii);
    }
    XmFontListFree( fontlist );
}

/*=====================================================================*/

void pgsigw_popup ( VG_DBStruct *el, XtCallbackProc callback )
/************************************************************************
 * pgsigw_popup								*
 *									*
 * This function manages the SIGMET popup.				*
 *									*
 * void pgsigw_popup (el, callback)					*
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
 * S. Law/GSC		07/99	initial coding				*
 * S. Law/GSC		09/99	added callback parameter		*
 * S. Law/GSC		09/99	augmented element			*
 * M. Li/GSC		05/00	added pgsigw_getState			*
 * M. Li/GSC		05/00	removed pgsigw_getState			*
 * F. J. Yen/NCEP	10/00	sensitize location widgets as needed	*
 * M. Li/SAIC           12/02   radio box -> check box                  *
 ***********************************************************************/
{
    int			obj_type, tmp, dummy, ier, len, ii;
    char		tempch[MAX_PHENMSTR];
    XmString		xmstr;
    VG_DBStruct		newel;
/*---------------------------------------------------------------------*/

    pgsigw_popdown ();

    if (el == NULL) {
	obj_type = pgpalw_getCurObjId();
	pgobj_getId (CLASS_SIGMETS, obj_type, &_vgType, &tmp, &dummy);
	newel.hdr.vg_class = CLASS_SIGMETS;
	newel.hdr.vg_type  = (char)_vgType;
	newel.elem.sig.info.subtype = _subType;

	pgsigw_setVgInfo ();

	for (ii = 0; ii < _maxType; ii++ ) {
	    if ( ii == _subType ) {
		XmToggleButtonSetState (_typePb[ii], TRUE, TRUE);
	    }
	    else {
		XmToggleButtonSetState (_typePb[ii], False, False);
	    }
	}
	cst_rxbl ( _phen[_vgLoc][_phenStrc.current], tempch, &len, &ier);
	if ( (_subType == SIGTYP_ISOL ) &&
		(strcmp (tempch, "TROPICAL_CYCLONE ") == 0 ) ) {
	    XtSetSensitive (_phenLatLon[0], FALSE);
	    XtSetSensitive (_phenLatLon[1], FALSE);
	}
	else {
	    XtSetSensitive (_phenLatLon[0], TRUE);
	    XtSetSensitive (_phenLatLon[1], TRUE);
	}
	pgsigw_typeCb (NULL, _subType, NULL);

	ces_get (_subType, &newel, &ier);

	_attrColor = newel.hdr.maj_col;
	XtVaSetValues(_colorPb,
		      XmNbackground,		NxmColrP_getColorPixel (_attrColor),
		      XmNtopShadowColor,	NxmColrP_getColorPixel (_attrColor),
		      XmNbottomShadowColor,	NxmColrP_getColorPixel (_attrColor),
		      NULL);

	_currSequence = 0;
    }
    else {
	if (callback != NULL) _editCbFunc = callback;

	pgsigw_setAttr (el);

	XtManageChild (_basicEditForm);	
	XtManageChild (_ctlBtnRc);

	switch (_vgType) {
	  case SIGINTL_ELM:
	    xmstr = XmStringCreateLocalized(" MWO:");
	    XtVaSetValues (_areaStrc.label, XmNlabelString, xmstr, NULL);
	    XmStringFree (xmstr);

	    XtManageChild (_editAttrPb);
	    break;
/*
	  case SIGCONV_ELM:
	    pgsigw_getState(el, _currST, &ier);
	    xmstr = XmStringCreateLocalized(_currST);
	    XtVaSetValues (_stateList, XmNlabelString, xmstr, NULL);
	    XmStringFree (xmstr);

	    XtManageChild (_stateForm);
	    break;
*/
	  default:
	    xmstr = XmStringCreateLocalized("Area:");
	    XtVaSetValues (_areaStrc.label, XmNlabelString, xmstr, NULL);
	    XmStringFree (xmstr);

	    break;
	}
    }
    XtManageChild (_mainForm);
}

/*=====================================================================*/

void pgsigw_popdown ( void )
/************************************************************************
 * pgsigw_popdown							*
 *									*
 * This function unmanages the SIGMET popup.				*
 *									*
 * void pgsigw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	initial coding				*
 * S. Law/GSC		09/99	removed format widget			*
 * H. Zeng/EAI          10/00   Rearranged the attribute ordering       *
 * J. Wu/SAIC		12/03	pop down color pallette			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

   if (XtIsManaged (_mainForm)) {
	NxmClrW_popdown();
	XtUnmanageChild (_mainForm);
    }

    if (XtIsManaged (_basicEditForm)) {
	XtUnmanageChild (_basicEditForm);
    }

    if (XtIsManaged (_editAttrPb)) {
	XtUnmanageChild (_editAttrPb);
    }

    if (XtIsManaged (_intlEditForm)) {
	XtUnmanageChild (_intlEditForm);
    }

    if (XtIsManaged (_extendedAttribForm)) {
	XtUnmanageChild (_extendedAttribForm);
    }

    if (XtIsManaged (_basicAttribForm)) {
	XtUnmanageChild (_basicAttribForm);
    }

    if (XtIsManaged (_basicAttrib2Form)) {
	XtUnmanageChild (_basicAttrib2Form);
    }

    if (XtIsManaged (_basicAttrib3Form)) {
	XtUnmanageChild (_basicAttrib3Form);
    }

    if (XtIsManaged (_intlEdit2Form)) {
	XtUnmanageChild (_intlEdit2Form);
    }

    if (XtIsManaged (_ctlBtnRc)) {
	XtUnmanageChild (_ctlBtnRc);
    }

    if (XtIsManaged (_svfileForm)) {
	XtUnmanageChild (_svfileForm);
    }
/*    if (XtIsManaged (_stateForm)) {
	XtUnmanageChild (_stateForm);
    }  */
}

/*=====================================================================*/

void pgsigw_setAttr ( VG_DBStruct *el )
/************************************************************************
 * pgsigw_setAttr							*
 *									*
 * This function sets the current attributes.				*
 *									*
 * void pgsigw_setAttr (el)						*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	the current elememt		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	initial coding				*
 * S. Law/GSC		09/99	augmented element			*
 * S. Law/GSC		09/99	cleaned up speed string			*
 * S. Law/GSC		09/99	moved distance out of conditional	*
 * H. Zeng/EAI          10/99   fixed a small prob. in sigmet edit box  *
 * A. Hardy/GSC         12/99   added lat/lon button toggle state       *
 * H. Zeng/EAI          01/00   modified for "LEVEL INFO"               *
 * S. Law/GSC		04/00	pgsigw_setOptMenu -> pgutls_setOptMenu	*
 * M. Li/GSC		05/00	added pgsigw_setState			*
 * M. Li/GSC		06/00	removed pgsigw_setState			*
 * F. J. Yen/NCEP	08/00	added volcanic ash			*
 * F. J. Yen/NCEP	10/00	Added tropical cyclone;			*
 * H. Zeng/EAI          10/00   Rearranged the attribute ordering       *
 * H. Zeng/EAI          11/00   Removed managing of _basicAttrib2Form   *
 * S. Jacobs/NCEP	 7/02	Remove blanks from seq num output strng	*
 * H. Zeng/XTRIA        11/02   modified for NxmVolcano.c               *
 * M. Li/SAIC           12/02   radio box -> check box                  *
 * J. Lewis/AWC		05/07	removed pgutls_setOptMenu
 * J. Lewis/AWC		05/07	removed call to pgsigw_setFIRs		*
 ***********************************************************************/
{
    int		npts, tarry[5], plusmin, ier, remainder, ii, last;
    int         itemnum, index;
    struct tm	*utctime;
    time_t	tp;
    char	temp_str[10], ptext[80], *ptr;
    char        *levelstr0[] = {"-none-", "FCST", "TOPS"};
    char        *levelstr1[] = {"TO", "ABV", "BLW", "BTN"};
    char        *levelstr2[] = {"-none-", "AND"};
    char	tempch[MAX_PHENMSTR];
    char        attrib_string[100];
    int		lenstr, iret;
    XmString    xmstr;
/*---------------------------------------------------------------------*/
    _vgType  = (int)el->hdr.vg_type;
    _subType = _origSubType = el->elem.sig.info.subtype;

    pgsigw_typeCb (NULL, _subType, NULL);

    for (ii = 0; ii < _maxType; ii++ ) {
        if ( ii == _subType ) {
            XmToggleButtonSetState (_typePb[ii], TRUE, TRUE);
        }
        else {
            XmToggleButtonSetState (_typePb[ii], False, False);
        }
    }

    pgsigw_setVgInfo ();

    pgutls_setOptMenu (el->elem.sig.info.area, _area[_vgLoc], 
		       _nArea[_vgLoc], &_areaStrc);

    pgutls_setOptMenu (el->elem.sig.info.msgid, _ids[_vgLoc], 
		       _nIds[_vgLoc], &_idsStrc);

    pgutls_setOptMenu (el->elem.sig.info.phenom, _phen[_vgLoc], 
		       _nPhen[_vgLoc], &_phenStrc);

    pgutls_setOptMenu (el->elem.sig.info.phenom2, _phen2[_vgLoc], 
		       _nPhen2[_vgLoc], &_phen2Strc);

    strcpy ( tempch, el->elem.sig.info.phenom );
    cst_rxbl ( tempch, tempch, &lenstr, &iret );

    sprintf(attrib_string, "%sAttributes:", tempch);
    xmstr = XmStringCreateLtoR(attrib_string, XmFONTLIST_DEFAULT_TAG);
    XtVaSetValues (_firstPhenomLabel, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);

    if ( strcmp ( tempch, "VOLCANIC_ASH " ) == 0 ) {
	if (  el->elem.sig.info.phennam[0] == '-' ) {

            pgsigw_volcPbCb(NULL, 0, NULL);        

	    strcpy ( _currPhenam, el->elem.sig.info.phennam );
	    _currPhenam[0] = ' ';
	    XmTextSetString ( _phennamText, _currPhenam );
	    XtSetSensitive ( _phLatLab , TRUE );
	    XtSetSensitive ( _phLonLab , TRUE );
	    XtSetSensitive ( _phenLatLon[0] , TRUE );
	    XtSetSensitive ( _phenLatLon[1] , TRUE );
	    strcpy ( _currLatLon[0], el->elem.sig.info.phenlat );
	    strcpy ( _currLatLon[1], el->elem.sig.info.phenlon );
	}
	else {
            index = NxmVolcano_getIdx(el->elem.sig.info.phennam);
            pgsigw_volcPbCb(NULL, (index+1), NULL);       

	    strcpy ( _currPhenam, el->elem.sig.info.phennam );
	    XmTextSetString ( _phenLatLon[0], "" );
	    XmTextSetString ( _phenLatLon[1], "" );
	    _currLatLon[0][0] = '\0';
	    _currLatLon[1][0] = '\0';

/*SEN not changed?*/
	    XtSetSensitive ( _phLatLab , FALSE );
	    XtSetSensitive ( _phLonLab , FALSE );
	    XtSetSensitive ( _phenLatLon[0] , FALSE );
	    XtSetSensitive ( _phenLatLon[1] , FALSE );
	}
	XmTextSetString ( _phenLatLon[0], _currLatLon[0] );
	XmTextSetString ( _phenLatLon[1], _currLatLon[1] );

	strcpy ( _currFreeText, el->elem.sig.info.freetext );
	XmTextSetString ( _freetxtText, _currFreeText );

        if (XtIsManaged (_basicAttrib2Form)) {
	    XtUnmanageChild (_basicAttrib2Form);
        }

	XtSetSensitive ( _presLab, FALSE );
	XtSetSensitive ( _presHPA, FALSE );
	XtSetSensitive ( _maxwndLab, FALSE );
	XtSetSensitive ( _maxWinds, FALSE );

    }
    else {
        pgsigw_volcPbCb(NULL, 0, NULL);      

	_currLatLon[0][0] = '\0';
	_currLatLon[1][0] = '\0';
	strcpy ( _currFreeText, "");
	XmTextSetString ( _phennamText, "\0" );
	XmTextSetString ( _phenLatLon[0], _currLatLon[0] );
	XmTextSetString ( _phenLatLon[1], _currLatLon[1] );
	XmTextSetString ( _freetxtText, _currFreeText );
	XtSetSensitive ( _phenMenuB,  FALSE );
	XtSetSensitive ( _phLatLab , FALSE );
	XtSetSensitive ( _phLonLab , FALSE );
	XtSetSensitive ( _phenLatLon[0] , FALSE );
	XtSetSensitive ( _phenLatLon[1] , FALSE );
        if ( strcmp ( tempch, "TROPICAL_CYCLONE " ) == 0 ) {
	    if (  el->elem.sig.info.phennam[0] == '-' ) {

                pgsigw_volcPbCb(NULL, 0, NULL);             
	        strcpy ( _currPhenam, el->elem.sig.info.phennam );
	        _currPhenam[0] = ' ';
	    }
	    else {
		strcpy ( _currPhenam, el->elem.sig.info.phennam );
	    }
	    XmTextSetString ( _phennamText, _currPhenam );

	    if ( (_subType == SIGTYP_ISOL ) &&
		(strcmp (tempch, "TROPICAL_CYCLONE ") == 0 ) ) {
	        XtSetSensitive (_phLatLab, FALSE);
	        XtSetSensitive (_phLonLab, FALSE);
	        XtSetSensitive (_phenLatLon[0], FALSE);
	        XtSetSensitive (_phenLatLon[1], FALSE);
            }
	    else {
	        XtSetSensitive (_phLatLab, TRUE);
	        XtSetSensitive (_phLonLab, TRUE);
	        XtSetSensitive (_phenLatLon[0], TRUE);
	        XtSetSensitive (_phenLatLon[1], TRUE);
            }
	    XmTextSetString ( _phennamText, _currPhenam );

	    XtSetSensitive ( _presLab, TRUE );
	    XtSetSensitive ( _presHPA, TRUE );
	    XtSetSensitive ( _maxwndLab, TRUE );
	    XtSetSensitive ( _maxWinds, TRUE );
	    XtSetSensitive ( _freetxtLab, TRUE );
	    XtSetSensitive ( _freetxtText, TRUE );
/*Add SEN ALSO need to get rid setting outside of IF and put in ELSE*/
	    strcpy ( _currLatLon[0], el->elem.sig.info.phenlat );
	    strcpy ( _currLatLon[1], el->elem.sig.info.phenlon );
	    XmTextSetString ( _phenLatLon[0], _currLatLon[0] );
	    XmTextSetString ( _phenLatLon[1], _currLatLon[1] );
    	    if ( _subType == SIGTYP_ISOL ) {
	        XtSetSensitive ( _phLatLab , TRUE );
	        XtSetSensitive ( _phLonLab , TRUE );
	        XtSetSensitive ( _phenLatLon[0] , TRUE );
	        XtSetSensitive ( _phenLatLon[1] , TRUE );
	    }
	    strcpy ( _currFreeText, el->elem.sig.info.freetext );
	    XmTextSetString ( _freetxtText, _currFreeText );
	}
	else {
	    XmTextSetString ( _phennamText, "\0" );
            _currPhenam[0] = '\0';
	    XmTextSetString ( _phenLatLon[0], "" );
	    XmTextSetString ( _phenLatLon[1], "" );
	    _currLatLon[0][0] = '\0';
	    _currLatLon[1][0] = '\0';

            if (XtIsManaged (_basicAttrib2Form)) {
	        XtUnmanageChild (_basicAttrib2Form);
            }

	    XtSetSensitive ( _presLab, FALSE );
	    XtSetSensitive ( _presHPA, FALSE );
	    XtSetSensitive ( _maxwndLab, FALSE );
	    XtSetSensitive ( _maxWinds, FALSE );
	    XtSetSensitive ( _freetxtLab, FALSE );
	    XtSetSensitive ( _freetxtText, FALSE );
	}
    }
 
    _currMove = (strcmp (el->elem.sig.info.move, "STNRY") == 0) ? 0 : 1;
    XmToggleButtonSetState (_movePb[_currMove], TRUE, TRUE);

    sprintf (temp_str, "%d", el->elem.sig.info.spd);
    last = MAX_SPDSTR - 1;
    for (ii = (int)strlen (temp_str); ii < last; ii++) {
	temp_str[ii] = ' ';
    }
    temp_str[last] = '\0';

    pgutls_setOptMenu (temp_str, _spd, _nSpd, &_spdStrc);

    pgutls_setOptMenu (el->elem.sig.info.dir, _dir, _nDir, &_dirStrc);

    pgutls_setOptMenu (el->elem.sig.info.trend, _trend[_vgLoc], 
		       _nTrend[_vgLoc], &_trendStrc);

    pgutls_setOptMenu (el->elem.sig.info.remarks, _rem[_vgLoc], 
		       _nRem[_vgLoc], &_remStrc);


    npts = el->elem.sig.info.npts;
    pgsigw_setFrom (npts, el->elem.sig.latlon, 
		    &(el->elem.sig.latlon[npts]));

    strcpy ( _currLatLon[0], el->elem.sig.info.phenlat );
    strcpy ( _currLatLon[1], el->elem.sig.info.phenlon );

    _currDist = el->elem.sig.info.distance;
    sprintf (temp_str, "%-6.2f", _currDist);
    cst_rmbl ( temp_str, temp_str, &lenstr, &iret );
    XmTextSetString (_distText, temp_str);

    _currPres = el->elem.sig.info.pres;
    sprintf (temp_str, "%d", _currPres);
    if( _currPres == IMISSD ) {
      XmTextSetString (_presHPA, "\0");
    }
    else {
      XmTextSetString (_presHPA, temp_str);
    }

    _currMaxWind = el->elem.sig.info.maxwind;
    sprintf (temp_str, "%d", _currMaxWind);
    if( _currMaxWind == IMISSD ) {
      XmTextSetString (_maxWinds, "\0");
    }
    else {
      XmTextSetString (_maxWinds, temp_str);
    }

/*
 * Sequence number of 0 means that the SIGMET has been drawn, but
 * has never been formatted.  The first time that it is formatted,
 * it is set to 1 and there is no previous SIGMET.  
 */
    if (0 < el->elem.sig.info.seqnum && 
	el->elem.sig.info.seqnum < MAX_SEQNUM) {
	_currSequence = el->elem.sig.info.seqnum;

	sscanf (el->elem.sig.info.stime, "%d", &(_currTimes[0]));
	sscanf (el->elem.sig.info.etime, "%d", &(_currTimes[1]));

	_currStatus = el->elem.sig.info.status;

/*
 *  Get level information from el
 */  
        strcpy(ptext, el->elem.sig.info.tops);

        ptr = strtok (ptext, "|" ) ;
        itemnum = XtNumber(levelstr0);
        for(ii = 0; ii < itemnum; ii++) {
           if(strcmp(ptr,levelstr0[ii]) == 0) {
              _levelInfoOp0.current = ii;
              XtVaSetValues (_levelInfoOp0.menu, 
		             XmNmenuHistory,	
                             _levelInfoOp0.pb[_levelInfoOp0.current], 
		             NULL );
           }
        }

        ptr = strtok(NULL, "|");
        itemnum = XtNumber(levelstr1);
        for(ii = 0; ii < itemnum; ii++) {
           if(strcmp(ptr,levelstr1[ii]) == 0) {
              _levelInfoOp1.current = ii;
              XtVaSetValues (_levelInfoOp1.menu, 
		             XmNmenuHistory,	
                             _levelInfoOp1.pb[_levelInfoOp1.current], 
		             NULL );
           }
        }

        ptr = strtok(NULL, "|");
        XmTextFieldSetString( _levelInfoText0, ptr );

        ptr = strtok(NULL, "|");
        itemnum = XtNumber(levelstr2);
        for(ii = 0; ii < itemnum; ii++) {
           if(strcmp(ptr,levelstr2[ii]) == 0) {
              _levelInfoOp2.current = ii;
              XtVaSetValues (_levelInfoOp2.menu, 
		             XmNmenuHistory,	
                             _levelInfoOp2.pb[_levelInfoOp2.current], 
		             NULL );
           }
        }

        ptr = strtok(NULL, "|");
        XmTextFieldSetString( _levelInfoText1, ptr );

    }
    else {
	_currSequence = 1;
	_currStatus   = 0;

/* 
 * set up _currTimes
 */
	tp = time (NULL);
	utctime = gmtime (&tp);

	tarry[0] = utctime->tm_year + 1900;
	tarry[1] = utctime->tm_mon + 1;
	tarry[2] = utctime->tm_mday;
	tarry[3] = utctime->tm_hour;
	tarry[4] = utctime->tm_min;

	if (tarry[4] != 0) {
	    remainder = tarry[4] % STIME_INTV;
	    plusmin = STIME_INTV - remainder;
	    ti_addm (tarry, &plusmin, tarry, &ier);
	}

	_currTimes[0] = (tarry[2] * 10000) + (tarry[3] * 100) + tarry[4];

	plusmin = ETIME_INTV;
	ti_addm (tarry, &plusmin, tarry, &ier);
	_currTimes[1] = (tarry[2] * 10000) + (tarry[3] * 100) + tarry[4];

/*
 * Set default level information
 */
        _levelInfoOp0.current = 0;
        XtVaSetValues (_levelInfoOp0.menu, 
		       XmNmenuHistory,  
                       _levelInfoOp0.pb[_levelInfoOp0.current], 
		       NULL );

        _levelInfoOp1.current = 0;
        XtVaSetValues (_levelInfoOp1.menu, 
		       XmNmenuHistory,	
                       _levelInfoOp1.pb[_levelInfoOp1.current], 
		       NULL );

        XmTextFieldSetString(_levelInfoText0, "" );

        _levelInfoOp2.current = 0;
        XtVaSetValues (_levelInfoOp2.menu, 
		       XmNmenuHistory,	
                       _levelInfoOp2.pb[_levelInfoOp2.current], 
		       NULL );

        XmTextFieldSetString(_levelInfoText1, "" );
        
/*
 * Set default phenomenon name, lat, lon, free text,
 * pressure and max winds.
 */
        XmTextSetString(_phennamText, "\0" );
	_currLatLon[0][0] = '\0';
	_currLatLon[1][0] = '\0';

        XmTextSetString(_phenLatLon[0], "" );
        XmTextSetString(_phenLatLon[1], "" );
        XmTextSetString(_freetxtText, "" );
        XmTextSetString(_presHPA, "" );
	_currPres = IMISSD;
        XmTextSetString(_maxWinds, "" );
	_currMaxWind = IMISSD;
    }

    sprintf (temp_str, "%-3d", _currSequence);
    cst_rmbl ( temp_str, temp_str, &lenstr, &iret );
    XmTextSetString (_seqnumText, temp_str);

    XmToggleButtonSetState (_statusPb[_currStatus], TRUE, TRUE);

    XmToggleButtonSetState (_formatPb[_fmtflag], TRUE, TRUE);

    sprintf (temp_str, "%6.6d", _currTimes[0]);
    XmTextSetString (_timesText[0], temp_str);
    sprintf (temp_str, "%6.6d", _currTimes[1]);
    XmTextSetString (_timesText[1], temp_str);

    _attrColor = el->hdr.maj_col;
    XtVaSetValues(_colorPb,
		  XmNbackground,	NxmColrP_getColorPixel (_attrColor),
		  XmNtopShadowColor,	NxmColrP_getColorPixel (_attrColor),
		  XmNbottomShadowColor,	NxmColrP_getColorPixel (_attrColor),
		  NULL);

    _solStrc.current = el->elem.sig.info.sol;
    XtVaSetValues (_solStrc.menu, 
		   XmNmenuHistory, _solStrc.pb[_solStrc.current], 
		   NULL);   
}

/*=====================================================================*/

void pgsigw_setFrom ( int np, float *lat, float *lon )
/************************************************************************
 * pgsigw_setFrom							*
 *									*
 * This function sets the from line based on the new coordinates.	*
 *									*
 * void pgsigw_setFrom (np, lat, lon )					*
 *									*
 * Input parameters:							*
 *	np		int	number of points			*
 *	*lat		float	points of latitude			*
 *	*lon		float	points of longitude			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	initial coding				*
 * E. Safford/GSC	08/99	increase fmline size                    *
 * D.W.Plummer/NCEP	 9/99	change calling sequence of clo_from	*
 * A. Hardy/GSC		12/99	change calling sequence of clo_from	*
 * M. Li/GSC		06/00	saved current latlon			*
 * R. Curtis/EAI 	10/00   increased fmline from 256 to 1500  	*
 * T. Piper/GSC		11/00	increased dlat and dlon from 20 to 	*
 *				MAX_SIGMET & fmline to 15*MAX_SIGMET	*
 * T. Piper/SAIC	 4/02	Fixed UMR; removed = from for loop	*
 ***********************************************************************/
{
    int		ii, ier;
    char	fmline[15*MAX_SIGMET];
    static  int     dnp;
    static  float   dlat[MAX_SIGMET], dlon[MAX_SIGMET];
/*---------------------------------------------------------------------*/
/*
 *  Save the latitude and longitude values
 */

    if (np > 0 ) {
	clo_from (_vgType, _subType, np, _fmtflag, lat, lon, 100, fmline, &ier);
	dnp = np;
	_currPts = np;
        for ( ii=0; ii < np; ii++){
    	    dlat[ii] = lat[ii];
    	    dlon[ii] = lon[ii];
	    _currLat[ii] = lat[ii];
	    _currLon[ii] = lon[ii];
        }
    }
    else {
	clo_from (_vgType, _subType, dnp, _fmtflag, dlat, dlon, 100, fmline, &ier);
    }
    
    XtVaSetValues (_fromText, XtVaTypedArg, XmNlabelString, XmRString, 
				fmline, strlen(fmline) + 1, NULL); 
}

/*=====================================================================*/

void pgsigw_getAttr ( VG_DBStruct *el )
/************************************************************************
 * pgsigw_getAttr							*
 *									*
 * This function retrieves the current attributes.			*
 *									*
 * void pgsigw_getAttr (el)						*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*el	VG_DBStruct	element structure			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	initial coding				*
 * S. Law/GSC		09/99	augmented element			*
 * S. Law/GSC		09/99	corrected id string			*
 * H. Zeng/EAI          01/00   modified for "LEVEL INFO"               *
 * F. J. Yen/NCEP	08/00	added volcanic ash			*
 * F. J. Yen/NCEP	10/00	added tropical cyclone			*
 * H. Zeng/XTRIA        11/02   modified for NxmVolcano.c               *
 * J. Lewis/AWC		05/07	removed call to pgsigw_getFIRstr	*
 * J. Lewis/AWC		05/07	removed strcpy for forecaster element	*
 ***********************************************************************/
{
    char	*temp_str;
    char	tempch[MAXTBLNAME], name[33];
    float       lat, lon;
    XmString    xmstr;
/*---------------------------------------------------------------------*/

    el->hdr.vg_class = CLASS_SIGMETS;
    el->hdr.vg_type  = (char)_vgType;
    el->hdr.maj_col = _attrColor;
    el->hdr.closed  = (char)((_subType == SIGTYP_AREA) ? 1 : 0);
    el->elem.sig.info.subtype = _subType;

    strcpy (el->elem.sig.info.area,    _area[_vgLoc][_areaStrc.current]);
    strcpy (el->elem.sig.info.msgid,   _ids[_vgLoc][_idsStrc.current]);
    strcpy (el->elem.sig.info.phenom,  _phen[_vgLoc][_phenStrc.current]);
    strcpy (el->elem.sig.info.phenom2, _phen2[_vgLoc][_phen2Strc.current]);

    el->elem.sig.info.seqnum   = _currSequence;
    el->elem.sig.info.distance = _currDist;
    el->elem.sig.info.sol      = _solStrc.current;

    el->elem.sig.info.status  = _currStatus;
    el->elem.sig.info.pres    = _currPres;
    el->elem.sig.info.maxwind = _currMaxWind;

    temp_str = XmTextGetString (_timesText[0]);
    strcpy (el->elem.sig.info.stime, temp_str);
    XtFree (temp_str);

    temp_str = XmTextGetString (_timesText[1]);
    strcpy (el->elem.sig.info.etime, temp_str);
    XtFree (temp_str);

    temp_str = XmTextGetString (_phenLatLon[0]);
    strcpy (el->elem.sig.info.phenlat, temp_str);
    XtFree (temp_str);

    temp_str = XmTextGetString (_phenLatLon[1]);
    strcpy (el->elem.sig.info.phenlon, temp_str);
    XtFree (temp_str);
 
    temp_str = XmTextGetString (_freetxtText);
    strcpy (el->elem.sig.info.freetext, temp_str);
    XtFree (temp_str);

    if ( _volcIdx == -1 ) {
	temp_str = XmTextGetString (_phennamText);
	strcpy (tempch, "-");
	strcat (tempch, temp_str);
	strcpy (el->elem.sig.info.phennam, tempch);
	XtFree (temp_str);
    }
    else {
        NxmVolcano_getInfo( _volcIdx, &lat, &lon, name );
        strcpy (el->elem.sig.info.phennam, name);
    }

/*
 * Restore level information into el
 */
    XtVaGetValues(_levelInfoOp0.pb[_levelInfoOp0.current],
                  XmNlabelString,         &xmstr,
                  NULL );
    XmStringGetLtoR(xmstr, 
                    XmFONTLIST_DEFAULT_TAG, &temp_str);     
    strcpy(el->elem.sig.info.tops, temp_str);
    strcat(el->elem.sig.info.tops, "|");
    XmStringFree(xmstr);
    XtFree(temp_str);

    XtVaGetValues(_levelInfoOp1.pb[_levelInfoOp1.current],
                  XmNlabelString,         &xmstr,
                  NULL );
    XmStringGetLtoR(xmstr, 
                    XmFONTLIST_DEFAULT_TAG, &temp_str);     
    strcat(el->elem.sig.info.tops, temp_str);
    strcat(el->elem.sig.info.tops, "|");
    XmStringFree(xmstr);
    XtFree(temp_str);

    temp_str = XmTextGetString(_levelInfoText0);

    if(temp_str[0] == '\0') {
       strcat(el->elem.sig.info.tops, " ");
    }
    else {
       strcat(el->elem.sig.info.tops, temp_str);
    }

    strcat(el->elem.sig.info.tops, "|");
    XtFree (temp_str);

    XtVaGetValues(_levelInfoOp2.pb[_levelInfoOp2.current],
                  XmNlabelString,         &xmstr,
                  NULL );
    XmStringGetLtoR(xmstr, 
                    XmFONTLIST_DEFAULT_TAG, &temp_str);     
    strcat(el->elem.sig.info.tops, temp_str);
    strcat(el->elem.sig.info.tops, "|");
    XmStringFree(xmstr);
    XtFree(temp_str);

    temp_str = XmTextGetString(_levelInfoText1);

    if(temp_str[0] == '\0') {
       strcat(el->elem.sig.info.tops, " ");
    }
    else {
       strcat(el->elem.sig.info.tops, temp_str);
    }

    strcat(el->elem.sig.info.tops, "|");
    XtFree (temp_str);

    strcpy (el->elem.sig.info.move, ((_currMove == 0) ? "STNRY" : "MVG"));

    sscanf (_spd[_spdStrc.current], "%d", &el->elem.sig.info.spd);

    strcpy (el->elem.sig.info.dir, _dir[_dirStrc.current]);

    strcpy (el->elem.sig.info.trend, _trend[_vgLoc][_trendStrc.current]);

    strcpy (el->elem.sig.info.remarks, _rem[_vgLoc][_remStrc.current]);

/*
 *  Set default values for currently unused elements
 */
    el->elem.sig.info.obstime[0] = '\0';
    el->elem.sig.info.obsfcst    = IMISSD;
    el->elem.sig.info.sonic      = IMISSD;
    el->elem.sig.info.fl      = IMISSD;
}

/*=====================================================================*/

Boolean pgsigw_isUp ( void )
/************************************************************************
 * pgsigw_isUp								*
 *									*
 * This function returns a boolean value specifying whether the SIGMET	*
 * dialog is managed or not.						*
 *									*
 * Boolean pgsigw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 * Return parameters:							*
 *	pgsigw_isUp		Boolean		Is/is not managed	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	Copied from pgtrkw_isUp			*
 * S. Law/GSC		09/99	removed format widget			*
 ***********************************************************************/
{
    return (XtIsManaged (_mainForm));
}

/*=====================================================================*/

int pgsigw_getSubType ( void )
/************************************************************************
 * pgsigw_getSubType							*
 *									*
 * This function returns the SIGMET subtype.				*
 *									*
 * int pgsigw_getSubType ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	pgsigw_getSubType	int	SIGMET subtype			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/99	initial coding				*
 ***********************************************************************/
{
    return (_subType);
}

/*=====================================================================*/

void pgsigw_getStrings ( int area_num, int ids_num, char *area_str, 
							char *id_str )
/************************************************************************
 * pgsigw_getStrings							*
 *									*
 * This function retrieves the area abbreviation string for area_num.	*
 *									*
 * void pgsigw_getStrings (area_num, ids_num, area_str, id_str)		*
 *									*
 * Input parameters:							*
 *	area_num		int	area menu number		*
 *	ids_num			int	id menu number			*
 *									*
 * Output parameters:							*
 *	*area_str		char	area abbreviation string	*
 *	*id_str			char	id abbreviation string		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/99	initial coding				*
 ***********************************************************************/
{
    if (0 <= area_num && area_num < _nArea[_vgLoc]) {
	strcpy (area_str, _area[_vgLoc][area_num]);
    }
    else {
	strcpy (area_str, "");
    }

    if (0 <= ids_num && ids_num < _nIds[_vgLoc]) {
	strcpy (id_str, _ids[_vgLoc][ids_num]);
    }
    else {
	strcpy (id_str, "");
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_arrowCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_arrowCb							*
 *									*
 * Callback function for the IDs menu.					*
 *									*
 * void pgsigw_arrowCb (wid, clnt, cbs)					*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		sequence increment		*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/99	initial coding				*
 * S. Jacobs/NCEP	 7/02	Remove blanks from output string	*
 ***********************************************************************/
{
    int		lenstr, ier;
    char	seqnum[5];
/*---------------------------------------------------------------------*/
    _currSequence += (int)clnt;

    if (_currSequence < 1) _currSequence = 1;
    if (_currSequence > MAX_SEQNUM) _currSequence = MAX_SEQNUM;

    sprintf (seqnum, "%-3d", _currSequence);
    cst_rmbl ( seqnum, seqnum, &lenstr, &ier );

    XtVaSetValues (_seqnumText, XmNvalue, seqnum, NULL);
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_typeCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_typeCb							*
 *									*
 * Callback function for the SIGMET type (isolated, line, or area).	*
 *									*
 * void pgsigw_typeCb (wid, clnt, cbs )					*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		SIGMET type			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	initial coding				*
 * E. Safford/GSC	08/99	add call to pgnew_setArmDynamic		*
 * S. Law/GSC		09/99	removed pgnew_setArmDynamic call	*
 * F. J. Yen/NCEP	10/00	handled TC				*
 * M. Li/SAIC		12/02	radio box -> check box			*
 ***********************************************************************/
{
    char	tempch[MAX_PHENMSTR];
    int		len, ier, ii;
/*---------------------------------------------------------------------*/

    _subType = (int)clnt;

    XtSetSensitive (_solStrc.form,  (_subType == SIGTYP_LINE));
    XtSetSensitive (_distForm, (_subType != SIGTYP_AREA));

    pgnew_setMultiPt ((char)(_subType != SIGTYP_ISOL));
    cst_rxbl ( _phen[_vgLoc][_phenStrc.current], tempch, &len, &ier);
    if ( (strcmp (tempch, "TROPICAL_CYCLONE ") == 0 ) ) {
        if ( _subType == SIGTYP_ISOL ) {  
	    XtSetSensitive (_phLatLab, FALSE);
	    XtSetSensitive (_phLonLab, FALSE);
	    XtSetSensitive (_phenLatLon[0], FALSE);
	    XtSetSensitive (_phenLatLon[1], FALSE);
	}
	else {
	    XtSetSensitive (_phLatLab, TRUE);
	    XtSetSensitive (_phLonLab, TRUE);
	    XtSetSensitive (_phenLatLon[0], TRUE);
	    XtSetSensitive (_phenLatLon[1], TRUE);
	}
    }
    for (ii = 0; ii < _maxType; ii++ ) { 
	if ( ii != (int)clnt )
            XmToggleButtonSetState (_typePb[ii], False, False);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_distanceCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_distanceCb							*
 *									*
 * Callback function for the distance text box.				*
 *									*
 * void pgsigw_distanceCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	initial coding				*
 * E. Safford/GSC	08/99	add call to pgnew_setArmDynamic		*
 * H. Zeng/EAI          10/99   fixed a small prob. in sigmet edit box  *
 * J. Wu/SAIC           05/02   wipe text before assigning new values  	*
 * S. Jacobs/NCEP	 7/02	Remove blanks from output string	*
 ***********************************************************************/
{
    float	dist;
    int		lenstr, ier;
    char	*dstr, newstr[8];
/*---------------------------------------------------------------------*/

    dstr = XmTextGetString (_distText);

    sscanf (dstr, "%f", &dist);

    XtFree (dstr);

    if (0.0F <= dist && dist <= 1000.0F) {
	sprintf (newstr, "%-6.2f", dist);
	_currDist = dist;
    }
    else {
	sprintf (newstr, "%-6.2f", _currDist);
    }

    cst_rmbl ( newstr, newstr, &lenstr, &ier );
    XmTextSetString (_distText, "");
    XmTextSetString (_distText, newstr);
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_presCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_presCb							*
 *									*
 * Callback function for the pressure text box.				*
 *									*
 * void pgsigw_presCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * F. J Yen/NCEP	10/00	based on pgsigw_phenllCb                *
 * H. Zeng/EAI          11/00   added check to the validity of the value*
 ***********************************************************************/
{
    int		pres;
    char	*newpres, presstr[8];
/*---------------------------------------------------------------------*/

    newpres = XmTextGetString (_presHPA);

    strcpy ( presstr, newpres );
    if ( presstr[0] == '\0' ) {
	_currPres = IMISSD;
    }
    else {
	sscanf ( newpres, "%d", &pres );
	
	if ( IMISSD < pres && pres < 10000 ) {
	    _currPres = pres;
	}
	else {
	    if ( _currPres == IMISSD ) {
		newpres[0] = '\0';
	    }
	}

	sprintf (newpres, "%d", _currPres );
        if( _currPres == IMISSD ) {
	  XtVaSetValues ( _presHPA, XmNvalue, "\0", NULL );
        }
        else {
	  XtVaSetValues ( _presHPA, XmNvalue, newpres, NULL );
        }

    }
    XtFree ( newpres );
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_maxwndCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_maxwndCb							*
 *									*
 * Callback function for the max winds text box.			*
 *									*
 * void pgsigw_maxwndCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * F. J Yen/NCEP	10/00	based on pgsigw_phenllCb                *
 * H. Zeng/EAI          11/00   added check to the validity of the value*
 ***********************************************************************/
{
    int		maxwnd;
    char	*newmxwd, mxwdstr[8];
/*---------------------------------------------------------------------*/

    newmxwd = XmTextGetString (_maxWinds);

    strcpy ( mxwdstr, newmxwd );
    if ( mxwdstr[0] == '\0' ) {
	_currMaxWind = IMISSD;
    }
    else {
	sscanf ( newmxwd, "%d", &maxwnd );
	
	if ( 0 < maxwnd && maxwnd < 1000 ) {
	    _currMaxWind = maxwnd;
	}
	else {
	    if ( _currMaxWind == IMISSD ) {
		newmxwd[0] = '\0';
	    }
	}

	sprintf (newmxwd, "%d", _currMaxWind );
        if( _currMaxWind == IMISSD ) {
	  XtVaSetValues ( _maxWinds, XmNvalue, "\0", NULL );
        }
        else {
	  XtVaSetValues ( _maxWinds, XmNvalue, newmxwd, NULL );
        }
    }
    XtFree ( newmxwd );
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_sequenceCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_sequenceCb							*
 *									*
 * Callback function for the sequence text box.				*
 *									*
 * void pgsigw_sequenceCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer		not used		*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          11/99   initial coding                          *
 * S. Jacobs/NCEP	 7/02	Remove blanks from output string	*
 ***********************************************************************/
{
    int 	num, lenstr, ier;
    char	*sqstr, seqnum[5];
/*---------------------------------------------------------------------*/

    sqstr = XmTextGetString (_seqnumText);

    sscanf (sqstr, "%d", &num);

    XtFree (sqstr);

    if ( num < 1) num = 1;
    if ( num > MAX_SEQNUM) num = MAX_SEQNUM;

    _currSequence = num;

    sprintf (seqnum, "%-3d", _currSequence);
    cst_rmbl ( seqnum, seqnum, &lenstr, &ier );

    XtVaSetValues (_seqnumText, XmNvalue, seqnum, NULL);

}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_editPbCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_editPbCb							*
 *									*
 * Callback function for the edit attributes button.			*
 *									*
 * void pgsigw_editPbCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	not used			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	initial coding				*
 * F. J. Yen/NCEP	08/00	added volcanic ash			*
 * F. J. Yen/NCEP	10/00	sensitize location widgets as needed	*
 * H. Zeng/EAI          10/00   Added updating of phenomenon label      *
 * H. Zeng/XTRIA        11/02   modified for NxmVolcano.c               *
 * J. Lewis/AWC		07/08	Remove exclusion of SIGTYP_ISOL when    *
 *				sensitizing widgets			*
 ***********************************************************************/
{
    char	tempch[MAXTBLNAME], attrib_string[100];
    int		lenstr, iret;
    XmString    xmstr;
/*---------------------------------------------------------------------*/

    XtUnmanageChild (_editAttrPb);
    XtManageChild (_intlEditForm);
    XtManageChild (_basicAttribForm);
    XtManageChild (_basicAttrib3Form);
    XtManageChild (_intlEdit2Form);
    cst_rxbl ( _phen[_vgLoc][_phenStrc.current], tempch, &lenstr, &iret );

    sprintf(attrib_string, "%sAttributes:", tempch);
    xmstr = XmStringCreateLtoR(attrib_string, XmFONTLIST_DEFAULT_TAG);
    XtVaSetValues (_firstPhenomLabel, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);

    if ( strcmp ( tempch, "VOLCANIC_ASH " ) == 0 ) {

        XtManageChild(_extendedAttribForm);        
	XtSetSensitive (_phenMenuB, TRUE );

	if ( _volcIdx != -1 ) {
            XtVaSetValues(_phennamText,
                          XmNeditable,                FALSE,
                          XmNcursorPositionVisible,   FALSE,
                          NULL  );

            XtSetSensitive ( _phLatLab , FALSE );
            XtSetSensitive ( _phLonLab , FALSE );
            XtSetSensitive ( _phenLatLon[0] , FALSE );
            XtSetSensitive ( _phenLatLon[1] , FALSE );
	}
	else {
            XtVaSetValues(_phennamText,
                          XmNeditable,                TRUE,
                          XmNcursorPositionVisible,   TRUE,
                          NULL  );

            XtSetSensitive ( _phLatLab , TRUE );
            XtSetSensitive ( _phLonLab , TRUE );
            XtSetSensitive ( _phenLatLon[0] , TRUE );
            XtSetSensitive ( _phenLatLon[1] , TRUE );
	}
	XtSetSensitive ( _freetxtLab, TRUE );
	XtSetSensitive ( _freetxtText, TRUE );

        if (XtIsManaged (_basicAttrib2Form)) {
	    XtUnmanageChild (_basicAttrib2Form);
        }

	XtSetSensitive ( _presLab, FALSE );
	XtSetSensitive ( _presHPA, FALSE );
	XtSetSensitive ( _maxwndLab, FALSE );
	XtSetSensitive ( _maxWinds, FALSE );
    }
    else {
        XtSetSensitive ( _phLatLab , FALSE );
        XtSetSensitive ( _phLonLab , FALSE );
        XtSetSensitive ( _phenLatLon[0] , FALSE );
        XtSetSensitive ( _phenLatLon[1] , FALSE );
	if ( strcmp ( tempch, "TROPICAL_CYCLONE " ) == 0 ) {

            XtManageChild(_extendedAttribForm);        

            if (!(XtIsManaged (_basicAttrib2Form)) ) {
	        XtManageChild (_basicAttrib2Form);
            }    

	    XtSetSensitive ( _presLab, TRUE );
	    XtSetSensitive ( _presHPA, TRUE );
	    XtSetSensitive ( _maxwndLab, TRUE );
	    XtSetSensitive ( _maxWinds, TRUE );
	    XtSetSensitive ( _freetxtLab, TRUE );
	    XtSetSensitive ( _freetxtText, TRUE );
            XtSetSensitive ( _phLatLab , TRUE );
            XtSetSensitive ( _phLonLab , TRUE );
	    XtSetSensitive ( _phenLatLon[0] , TRUE );
	    XtSetSensitive ( _phenLatLon[1] , TRUE );
	}
	else {

            if (XtIsManaged (_basicAttrib2Form)) {
	        XtUnmanageChild (_basicAttrib2Form);
            }

	    XtSetSensitive ( _presLab, FALSE );
	    XtSetSensitive ( _presHPA, FALSE );
	    XtSetSensitive ( _maxwndLab, FALSE );
	    XtSetSensitive ( _maxWinds, FALSE );
	    XtSetSensitive ( _freetxtLab, FALSE );
	    XtSetSensitive ( _freetxtText, FALSE );

            XtUnmanageChild(_extendedAttribForm);        
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_statusTbCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_statusTbCb							*
 *									*
 * Callback function for the status toggle buttons.			*
 *									*
 * void pgsigw_statusTbCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		which button			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	initial coding				*
 ***********************************************************************/
{
    _currStatus = (int)clnt;
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_formatCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_formatCb							*
 * Callback function for the decimal/minutes of degree toggle buttons.	*
 *									*
 * void pgsigw_formatCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		which button			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		12/99		inital coding			*
 ***********************************************************************/
{
   int     npts;
   float   lat[20], lon[20];
/*---------------------------------------------------------------------*/
    npts = 0;

    if ( (int)clnt != _fmtflag ){
        _fmtflag = (int)clnt;
       pgsigw_setFrom( npts, lat, lon );
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_phenllCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_phenllCb							*
 *									*
 * Callback function for the lat/lon widgets.				*
 *									*
 * void pgsigw_phenllCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		which (0=lat or 1=lon)		*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	08/00	Modified from pgsigw_timesCb		*
 * F. J. Yen/NCEP	10/00	Changed decimal entry to degree/minutes *
 * H. Zeng/EAI          11/00   Added more rigid check to the values    *
 * E. Safford/SAIC	12/01	correct array bound write, use NO_VAL   *
 * J. Lewis/AWC         09/07   Prepend N,S,E,W to phenom lat/lon values*
 ***********************************************************************/
{
    char	*newlatlon,  chlatlon[MAX_LONSTR], chnum[MAX_LONSTR];
    char	dirch[] = "N", chtmp[4];
    int		length, iret, nlatlon;
/*---------------------------------------------------------------------*/

    newlatlon = XmTextGetString (_phenLatLon[clnt]);

    strcpy ( chlatlon, newlatlon);
    cst_rmbl ( chlatlon, chlatlon, &length, &iret );
    length--;
	    
    if ( chlatlon[0] == '\0' ) {
	strcpy ( _currLatLon[clnt], NO_VAL );
    }
    else {
	cst_lcuc ( chlatlon, chlatlon, &iret);
	if ( 
             ( (clnt == 0) &&
                ((chlatlon[length]=='N')||(chlatlon[length]=='S')) )
             ||
             ( (clnt == 1) &&
                ((chlatlon[length]=='W')||(chlatlon[length]=='E')) )
	   ) {             

	    /*
	     *  OK
	     */	
	    dirch [0] = chlatlon[length];
	    sprintf ( chtmp, "%c", dirch[0]);
	    strcpy ( chnum, chlatlon );
	    chnum[length] = '\0';
	    strcat ( chtmp, chnum );
	    cst_numb ( chnum, &nlatlon, &iret );
	    if ( iret == 0 && nlatlon >= 0 ) {
		if ( clnt == 0 ) {
		    if ( nlatlon > 9000 ) {
			strcpy ( _currLatLon[clnt], NO_VAL );
	    	    }
	    	    else{
			sprintf ( chnum, "%04d", nlatlon );
			strcat ( chnum, dirch );
			strcpy ( _currLatLon[clnt], chnum );
	    	    }
		}
		else {
		    if ( nlatlon > 36000 ) {
			strcpy ( _currLatLon[clnt], NO_VAL );
	    	    }
	    	    else{
			sprintf ( chnum, "%04d", nlatlon );
			strcat ( chnum, dirch );
			strcpy ( _currLatLon[clnt], chnum );
	    	    }
		}
	    }
            else {
        	strcpy ( _currLatLon[clnt], NO_VAL );
           }
	}
	else {
	    strcpy ( _currLatLon[clnt], NO_VAL );
	}

	if ( strcmp(_currLatLon[clnt], NO_VAL) == 0) {
	  XtVaSetValues(_phenLatLon[clnt], XmNvalue, "\0", NULL);
        }
        else {
	  strcpy ( newlatlon, chtmp );
	  XtVaSetValues(_phenLatLon[clnt], XmNvalue, newlatlon, NULL);
        }
	
    }
    XtFree (newlatlon);
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_timesCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_timesCb							*
 *									*
 * Callback function for the time text widgets.				*
 *									*
 * void pgsigw_timesCb (wid, clnt, cbs)					*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		which time			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/99	initial coding				*
 ***********************************************************************/
{
    int		dtg, day, hour, minute;
    char	*newtime;
/*---------------------------------------------------------------------*/

    newtime = XmTextGetString (_timesText[clnt]);

    sscanf (newtime, "%d", &dtg);
    day = dtg / 10000;
    hour = ((int) (dtg / 100)) % 100;
    minute = dtg % 100;

    if ((1 <= day    && day <= 31) &&
	(0 <= hour   && hour <= 23) &&
	(0 <= minute && minute <= 59)) {

	dtg = (day * 10000) + (hour * 100) + minute;

	if (clnt == 0 || dtg > _currTimes[0]) {
	    _currTimes[clnt] = dtg;
	}
    }

    sprintf (newtime, "%6.6d", _currTimes[clnt]);
    XtVaSetValues (_timesText[clnt], XmNvalue, newtime, NULL);

    XtFree (newtime);
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_validTimeCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_validTimeCb							*
 *									*
 * Callback function for start plus push buttons			*
 *									*
 * void pgsigw_validTimeCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		which time			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		11/99	initial coding				*
 * A. Hardy/NCEP	 6/03   added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 ***********************************************************************/
{    
    int         iyear, imon, iday, ihour, imin, isec, julian, ier;
    int		dtg, tintv, itype = 1;
    int         etime[5];
    char	*newtime, tmzn[4];
/*---------------------------------------------------------------------*/

    css_date ( &itype, &iyear, &imon, &iday, &ihour, &imin, &isec, &julian, 
                  tmzn, &ier);

    newtime = XmTextGetString (_timesText[0]);

    sscanf (newtime, "%d", &dtg);

    etime[0]=iyear; etime[1]=imon; etime[2]=dtg / 10000;
    etime[3]=((int) (dtg / 100)) % 100; etime[4]= dtg % 100;

    tintv = (int)clnt * 60;
    ti_addm(etime, &tintv, etime, &ier);

    dtg = (etime[2] * 10000) + (etime[3] * 100) + etime[4];
    _currTimes[1] = dtg;
    
    sprintf (newtime, "%6.6d", _currTimes[1]);
    XtVaSetValues (_timesText[1], XmNvalue, newtime, NULL);

    XtFree (newtime);
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_phenPbCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgsigw_phenPbCb							*
 *									*
 * Callback function for the phenomenon buttons.			*
 *									*
 * void pgsigw_phenPbCb (wid, which, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   which		long		which button			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	08/00	modified from pgutls_optPbCb		*
 * F. J. Yen/NCEP	10/00	Add TC; sensitize loc widgets as needed *
 * H. Zeng/EAI          10/00   Added updating of phenomenon label      *
 * H. Zeng/XTRIA        11/02   modified for NxmVolcano.c               *
 ***********************************************************************/
{
    int         lenstr, iret;
    char	tempch[MAXTBLNAME], attrib_string[100];
    XmString    xmstr;
/*---------------------------------------------------------------------*/

    _phenStrc.current = (int)which;
    cst_rxbl ( _phen[_vgLoc][which], tempch, &lenstr, &iret );

    sprintf(attrib_string, "%sAttributes:", tempch);
    xmstr = XmStringCreateLtoR(attrib_string, XmFONTLIST_DEFAULT_TAG);
    XtVaSetValues (_firstPhenomLabel, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);

    if ( strcmp( tempch, "VOLCANIC_ASH " ) != 0 ) {
	XtSetSensitive (_phenMenuB,  FALSE );
	XtSetSensitive ( _phLatLab , FALSE );
	XtSetSensitive ( _phLonLab , FALSE );
	XtSetSensitive ( _phenLatLon[0] , FALSE );
	XtSetSensitive ( _phenLatLon[1] , FALSE );
	XtSetSensitive ( _freetxtLab, FALSE );
	XtSetSensitive ( _freetxtText, FALSE );


        if ( strcmp ( tempch, "TROPICAL_CYCLONE " ) == 0 ) {

            XtManageChild(_extendedAttribForm); 
            pgsigw_volcPbCb(NULL, 0, NULL);              

            if (!(XtIsManaged (_basicAttrib2Form)) ) {
	        XtManageChild (_basicAttrib2Form);
            }    

	    XtSetSensitive ( _presLab, TRUE );
	    XtSetSensitive ( _presHPA, TRUE );
	    XtSetSensitive ( _maxwndLab, TRUE );
	    XtSetSensitive ( _maxWinds, TRUE );
	    XtSetSensitive ( _freetxtLab, TRUE );
	    XtSetSensitive ( _freetxtText, TRUE );
	    if ( _subType != SIGTYP_ISOL ) {
	        XtSetSensitive ( _phLatLab , TRUE );
	        XtSetSensitive ( _phLonLab , TRUE );
	        XtSetSensitive (_phenLatLon[0], TRUE);
	        XtSetSensitive (_phenLatLon[1], TRUE);
	    }

	}
	else {

            if (XtIsManaged (_basicAttrib2Form)) {
	        XtUnmanageChild (_basicAttrib2Form);
            }

	    XtSetSensitive ( _presLab, FALSE );
	    XtSetSensitive ( _presHPA, FALSE );
	    XtSetSensitive ( _maxwndLab, FALSE );
	    XtSetSensitive ( _maxWinds, FALSE );
	    XtSetSensitive ( _freetxtLab, FALSE );
	    XtSetSensitive ( _freetxtText, FALSE );

            XtUnmanageChild(_extendedAttribForm);        
	}
    }
    else {
        XtManageChild(_extendedAttribForm);        
 
	XtSetSensitive ( _phenMenuB,  TRUE );
	XtSetSensitive ( _freetxtLab, TRUE );
	XtSetSensitive ( _freetxtText, TRUE );

        if (XtIsManaged (_basicAttrib2Form)) {
	    XtUnmanageChild (_basicAttrib2Form);
        }

	XtSetSensitive ( _presLab, FALSE );
	XtSetSensitive ( _presHPA, FALSE );
	XtSetSensitive ( _maxwndLab, FALSE );
	XtSetSensitive ( _maxWinds, FALSE );
	XtSetSensitive ( _phLatLab , TRUE );
	XtSetSensitive ( _phLonLab , TRUE );
	XtSetSensitive ( _phenLatLon[0] , TRUE );
	XtSetSensitive ( _phenLatLon[1] , TRUE );
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_phen2PbCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgsigw_phen2PbCb							*
 *									*
 * Callback function for the second phenomenon buttons.			*
 *									*
 * void pgsigw_phen2PbCb (wid, which, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   which		long		which button			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          10/00   Copied from pgsigw_phen2PbCb()          *
 ***********************************************************************/
{
    _phen2Strc.current = (int)which;
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_volcPbCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgsigw_volcPbCb							*
 *									*
 * Callback function for the volcanic name buttons.			*
 *									*
 * void pgsigw_volcPbCb (wid, which, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   which		long		which button			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	08/00	modified from pgsigw_phenPbCb		*
 * F. J. Yen/NCEP	10/00	sensitized location widgets as needed	*
 * H. Zeng/EAI          10/00   modified for the new text menu          *
 * H. Zeng/XTRIA        11/02   modified for NxmVolcano.c               *
 ***********************************************************************/
{
    float   lat, lon;
    char    name[33];
/*---------------------------------------------------------------------*/

    _volcIdx = (int)which - 1;

    switch (which) {

           case 0:

                XtVaSetValues(_phennamText,
                              XmNvalue,                   "\0",
                              XmNeditable,                TRUE,
                              XmNcursorPositionVisible,   TRUE,
                              NULL  );
       
	        XtSetSensitive ( _phLatLab , TRUE );
	        XtSetSensitive ( _phLonLab , TRUE );

                XmTextSetString( _phenLatLon[0],  "\0" );
                XmTextSetString( _phenLatLon[1],  "\0" );
	        XtSetSensitive ( _phenLatLon[0] , TRUE );
	        XtSetSensitive ( _phenLatLon[1] , TRUE );
                break;

           default:

                NxmVolcano_getInfo( _volcIdx, &lat, &lon, name );
                XtVaSetValues (_phennamText, 
                               XmNvalue,                   name, 
                               XmNeditable,                FALSE,
                               XmNcursorPositionVisible,   FALSE,
                               NULL );

	        XtSetSensitive ( _phLatLab , FALSE );
	        XtSetSensitive ( _phLonLab , FALSE );

                XmTextSetString( _phenLatLon[0],  "\0" );
                XmTextSetString( _phenLatLon[1],  "\0" );
	        XtSetSensitive ( _phenLatLon[0] , FALSE );
	        XtSetSensitive ( _phenLatLon[1] , FALSE );
                break;
    }
    XtSetSensitive ( _presLab, FALSE );
    XtSetSensitive ( _presHPA, FALSE );
    XtSetSensitive ( _maxwndLab, FALSE );
    XtSetSensitive ( _maxWinds, FALSE );
}

/*=====================================================================*/

void pgsigw_ctlPbCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_ctlPbCb							*
 *									*
 * Callback function for the control buttons.				*
 *									*
 * void pgsigw_ctlPbCb (wid, clnt, cbs)					*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		which button			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/99	initial coding				*
 * S. Law/GSC		09/99	changed to use _editCbFunc		*
 * S. Law/GSC		09/99	removed format widget			*
 * S. Law/GSC		09/99	apply added to save			*
 * M. Li/GSC		10/99	modified clo_dltln code			*
 * D.W.Plummer/NCEP	11/99	stripped "&" from pgsigw_getSIGMET call	*
 * M. Li/GSC		05/00	added pgsigw_setState			*
 * M. Li/GSC		05/00	added pgsigw_popdown to SAVE		*
 * S. Law/GSC		05/00	fixed from line updating		*
 * M. Li/GSC		06/00	moved pgsigw_setState from APPLY TO SAVE*
 * T. Piper/GSC		11/00	increased size of newsig from 768	*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * J. Lewis/AWC		05/07	added call to pgsigw_setFIRs		*
 ***********************************************************************/
{
    int		ii, dcn, ier, near;
    float	*dcx, *dcy, cenlat, cenlon, distance;
    float	lat[MAX_SIGMET], lon[MAX_SIGMET];
    float	newx[MAX_SIGMET], newy[MAX_SIGMET];
    char	fname[50], newsig[100*MAX_SIGMET];
    float	p45 = 45.0F, p135 = 135.0F, p225 = 225.0F, p315 = 315.0F;
/*---------------------------------------------------------------------*/

    switch (clnt) {
      case 0:		/* Save */
	pgsigw_ctlPbCb (wid, 1, cbs);

	if (_vgType == SIGCONV_ELM )
	    pgsigw_setState(_currPts, _currLat, _currLon);

	if (_vgType == SIGINTL_ELM )
	    pgsigw_setFIRs(_currPts, _currLat, _currLon);
        
	    pgsigw_getSIGMET ( newsig );

        if (strlen(newsig) > 0) {
	        XmTextSetString (_newsigText, newsig);
	        pgsigw_getFname (fname);
	        XmTextSetString (_svfileText, fname);
        }

	    pgsigw_popdown ();

        if (strlen(newsig) > 0) {
	        XtManageChild (_svfileForm);
        }
        else {
	        _editCbFunc (NULL, &one, NULL);
        }

	    break;

      case 1:		/* Apply */
	if (_origSubType != _subType) {
	    pgactv_getDevPts (&dcn, &dcx, &dcy);

	    if (_origSubType == SIGTYP_ISOL) {	/* moving from a point */
		dcn = 1;

		gtrans (sys_D, sys_M, &dcn, dcx, dcy, &cenlat, &cenlon, 
			&ier, strlen (sys_D), strlen (sys_M));

		distance = _currDist * NM2M;	/* convert to meters */

		if (_subType == SIGTYP_LINE) {	/* to a line */
		    /*
		     * 3 pt diagonal with original point as middle pt
		     */
		    clo_dltln (&cenlat, &cenlon, &distance, &p315, 
			       &lat[0], &lon[0], &ier);

		    lat[1] = cenlat;
		    lon[1] = cenlon;

		    clo_dltln (&cenlat, &cenlon, &distance, &p135, 
			       &lat[2], &lon[2], &ier);

		    dcn = 3;
		}
		else {				/* to an area */
		    /*
		     * square centered around original point
		     */
		    clo_dltln (&cenlat, &cenlon, &distance, &p45, 
			       &lat[0], &lon[0], &ier);

		    clo_dltln (&cenlat, &cenlon, &distance, &p135, 
			       &lat[1], &lon[1], &ier);

		    clo_dltln (&cenlat, &cenlon, &distance, &p225, 
			       &lat[2], &lon[2], &ier);

		    clo_dltln (&cenlat, &cenlon, &distance, &p315, 
			       &lat[3], &lon[3], &ier);

		    dcn = 4;
		}

		gtrans (sys_M, sys_D, &dcn, lat, lon, newx, newy, 
			&ier, strlen (sys_M), strlen (sys_D));
	    }
	    else if (_subType == SIGTYP_ISOL) {	/* moving to a point */
		near = pgactv_getNearPt ();
		newx[0] = dcx[near];
		newy[0] = dcy[near];
		dcn = 1;

		gtrans (sys_D, sys_M, &dcn, newx, newy, lat, lon, 
			&ier, strlen (sys_D), strlen (sys_M));
	    }
	    else {				/* line <-> area */
		for (ii = 0; ii < dcn; ii++) {
		    newx[ii] = dcx[ii];
		    newy[ii] = dcy[ii];
		}

		gtrans (sys_D, sys_M, &dcn, newx, newy, lat, lon, 
			&ier, strlen (sys_D), strlen (sys_M));
	    }

	    pgsigw_setFrom (dcn, lat, lon);

	    pgactv_deleteAll ();
	    pgactv_addPts (newx, newy, dcn, 0, &ier);
	}

	_origSubType = _subType;

	_editCbFunc (NULL, 0, NULL);

	break;

      case 2:		/* Cancel */
	_editCbFunc (NULL, &one, NULL);
	pgsigw_popdown ();
	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_svfilePbCb ( Widget wid, long clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_svfilePbCb							*
 *									*
 * Callback function for the control buttons.				*
 *									*
 * void pgsigw_svfilePbCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		long		which button			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/99	initial coding				*
 * S. Law/GSC		09/99	changed to use _editCbFunc		*
 * S. Law/GSC		09/99	removed format widget			*
 * T. Piper/SAIC	06/04	added cvg_getoutdir			*
 * T. Piper/SAIC	09/04	replace fopen with cfl_wopn		*
 ***********************************************************************/
{
    char	*fname, *newsig, mesg[128];
    FILE	*fp;
    int		ier;
    Widget      draw_w;
    /*---------------------------------------------------------------------*/

    switch (clnt) {
      case 0:		/* Issue */
	_editCbFunc (NULL, 0, NULL);

	fname = XmTextGetString (_svfileText);
	fp = cfl_wopn ( cvg_getoutdir ( "SIG_FILE_DIR", fname), &ier);
	if (ier < 0) {
        /*
         *  can't create a SIGMET output file
         */
            NxmErr_update();

            draw_w = (Widget)mcanvw_getDrawingW();
            sprintf(mesg,
              "Unable to create  %s  file -- no write permission", 
				cvg_getoutdir ( "SIG_FILE_DIR", fname));
            NxmWarn_show (draw_w, mesg);
        }
        else {

	    newsig = XmTextGetString (_newsigText);
	    fputs (newsig, fp);
	    XtFree (newsig);

	    fclose (fp);

	    _editCbFunc (NULL, &one, NULL);
	    pgsigw_popdown ();
	}
	XtFree (fname);
	break;

      case 1:		/* Cancel */
	_editCbFunc (NULL, &one, NULL);
	pgsigw_popdown ();
	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_levelInfo0Cb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_levelInfo0Cb							*
 *									*
 * This is the callback function for level info. option menu 0.		*
 *									*
 * void pgsigw_levelInfo0Cb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget calling this function	*
 *	clnt		XtPointer	new type			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   initial coding                          *
 ***********************************************************************/
{
    _levelInfoOp0.current = (long)clnt;
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_levelInfo1Cb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_levelInfo1Cb							*
 *									*
 * This is the callback function for level info. option menu 1.		*
 *									*
 * void pgsigw_levelInfo1Cb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget calling this function	*
 *	clnt		XtPointer	new type			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   initial coding                          *
 ***********************************************************************/
{
    _levelInfoOp1.current = (long)clnt;
}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_levelInfo2Cb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_levelInfo2Cb							*
 *									*
 * This is the callback function for level info. option menu 2.	        *
 *									*
 * void pgsigw_levelInfo2Cb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget calling this function	*
 *	clnt		XtPointer	new type			*
 *	cbs		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/00   initial coding                          *
 ***********************************************************************/
{
    _levelInfoOp2.current = (long)clnt;
    if(_levelInfoOp2.current == 0) {
        XmTextFieldSetString(_levelInfoText1, "" );
    }

}

/*=====================================================================*/
/* ARGSUSED */
void pgsigw_phennamCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgsigw_phennamCb							*
 *									*
 * This is the callback function for entered phenomenon name in the	*
 * text widget.								*
 *									*
 * void pgsigw_phennamCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget calling this function	*
 *	clnt		XtPointer	not used			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * F. J. Yen/NCEP       08/00   Created					*
 * F. J. Yen/NCEP       10/00   Sensitize location widgets as needed	*
 ***********************************************************************/
{
    char	*vnam, newstr[MAX_PHENMSTR];
/*---------------------------------------------------------------------*/

    vnam = XmTextGetString (_phennamText);

    sscanf (vnam, "%s", newstr);

    sscanf (vnam, "%s", _currPhenam);

    XtFree (vnam);

    if (strcmp(_currPhenam,"-Not_listed.__Enter_Name/Location- ") == 0) {
	XtSetSensitive ( _phLatLab , TRUE );
	XtSetSensitive ( _phLonLab , TRUE );
	XtSetSensitive ( _phenLatLon[0] , TRUE );
	XtSetSensitive ( _phenLatLon[1] , TRUE );
    }
    else {
	XtSetSensitive ( _phLatLab , FALSE );
	XtSetSensitive ( _phLonLab , FALSE );
	XtSetSensitive ( _phenLatLon[0] , FALSE );
	XtSetSensitive ( _phenLatLon[1] , FALSE );
    }
    XmTextSetString (_phennamText, newstr);
}

/*=====================================================================*/

void pgsigw_getSIGMET ( char *sigmet )
/************************************************************************
 * pgsigw_getSIGMET							*
 *									*
 * Builds the new SIGMET based on currently selected items.		*
 *									*
 * void pgsigw_getSIGMET (sigmet)					*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*sigmet		char	SIGMET string				*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	initial coding				*
 * E. Safford/GSC	08/99	fix error in format length		*
 * D.W.Plummer/NCEP	 9/99	Add encoding for international sigmet	*
 * S. Law/GSC		09/99	increased line2x array size		*
 * D.W.Plummer/NCEP	11/99	Added "FROM" to output text		*
 * D.W.Plummer/NCEP	11/99	bug fix for non-intl SIGMET build	*
 * D.W.Plummer/NCEP	12/99	added call to cst_wrap			*
 * H. Zeng/EAI          01/00   modified for "LEVEL INFO"               *
 * D.W.Plummer/NCEP	 2/00	changed calling sequence of cst_wrap	*
 * D. Kidwell/NCEP	 4/00	added WMO and AFOS header lines         *
 * D. Kidwell/NCEP	 4/00	fixed header lines if two FIRs given    *
 * M. Li/GSC		 5/00	added the State list to CONV output	*
 * F. J. Yen/NCEP	 8/00	Added volcanic ash.  Removed space in	*
 *				level. Prefixed flight levels with "FL".*
 * F. J. Yen/NCEP	10/00	Added TC.  Removed space btn speed and	*
 *				"KT".  Added comments to change wmo hdr	*
 *				for VA. Implement location for VA & TC.	*
 * T. Piper/GSC		03/01	Fixed IRIX6 compiler warnings		*
 * D.W.Plummer/NCEP	 5/01	Added Int'l AFOS hdr option processing	*
 * D.W.Plummer/NCEP	 6/01	Added chk for "-none-" as TREND option	*
 * E. Safford/SAIC	12/01	use NO_VAL instead of "-9999" string    *
 * H. Zeng/XTRIA        11/02   modified for NxmVolcano.c               *
 * F. J. Yen/NCEP	 6/03	added headers for PHFO, PGUM, & PANC	*
 * A. Hardy/NCEP	 6/03	added tmzn to CSS_DATE			*
 * F. J. Yen/NCEP	 1/04	inserted from string for VOR intl SIGMET* 
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 * J. Lewis/AWC         02/05   added chk for using new WMO headers     *
 * T. Piper/SAIC	05/05	increased size of idnode, initialized	*
 *				strings and fixed logic ( moved } )	*
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * J. Wu/SAIC		04/06	add para in cst_wrap for indentation	*
 * J. Lewis/AWC		05/07	pgsigw_getFIRstr -> pgsigw_setFIRs	*
 * J. Lewis/AWC		05/07	remove logic for using old WMO headers	*
 * J. Lewis/AWC         09/07   changed 12-hr outlk time to 6-hr fcst;  *
 *                              removed date from current position and  *
 *                              fcst time strings;                      *
 *                              Use phenom lat/lon for position of TC;  *
 *                              Misc format changes to FORECAST section *
 * S. Jacobs/NCEP	 3/10	Changed KZOA to KZAK			*
 * S. Jacobs/NCEP	 3/10	Added FIR ID to info line2		*
 * S. Jacobs/NCEP	 9/10	Removed FIR ID from line2		*
 * S. Jacobs/NCEP	 9/10	Removed AREA BOUNDED BY from product	*
 * S. Guan/NCEP         11/16   Changed KZNY to KZWY                    *
 * B. Hebbard/NCEP      12/18   Jira NAWIPS-13:  In case where "SIGMET  *
 *                              polygon does not intersect any FIRs for *
 *                              selected MWO", show error dialog        *
 *                              instead of producing incorrectly        * 
 *                              formatted SIGMET                        *
 * B. Hebbard/NCEP      10/19   Jira NAWIPS-112:  Per AWC, exempt       *
 *                              MDCS MKJK MMFO MMFR MUFH TTZP from      *
 *                              above (NAWIPS-13-added) error message   *
 *                              since AWC has these as valid use cases  *
 *                              in which the resulting formatting is    *
 *                              fixed by AWC's builder software.        *
 ***********************************************************************/
{
const int line_len=65;
int	ii, nn, len, ier, inum, lenstr, iret;
int	dtg, day, idtarr[5], addmin, daydiff;
int     iyear, imon, iday, ihour, imin, isec, julian;
int	itype = 1;
float   lat, lon;
char	wmo[100], afospil[100], hdrocn[3], idinit[2], name[33];
char    blank[2]={' '}, awpphen[5];
char    hdrwmo[3], hdrawp[3], idnode[4];
char	*temp_str, *ptr, stime_str[20], etime_str[20];
char	line1[512], line2[1536+2*15*MAX_SIGMET];
char	line2x[512+15*MAX_SIGMET], line2y[512+15*MAX_SIGMET];
char	from_line[15*MAX_SIGMET], area[5], id[MAXTBLNAME];
char	phen[MAXTBLNAME], volcn[MAXTBLNAME];
char    phen2[MAXTBLNAME], spdns[MAX_SPDSTR], wmophen;
char	rem[MAXTBLNAME], fir[MAXTBLNAME], move[MAXTBLNAME];
char	trend[MAXTBLNAME], tops[MAXTBLNAME], freetext[MAX_FTEXTSTR];
char	phenlat[MAX_LATSTR], phenlon[MAX_LONSTR], chpres[7], chmaxwnd[7];
char	dir[5][15]={"EITHER SIDE OF",
		    "NORTH OF","SOUTH OF","EAST OF","WEST OF"};
char	tempch[MAXTBLNAME], fcsttim[8], loctim[8], *cptr, tmzn[4];
Boolean entvname, tc;
XmString  xmstr;
/*---------------------------------------------------------------------*/

/* 
 * This function prepares the actual sigmet text.
 */

    switch ( _vgType )  {

	case	SIGINTL_ELM:		/* International SIGMETs */

/*
 *  Get FIRs, start time, id, phenomenon, volcano name
 *  and remarks
 */
            pgsigw_setFIRs ( _currPts, _currLat, _currLon );
            strcpy ( fir, _currFIR );
           
            strncpy ( area, _area[_vgLoc][_areaStrc.current], 4 );
            area[4] = '\0';

            temp_str = XmTextGetString ( _timesText[0] );
            strcpy ( stime_str, temp_str );
            XtFree (temp_str);
            temp_str = XmTextGetString ( _timesText[1] );
            strcpy ( etime_str, temp_str );
            XtFree (temp_str);

            strcpy ( id, _ids[_vgLoc][_idsStrc.current] );

            strcpy ( phen, _phen[_vgLoc][_phenStrc.current] );
	    cst_rxbl ( phen, phen, &lenstr, &iret );

	    phenlat[0] = '\0';
	    phenlon[0] = '\0';
	    if ( strcmp ( phen, "VOLCANIC_ASH " ) == 0 ) {

                wmophen = 'V';
  
	        if ( _volcIdx != -1 ) {
                    NxmVolcano_getInfo( _volcIdx, &lat, &lon, name );
                    strcpy ( volcn, name );
		    entvname = FALSE;
	        }
		else {
		    temp_str = XmTextGetString ( _phennamText );
		    strcpy ( volcn, temp_str );
                    XtFree (temp_str);
		    cst_lcuc ( volcn, volcn, &iret );
		    strcat ( volcn, " " );
		    temp_str = XmTextGetString ( _phenLatLon[0] );
		    strcpy ( phenlat, temp_str );
                    XtFree (temp_str);
		    temp_str = XmTextGetString ( _phenLatLon[1] );
		    strcpy ( phenlon, temp_str );
                    XtFree (temp_str);
		    entvname = TRUE;

		}
	    }

	    else if ( strcmp ( phen, "TROPICAL_CYCLONE " ) == 0 ) {
                
		wmophen = 'C';

                strcpy ( phen2, _phen2[_vgLoc][_phen2Strc.current] );
	        cst_rxbl ( phen2, phen2, &lenstr, &iret );
		temp_str = XmTextGetString ( _phennamText );
		strcpy ( volcn, temp_str );
                XtFree (temp_str);
		cst_lcuc ( volcn, volcn, &iret );
		strcat ( volcn, " " );
		entvname = TRUE;
		temp_str = XmTextGetString ( _phenLatLon[0] );
		strcpy ( phenlat, temp_str );
		XtFree (temp_str);
		temp_str = XmTextGetString ( _phenLatLon[1] );
		strcpy ( phenlon, temp_str );
		XtFree (temp_str);
	    }

            else {

                wmophen = 'S';

            }

	    if ( strcmp ( phenlat, NO_VAL ) == 0 ){
	 	phenlat[0] = '\0';
	    }
	    if ( strcmp ( phenlon, NO_VAL ) == 0 ){
	 	phenlon[0] = '\0';
	    }
	    if ( strcmp ( phen, "VOLCANIC_ASH " ) == 0 ||
		     strcmp ( phen, "TROPICAL_CYCLONE " ) == 0) {
		temp_str = XmTextGetString ( _freetxtText );
		strcpy ( freetext, temp_str );
                XtFree (temp_str);

		cst_rpst ( freetext, "\n", " ", freetext, &iret );
		cst_lcuc (freetext, freetext, &iret);

/*
 * Prepare forecast time:  6 hours from
 * valid start time.
 */
		sscanf (stime_str, "%d", &dtg);
		day = dtg / 10000;
		css_date ( &itype, &iyear, &imon, &iday, &ihour, &imin,
			   &isec, &julian, tmzn, &iret);
		daydiff = day - iday;
		if ( (int)G_ABS((float)daydiff) > 10 ) {
		    if ( daydiff > 0 ) {
			if ( imon == 1 ) {
			    imon = 12;
			    iyear = iyear - 1;
			}
			else {
			    imon = imon - 1;
			}
		    }
		    else {
			if ( imon == 12 ) {
			    imon = 1;
			    iyear = iyear + 1;
			}
			else {
			    imon = imon + 1;
			}
		    }
		}
		idtarr[0] = iyear;
		idtarr[1] = imon;
		idtarr[2] = day;
		idtarr[3] = ((int) (dtg / 100)) % 100;
		idtarr[4] = dtg % 100;
		dtg = (idtarr[3] * 100) + idtarr[4];
		sprintf ( loctim, "%4.4d", dtg);
		addmin = FCSTTIM_INTV;
		ti_addm ( idtarr, &addmin, idtarr, &iret);
		dtg = (idtarr[3] * 100) + idtarr[4];
		sprintf ( fcsttim, "%4.4d", dtg);
		
	    }

            strcpy ( rem, _rem[_vgLoc][_remStrc.current] );

/*  
 * Save level info. into string tops
 */
            XtVaGetValues(_levelInfoOp0.pb[_levelInfoOp0.current],
                          XmNlabelString,         &xmstr,
                          NULL );
            XmStringGetLtoR(xmstr, 
                          XmFONTLIST_DEFAULT_TAG, &temp_str);     
            strcpy(tops, temp_str);
            strcat(tops, " ");
            XmStringFree(xmstr);
            XtFree(temp_str);

            XtVaGetValues(_levelInfoOp1.pb[_levelInfoOp1.current],
                          XmNlabelString,         &xmstr,
                          NULL );
            XmStringGetLtoR(xmstr, 
                            XmFONTLIST_DEFAULT_TAG, &temp_str);     
            strcat(tops, temp_str);
	    strcat(tops, " ");
	    XmStringFree(xmstr);
	    XtFree(temp_str);

            temp_str = XmTextGetString(_levelInfoText0);
	    cst_rmbl (temp_str, temp_str, &lenstr, &iret);
	    strcat(tops, "FL");

            strcat(tops, temp_str);

            strcat(tops, " ");

            XtFree (temp_str);

            XtVaGetValues(_levelInfoOp2.pb[_levelInfoOp2.current],
                          XmNlabelString,         &xmstr,
                          NULL );
            XmStringGetLtoR(xmstr, 
                            XmFONTLIST_DEFAULT_TAG, &temp_str);     
            strcat(tops, temp_str);
            strcat(tops, " ");
            XmStringFree(xmstr);
            XtFree(temp_str);

	    strcat(tops, "FL");
            temp_str = XmTextGetString(_levelInfoText1);
	    cst_rmbl ( temp_str, temp_str, &lenstr, &ier );
            strcat(tops, temp_str);

            strcat(tops, " ");
	    cst_rxbl ( tops, tops, &len, &ier );
	    if ( strncmp ( tops, "-none-", 6 ) == 0 )  {
		tops[0] = '\0';
	    }
	    else  {
		ier = 0;
		while ( ier == 0 )  cst_rmst(tops, "-none- FL", &len, tops,
			&ier);
	        cst_rxbl ( tops, tops, &len, &ier );
	    }
	    cst_lcuc ( tops, tops, &ier );
	    if ( tops[len-1] == ' ' && len > 0 ) {
		len--;
	  	tops[len] = '\0';
	    } 
            XtFree (temp_str);

/*
 *  Create WMO and AWIPS header lines.
 */
	    idinit[1] = '\0';
	    idinit[0] = id[0];
	    inum      = idinit[0] - 64;

/*
 *  Set variable for AWIPS header based on phenomenon
 */
            if ( wmophen == 'S' ) {
                strcpy ( awpphen, "SIG" );
            }
            else if ( wmophen == 'V' ) {
                strcpy ( awpphen, "WSV" );
            }
            else {
                strcpy ( awpphen, "WST" );
            }

/*
 *  Set variables for headers based on WMO area
 */
	    if ( ( strncmp ( area, "KKCI", 4 ) == 0 ) ||
	         ( strncmp ( area, "KNHC", 4 ) == 0 ) ) {

	        if ( ( strncmp ( fir, "KZHU", 4 ) == 0 ) ||
	             ( strncmp ( fir, "KZMA", 4 ) == 0 ) ||
	             ( strncmp ( fir, "KZWY", 4 ) == 0 ) ||
	             ( strncmp ( fir, "TJZS", 4 ) == 0 ) ) {
	            strcpy ( hdrocn, "NT" );
	            strcpy ( hdrwmo, "NT" );
	            strcpy ( hdrawp, "A0" );
	        }
	        else if ( ( strncmp ( fir, "KZAK", 4 ) == 0 ) ||
	                  ( strncmp ( fir, "PAZA", 4 ) == 0 ) ) {
	            strcpy ( hdrocn, "PN" );
	            strcpy ( hdrwmo, "PN" );
	            strcpy ( hdrawp, "P0" );
	        }
	        else if ( ( strncmp ( fir, "MDCS", 4 ) == 0 ) ||
	                  ( strncmp ( fir, "MKJK", 4 ) == 0 ) ||
	                  ( strncmp ( fir, "MMFO", 4 ) == 0 ) ||
	                  ( strncmp ( fir, "MMFR", 4 ) == 0 ) ||
	                  ( strncmp ( fir, "MUFH", 4 ) == 0 ) ||
	                  ( strncmp ( fir, "TTZP", 4 ) == 0 ) ) { 
	            hdrocn[0] = '\0'; 
	            hdrwmo[0] = '\0'; 
	            hdrawp[0] = '\0'; 
	            strcpy(sigmet,""); 
	        }
	        else {
                /*
                 *  Error:  SIGMET polygon does not intersect any FIRs for selected MWO
                 */
                    NxmErr_update();
                    NxmWarn_show ((Widget)mcanvw_getDrawingW(), 
                            "SIGMET polygon does not intersect any FIRs for selected MWO");
	            hdrocn[0] = '\0';
		    hdrwmo[0] = '\0';
		    hdrawp[0] = '\0';
	            strcpy(sigmet, "");
                    return;	
	        }

            if ( strncmp ( area, "KKCI", 4 ) == 0 ) { 
                strcpy ( idnode, "MKC" );
            }
            else {
                strcpy ( idnode, "NHC" );
            }
        } 
        else if ( strncmp ( area, "PHFO", 4 ) == 0 ) {
            strcpy ( hdrwmo, "PA" );
            strcpy ( hdrawp, "PA" );
            strcpy ( idnode, "HFO" );
	        inum = idinit[0] - 77;
        }
        else if ( strncmp ( area, "PAWU", 4 ) == 0 ) {
            strcpy ( hdrwmo, "AK");
            strcpy ( hdrawp, "AK");
            strcpy ( idnode, "ANC");
		    inum = idinit[0] - 72;
        }
        else {
		    hdrwmo[0] = '\0';
		    hdrawp[0] = '\0';
		    idnode[0] = '\0';
        }
            
            sprintf ( wmo, "W%c%s%02d %s %s",
           	     wmophen, hdrwmo, inum, area, stime_str );
	    strcat ( wmo, NL );

            if ( strncmp ( area, "PAWU", 4 ) == 0 ) {
	        sprintf ( afospil, "%s%s%d\n%s%c WS %s", awpphen, hdrawp, 
						inum, idnode, id[0], stime_str );
            }
            else {
                sprintf ( afospil, "%s%s%s", awpphen, hdrawp, idinit );
            }

	    strcat ( afospil, NL );

/*
 *  Create first text line.
 */
            sprintf( line1, "%s SIGMET %s %-d VALID %s/%s %-4s-", 
	        fir, id, _currSequence, stime_str, etime_str, area );
	    strcat ( line1, NL );

/*
 *  remove any extra blanks and underscores.
 */
	    cst_rxbl ( line1, line1, &len, &ier );
	    len = (int)strlen ( line1 );
	    for ( nn = 0; nn < len; nn++ )  {
		if ( line1[nn] == '_' )  line1[nn] = ' ';
	    }

/*
 *  Get "FROM" line.
 */
	    XtVaGetValues(_fromText, XmNlabelString, &xmstr, NULL); 
	    XmStringGetLtoR(xmstr, XmFONTLIST_DEFAULT_TAG, &temp_str);
	    XmStringFree(xmstr);
            strcpy ( from_line, temp_str );
            XtFree (temp_str);

/*
 *  Generate second text line, depending on the subtype.
 *
 *  Start with FIR informational string(s).
 */
	    line2[0] = '\0';
/* 	    strcat ( line2, fir ); */
/* 	    strcat ( line2, " " ); */
	    ptr = strtok ( fir, " " );
	    while ( ptr != (char *)NULL )  {
		for ( ii = 0; ii < _nFIR; ii++ )  {
		    if ( strcmp ( ptr, _FIR[ii] ) == 0 )  {
			strcat ( line2, _FIRinfo[ii] );
			strcat ( line2, " FIR " );
		    }
		}
		ptr = strtok ( NULL, " " );
	    }

/*
 *  Next add phenomenon.
 */
	    cst_rxbl ( phen, tempch, &lenstr, &iret );

	    if ( strcmp ( tempch, "TROPICAL_CYCLONE " ) == 0 ) {
		tc = TRUE;
		if ( entvname ) { 	
    	            sprintf( line2x, "%s ", "TC " );
                    strcat ( line2x, volcn );
                    strcat ( line2x, " " );
		    if ( _currPres != IMISSD ) {
		        sprintf ( chpres, "%d", _currPres );
                        strcat ( line2x, chpres );
		        strcat ( line2x, "HPA " );
		    }
		    if ( _subType == SIGTYP_ISOL ) {
		        strcat ( line2x, "NEAR " );
                        strcat ( line2x, phenlat );
                        strcat ( line2x, phenlon );
		    }
		    else {
		
			if ( entvname && (strcmp ( phenlat, "\0" ) != 0 ) &&
			        (strcmp ( phenlon, "\0") != 0) ) {
		            strcat ( line2x, "NEAR " );
                            strcat ( line2x, phenlat );
                            strcat ( line2x, phenlon );
		        }

		    }
		    strcat ( line2x, " AT " );
		    strcat ( line2x, loctim );
		    strcat ( line2x, "Z." );

/*
 *  add movement.
 */
	            if ( _currMove == 0 )  {
	                sprintf ( move, "  STNR." );
                        strcat ( line2x, move );
	            }
	            else if ( _currMove == 1 )  {
			strcpy ( spdns, _spd[_spdStrc.current]);
	    		cst_rmbl ( spdns, spdns, &lenstr, &iret );
	                sprintf ( move, " MOV %s %sKT.", 
			      _dir[_dirStrc.current], spdns );
                        strcat ( line2x, move );
	            } 

/*
 *  add max winds.
 */
		    if ( _currMaxWind != IMISSD ) {
		        strcat ( line2x, "  MAX WINDS " );
		        sprintf ( chmaxwnd, "%d", _currMaxWind );
                        strcat ( line2x, chmaxwnd );
		        strcat ( line2x, "KT.");
		    }

/*
 *  add trend.
 */
		    cptr = strstr(_trend[_vgLoc][_trendStrc.current],"-none-");
		    if ( cptr == (char *)NULL ) {
	                sprintf (trend,"%s.",_trend[_vgLoc][_trendStrc.current]);
	                cst_rmbl ( trend, trend, &len, &ier );
                        strcat ( line2x, "  " );
                        strcat ( line2x, trend );
		    }
		}
		strcat ( line2x, "  " );
		strcat ( line2x, phen2 );
		strcat ( line2x, " " );
	    }
	    else {
		tc = FALSE;
    	        sprintf( line2x, "%s ", phen );
	    }

	    if ( strcmp ( tempch, "VOLCANIC_ASH " ) == 0 ) {
                strcat ( line2x, " FROM " );
                strcat ( line2x, volcn );
                strcat ( line2x, "." );
		if ( entvname && (strcmp ( phenlat, "\0" ) != 0 ) &&
			(strcmp ( phenlon, "\0") != 0) ) {
		    strcat ( line2x, " LOC " );
                    strcat ( line2x, phenlat );
                    strcat ( line2x, phenlon );
		}
	    }

/*
 *  add FCST level information here.
 */
	    if ( strncmp ( tops, "FCST", 4 ) == 0 )  {
	        if ( strcmp ( tempch, "VOLCANIC_ASH " ) == 0 ) {
		    strcat ( line2x, " " );
		}
                strcat ( line2x, tops );
                strcat ( line2x, "." );
	    }
	    if ( tc ) {

/*
 *  add TOPS level information here.
 */
	    	if ( strncmp ( tops, "TOPS", 4 ) == 0 )  {
               	    strcat ( line2x, " " );
               	    strcat ( line2x, tops );
	    	}
	    }
            switch ( _subType )  {
	
	        case	SIGTYP_ISOL:		/* ISOLATED	*/

		    
		    if ( tc ) {
			sprintf( line2y, " WITHIN %-d NM CENTER.",
			    (int)_currDist);
		    }
		    else {
			sprintf( line2y, " WI %-d NM OF %s.", 
			    (int)_currDist, from_line );
		    }
	            break;

	        case	SIGTYP_LINE:		/* LINE		*/

		    if ( _fmtflag != 2 ) {
			sprintf( line2y, " WI %-d NM %s LINE %s.",
		        	(int)_currDist, dir[_solStrc.current],
				from_line );
		    }
		    else {
			sprintf( line2y, " WI %-d NM %s LINE FM %s.",
		        	(int)_currDist, dir[_solStrc.current],
				from_line );
		    }
	            break;

	        case	SIGTYP_AREA:		/* AREA		*/

		    if ( _fmtflag != 2 ) {
			if ( strcmp ( tempch, "VOLCANIC_ASH " ) == 0 ) {
/* 			  sprintf( line2y, "  VA CLD WI AREA BOUNDED BY %s.", */
/* 				  from_line ); */
			  sprintf( line2y, "  VA CLD WI %s.", from_line );
			}
			else {
/* 			    sprintf( line2y, "  WI AREA BOUNDED BY %s.", */
/* 				    from_line ); */
			    sprintf( line2y, "  WI %s.", from_line );
			}
		    }
		    else {
/*     	                sprintf( line2y, " WI AREA BOUNDED BY LINE FM %s.", */
/* 				from_line ); */
    	                sprintf( line2y, " WI LINE FM %s.", from_line );
		    }

	            break;
        
            }

	    strcat ( line2, " " );
	    strcat ( line2x, line2y );
	    strcat ( line2, line2x );

	    if ( ! tc ) {

/*
 *  add TOPS level information here.
 */
	        if ( strncmp ( tops, "TOPS", 4 ) == 0 )  {
                    strcat ( line2, " " );
                    strcat ( line2, tops );
                    strcat ( line2, "." );
	        }

/*
 *  add movement.
 */
	        if ( _currMove == 0 )  {
	            sprintf ( move, " STNR." );
                    strcat ( line2, move );
	        }
	        else if ( _currMove == 1 )  {
		    strcpy ( spdns, _spd[_spdStrc.current]);
	    	    cst_rmbl ( spdns, spdns, &lenstr, &iret );
	            sprintf ( move, " MOV %s %sKT.", 
			    _dir[_dirStrc.current], spdns );
                    strcat ( line2, move );
		}

/*
 *  add trend.
 */
		cptr = strstr(_trend[_vgLoc][_trendStrc.current],"-none-");
		if ( cptr == (char *)NULL ) {
	            sprintf ( trend, "%s.", _trend[_vgLoc][_trendStrc.current] );
	            cst_rmbl ( trend, trend, &len, &ier );
                    strcat ( line2, " " );
                    strcat ( line2, trend );
		}
	    }

/*
 *  add remarks (if available).
 */
	    cst_rmbl ( rem, rem, &len, &ier );
	    if ( strcmp ( rem, "-none-" ) != 0 )  {
                strcat ( line2, " " );
                strcat ( line2, rem );
                strcat ( line2, "." );
	    }

/*
 *  if volcano ash then add outlook.
 */
	    if ( strcmp ( tempch, "VOLCANIC_ASH " ) == 0 ) {
		strcat ( line2, "  FORECAST " );
		strcat ( line2, fcsttim );
		strcat ( line2, "Z" );
		strcat ( line2, " VA CLD APRX ");
	    	strcat ( line2, freetext );
	    }

/*
 *  if tropical cyclone then add outlook.
 */
	    if ( strcmp ( tempch, "TROPICAL_CYCLONE " ) == 0 ) {
		strcat ( line2, "  FORECAST " );
		strcat ( line2, fcsttim );
		strcat ( line2, "Z" );
		strcat ( line2, " TC CENTER ");
	    	strcat ( line2, freetext );
	    }

            strcat ( line2, NL );

/*
 *  remove any extra blanks and underscores.
 */
	    cst_rxbl ( line2, line2, &len, &ier );
	    len = (int)strlen ( line2 );
	    for ( nn = 0; nn < len; nn++ )  {
		if ( line2[nn] == '_' )  line2[nn] = ' ';
	    }

/*
 *  Finally, combine all lines to make full sigmet.
 */
	    if ( _AFOSflag )  {
	        sprintf ( sigmet, "ZCZC %s%s%s%s%s", 
			idnode, afospil, wmo, line1, line2 );   
                strcat ( sigmet, "NNNN" );
                strcat ( sigmet, NL );
	    }
	    else  {
	        sprintf ( sigmet, "%s%s%s%s", wmo, afospil, line1, line2 );   
	    }

/*
 *  Wrap SIGMET to maximum line_len.
 */
	    cst_wrap ( sigmet, blank, &line_len, NL, (char *)NULL, sigmet, &ier );

/*
 *  Replace temporary NL w/ official EOL.
 */
            cptr = strstr ( sigmet, NL );
            while ( cptr != (char *)NULL )  {
                cst_rpst ( cptr, NL, EOL, cptr, &ier );
                cptr = strstr ( cptr+strlen(EOL), NL );
            }

	    break;

	case	SIGCONV_ELM:

/*
 *  List States and "FROM" line.
 */
            XtVaGetValues(_fromText, XmNlabelString, &xmstr, NULL);
            XmStringGetLtoR(xmstr, XmFONTLIST_DEFAULT_TAG, &temp_str);
            XmStringFree(xmstr);
            strcpy ( from_line, temp_str );
            XtFree (temp_str);
	    sprintf ( sigmet, "%sFROM %s\n", _currST, from_line );

	    break;

	case	SIGAIRM_ELM:	
	case	SIGNCON_ELM:
	case	SIGOUTL_ELM:

/*
 *  Simply list "FROM" line.
 */
            XtVaGetValues(_fromText, XmNlabelString, &xmstr, NULL);
            XmStringGetLtoR(xmstr, XmFONTLIST_DEFAULT_TAG, &temp_str);
            XmStringFree(xmstr);
            strcpy ( from_line, temp_str );
            XtFree (temp_str);

	    sprintf ( sigmet, "FROM %s\n", from_line );

	    break;
    }
}

/*=====================================================================*/

void pgsigw_getFname ( char *fname )
/************************************************************************
 * pgsigw_getFname							*
 *									*
 * Function to generate appropriate SIGMET filename.			*
 *									*
 * void pgsigw_getFname ( fname )					*
 *									*
 * Input parameters:							*
 *   *fname	char	Filename					*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 9/99	initial coding				*
 * S. Law/GSC		09/99	changed SIG_OUTL			*
 ***********************************************************************/
{
    int		ier, len;
    char	ids[MAX_SIGSTR], area[MAX_SIGSTR];
/*---------------------------------------------------------------------*/

    cst_rmbl(  _ids[_vgLoc][ _idsStrc.current],  ids, &len, &ier );
    cst_rmbl( _area[_vgLoc][_areaStrc.current], area, &len, &ier );

    switch ( _vgType )  {
      case	SIGINTL_ELM:		/* International	*/

	sprintf (fname,  "%s_%s_%d.sigintl", area, ids, _currSequence );
	break;

      case	SIGAIRM_ELM:		/* International	*/
      case	SIGNCON_ELM:		/* Non-convective	*/

	sprintf (fname,  "%s_%d.from", ids, _currSequence );
	break;

      case	SIGCONV_ELM:		/* Convective		*/

	sprintf (fname,  "%d%c.from", _currSequence, ids[0] );
	break;

      case	SIGOUTL_ELM:		/* Outlook		*/

	sprintf (fname,  "%dO.from", _currSequence);
	break;

    }
}

/*=====================================================================*/

void pgsigw_rdInfo ( int *iret )
/************************************************************************
 * pgsigw_rdInfo							*
 *									*
 * This function reads the SIGMET information table.			*
 * The table is read in only once.					*
 *									*
 * void pgsigw_rdInfo (iret)						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret	int		Return value				*
 *				 -1 - Unable to open table		*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 6/99	Created					*
 * S. Law/GSC		08/99	changed to read all SIGMET types	*
 * D.W.Plummer/NCEP	 6/99	added Internation options		*
 * F. J. Yen/NCEP	 8/00	added volcanic ash			*
 * F. J. Yen/NCEP	 9/00	added tropical cyclone			*
 * H. Zeng/XTRIA        11/02   modified for NxmVolcano.c               *
 * J. Lewis		05/07	removed processing of fcstr name	*
 ***********************************************************************/
{
    int		ii, ier, vg;
    char	buff[256], fnm[32], catg[12], str[128], vgstr[8];
    char	*ptr;
    FILE	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 *  Open the information table. If not found, return an error.
 */
    strcpy(fnm, INTL_TBL);
    fp = cfl_tbop(fnm, "pgen", &ier);
    if (fp == NULL || ier != 0) {
        *iret = -1;
        return;
    }

/*
 *  Initialize.
 */
    for (ii = 0; ii < MAXNSIGMET; ii++) {
	_nArea[ii]	= 0;
	_nIds[ii]	= 0;
 	_nPhen[ii]	= 0;
 	_nPhen2[ii]	= 0;
	_nTrend[ii]	= 0;
	_nRem[ii]	= 0;
   }
    _nFIR	= 0;
    _nSpd	= 0;
    _nDir	= 0;

/*
 *  Scan table line-by-line.
 */
    while (!feof(fp)) {

	cfl_trln (fp, sizeof(buff), buff, &ier);

	if (ier == 0) {

	    sscanf (buff, "%s %s", vgstr, catg);

	    if (strcmp (vgstr, "AIRM") == 0) {
		vg = 0;
	    }
	    else if (strcmp (vgstr, "CONV") == 0) {
		vg = 1;
	    }
	    else if (strcmp (vgstr, "INTL") == 0) {
		vg = 2;
	    }
	    else if (strcmp (vgstr, "NCON") == 0) {
		vg = 3;
	    }
	    else if (strcmp (vgstr, "OUTL") == 0) {
		vg = 4;
	    }
	    else if (strcmp (vgstr, "ALL") == 0) {
		vg = 0;
	    }
	    else {
		continue;
	    }


	    if ((strcmp (catg, "AREA") == 0) && (_nArea[vg] < MAXNOPT)) {

/*
 *  Process entry for area.
 */
		sscanf ( buff, "%*s %*s %s", str );

		pgsigw_fillStrArray (MAXNOPT, MAX_AREASTR, str, 
				     &(_nArea[vg]), _area[vg]);
	    }
	    else if ((strcmp (catg, "ID") == 0) && (_nIds[vg] < MAXNOPT)) {

/*
 *  Process entry for ids.
 */
		sscanf ( buff, "%*s %*s %s", str );

		pgsigw_fillStrArray (MAXNOPT, MAX_MIDSTR, str, 
				     &(_nIds[vg]), _ids[vg]);
	    }
	    else if ((strcmp (catg, "FIR") == 0) && (_nFIR < MAXNOPT)) {

/*
 *  Process entry for FIRs.
 */
		sscanf ( buff, "%*s %*s %s", str );
	
		pgsigw_fillStrArray (MAXNFIR, MAXFIRSZ, str, &_nFIR, _FIR);
	    }
	    else if ((strcmp (catg, "PHEN") == 0) && (_nPhen[vg] < MAXNOPT)) {

/*
 *  Process entry for phenomenon and second phenomenon.
 */
		sscanf ( buff, "%*s %*s %s", str );

		pgsigw_fillStrArray (MAXNOPT, MAX_PHENSTR, str, 
				     &(_nPhen[vg]), _phen[vg]);

	        if (_nPhen2[vg] < MAXNOPT) {
		    sscanf ( buff, "%*s %*s %s", str );
		    pgsigw_fillStrArray (MAXNOPT, MAX_PHENSTR, str, 
				     &(_nPhen2[vg]), _phen2[vg]);
		}
	    }
	    else if ((strcmp (catg, "SPEED") == 0) && (_nSpd < MAXNOPT)) {

/*
 *  Process entry for speed.
 */
		sscanf ( buff, "%*s %*s %s", str );

		pgsigw_fillStrArray (MAXNOPT, MAX_SPDSTR, str, &_nSpd, _spd);
	    }
	    else if ((strcmp (catg, "DIRECT") == 0) && (_nDir < MAXNOPT)) {

/*
 *  Process entry for direction.
 */
		sscanf ( buff, "%*s %*s %s", str );

		pgsigw_fillStrArray (MAXNOPT, MAX_DIRSTR, str, &_nDir, _dir);
	    }
	    else if ((strcmp (catg,"TREND") == 0) && 
		     (_nTrend[vg] < MAXNOPT)) {

/*
 *  Process entry for trend.
 */
		sscanf ( buff, "%*s %*s %s", str );

		pgsigw_fillStrArray (MAXNOPT, MAX_TRENDSTR, str, 
				     &(_nTrend[vg]), _trend[vg]);
	    }
	    else if ((strcmp (catg, "REM") == 0) && (_nRem[vg] < MAXNREM)) {

/*
 *  Process entry for remarks.
 */
		sscanf ( buff, "%*s %*s %s", str );

		pgsigw_fillStrArray (MAXNREM, 50, str, 
				     &(_nRem[vg]), _rem[vg]);
	    }
	}
    }

    for ( ii = 0; ii < _nFIR; ii++ )  {

	ptr = strtok ( _FIR[ii], "|" );

	if ( ptr != (char *)NULL )  {
	    ptr = strtok ( NULL, "|" );
	    if ( ptr != (char *)NULL )  strcpy ( _FIRinfo[ii], ptr );
	}
    }
    cfl_clos (fp, &ier);
}

/*=====================================================================*/

void pgsigw_fillStrArray ( int max_strarr, int max_string, char origstr[], 
			     int *nstrarr, char strarr[][MAXTBLNAME] )
/************************************************************************
 * pgsigw_fillStrArray							*
 *									*
 * This function copies a string of multiple names delimited by a ';'	*
 * to an array of strings.						*
 *									*
 * void pgsigw_fillStrArray (max_strarr, max_string, origstr, nstrarr, 	*
 *								strarr)	*
 *									*
 * Input parameters:							*
 *	max_strarr	int	maximum array size			*
 *	max_string	int	maximum string length			*
 *	origstr[]	char	the original string			*
 *									*
 * Output parameters:							*
 *	*nstrarr		int	current number in array		*
 *	strarr[][MAXTBLNAME]	char	the array of strings		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/99	moved from pgsigw_rdInfo		*
 ***********************************************************************/
{
    int		ii, last;
    char	*ptr;
/*---------------------------------------------------------------------*/

    ptr = strtok (origstr, ";");
    last = max_string - 1;
    while ((ptr != (char *)NULL) && (*nstrarr < max_strarr)) {
	strncpy (strarr[*nstrarr], ptr, (size_t)last);
	strarr[*nstrarr][last] = '\0';

	for (ii = (int)strlen (ptr); ii < last; ii++) {
	    strarr[*nstrarr][ii] = ' ';
	}

	ptr = strtok( NULL, ";" );	
	(*nstrarr)++;
    }
}

/*=====================================================================*/

void pgsigw_setVgInfo ( void )
/************************************************************************
 * pgsigw_setVgInfo							*
 *									*
 * This function sets various information based on _vgType.		*
 *									*
 * void pgsigw_setVgInfo ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/99	initial coding				*
 * A. Hardy/GSC         12/99   added lat/lon button sensitivity        *
 * S. Law/GSC		12/99	added SIGOUTL_ELM to area-only check	*
 * F. J. Yen/NCEP	08/00	added volcanic ash			*
 * F. J. Yen/NCEP	09/00	add TC; sensitize loc widgets as needed *
 * H. Zeng/EAI          10/00   Added the updating of phenomenon label  *
 * M. Li/SAIC           12/02   radio box -> check box                  *
 * F. J. Yen/NCEP	09/00	updated for selection of VOR _formatPB	*
 ***********************************************************************/
{
    int		lenstr, iret, ii;
    char	tempch[MAXTBLNAME];
    XmString    xmstr;
/*---------------------------------------------------------------------*/

    if (_vgType == SIGAIRM_ELM) {
	_vgLoc = 0;

	xmstr = XmStringCreateLocalized("AIRMET Edit");
     }
    else if (_vgType == SIGCONV_ELM) {
	_vgLoc = 1;

	xmstr = XmStringCreateLocalized("Convective SIGMET Edit");
    }
    else if (_vgType == SIGINTL_ELM) {
	_vgLoc = 2;

	xmstr = XmStringCreateLocalized("International SIGMET Edit");
    }
    else if (_vgType == SIGNCON_ELM) {
	_vgLoc = 3;

	xmstr = XmStringCreateLocalized("Non-convective SIGMET Edit");
    }
    else if (_vgType == SIGOUTL_ELM) {
	_vgLoc = 4;

	xmstr = XmStringCreateLocalized("Convective Outlook Edit");
    }

    XtVaSetValues(_mainForm, XmNdialogTitle, xmstr, NULL);
    XmStringFree(xmstr);

/*
 * Reset phenomenon option menu.
 */
    _phenStrc.current = 0;


    if (_vgType == SIGAIRM_ELM || _vgType == SIGNCON_ELM || 
	_vgType == SIGOUTL_ELM) {
	XtSetSensitive (_typePb[SIGTYP_ISOL], FALSE);
	XtSetSensitive (_typePb[SIGTYP_LINE], FALSE);
	_subType = SIGTYP_AREA;
	for (ii = 0; ii < _maxType; ii++ ) {
            if ( ii == _subType ) {
                XmToggleButtonSetState (_typePb[ii], TRUE, TRUE);
            }
            else {
                XmToggleButtonSetState (_typePb[ii], False, False);
            }
        }

	cst_rxbl ( _phen[_vgLoc][_phenStrc.current], tempch,
		&lenstr, &iret);
	if ( (_subType == SIGTYP_ISOL ) &&
		(strcmp (tempch, "TROPICAL_CYCLONE ") == 0 ) ) {
	    XtSetSensitive (_phenLatLon[0], FALSE);
	    XtSetSensitive (_phenLatLon[1], FALSE);
	}
	else {
	    XtSetSensitive (_phenLatLon[0], TRUE);
	    XtSetSensitive (_phenLatLon[1], TRUE);
	}
    }
    else {
	XtSetSensitive (_typePb[SIGTYP_ISOL], TRUE);
	XtSetSensitive (_typePb[SIGTYP_LINE], TRUE);
    }

    if (_vgType != SIGINTL_ELM ) {
        XtSetSensitive( _formatPb[0], FALSE );
        XtSetSensitive( _formatPb[1], FALSE );
        XtSetSensitive( _formatPb[2], FALSE );
        XmToggleButtonSetState (_formatPb[0], FALSE, FALSE);
        XmToggleButtonSetState (_formatPb[1], FALSE, FALSE);
        XmToggleButtonSetState (_formatPb[2], FALSE, FALSE);
    }
    else {
        XtSetSensitive( _formatPb[0], TRUE);
        XtSetSensitive( _formatPb[1], TRUE);
        XtSetSensitive( _formatPb[2], TRUE);
	cst_rxbl ( _phen[_vgLoc][_phenStrc.current], tempch, &lenstr, &iret );

	if ( strcmp( tempch, "VOLCANIC_ASH " ) == 0 ) {
	    XtSetSensitive ( _phenMenuB, TRUE );
            XtSetSensitive ( _phLatLab , TRUE );
            XtSetSensitive ( _phLonLab , TRUE );
            XtSetSensitive ( _phenLatLon[0] , TRUE );
            XtSetSensitive ( _phenLatLon[1] , TRUE );
	    XtSetSensitive ( _freetxtLab, TRUE );
	    XtSetSensitive ( _freetxtText, TRUE );

            if (XtIsManaged (_basicAttrib2Form)) {
	        XtUnmanageChild (_basicAttrib2Form);
            }

	    XtSetSensitive ( _presLab, FALSE );
	    XtSetSensitive ( _presHPA, FALSE );
	    XtSetSensitive ( _maxwndLab, FALSE );
	    XtSetSensitive ( _maxWinds, FALSE );
	}
	else {
	    XtSetSensitive ( _phenMenuB, FALSE );
            XtSetSensitive ( _phLatLab , FALSE );
            XtSetSensitive ( _phLonLab , FALSE );
            XtSetSensitive ( _phenLatLon[0] , FALSE );
            XtSetSensitive ( _phenLatLon[1] , FALSE );
	    if ( strcmp( tempch, "TROPICAL_CYCLONE " ) == 0 ) {

                if (!(XtIsManaged (_basicAttrib2Form)) ) {
	            XtManageChild (_basicAttrib2Form);
                }

	        XtSetSensitive ( _presLab, TRUE );
	        XtSetSensitive ( _presHPA, TRUE );
	        XtSetSensitive ( _maxwndLab, FALSE );
	        XtSetSensitive ( _maxWinds, FALSE );
		XtSetSensitive ( _freetxtLab, TRUE );
		XtSetSensitive ( _freetxtText, TRUE );
	        if (_subType != SIGTYP_ISOL ) {
	    	    XtSetSensitive (_phenLatLon[0], TRUE);
	    	    XtSetSensitive (_phenLatLon[1], TRUE);
		}
	    }
	    else {

                if (XtIsManaged (_basicAttrib2Form)) {
	            XtUnmanageChild (_basicAttrib2Form);
                }

	        XtSetSensitive ( _presLab, FALSE );
	        XtSetSensitive ( _presHPA, FALSE );
	        XtSetSensitive ( _maxwndLab, FALSE );
	        XtSetSensitive ( _maxWinds, FALSE );
		XtSetSensitive ( _freetxtLab, FALSE );
		XtSetSensitive ( _freetxtText, FALSE );
	    }
	}
    }
}

/*=====================================================================*/

void pgsigw_setTable ( void )
/************************************************************************
 * pgsigw_setTable							*
 *									*
 * This function sets the CES table values.				*
 *									*
 * void pgsigw_setTable ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/99	initial coding				*
 * E. Safford/SAIC	12/01	add pgutls_initHdr()			*
 ***********************************************************************/
{
    int		ier;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    pgutls_initHdr ( &(el.hdr) );

    el.hdr.vg_type = (char)_vgType;
    el.hdr.vg_class = CLASS_TRACKS;
    el.elem.sig.info.subtype = _subType;

    ces_get (_subType, &el, &ier);
    pgsigw_getAttr (&el);

    ces_set (_subType, &el, &ier);
}

/*=====================================================================*/

void pgsigw_getState ( VG_DBStruct *el, char st[], int *iret )
/************************************************************************
 * pgsigw_getState 							*
 *									*
 * This function retrieves the state list from the sigmets.		*
 *									*
 * pgsigw_getState (el, st, iret)					*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *									*
 * Output parameters:							*
 *	st[]		char		state list 			*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * M. Li/GSC		 5/00	Create					*
 * M. Li/GSC		 5/00	Added clo_tgst				*
 * T. Piper/GSC		11/00	Made st [] from [80], increased to 151  *
 * T. Piper/GSC		11/00   lat/lon changed from 120 to MAX_SIGMET*2*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * D.W.Plummer/NCEP	 4/01	Added Great Lakes & Adj Coastal Waters	*
 * D.W.Plummer/NCEP	 7/01	Correction for Coastal Waters		*
 * D.W.Plummer/NCEP	 8/01	Repl clo_bqinfo w/ cst_gtag		*
 ***********************************************************************/
{
    int		ii, kk, all, npts, np, intrsct, ier, two;
    float	lat[2*MAX_SIGMET], lon[2*MAX_SIGMET];  /* *2 for ESOL */
    float	dist, dir, ang1, ang2;
    float	dirs[]={ 0.0F, 180.0F, 90.0F, 270.0F };
    float	s1lat[2], s1lon[2], s2lat[2], s2lon[2];
    float	x1[2], y1[2], x2[2], y2[2];
    float	xint, yint;
    SigmetType	*psig;
    char 	state_list[512], cstl_list[128], lakes_list[128];
    char	info[128], stpo[4], *cptr, qst[4];
/*---------------------------------------------------------------------*/
	
    *iret = 0;
    
/*
 * setup basic information
 */
    psig  = &(el->elem.sig);
    np = psig->info.npts;
       
    switch ( psig->info.subtype )  {

      case	SIGTYP_LINE:		/* line		*/

	for ( ii = 0; ii < np; ii++ )  {
	    lat[ii] = psig->latlon[ii];
	    lon[ii] = psig->latlon[ii+np];
	}

	if ( !G_DIFF(psig->info.distance, 0.0F) )  {

	    dist = psig->info.distance * NM2M;   
	    switch ( psig->info.sol )  {

		case	SIGLINE_NOF:
		case	SIGLINE_SOF:
		case	SIGLINE_EOF:
		case	SIGLINE_WOF:

		    npts = 1;
		    for ( ii = 0; ii < np; ii++ )  {
			clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[ii+np]),
				    &dist, &(dirs[psig->info.sol-1]),
				    &(lat[npts]), &(lon[npts]), &ier );
			npts++;
		    }
		    lat[npts] = psig->latlon[np-1];
		    lon[npts] = psig->latlon[2*np-1];
		    npts++;
	
		    break;

		case	SIGLINE_ESOL:

		    lat[0] = psig->latlon[0];
		    lon[0] = psig->latlon[np];

		    clo_direct ( &(psig->latlon[1]), &(psig->latlon[np+1]),
				 &(psig->latlon[0]), &(psig->latlon[np  ]),
				 &ang1, &ier );

		    ang1 -= 90.0F;
		    clo_dltln ( &(psig->latlon[0]), &(psig->latlon[np]), &dist, 
				&ang1, &(lat[2*np+1]), &(lon[2*np+1]), &ier );
		    ang1 = ang1 - 180.0F;
		    clo_dltln ( &(psig->latlon[0]), &(psig->latlon[np]), &dist, 
				&ang1, &(lat[1]), &(lon[1]), &ier );

		    ang2 = ang1;

		    two = 2;
		    for ( ii = 1; ii < np-1; ii++ )  {

		     clo_direct ( &(psig->latlon[ii-1]), &(psig->latlon[np+ii-1]),
				  &(psig->latlon[ii]), &(psig->latlon[np+ii]), 
				  &ang1, &ier );
		     ang1 = (float)fmod( (double)(ang1+270.0F), 360.0);
		     clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]), 
				 &dist, &ang1, &(s1lat[1]), &(s1lon[1]), &ier );
		     clo_direct ( &(psig->latlon[ii+1]), &(psig->latlon[np+ii+1]),
				  &(psig->latlon[ii]), &(psig->latlon[np+ii]),
				  &ang2, &ier );
		     ang2 = (float)fmod( (double)(ang2+90.0F), 360.0);
		     clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]), 
				 &dist, &ang2, &(s2lat[0]), &(s2lon[0]), &ier );

		     if ( (int)G_ABS(ang1-ang2) > 1 )  {

		       clo_dltln ( &(psig->latlon[ii-1]), &(psig->latlon[np+ii-1]), 
				   &dist, &ang1, &(s1lat[0]), &(s1lon[0]), &ier );
		       clo_dltln ( &(psig->latlon[ii+1]), &(psig->latlon[np+ii+1]), 
				   &dist, &ang2, &(s2lat[1]), &(s2lon[1]), &ier );

		       gtrans ( sys_M, sys_N, &two, s1lat, s1lon, x1, y1, 
		                &ier, strlen(sys_M), strlen(sys_N) );
		       gtrans ( sys_M, sys_N, &two, s2lat, s2lon, x2, y2,
		                &ier, strlen(sys_M), strlen(sys_N) );
		       cgr_segint( sys_N, x1, y1, sys_N, x2, y2,
			           sys_M, &xint, &yint, &intrsct, &ier );

		     }
		     else  {
		       xint = (s1lat[1] + s2lat[0]) / 2.0F;
		       yint = (s1lon[1] + s2lon[0]) / 2.0F;
		     }

		     kk = ii + 1;
		     lat[kk] = xint;
		     lon[kk] = yint;

		     ang1 = (float)fmod( (double)(ang1+180.0F), 360.0);
		     ang2 = (float)fmod( (double)(ang2+180.0F), 360.0);

		     clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]), 
				 &dist, &ang1, &(s1lat[1]), &(s1lon[1]), &ier );
		     clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]), 
				 &dist, &ang2, &(s2lat[0]), &(s2lon[0]), &ier );

		     if ( (int)G_ABS(ang1-ang2) > 1 )  {

		       clo_dltln ( &(psig->latlon[ii-1]), &(psig->latlon[np+ii-1]), 
				   &dist, &ang1, &(s1lat[0]), &(s1lon[0]), &ier );
		       clo_dltln ( &(psig->latlon[ii+1]), &(psig->latlon[np+ii+1]), 
				   &dist, &ang2, &(s2lat[1]), &(s2lon[1]), &ier );

		       gtrans ( sys_M, sys_N, &two, s1lat, s1lon, x1, y1,
		                &ier, strlen(sys_M), strlen(sys_N) );
		       gtrans ( sys_M, sys_N, &two, s2lat, s2lon, x2, y2,
		                &ier, strlen(sys_M), strlen(sys_N) );
		       cgr_segint( sys_N, x1, y1, sys_N, x2, y2,
			           sys_M, &xint, &yint, &intrsct, &ier );
		     }
		     else  {
		       xint = (s1lat[1] + s2lat[0]) / 2.0F;
		       yint = (s1lon[1] + s2lon[0]) / 2.0F;
		     }

		     kk = 2*np - ii + 1;
		     lat[kk] = xint;
		     lon[kk] = yint;

		     ang1 = (float)fmod( (double)(ang1+180.0F), 360.0);
		     ang2 = (float)fmod( (double)(ang2+180.0F), 360.0);

		     ang1 = ang2;

		    }

		    clo_direct ( &(psig->latlon[np-2]), &(psig->latlon[2*np-2]),
				 &(psig->latlon[np-1]), &(psig->latlon[2*np-1]),
				 &ang2, &ier );

		    ang2 -= 90.0F;
		    clo_dltln ( &(psig->latlon[np-1]), &(psig->latlon[2*np-1]), 
				&dist, &ang2, &(lat[np]), &(lon[np]), &ier );

		    ang2 = (float)fmod( (double)(ang2+180.0F), 360.0);
		    clo_dltln ( &(psig->latlon[np-1]), &(psig->latlon[2*np-1]), 
				&dist, &ang2, &(lat[np+2]), &(lon[np+2]), &ier );

		    lat[np+1] = psig->latlon[np-1];
		    lon[np+1] = psig->latlon[2*np-1];

		    lat[2*np+2] = lat[0];
		    lon[2*np+2] = lon[0];

		    npts = 2*np + 3;

		    break;
	    }

	} else
	    npts = np;

	break;

      case	SIGTYP_AREA:		/* area		*/

	for ( ii = 0; ii < np; ii++ )  {
	    lat[ii] = psig->latlon[ii];
	    lon[ii] = psig->latlon[ii+np];
	}
	lat[np] = psig->latlon[0];
	lon[np] = psig->latlon[np];
	np++;
	npts = np;

	break;

      case	SIGTYP_ISOL:		/* isolated	*/

	xint = psig->latlon[0];
        yint = psig->latlon[np];

	if ( !G_DIFF(psig->info.distance, 0.0F) )  {
	    dist = psig->info.distance * NM2M;	    
	    npts = (int) (psig->info.distance * (float)CIRCFACT / 10.0F);
	    if (npts < CIRCFACT ) npts = CIRCFACT;
	    if (npts > 10 * CIRCFACT) npts = 10 * CIRCFACT;

	    for(ii = 0; ii < npts; ii++) {
		dir = (float)ii * 360.0F / (float)npts;
		clo_dltln(&xint, &yint, &dist, &dir, &lat[ii], &lon[ii], &ier);
		}
	} else {
	    lat[0] = psig->latlon[0];
            lon[0] = psig->latlon[np];
	    npts = 1;
	}
	break;
    }

/*
 * get the state list 
 */
    if ( el->hdr.vg_type == SIGCONV_ELM ) {        

	clo_binpoly( "GREAT_LAKES", npts, lat, lon, iret);

	lakes_list[0] = '\0';
        for ( ii = 0; ii < clo_qnhot(); ii++ )  {

            clo_bginfo( "GREAT_LAKES", ii, info, &ier );
            cst_gtag( "ID", info, "?", stpo, &ier );
            strcat(lakes_list, stpo);
            strcat(lakes_list, " ");

    	}

	clo_binpoly( "ADJ_CSTL", npts, lat, lon, iret);

	cstl_list[0] = '\0';
        for ( ii = 0; ii < clo_qnhot(); ii++ )  {

            clo_bginfo( "ADJ_CSTL", ii, info, &ier );
            cst_gtag( "ID", info, "?", stpo, &ier );
            strcat(cstl_list, stpo);
            strcat(cstl_list, " ");

    	}

	clo_binpoly( "STATE_BNDS", npts, lat, lon, iret);

	state_list[0] = '\0';
        for ( ii = 0; ii < clo_qnhot(); ii++ )  {

            clo_bginfo( "STATE_BNDS", ii, info, &ier );
            cst_gtag( "STATE", info, "?", stpo, &ier );
            strcat(state_list, stpo);
            strcat(state_list, " ");

    	}

	/*
	 *  Check each state to see if it's coastal waters are included.
	 *  If all states' coastal waters are included, then simply add
	 *  "AND CSTL WTRS" to the states/lakes string.  
	 *  If any state does not include it's coastal waters, then 
	 *  simply add "AND" plus the coastal string plus "CSTL WTRS".
	 */
	all = G_TRUE;
	cptr = cst_split ( state_list, ' ', sizeof(qst)/sizeof(qst[0]),
			   qst, &ier );
	while ( cptr != (char *)NULL )  {
	    if ( strstr ( cstl_list, qst ) == (char *)NULL )  {
		all = G_FALSE;
		break;
	    }
	    else  {
	        cptr = cst_split ( cptr, ' ', 
			           sizeof(qst)/sizeof(qst[0]), qst, &ier );
	    }
	}

	strcat ( state_list, lakes_list );
	if ( all && strlen(state_list) > (size_t)0  &&
		strlen(state_list) == strlen(cstl_list) )  {
	    strcat ( state_list, "AND CSTL WTRS" );
	}
	else  {
	    if ( strlen ( cstl_list ) != (size_t)0 )  {
	        if ( strlen(state_list) > (size_t)0 )  strcat(state_list, "AND ");
	        strcat ( state_list, cstl_list );
	        strcat ( state_list, "CSTL WTRS" );
	    }
	}
    	strcat(state_list, "\n");    	
    }	
    strcpy(st, state_list);
}

/*=====================================================================*/

void pgsigw_setState ( int np, float *ilat, float *ilon )
/************************************************************************
 * pgsigw_setState							*
 *									*
 * This function sets the State list					*
 *									*
 * void pgsigw_setState (np, ilat, ilon)				*
 *									*
 * Input parameters:							*
 *	np		int	number of points			*
 *	*ilat		float	points of latitude			*
 *	*ilon		float	points of longitude			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * M. Li/GSC		05/00	initial coding				*
 * M. Li/GSC		05/00	removed XmStringCreateLocalized		*
 ***********************************************************************/
{
    int		ii, ier;
/*    XmString	xmstr;  */
    VG_DBStruct el;
/*---------------------------------------------------------------------*/

    el.hdr.vg_type 		= (char)_vgType;
    el.elem.sig.info.subtype 	= _subType;
    el.elem.sig.info.sol 	= _solStrc.current;
    el.elem.sig.info.distance 	= _currDist;

    el.elem.sig.info.npts	= np;
    for ( ii = 0; ii < np; ii++ )  {
	el.elem.sig.latlon[ii] 	  = ilat[ii];
	el.elem.sig.latlon[np+ii] = ilon[ii];
    }    

    if (_vgType == SIGCONV_ELM) {
	pgsigw_getState(&el, _currST, &ier);
/*
        xmstr = XmStringCreateLocalized(_currST);
        XtVaSetValues (_stateList, XmNlabelString, xmstr, NULL);
        XmStringFree (xmstr);  
*/
    }
}

/*=====================================================================*/
void pgsigw_getFIRs ( VG_DBStruct *el, char fir[], int *iret )
/************************************************************************
 * pgsigw_getFIRs                                                       *
 *                                                                      *
 * This function determines the FIRs list from the sigmets.             *
 *                                                                      *
 * pgsigw_getFIRs (el, fir, iret)                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *      *el             VG_DBStruct     Pointer to VG record structure  *
 *                                                                      *
 * Output parameters:                                                   *
 *      fir[]           char            fir list                        *
 *      *iret           int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Lewis/AWC         05/07   Initial implementation. Adapted from	*
 *					pgsigw_getStates		*
 ***********************************************************************/
{
    int         ii, kk, npts, np, intrsct, ier, two;
    float       lat[2*MAX_SIGMET], lon[2*MAX_SIGMET];  /* *2 for ESOL */
    float       dist, dir, ang1, ang2;
    float       dirs[]={ 0.0F, 180.0F, 90.0F, 270.0F };
    float       s1lat[2], s1lon[2], s2lat[2], s2lon[2];
    float       x1[2], y1[2], x2[2], y2[2];
    float       xint, yint;
    SigmetType  *psig;
    char        fir_list[12];
    char        info[128], stpo[4];
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 * setup basic information
 */
    psig  = &(el->elem.sig);
    np = psig->info.npts;

    switch ( psig->info.subtype )  {

      case      SIGTYP_LINE:            /* line         */

        for ( ii = 0; ii < np; ii++ )  {
            lat[ii] = psig->latlon[ii];
            lon[ii] = psig->latlon[ii+np];
        }

        if ( !G_DIFF(psig->info.distance, 0.0F) )  {

            dist = psig->info.distance * NM2M;
            switch ( psig->info.sol )  {

                case    SIGLINE_NOF:
                case    SIGLINE_SOF:
                case    SIGLINE_EOF:
                case    SIGLINE_WOF:

                    npts = 1;
                    for ( ii = 0; ii < np; ii++ )  {
                        clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[ii+np]),
                                    &dist, &(dirs[psig->info.sol-1]),
                                    &(lat[npts]), &(lon[npts]), &ier );
                        npts++;
                    }
                    lat[npts] = psig->latlon[np-1];
                    lon[npts] = psig->latlon[2*np-1];
                    npts++;

                    break;

                case    SIGLINE_ESOL:

                    lat[0] = psig->latlon[0];
                    lon[0] = psig->latlon[np];

                    clo_direct ( &(psig->latlon[1]), &(psig->latlon[np+1]),
                                 &(psig->latlon[0]), &(psig->latlon[np  ]),
                                 &ang1, &ier );

                    ang1 -= 90.0F;
                    clo_dltln ( &(psig->latlon[0]), &(psig->latlon[np]), &dist,
                                &ang1, &(lat[2*np+1]), &(lon[2*np+1]), &ier );
                    ang1 = ang1 - 180.0F;
                    clo_dltln ( &(psig->latlon[0]), &(psig->latlon[np]), &dist,
                                &ang1, &(lat[1]), &(lon[1]), &ier );

                    ang2 = ang1;

                    two = 2;
                    for ( ii = 1; ii < np-1; ii++ )  {

                     clo_direct ( &(psig->latlon[ii-1]), &(psig->latlon[np+ii-1]),
                                  &(psig->latlon[ii]), &(psig->latlon[np+ii]),
                                  &ang1, &ier );
                     ang1 = (float)fmod( (double)(ang1+270.0F), 360.0);
                     clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]),
                                 &dist, &ang1, &(s1lat[1]), &(s1lon[1]), &ier );
                     clo_direct ( &(psig->latlon[ii+1]), &(psig->latlon[np+ii+1]),
                                  &(psig->latlon[ii]), &(psig->latlon[np+ii]),
                                  &ang2, &ier );
                     ang2 = (float)fmod( (double)(ang2+90.0F), 360.0);
                     clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]),
                                 &dist, &ang2, &(s2lat[0]), &(s2lon[0]), &ier );

                     if ( (int)G_ABS(ang1-ang2) > 1 )  {

                       clo_dltln ( &(psig->latlon[ii-1]), &(psig->latlon[np+ii-1]),
                                   &dist, &ang1, &(s1lat[0]), &(s1lon[0]), &ier );
                       clo_dltln ( &(psig->latlon[ii+1]), &(psig->latlon[np+ii+1]),
                                   &dist, &ang2, &(s2lat[1]), &(s2lon[1]), &ier );

                       gtrans ( sys_M, sys_N, &two, s1lat, s1lon, x1, y1,
                                &ier, strlen(sys_M), strlen(sys_N) );
                       gtrans ( sys_M, sys_N, &two, s2lat, s2lon, x2, y2,
                                &ier, strlen(sys_M), strlen(sys_N) );
                       cgr_segint( sys_N, x1, y1, sys_N, x2, y2,
                                   sys_M, &xint, &yint, &intrsct, &ier );
                     }
                     else  {

                       xint = (s1lat[1] + s2lat[0]) / 2.0F;
                       yint = (s1lon[1] + s2lon[0]) / 2.0F;
                     }

                     kk = ii + 1;
                     lat[kk] = xint;
                     lon[kk] = yint;

                     ang1 = (float)fmod( (double)(ang1+180.0F), 360.0);
                     ang2 = (float)fmod( (double)(ang2+180.0F), 360.0);

                     clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]),
                                 &dist, &ang1, &(s1lat[1]), &(s1lon[1]), &ier );
                     clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]),
                                 &dist, &ang2, &(s2lat[0]), &(s2lon[0]), &ier );

                     if ( (int)G_ABS(ang1-ang2) > 1 )  {

                       clo_dltln ( &(psig->latlon[ii-1]), &(psig->latlon[np+ii-1]),
                                   &dist, &ang1, &(s1lat[0]), &(s1lon[0]), &ier );
                       clo_dltln ( &(psig->latlon[ii+1]), &(psig->latlon[np+ii+1]),
                                   &dist, &ang2, &(s2lat[1]), &(s2lon[1]), &ier );

                       gtrans ( sys_M, sys_N, &two, s1lat, s1lon, x1, y1,
                                &ier, strlen(sys_M), strlen(sys_N) );
                       gtrans ( sys_M, sys_N, &two, s2lat, s2lon, x2, y2,
                                &ier, strlen(sys_M), strlen(sys_N) );
                       cgr_segint( sys_N, x1, y1, sys_N, x2, y2,
                                   sys_M, &xint, &yint, &intrsct, &ier );
                     }
                     else  {

                       xint = (s1lat[1] + s2lat[0]) / 2.0F;
                       yint = (s1lon[1] + s2lon[0]) / 2.0F;
                     }

                     kk = 2*np - ii + 1;
                     lat[kk] = xint;
                     lon[kk] = yint;

                     ang1 = (float)fmod( (double)(ang1+180.0F), 360.0);
                     ang2 = (float)fmod( (double)(ang2+180.0F), 360.0);

                     ang1 = ang2;
                    }

                    clo_direct ( &(psig->latlon[np-2]), &(psig->latlon[2*np-2]),
                                 &(psig->latlon[np-1]), &(psig->latlon[2*np-1]),
                                 &ang2, &ier );

                    ang2 -= 90.0F;
                    clo_dltln ( &(psig->latlon[np-1]), &(psig->latlon[2*np-1]),
                                &dist, &ang2, &(lat[np]), &(lon[np]), &ier );

                    ang2 = (float)fmod( (double)(ang2+180.0F), 360.0);
                    clo_dltln ( &(psig->latlon[np-1]), &(psig->latlon[2*np-1]),
                                &dist, &ang2, &(lat[np]), &(lon[np]), &ier );

                    ang2 = (float)fmod( (double)(ang2+180.0F), 360.0);
                    clo_dltln ( &(psig->latlon[np-1]), &(psig->latlon[2*np-1]),
                                &dist, &ang2, &(lat[np+2]), &(lon[np+2]), &ier );

                    lat[np+1] = psig->latlon[np-1];
                    lon[np+1] = psig->latlon[2*np-1];

                    lat[2*np+2] = lat[0];
                    lon[2*np+2] = lon[0];

                    npts = 2*np + 3;

                    break;
            }

        } else
            npts = np;

        break;

      case      SIGTYP_AREA:            /* area         */

        for ( ii = 0; ii < np; ii++ )  {
            lat[ii] = psig->latlon[ii];
            lon[ii] = psig->latlon[ii+np];
        }
        lat[np] = psig->latlon[0];
        lon[np] = psig->latlon[np];
        np++;
        npts = np;

        break;

      case      SIGTYP_ISOL:            /* isolated     */

        xint = psig->latlon[0];
        yint = psig->latlon[np];

        if ( !G_DIFF(psig->info.distance, 0.0F) )  {
            dist = psig->info.distance * NM2M;
            npts = (int) (psig->info.distance * (float)CIRCFACT / 10.0F);
            if (npts < CIRCFACT ) npts = CIRCFACT;
            if (npts > 10 * CIRCFACT) npts = 10 * CIRCFACT;

            for(ii = 0; ii < npts; ii++) {
                dir = (float)ii * 360.0F / (float)npts;
                clo_dltln(&xint, &yint, &dist, &dir, &lat[ii], &lon[ii], &ier);
                }
        } else {
            lat[0] = psig->latlon[0];
            lon[0] = psig->latlon[np];
            npts = 1;
        }

        break;
    }

/*
 * get the fir list   
 */
    if ( el->hdr.vg_type == SIGINTL_ELM ) {

        clo_binpoly( "FIR_BNDS", npts, lat, lon, iret);

        fir_list[0] = '\0';
        for ( ii = 0; ii < clo_qnhot(); ii++ )  {

            clo_bginfo( "FIR_BNDS", ii, info, &ier );
            cst_gtag( "FIR", info, "?", stpo, &ier );
            strcat(fir_list, stpo);
            strcat(fir_list, " ");
        }
    }
    strcpy(fir, fir_list);
}

/*=====================================================================*/

void pgsigw_setFIRs ( int np, float *ilat, float *ilon )
/************************************************************************
 * pgsigw_setFIRs                                                       *
 *                                                                      *
 * Sets the FIR list.                                                   *
 *                                                                      *
 * void pgsigw_setFIRs (np, ilat, ilon)                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *      np              int     number of points                        *
 *      *ilat           float   points of latitude                      *
 *      *ilon           float   points of longitude                     *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Lewis/AWC         05/07   Initial implementation.  Adapated from  *
 *                                      pgsigw_setState                 *
 ***********************************************************************/
{
    int         ii, ier;
    VG_DBStruct el;
/*---------------------------------------------------------------------*/

    el.hdr.vg_type              = (char)_vgType;
    el.elem.sig.info.subtype    = _subType;
    el.elem.sig.info.sol        = _solStrc.current;
    el.elem.sig.info.distance   = _currDist;

    el.elem.sig.info.npts       = np;
    for ( ii = 0; ii < np; ii++ )  {
        el.elem.sig.latlon[ii]    = ilat[ii];
        el.elem.sig.latlon[np+ii] = ilon[ii];
    }

    if (_vgType == SIGINTL_ELM) {
        pgsigw_getFIRs(&el, _currFIR, &ier);

    }
}

/*=====================================================================*/

void pgsigw_setAFOSflg ( int *iret )
/************************************************************************
 * pgsigw_setAFOSflg                                                    *
 *                                                                      *
 * This function sets the AFOS flag.                                    *
 *                                                                      *
 * void pgsigw_setAFOSflg ( iret )                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *      none                                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int     Return code                             *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      5/01                                           *
 ***********************************************************************/
{
    *iret = 0;
    _AFOSflag = G_FALSE;

    if ( getenv ( "SIGMETFMT" ) == (char *)NULL ) {
        _AFOSflag = G_FALSE;
    }
    else  {
        if ( strcmp( getenv ( "SIGMETFMT" ), "AFOS" ) == 0 )  {
            _AFOSflag = G_TRUE;
        }
    }
}

