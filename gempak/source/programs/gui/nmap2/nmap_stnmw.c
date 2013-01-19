#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"
#include "nmap_stnm.h"


#define STNM_ELM_NUM	 11	/* # of station element */

#define STNM_ELM_FRAMEW	100	/* width of station element frame*/
#define STNM_ELM_FRAMEH	 70	/* height of station element frame */
#define STNM_ELM_BTNW	 60	/* station parm button width */
#define STNM_ELM_BTNH	 45	/* station parm button height */

#define SCALE_MAXVAL	  3	/* maximum scale value		*/
#define SCALE_MINVAL	  0.5	/* minimam scale value		*/

#define GEMPARM_MAX	100	/* maximum number of applicable GEM parms */

#define ICON_FGNAME	"white"
#define ICON_BGNAME	"blue"
#define ICON_DIR	"$NAWIPS/icons/nmap"

#define SET_FILTER	  0
#define SET_LEVEL	  1

#define MAX_LEVEL	  2

typedef  struct {
	char	name[10];
	char	size[10];
	char	others[20];
	Boolean	pzeroflg;	/* special parm for position 0 only */ 
} parmrec_str;			/* GEMPAK parm record structure */	

typedef  struct {
	parmrec_str parmarry[GEMPARM_MAX];
	int       nparms;	/* total number of parameters */
	int       npzero;       /* total number of parms with pzeroflg set */
} parm_str;			/* GEMPAK parm array structure */	

static parm_str _gemParms;

typedef struct {
        Widget   frame;
        Widget   checkb;
        Widget   checkf; 
	Widget   drawb;
	Widget   bgw[2];   /* widgets needed for setting BG color */
	char     chkstate; /* check button state, 0=false, 1=true */
        Pixmap   pxm;
	char     parmstr[20];
	char     size[10];
	int      mult; 
	char     others[20];
	int      color;
}stnmw_t; 		        /* structure for each parameter element   */

static stnmw_t	_stnmElmW[STNM_ELM_NUM];   /* station model element widget */

typedef struct {
        char    gemparm[13];    /* GEMPAK parm name */
        char    name[40];        /* parm name */
        char    unit[6];         /* parm unit */
}gemparm_t;

gemparm_t   _vcoordStr[]={
	{"PRES",  "Pressure",              "mb"},
	{"HGHT",  "Height",                "m"},
	{"THTA",  "Potential Temperature", "K"},
}; 			/* vertical coordinates struct */


static Widget	_stnmPopW; 	 /* STNM parm editing window */
static Widget	_vcoordPane;
static Widget	_vcoordLabelW;     /* vertical coordinate label */
static Widget	_levelScaleW; 	 /* scale widget for Level */
static Widget	_levelPane; 	 /* level frame widget */
static Widget	_levelSel; 	 /* level select widget */
static Widget	_levelArrowBW[2]; /* mandatory level arrow button widgets */
static Widget	_levelTextW;	 /* level text field widget	*/
static Widget	_stnmFilterW;     /* filter scale widget */
static Widget	_vcLvlUnitW;	 /* vertical level label widget */
 
static Widget	_stnmModW;	        /* station model popup window widget      */
static Widget	_txtSizeW;	/* text size option widget 		  */
static Widget	_txtFontW;	/* text font option widget		  */
static Widget	_txtStyleW;	/* text style option widget		  */
static Widget	_stnmParmListW;	/* parameter list widget 		  */
static Widget	_smblSizeW;	/* weather symbol size widget		  */
static NxmColrP_t *_stnmColrW;	/* color pallete 			  */ 
static Widget	_levelSelW[2];  /* level select widget	  		  */

static Pixel	_blackPixel, _whitePixel;
static Pixel	_parmBG, _noparmBG; /* background color with/without parm */
static XmString	_xmNoparm; 	/* string to display when no parm */ 

static stnm_t	_stnmEdit;         /* the station model to be edited */

int		_levelMin, _levelMax;
int		_levelStep; /* defult steps for clicking level scale */
unsigned char	_levelDir;  /* level scale processing direction */
int		_mainLevelN, _mainLevel[20]; /* mandatory levels */

int     	_stnmLevel;
int		_attrIndex;

static char	_typeStr[20];	  /* type string */
static char	_stnmAlias[20];	  /* station model alias  */
static Boolean	_stnmFfgFlg;	  /* color coding flag  */
static Boolean	_dtypeChangeFlg;  /* indicate whether data type is changed*/
static Boolean	_stnmSaveLastFlg; /* indicate whether to save last button */
static Boolean	_parmTblFlg;      /* indicate whether gemparm table exists */
static Boolean  _sfcChangeFlg; 	  /* indicate sfc value change*/

static GC		_parmGC;
static XFontStruct	*_parmFont = NULL;

static int	_parmSelected = -1;     /* selected parameter      */
static int	_fontStyle, _fontFamily, _fontSize; /* index to opt widget */

static int	_nDeselect, _deSelected[STNM_ELM_NUM]; /* parms are off */ 
static int	_saveLevel;		/* last sounding level loaded */

static char	*_txtFont[] =  { "Courier", "Helvetica", "Times" }; 
static char	*_txtStyle[] = { "Regular", "Italic", "Bold", "Bold-Italic" }; 


/*
 *  private functions - create widgets
 */
Widget	stnmw_createFilter	( Widget parent, Widget *scaleptr);
Widget	stnmw_createModel	( Widget parent);
Widget	stnmw_createStnmElm	( Widget parent, char *name, long index);
Widget	stnmw_createStnmIcon	( Widget parent);
Widget	stnmw_createStnmLayout	( Widget parent);
Widget	stnmw_createTxtEdit	( Widget parent);

/*
 *  private callback functions
 */
void	stnmw_arrowCb ( Widget wid, long which, XtPointer call );
void	stnmw_clearCb ( Widget w, XtPointer clnt, XtPointer call );
void	stnmw_colorCb ( Widget w, long which, XtPointer call );
void	stnmw_ctlBtnCb ( Widget w, long which, XtPointer call );
void	stnmw_defaultCb ( Widget w, XtPointer clnt, XtPointer call );
void	stnmw_elemExposeCb ( Widget w, long which, XtPointer call );
void	stnmw_elemParmCb ( Widget w, long which, XtPointer call );
void	stnmw_filterCb ( Widget	wid, XtPointer clnt, XtPointer	call );
void	stnmw_iconCb ( Widget w, XtPointer clnt, XtPointer call );
void	stnmw_lvlSelCb ( Widget wid, long which, XtPointer call );
void	stnmw_levelCb ( Widget wid, XtPointer clnt, XtPointer call );
void	stnmw_modCtlBtnCb ( Widget wid, long which, XtPointer call );
void	stnmw_parmListCb ( Widget  w, XtPointer clnt, XtPointer call );
void	stnmw_smblScaleCb ( Widget w, XtPointer clnt, XmScaleCallbackStruct *call );
void	stnmw_stnmElmCheckCb ( Widget w, long which, XtPointer call );
void	stnmw_txtfontOptCb ( Widget w, long which, XtPointer call );
void	stnmw_txtsizeOptCb ( Widget w, long which, XtPointer call );
void	stnmw_txtstyleOptCb ( Widget w, long which, XtPointer call );
void	stnmw_textfCb ( Widget wid, XtPointer clnt, XtPointer call );

/*
 *  private functions - parameter work
 */
void	stnmw_clearParm ( int which );
void	stnmw_composeParm ( char *parmstr, char *colrstr );
void	stnmw_drawParmStr ( int which );
void	stnmw_initParmList ( void );
int	stnmw_isDeselectParm ( int which );
void	stnmw_makeParmComp ( int which, char *parmstr );
void	stnmw_parmtbl ( char *tblnam );
void	stnmw_parseParmComp ( char *parmstr, char *name, 
                              char *size, char *others );
void	stnmw_redrawParm ( void );
void	stnmw_setParm ( int which, char *parmstr, int color );
void	stnmw_setParmSize ( char *parm, float size, int *inx );
void	stnmw_showParm ( int which );
void	stnmw_updParmList ( int which );

/*
 *  private functions
 */
void	stnmw_getDEFfsz (int in_size, int *out_size );
void	stnmw_initFilter ( void );
void	stnmw_initTxt (char  *txtstr);
void	stnmw_levelSensitive ( Boolean value );
void	stnmw_lvlSelSensitive ( Boolean value );
void	stnmw_popupModel ( void );
void	stnmw_removeExtraChar (char *parmstr, char c );
void	stnmw_setElmBgColr ( int which, Pixel color );
void	stnmw_setFont (int ifont, float size );
void	stnmw_setStnm ( void );
void	stnmw_setValue (int which );
void	stnmw_setVcoord (char *vcoord );
void	stnmw_updSizeScale (char *parm );


/************************************************************************
 * nmap_stnmw.c								*
 *									*
 * This module creates the station model editing popup window.		*
 * components and defines the callback functions for nmap.		*
 *									*
 * CONTENTS:								*
 *   stnmw_create()		create the parm editing popup window	*
 *   stnmw_popup()		pop up the parm editing window		*
 *   stnmw_popdown()		pop down the parm editing window	*
 *									*
 *   stnmw_isUp()		checks if popup is up			*
 *									*
 *   stnmw_createModel()	create a station model editing popup	*
 *   stnmw_createFilter()	create a filter widget			*
 *   stnmw_createStnmIcon()	create station model icon		*
 *   stnmw_createTxtEdit()	create text attribute edit panel	*
 *   stnmw_createStnmLayout()	create layout of station element	*
 *   stnmw_createStnmElm()	create a station element		*
 *									*
 *   stnmw_filterCb()		callback for filter scale		*
 *   stnmw_iconCb()		callback for station model icon button	*
 *   stnmw_txtsizeOptCb()	callback for text size option menu	*
 *   stnmw_txtfontOptCb()	callback for text font option menu	*
 *   stnmw_txtstyleOptCb()	callback for text style option menu	*
 *   stnmw_stnmElmCheckCb()	callback for check button of element	*
 *   stnmw_elemParmCb()		callback for parm button of element	*
 *   stnmw_elemExposeCb()	callback for expose event of elmemnt	*
 *   stnmw_defaultCb()		callback for default button		*
 *   stnmw_clearCb()		callback for clear button		*
 *   stnmw_parmListCb()		callback for parameter list		*
 *   stnmw_colorCb()		callback for color selection		*
 *   stnmw_smblScaleCb()	callback for weather symbol scale	*
 *   stnmw_levelCb()		callback of level scale			*
 *   stnmw_lvlSelCb()		callback of surface or upper level	*
 *   stnmw_arrowCb()		callback of level arrows		*
 *   stnmw_textfCb()		callback of level text field input	*
 *   stnmw_ctlBtnCb()		callback of control buttons		*
 *   stnmw_modCtlBtnCb()	callback for control buttons		*
 *									*
 *   stnmw_parmtbl()		read station model parm input table	*
 *   stnmw_setParm()		set up the station element		*
 *   stnmw_clearParm()		clear the station element		*
 *   stnmw_drawParmStr()	draw parameter string			*
 *   stnmw_showParm()		display parm string for an element	*
 *   stnmw_redrawParm()		redraw parm string for each element	*
 *   stnmw_composeParm()	compose the station model parm string	*
 *   stnmw_isDeselectParm()	checks whether the element is deselected*
 *   stnmw_initParmList()	initialize parameter list widget	*
 *   stnmw_setParmSize()	set the size in _gemParms		*
 *   stnmw_updParmList()	update parameter list widget		*
 *   stnmw_makeParmComp()	create parameter component		*
 *   stnmw_parseParmComp()	parse parameter component		*
 *									*
 *   stnmw_initFilter()		initialize the filter			*
 *   stnmw_popupModel()		pop up a station model editing window	*
 *   stnmw_popdownModel()	pop down a station model editing window	*
 *   stnmw_setStnm()		set station model			*
 *   stnmw_setElmBgColr()	set background color of station element	*
 *   stnmw_removeExtraChar()	remove the extra char in parm string	*
 *   stnmw_initTxt()		initialize the text based on TEXT string*
 *   stnmw_setFont()		set the text font			*
 *   stnmw_getDEFfsz()		get the default X font size		*
 *   stnmw_updSizeScale()	update size scale			*
 *   stnmw_setVcoord()		set vertical coordinates		*
 *   stnmw_levelSensitive()	set level scale sensitiveness		*
 *   stnmw_lvlSelSensitive()	set level select sensitiveness		*
 *   stnmw_setValue()		saves a modified value			*
 ***********************************************************************/

/*=====================================================================*/

Widget stnmw_create ( Widget parent )
/************************************************************************
 * stnmw_create                                              		*
 *                                                                      *
 * This function creates the parameter editing popup window for STNM.   *
 *                                                                      *
 * Widget stnmw_create(parent)                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent form widget ID                          *
 *                                                                      *
 * Output parameters:                                                   *
 * stnmw_create	Widget  stnm parm editing popup window widget ID        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96  						*
 * S. Wang/GSC     09/96 	Add level text field widget		*
 * C. Lin/EAI      03/97  	vertical coord selection panel->display	*
 * G. Krueger/EAI  10/97	NxmFrameLabel->NxmLabel_createFrameLbl	*
 * G. Krueger/EAI  10/97	NxmControlBtn->NxmCtlBtn_create 	*
 * C. Lin/EAI      10/97	modify to use new stnmw_* functions	*
 * T. Lee/SAIC	   02/03	add surface level widget		*
 * H. Zeng/XTRIA   12/03	added losing focus callback for text	*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    long	ii, nn;
    char	*btnstr[]={"LOAD", "Close"};

    char	*levelstr[]={"Sfc", "Level"};
    Widget	pane, frame, rc, rc1, bb, button, lv_form;
    Widget	man_form;

/*---------------------------------------------------------------------*/

    /*
     * create dialog shell
     */
    _stnmPopW = XmCreateFormDialog(parent, "stnmw_popup", NULL, 0);
    XtVaSetValues(_stnmPopW, XmNnoResize, True, NULL);
    XtVaSetValues(XtParent(_stnmPopW),
		  XmNtitle, "STNM Parm Edit",
		  NULL);

    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget("stnmw_pane",
			    xmPanedWindowWidgetClass, _stnmPopW,
			    XmNsashWidth,  		  1,
			    XmNsashHeight, 		  1,
			    NULL);

    /*
     * create the vertical coordinate display area 
     */
    _vcoordPane = XtVaCreateWidget("stnmw_vcoordFrame",
			     xmRowColumnWidgetClass,  pane,
			     XmNisAligned,	True,
			     XmNentryAlignment,	XmALIGNMENT_CENTER,
			     XmNorientation,	XmVERTICAL,
			     NULL);

    XtVaCreateManagedWidget("Vertical Coordinate:",
			    xmLabelWidgetClass, _vcoordPane,
			    NULL);

    _vcoordLabelW = XtVaCreateManagedWidget("Pressure",
					    xmLabelWidgetClass, _vcoordPane,
					    NULL);

    XtManageChild(_vcoordPane);


    /*
     * create vertical level selection area 
     */
    frame = XtVaCreateWidget("stnmw_levelFrame",
			     xmFrameWidgetClass,  pane,
			     NULL);

    _vcLvlUnitW = NxmLabel_createFrameLbl("Vertical Level", frame,
					  frame);

    lv_form = XtVaCreateWidget("lv_form", xmFormWidgetClass, 
				frame, NULL);

    /*
     *  Surface or level selection.
     */
    _levelSel = XtVaCreateManagedWidget ("sfc_level",
				 xmRowColumnWidgetClass, lv_form,
				 XmNpacking,		XmPACK_TIGHT,
				 XmNorientation,	XmHORIZONTAL,
				 XmNspacing,		10,
				 XmNradioBehavior, 	False,
				 NULL);

    nn = XtNumber (levelstr);

    for ( ii=0; ii < nn; ii++ ) {
	_levelSelW [ii] = XtVaCreateManagedWidget ( levelstr [ii],
			      xmToggleButtonWidgetClass, _levelSel,
			      XmNhighlightThickness,     0,
			      NULL );
	XtAddCallback ( _levelSelW[ii], XmNarmCallback,
                        (XtCallbackProc)stnmw_lvlSelCb, (XtPointer)ii);
    }

    rc = XtVaCreateManagedWidget("stnmw_levelRc",
			  xmRowColumnWidgetClass, lv_form,
			  XmNtopAttachment,    XmATTACH_WIDGET,
			  XmNtopWidget,        _levelSel,
			  XmNorientation,      XmVERTICAL,
			  NULL);

    /*
     * create level scale widget
     */
    _levelDir    = XmMAX_ON_LEFT;
    _levelScaleW = 
	XtVaCreateManagedWidget("scale",
				xmScaleWidgetClass, rc,
				XmNorientation,		XmHORIZONTAL,
				XmNprocessingDirection,	XmMAX_ON_RIGHT,
				XmNshowValue,		True,
				NULL);

    XtAddCallback(_levelScaleW, XmNdragCallback, stnmw_levelCb, NULL);
    XtAddCallback(_levelScaleW, XmNvalueChangedCallback, stnmw_levelCb, NULL);


    /*
     * create mandatory level arrow buttons
     */
    man_form = XtVaCreateWidget("man_form", xmFormWidgetClass, rc, NULL);

    rc1 = XtVaCreateWidget("stnmw_levelArrowRc",
			   xmRowColumnWidgetClass, man_form,
			   XmNleftAttachment,      XmATTACH_FORM,
			   XmNbottomAttachment,    XmATTACH_FORM,
			   XmNorientation,         XmHORIZONTAL,
			   NULL);

    _levelArrowBW[1] = 
	XtVaCreateManagedWidget("stnmw_arrowdown",
				xmArrowButtonWidgetClass,  rc1,
				XmNarrowDirection,         XmARROW_DOWN,
				NULL);
    XtAddCallback(_levelArrowBW[1], XmNactivateCallback,
		  (XtCallbackProc)stnmw_arrowCb, (XtPointer)1);

    _levelArrowBW[0] = 
	XtVaCreateManagedWidget("stnmw_arrowup",
				xmArrowButtonWidgetClass,  rc1,
				XmNarrowDirection,         XmARROW_UP,
				NULL);
    XtAddCallback(_levelArrowBW[0], XmNactivateCallback,
		  (XtCallbackProc)stnmw_arrowCb, (XtPointer)0);

    /*
     * create text input field
     */
    _levelTextW = 
	XtVaCreateManagedWidget("level_txt",
				xmTextFieldWidgetClass,   man_form,
				XmNleftAttachment,	  XmATTACH_WIDGET,
				XmNleftWidget,		  rc1,
				XmNleftOffset,		  15,
				XmNbottomAttachment,	  XmATTACH_FORM,
				XmNcolumns,		  8,
				XmNmarginHeight,	  0, 
				NULL);

    XtAddCallback(_levelTextW, XmNactivateCallback, stnmw_textfCb, NULL );
    XtAddCallback(_levelTextW, XmNlosingFocusCallback, stnmw_textfCb, NULL );
	
    XtManageChild(rc1);

    XtManageChild(man_form);
    XtManageChild(lv_form); 
    XtManageChild(frame);

    _levelPane = frame;

    /*
     * create filter selection area 
     */
    stnmw_createFilter(pane, &_stnmFilterW);

    /*
     * create station model edit button
     */
    bb = XtVaCreateWidget("bb",
			  xmBulletinBoardWidgetClass, pane,
			  NULL );
    button = stnmw_createStnmIcon(bb);
    XtVaSetValues(button, XmNx, 65, NULL);
    XtManageChild(bb);

    /*
     * create control buttons
     */
    NxmCtlBtn_create(pane, 1, "stnmw_ctlBtn", XtNumber(btnstr), 
				btnstr, (XtCallbackProc)stnmw_ctlBtnCb, NULL);

    XtManageChild(pane);

    stnmw_createModel(parent);

    return(_stnmPopW);
}

/*=====================================================================*/

void stnmw_popup ( char *ctype, char *stnm_alias, int index )
/************************************************************************
 * stnmw_popup								*
 *									*
 * This function pops up the STNM parm editing popup window.		*
 *									*
 * void stnmw_popup (ctype, stnm_alias, index)				*
 *									*
 * Input parameters:							*
 *	*ctype		char	data type string			*
 *	*stnm_alias	char	station model alias			*
 *	index		int	attribute index				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	reworked from various popups and inits	*
 * S. Jacobs/NCEP	 5/00	Use sub cat number to check data type	*
 * T. Lee/SAIC		01/03	add surface level			*
 * T. Lee/SAIC		09/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS          *
 * F. J. Yen/NCEP	 4/08	add bin mins and mstrct to ctb_dtget CSC*
 ***********************************************************************/
{
    int		ii, ibscat, isub, ilevel, iret;
    char	d1[256], d2[256];
    int		d3, d4, d5, d6, d7, d7m, d8, d8m, mstrct, dionoff, dtmch;
    char	level[40], text[40], filter[40], alias[40], cycle[40];
    char	title[50], levelstr[20], vcord[5];
/*---------------------------------------------------------------------*/

    /*
     * bring down this dialog if it was up
     */
    stnmw_popdown ();
 
    /*
     * bring down the station model editing window if it was up
     */
    stnmw_popdownModel ();

    _attrIndex = index;

    strcpy(_typeStr, ctype);
    strcpy(_stnmAlias, stnm_alias);

    ctb_dtget ( ctype, d1, d2, &d3, &isub, &d4, &d5, &d6, 
		&dionoff, &d7, &d7m, &d8, &d8m, &mstrct, &dtmch, &iret );

    if ( isub == SCAT_SND || isub == SCAT_SNF ) {
	nsn_qatt (_attrIndex, alias, &ibscat, cycle, _stnmEdit.parm, 
		  _stnmEdit.colors, level, _stnmEdit.vcoord, 
		  filter, text, &iret);
    /*
     * handle surface level 
     */
	_sfcChangeFlg = False;
	if ( strcmp (level,"0") == 0 ) {
	    strcpy ( vcord, _stnmEdit.vcoord );
	    if ( strcmp ( vcord, "PRES" ) == 0 ) {
		_stnmLevel = 1000;
		_sfcChangeFlg = True;
	    }
	    else if ( strcmp ( vcord, "THTA" ) == 0 ) {
		_stnmLevel = 250;
		_sfcChangeFlg = True;
	    }
	    else {
		if ( strcmp ( vcord, "HGHT" ) == 0 ) {
		    _stnmLevel = 0;
		    _sfcChangeFlg = True;
		}
	    }
	    ilevel = 0;
	    stnmw_lvlSelSensitive (False);
	}
	else {
	    ilevel = 1;
	    _stnmLevel = atoi (level);
	    stnmw_lvlSelSensitive (True);
	}
	for ( ii = 0; ii < MAX_LEVEL; ii++ ) {
	    if ( ii == ilevel )
		XmToggleButtonSetState ( _levelSelW[ii], True, True );
	    else
		XmToggleButtonSetState ( _levelSelW[ii], False , False );
	}
    }
    else {
	nsf_qatt (_attrIndex, alias, &ibscat, cycle, _stnmEdit.parm, 
		  _stnmEdit.colors, filter, text, &iret);
    }

    stnmw_initTxt (text);
    _stnmEdit.filter = (float) atof (filter);

    _stnmFfgFlg = (Boolean)((strcmp (ctype, "FFG") == 0) ? TRUE : FALSE);

    _dtypeChangeFlg  = True;
    _stnmSaveLastFlg = False;
    _nDeselect       = -1;

    /*
     * set title
     */
    sprintf(title, "%s Parm Edit", _typeStr);
    XtVaSetValues(XtParent(_stnmPopW),
		  XmNtitle, title,
		  NULL);

    if  ( isub == SCAT_SND || isub == SCAT_SNF ) {
	if (!(XtIsManaged (_vcoordPane))) {
	    XtManageChild (_vcoordPane);
	    XtManageChild (_levelPane);
	}

	/*
	 * initialize VCOORD and LEVEL
	 */
	stnmw_setVcoord (_stnmEdit.vcoord);

	XmScaleSetValue(_levelScaleW, _stnmLevel);

	sprintf(levelstr, "%d", _stnmLevel);
	XmTextSetString(_levelTextW, levelstr);
    }
    else {
	if (XtIsManaged (_vcoordPane)) {
	    XtUnmanageChild (_vcoordPane);
	    XtUnmanageChild (_levelPane);
	}
    }

    /* 
     * initialize the filter scale 
     */
    stnmw_initFilter ();

    XtManageChild(_stnmPopW);
}

/*=====================================================================*/

void stnmw_popdown ( void )
/************************************************************************
 * stnmw_popdown                                             		*
 *                                                                      *
 * This function pops down the STNM parm editing popup window.    	*
 *                                                                      *
 * void stnmw_popdown()                   				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96  						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_stnmPopW)) {
	XtUnmanageChild(_stnmPopW);
    }
}

/*=====================================================================*/

Boolean stnmw_isUp ( void )
/************************************************************************
 * stnmw_isUp								*
 *									*
 * This function checks to see if _stnmPopW is up.			*
 *									*
 * Boolean stnmw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * stnmw_isUp	Boolean		the current state of the popup		*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 ***********************************************************************/
{
    return (XtIsManaged (_stnmPopW));
}

/*=====================================================================*/

Widget stnmw_createModel ( Widget parent )
/************************************************************************
 * stnmw_createModel                                              	*
 *                                                                      *
 * This function creates a station model editing popup.   		*
 *                                                                      *
 * Widget stnmw_createModel ( parent )                			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent form widget ID                          *
 *                                                                      *
 * Output parameters:                                                   *
 * stnmw_createModel	Widget       station model popup widget ID 	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96  						*
 * S. Wang/GSC     04/97	add parm table, weather symbol size  	*
 * S. Wang/GSC     09/97	colrw_*() -> NxmColrP_*()		*
 * G. Krueger/EAI  10/97	NxmFrameLabel->NxmLabel_createFrameLbl	*
 * G. Krueger/EAI  10/97	NxmControlBtn->NxmCtlBtn_create 	*
 * C. Lin/EAI      03/98  	modify size scale min/max, _nDeselect	*
 * I. Durham/GSC   05/98	changed underscore decl. to an include	*
 * T. Piper/SAIC	05/03	replaced XAllocNamedColor w/xsncolr	*
 ***********************************************************************/
{
Widget        pane, form, rc, frame, pane_fr;
XGCValues     gcval;
char          *btnstr[]={"Accept", "Close"};
int		ier;
/*---------------------------------------------------------------------*/

	/*
	 * get _blackPixel and _whitePixel
	 */
	xsncolr("black", &_blackPixel, &ier);
	xsncolr("white", &_whitePixel, &ier);

	_xmNoparm = XmStringCreateLocalized("parm");

        /*
         * create dialog shell
         */
        _stnmModW = XmCreateFormDialog(parent, "stnmw_popupModel", NULL, 0);
        XtVaSetValues(_stnmModW, XmNnoResize, True, NULL);
        XtVaSetValues(XtParent(_stnmModW),
                        XmNtitle, "Edit Station Model Parameter",
                        NULL);

        /*
         * create a parent pane widget
         */
        pane = XtVaCreateWidget("stnmw_pane",
                        xmPanedWindowWidgetClass, _stnmModW,
                        XmNsashWidth,             1,
                        XmNsashHeight,            1,
                        NULL);

	/*
	 * create text size selection
	 */
	stnmw_createTxtEdit(pane);

	/*
	 * create station model element layout 
	 */
	stnmw_createStnmLayout(pane);

	/*
	 * create parm list, weather symbol size & color
	 */
        rc = XtVaCreateWidget("stnmw_rc",
                        xmRowColumnWidgetClass,   pane,
                        XmNorientation,           XmHORIZONTAL,
                        XmNspacing,               10,
                        XmNmarginWidth,           20,
                        NULL);

	/*
	 * parm list
	 */
        frame = XtVaCreateWidget("stnmw_parmListFrame",
                        xmFrameWidgetClass,  rc,
                        NULL);
        pane_fr = XtVaCreateManagedWidget("stnmw_parmListFrame",
                        xmPanedWindowWidgetClass,  frame,
                	XmNsashWidth,              1,
                	XmNsashHeight,             1,
                        NULL);
	NxmLabel_createFrameLbl("Parameter List", pane_fr, frame);

        _stnmParmListW = XmCreateScrolledList(pane_fr,
                        "stnmw_parmList", NULL, 0);
        XtVaSetValues(_stnmParmListW,
                        XmNselectionPolicy,  XmSINGLE_SELECT,
                        XmNscrollingPolicy,  XmAUTOMATIC,
                        NULL);

        XtManageChild(_stnmParmListW);
        XtAddCallback(_stnmParmListW, XmNsingleSelectionCallback,
                                stnmw_parmListCb, NULL);
	
	XtManageChild(frame);

	/*
	 * weather symbol size
	 */
        frame = XtVaCreateWidget("stnmw_scaleFrame",
                        xmFrameWidgetClass,  rc,
                        NULL);
        pane_fr = XtVaCreateManagedWidget("stnmw_scaleFrame",
                        xmPanedWindowWidgetClass,  frame,
                        XmNsashWidth,              1,
                        XmNsashHeight,             1,
                        NULL);
        NxmLabel_createFrameLbl("Size", pane_fr, frame);

	_smblSizeW = XtVaCreateManagedWidget("stnmw_Scale",
			xmScaleWidgetClass,	pane_fr,
			XmNorientation,		XmVERTICAL,
			XmNprocessingDirection, XmMAX_ON_BOTTOM, 
			XmNshowValue,		True,
			XmNscaleMultiple,	1,
			XmNminimum,		(int)((SCALE_MINVAL)*10.),
			XmNmaximum,		SCALE_MAXVAL*10,
			XmNvalue,		(int)((SCALE_MINVAL)*10.),
			XmNdecimalPoints,       1,
			XmNheight, 		85, 
			NULL);

	XtAddCallback(_smblSizeW, XmNdragCallback,
                      (XtCallbackProc)stnmw_smblScaleCb, NULL);
        XtAddCallback(_smblSizeW, XmNvalueChangedCallback,
                      (XtCallbackProc)stnmw_smblScaleCb, NULL);
	XtManageChild(frame);
	
	/*
	 * color list
	 */
        frame = XtVaCreateWidget("stnmw_colorFrame",
                        xmFrameWidgetClass,  rc,
                        NULL);
        pane_fr = XtVaCreateManagedWidget("stnmw_colorFrame",
                        xmPanedWindowWidgetClass,  frame,
                	XmNsashWidth,              1,
                	XmNsashHeight,             1,
                        NULL);
	NxmLabel_createFrameLbl("Color", pane_fr, frame);

	_stnmColrW = NxmColrP_create(pane_fr, 4, 1, (XtEventHandler)stnmw_colorCb);
	XtManageChild(frame);

	_parmBG = NxmColrP_getColorPixel(0);

	XtManageChild(rc);

	/*
	 * create control buttons
	 */
        form = NxmCtlBtn_create(pane, 1,  "stnmw_ctlBtn", XtNumber(btnstr),
                                btnstr, (XtCallbackProc)stnmw_modCtlBtnCb, NULL);
        XtVaSetValues(form, XmNmarginHeight, 15, NULL);

        XtManageChild(pane);

	XtVaGetValues(frame, XmNbackground, &_noparmBG, NULL);

	XtRealizeWidget(_stnmModW);

	/*
	 * create _parmGC
	 */
	gcval.foreground = _parmBG;
	gcval.line_width = 2;
        _parmGC = XCreateGC(XtDisplay(parent), _stnmElmW[0].pxm,
                                  GCForeground|GCLineWidth, &gcval);
	stnmw_setFont(21, 1.0F);

	_nDeselect = -1;

        return(_stnmModW);

}

/*=====================================================================*/

Widget stnmw_createFilter ( Widget parent, Widget *scaleptr )
/************************************************************************
 * stnmw_createFilter                                              	*
 *                                                                      *
 * This function creates a filter scale widget for station model.   	*
 *                                                                      *
 * Widget stnmw_createFilter ( parent, scaleptr )               	*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent form widget ID                          *
 *                                                                      *
 * Output parameters:                                                   *
 *  *scaleptr    	Widget  	pointer to a scale widget       *
 *  stnmw_createFilter	Widget		filter scale frame widge ID 	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96  						*
 * G. Krueger/EAI  10/97  NxmFrameLabel->NxmLabel_createFrameLbl	*
 * C. Lin/EAI      03/98  modify to use stnmw_filterCb			*
 ***********************************************************************/
{
Widget frame, scale, pane_fr;
/*---------------------------------------------------------------------*/

	/*
	 * create filter selection area 
	 */
	frame = XtVaCreateWidget("stnmw_filterFrame",
                        xmFrameWidgetClass,  parent,
                        NULL);
	pane_fr = XtVaCreateManagedWidget("stnmw_filterFrame",
                        xmPanedWindowWidgetClass,  frame,
                	XmNsashWidth,              1,
                	XmNsashHeight,             1,
                        NULL);
	NxmLabel_createFrameLbl("Filter", pane_fr, frame);

        scale = XtVaCreateManagedWidget("scale",
                        xmScaleWidgetClass,     pane_fr,
                        XmNorientation,         XmHORIZONTAL,
                        XmNprocessingDirection, XmMAX_ON_RIGHT,
                        XmNshowValue,           True,
                        XmNminimum,             0,
                        XmNmaximum,             200,
                        XmNvalue,               0,
                        XmNdecimalPoints,       2,
                        NULL);

        XtAddCallback(scale, XmNdragCallback, 
		      stnmw_filterCb, NULL);
        XtAddCallback(scale, XmNvalueChangedCallback,
		      stnmw_filterCb, NULL);

	XtManageChild(frame);

	*scaleptr = scale;

	return(frame);

}

/*=====================================================================*/

Widget stnmw_createStnmIcon ( Widget parent )
/************************************************************************
 * stnmw_createStnmIcon                                              	*
 *                                                                      *
 * This function creates a station model editor icon.   		*
 *                                                                      *
 * Widget stnmw_createStnmIcon ( parent )                   		*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent form widget ID                          *
 *                                                                      *
 * Output parameters:                                                   *
 * stnmw_createStnmIcon	Widget   station model editing icon widget ID  	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96  						*
 * C. Lin/EAI      03/98      remove callback input parameter		*
 * R. Tian/SAIC    01/03        add True flag to NxmBxmBtn_create(Multi)*
 ***********************************************************************/
{
Widget button;
char   iconfile[256];
long   ignore;
int    iret;

/*---------------------------------------------------------------------*/

        cfl_inqr("stnm.xbm", ICON_DIR, &ignore, iconfile, &iret);

        button = NxmBxmBtn_create( parent,
                        "stnm", NULL, 32, 32,
                        ICON_FGNAME , ICON_BGNAME, NULL,
                        iconfile, NULL, True, stnmw_iconCb, 0 );
	return(button);

}

/*=====================================================================*/

Widget stnmw_createTxtEdit ( Widget parent )
/************************************************************************
 * stnmw_createTxtEdit                                              	*
 *                                                                      *
 * This function creates text editing panel.   				*
 *                                                                      *
 * Widget stnmw_createTxtEdit(parent)                   		*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent form widget ID                          *
 *                                                                      *
 * Output parameters:                                                   *
 * stnmw_createTxtEdit	Widget   frame widget ID for text edit panel    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96  						*
 * G. Krueger/EAI  10/97  NxmFrameLabel->NxmLabel_createFrameLbl	*
 * C. Lin/EAI      08/96  modified to call ctb_fszXXX			*
 ***********************************************************************/
{
int      nfsz, iret;
char	 fsznam[20];
long	ii;
Widget   frame, rc, rc1, rc2, rc3, button;
Widget   txt_menu, font_menu, style_menu, pane_fr;
XmString xmstr;
Arg	 args[10];
Cardinal argcnt;
/*---------------------------------------------------------------------*/

	frame = XtVaCreateWidget("stnmw_modelFrame",
                        xmFrameWidgetClass,  parent,
                        NULL);
	pane_fr = XtVaCreateManagedWidget("stnmw_modelFrame",
                        xmPanedWindowWidgetClass,  frame,
                	XmNsashWidth,              1,
                	XmNsashHeight,             1,
                        NULL);
	NxmLabel_createFrameLbl("Text", pane_fr, frame);

	rc = XtVaCreateWidget("stnmw_modelRc",
                        xmRowColumnWidgetClass,  pane_fr,
                        XmNorientation,          XmHORIZONTAL,
                        NULL);

	/*
	 * create size selection area 
	 */
        txt_menu = XmCreatePulldownMenu(rc, "menu", NULL, 0);

        argcnt=0;
        xmstr = XmStringCreateLocalized("size");
        XtSetArg(args[argcnt], XmNsubMenuId,   txt_menu); argcnt++;
        XtSetArg(args[argcnt], XmNlabelString, xmstr); argcnt++;
        XtSetArg(args[argcnt], XmNmarginHeight,0); argcnt++;
        rc1 = XmCreateOptionMenu(rc, "stnmw_txtSizeOpt", args, argcnt);
        XmStringFree(xmstr);
        _txtSizeW = XmOptionButtonGadget(rc1);

	ctb_fszqn(&nfsz, &iret);
        for (ii = 0; ii < (long)nfsz; ii++) {
		ctb_fsznam(ii, fsznam, &iret);
                button = XtVaCreateManagedWidget(fsznam,
                        xmPushButtonGadgetClass, txt_menu,
                        NULL );
                XtAddCallback(button, XmNactivateCallback,
                        (XtCallbackProc)stnmw_txtsizeOptCb,
                        (XtPointer)ii);
        }
	XtManageChild(rc1);

	/*
	 * create font selection area
	 */
        font_menu = XmCreatePulldownMenu(rc, "menu", NULL, 0);

        argcnt=0;
        xmstr = XmStringCreateLocalized("font");
        XtSetArg(args[argcnt], XmNsubMenuId,   font_menu); argcnt++;
        XtSetArg(args[argcnt], XmNlabelString, xmstr); argcnt++;
        XtSetArg(args[argcnt], XmNmarginHeight,0); argcnt++;
        rc2 = XmCreateOptionMenu(rc, "stnmw_txtFontOpt", args, argcnt);
        XmStringFree(xmstr);
        _txtFontW = XmOptionButtonGadget(rc2);

        for (ii = 0; ii < (long)XtNumber(_txtFont); ii++) {
                button = XtVaCreateManagedWidget(_txtFont[ii],
                        xmPushButtonGadgetClass, font_menu,
                        NULL );
                XtAddCallback(button, XmNactivateCallback,
                        (XtCallbackProc)stnmw_txtfontOptCb,
                        (XtPointer)ii);
        }

	XtManageChild(rc2);

	/*
	 * create style option
	 */
        style_menu = XmCreatePulldownMenu(rc, "menu", NULL, 0);

        argcnt=0;
        xmstr = XmStringCreateLocalized("style");
        XtSetArg(args[argcnt], XmNsubMenuId,   style_menu); argcnt++;
        XtSetArg(args[argcnt], XmNlabelString, xmstr); argcnt++;
        XtSetArg(args[argcnt], XmNmarginHeight,0); argcnt++;
        rc3 = XmCreateOptionMenu(rc, "stnmw_txtStyleOpt", args, argcnt);
        XmStringFree(xmstr);
        _txtStyleW = XmOptionButtonGadget(rc3);

        for (ii = 0; ii < (long)XtNumber(_txtStyle); ii++) {
                button = XtVaCreateManagedWidget(_txtStyle[ii],
                        xmPushButtonGadgetClass, style_menu,
                        NULL );
                XtAddCallback(button, XmNactivateCallback,
                        (XtCallbackProc)stnmw_txtstyleOptCb,
                        (XtPointer)ii);
        }

	XtManageChild(rc3);

	XtManageChild(rc);
	XtManageChild(frame);

	return(frame);

}

/*=====================================================================*/

Widget stnmw_createStnmLayout ( Widget parent )
/************************************************************************
 * stnmw_createStnmLayout                                              	*
 *                                                                      *
 * This function creates a station model layout.   			*
 *                                                                      *
 * Widget stnmw_createStnmLayout(parent)                   		*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent form widget ID                          *
 *                                                                      *
 * Output parameters:                                                   *
 * stnmw_createStnmLayout	Widget    station model layout bulletin *
 *							board widget ID *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	06/96  						*
 * S. Wang/GSC     	09/96  	add clear button			*
 * E. Safford/GSC	12/98	check XmVERSION as well as XmREVISION	*
 * T. Piper/SAIC	03/05	Removed XmVERSION/XmREVISION check	*
 ***********************************************************************/
{
int      x;
unsigned long	ii;
Widget   bb, rc, frame, button;

int      id[] = {1, 9, 3, 2, 0, 4, 5, 10, 6};
/*---------------------------------------------------------------------*/
	
	bb = XtVaCreateWidget("stnmw_layoutBb",
                        xmBulletinBoardWidgetClass,  parent,
                        NULL);

	x = STNM_ELM_FRAMEW+70;

	frame = stnmw_createStnmElm(bb, "stnmw_elem7", 7);

	XtVaSetValues(frame,
			XmNx, x,
			NULL);

	rc = XtVaCreateWidget("stnmw_layoutRc",
                        xmRowColumnWidgetClass,  bb,
                        XmNpacking,              XmPACK_COLUMN,
                        XmNorientation,          XmHORIZONTAL,
                        XmNnumColumns,           3,
                        XmNspacing,              10,
                        XmNmarginWidth,          50,
                        XmNy,              	 STNM_ELM_FRAMEH+15,
                        NULL);

	for ( ii = 0; ii < XtNumber(id); ii++) {
	    stnmw_createStnmElm(rc, NULL, id[ii]);
	}

	XtManageChild(rc);

	frame = stnmw_createStnmElm(bb, "stnmw_elem8", 8);
	XtVaSetValues(frame,
			XmNx, x,
			XmNy, 4*(STNM_ELM_FRAMEH+10)+5,
			NULL);

	/*
	 * create a default button
	 */
	button = XtVaCreateManagedWidget("Default",
                        xmPushButtonWidgetClass, bb,
                        XmNx,              	 15,
                        XmNy,              	 10,
                        XmNmarginWidth,       2,
                        NULL);

        XtAddCallback(button, XmNactivateCallback,
                        stnmw_defaultCb, NULL);

	/*
	 * create a clear button
	 */
	button = XtVaCreateManagedWidget("Clear",
                        xmPushButtonWidgetClass, bb,
			XmNx,			 95,
                        XmNy,              	 10,
                        XmNmarginWidth,        2,
                        NULL);

        XtAddCallback(button, XmNactivateCallback,
	                        stnmw_clearCb, NULL);

	XtManageChild(bb);

	return(bb);

}

/*=====================================================================*/

Widget stnmw_createStnmElm ( Widget parent, char *name, long index )
/************************************************************************
 * stnmw_createStnmElm                                              	*
 *                                                                      *
 * This function creates a station model element.   			*
 *                                                                      *
 * Widget stnmw_createStnmElm(parent, name, index)                  	*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent form widget ID                          *
 *  *name        char    element parameter name                         *
 *  index        int     element index                         		*
 *                                                                      *
 * Output parameters:                                                   *
 * stnmw_createStnmElm	Widget  station parameter element composite 	*
 *						       	      widget ID *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	06/96  						*
 * E. Safford/GSC	04/99	fix irix6 compiler warning		*
 ***********************************************************************/
{
int      xdepth;
Drawable screen;
Widget   frame, form, rc;
stnmw_t  *stnmw;
Display  *dsp;
/*---------------------------------------------------------------------*/
	
	if ( index >= STNM_ELM_NUM ) return (Widget)NULL;

	stnmw = &_stnmElmW[index];

	frame = XtVaCreateWidget("stnmw_elemFrame",
                        xmFrameWidgetClass,  parent,
			XmNwidth,	     STNM_ELM_FRAMEW,
			XmNheight,	     STNM_ELM_FRAMEH,
			XmNshadowType,	     XmSHADOW_OUT,
			XmNmarginHeight,     5,
                        NULL);
	
	form = XtVaCreateWidget("stnmw_elemForm",
                        xmFormWidgetClass,   frame,
                        NULL);

	/*
	 * create the check button + parm push button
	 */
	rc = XtVaCreateWidget("stnmw_elemRc1",
                        xmRowColumnWidgetClass,  form,
                        XmNorientation,          XmVERTICAL,
                        XmNspacing,          	 5,
                        NULL);

	stnmw->checkf = XtVaCreateManagedWidget("",
                        xmFrameWidgetClass,    rc,
                        XmNmarginWidth,        0,
                        XmNmarginHeight,       0,
                        NULL);

	stnmw->checkb = XtVaCreateManagedWidget("",
                        xmPushButtonWidgetClass,  _stnmElmW[index].checkf,
              		XmNshadowThickness,       0, 
		        XmNwidth,                 10,
                        XmNheight,                10,
                        XmNrecomputeSize,         False,
                        XmNtraversalOn,           False,
                	XmNbackground, 		  _noparmBG,
                        NULL);

        XtAddCallback(stnmw->checkb, XmNactivateCallback,
                      (XtCallbackProc)stnmw_stnmElmCheckCb, (XtPointer)index);

	stnmw->chkstate = 0;

	XtManageChild(rc);
	stnmw->bgw[0] = rc;

	stnmw->drawb = XtVaCreateManagedWidget("parm",
                        xmDrawnButtonWidgetClass, form,
			XmNleftAttachment,	  XmATTACH_WIDGET,
			XmNleftWidget,	          rc,
                        XmNwidth,      	  	  STNM_ELM_BTNW+6,
                        XmNheight,      	  STNM_ELM_BTNH+6,
                        XmNrecomputeSize,      	  False,
              		XmNshadowThickness,       3,
		        XmNhighlightThickness,    0,
                        XmNshadowType,      	  XmSHADOW_OUT,
                        NULL);

        XtAddCallback(stnmw->drawb, XmNactivateCallback,
                      (XtCallbackProc)stnmw_elemParmCb, (XtPointer)index);
        XtAddCallback(stnmw->drawb, XmNexposeCallback,
                      (XtCallbackProc)stnmw_elemExposeCb, (XtPointer)index);

	dsp = XtDisplay(parent);
	xdepth = DefaultDepth((XtPointer)dsp, DefaultScreen((XtPointer)dsp));

	screen = RootWindowOfScreen(XtScreen(stnmw->drawb));

	stnmw->pxm = XCreatePixmap(dsp, screen, 
			STNM_ELM_BTNW, STNM_ELM_BTNH, (unsigned int)xdepth); 

	XtManageChild(form);
	stnmw->bgw[1] = form;

	stnmw->frame = frame;
	stnmw->color = 1;

	XtManageChild(frame);
	return(frame);

}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_filterCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * stnmw_filterCb							*
 *									*
 * This function is the callback for filter scale.			*
 *									*
 * void stnmw_filterCb (wid, clnt, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	not used			*
 *	call		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		03/98						*
 * S. Law/GSC		11/99	changed to use stnmw_setValue		*
 * T. Lee/SAIC		 2/02	able to load from data window		*
 ***********************************************************************/
{
    int		value;
/*---------------------------------------------------------------------*/

    XmScaleGetValue (wid, &value);
    _stnmEdit.filter = (float)value/100.0F;

    stnmw_setValue (SET_FILTER);
    loop_setDataChngd (loop_getCurLoop(), TRUE);
}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_iconCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * stnmw_iconCb                                                     	*
 *                                                                      *
 * This function is the callback for icon button.          		*
 *                                                                      *
 * void stnmw_iconCb(w, clnt, call)                                 	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  clnt          XtPointer  not used                                   *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      3/98                                                 *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

        stnmw_popupModel();

}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_txtsizeOptCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * stnmw_txtsizeOptCb                                                   *
 *                                                                      *
 * This function is the callback for the text size option menu.         *
 *                                                                      *
 * void stnmw_txtsizeOptCb ( w, which, call )                           *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        which option 				*
 *  call          XtPointer  not used                              	*
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 * C. Lin/EAI      8/98	    modified to call ctb_fszval()		*
 ***********************************************************************/
{
int     iret;
float   size;
/*---------------------------------------------------------------------*/

	ctb_fszval((int)which, &size, &iret);
	stnmw_setFont(-1, size);

}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_txtfontOptCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * stnmw_txtfontOptCb                                                   *
 *                                                                      *
 * This function is the callback for the text font option menu.         *
 *                                                                      *
 * void stnmw_txtfontOptCb ( w, which, call )                           *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        which option                               *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 ***********************************************************************/
{
int ifont;
/*---------------------------------------------------------------------*/

	_fontFamily = (int)which;
	ifont = _fontStyle*10 + ((int)which + 1);
	stnmw_setFont(ifont, -1.0F);

}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_txtstyleOptCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * stnmw_txtstyleOptCb                                                  *
 *                                                                      *
 * This function is the callback for the text style option menu.        *
 *                                                                      *
 * void stnmw_txtstyleOptCb ( w, which, call )                          *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        which option                               *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 ***********************************************************************/
{
int ifont;

/*---------------------------------------------------------------------*/

	_fontStyle = (int)which;
	ifont = (int)which*10 + (_fontFamily + 1);
	stnmw_setFont(ifont, -1.0F);

}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_stnmElmCheckCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * stnmw_stnmElmCheckCb                                                 *
 *                                                                      *
 * This function is the callback for the check button of station        *
 * element.         							*
 *                                                                      *
 * void stnmw_stnmElmCheckCb ( w, which, call )                         *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        which button                               *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	9/96                                            *
 ***********************************************************************/
{
stnmw_t  *stnmw;
/*---------------------------------------------------------------------*/

	stnmw = &_stnmElmW[which];

	if ( stnmw->chkstate == 0 ){

		/*
		 * select the parameter
		 */
		if (stnmw->parmstr[0] != '\0') {
			XtVaSetValues(stnmw->checkf,
				XmNshadowType, XmSHADOW_IN,
				XmNbackground, NxmColrP_getColorPixel(stnmw->color),
				NULL);
			XtVaSetValues(stnmw->checkb,
				XmNbackground, NxmColrP_getColorPixel(stnmw->color),
				NULL);
			stnmw->chkstate = 1;
		}
	}
	else {
		/*
		 * de-select the parameter
		 */
		XtVaSetValues(stnmw->checkf,
			XmNshadowType, XmSHADOW_OUT,
			XmNbackground, _noparmBG,
			NULL);
		XtVaSetValues(stnmw->checkb,
			XmNbackground, _noparmBG,
			NULL);

		XtVaSetValues(stnmw->frame,
			XmNshadowType, XmSHADOW_OUT,
			NULL);

		if ( stnmw->parmstr[0] == '\0' ) 
			stnmw_clearParm((int)which);

		stnmw->chkstate = 0;
		stnmw_setElmBgColr((int)which, _noparmBG);

		NxmColrP_deselectAll(_stnmColrW);
	}
}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_elemParmCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * stnmw_elemParmCb    		                                        *
 *                                                                      *
 * This function is the callback for the parm button in each station    *
 * element.         							*
 *                                                                      *
 * void stnmw_elemParmCb ( w, which, call )                          	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        which button                               *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 * S. Wang/GSC     9/96       Make parm a toggle button and fix bugs    *
 * C. Lin/EAI      4/97	      reset the framesave when necessary        *
 * C. Lin/EAI      3/98	      major redesign        			*
 * C. Lin/EAI      6/98	      use XtFree instead of free        	*
 ***********************************************************************/
{
Pixel    	 bgcolr;
int      	 i, nitems;
stnmw_t	 	 *stnmw;
char     	 *name;
unsigned char	 shadow_char;
XmStringTable    xmitems;

static int frame_save;
/*---------------------------------------------------------------------*/

	/*
	 * clear the memory of last button press when necessary
	 */
	if ( !_stnmSaveLastFlg ) {
	    frame_save       = -1;
	    _stnmSaveLastFlg = True;
	}

	stnmw = &_stnmElmW[which];

	bgcolr = (Pixel)NxmColrP_getColorPixel(stnmw->color);
	NxmColrP_setColor(_stnmColrW, stnmw->color);

	XtVaGetValues(stnmw->frame,
		XmNshadowType, &shadow_char,
		NULL);

	if ( shadow_char == XmSHADOW_OUT ) { /* select */

		stnmw->chkstate = 0;
		_parmSelected = (int)which;

		if (frame_save > -1) {

		    if ( _stnmElmW[frame_save].parmstr[0] == '\0' ){ 
			stnmw_clearParm(frame_save);
		    }

		    else if ( _stnmElmW[frame_save].chkstate != 0 ) {
			XtVaSetValues(_stnmElmW[frame_save].checkf,
			    XmNbackground, 
			        NxmColrP_getColorPixel(
					_stnmElmW[frame_save].color),
			    NULL);
		
			XtVaSetValues(_stnmElmW[frame_save].checkb,
			    XmNbackground, 
			        NxmColrP_getColorPixel(
					_stnmElmW[frame_save].color),
			    NULL);
		    }

		    XtVaSetValues(_stnmElmW[frame_save].frame,
			    XmNshadowType, XmSHADOW_OUT,
			    NULL);
	    
	            stnmw_setElmBgColr(frame_save, _noparmBG);

		}

		XtVaSetValues(stnmw->frame,
			XmNshadowType, XmSHADOW_IN,
			NULL);

		stnmw_setElmBgColr((int)which, bgcolr);
		frame_save = (int)which;

		/*
		 * update the parmlist when necessary 
		 */

		if ( _parmTblFlg ) {

		    stnmw_updParmList((int)which);

		    if ( stnmw->parmstr[0] != '\0' ) {

	    	        /*
	     	         * set the parm list
	     	         */
		        XtVaGetValues(_stnmParmListW,
				XmNitems,     &xmitems, 
				XmNitemCount, &nitems, 
				NULL);

		        for ( i = 0; i < nitems; i++ ) {
			    XmStringGetLtoR(xmitems[i], 
				XmFONTLIST_DEFAULT_TAG, &name);
			    if (strncmp(name, stnmw->parmstr, 4) == 0 ){
	    		        XmListSelectPos(_stnmParmListW, i+1, 
						True);
	    			XmListSetPos(_stnmParmListW, i+1);
				break;
			    }
			    XtFree(name);
		        }
		    }
		    else {
  		        XmListDeselectAllItems(_stnmParmListW);
		    }

		    XtSetSensitive( _stnmParmListW, True );
		}
		else
		    XtSetSensitive( _stnmParmListW, False );


		/*
		 * update the size scale
		 */
		if ( stnmw->parmstr[0] != '\0' ) {
		    stnmw_updSizeScale(stnmw->parmstr);
		}
		else {
		    XtSetSensitive( _smblSizeW, False );
		}
	
	    	/*
	     	 * set the color pallette 
	     	 */
	   	NxmColrP_setColor(_stnmColrW, stnmw->color);

        	XtVaSetValues(stnmw->checkf,
                	XmNshadowType, XmSHADOW_IN,
                	XmNbackground, _noparmBG,
                	NULL);
        	XtVaSetValues(stnmw->checkb,
                	XmNbackground, _noparmBG,
                	NULL);

       		stnmw->chkstate = 1;


	}
	else { /* deselect */

		_parmSelected = -1;

		if (stnmw->parmstr[0] != '\0') {
			XtVaSetValues(stnmw->checkf,
				XmNshadowType, XmSHADOW_IN,
				XmNbackground, bgcolr,
				NULL);
			XtVaSetValues(stnmw->checkb,
				XmNbackground, bgcolr,
				NULL);
        		stnmw->chkstate = 1;
		}
		else 
			stnmw_clearParm((int)which);

		XtVaSetValues(stnmw->frame,
			XmNshadowType, XmSHADOW_OUT,
			NULL);
	
		stnmw_setElmBgColr((int)which, _noparmBG);

		XtSetSensitive(_stnmParmListW, False);
		XtSetSensitive(_smblSizeW, False);
	}

}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_elemExposeCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * stnmw_elemExposeCb         		                                *
 *                                                                      *
 * This function is the callback for the expose event of parm button    *
 * in each station element.                                             *
 *                                                                      *
 * void stnmw_elemExposeCb ( w, which, call ) 		                *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        which button                               *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	if (_stnmElmW[which].parmstr[0] != '\0') 
		stnmw_showParm((int)which);

}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_defaultCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * stnmw_defaultCb                                                  	*
 *                                                                      *
 * This function is the default button callback.    			*
 *                                                                      *
 * void stnmw_defaultCb ( w, clnt, call )                           	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  clnt          XtPointer  not used                                   *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 * C. Lin/EAI      4/97	call sfm_getDefStnm()/snm_getDefStnm()          *
 * C. Lin/EAI      5/97	add SHIP type         				*
 * C. Lin/EAI      3/98	major redesign 					*
 * S. Jacobs/NCEP	 5/00	Use sub cat number to check data type	*
 * S. Jacobs/NCEP	 8/00	Added calls to _qatt and _satt funcs	*
 * T. Lee/SAIC		 9/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS          *
 * F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to ctb_dtget CSC*
 ***********************************************************************/
{
    int		iret, isub, iindx;
    char	cclrs[256], ctext[256], cfltr[256], csfpm[256];
    char	csnpm[256], clevl[256], cvcrd[256];

    int		ibscat;
    char	alias[256], cycle[256], parm[256], colors[256];
    char	level[256], vcoord[256], filter[256], text[256];
    
    char	d1[256], d2[256];
    int		d3, d4, d5, d6, d7, d7m, d8, d8m, mstrct, dionoff, dtmch;
/*---------------------------------------------------------------------*/

	ctb_dtget ( _typeStr, d1, d2, &d3, &isub, &d4, &d5, &d6, &dionoff,
		    &d7, &d7m, &d8, &d8m, &mstrct, &dtmch, &iret );

	ctb_plsdef(_typeStr, _stnmAlias, "COLORS", &iret);
	ctb_plsdef(_typeStr, _stnmAlias, "TEXT", &iret);

	ctb_plget(_typeStr, _stnmAlias, "COLORS", cclrs, &iret);
	ctb_plget(_typeStr, _stnmAlias, "TEXT",   ctext, &iret);
	ctb_plget(_typeStr, _stnmAlias, "FILTER", cfltr, &iret);

	if ( isub == SCAT_SND || isub == SCAT_SNF ) {
	    ctb_plsdef(_typeStr, _stnmAlias, "SNPARM", &iret);

	    ctb_plget(_typeStr, _stnmAlias, "SNPARM", csnpm, &iret);
	    ctb_plget(_typeStr, _stnmAlias, "LEVEL",  clevl, &iret);
	    ctb_plget(_typeStr, _stnmAlias, "VCOORD", cvcrd, &iret);

	    nsn_qatt (_attrIndex, alias, &ibscat, cycle, parm,
		      colors, level, vcoord, filter, text, &iret);

	    nsn_satt (_attrIndex, alias, ibscat, cycle, csnpm, 
		      cclrs, clevl, cvcrd, cfltr, ctext, 
		      &iindx, &iret);
	}
	else {
	    ctb_plsdef(_typeStr, _stnmAlias, "SFPARM", &iret);

	    ctb_plget(_typeStr, _stnmAlias, "SFPARM", csfpm, &iret);

	    nsf_qatt (_attrIndex, alias, &ibscat, cycle, parm, 
		      colors, filter, text, &iret);

	    nsf_satt (_attrIndex, alias, ibscat, cycle, csfpm,
		      cclrs, cfltr, ctext, &iindx, &iret);
	}

	stnmw_popup (_typeStr, _stnmAlias, _attrIndex);
	stnmw_setStnm();
	stnmw_popupModel();

}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_clearCb ( Widget w, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * stnmw_clearCb                                                    	*
 *                                                                      *
 * This function is the callback for the clear button.    		*
 *                                                                      *
 * void stnmw_clearCb ( w, clnt, call )                             	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  clnt          XtPointer  not used                                   *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      9/96                                                *
 * C. Lin/EAI       3/98    add greying out parmlist and _smblSizeW     *
 ***********************************************************************/
{
int	i;
/*--------------------------------------------------------------------*/

	_nDeselect    = -1;
	_parmSelected = -1;

	for (i = 0; i < STNM_ELM_NUM; i++) {
		stnmw_clearParm(i);
		stnmw_setElmBgColr( i, _noparmBG );
		_stnmElmW[i].color = 1;
	}

	XtSetSensitive(_stnmParmListW, False);
	XtSetSensitive(_smblSizeW, False);
}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_parmListCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * stnmw_parmListCb                                                  	*
 *                                                                      *
 * This function is the callback for the parm list.    			*
 *                                                                      *
 * void stnmw_parmListCb ( w, clnt, call )                        *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  clnt        XtPointer  client data                                *
 *  call      XtPointer  call back data                             *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 * S. Wang/GSC	   11/96   add weather symbol size scale set ups &      *
 *			   trancate parm after name			*
 * S. Jacobs/NCEP  10/97   Removed the extra "*" before scaling factors	*
 * C. Lin/EAI       3/98   major redesign                               *
 * C. Lin/EAI       6/98   use XtFree instead of free                   *
 ***********************************************************************/
{
    int		nselect, i, isize;
    char	*name;
    XmListCallbackStruct *cbs;

/*---------------------------------------------------------------------*/

	if ( _parmSelected == -1 ) 
	    return;

        XtVaGetValues(w, XmNselectedItemCount, &nselect, NULL);
        if ( nselect == 0 ) 
		return;

	cbs = (XmListCallbackStruct *)call;
	XmStringGetLtoR(cbs->item, XmFONTLIST_DEFAULT_TAG, &name);

	for ( i = 0; i < _gemParms.nparms; i++ ) {
	    if ( strncmp( name, _gemParms.parmarry[i].name, 4) == 0 )
		break;
	}

	strcpy( _stnmElmW[_parmSelected].parmstr, _gemParms.parmarry[i].name ); 
	strcpy( _stnmElmW[_parmSelected].size, _gemParms.parmarry[i].size ); 
	strcpy( _stnmElmW[_parmSelected].others, _gemParms.parmarry[i].others ); 
	stnmw_drawParmStr(_parmSelected);

	isize = 0;
	if ( _gemParms.parmarry[i].size[0] != '\0' ) {
	    isize = (int)(atof(_gemParms.parmarry[i].size)*10.0);
	}
	else if ( _gemParms.parmarry[i].pzeroflg ) {
	    strcpy(_stnmElmW[_parmSelected].size, "1.0");
	    strcpy(_gemParms.parmarry[i].size, "1.0");
	    isize = 10;
	}

	if ( isize ) {
	    XmScaleSetValue( _smblSizeW, isize );
	    XtSetSensitive( _smblSizeW, True );
	}
	else 
	    XtSetSensitive( _smblSizeW, False );

	XtFree(name);
}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_colorCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * stnmw_colorCb                                                  	*
 *                                                                      *
 * This function is the callback for color selection.    		*
 *                                                                      *
 * void stnmw_colorCb ( w, which, call )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        which color                                *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 * S. Wang/GSC     9/96       add shadow_char  parameter and logic      *
 ***********************************************************************/
{
unsigned  char	shadow_char;

/*---------------------------------------------------------------------*/

	NxmColrP_setColor(_stnmColrW, (int)which);

	if ( _parmSelected > -1 ) {
	    XtVaGetValues(_stnmElmW[_parmSelected].frame,
			XmNshadowType, &shadow_char,
			NULL);

	    if ( shadow_char == XmSHADOW_IN ) {
		_stnmElmW[_parmSelected].color = (int)which;
		if ( _stnmElmW[_parmSelected].parmstr[0] != '\0' )
		    stnmw_drawParmStr(_parmSelected);
		stnmw_setElmBgColr(_parmSelected, 
			(Pixel)NxmColrP_getColorPixel((int)which));
	    }
	}
}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_smblScaleCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *call )
/************************************************************************
 * stnmw_smblScaleCb                                                    *
 *                                                                      *
 * This is the callback function of weather symbol size scale.		*
 *                                                                      *
 * void stnmw_smblScaleCb ( w, clnt, call )                             *
 *									*
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  clnt	  XtPointer			                        *
 *  *call          XmScaleCallbackStruct  clint data			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *              NONE        	                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC		11/96						*
 * C. Lin/EAI		03/98    major redesign				*
 ***********************************************************************/
{
int	ignore;
float   fsize;
/*---------------------------------------------------------------------*/

 	fsize = (float)call->value/10.0F;

	/*
	 * update _gemParms array info
	 */
	stnmw_setParmSize(_stnmElmW[_parmSelected].parmstr, fsize, 
				&ignore);

	/*
	 * update _stnmElmW info
	 */
	sprintf(_stnmElmW[_parmSelected].size, "%.1f", fsize);

}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_lvlSelCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * stnmw_lvlSelCb							*
 *									*
 * This function is the callback for surface/level checkbox callback.	*
 *									*
 * void stnmw_lvlSelCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		which button			*
 *	call		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		01/03						*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    _saveLevel = _stnmLevel;

    for (ii=0; ii < MAX_LEVEL; ii++) {
	    XmToggleButtonSetState ( _levelSelW[ii], False, False);
    }

    if ( which == 0 ) {
	_stnmLevel = 0;
        stnmw_lvlSelSensitive (False);
    }
    else {
	XmScaleGetValue (_levelScaleW, &_stnmLevel);
        stnmw_lvlSelSensitive (True);
    }
    stnmw_setValue (SET_LEVEL);

    if ( _saveLevel != _stnmLevel || _sfcChangeFlg )  {
	loop_setDataChngd (loop_getCurLoop(), TRUE);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_levelCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * stnmw_levelCb							*
 *									*
 * This function is the callback for level scale.			*
 *									*
 * void stnmw_levelCb (wid, clnt, call)					*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	not used			*
 *	call		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		 9/96						*
 * C. Lin/EAI		 3/98	remove _snmattrEdit			*
 * S. Law/GSC		11/99	changed to use stnmw_setValue		*
 * T. Lee/SAIC		01/03   able to load from data window		*
 ***********************************************************************/
{
    char	str[100];	
/*---------------------------------------------------------------------*/

    XmScaleGetValue (wid, &_stnmLevel);

    sprintf(str, "%d", _stnmLevel);
    XmTextSetString( _levelTextW, str );

    stnmw_setValue (SET_LEVEL);

    if ( _saveLevel != _stnmLevel || _sfcChangeFlg )  {
	_saveLevel = _stnmLevel;
	loop_setDataChngd (loop_getCurLoop(), TRUE);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_arrowCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * stnmw_arrowCb							*
 *									*
 * This function is the callback for level arrows.			*
 *									*
 * void stnmw_arrowCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		element				*
 *	call		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		 9/96						*
 * C. Lin/EAI		 3/98	remove _snmattrEdit			*
 * S. Law/GSC		11/99	changed to use stnmw_setValue		*
 * T. Lee/SAIC		 1/03   able to load from data window		*
 ***********************************************************************/
{
    int 	ii;
    char	str[100];	
/*---------------------------------------------------------------------*/

    XmScaleGetValue(_levelScaleW, &_stnmLevel);

    if ((which == 0 && _levelDir == XmMAX_ON_LEFT) ||
	 (which == 1 && _levelDir == XmMAX_ON_RIGHT)) {
	for (ii = _mainLevelN-1; ii >= 0; ii--) {
	    if (_mainLevel[ii] < _stnmLevel) {
		_stnmLevel = _mainLevel[ii];
		break;
	    }
	}
    }
    else {
	for (ii = 0; ii < _mainLevelN; ii++) {
	    if (_mainLevel[ii] > _stnmLevel) {
		_stnmLevel = _mainLevel[ii];
		break;
	    }
	}
    }

    sprintf (str, "%d", _stnmLevel);
    XmTextSetString (_levelTextW, str);

    XmScaleSetValue (_levelScaleW, _stnmLevel);

    stnmw_setValue (SET_LEVEL);

    if ( _saveLevel != _stnmLevel || _sfcChangeFlg )  {
	_saveLevel = _stnmLevel;
	loop_setDataChngd (loop_getCurLoop(), TRUE);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_textfCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * stnmw_textfCb							*
 *									*
 * This is the callback function for level text field widget.		*
 *									*
 * void stnmw_textfCb (wid, clnt, call)					*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	resource			*
 *	call		XtPointer	resource			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *                      NONE                                            *
 *									*
 **									*
 * Log:									*
 * S. Wang/GSC		09/96						*
 * C. Lin/EAI		03/98	modify to call ctb_plset to save value	*
 * S. Law/GSC		11/99	changed to use stnmw_setValue		*
 * H. Zeng/XTRIA	12/03   added call to loop_setDataChngd()	*
 ***********************************************************************/
{
    int		int_value, val_scal, iret;
    char	*value, text_str[20];
/*---------------------------------------------------------------------*/

    value = XmTextFieldGetString (wid);

    cst_numb( value, &int_value, &iret);
    if ( iret != 0 ) {
	int_value = _stnmLevel;
    }

    if ( int_value > _levelMax ) {
	int_value = _levelMax;
    }

    if ( int_value < _levelMin ) {
	int_value = _levelMin;
    }

    sprintf(text_str, "%d", int_value);

    if ( strcmp( text_str, value) != 0 ) {
	XmTextSetString( _levelTextW, text_str );
    }

    XmScaleGetValue( _levelScaleW, &val_scal );

    if ( val_scal != int_value ) {
	XmScaleSetValue( _levelScaleW, int_value );
    }

    _stnmLevel = int_value;
    XtFree(value);

    stnmw_setValue (SET_LEVEL);

    if ( _saveLevel != _stnmLevel || _sfcChangeFlg )  {
	_saveLevel = _stnmLevel;
	loop_setDataChngd (loop_getCurLoop(), TRUE);
    }

}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_ctlBtnCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * stnmw_ctlBtnCb                                                       *
 *                                                                      *
 * This function is the callback for level scale.                       *
 *                                                                      *
 * void stnmw_ctlBtnCb ( w, which, call )                               *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long	     element                                    *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 * C. Lin/EAI      2/97  add fadeflg parm in calling dataw_loadFrame    *
 * C. Lin/EAI      3/97  modify for new design    			*
 * M. Li/GSC	   8/00	 added call to loop_setDataChngd		*
 * M. Li/GSC	  11/00	 modified mapw_ctlBtnCb				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* Load */

	/*
	 * check the map window
	 */
	if (mapw_isUp()) {
	    mapw_ctlBtnCb(NULL, 2, NULL);
	}

	loop_setDataChngd (loop_getCurLoop(), TRUE);	
	dataw_loadData ();
	break;

      case 1:	/* Close */

	stnmw_popdown();
	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void stnmw_modCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * stnmw_modCtlBtnCb							*
 *									*
 * This function is the callback for control button at the bottom.	*
 *									*
 * void stnmw_modCtlBtnCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		which button			*
 *	call		XtPointer	not used			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *                      NONE                                            *
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		 9/96						*
 * C. Lin/EAI		 3/98	major redesign				*
 * C. Lin/EAI		 8/98	use 3 decimal points for text size	*
 * S. Law/GSC		11/99	changed to use nsf_/nsn_ routines	*
 * E. Safford/GSC	05/00	add call to loop_setDataChngd		*
 * S. Jacobs/NCEP	 5/00	Use sub cat number to check data type	*
 * T. Lee/SAIC		 9/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS          *
 * F. J. Yen/NCEP	 4/08   added bin mins & mstrct to ctb_dtget CSC*
 ***********************************************************************/
{
    int		ii, return_idx, ibscat, iret, isub;
    char	txtstr[40], parm[123], colors[123], vcoord[73];
    char	level[73], text[40], filter[10], alias[20], cycle[123];
    char	d1[256], d2[256];
    int		d3, d4, d5, d6, d7, d7m, d8, d8m, mstrct, dionoff, dtmch;
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:    	/* Accept */

	stnmw_popdownModel();

	/*
	 * get parm and color strings
	 */
	stnmw_composeParm(_stnmEdit.parm, _stnmEdit.colors);
	_nDeselect = 0;
	for (ii = 0; ii < STNM_ELM_NUM; ii++) {
	    if (_stnmElmW[ii].parmstr[0] != '\0' &&
		_stnmElmW[ii].chkstate == 0) {
		_deSelected[_nDeselect] = ii;
		_nDeselect++;
	    }
	}

	/*
	 * modify the station model table
	 */
	sprintf(txtstr, "%.3f/%d/%d/111/1/1/", 
		_stnmEdit.txtsiz, _stnmEdit.ifont, 
		_stnmEdit.itxtw);  

	if (_stnmEdit.ihwsw == 1) {
	    strcat(txtstr, "SW\0");
	}
	else {
	    strcat(txtstr, "HW\0");
	}

	ctb_dtget ( _typeStr, d1, d2, &d3, &isub, &d4, &d5, &d6, &dionoff,
		    &d7, &d7m, &d8, &d8m, &mstrct, &dtmch, &iret );

	if ( isub == SCAT_SND || isub == SCAT_SNF ) {
	    nsn_qatt (_attrIndex, alias, &ibscat, cycle, parm,
		      colors, level, vcoord, filter, text, &iret);
	    nsn_satt (_attrIndex, alias, ibscat, cycle, _stnmEdit.parm, 
		      _stnmEdit.colors, level, _stnmEdit.vcoord, 
		      filter, txtstr, &return_idx, &iret);
	}
	else {
	    nsf_qatt (_attrIndex, alias, &ibscat, cycle, parm, 
		      colors, filter, text, &iret);
	    nsf_satt (_attrIndex, alias, ibscat, cycle, _stnmEdit.parm, 
		      _stnmEdit.colors, filter, txtstr, &return_idx, &iret);
	}
		
	loop_setDataChngd (loop_getCurLoop(), TRUE);

	break;

      case 1:    	/* Close */
	_nDeselect = -1;
	stnmw_popdownModel();
	break;
    }
}

/*=====================================================================*/

void stnmw_parmtbl ( char *tblnam )
/************************************************************************
 * stnmw_parmtbl							*
 *                                                                      *
 * This function reads the station model parameter input table.		*
 *                                                                      *
 * void stnmw_parmtbl ( tblnam )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*tblnam		char	GEMAPK parm table name			*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI           03/98	                          		*
 * S. Jacobs/NCEP	 9/02	Increased GEMPARM_MAX from 50 to 100	*
 ***********************************************************************/
{
FILE	*fp;
int	n, n1, p0_flag, iret;
char	buffer[256], parm_buf[40];

/*---------------------------------------------------------------------*/

 	fp = cfl_tbop(tblnam, "nmap", &iret);
        if (iret != 0) {
	    printf("Cannot open station model parm input table %s.\n",
			tblnam);
	    return;
        }

	n  = 0;
	n1 = 0;
        while ( !feof(fp) && n < GEMPARM_MAX ) {

	    cfl_trln( fp, 256, buffer, &iret );
	    if ( iret != 0 )
	    	    break;	

	    sscanf( buffer, "%s %d", parm_buf, &p0_flag );
	
	    stnmw_parseParmComp(parm_buf, _gemParms.parmarry[n].name, 
				      _gemParms.parmarry[n].size, 
				      _gemParms.parmarry[n].others);

	    if (p0_flag) {
		_gemParms.parmarry[n].pzeroflg = True;
		n1++;
	    }
	    else
		_gemParms.parmarry[n].pzeroflg = False;

	    n++;
	}

	fclose(fp);		    

	_gemParms.nparms = n;
	_gemParms.npzero = n1;

}

/*=====================================================================*/

void stnmw_setParm ( int which, char *parmstr, int color )
/************************************************************************
 * stnmw_setParm                                                    	*
 *                                                                      *
 * This function sets the parm for the specified element.               *
 *                                                                      *
 * void stnmw_setParm ( which, parmstr, color )                         *
 *                                                                      *
 * Input parameters:                                                    *
 *  which       int   which element                          		*
 *  *parmstr    char  parm string                          		*
 *  color       int   parm color index					*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		06/96						*
 * S. Jacobs/NCEP	 2/02	Added check for color out of valid range*
 ***********************************************************************/
{
int	 itclr;
stnmw_t  *stnmw;

/*---------------------------------------------------------------------*/

	stnmw = &_stnmElmW[which];

	strcpy(stnmw->parmstr, parmstr);
	
	/*
	 * Check for an out-of-bounds color index.
	 * If the color is less than 1, set it to 1. Otherwise,
	 * compute the mod 32 of the color index.
	 */
	if  ( color <= 0 )  {
	    stnmw->color = 1;
	}
	else {
	    itclr = color % 32;
	    if  ( itclr == 0 )  {
		stnmw->color = 32;
	    }
	    else {
		stnmw->color = itclr;
	    }
	}

	stnmw_drawParmStr(which);

	/*
	 * set the parm selection toggle on
	 */ 
	if ( stnmw_isDeselectParm(which) ) {
            XtVaSetValues(stnmw->checkf,
                        XmNbackground,  _noparmBG,
                        XmNshadowType,  XmSHADOW_OUT,
                        NULL);
            XtVaSetValues(stnmw->checkb,
                        XmNbackground,  _noparmBG,
                        NULL);
	    stnmw->chkstate = 0;
	}
	else {
            XtVaSetValues(stnmw->checkf,
                        XmNbackground,  NxmColrP_getColorPixel(color),
                        XmNshadowType,  XmSHADOW_IN,
                        NULL);
            XtVaSetValues(stnmw->checkb,
                        XmNbackground,  NxmColrP_getColorPixel(color),
                        NULL);
	    stnmw->chkstate = 1;
	}


}

/*=====================================================================*/

void stnmw_clearParm ( int which )
/************************************************************************
 * stnmw_clearParm                                                      *
 *                                                                      *
 * This function clears the parm for specified element.                 *
 *                                                                      *
 * void stnmw_clearParm ( which )                          		*
 *                                                                      *
 * Input parameters:                                                    *
 *  which       int   which element                                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96                                                *
 * S. Wang/GSC	   10/96	add drawn button label type             *
 * C. Lin/EAI      03/98        add size and others                     *
 ***********************************************************************/
{
stnmw_t *stnmw;

/*---------------------------------------------------------------------*/

	stnmw = &_stnmElmW[which];

	stnmw->parmstr[0] = '\0';
	stnmw->size[0]    = '\0';
	stnmw->others[0]  = '\0';
	stnmw->color      = 1;

        XtVaSetValues(stnmw->drawb,
                        XmNforeground,  _whitePixel,
                        XmNbackground,  _noparmBG,
                        XmNlabelString, _xmNoparm,
			XmNlabelType,   XmSTRING,
                        NULL);
        XtVaSetValues(stnmw->checkf,
                        XmNshadowType,  XmSHADOW_OUT,
                        XmNbackground,  _noparmBG,
                        NULL);
        XtVaSetValues(stnmw->checkb,
                        XmNbackground,  _noparmBG,
                        NULL);
        XtVaSetValues(stnmw->frame,
                        XmNshadowType,  XmSHADOW_OUT,
                        NULL);
	
	stnmw->chkstate = 0;

}

/*=====================================================================*/

void stnmw_drawParmStr ( int which )
/************************************************************************
 * stnmw_drawParmStr                                                    *
 *                                                                      *
 * This function draws the parameter string.                   		*
 *                                                                      *
 * void stnmw_drawParmStr ( which )                          		*
 *                                                                      *
 * Input parameters:                                                    *
 *  which       int   which element                                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96                                                *
 ***********************************************************************/
{
int             x, y, w, h, ignore[3], iret;
Display         *dsp;
XGCValues       gcval;
char            parm[5];
stnmw_t         *stnmw;
XCharStruct     overall;

/*---------------------------------------------------------------------*/

	stnmw = &_stnmElmW[which];

	/*
	 * clear background
	 */

        gcval.foreground =  _parmBG;
	
	dsp = XtDisplay(_stnmModW);
	XChangeGC(dsp, _parmGC, GCForeground, &gcval);
	XFillRectangle(dsp, stnmw->pxm, _parmGC, 0, 0,
				STNM_ELM_BTNW, STNM_ELM_BTNH); 
	/*
	 * draw parm string
	 */
        gcval.foreground = (Pixel)NxmColrP_getColorPixel(stnmw->color);
	XChangeGC(dsp, _parmGC, GCForeground, &gcval);

	cst_ncpy(parm, stnmw->parmstr, 4, &iret);

	XTextExtents(_parmFont, parm, 4, &ignore[0], &ignore[1], 
		    &ignore[2], &overall);
	w = (int)overall.width;
	h = overall.ascent + overall.descent;
	x = (STNM_ELM_BTNW - w)/2;
	y = (STNM_ELM_BTNH - h)/2 + overall.ascent;

        XDrawString(dsp, stnmw->pxm, _parmGC, x, y, parm, 4); 
	
	stnmw_showParm(which);

}

/*=====================================================================*/

void stnmw_showParm ( int which )
/************************************************************************
 * stnmw_showParm                                                       *
 *                                                                      *
 * This function displays the parm string for the specified element.    *
 *                                                                      *
 * void stnmw_showParm ( which )                            		*
 *                                                                      *
 * Input parameters:                                                    *
 *  which       int   which element                                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96                                                *
 * S. Wang/GSC	   10/96	add drawn button label type             *
 ***********************************************************************/
{
	XtVaSetValues( _stnmElmW[which].drawb,
                        XmNlabelType,  XmPIXMAP,
                        NULL );

	XCopyArea( XtDisplay(_stnmModW), _stnmElmW[which].pxm,
                        XtWindow(_stnmElmW[which].drawb),
                        _parmGC, 0, 0, STNM_ELM_BTNW, STNM_ELM_BTNH,
                        3, 3 );
}

/*=====================================================================*/

void stnmw_redrawParm ( void )
/************************************************************************
 * stnmw_redrawParm                                                     *
 *                                                                      *
 * This function redraws parm string for each element.    		*
 *                                                                      *
 * void stnmw_redrawParm()                                              *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96                                                *
 ***********************************************************************/
{
int     i;

/*---------------------------------------------------------------------*/

        for ( i = 0; i < STNM_ELM_NUM; i++) {
                if ( _stnmElmW[i].parmstr[0] != '\0' )
                        stnmw_drawParmStr(i);
        }

}

/*=====================================================================*/

void stnmw_composeParm ( char *parmstr, char *colrstr )
/************************************************************************
 * stnmw_composeParm                                                    *
 *                                                                      *
 * This function composes the station model parm string.    		*
 *                                                                      *
 * void stnmw_composeParm ( parmstr, colrstr )                          *
 *                                                                      *
 * Input parameters:                                                    *
 *                      NONE                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *  *parmstr       char   station model parm string 			*
 *  *colrstr       char   station model color string 			*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96                                                *
 * C. Lin/EAI      03/98   use size and others                          *
 ***********************************************************************/
{
int  i;
char cstr[4], parmtmp[40];
/*---------------------------------------------------------------------*/
		
	colrstr[0] = '\0';
	parmstr[0] = '\0';

	stnmw_makeParmComp(0, parmtmp);

	if ( parmtmp[0] != '\0' && _stnmElmW[0].chkstate) {
		sprintf(parmstr, "%s;", parmtmp);
		sprintf(colrstr, "%d;", _stnmElmW[0].color);
	} 
	else {
		strcpy(parmstr, ";");
	}

	for( i = 1; i < STNM_ELM_NUM; i++ ) {

	    	stnmw_makeParmComp(i, parmtmp);

		if ( parmtmp[0] != '\0' && _stnmElmW[i].chkstate ) {
			strcat(parmstr, parmtmp);

			sprintf(cstr, "%d;", _stnmElmW[i].color);
			strcat(colrstr, cstr);
		}

		strcat(parmstr, ";");
	}

	/*
	 * take out extra ';'
	 */
	stnmw_removeExtraChar(parmstr, ';');
	stnmw_removeExtraChar(colrstr, ';');

}

/*=====================================================================*/

int stnmw_isDeselectParm ( int which )
/************************************************************************
 * stnmw_isDeselectParm                                                 *
 *                                                                      *
 * This function checks whether the element is deselected.    		*
 *                                                                      *
 * int stnmw_isDeselectParm ( which )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *  which       int   which element                                     *
 *                                                                      *
 * Output parameters:                                                   *
 * stnmw_isDeselectParm	int       0 - false, 1 - true                   *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96                                                *
 * C. Lin/EAI      03/98    modify to check _nDeselect<=0 from ==0      *
 ***********************************************************************/
{
int i;
/*---------------------------------------------------------------------*/

	if (_nDeselect <= 0 )
		return(0);

	for ( i = 0; i < _nDeselect; i++ ) {
		if ( which == _deSelected[i] ) 
			return(1);
	}

	return(0);
}

/*=====================================================================*/

void stnmw_initParmList ( void )
/************************************************************************
 * stnmw_initParmList                                                   *
 *                                                                      *
 * This function initializes the parameter list widget.                 *
 *                                                                      *
 * void stnmw_initParmList()                              		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI	      03/98    						*
 * T. Piper/SAIC	03/04	Replaced cfl_inqr with cfl_tinq		*
 ***********************************************************************/
{
int  ier, iret;
long flen;
char ctype[20], newfil[256], tblname[80];
/*---------------------------------------------------------------------*/

	cst_uclc(_typeStr, ctype, &ier);

	sprintf(tblname, "%s_parms.tbl", ctype);
	cfl_tinq(tblname, "nmap", &flen, newfil, &iret); 

	if ( iret != 0 ) {

	    _parmTblFlg = False;
	    XmListDeleteAllItems(_stnmParmListW);

	}
	else {

	    _parmTblFlg = True;

	    stnmw_parmtbl(newfil);

            /*
             * set the parm list
             */
	    stnmw_updParmList(0);

	}

}

/*=====================================================================*/

void stnmw_setParmSize ( char *parm, float size, int *inx )
/************************************************************************
 * stnmw_setParmSize                                                    *
 *                                                                      *
 * This function sets the size value in _gemParms data structure.	*
 *                                                                      *
 * void stnmw_setParmSize ( parm, size, inx )                           *
 *                                                                      *
 * Input parameters:                                                    *
 *  *parm	char		name of the parameter			*
 *  size	float		size of the weather symbol		*
 *                                                                      *
 * Input/Output parameters:                                             *
 *  *inx	int		index to the array			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		03/98						*
 ***********************************************************************/
{
int	i;
char    sizestr[10];
/*---------------------------------------------------------------------*/

	if ( !_parmTblFlg )
	    return;

	for ( i = 0; i < _gemParms.nparms; i++) {
	    if  (strncmp(_gemParms.parmarry[i].name, parm, 4) == 0 )
		break;
	}

	if ( i < _gemParms.nparms ) {
	    sprintf(sizestr, "%.1f", size);
	    strcpy(_gemParms.parmarry[i].size, sizestr);
	    *inx = i;
	}
	else
	    *inx = -1;

}

/*=====================================================================*/

void stnmw_updParmList ( int which )
/************************************************************************
 * stnmw_updParmList                                                    *
 *                                                                      *
 * This function updates the parameter list when necessary. 		*
 *                                                                      *
 * void stnmw_updParmList ( which )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  which	int	which position					*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *              NONE        	                                        *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		03/98						*
 ***********************************************************************/
{
int	      ii, jj, nn;
Boolean       poschg_flg;
XmStringTable xmstrs;

static int _last_pos=-1;
/*---------------------------------------------------------------------*/

	if ( !_parmTblFlg )
		return;
 
	poschg_flg = False;

	if ( which != _last_pos ) {

	    if ( which == 0 || _last_pos == 0 )
		poschg_flg = True;

	    _last_pos = which;
	}

	if ( _dtypeChangeFlg || poschg_flg ) {

	    XmListDeleteAllItems(_stnmParmListW);

	    if ( which == 0 ) { /* position 0 */
		nn = _gemParms.nparms;
                xmstrs = (XmStringTable)XtMalloc((size_t)nn * sizeof(XmString *));
                for ( jj = 0; jj < nn; jj ++ ) {
                    xmstrs[jj] = XmStringCreateLocalized(
					_gemParms.parmarry[jj].name);
                }
	    }
	    else { /* other position */
		nn = _gemParms.nparms - _gemParms.npzero;
		xmstrs = (XmStringTable)XtMalloc((size_t)nn * sizeof(XmString *));
                for ( jj = 0, ii = 0; jj < _gemParms.nparms && ii < nn; 
							jj++) {
		    if ( !(_gemParms.parmarry[jj].pzeroflg) ) {
                        xmstrs[ii] = XmStringCreateLocalized(
					_gemParms.parmarry[jj].name);
			ii++;
		    }
                }
	    }

            XtVaSetValues(_stnmParmListW,
                        XmNitems,       xmstrs,
                        XmNitemCount,   nn,
                        NULL);

            for (jj = 0; jj < nn; jj++)
                XmStringFree(xmstrs[jj]);
            XtFree((XtPointer)xmstrs);

	}

}

/*=====================================================================*/

void stnmw_makeParmComp ( int which, char *parmstr )
/************************************************************************
 * stnmw_makeParmComp                                                   *
 *                                                                      *
 * This function cascades the size and other fields with parameter      *
 * name to create a parameter component.  				*
 *                                                                      *
 * void stnmw_makeParmComp ( which, parmstr )	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   which	int	which element					*
 *                                                                      *
 * Output parameters:                                                   *
 *  *parmstr    char	parm component string				*
 *                                                                      *
 * Return parameters:                                                   *
 *              NONE        	                                        *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		03/98						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	parmstr[0] = '\0';
	
 	if ( _stnmElmW[which].parmstr[0] != '\0' ) {

	    strcpy(parmstr, _stnmElmW[which].parmstr); 

	    if ( _stnmElmW[which].size[0] != '\0' ) {

		strcat(parmstr, ":");
		strcat(parmstr, _stnmElmW[which].size);

	        if ( _stnmElmW[which].others[0] != '\0' ) {
		    strcat(parmstr, ":");
		    strcat(parmstr, _stnmElmW[which].others);
		}

	    }
	}

}

/*=====================================================================*/

void stnmw_parseParmComp (char *parmstr, char *name, char *size, char *others )
/************************************************************************
 * stnmw_parseParmComp                                                  *
 *                                                                      *
 * This function parses the input string into parameter name, size      *
 * and others components.  						*
 *                                                                      *
 * void stnmw_parseParmComp ( parmstr, name, size, others )	        *
 *                                                                      *
 * Input parameters:                                                    *
 *   *parmstr	char	input parameter string				*
 *                                                                      *
 * Output parameters:                                                   *
 *  *name    char	GEMPAK parameter name				*
 *  *size    char	GEMPAK parameter size				*
 *  *others  char	GEMPAK parameter others field			*
 *                                                                      *
 * Return parameters:                                                   *
 *              NONE        	                                        *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		03/98						*
 ***********************************************************************/
{
int  len;
char *ptr, *ptr1;
/*---------------------------------------------------------------------*/
	
	name[0] = '\0';
	size[0] = '\0';
	others[0] = '\0';

	ptr  = strchr(parmstr, ':');

	if ( ptr ) {

	    ptr1 = strchr(ptr+1, ':');

	    if ( ptr1 ) {
	        strcpy(others, ptr1+1);

	        len = ptr1 - ptr - 1;
	        strncpy(size, ptr+1, (size_t)len);
	        size[len] = '\0';
	    }
	    else {
	        strcpy(size, ptr+1);
	    }

	    len = ptr - parmstr;
	    strncpy(name, parmstr, (size_t)len);
	    name[len] = '\0';
	}
	else {
	    strcpy(name, parmstr);
	}
}

/*=====================================================================*/

void stnmw_initFilter ( void )
/************************************************************************
 * stnmw_initFilter                                                   	*
 *                                                                      *
 * This function initializes the filter scale.   			*
 *                                                                      *
 * void stnmw_initFilter ()                             		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      3/98                                                 *
 ***********************************************************************/
{
    float	filter;
    int		ival;
/*---------------------------------------------------------------------*/

    filter = (_stnmEdit.filter*100.0F);
    ival = (int) filter;

    XmScaleSetValue (_stnmFilterW, ival);

}

/*=====================================================================*/

void stnmw_popupModel ( void )
/************************************************************************
 * stnmw_popupModel                                                     *
 *                                                                      *
 * This function pops up the station model editing popup window.        *
 *                                                                      *
 * void stnmw_popupModel()                             			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96                                                *
 * S. Wang/GSC	   03/97	add weather symbol editing		*
 * C. Lin/EAI      05/97        take title out from input parm          *
 *				pop down the dialog window if it was up *
 * C. Lin/EAI      03/98        rewrite                                 *
 ***********************************************************************/
{
    char	title[120];

    static char	_last_type[20] = "\0";
    static char	_last_alias[20] = "\0";
/*---------------------------------------------------------------------*/

    stnmw_popdownModel ();
 
    if ( _stnmFfgFlg ) return;

    if ( strcmp(_last_type, _typeStr) == 0  &&
	 strcmp(_last_alias, _stnmAlias) == 0 &&
	 _nDeselect != -1 ) {

	_dtypeChangeFlg  = False;
	_stnmSaveLastFlg = True;
	XtManageChild(_stnmModW);
	return;
    }
    else {
	_dtypeChangeFlg  = True;
	_stnmSaveLastFlg = False;
	_nDeselect       = -1;
	_parmSelected    = -1;
	strcpy(_last_type,  _typeStr);
	strcpy(_last_alias,  _stnmAlias);
    }

    /*
     * set the title
     */
    sprintf(title, "%s Station Model Edit", _typeStr); 
    XtVaSetValues(XtParent(_stnmModW),
		  XmNtitle, title,
		  NULL);

    /*
     * get the parm list
     */
    stnmw_initParmList();
    XtSetSensitive(_stnmParmListW, False);

    stnmw_setStnm();

    XtSetSensitive(_smblSizeW, False);

    XtManageChild(_stnmModW);
}

/*=====================================================================*/

void stnmw_popdownModel ( void )
/************************************************************************
 * stnmw_popdownModel                                                   *
 *                                                                      *
 * This function pops down the station model editing popup window.      *
 *                                                                      *
 * void stnmw_popdownModel()                                            *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96                                                *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged(_stnmModW)) {
        XtUnmanageChild(_stnmModW);
    }
}

/*=====================================================================*/

void stnmw_setStnm ( void )
/************************************************************************
 * stnmw_setStnm                                                  	*
 *                                                                      *
 * This function sets the text attribute, parm, color for each station  *
 * element.								*
 *                                                                      *
 * void stnmw_setStnm()                             			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 * G. Krueger/EAI 10/97	 CST_xLST: Removed RSPTB; Add str limit		*
 * C. Lin/EAI     10/97	 fix calling sequence to cst_ilst()		*
 * C. Lin/EAI     03/98	 major redesign					*
 * C. Lin/EAI     08/98  move free(parms[i]) to fix potential mem leak  *
 ***********************************************************************/
{

int     i, j, count, iret;
int     nparm, ncolr, icolors[STNM_ELM_NUM], color;
char    **parms, name[10];
stnmw_t *stnmw;
/*---------------------------------------------------------------------*/

	/*
	 * set the font and size
	 */
	stnmw_setFont(_stnmEdit.ifont, _stnmEdit.txtsiz);

	/*
	 * get station parms
	 */
	parms = (char **)malloc(STNM_ELM_NUM * sizeof(char *));
	for (i = 0; i < STNM_ELM_NUM; i++)
		parms[i] = (char *)malloc(73);
	cst_clst(_stnmEdit.parm, ';', "\0", STNM_ELM_NUM, 73, parms,
		&nparm, &iret);

	/*
	 * get station colors
	 */
	cst_ilst(_stnmEdit.colors, ';', 0, STNM_ELM_NUM, icolors,
		&ncolr, &iret);

	count = 0;
	for ( i = 0; i < STNM_ELM_NUM; i++) {

	    if ( _nDeselect > 0 && stnmw_isDeselectParm(i) ) {
		    stnmw = &_stnmElmW[i];
		    stnmw_setParm(i, stnmw->parmstr, stnmw->color); 
	    }
	    else {

		if ( i < nparm ) {

	            if ( parms[i][0] == '\0' ) { 
			/* 
			 * not defined 
			 */
			stnmw_clearParm(i);
	            }
	            else {

		        stnmw_parseParmComp(parms[i], name, 
				_stnmElmW[i].size, _stnmElmW[i].others);

	                cst_uclc(name, name, &iret);

	                if ( name[0] == '\0' ||
			    strncmp(name, "spac", 4) == 0 ||
			    strncmp(name, "blnk", 4) == 0 ) {
		            stnmw_clearParm(i);
	                }
	                else {

	                    if ( icolors[count] == 0 )
			        color = 1;
	                    else
			        color = icolors[count];
		            count++;

		            stnmw_setParm(i, name, color); 

	                }
	            } /* parms[i][0] */
		
	        }
		else {
			stnmw_clearParm(i);
	        } /* nparm */

	   }/* _nDeselect */

	   free(parms[i]);

	}

	if ( _parmTblFlg ) {
            /*
             * modify the size and others fields based on the
             * the current station model
             */
            for ( i = 0; i < STNM_ELM_NUM; i++ ) {
                for ( j = 0; j < _gemParms.nparms; j++ ) {
                    if ( strncasecmp( _stnmElmW[i].parmstr,
                            _gemParms.parmarry[j].name, 4 ) == 0 ) {

                        strcpy(_gemParms.parmarry[j].name,
                                         _stnmElmW[i].parmstr);

                        if ( _stnmElmW[i].size[0] != '\0' ) {
                            strcpy(_gemParms.parmarry[j].size,
                                        _stnmElmW[i].size);
                        }
                        else
                            _gemParms.parmarry[j].size[0] = '\0';

                        if ( _stnmElmW[i].others[0] != '\0' ) {
                            strcpy(_gemParms.parmarry[j].others,
                                        _stnmElmW[i].others);
                        }
                        else
                            _gemParms.parmarry[j].others[0] = '\0';

                        break;
                    }
                }
            }
	}

	for ( i = 0; i < STNM_ELM_NUM; i++ ) {
            XtVaSetValues(_stnmElmW[i].frame,
                        XmNshadowType,  XmSHADOW_OUT,
                        NULL);
	    stnmw_setElmBgColr(i, _noparmBG);
	}

  	XmListDeselectAllItems(_stnmParmListW);
	XtSetSensitive( _stnmParmListW, False );
	XtSetSensitive( _smblSizeW, False );
	
	NxmColrP_deselectAll(_stnmColrW);
	_parmSelected = -1;

	free(parms);

}

/*=====================================================================*/
 
void stnmw_setElmBgColr ( int which, Pixel color )
/************************************************************************
 * stnmw_setElmBgColr                                                   *
 *                                                                      *
 * This function sets the background color for the specified station    *
 * element.                 						*
 *                                                                      *
 * void stnmw_setElmBgColr ( which, color )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *  which       int    which element                           		*
 *  color       Pixel  color index to default color map                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96                                                *
 ***********************************************************************/
{  
stnmw_t *stnmw;
/*---------------------------------------------------------------------*/

	stnmw = &_stnmElmW[which];

	XtVaSetValues(stnmw->frame,
			XmNbackground, color,
			NULL);
	XtVaSetValues(stnmw->bgw[0],
			XmNbackground, color,
			NULL);

	XtVaSetValues(stnmw->bgw[1],
			XmNbackground, color,
			NULL);

}

/*=====================================================================*/

void stnmw_removeExtraChar ( char *parmstr, char c )
/************************************************************************
 * stnmw_removeExtraChar                                                *
 *                                                                      *
 * This utility function removes the specified extra character in the   *
 * parm string.   							*
 *                                                                      *
 * void stnmw_removeExtraChar ( parmstr, c )                            *
 *                                                                      *
 * Input parameters:                                                    *
 *  *parmstr       char   parm string                                   *
 *  c              char  extra character                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96                                                *
 * C. Lin/EAI      03/98    remove the char at the end of the string    *
 ***********************************************************************/
{
unsigned int  len;
char *ptr;

/*---------------------------------------------------------------------*/

	len = strlen(parmstr);
	ptr = &parmstr[len-1];
	while( *ptr ) {
		if ( *ptr == c && *(ptr-1) == c ) {
			*ptr = '\0';
			ptr--;
		}
		else
			break;
	}

	if ( *ptr == c )
		*ptr = '\0';

}

/*=====================================================================*/

void stnmw_initTxt ( char *txtstr )
/************************************************************************
 * stnmw_initTxt                                                        *
 *                                                                      *
 * This function finds the index of a named weather symbol in 		*
 * the global symbol list.						*
 *                                                                      *
 * void stnmw_initTxt ( txtstr )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  *txtstr	char	GEMPAK text attribute string			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *              NONE        	                                        *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		03/98						*
 ***********************************************************************/
{
int ibrdr, irrotn, ijust, iret;
/*---------------------------------------------------------------------*/

	in_txtn(txtstr, &(_stnmEdit.ifont), &(_stnmEdit.ihwsw), 
		   	&(_stnmEdit.txtsiz), &(_stnmEdit.itxtw), 
			&ibrdr, &irrotn,
			&ijust, &iret, strlen(txtstr));


}

/*=====================================================================*/

void stnmw_setFont ( int ifont, float size )
/************************************************************************
 * stnmw_setFont                                                        *
 *                                                                      *
 * This function sets the text font for drawing.    			*
 *                                                                      *
 * void stnmw_setFont ( ifont, size )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *  ifont      int     font index                                       *
 *  size       float   font size                                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      06/96                                                *
 * G. Krueger/EAI  10/97	NxmSetLabel->NxmLabel_setStr		*
 * C. Lin/EAI      06/98        bug fix in getting the right size       *
 * C. Lin/EAI      08/98        modified to use font size table         *
 * A. Hardy/GSC    04/00	added font size multiplier		*
 ***********************************************************************/
{
int     iftyp1, iftyp2, isize, imult, iret;
char    fontname[256], fsznam[20], oldfont;
Display *dsp;

static char     *fonts[12] = {
                                "-adobe-courier-medium-r-normal--",
                                "-adobe-helvetica-medium-r-normal--",
                                "-adobe-times-medium-r-normal--",
                                "-adobe-courier-medium-o-normal--",
                                "-adobe-helvetica-medium-o-normal--",
                                "-adobe-times-medium-i-normal--",
                                "-adobe-courier-bold-r-normal--",
                                "-adobe-helvetica-bold-r-normal--",
                                "-adobe-times-bold-r-normal--",
                                "-adobe-courier-bold-o-normal--",
                                "-adobe-helvetica-bold-o-normal--",
                                "-adobe-times-bold-i-normal--"
                        };

static int font_save=-1;
static int size_save=-1;
static int mult_save=-1;

/*---------------------------------------------------------------------*/

        if ( (size < 0.0F && size_save < 0)  ||
                (ifont < 0 && font_save < 0) )
                return;

	oldfont = 0;
	if ( ifont == -1) { 
		ifont = font_save;
		oldfont = 1;
	}
	else {

	    _stnmEdit.ifont = ifont;

            /*
             * Get valid text font and save the values.
             */
            iftyp1 = ifont/10;
            iftyp2 = ifont%10;

            if  ( ( iftyp1 < 0 ) || ( iftyp1 > 3 ) ) iftyp1 = 0;
            if  ( ( iftyp2 < 1 ) || ( iftyp2 > 3 ) ) iftyp2 = 1;

            ifont = iftyp1 * 3 + iftyp2;

	    if ( ifont == font_save )
		oldfont = 1;
	    else
	    	font_save = ifont;

	    _fontStyle  = iftyp1;
	    _fontFamily = iftyp2-1;
	}

        /*
         * Get the size of the font in point values.
         */
	if ( size < 0.0F ) {
	    isize = size_save;
	    imult = mult_save;
	}
	else {

	    ctb_fszfnd(size, &_fontSize, &iret);
	    ctb_fszxvl(_fontSize, &isize, &imult, &iret);

	    _stnmEdit.txtsiz = size;
	    _stnmEdit.mult = imult;

	}

	if ( oldfont && isize == size_save ) {
		return;
	}

	size_save = isize;
	mult_save = imult;

        /*
         * Get the name of the font.
         */
        sprintf ( fontname, "%s%d-%d-*",
                                fonts[ifont-1], isize, imult );

	dsp = XtDisplay(_stnmModW);
	if ( _parmFont ) {
		XFreeFont(dsp, _parmFont);
	}

        _parmFont = XLoadQueryFont(dsp, fontname);
        if (_parmFont) {
                XSetFont(dsp, _parmGC, _parmFont->fid);
        }
	else {
	    stnmw_getDEFfsz(isize, &isize);
            sprintf ( fontname, "%s%d-%d-*",
                                fonts[ifont-1], isize, imult );
            _parmFont = XLoadQueryFont(dsp, fontname);
            XSetFont(dsp, _parmGC, _parmFont->fid);
	}

	/*
	 * set the font option
	 */
	NxmLabel_setStr(_txtFontW, _txtFont[_fontFamily]);

	/*
	 * set the size option
	 */
	ctb_fsznam(_fontSize, fsznam, &iret);
	NxmLabel_setStr(_txtSizeW, fsznam);

	/*
	 * set the style option
	 */
	NxmLabel_setStr(_txtStyleW, _txtStyle[_fontStyle]);

	stnmw_redrawParm();

}

/*=====================================================================*/

void stnmw_getDEFfsz ( int in_size, int *out_size )
/************************************************************************
 * stnmw_getDEFfsz                                                      *
 *                                                                      *
 * This function gets the default X window font size.    		*
 *                                                                      *
 * void stnmw_getDEFfsz ( in_size, out_size )                           *
 *                                                                      *
 * Input parameters:                                                    *
 *  in_size      int     input font size                                *
 *                                                                      *
 * Output parameters:                                                   *
 *  *out_size     int     output font size                              *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      08/98                                                *
 ***********************************************************************/
{
int     i, n, ilow, ihi;
int 	fszarry[] = {10, 12, 14, 18, 24};
/*---------------------------------------------------------------------*/

	n = XtNumber(fszarry);

	ilow = (fszarry[0] + fszarry[1])/2;
	ihi  = (fszarry[n-2] + fszarry[n-1])/2;

	if ( in_size <= ilow )
	    *out_size = fszarry[0];
	else if ( in_size > ihi )
	    *out_size = fszarry[n-1];
	else {
	    for ( i = 1; i < n-1; i++ ) {
		ilow = (fszarry[i-1] + fszarry[i])/2; 
		ihi  = (fszarry[i] + fszarry[i+1])/2; 
		if ( (in_size > ilow) && (in_size <= ihi) ) {
			*out_size = fszarry[i];
			break;
		}
	    }
	}

}

/*=====================================================================*/

void stnmw_updSizeScale ( char *parm )
/************************************************************************
 * stnmw_updSizeScale                                                   *
 *                                                                      *
 * This function updates the size scale. 				*
 *                                                                      *
 * void stnmw_updSizeScale ( parm )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  *parm	char	parameter name					*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *              NONE        	                                        *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		03/98						*
 ***********************************************************************/
{
int	i, isize;
/*---------------------------------------------------------------------*/

	isize = 0;

	if ( _parmTblFlg ) {
	    for ( i = 0; i < _gemParms.nparms; i ++ ) {
	        if ( strncmp(_gemParms.parmarry[i].name, parm, 4) == 0 )
		    break;
	    }


	    if ( _gemParms.parmarry[i].size[0] != '\0' ) {
	        isize = (int)(atof( _gemParms.parmarry[i].size)*10.0);
	    }
	    else if ( _gemParms.parmarry[i].pzeroflg ) {
	        strcpy(_gemParms.parmarry[i].size, "1.0");
	        strcpy(_stnmElmW[_parmSelected].size, "1.0");
	        isize = 10;
	    }
	}
	else {
	   if ( _stnmElmW[_parmSelected].size[0] != '\0' )
		isize = (int)(atof( _stnmElmW[_parmSelected].size)*10.0);
	}

	if ( isize ) {
	    XmScaleSetValue(_smblSizeW, isize);
	    XtSetSensitive(_smblSizeW, True);
	}
	else 
	    XtSetSensitive(_smblSizeW, False);

}

/*=====================================================================*/

void stnmw_setVcoord ( char *vcoord )
/************************************************************************
 * stnmw_setVcoord                                                      *
 *                                                                      *
 * This function sets vertical coordinates.                       	*
 *                                                                      *
 * void stnmw_setVcoord(vcoord)                                    	*
 *                                                                      *
 * Input parameters:                                                    *
 *  *vcoord   char          VCOORD string                               *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 * C. Lin/EAI      3/97                                                 *
 * G. Krueger/EAI 10/97	NxmSetLabel->NxmLabel_setStr			*
 * C. Lin/EAI      3/98 remove _snmattrEdit                             *
 * S. Jacobs/NCEP  5/98	Added mandatory pressure levels above 100 mb	*
 ***********************************************************************/
{
int    i, n;
char   title[80], flag;

int    pres_step[] = {10, 20, 30, 50, 70, 100, 150, 200, 250, 300,
			400, 500, 700, 850, 925, 1000};
int    hght_step[] = {0, 2000, 4000, 6000, 8000, 10000, 12000, 14000,
                        16000, 18000};
int    thta_step[] = {250, 270, 290, 300, 310, 320, 330, 340, 350, 360,
                        380, 410};

/*---------------------------------------------------------------------*/

	/*
	 * set the level information
	 */
	flag = 1;
	if ( strcmp(vcoord, "PRES") == 0 ) {

        	_mainLevelN = XtNumber(pres_step);
                _levelMin  = pres_step[0];
                _levelMax  = pres_step[_mainLevelN-1];
                _levelStep = 25;
                _levelDir  = XmMAX_ON_LEFT;

                for ( i = 0; i < _mainLevelN; i++)
                	_mainLevel[i] = pres_step[i];

	}
	else if ( strcmp(vcoord, "HGHT") == 0 ) {

                _mainLevelN = XtNumber(hght_step);
                _levelMin  = hght_step[0];
                _levelMax  = hght_step[_mainLevelN - 1];
                _levelStep = 200;
                _levelDir  = XmMAX_ON_RIGHT;

                for ( i = 0; i < _mainLevelN; i++)
                	_mainLevel[i] = hght_step[i];
	}
	else if ( strcmp(vcoord, "THTA") == 0 ) {

                _mainLevelN = XtNumber(thta_step);
                _levelMin  = thta_step[0];
                _levelMax  = thta_step[_mainLevelN-1];
                _levelStep = 2;
                _levelDir  = XmMAX_ON_RIGHT;

                for ( i = 0; i < _mainLevelN; i++)
                	_mainLevel[i] = thta_step[i];
	}
	else { 	/* NONE */

		flag = 0;
        }

	/*
	 * set the widgets in vcoord area
	 */
        if ( flag ) {

            XtVaSetValues(_levelScaleW,
                        XmNminimum,              _levelMin,
                        XmNmaximum,              _levelMax,
                        XmNscaleMultiple,        _levelStep,
                        XmNvalue,                _stnmLevel,
                        XmNprocessingDirection,  _levelDir,
                        NULL);


	    /*
	     * set the vertical coordinate name label
	     */
	    n = XtNumber(_vcoordStr);

	    for ( i = 0; i < n; i++ ) { 
	        if ( strcmp(vcoord, _vcoordStr[i].gemparm) == 0 ) 
		    break;
	    }

	    NxmLabel_setStr(_vcoordLabelW, _vcoordStr[i].name);

	    /*
	     * set dialog title
	     */
            sprintf(title, "Vertical Level (%s)",
                                _vcoordStr[i].unit);
            NxmLabel_setStr(_vcLvlUnitW, title);

	    stnmw_levelSensitive(True);

        }
        else {

            XtVaSetValues(_levelScaleW,
                        XmNshowValue,              False,
                        NULL);

	    NxmLabel_setStr(_vcoordLabelW, vcoord);
            NxmLabel_setStr(_vcLvlUnitW, "Vertical Level");

	    stnmw_levelSensitive(False);
	}
}

/*=====================================================================*/

void stnmw_levelSensitive ( Boolean value )
/************************************************************************
 * stnmw_levelSensitive                                                 *
 *                                                                      *
 * This function sets the level area sensitiveness.                     *
 *                                                                      *
 * void stnmw_levelSensitive (value)                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 *  value   Boolean     true or false                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	 9/96                                           *
 * T. Lee/SAIC		01/03  add surface level			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    if ( XtIsSensitive(_levelPane) == value ) {
	return;
    }
    else {
	XtSetSensitive(_levelPane, (int)value);
	XtSetSensitive(_levelSel, (int)value);
	XtSetSensitive(_levelScaleW, (int)value);
	XtSetSensitive(_levelArrowBW[0], (int)value);
	XtSetSensitive(_levelArrowBW[1], (int)value);
    }

}
/*=====================================================================*/

void stnmw_lvlSelSensitive ( Boolean value )
/************************************************************************
 * stnmw_lvlSelSensitive                                                *
 *                                                                      *
 * This function sets the level area sensitiveness.                     *
 *                                                                      *
 * void stnmw_lvlSelSensitive (value)                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *  value   Boolean     true or false                                   *
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    XtSetSensitive(_levelTextW, (int)value);
    XtSetSensitive(_levelScaleW, (int)value);
    XtSetSensitive(_levelArrowBW[0], (int)value);
    XtSetSensitive(_levelArrowBW[1], (int)value);

}

/*=====================================================================*/

void stnmw_setValue ( int which )
/************************************************************************
 * stnmw_setValue							*
 *									*
 * This function is sets the specified value in the nsf_/nsn_ tables.	*
 *									*
 * void stnmw_setValue (which)						*
 *									*
 * Input parameters:							*
 *	which		int		which button			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *                      NONE                                            *
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		11/99	initial coding				*
 * S. Jacobs/NCEP	 5/00	Use sub cat number to check data type	*
 * T. Lee/SAIC		 9/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS          *
 * F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to ctb_dtget CSC*
 ***********************************************************************/
{
    int		return_idx, ibscat, iret, isub;
    char	txtstr[40], parm[123], colors[123], vcoord[73];
    char	level[73], filter[10], alias[20], cycle[123];
    char	d1[256], d2[256];
    int		d3, d4, d5, d6, d7, d7m, d8, d8m, mstrct, dionoff, dtmch;
/*---------------------------------------------------------------------*/

    ctb_dtget ( _typeStr, d1, d2, &d3, &isub, &d4, &d5, &d6, 
		&dionoff, &d7, &d7m, &d8, &d8m, &mstrct, &dtmch, &iret );

    if ( isub == SCAT_SND || isub == SCAT_SNF ) {
	nsn_qatt (_attrIndex, alias, &ibscat, cycle, parm,
		  colors, level, vcoord, filter, txtstr, &iret);

	switch (which) {
	  case SET_FILTER:
	    sprintf (filter, "%f", _stnmEdit.filter);
	    break;

	  case SET_LEVEL:
	    sprintf (level, "%d", _stnmLevel);
	    break;
	}

	nsn_satt (_attrIndex, alias, ibscat, cycle, parm, 
		  colors, level, vcoord, filter, txtstr, 
		  &return_idx, &iret);
    }
    else {
	nsf_qatt (_attrIndex, alias, &ibscat, cycle, parm, 
		  colors, filter, txtstr, &iret);

	switch (which) {
	  case SET_FILTER:
	    sprintf (filter, "%f", _stnmEdit.filter);
	    break;

	  case SET_LEVEL:
	    sprintf (level, "%d", _stnmLevel);
	    break;
	}

	nsf_satt (_attrIndex, alias, ibscat, cycle, parm, 
		  colors, filter, txtstr, &return_idx, &iret);
    }
}

/*=====================================================================*/
