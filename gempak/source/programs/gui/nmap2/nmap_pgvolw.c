#include "geminc.h"
#include "gemprm.h"
#include "proto_vf.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "pgcmn.h"

#define NL             "\n"     /* end-of-line for text product */
#define VISIBLE_ITEM   10 
static float special_prod = -9999.0F;

#define PREF_TBL "prefs.tbl"
#define VAA_TBL  "vaa.tbl"
#define LPF_NM   "VAA_lpf_template.lpf"
#define LPF_DIR  "$GEMTBL/pgen"

#define	NINFO	 7	/* Number of different elements listed here	*/
#define	VOLC	 0	/* Vocano names					*/
#define	VAAC	 1	/* Orign Stn/VAAC names				*/
#define INSR     2      /* Information Source entries			*/
#define	AVCC	 3	/* Aviation Color Code  entries			*/
#define	FACD	 4	/* Fcst Ash Cloud entries			*/
#define	NXAD	 5	/* Next Advisory  entriess			*/
#define	FCST	 6	/* Forecaster names				*/

#define	WMID	 101	/* WMO ID entries				*/
#define	HDRN	 102	/* Header Number entries			*/


#define NM_SIZE 64      /* Max chars for name				*/
#define	MXELE	20	/* Max elements per info type			*/

#define TEXT_FIELD         -1

typedef struct volcinfo_t {
    char	**range;	/* Element range		*/
    int		nelems;		/* Number of elements in range	*/
    int		index;		/* index of elements            */
} VolcanoInfo_t;

typedef struct {
    int		prev_year;	/* Year info in the most recent VAA product */
    int		prev_advnm;	/* Advisory No info in the most recent VAA
				   product */
    int		year;		/* Current year info */
    int		advnm;		/* Current advisory number info */
    int		status;		/* Advisory No status:
				   0  --  advisory no on hold, it doesn't change
					  with the correction flag.
				   1  --  advisory no on correction mode, it will
					  increment when correction flag removed.
				   2  --  advisory no on non-correction mode. it
					  will decrement when correction flag set.
				 */
} AdvNoInfo_t;

static VolcanoInfo_t	_vlInfo[NINFO], *_wmoRange, *_hdrRange;

static AdvNoInfo_t	_advNoInfo;

static VG_DBStruct	_spProdEl;

static Widget		_pgvolwWin, _pgvolwEditWin;
static Widget		_pgvolwInsrWin, _pgvolwProdWin, _pgvolwObsWin;
static Widget		_volCreateForm, _volFixedInfoForm, _volSpProdForm;
static Widget		_volcTxtW, _numberTxtW, _locTxtW, _areaTxtW;
static Widget		_elevTxtW, _hdrTxtW, _vaacTxtW, _addlTxtW;
static Widget		_yearTxtW, _prodTxtW, _fnameTxtW, _remkTxtW;
static Widget		_adnmTxtW, _correctTxtW, _wmoTxtW, _aviaForm;
static Widget		_infoTxtW, _aviaTxtW, _erupTxtW, _obdateTxtW;
static Widget		_obtimeTxtW, _obcloudTxtW;
static Widget		_facdTxt06W, _facdTxt12W, _facdTxt18W;
static Widget		_nxadTxtW, _fcstTxtW, _insrLstW, _nilBtnW;
static Widget		_locLbl, _areaLbl, _elevLbl;
static Widget		*_wmoBtnW, *_hdrBtnW, _corrBtnW, _corrRc;
static WidgetList	_ctlBtnW;

static	struct	optMenuStrc
			_loc1Strc, _loc2Strc;

static char		_lpfPath[LLPATH], _txtPath[LLPATH];

static char             ***_loc1Info, ***_loc2Info;
static char		**_loc1Str,   **_loc2Str;		
static int		_loc1Num,     _loc2Num;

static char		**_vaaTxtInfo;
static int		_vaaTxtInfoNum;	

static char		*_vaaMesgInfo;	

static char		***_otherFcstInfo;
static int		_otherFcstNum;	

static char		_volName[NM_SIZE];
static char		_volNum[17];
static char		_volLoc[17];
static char		_volArea[33];
static float 		_volElev = -1.0;

static float		_volLat  = -10000.0;
static float		_volLon  = -10000.0;

static Boolean		_nameOK  =  FALSE;
static Boolean		_numOK   =  FALSE;
static Boolean		_locOK   =  FALSE;
static Boolean		_areaOK  =  FALSE;
static Boolean		_elevOK  =  FALSE;

/*
 *  private callback functions
 */
void pgvolw_corrTxtCb     ( Widget, long, XtPointer );
void pgvolw_createCtlBtnCb( Widget, long, XtPointer );
void pgvolw_editCtlBtnCb  ( Widget, long, XtPointer );
void pgvolw_insrCtlBtnCb  ( Widget, long, XtPointer );
void pgvolw_prodCtlBtnCb  ( Widget, long, XtPointer );
void pgvolw_obsCtlBtnCb   ( Widget, long, XtPointer );
void pgvolw_infoBtnCb     ( Widget, long, XtPointer );
void pgvolw_obsBtnCb      ( Widget, long, XtPointer );
void pgvolw_menuTextCb    ( Widget, long, XtPointer );
void pgvolw_volcMenuCb    ( Widget, long, XtPointer );
void pgvolw_numTxtCb      ( Widget, long, XtPointer );
void pgvolw_locTxtCb      ( Widget, long, XtPointer );
void pgvolw_areaTxtCb     ( Widget, long, XtPointer );
void pgvolw_elevTxtCb     ( Widget, long, XtPointer );
void pgvolw_corrBtnCb     ( Widget, long, XmToggleButtonCallbackStruct* );
void pgvolw_goBtnCb       ( Widget, long, XmToggleButtonCallbackStruct* );
void pgvolw_nilBtnCb      ( Widget, long, XmToggleButtonCallbackStruct* );

/*
 *  private functions
 */
Widget  pgvolw_createMenuText ( Widget parent, char *labelstr, int ncol, 
			    int textoff, int info_type, Widget *textwid,
                            Widget *btnwid );
void    pgvolw_rdInfo ( int *iret );
void    pgvolw_checkReady ( void  );
Boolean pgvolw_isLocValid ( void  ); 
void    pgvolw_setWmoHdr  ( int index );
void    pgvolw_clearEditForm (   void ); 
void    pgvolw_attrSave   ( Boolean do_prod );
void    pgvolw_insertObsTime (   void );
void    pgvolw_getAshInfo ( int fcst_hr, char* ash_info );
void    pgvolw_createProd ( VG_DBStruct *vol, char** filter, char *text, 
			    char *fname, int *iret );
void    pgvolw_createObs  ( VG_DBStruct *ashcld, char *text, 
				int *iret );
Widget  pgvolw_insrCreate ( Widget parent );
void    pgvolw_insrPopup  ( void  );
void    pgvolw_insrPopdown( void  );
Boolean pgvolw_insrIsUp   ( void  );
Widget  pgvolw_prodCreate ( Widget parent );
void    pgvolw_prodPopup  ( void  );
void    pgvolw_prodPopdown( void  );
Boolean pgvolw_prodIsUp   ( void  );
Widget  pgvolw_obsCreate  ( Widget parent );
void    pgvolw_obsPopup   ( void  );
void    pgvolw_obsPopdown ( void  );
Boolean pgvolw_obsIsUp    ( void  );
void    pgvolw_setVolLys  ( int    rec_loc);
void    pgvolw_iniTxtInfo ( void );
void    pgvolw_getTxtInfo ( char** loc, char* buf );
void    pgvolw_getMesgInfo( char*  buf );
void	pgvolw_rdVAATbl( int * );

/************************************************************************
 * nmap_pgvolw.c							*
 *									*
 * This module defines a volcano create & edit windows for product	*
 * generation.								*
 *									*
 * CONTENTS:								*
 *	pgvolw_create()		create the volcano create window	*
 *      pgvolw_editCreate()     creates the volcano edit window         *
 *	pgvolw_insrCreate()	creates the info. source window		*
 *	pgvolw_prodCreate()	create the VAA Save window		*
 *	pgvolw_obsCreate()	create the VAA Ash Cloud Info window	*
 *									*
 *	pgvolw_popup()		pop up the volcano create window	*
 *      pgvolw_editPopup()      pop up the volcano edit   window        *
 *	pgvolw_insrPopup()	pop up the info. source window		*
 *	pgvolw_prodPopup()	pop up the VAA Save window		*
 *	pgvolw_obsPopup()	pop up the VAA Ash Cloud Info window	*
 *	pgvolw_popdown()	pop down the volcano create window	*
 *      pgvolw_editPopdown()    pop down the volcano edit window        *
 *	pgvolw_insrPopdown()	pop down the info. source window	*
 *	pgvolw_prodPopdown()	pop down the VAA Save window		*
 *	pgvolw_obsPopdown()	pop down the VAA Ash Cloud Info window	*
 *									*
 *	pgvolw_isUp()		query if the create window is up 	*
 *	pgvolw_editIsUp()	query if the edit window is up		*
 *	pgvolw_insrIsUp()	query if the info source window is up	*
 *	pgvolw_prodIsUp()	query if the VAA Save window is up	*
 *	pgvolw_obsIsUp()	query if the VAA Ash Cloud window is up	*
 *									*
 *      pgvolw_checkReady()	check if all volcano info. are ready	*
 *	pgvolw_isLocValid()	validate the location str		*
 *      pgvolw_setWmoHdr()	set WMO&HDR pulldown menu		* 
 *	pgvolw_clearEditForm()  clear volcano edit window		*
 *	pgvolw_attrSave()	save attr. on GUI into element		*
 *	pgvolw_insertObsTime()	update obs time info for fcst ash cloud *
 *	pgvolw_createProd()	create VAA text product and file name	*
 *      pgvolw_createObs()	create observation from ash cloud ele.  *
 *	pgvolw_getGrptyp()	get the group type info.		*
 *	pgvolw_getAttr()	get the attribute info. from GUI	*
 *	pgvolw_getAshInfo()	get the ash cloud info			*
 *	pgvolw_rdInfo()		reads the volcano info table		*
 *	pgvolw_createMenuText()	creates a text widget with a menu	*
 *	pgvolw_setVolLys()	sets volcano in layers			*
 *	pgvolw_iniTxtInfo()     initializes _vaaTxtInfo var.		*
 *	pgvolw_getTxtInfo()	gets vaa text info from buffer		*
 *	pgvolw_getMesgInfo()	gets vaa message info from buffer	*
 *	pgvolw_getFcstInfo()	gets the pointer to other-Fcst info	*
 *	pgvolw_getFcstNum ()	gets the total # of ohter-Fcst info	*
 *	pgvolw_getProdInfo()	gets the latest year&adv. no. info	*
 *	pgvolw_rdWords()	reads VAA wording text			*
 *									*
 *	pgvolw_menuTextCb()	callback for the menu text widgets	*
 *	pgvolw_volMenuCb()	callback for volcano menu items		*
 *	pgvolw_numTxtCb()	callback for number textfield		*
 *	pgvolw_locTxtCb()	callback for location textfield		*
 *	pgvolw_areaTxtCb()	callback for area textfield		*
 *	pgvolw_elevTxtCb()	callback for elevation textfield	*
 *	pgvolw_corrTxtCb()	callback for correction textfield	*
 *	pgvolw_createCtlBtnCb() callback for ctl btn of create window   *
 *	pgvolw_editCtlBtnCb()	callback for ctl btn of edit window	*
 *	pgvolw_insrCtlBtnCb()	callback for info source ctl btns	*
 *	pgvolw_prodCtlBtnCb()	callback for text prod ctl btns		* 
 *	pgvolw_obsCtlBtnCb()	callback for ahs cloud ctl btns		*
 *	pgvolw_infoBtnCb()	callback for info source btn		* 
 *      pgvolw_obsBtnCb()	callback for ash cloud info btn		*
 *	pgvolw_nilBtnCb()	callback for nil btn			*
 *	pgvolw_corrBtnCb()	callback for correction btn		*
 *	pgvolw_goBtnCb()	callback for go btn			*
 ***********************************************************************/

/*=====================================================================*/

Widget pgvolw_create ( Widget parent )
/************************************************************************
 * pgvolw_create							*
 *									*
 * This function creates the VAA Volcano Create popup window.		*
 *									*
 * Widget pgvolw_create(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgvolw_create	Widget	Widget ID of Volcano Create popup 	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 * H. Zeng/XTRIA	08/03   added pgvolw_insrCreate()		*
 * H. Zeng/XTRIA	11/03   modified "Location" label		*
 * H. Zeng/XTRIA	01/04   added pgvolw_obsCreate()		*
 * H. Zeng/XTRIA	02/04	added a new btn				*
 * H. Zeng/XTRIA	02/04	added special product form		*
 ***********************************************************************/
{
    Widget	pane, volc_form, number_rc, area_rc, elev_rc;
    Widget	loc_rc, go_btn;
    int		nn, iret, toff = 5, loff = 2;
    char	*btnstrs[] = {"Create VAA Volcano",
			      "Create Volcano in Layers", "Cancel"};
/*---------------------------------------------------------------------*/
/*
 *  Read in watch format base information.
 */
    pgvolw_rdInfo( &iret );
    if ( iret != 0 ) {
         fprintf(stderr, 
                 "Error in reading volcanic ash advisory table.\n");
         exit(1);
    }

/*
 * create dialog shell
 */
    _pgvolwWin = XmCreateFormDialog(parent, "pgvolw_popup",
				    NULL, 0);
    XtVaSetValues(_pgvolwWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pgvolwWin),
		  XmNtitle, "VAA Volcano Create",
		  NULL);
    
/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("pgvolw_pane",
			    xmPanedWindowWidgetClass, _pgvolwWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    
/*
 * create FORMATTING area
 */
    _volCreateForm = XtVaCreateWidget("form",
				   xmFormWidgetClass,      pane,
				   NULL);

/*
 * volcano menu
 */
    volc_form = pgvolw_createMenuText (_volCreateForm, 
				      "Volcano: ", MXCHR, 
				      loff, VOLC, &_volcTxtW, NULL );

    XtVaSetValues(_volcTxtW, XmNmaxLength, 62, NULL);

    XtVaSetValues(volc_form, 
		  XmNtopAttachment,	XmATTACH_FORM,
		  XmNtopOffset,		toff,
                  XmNleftAttachment,    XmATTACH_FORM,
                  XmNleftOffset,        2,
		  NULL);

/*
 * Number
 */
    number_rc = (Widget)NxmTxtIn_create(_volCreateForm,
                                 "Number: ", 10, &_numberTxtW);

    XtVaSetValues(_numberTxtW, XmNmaxLength, 15, NULL);

    XtVaSetValues (number_rc,
                   XmNtopAttachment,    XmATTACH_FORM,
                   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	volc_form,
                   XmNleftOffset,       8,
                   NULL);

    XtAddCallback( _numberTxtW, XmNvalueChangedCallback,
                   (XtCallbackProc) pgvolw_numTxtCb, NULL ); 

/*
 * Location
 */
    loc_rc = (Widget)NxmTxtIn_create(_volCreateForm,
                        "Location (e.g. N3900W07700): ", 24, &_locTxtW);

    XtVaSetValues(_locTxtW, XmNmaxLength, 15, NULL);

    XtVaSetValues (loc_rc,
                   XmNtopAttachment,    XmATTACH_WIDGET,
		   XmNtopWidget,	volc_form,
                   XmNtopOffset,        toff,
                   XmNleftAttachment,   XmATTACH_FORM,
                   XmNleftOffset,       2,
                   NULL);
  
    XtAddCallback( _locTxtW, XmNvalueChangedCallback,
                   (XtCallbackProc) pgvolw_locTxtCb, NULL ); 

/*
 * Area
 */
    area_rc = (Widget)NxmTxtIn_create(_volCreateForm,
                                 "Area: ", 13, &_areaTxtW);

    XtVaSetValues(_areaTxtW, XmNmaxLength, 31, NULL);

    XtVaSetValues (area_rc,
                   XmNtopAttachment,    XmATTACH_WIDGET,
		   XmNtopWidget,	loc_rc,
                   XmNtopOffset,        toff,
                   XmNleftAttachment,   XmATTACH_FORM,
                   XmNleftOffset,       2,
                   NULL);

    XtAddCallback( _areaTxtW, XmNvalueChangedCallback,
                   (XtCallbackProc) pgvolw_areaTxtCb, NULL );

/*
 * Elevation
 */
    elev_rc = (Widget)NxmTxtIn_create(_volCreateForm,
                                 "Elevation: ", 17, &_elevTxtW);

    XtVaSetValues(_elevTxtW, XmNmaxLength, 7, NULL);

    XtVaSetValues (elev_rc,
                   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopWidget,	area_rc,
                   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	area_rc,
		   XmNleftOffset,	8,
                   NULL);

    XtAddCallback( _elevTxtW, XmNvalueChangedCallback,
                   (XtCallbackProc) pgvolw_elevTxtCb, (XtPointer)0 );

    XtAddCallback( _elevTxtW, XmNlosingFocusCallback,
                   (XtCallbackProc) pgvolw_elevTxtCb, (XtPointer)1 );

/*
 * "ft" label
 */
    XtVaCreateManagedWidget ("ft",
		   xmLabelWidgetClass,	_volCreateForm,
                   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopWidget,	elev_rc,
		   XmNtopOffset,	10,
                   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	elev_rc,
		   NULL );


    XtManageChild (_volCreateForm);

/*
 * create control buttons
 */
    nn = XtNumber(btnstrs);
    _ctlBtnW = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    NxmCtlBtn_create(pane, 0, "pgvolw_ctlBtn", nn, btnstrs, 
		     (XtCallbackProc)pgvolw_createCtlBtnCb, _ctlBtnW);  
   
/*
 * create special products area.
 */
    _volSpProdForm = XtVaCreateWidget("form",
				   xmFormWidgetClass,      pane,
				   NULL);

    _loc2Strc.current = 0;
    pgutls_createOptionMenu (_volSpProdForm, _loc2Num, 
			     (XtPointer)&_loc2Strc.current, 
			     "Special Products:", NULL, &_loc2Strc.form, 
			     &_loc2Strc.label, &_loc2Strc.menu, 
			     _loc2Strc.pb, _loc2Str);

    XtVaSetValues (_loc2Strc.label, 
		   XmNtopAttachment,    XmATTACH_FORM,
		   XmNtopOffset,        10,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    XtVaSetValues (_loc2Strc.form, 
		   XmNtopAttachment,    XmATTACH_FORM,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    go_btn = XtVaCreateManagedWidget ( "Go...",
                      xmPushButtonWidgetClass,  _volSpProdForm,
                      XmNtopAttachment,         XmATTACH_FORM,
		      XmNtopOffset,		5,
                      XmNleftAttachment,        XmATTACH_WIDGET,
		      XmNleftWidget,		_loc2Strc.form,
		      NULL );

    XtAddCallback (go_btn, XmNactivateCallback,
                   (XtCallbackProc)pgvolw_goBtnCb, NULL );

    XtManageChild (_volSpProdForm);

    XtManageChild(pane);

/*
 * Create "Information Source Selection" and "VAA Save" window.
 */
    pgvolw_insrCreate ( _pgvolwWin );
    pgvolw_prodCreate ( _pgvolwWin );
    pgvolw_obsCreate  ( _pgvolwWin );

    return(_pgvolwWin);
}

/*=====================================================================*/

void pgvolw_popup ( void )
/************************************************************************
 * pgvolw_popup								*
 *									*
 * This function pops up VAA Volcano Create window.		        *
 *									*
 * void pgvolw_popup()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 * H. Zeng/XTRIA	08/03	modified to call pgvolw_volcMenuCb()	*
 * H. Zeng/XTRIA	02/04   added a new btn				*
 ***********************************************************************/
{
    pgvolw_volcMenuCb ( NULL, 0, NULL );

    _nameOK = FALSE;
    _numOK  = FALSE;
    _locOK  = FALSE;
    _areaOK = FALSE;
    _elevOK = FALSE;

    XtSetSensitive (_ctlBtnW[0], FALSE);
    XtSetSensitive (_ctlBtnW[1], FALSE);    

    XtManageChild (_pgvolwWin);
}

/*=====================================================================*/

void pgvolw_popdown ( void ) 
/************************************************************************
 * pgvolw_popdown							*
 *									*
 * This function pops down VAA Volcano Create window.			*
 *									*
 * void pgvolw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03   initial coding				*
 ***********************************************************************/
{
    if (XtIsManaged (_pgvolwWin)) {
	XtUnmanageChild (_pgvolwWin);
    }
}

/*=====================================================================*/

Boolean pgvolw_isUp ( void ) 
/************************************************************************
 * pgvolw_isUp								*
 *									*
 * This function queries whether the VAA Volcano Create window is up.	*
 *									*
 * Boolean pgvolw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgvolw_isUp		Boolean	     True -- up,  False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 ***********************************************************************/
{
    return (XtIsManaged (_pgvolwWin));
}

/*=====================================================================*/

Widget pgvolw_editCreate ( Widget parent )
/************************************************************************
 * pgvolw_editCreate							*
 *									*
 * This function creates the VAA Volcano Edit popup window.		*
 *									*
 * Widget pgvolw_editCreate(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgvolw_editCreate	Widget	Widget ID of Volcano Edit popup 	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 * H. Zeng/XTRIA	09/03   made minor modifications		*
 * H. Zeng/XTRIA	10/03   changed attr. to _obcloudTxtW text wid. *
 * H. Zeng/XTRIA	01/04   added Obs Ash Cloud Btn			*
 * H. Zeng/XTRIA	02/04   added text formating option menu	*
 * H. Zeng/XTRIA	03/04   minor GUI changes			*
 * H. Zeng/SAIC		04/06	added callback for corr. textfield	*
 ***********************************************************************/
{
    Widget	pane, orign_form, hdr_form, vaac_form, year_form, year_rc;
    Widget	adnm_rc, wmo_form, info_form;
    Widget	info_btn, erup_lbl, obdate_rc, obs_btn;
    Widget	obtime_rc;
    Widget	fcst_form, remk_lbl, nxad_form;
    int		nn, toff = 5, loff = 2;
    char	*btnstrs[]={"Apply", "Cancel", "Format VAA", "Reset Form"};
/*---------------------------------------------------------------------*/
/*
 * create dialog shell
 */
    _pgvolwEditWin = XmCreateFormDialog(parent, "pgvolw_editPopup",
				        NULL, 0);
    XtVaSetValues(_pgvolwEditWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pgvolwEditWin),
		  XmNtitle, "VAA Volcano Edit",
		  NULL);
    
/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("pgvolw_pane",
			    xmPanedWindowWidgetClass, _pgvolwEditWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    
/*
 * create fixed info area
 */
    _volFixedInfoForm = XtVaCreateWidget("form",
				   xmFormWidgetClass,      pane,
				   NULL);

/*
 * volcano name label
 */
    _locLbl =  XtVaCreateManagedWidget ("Location: A_LOCATION",
			 xmLabelWidgetClass,	_volFixedInfoForm,
			 XmNtopAttachment,	XmATTACH_FORM,
			 XmNleftAttachment,	XmATTACH_FORM,
			 NULL);

    _areaLbl = XtVaCreateManagedWidget ("Area: A_AREA",
			 xmLabelWidgetClass,	_volFixedInfoForm,
			 XmNtopAttachment,	XmATTACH_OPPOSITE_WIDGET,
			 XmNtopWidget,		_locLbl,
			 XmNleftAttachment,	XmATTACH_WIDGET,
			 XmNleftWidget,		_locLbl,
			 XmNleftOffset,		10,
			 NULL);

    _elevLbl = XtVaCreateManagedWidget ("Elevation: A_ELEVATION",
			 xmLabelWidgetClass,	_volFixedInfoForm,
			 XmNtopAttachment,	XmATTACH_WIDGET,
			 XmNtopWidget,		_locLbl,
			 XmNtopOffset,		5,
			 XmNleftAttachment,	XmATTACH_FORM,
			 NULL);

    XtManageChild (_volFixedInfoForm);

/*
 * create Orig Stn/VAAC area
 */
    orign_form = XtVaCreateWidget("form",
				  xmFormWidgetClass,      pane,
				  NULL);

/*
 * vaac menu
 */
    vaac_form = pgvolw_createMenuText (orign_form, 
				      "Orig Stn/VAAC: ", MXCHR+10, 
				      loff, VAAC, &_vaacTxtW, NULL );

    XtVaSetValues(_vaacTxtW, 
                  XmNeditable,              False,
                  XmNcursorPositionVisible, False,
		  NULL);

    XtVaSetValues(vaac_form, 
		  XmNtopAttachment,	XmATTACH_FORM,
		  XmNtopOffset,		toff,
                  XmNleftAttachment,    XmATTACH_FORM,
		  NULL);

/*
 * wmo id menu
 */
    _wmoBtnW = (Widget*)XtMalloc( MXELE * sizeof(Widget) );
    wmo_form = pgvolw_createMenuText (orign_form, 
				      "WMO ID: ", 4, 
				      loff, WMID, &_wmoTxtW, _wmoBtnW );

    XtVaSetValues(_wmoTxtW, 
                  XmNeditable,              False,
                  XmNcursorPositionVisible, False,
		  NULL);

    XtVaSetValues(wmo_form, 
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		vaac_form,
		  XmNtopOffset,		toff,
                  XmNleftAttachment,    XmATTACH_FORM,
		  NULL);

/*
 * header number menu
 */
    _hdrBtnW = (Widget*)XtMalloc( MXELE * sizeof(Widget) );
    hdr_form = pgvolw_createMenuText (orign_form, 
				      "Hdr Number: ", 4, 
				      loff, HDRN, &_hdrTxtW, _hdrBtnW );

    XtVaSetValues(_hdrTxtW, 
                  XmNeditable,              False,
                  XmNcursorPositionVisible, False,
		  NULL);

    XtVaSetValues(hdr_form, 
		  XmNtopAttachment,	XmATTACH_OPPOSITE_WIDGET,
		  XmNtopWidget,		wmo_form,
                  XmNleftAttachment,    XmATTACH_WIDGET,
		  XmNleftWidget,        wmo_form,
		  XmNleftOffset,	15,
		  NULL);

    _loc1Strc.current = 0;
    pgutls_createOptionMenu (orign_form, _loc1Num, 
			     (XtPointer)&_loc1Strc.current, 
			     NULL, NULL, &_loc1Strc.form, 
			     &_loc1Strc.label, &_loc1Strc.menu, 
			     _loc1Strc.pb, _loc1Str);

    XtVaSetValues (_loc1Strc.form, 
		   XmNtopAttachment,	XmATTACH_WIDGET,
		   XmNtopWidget,	vaac_form,
                   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,       hdr_form,
		   NULL);

    XtManageChild (orign_form);

/*
 * create YEAR area
 */
    year_form = XtVaCreateWidget("form",
				  xmFormWidgetClass,      pane,
				  NULL);

/*
 * Year textfield
 */
    year_rc = (Widget)NxmTxtIn_create(year_form,
                                 "Year: ", 5, &_yearTxtW);

    XtVaSetValues(_yearTxtW, XmNmaxLength, 7, NULL);

    XtVaSetValues (year_rc,
                   XmNtopAttachment,    XmATTACH_FORM,
		   XmNtopOffset,	toff,
                   XmNleftAttachment,   XmATTACH_FORM,
                   NULL);

/*
 * Advisory Number
 */
    adnm_rc = (Widget)NxmTxtIn_create(year_form,
                                 "Advisory No.: ", 5, &_adnmTxtW);
 
    XtVaSetValues(_adnmTxtW, XmNmaxLength, 7, NULL);

    XtVaSetValues (adnm_rc,
                   XmNtopAttachment,    XmATTACH_FORM,
                   XmNtopOffset,        toff,
                   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	year_rc,
                   XmNleftOffset,       2,
                   NULL);

/*
 * Correction check box
 */
    _corrBtnW = XtVaCreateManagedWidget(" ",
			    xmToggleButtonWidgetClass,  year_form,
			    XmNindicatorType,	        XmN_OF_MANY,
			    XmNtraversalOn,		FALSE,
			    XmNset,		        FALSE,
			    XmNspacing,			0,
			    NULL);

    XtVaSetValues (_corrBtnW,
                   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopWidget,	adnm_rc,
		   XmNtopOffset,        5,
                   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	adnm_rc,
		   XmNleftOffset,	15,
                   NULL);

    XtAddCallback(_corrBtnW, XmNvalueChangedCallback,
		  (XtCallbackProc)pgvolw_corrBtnCb, (XtPointer)NULL);

/*
 * Correction
 */
    _corrRc = (Widget)NxmTxtIn_create(year_form,
                                 "Correction: ", 3, &_correctTxtW);

    XtVaSetValues(_correctTxtW, XmNmaxLength, 1, NULL);

    XtAddCallback( _correctTxtW, XmNvalueChangedCallback,
                   (XtCallbackProc) pgvolw_corrTxtCb, NULL ); 

    XtVaSetValues (_corrRc,
                   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopWidget,	adnm_rc,
                   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	_corrBtnW,
		   XmNleftOffset,       -5,
                   NULL);

    XtSetSensitive (_corrRc, FALSE);

    XtManageChild (year_form);

/*
 * create INFO SOURCE area
 */
    info_form = XtVaCreateWidget("form",
				  xmFormWidgetClass,      pane,
				  NULL);

/*
 * Info source button
 */
    info_btn = XtVaCreateManagedWidget ( "Information Source:",
                      xmPushButtonWidgetClass,  info_form,
                      XmNtopAttachment,         XmATTACH_FORM,
		      XmNtopOffset,	        toff,
                      XmNleftAttachment,        XmATTACH_FORM,
		      NULL );

    XtAddCallback (info_btn, XmNactivateCallback,
                    (XtCallbackProc)pgvolw_infoBtnCb, NULL );

    _infoTxtW = XtVaCreateManagedWidget ("info_text",
		      xmTextWidgetClass,        info_form,
		      XmNrows,			3,
		      XmNcolumns,	        30,
		      XmNeditMode,	        XmMULTI_LINE_EDIT,
		      XmNmaxLength,	        MAX_FTEXTSTR-1,
		      XmNwordWrap,	        TRUE,
		      XmNscrollVertical,	TRUE,
                      XmNeditable,              False,
		      XmNtopAttachment,	        XmATTACH_FORM,
		      XmNleftAttachment,        XmATTACH_WIDGET,
		      XmNleftWidget,		info_btn,
		      NULL ); 

    XtVaCreateManagedWidget ( "Add'l Info Source:",
                      xmLabelWidgetClass,	info_form,
                      XmNtopAttachment,         XmATTACH_WIDGET,
		      XmNtopWidget,		_infoTxtW,
		      XmNtopOffset,	        toff,
                      XmNleftAttachment,        XmATTACH_FORM,
		      XmNleftOffset,		1,
		      NULL );

    _addlTxtW = XtVaCreateManagedWidget ("addl_info_text",
		      xmTextWidgetClass,        info_form,
		      XmNrows,			2,
		      XmNcolumns,	        30,
		      XmNeditMode,	        XmMULTI_LINE_EDIT,
		      XmNmaxLength,	        MAX_FTEXTSTR-2,
		      XmNwordWrap,	        TRUE,
		      XmNscrollVertical,	TRUE,
		      XmNtopAttachment,	        XmATTACH_WIDGET,
		      XmNtopWidget,		_infoTxtW,
		      XmNleftAttachment,        XmATTACH_OPPOSITE_WIDGET,
		      XmNleftWidget,		_infoTxtW,
		      NULL ); 

/*
 * Aviation Color Code  menu
 */
    _aviaForm = pgvolw_createMenuText (info_form, "Aviation Color Code: ", 
				       MXCHR, loff, AVCC, &_aviaTxtW, NULL );

    XtVaSetValues(_aviaTxtW, 
                  XmNeditable,              False,
                  XmNcursorPositionVisible, False,
		  NULL);

    XtVaSetValues(_aviaForm, 
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		_addlTxtW,
		  XmNtopOffset,		3,
                  XmNleftAttachment,    XmATTACH_FORM,
		  NULL);

/*
 * Eruption Details:
 */
    erup_lbl = XtVaCreateManagedWidget ( "Eruption Details:",
                      xmLabelWidgetClass,	info_form,
                      XmNtopAttachment,         XmATTACH_WIDGET,
		      XmNtopWidget,		_aviaForm,
		      XmNtopOffset,	        toff+5,
                      XmNleftAttachment,        XmATTACH_FORM,
		      NULL );

    _erupTxtW = XtVaCreateManagedWidget ("erup_text",
		      xmTextWidgetClass,        info_form,
		      XmNrows,			2,
		      XmNcolumns,	        30,
		      XmNeditMode,	        XmMULTI_LINE_EDIT,
		      XmNmaxLength,	        254,
		      XmNwordWrap,	        TRUE,
		      XmNscrollVertical,	TRUE,
		      XmNtopAttachment,	        XmATTACH_WIDGET,
		      XmNtopWidget,		_aviaForm,
		      XmNtopOffset,		3,
		      XmNleftAttachment,        XmATTACH_WIDGET,
		      XmNleftWidget,		erup_lbl,
		      NULL ); 

/*
 * Obs Ash Date textfield
 */
    obdate_rc = (Widget)NxmTxtIn_create(info_form,
                                 "Obs Ash Date(DD): ", 5, &_obdateTxtW);

    XtVaSetValues(_obdateTxtW, XmNmaxLength, 14, NULL);

    XtVaSetValues (obdate_rc,
                   XmNtopAttachment,    XmATTACH_WIDGET,
		   XmNtopWidget,	_erupTxtW,
		   XmNtopOffset,	toff,
                   XmNleftAttachment,   XmATTACH_FORM,
                   NULL);

/*
 * Time
 */
    obtime_rc = (Widget)NxmTxtIn_create(info_form,
                                 "Time(HHHH): ", 5, &_obtimeTxtW);

    XtVaSetValues(_obtimeTxtW, XmNmaxLength, 14, NULL);

    XtVaSetValues (obtime_rc,
                   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopWidget,	obdate_rc,
                   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	obdate_rc,
                   XmNleftOffset,       2,
                   NULL);

/*
 * "z" label
 */
    XtVaCreateManagedWidget ("Z",
		   xmLabelWidgetClass,	info_form,
                   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopWidget,	obtime_rc,
		   XmNtopOffset,	10,
                   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	obtime_rc,
		   NULL );

/*
 * NIL check box
 */
    _nilBtnW = XtVaCreateManagedWidget(" ",
			xmToggleButtonWidgetClass,  info_form,
			XmNindicatorType,	    XmN_OF_MANY,
			XmNtraversalOn,		    FALSE,
			XmNset,		            FALSE,
			XmNspacing,		    0,
			NULL);

    XtAddCallback(_nilBtnW, XmNvalueChangedCallback,
		  (XtCallbackProc)pgvolw_nilBtnCb, (XtPointer)NULL);

    XtVaSetValues (_nilBtnW,
                   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopWidget,	obtime_rc,
		   XmNtopOffset,        10,
                   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	obtime_rc,
		   XmNleftOffset,	30,
                   NULL);

/*
 * NIL label
 */
    XtVaCreateManagedWidget ("NIL",
		   xmLabelWidgetClass,	info_form,
                   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopWidget,	obtime_rc,
		   XmNtopOffset,        12,
                   XmNleftAttachment,   XmATTACH_WIDGET,
		   XmNleftWidget,	_nilBtnW,
		   XmNleftOffset,	0,
		   NULL );

/*
 * Obs Ash Cloud button
 */
    obs_btn = XtVaCreateManagedWidget ( 
		      "Observed and Forecast Ash Cloud Information...",
                      xmPushButtonWidgetClass,  info_form,
                      XmNtopAttachment,         XmATTACH_WIDGET,
		      XmNtopWidget,		obdate_rc,
		      XmNtopOffset,	        toff,
                      XmNleftAttachment,        XmATTACH_FORM,
		      NULL );

    XtAddCallback (obs_btn, XmNactivateCallback,
                   (XtCallbackProc)pgvolw_obsBtnCb, NULL );

/*
 * Remarks:
 */
    remk_lbl = XtVaCreateManagedWidget ( "Remarks: ",
                      xmLabelWidgetClass,	info_form,
                      XmNtopAttachment,         XmATTACH_WIDGET,
		      XmNtopWidget,		obs_btn,
		      XmNtopOffset,	        toff+5,
                      XmNleftAttachment,        XmATTACH_FORM,
		      NULL );

    _remkTxtW = XtVaCreateManagedWidget ("remk_text",
		      xmTextWidgetClass,        info_form,
		      XmNrows,			5,
		      XmNcolumns,	        30,
		      XmNeditMode,	        XmMULTI_LINE_EDIT,
		      XmNmaxLength,	        510,
		      XmNwordWrap,	        TRUE,
		      XmNscrollVertical,	TRUE,
		      XmNtopAttachment,	        XmATTACH_WIDGET,
		      XmNtopWidget,		obs_btn,
		      XmNtopOffset,		3,
		      XmNleftAttachment,        XmATTACH_WIDGET,
		      XmNleftWidget,		remk_lbl,
		      NULL ); 

/*
 * Next Advisory menu
 */
    nxad_form = pgvolw_createMenuText (info_form, 
				      "Next Advisory: ", MXCHR+18, 
				      loff, NXAD, &_nxadTxtW, NULL );

    XtVaSetValues(_nxadTxtW, XmNmaxLength, 126, NULL);

    XtVaSetValues(nxad_form, 
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		_remkTxtW,
		  XmNtopOffset,		toff,
                  XmNleftAttachment,    XmATTACH_FORM,
		  NULL);

/*
 * Forecaster(s) menu
 */
    fcst_form = pgvolw_createMenuText (info_form, 
				      "Forecaster(s):  ", MXCHR, 
				      loff, FCST, &_fcstTxtW, NULL );

    XtVaSetValues(_fcstTxtW, XmNmaxLength, 62, NULL);

    XtVaSetValues(fcst_form, 
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget,		nxad_form,
		  XmNtopOffset,		toff,
                  XmNleftAttachment,    XmATTACH_FORM,
		  NULL);

    XtManageChild (info_form);

/*
 * create control buttons
 */
    nn = XtNumber(btnstrs);
    NxmCtlBtn_create(pane, 0, "pgvolw_editCtlBtn", nn, btnstrs, 
		     (XtCallbackProc)pgvolw_editCtlBtnCb,    NULL );

    XtManageChild(pane);

    return(_pgvolwEditWin);
}

/*=====================================================================*/

void pgvolw_editPopup ( VG_DBStruct* el )
/************************************************************************
 * pgvolw_editPopup							*
 *									*
 * This function pops up VAA Volcano Edit window.		        *
 *									*
 * void pgvolw_editPopup()						*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	current element			*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 * H. Zeng/XTRIA	09/03   removed "_" from volcano name		*
 * H. Zeng/XTRIA	10/03   get Obs Ash Cloud info. from surrounding*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * H. Zeng/XTRIA	11/03   added additional info source		*
 * H. Zeng/XTRIA	12/03   added obs_type				*
 * H. Zeng/XTRIA	01/04   used current year for "Year" text wid.  *
 * H. Zeng/XTRIA	02/04   added call to pgvolw_getAshInfo		*
 * H. Zeng/XTRIA	02/04	sets text formating choices		*
 * H. Zeng/XTRIA	03/04   minor changes for intialization		*
 * H. Zeng/SAIC		04/05	removed auto insertion of "NOT AVBL"	*
 * H. Zeng/SAIC		05/05	Rounded inserted time to nearest half-hr*
 * H. Zeng/SAIC		06/05	added call to pgvolw_insertObsTime	*
 * H. Zeng/SAIC		04/06   new way of setting advisory no on GUI	*
 ***********************************************************************/
{
    char      label_str[96], vaac_str[64], vol_name[64], cyear[8];
    char      ash_info[1024],cadvnm[8], orig_volnm[64];
    float     elev_ft, elev_m;
    struct tm	  *utctime;
    time_t    tp;
    int	      ii, ier, latest_y, latest_n, index;
/*---------------------------------------------------------------------*/
/*
 * Change volcano name to be all upper-case and remove "_"
 */
    strcpy (orig_volnm, el->elem.vol.info.name);
    strcpy (vol_name,   el->elem.vol.info.name);
    cst_lcuc (vol_name, vol_name, &ier);

    while ( strstr (vol_name, "_") != NULL ) {
	  cst_rpst ( vol_name, "_", " ", vol_name, &ier);
    } 

/*
 * Change the window title according to current volcano name&number
 */
    sprintf( label_str, "VAA - %s(%s)", vol_name, 
			el->elem.vol.info.number);
    XtVaSetValues(XtParent(_pgvolwEditWin), XmNtitle, label_str, NULL);


    sprintf( label_str, "Location:  %s", el->elem.vol.info.location );
    NxmLabel_setStr(_locLbl, label_str);

    sprintf( label_str, "Area:  %s", el->elem.vol.info.area );
    NxmLabel_setStr(_areaLbl, label_str);

    elev_ft = 0;
    sscanf(el->elem.vol.info.elev, "%f", &elev_ft);
    elev_m  = elev_ft * F2M;

    sprintf( label_str, "Elevation:  %-7.0f ft (%-7.0f m)", 
		        elev_ft, elev_m );
    NxmLabel_setStr(_elevLbl, label_str);

    if ( !(XtIsManaged (_volFixedInfoForm)) ) {
	XtManageChild ( _volFixedInfoForm );
    }

/*
 * Retrieve volcano info from current element and update
 * extended attribute info. on edit GUI.
 */
    vaac_str[0] = '\0';

    if (el->elem.vol.info.origstn[0] == '\0' &&
	el->elem.vol.info.vaac[0]    == '\0'    ) {

        XmTextSetString(_vaacTxtW, "\0");
    }
    else {

        strcpy ( vaac_str, el->elem.vol.info.origstn );
        strcat ( vaac_str, "/" );
        strcat ( vaac_str, el->elem.vol.info.vaac );
        XmTextSetString(_vaacTxtW,     vaac_str   );
    }

    index = -1;
    for ( ii = 0; ii < _vlInfo[VAAC].nelems; ii++ ) {

        if ( strcmp (_vlInfo[VAAC].range[ii], vaac_str) == 0 ) {
             index = ii;
             break;
	}
    }
    pgvolw_setWmoHdr ( index ); 

    if ( strstr (vaac_str, "PAWU") != NULL ) {

       XtSetSensitive (_aviaForm, TRUE );
    }
    else {

       XmTextSetString(_aviaTxtW, "\0" );
       XtSetSensitive (_aviaForm, FALSE);
    }

/*
 * Get the current year.
 */
    tp      = time (NULL);
    utctime = gmtime (&tp);
    sprintf(cyear, "%4d", utctime->tm_year + 1900);

/*
 * Get the latest year&adv. no. info.
 */
    pgvolw_getProdInfo (orig_volnm, &latest_y, &latest_n, &ier);

    if ( ier == 0 ) {

/*
 * Set values for _advNoInfo.
 */
      _advNoInfo.prev_year  = latest_y;
      _advNoInfo.prev_advnm = latest_n;
      _advNoInfo.year       = utctime->tm_year + 1900;
      _advNoInfo.status = 2;

      if ( latest_y == utctime->tm_year + 1900 ) {

        sprintf(cadvnm, "%03d", latest_n+1);
        _advNoInfo.advnm = latest_n+1;
      }
      else {

        sprintf(cadvnm, "001");
        _advNoInfo.advnm = 1;
      }
    }
    else {

        sprintf(cadvnm, "001");
        _advNoInfo.status = 0;
    }

    XmTextSetString(_wmoTxtW,      el->elem.vol.info.wmoid     ); 
    XmTextSetString(_hdrTxtW,      el->elem.vol.info.hdrnum    );
    XmTextSetString(_yearTxtW,			      cyear    );
    XmTextSetString(_adnmTxtW,                       cadvnm    ); 

    XmToggleButtonSetState(_corrBtnW, FALSE, TRUE);
   
    XmTextSetString(_infoTxtW,     el->elem.vol.info.infosorc  );
    XmTextSetString(_addlTxtW,     el->elem.vol.info.addlsorc  );
    XmTextSetString(_aviaTxtW,     el->elem.vol.info.avcc      );
    XmTextSetString(_erupTxtW,     el->elem.vol.info.details   );

    if ( strcmp(el->elem.vol.info.obsdate, "NIL") == 0 || 
         strcmp(el->elem.vol.info.obstime, "NIL") == 0    ) {

         XmToggleButtonSetState(_nilBtnW, TRUE, TRUE);
    }
    else {

         XmToggleButtonSetState(_nilBtnW, FALSE, TRUE);
         XmTextSetString(_obdateTxtW,   el->elem.vol.info.obsdate); 
         XmTextSetString(_obtimeTxtW,   el->elem.vol.info.obstime);
    }

/*
 * Get Ash Cloud Info and update the VAA Ash Cloud Info window.
 */ 
    pgvolw_getAshInfo ( 0, ash_info );
    XmTextSetString(_obcloudTxtW,  ash_info );

    pgvolw_getAshInfo ( 6, ash_info );
    XmTextSetString(_facdTxt06W,   ash_info );

    pgvolw_getAshInfo (12, ash_info );
    XmTextSetString(_facdTxt12W,   ash_info ); 

    pgvolw_getAshInfo (18, ash_info );
    XmTextSetString(_facdTxt18W,   ash_info ); 

/*
 * Insert obs date/time info in fcst ash cloud text boxes on 
 * VAA Ash Cloud Info window. 
 */
    pgvolw_insertObsTime ();


    XmTextSetString(_remkTxtW,     el->elem.vol.info.remarks   );
    XmTextSetString(_nxadTxtW,     el->elem.vol.info.nextadv   ); 
    XmTextSetString(_fcstTxtW,     el->elem.vol.info.fcstrs    );

/*
 * Set the availability of text formating choices.
 */
    _loc1Strc.current = 0;
    XtVaSetValues (_loc1Strc.menu, 
		   XmNmenuHistory, _loc1Strc.pb[_loc1Strc.current], 
		   NULL);

    XtSetSensitive (_loc1Strc.form,
		    !G_DIFF(el->elem.vol.info.code, special_prod) );

    if ( !(XtIsManaged (_pgvolwEditWin)) ) {
	XtManageChild (_pgvolwEditWin);
    }
}

/*=====================================================================*/

void pgvolw_editPopdown ( void ) 
/************************************************************************
 * pgvolw_editPopdown							*
 *									*
 * This function pops down VAA Volcano Edit window.			*
 *									*
 * void pgvolw_editPopdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03   initial coding				*
 * H. Zeng/XTRIA	08/03   added pgvolw_prodPopdown()		*
 * H. Zeng/XTRIA	01/04   added pgvolw_obsPopdown()		*
 ***********************************************************************/
{
    if (XtIsManaged (_pgvolwEditWin)) {
	XtUnmanageChild (_pgvolwEditWin);
    }

    pgvolw_insrPopdown();
    pgvolw_prodPopdown(); 
    pgvolw_obsPopdown ();
}

/*=====================================================================*/

Boolean pgvolw_editIsUp ( void ) 
/************************************************************************
 * pgvolw_editIsUp							*
 *									*
 * This function queries whether the VAA Volcano Edit window is up.	*
 *									*
 * Boolean pgvolw_editIsUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgvolw_isUp		Boolean	     True -- up,  False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 ***********************************************************************/
{
    return (XtIsManaged (_pgvolwEditWin));
}

/*=====================================================================*/

Widget pgvolw_insrCreate ( Widget parent )
/************************************************************************
 * pgvolw_insrCreate							*
 *									*
 * This function creates the Information Source Selection popup window.	*
 *									*
 * Widget pgvolw_insrCreate(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgvolw_insrCreate	Widget	Widget ID of Info. Source popup 	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 * H. Zeng/XTRIA	09/03   modified the creation of List Widget	*
 ***********************************************************************/
{
    Widget	pane, frame, insr_swin;
    XmString    *xm_items;
    int		nn, ii;
    char	*btnstrs[] = {"OK", "Cancel"};
/*---------------------------------------------------------------------*/
/*
 * create dialog shell
 */
    _pgvolwInsrWin = XmCreateFormDialog(parent, "pgvolw_popup",
				    NULL, 0);
    XtVaSetValues(_pgvolwInsrWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pgvolwInsrWin),
		  XmNtitle, "Information Source Selection",
		  NULL);
    
/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("pgvolw_pane",
			    xmPanedWindowWidgetClass, _pgvolwInsrWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);

    frame = XmCreateFrame( pane, "_Insr_frameW", NULL, 0 );

    insr_swin = XmCreateScrolledWindow( frame, "insr_window", NULL, 0 );

    _insrLstW = XtVaCreateManagedWidget ("_Insr_lstW",
		   xmListWidgetClass,           insr_swin,
                   XmNvisibleItemCount,         VISIBLE_ITEM,
		   XmNscrollBarDisplayPolicy,   XmSTATIC,
		   XmNselectionPolicy,		XmMULTIPLE_SELECT,
		   XmNtraversalOn,		FALSE,
		   XmNlistSizePolicy,		XmRESIZE_IF_POSSIBLE,
		   NULL );


    if ( _vlInfo[INSR].nelems > 0) {

        xm_items = (XmString*) XtMalloc 
	    ( _vlInfo[INSR].nelems * sizeof (XmString) );

        for (ii = 0; ii < _vlInfo[INSR].nelems; ii++) {
	    xm_items[ii] = XmStringCreateLocalized (_vlInfo[INSR].range[ii]);
        }

        XtVaSetValues (_insrLstW,
		       XmNitems,	xm_items,
		       XmNitemCount,	_vlInfo[INSR].nelems,
		       NULL);

        for (ii = 0; ii < _vlInfo[INSR].nelems; ii++) {
	    XmStringFree (xm_items[ii]);
        }
	XtFree ((XtPointer) xm_items);

    }

    XtManageChild( _insrLstW );
    XtManageChild( insr_swin );
    XtManageChild( frame );

/*
 * create control buttons
 */
    nn = XtNumber(btnstrs);
    NxmCtlBtn_create(pane, 1, "pgvolw_insrCtlBtn", nn, btnstrs, 
		     (XtCallbackProc)pgvolw_insrCtlBtnCb,     NULL );  

    XtManageChild(pane);

    return(_pgvolwInsrWin);
}

/*=====================================================================*/

void pgvolw_insrPopup ( void )
/************************************************************************
 * pgvolw_insrPopup							*
 *									*
 * This function pops up Information Source Selection window.		*
 *									*
 * void pgvolw_insrPopup()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 * H. Zeng/XTRIA	09/03   added listed item deselection		*
 ***********************************************************************/
{
    int        nitems, ii;
    char       *ptext=NULL, *ptr=NULL, info_src[256];
    XmString   *items;
/*---------------------------------------------------------------------*/

    XmListDeselectAllItems(_insrLstW);
    XtVaGetValues (_insrLstW, 
		   XmNitemCount,   &nitems,
		   XmNitems,       &items,
		   NULL                      );

    XtVaGetValues (_infoTxtW, XmNvalue, &ptext,  NULL); 
    info_src[0] = '\0';

    if ( ptext != NULL ) {

         strcpy(info_src, ptext);
         XtFree(ptext);
    }

    if ( info_src[0] != '\0' ) {

         ptr = strtok (info_src, ".");

         while ( ptr != NULL ) {

             if (ptr[0] == ' ') ptr = ptr + 1;
	     for ( ii = 0; ii < nitems; ii++ ) {

	        XmStringGetLtoR (items[ii], XmFONTLIST_DEFAULT_TAG, 
                                 &ptext );
                if ( strcmp (ptr, ptext) == 0 ) {

                     XmListSelectPos (_insrLstW, ii+1, FALSE);
		     if (ptext != NULL) XtFree (ptext);
                     break;
                }

                if (ptext != NULL) XtFree (ptext);
             }
	    ptr = strtok (NULL, ".");
         }
    }
    XtManageChild (_pgvolwInsrWin);
}

/*=====================================================================*/

void pgvolw_insrPopdown ( void ) 
/************************************************************************
 * pgvolw_insrPopdown							*
 *									*
 * This function pops down Information Source Selection window.		*
 *									*
 * void pgvolw_insrPopdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03   initial coding				*
 ***********************************************************************/
{
    if (XtIsManaged (_pgvolwInsrWin)) {
	XtUnmanageChild (_pgvolwInsrWin);
    }
}

/*=====================================================================*/

Boolean pgvolw_insrIsUp ( void ) 
/************************************************************************
 * pgvolw_insrIsUp							*
 *									*
 * This function queries whether Infor. Source Selection window is up.	*
 *									*
 * Boolean pgvolw_insrIsUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgvolw_insrIsUp	Boolean	     True -- up,  False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 ***********************************************************************/
{
    return (XtIsManaged (_pgvolwInsrWin));
}

/*=====================================================================*/

Widget pgvolw_prodCreate ( Widget parent )
/************************************************************************
 * pgvolw_prodCreate							*
 *									*
 * This function creates the VAA Save popup window.			*
 *									*
 * Widget pgvolw_prodCreate(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgvolw_prodCreate	Widget	Widget ID of Volcano Save popup 	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	08/03	initial coding				*
 * H. Zeng/XTRIA	10/03   use fixed font for text widgets 	*
 * E. Safford/SAIC	05/05	free fontlist				*
 ***********************************************************************/
{
    Widget	pane, frame1, frame2;
    int		nn;
    char	*btnstrs[] = {"Save", "Cancel"};
    char  fontname[] = "-adobe-courier-bold-r-normal-*-*-120-*-*-m-*-*-*";
    Display	     *dsp;
    XmFontListEntry  flentry;
    XmFontList	     fontlist;
/*---------------------------------------------------------------------*/
/*
 * create dialog shell
 */
    _pgvolwProdWin = XmCreateFormDialog(parent, "pgvolw_popup",
				    NULL, 0);
    XtVaSetValues(_pgvolwProdWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pgvolwProdWin),
		  XmNtitle, "VAA Save",
		  NULL);
  
/*
 * Create font list for text widgets.
 */
    dsp = XtDisplay (_pgvolwProdWin);
    flentry = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);
 
/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("pgvolw_pane",
			    xmPanedWindowWidgetClass, _pgvolwProdWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);

    frame1 = XmCreateFrame( pane, "_Insr_frameW", NULL, 0 );

    _prodTxtW = XtVaCreateManagedWidget ("prod_text",
		      xmTextWidgetClass,        frame1,
		      XmNrows,		        40,
		      XmNcolumns,	        50,
		      XmNeditMode,	        XmMULTI_LINE_EDIT,
		      XmNwordWrap,	        TRUE,
		      XmNscrollVertical,	TRUE,
                      XmNeditable,              False,
		      XmNfontList,		fontlist,
		      NULL ); 

    XtManageChild( frame1 );


    frame2 = XmCreateFrame( pane, "_Prod_frameW", NULL, 0 );

    _fnameTxtW = XtVaCreateManagedWidget ("file_text",
		      xmTextFieldWidgetClass,   frame2,
		      XmNcolumns,	        50,
                      XmNeditable,              False,
                      XmNcursorPositionVisible, False,
		      XmNfontList,		fontlist,
		      NULL ); 

    XtManageChild( frame2 );

/*
 * create control buttons
 */
    nn = XtNumber(btnstrs);
    NxmCtlBtn_create(pane, 1, "pgvolw_prodCtlBtn", nn, btnstrs, 
		     (XtCallbackProc)pgvolw_prodCtlBtnCb,    NULL);  

    XtManageChild(pane);

    XmFontListFree( fontlist );

    return(_pgvolwProdWin);
}

/*=====================================================================*/

void pgvolw_prodPopup ( void )
/************************************************************************
 * pgvolw_prodPopup							*
 *									*
 * This function pops up VAA Save window.				*
 *									*
 * void pgvolw_prodPopup()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	08/03	initial coding				*
 ***********************************************************************/
{
    XtManageChild (_pgvolwProdWin);
}

/*=====================================================================*/

void pgvolw_prodPopdown ( void ) 
/************************************************************************
 * pgvolw_prodPopdown							*
 *									*
 * This function pops down VAA Save window.				*
 *									*
 * void pgvolw_prodPopdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	08/03   initial coding				*
 ***********************************************************************/
{
    if (XtIsManaged (_pgvolwProdWin)) {
	XtUnmanageChild (_pgvolwProdWin);
    }
}

/*=====================================================================*/

Boolean pgvolw_prodIsUp ( void ) 
/************************************************************************
 * pgvolw_prodIsUp							*
 *									*
 * This function queries whether VAA Save window is up.			*
 *									*
 * Boolean pgvolw_prodIsUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgvolw_prodIsUp	Boolean	     True -- up,  False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	08/03	initial coding				*
 ***********************************************************************/
{
    return (XtIsManaged (_pgvolwProdWin));
}

/*=====================================================================*/

Widget pgvolw_obsCreate ( Widget parent )
/************************************************************************
 * pgvolw_obsCreate							*
 *									*
 * This function creates the VAA Ash Cloud Info window.			*
 *									*
 * Widget pgvolw_obsCreate(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgvolw_obsCreate	Widget	Widget ID of Ash Cloud Info window 	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04	initial coding				*
 * D.W.Plummer/NCEP	08/04	Allow forecast hour info to be editable	*
 ***********************************************************************/
{
    Widget	pane, form, obcloud_lbl;
    Widget	facd_lbl_06, facd_lbl_12, facd_lbl_18;
    int		nn, toff = 5;
    char	*btnstrs[] = {"Close"};
/*---------------------------------------------------------------------*/
/*
 * create dialog shell
 */
    _pgvolwObsWin = XmCreateFormDialog(parent, "pgvolw_obsPopup",
				    NULL, 0);
    XtVaSetValues(_pgvolwObsWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_pgvolwObsWin),
		  XmNtitle, "VAA Ash Cloud Info",
		  NULL);
   
/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("pgvolw_pane",
			    xmPanedWindowWidgetClass, _pgvolwObsWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);

    form = XtVaCreateWidget("form", xmFormWidgetClass, pane, NULL);

/*
 * Obs Ash Cloud:
 */
    obcloud_lbl = XtVaCreateManagedWidget ( "Obs Ash Cloud:",
                      xmLabelWidgetClass,	form,
                      XmNtopAttachment,         XmATTACH_FORM,
		      XmNtopOffset,	        toff,
                      XmNleftAttachment,        XmATTACH_FORM,
		      NULL );

    _obcloudTxtW = XtVaCreateManagedWidget ("obcloud_text",
		      xmTextWidgetClass,        form,
		      XmNrows,			3,
		      XmNcolumns,	        47,
                      XmNeditable,              False,
		      XmNeditMode,	        XmMULTI_LINE_EDIT,
		      XmNmaxLength,	        1022,
		      XmNwordWrap,	        TRUE,
		      XmNscrollVertical,	TRUE,
		      XmNtopAttachment,	        XmATTACH_WIDGET,
		      XmNtopWidget,		obcloud_lbl,
		      XmNleftAttachment,        XmATTACH_FORM,
		      NULL ); 

/*
 * Fcst Ash Cloud +06hr:
 */
    facd_lbl_06 = XtVaCreateManagedWidget ( "Fcst Ash Cloud +06hr:",
                      xmLabelWidgetClass,	form,
                      XmNtopAttachment,         XmATTACH_WIDGET,
		      XmNtopWidget,		_obcloudTxtW,
		      XmNtopOffset,	        toff,
                      XmNleftAttachment,        XmATTACH_FORM,
		      NULL );

    _facdTxt06W = XtVaCreateManagedWidget ("facd_text_06",
		      xmTextWidgetClass,        form,
		      XmNrows,			3,
		      XmNcolumns,	        47,
                      XmNeditable,              True,
		      XmNeditMode,	        XmMULTI_LINE_EDIT,
		      XmNmaxLength,	        1022,
		      XmNwordWrap,	        TRUE,
		      XmNscrollVertical,	TRUE,
		      XmNtopAttachment,	        XmATTACH_WIDGET,
		      XmNtopWidget,		facd_lbl_06,
		      XmNleftAttachment,        XmATTACH_FORM,
		      NULL ); 

/*
 * Fcst Ash Cloud +12hr:
 */
    facd_lbl_12 = XtVaCreateManagedWidget ( "Fcst Ash Cloud +12hr:",
                      xmLabelWidgetClass,	form,
                      XmNtopAttachment,         XmATTACH_WIDGET,
		      XmNtopWidget,		_facdTxt06W,
		      XmNtopOffset,	        toff,
                      XmNleftAttachment,        XmATTACH_FORM,
		      NULL );

    _facdTxt12W = XtVaCreateManagedWidget ("facd_text_12",
		      xmTextWidgetClass,        form,
		      XmNrows,			3,
		      XmNcolumns,	        47,
                      XmNeditable,              True,
		      XmNeditMode,	        XmMULTI_LINE_EDIT,
		      XmNmaxLength,	        1022,
		      XmNwordWrap,	        TRUE,
		      XmNscrollVertical,	TRUE,
		      XmNtopAttachment,	        XmATTACH_WIDGET,
		      XmNtopWidget,		facd_lbl_12,
		      XmNleftAttachment,        XmATTACH_FORM,
		      NULL ); 

/*
 * Fcst Ash Cloud +18hr:
 */
    facd_lbl_18 = XtVaCreateManagedWidget ( "Fcst Ash Cloud +18hr:",
                      xmLabelWidgetClass,	form,
                      XmNtopAttachment,         XmATTACH_WIDGET,
		      XmNtopWidget,		_facdTxt12W,
		      XmNtopOffset,	        toff,
                      XmNleftAttachment,        XmATTACH_FORM,
		      NULL );

    _facdTxt18W = XtVaCreateManagedWidget ("facd_text_18",
		      xmTextWidgetClass,        form,
		      XmNrows,			3,
		      XmNcolumns,	        47,
                      XmNeditable,              True,
		      XmNeditMode,	        XmMULTI_LINE_EDIT,
		      XmNmaxLength,	        1022,
		      XmNwordWrap,	        TRUE,
		      XmNscrollVertical,	TRUE,
		      XmNtopAttachment,	        XmATTACH_WIDGET,
		      XmNtopWidget,		facd_lbl_18,
		      XmNleftAttachment,        XmATTACH_FORM,
		      NULL ); 

    XtManageChild ( form );

/*
 * create control buttons
 */
    nn = XtNumber(btnstrs);
    NxmCtlBtn_create(pane, 1, "pgvolw_obsCtlBtn", nn, btnstrs, 
		     (XtCallbackProc)pgvolw_obsCtlBtnCb,    NULL);  

    XtManageChild(pane);

    return(_pgvolwObsWin);
}

/*=====================================================================*/

void pgvolw_obsPopup ( void )
/************************************************************************
 * pgvolw_obsPopup							*
 *									*
 * This function pops up VAA Ash Cloud Info window.			*
 *									*
 * void pgvolw_obsPopup()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04	initial coding				*
 ***********************************************************************/
{
    XtManageChild (_pgvolwObsWin);
}

/*=====================================================================*/

void pgvolw_obsPopdown ( void ) 
/************************************************************************
 * pgvolw_obsPopdown							*
 *									*
 * This function pops down VAA Ash Cloud Info window.			*
 *									*
 * void pgvolw_obsPopdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04   initial coding				*
 ***********************************************************************/
{
    if (XtIsManaged (_pgvolwObsWin)) {
	XtUnmanageChild (_pgvolwObsWin);
    }
}

/*=====================================================================*/

Boolean pgvolw_obsIsUp ( void ) 
/************************************************************************
 * pgvolw_obsIsUp							*
 *									*
 * This function queries whether VAA Ash Cloud Info window is up.	*
 *									*
 * Boolean pgvolw_obsIsUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * pgvolw_obsIsUp	Boolean	     True -- up,  False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04	initial coding				*
 ***********************************************************************/
{
    return (XtIsManaged (_pgvolwObsWin));
}

/*=====================================================================*/

void pgvolw_checkReady ( void ) 
/************************************************************************
 * pgvolw_checkReady							*
 *									*
 * This function checks if it is ready to create a new volcano element	*
 *									*
 * void pgvolw_checkReady ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03   initial coding				*
 * H. Zeng/XTRIA	02/04   added a new btn				*
 ***********************************************************************/
{
    if (_nameOK && _numOK && _locOK && _areaOK && _elevOK) {
        XtSetSensitive (_ctlBtnW[0], TRUE); 
        XtSetSensitive (_ctlBtnW[1], TRUE); 
    }
    else {
        XtSetSensitive (_ctlBtnW[0], FALSE);
        XtSetSensitive (_ctlBtnW[1], FALSE);   
    }
}

/*=====================================================================*/

Boolean pgvolw_isLocValid ( void ) 
/************************************************************************
 * pgvolw_isLocValid							*
 *									*
 * This function checks if we can successfully get valid latlon values	*
 * from _volLoc str.							*
 *									*
 * Boolean pgvolw_isLocValid ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 * pgvolw_isLocValid    Boolean      True -- yes,  False -- no		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03   initial coding				*
 * H. Zeng/XTRIA	08/03	modified for more check			*
 * H. Zeng/XTRIA	10/03   changed for new format			*
 * H. Zeng/XTRIA	11/03   made format more flexible		*
 ***********************************************************************/
{
    char     loc_str[25], lat_str[8], lon_str[8];
    float    lat, lon;
    int      length, idx, ii, ier;
/*---------------------------------------------------------------------*/

    strcpy( loc_str, _volLoc );
    cst_rmbl (loc_str, loc_str, &length, &ier);

    if ( length != 11 ) return (FALSE);

    idx = 0;

    if ( loc_str[idx] == 'N' || loc_str[idx] == 'n' ) {
         lat =  1.0F;
    }
    else if ( loc_str[idx] == 'S' || loc_str[idx] == 's' ) {
         lat = -1.0F;
    }
    else {
         return (FALSE);
    }

    idx++;

    for ( ii = 0; ii < 4; ii++ ) {
        if ( !isdigit( (int)loc_str[idx+ii] ) ) return (FALSE);
        lat_str[ii] = loc_str[idx+ii];
    }

    lat_str[ii] = '\0';
    lat = lat * atoi(lat_str) / 100.0F;

    idx += 4;

    if ( loc_str[idx] == 'E' || loc_str[idx] == 'e' ) {
         lon =  1.0F;
    }
    else if ( loc_str[idx] == 'W' || loc_str[idx] == 'w' ) {
         lon = -1.0F;
    }
    else {
         return (FALSE);
    }

    idx++;

    for ( ii = 0; ii < 5; ii++ ) {
        if ( !isdigit( (int)loc_str[idx+ii] ) ) return (FALSE);
        lon_str[ii] = loc_str[idx+ii];
    }

    lon_str[ii] = '\0';
    lon = lon * atoi(lon_str) / 100.0F;

    idx += 5;

    if ( lat > 90.0 || lat < -90.0 || lon > 180.0 || lon <= -180.0) {

         return (FALSE);
    }

    _volLat = lat;
    _volLon = lon;
    strcpy( _volLoc, loc_str );

    return (TRUE);
}

/*=====================================================================*/

void pgvolw_setWmoHdr ( int index )
/************************************************************************
 * pgvolw_setWmoHdr							*
 *									*
 * This function sets WMO&HDR pulldown menu according to current	*
 * Orig Stn/VAAC index.							*
 *									*
 * void pgvolw_setWmoHdr ()						*
 *									*
 * Input parameters:							*
 *           index	int	current Orig Stn/VAAC index		*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03   initial coding				*
 ***********************************************************************/
{
    int	       ii, nelems;
    XmString   xmstr;
/*---------------------------------------------------------------------*/
/*
 * set up WMO pulldown menu.
 */
    nelems = (index == -1) ? 0 : _wmoRange[index].nelems;

    for ( ii = 0; ii < nelems; ii++ ) {

       xmstr = XmStringCreateLocalized (_wmoRange[index].range[ii]);
       XtVaSetValues (_wmoBtnW[ii], 
		      XmNlabelString,	       xmstr,
		      NULL);

       XmStringFree (xmstr);
       XtManageChild (_wmoBtnW[ii]);

    }

    for ( ii = nelems; ii < MXELE; ii++ ) {
       XtUnmanageChild (_wmoBtnW[ii]);
    }

/*
 * Set up HDR pulldown menu.
 */
    nelems = (index == -1) ? 0 : _hdrRange[index].nelems;

    for ( ii = 0; ii < nelems; ii++ ) {

       xmstr = XmStringCreateLocalized (_hdrRange[index].range[ii]);
       XtVaSetValues (_hdrBtnW[ii], 
		      XmNlabelString,	       xmstr,
		      NULL);

       XmStringFree (xmstr);
       XtManageChild (_hdrBtnW[ii]);

    }

    for ( ii = nelems; ii < MXELE; ii++ ) {
	XtUnmanageChild (_hdrBtnW[ii]);
    }
}

/*=====================================================================*/

void pgvolw_clearEditForm ( void ) 
/************************************************************************
 * pgvolw_clearEditForm							*
 *									*
 * This function clears editable VAA Volcano Edit window info.		*
 *									*
 * void pgvolw_clearEditForm ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03   initial coding				*
 * H. Zeng/XTRIA	09/03   added list item deselection		*
 * H. Zeng/XTRIA	01/04   added more forecast text widgets	*
 ***********************************************************************/
{
    XmTextSetString(_vaacTxtW,     "\0");
    XmTextSetString(_wmoTxtW,      "\0"); 
    XmTextSetString(_hdrTxtW,      "\0");
    XmTextSetString(_yearTxtW,     "\0");
    XmTextSetString(_adnmTxtW,     "\0"); 
    XmTextSetString(_infoTxtW,     "\0");

    XmTextSetString(_aviaTxtW,     "\0");
    XtSetSensitive (_aviaForm, FALSE);

    XmTextSetString(_erupTxtW,     "\0");
    XmTextSetString(_obdateTxtW,   "\0"); 
    XmTextSetString(_obtimeTxtW,   "\0");
    XmTextSetString(_obcloudTxtW,  "\0");
    XmTextSetString(_facdTxt06W,   "\0"); 
    XmTextSetString(_facdTxt12W,   "\0"); 
    XmTextSetString(_facdTxt18W,   "\0"); 
    XmTextSetString(_remkTxtW,     "\0");
    XmTextSetString(_nxadTxtW,     "\0"); 
    XmTextSetString(_fcstTxtW,     "\0");

    XmToggleButtonSetState(_nilBtnW,  FALSE, TRUE);
    XmToggleButtonSetState(_corrBtnW, FALSE, TRUE);

    XmListDeselectAllItems(_insrLstW);
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_menuTextCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_menuTextCb							*
 *									*
 * Callback function for general option radio buttons.			*
 *									*
 * void pgvolw_menuTextCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	07/03	copied from pgwfmt_menuTextCb()		*
 ***********************************************************************/
{
    char	*ptext=NULL;
    int		type;
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
	type = (long)userdata;

	switch ( type ) {

	  case VOLC:
	       if ( ptext != NULL && ptext[0] != '\0' ) {

                  strcpy ( _volName, ptext );
		  _nameOK = TRUE;
		  pgvolw_checkReady();
	       }
	       else {
		  _volName[0] = '\0';
		  _nameOK = FALSE;
		  pgvolw_checkReady();
	       }
	       break;

	  default:
	       break;

        }
 
	if ( ptext != NULL ) XtFree(ptext);
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
        XmTextSetString(*twid, ptext);

/*
 * If this is a Orig Stn/VAAC option button, set up WMO&HDR
 * pulldown menu accordingly.
 */
        if ( *twid == _vaacTxtW ) {
        
	     pgvolw_setWmoHdr ( (int)which );

             if ( strstr (ptext, "PAWU") != NULL ) {

                XtSetSensitive (_aviaForm, TRUE );
             }
             else {

                XmTextSetString(_aviaTxtW, "\0" );
                XtSetSensitive (_aviaForm, FALSE);
             }
        }

	XtFree (ptext);
        break;    

    } /* the end of switch */
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_volcMenuCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_volcMenuCb							*
 *									*
 * Callback function for volcano name menu.				*
 *									*
 * void pgvolw_volcMenuCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	07/03	initial coding				*
 * H. Zeng/XTRIA	08/03   made non-editable in certain situations *
 * H. Zeng/XTRIA	10/03   changed for new location format		*
 ***********************************************************************/
{
    char	*ptext=NULL, area[30], elev_str[20];
    char	dummy[NM_SIZE], smnm[12], loca[25];
    int		index;
    float	elev, lat, lon;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:

         XmTextSetString(_volcTxtW,    "\0" );
         XmTextSetString(_numberTxtW,  "\0" );
         XmTextSetString(_locTxtW,     "\0" );
         XmTextSetString(_areaTxtW,    "\0" );
         XmTextSetString(_elevTxtW,    "\0" );

/*
 * Make all text fields editable.
 */
         XtVaSetValues(_volcTxtW, 
                       XmNeditable,              TRUE,
                       XmNcursorPositionVisible, TRUE,
		       NULL);

         XtVaSetValues(_numberTxtW, 
                       XmNeditable,              TRUE,
                       XmNcursorPositionVisible, TRUE,
		       NULL);

         XtVaSetValues(_locTxtW, 
                       XmNeditable,              TRUE,
                       XmNcursorPositionVisible, TRUE,
		       NULL);

         XtVaSetValues(_areaTxtW, 
                       XmNeditable,              TRUE,
                       XmNcursorPositionVisible, TRUE,
		       NULL);

         XtVaSetValues(_elevTxtW, 
                       XmNeditable,              TRUE,
                       XmNcursorPositionVisible, TRUE,
		       NULL);

         break;

      default :

  	 XtVaGetValues (wid, 
		        XmNlabelString,	&xmstr,
		        NULL);
	 if ( ptext != NULL ) XtFree(ptext);
	 XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &ptext);
	 XmStringFree (xmstr);

         XmTextSetString(_volcTxtW, ptext);

/*
 * Automatiically fill in other info. of this particular volcano
 * from volcano table.
 */
         index = NxmVolcano_getIdx (ptext);

	 NxmVolcano_getInfo ( index, &lat, &lon, dummy );
         loca[0] = '\0';

         if ( lat >= 0 ) {
              strcat (loca, "N");
         }
         else {
	      strcat (loca, "S");
         }

	 sprintf (loca+strlen(loca), "%04d", (int)fabs(lat*100) );

         if ( lon >= 0 ) {
              strcat (loca, "E");
         }
         else {
	      strcat (loca, "W");
         }

	 sprintf (loca+strlen(loca), "%05d", (int)fabs(lon*100) );

         XmTextSetString(_locTxtW, loca );

	 NxmVolcano_getSmNm ( index,  smnm );
         XmTextSetString(_numberTxtW, smnm );

	 NxmVolcano_getArea ( index, area );
         XmTextSetString(_areaTxtW,  area );

	 NxmVolcano_getElev ( index, &elev );

/*
 * Covert meter to foot.
 */
         elev = elev * M2F;

	 sprintf(elev_str, "%-10.1f", elev );
         XmTextSetString(_elevTxtW,   elev_str );

	 XtFree (ptext);

/*
 * Make all text fields non-editable.
 */
         XtVaSetValues(_volcTxtW, 
                       XmNeditable,              FALSE,
                       XmNcursorPositionVisible, FALSE,
		       NULL);

         XtVaSetValues(_numberTxtW, 
                       XmNeditable,              FALSE,
                       XmNcursorPositionVisible, FALSE,
		       NULL);

         XtVaSetValues(_locTxtW, 
                       XmNeditable,              FALSE,
                       XmNcursorPositionVisible, FALSE,
		       NULL);

         XtVaSetValues(_areaTxtW, 
                       XmNeditable,              FALSE,
                       XmNcursorPositionVisible, FALSE,
		       NULL);

         XtVaSetValues(_elevTxtW, 
                       XmNeditable,              FALSE,
                       XmNcursorPositionVisible, FALSE,
		       NULL);

         break;    

    } /* the end of switch */
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_numTxtCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_numTxtCb							*
 *									*
 * Callback function for Number text widgets.				*
 *									*
 * void pgvolw_numTxtCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	07/03	initial coding				*
 ***********************************************************************/
{
    char	*ptext=NULL;
/*---------------------------------------------------------------------*/

    XtVaGetValues (wid,  XmNvalue,  &ptext,  NULL);
       
    if ( ptext != NULL && ptext[0] != '\0' ) {

       strcpy ( _volNum, ptext );
       _numOK = TRUE;
       pgvolw_checkReady();
    }
    else {
       _volNum[0] = '\0';
       _numOK = FALSE;
       pgvolw_checkReady();
    }

    if ( ptext != NULL ) XtFree(ptext);
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_locTxtCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_locTxtCb							*
 *									*
 * Callback function for Location text widgets.				*
 *									*
 * void pgvolw_locTxtCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	07/03	initial coding				*
 ***********************************************************************/
{
    char	*ptext=NULL;
/*---------------------------------------------------------------------*/

    XtVaGetValues (wid,  XmNvalue,  &ptext,  NULL);
       
    if ( ptext != NULL && ptext[0] != '\0' ) {

       strcpy ( _volLoc, ptext );
       _locOK = pgvolw_isLocValid ();
       pgvolw_checkReady();
    }
    else {
       _volLoc[0] = '\0';
       _locOK = FALSE;
       pgvolw_checkReady();
    }

    if ( ptext != NULL ) XtFree(ptext);
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_areaTxtCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_areaTxtCb							*
 *									*
 * Callback function for Area text widgets.				*
 *									*
 * void pgvolw_areaTxtCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	07/03	initial coding				*
 ***********************************************************************/
{
    char	*ptext=NULL;
/*---------------------------------------------------------------------*/

    XtVaGetValues (wid,  XmNvalue,  &ptext,  NULL);
       
    if ( ptext != NULL && ptext[0] != '\0' ) {

       strcpy ( _volArea, ptext );
       _areaOK = TRUE;
       pgvolw_checkReady();
    }
    else {
       _volArea[0] = '\0';
       _areaOK = FALSE;
       pgvolw_checkReady();
    }

    if ( ptext != NULL ) XtFree(ptext);
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_elevTxtCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_elevTxtCb							*
 *									*
 * Callback function for Elevation  text widgets.			*
 *									*
 * void pgvolw_elevTxtCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which callback type			*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 ***********************************************************************/
{
    char	*ptext=NULL, text_val[15];
    float	elevation, negone=-1.0;
    int		vol_elev;
/*---------------------------------------------------------------------*/

    switch ( which ) {

      case 0:  /* XmNvalueChangedCallback */

         XtVaGetValues (wid,  XmNvalue,  &ptext,  NULL);

         if ( sscanf (ptext, "%f", &elevation) == 1 ) {

            _volElev = elevation;
            _elevOK = TRUE;
            pgvolw_checkReady();
         }
         else {
            _volElev = -1.0;
            _elevOK = FALSE;
            pgvolw_checkReady();
         }

         if ( ptext != NULL ) XtFree(ptext);

	 break;

      case 1: /* XmNlosingFocusCallback */

	 if ( !G_DIFF(_volElev, negone) ) {
            sprintf( text_val, "%-7.0f", _volElev );
	    text_val[8] = '\0';
            XtVaSetValues (wid,  XmNvalue,  text_val,  NULL);

	    sscanf (text_val, "%d", &vol_elev);
	    _volElev = (float)vol_elev;
	   
         }

	 break;
    }       
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_corrTxtCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_corrTxtCb							*
 *									*
 * Callback function for Correction text widget.			*
 *									*
 * void pgvolw_corrTxtCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	04/06	initial coding				*
 ***********************************************************************/
{
    char	*ptext=NULL, *ptext2=NULL, cadvnm[8], cyear[8];
    int		incr, advnm, year;
/*---------------------------------------------------------------------*/
/*
 * If _advNoInfo.status == 0, advisory no won't change with 
 * correction flag, just return.
 */
    if ( _advNoInfo.status == 0 )  return;

    XtVaGetValues (wid,  XmNvalue,  &ptext,  NULL); 
    if ( ptext != NULL && ptext[0] != '\0' )  incr = -1;
    else  incr = 1;
    if ( ptext != NULL ) XtFree(ptext);

    if ( (incr ==  1 && _advNoInfo.status == 2) || 
         (incr == -1 && _advNoInfo.status == 1)    )  return;

    ptext2 = XmTextGetString (_adnmTxtW);
    if ( sscanf(ptext2, "%d", &advnm) != 1 ) {

      if ( ptext2 != NULL ) XtFree(ptext2);
      return;
    }

    if ( ptext2 != NULL )  XtFree(ptext2);

    if ( incr == 1 )  {

      advnm = _advNoInfo.advnm;
      year  = _advNoInfo.year;
      sprintf (cadvnm, "%03d", advnm);
      sprintf (cyear,  "%4d",  year);   
      XtVaSetValues (_adnmTxtW, XmNvalue, cadvnm, NULL);
      XtVaSetValues (_yearTxtW, XmNvalue, cyear,  NULL);
      _advNoInfo.status = 2;
    }
    else if ( incr == -1 ) {

      advnm = _advNoInfo.prev_advnm;
      year  = _advNoInfo.prev_year;
      sprintf (cadvnm, "%03d", advnm);
      sprintf (cyear,  "%4d",  year);   
      XtVaSetValues (_adnmTxtW, XmNvalue, cadvnm, NULL);
      XtVaSetValues (_yearTxtW, XmNvalue, cyear,  NULL);
      _advNoInfo.status = 1;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_createCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_createCtlBtnCb						*
 *									*
 * Callback function for control buttons at the bottom of VAA Volcano   *
 * Create window.							*
 *									*
 * void pgvolw_createCtlBtnCb (wid, which, call)			*
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
 * H. Zeng/XTRIA	07/03	initial coding				*
 * H. Zeng/XTRIA	02/04	added a new btn				*
 ***********************************************************************/
{
    float   latitude[1], longitude[1];
    int	    location, grpnum, np, ier, ier2;
    char    grptyp;
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* Create VAA Volcano */

        latitude[0]  = _volLat;
	longitude[0] = _volLon;	

	np = 1;
	pgnew_getGrpInfo (&grptyp, &grpnum);
        pgvgf_save(grptyp, grpnum, np, latitude, longitude,
			                &location, &ier );

        if (ier == 0) {
	    pgundo_newStep();
	    pgundo_storeThisLoc (location, UNDO_ADD, &ier2);
            pgundo_endStep();
        }

        pgvolw_popdown();

	break;

      case 1:	/* Create Volcano in Layers */

        latitude[0]  = _volLat;
	longitude[0] = _volLon;	

	np = 1;
	pgnew_getGrpInfo (&grptyp, &grpnum);
        pgvgf_save(grptyp, grpnum, np, latitude, longitude,
			                &location, &ier );

        if (ier == 0) {
	    pgundo_newStep();
	    pgundo_storeThisLoc (location, UNDO_ADD, &ier2);
            pgundo_endStep();
        }

        pgvolw_popdown();

	pgvolw_setVolLys( location );

	break;

      case 2:	/* Cancel */   
        pgvolw_popdown();    

	break;

    } /* the end of switch(which) */
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_editCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_editCtlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of VAA Volcano   *
 * Edit window.								*
 *									*
 * void pgvolw_editCtlBtnCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	07/03	initial coding				*
 * H. Zeng/SAIC		06/05	added call to pgvolw_insertObsTime()	*
 ***********************************************************************/
{
    switch(which) {

      case 0:	/* Apply */
        mcanvw_setCursor(CURS_BUSY);
        pgvolw_insertObsTime();
        pgvolw_attrSave (FALSE    ); 
        mcanvw_setCursor(CURS_DEFAULT); 

	break;

      case 1:	/* Cancel */
        pgvolw_editPopdown();  

	break;

      case 2:	/* Format VAA */
        mcanvw_setCursor(CURS_BUSY);
        pgvolw_insertObsTime();
        pgvolw_attrSave (TRUE     ); 
        mcanvw_setCursor(CURS_DEFAULT); 
        pgvolw_editPopdown();
	pgvolw_prodPopup();

	break;

      case 3:	/* Reset Form */   
	pgvolw_clearEditForm();

	break;

    } /* the end of switch(which) */
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_insrCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_insrCtlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of Information   *
 * Source Selection window.						*
 *									*
 * void pgvolw_insrCtlBtnCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	07/03	initial coding				*
 * H. Zeng/XTRIA	09/03   added check to the size of selections	*
 ***********************************************************************/
{
    int	       nitems, ii;
    char       *ptext=NULL, text_str[256];
    XmString   *items;
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* OK */

        text_str[0] = '\0';
        XtVaGetValues (_insrLstW, 
		       XmNselectedItemCount,   &nitems,
		       XmNselectedItems,       &items,
		       NULL                             );

	for ( ii = 0; ii < nitems; ii++ ) {

	   XmStringGetLtoR (items[ii], XmFONTLIST_DEFAULT_TAG, &ptext);

           if ( strlen(text_str) + strlen(ptext) <= (size_t)250 ) {

                strcat (text_str, ptext);
	        strcat (text_str, ". "  );
	   }

           XtFree (ptext);
        }

        XmTextSetString( _infoTxtW, text_str );

        pgvolw_insrPopdown();

	break;

      case 1:	/* Cancel */
        pgvolw_insrPopdown();  

	break;

    } /* the end of switch(which) */
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_prodCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_prodCtlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of VAA Save      *
 * window.								*
 *									*
 * void pgvolw_prodCtlBtnCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	08/03	initial coding				*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * H. Zeng/XTRIA	11/03   added VAA.txt file			*
 * H. Zeng/XTRIA	02/04	added special VAA product case		*
 * S. Danz/AWC		07/06	Update to new cvg_write() parameter     *
 * H. Zeng/SAIC		07/09	modification for "Cancel" callback	*
 ***********************************************************************/
{
    int		flag, orecsz, nrecsz, el_layer, ier; 
    int		total_lyr, lyr_idx;
    long        cursiz, curpos, wrk_size;
    char	*text=NULL, *fname=NULL, tmp_file[FILE_FULLSZ];
    char	full_name[FILE_FULLSZ], file_name[MXFLSZ];
    char	file_path[LLPATH];
    Boolean	more;    
    FILE	*wrkfp, *outfp;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* Save */
	fname = XmTextFieldGetString (_fnameTxtW);
	text  = XmTextGetString (_prodTxtW);

/*
 * Save VAA text product.
 */
        strcpy (full_name, _txtPath);
        strcat (full_name, fname   );

	outfp = fopen (full_name, "w");
	if (outfp == (FILE *) NULL) {
	    NxmWarn_show (_pgvolwProdWin, "Invalid filename\n");
	    return;
	}
	fputs (text, outfp);
	fclose (outfp);

/*
 * Save VAA text product into VAA.txt
 */
        sprintf (full_name, "%sVAA.txt", _txtPath);
	outfp = fopen (full_name, "w");
	if (outfp == (FILE *) NULL) {
	    NxmWarn_show (_pgvolwProdWin, "Invalid filename\n");
	    return;
	}
	fputs (text, outfp);
	fclose (outfp);

	if ( text != NULL ) XtFree(text);

/*
 * Stop here for special VAA products.
 */
        if ( XtIsSensitive (_loc1Strc.form) == FALSE ) {

	     if ( fname != NULL ) XtFree(fname);

             pgvolw_prodPopdown();

	     break;
        }

/*
 * Save vgf product for regular VAA.
 */
        total_lyr = pglayer_getNumLayers( );

        for ( lyr_idx = 0; lyr_idx < total_lyr; lyr_idx++ ) {

            pglayer_getFilePath ( lyr_idx, file_path );
            pglayer_getFileName ( lyr_idx, file_name );
                
            if ( strlen(file_name) > (size_t)0 ) {
		
	       strcpy (full_name, file_path);
	       strcat (full_name, file_name);
	    }
            else {

               cst_rpst  ( fname, ".txt", ".vgf", fname, &ier );

               strcpy (full_name, _txtPath);
               strcat (full_name, fname   );
	    }

	    cvg_crvgf ( full_name, &ier);
	    cfl_inqr  ( full_name, NULL, &cursiz, tmp_file, &ier);
	    cvg_open  ( full_name, G_TRUE, &outfp, &ier);        
	    cfl_seek  ( outfp, cursiz, 0, &ier);

/* 
 *  Select non-deleted elements on the specified layer
 *  from WORK_FILE and save into the output file. 
 */
	    more = TRUE;
	    cfl_inqr (cvg_getworkfile(), NULL, &wrk_size, tmp_file, &ier);
	    cvg_open (cvg_getworkfile(), G_FALSE, &wrkfp, &ier);  	
       	
	    curpos = sizeof(el.hdr) + sizeof(el.elem.fhed);
        
	    while ( ( more ) && ( curpos < wrk_size ) ) {

	        el_layer = crg_getLayer ((int)curpos);
	        cvg_rdhdr (cvg_getworkfile(), wrkfp, (int)curpos, 
		           (int)wrk_size, &el, &flag, &ier);
	    
	        if ( ier == 0 && el.hdr.recsz > 0 ) { 
	        
		    orecsz = el.hdr.recsz;

	            cvg_rdele (&el, (int)curpos, el.hdr.recsz, wrkfp, &ier);

	            if ( ier != 0 ) {
	                more = G_FALSE;
	            }

/*
 * Skip non-current-layer elements, deleted elements and file-head element.
 */
	            if ( ( more ) && ( el.hdr.delete == 0 ) &&
		         ( el_layer == lyr_idx ) &&
	    	         ( el.hdr.vg_type != FILEHEAD_ELM ) )  {

	                 nrecsz = el.hdr.recsz;	
		         cvg_write (&el, -2, nrecsz, outfp, FALSE, &ier);
		    
		         cursiz = cursiz + (long)nrecsz; /* re-position in output file */
		    }	     
                
	            curpos += (long)orecsz;  /* re-position in WORK_FILE */
	        }	    	    
	        else {
		more = G_FALSE;
	        }
	    	    
            }  /* End of "while" loop */

/* 
 * Close all files.
 */
	    cvg_clos (wrkfp, &ier);
	    cvg_clos (outfp, &ier);
 

	} /* the end of for ( lyr_idx = 0... */

	if ( fname != NULL ) XtFree(fname);

        pgvolw_prodPopdown();

	break;

      case 1:	/* Cancel */
        pgvolw_prodPopdown();

        if ( XtIsSensitive (_loc1Strc.form) == TRUE) {

/*
 * Pop up the Volcano edit window again.
 */
           if ( !(XtIsManaged (_pgvolwEditWin)) ) {

             XtManageChild (_pgvolwEditWin);
           }

        }
        else {

/*
 * Popup the current special VAA product.
 */
	   pgvolw_editPopup (&_spProdEl);
        }

	break;

    } /* the end of switch(which) */
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_obsCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_obsCtlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of VAA Ash Cloud *
 * Info window.								*
 *									*
 * void pgvolw_obsCtlBtnCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	01/04	initial coding				*
 ***********************************************************************/
{
    switch(which) {

      case 0:	/* Close */
        pgvolw_obsPopdown();  

	break;

    } /* the end of switch(which) */
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_infoBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_infoBtnCb							*
 *									*
 * Callback function for "Information Source" button on edit window.	*
 *									*
 * void pgvolw_infoBtnCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	07/03	initial coding				*
 ***********************************************************************/
{
    if ( !pgvolw_insrIsUp() ) {

          pgvolw_insrPopup ();
    }
    else {

	  pgvolw_insrPopdown();
          pgvolw_insrPopup ();
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_obsBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgvolw_obsBtnCb							*
 *									*
 * Callback function for "Observed and Forecast Ash Cloud Info..."	*
 * button on edit window.						*
 *									*
 * void pgvolw_obsBtnCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	01/04	initial coding				*
 ***********************************************************************/
{
    if ( !pgvolw_obsIsUp() ) {

          pgvolw_obsPopup  ();
    }
    else {

	  pgvolw_obsPopdown();
          pgvolw_obsPopup  ();
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_nilBtnCb ( Widget wid, long which, 
					XmToggleButtonCallbackStruct *cbs)
/************************************************************************
 * pgvolw_nilBtnCb							*
 *									*
 * Callback function for "NIL" toggle button on edit window.		*
 *									*
 * void pgvolw_nilBtnCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	cbs	XmToggleButtonCallbackStruct				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	08/03	initial coding				*
 ***********************************************************************/
{
    Boolean    toggle_on;
/*---------------------------------------------------------------------*/

    toggle_on = (Boolean)cbs->set;

    if ( toggle_on ) {

       XtVaSetValues(_obdateTxtW, 
                  XmNeditable,              False,
                  XmNcursorPositionVisible, False,
		  NULL);
       XmTextSetString(_obdateTxtW, "NIL");

       XtVaSetValues(_obtimeTxtW, 
                  XmNeditable,              False,
                  XmNcursorPositionVisible, False,
		  NULL);
       XmTextSetString(_obtimeTxtW, "NIL");

    }
    else {

       XtVaSetValues(_obdateTxtW, 
                  XmNeditable,              True,
                  XmNcursorPositionVisible, True,
		  NULL);
       XmTextSetString(_obdateTxtW, "\0");

       XtVaSetValues(_obtimeTxtW, 
                  XmNeditable,              True,
                  XmNcursorPositionVisible, True,
		  NULL);
       XmTextSetString(_obtimeTxtW, "\0");

    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_corrBtnCb ( Widget wid, long which, 
					XmToggleButtonCallbackStruct *cbs)
/************************************************************************
 * pgvolw_corrBtnCb							*
 *									*
 * Callback function for "Correction" toggle button on edit window.	*
 *									*
 * void pgvolw_corrBtnCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	cbs	XmToggleButtonCallbackStruct				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	08/03	initial coding				*
 ***********************************************************************/
{
    Boolean    toggle_on;
/*---------------------------------------------------------------------*/

    toggle_on = (Boolean)cbs->set;

    if ( toggle_on ) {

       XtSetSensitive (_corrRc, TRUE);

    }
    else {

       XmTextSetString(_correctTxtW,  "\0");
       XtSetSensitive (_corrRc, FALSE);

    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgvolw_goBtnCb ( Widget wid, long which, 
					XmToggleButtonCallbackStruct *cbs)
/************************************************************************
 * pgvolw_goBtnCb							*
 *									*
 * Callback function for "Go..." Button on Create Window.		*
 *									*
 * void pgvolw_goBtnCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	cbs	XmToggleButtonCallbackStruct				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	02/04	initial coding				*
 ***********************************************************************/
{
    int		 ier;
    char	 **prod_info, vaac_str[64], *ptr=NULL;
/*---------------------------------------------------------------------*/
/*
 * Obtain the prod_info pointer based on user's choice of special 
 * product on Volcano Create Window.
 */
    prod_info = (char**)(_loc2Info[_loc2Strc.current]);

/*
 * Create a volcano element based on special product info.
 */
    _spProdEl.hdr.vg_class = CLASS_SIGMETS;
    _spProdEl.hdr.vg_type  = VOLC_ELM;

    if ( prod_info[0] != NULL && 
         strcmp(prod_info[0], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.name,
	            prod_info[0], 62, &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.name, "\0");
    }

    if ( prod_info[1] != NULL && 
         strcmp(prod_info[1], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.number, 
	            prod_info[1], 15, &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.number, "\0");
    }

    if ( prod_info[2] != NULL && 
         strcmp(prod_info[2], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.location, 
	            prod_info[2], 15, &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.location, "\0");
    }

    if ( prod_info[3] != NULL && 
         strcmp(prod_info[3], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.area, 
	            prod_info[3], 31, &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.area, "\0");
    }

    if ( prod_info[4] != NULL && 
         strcmp(prod_info[4], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.elev,
	            prod_info[4], 7,  &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.elev, "\0");
    }

    strcpy ( vaac_str, _vlInfo[VAAC].range[0] );
    ptr = strtok (vaac_str, "/");
    strcpy(_spProdEl.elem.vol.info.origstn, ptr);
    ptr = strtok (NULL, "/");
    strcpy(_spProdEl.elem.vol.info.vaac, ptr);

    strcpy ( _spProdEl.elem.vol.info.hdrnum, _hdrRange[0].range[0] );
    strcpy ( _spProdEl.elem.vol.info.wmoid,  _wmoRange[0].range[0] );

    if ( prod_info[5] != NULL && 
         strcmp(prod_info[5], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.advnum, 
	            prod_info[5], 7,  &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.advnum, "\0");
    }

    _spProdEl.elem.vol.info.corr[0] = '\0';

    if ( prod_info[6] != NULL && 
         strcmp(prod_info[6], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.infosorc, 
	            prod_info[6], 254,   &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.infosorc, "\0");
    }  

    strcpy ( _spProdEl.elem.vol.info.addlsorc, "\0" );	
    strcpy ( _spProdEl.elem.vol.info.avcc,     "\0" ); 

    if ( prod_info[7] != NULL && 
         strcmp(prod_info[7], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.details, 
	            prod_info[7], 254,   &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.details, "\0");
    }
      
    strcpy ( _spProdEl.elem.vol.info.obsdate, "NIL" );

    if ( prod_info[9] != NULL && 
         strcmp(prod_info[9], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.obsashcld, 
	            prod_info[9], 1022,  &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.obsashcld, "\0");
    }

    if ( prod_info[10] != NULL && 
         strcmp(prod_info[10], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.fcst_06, 
	            prod_info[10], 1022, &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.fcst_06, "\0");
    }

    if ( prod_info[11] != NULL && 
         strcmp(prod_info[11], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.fcst_12, 
	            prod_info[11], 1022, &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.fcst_12, "\0");
    }

    if ( prod_info[12] != NULL && 
         strcmp(prod_info[12], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.fcst_18, 
	            prod_info[12], 1022, &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.fcst_18, "\0");
    }

    if ( prod_info[13] != NULL && 
         strcmp(prod_info[13], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.remarks, 
	            prod_info[13], 510,  &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.remarks, "\0");
    }

    if ( prod_info[14] != NULL && 
         strcmp(prod_info[14], "USE_DEFAULT") != 0 ) {

         cst_ncpy ( _spProdEl.elem.vol.info.nextadv, 
	            prod_info[14], 126,  &ier );
    }
    else {

	 strcpy ( _spProdEl.elem.vol.info.nextadv, "\0");
    }

/*
 * Mark that this is a special VAA product.
 */
    _spProdEl.elem.vol.info.code = special_prod;

/*
 * Popup volcano edit window.
 */
    pgvolw_popdown();
    pgvolw_editPopup ( &_spProdEl );
}

/*=====================================================================*/

void pgvolw_createProd ( VG_DBStruct *vol, char **filter, char *text, 
					   char *fname,   int *iret   )
/************************************************************************
 * pgvolw_createProd                                                    *
 *                                                                      *
 * This function creates a VAA text message and file name from a VAA	*
 * volcano element. The text message is filtered according to the input *
 * filter setting.							*
 *                                                                      *
 * void pgvolw_createProd ( vol, filter, text, fname, iret )            *
 *                                                                      *
 * Input parameters:                                                    *
 *      *vol            VG_DBStruct	VAA volcano element		*
 *	**filter	char		message filter			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *text           char            VAA text product		*
 *      *fname		char		VAA file name			*
 *      *iret           int             Return code                     *
 *                                         0 - Normal                   *
 *                                        -1 - Invalid element          *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          8/03           Coding                          *
 * H. Zeng/XTRIA	08/03		renamed and added a parameter	*
 * H. Zeng/XTRIA	09/03           made minor modifications	*
 * H. Zeng/XTRIA	10/03           changed current time generator  *
 * H. Zeng/XTRIA	11/03           rounded next_adv time to 5 mins *
 * H. Zeng/XTRIA	11/03           added additional info source	*
 * H. Zeng/XTRIA	01/04		rounded next_adv time to 15 mins*
 * H. Zeng/XTRIA	02/04		added message filter		*
 * D.W.Plummer/NCEP	04/04		added correction info		*
 * H. Zeng/SAIC		06/04		added misc message		*
 * D.W.Plummer/NCEP	07/05	Call clo_from to get (lat,lon) string	*
 * H. Zeng/SAIC		11/05		changed some wording for text	*
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * R. Tian/SAIC		 2/06	Add reading VAA wordind text		*
 * J. Wu/SAIC		04/06		add parameter in cst_wrap 	*
 * H. Zeng/SAIC		04/06	changed to use month index number	*
 * S. Jacobs/NCEP	10/12	Removed NNNN and preceding blank line	*
 ***********************************************************************/
{
    int   vg_class, vg_type, adjust_min;
    int   tarry[5], new_tarry[5], ier;
    const int line_len =   50;	/* line lenth for text product  */
    float elev;
    struct tm	    *utctime;
    time_t	    tp;
    char  *wdstr, wdtxt1[128], wdtxt2[128];
    char  blank[2]={' '},time_str[20], latlon_str[25];
    char  elevm[9], vol_name[64], mon_abbrev[8], next_adv[128];
    char  month[13][4] = {"   ", "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
			  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"};
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 * Read VAA wording text.
 */
    pgvolw_rdWords ( PREF_TBL, VAA_TBL, &wdstr, iret );
    if ( *iret != 0 ) return;

    vg_class = vol->hdr.vg_class;
    vg_type = vol->hdr.vg_type;

    if ( vg_class != CLASS_SIGMETS || vg_type != VOLC_ELM ) {
        *iret = -1;
        return;
    }

    tp = time (NULL);
    utctime = gmtime (&tp);

    tarry[0] = utctime->tm_year + 1900;
    tarry[1] = utctime->tm_mon + 1;
    tarry[2] = utctime->tm_mday;
    tarry[3] = utctime->tm_hour;
    tarry[4] = utctime->tm_min;

    elev = 0;
    cst_crnm ( vol->elem.vol.info.elev, &elev, &ier );
    sprintf ( elevm, "%.0f", 0.3048 * elev );

    strcpy ( text, "FV" );
    sprintf ( &text[strlen(text)], "%s", vol->elem.vol.info.wmoid );
    sprintf ( &text[strlen(text)], "%s ", vol->elem.vol.info.hdrnum );
    strcat ( text, vol->elem.vol.info.origstn ); 
    strcat ( text, " " );
    sprintf ( &text[strlen(text)], "%02d%02d%02d", 
              tarry[2], tarry[3], tarry[4] );
    if ( strlen(vol->elem.vol.info.corr) != (size_t)0 )  
	sprintf ( &text[strlen(text)], " CC%s", vol->elem.vol.info.corr );
    cst_gtag ( "<VAA>", wdstr, " ", wdtxt1, iret );
    cst_gtag ( "<DTG>", wdstr, " ", wdtxt2, iret );
    sprintf ( &text[strlen(text)], "\n%s%s\n%s: ", wdtxt1,
	strlen(vol->elem.vol.info.corr) != (size_t)0 ? " -CORRECTION":"",
	wdtxt2 );
    strcpy ( mon_abbrev, month[tarry[1]] );
    sprintf ( &text[strlen(text)], "%4d%02d%02d/%02d%02dZ", 
              tarry[0], tarry[1], tarry[2], tarry[3], tarry[4] );
    strcat ( text, "\n\n" );
    strcat ( text, "VAAC: " );
    strcat ( text, vol->elem.vol.info.vaac );
    strcat ( text, "\n\n" );

    if ( filter[0] != NULL ) {

         strcat ( text, "VOLCANO: " );

         if ( strcmp(filter[0], "USE_DEFAULT") == 0 ) {

/*
 * Change volcano name to be all upper-case and remove "_"
 */
              strcpy (vol_name, vol->elem.vol.info.name);
              cst_lcuc (vol_name, vol_name, &ier);

              while ( strstr (vol_name, "_") != NULL ) {
	            cst_rpst ( vol_name, "_", " ", vol_name, &ier);
              } 

              strcat ( text, vol_name );
              strcat ( text, " " );
	 }
         else {

              strcat ( text, filter[0] );
              strcat ( text, " " );
         }
    }

    if ( filter[1] != NULL ) {

         if ( strcmp(filter[1], "USE_DEFAULT") == 0 ) {

              strcat ( text, vol->elem.vol.info.number );
	      strcat ( text, "\n" );
	 }
	 else {

              strcat ( text, filter[1] );
	      strcat ( text, "\n" );
         }
    }
    else {

	      strcat ( text, "\n" );
    }

    if ( filter[2] != NULL ) {

         cst_gtag ( "<PSN>", wdstr, " ", wdtxt1, iret );
	 strcat ( text, wdtxt1 );
         strcat ( text, ": " );

         if ( strcmp(filter[2], "USE_DEFAULT") == 0 ) {

	      clo_from ( VOLC_ELM, IMISSD, 1, IMISSD, 
		      &(vol->elem.vol.latlon[0]), &(vol->elem.vol.latlon[1]),
		      sizeof(latlon_str), latlon_str, &ier );
              strcat ( text, latlon_str );
              strcat ( text, "\n\n" );
	 }
	 else {

              strcat ( text, filter[2] );
              strcat ( text, "\n\n" );
         }
    }

    if ( filter[3] != NULL ) {

         strcat ( text, "AREA: " );

         if ( strcmp(filter[3], "USE_DEFAULT") == 0 ) {

              strcat ( text, vol->elem.vol.info.area );
              strcat ( text, "\n\n" );
         }
	 else {

	      strcat ( text, filter[3] );
	      strcat ( text, "\n\n"    );
         }
    }

    if ( filter[4] != NULL ) {

         cst_gtag ( "<SUM>", wdstr, " ", wdtxt1, iret );
	 strcat ( text, wdtxt1 );
         strcat ( text, ": " );

         if ( strcmp(filter[4], "USE_DEFAULT") == 0 ) {

              sprintf( &text[strlen(text)], "%.0f", elev );
              strcat ( text, " FT (" );
              strcat ( text, elevm );
              strcat ( text, " M)\n\n" );
	 }
	 else {

	      strcat ( text, filter[4] );
	      strcat ( text, "\n\n"    );
         }
    }

    if ( filter[5] != NULL ) { 
 
         cst_gtag ( "<ADV>", wdstr, " ", wdtxt1, iret );
	 strcat ( text, wdtxt1 );
         strcat ( text, ": " );

         if ( strcmp(filter[5], "USE_DEFAULT") == 0 ) {

              strcat ( text, vol->elem.vol.info.year );
              strcat ( text, "/" );
              strcat ( text, vol->elem.vol.info.advnum );
              strcat ( text, "\n\n" );
	 }
	 else {

	      strcat ( text, filter[5] );
	      strcat ( text, "\n\n"    );
         }
    }

    if ( filter[6] != NULL ) { 

         cst_gtag ( "<INF>", wdstr, " ", wdtxt1, iret );
	 strcat ( text, wdtxt1 );
         strcat ( text, ": " );

         if ( strcmp(filter[6], "USE_DEFAULT") == 0 ) {

              strcat ( text, vol->elem.vol.info.infosorc );
              strcat ( text, vol->elem.vol.info.addlsorc );
              strcat ( text, "\n\n" );
	 }
	 else {

	      strcat ( text, filter[6] );
	      strcat ( text, "\n\n"    );
         }
    }

    if ( filter[7] != NULL ) { 

         strcat ( text, "ERUPTION DETAILS: " );

         if ( strcmp(filter[7], "USE_DEFAULT") == 0 ) {

              strcat ( text, vol->elem.vol.info.details );
              strcat ( text, "\n\n" );
	 }
	 else {

	      strcat ( text, filter[7] );
	      strcat ( text, "\n\n"    );
         }
    }

    if ( filter[8] != NULL ) { 

         cst_gtag ( "<OAD>", wdstr, " ", wdtxt1, iret );
	 strcat ( text, wdtxt1 );
         strcat ( text, ": " );

         if ( strcmp(filter[8], "USE_DEFAULT") == 0 ) {

              if ( strcmp(vol->elem.vol.info.obsdate, "NIL") == 0 || 
                   strcmp(vol->elem.vol.info.obstime, "NIL") == 0    ) {

                   strcat ( text, "NIL" );
              }
              else {
                   strcat ( text, vol->elem.vol.info.obsdate );
                   strcat ( text, "/" );
                   strcat ( text, vol->elem.vol.info.obstime );
	           strcat ( text, "Z" );
              }
              strcat ( text, "\n\n" );

	 }
	 else {

	      strcat ( text, filter[8] );
	      strcat ( text, "\n\n"    );
         }
    }

    if ( filter[9] != NULL ) { 

         cst_gtag ( "<OAC>", wdstr, " ", wdtxt1, iret );
	 strcat ( text, wdtxt1 );
         strcat ( text, ": " );

         if ( strcmp(filter[9], "USE_DEFAULT") == 0 ) {

              strcat ( text, vol->elem.vol.info.obsashcld );
              strcat ( text, "\n\n" );
	 }
	 else {

	      strcat ( text, filter[9] );
	      strcat ( text, "\n\n"    );
         }
    }

    if ( filter[10] != NULL ) { 

         cst_gtag ( "<F06>", wdstr, " ", wdtxt1, iret );
	 strcat ( text, wdtxt1 );
         strcat ( text, ": " );

         if ( strcmp(filter[10], "USE_DEFAULT") == 0 ) {

              strcat ( text, vol->elem.vol.info.fcst_06 );
              strcat ( text, "\n\n" );
	 }
	 else {

	      strcat ( text, filter[10] );
	      strcat ( text, "\n\n"    );
         }
    }

    if ( filter[11] != NULL ) { 

         cst_gtag ( "<F12>", wdstr, " ", wdtxt1, iret );
	 strcat ( text, wdtxt1 );
         strcat ( text, ": " );

         if ( strcmp(filter[11], "USE_DEFAULT") == 0 ) {

              strcat ( text, vol->elem.vol.info.fcst_12 );
              strcat ( text, "\n\n" );
	 }
	 else {

	      strcat ( text, filter[11] );
	      strcat ( text, "\n\n"    );
         }
    }

    if ( filter[12] != NULL ) { 

         cst_gtag ( "<F18>", wdstr, " ", wdtxt1, iret );
	 strcat ( text, wdtxt1 );
         strcat ( text, ": " );

         if ( strcmp(filter[12], "USE_DEFAULT") == 0 ) {

              strcat ( text, vol->elem.vol.info.fcst_18 );
              strcat ( text, "\n\n" );
	 }
	 else {

	      strcat ( text, filter[12] );
	      strcat ( text, "\n\n"    );
         }
    }

    if ( filter[13] != NULL ) { 

         cst_gtag ( "<RMK>", wdstr, " ", wdtxt1, iret );
	 strcat ( text, wdtxt1 );
         strcat ( text, ": " );

         if ( strcmp(filter[13], "USE_DEFAULT") == 0 ) {

              strcat ( text, vol->elem.vol.info.remarks );
              strcat ( text, " ...");
              strcat ( text, vol->elem.vol.info.fcstrs  );
              strcat ( text, "\n\n" );
	 }
	 else {

	      strcat ( text, filter[13] );
	      strcat ( text, "\n\n"    );
         }
    }

/*
 * Insert miscellaneous message here.
 */
    if ( _vaaMesgInfo != NULL ) {

         strcat ( text, _vaaMesgInfo );
         strcat ( text, "\n\n" );
    }

    if ( filter[14] != NULL ) { 

         cst_gtag ( "<TXT>", wdstr, " ", wdtxt1, iret );
	 strcat ( text, wdtxt1 );
         strcat ( text, ": " );

         if ( strcmp(filter[14], "USE_DEFAULT") == 0 ) {

              strcpy ( next_adv, vol->elem.vol.info.nextadv );

              if ( strstr (next_adv, "YYYYMMMDD/HHNNZ") != NULL ) {

	           adjust_min = 6*60;
                   ti_addm (tarry, &adjust_min, new_tarry, &ier);

	           adjust_min = new_tarry[4] % 15;
                   if ( adjust_min <= 7 ) {

                      ti_subm (new_tarry, &adjust_min, new_tarry, &ier);
                   }
                   else {

                      adjust_min = 15 - adjust_min;
                      ti_addm (new_tarry, &adjust_min, new_tarry, &ier);
                   }

                   sprintf (time_str, "%4d%02d%02d/%02d%02dZ", 
                            new_tarry[0], new_tarry[1], new_tarry[2], 
                            new_tarry[3],       new_tarry[4]   );
	           cst_rpst ( next_adv, "YYYYMMMDD/HHNNZ", time_str, next_adv, &ier);
              }

              strcat ( text, next_adv );
              /* strcat ( text, "\n\n" ); */
              strcat ( text, "\n" );
	 }
	 else {

	      strcat ( text, filter[14] );
              /* strcat ( text, "\n\n" ); */
              strcat ( text, "\n" );
         }
    }

    /*
     * Removed the NNNN and the preceding blank line at the request
     * of NESDIS. Left the code here until it is definitely the 
     * appropriate fix.
     */
    /* strcat ( text, "NNNN\n" ); */

/*
 *  Wrap SIGMET to maximum line_len.
 */
    cst_wrap ( text, blank, &line_len, NL, (char *)NULL, text, &ier );

/*
 * Convert lower case characters into upper case.
 */
    cst_lcuc (text, text, &ier);

/*
 * Generate file name from element info. and system time.
 */
    fname[0] = '\0';
    strcat ( fname, vol->elem.vol.info.name );
    strcat ( fname, "_" );
    sprintf( fname+strlen(fname), "%04d%02d%02d", 
             tarry[0], tarry[1], tarry[2] );
    strcat ( fname, "_" );  
    sprintf( fname+strlen(fname), "%02d%02d", tarry[3], tarry[4] ); 

    if ( vol->elem.vol.info.corr[0] != '\0' ) {

         strcat ( fname, "_" );
	 strcat ( fname, vol->elem.vol.info.corr ); 
    } 

    strcat ( fname, ".txt" );

    G_FREE ( wdstr, char );
}

/*=====================================================================*/

void pgvolw_createObs ( VG_DBStruct *ashcld, char *text, int *iret )
/************************************************************************
 * pgvolw_createObs                                                     *
 *                                                                      *
 * This function gets an observation from a given VAA ash cloud element.*
 *                                                                      *
 * void pgvolw_createObs ( ashcld, text, iret )                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      *ashcld         VG_DBStruct	VAA ash cloud element		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*text		char		ash cloud observation		*
 *      *iret           int             Return code                     *
 *					   0 - Normal			*
 *					  -1 - Invalid element		*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 8/03		Coding				*
 * H. Zeng/XTRIA	10/03           removed ASHCLD_ISOL		*
 * H. Zeng/XTRIA	11/03		added "FL" prefix		*
 * H. Zeng/XTRIA	11/03		modified to use spd str		*
 * H. Zeng/XTRIA	12/03		minor modification		*
 * H. Zeng/XTRIA	01/04		added more Ash Cloud types	*
 * H. Zeng/XTRIA	03/04           no moving info for forecasts	*
 * H. Zeng/SAIC		04/05		added more types		*
 * H. Zeng/SAIC		05/05		misc changes			*
 * H. Zeng/SAIC		05/05		manipulated FL prefix		*
 * D.W.Plummer/NCEP	07/05		Correct FL prefix and clo_from	*
 * D.W.Plummer/NCEP	01/08		Rm period from formatted text	*
 * D.W.Plummer/NCEP	07/08	Chg WID LINE BETWEEN to WID LINE BTN	*
 ***********************************************************************/
{
    float lat[MAX_ASH], lon[MAX_ASH];
    int vg_class, vg_type, subtype, npts;
    char tmpstr[12*MAX_ASH], fl_info[32], *ptext=NULL;
    int ii, jj, ier, is_SFC;
/*---------------------------------------------------------------------*/
    *iret = 0;

    vg_class = ashcld->hdr.vg_class;
    vg_type = ashcld->hdr.vg_type;
    subtype = ashcld->elem.ash.info.subtype;
    npts = ashcld->elem.ash.info.npts;

    if ( vg_class != CLASS_SIGMETS || vg_type != ASHCLD_ELM ) {
	*iret = -1;
	return;
    }

    for ( ii = 0, jj = npts; ii < npts; ii++, jj++ ) {
	lat[ii] = ashcld->elem.ash.latlon[ii];
	lon[ii] = ashcld->elem.ash.latlon[jj];
    }

    switch ( subtype ) {
	case ASHCLD_AREA:

	    text[0] = '\0';

	    is_SFC = G_FALSE;
	    if ( strcasecmp (ashcld->elem.ash.info.flvl1, "SFC") == 0 ) {

	      is_SFC = G_TRUE;
	      strcat (text, ashcld->elem.ash.info.flvl1);
	      strcat (text, "/");
            }
            else if ( (ashcld->elem.ash.info.flvl1[0] == 'F' ||
		       ashcld->elem.ash.info.flvl1[0] == 'f')   && 
                      (ashcld->elem.ash.info.flvl1[1] == 'L' ||
		       ashcld->elem.ash.info.flvl1[1] == 'l')      ) {

	      strcat (text, ashcld->elem.ash.info.flvl1);
	      strcat (text, "/");
	    }
	    else {

	      strcat (text, "FL");
	    strcat ( text, ashcld->elem.ash.info.flvl1 );
	    strcat ( text, "/" );
            }

	    if ( (ashcld->elem.ash.info.flvl2[0] == 'F' ||
		  ashcld->elem.ash.info.flvl2[0] == 'f')   && 
                 (ashcld->elem.ash.info.flvl2[1] == 'L' ||
		  ashcld->elem.ash.info.flvl2[1] == 'l')      ) {

	    strcat ( text, ashcld->elem.ash.info.flvl2 );
	    }
	    else {

	      if ( is_SFC == G_TRUE )  strcat (text, "FL");
	      strcat (text, ashcld->elem.ash.info.flvl2);
            }

	    strcat ( text, " " );
	    clo_from ( ASHCLD_ELM, subtype, npts, 0, lat, lon, 
		       sizeof(tmpstr), tmpstr, &ier ); 	
	    strcat ( text, tmpstr );
	    strcat ( text, " " );

            if ( ashcld->elem.ash.info.fhr == 0 ) {

	       strcat ( text, "MOV " );
	       strcat ( text, ashcld->elem.ash.info.dir );
	       strcat ( text, " " );
	       strcat ( text, ashcld->elem.ash.info.spds);
	       strcat ( text , "KT " );
	    }

	break;

	case ASHCLD_LINE:

	    text[0] = '\0';
	  
	    is_SFC = G_FALSE;
	    if ( strcasecmp (ashcld->elem.ash.info.flvl1, "SFC") == 0 ) {

	      is_SFC = G_TRUE;
	      strcat (text, ashcld->elem.ash.info.flvl1);
	      strcat (text, "/");
            }
            else if ( (ashcld->elem.ash.info.flvl1[0] == 'F' ||
		       ashcld->elem.ash.info.flvl1[0] == 'f')   && 
                      (ashcld->elem.ash.info.flvl1[1] == 'L' ||
		       ashcld->elem.ash.info.flvl1[1] == 'l')      ) {

	      strcat (text, ashcld->elem.ash.info.flvl1);
	      strcat (text, "/");
	    }
	    else {

	      strcat (text, "FL");
	    strcat ( text, ashcld->elem.ash.info.flvl1 );
	    strcat ( text, "/" );
            }

	    if ( (ashcld->elem.ash.info.flvl2[0] == 'F' ||
		  ashcld->elem.ash.info.flvl2[0] == 'f')   && 
                 (ashcld->elem.ash.info.flvl2[1] == 'L' ||
		  ashcld->elem.ash.info.flvl2[1] == 'l')      ) {

	    strcat ( text, ashcld->elem.ash.info.flvl2 );
	    }
	    else {

	      if ( is_SFC == G_TRUE )  strcat (text, "FL");
	      strcat (text, ashcld->elem.ash.info.flvl2);
            }

	    strcat ( text, " " );
	    sprintf ( &text[strlen(text)], "%.0f", 
 		      ashcld->elem.ash.info.distance );
	    strcat ( text, "NM WID LINE BTN " );
	    clo_from ( ASHCLD_ELM, subtype, npts, 0, lat, lon, 
		       sizeof(tmpstr), tmpstr, &ier ); 	
	    strcat ( text, tmpstr );
	    strcat ( text, ". " );

            if ( ashcld->elem.ash.info.fhr == 0 ) {

	       strcat ( text, "MOV " );
	       strcat ( text, ashcld->elem.ash.info.dir );
	       strcat ( text, " " );
	       strcat ( text, ashcld->elem.ash.info.spds);
	       strcat ( text , "KT " );
	    }

	break;

        case ASHCLD_NOTSEEN:

            ptext = strstr (ashcld->elem.ash.spt.text, "WINDS");
	    strcpy ( text, ptext+5 );
	break;

        default:

/*
 * subtypes other than the above.
 */
            strcpy (text, 
		    _otherFcstInfo[subtype-ASHCLD_OTHERS][2]);
	    
	    fl_info[0] = '\0';
	  
	    is_SFC = G_FALSE;
	    if ( strcasecmp (ashcld->elem.ash.info.flvl1, "SFC") == 0 ) {

	      is_SFC = G_TRUE;
	      strcat (fl_info, ashcld->elem.ash.info.flvl1);
	      strcat (fl_info, "/");
	    }
            else if ( (ashcld->elem.ash.info.flvl1[0] == 'F' ||
		       ashcld->elem.ash.info.flvl1[0] == 'f')   && 
                      (ashcld->elem.ash.info.flvl1[1] == 'L' ||
		       ashcld->elem.ash.info.flvl1[1] == 'l')      ) {

	      strcat (fl_info, ashcld->elem.ash.info.flvl1);
	      strcat (fl_info, "/");
	    }
	    else {

	      strcat (fl_info, "FL");
	      strcat (fl_info, ashcld->elem.ash.info.flvl1);
	      strcat (fl_info, "/");
            }

	    if ( (ashcld->elem.ash.info.flvl2[0] == 'F' ||
		  ashcld->elem.ash.info.flvl2[0] == 'f')   && 
                 (ashcld->elem.ash.info.flvl2[1] == 'L' ||
		  ashcld->elem.ash.info.flvl2[1] == 'l')      ) {

	      strcat (fl_info, ashcld->elem.ash.info.flvl2);
	    }
	    else {

	      if ( is_SFC == G_TRUE )  strcat (fl_info, "FL");
	      strcat (fl_info, ashcld->elem.ash.info.flvl2);
            }


	    while ( strstr(text, "{FL}") != NULL ) {

	      cst_rpst (text, "{FL}", fl_info, text, &ier);
            }

	break;
    }
}

/*=====================================================================*/

Widget pgvolw_createMenuText ( Widget parent, char *labelstr, int ncol, 
			       int textoff, int info_type, 
			       Widget *textwid, Widget *btnwid )
/************************************************************************
 * pgvolw_createMenuText						*
 *									*
 * Creates a labeled text field widget with a pulldown menu.		*
 *									*
 * Widget pgvolw_createMenuText ( parent, labelstr, ncol, textoff, 	*
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
 * pgvolw_createMenuText Widget	Widget ID of the form widget 		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
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
    label = XtVaCreateManagedWidget ("tmlabel",
			       xmLabelWidgetClass,	form,
			       XmNtopAttachment,	XmATTACH_FORM,
			       XmNtopOffset,		toff,
			       NULL);
    NxmLabel_setStr(label, labelstr);

/*
 * create text field 
 */
    *textwid = XtVaCreateManagedWidget ("tmtext", 
					xmTextFieldWidgetClass, form,
					XmNcolumns,		ncol,
                                        XmNleftAttachment,      XmATTACH_FORM,
                                        XmNuserData,            info_type,
					NULL);

    XtVaSetValues(*textwid,
                  XmNleftAttachment,    XmATTACH_WIDGET,
                  XmNleftWidget,        label,
		  XmNleftOffset,	textoff,
                  NULL  );
   
    ii = TEXT_FIELD ;
    XtAddCallback (*textwid, XmNvalueChangedCallback,
		(XtCallbackProc) pgvolw_menuTextCb,
		(XtPointer) ii);
 
/*
 * For volcano name menu, get menu bar directly from NxmVolcano.c
 */
    if ( info_type == VOLC ) {

	 menub = NxmVolcano_menuCreate(form, *textwid, 
					(XtCallbackProc)pgvolw_volcMenuCb);
         XtManageChild(form);
         return (form);
    }

/*
 * create non-volcano-name menu.
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

/*
 * Create pulldown menu buttons.
 */
    if ( info_type == WMID || info_type == HDRN ) {

       for (ii = 0; ii < MXELE; ii++) {
	   xmstr = XmStringCreateLocalized ("xxxx");
	   button = XtVaCreateWidget ("tmbutton", 
				      xmPushButtonWidgetClass, menu, 
				      XmNlabelString,	       xmstr,
				      XmNuserData,	       *textwid,
				      NULL);
           if ( btnwid != NULL ) {
              *(btnwid+ii) = button;
           }

	   XmStringFree (xmstr);

	   XtAddCallback (button, XmNactivateCallback,
		          (XtCallbackProc) pgvolw_menuTextCb,
		          (XtPointer) ii);
	
       }
    }   
    else { 

       for (ii = 0; ii < _vlInfo[info_type].nelems; ii++) {
	   xmstr = XmStringCreateLocalized (_vlInfo[info_type].range[ii]);
	   button = XtVaCreateManagedWidget ("tmbutton", 
					  xmPushButtonWidgetClass, menu, 
					  XmNlabelString,	   xmstr,
					  XmNuserData,		   *textwid,
					  NULL);
           if ( btnwid != NULL ) {
              *(btnwid+ii) = button;
           }

	   XmStringFree (xmstr);

	   XtAddCallback (button, XmNactivateCallback,
		          (XtCallbackProc) pgvolw_menuTextCb,
		          (XtPointer) ii);
	
       }
    }   

    XtManageChild(menub);
    XtManageChild(form);
    return (form);
}

/*=====================================================================*/

char pgvolw_getGrptyp ( void ) 
/************************************************************************
 * pgvolw_getGrptyp                                                     *
 *                                                                      *
 * returns the current group type 					*
 *                                                                      *
 * char pgvolw_getGrptyp( )      					*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 *			NONE						*
 * Return:								*
 *	pgvolw_getGrptyp    char	current group type		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  H. Zeng/XTRIA	07/03	initial coding				*
 ***********************************************************************/
{
char	grptyp;
/*---------------------------------------------------------------------*/

    grptyp = (char)(0);
    return ( grptyp );
}

/*=====================================================================*/

void pgvolw_getAttr ( VG_DBStruct *el )
/************************************************************************
 * pgvolw_getAttr							*
 *									*
 * This routine gets the current volcano element attributes.		*
 *									*
 * void pgvolw_getAttr ( el )						*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	current element			*
 *									*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	07/03	initial coding				*
 * H. Zeng/XTRIA	08/03   modified for new fields			*
 * H. Zeng/XTRIA	10/03   obtained obs info. from ash clouds	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * H. Zeng/XTRIA	11/03   added additional info source		*
 * H. Zeng/XTRIA	02/04	added call to pgvolw_getAshInfo		*
 ***********************************************************************/
{
    char   vol_elev[24], vaac_str[64], *ptext=NULL, *ptr=NULL;
    char   ash_info[1024];
    int	   ier;
/*---------------------------------------------------------------------*/

    if ( pgvolw_isUp() ) {

         strcpy ( el->elem.vol.info.name,       _volName );
         strcpy ( el->elem.vol.info.number,     _volNum	 );
         strcpy ( el->elem.vol.info.location,   _volLoc	 );
         strcpy ( el->elem.vol.info.area,       _volArea );
         sprintf( vol_elev, "%-7.0f", _volElev  );
         cst_ncpy ( el->elem.vol.info.elev, vol_elev, 8, &ier );

	 strcpy ( vaac_str, _vlInfo[VAAC].range[0] );
         ptr = strtok (vaac_str, "/");
	 strcpy(el->elem.vol.info.origstn, ptr);
         ptr = strtok (NULL, "/");
	 strcpy(el->elem.vol.info.vaac, ptr);

         strcpy ( el->elem.vol.info.hdrnum, _hdrRange[0].range[0] );
         strcpy ( el->elem.vol.info.wmoid,  _wmoRange[0].range[0] );

         strcpy ( el->elem.vol.info.year,          "\0" );
         strcpy ( el->elem.vol.info.advnum,        "\0" );
         strcpy ( el->elem.vol.info.corr,          "\0" );
         strcpy ( el->elem.vol.info.infosorc,      "\0" );
         strcpy ( el->elem.vol.info.addlsorc,      "\0" );
         strcpy ( el->elem.vol.info.avcc,	   "\0" );
         strcpy ( el->elem.vol.info.details,       "\0" );
         strcpy ( el->elem.vol.info.obsdate,       "\0" );
         strcpy ( el->elem.vol.info.obstime,       "\0" );

	 pgvolw_getAshInfo ( 0, ash_info );
	 strcpy (el->elem.vol.info.obsashcld, ash_info);

	 pgvolw_getAshInfo ( 6, ash_info );
         strcpy ( el->elem.vol.info.fcst_06,  ash_info);

	 pgvolw_getAshInfo (12, ash_info );
         strcpy ( el->elem.vol.info.fcst_12,  ash_info);

	 pgvolw_getAshInfo (18, ash_info );
         strcpy ( el->elem.vol.info.fcst_18,  ash_info);

         strcpy ( el->elem.vol.info.remarks,       "\0" );
         strcpy ( el->elem.vol.info.nextadv,       "\0" );
         strcpy ( el->elem.vol.info.fcstrs,        "\0" );

    }
    else if ( pgvolw_editIsUp() ) {

         XtVaGetValues (_vaacTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

            ptr = strtok (ptext, "/");
	    if (ptr!=NULL) strcpy(el->elem.vol.info.origstn, ptr);
            ptr = strtok (NULL, "/");
	    if (ptr!=NULL) strcpy(el->elem.vol.info.vaac, ptr);
         }
	 else {

	    strcpy(el->elem.vol.info.origstn, "\0");
	    strcpy(el->elem.vol.info.vaac, "\0");
         }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_wmoTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.wmoid, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.wmoid, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_hdrTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.hdrnum, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.hdrnum, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_yearTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.year, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.year, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_adnmTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.advnum, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.advnum, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_correctTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.corr, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.corr, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_infoTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.infosorc, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.infosorc, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_addlTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.addlsorc, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.addlsorc, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_aviaTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.avcc, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.avcc, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_erupTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.details, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.details, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_obdateTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.obsdate, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.obsdate, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_obtimeTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.obstime, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.obstime, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_obcloudTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.obsashcld, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.obsashcld, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_facdTxt06W, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.fcst_06, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.fcst_06, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_facdTxt12W, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.fcst_12, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.fcst_12, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_facdTxt18W, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.fcst_18, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.fcst_18, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_remkTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.remarks, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.remarks, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_nxadTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.nextadv, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.nextadv, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);


         XtVaGetValues (_fcstTxtW, XmNvalue, &ptext,  NULL);      
         if ( ptext != NULL && ptext[0] != '\0' ) {

	    strcpy(el->elem.vol.info.fcstrs, ptext);
         }
	 else {

	    strcpy(el->elem.vol.info.fcstrs, "\0");
	 }
         if ( ptext != NULL ) XtFree(ptext);
    }
}

/*=====================================================================*/

void pgvolw_getAshInfo ( int fcst_hr, char* ash_info )
/************************************************************************
 * pgvolw_getAshInfo							*
 *									*
 * This routine gets the ash cloud info of specified forecast hour.	*
 *									*
 * void pgvolw_getAshInfo ( fcst_hr, ash_info )				*
 *									*
 * Input parameters:							*
 *	fcst_hr		int		forecast hour			*
 *									*
 * Output parameters:							*
 *	ash_info	char*		ash cloud info			*
 *									*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	02/04	initial coding				*
 * H. Zeng/XTRIA	03/04   allowed multiple NOT_SEEN types		*
 * H. Zeng/SAIC		07/04	removed NOTAVBL&ENDVAA			*
 * H. Zeng/SAIC		04/05	added more types			*
 * H. Zeng/SAIC		05/05	ash_type re-grouping			*
 * D.W.Plummer/NCEP     01/08   Formatting changes per Annex 3          *
 ***********************************************************************/
{
    char      ash_text[1024], vg_class, vg_type;
    int	      el_num, el_loc, ier, ash_type;
    VG_DBStruct   ash_el;
/*---------------------------------------------------------------------*/
/*
 * Get Ash Cloud info. from surrounding ash cloud elements.
 * First initialize ash_info string and ash_type value. For 
 * ash_type, possible values are 0 for no ash cloud element, 
 * 1 for areas, lines and all other-FCST types, 2 for not-seen 
 * type only.
 */
    strcpy (ash_info, "\0");
    ash_type = 0;

    for (el_num = 0; el_num < MAX_EDITABLE_ELEMS; el_num++) {
	crg_goffset (el_num, &el_loc, &ier);

/*
 * Skip cleared range record
 */
	if ( el_loc == -1 ) {
             continue;
        }

	crg_gtyp( el_num, &vg_class, &vg_type, &ier );

	if ( vg_class == CLASS_SIGMETS && vg_type == ASHCLD_ELM ) {

	     cvg_rdrec( cvg_getworkfile(), el_loc, &ash_el, &ier );

/*
 * Make sure this ash cloud is of the correct forecast
 * hour. Otherwise skip it and go to the next one.
 */
	     if ( ash_el.elem.ash.info.fhr != fcst_hr ) {

		  continue;
	     }

/*
 * Determine the ash_type value based on the first elligible
 * ash cloud element. once it is set, ash clouds of other 
 * types will be excluded from being read into ash_info string.
 */
	     if ( ash_type == 0 && 
		 (ash_el.elem.ash.info.subtype == ASHCLD_AREA || 
		  ash_el.elem.ash.info.subtype == ASHCLD_LINE ||
		  ash_el.elem.ash.info.subtype >= ASHCLD_OTHERS)  ) {

		  ash_type = 1;
             }
	     else if ( ash_type == 0 && 
		       ash_el.elem.ash.info.subtype == ASHCLD_NOTSEEN ) {

		  strcat (ash_info, "VA NOT IDENTIFIABLE FROM ");
                  strcat (ash_info, "SATELLITE DATA WINDS");
		  ash_type = 2;
	     }

/*
 * only get ash info from the ash cloud element when 
 * the element is of the right type.
 */
	     if ( ash_type == 1 && 
		 (ash_el.elem.ash.info.subtype == ASHCLD_AREA || 
		  ash_el.elem.ash.info.subtype == ASHCLD_LINE ||
                  ash_el.elem.ash.info.subtype >= ASHCLD_OTHERS)  ) {

	          pgvolw_createObs( &ash_el, ash_text, &ier);

	          if ( strlen(ash_info) + strlen(ash_text) < (size_t)1022 ) {

		       strcat (ash_info, ash_text);
                  }
             }
	     else if ( ash_type == 2 && 
		       ash_el.elem.ash.info.subtype == ASHCLD_NOTSEEN ) {

	          pgvolw_createObs( &ash_el, ash_text, &ier);

	          if ( strlen(ash_info) + strlen(ash_text) < (size_t)1022 ) {

		       strcat (ash_info, ash_text);
                  }
	     }
	} /* the end of if ( vg_class == CLASS_SIGMETS... */
    } /* the end of for (el_num */
}

/*=====================================================================*/

void pgvolw_attrSave ( Boolean do_prod )
/************************************************************************
 * pgvolw_attrSave							*
 *									*
 * This function saves the attributes setting on volcano edit GUI to    *
 * current volcano element and depending on the flag, generate VAA      *
 * text product and file name.						*
 *									*
 * void pgvolw_attrSave( do_prod )				        *
 *									*
 * Input   parameters:							*
 *    do_prod	   Boolean	whether to generate product or not	*
 * Output  parameters:							*
 * Return  parameters:							*
 *		   NONE				                        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	08/03   initial coding				*
 * H. Zeng/XTRIA	02/04   added case for special product		*
 * H. Zeng/SAIC		05/05	added call to pgvolw_editPopup		*
 * H. Zeng/SAIC		06/05	removed call to pgvolw_editPopup	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		location, np, ier; 
    char	text[4096], fname[128];
    float	llx, lly, urx, ury, *x_coords, *y_coords;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    if ( XtIsSensitive (_loc1Strc.form) == TRUE) {

/*
 * There is a selected volcano element on the map.
 */
       location = pgactv_getElmLoc();

       pgactv_getDevPts (&np, &x_coords, &y_coords);
       pgutls_prepNew (location, &el, &llx, &lly, &urx, &ury, &ier);

       pgundo_newStep();
       pgundo_storeThisLoc(location, UNDO_DEL, &ier);

       pgvolw_getAttr ( &el );	
       pgvgf_saveNewElm (NULL, sys_D, &el, np, 
				  x_coords, y_coords, TRUE, &location, &ier); 

       pgundo_storeThisLoc(location, UNDO_ADD, &ier);
       pgundo_endStep();

       pgutls_redraw (location, &el, &ier);
    } 
    else {

/*
 * This is a special product.
 */
       pgvolw_getAttr ( &_spProdEl );
    }  
    
/* 
 * Generate VAA text product and file name if flag is set. 
 */
    if ( do_prod ) {

       if ( XtIsSensitive (_loc1Strc.form) == TRUE) {

          pgvolw_createProd ( &el, _loc1Info[_loc1Strc.current],
			      text, fname, &ier );
       }
       else {

          pgvolw_createProd ( &_spProdEl, _loc2Info[_loc2Strc.current],
			      text, fname, &ier );
       }

       XmTextSetString( _prodTxtW,  text );
       XmTextSetString( _fnameTxtW, fname);
    }
}

/*=====================================================================*/

void pgvolw_setVolLys ( int rec_loc )
/************************************************************************
 * pgvolw_setVolLys							*
 *									*
 * Creates the lpf file and set the current volcano in layers.		*
 *									*
 * void pgvolw_setVolLys (rec_loc)					*
 *									*
 * Input parameters:							*
 *	rec_loc		int	record location in the target vgf file	*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	02/04	initial coding				*
 * H. Zeng/XTRIA	02/04	allowed directory for vgf files		*
 * S. Danz/AWC		07/06	Update to new cvg_writef() parameter    *
 * S. Gilbert/SIB   04/14   Added some padding to size of lpf_str   *
 ***********************************************************************/
{ 
    int		ignore, lyr_idx, length, ier, start=-1, extra=1000;
    char	vgf_nm[256], lpf_nm[256], tag[25], *lpf_str; 
    long	lpf_size;
    FILE	*infp, *outfp;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    cfl_inqr (LPF_NM, LPF_DIR, &lpf_size, lpf_nm, &ier);
/*
 * read from template lpf file.
 */
    infp = fopen (lpf_nm, "r");
    if (infp == (FILE *) NULL) {
        fprintf (stderr, "Failed to read template lpf file!\n");
	return;
    }
    G_CALLOC(lpf_str, char, lpf_size+extra, "pgvolw_setVolLys:  lpf_str");
    cfl_read (infp, (int)lpf_size, (unsigned char*)lpf_str, &ignore, &ier);
    fclose   (infp);

    while ( strstr (lpf_str, "VOLCANO_NAME_HERE") != NULL ) {
	cst_rpst (lpf_str, "VOLCANO_NAME_HERE", _volName, lpf_str, &ier);
    }
/*
 * create local lpf file.
 */
    strcpy (lpf_nm, _lpfPath);
    strcat (lpf_nm, _volName);
    strcat (lpf_nm, ".lpf"  );

    outfp = fopen (lpf_nm, "w");
    if (outfp == (FILE *) NULL) {

        fprintf (stderr, "Failed to create lpf file!\n");
	G_FREE(lpf_str, char);
	return;
    }
    fputs (lpf_str, outfp);
    fclose (outfp);
/*
 * create vgf files specified in local lpf file.
 */
    for ( lyr_idx = 0; lyr_idx < 5; lyr_idx++ ) {

        sprintf  (tag, "layer%1d_file", lyr_idx+1);
        cst_gtag (tag, lpf_str, "\0", vgf_nm, &ier );

        if ( ier != 0 )  continue;

	cst_rxbl (vgf_nm, vgf_nm, &length, &ier);
        cvg_crvgf (vgf_nm, &ier);

        if ( lyr_idx == 0 ) {
/*
 * First layer vgf file contains the volcano element.
 */
           cvg_rdrec (cvg_getworkfile(), rec_loc, &el, &ier);
           cvg_writef( &el, start, el.hdr.recsz, vgf_nm, FALSE, &rec_loc, &ier);
        }

    }
    G_FREE(lpf_str, char);

/*
 * open local lpf file.
 */
    if ( pglayrw_isUp () ) {

	 pglayrw_exit ();
    }

    pglpfw_setPath ("\0");
    pglpfw_setName (lpf_nm);
	    
    pglpfw_loadLPF (&ier);

    pgundo_initUndo ();
}

/*=====================================================================*/

void pgvolw_iniTxtInfo ( void )
/************************************************************************
 * pgvolw_iniTxtInfo							*
 *									*
 * This function initializes _vaaTxtInfo and _vaaTxtInfoNum variables.  *
 *									*
 * void pgvolw_iniTxtInfo ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	02/04   initial coding				*
 ***********************************************************************/
{
    int	       nn, ii;
/*---------------------------------------------------------------------*/
   
    _vaaTxtInfoNum = 15;

    nn = _vaaTxtInfoNum;
    _vaaTxtInfo = (char**) malloc ( nn * sizeof(char*) );

    for ( ii = 0; ii < _vaaTxtInfoNum; ii++ ) {

        _vaaTxtInfo[ii] = (char*) malloc ( 25 * sizeof(char) );
    }

    strcpy (_vaaTxtInfo[0],  "VOLCANO");
    strcpy (_vaaTxtInfo[1],  "NUMBER" );
    strcpy (_vaaTxtInfo[2],  "LOCATION");
    strcpy (_vaaTxtInfo[3],  "AREA" );
    strcpy (_vaaTxtInfo[4],  "SUMMIT ELEVATION");
    strcpy (_vaaTxtInfo[5],  "ADVISORY NUMBER" );
    strcpy (_vaaTxtInfo[6],  "INFORMATION SOURCE");
    strcpy (_vaaTxtInfo[7],  "ERUPTION DETAILS" );
    strcpy (_vaaTxtInfo[8],  "OBS ASH DATE/TIME");
    strcpy (_vaaTxtInfo[9],  "OBS ASH CLOUD" );
    strcpy (_vaaTxtInfo[10], "FCST ASH CLOUD +6H");
    strcpy (_vaaTxtInfo[11], "FCST ASH CLOUD +12H" );
    strcpy (_vaaTxtInfo[12], "FCST ASH CLOUD +18H");
    strcpy (_vaaTxtInfo[13], "REMARKS" );
    strcpy (_vaaTxtInfo[14], "NEXT ADVISORY");
}

/*=====================================================================*/

void pgvolw_getTxtInfo ( char** loc, char* buf )
/************************************************************************
 * pgvolw_getTxtInfo							*
 *									*
 * This function gets vaa text product info from buffer.		*
 *									*
 * void pgvolw_getTxtInfo ( loc, buf )					*
 *									*
 * Input parameters:							*
 *	loc	char**	location that info will be set			*
 *	buf	char*	buffer that holds the info			*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	02/04   initial coding				*
 ***********************************************************************/
{
    char	*ptr1=NULL, *ptr2=NULL, tag[50];
    int	        index, ii;
/*---------------------------------------------------------------------*/
   
    if ( buf == NULL || buf[0] == '\0' )  return;

    ptr1 = strchr (buf, '<');
    if ( ptr1 == NULL ) return;

    ptr2 = strchr (buf, '>');
    if ( ptr2 == NULL ) return;

    if ( (ptr2 - ptr1) <= 1 ) return;

    memcpy ( (void*)tag, (void*)(ptr1+1), (size_t)(ptr2-ptr1-1) );
    tag[ptr2-ptr1-1] = '\0';

    index = -99;
    for ( ii = 0; ii < _vaaTxtInfoNum; ii++ ) {

        if ( strcasecmp(tag, _vaaTxtInfo[ii]) == 0 ) {

	     index = ii;
	     break;
        }
    }

    if ( index == -99 && strcasecmp(tag, "ALL") == 0 ) {

	 index = 99;
    }

    if ( index == -99 ) return;

/*
 * Get info from buffer and store it to the location 
 * where "loc" points to.
 */
    if ( index == 99 ) {

       for ( ii = 0; ii < _vaaTxtInfoNum; ii++ ) {

	   if ( loc[ii] != NULL ) continue;

           loc[ii] = (char*) malloc( (strlen("USE_DEFAULT")+2) * sizeof(char) );
	   strcpy ( loc[ii], "USE_DEFAULT");
       }
    }
    else if ( (ptr2+1)[0] != '\0' ) {

           if ( loc[index] != NULL ) free (loc[index]);

           loc[index] = (char*) malloc( (strlen(ptr2+1)+2) * sizeof(char) );
	   strcpy ( loc[ii], (ptr2+1) );
    }
    else if ( (ptr2+1)[0] == '\0' ) {

           if ( loc[index] == NULL ) {

                loc[index] = (char*) malloc( (strlen("USE_DEFAULT")+2) * sizeof(char) );
	        strcpy ( loc[ii], "USE_DEFAULT");
           }
    }
}

/*=====================================================================*/

void pgvolw_getMesgInfo ( char* buf )
/************************************************************************
 * pgvolw_getMesgInfo							*
 *									*
 * This function gets vaa mics message info from buffer.		*
 *									*
 * void pgvolw_getMesgInfo ( buf )					*
 *									*
 * Input parameters:							*
 *	buf	char*	buffer that holds the info			*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC 	06/04   initial coding				*
 ***********************************************************************/
{
    if ( buf == NULL || buf[0] == '\0' )  return;

    _vaaMesgInfo = (char*) malloc( (strlen(buf)+2) * sizeof(char) );
    strcpy ( _vaaMesgInfo, buf );
}

/*=====================================================================*/

char*** pgvolw_getFcstInfo ( void )
/************************************************************************
 * pgvolw_getFcstInfo							*
 *									*
 * This function gets the pointer to "Other-FCST" info.			*
 *									*
 * char*** pgvolw_getFcstInfo ()   					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 *	char***		pgvolw_getFcstInfo	pointer to info		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC 	04/05   initial coding				*
 ***********************************************************************/
{
    return ( _otherFcstInfo );
}

/*=====================================================================*/

int pgvolw_getFcstNum ( void )
/************************************************************************
 * pgvolw_getFcstNum							*
 *									*
 * This function gets the total number of "Other-FCST" info.		*
 *									*
 * int pgvolw_getFcstNum ()	 					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return parameters:							*
 *	int		pgvolw_getFcstNum	total number		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC 	04/05   initial coding				*
 ***********************************************************************/
{
    return ( _otherFcstNum );
}

/*=====================================================================*/

void pgvolw_getProdInfo ( char* volnm, int* latest_y, int* latest_n, 
			  int* iret )
/************************************************************************
 * pgvolw_getProdInfo							*
 *									*
 * This function gets the year and advisory number info from the latest	*
 * VAA text product of a particular volcano.				*
 *									*
 * void pgvolw_getProdInfo ()	 					*
 *									*
 * Input parameters:							*
 *	char*		volnm			volcano name		*
 * Output parameters:							*
 *	int*		latest_y		year info		*
 *	int*		latest_n		advisory number info	*
 *	int*		iret			Return code		*
 *						 0 -- Normal		*
 *						-1 -- File not found	*
 *						-2 -- File not read	*
 *						-3 -- Tag name not found*
 *						-4 -- invalid year or   *
 *						      adv. no. info	*
 *						-5 -- year & adv. no    *
 *						      line no found	*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * H. Zeng/SAIC 	04/06   initial coding				*
 * T. Piper/SAIC	10/07	cleaned up dnamelist freeing		*
 ***********************************************************************/
{
   FILE *fp;
   double date_time, latest_t;
   char *ptr, *ptr2, *wdstr, file_nm[128], compo[4][64], latest_f[128];
   char tagstr[128], buff[256], adv_st[32], year_st[32], file_nm_r[128];
   char substr[64];
   int	num_file, nsdir, ii, jj, nchar, ier, ier2;
   unsigned long date, time;
   struct dirent   **dnamelist=NULL;
/*---------------------------------------------------------------------*/
   
    num_file = cfl_rdir(2, _txtPath, ".txt", &dnamelist, &nsdir);
  
    latest_t = 0.0;
    latest_f[0] = '\0';

    for ( ii = 0; ii < num_file; ii++ ) {

/*
 * Copy the file name.
 */
	if ( dnamelist[ii]->d_name == NULL || 
	     dnamelist[ii]->d_name[0] == '\0' )  continue;

	strcpy (file_nm, dnamelist[ii]->d_name);

/*
 * Truncate the file name extension.
 */
	ptr = strstr ( file_nm, ".txt" );
	if ( ptr != NULL )  ptr[0] = '\0';  

/*
 * Reverse the file name character array.
 */
	nchar = strlen (file_nm);
	for ( jj = 0; jj < nchar; jj++ ) {
	    file_nm_r[jj] = file_nm[nchar-jj-1];
	}
	file_nm_r[nchar] = '\0';

/*
 * Get the four components before the file name extension
 * starting from the 4th back to the 1st.
 */
	substr[0] = '\0';
	for ( jj=0; jj<4; jj++ )  compo[jj][0] = '\0';
	ptr = cst_split ( file_nm_r, '_', sizeof(substr), substr, &ier );
	nchar = strlen ( substr );
	for ( jj = 0; jj < nchar; jj++ ) compo[3][jj] = substr[nchar-jj-1];
	compo[3][nchar] = '\0';

	if ( strlen(compo[3]) == 1 && isalpha(compo[3][0]) != 0 ) {

	    if ( ptr == NULL )  continue;

	    ptr = cst_split ( ptr, '_', sizeof(substr), substr, &ier );
	    nchar = strlen ( substr );
	    for ( jj = 0; jj < nchar; jj++ ) compo[2][jj] = substr[nchar-jj-1];
	    compo[2][nchar] = '\0';
	    if ( sscanf(compo[2], "%lu", &time) != 1 )  continue;

	}
	else {
	    strcpy ( compo[2], compo[3] );
	    if ( sscanf(compo[2], "%lu", &time) != 1 )  continue;

	}

	if ( ptr == NULL )  continue;

	ptr = cst_split ( ptr, '_', sizeof(substr), substr, &ier );
	nchar = strlen ( substr );
	for ( jj = 0; jj < nchar; jj++ ) compo[1][jj] = substr[nchar-jj-1];
	compo[1][nchar] = '\0';
	if ( sscanf(compo[1], "%lu", &date) != 1 )  continue;

	if ( ptr == NULL )  continue;

	strcpy ( substr, ptr );
	nchar = strlen ( substr );
	for ( jj = 0; jj < nchar; jj++ ) compo[0][jj] = substr[nchar-jj-1];
	compo[0][nchar] = '\0';  
	if ( strcasecmp(volnm, compo[0]) != 0 )  continue;

/*
 * Create a long integer that represents date&time info.
 */
	date_time = (double)date + (double)time * 0.0001;

	if ( date_time > latest_t ) {
	    latest_t = date_time;
	    strcpy ( latest_f, dnamelist[ii]->d_name );
	}
	free (  dnamelist[ii] );
    } /* the end of for ( ii = 0; ... */
    if ( dnamelist != NULL ) free ( dnamelist );

/*
 * If there is no file, return an error.
 */
    if ( latest_f[0] == '\0' ) {
	*iret = -1;
	return;
    }

/*
 *  Open the file in latest_f, if fails, return an error.
 */
    fp = cfl_ropn(latest_f, _txtPath, &ier);
    if ( fp == NULL  ||  ier != 0 )  {
	*iret = -2;
	return;
    }

/*
 * Get the tag str associated with the year&advisory no.
 */
   pgvolw_rdWords ( PREF_TBL, VAA_TBL, &wdstr, &ier );

    if ( ier != 0 ) {
	*iret = -3;
	cfl_clos(fp, &ier2);
	G_FREE ( wdstr, char );
	return;
    }

    cst_gtag ( "<ADV>", wdstr, "\0", tagstr, &ier );
    if ( ier != 0 || tagstr[0] == '\0' ) {
	*iret = -3;
	cfl_clos(fp, &ier2);
	G_FREE ( wdstr, char );
	return;
    }

/*
 * Get the year and advisory number info in the file.
 */
    rewind (fp);
    cfl_trln (fp, sizeof(buff), buff, &ier);

    while ( !feof(fp) ) {
	ptr = strstr ( buff, tagstr );
	if ( ptr != NULL ) {
	    ptr  = ptr + strlen (tagstr);
	    ptr2 = strchr (ptr, '/');

	    if ( ptr2 == NULL ) {
		*iret = -4;
		cfl_clos(fp, &ier2);
		G_FREE ( wdstr, char );
		return;
	    }

/*
 * Get year & advisory no from before & after '/'.
 */
	    adv_st[0]  = '\0';
	    year_st[0] = '\0';

	    ii = 1;
	    while ( (ptr2+ii) != NULL && ptr2[ii] != ' ' )  ii++;
	    if ( ii < 32 ) {
		strncpy (adv_st, ptr2+1, ii-1);
		adv_st[ii-1] = '\0';
	    }

	    ii = 1;
	    while ( (ptr2-ii) != NULL && (ptr2-ii)[0] != ' ' )  ii++;
	    if ( ii < 32 ) {
		strncpy (year_st, ptr2-ii+1, ii-1);
		year_st[ii-1] = '\0';
	    }
        
/*
 * Write into latest_y&latest_n, if failed, return an error.
 */
	    if ( sscanf( adv_st, "%d", latest_n) != 1 || 
		 sscanf(year_st, "%d", latest_y) != 1)   {
		*iret = -4;
		cfl_clos(fp, &ier2);
		G_FREE ( wdstr, char );
		return;
	    }
	    else {
		*iret = 0;
		cfl_clos(fp, &ier2);
		G_FREE ( wdstr, char );
		return;
	    }
 
	} /* the end of if ( ptr != NULL ) ... */

	cfl_trln(fp, sizeof(buff), buff, &ier);

    } /* while ( !feof(fp) ) ... */

    *iret = -5;
    cfl_clos(fp, &ier2);
    G_FREE ( wdstr, char );
}

/*=====================================================================*/

void pgvolw_insertObsTime ( void )
/************************************************************************
 * pgvolw_insertObsTime							*
 *									*
 * This function updates the OBS date/time info. on VAA Ash Cloud info  *
 * window.							        *
 *									*
 * void pgvolw_insertObsTime()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	06/05	initial coding				*
 ***********************************************************************/
{
    char      dat_tim[DTTMSZ], utc_time[DTTMSZ];
    char      time_str[DTTMSZ], insert_str[16], text_str[16];
    char      obsdate[16], obstime[16], *ptext=NULL;
    int	      time_arry[5], adjust_min, min, result;
    int	      ier, ier2, ier3, itype=1;
/*---------------------------------------------------------------------*/
/*
 * Get obs ash date/time info from GUI.
 */
    XtVaGetValues (_obdateTxtW, XmNvalue, &ptext,  NULL);      
    if ( ptext != NULL && ptext[0] != '\0' ) {

       strcpy(obsdate, ptext);
    }
    else {

       strcpy(obsdate, "\0");
    }
    if ( ptext != NULL ) XtFree(ptext);


    XtVaGetValues (_obtimeTxtW, XmNvalue, &ptext,  NULL);      
    if ( ptext != NULL && ptext[0] != '\0' ) {

       strcpy(obstime, ptext);
    }
    else {

       strcpy(obstime, "\0");
    }
    if ( ptext != NULL ) XtFree(ptext);

/*
 * Change obs ash date/time in correct gempak format.
 */
    strcpy ( time_str, obsdate );
    strcat ( time_str, "/" );
    strcat ( time_str, obstime );

    css_gtim (&itype, utc_time, &ier);
    ti_stan  (time_str, utc_time, dat_tim, &ier, strlen(time_str), 
              strlen(utc_time), (DTTMSZ-1) );
    dat_tim[11] = '\0';

    if ( ier != 0 ) {

      dat_tim[0] = '\0';
    }
    else {

      ti_ctoi (dat_tim, time_arry, &ier2, strlen(dat_tim) );

      if ( ier2 != 0 ) {

        dat_tim[0] = '\0';
      }
      else {

	min = time_arry[4] % 30;

	if ( min >= 15 ) {

	  adjust_min = 30 - min;
	  ti_addm (time_arry, &adjust_min, time_arry, &ier3);
          ti_itoc (time_arry, dat_tim, &ier3, sizeof(dat_tim) );
          dat_tim[11] = '\0';
	}
        else {

	  adjust_min = min;
	  ti_subm (time_arry, &adjust_min, time_arry, &ier3);
          ti_itoc (time_arry, dat_tim, &ier3, sizeof(dat_tim) );
          dat_tim[11] = '\0';
        }
      }
    }

/*
 * Return if dat_tim str doesn't have anything.
 */
    if ( dat_tim[0] == '\0' ) return;

/*
 * Insert the obs date/time info. into "Fcst Ash Cloud" text boxes.
 */
    adjust_min = 6 * 60;
    ti_addm (time_arry, &adjust_min, time_arry, &ier);
    ti_itoc (time_arry, dat_tim, &ier, sizeof(dat_tim) );
    dat_tim[11] = '\0';

    memset ( text_str, '\0', 16);
    result = XmTextGetSubstring (_facdTxt06W, (XmTextPosition)0, 
				 8, 15, text_str);

    if ( result == XmCOPY_SUCCEEDED && 
	 text_str[2] == '/' && text_str[7] == 'Z' ) {

      sprintf ( insert_str, "%sZ", &(dat_tim[4]) );
      XmTextReplace(_facdTxt06W, (XmTextPosition)0, (XmTextPosition)8, insert_str);
    }
    else {

      sprintf ( insert_str, "%sZ ", &(dat_tim[4]) );
      XmTextInsert(_facdTxt06W, (XmTextPosition)0, insert_str);
    }
   
    adjust_min = 6 * 60;
    ti_addm (time_arry, &adjust_min, time_arry, &ier);
    ti_itoc (time_arry, dat_tim, &ier, sizeof(dat_tim) );
    dat_tim[11] = '\0';

    memset ( text_str, '\0', 16);
    result = XmTextGetSubstring (_facdTxt12W, (XmTextPosition)0, 
				 8, 15, text_str);

    if ( result == XmCOPY_SUCCEEDED && 
	 text_str[2] == '/' && text_str[7] == 'Z' ) {

      sprintf ( insert_str, "%sZ", &(dat_tim[4]) );
      XmTextReplace(_facdTxt12W, (XmTextPosition)0, (XmTextPosition)8, insert_str);
    }
    else {

      sprintf ( insert_str, "%sZ ", &(dat_tim[4]) );
      XmTextInsert(_facdTxt12W, (XmTextPosition)0, insert_str);
    }

    adjust_min = 6 * 60;
    ti_addm (time_arry, &adjust_min, time_arry, &ier);
    ti_itoc (time_arry, dat_tim, &ier, sizeof(dat_tim) );
    dat_tim[11] = '\0';

    memset ( text_str, '\0', 16);
    result = XmTextGetSubstring (_facdTxt18W, (XmTextPosition)0, 
				 8, 15, text_str);

    if ( result == XmCOPY_SUCCEEDED && 
	 text_str[2] == '/' && text_str[7] == 'Z' ) {

      sprintf ( insert_str, "%sZ", &(dat_tim[4]) );
      XmTextReplace(_facdTxt18W, (XmTextPosition)0, (XmTextPosition)8, insert_str);
    }
    else {

      sprintf ( insert_str, "%sZ ", &(dat_tim[4]) );
      XmTextInsert(_facdTxt18W, (XmTextPosition)0, insert_str);
    }
}

/*=====================================================================*/

void pgvolw_rdInfo ( int *iret )
/************************************************************************
 * pgvolw_rdInfo							*
 *									*
 * This function reads the volcano table and volcanic ash advisory      *
 * table.								*
 *									*
 * void pgvolw_rdInfo( iret )						*
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
 * H. Zeng/XTRIA	07/03   copied from pgvolw_rdInfo()		*
 * H. Zeng/XTRIA	08/03   added new stuff				*
 * H. Zeng/XTRIA	02/04	added text formating stuff		*
 * H. Zeng/SAIC		06/04	added misc mesg info			*
 * H. Zeng/SAIC		04/05	added more types			*
 * H. Zeng/SAIC		05/05	increased the size of tagstr3		*
 ***********************************************************************/
{
    int		ii, jj, kk, num, length, loc1_idx, loc2_idx;
    int		fcst_idx, ier, ier2;
    char	tagstr1[16], tagstr2[256], tagstr3[LLPATH], tagstr4[256];
    char	buff[256], fnm[32], *substr, *ptr=NULL, *ptr2=NULL;
    char	item[256], item2[256], loc1_prev[20], loc2_prev[20];
    FILE    	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 *  Initialize the information structures.
 */
    for (ii = 0; ii < NINFO; ii++)  {

        _vlInfo[ii].nelems = 0;
	_vlInfo[ii].index  = 0;
    }

/*
 * Do more initializations.
 */
    pgvolw_iniTxtInfo ();
    _loc1Num = 0;
    _loc2Num = 0;
    _otherFcstNum = 0;
    loc1_idx = 0;
    loc2_idx = 0;
    fcst_idx = 0;
    strcpy (loc1_prev, "\0");
    strcpy (loc2_prev, "\0");
    strcpy (_lpfPath,  "./");
    strcpy (_txtPath,  "./");

    _vaaMesgInfo = NULL;

/*
 *  Open the vaa table. If not found, return an error.
 */
    strcpy(fnm, VAA_TBL);
    fp = cfl_tbop(fnm, "pgen", &ier);
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }
 
/*
 * Count # of records for each information category.
 */
    rewind (fp);
    cfl_trln (fp, sizeof(buff), buff, &ier);

    while ( !feof(fp) ) {

	num = sscanf (buff, "%s %s %s %s", tagstr1, tagstr2,
                            tagstr3, tagstr4 );

	if ( num >= 2 && strcmp(tagstr1, "Header") == 0 
		      && strcmp(tagstr2, "Information") == 0 ) {

             (_vlInfo[VAAC].nelems)++;
        }
        else if ( num >= 2 && strcmp(tagstr1, "Information") == 0 
		           && strcmp(tagstr2, "Source") == 0 ) {

             (_vlInfo[INSR].nelems)++;
        }
        else if ( num >= 3 && strcmp(tagstr1, "Aviation") == 0 
		           && strcmp(tagstr2, "Color") == 0 
			   && strcmp(tagstr3, "Code") == 0   ) {

             (_vlInfo[AVCC].nelems)++;
        }
        else if ( num >= 4 && strcmp(tagstr1, "Fcst") == 0 
		           && strcmp(tagstr2, "Ash") == 0 
			   && strcmp(tagstr3, "Cloud") == 0  
			   && strcmp(tagstr4, "+6hr") == 0   ) {

             (_vlInfo[FACD].nelems)++;
        }
        else if ( num >= 2 && strcmp(tagstr1, "Next") == 0 
		           && strcmp(tagstr2, "Advisory") == 0 ) {

             (_vlInfo[NXAD].nelems)++;
        }
        else if ( num >= 1 && strcmp(tagstr1, "Forecaster(s)") == 0 
		                                               ) {
             (_vlInfo[FCST].nelems)++;
        }
        else if ( num >= 4 && strcmp(tagstr1, "FORMAT") == 0 
		           && strcmp(tagstr2, "LOC_1") == 0    ) {

	     if ( strcasecmp(tagstr3, loc1_prev) != 0 ) {

		  strcpy (loc1_prev, tagstr3);
		  (_loc1Num)++;
	     }
	}
        else if ( num >= 4 && strcmp(tagstr1, "FORMAT") == 0 
		           && strcmp(tagstr2, "LOC_2") == 0    ) {

	     if ( strcasecmp(tagstr3, loc2_prev) != 0 ) {

		  strcpy (loc2_prev, tagstr3);
		  (_loc2Num)++;
	     }
	}
        else if ( num >= 3 && strcmp(tagstr1, "PATH") == 0 ) {

	     if ( strcasecmp(tagstr2, "LPF_PATH") == 0 ) {

		  strcpy (_lpfPath, tagstr3);
	     }
	     else if ( strcasecmp(tagstr2, "TXT_PATH") == 0 ) {

		  strcpy (_txtPath, tagstr3);
	     }
	}
        else if ( num >= 2 && strcmp(tagstr1, "OTHERS-FCST") == 0 ) {

	     (_otherFcstNum)++;	     
	}

        cfl_trln(fp, sizeof(buff), buff, &ier);

    }

/*
 * Allocate space for each information category according to # of
 * records.
 */
    for (ii = 1; ii < NINFO; ii++)  {
 
         _vlInfo[ii].range = (char**) malloc( sizeof(char *) * 
                             _vlInfo[ii].nelems );

         for (jj = 0; jj < _vlInfo[ii].nelems; jj++)  {
              _vlInfo[ii].range[jj] = (char*)
				      malloc( sizeof(char) * NM_SIZE);
         }

         if ( ii == VAAC ) {
              _wmoRange = (VolcanoInfo_t*) malloc( sizeof(VolcanoInfo_t) * 
                           _vlInfo[ii].nelems );

              _hdrRange = (VolcanoInfo_t*) malloc( sizeof(VolcanoInfo_t) * 
                           _vlInfo[ii].nelems );

              for (jj = 0; jj < _vlInfo[ii].nelems; jj++)  {

                 _wmoRange[jj].range = (char**) malloc( sizeof(char *) 
						* MXELE );
                 _hdrRange[jj].range = (char**) malloc( sizeof(char *) 
						* MXELE );

	         _wmoRange[jj].nelems = 0;
                 _hdrRange[jj].nelems = 0;

	         _wmoRange[jj].index  = 0;
                 _hdrRange[jj].index  = 0;

                 for (kk = 0; kk < MXELE; kk++)  {

                    _wmoRange[jj].range[kk] = (char*)
				  malloc( sizeof(char) * 10 );
                    _hdrRange[jj].range[kk] = (char*)
				  malloc( sizeof(char) * 10 );
                 }
	      }
         } /* if (ii == VAAC ) { */
    }

    _loc1Info = (char***) malloc ( _loc1Num * sizeof (char**) );
    _loc2Info = (char***) malloc ( _loc2Num * sizeof (char**) );

    for ( ii = 0; ii < _loc1Num; ii ++ ) {

	  _loc1Info[ii] = (char**) malloc ( _vaaTxtInfoNum * sizeof (char*) );

	  for ( jj = 0; jj < _vaaTxtInfoNum; jj++ ) {

	        _loc1Info[ii][jj] = NULL;
          }
    }

    for ( ii = 0; ii < _loc2Num; ii ++ ) {

	  _loc2Info[ii] = (char**) malloc ( _vaaTxtInfoNum * sizeof (char*) );

	  for ( jj = 0; jj < _vaaTxtInfoNum; jj++ ) {

	        _loc2Info[ii][jj] = NULL;
          }
    }

    _loc1Str = (char**) malloc ( _loc1Num * sizeof (char*) );
    _loc2Str = (char**) malloc ( _loc2Num * sizeof (char*) );

    for ( ii = 0; ii < _loc1Num; ii ++ ) {

	  _loc1Str[ii] = (char*) malloc ( 20 * sizeof (char) );
	  _loc1Str[ii][0] = '\0';
    }

    for ( ii = 0; ii < _loc2Num; ii ++ ) {

	  _loc2Str[ii] = (char*) malloc ( 20 * sizeof (char) );
	  _loc2Str[ii][0] = '\0';
    }

    strcpy (loc1_prev, "\0");
    strcpy (loc2_prev, "\0");

    _otherFcstInfo = (char***) malloc ( _otherFcstNum * sizeof (char**) );

    for ( ii = 0; ii < _otherFcstNum; ii ++ ) {

	  _otherFcstInfo[ii] = (char**) malloc ( 3 * sizeof (char*) );

	  for ( jj = 0; jj < 3; jj++ ) {

	        _otherFcstInfo[ii][jj] = (char*) malloc ( 64 * sizeof (char) );
          }
    }

/*
 *  Scan table line-by-line.
 */
    rewind (fp);
    while ( !feof(fp) )  {

	cfl_trln(fp, sizeof(buff), buff, &ier);
	if ( ier != 0 ) continue;

	num = sscanf (buff, "%s %s %s %s", tagstr1, tagstr2,
                            tagstr3, tagstr4 );

	if ( num >= 2 && strcmp(tagstr1, "Header") == 0 
		      && strcmp(tagstr2, "Information") == 0 ) {

             substr = strstr (buff, tagstr2);
             substr = substr + strlen (tagstr2);
	     cst_rxbl (substr, substr, &length, &ier2);

             ptr = strtok (substr, "/" ) ;
	     sprintf (item, "%s", ptr);
             if (item[strlen(item)-1] == ' ') item[strlen(item)-1]='\0';     
	     strcpy ( _vlInfo[VAAC].range[_vlInfo[VAAC].index], item );

	     ptr = strtok (NULL, "/" ) ;
	     sprintf (item, "%s", ptr);
             if (item[strlen(item)-1] == ' ') item[strlen(item)-1]='\0'; 
	     strcat ( _vlInfo[VAAC].range[_vlInfo[VAAC].index], "/"  );	     
	     strcat ( _vlInfo[VAAC].range[_vlInfo[VAAC].index], item );

	     ptr = strtok (NULL, "/" ) ;
	     sprintf (item, "%s", ptr);
             if (item[strlen(item)-1] == ' ') item[strlen(item)-1]='\0'; 

	     ptr = strtok (NULL, "/" ) ;
	     sprintf (item2, "%s", ptr);
             if (item2[strlen(item2)-1] == ' ') item2[strlen(item2)-1]='\0'; 

	     jj = _vlInfo[VAAC].index;

	     ptr2 = strtok (item, ";");
	     while ( ptr2 != NULL) {
                 strcpy (_wmoRange[jj].range[_wmoRange[jj].index],
			 ptr2 );
                 _wmoRange[jj].index++;
	         ptr2 = strtok (NULL, ";");
	     }
	     _wmoRange[jj].nelems = _wmoRange[jj].index;

	     ptr2 = strtok (item2, ";");
	     while ( ptr2 != NULL) {
                 strcpy (_hdrRange[jj].range[_hdrRange[jj].index],
			 ptr2 );
                 _hdrRange[jj].index++;
	         ptr2 = strtok (NULL, ";");
	     }
	     _hdrRange[jj].nelems = _hdrRange[jj].index;


	     (_vlInfo[VAAC].index)++;

        }
        else if ( num >= 2 && strcmp(tagstr1, "Information") == 0 
		           && strcmp(tagstr2, "Source") == 0 ) {

             substr = strstr (buff, tagstr2);
             substr = substr + strlen (tagstr2);
	     cst_rxbl (substr, substr, &length, &ier2);
   
	     strcpy ( _vlInfo[INSR].range[_vlInfo[INSR].index], substr );

             (_vlInfo[INSR].index)++;

        }
        else if ( num >= 3 && strcmp(tagstr1, "Aviation") == 0 
		           && strcmp(tagstr2, "Color") == 0 
			   && strcmp(tagstr3, "Code") == 0   ) {

             substr = strstr (buff, tagstr3);
             substr = substr + strlen (tagstr3);
	     cst_rxbl (substr, substr, &length, &ier2);
   
	     strcpy ( _vlInfo[AVCC].range[_vlInfo[AVCC].index], substr );

             (_vlInfo[AVCC].index)++;

        }
        else if ( num >= 4 && strcmp(tagstr1, "Fcst") == 0 
		           && strcmp(tagstr2, "Ash") == 0 
			   && strcmp(tagstr3, "Cloud") == 0  
			   && strcmp(tagstr4, "+6hr") == 0   ) {

             substr = strstr (buff, tagstr4);
             substr = substr + strlen (tagstr4);
	     cst_rxbl (substr, substr, &length, &ier2);
   
	     strcpy ( _vlInfo[FACD].range[_vlInfo[FACD].index], substr );

             (_vlInfo[FACD].index)++;

        }
        else if ( num >= 2 && strcmp(tagstr1, "Next") == 0 
		           && strcmp(tagstr2, "Advisory") == 0 ) {

             substr = strstr (buff, tagstr2);
             substr = substr + strlen (tagstr2);
	     cst_rxbl (substr, substr, &length, &ier2);
   
	     strcpy ( _vlInfo[NXAD].range[_vlInfo[NXAD].index], substr );

             (_vlInfo[NXAD].index)++;

        }
        else if ( num >= 1 && strcmp(tagstr1, "Forecaster(s)") == 0 
		                                               ) {

             substr = strstr (buff, tagstr1);
             substr = substr + strlen (tagstr1);
	     cst_rxbl (substr, substr, &length, &ier2);
   
	     strcpy ( _vlInfo[FCST].range[_vlInfo[FCST].index], substr );

             (_vlInfo[FCST].index)++;

        }
        else if ( num >= 4 && strcmp(tagstr1, "FORMAT") == 0 
		           && strcmp(tagstr2, "LOC_1") == 0 ) {

	     if ( strcasecmp(tagstr3, loc1_prev) != 0 ) {

		  strcpy (loc1_prev, tagstr3);
		  strcpy (_loc1Str[loc1_idx], tagstr3);
		  (loc1_idx)++;
	     }

             substr = strstr (buff, tagstr3);
             substr = substr + strlen (tagstr3);
	     cst_rxbl (substr, substr, &length, &ier2);
	     pgvolw_getTxtInfo ( _loc1Info[loc1_idx-1], substr );

        }
        else if ( num >= 4 && strcmp(tagstr1, "FORMAT") == 0 
		           && strcmp(tagstr2, "LOC_2") == 0 ) {

	     if ( strcasecmp(tagstr3, loc2_prev) != 0 ) {

		  strcpy (loc2_prev, tagstr3);
		  strcpy (_loc2Str[loc2_idx], tagstr3);
		  (loc2_idx)++;
             }

             substr = strstr (buff, tagstr3);
             substr = substr + strlen (tagstr3);
	     cst_rxbl (substr, substr, &length, &ier2);
	     pgvolw_getTxtInfo ( _loc2Info[loc2_idx-1], substr );

        }
        else if ( num >= 1 && strcmp(tagstr1, "MISCMESG") == 0 ) {

             substr = strstr (buff, tagstr2);
	     cst_rxbl (substr, substr, &length, &ier2);
	     pgvolw_getMesgInfo ( substr );

        }
        else if ( num >= 2 && strcmp(tagstr1, "OTHERS-FCST") == 0 ) {

             substr = strstr (buff, tagstr2);
	     cst_rxbl (substr, substr, &length, &ier2);

             ptr = strtok (substr, "|" ) ;    
	     strcpy ( _otherFcstInfo[fcst_idx][0], ptr );

	     ptr = strtok (NULL, "|" ) ;
	     strcpy ( _otherFcstInfo[fcst_idx][1], ptr );

	     ptr = strtok (NULL, "|" ) ;
	     strcpy ( _otherFcstInfo[fcst_idx][2], ptr );

	     (fcst_idx)++;
        }
    } /* the end of while( !feof...  */
    cfl_clos(fp, &ier);
}

/*=====================================================================*/

void pgvolw_rdWords ( const char *ptbl, const char *wtbl, char **wdtxt,
                      int *iret )
/************************************************************************
 * pgvolw_rdWords							*
 *                                                                      *
 * This function reads the VAA wording text.				*
 *									*
 * NOTE: The caller is responsible for free the text string after use.	*
 *                                                                      *
 * void pgvolw_rdWords ( ptbl, wtbl, wdtxt, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ptbl		const char	Pref table name			*
 *	*wtbl		const char	Wording table name		*
 *                                                                      *
 * Output parameters:                                                   *
 *	**wdtxt		char		Wording text			*
 *      *iret		int		Return value			*
 *					  -1 - Unable to open table	*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          2/06                                           *
 * T. Piper/SAIC	06/06	Replaced constant numbers w/variables	*
 ***********************************************************************/
{
    int         nc, first, wdidx, buflen, ier;
    char        buff[256], fld1[128], fld2[128];
    char	*wdstr, *txtp, **aryptr;
    FILE        *fp;
    size_t	capacity, length;
/*---------------------------------------------------------------------*/
    *iret = 0;
    buflen = 256;

/*
 * Read the wording preference.
 */
    fp = cfl_tbop( (char *)ptbl, "config", &ier );
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }
    while ( !feof(fp) ) {
        cfl_trln (fp, sizeof(buff), buff, &ier);
	sscanf ( buff, "%s %s", fld1, fld2 );

	if ( strcmp ( fld1, "NEW_VAA_WORDING" ) == 0 ) {
	    if ( strcmp ( fld2, "TRUE" ) == 0 ){
	        wdidx = 1;
	    } else {
	        wdidx = 0;
	    }
	}
    }
    cfl_clos ( fp, &ier );

/*
 *  Read the wording text.
 */
    fp = cfl_tbop( (char *)wtbl, "pgen", &ier );
    if ( fp == NULL  ||  ier != 0 )  {
        *iret = -1;
        return;
    }

    capacity = 2;
    G_MALLOC ( aryptr, char*, capacity, "pgvolw_rdWords" );
    capacity = 256;
    for ( nc = 0; nc < 2; nc++ ) {
        G_MALLOC ( aryptr[nc], char, capacity, "pgvolw_rdWords" );
    }

/*
 * Read table line by line.
 */
    G_MALLOC ( wdstr, char, capacity, "pgvow_rdWords" );
    length = 0;
    first = G_TRUE;
    while ( !feof(fp) ) {
        cfl_trln (fp, sizeof(buff), buff, &ier);
	sscanf ( buff, "%s %s", fld1, fld2 );

        if ( strcmp ( fld1, "WORDING" ) == 0 ) {
	    txtp =  strstr ( buff, fld2) + strlen ( fld2 );
	    cst_clst ( txtp, '|', " ", 2, 256, aryptr, &nc, &ier );
	    cst_rnan ( aryptr[wdidx], aryptr[wdidx], &ier );
	    memset ( buff, 0, buflen );
	    cst_stag ( fld2, aryptr[wdidx], &buflen, buff, &ier );
	    length += strlen ( buff );

/*
 * Increase string size if necessary.
 */
	    if ( length >= capacity ) {
		capacity += 256;
		G_REALLOC ( wdstr, char, capacity, "pgvolw_rdWords" );
	    }

	    if ( first == G_TRUE ) {
		first = G_FALSE;
		strcpy ( wdstr, buff );
	    } else {
		strcat ( wdstr, buff );
	    }
        }
    }

    for ( nc = 0; nc < 2; nc++ ) {
        G_FREE ( aryptr[nc], char );
    }
    G_FREE ( aryptr, char*);
    cfl_clos ( fp, &ier );

    *wdtxt = wdstr;

    return;
}
