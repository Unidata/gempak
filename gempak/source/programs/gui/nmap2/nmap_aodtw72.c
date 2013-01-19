#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"
#include "hints.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "pgcmn.h"
#include "Nxm.h"
#include "AODT/v72/odtapi.h"
#include "AODT/v72/odtlibfuncs.h"
#include "proto_nmaplib.h"
#include "proto_xw.h"

#define AODT_TBL       "aodt.tbl"
#define VISIBLE_ITEM   10 
#define INVALID_VAL    -9999.0F
#define MAXLINE	       400
#define DIM	       500
#define RAD	       190

#define RED	       2
#define WHITE	       31

#define	NINFO	 1	/* Number of different elements listed here	*/
#define	HIST	 0	/* history file names				*/

#define	MXELE	30	/* Max elements per info type			*/


static Widget		_aodtwWin = 0,  _aodtwHistWin;
static Widget		_latTxtW,   _lonTxtW, _mesgTxtW,  _runBtnW;
static Widget		_fnameTxtW, _histLstW, _histTxtW, _histForm, _histNameW;
static Widget		_histMenu,  _histMenuB,_histCasc;
static Widget		_commentW, _commTxtW;

static	struct	optMenuStrc	_domStrc;
static	struct	optMenuStrc	_landStrc;
static	struct	optMenuStrc	_searStrc;
static	struct	optMenuStrc	_typeStrc;

static  char	*_domStr[] = {"DEF", "ATL", "PAC"};
static  char	*_landStr[] = {"NO", "YES"};
static  char	*_searStr[] = {"NO", "YES"};
static  char	*_typeStr[] = {"COMPUTED", "EYE", "PINHOLE EYE", "LARGE EYE",
			       "UNIFORM CDO", "EMBEDDED CENTER", 
			       "IRREGULAR CDO", "CURVED BAND", "SHEAR"      };

static float    _currLat, _currLon;
static char	_aodtwVer[16];
static char	_hist_dir[FILE_FULLSZ];

static Pixel	_defFg, _defBg, _errorFg, _errorBg;


/*
 *  private callback functions
 */
void aodtw72_commCtlBtnCb ( Widget, long, XtPointer );
void aodtw72_ctlBtnCb     ( Widget, long, XtPointer );
void aodtw72_histCtlBtnCb ( Widget, long, XtPointer );
void aodtw72_histPushBtnCb( Widget, long, XtPointer );
void aodtw72_latLonTxtCb  ( Widget, long, XtPointer );
void aodtw72_menuTextCb   ( Widget, long, XtPointer );
void aodtw72_histSelCb    ( Widget, long, XtPointer );
void aodtw72_histTxtCb    ( Widget, long, XtPointer );
void aodtw72_pointerEh    ( Widget, long, XEvent*, Boolean* );
void aodtw72_pushBtnCb    ( Widget, long, XtPointer );


/*
 *  private functions
 */
void 	aodtw72_commFrameOff ( void );
void 	aodtw72_commFrameOn ( char *comment );
void    aodtw72_commHist ( void );
Widget  aodtw72_createMenuText ( Widget parent, char *labelstr, int ncol, 
			       int textoff, int info_type, 
			       Widget *textwid, Widget *btnwid );
void    aodtw72_delHist (    void  );
void	aodtw72_freeRes (	   void  );
void    aodtw72_getSceneTypes (  int user_pick, int* eye_typ, 
			       int* cloud_typ );
void	aodtw72_ghostPts ( Boolean make_new );
Widget  aodtw72_histCreate ( Widget parent );
Boolean aodtw72_histIsUp   ( void  );
void    aodtw72_histPopdown( void  );
void    aodtw72_histPopup  ( void  );
void    aodtw72_locUpdate  ( void  );
void	aodtw72_popdown ( void );
void    aodtw72_prntMesg   (     void  );
void    aodtw72_prntRecs   (     Boolean prnt_all );
void    aodtw72_runAODT (    int* ier, char* retmsg  );
void    aodtw72_saveMesg   (     void  );
void    aodtw72_saveRecs   (     Boolean save_all );
void    aodtw72_setFileMenu( void );
void    aodtw72_setHist (    Boolean set_file );


/************************************************************************
 * nmap_aodtw72.c							*
 *									*
 * This module defines AODT popup window for nmap			*
 *									*
 * CONTENTS:								*
 *	aodtw72_create()		create the AODT window		*
 *	aodtw72_popup()		pop up the AODT window			*
 *	aodtw72_popdown()		pop down the AODT window	*
 *									*
 *	aodtw72_histCreate()	create the hist. management window	*
 *	aodtw72_histPopup()	pop up the hist. management window	*
 *	aodtw72_histPopdown()	pop down the hist. management window	*
 *									*
 *	aodtw72_isUp()		query if the window is up 		*
 *	aodtw72_histIsUp()	query if the hist. mgt. window is up	*
 *									*
 *	aodtw72_commCtlBtnCb()  callback for "Apply" comment		*
 *	aodtw72_ctlBtnCb()	callback for control buttons 		*
 *	aodtw72_histCtlBtnCb	callback for hist. mgt. ctl. buttons	*
 *      aodtw72_histSelCb	callback for history file selction	*
 *      aodtw72_histTxtCb	callback for history text field		*
 *	aodtw72_pushBtnCb()	callback for push buttons		*
 *	aodtw72_histPushBtnCb()	callback for push btns on Hist. Win.	*
 *	aodtw72_latLonTxtCb()	callback for lat&lon text widgets	*
 *      aodtw72_pointerEh()	event handler for input from the map    *
 *                              widget	                                *
 *                                                                      *
 *	aodtw72_locUpdate()	update the lat&lon text widget values	*
 *      aodtw72_setFileMenu()	set history file menu options.		*
 *	aodtw72_setHist()	set history list on History Window	*
 *	aodtw72_delHist()	delete selected itmes from the list	*
 *	aodtw72_runAODT()	run AODT algorithm			*
 *	aodtw72_freeRes()	free resource back to OS		*
 *      aodtw72_getSceneTypes()	get scene types in AODT			*
 *	aodtw72_saveMesg()	save the text message			*
 *	aodtw72_prntMesg()	print the text message			*
 *	aodtw72_saveRecs()	save the records from hist. file	* 
 *	aodtw72_prntRecs()	print the records from hist. file	*
 *	aodtw72_commHist()      Add comment to history file		*
 *    	aodtw72_commFrameOff()	De-sensitize comment frame		*
 *	aodtw72_commFrameOn ()	Sensitize comment frame and load comment*
 ***********************************************************************/

/*=====================================================================*/

Widget aodtw72_create ( Widget parent )
/************************************************************************
 * aodtw72_create							*
 *									*
 * This function creates the AODT popup window.				*
 *									*
 * Widget aodtw72_create(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * aodtw72_create	Widget	ID of the AODT popup window		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA     01/04	initial coding				*
 * H. Zeng/SAIC	     04/04	added history file menu			*
 * H. Zeng/SAIC	     05/04	added print button callback		*
 * H. Zeng/SAIC	     06/04	change # of row for _mesgTxtW		*
 * E. Safford/SAIC   05/05	free fontlist				*
 * M. Li/SAIC        05/05      Added callback function for _histTxtW   *
 * M. Li/SAIC        06/05      From aodtw_create                       *
 * M. Li/SAIC        01/07      From aodtw64_                          	*
 ***********************************************************************/
{
    Widget	     pane, loc_lbl, loc_btn, attr_form, loc_form;
    Widget           mesg_form, prnt_btn, save_btn;
    Widget	     frame1, clr_btn;
    char	     *btnstrs[] = {"History File Management...", "Close"};
    Display	     *dsp;
    XmFontListEntry  flentry;
    XmFontList	     fontlist;
    int		     nn, loff = 2;
    char  fontname[] = "-adobe-courier-bold-r-normal-*-*-120-*-*-m-*-*-*";
/*---------------------------------------------------------------------*/
/*
 * read option info. from nmap_aodt.tbl
 */
    /* aodtw72_rdTbl(); */

/*
 * create dialog shell
 */
    _aodtwWin = XmCreateFormDialog(parent, "aodtw72_popup",
				    NULL, 0);
    XtVaSetValues(_aodtwWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_aodtwWin), XmNtitle, "AODT", NULL);
    
/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("aodtw72_pane",
			    xmPanedWindowWidgetClass, _aodtwWin,
			    XmNsashWidth,             1,
			    XmNsashHeight,            1,
			    NULL);
    
/*
 * create LOCATION area
 */
    loc_form = XtVaCreateWidget("location_form",
				xmFormWidgetClass,    pane,
				NULL);
 
     loc_lbl = XtVaCreateManagedWidget ("Center Location: ",
		     xmLabelWidgetClass,  loc_form,
		     XmNtopAttachment,	  XmATTACH_FORM,
		     XmNtopOffset,	  10,
		     XmNleftAttachment,	  XmATTACH_FORM,
		     NULL);

    _latTxtW =
	(Widget) XtVaCreateManagedWidget ( "lat_text",
				xmTextFieldWidgetClass,    
						      loc_form,
				XmNcolumns,           8,
				XmNmaxLength,	      15,
				XmNcursorPositionVisible, 
						      FALSE,
				XmNtopAttachment,     XmATTACH_FORM,
				XmNleftAttachment,    XmATTACH_WIDGET,
				XmNleftWidget,        loc_lbl,
				XmNleftOffset,	      5,
				NULL);

    XtAddCallback( _latTxtW, XmNlosingFocusCallback,
                   (XtCallbackProc)aodtw72_latLonTxtCb, (XtPointer)0 );

    XtAddCallback( _latTxtW, XmNfocusCallback,
                   (XtCallbackProc)aodtw72_latLonTxtCb, (XtPointer)1 );

    _lonTxtW =
	(Widget) XtVaCreateManagedWidget ( "lon_text",
				xmTextFieldWidgetClass,    
						      loc_form,
				XmNcolumns,           8,
				XmNmaxLength,	      15,
				XmNcursorPositionVisible, 
						      FALSE,
			        XmNtopAttachment,     XmATTACH_FORM,
				XmNleftAttachment,    XmATTACH_WIDGET,
				XmNleftWidget,        _latTxtW,
				XmNleftOffset,	      5,
				NULL);

    XtAddCallback( _lonTxtW, XmNlosingFocusCallback,
                   (XtCallbackProc)aodtw72_latLonTxtCb, (XtPointer)2 );

    XtAddCallback( _lonTxtW, XmNfocusCallback,
                   (XtCallbackProc)aodtw72_latLonTxtCb, (XtPointer)3 );

    loc_btn = XtVaCreateManagedWidget ( "Click Loc", 
			xmPushButtonWidgetClass, loc_form,
			XmNtopAttachment,	 XmATTACH_FORM,
			XmNtopOffset,		 4,
			XmNleftAttachment,       XmATTACH_WIDGET,
			XmNleftWidget,           _lonTxtW,
			XmNleftOffset,	         25,
			NULL );

    XtAddCallback ( loc_btn, XmNactivateCallback,
		    (XtCallbackProc)aodtw72_pushBtnCb, (XtPointer)0 );

    XtManageChild( loc_form );

/*
 * create ATTRIBUTE area
 */
    attr_form = XtVaCreateWidget("attrib_form",
				 xmFormWidgetClass,    pane,
				 NULL);


    _histForm = aodtw72_createMenuText (attr_form, "Storm/History File: ", 
				      MXCHR, loff, HIST, &_histTxtW, 
				      NULL );
    XtAddCallback( _histTxtW, XmNlosingFocusCallback,
                   (XtCallbackProc)aodtw72_histTxtCb, (XtPointer)0 );

    XtVaSetValues(_histForm, 
		  XmNtopAttachment,	XmATTACH_FORM,
                  XmNleftAttachment,    XmATTACH_FORM,
		  NULL);

    nn = XtNumber (_domStr);
    _domStrc.current = 0;
    pgutls_createOptionMenu (attr_form, nn, (XtPointer)&_domStrc.current, 
			     "Domain", NULL, &_domStrc.form, &_domStrc.label, 
                             &_domStrc.menu, _domStrc.pb, _domStr);

    XtVaSetValues (_domStrc.label, 
		   XmNtopAttachment,    XmATTACH_FORM,
		   XmNtopOffset,        10,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    XtVaSetValues (_domStrc.form, 
		   XmNtopAttachment,    XmATTACH_WIDGET,
		   XmNtopWidget,	_histForm,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    nn = XtNumber (_landStr);
    _landStrc.current = 0;
    pgutls_createOptionMenu (attr_form, nn, (XtPointer)&_landStrc.current, 
			     "Land", NULL, &_landStrc.form, 
			     &_landStrc.label, &_landStrc.menu, _landStrc.pb, 
			     _landStr);

    XtVaSetValues (_landStrc.label, 
		   XmNtopAttachment,    XmATTACH_FORM,
		   XmNtopOffset,        10,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    XtVaSetValues (_landStrc.form, 
		   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopWidget,	_domStrc.form,
		   XmNleftAttachment,	XmATTACH_WIDGET,
		   XmNleftWidget,	_domStrc.form,
		   XmNleftOffset,	10,
		   NULL);

    nn = XtNumber (_searStr);
    _searStrc.current = 0;
    pgutls_createOptionMenu (attr_form, nn, (XtPointer)&_searStrc.current, 
			     "Search", NULL, &_searStrc.form, 
			     &_searStrc.label, &_searStrc.menu, _searStrc.pb, 
			     _searStr);

    XtVaSetValues (_searStrc.label, 
		   XmNtopAttachment,    XmATTACH_FORM,
		   XmNtopOffset,        10,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    XtVaSetValues (_searStrc.form, 
		   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNtopWidget,	_landStrc.form,
		   XmNleftAttachment,	XmATTACH_WIDGET,
		   XmNleftWidget,	_landStrc.form,
		   XmNleftOffset,	10,
		   NULL);

    nn = XtNumber (_typeStr);
    _typeStrc.current = 0;
    pgutls_createOptionMenu (attr_form, nn, (XtPointer)&_typeStrc.current, 
			     "Scene Type", NULL, &_typeStrc.form, 
			     &_typeStrc.label, &_typeStrc.menu, _typeStrc.pb, 
			     _typeStr);

    XtVaSetValues (_typeStrc.label, 
		   XmNtopAttachment,    XmATTACH_FORM,
		   XmNtopOffset,        10,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    XtVaSetValues (_typeStrc.form, 
		   XmNtopAttachment,    XmATTACH_WIDGET,
		   XmNtopWidget,	_domStrc.form,
		   XmNleftAttachment,	XmATTACH_FORM,
		   NULL);

    _runBtnW = XtVaCreateManagedWidget ( "Run AODT", 
			xmPushButtonWidgetClass, attr_form,
			XmNtopAttachment,	 XmATTACH_OPPOSITE_WIDGET,
			XmNtopWidget,		 _typeStrc.form,
			XmNtopOffset,		 5,
			XmNleftAttachment,       XmATTACH_WIDGET,
			XmNleftWidget,           _typeStrc.form,
			XmNleftOffset,	         25,
			NULL );

    XtAddCallback ( _runBtnW, XmNactivateCallback,
		    (XtCallbackProc)aodtw72_pushBtnCb, (XtPointer)1 );

    XtManageChild( attr_form );

/*
 * create MESSAGE area
 */
    mesg_form = XtVaCreateWidget("mesg_form",
				 xmFormWidgetClass,    pane,
				 NULL);

/*
 * Create font list for text widgets.
 */
    dsp = XtDisplay (_aodtwWin);
    flentry = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);


    frame1 = XmCreateFrame( mesg_form, "_Mesg_frameW", NULL, 0 );

    _mesgTxtW = XtVaCreateManagedWidget ("mesg_text",
		      xmTextWidgetClass,        frame1,
		      XmNrows,		        30,
		      XmNcolumns,	        60,
		      XmNeditMode,	        XmMULTI_LINE_EDIT,
		      XmNwordWrap,	        TRUE,
		      XmNscrollVertical,	TRUE,
                      XmNcursorPositionVisible, FALSE,
                      XmNeditable,              FALSE,
		      XmNfontList,		fontlist,
		      NULL ); 

    XtVaGetValues (_mesgTxtW,
		   XmNforeground, 	        &_defFg,
           	   XmNbackground, 	   	&_defBg,
    		   NULL);	

    _errorFg = NxmColrP_getColorPixel ( RED ); 
    _errorBg = _defBg; 

    XtManageChild( frame1 );

    prnt_btn = XtVaCreateManagedWidget ( "Print...", 
			xmPushButtonWidgetClass, mesg_form,
			XmNtopAttachment,	 XmATTACH_WIDGET,
			XmNtopWidget,		 frame1,
			XmNleftAttachment,       XmATTACH_FORM,
			NULL );

    XtAddCallback ( prnt_btn, XmNactivateCallback,
		    (XtCallbackProc)aodtw72_pushBtnCb, (XtPointer)2 );
		      
    save_btn = XtVaCreateManagedWidget ( "Save to", 
			xmPushButtonWidgetClass, mesg_form,
			XmNtopAttachment,	 XmATTACH_WIDGET,
			XmNtopWidget,		 frame1,
			XmNleftAttachment,       XmATTACH_WIDGET,
			XmNleftWidget,		 prnt_btn,
			XmNleftOffset,		 15,
			NULL );

    XtAddCallback ( save_btn, XmNactivateCallback,
		    (XtCallbackProc)aodtw72_pushBtnCb, (XtPointer)3 );

    _fnameTxtW = XtVaCreateManagedWidget ("file_text",
		      xmTextFieldWidgetClass,   mesg_form,
		      XmNcolumns,	        20,
		      XmNmaxLength,             62,
		      XmNtopAttachment,	        XmATTACH_WIDGET,
		      XmNtopWidget,		frame1,
		      XmNleftAttachment,        XmATTACH_WIDGET,
		      XmNleftWidget,		save_btn,
		      NULL ); 

    clr_btn = XtVaCreateManagedWidget ( "Clear", 
			xmPushButtonWidgetClass, mesg_form,
			XmNtopAttachment,	 XmATTACH_WIDGET,
			XmNtopWidget,		 frame1,
			XmNleftAttachment,       XmATTACH_WIDGET,
			XmNleftWidget,		 _fnameTxtW,
			XmNleftOffset,		 15,
			NULL );

    XtAddCallback ( clr_btn, XmNactivateCallback,
		    (XtCallbackProc)aodtw72_pushBtnCb, (XtPointer)4 );

    XtManageChild( mesg_form );

/*
 * create control buttons
 */
    nn = XtNumber(btnstrs);
    NxmCtlBtn_create(pane, 0, "aodtw72_ctlBtn", nn, btnstrs, 
		     (XtCallbackProc)aodtw72_ctlBtnCb,    NULL);  

    XtManageChild(pane);

    aodtw72_histCreate ( parent );

    XmFontListFree( fontlist );

    return(_aodtwWin);
}

/*=====================================================================*/

void aodtw72_popup ( char* ver )
/************************************************************************
 * aodtw72_popup							*
 *									*
 * This function pops up AODT popup window.				*
 *									*
 * void aodtw72_popup()							*
 *									*
 * Input parameters:							*
 *	ver		char*	which version of AODT			*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04   initial coding				*
 * H. Zeng/SAIC		04/04	added version info			*
 * T. Piper/SAIC	12/04	Added aodtw72_refresh			*
 * M. Li/SAIC           06/05   From aodtw_popup                        *
 * M. Li/SAIC		03/06	Added ctb_rdprf				*
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    int	  ier, length;
    char  title[64];
/*---------------------------------------------------------------------*/
/*
 * Retrieve the storage directory for history files.
 */
    ctb_rdprf ( "prefs.tbl", "config", "AODT_HIST_DIR", _hist_dir, &ier );
    cst_rmbl ( _hist_dir, _hist_dir, &length, &ier );
    if ( _hist_dir[length-1] != '/' ) strcat ( _hist_dir, "/" );

/*
 * create a title that contains version info.
 */
    strcpy (_aodtwVer, ver);
    sprintf (title, "AODT(%s)", ver);
    XtVaSetValues(XtParent(_aodtwWin), XmNtitle, title, NULL); 

    XtManageChild (_aodtwWin);

    _currLat = INVALID_VAL;
    _currLon = INVALID_VAL;
    aodtw72_locUpdate();

/*
 * set current storm/history file menu.
 */
    aodtw72_setFileMenu();

    XmTextSetString(_histTxtW,  "\0");
    XmTextSetString(_mesgTxtW,  "\0");
    XmTextSetString(_fnameTxtW, "\0");

    XtVaSetValues (_mesgTxtW,
                   XmNforeground,           _defFg,
                   XmNbackground,           _defBg,
           	   NULL);

    XtVaSetValues (_domStrc.menu, 
		   XmNmenuHistory, _domStrc.pb[0], 
		   NULL);
    _domStrc.current = 0;

    XtVaSetValues (_landStrc.menu, 
		   XmNmenuHistory, _landStrc.pb[0], 
		   NULL);
    _landStrc.current = 0;

    XtVaSetValues (_searStrc.menu, 
		   XmNmenuHistory, _searStrc.pb[0], 
		   NULL);
    _searStrc.current = 0;

    XtVaSetValues (_typeStrc.menu, 
		   XmNmenuHistory, _typeStrc.pb[0], 
		   NULL);
    _typeStrc.current = 0;

    mcanvw_setDynamicFunc ((XtEventHandler)&aodtw72_pointerEh,
	 		   (XtEventHandler)NULL,
                           (XtEventHandler)NULL, CURS_POINT_SELECT);
    aodtw72_refresh(TRUE); 
}

/*=====================================================================*/

void aodtw72_popdown ( void ) 
/************************************************************************
 * aodtw72_popdown							*
 *									*
 * This function pops down the AODT window.				*
 *									*
 * void aodtw72_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04	initial coding				*
 * M. Li/SAIC           06/05   From aodtw_popdown                      *
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    if (XtIsManaged (_aodtwWin)) {
	XtUnmanageChild (_aodtwWin);

    }
    aodtw72_histPopdown();  

    mcanvw_disarmDynamic();
    mcanvw_setCursor (CURS_DEFAULT);
    
    if ( pgpalw_isUp() ) {
        pgpalw_setupOper( );
    }
}

/*=====================================================================*/

Boolean aodtw72_isUp ( void ) 
/************************************************************************
 * aodtw72_isUp								*
 *									*
 * This function queries whether the AODT window is up.			*
 *									*
 * Boolean aodtw72_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * cldhgtw_isUp	Boolean		True -- up,	False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04	initial coding				*
 * M. Li/SAIC           06/05   Added a check for _aodtwWin           	*
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
	if ( _aodtwWin == 0 ) {
	    return False;
	}
	else {
	    return (XtIsManaged (_aodtwWin));
	}
}

/*=====================================================================*/

Widget aodtw72_histCreate ( Widget parent )
/************************************************************************
 * aodtw72_histCreate							*
 *									*
 * This function creates the AODT History File Management popup window.	*
 *									*
 * Widget aodtw72_histCreate(parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * aodtw72_histCreate	Widget	Widget ID of Hist. Management popup 	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04   initial coding				*
 * H. Zeng/XTRIA	03/04   added delete btn			*
 * H. Zeng/SAIC		05/04	added callbacks for btns		*
 * H. Zeng/SAIC		07/04	added header info label			*
 * T. Piper/SAIC	09/04	added XmNalignment and XmNwidth		*
 * E. Safford/SAIC	05/05	free fontlist				*
 * M. Li/SAIC		06/05	Increase testarea width from 900 to 999	*
 * M. Li/SAIC		07/05	Added comment frame			*
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    Widget	pane, form, hdr_lbl, frame, hist_swin;
    Widget	prnt_btn, prnt_btn2, save_btn, save_btn2, del_btn;
    Widget	pane_fr1, form_fr1;
    int		nn, itype;
    char	*btnstrs[] = {"Close"}, header[512], srcID[512], strmID[512];
    char	*btnstrs2[] = {"Apply"};
    XmString	xmstr;
    char  fontname[] = "-misc-fixed-bold-r-normal--13-120-75-75-c-70-iso8859-1";
    Display	     *dsp;
    XmFontListEntry  flentry;
    XmFontList	     fontlist;
/*---------------------------------------------------------------------*/
/*
 * create dialog shell
 */
    _aodtwHistWin = XmCreateFormDialog(parent, "aodtw72_histPopup",
				    NULL, 0);
    XtVaSetValues(_aodtwHistWin, 
		  XmNnoResize,        True,
		  XmNdefaultPosition, False,
		  NULL);
    XtVaSetValues(XtParent(_aodtwHistWin),
		  XmNtitle, "AODT History File Management",
		  NULL);
 
/*
 * Create font list for text widgets.
 */
    dsp = XtDisplay (_aodtwHistWin);
    flentry = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    fontlist = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);

/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("aodtw72_pane",
			    xmPanedWindowWidgetClass, _aodtwHistWin,
			    XmNsashWidth,	    1,
			    XmNsashHeight,	    1,
			    NULL);

    form = XtVaCreateWidget("hist_form", xmFormWidgetClass, pane, NULL);

/*
 * History file column header info.
 */
    itype = -1;
    srcID[0] = CHNULL;
    aodtv72_historylistfmt(0, itype, srcID, strmID, header);
    xmstr = XmStringCreateLtoR(header, XmFONTLIST_DEFAULT_TAG);

    hdr_lbl = XtVaCreateManagedWidget ("Header_Info",
				xmLabelWidgetClass,	form,
				XmNalignment,		XmALIGNMENT_BEGINNING,
				XmNfontList,		fontlist,
				XmNlabelString,		xmstr,
				XmNleftAttachment,	XmATTACH_FORM,
				XmNtopAttachment,	XmATTACH_FORM,
				XmNwidth,		1020,
				NULL);

    XmStringFree(xmstr);

/*
 * Create a frame to hold list widget.
 */
    frame = XmCreateFrame( form, "_Hist_frameW", NULL, 0 );

    hist_swin = XmCreateScrolledWindow( frame, "hist_window", NULL, 0 );

    _histLstW = XtVaCreateManagedWidget ("_Hist_lstW",
		   xmListWidgetClass,	        hist_swin,
		   XmNvisibleItemCount,	        VISIBLE_ITEM,
		   XmNscrollBarDisplayPolicy,   XmSTATIC,
		   XmNselectionPolicy,		XmEXTENDED_SELECT,
		   XmNtraversalOn,		FALSE,
		   XmNlistSizePolicy,		XmCONSTANT,
		   XmNfontList,			fontlist,
		   NULL );

    XtAddCallback ( _histLstW, XmNextendedSelectionCallback,
                    (XtCallbackProc)aodtw72_histSelCb, NULL );


    XtManageChild( _histLstW );
    XtManageChild( hist_swin );
    XtManageChild( frame );


    XtVaSetValues (frame,
		   XmNtopAttachment,	 XmATTACH_WIDGET,
		   XmNtopWidget,	 hdr_lbl,
		   XmNleftAttachment,    XmATTACH_FORM,
		   XmNrightAttachment,   XmATTACH_FORM,
		   NULL);

    prnt_btn = XtVaCreateManagedWidget ( "Print All",
			xmPushButtonWidgetClass, form,
			XmNtopAttachment,	 XmATTACH_WIDGET,
			XmNtopWidget,		 frame,
			XmNtopOffset,		 10,
			XmNleftAttachment,       XmATTACH_FORM,
			NULL );
   
    XtAddCallback ( prnt_btn, XmNactivateCallback,
		    (XtCallbackProc)aodtw72_histPushBtnCb, (XtPointer)0 );
		
    prnt_btn2 = XtVaCreateManagedWidget ( "Print Selected",
			xmPushButtonWidgetClass, form,
			XmNtopAttachment,	 XmATTACH_WIDGET,
			XmNtopWidget,		 frame,
			XmNtopOffset,		 10,
			XmNleftAttachment,       XmATTACH_WIDGET,
			XmNleftWidget,		 prnt_btn,
			XmNleftOffset,		 10,
			NULL );

    XtAddCallback ( prnt_btn2, XmNactivateCallback,
		    (XtCallbackProc)aodtw72_histPushBtnCb, (XtPointer)1 );

    save_btn = XtVaCreateManagedWidget ( "Save All",
			xmPushButtonWidgetClass, form,
			XmNtopAttachment,	 XmATTACH_WIDGET,
			XmNtopWidget,		 prnt_btn,
			XmNtopOffset,		 3,
			XmNleftAttachment,       XmATTACH_FORM,
			NULL );

    XtAddCallback ( save_btn, XmNactivateCallback,
		    (XtCallbackProc)aodtw72_histPushBtnCb, (XtPointer)2 );

    save_btn2 = XtVaCreateManagedWidget ( "Save Selected",
			xmPushButtonWidgetClass, form,
			XmNtopAttachment,	 XmATTACH_WIDGET,
			XmNtopWidget,		 prnt_btn,
			XmNtopOffset,		 3,
			XmNleftAttachment,       XmATTACH_WIDGET,
			XmNleftWidget,		 save_btn,
			XmNleftOffset,		 10,
			NULL );

    XtAddCallback ( save_btn2, XmNactivateCallback,
		    (XtCallbackProc)aodtw72_histPushBtnCb, (XtPointer)3 );

    _histNameW = XtVaCreateManagedWidget ("file_text",
		      xmTextFieldWidgetClass,   form,
		      XmNcolumns,	        20,
		      XmNtopAttachment,	        XmATTACH_WIDGET,
		      XmNtopWidget,		prnt_btn,
		      XmNtopOffset,		1,
		      XmNleftAttachment,        XmATTACH_WIDGET,
		      XmNleftWidget,		save_btn2,
		      NULL ); 

    del_btn = XtVaCreateManagedWidget ( 
			"Delete Selected and Correct Remaining Times", 
			xmPushButtonWidgetClass, form,
			XmNtopAttachment,	 XmATTACH_WIDGET,
			XmNtopWidget,		 save_btn,
			XmNtopOffset,		 6,
			XmNleftAttachment,       XmATTACH_FORM,
			NULL );
 
    XtAddCallback ( del_btn, XmNactivateCallback,
		    (XtCallbackProc)aodtw72_histPushBtnCb, (XtPointer)4 );

    _commentW =  XtVaCreateWidget ( "Comment_text",
                xmFrameWidgetClass,             form,
                XmNleftAttachment,              XmATTACH_WIDGET,
	        XmNleftWidget,                  _histNameW,
		XmNleftOffset,			20,
                XmNtopAttachment,               XmATTACH_WIDGET,
	        XmNtopWidget,                   _histLstW,
		XmNtopOffset,			6,
                XmNrightAttachment,             XmATTACH_FORM,
	        XmNrightOffset,			150,
                NULL );

    pane_fr1 = XtVaCreateWidget("mapw_pdfFrame",
                xmPanedWindowWidgetClass,       _commentW,
                XmNsashWidth,                   1,
                XmNsashHeight,                  1,
                NULL);

    NxmLabel_createFrameLbl("Comment", pane_fr1, _commentW); 

    form_fr1 = XtVaCreateWidget("hist_form", xmFormWidgetClass, pane_fr1, NULL);

    _commTxtW = XtVaCreateManagedWidget ("Comment text",
                      xmTextFieldWidgetClass,   form_fr1,
                      XmNcolumns,               45,
		      XmNmaxLength,             50,
		      XmNleftAttachment,	XmATTACH_FORM,
		      XmNleftOffset,	        15,
                      NULL );

    XtManageChild ( form_fr1 );
    XtManageChild ( pane_fr1 );
    XtManageChild ( _commentW );


    nn = XtNumber(btnstrs2);
    NxmCtlBtn_create( pane_fr1, 1, "Comment Aplly", nn, btnstrs2,
                     (XtCallbackProc)aodtw72_commCtlBtnCb, NULL   );

    
    XtManageChild( form );

/*
 * create control buttons
 */
    nn = XtNumber(btnstrs);
    NxmCtlBtn_create(pane, 1, "aodtw72_histCtlBtn", nn, btnstrs, 
		     (XtCallbackProc)aodtw72_histCtlBtnCb,     NULL );  

    XtManageChild(pane);

    XmFontListFree( fontlist );

    return(_aodtwHistWin);
}

/*=====================================================================*/

void aodtw72_histPopup ( void )
/************************************************************************
 * aodtw72_histPopup							*
 *									*
 * This function pops up AODT History File Management window.		*
 *									*
 * void aodtw72_histPopup()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04   initial coding				*
 * H. Zeng/SAIC		05/04	minor modification			*
 * M. Li/SAIC           06/05   From aodtw_histPopup                    *
 * M. Li/SAIC		07/05	Set comment frame and text		*
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    XtManageChild (_aodtwHistWin);
    XmTextFieldSetString (_histNameW, "\0");
    XmTextFieldSetString (_commTxtW, "\0");
    XtSetSensitive ( _commentW, FALSE);
}

/*=====================================================================*/

void aodtw72_histPopdown ( void ) 
/************************************************************************
 * aodtw72_histPopdown							*
 *									*
 * This function pops down AODT Hist. File Management window.		*
 *									*
 * void aodtw72_histPopdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04   initial coding				*
 * H. Zeng/SAIC		05/04	minor modification			*
 * M. Li/SAIC           06/05   From aodtw_histPopdown                  *
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    if (XtIsManaged (_aodtwHistWin)) {

	XtUnmanageChild (_aodtwHistWin);
        XtSetSensitive (_histForm, TRUE);
        XtSetSensitive (_runBtnW , TRUE);       
        aodtw72_freeRes();
    }
}

/*=====================================================================*/

Boolean aodtw72_histIsUp ( void ) 
/************************************************************************
 * aodtw72_histIsUp							*
 *									*
 * This function queries whether AODT Hist. Management window is up.	*
 *									*
 * Boolean aodtw72_histIsUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * aodtw72_histIsUp	Boolean	     True -- up,  False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04	initial coding				*
 * M. Li/SAIC           06/05   From aodtw_histIsUp                     *
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    return (XtIsManaged (_aodtwHistWin));
}

/*=====================================================================*/
/* ARGSUSED */
void aodtw72_pointerEh ( Widget wid, long which, XEvent *event, Boolean *ctdr )
/************************************************************************
 * aodtw72_pointerEh							*
 *									*
 * This is the event handler for input from the map widget by the	*
 * mouse.								*
 *									*
 * void aodtw72_pointerEh ( wid, which, event, ctdr )			*
 *									*
 * Input parameters:							*
 *	wid	Widget	the widget calling this function		*
 *	which	long	press, drag, or drop				*
 *	*event	XEvent	the event callback structure			*
 *									*
 * Output parameters:							*
 *	*ctdr	Boolean	continue to dispatch return flag		*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04	initial coding				*
 * T. Piper/SAIC	12/04	Added aodtw72_ghostPts			*
 * M. Li/SAIC           06/05   From aodtw_pointerEh                    *
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    int		npts, ier, xoff, yoff;
    float	xx[1], yy[1];
/*---------------------------------------------------------------------*/
/*
 *  "which" always be MCANVW_PRESS
 */
    if (event->xbutton.button == Button1) {

	mcanvw_setCursor (CURS_BUSY);

	xgtoff (&xoff, &yoff, &ier);
	xx[0] = (float) (event->xbutton.x + xoff);
	yy[0] = (float) (event->xbutton.y + yoff);

	npts = 1;
	gtrans (sys_D, sys_M, &npts, xx, yy, 
		&_currLat, &_currLon, &ier, strlen(sys_D), strlen(sys_M));

	aodtw72_ghostPts ( FALSE);
	aodtw72_ghostPts ( TRUE );
	aodtw72_locUpdate ();

	mcanvw_setCursor (CURS_POINT_SELECT);
    }
}

/*=====================================================================*/

void aodtw72_refresh ( Boolean make_new )
/************************************************************************
 * aodtw72_refresh                                                      *
 *                                                                      *
 * This function redraws the ghosting.                                  *
 *                                                                      *
 * void aodtw72_refresh ( make_new )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 * make_new	Boolean	Flag for make_new or using existing location	*
 * 									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        12/04   Copied from cldhgtw_refresh             *
 * M. Li/SAIC           06/05   From aodtw_refresh                      *
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    if ( aodtw72_isUp()) {
        aodtw72_ghostPts(make_new); /* FALSE means use existing point */
    }
}

/*=====================================================================*/

void aodtw72_ghostPts ( Boolean make_new )
/************************************************************************
 * aodtw72_ghostPts                                                     *
 *                                                                      *
 * This function shows or hides the ghosting based on make_new.         *
 *                                                                      *
 * void cldhgtw_ghostPts ( make_new )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *      make_new        Boolean         recalculate cursor position     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        12/04   Copied from cldhgtw_ghostPts            *
 * M. Li/SAIC           06/05   From aodtw_ghostPts                     *
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    int                 np, ier;
    static float        pt1x[1], pt1y[1];
/*---------------------------------------------------------------------*/

    pggst_veilGhost (FALSE);

    if (make_new) {
        np = 1;
        gtrans (sys_M, sys_D, &np, &_currLat, &_currLon,
                pt1x, pt1y, &ier, strlen(sys_M), strlen(sys_D) );
    }

    pggst_cursorGhost ( pt1x, pt1y, &ier);
    pggst_clearGhost (TRUE);
}

/*=====================================================================*/
/* ARGSUSED */
void aodtw72_latLonTxtCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * aodtw72_latLonTxtCb							*
 *									*
 * Callback function for lat&lon text widgets.				*
 *									*
 * void aodtw72_latLonTxtCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which text widget			*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04	initial coding				*
 * M. Li/SAIC           06/05   From aodtw_latLonTxtCb                  *
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    char	*ptext=NULL;
    float	lat_val, lon_val;
/*---------------------------------------------------------------------*/

    switch ( which ) {

      case 0:  /* lat text widget losing focus */

         XtVaSetValues (wid,  XmNcursorPositionVisible, FALSE, NULL);

         XtVaGetValues (wid,  XmNvalue,  &ptext,  NULL);

         if ( sscanf (ptext, "%f", &lat_val) == 1 ) {

	      if ( lat_val >= -90.0F && lat_val <= 90.0F ) {

		   _currLat = lat_val;
              }
         }

	 aodtw72_locUpdate ();

         if ( ptext != NULL ) XtFree(ptext);

	 break;

      case 1:  /* lat text widget gaining focus */

         XtVaSetValues (wid,  XmNcursorPositionVisible, TRUE, NULL);

	 break;

      case 2: /* lon text widget losing focus */

         XtVaSetValues (wid,  XmNcursorPositionVisible, FALSE, NULL);

         XtVaGetValues (wid,  XmNvalue,  &ptext,  NULL);

         if ( sscanf (ptext, "%f", &lon_val) == 1 ) {

	      if ( lon_val > -180.0F && lon_val <= 180.0F ) {

		   _currLon = lon_val;
              }
         }

	 aodtw72_locUpdate ();

         if ( ptext != NULL ) XtFree(ptext);

	 break;

      case 3:  /* lon text widget gaining focus */

         XtVaSetValues (wid,  XmNcursorPositionVisible, TRUE, NULL);

	 break;

    }
}

/*=====================================================================*/

void aodtw72_locUpdate ( void )
/************************************************************************
 * aodtw72_locUpdate							*
 *									*
 * Update values into lat&lon text widgets.				*
 *									*
 * void aodtw72_locUpdate ()						*
 *									*
 * Input  parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	01/04	initial coding				*
 * H. Zeng/SAIC		05/04	minor modifications			*
 * M. Li/SAIC           06/05   From aodtw_locUpdate                    *
 * M. Li/SAIC		03/06	Add G_DIFF to eliminate compile warning	*
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    char	text_val[20];
/*---------------------------------------------------------------------*/

    if ( !G_DIFF(_currLat, (float)INVALID_VAL) && 
	 _currLat <=  90.0F       &&
	 _currLat >= -90.0F          ) {

         sprintf( text_val, "%-10.4f", _currLat );
         XtVaSetValues (_latTxtW,  XmNvalue,  text_val,  NULL);
    }
    else {

         XtVaSetValues (_latTxtW,  XmNvalue,  "\0",  NULL);
    }


    if ( !G_DIFF(_currLon, (float)INVALID_VAL) &&
	 _currLon <=  180.0F      && 
	 _currLon >  -180.0F         ) {

         sprintf( text_val, "%-10.4f", _currLon );
         XtVaSetValues (_lonTxtW,  XmNvalue,  text_val,  NULL);
    }
    else {

         XtVaSetValues (_lonTxtW,  XmNvalue,  "\0",  NULL);
    }
}

/*=====================================================================*/

void aodtw72_setFileMenu ( void )
/************************************************************************
 * aodtw72_setFileMenu							*
 *									*
 * Sets the current Storm/History File menu based on given directory.	*
 *									*
 * void aodtw72_setFileMenu ( )						*
 *									*
 * Input  parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC 	04/04	initial coding				*
 * H. Zeng/SAIC		05/04	added check of maximum number		*
 * H. Zeng/SAIC		06/04	set restriction on history file name	*
 * H. Zeng/SAIC		01/05	added cascade buttons			*
 * M. Li/SAIC		05/05	Removed check for ".hst"		*
 * M. Li/SAIC           06/05   Only ".hst_64"       			*
 * T. Piper/SAIC	10/05	declared btn_idx long			*
 * M. Li/SAIC		03/06	Removed input dir_name			*
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 * T. Piper/SAIC	10/07	Added if test before free dnamelist	*
 ***********************************************************************/
{
    char		*ptr, first_two[3], cas_list[MXELE][3];
    struct dirent	**dnamelist=NULL;
    int			ii, jj, ier;
    int			num_file, nsdir, cas_idx, num_cas;
    long		btn_idx;
    Widget		cas_menu[MXELE], push_btn;
    Boolean		on_hold;
    XmString		xmstr;
/*---------------------------------------------------------------------*/
/*
 * Destroy the current history menu.
 */
    XtDestroyWidget (_histMenu);

/*
 * Create a new history menu.
 */
    _histMenu = XmCreatePulldownMenu (_histMenuB, "histmenu", NULL, 0);
    XtVaSetValues (_histCasc, XmNsubMenuId, _histMenu, NULL);

/*
 * Initialize some variables.
 */
    on_hold = FALSE;
    cas_idx = 0;
    for ( ii = 0; ii < 3; ii++ ) first_two[ii] = '\0'; 

/*
 * Fetch files from the underlining directory.
 * The output name list is sorted in alphabetical order and is ready
 * for following processing.
 */
    num_file = cfl_rdir(2, _hist_dir, ".hst_72", &dnamelist, &nsdir);
    for ( ii = 0; ii < num_file; ii++ ) {
	cst_ncpy ( dnamelist[ii]->d_name, dnamelist[ii]->d_name, 
		   (strlen (dnamelist[ii]->d_name) - 3), &ier );

/*
 * Check if there are less than two letters before name extension.
 */
        ptr = dnamelist[ii]->d_name;
        if ( ptr[0] == '.' || ptr[1] == '.' ) continue;

/*
 * Construct cas_list.
 */
	if ( ptr[0] != first_two[0] || ptr[1] != first_two[1] ) {

	   first_two[0] = ptr[0];
           first_two[1] = ptr[1];
           first_two[3] = '\0';
           on_hold = FALSE;

	}
        else if ( !on_hold ) {

	   strcpy ( cas_list[cas_idx], first_two);
           cas_idx++;
           if ( cas_idx >= MXELE ) break;
           on_hold = TRUE;
        }

    } /* the end of for (... */

    num_cas = cas_idx;
 
/*
 * Create cascade buttons and their associated menus.
 */
    for ( ii = 0; ii < num_cas; ii++ ) {

      xmstr = XmStringCreateLocalized (cas_list[ii]);
      cas_menu[ii] = XmCreatePulldownMenu (_histMenu, "cas_menu", NULL, 0);
      XtVaCreateManagedWidget ("cascades",
			      xmCascadeButtonWidgetClass, _histMenu,
			      XmNlabelString,		  xmstr,
			      XmNsubMenuId,		  cas_menu[ii],
			      NULL );
      XmStringFree (xmstr);

    }

/*
 * Create rest history file push buttons.
 */
    btn_idx = 0;

    for ( ii = 0; ii < num_file; ii++ ) {

/*
 * For history file buttons, either list them under cascade 
 * buttons or list them under corresponding cascade menus.
 */
        ptr = dnamelist[ii]->d_name;
	free( dnamelist[ii] );
	cas_idx = -1;
        for ( jj = 0; jj < num_cas; jj++ ) {

           if ( ptr[0] == cas_list[jj][0] && 
	        ptr[1] == cas_list[jj][1]    ) {

		cas_idx = jj;
		break;
	   }
        }

        if ( cas_idx >= 0 ) {

           xmstr = XmStringCreateLocalized ( ptr );
           push_btn = XtVaCreateManagedWidget ("hist_file_button",
                                xmPushButtonWidgetClass, cas_menu[cas_idx],
                                XmNlabelString,          xmstr,
                                XmNuserData,             _histTxtW,
                                NULL);
           XmStringFree (xmstr);
	   XtAddCallback (push_btn, XmNactivateCallback,
		          (XtCallbackProc) aodtw72_menuTextCb,
		          (XtPointer) btn_idx);
           btn_idx++;

	}
	else {

           xmstr = XmStringCreateLocalized ( ptr );
           push_btn = XtVaCreateManagedWidget ("hist_file_button",
                                xmPushButtonWidgetClass, _histMenu,
                                XmNlabelString,          xmstr,
                                XmNuserData,             _histTxtW,
                                NULL);
           XmStringFree (xmstr);
	   XtAddCallback (push_btn, XmNactivateCallback,
		          (XtCallbackProc) aodtw72_menuTextCb,
		          (XtPointer) btn_idx);
           btn_idx++;

        }
    } /* the end of for (... */

    if ( dnamelist != NULL )  free( dnamelist[ii] );

    XtManageChild ( _histMenu );
}

/*=====================================================================*/

void aodtw72_runAODT ( int* ier, char* retmsg )
/************************************************************************
 * aodtw72_runAODT							*
 *                                                                      *
 * This function runs AODT algorithm and display the text message on	*
 * message display area if an IR image has been loaded.			*
 *                                                                      *
 * void aodtw72_runAODT ( ier, retmsg )					*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 *	ier	int*	return code					*
 *	retmsg	char*	return message					*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA	01/04		initial coding			*
 * H. Zeng/SAIC		04/04		renamed to aodtw72_runAODT	*
 * M. Li/SAIC	 	03/05		Move the call to dataw_getIRInfo*
 * M. Li/SAIC		05/05		Update history file cascade list* 
 * M. Li/SAIC           06/05           From aodtw_runAODT              *
 * M. Li/SAIC		07/05		malloc -> G_MALLOC		*
 * M. Li/SAIC           10/05           Relocated aodtv72_initialize    *
 * M. Li/SAIC		03/06		Added history file directory	*
 * T. Piper/SAIC	05/06	Properly allocate two-dimensional arrays*
 * T. Piper/SAIC	05/06	Properly free temps, lats, lons arrays	*
 * M. Li/SAIC        	01/07   From aodtw64_                          	*
 ***********************************************************************/
{
    Widget  draw_w;
    char    *hist_file=NULL, h_file[FILE_FULLSZ];
    char    img_path[256], dattim[20], bulletin[5010]; 
    char    mesg[] = "Unable to invoke AODT.\n\nPlease load an IR image!\n";
    int	    t_arry[5], length, jday, ignore, ignore2, ier2;
    int	    img_dat, img_tim, img_sat, one, irad, ii, jj, indx;
    int	    eye_typ, eyeSize, cloud_typ, ori_eye, ori_cloud, numx, numy;
    int	    hmods, hrecs;
    float   **temps, **lats, **lons;
    float   dx, dy, *ftmps, *flats, *flons;
    char    garea[64];
    int     data_size=105*105, lp, which;
    size_t  sofflt, soffpr;
    Boolean can_read, can_write;
    nmpstr_t    mapDrp, projDrp, gareaDrp[2];
/*---------------------------------------------------------------------*/
/*
 * initialize the return info.
 */
    *ier = 0;
    retmsg[0] = '\0';

    dataw_getIRInfo(dattim, img_path, &ier2);
    if ( ier2 != 0) {
        draw_w = (Widget)mcanvw_getDrawingW();
        NxmWarn_show (draw_w, mesg);
        *ier = -1;
        return;
    }

    eyeSize = -99;
    aodtv72_setmiscoptions (_landStrc.current, _searStrc.current, eyeSize);

/*
 * set history file.
 */
    aodtv72_initialize();
    h_file[0] = '\0';
    hist_file = XmTextFieldGetString (_histTxtW);

    if ( hist_file != NULL && hist_file[0] != '\0' ) {

         cst_rmbl ( hist_file, hist_file, &length, &ier2 );
	 strcpy ( h_file, _hist_dir );
         strcat ( h_file, hist_file );

         if ( h_file[0] != '\0' ) {

	      *ier = aodtv72_sethistoryfile(h_file);

              if ( *ier < 0 ) {

		aodtv72_qmessage( *ier, 0, h_file, retmsg);
                if ( hist_file != NULL ) XtFree ( hist_file );
                return;
              }
         }
    }

    if ( hist_file != NULL ) XtFree ( hist_file );

/*
 * set satellite date/time info in AODT library.
 */
    ti_ctoi (dattim, t_arry, &ier2, strlen(dattim) );
    ti_itoj (t_arry, &ignore, &jday, &ier2);

    img_dat = t_arry[0] * 1000 + jday;
    img_tim = t_arry[3] * 10000  + t_arry[4] * 100;

    img_sat = -1;
    aodtv72_setIRimageinfo ( img_dat, img_tim, img_sat );

    lp = loop_getCurLoop();
    nmp_gmapattr(lp, mapDrp, projDrp, gareaDrp, &ier2);

    if (strlen(gareaDrp[1]) > (size_t)0) {
	which = 1;
    } else {
	which = 0;
    }

    strcpy(garea, gareaDrp[which]);

/* 
 * get MANUAL curson location from GUI.
 */
    if ( _currLat > 90.0F  ||  _currLat < -90.0F  ||
	 _currLon > 180.0F ||  _currLon <= -180.0F   ) {

         *ier = -500;
         strcpy ( retmsg, "Invalid lat/lon values!" );
         return;
    } 

    *ier = aodtv72_setlocation(_currLat, _currLon*(-1.0F), 0);
    if ( *ier < 0 ) {

	 aodtv72_qmessage( *ier, 0, NULL, retmsg);
         return;
    }

/*
 * set oceanic domain flag in AODT library.
 */
    *ier = aodtv72_setdomain (_domStrc.current);
    if ( *ier < 0 ) {

	 aodtv72_qmessage( *ier, 0, NULL, retmsg);
         return;
    }

/*
 * Convert center (lat,lon) into device coordinates.
 */
    one = 1;
    gtrans ( sys_M, sys_D, &one, &_currLat, &_currLon, &dx, &dy, 
             &ier2, strlen(sys_M), strlen(sys_D) );

/*
 * Allocate memory for the data arrays.
 */
    G_MALLOC ( ftmps, float, data_size, "ftmps" );
    G_MALLOC ( flats, float, data_size, "flats" );
    G_MALLOC ( flons, float, data_size, "flons" );

/*
 * Retrieve temperatures from image.
 */
    irad = (int)RAD / 4 + 5;
    numx = numy = irad * 2 + 1;

    im_gtmp ( img_path, garea, sys_M, &_currLat, &_currLon, &irad, 
	      &numx, &numy, ftmps, flats, flons, &ier2, 
	      strlen(img_path), strlen(garea), strlen(sys_D) );

    if ( ier2 != 0 )  {

         *ier = -500;
         strcpy ( retmsg, "Failed to get temperatures from image!");
         return;
    }

    sofflt = sizeof(float);
    soffpr = sizeof(float*);
    temps = (float **)calloc((size_t)DIM, soffpr);
    lats  = (float **)calloc((size_t)DIM, soffpr);
    lons  = (float **)calloc((size_t)DIM, soffpr);
    for ( jj = 0; jj < DIM; jj++ ) {
        temps[jj] = (float *)calloc((size_t)DIM, sofflt);
        lats[jj]  = (float *)calloc((size_t)DIM, sofflt);
        lons[jj]  = (float *)calloc((size_t)DIM, sofflt);
    }

/*
 *  Move local one-dim arrays into two-dim arrays.
 */
    for ( jj = 0; jj < numy; jj++ )  {

	for ( ii = 0; ii < numx; ii++ )  {

	    indx = jj * numx + ii;
	    temps[jj][ii] = ftmps[indx];
	    lats [jj][ii] = flats[indx];
	    lons [jj][ii] = flons[indx] *= -1.0F;

	}
    }

/*
 *  Free up all malloc'd memory.
 */
    G_FREE ( flons, float );
    G_FREE ( flats, float );
    G_FREE ( ftmps, float );

/*
 * set satellite image data array in AODT library.
 */
    *ier = aodtv72_loadIRimage(temps, lats, lons, numx, numy);

/* free memory */

    for ( jj = 0; jj < DIM; jj++ ) {
	free(temps[jj]);
	free(lats[jj]);
	free(lons[jj]);
    }

    free(temps);
    free(lats);
    free(lons);

/* 
 * set eye and cloud temp values in AODT library.
 */ 
    *ier = aodtv72_seteyecloudtemp( );
    if ( *ier < 0 ) {
	aodtv72_qmessage( *ier, 0, NULL, retmsg);
	return;
    } 

/* 
 * determine scene type.
 */
    *ier = aodtv72_scenetype( );
    if ( *ier < 0 ) {

	 aodtv72_qmessage( *ier, 0, NULL, retmsg);
         return;
    }

/* 
 * override scene types, if desired by the user.
 */
    if( _typeStrc.current > 0 ) {
	aodtw72_getSceneTypes (_typeStrc.current, &eye_typ, &cloud_typ);
	aodtv72_getscenetypes  (&ori_eye, &ori_cloud, &ignore, &ignore2);
	aodtv72_setscenetypes(eye_typ, cloud_typ, ori_eye, ori_cloud);
    }

/* 
 * determine intensity 
 */
    *ier = aodtv72_intensity( );
    if ( *ier < 0 ) {
	aodtv72_qmessage( *ier, 0, NULL, retmsg);
	return;
    }

/* 
 * print AODT intensity estimate in message display area.
 */
    bulletin[0] = '\0';
    aodtv72_bulletinoutput(bulletin);
    if ( bulletin[0] == '\0' ) {

         strcpy ( bulletin, 
	    "Calculation completed, with no message out.");
    }

    strcat  (bulletin, "\n\n" );

    XtVaSetValues (_mesgTxtW,
                   XmNforeground,           _defFg,
                   XmNbackground,           _defBg,
           	   NULL);

    XmTextSetString( _mesgTxtW, bulletin );

/*
 * If history file is specified, write output to history file.
 */
    if( h_file[0] != '\0' ) {

/*
 * Check write permission.
 */
      cfl_perms ( h_file, &can_read, &can_write, &ier2 );
      if ( !can_write ) {
          draw_w = (Widget)mcanvw_getDrawingW();
          NxmWarn_show (draw_w, "No permission to write!\n Or invalid directory!");
	  *ier = -1;
          return;
      }

/* 
 * insert current intensity analysis into history file.
 */
      *ier = aodtv72_historyrecordinsert(&hmods,&hrecs);
      if ( *ier < 0 ) {

         aodtv72_qmessage( *ier, hrecs-hmods+1, h_file, retmsg);
      }

/* 
 * write updated history records to file.
 */

      *ier = aodtv72_historywritefile(&hrecs);
      if ( *ier < 0 ) { 

         aodtv72_qmessage( *ier, hrecs, h_file, retmsg);
      }

      aodtw72_setFileMenu( );

    }
}

/*=====================================================================*/

void aodtw72_freeRes ( void )
/************************************************************************
 * aodtw72_freeRes							*
 *                                                                      *
 * This function frees the resource back to OS at the end of AODT run.	*
 *									*
 * void aodtw72_freeRes ( )						*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 *			NONE						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC 	04/04		initial coding			*
 * M. Li/SAIC           06/05           From aodtw_freeRes              *
 * M. Li/SAIC        	01/07   	From aodtw64_                  	*
 ***********************************************************************/
{
    aodtv72_freememory();
}

/*=====================================================================*/

void aodtw72_saveMesg ( void )
/************************************************************************
 * aodtw72_saveMesg							*
 *                                                                      *
 * This function saves AODT text message.				*
 *                                                                      *
 * void aodtw72_saveMesg ( )						*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 *			NONE						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA	01/04		initial coding			*
 * M. Li/SAIC           06/05           From aodtw_saveMesg             *
 * M. Li/SAIC        	01/07   	From aodtw64_                  	*
 ***********************************************************************/
{
    char	*text=NULL, *fname=NULL, f_name[64];
    int		length, ier;
    FILE	*fp;
/*---------------------------------------------------------------------*/

    fname = XmTextFieldGetString (_fnameTxtW);

    if ( fname == NULL || fname[0] == '\0' ) {

	 NxmWarn_show(_aodtwWin, "Please specify a file name!");
	 if ( fname != NULL ) XtFree(fname);
         return;
    }

    strcpy ( f_name, fname );
    XtFree(fname);
    cst_rmbl ( f_name, f_name, &length, &ier );

    if ( f_name[0] == '\0' ) {

	 NxmWarn_show(_aodtwWin, "Please specify a file name!");
         return;
    }


    text  = XmTextGetString (_mesgTxtW);

    if ( text == NULL || text[0] == '\0' ) {

	 NxmWarn_show(_aodtwWin, "There is no message to Save!");
	 if ( text != NULL ) XtFree(text);
         return;
    }

/*
 * Save AODT messge text.
 */
    fp = fopen (f_name, "w");

    if ( fp == (FILE *) NULL) {

	 NxmWarn_show (_aodtwWin, "Failed to create the file!\n");
	 return;
    }

    fputs (text, fp);
    fclose (fp);

    XtFree(text);
}

/*=====================================================================*/

void aodtw72_prntMesg ( void )
/************************************************************************
 * aodtw72_saveMesg							*
 *                                                                      *
 * This function prints AODT text message.				*
 *                                                                      *
 * void aodtw72_prntMesg ( )						*
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 *			NONE						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC 	06/04		initial coding			*
 * M. Li/SAIC           06/05           From aodtw_prntMesg             *
 * M. Li/SAIC        	01/07   	From aodtw64_                  	*
 ***********************************************************************/
{
    char	*text=NULL, f_name[64];
    FILE	*fp;
/*---------------------------------------------------------------------*/
/*
 * Get message text.
 */
    text  = XmTextGetString (_mesgTxtW);

    if ( text == NULL || text[0] == '\0' ) {

	 NxmWarn_show(_aodtwWin, "There is no message to print!");
	 if ( text != NULL ) XtFree(text);
         return;
    }

/*
 * Save AODT messge text into a temporary file.
 */
    strcpy (f_name, "mesg_print_000001.txt");

    fp = fopen (f_name, "w+");

    if ( fp == (FILE *) NULL) {

	 NxmWarn_show (_aodtwWin, "Failed to create the file!\n");
	 return;
    }

    fputs (text, fp);
    fclose (fp);

/*
 * Free the message and open up the printer selection window.
 */
    XtFree(text);
    NxmPrt_txtPrtShow(_aodtwWin, f_name);
}

/*=====================================================================*/

void aodtw72_saveRecs ( Boolean save_all )
/************************************************************************
 * aodtw72_saveRecs							*
 *                                                                      *
 * This function saves all or selected history records.			*
 *                                                                      *
 * void aodtw72_saveRecs ( save_all )					*
 *                                                                      *
 * Input  parameters:                                                   *
 *	save_all	Boolean	whether or not to save all records	*
 * Output parameters:                                                   *
 *			NONE						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC 	05/04		initial coding			*
 * M. Li/SAIC           06/05           From aodtw_saveRecs             *
 * M. Li/SAIC        	01/07   	From aodtw64_                  	*
 ***********************************************************************/
{
    char	*fname=NULL, *ptext=NULL, f_name[64];
    int		nitems, length, ii, ier;
    FILE	*fp;
    XmString	*items;
/*---------------------------------------------------------------------*/

    fname = XmTextFieldGetString (_histNameW);

    if ( fname == NULL || fname[0] == '\0' ) {

	 NxmWarn_show(_aodtwHistWin, "Please specify a file name!");
	 if ( fname != NULL ) XtFree(fname);
         return;
    }

    strcpy ( f_name, fname );
    XtFree(fname);
    cst_rmbl ( f_name, f_name, &length, &ier );

    if ( f_name[0] == '\0' ) {

	 NxmWarn_show(_aodtwHistWin, "Please specify a file name!");
         return;
    }

/*
 * Get the items to save from the List.
 */
    if ( save_all ) {

       XtVaGetValues (_histLstW,
		      XmNitemCount,   &nitems,
		      XmNitems,       &items,
		      NULL                     );
    }
    else {

       XtVaGetValues (_histLstW,
		   XmNselectedItemCount,   &nitems,
		   XmNselectedItems,       &items,
		   NULL                             );
    }

    if ( nitems == 0 ) {

	 NxmWarn_show(_aodtwHistWin, "No items or no selected items to save!");
         return;
    }

/*
 * Open the file and get file descriptor.
 */
    fp = fopen (f_name, "w+");

    if ( fp == (FILE *) NULL) {

	 NxmWarn_show (_aodtwHistWin, "Failed to create the file!\n");
	 return;
    }

/*
 * Save all items or selected items.
 */
    for ( ii = 0; ii < nitems; ii++ ) {

	XmStringGetLtoR (items[ii], XmFONTLIST_DEFAULT_TAG, &ptext);
        fputs (ptext, fp);
        XtFree (ptext);
    }

    fclose (fp);
}

/*=====================================================================*/

void aodtw72_prntRecs ( Boolean prnt_all )
/************************************************************************
 * aodtw72_prntRecs							*
 *                                                                      *
 * This function prints all or selected history records.		*
 *                                                                      *
 * void aodtw72_prntRecs ( prnt_all )					*
 *                                                                      *
 * Input  parameters:                                                   *
 *	prnt_all	Boolean	whether or not to print all records	*
 * Output parameters:                                                   *
 *			NONE						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC 	06/04		initial coding			*
 * T. Piper/SAIC	09/04	Added column labels for history print	*
 * M. Li/SAIC		06/05	Modified header format			*
 * M. Li/SAIC		07/05	Added comment line			*
 * M. Li/SAIC        	01/07   From aodtw64_                  		*
 ***********************************************************************/
{
    char	*ptext=NULL, f_name[64], string[256];
    int		nitems, ii;
    FILE	*fp;
    XmString	*items;
/*---------------------------------------------------------------------*/
/*
 * Get the items to save from the List.
 */
    if ( prnt_all ) {

       XtVaGetValues (_histLstW,
		      XmNitemCount,   &nitems,
		      XmNitems,       &items,
		      NULL                     );
    }
    else {

       XtVaGetValues (_histLstW,
		   XmNselectedItemCount,   &nitems,
		   XmNselectedItems,       &items,
		   NULL                             );
    }

    if ( nitems == 0 ) {

	 NxmWarn_show(_aodtwHistWin, "No items or no selected items to print!");
         return;
    }

/*
 * Open the temporary file and get file descriptor.
 */
    strcpy (f_name, "history_print_000001.txt");

    fp = fopen (f_name, "w+");

    if ( fp == (FILE *) NULL) {

	 NxmWarn_show (_aodtwHistWin, "Failed to create the file!\n");
	 return;
    }

/*
 * Save all items or selected items to the temporary file.
 */
    sprintf(string,"                  --------Intensity-------  ---Tno Values--  -Tno/CI Rules-  -Temperature-                 \n");
    fputs (string, fp);
    sprintf(string,"           Time        Final/MSLPLat/Vmax   6hr 3hr Adj Ini   Cnstrnt  Wkng   Eye    Mean   Scene  EstRMW   Storm Location  Fix\n");
    fputs (string, fp);
    sprintf(string,"   Date    (UTC)   CI  MSLP /BiasAdj/(kts)  Ave Ave Raw Raw    Limit   Flag  Region  Cloud  Type    (km)     Lat     Lon    Mthd\n");
    fputs (string, fp);
    for ( ii = 0; ii < nitems; ii++ ) {

	XmStringGetLtoR (items[ii], XmFONTLIST_DEFAULT_TAG, &ptext);
	strncpy(string, ptext, 128);
	fputs (string, fp);

       	if ( strlen(ptext) > 131 ) {
    	    sprintf(string,"                   Comment: %s\n", ptext+131 );
	}
	else {
	    sprintf(string,"                   Comment: \n" ); 
	}
	XtFree (ptext);
	fputs (string, fp);
    }

    fclose (fp);

/*
 * Open up the printer selection window.
 */
    NxmPrt_txtPrtShow(_aodtwHistWin, f_name);
}

/*=====================================================================*/

void aodtw72_getSceneTypes ( int user_pick, int* eye_typ, int* cloud_typ )
/************************************************************************
 * aodtw72_getSceneTypes						*
 *                                                                      *
 * This function figures out eye type and cloud type based on user      *
 * choice on Scene Type option menu.					*
 *                                                                      *
 * void aodtw72_getSceneTypes ( user_pick, eye_typ, cloud_typ )		*
 *                                                                      *
 * Input  parameters:                                                   *
 *	user_pick	int	user choice				*
 * Output parameters:                                                   *
 *	eye_typ		int*	eye type				*
 *	cloud_typ	int*	cloud type				*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           12/06						*
 * M. Li/SAIC        	01/07   From aodtw64_                  		*
 ***********************************************************************/
{
    if ( user_pick <= 0 || user_pick > 8 ) {
         *eye_typ   = -1; 
	 *cloud_typ = -1;
    }
    else if ( user_pick > 0 && user_pick < 5 ) {
	*eye_typ   = user_pick - 1;
	*cloud_typ = 0;
    }
    else if ( user_pick >= 5 && user_pick <= 8 ) {
	*eye_typ   = 3;
	*cloud_typ = user_pick - 4;
    } 
}

/*=====================================================================*/

void aodtw72_setHist ( Boolean set_file )
/************************************************************************
 * aodtw72_setHist							*
 *                                                                      *
 * This function sets the history list on AODT History File Management  *
 * window.								*
 *                                                                      *
 * void aodtw72_setHist ( )						*
 *                                                                      *
 * Input  parameters:                                                   *
 *   set_file	Boolean	whether or not to set history file		*	
 * Output parameters:                                                   *
 *		NONE							*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC 	05/04		initial coding			*
 * H. Zeng/SAIC		06/04		changed display format		*
 * M. Li/SAIC           06/05           From aodtw_setHist              *
 * M. Li/SAIC           10/05           Relocated aodtv72_initialize    *
 * M. Li/SAIC		03/06		Added history file directory	*
 * M. Li/SAIC        	01/07   	From aodtw64_                 	*
 ***********************************************************************/
{
    char    *hist_file=NULL, h_file[FILE_FULLSZ], line[MAXLINE], srcID[512], strmID[512];
    int	    hist_count, nitems, length, idx, ier2, itype;
    XmString            *xm_items;
    FILE                *fp;
    struct odtdata      *historyrec;
/*---------------------------------------------------------------------*/
/*
 * GUI preparations.
 */
    XtVaGetValues (_histLstW,
		   XmNitemCount,           &nitems,
		   NULL                             );

    if ( nitems > 0 ) {

         XmListDeleteAllItems(_histLstW);
    }

    XtSetSensitive (_histForm, FALSE);
    XtSetSensitive (_runBtnW , FALSE);

/*
 * Check the set file flag to see if we need to set history file.
 */
    if ( set_file == TRUE ) {


       aodtv72_initialize();
       h_file[0] = '\0';
       hist_file = XmTextFieldGetString (_histTxtW);

       if ( hist_file != NULL && hist_file[0] != '\0' ) {

	    cst_rmbl ( hist_file, hist_file, &length, &ier2 );
            strcpy ( h_file, _hist_dir );
            strcat ( h_file, hist_file );

            if ( h_file[0] != '\0' ) {

	         ier2 = aodtv72_sethistoryfile(h_file);

                 if ( ier2 < 0 ) {

                   if ( hist_file != NULL ) XtFree ( hist_file );
                   return;
                 }
            }
       }

       if ( hist_file != NULL ) XtFree ( hist_file );

       if ( h_file[0] == '\0' ) return;

    } /* the end of if ( set_file == TRUE )... */

/*
 * Get the history file name from the AODT library.
 */
    h_file[0] = '\0';
    aodtv72_gethistoryfile (h_file);

    if ( h_file[0] == '\0' ) return;

/*
 * Get file descriptor.
 */
    fp=fopen( h_file, "r" );
    if ( fp == NULL ) return;

/*
 * Get total # of history records.
 */
    hist_count = 0;
    while( fgets(line,MAXLINE,fp) != NULL )  hist_count++;

    if ( hist_count == 0 ) {

         fclose(fp);
         return;
    }

/*
 * Set the date/time for history record display purpose.
 */
    aodtv72_setdatetime(0, 0, "", "", G_FALSE);

/*
 * Set history records on History File Management Window.
 */
    xm_items = (XmString*) XtMalloc ( hist_count * sizeof (XmString) );
    idx = 0;

    aodtv72_historygetnextrec(0,&historyrec);

    while( historyrec != NULL ) {
       
       itype = -1;
       srcID[0] = CHNULL;
       aodtv72_historylistfmt(historyrec, itype, srcID, strmID, line);
       line[strlen(line)-1] = ' ';
       strcat (line, " ");
   
       xm_items[idx] = XmStringCreateLocalized ( line );
       idx++;

/* 
 * obtain pointer to next record in history file data structure.
 */
       aodtv72_historygetnextrec( 1, &historyrec );

    } /* the end of while(... */

    XmListAddItems (_histLstW, xm_items, hist_count, 1);

    for (idx = 0; idx < hist_count; idx++) XmStringFree (xm_items[idx]);    
    XtFree ((XtPointer) xm_items);

    fclose(fp);
}

/*=====================================================================*/

void aodtw72_delHist ( void )
/************************************************************************
 * aodtw72_delHist							*
 *									*
 * The function deletes the selected records on History Management      *
 * Window.								*
 *									*
 * void aodtw72_delHist ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		05/04	initial coding				*
 * M. Li/SAIC           06/05   From aodtw_delHist                      *
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
    int	       nitems, time, ii, jj, hdels, hmods, hrecs, ier;
    char       *ptext=NULL, cdate[12];
    XmString   *items;
/*---------------------------------------------------------------------*/

    XtVaGetValues (_histLstW,
		   XmNselectedItemCount,   &nitems,
		   XmNselectedItems,       &items,
		   NULL                             );

    for ( ii = nitems-1; ii >= 0; ii-- ) {

	XmStringGetLtoR (items[ii], XmFONTLIST_DEFAULT_TAG, &ptext);

        jj  = sscanf( ptext,"%s %d", cdate, &time );
        if ( jj != 2 ) continue;

        ier = aodtv72_datetime(time, time, cdate, cdate, TRUE);
        if ( ier != 0 ) continue;

        ier = aodtv72_historydeleterec( &hdels, &hmods );

        XtFree (ptext);

    }

/* 
 * write updated history records to file.
 */
    ier = aodtv72_historywritefile(&hrecs);

/*
 * Set the updated history records on GUI.
 */
    aodtw72_setHist( FALSE );

}

/*=====================================================================*/

/* ARGSUSED */
void aodtw72_pushBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * aodtw72_pushBtnCb							*
 *									*
 * Callback function for push buttons on AODT popup window.		*
 *									*
 * void aodtw72_pushBtnCb (wid, which, call)				*
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
 * H. Zeng/XTRIA	01/04   initial coding				*
 * H. Zeng/SAIC		04/04	modified "Clear" btn function		*
 * H. Zeng/SAIC		06/04	added printing functionality		*
 * M. Li/SAIC           06/05   From aodtw_pushBtnCb                    *
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
    int		ier;
    char	retmsg[500];
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* Click Loc */

        pgpalw_classPopdown();
        mcanvw_setDynamicFunc ( (XtEventHandler)&aodtw72_pointerEh,
				(XtEventHandler)NULL,
                                (XtEventHandler)NULL, CURS_POINT_SELECT);
  
	break;

      case 1:	/* Run AODT */

        /*
         * Signal the starting of AODT algorithm run.
         */
        XtVaSetValues (_mesgTxtW,
                   XmNforeground,           _defFg,
                   XmNbackground,           _defBg,
           	   NULL);

        XmTextSetString( _mesgTxtW,
	      "Calculation in process, please wait.........");	
	XmUpdateDisplay ( _mesgTxtW );


        retmsg[0] = '\0';
        aodtw72_runAODT ( &ier, retmsg );

        if ( ier < 0 ) {

	     if ( retmsg[0] == '\0' ) {

	          strcpy (retmsg, 
		     "Calculation aborted, error occured somewhere!");
             }

             strcat  (retmsg, "\n\n" );

             XtVaSetValues (_mesgTxtW,
                      XmNforeground,           _errorFg,
                      XmNbackground,           _errorBg,
           	      NULL);

             XmTextSetString( _mesgTxtW, retmsg );
        }


        /*
         * Free the resource back to OS at the end of AODT run.
         */
        aodtw72_freeRes ();

	break;

      case 2:	/* Print... */

        aodtw72_prntMesg ( );

	break;

      case 3:	/* Save to  */

        aodtw72_saveMesg ( );

	break;

      case 4:	/* Clear    */

        XmTextSetString(_histTxtW,  "\0");
        XmTextSetString(_mesgTxtW,  "\0");
        XmTextSetString(_fnameTxtW, "\0");

        XtVaSetValues (_mesgTxtW,
                   XmNforeground,           _defFg,
                   XmNbackground,           _defBg,
           	   NULL);

	break;

    }

}

/*=====================================================================*/

/* ARGSUSED */
void aodtw72_histPushBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * aodtw72_histPushBtnCb						*
 *									*
 * Callback function for push buttons on AODT history management window.*
 *									*
 * void aodtw72_histPushBtnCb (wid, which, call)			*
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
 * H. Zeng/SAIC		05/04	initial coding				*
 * H. Zeng/SAIC		06/04	added print functionality		*
 * M. Li/SAIC           06/05   From aodtw_histPushBtnCb                *
 * M. Li/SAIC		07/05	Set comment frame off after deleting	*
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* Print All */

        aodtw72_prntRecs ( TRUE );

	break;

      case 1:	/* Print Selected */

        aodtw72_prntRecs ( FALSE );

	break;

      case 2:	/* Save All */

        aodtw72_saveRecs ( TRUE );

	break;

      case 3:	/* Save Selected */

        aodtw72_saveRecs ( FALSE );

	break;

      case 4:	/* Delete Selected and... */

        aodtw72_delHist();
	if ( XtIsSensitive(_commentW) ) aodtw72_commFrameOff();

	break;

    }

}

/*=====================================================================*/

/* ARGSUSED */
void aodtw72_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * aodtw72_ctlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of AODT popup	*
 * window.								*
 *									*
 * void aodtw72_ctlBtnCb (wid, which, call)				*
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
 * H. Zeng/SAIC		04/04	minor modifications			*
 * M. Li/SAIC           06/05   From aodtw_ctlBtnCb                     *
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
   
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* History File Management */

        if ( !aodtw72_histIsUp() ) {

	      aodtw72_setHist   ( TRUE );
              aodtw72_histPopup ();
        }
        else {

	      XtUnmanageChild (_aodtwHistWin);
              XtManageChild   (_aodtwHistWin);
        }

	break;

      case 1:	/* Close */

	aodtw72_popdown ();

	break;

    }

}

/*=====================================================================*/

/* ARGSUSED */
void aodtw72_histCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * aodtw72_histCtlBtnCb							*
 *									*
 * Callback function for control buttons at the bottom of AODT Hist.	*
 * File Management window.						*
 *									*
 * void aodtw72_histCtlBtnCb (wid, which, call)				*
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
 * M. Li/SAIC           06/05   From aodtw_histCtlBtnCb                 *
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    switch(which) {

      case 0:	/* Close */
        aodtw72_histPopdown();  

	break;

    } /* the end of switch(which) */

}

/*=====================================================================*/

/* ARGSUSED */
void aodtw72_menuTextCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * aodtw72_menuTextCb							*
 *									*
 * Callback function for text field and general option buttons.		*
 *									*
 * void aodtw72_menuTextCb (wid, which, call)				*
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
 * H. Zeng/SAIC 	04/04	copied from pgwfmt_menuTextCb()		*
 * M. Li/SAIC           06/05   Append "_64"				*
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
    char	*ptext=NULL;
    XmString	xmstr;
    XtPointer	userdata;
    Widget	*twid;
/*---------------------------------------------------------------------*/

    /*
     * textfield callback
     */

    switch(which) {

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

	strcat ( ptext, "_72" );
	twid = (Widget *) userdata;     
        XmTextSetString(*twid, ptext);

	XtFree (ptext);
        break;    


    } /* the end of switch */

}

/*=====================================================================*/

Widget aodtw72_createMenuText ( Widget parent, char *labelstr, int ncol, 
			       int textoff, int info_type, 
			       Widget *textwid, Widget *btnwid )
/************************************************************************
 * aodtw72_createMenuText						*
 *									*
 * Creates a labeled text field widget with a pulldown menu.		*
 *									*
 * Widget aodtw72_createMenuText ( parent, labelstr, ncol, textoff, 	*
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
 * aodtw72_createMenuText Widget	Widget ID of the form widget 	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC 	04/04	initial coding				*
 * H. Zeng/SAIC		01/05   added _histMenuB			*
 * M. Li/SAIC           06/05   From aodtw_createMenuText               *
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
    Widget	form, label, menub, cascade, menu, button;
    int		ii, iret, toff = 5;
    XmString	xmstr;
    Pixel	fg, bg;
    long	ignore;
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


    /*
     * Create pulldown menu buttons.
     */
    if ( info_type == HIST ) {

       for (ii = 0; ii < MXELE; ii++) {
	   xmstr = XmStringCreateLocalized ("xxxxxxx");
	   button = XtVaCreateManagedWidget ("tmbutton", 
				      xmPushButtonWidgetClass, menu, 
				      XmNlabelString,	       xmstr,
				      XmNuserData,	       *textwid,
				      NULL);
           if ( btnwid != NULL ) {
              *(btnwid+ii) = button;
           }

	   XmStringFree (xmstr);

	   _histMenuB = menub;
           _histMenu  = menu;
	   _histCasc  = cascade;
	
       }

    }  


    XtManageChild(menub);
    XtManageChild(form);
    return (form);

}

/*=====================================================================*/
/* ARGSUSED */
void aodtw72_histTxtCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * aodtw72_histTxtCb                                                   	*
 *                                                                      *
 * Callback function for history text field.   				*
 *                                                                      *
 * void aodtw72_histTxtCb (wid, which, call)                           	*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long		which button                            *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC         	05/05						* 
 * M. Li/SAIC           06/05   .hst -> .hst_64                         *
 * M. Li/SAIC		08/05	Not append ".hst_64" if nothing entered	*
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
    int		length, ipos, ier, ier1;
    char        *pstr, cptr[40], hist_txt[40];

/*---------------------------------------------------------------------*/
    pstr = XmTextGetString ( _histTxtW );
    strcpy ( hist_txt, pstr );
    XtFree (pstr);

    cst_rmbl ( hist_txt, hist_txt, &length, &ier ); 
    if ( length <= 0 ) return;

    cst_uclc ( hist_txt, cptr, &ier );
    cst_srch ( 0, length, ".hst_72", cptr, &ipos, &ier );

    if ( ier == -4 ) {
	cst_srch ( 0, length, ".hst", cptr, &ipos, &ier1 );
	if ( ier1 == 0 ) {
	    strcat ( hist_txt, "_72" );
	}
	else {
	    strcat ( hist_txt, ".hst_72" );
	}	

    	XmTextSetString ( _histTxtW, hist_txt );
    }

}

/*=====================================================================*/
/* ARGSUSED */
void aodtw72_histSelCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * aodtw72_histSelCb                             	              	*
 *                                                                      *
 * Callback function for history file selection.                        *
 *                                                                      *
 * void aodtw72_histSelCb (wid, which, call)                           	*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long		which button                            *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           07/05                                           * 
 * M. Li/SAIC		08/05	Removed extra blanks			*
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
    int        len, ier, nitems;
    char       *ptext = NULL, commtxt[51], temptxt[51];
    XmString   *items;
/*---------------------------------------------------------------------*/
    
    XtVaGetValues (_histLstW,
                   XmNselectedItemCount,   &nitems,
                   XmNselectedItems,       &items,
                   NULL                             );

    if ( nitems == 1 ) {
	XmStringGetLtoR (items[0], XmFONTLIST_DEFAULT_TAG, &ptext);
	if ( strlen(ptext) > 131 ) {
	    strcpy ( commtxt, ptext+131 );
	}
	else {
    	    commtxt[0] = '\0';
	}
	XtFree ( ptext );

        cst_rxbl ( commtxt, temptxt, &len, &ier );
	if ( len == 0 ) commtxt[0] = '\0';
	aodtw72_commFrameOn ( commtxt );
    }
    else {
	aodtw72_commFrameOff ();
    }
        
}

/*=====================================================================*/
/* ARGSUSED */
void aodtw72_commFrameOn ( char *comment )
/************************************************************************
 * aodtw72_commFrameOn                                                  *
 *                                                                      *
 * This function sensitizes the comment frame and loads the comment     *
 * text if there is already a comment.					*
 *                                                                      *
 * void aodtw72_commFrameOn ( comment )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      *comment	char	comment text				*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           07/05                                           * 
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( !XtIsSensitive(_commentW) ) XtSetSensitive ( _commentW, TRUE );
    XmTextFieldSetString ( _commTxtW, comment );
}

/*=====================================================================*/
/* ARGSUSED */
void aodtw72_commFrameOff ( void )
/************************************************************************
 * aodtw72_commFrameOff                                                 *
 *                                                                      *
 * This function de-sensitizes the comment frame and loads the comment  *
 * text box with a string that has been set to NULL.			*
 *                                                                      *
 * void aodtw72_commFrameOff ()                                 	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           07/05                                           * 
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    XmTextFieldSetString (_commTxtW, "\0");
    if ( XtIsSensitive(_commentW) ) XtSetSensitive ( _commentW, FALSE);
}

/*=====================================================================*/
/* ARGSUSED */
void aodtw72_commCtlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * aodtw72_commCtlBtnCb                                                 *
 *                                                                      *
 * Callback function for "Apply" comment.				*
 *                                                                      *
 * void aodtw72_commCtlBtnCb (wid, which, call)                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   long		which button                            *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           07/05						* 
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    aodtw72_commHist();

}

/*=====================================================================*/

void aodtw72_commHist ( void )
/************************************************************************
 * aodtw72_commHist                                                     *
 *                                                                      *
 * The function gets the comment text entered by the user, and append 	*
 * it to the history file record and to the file box line.		*
 *                                                                      *
 * void aodtw72_commHist ( )                                            *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           07/05						*
 * M. Li/SAIC		08/05	Added cst_rxbl				*
 * M. Li/SAIC        	01/07   From aodtw64_                 		*
 ***********************************************************************/
{
    int        ii, jj, nsel, nitems, modrec, hrecs, time, len, ier;
    char       *comm = NULL, *ptext=NULL, cdate[12];
    XmString   *items;
/*---------------------------------------------------------------------*/

    XtVaGetValues (_histLstW,
                   XmNselectedItemCount,   &nsel,
		   XmNitemCount,   	   &nitems,
                   XmNitems,       	   &items,
                   NULL                             );

    if ( nsel != 1 ) return;

    for ( ii = nitems-1; ii >= 0; ii-- ) {

        XmStringGetLtoR (items[ii], XmFONTLIST_DEFAULT_TAG, &ptext);

	if ( strlen ( ptext ) > 16 ) {
            jj  = sscanf( ptext,"%s %d", cdate, &time );
	}
	else {
	    continue;
	}
        if ( jj != 2 ) continue;
	XtFree (ptext);

        ier = aodtv72_datetime(time, time, cdate, cdate, FALSE);
        if ( ier != 0 ) continue;

	if (XmListPosSelected ( _histLstW, (ii+1) ) ) {
    	    comm = XmTextFieldGetString (_commTxtW);
	    cst_rxbl ( comm, comm, &len, &ier );
    	    ier = aodtv72_historyaddcomment ( comm, &modrec );
    	    XtFree ( comm );
	    break;
	}
    }

   /* 
    * write updated history records to file.
    */
    ier = aodtv72_historywritefile( &hrecs );

    /*
     * Set the updated history records on GUI.
     */
    aodtw72_setHist( FALSE );

    XmListSelectPos ( _histLstW, ii+1, FALSE );

}

/*=====================================================================*/
