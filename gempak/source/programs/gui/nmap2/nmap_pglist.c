#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "drwids.h"
#include "hints.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "proto_xw.h"

#define  MAXOFF          5000

#define  MAXSTN		3500	/* Should <= MAXHOT (3500) in clocmn.h */
#define  LIMIT_REACHED	1	/* Reached max. number of items (MAXLISTITEMS) */

#define  TOTAL_MARKER	21
#define  ICON_DIR	"$NAWIPS/icons/nmap"

#define MIN_SIZE_SCALE	 0.1F
#define MAX_SIZE_SCALE	10.0F

#define MIN_WIDTH_SCALE	 1
#define MAX_WIDTH_SCALE	10

#define ADD_ITEM	 0
#define DELETE_ITEM	 1
#define TOGGLE_ITEM	 2

#define	BNDS_FILE	"WBCMZ_BNDS"
#define	WBCMZ_TBL	"MZ_CNTY"
#define MAXFIPS         100 /* max. number of clustered FIPS codes */

static Widget		_list_dlgW;

static Widget		_mrkTypOptW;	/* marker type option menu */
static WidgetList	_mrkTypBtn;	/* marker type option buttons */
static int		_curMrkTyp = 1;	/* current marker type */

static Widget	  	_typClrForm;
static Widget	  	_listColrW;

static Widget		_list_modeW;
static Widget		_list_radioW[2];

static Widget		_list_mselW;
static Widget		_list_mselOptW[3];

static Widget		_lst_size_sldW;
static Widget		_lst_size_txtW;

static Widget		_lst_width_sldW;
static Widget		_lst_width_txtW;

static Widget	        _clusterRc;
static Widget		_ctlForm;
static Widget		_list_clstW;
static WidgetList 	_ctlBtns;

static float		_listSize = 1.0F;
static int		_listWdth = 3;
static int		_listColr = 0;

static int		_listMode = 0;		 /* default list-creating mode */
static int		_mselMode = TOGGLE_ITEM; /* default multi-selection mode */

static int		_subTyp;

static char		*_bndstr[] = { "CNTY_BNDS", "PFZ",
                                      "CWA_BNDS",  "STATE_BNDS",
				      "WBCMZ_BNDS"};
static char		_filnam[128];
static int	 	_clstStatus = 1;
static int	        _curListLoc = -1;

static VG_DBStruct	_curList; /* Current list element */

/*
 * Local Global Variables used for storing FIPS codes.
 */
static  int     _ncfips;
static  int     _cfips[MAXFIPS];

/*
 *  Private functions
 */
void pglist_crtMrkTyp ( Widget parent );
void pglist_crtSzWth ( Widget parent );

static void pglist_clstOptCb( Widget, long, XtPointer );
void pglist_MrkTypCb    ( Widget, long,  XtPointer );
void pglist_colorCb 	( Widget, XtPointer, XtPointer ); 
void pglist_sizeCb 	( Widget, XtPointer, XmScaleCallbackStruct *cbs ); 
void pglist_sizTxtCb 	( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs ); 
void pglist_widthCb 	( Widget, XtPointer, XmScaleCallbackStruct *cbs );
void pglist_widTxtCb 	( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs );
void pglist_radioBoxCb 	( Widget, long which, XtPointer ); 
void pglist_mselOptCb 	( Widget, long which, XtPointer );

static void pglist_setMode ( void );
static void pglist_selectLineEh ( Widget, XtPointer, XEvent*, Boolean* );
static void pglist_confirmItemsEh ( Widget, XtPointer, XEvent*, Boolean* );
static Boolean pglist_getClstStatus ( void );

static void pglist_addItem ( float lat, float lon, int *iret );
static void pglist_createItems ( void );
static void pglist_reset ( void );
static void pglist_saveList ( int np, float lat[], float lon[] );
static void pglist_setHints ( void );

static void pglist_selectSingle ( float lat, float lon ); 
static void pglist_selectArea ( int np, const float lat[], const float lon[] ); 
static void pglist_selectDone ( void ); 

/************************************************************************
 * nmap_pglist.c							*
 *									*
 * This module creates and displays the VG LIST setting box. It also	*
 * contains the callbacks for the box.					*
 *									*
 * CONTENTS:								*
 *  pglist_create()	creates list attribute window			*
 *  pglist_crtMrkTyp()  creates list marker type selection		*
 *  pglist_crtSzWth()	creates list size/width input panel		*
 *  pglist_popup()	popup list attribute window			*
 *  pglist_popdown()	pop down the list attribute window		*
 *									*
 *  pglist_addItem()	add a new item into the current list		*
 *  pglist_setAttr()	set the list attributes				*
 *  pglist_saveAttr()	save the list attributes to the setting table	*
 *  pglist_getAttr()	get the list attributes				*
 *  pglist_isUp()	querry whether the list window is up		*
 *  pglist_getMode()	get the current list-creating mode		*
 *  pglist_setMode()	set up the list-creating mode			*
 *  pglist_selectLineEh() selec a closed line for creating a new list	*
 *  pglist_confirmItemsEh confirm using the selected line to create list	*
 *  pglist_createItems()create items from the selected line		*
 *									*
 *  pglist_getClstStatus() get current clustering status		*
 *  pglist_createWtchLst() create a LIST from a watch			*
 *  pglist_grpToWtch()	   group a watch List to GRPTYP_WATCH		*
 *  pglist_wrtLstWtch()    write a watch and its associated list	*
 *									*
 *  pglist_reset()	reset to draw a new list			*
 *  pglist_saveList()	save the current active list			*
 *  pglist_selectSingle	process single point selection			*
 *  pglist_selectArea()	process area selection by drag/shift+click	*
 *  pglist_selectDone()	clean up when selection is done			*
 *  pglist_setHints()	set mouse hints at the bottom of the window	*
 *									*
 *  pglist_mrkTypCb()	Callback for list marker type widget		*
 *  pglist_colorCb()    callback for color button        		*
 *  pglist_sizeCb()	Callback for list size scale			*
 *  pglist_sizeTxtCb()	Callback for list size text widget		*
 *  pglist_widthCb()	Callback for list width scale			*
 *  pglist_widTxtCb()	Callback for list width text widget		*
 *  pglist_radioBoxCb()	Callback for list mode radio box widget		*
 *  pglist_clstOptCb	Callback for the clusteing radio buttons	*
 *  pglist_mselCb()	Callback for multi-selection mode radio boxes	*
 *									*
 *  pglist_updtStatusCntys  update WCN active counties in the watch	*
 *									*
 ***********************************************************************/

/*=====================================================================*/

void pglist_create ( Widget parent )
/************************************************************************
 * pglist_create							*
 *									*
 * This function creates a LIST attribute selection box.		*
 *									*
 * void pglist_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC           11/02   initial coding				*
 * J. Wu/SAIC           11/02   add color selection widget		*
 * A. Hardy/NCEP	 2/04   added clustering buttons		*
 * J. Wu/SAIC           04/04   add multi-selection mode panel		*
 * E. Safford/SAIC	05/05	free clst_pbs				*
 ***********************************************************************/
{
    Widget	clst_labl, pane, rc, list_rdbox, mode_label;
    WidgetList	clst_pbs;

    XmString	xmstr;
    long	ii, nn;
    char	*modestr[] = {"Clicking", "Selecting another\nobject"};
    char	*mselstr[] = {"Add to List", "Delete from List", 
                            "Toggle List Members"};
    char	*btnstr[] = {"Apply", "Cancel"};

    char	*clst_str[] = { "Off", "On" };
/*---------------------------------------------------------------------*/
    
    /*
     * Create the VG List selection box.
     */
    _list_dlgW = XmCreateFormDialog ( parent, "list_edit",
				       NULL, 0 );
    xmstr = XmStringCreateLocalized("List Attributes");

    XtVaSetValues ( _list_dlgW,
		 XmNnoResize,			TRUE,
		 XmNautoUnmanage,		FALSE,
    		 XmNdialogTitle, 		xmstr,
		 NULL);

    XmStringFree ( xmstr );    

    pane = XtVaCreateManagedWidget ( "pane",
		xmPanedWindowWidgetClass,	_list_dlgW,
		XmNsashWidth,			1,
		XmNsashHeight,			1,
		NULL );

    rc = XtVaCreateWidget ( "rc1",
		xmRowColumnWidgetClass,		pane,
		XmNorientation,			XmVERTICAL,
		XmNspacing,			10,
		XmNradioAlwaysOne,		FALSE,
		NULL );

    /*
     * create marker type selection area
     */
    pglist_crtMrkTyp ( rc );


    /*
     * create color widget
     */
    _listColrW	= XtVaCreateManagedWidget (" ",
		xmPushButtonWidgetClass,	_typClrForm,
		XmNwidth,			25,
		XmNheight,			20,
         	XmNtopAttachment,       	XmATTACH_FORM,
         	XmNtopOffset,           	10,
         	XmNleftAttachment,       	XmATTACH_WIDGET,
         	XmNleftWidget,           	_mrkTypOptW,
         	XmNleftOffset,           	20,
		NULL );

    XtAddCallback ( _listColrW, XmNactivateCallback, 
    			(XtCallbackProc)pglist_colorCb, NULL );


    /*
     * create width and size selection area
     */
    pglist_crtSzWth ( rc );
        

    /*
     * create list-creating mode form.
     */
    _list_modeW = XtVaCreateManagedWidget ( "list_modeW",
		xmFormWidgetClass,	pane,
		NULL );
    
    mode_label  = XtVaCreateManagedWidget (" Create List By:",
	 	xmLabelGadgetClass,	_list_modeW,
         	XmNtopAttachment,       XmATTACH_FORM,
         	XmNtopOffset,       	8,
	 	NULL ); 
    
    list_rdbox = XtVaCreateManagedWidget ( "list_rdboxW",
                xmRowColumnWidgetClass,	_list_modeW,
                XmNpacking,             XmPACK_TIGHT,
         	XmNtopAttachment,       XmATTACH_WIDGET,
         	XmNtopWidget,           mode_label,
         	XmNtopOffset,       	5,
                XmNorientation,         XmVERTICAL,
		XmNradioBehavior,       True,
                XmNtraversalOn,         False,
                XmNmarginHeight,	2, 
                XmNmarginWidth,		20, 
                XmNspacing,         	2,
                NULL);

    for ( ii = 0; ii < (long)XtNumber(modestr); ii++ ){
	_list_radioW[ii] = XtVaCreateManagedWidget ( modestr[ii],
                xmToggleButtonGadgetClass,	list_rdbox,
                NULL);
        XtAddCallback ( _list_radioW[ii], XmNarmCallback,
		        (XtCallbackProc)pglist_radioBoxCb, (XtPointer) ii );
    }	

    /*
     * create cluster radio buttons
     */

     _list_clstW = XtVaCreateManagedWidget ( "list_clstW",
     		xmFormWidgetClass,	pane,
		NULL );
    clst_labl = XtVaCreateManagedWidget ("Clustering:",
			   xmLabelWidgetClass, _list_clstW,
			   XmNtopAttachment,       XmATTACH_FORM,
			   XmNtopOffset,       	8,
			   NULL );

    _clusterRc = XtVaCreateManagedWidget ("pglist_clstrRc",
				xmRowColumnWidgetClass, _list_clstW,
				XmNleftAttachment,      XmATTACH_WIDGET,
				XmNleftWidget,          clst_labl,
				XmNorientation,         XmHORIZONTAL,
				XmNpacking,             XmPACK_TIGHT,
				XmNradioBehavior,       TRUE,
				NULL );

    nn = XtNumber ( clst_str );
    clst_pbs = (WidgetList)XtMalloc( nn * sizeof(Widget));
    for ( ii = 0; ii < nn; ii++ )  {
	clst_pbs[ii] = XtVaCreateManagedWidget (clst_str[ii],
			    xmToggleButtonWidgetClass, _clusterRc,
			    XmNtraversalOn,            FALSE,
			    XmNset,                    (ii == (long)_clstStatus),
			    NULL );
      
	XtAddCallback ( clst_pbs[ii], XmNarmCallback,
			(XtCallbackProc)pglist_clstOptCb, (XtPointer)ii);

    }


    /*
     * create multiple selection buttons.
     */
    _list_mselW = XtVaCreateManagedWidget ( "list_mselW",
		xmFormWidgetClass,	pane,
		NULL );
    
    mode_label  = XtVaCreateManagedWidget (" Multi-Selection Action:",
	 	xmLabelGadgetClass,	_list_mselW,
         	XmNtopAttachment,       XmATTACH_FORM,
         	XmNtopOffset,       	8,
	 	NULL ); 
    
    list_rdbox = XtVaCreateManagedWidget ( "list_mselW",
                xmRowColumnWidgetClass,	_list_mselW,
                XmNpacking,             XmPACK_TIGHT,
         	XmNtopAttachment,       XmATTACH_WIDGET,
         	XmNtopWidget,           mode_label,
         	XmNtopOffset,       	5,
                XmNorientation,         XmVERTICAL,
		XmNradioBehavior,       True,
                XmNtraversalOn,         False,
                XmNmarginHeight,	2, 
                XmNmarginWidth,		20, 
                XmNspacing,         	2,
                NULL);

    for ( ii = 0; ii < (long)XtNumber(mselstr); ii++ ){
	_list_mselOptW[ii] = XtVaCreateManagedWidget ( mselstr[ii],
                xmToggleButtonGadgetClass,	list_rdbox,
                NULL);
        XtAddCallback ( _list_mselOptW[ii], XmNarmCallback,
		        (XtCallbackProc)pglist_mselOptCb, (XtPointer) ii );
    }	

    /*
     * Create control buttons
     */
    _ctlForm  = (Widget)XtVaCreateManagedWidget ("_lst_ctl_formW",
		xmFormWidgetClass,	 pane,
	 	NULL );

    _ctlBtns = (WidgetList) XtMalloc ( XtNumber(btnstr) * sizeof(Widget) );
    NxmCtlBtn_create ( _ctlForm, 1, "ctlBtns", XtNumber(btnstr), btnstr,
		      NULL, _ctlBtns);

    XtManageChild(rc);
    XtManageChild(pane);
   
    XtFree( (XtPointer) clst_pbs );

    return;
}

/*=====================================================================*/

void pglist_crtMrkTyp ( Widget parent )
/************************************************************************
 * pglist_crtMrkTyp							*
 *									*
 * This function creates the area to select a list marker type. 	*
 *									*
 * void pglist_crtMrkTyp ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget	 parent widget ID			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 ** Log:								*
 * J. Wu/SAIC		11/02	initial coding				*
 * J. Wu/SAIC		11/02	adjust for adding color widget 		*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    Widget	pulldown;
    int		iret;
    long	ii, ignore;
    Pixel	fg, bg;
    Pixmap	mark_pxm[TOTAL_MARKER];
    XmString	xmstr;
    char	filename[256], warning[200], type_id[100];
    char	*type_list[] = { "marker1.xbm",  "marker2.xbm",  "marker3.xbm",
			 "marker4.xbm",  "marker5.xbm",   "marker6.xbm",
			 "marker7.xbm",  "marker8.xbm",   "marker9.xbm",
			 "marker10.xbm", "marker11.xbm",  "marker12.xbm",
			 "marker13.xbm", "marker14.xbm",  "marker15.xbm",
			 "marker16.xbm", "marker17.xbm",  "marker18.xbm",
			 "marker19.xbm", "marker20.xbm",  "marker21.xbm" };

/*---------------------------------------------------------------------*/

    _mrkTypBtn = (WidgetList) XtMalloc ( TOTAL_MARKER * sizeof(Widget) );

    /*
     * create type selection option menu
     */
    _typClrForm = XtVaCreateManagedWidget ( "type_formW",
		xmFormWidgetClass,	parent,
		NULL ); 

    pulldown = XmCreatePulldownMenu ( _typClrForm, " ", NULL, 0);

    _mrkTypOptW = XmCreateOptionMenu ( _typClrForm, " ", NULL, 0);

    XtVaSetValues ( pulldown, 
		XmNorientation,		XmHORIZONTAL,
		XmNpacking,		XmPACK_COLUMN,
		XmNnumColumns,		7,
		NULL );
 
    XtVaGetValues ( _typClrForm,
		XmNforeground,		&fg,
		XmNbackground,		&bg,
		NULL );

    for ( ii = 0; ii < TOTAL_MARKER; ii++ ) {
        sprintf ( type_id, "%ld", (ii+1) );
        cfl_inqr ( type_list[ii], ICON_DIR, &ignore, filename, &iret );

        mark_pxm[ii] = XmGetPixmap ( XtScreen(parent),
					filename, fg, bg );

        if ( mark_pxm[ii] == (Pixmap)XmUNSPECIFIED_PIXMAP ) {
            sprintf( warning, "cannot load pixmap file %s",
					filename );
            NxmWarn_show ( parent, warning );
            _mrkTypBtn[ii] = XtVaCreateManagedWidget( type_id, 
		xmPushButtonWidgetClass,	pulldown,
		NULL );
        }
        else {
            _mrkTypBtn[ii] = XtVaCreateManagedWidget ( type_id, 
		xmPushButtonWidgetClass,	pulldown,
		XmNlabelType,			XmPIXMAP,
		XmNlabelPixmap,			mark_pxm[ii],
		NULL );
        }

        XtAddCallback ( _mrkTypBtn[ii], XmNactivateCallback, 
			(XtCallbackProc)pglist_MrkTypCb, (XtPointer)ii );

    }

    xmstr = XmStringCreateLocalized("Type    ");
    XtVaSetValues ( _mrkTypOptW, 
		XmNsubMenuId,		pulldown,
		XmNmenuHistory,		_mrkTypBtn[0], 
		XmNlabelString, 	xmstr, 
		NULL );
    XmStringFree(xmstr);

    XtManageChild ( _mrkTypOptW );

}

/*=====================================================================*/

void pglist_crtSzWth ( Widget parent )
/************************************************************************
 * pglist_crtSzWth							*
 *									*
 * This function creates lists' size and width input panel.		*
 *									*
 * void pglist_crtSzWth ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/02   initial coding				*
 ***********************************************************************/
{
    Widget	size_form, width_form;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    /*
     * create width and size input
     */
    width_form = XtVaCreateManagedWidget ( "list_widthformW",
		xmFormWidgetClass, parent,
		NULL );

    size_form = XtVaCreateManagedWidget ( "_lst_size_formW",
		xmFormWidgetClass,	parent,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		width_form,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_FORM,
		NULL);

    _lst_size_txtW = XtVaCreateManagedWidget ( "lst_size",
		xmTextFieldWidgetClass,		size_form,
		XmNcolumns,			4,
		XmNvalue,			"1.0",
		XmNcursorPositionVisible,	True,
		XmNrightAttachment,		XmATTACH_FORM,
		NULL );

    XtAddCallback ( _lst_size_txtW, XmNmodifyVerifyCallback, 
                    (XtCallbackProc)pgutls_vrfyPosFltCb, NULL );    
    XtAddCallback ( _lst_size_txtW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pglist_sizTxtCb, NULL );

    _lst_size_sldW = (Widget)XmCreateScale ( size_form, "size", NULL, 0 );
    XtManageChild( _lst_size_sldW );
    xmstr = XmStringCreateLocalized( "Size" );
    XtVaSetValues( _lst_size_sldW,
		XmNorientation,             	XmHORIZONTAL,
		XmNminimum,                 	(int) (MIN_SIZE_SCALE * 10.0F),
		XmNmaximum,                 	(int) (MAX_SIZE_SCALE * 10.0F),
		XmNprocessingDirection,     	XmMAX_ON_RIGHT,
		XmNvalue,                   	(int) (_listSize * 10.0F),
		XmNshowValue,               	False,
		XmNtitleString,             	xmstr,
		XmNtopAttachment,           	XmATTACH_FORM,
		XmNleftAttachment,          	XmATTACH_FORM,
		XmNrightAttachment,         	XmATTACH_WIDGET,
		XmNrightWidget,             	_lst_size_txtW,
		NULL );
    XmStringFree(xmstr);

    XtAddCallback ( _lst_size_sldW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pglist_sizeCb, NULL);
    XtAddCallback ( _lst_size_sldW, XmNdragCallback, 
		    (XtCallbackProc)pglist_sizeCb, NULL);
   
    /*
     * create width input
     */   
    _lst_width_txtW = 
	XtVaCreateManagedWidget ( "lst_width",
		xmTextFieldWidgetClass,		width_form,
		XmNcolumns,	                4,
		XmNvalue,			"3",
		XmNcursorPositionVisible,	True,
		XmNrightAttachment,		XmATTACH_FORM,
		NULL );

    XtAddCallback ( _lst_width_txtW, XmNmodifyVerifyCallback, 
                    (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);    
    XtAddCallback ( _lst_width_txtW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pglist_widTxtCb, NULL);

    _lst_width_sldW = (Widget)XmCreateScale ( width_form, "width", NULL, 0);
    XtManageChild ( _lst_width_sldW );
    xmstr = XmStringCreateLocalized ( "Width" );
    XtVaSetValues ( _lst_width_sldW,
		XmNorientation,		XmHORIZONTAL,
		XmNminimum,		MIN_WIDTH_SCALE,
		XmNmaximum,		MAX_WIDTH_SCALE,
		XmNprocessingDirection,	XmMAX_ON_RIGHT,
		XmNvalue,		_listWdth,
		XmNscaleMultiple,	1,
		XmNshowValue,		False,
		XmNtitleString,		xmstr,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_WIDGET,
		XmNrightWidget,		_lst_width_txtW,
		NULL );
    XmStringFree(xmstr);
    XtAddCallback ( _lst_width_sldW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pglist_widthCb, NULL);
    XtAddCallback ( _lst_width_sldW, XmNdragCallback, 
		    (XtCallbackProc)pglist_widthCb, NULL);
		  
}

/*=====================================================================*/
void pglist_popup ( int lst_typ, int show_ctl, XtCallbackProc callback )
/************************************************************************
 * pglist_popup								*
 *									*
 * This function pops up the VG LIST drawing attribute box.		*
 *									*
 * void pglist_popup ( lst_typ, show_ctl, callback )			*
 *									*
 * Input parameters:							*
 *    lst_typ	        int	list type				*
 *    show_ctl 		int	flag whether to show control buttons 	*
 *    callback	XtCallbackProc	Callback function for control buttons	*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/02	initial colding				*
 * J. Wu/SAIC		11/02	add color to pglist_setAttr		*
 * J. Wu/SAIC		12/02	activate "Selecting object" mode 	*
 * J. Wu/SAIC		04/04	add multi_selection panel		*
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		iopr, ier;
    long	ii;
    /*---------------------------------------------------------------------*/
    
    if ( XtIsManaged ( _list_dlgW ) ) {
	XtUnmanageChild ( _list_dlgW );
    }
    
    if ( show_ctl ) {
	XtManageChild ( _ctlForm );
	XtUnmanageChild ( _list_modeW );
        if ( callback ) {
	    for ( ii = 0; ii < 2; ii++ ) {
                XtRemoveAllCallbacks ( _ctlBtns[ii], XmNactivateCallback );
  	        XtAddCallback ( _ctlBtns[ii], XmNactivateCallback, 
			        callback, (XtPointer)ii );
	    }
	}
    }
    else {
	XtUnmanageChild ( _ctlForm );
        XtManageChild ( _list_modeW );
    }
    

    /*
     *  Set up list multi-selection mode.
     */       
    iopr = pgpalw_getCurOperId ();
    if ( iopr == FUNC_MULTISEL ) {
        XtUnmanageChild ( _list_mselW );        
    }
    else {
        XmToggleButtonSetState ( _list_mselOptW[_mselMode], TRUE, FALSE );
        XtManageChild ( _list_mselW );    
    }
        
    
    /*
     *  Get the settings for this element type.
     */
    _subTyp  = lst_typ;

    el.hdr.vg_type = LIST_ELM;
    el.hdr.vg_class = CLASS_LIST;

    ces_get ( _subTyp, &el, &ier );
        
    pglist_setAttr ( el.elem.lst.info.mrktyp,
		     el.hdr.maj_col,
		     el.elem.lst.info.mrksiz,
		     el.elem.lst.info.mrkwid );
              
    XtManageChild ( _list_dlgW );
    
        
    /*
     *  Set up list-creating mode to draw a new LIST object.
     */           
    if ( show_ctl ) { 
        _listMode = 0;
    }
    	
    pglist_reset ();
    
    if ( iopr == FUNC_SELECT ) pglist_setMode ();
}

/*=====================================================================*/

void pglist_popdown ( void )
/************************************************************************
 * pglist_popdown							*
 *									*
 * This function unmanages the LIST dialog box.				*
 *									*
 * void pglist_popdown ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/02	initial colding				*
 * J. Wu/SAIC		12/03	pop down color pallette			*
 ***********************************************************************/
{
    if ( XtIsManaged ( _list_dlgW ) ) {
        NxmClrW_popdown();
    	XtUnmanageChild ( _list_dlgW );
    }
}

/*=====================================================================*/

void pglist_setAttr ( int mrktyp, int mrkclr, float mrksiz, int mrkwid )
/************************************************************************
 * pglist_setAttr							*
 *									*
 * This function sets the attribute values in the list dialog box.	*
 *									*
 * void pglist_setAttr ( mrktyp, mrkclr, mrksiz, mrkwid )		*
 *									*
 * Input parameters:							*
 *	mrktyp		int		marker type			*
 *	mrkclr		int		marker color			*
 *	mrksiz		float		marker size			*
 *	mrkwid		int		marker width			*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/02	initial colding				*
 * J. Wu/SAIC		11/02	add color				*
 ***********************************************************************/
{
    char	str[5];
/*---------------------------------------------------------------------*/
       
    /*
     *  Marker type
     */
    if ( mrktyp >= 1 && mrktyp <= TOTAL_MARKER ) {
        _curMrkTyp = mrktyp;
        XtVaSetValues ( _mrkTypOptW, 
		XmNmenuHistory,		_mrkTypBtn[_curMrkTyp - 1], 
		NULL );
    }
    
    /*
     * color
     */
    _listColr = mrkclr;

    XtVaSetValues ( _listColrW,
		XmNbackground,		NxmColrP_getColorPixel ( _listColr ),
		XmNtopShadowColor,	NxmColrP_getColorPixel ( _listColr ),
		XmNbottomShadowColor,	NxmColrP_getColorPixel ( _listColr ),
		NULL );
    

    /*
     *  Marker size
     */
    if ( MIN_SIZE_SCALE <= mrksiz && mrksiz <= MAX_SIZE_SCALE ) {
        sprintf ( str, "%.1f", mrksiz );
        XmTextFieldSetString ( _lst_size_txtW, "" );
        XmTextFieldSetString ( _lst_size_txtW, str );
        XmScaleSetValue ( _lst_size_sldW, (int) (mrksiz * 10.0F) );

	_listSize = mrksiz;
    }


    /* 
     *  Marker width
     */
    if ( MIN_WIDTH_SCALE <= mrkwid && mrkwid <= MAX_WIDTH_SCALE ) {
        sprintf ( str, "%i", mrkwid );
        XmTextFieldSetString ( _lst_width_txtW, str );
        XmScaleSetValue ( _lst_width_sldW, mrkwid );

	_listWdth = mrkwid;
    }

}

/*=====================================================================*/

void pglist_getAttr ( int *mrktyp, int *mrkclr, float *mrksiz, int *mrkwid )
/************************************************************************
 * pglist_getAttr							*
 *									*
 * This function gets the values in the list dialog box			*
 *									*
 * void pglist_getAttr ( mrktyp, mrkclr, mrksiz, mrkwid )		*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	*mrktyp		int		marker type			*
 *	*mrksiz		int		marker color			*
 *	*mrksiz		float		marker size			*
 *	*mrkwid		int		marker width			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/02	initial coding				*
 * J. Wu/SAIC		11/02	add color				*
 ***********************************************************************/
{

    *mrktyp = _curMrkTyp;
    *mrkclr = _listColr;
    *mrkwid = _listWdth;
    *mrksiz = _listSize;
 
}

/*=====================================================================*/

void pglist_saveAttr ( void )
/************************************************************************
 * pglist_saveAttr							*
 *									*
 * Saves current settings in the attribute window to the setting table.	*
 *									*
 * void pglist_saveAttr ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/02	initial coding				*
 * J. Wu/SAIC		11/02	save color 				*
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		loglev, ier, ier1;
    char	logstr[10], grp[4];
/*---------------------------------------------------------------------*/

    loglev = 2;
    strcpy ( grp, "CES" );

    pgutls_initHdr ( &(el.hdr) );

    el.hdr.vg_type = LIST_ELM;
    el.hdr.vg_class = CLASS_LIST;
    ces_get ( _subTyp, &el, &ier );

    if ( ier  != 0 ) {
	sprintf ( logstr, "%d ", _subTyp );

        er_lmsg ( &loglev, grp, &ier, logstr, &ier1,
                        strlen(grp), strlen(logstr) );
	NxmErr_update();
        return;
    }

    el.hdr.maj_col = _listColr;
    el.elem.lst.info.mrktyp = _curMrkTyp;
    el.elem.lst.info.mrksiz = _listSize;
    el.elem.lst.info.mrkwid = _listWdth;

    ces_set ( _subTyp, &el, &ier );
    if ( ier  != 0 ) {
	sprintf ( logstr, "%d ", _subTyp );

        er_lmsg ( &loglev, grp, &ier, logstr, &ier1,
                        strlen(grp), strlen(logstr) );
	NxmErr_update();
        return;
    }

}

/*=====================================================================*/

int pglist_getMode ( void )
/************************************************************************
 * pglist_getMode							*
 *									*
 * This function gets the list-creating mode.				*
 *									*
 * int pglist_getMode ( void )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *      pglist_getMode()	int	List-creating mode		*
 *					 0 - Clicking			*
 *					 1 - Select another object	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/02	initial colding				*
 ***********************************************************************/
{
    return ( _listMode ); 
}

/*=====================================================================*/

Boolean pglist_isUp ( void )
/************************************************************************
 * pglist_isUp								*
 *									*
 * This function queries whether the LIST dialog is managed or not.	*
 *									*
 * Boolean pglist_isUp ( void )						*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	pglist_isUp()	Boolean	    True -- managed (up), False -- down	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/02	initial colding				*
 ***********************************************************************/
{

    return ( XtIsManaged ( _list_dlgW ) );

}

/*=====================================================================*/
/* ARGSUSED */
void pglist_MrkTypCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pglist_MrkTypCb							*
 *									*
 * Callback for the marker type selection.				*
 *									*
 * void pglist_MrkTypCb ( w, which, call )				*
 *									*
 * Input parameters:							*
 *	w	   Widget	widget ID				*
 *	which	   long		widget button				*
 *	call	   XtPointer	resource				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/02	initial coding				*
 ***********************************************************************/
{   
    _curMrkTyp = (int)which + 1;
}

/*=====================================================================*/
/* ARGSUSED */
void pglist_colorCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pglist_colorCb							*
 *									*
 * Callback for color button widget.  					*
 *									*
 * void pglist_colorCb ( wid, clnt, call )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer      	color				*
 *   call		XtPointer 	callback struct			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/02	initial coding				*
 ***********************************************************************/
{
    clnt = (XtPointer)&_listColr;
    NxmClrW_popup ( wid, clnt, call );
}

/*=====================================================================*/
/* ARGSUSED */
void pglist_widthCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pglist_widthCb                                                       *
 *                                                                      *
 * Callback for width scale widget.                                     *
 *                                                                      *
 * void pglist_widthCb ( w, clnt, cbs )		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   w          Widget          	   Widget ID                    *
 *   clnt	XtPointer       	   not used                     *
 *   *cbs	XmScaleCallbackStruct  callback struct              *
 *                                                                      *
 * Output parameters:							*
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		11/02	initial coding				*
 ***********************************************************************/
{
    int		lstwid;		/* list width */
    char	txtstr[5];
/*---------------------------------------------------------------------*/

    lstwid = cbs->value;
    XmScaleSetValue ( w, lstwid );
    sprintf ( txtstr, "%i", lstwid );
    XmTextFieldSetString ( _lst_width_txtW, txtstr );

    if ( cbs->reason == XmCR_VALUE_CHANGED ) {
	_listWdth = lstwid;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pglist_widTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pglist_widTxtCb                                                      *
 *                                                                      *
 * Callback for list width text widget.                                 *
 *                                                                      *
 * void pglist_widTxtCb ( w, clnt, cbs )		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   w		Widget          	  Widget ID                     *
 *   clnt	XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 * Output parameters:							*
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		11/02	initial coding				*
 ***********************************************************************/
{
    char	*ss;
    int		slval, lstwid;
/*---------------------------------------------------------------------*/

    /*
     * Confirm there is an event.  If not, this text has already
     * been set up.
     */
    if (!cbs->event) 
	return;

    /*
     * if the value on corresponding slider is different, set the sliders
     * value accordingly.
     */
    XmScaleGetValue ( _lst_width_sldW, &slval );
    ss = XmTextFieldGetString ( _lst_width_txtW );
    lstwid = atoi(ss);
    XtFree ( ss );

    if ( MIN_WIDTH_SCALE <= lstwid && lstwid <= MAX_WIDTH_SCALE ) {
        if ( lstwid != slval ) {
            XmScaleSetValue ( _lst_width_sldW, lstwid );

	    _listWdth = lstwid;
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pglist_sizeCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pglist_sizeCb                                                      	*
 *                                                                      *
 * Callback for pattern size scale widget.                              *
 *                                                                      *
 * void pglist_sizeCb ( w, clnt, cbs )                     		*
 *                                                                      *
 * Input parameters:                                                    *
 *   w		Widget           Widget ID                          	*
 *   clnt	XtPointer        not used                           	*
 *   *cbs     XmScaleCallbackStruct  callback struct 		        *
 *                                                                      *
 * Output parameters:							*
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		11/02	initial coding				*
 ***********************************************************************/
{
    float	lstsiz;		/* list size */
    char	txtstr[5];
/*---------------------------------------------------------------------*/

    lstsiz = (float)cbs->value / 10.0F;
    sprintf ( txtstr, "%.1f", lstsiz );
    XmTextFieldSetString ( _lst_size_txtW, "" );
    XmTextFieldSetString ( _lst_size_txtW, txtstr );

    if ( cbs->reason == XmCR_VALUE_CHANGED ) {
	_listSize = lstsiz;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pglist_sizTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pglist_sizTxtCb                                                      *
 *                                                                      *
 * Callback for  pattern size text widget.                              *
 *                                                                      *
 * void pglist_sizTxtCb( w, clnt, cbs )		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt	XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 * Output parameters:							*
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		11/02	initial coding				*
 ***********************************************************************/
{
    char        *ss;
    int         slval;
    float	lstsiz;
/*---------------------------------------------------------------------*/

    /*
     * Confirm there is an event.  If not, this text has already
     * been set up.
     */
    if (!cbs->event) 
	return;

    /* if the value on corresponding slider is different, set the sliders
     * value accordingly.
     */
    XmScaleGetValue ( _lst_size_sldW, &slval );
    ss = XmTextFieldGetString ( _lst_size_txtW );
    lstsiz = (float)(atof(ss));
    XtFree ( ss );

    if ( MIN_SIZE_SCALE <= lstsiz && lstsiz <= MAX_SIZE_SCALE ) {
        if ( (int) (lstsiz * 10.0F) != slval ) {
            XmScaleSetValue ( _lst_size_sldW, (int) (lstsiz * 10.0F) );

	    _listSize = lstsiz;
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pglist_radioBoxCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pglist_radioBoxCb							*
 *									*
 * This function is the callback function of radio box widget.		*
 *									*
 * void pglist_radioBoxCb ( w, which, call )				*
 *									*
 * Input parameters:							*
 *	w		Widget		widget ID			*
 *	which		long		client data			*
 *	*call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          11/02   initial coding             		*
 * J. Wu/SAIC          12/02   set up drawing mode with pglist_setMode	*
 ***********************************************************************/
{
    _listMode = (int)which;       
    pglist_setMode ();       
}

/*=====================================================================*/
/* ARGSUSED */
void pglist_mselOptCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pglist_mselOptCb							*
 *									*
 * Callback function of multi-selection radio box widget.		*
 *									*
 * void pglist_mselOptCb ( w, which, call )				*
 *									*
 * Input parameters:							*
 *	w		Widget		widget ID			*
 *	which		long		client data			*
 *	call	 	XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          04/04   initial coding             		*
 ***********************************************************************/
{
    _mselMode = (int)which;       
    
    pglist_setHints ();    

}

/*=====================================================================*/
		 
static void pglist_addItem ( float lat, float lon, int *iret )
/************************************************************************
 * pglist_addItem							*
 *									*
 * This function adds the point (lat, lon) as a list item.		*
 *									*
 * static void pglist_addItem ( lat, lon, iret )			*
 *									*
 * Input parameters:							*
 *	lat		float			Input latitude		*
 *	lon		float			Input longitude		*
 *									*
 * Output parameters:							*
 *	*iret		int			Return code		*
 *				                 1 - LIMIT_REACHED	*
 *				                 0 - Add successfully	*
 *				                -1 - Failed		*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC           11/02   initial coding             		*
 * S. Jacobs/NCEP	 3/03	Changed State list to use state id	*
 * A. Hardy/NCEP	 2/04   Added county clusters			*
 * A. Hardy/NCEP	 3/04   Changed 'COUNTY' to 'WBCMZ_TBL		*
 * J. Wu/SAIC           04/04   add multi-selection mode 		*
 * J. Wu/SAIC           08/04   check limit on the # of items bf adding	*
 * A. Hardy/NCEP	10/04   Added permanent cluster	check		*
 * H. Zeng/SAIC		01/05	added marine zone check			*
 * A. Hardy/NCEP	02/05   reworked optional/perm. clustering	*
 * H. Zeng/SAIC		12/05	re-placed cst_numb() for cnty fips	*
 ***********************************************************************/
{
    int		ii, jj, ij, ik, nn, icnt, ier, ilp, ihot = 0;
    int         thrshld, in_list = -1, *nitems;
    float	clat, clon;
    char	strout[128], info[256], clotyp[32], loctag[32], fips[9];

    int         len_cfips, nret, cy_fips, ierr;
    int         tmpfips[MAXFIPS], pfips[MAXFIPS], nperm;
    char	clstfips[9], prefs_tag[11], tfips[9], loctag2[9];
    char        ccwfo[12], ccname[32], data[12], info1[256];
    Boolean     clsflg, done, usezn, useperm, match;
/*---------------------------------------------------------------------*/

    *iret = 0; 
    ilp   = 0;
    icnt  = 0;
    clsflg = False;
    done   = False;
    
    strcpy ( clotyp,  _bndstr[_subTyp-1] );

    /*
     * Get the county cluster flag.
     */
    clsflg = pglist_getClstStatus();
    strcpy (prefs_tag, "PERM_CLUST");
    ctb_pfbool (prefs_tag, &useperm, &ier );

    /*
     * Check if marine zones are to be used.
     */

    strcpy (prefs_tag, "ADD_MARZON");
    ctb_pfbool (prefs_tag, &usezn, &ier );

    if ( usezn == FALSE ) {
        thrshld = 51;
    }
    else {
        thrshld = 100;
    }
    if ( ier != 0 ) {
        thrshld = 100;
    }

    /*
     *  Find which bound contains the point.
     */
    clo_tqbnd ( clotyp, lat, lon, strout, &ier ); 	
    
    if ( strcmp ( strout, "-" ) == 0 )  {
        *iret = -1;	/* Not within bounds */
        return;
    }

    /*
     *  Retrieve FIPS/WFO code.
     */
    clo_bginfo ( clotyp, ihot, info, &ier );
            
    if ( _subTyp == LISTYP_WFO ) {
        strcpy ( loctag, "<WFO>");
        strcpy ( loctag2, "<WFO>");
	cst_gtag ( "WFO", info, "99999", fips, &ier );	    	        
    }
    else if ( _subTyp == LISTYP_STATE ) {
        strcpy ( loctag, "<STATE>");
        strcpy ( loctag2, "<STATE>");
	cst_gtag ( "STATE", info, "99999", fips, &ier );
    }
    else {
        strcpy ( loctag, "<FIPS>");
        strcpy ( loctag2, "<FIPS>");
	cst_gtag ( "FIPS", info, "99999", fips, &ier );	    	        

	/*
        * Initialize global variables.
        */
        _ncfips = 0;
        for ( ii=0;ii < MAXFIPS; ii++) {
            _cfips[ii] = 0; 
        }

        /*
         * Get integer value of county fips from fips str.
         */
	cst_numb ( fips, &cy_fips, &ierr );

	/*
	 * If clustering is active, find the clustered counties.
	 */
	if ( clsflg ) {

            len_cfips = sizeof(_cfips) / sizeof(_cfips[0]);
	    ctb_ccfind(cy_fips, ccwfo, ccname, &_ncfips, _cfips, &ierr);
	}

    }
    	    
    strcpy ( tfips, fips );
    if ( ier != 0 ) {
        *iret = -1;	/* FIPS/WFO code not found */
        return;    
    }
    

    /*
     *  Handle the retrieved FIPS/WFO based on multi-selection mode.
     *  Add the item if it is not in the list and _mselMode is ADD or TOGGLE
     *  Romove the item if it is in the list and _mselMode is DELETE or TOGGLE
     */
    
    nitems = &_curList.elem.lst.data.nitems;
    for ( ii = 0; ii < *nitems; ii++ ) {        
	if ( strcmp ( fips, _curList.elem.lst.data.item[ii] ) == 0 ) {
	    in_list = ii;
	    break;
	}
    }
    
    if ( (in_list >= 0)  && ( _mselMode == DELETE_ITEM ||
                           _mselMode == TOGGLE_ITEM ) )  {   /* Remove it */
       /*
	* There are not any clustered counties.
	*/
        if ( _ncfips == 0 ) { 
	    for ( ii = in_list; ii < *nitems - 1; ii++ ) {
	        strcpy ( _curList.elem.lst.data.item[ii], 
	                 _curList.elem.lst.data.item[ii+1] );
	        _curList.elem.lst.data.lat[ii] = _curList.elem.lst.data.lat[ii+1];
	        _curList.elem.lst.data.lon[ii] = _curList.elem.lst.data.lon[ii+1];
	    }    
                    
	    (*nitems)--;
	    in_list = -1;        
        }

       /*
	* There are clustered counties with the county selected. Remove them all 
	* if clustering is on. First check the transient clusters then check the
	* permanent clusters.
	*/
        while ( !done ) { 
            if ( ( clsflg ) && ( _ncfips > 0 ) ) {
	        for ( jj = 0 ; jj < _ncfips; jj++ ) {
	             clo_findnum(WBCMZ_TBL, _cfips[jj], len_cfips, &nret, info1, &ier);

                     cst_gtag ( "STNM", info1,   "-9999",    data, &ier );
                     cst_numb ( data, &cy_fips, &ier );

		     cst_inch ( _cfips[jj], clstfips, &ier);

	             for ( ik = 0; ik < *nitems; ik++){
	                 if ( strcmp (clstfips, _curList.elem.lst.data.item[ik]) == 0 ) {  
	                     in_list = ik;
	                     for ( ii = in_list; ii < *nitems; ii++ ) {
	                         strcpy ( _curList.elem.lst.data.item[ii], 
	                              _curList.elem.lst.data.item[ii+1] );
	                         _curList.elem.lst.data.lat[ii] = _curList.elem.lst.data.lat[ii+1];
	                         _curList.elem.lst.data.lon[ii] = _curList.elem.lst.data.lon[ii+1];
	                     }    
	                     (*nitems)--;
	                     in_list = -1;        
	                 }
	             }
	        }
	    }
           /* 
	    * Permanent clustering
	    */
	    if ( (useperm == TRUE ) && strcmp (loctag2, "<FIPS>") == 0 ) {
	        if ( ilp == 0 ) {

                   /*
                    * If there are not any optional clusters, reset the global variables.
                    */
                    if ( _ncfips == 0 ) {
                        _ncfips = 1;
	                cst_numb ( fips, &_cfips[0], &ierr );
                    }

                   /*
                    * Loop over all of the counties found by the original cluster 
                    * and find the first permanent grouping for each county. The 
                    * search in the permanent table is only on the first FIPS code, 
                    * not the entire string.
                    */
                    len_cfips = sizeof(_cfips) / sizeof(_cfips[0]);
                    for (ij=0; ij < _ncfips; ij++ ) {
	                ctb_permccfind(_cfips[ij], ccwfo, ccname, &nperm, pfips, &ierr);
                        if ( ier == 0 ) {
                            for ( nn=0; nn < nperm; nn++ ) {
                                tmpfips[icnt] = pfips[nn];
                                icnt++;
                            }
                        }
                    }
                   /*
                    * Uniquely sort the temporary fips code array.
                    */

                    if ( icnt > 0 ) {
                        pgwatch_numsort ( &icnt, tmpfips, &_ncfips, _cfips, &ier);
                    }
                    else {
                       _ncfips = icnt;
                    }
                   

		    clsflg = True;
		    ilp++;
	        }
	        else {
		    done = True;
	        }
	    }
	    else {
	        done = True;
	    }
	}

    }    
    else if ( in_list < 0 && ( _mselMode == ADD_ITEM ||
                               _mselMode == TOGGLE_ITEM ) )  {  /* Add it */
   
       /*
        *  Check the maximum limit before adding. 
        */    
        if ( *nitems >= MAXLISTITEMS ) {
	    *iret = LIMIT_REACHED;  
	    return;
	}


	if ( strcmp(loctag2, "<FIPS>") == 0 ) {
	if ( (cy_fips / 10000) > thrshld ) return;
        }

        /*
         * If within the threshold, do the following:
         */
	strcpy ( _curList.elem.lst.data.item[*nitems], fips );    
        
	strcat ( loctag, fips );    
	    
        clo_init( &ier );
        clo_bstype ( clotyp, &ier );		/* set bounds type */
        clo_bstag ( loctag, &ier );		/* set bounds tag  */	
        clo_bgcent ( &clat, &clon, &ier );	/* get centriod    */       

	_curList.elem.lst.data.lat[*nitems] = clat;
	_curList.elem.lst.data.lon[*nitems] = clon;

        (*nitems)++;   

       /*
	* Check for optional clusters.
	*/

        clsflg = pglist_getClstStatus();
	if ( clsflg ) {
            len_cfips = sizeof(_cfips) / sizeof(_cfips[0]);
	    cst_numb ( fips, &cy_fips, &ierr );
	    ctb_ccfind(cy_fips, ccwfo, ccname, &_ncfips, _cfips, &ierr);
	}
       /*
        * If a clustered county, add rest of the clustered counties.
	*/

	while ( !done ) {
	    if ( ( clsflg ) && ( _ncfips > 0 ) ) {
	        for ( ii = 0 ; ii < _ncfips; ii++ ) {
                    data[0] ='\0';
	            clo_findnum(WBCMZ_TBL, _cfips[ii], len_cfips, 
				&nret, info1, &ier);
                    cst_gtag ( "STNM", info1,   "-9999",    data, &ier );
                    cst_numb ( data, &cy_fips, &ier );

                    if ( (cy_fips/10000) <= thrshld ) {

		       cst_inch ( _cfips[ii], clstfips, &ier);	
        
                      ij = 0;
                      match = False;
	              while (  (ij < *nitems) && ( !match) ) {
		           if (  strcmp(clstfips, _curList.elem.lst.data.item[ij] ) != 0 ) {  
                               match = False;
                           }
                           else {
                               match = TRUE;
                           }
                          ij++;
                       }
        
		       if ( !match ) {  
	            
                          /*
                           *  Check the maximum limit before adding. 
                           */    
                          if ( *nitems >= MAXLISTITEMS ) {
	                       *iret = LIMIT_REACHED;  
	                       return;
	                  }
		    
		          strcpy ( _curList.elem.lst.data.item[*nitems], data);
	                  strcpy ( loctag, loctag2);    
	                  strcat ( loctag, data);    
	    
                          clo_init( &ier );
                          clo_bstype ( clotyp, &ier );/* set bounds type */
                          clo_bstag ( loctag, &ier );	/* set bounds tag  */ 
                          clo_bgcent ( &clat, &clon, &ier );/* get centriod    */

                          cst_gtag ( "LAT", info1, "-9999.0", data, &ier );
            	          cst_crnm ( data, &clat, &ier );
            	          cst_gtag ( "LON", info1, "-9999.0",  data, &ier );
            	          cst_crnm ( data, &clon, &ier );
	                  _curList.elem.lst.data.lat[*nitems] = clat;
	                  _curList.elem.lst.data.lon[*nitems] = clon;

                          (*nitems)++;   
		       }  
 
		    } /* the end of if ( (cy_fips/10000... */
 
	        }
	    }
           /* 
	    * Check for permanent clusters.
	    */
	    if ( (useperm == TRUE ) && strcmp (loctag2, "<FIPS>") == 0 ) {
	        if ( ilp == 0 ) {
                   /*
                    * If there are not any optional clusters, reset the global variables.
                    */
                    if ( _ncfips == 0 ) {
                        _ncfips = 1;
	                cst_numb ( tfips, &_cfips[0], &ierr );
                    }
                   /*
                    * Loop over all of the counties found by the original cluster 
                    * and find the first permanent grouping for each county. The 
                    * search in the permanent table is only on the first FIPS code, 
                    * not the entire string.
                    */
                    len_cfips = sizeof(_cfips) / sizeof(_cfips[0]);
                    for (ij=0; ij < _ncfips; ij++ ) {
	                ctb_permccfind(_cfips[ij], ccwfo, ccname, &nperm, pfips, &ierr);
                        if ( ierr == 0 ) {
                            for ( nn=0; nn < nperm; nn++ ) {
                                tmpfips[icnt] = pfips[nn];
                                icnt++;
                            }
                        }
                    }

                   /*
                    * Uniquely sort the temporary fips code array.
                    */

                    if ( icnt > 0 ) {
                        _ncfips = 0;
                        for ( ij=0;ij < MAXFIPS; ij++) {
                            _cfips[ij] = 0; 
                        }
                        pgwatch_numsort ( &icnt, tmpfips, &_ncfips, _cfips, &ier);
                    }
                    else {
                       _ncfips = 0;
                    }

	        }
	        else {
		    done = True;
	        }
	            clsflg = True;
		    ilp++;
	    }
	    else {
	        done = True;
	    }
        }
    }
}

/*=====================================================================*/

VG_DBStruct *pglist_getCurList ( void )
/************************************************************************
 * pglist_getCurList							*
 *									*
 * This function gets the current stored list element.			*
 *									*
 * VG_DBStruct *pglist_getCurList ( void )				*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	*pglist_getCurList()	VG_DBStruct	Current list element	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          11/02   initial coding             		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
   
   return (&_curList);
           
}

/*=====================================================================*/

void pglist_setCurList ( const VG_DBStruct *list )
/************************************************************************
 * pglist_setCurList							*
 *									*
 * This function sets FIPS/centriods for the current list element as	*
 * those in the input list element.					*
 *									*
 * void pglist_setCurList ( list )					*
 *									*
 * Input parameters:							*
 *	list		VG_DBStruct	List element			*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          11/02   initial coding             		*
 ***********************************************************************/
{
   _curList.elem.lst.data = list->elem.lst.data; 

}

/*=====================================================================*/

static void pglist_setMode ( void )
/************************************************************************
 * pglist_setMode							*
 *									*
 * This function sets the current list-creating mode.			*
 *									*
 * static void pglist_setMode ( void )					*
 *									*
 * Input parameters:							*
 *	None								*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          	12/02   initial coding             		*
 * J. Wu/SAIC          	04/04   call pgmsel_start for drag/shift+click	*
 * E. Safford/SAIC	06/04	pgmsel_start -> pgasel_start		*
 ***********************************************************************/
{        
/*---------------------------------------------------------------------*/

    XmToggleButtonSetState ( _list_radioW[_listMode], TRUE, TRUE );
    
    mcanvw_disarmDynamic ();
    
    if ( _listMode == 0 ) {	/* Clicking ->click/drag/shift+click */      
        pgasel_start ( &pglist_selectSingle,
                       &pglist_selectArea,
                       &pglist_selectDone );
        pglist_setHints ();
    }
    else {			/* Selecting another object */  
        pghdlb_deselectAll ();	
	mcanvw_setPressFunc ( (XtEventHandler)&pglist_selectLineEh, CURS_DEFAULT );
        mbotw_mouseSet ( LMHINT_SELECT, MMHINT_DONE );
    }
       
}

/*=====================================================================*/
/* ARGSUSED */
static void pglist_selectLineEh ( Widget wid, XtPointer clnt,
				XEvent *event, Boolean *ctdr )
/************************************************************************
 * pglist_selectLineEh							*
 *									*
 * This function selects a closed line for creating a new list.		*
 *									*
 * static void pglist_selectLineEh ( wid, clnt, event, ctdr  )		*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	not used				*
 *	*event	XEvent		button event				*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          12/02   initial coding             		*
 ***********************************************************************/
{
    int		cur_layer, ier, xoff, yoff, iclass, selected, nearest;
    float	xx, yy;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
    
    if ( event->xbutton.button == Button1 ) {
	
        cur_layer = pglayer_getCurLayer ();
	
        /*
         * Get device coordinates.
         */
        xgtoff (&xoff, &yoff, &ier);
        xx = (float) (event->xbutton.x + xoff);
        yy = (float) (event->xbutton.y + yoff);
    
        
	/*
         * Locate a closed line.
         */        
	el.hdr.vg_class = iclass = CLASS_LINES;
        cvg_scan ( NULL, cur_layer, (char) iclass, xx, yy, 0,
                  &el, &selected, &nearest, &ier );
        
	if ( ier >= 0 && el.hdr.closed ) {
	    
	    pgactv_setActvElm ( &el, selected );
	    pghdlb_select( &el, selected );

	    mcanvw_disarmPress();
		
	    mcanvw_setPressFunc ( (XtEventHandler)&pglist_confirmItemsEh, CURS_DEFAULT );
	    mbotw_mouseSet ( LMHINT_CONFIRM, MMHINT_TOSELECTOPER );
        
	}
	else {
	    mcanvw_setPressFunc ( (XtEventHandler)&pglist_selectLineEh, CURS_DEFAULT ); 	    
	    mbotw_mouseSet ( LMHINT_SELECT, MMHINT_DONE );	
	}
    }
    else {	
        pgevt_unsetOper ( TRUE );
	if (pgpalw_getCurClassId() > 0) {
	    mbotw_mouseSet ( LMHINT_SELECT, MMHINT_NOACTION );
	} 
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pglist_confirmItemsEh ( Widget wid, XtPointer clnt,
                                      XEvent *event, Boolean *ctdr )
/************************************************************************
 * pglist_confirmItemsEh						*
 *									*
 * This function confirms the creation of items using the selected line.*
 *									*
 * static void pglist_confirmItemsEh ( wid, clnt, event, ctdr )	*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	not used				*
 *	*event	XEvent		button event				*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          12/02   initial coding             		*
 * J. Wu/SAIC          04/04   call pglist_createItems        		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
   
    if ( event->xbutton.button == Button1 ) {
        pglist_createItems () ;
    }
    else {	
        pghdlb_deselectAll();
	pgactv_clearActv ();
	mcanvw_setPressFunc ( (XtEventHandler)&pglist_selectLineEh, CURS_DEFAULT ); 	    
	mbotw_mouseSet ( LMHINT_SELECT, MMHINT_DONE );	
    }

}

/*=====================================================================*/

static void pglist_createItems ( void )
/************************************************************************
 * pglist_createItems							*
 *									*
 * This function creates/save list items contained within a closed line.*
 *									*
 * static void pglist_createItems ( iret )				*
 *									*
 * Input parameters:							*
 *	None								*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		12/02	Initial coding				*
 * S. Jacobs/NCEP	 3/03	Added return of group type and number	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		04/04	use pglist_selectArea			*
 ***********************************************************************/
{
    int		ii, np, ier, location;
    float	lat[MAXPTS], lon[MAXPTS];
    float	xx[MAXPTS], yy[MAXPTS];
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
    	   	            
    /*
     *  Read the currently-selected line (closed).
     */    
    location = pgactv_getElmLoc();
    if ( location <= 0 ) {
	return;	
    }
    
    cvg_rdrec ( cvg_getworkfile(), location, &el, &ier ); 
    
    if ( ier != 0 || el.hdr.vg_class != CLASS_LINES || !el.hdr.closed ) {
	return;		 
    }
    
    /*
     *  Build a closed polygon from the line.
     */        
    if ( el.hdr.vg_type == LINE_ELM ) { 
        np = el.elem.lin.info.numpts;
	for ( ii = 0; ii < np; ii++ ) {
	    lat[ii] = el.elem.lin.latlon[ii];
            lon[ii] = el.elem.lin.latlon[np+ii];
        }
	
	np++;
	lat[np-1] = lat[0];
        lon[np-1] = lon[0];
    
    } 
    else {			/* Special Lines */
        np = el.elem.spl.info.numpts;
	for ( ii = 0; ii < np; ii++ ) {
	    lat[ii] = el.elem.spl.latlon[ii];
            lon[ii] = el.elem.spl.latlon[np+ii];
        }
	
	np++;
	lat[np-1] = lat[0];
        lon[np-1] = lon[0];
    }
    	                
    
    /*
     *  Reset to draw a new LIST object.
     */   
    pglist_reset ();
    
    gtrans ( sys_M, sys_D, &np, lat, lon, xx, yy, &ier, 
	     strlen(sys_M), strlen(sys_D) );
    

    /*
     *  Create items and save elements.
     */   
    pglist_selectArea ( np, xx, yy );    

    
    /*
     *  Reset to SELECT status.
     */   
    pghdlb_deselectAll ();
    pgactv_clearActv ();
    mcanvw_disarmDynamic ();
    mcanvw_setPressFunc ( (XtEventHandler)&pglist_selectLineEh, CURS_DEFAULT );
    mbotw_mouseSet ( LMHINT_SELECT, MMHINT_DONE );

}

/*=====================================================================*/
/* ARGSUSED */
static void pglist_clstOptCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pglist_clstOptCb                                                     *
 *                                                                      *
 * Callback function for the clusteing radio buttons.		        *
 *                                                                      *
 * void pglist_clstOptCb (wid, which, cbs)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which		long		Which item			*
 *   *cbs               XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 1/04	Initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	_clstStatus = (int)which;

}
/*=====================================================================*/
static Boolean pglist_getClstStatus ( void )
/************************************************************************
 * pglist_getClstStatus						        *
 *									*
 * This function returns a int value specifying the current clustering  *
 * status.                                                              *
 *									*
 * int pglist_getClstStatus( )					        *
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *                              NONE                                    *
 * Return value:                                                        *
 * pglist_getClstStatus	        int	  current clustering status     *
 *									*
 **									*
 * Log:									*
 * A. Hardy/NCEP	 1/04     initial coding			*
 ***********************************************************************/
{
   return ( ( Boolean) _clstStatus );
}

/*=====================================================================*/

void pglist_createWtchLst ( VG_DBStruct *wtchEl, char *fname )
/************************************************************************
 * pglist_createWtchLst							*
 *									*
 * This function creates a LIST element and writes it to the given file.*
 *									*
 * void pglist_createWtchLst ( fname, iret )				*
 *									*
 * Input parameters:							*
 *	*fname		char		VG file name to write element	*
 *      *wtchEl         VG_DBStruct	Watch element    		*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * A. Hardy/NCEP	 2/04		Initial coding			*
 * A. Hardy/NCEP	 3/04	Changed lst_typ 1 -> 5; CNTY_BNDS to 	*
 *				BNDS_FILE; COUNTY -> WBCMZ_TBL		*
 * A. Hardy/NCEP	 5/04	Initialize List element header 		*
 * T. Piper/SAIC	02/05	Removed unused variables start & np	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		nitems, ii, ier, lst_typ;
    int		maxlen, nret, layer, list_offset, wbx_offset;
    char	clotyp[32], locnam[32];
    char	loctag[32], locitem[32], info[128];
    float	plat, plon;
    VG_DBStruct     lstEl;
/*---------------------------------------------------------------------*/
    
    lst_typ = 5;  /* List subtype - marine zones with counties */
    list_offset = -1;
    pgutls_initHdr ( &(lstEl.hdr) );
    
   /*
    * Set up List element header information.
    */

    lstEl.hdr.vg_class = CLASS_LIST;
    lstEl.hdr.vg_type = LIST_ELM;
    lstEl.elem.lst.info.subtyp = lst_typ;
            
   /*
    * Set up the marker type, size and width from setting.tbl.
    */

    ces_get ( lst_typ, &lstEl, &ier );

   /*
    * Begin filling in the List element.
    * Set the number of counties in the watch.
    */

    nitems = wtchEl->elem.wbx.info.numcnty;
    lstEl.elem.lst.data.nitems = nitems;
    lstEl.hdr.grptyp = wtchEl->hdr.grptyp;

    lstEl.hdr.grpnum = wtchEl->hdr.grpnum;

    strcpy ( clotyp, BNDS_FILE );
    strcpy ( locnam, "<FIPS>");

   /*
    * Set the counties in the List element.
    */

    for ( ii = 0; ii < nitems; ii++ ) {
        plat = wtchEl->elem.wbx.info.cn_ltln[ii];
        plon = wtchEl->elem.wbx.info.cn_ltln[ii+nitems];
	cst_inch ( wtchEl->elem.wbx.info.cn_fips[ii], locitem, &ier );

        clo_findnum ( WBCMZ_TBL, wtchEl->elem.wbx.info.cn_fips[ii],
                        maxlen, &nret, info, &ier );
	strcpy ( lstEl.elem.lst.data.item[ii], locitem );
	    
	strcpy ( loctag, locnam );
	strcat ( loctag, locitem );    
	    
	clo_init( &ier );
        clo_bstype ( clotyp, &ier );	
	clo_bstag ( loctag, &ier );
        clo_bgcent ( &plat, &plon, &ier );
	    
	lstEl.elem.lst.data.lat[ii] = plat;	        	
	lstEl.elem.lst.data.lon[ii] = plon;	        	 
    }

   /*
    * Save the list element to the work file.
    */

    pgvgf_saveNewElm ( cvg_getworkfile(), sys_D, &lstEl, nitems, 
                        lstEl.elem.lst.data.lat, lstEl.elem.lst.data.lon, 
			TRUE, &list_offset, &ier);
   /*
    * group the list element to the watch and write elements out to
    * file.
    */

    cvg_rdrec (cvg_getworkfile(), (pgactv_getElmLoc ()), wtchEl, &ier);
    wbx_offset = pgactv_getElmLoc ();

    pgactv_setActvElm (wtchEl, wbx_offset); 
    pglist_grpToWtch ( list_offset );
    strcpy ( _filnam, fname);
    pglist_wrtLstWtch (fname, &list_offset);

   /*
    * Display the list element.
    */

    cvg_rdrec (cvg_getworkfile(), list_offset, &lstEl, &ier);
    layer = pglayer_getCurLayer();
    crg_set (&lstEl, list_offset, layer, &ier);
    pgactv_setActvElm (&lstEl, list_offset); 

}

/*=====================================================================*/

void pglist_grpToWtch ( int list_offset )
/************************************************************************
 * pglist_grpToWtch							*
 *                                                                      *
 * This function groups a newly added watch List element with the 	*
 * previously selected watch into GRPTYP_WATCH.                         *
 *                                                                      *
 * void pglist_grpToWtch ( list_offset )                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      list_offset     int     file offset (WORK_FILE) for watch list  *
 *				element					*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 2/04		Initial code			*
 * A. Hardy/NCEP	 4/04		Add check for prev. grouped list*
 *                                      element 			*
 * A. Hardy/NCEP	 5/04		Fixed picking List elm to delete*
 * A. Hardy/NCEP	 6/04	Fixed incorrect range clearing of elm	*
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 * T. Piper/SAIC	02/05	Removed unused variable 'list'		*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 ***********************************************************************/
{
int     grpnum, list_idx, ier, nelm, nmix, *idxarr, iclr;
int     watch_offset, watch_idx, jj, layer, offset, del_offset;
float	llx, lly, urx, ury;
char    grptyp, vg_class, vg_type;
filter_t	filter;
/*---------------------------------------------------------------------*/

   watch_offset = pgactv_getElmLoc();
   crg_getinx (watch_offset, &watch_idx, &ier);

   crg_ggrp (watch_idx, &grptyp, &grpnum, &ier);

   if (grptyp != GRPTYP_WATCH) {

      /*
       *  Watch is ungrouped -- both it and the watch line
       *  will be grouped together.
       */

       crg_ggnxt(GRPTYP_WATCH, &grpnum, &ier);

       crg_sgrp( watch_idx, GRPTYP_WATCH, grpnum, &ier);
       cvg_setginf (cvg_getworkfile(), watch_offset, GRPTYP_WATCH, grpnum, &ier);

       crg_getinx (list_offset, &list_idx, &ier);

       crg_sgrp( list_idx, GRPTYP_WATCH, grpnum, &ier);
       cvg_setginf (cvg_getworkfile(), list_offset, GRPTYP_WATCH, grpnum, &ier);

    }
    else {
	/*
	 * Check if there is another list element grouped to the watch.
	 * If so, delete it before grouping the new list element.
	 */
	 crg_ggnel ( grptyp, grpnum, &nelm, &ier );
	 idxarr = (int *)malloc(nelm*sizeof(int));
	 crg_gginx ( grptyp, grpnum, nelm, idxarr, &nmix, &ier );
         jj = 0;
	 del_offset = -1;
         while ( jj < nmix) {
	    crg_get ( idxarr[jj], &layer, filter, &llx, &lly, &urx, &ury, &ier);
	    crg_gtyp ( idxarr[jj], &vg_class, &vg_type, &ier );
	    if (vg_class == CLASS_LIST ) {
		crg_goffset ( idxarr[jj], &offset, &ier ); 
		if ( offset != list_offset ) {
		    del_offset = offset;
		    iclr = jj;
	        }
	    }
	    jj++;
	 }
	 if (del_offset >= 0 ) {
	     cvg_delet ( cvg_getworkfile(), del_offset, TRUE, &ier);
	     crg_clear ( idxarr[iclr], &ier );
	     cvg_rfrsh(NULL, llx, lly, urx, ury, &ier);
	 }
	 free(idxarr);

        /*
         *  Watch is already grouped -- add the watch line to the same group.
         */
       crg_sgrp( list_idx, GRPTYP_WATCH, grpnum, &ier);
       cvg_setginf (cvg_getworkfile(), list_offset, GRPTYP_WATCH, grpnum, &ier);

    }

}
/*=====================================================================*/

void pglist_wrtLstWtch ( char *fname, int *location )
/************************************************************************
 * pglist_wrtLstWtch							*
 *                                                                      *
 * This function writes a particular watch and associated list element  *
 * information to a file.						*
 *                                                                      *
 * void pglist_wrtLstWtch ( fname, location )				*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 *	*fname		char		VG file name to write element	*
 *  	*location	int 		List element location		*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 2/04		Initial coding			*
 * A. Hardy/NCEP	 4/04		Add istoff check for loop;chng  *
 *                                      hv_lst,hv_wtch in if check	*
 * A. Hardy/NCEP	 5/04		use 'start' for cvg_writef call *
 * T. Piper/SAIC	02/05	Removed unused variable 'maxgnm'	*
 * S. Danz/AWC		07/06	Update to new cvg_writef() parameter    *
 ***********************************************************************/
{
int     istoff, sbtyp, ieloff, gpnum, ier, elnmw, grpnum;
int     ii, flag, ingrp, members[MAXOFF], loc, start;
char    vgclss, vgnum, gptyp, outfil[FILE_FULLSZ], grptyp;
long	size, curpos;
FILE    *ofp;
Boolean hv_lst, hv_wtch;
VG_DBStruct    lstEl;
/*---------------------------------------------------------------------*/
    istoff  = 0;
    sbtyp   = -99;
    gptyp   = GRPTYP_WATCH; 
    vgclss  = CLASS_WATCHES;
    vgnum   = WBOX_ELM;
    hv_lst  = False;
    hv_wtch = False;
    start   = -1;

    curpos = sizeof(lstEl.hdr) + sizeof(lstEl.elem.fhed);
    cfl_inqr (cvg_getworkfile(), NULL, &size, outfil, &ier);
    cvg_open (outfil, G_FALSE, &ofp, &ier);
    cfl_seek(ofp, curpos, 0, &ier);

   /*
    * Use the element location to determine the group type and group
    * number.
    */

    crg_getinx( *location, &elnmw, &ier );
    crg_ggrp ( elnmw, &grptyp, &grpnum , &ier);

   /*
    * Open and initialize the output VG file.
    */

    cvg_crvgf (fname, &ier);

   /*
    * Read an element header to find the group type number.
    */

    while ( ( !hv_wtch) && ( !hv_lst ) && ( istoff != IMISSD ) ) {

        cvg_rdgtn ( cvg_getworkfile(), ofp, &size, istoff, gptyp, vgclss, 
                    vgnum, sbtyp, &ieloff, &gpnum, &ier );

        istoff = ieloff;

       /* 
        * If the element offset exists, the group number has been found, 
        * and the return value is equal to 0, continue. 
	*/

        if ( ( ieloff != IMISSD ) && ( gpnum > 0 ) && ( ier == 0 ) ){

            cvg_srchgrp ( cvg_getworkfile(), ofp, &size, gptyp, gpnum, 
	                  MAXOFF, members, &ingrp, &ier);

            for ( ii = 0; ii < ingrp; ii++ ) {

	        cvg_rdhdr ( cvg_getworkfile(), ofp, members[ii], 
		           (int) size, &(lstEl), &flag, &ier );

                if ( ( lstEl.hdr.delete == 0 ) &&
                    ( lstEl.hdr.vg_type != FILEHEAD_ELM ) &&
		    ( lstEl.hdr.grpnum == grpnum ) )  {

                     cvg_rdele(&(lstEl), members[ii], lstEl.hdr.recsz, ofp, &ier);
	            if ( ( lstEl.hdr.vg_class == CLASS_WATCHES) &&
	                ( lstEl.hdr.vg_type == WBOX_ELM ) ) {
	                hv_wtch = True;
		        cvg_writef ( &(lstEl), start, lstEl.hdr.recsz, fname, FALSE, &loc, &ier);
	    	    }
	            else if ( ( lstEl.hdr.vg_class == CLASS_LIST) &&
	                ( lstEl.hdr.vg_type == LIST_ELM ) ) {
		        hv_lst = True;
		        cvg_writef ( &(lstEl), start, lstEl.hdr.recsz, fname, FALSE, &loc, &ier);
		    }
		}
	    } 
	}  
	if ( ieloff == IMISSD ) {
	    hv_lst = True;
	    hv_wtch = True;
	}
	
    } 
}
/*=====================================================================*/

static void pglist_selectSingle ( float xx, float yy )
/************************************************************************
 * pglist_selectSingle							*
 *									*
 * This function processes the single selection of list.		*
 *									*
 * static void pglist_selectSingle ( xx, yy )				*
 *									*
 * Input parameters:							*
 *	xx		float	x coordinate of point			*
 *	yy		float	y coordinate of point			*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          04/04   initial coding             		*
 ***********************************************************************/
{        
    int		np, ier;
    float	lat[1], lon[1], xtmp[1], ytmp[1];
/*---------------------------------------------------------------------*/
    
    np = 1;
    xtmp[0] = xx;
    ytmp[0] = yy;
    
    gtrans ( sys_D, sys_M, &np, xtmp, ytmp, 
	     lat, lon, &ier, strlen(sys_D), strlen(sys_M) );
    
    pglist_saveList ( np, lat, lon );
    
}

/*=====================================================================*/

static void pglist_selectArea ( int np, const float xx[], const float yy[] )
/************************************************************************
 * pglist_selectArea							*
 *									*
 * This function processes the area selection of a list.		*
 *									*
 * static void pglist_selectArea ( np, xx[], yy[] )			*
 *									*
 * Input parameters:							*
 *	np		np	number of points			*
 *	xx		float	x coordinates of points			*
 *	yy		float	y coordinates of points			*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          04/04   initial coding             		*
 * J. Wu/SAIC          08/04   Adjust max. stns to definition in CLO	*
 ***********************************************************************/
{        
    int		ii, npts, nitems, ier;
    float	flat[MAXSTN], flon[MAXSTN];
    char	clotyp[32];
    float	xtmp[MAXPTS], ytmp[MAXPTS], lat[MAXPTS], lon[MAXPTS];

/*---------------------------------------------------------------------*/
        	   	                           
    /*
     *  Make a copy of all points except the last one. 
     */
    npts = np - 1;
    for ( ii = 0; ii < npts; ii++ ) {
        xtmp[ii] = xx[ii];
        ytmp[ii] = yy[ii];    
    }
            
    gtrans ( sys_D, sys_M, &npts, xtmp, ytmp, lat, lon, &ier, 
	     strlen(sys_D), strlen(sys_M) );

    
    /*
     *  Find FIPS bounded by the closed line.
     */    
    strcpy ( clotyp,  _bndstr[_subTyp-1] );        
    
    clo_binpoly ( clotyp, npts, lat, lon, &ier );

    clo_tgltln ( clotyp, MAXSTN, &nitems, flat, flon, &ier );
    if ( nitems <= 0 ) {
	 return;		 
    }
        

    /*
     *  Add items to the current list and save the element.
     */    
    pglist_saveList ( nitems, flat, flon );

}

/*=====================================================================*/

static void pglist_selectDone ( void )
/************************************************************************
 * pglist_selectDone							*
 *									*
 * This function resets pgen operation when the selction is done.	*
 *									*
 * static void pglist_selectDone ( void )				*
 *									*
 * Input parameters:							*
 *	None								*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          04/04   initial coding             		*
 ***********************************************************************/
{            
    
    /*
     *  Reset to draw a new list element.
     */    
    pglist_reset ();    


    /*
     *  Reset pgen operation to SELECT.
     */    
    pgevt_unsetOper ( TRUE );
    if ( pgpalw_getCurClassId() > 0) {
	mbotw_mouseSet ( LMHINT_SELECT, MMHINT_NOACTION );
    } 
    
}

/*=====================================================================*/

static void pglist_reset ( void )
/************************************************************************
 * pglist_reset								*
 *									*
 * This function resets to draw a new list element.			*
 *									*
 * static void pglist_reset ( void )					*
 *									*
 * Input parameters:							*
 *	None								*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          04/04	initial coding             		*
 ***********************************************************************/
{               
    _curListLoc = -1;
    _curList.elem.lst.data.nitems = 0;        
}

/*=====================================================================*/

static void pglist_saveList ( int np, float lat[], float lon[] )
/************************************************************************
 * pglist_saveList							*
 *									*
 * This function adds the given items to the current list and save it.	*
 *									*
 * static void pglist_saveList ( np, lat, lon )				*
 *									*
 * Input parameters:							*
 *	np		int	number of list items			*
 *	lat[]		float	Latitudes of list items			*
 *	lon[]		float	Longitudes of list items		*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          04/04   initial coding             		*
 * J. Wu/SAIC          08/04   check limit on the # of items bf adding	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{               
    int		ii, location, grpnum, ier, add_flag;
    char	grptyp;
    float	llx, lly, urx, ury;

    VG_DBStruct	el;
    Boolean	isNewList;
/*---------------------------------------------------------------------*/
    
    isNewList = ( _curList.elem.lst.data.nitems == 0 );
    
    /*
     *   Add, delete, or toggle items in the current active list.
     */
    for ( ii = 0; ii < np; ii++ )  {	
	
	pglist_addItem ( lat[ii], lon[ii], &add_flag );        
	
	/* 
	 *  Check if the limit has been reached. If so, stop adding
	 *  but still allow deleting and toggling.
	 */
	if ( add_flag == LIMIT_REACHED ) {
	    if ( isNewList || _mselMode == ADD_ITEM ) {
	        break;		/* Limit reached - stop adding */ 
	    }
	}
    }
    
    np =  _curList.elem.lst.data.nitems;
          
    /*
     *  Save the current active list.
     */
    if ( XtIsManaged ( _ctlForm ) ) {	/* editing an existing list */
	
	_curListLoc = pgactv_getElmLoc ();
	pgutls_prepNew ( _curListLoc, &el, &llx, &lly, &urx, &ury, &ier );

	pgundo_newStep ();
	pgundo_storeThisLoc ( _curListLoc, UNDO_DEL, &ier );
            
	el.elem.lst.data.nitems = 0; 

	pgvgf_saveNewElm ( NULL, sys_D, &el, np, lat, lon, TRUE,
			&location, &ier );

	pgundo_storeThisLoc ( location, UNDO_ADD, &ier );
        pgundo_endStep ();
	
	pgutls_redraw ( location, &el, &ier );    
	
    }
    else {				/* creating a new list */
	
	pgnew_getGrpInfo ( &grptyp, &grpnum );    
    
        pgundo_newStep ();
        if ( _curListLoc >= 0 ) {		    
            pgutls_prepNew ( _curListLoc, &el, &llx, &lly, &urx, &ury, &ier );
            pgundo_storeThisLoc ( _curListLoc, UNDO_DEL, &ier );	     
        }
    
        pgvgf_save ( grptyp, grpnum, np, lat, lon, &location, &ier );
   
        _curListLoc = location;
        
	pgundo_storeThisLoc ( location, UNDO_ADD, &ier );
        pgundo_endStep();

    }

    
    /*
     *   Pop up a warning if the limit has been reached.
     */
    if ( add_flag == LIMIT_REACHED ) { 
	NxmWarn_show ( mcanvw_getDrawingW (), 
	    "Limit reached - a list can only have a maximum of MAXLISTITEMS items." );
    }

}

/*=====================================================================*/

static void pglist_setHints ( void )
/************************************************************************
 * pglist_setHints							*
 *									*
 * This function sets proper mouse hints at the bottom of the window.	*
 *									*
 * static void pglist_setHints ( void )					*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          04/04   initial coding             		*
 ***********************************************************************/
{
    
    if ( _listMode == 0 ) {
        if ( _mselMode == ADD_ITEM ) {
            mbotw_actionSet ( ACHINT_ADD );
            mbotw_mouseSet ( ACHINT_ADD, MMHINT_DONE );
        }
        else if ( _mselMode == DELETE_ITEM ) {
            mbotw_actionSet ( LMHINT_DELETE );    
            mbotw_mouseSet ( LMHINT_DELETE, MMHINT_DONE );
        }
        else {
            mbotw_actionSet ( LMHINT_TOGGLE );
            mbotw_mouseSet ( LMHINT_TOGGLE, MMHINT_DONE );
        }
    
        if ( XtIsManaged (_ctlForm) ) {
	    mbotw_classSet ( "Item" ); 	
	}
	else {
	    mbotw_classSet ( "List" ); 	
	}
    }    
}

/*=====================================================================*/

void pglist_updtStatusCntys ( int watch_offset )
/************************************************************************
 * pglist_updtStatusCntys                                               *
 *                                                                      *
 * This function processes the area selection of a list.                *
 *                                                                      *
 * void pglist_updtStatusCntys ( int watch_offset )                     *
 *                                                                      *
 * Input parameters:                                                    *
 *      watch_offset    int     file offset (WORK_FILE) for parent watch*
 *                                                                      *
 * Output parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC 04/04   initial coding                          *
 * S. Jacobs/NCEP	10/05	Fixed size of wcnty array, increased	*
 * 				from 100 to MAX_CNTY			*
 * F. J. Yen/NCEP	 2/06	Fixed to handle marine zones		*
 * S. Danz/AWC		07/06	Update to new cvg_write() parameter     *
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * F. J. Yen/NCEP	 3/07	Set error if number of counties>MAX_CNTY*
 *				(watch #44, 46). Fix memory leak.	*
 ***********************************************************************/
{
int     ii, jj, ier, ierr, location, ieloff, add_flag;
int     lstat, lmode, elnmw, istoff, sbtyp, grpnum, gpnum_w, ingrp, flag;
int     members[MAXOFF], nlist, watch_number, nwcnties, nmwcn;
int	wcnty[MAX_CNTY];
int     len_wcnty, nret, list_offset, layer, itype;
int     inxrem[MAX_CNTY], inxkeep[MAX_CNTY];
int     found, nremove, nn, ilen;
char    dattim[20], systim[20], info[256], outfil[FILE_FULLSZ], dummy[9];
char    gptyp_w, vgclss_w, vgtyp_w, grptyp, fname[FILE_FULLSZ];
char    wcn_fips[MAX_CNTY][7], w_fips[MAX_CNTY][7], l_fips[MAX_CNTY][7];
float   l_lats[MAX_CNTY], l_lons[MAX_CNTY], w_lat, w_lon;

long    size;
FILE    *ofp;
VG_DBStruct    el, lstEl, wchEl;
/*---------------------------------------------------------------------*/

    istoff    = 0;
    sbtyp     = -99;
    itype     = 1;
    gptyp_w   = GRPTYP_WATCH;
    vgclss_w  = CLASS_WATCHES;
    vgtyp_w   = WBOX_ELM;

    _subTyp   = 5;
    lstat     = _clstStatus;
    lmode     = _mselMode;
    _clstStatus = 0;

    for ( ii = 0; ii < MAX_CNTY; ii++ ) {
        inxrem[ii]  = IMISSD;
        inxkeep[ii] = IMISSD;
    }

    css_gtim ( &itype, dattim, &ier );
    css_gtim ( &itype, systim, &ier );
   /*
    * Find the location of the curently selected element (watch_box)
    */
    location = pgactv_getElmLoc();

   /*
    * Read in the curently selected element (watch)
    */
    cvg_rdrec ( cvg_getworkfile(), location, &wchEl, &ier );

   /*
    * Store watch's number and county FIPs
    */
    if ( wchEl.hdr.vg_class == vgclss_w && wchEl.hdr.vg_type == vgtyp_w ) {
        nwcnties     = wchEl.elem.wbx.info.numcnty;
        watch_number = wchEl.elem.wbx.info.w_number;
        for ( ii = 0; ii < nwcnties; ii++ ) {
            sprintf( dummy, "%d", wchEl.elem.wbx.info.cn_fips[ii]);
            cst_lstr ( dummy, &ilen, &ier );
	    if ( ilen == 5 ) {
                sprintf( w_fips[ii], "0%d", wchEl.elem.wbx.info.cn_fips[ii]);
            }
	    else if ( ilen == 6 ) {
                sprintf( w_fips[ii], "%d", wchEl.elem.wbx.info.cn_fips[ii]);
	    }
            else {
                sprintf( w_fips[ii], "00%d", wchEl.elem.wbx.info.cn_fips[ii]);
            }
        }
    }
   /*
    * Get the array of WCN counties
    */
    gg_wfps ( &watch_number, dattim, systim, &nmwcn, wcnty, &ier,
              strlen(dattim), strlen(systim) );

    if ( ier == 8 || ier == 7 ) {
        er_wmsg ( "gg", &ier, NULL, &ierr, strlen("gg"), 0);
        NxmErr_update();
    }

    if ( nmwcn == 0 ) {
        _clstStatus = lstat;
        _mselMode = lmode;
        return;
    }
    for ( jj = 0; jj < nmwcn; jj++ ) {
        sprintf( dummy, "%d", wcnty[jj]);
        cst_lstr ( dummy, &ilen, &ier );
        if ( ilen == 5) {
            sprintf( wcn_fips[jj], "0%d", wcnty[jj]);
        }
	else if ( ilen == 6 ) {
            sprintf( wcn_fips[jj], "%d", wcnty[jj]);
	}
        else {
            sprintf( wcn_fips[jj], "00%d", wcnty[jj]);
        }
    }

   /*
    * Open the work_file and get the pointer
    */
    cfl_inqr (cvg_getworkfile(), NULL, &size, outfil, &ier);
    cvg_open (outfil, G_TRUE, &ofp, &ier);

   /*
    * Use the element location to determine the group type and
    * group number of the element (watch_box)
    */
    crg_getinx( location, &elnmw, &ier );
    crg_ggrp ( elnmw, &grptyp, &grpnum, &ier);
    cvg_rdgtn ( cvg_getworkfile(), ofp, &size, istoff,
                gptyp_w, vgclss_w, vgtyp_w, sbtyp, 
                &ieloff, &gpnum_w, &ier );
   /*
    * Find starting positions (in bytes) of each element with
    * matching group type and group number within VGF file
    */
    cvg_srchgrp ( cvg_getworkfile(), ofp, &size, gptyp_w, grpnum,
                  MAXOFF, members, &ingrp, &ier);
   
    for ( ii = 0; ii < ingrp; ii++ ) {

      /*
       * Read in headers of elements from VGF file at each
       * position with matching group type and group number
       */
       cvg_rdhdr ( cvg_getworkfile(), ofp, members[ii],
                  (int) size, &(el), &flag, &ier );

      /*
       * Find the LIST element that has the same group number 
       * and group type as the watch box
       */
       if ( ( el.hdr.delete  == 0 )            &&
            ( el.hdr.vg_type != FILEHEAD_ELM ) &&
            ( el.hdr.grptyp  == grptyp )       &&
            ( el.hdr.grpnum  == grpnum ) )         {

         /*
          * Read the LIST element
          */
          cvg_rdele(&(el), members[ii], el.hdr.recsz, ofp, &ier);
          if ( ( el.hdr.vg_class == CLASS_LIST) &&
               ( el.hdr.vg_type == LIST_ELM ) ) {
             /*
              * Locally store LIST element data
              */
             lstEl = el;
             list_offset = members[ii];
             nlist = lstEl.elem.lst.data.nitems;
             for ( jj = 0; jj < nlist; jj++ ) {
                  sprintf( dummy, "%s", lstEl.elem.lst.data.item[jj]);
                  cst_lstr ( dummy, &ilen, &ier );
                  if ( ilen == 5 ) {
                      sprintf( l_fips[jj], "0%s", lstEl.elem.lst.data.item[jj]);
                  }
                  else if ( ilen == 6 ) {
                      sprintf( l_fips[jj], "%s", lstEl.elem.lst.data.item[jj]);
                  }
                  else {
                      sprintf( l_fips[jj], "00%s", lstEl.elem.lst.data.item[jj]);
                  }
                  l_lats[jj] = lstEl.elem.lst.data.lat[jj];
                  l_lons[jj] = lstEl.elem.lst.data.lon[jj];
             }
          }
       }
    } 
   /*
    * Find the indices of List FIPs not to be removed
    */
    nn = 0;
    for ( ii = 0; ii < nlist; ii++ ){
        for ( jj = 0; jj < nmwcn; jj++ ){
            if ( strcmp (l_fips[ii], wcn_fips[jj]) == 0 ) {
                inxkeep [nn] = ii;
                nn++;
                break;
            }
        }
    }

    el = lstEl;
    _curListLoc = list_offset;
    _curList = lstEl;

   /*
    * Remove the List FIPs
    */
    jj = 0;
    for ( ii = 0; ii < nlist; ii++ ){
        if ( ii != inxkeep[jj] ) {
            _mselMode = DELETE_ITEM;
            pglist_addItem ( l_lats[ii], l_lons[ii], &add_flag);
        }
        else {
            jj++;
            continue;
        }
    }
    nlist = _curList.elem.lst.data.nitems;
   /*
    * Find the indices of Watch FIPs to be added
    */
    nn = 0;
    for ( ii = 0; ii < nwcnties; ii++ ){
        for ( jj = 0; jj < nmwcn; jj++ ){
            if ( strcmp (w_fips[ii], wcn_fips[jj]) == 0 ) {
                inxrem [nn] = jj;
                nn++;
                break;
            }
        }
    }
    nremove = nn;
   /*
    * Add the WCN FIPs to the Watch FIPs
    */
    jj = 0;
    len_wcnty = sizeof(wcnty) / sizeof(wcnty[0]);
    for ( ii = 0; ii < nmwcn; ii++ ){
        found = FALSE;
        for ( jj = 0; jj < nremove; jj++ ){
            if ( ii == inxrem[jj] ) {
                found = TRUE;
            }
        }
        if (!found) {
            clo_findnum(WBCMZ_TBL, wcnty[ii], len_wcnty, &nret, info, &ier);
            cst_gtag ( "LAT", info,   "-9999", dummy, &ier );
            cst_gtag ( "LAT", info,   "-9999", dummy, &ier );
            cst_crnm ( dummy, &w_lat, &ier );
            cst_gtag ( "LON", info,   "-9999", dummy, &ier );
            cst_crnm ( dummy, &w_lon, &ier );
            _mselMode = ADD_ITEM;
            pglist_addItem ( w_lat, w_lon, &add_flag);
        }
    }
    nlist = _curList.elem.lst.data.nitems;
   /*
    * Save the list element to the work file.
    */
    lstEl = _curList;
    pgvgf_saveNewElm ( cvg_getworkfile(), sys_D, &lstEl, nlist,
                        lstEl.elem.lst.data.lat, lstEl.elem.lst.data.lon,
                        TRUE, &ieloff, &ier);

   /*
    *  bookkeeping  - writing out old LIST element with delete ON
    */
    el.hdr.delete = 1;
    cvg_write(&(el), list_offset, el.hdr.recsz, ofp, FALSE, &ier);

   /*
    *  making it impossible to undo changdes to the VGF file
    *  this is neccesary because ww####.vgf gets rewritten
    */
    pgundo_initUndo ();

   /*
    * Group the list element to the watch 
    */
    pgactv_setActvElm (&wchEl, location);
    pglist_grpToWtch ( ieloff );

   /*
    * Write list element to VGF file
    */
    sprintf(fname, "ww%04d", watch_number);
    strcat (fname, ".vgf");
    pglist_wrtLstWtch (fname, &ieloff);

   /*
    * Display the list element.
    */

    pgpalw_refresh ();

    cvg_rdrec (cvg_getworkfile(), location, &wchEl, &ier);
    layer = pglayer_getCurLayer( );
    crg_set (&wchEl, location, layer, &ier);
    pgactv_setActvElm (&wchEl, location);
    pghdlb_select (&wchEl, location);
    
    _clstStatus = lstat;
    _mselMode = lmode;
}
