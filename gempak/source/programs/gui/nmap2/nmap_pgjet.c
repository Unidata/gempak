#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "drwids.h"
#include "hints.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "proto_xw.h"

#define  ICON_DIR       "$NAWIPS/icons/nmap"
#define ICON_FG		"black"
#define ICON_BG		"bisque1"
#define ICON_WIDTH	24	
#define ICON_HEIGHT	24	

#define MAX_SUBTYPS	2
#define MAX_ACTIONS	3

#define MIN_SIZE_SCALE	 0.1F
#define MAX_SIZE_SCALE	10.0F

#define MIN_WIDTH_SCALE	1
#define MAX_WIDTH_SCALE	10

#define MAX_SMTH_LVLS	3

#define MIN_SPEED	80
#define MAX_SPEED	400

#define MIN_LEVEL	0
#define MAX_LEVEL	999

#define MIN_DELTA 	0	
#define MIN_DELTA0 	-999
#define MAX_DELTA	999

#define SUB_BARB	0	/* Subtypes: Barb */
#define SUB_HASH	1	/*	     Hash */

#define ACT_MOVE	0	/* Actions:  MOVE   */
#define ACT_ADD		1	/* 	     ADD    */	
#define ACT_DELETE	2	/* 	     DELETE */

#define MOVE_BARB	1
#define ADD_BARB	2
#define DELETE_BARB	3
#define MOVE_HASH	4
#define ADD_HASH	5
#define DELETE_HASH	6

#define CLOSEST_DIST	20.0F	/* Tie distance to locate a barb/hash */

static Widget	_pgjetW;
static Widget	_jetAttrBtn;
static Widget	_subtypBtns[MAX_SUBTYPS];
static Widget	_barbActBtns[MAX_ACTIONS];
static Widget	_hashActBtns[MAX_ACTIONS];

static Widget	_pgjet_barbInfoW, _barbinfo_ctlfrmW, _spd_txtW, _lvl_txtW;
static Widget	_delta_txt1W, _delta_txt2W, _barbinfo_arwfrmW;

static Widget	_pgjet_attrW, _attr_clrW, _attr_ctlfrmW;
static Widget	_width_txtW, _width_sldW;
static Widget	_size_frmW, _size_txtW, _size_sldW;
static Widget	_smth_frmW, _smth_optW, _smth_pbW[MAX_SMTH_LVLS];

static WidgetList	_attr_ctlBtns;

static Widget	   _clearRcW;
static Widget	   _clearLblW;
static WidgetList  _clearBtnW;

static int	_jetSubtyp  = -1;	
static int	_jetBarbAct = ACT_MOVE;	
static int	_jetHashAct = ACT_MOVE;	

static int	_attrSplColr;	
static int	_attrSplWdth = MIN_WIDTH_SCALE;
static int	_attrSmth;

static int	_attrBarbColr;	
static int	_attrBarbWdth;
static float	_attrBarbSize;

static int	_attrHashColr;
static int	_attrHashWdth;
static float	_attrHashSize;

static float	_barbSpeed = 100.0F;	/* speed, in unit of knots */
static int	_barbLevel = 300;	/* level, in unit of 100 ft */
static int	_barbDelta1 = 0;	/* delta level */
static int	_barbDelta2 = 0;	

static int	_selectedSub = -1; /* Index of the selected barb/hash */

static Pixel	_yellowPix, _greenPix, _bluePix;
static Pixel	_whitePix, _blackPix;	

static Boolean	_barbClear = True; /* Flag to clear barb background */	


/*
 *  private callback functions
 */
static void pgjet_subtypBtnCb ( Widget, long, XtPointer );
static void pgjet_attrBtnCb   ( Widget, XtPointer, XtPointer );
static void pgjet_barbBtnCb   ( Widget, long, XtPointer );
static void pgjet_hashBtnCb   ( Widget, long, XtPointer );
static void pgjet_spdTxtCb    ( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs );
static void pgjet_lvlTxtCb    ( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs );
static void pgjet_deltaTxt1Cb ( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs );
static void pgjet_deltaTxt2Cb ( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs );
static void pgjet_barbInfoCb  ( Widget, long, XtPointer );
static void pgjet_arrowPbCb   ( Widget, long, XtPointer ); 
static void pgjet_clearCb     ( Widget, long, XtPointer );
static void pgjet_colorCb     ( Widget, XtPointer, XtPointer ); 
static void pgjet_sizSldCb    ( Widget, XtPointer, XmScaleCallbackStruct *cbs );
static void pgjet_sizTxtCb    ( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs );
static void pgjet_widSldCb    ( Widget, XtPointer, XmScaleCallbackStruct *cbs );
static void pgjet_widTxtCb    ( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs );
static void pgjet_smthPbCb    ( Widget, long, XtPointer ); 

/*
 *  Private functions
 */
static void pgjet_createBarbInfo ( Widget parent );
static void pgjet_popupBarbInfo ( void );
static void pgjet_popdownBarbInfo ( void );

static void pgjet_createAttr ( Widget parent );
static Boolean pgjet_isUpAttr ( void );

static void pgjet_switchClr ( void );
static void pgjet_refresh ( void );

static void pgjet_setAttr ( int splcol, int splwdth, char smth,
		     int brbcol, int brbwdth, float brbsiz,
		     int hshcol, int hshwdth, float hshsiz );

static void pgjet_setSpdLvl ( float speed, int level );
static void pgjet_setDelta ( int delta1, int delta2 );

static int pgjet_getNearestSub ( VG_DBStruct el, float xx, float yy, int act );

static void pgjet_eventHandler ( void );
static void pgjet_addHandler ( Widget wid, XtPointer clnt, XEvent *event, Boolean *ctdr );
static void pgjet_mvdelHandler ( Widget wid, XtPointer clnt, XEvent *event, Boolean *ctdr );
static void pgjet_mvdelSub ( Widget wid, XtPointer clnt,XEvent *event, Boolean *ctdr );

static void pgjet_formatFL ( int level, int delta1, int delta2, char *flstr );

/************************************************************************
 * nmap_pgjet.c								*
 *									*
 * This module creates and displays the VG jet editing box. It also	*
 * contains the callbacks for the box.					*
 *									*
 * CONTENTS:								*
 *  pgjet_create()     		create jet editing window		*
 *  pgjet_popup()      		pop up jet editing window		*
 *  pgjet_popdown()    		pop down jet editing window		*
 *  pgjet_createBarbInfo()	create barb speed/level input window	*
 *  pgjet_popup()      		pop up barb speed/level input window	*
 *  pgjet_popdown()		pop down barb speed/level input window	*
 *  pgjet_createAttr()		create jet attribute editing window	*
 *  pgjet_popupAttr()      	pop up jet attribute editing window	*
 *  pgjet_popdownAttr()		pop down jet attribute editing window	*
 *									*
 *  pgjet_isUp()		querry if the jet editing window is up	*
 *  pgjet_isUpAttr()		querry if the attr. editing window is up*
 *									*
 *  pgjet_subtypBtnCb()		callback for subtype selection		*
 *  pgjet_attrBtnCb()		callback for show/hide attr. button	*
 *  pgjet_barbBtnCb()		callback for barb action buttons	*
 *  pgjet_hashBtnCb()		callback for hash action buttons	*
 *  pgjet_spdTxtCb()		callback for barb speed text widget	*
 *  pgjet_lvlTxtCb()		callback for barb level text widget	*
 *  pgjet_barbInfoCb()		callback for barb info. control buttons	*
 *  pgjet_arrowPbCb()		callback for arrow buttons on barb info	*
 *  pgjet_widSldCb()		callback for width scale		*
 *  pgjet_widTxtCb()		callback for width text widget		*
 *  pgjet_sizeSldCb()		callback for size scale        		*
 *  pgjet_sizTxtCb()		callback for size text widget		*
 *  pgjet_clearCb()		callback for clear control buttons	*
 *  pgjet_colorCb()		callback for color button        	*
 *  pgjet_smthPbCb()		Callback for jet line smoothing level	*
 *									*
 *  pgjet_switchClr()		set button colors in jet editing win.	*
 *  pgjet_refresh()		refresh jet display & place handbar	*
 *  pgjet_getAttr()		get attributes in jet attribute win.	*
 *  pgjet_setAttr()		set attributes in jet attribute win.	*
 *  pgjet_setSpdLvl()		set speedlevel in barb information win.	*
 *  pgjet_setDelta()		set delats in barb information win.	*
 *  pgjet_getCurSubtyp()	get the index of the selected barb/hash	*
 *  pgjet_getSelectedSub()	get the current subtype mode		*
 *  pgjet_getNearestSub()	locate the nearest barb/hash on jet line*
 *  pgjet_eventHandler()	handle editing events for a selected jet*
 *  pgjet_addHandler()		add barb/hash onto a selected jet line	*
 *  pgjet_mvdelHandler()	handle MOVE/DELETE events of barb/hash	*
 *  pgjet_mvdelSub()		move or delete selected barb/hash	*
 *  pgjet_formatFL()		format flight level text as "FLxxx"	*
 ***********************************************************************/

/*=====================================================================*/

Widget pgjet_create ( Widget parent )
/************************************************************************
 * pgjet_create								*
 *									*
 * This function creates a jet editing box.				*
 *									*
 * void pgjet_create ( parent )						*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03   intial coding				*
 * J. Wu/SAIC		12/03   revise to make GUI compact		*
 * E. Safford/SAIC	08/04	init width (at top) to avoid warning msg*
 * T. Piper/SAIC	10/05	declared ii long			*
 * M. Li/SAIC		10/05	Added a call to ctb_rdprf		*
 * J. Lewis/AWC         03/07   Remove check for new flight level format*
 ***********************************************************************/
{
    Widget	pane, subtyp_form;
    Widget	action_form, action_rc, showattr_form;
    Widget	barb_form, barb_rc, hash_form, hash_rc;
    
    char	*subtyp_str[] = { "Barb", "Hash" };
    char	*action_str[] = { " MOVE", "   ADD", "DELETE" };
    
    XmString	xmstr;
    long		ii;
/*---------------------------------------------------------------------*/

    /*
     *  Create the VG jet box for adding/moving/deleting barbs & hashs.
     */
    _pgjetW = XmCreateFormDialog ( parent, "pgjet_popup", NULL, 0 );
    
    XtVaSetValues ( _pgjetW,
		XmNnoResize,		TRUE,
		XmNautoUnmanage,	FALSE,
		NULL );

    xmstr = XmStringCreateLocalized ( "JET" );
    XtVaSetValues ( _pgjetW, XmNdialogTitle, xmstr, NULL );
    XmStringFree(xmstr);

    pane  = XtVaCreateWidget ( "pgjet_pane",
		xmPanedWindowWidgetClass,	_pgjetW,
		XmNsashWidth,			1,
		XmNsashHeight,			1,
		NULL );

    /*
     * creat jet sub-element selections - "Barb", or "Hash"
     */
    subtyp_form = XtVaCreateWidget ( "jet_subtyp_form",
		xmFormWidgetClass,		pane,
		NULL );    
         
    for ( ii = 0; ii < MAX_SUBTYPS; ii++ ) {
	if ( ii == 0 ) {
	    _subtypBtns[ii] = XtVaCreateManagedWidget ( subtyp_str[ii],
		xmPushButtonWidgetClass,	subtyp_form, 
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			4,		
		XmNbottomAttachment,		XmATTACH_FORM,
		XmNbottomOffset,		4,		
		XmNleftAttachment,		XmATTACH_FORM,		
		XmNleftOffset,			7,		
		XmNwidth,			65,
		XmNheight,			30,
		XmNtraversalOn,			False,  
		NULL );
        }
	else {
	    _subtypBtns[ii] = XtVaCreateManagedWidget ( subtyp_str[ii],
		xmPushButtonWidgetClass,	subtyp_form, 
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			4,		
		XmNbottomAttachment,		XmATTACH_FORM,
		XmNbottomOffset,		4,		
		XmNleftAttachment,		XmATTACH_WIDGET,		
		XmNleftWidget,			_subtypBtns[ii-1],
		XmNleftOffset,			9,		
		XmNwidth,			65,
		XmNheight,			30,
		XmNtraversalOn,			False,  
		NULL );	
	}
	
	XtAddCallback ( _subtypBtns[ii],
		XmNarmCallback, (XtCallbackProc)pgjet_subtypBtnCb,
		(XtPointer)ii );
    }

    XtManageChild ( subtyp_form );
    
    /*
     *  create jet action selection area
     */
    action_form = XtVaCreateWidget ( "jet_action_form",
		xmFormWidgetClass,		pane,
		NULL );    
         
    action_rc = XtVaCreateWidget ( "jet_action_rc",
		xmRowColumnWidgetClass, 	action_form,
		XmNorientation, 		XmHORIZONTAL,
		XmNradioAlwaysOne,		FALSE,
		NULL );
    
    /*
     *  create jet barb action options - "MOVE", "ADD", or "DELETE"
     */
    barb_form = XtVaCreateWidget ( "jet_barb_form",
		xmFormWidgetClass,		action_rc,
		NULL );    

    barb_rc = XtVaCreateWidget ( "jet_barb_rc",
		xmRowColumnWidgetClass, 	barb_form,
		XmNorientation,         	XmVERTICAL,
		XmNpacking,             	XmPACK_TIGHT,
		XmNradioBehavior,		False,
		NULL );
         
    for ( ii = 0; ii < MAX_ACTIONS; ii++ ) {
	_barbActBtns[ii] = XtVaCreateManagedWidget ( action_str[ii],
		xmPushButtonWidgetClass,	barb_rc, 
		XmNtraversalOn,			False,  
		NULL );

	XtAddCallback ( _barbActBtns[ii],
		XmNarmCallback, (XtCallbackProc)pgjet_barbBtnCb,
		(XtPointer)ii );
    }

    XtManageChild ( barb_rc );
    XtManageChild ( barb_form );
    
    /*
     *  create jet hash action options - "MOVE", "ADD", or "DELETE".
     */
    hash_form = XtVaCreateWidget ( "jet_hash_form",
		xmFormWidgetClass,		action_rc,
		NULL );    

    hash_rc = XtVaCreateWidget ( "jet_hash_rc",
		xmRowColumnWidgetClass, 	hash_form,
		XmNorientation,         	XmVERTICAL,
		XmNpacking,             	XmPACK_TIGHT,
		XmNradioBehavior,		False,
		NULL );
         
    for ( ii = 0; ii < MAX_ACTIONS; ii++ ) {
	_hashActBtns[ii] = XtVaCreateManagedWidget ( action_str[ii],
		xmPushButtonWidgetClass,	hash_rc, 
		XmNtraversalOn,			False,  
		NULL );

	XtAddCallback ( _hashActBtns[ii],
		XmNarmCallback, (XtCallbackProc)pgjet_hashBtnCb,
		(XtPointer)ii );
    }

    XtManageChild ( hash_rc );
    XtManageChild ( hash_form );

    /*
     *  Manage action panel
     */
    XtManageChild ( action_rc );
    XtManageChild ( action_form );


    /*
     *  create jet attribute edit option - "Hide/Show Attributes"
     */
    showattr_form = XtVaCreateWidget ( "jet_showattr_form",
		xmFormWidgetClass,		pane,
		NULL );    

    xmstr = XmStringCreateLtoR ( " Show Attributes ",
                                 XmFONTLIST_DEFAULT_TAG );    
    _jetAttrBtn = XtVaCreateManagedWidget( "Show Attributes",
		xmPushButtonWidgetClass,	showattr_form,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			4,		
		XmNbottomAttachment,		XmATTACH_FORM,
		XmNbottomOffset,		4,		
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			11,		
		XmNlabelType,			XmSTRING,
		XmNlabelString,			xmstr,
		XmNheight,			30,
		XmNtraversalOn,			False,  
		NULL );
    XmStringFree ( xmstr );

    XtAddCallback ( _jetAttrBtn,
    		XmNactivateCallback, (XtCallbackProc)pgjet_attrBtnCb,
		NULL );
    
    XtManageChild ( showattr_form );
    
    XtManageChild ( pane );

    /*
     *  create jet attribute edit window and barb info window 
     */
    pgjet_createAttr ( parent );
    pgjet_createBarbInfo ( parent );
    
    /*
     *  retrieve color map for YELLOW/GREEN/BLUE 
     */
    _yellowPix = NxmColrP_getColorPixel(5);
    _greenPix = NxmColrP_getColorPixel(3);
    _bluePix = NxmColrP_getColorPixel(4);
    _whitePix = NxmColrP_getColorPixel(31);
    _blackPix = NxmColrP_getColorPixel(32);

    return ( _pgjetW );

}

/*=====================================================================*/

void pgjet_popup ( XtCallbackProc callback )
/************************************************************************
 * pgjet_popup								*
 *									*
 * This function pops up the VG jet editing window.			*
 *									*
 * pgjet_popup ( callback )    						*
 *									*
 * Input parameters:							*
 *    callback	XtCallbackProc	Callback function for control buttons	*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03   initial coding				*
 * J. Wu/SAIC		12/03   adjust for GUI change			*
 * M. Li/SAIC		07/04	get default attributes from setting.tbl	*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    int		ier;
    long	ii;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
    
    if ( XtIsManaged ( _pgjetW ) ) {
	XtUnmanageChild ( _pgjetW );
    }

    XtVaSetValues ( _jetAttrBtn,
		XmNbackground,	_bluePix,
		XmNforeground,	_whitePix,
		NULL );
    
    /*
     * Initialize actions to MOVE and set proper background/foreground
     * colors for each button.
     */
    _jetSubtyp = -1;
    _jetBarbAct = ACT_MOVE;	
    _jetHashAct = ACT_MOVE;	

    pgjet_switchClr ();
                           
    XtManageChild ( _pgjetW );


    /*
     *  Read and apply the default attribute settings.
     */
    el.hdr.vg_type = JET_ELM;
    el.hdr.vg_class = CLASS_MET;

    ces_get ( -99, &el, &ier );

    _attrSplColr  = el.elem.jet.line.splcol;
    _attrSplWdth  = el.elem.jet.line.spl.info.splwid;
    _attrSmth     = el.hdr.smooth;

    _attrBarbColr = el.elem.jet.barb[0].wndcol;
    _attrBarbWdth = el.elem.jet.barb[0].wnd.info.width;
    _attrBarbSize = el.elem.jet.barb[0].wnd.info.size;

    _attrHashColr = el.elem.jet.hash[0].wndcol;
    _attrHashWdth = el.elem.jet.hash[0].wnd.info.width;
    _attrHashSize = el.elem.jet.hash[0].wnd.info.size;

    pgjet_setAttr ( el.elem.jet.line.splcol,
		    el.elem.jet.line.spl.info.splwid,
	            el.hdr.smooth,
	            el.elem.jet.barb[0].wndcol,
	            el.elem.jet.barb[0].wnd.info.width,
	            el.elem.jet.barb[0].wnd.info.size,	    	
	            el.elem.jet.hash[0].wndcol,
	            el.elem.jet.hash[0].wnd.info.width,
	            el.elem.jet.hash[0].wnd.info.size );	    	
                     
    if ( callback ) {
	for ( ii = 0; ii < 2; ii++ ) {
            XtRemoveAllCallbacks ( _attr_ctlBtns[ii], XmNactivateCallback );
  	    XtAddCallback ( _attr_ctlBtns[ii], XmNactivateCallback, 
			        callback, (XtPointer)ii );
	}
    }

}

/*=====================================================================*/

void pgjet_popdown ( void )
/************************************************************************
 * pgjet_popdown							*
 *									*
 * This function pops down the jet editing box.				*
 *									*
 * void pgjet_popdown ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03	initail coding	         		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( XtIsManaged ( _pgjetW ) ) {    	
	_selectedSub = -1;
	XtUnmanageChild ( _pgjetW );        
    }
    
    pgjet_popdownAttr();
    pgjet_popdownBarbInfo();

}

/*=====================================================================*/

Boolean pgjet_isUp ( void )
/************************************************************************
 * pgjet_isUp								*
 *									*
 * Queries whether the JET editing dialog is managed or not.		*
 *									*
 * Boolean pgjet_isUp ( void )						*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	pgjet_isUp()	Boolean	    True -- managed (up), False -- down	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03	initial colding				*
 ***********************************************************************/
{
    return ( XtIsManaged ( _pgjetW ) );
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_subtypBtnCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * pgjet_subtypBtnCb                                               	*
 *                                                                      *
 * Callback for jet sub-element type push buttons.			*
 *                                                                      *
 * static void pgjet_subtypBtnCb ( w, which, cbs )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	w             Widget     	widget ID			*
 *	which         long        	which type			*
 *	cbs          XtPointer  	not used			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           10/03   initial coding				*
 * J. Wu/SAIC           12/03   adjust for GUI change			*
 ***********************************************************************/
{
    Boolean	subtype_changed = False;
/*---------------------------------------------------------------------*/
 	
    switch( which ) {

      case 0:	/* Barb */
	
	if ( _jetSubtyp != SUB_BARB ) {
	    _jetSubtyp = SUB_BARB;
	    subtype_changed = True;
	    
	    if ( _jetBarbAct == ACT_ADD ) {
	        pgjet_popupBarbInfo ();
	    }
	}
	break;

      case 1:	/* Hash */
	if ( _jetSubtyp != SUB_HASH ) {
	    _jetSubtyp = SUB_HASH;
	    subtype_changed = True;
	    
	    pgjet_popdownBarbInfo ();
	}
	break;
    }
    
    if ( subtype_changed ) {
        /*
         *  Reset background color for subtype/action buttons 
         */
        pgjet_switchClr ();
        

        /*
         *  Pop down attribute window
         */
        pgjet_popdownAttr ();


        /*
         *  Refresh display & dispatch events.
         */
	pgjet_refresh ();
        pgjet_eventHandler ();
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_attrBtnCb ( Widget w, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgjet_attrBtnCb                                               	*
 *                                                                      *
 * Callback for jet attribute hide/show push buttons.			*
 *                                                                      *
 * static void pgjet_attrBtnCb ( w, clnt, cbs )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	w		Widget     	widget ID			*
 *	clnt	XtPointer       not used			*
 *	cbs	XtPointer  	not used			*
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	none			                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           10/03   initial coding				*
 ***********************************************************************/
{    

    if ( pgjet_isUpAttr() ) {  
       pgjet_popdownAttr ();
    }
    else {
       pgjet_popupAttr ();
    }                
    
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_barbBtnCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * pgjet_barbBtnCb                                               	*
 *                                                                      *
 * Callback for jet barb action push buttons.				*
 *                                                                      *
 * static void pgjet_barbBtnCb ( w, which, cbs )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	w             Widget     	widget ID			*
 *	which         long        	which action			*
 *	cbs          XtPointer  	not used			*
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	none			                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           10/03   initial coding				*
 * J. Wu/SAIC           12/03   adjust for GUI change			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	    
    if ( _jetSubtyp != SUB_BARB || _jetBarbAct != (int)which ) {

        /*
         *  Set subtype and action, then switch button color.
         */
        _jetSubtyp = SUB_BARB;
        _jetBarbAct = (int)which;
    
        pgjet_switchClr ();
    
    
        /*
         *  pop up barb information window for "ADD"
         */
        if ( _jetBarbAct == ACT_ADD ) {	/* Add */
            pgjet_popupBarbInfo ();
        }
        else {			/* Move/Delete */
            pgjet_popdownBarbInfo ();    
        }

        /*
         *  Pop down attribute window.
         */
        pgjet_popdownAttr ();
        
        /*
         *  Refresh display & dispatch events.
         */
	pgjet_refresh ();
        pgjet_eventHandler ();
    
    }      
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_hashBtnCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * pgjet_hashBtnCb                                               	*
 *                                                                      *
 * Callback for jet hash action push buttons.				*
 *                                                                      *
 * static void pgjet_hashBtnCb ( w, which, cbs )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	w             Widget     	widget ID			*
 *	which         long        	which action			*
 *	cbs          XtPointer  	not used			*
 *                                                                      *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	none			                                	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           10/03   initial coding				*
 * J. Wu/SAIC           12/03   adjust for GUI change			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( _jetSubtyp != SUB_HASH || _jetHashAct != (int)which ) {
        
        /*
         *  Set subtype and action, then switch button color.
         */
        _jetSubtyp = SUB_HASH;
        _jetHashAct = (int)which;
	
	pgjet_switchClr ();
	
        
        /*
         *  Pop down attribute window & barb information window.
         */
        pgjet_popdownAttr ();
        pgjet_popdownBarbInfo ();
        

        /*
         *  Refresh display & dispatch events.
         */
	pgjet_refresh ();
	pgjet_eventHandler ();
    
    }
}

/*=====================================================================*/

int pgjet_getCurSubtyp ( void )
/************************************************************************
 * pgjet_getCurSubtyp							*
 *									*
 * Gets the current jet subtype mode. 					*
 *									*
 * int pgjet_getCurSubtyp ( void )					*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	pgjet_getCurSubtyp()	int	  current jet subtype mode	*
 *					    				*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/03	initial colding				*
 ***********************************************************************/
{  
    return ( _jetSubtyp );
}

/*=====================================================================*/

static void pgjet_createBarbInfo ( Widget parent )
/************************************************************************
 *  pgjet_createBarbInfo						*
 *                                                                      *
 * Creates the input window for jet barb speed/level.			*
 *                                                                      *
 * static void pgjet_createBarbInfo ( parent )	                   	*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget		parent widget ID                  	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 * 	none		                                       		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/03	initial	coding				*
 * J. Wu/SAIC		02/04	add delta information			*
 * J. Wu/SAIC		03/04	add arrow buttons to move FL text	*
 * J. Wu/SAIC		03/04	retrieve arrow bitmaps from ICON_DIR	*
 * E. Safford/SAIC	05/05	free ctl_btns WidgetList		*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 * M. Li/SAIC		10/05	checked for new flight level format	*
 * J. Lewis/AWC         03/07   remove check for new flight level format*
 ***********************************************************************/
{
    int		ier;
    long	ii, ignore, nn;
    char	*btnstr[] = {"Apply", "Cancel"};
    char	*arrow_btnstr[] = { "arrow_ul", "arrow_up", "arrow_ur",
                        "arrow_left", "arrow_circle", "arrow_right",
			"arrow_ll", "arrow_down", "arrow_lr" };
    char	iconfile[256], filename[256], icon_tip[15];
    Widget	rc, rc1, rc2, rc3, label;
    Widget	arrow_rc;
    XmString	title_string;
    WidgetList	ctl_btns;
/*---------------------------------------------------------------------*/
        
    /*
     * create a new dialog form.
     */
    _pgjet_barbInfoW = XmCreateFormDialog ( parent, "pgjet_barbpopup",
    			NULL, 0 );

    title_string = XmStringCreateLocalized ("Barb Info");

    XtVaSetValues ( _pgjet_barbInfoW, 
		XmNnoResize,        	True, 
		XmNdialogTitle,		title_string,
		XmNautoUnmanage,	False, 	
		NULL );
    XmStringFree ( title_string );

    rc = XtVaCreateWidget("barbinfo_rc",
                xmRowColumnWidgetClass,      	_pgjet_barbInfoW,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			5,
                XmNorientation,              	XmVERTICAL,
                XmNpacking,                  	XmPACK_TIGHT,
                NULL);

    /*
     * create label/text field for speed input.
     */
    rc1 = XtVaCreateWidget ( "barbinfo_rc1",
                xmRowColumnWidgetClass,      	rc,
                XmNorientation,              	XmHORIZONTAL,
                XmNpacking,                  	XmPACK_TIGHT,
                NULL );
    
    label = XtVaCreateManagedWidget ( "speed",
		xmLabelWidgetClass,	rc1,
		NULL );
    NxmLabel_setStr ( label, "SPEED" );
	 
    _spd_txtW = XtVaCreateManagedWidget ( "spd_text",
		xmTextFieldWidgetClass,		rc1,
        	XmNcolumns,			5,
        	XmNvalue,			"",
        	XmNcursorPositionVisible,	True,
  		XmNmaxLength,			10, 
		NULL );
    XtAddCallback ( _spd_txtW, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosFltCb, NULL); 
    XtAddCallback ( _spd_txtW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgjet_spdTxtCb, NULL );
   
    label = XtVaCreateManagedWidget ( "speed_unit",
		xmLabelWidgetClass,	rc1,
		NULL );
    NxmLabel_setStr ( label, "KTS" );


    XtManageChild(rc1);


    /*
     * create label/text field for level input.
     */
    rc2 = XtVaCreateWidget ( "barbinfo_rc2",
                xmRowColumnWidgetClass,      	rc,
                XmNorientation,              	XmHORIZONTAL,
                XmNpacking,                  	XmPACK_TIGHT,
                NULL);
    
    label = XtVaCreateManagedWidget ( "level",
		xmLabelWidgetClass,		rc2,
		NULL );
    NxmLabel_setStr ( label, "LEVEL " );
	 
    _lvl_txtW = XtVaCreateManagedWidget ( "lvl_text",
		xmTextFieldWidgetClass,		rc2,
       		XmNcolumns,			5,
        	XmNvalue,			"",
        	XmNcursorPositionVisible,	True,
  		XmNmaxLength,			10, 
        	NULL );
    XtAddCallback ( _lvl_txtW, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback ( _lvl_txtW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgjet_lvlTxtCb, NULL );
    
    label = XtVaCreateManagedWidget ( "level_unit",
		xmLabelWidgetClass,		rc2,
		NULL );
    NxmLabel_setStr ( label, "100 ft" );

    XtManageChild( rc2 );
   
    
    /*
     * create label/text field for delta input.
     */
    rc3 = XtVaCreateWidget ( "barbinfo_rc2",
                xmRowColumnWidgetClass,      	rc,
                XmNorientation,              	XmHORIZONTAL,
                XmNpacking,                  	XmPACK_TIGHT,
                NULL);
    
    label = XtVaCreateManagedWidget ( "delta",
		xmLabelWidgetClass,		rc3,
		NULL );
    	NxmLabel_setStr ( label, "TOP/BOT" );
	 
    _delta_txt1W = XtVaCreateManagedWidget ( "delta_text1",
		xmTextFieldWidgetClass,		rc3,
       		XmNcolumns,			3,
        	XmNvalue,			"",
        	XmNcursorPositionVisible,	True,
  		XmNmaxLength,			5, 
        	NULL );
    XtAddCallback ( _delta_txt1W, XmNmodifyVerifyCallback, 
                      (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback ( _delta_txt1W, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgjet_deltaTxt1Cb, NULL );
    
    label = XtVaCreateManagedWidget ( "dummy",
		xmLabelWidgetClass,		rc3,
		NULL );
    NxmLabel_setStr ( label, "/" );

    _delta_txt2W = XtVaCreateManagedWidget ( "delta_text2",
		xmTextFieldWidgetClass,		rc3,
       		XmNcolumns,			4,
        	XmNvalue,			"",
        	XmNcursorPositionVisible,	True,
  		XmNmaxLength,			5, 
        	NULL );
	XtAddCallback ( _delta_txt2W, XmNmodifyVerifyCallback,
                        (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);
    XtAddCallback ( _delta_txt2W, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgjet_deltaTxt2Cb, NULL );
    XtManageChild( rc3 );
    
    XtManageChild ( rc );
     
    /*
     *  create arrow buttons.
     */
    nn = XtNumber ( arrow_btnstr );
    _barbinfo_arwfrmW = XtVaCreateManagedWidget ( "ctnl_form",
		xmFormWidgetClass,		_pgjet_barbInfoW,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			rc,
		XmNtopOffset,			2,
		NULL );
    
    arrow_rc = XtVaCreateWidget ( "arrow_rc",
                xmRowColumnWidgetClass,      	_barbinfo_arwfrmW,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			0,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			35,
		XmNorientation,			XmHORIZONTAL,
                XmNpacking,                  	XmPACK_COLUMN,
                XmNspacing,			0,
		XmNnumColumns,			3,
		XmNadjustLast,			FALSE,
                NULL);
    
    for ( ii = 0; ii < nn; ii++ ) {
	sprintf ( iconfile, "%s.xbm", arrow_btnstr[ii] );
	cfl_inqr ( iconfile, ICON_DIR, &ignore, filename, &ier );
   	
        strcpy ( icon_tip, "Move Text" );
        NxmBxmBtn_create( arrow_rc, iconfile, NULL, 
    		ICON_WIDTH, ICON_HEIGHT, ICON_FG, ICON_BG, NULL, 
		filename, icon_tip, True,
		(XtCallbackProc)pgjet_arrowPbCb, (XtPointer)ii );
        	
    }
    
    XtManageChild ( arrow_rc );
    

    /*
     * create control buttons.
     */
    ctl_btns = (WidgetList)XtMalloc(XtNumber(btnstr) * sizeof(Widget));    
        
    _barbinfo_ctlfrmW = NxmCtlBtn_create ( _pgjet_barbInfoW, 0, "ctlBtns", 
                   XtNumber(btnstr), btnstr, NULL, ctl_btns );

    XtVaSetValues ( _barbinfo_ctlfrmW, 
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			_barbinfo_arwfrmW,
		XmNtopOffset,			2,
                XmNmarginWidth,			25,
                XmNspacing,			25,
		NULL );
		                    
    for ( ii = 0; ii < 2; ii++ ) {
        XtAddCallback ( ctl_btns[ii], XmNactivateCallback,
   			(XtCallbackProc)pgjet_barbInfoCb, (XtPointer)ii );
    }
 
    free( ctl_btns );
}

/*=====================================================================*/

static void pgjet_popupBarbInfo ( void )
/************************************************************************
 * pgjet_popupBarbInfo							*
 *									*
 * This function shows jet barb speed/level window.			*
 *									*
 * static void pgjet_popupBarbInfo ( )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03   initial coding				*
 * J. Wu/SAIC		02/04	add flight level delta information	*
 * J. Wu/SAIC		02/04	reset delta fields properly		*
 * J. Wu/SAIC		03/04	add _barbinfo_arwfrmW			*
 * M. Li/SAIC		10/05	Checked for delta format		*
 * J. Lewis/AWC         03/07   Remove check for delta format           *
 * J. Lewis/AWC         03/07   Add check for ADD_BARB action           *
 * J. Lewis/AWC         03/07   If deltas are 0 don't write to string   *
 * J. Lewis/AWC         08/07	Remove forced location of window	*
 ***********************************************************************/
{
    char	str[10];
/*---------------------------------------------------------------------*/

    if ( XtIsManaged ( _pgjet_barbInfoW ) ) {
	XtUnmanageChild ( _pgjet_barbInfoW );
    }            
           
    XmTextFieldSetString ( _spd_txtW, "" );	
    sprintf ( str, "%.1f", _barbSpeed );
    XmTextFieldSetString ( _spd_txtW, str );

    XmTextFieldSetString ( _lvl_txtW, "" );
    sprintf ( str, "%i", _barbLevel );
    XmTextFieldSetString ( _lvl_txtW, str );
        
    if ( pgjet_getCurAction() == ADD_BARB ) {
        _barbDelta1 = 0;
        _barbDelta2 = 0;
    }
    XmTextFieldSetString ( _delta_txt1W, "" );
    if ( _barbDelta1 != 0 ) {
        sprintf ( str, "%i", _barbDelta1 );
        XmTextFieldSetString ( _delta_txt1W, str );
    }
    
    XmTextFieldSetString ( _delta_txt2W, "" );
    if ( _barbDelta2 != 0 ) {
        sprintf ( str, "%i", _barbDelta2 );
        XmTextFieldSetString ( _delta_txt2W, str );
    }
    
    if ( pgjet_getCurAction() == MOVE_BARB ) {
	XtManageChild ( _barbinfo_arwfrmW );
	XtManageChild ( _barbinfo_ctlfrmW );
    }
    else {
        XtUnmanageChild ( _barbinfo_arwfrmW );    
	XtUnmanageChild ( _barbinfo_ctlfrmW );
    }

    XtManageChild ( _pgjet_barbInfoW );
    
}

/*=====================================================================*/

static void pgjet_popdownBarbInfo ( void )
/************************************************************************
 * pgjet_popdownBarbInfo						*
 *									*
 * This function unmanages the jet barb speed/level window		*
 *									*
 * static void pgjet_popdownBarbInfo ( )				*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03	initail coding	         		*
 ***********************************************************************/
{
    if ( XtIsManaged ( _pgjet_barbInfoW ) ) {
    	XtUnmanageChild ( _pgjet_barbInfoW );
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_spdTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgjet_spdTxtCb							*
 *                                                                      *
 * Callback for barb speed text widget.					*
 *                                                                      *
 * static void pgjet_spdTxtCb ( w, clnt, cbs )	                *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 * Output parameters:							*
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/03	initial coding				*
 * J. Wu/SAIC		02/04	set input limitation			*
 ***********************************************************************/
{
    char	*ss;
    int		speed;
/*---------------------------------------------------------------------*/

    /*
     * Confirm there is an event.  If not, this text has already
     * been set up.
     */
    if ( !cbs->event ) 
	return;

    /*
     * get the new speed input.
     */
    ss = XmTextFieldGetString( _spd_txtW );
    speed = (float) atof ( ss );
    XtFree ( ss );

    if ( speed < (float)MIN_SPEED ) {
        _barbSpeed = (float)MIN_SPEED;
    }
    else if ( speed > (float)MAX_SPEED ) {
        _barbSpeed = (float)MAX_SPEED;    
    }
    else {
	_barbSpeed = speed;
    }
    
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_lvlTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgjet_lvlTxtCb							*
 *                                                                      *
 * Callback for barb level text widget.					*
 *                                                                      *
 * static void pgjet_lvlTxtCb ( w, clnt, cbs )	                *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 * Output parameters:							*
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/03	initial coding				*
 * J. Wu/SAIC		02/04	set input limitation			*
 * M. Li/SAIC		07/06	Update deltas after flight level changes*
 * J. Lewis/AWC         03/07   Remove check for new format             *
 *                              Remove updating of deltas               *
 *                              Allow 2-digit flight level value        *
 *                              Remove check for delta min/max          * 
 ***********************************************************************/
{
    char	*ss;
    int		level;
/*---------------------------------------------------------------------*/

    /*
     * Confirm there is an event.  If not, this text has already
     * been set up.
     */
    if ( !cbs->event ) 
	return;

    /*
     * get the new level input.
     */
    ss = XmTextFieldGetString( _lvl_txtW );
    if ( strlen ( ss ) > 2) {
    level = atoi ( ss );
    }
    else {
        level = ( atoi ( ss ) ) * 10;
    }
    XtFree ( ss );

    if ( level <= MIN_LEVEL ) {
        _barbLevel = 300;
    }
    else if ( level > MAX_LEVEL ) {
       _barbLevel =  MAX_LEVEL;
    }
    else {
	_barbLevel = level;    
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_deltaTxt1Cb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgjet_deltaTxt1Cb							*
 *                                                                      *
 * Callback for barb level delta text widget 1.				*
 *                                                                      *
 * static void pgjet_deltaTxt1Cb ( w, clnt, cbs )		*
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 * Output parameters:							*
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		02/04	adjust input limitation			*
 * M. Li/SAIC		10/05	Added a check for delta format		*
 * J. Lewis/AWC         03/07   Remove check for delta format           *
 *                              Allow 2-digit delta                     *
 *                              Set delta to zero on invalid input      *
 ***********************************************************************/
{
    char	*ss;
    int		delta;
/*---------------------------------------------------------------------*/

    /*
     * Confirm there is an event.  If not, this text has already
     * been set up.
     */
    if ( !cbs->event ) 
	return;

    /*
     * get the new speed input.
     */
    ss = XmTextFieldGetString( _delta_txt1W );
    if ( strlen ( ss ) > 2 ) {
    delta = atoi ( ss );
    }
    else {
        delta = ( atoi ( ss ) ) * 10;
    }

    XtFree ( ss );

    /*
     * Check delta value.
     */
    if ( delta < _barbLevel ) {           
        delta = 0;                                  
    }                                              

    if ( delta > MAX_DELTA ) {
        _barbDelta1 = 0;
    }
    else {
        _barbDelta1 = delta;
    }

}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_deltaTxt2Cb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgjet_deltaTxt2Cb							*
 *                                                                      *
 * Callback for barb level delta text widget 2.				*
 *                                                                      *
 * static void pgjet_deltaTxt2Cb ( w, clnt, cbs )		*
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 * Output parameters:							*
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		02/04	adjust input limitation			*
 * M. Li/SAIC		10/05	Added a check for delta format		*
 * J. Lewis/AWC         03/07   Remove check for delta format           *
 *                              Allow 2-digit delta                     *
 ***********************************************************************/
{
    char	*ss;
    int		delta;
/*---------------------------------------------------------------------*/

    /*
     * Confirm there is an event.  If not, this text has already
     * been set up.
     */
    if ( !cbs->event ) 
	return;

    /*
     * get the new speed input.
     */
    ss = XmTextFieldGetString( _delta_txt2W );
    if ( strlen ( ss ) > 2 ) { 
        delta = atoi ( ss ); 
    }
    else {
        delta = ( atoi ( ss ) ) * 10;
    } 
    
    XtFree ( ss );

   /*
    * Check delta value.  
    */

    if ( delta > _barbLevel ) {
	delta = 0;
    } 

    if ( delta < MIN_DELTA ) {
        _barbDelta2 = 0;
    }
    else {
	_barbDelta2 = delta;
    }        
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_arrowPbCb ( Widget wid, long which, XtPointer cbs ) 
/************************************************************************
 * pgjet_arrowPbCb							*
 *									*
 * Callback for the arrow buttons to move the flight level text.	*
 *									*
 * static void pgjet_arrowPbCb ( wid, which, cbs )			*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding				*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		np, ier, loco, loc, sel;
    int		xoff, yoff, yoffset, lwfactor;
    float	llx, lly, urx, ury, *xs, *ys;
    VG_DBStruct	el; 
/*---------------------------------------------------------------------*/
    
    xoff = 0;
    yoff = 0;
    sel = pgjet_getSelectedSub ();

    if ( sel >= 0 ) {
    
        loc = pgactv_getElmLoc ();
        pgutls_prepNew ( loc, &el, &llx, &lly, &urx, &ury, &ier );

	xoff = el.elem.jet.barb[sel].spt.info.offset_x;
	yoff = el.elem.jet.barb[sel].spt.info.offset_y;

	switch ( which ) {
        
	    case 0:	/* Upper Left */
	        xoff--;        
	        yoff++;                         	    	    
	    break;        
	    
	    case 1:	/* Up */
	        yoff++;                         	    	    
	    break;        

	    case 2:	/* Upper Right */
	        xoff++;                         	    	    
	        yoff++;                         	    	    
	    break;        
	    
	    case 3:	/* Left */
	        xoff--;                         	    	    
	    break;        
	    
	    case 4:	/* Center, return to default position */
		xoff = 0;
		
	        if ( strchr( el.elem.jet.barb[sel].spt.text, '\n') == NULL ) {
	            yoffset = 2;  /* no deltas in FL text, 1 line */
	        }
	        else {
	            yoffset = 3;  /* there are 2 lines of FL text */
	        }

		lwfactor = (int)(el.elem.jet.line.spl.info.splwid / 14.0);
				        
		if ( el.elem.jet.barb[sel].spt.info.lat >= 0.0 ) {		
		    yoff = -yoffset - lwfactor;
                }
		else {
		    yoff = yoffset + lwfactor;		
		}
		
	    break;        
	    
	    case 5:	/* Right */
	        xoff++;                         	    	    
	    break;        
	    
	    case 6:	/* Lower Left */
	        xoff--;                         	    	    
	        yoff--;                         	    	    
	    break;        	
	    
	    case 7:	/* Down */
	        yoff--;                         	    	    
	    break;        

	    case 8:	/* Lower Right */
	        xoff++;                         	    	    
	        yoff--;                         	    	    
	    break;        
        } 
       
        pgactv_getDevPts ( &np, &xs, &ys );
 
        pgundo_newStep ();
        pgundo_storeThisLoc ( loc, UNDO_DEL, &ier );        
	
        el.elem.jet.barb[sel].spt.info.offset_x = xoff;
        el.elem.jet.barb[sel].spt.info.offset_y = yoff;

        pgvgf_saveNewElm ( NULL, sys_D, &el, np, xs, ys, TRUE, &loco, &ier);
        pgundo_storeThisLoc ( loco, UNDO_ADD, &ier );
        pgundo_endStep ();


        /*
         *  Redraw the new element. 
	 */
        pgutls_redraw ( loco, &el, &ier );


        /*
         *  Keep the handlebar on the barb.
         */
        pghdlb_select ( &el, loco );

    }
                   
}

/*=====================================================================*/

static void pgjet_setSpdLvl ( float speed, int level )
/************************************************************************
 * pgjet_setSpdLvl							*
 *									*
 * This function sets the jet barb speed & level.			*
 *									*
 * static void pgjet_setSpdLvl ( speed, level )				*
 *									*
 * Input parameters:							*
 *	speed		float		barb speed			*
 *	level		int		barb level			*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03	initial coding				*
 * J. Wu/SAIC		02/04	adjust input limitation			*
 ***********************************************************************/
{
    char	str[10];
/*---------------------------------------------------------------------*/

    if ( (float)MIN_SPEED <= speed && speed <= (float)MAX_SPEED ) {
        _barbSpeed = speed;
        
	XmTextFieldSetString ( _spd_txtW, "" );
	sprintf ( str, "%.1f", speed );
	XmTextFieldSetString ( _spd_txtW, str );
    }

    
    if ( MIN_LEVEL <= level && level <= MAX_LEVEL ) {
        _barbLevel = level;
        
	XmTextFieldSetString ( _lvl_txtW, "" );
	sprintf ( str, "%i", level );
	XmTextFieldSetString ( _lvl_txtW, str );
    }

}

/*=====================================================================*/

static void pgjet_setDelta ( int delta1, int delta2 )
/************************************************************************
 * pgjet_setDelta							*
 *									*
 * This function sets the jet barb deltas.				*
 *									*
 * static void pgjet_setDelta ( delta1, delta2 )			*
 *									*
 * Input parameters:							*
 *	delta1		int		barb delta 1			*
 *	delta2		int		barb delta 2			*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		02/04	set to blank if the deltas are 0	*
 * M. Li/SAIC		10/05	Checked for delta format		*
 * J. Lewis/AWC         03/07   Remove check for delta format           *
 * J. Lewis/AWC         03/07   If deltas are 0 don't write to string   *
 ***********************************************************************/
{
    char	str[10];
    int		temp0, temp1, cur_act;
/*---------------------------------------------------------------------*/

    cur_act = pgjet_getCurAction ();
    
	temp1 = _barbLevel;
	temp0 = MIN_DELTA;

    if ( cur_act == ADD_BARB ) {
        delta1 = 0;
        delta2 = 0;
    }

    if ( temp1 <= delta1 && delta1 <= MAX_DELTA ) {
        _barbDelta1 = delta1;
    } else {
         _barbDelta1 = 0;
        
    }

    if ( temp0 <= delta2 && delta2 <= temp1 ) {
        _barbDelta2 = delta2;
    } else {
         _barbDelta2 = 0;
    }

	XmTextFieldSetString ( _delta_txt1W, "" );
	if ( delta1 != 0 ) {
	    sprintf ( str, "%i", delta1 );
	    XmTextFieldSetString ( _delta_txt1W, str );
        }
        
	XmTextFieldSetString ( _delta_txt2W, "" );
	if ( delta2 != 0 ) {
	    sprintf ( str, "%i", delta2 );
	    XmTextFieldSetString ( _delta_txt2W, str );
        }

}
/*=====================================================================*/
/* ARGSUSED */
static void pgjet_barbInfoCb ( Widget wid, long which, XtPointer cbs ) 
/************************************************************************
 * pgjet_barbInfoCb							*
 *									*
 * Callback for the control buttons in the barb speed/level window.	*
 *									*
 * static void pgjet_barbInfoCb ( wid, which, cbs)			*
 *									*
 * Input parameters:							*
 *  	wid		Widget		widget ID			*
 *  	which		long		which button			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/03	initial coding				*
 * J. Wu/SAIC		02/04   add delta into barb FL text		*
 * J. Wu/SAIC		02/04	keep window up after applying new attr.	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		np, ier, loco, loc, sel;
    float	llx, lly, urx, ury, *xs, *ys;
    char	str[MAX_TEXT];
    VG_DBStruct	el; 
/*---------------------------------------------------------------------*/
    
    switch ( which ) {
        
	case 0:		/* Apply */        
            sel = pgjet_getSelectedSub ();

            if ( sel >= 0 ) {
                
		pgactv_getDevPts ( &np, &xs, &ys );

                loc = pgactv_getElmLoc ();
                pgutls_prepNew ( loc, &el, &llx, &lly, &urx, &ury, &ier );
                pgundo_newStep ();
                pgundo_storeThisLoc ( loc, UNDO_DEL, &ier );
        
                el.elem.jet.barb[sel].wnd.data.spddir[0] = _barbSpeed;

		pgjet_formatFL ( _barbLevel, _barbDelta1, _barbDelta2, str );

                strcpy ( el.elem.jet.barb[sel].spt.text, str );
		
		pgjet_setSpdLvl ( _barbSpeed, _barbLevel );
	
                pgvgf_saveNewElm ( NULL, sys_D, &el, np, xs, ys, TRUE,
			&loco, &ier);
                pgundo_storeThisLoc ( loco, UNDO_ADD, &ier );
                pgundo_endStep ();
                pgutls_redraw ( loco, &el, &ier );
                
		/*
                 *  Place handbar on the jet line.
                 */
                pghdlb_select ( &el, loco );

            }
	    	    
	  break;        
	
	case 1:		/* Cancel */
                XtUnmanageChild ( _pgjet_barbInfoW );
                pgjet_refresh ();
    
                pgjet_eventHandler ();
	  break;	
        
    }               
}
/*=====================================================================*/

static void pgjet_createAttr ( Widget parent )
/************************************************************************
 * pgjet_createAttr							*
 *									*
 * This function creates a jet attribute edit box.			*
 *									*
 * static void pgjet_createAttr ( parent )				*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC           10/03   initial coding				*
 * J. Wu/SAIC           05/04   add "clear" option on barb		*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    int		ier;
    long	ii, nn;
    char	cc[10], *btnstr[] = {"Apply", "Cancel"};
    char	*clea_type[] = {"On", "Off"}; 
    Widget	pane, clear_form, smth_lbl, smth_menubar;
    Widget	width_form;
    XmString	xmstr;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
    
    /*
     * Create the jet attribute edit window.
     */
    _pgjet_attrW = XmCreateFormDialog ( parent, "pgjet_attrpopup",
				       NULL, 0 );
    
    xmstr = XmStringCreateLocalized("Jet Attributes");
    XtVaSetValues ( _pgjet_attrW,
		 XmNnoResize,			TRUE,
		 XmNautoUnmanage,		False,
    		 XmNdialogTitle, 		xmstr,
		 NULL);
    XmStringFree ( xmstr );    


    pane = XtVaCreateManagedWidget ( "pane",
		xmPanedWindowWidgetClass,	_pgjet_attrW,
		XmNsashWidth,			1,
		XmNsashHeight,			1,
		NULL );

    /* 
     *  create a form for object clear and color botton input
     */
    clear_form = (Widget)XtVaCreateManagedWidget ("clear_form",
		xmFormWidgetClass,		pane,
		XmNtopAttachment, 		XmATTACH_FORM,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNrightAttachment,		XmATTACH_FORM,
		NULL);

    _clearLblW = XtVaCreateManagedWidget( "Clear:",
		xmLabelWidgetClass,		clear_form,
         	XmNtopAttachment,       	XmATTACH_FORM,
         	XmNtopOffset,           	5,
         	XmNleftAttachment,       	XmATTACH_FORM,
         	XmNleftOffset,           	3,		
		NULL );
 		
    _clearRcW   =  XtVaCreateWidget ( "_clearRowColW",
		xmRowColumnWidgetClass,		clear_form, 
                XmNorientation,			XmHORIZONTAL,
		XmNpacking,			XmPACK_TIGHT,
		XmNnumColumns,			2,
		XmNradioBehavior,       	False,
		XmNtraversalOn,         	False,
		XmNtopAttachment, 		XmATTACH_FORM,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			50,	
		NULL);
 
    nn = XtNumber ( clea_type );
    _clearBtnW = (WidgetList) XtMalloc ( nn * sizeof(Widget) );

    for ( ii = 0; ii < nn; ii++ ) {
	_clearBtnW[ii] = XtVaCreateManagedWidget ( clea_type[ii],
		xmToggleButtonGadgetClass,	_clearRcW,
		NULL);
	XtAddCallback ( _clearBtnW[ii], XmNvalueChangedCallback,
		      (XtCallbackProc)pgjet_clearCb, (XtPointer)ii );
    }
    
    /*
     *  Initialize the clearing status from the barb width in "setting.tbl" 
     */
    el.hdr.vg_type = JET_ELM;
    el.hdr.vg_class = CLASS_MET;
    ces_get ( -99, &el, &ier );
    if ( el.elem.jet.barb[0].wnd.info.width >= 800 ) {
        _barbClear = True;
        XmToggleButtonGadgetSetState ( _clearBtnW[0], TRUE, FALSE );
        XmToggleButtonGadgetSetState ( _clearBtnW[1], FALSE, FALSE );
    }
    else {
        _barbClear = False;    
        XmToggleButtonGadgetSetState ( _clearBtnW[0], FALSE, FALSE );
        XmToggleButtonGadgetSetState ( _clearBtnW[1], TRUE, FALSE );    
    }
        
    XtManageChild ( _clearRcW );

    
    /*
     * create color edit area 
     */
    _attr_clrW	= XtVaCreateManagedWidget ("",
		xmPushButtonWidgetClass,	clear_form,
		XmNwidth,			25,
		XmNheight,			20,
         	XmNtopAttachment,       	XmATTACH_FORM,
         	XmNtopOffset,           	5,
         	XmNrightAttachment,       	XmATTACH_FORM,
         	XmNrightOffset,           	10,
		NULL );

    XtAddCallback ( _attr_clrW, XmNactivateCallback, 
    			(XtCallbackProc)pgjet_colorCb, NULL );

    XtManageChild ( clear_form );
    

    /*
     * create width input area
     */
    width_form = XtVaCreateManagedWidget ( "attr_width_form",
		xmFormWidgetClass, 		pane,
		XmNtopAttachment,       	XmATTACH_WIDGET,
		XmNtopWidget,           	clear_form,
		NULL );

    _width_txtW = XtVaCreateManagedWidget ( "attr_width",
		xmTextFieldWidgetClass,		width_form,
		XmNcolumns,	                4,
		XmNvalue,			"1",
		XmNcursorPositionVisible,	True,
		XmNrightAttachment,		XmATTACH_FORM,
		NULL );

    XtAddCallback ( _width_txtW, XmNmodifyVerifyCallback, 
                    (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);    
    XtAddCallback ( _width_txtW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgjet_widTxtCb, NULL);

    _width_sldW = (Widget)XmCreateScale ( width_form, "width", NULL, 0 );
    XtManageChild ( _width_sldW );
    xmstr = XmStringCreateLocalized ( "Width" );
    XtVaSetValues ( _width_sldW,
		XmNorientation,		XmHORIZONTAL,
		XmNminimum,		MIN_WIDTH_SCALE,
		XmNmaximum,		MAX_WIDTH_SCALE,
		XmNprocessingDirection,	XmMAX_ON_RIGHT,
		XmNvalue,		_attrSplWdth,
		XmNscaleMultiple,	1,
		XmNshowValue,		False,
		XmNtitleString,		xmstr,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_WIDGET,
		XmNrightWidget,		_width_txtW,
		NULL );
    XmStringFree(xmstr);
    XtAddCallback ( _width_sldW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgjet_widSldCb, NULL);
    XtAddCallback ( _width_sldW, XmNdragCallback, 
		    (XtCallbackProc)pgjet_widSldCb, NULL);

    
    /*
     * create size input area
     */    
    _size_frmW = XtVaCreateManagedWidget ( "attr_size_form",
		xmFormWidgetClass,		pane,
		NULL);

    _size_txtW = XtVaCreateManagedWidget ( "attr_size",
		xmTextFieldWidgetClass,		_size_frmW,
		XmNcolumns,			4,
		XmNvalue,			"1.0",
		XmNcursorPositionVisible,	True,
		XmNrightAttachment,		XmATTACH_FORM,
		NULL );

    XtAddCallback ( _size_txtW, XmNmodifyVerifyCallback, 
                    (XtCallbackProc)pgutls_vrfyPosFltCb, NULL );    
    XtAddCallback ( _size_txtW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgjet_sizTxtCb, NULL );

    _size_sldW = (Widget)XmCreateScale ( _size_frmW, "size", NULL, 0 );
    XtManageChild ( _size_sldW );
    xmstr = XmStringCreateLocalized ( "Size" );
    XtVaSetValues( _size_sldW,
		XmNorientation,             	XmHORIZONTAL,
		XmNminimum,                 	(int) (MIN_SIZE_SCALE * 10.0F),
		XmNmaximum,                 	(int) (MAX_SIZE_SCALE * 10.0F),
		XmNprocessingDirection,     	XmMAX_ON_RIGHT,
		XmNvalue,                   	(int) (1 * 10.0F),
		XmNshowValue,               	False,
		XmNtitleString,             	xmstr,
		XmNtopAttachment,           	XmATTACH_FORM,
		XmNleftAttachment,          	XmATTACH_FORM,
		XmNrightAttachment,         	XmATTACH_WIDGET,
		XmNrightWidget,             	_size_txtW,
		NULL );
    XmStringFree(xmstr);

    XtAddCallback ( _size_sldW, XmNvalueChangedCallback, 
		    (XtCallbackProc)pgjet_sizSldCb, NULL);
    XtAddCallback ( _size_sldW, XmNdragCallback, 
		    (XtCallbackProc)pgjet_sizSldCb, NULL);
           
    /*
     * create smooth widget input
     */
    _smth_frmW = XtVaCreateManagedWidget ( "smth_form",
		xmFormWidgetClass,		pane,
		NULL );
    smth_lbl  = XtVaCreateManagedWidget ( "Smoothing Level",
		xmLabelGadgetClass,		_smth_frmW,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNtopOffset,			8,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			5,
		NULL ); 

    smth_menubar  = XmCreatePulldownMenu ( _smth_frmW, "Smooth", NULL, 0 );
    _smth_optW = XmCreateOptionMenu ( _smth_frmW, "smth", NULL, 0 );

    for ( ii = 0; ii < MAX_SMTH_LVLS; ii++ ) {
	sprintf ( cc, "%ld", ii );
        xmstr = XmStringCreateLocalized ( cc );
        _smth_pbW[ii] = XtVaCreateManagedWidget ( cc,
		xmPushButtonWidgetClass,	smth_menubar,
		XmNlabelString,			xmstr,
		NULL );
        XmStringFree ( xmstr );
        XtAddCallback ( _smth_pbW[ii], XmNactivateCallback,
		      (XtCallbackProc)pgjet_smthPbCb, (XtPointer) ii );
    }

    xmstr = XmStringCreateLocalized ( "\0" );
    XtVaSetValues ( _smth_optW, 
		XmNlabelString,		xmstr,	
		XmNsubMenuId,		smth_menubar,
		XmNmenuHistory,		_smth_pbW[0], 
		XmNrightAttachment,	XmATTACH_FORM,
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNleftWidget,		smth_lbl,
		XmNleftOffset,		5,
		NULL );
    XmStringFree ( xmstr );

    XtManageChild ( _smth_optW );


    /*
     * Create control buttons
     */
    _attr_ctlfrmW  = (Widget)XtVaCreateManagedWidget ("attr_ctlform",
		xmFormWidgetClass,	 	pane,
		XmNtopAttachment,         	XmATTACH_WIDGET,
		XmNtopWidget,             	_smth_frmW,
	 	NULL );

    _attr_ctlBtns = (WidgetList) XtMalloc ( XtNumber(btnstr) * sizeof(Widget) );
    NxmCtlBtn_create ( _attr_ctlfrmW, 1, "ctlBtns", XtNumber(btnstr), btnstr,
		      NULL, _attr_ctlBtns );
            		                        
    XtManageChild ( pane );
    
    return;
    
}

/*=====================================================================*/

void pgjet_popupAttr ( void )
/************************************************************************
 * pgjet_popupAttr							*
 *									*
 * This function shows jet attribute edit window.			*
 *									*
 * pgjet_popupAttr ( )    						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03   initial coding				*
 * J. Wu/SAIC		11/03   adjust calling sequence for proper	*
 *				initial popup on IBM AIX platform	*
 * J. Wu/SAIC		12/03   adjust for GUI change			*
 * J. Wu/SAIC		12/03   set proper initial attributes		*
 * J. Wu/SAIC           05/04   add "clear" control on barb		*
 ***********************************************************************/
{
    int		ier, loco, sel, cur_act;
    int		splclr, splwid, smooth, brbwid, brbclr, hshwid, hshclr;  
    float	brbsiz, hshsiz;  
    VG_DBStruct	el, cur_jet;
    Boolean	curJetExist = False;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    if ( XtIsManaged ( _pgjet_attrW ) ) {
	XtUnmanageChild ( _pgjet_attrW );
    }            
    
        
    /*
     *  No control buttons when ADD barb or hash. 
     */
    cur_act = pgjet_getCurAction ();
    if ( cur_act == ADD_BARB || cur_act == ADD_HASH ) {
        XtUnmanageChild ( _attr_ctlfrmW );
    }
    else {
        XtManageChild ( _attr_ctlfrmW );    
    }
    
    XtManageChild ( _pgjet_attrW );
    
    
    /*
     *  Read the attribute settings.
     */
    el.hdr.vg_type = JET_ELM;
    el.hdr.vg_class = CLASS_MET;

    ces_get ( -99, &el, &ier );
        
    
    /*
     *  Read the currently-selected jet line.
     */    
    loco = pgactv_getElmLoc ();
    if ( loco > 0 ) {
        cvg_rdrec ( cvg_getworkfile(), loco, &cur_jet, &ier ); 
        if ( ier == 0 && cur_jet.hdr.vg_type == JET_ELM ) {
	    curJetExist = True;	    
	}
    }
    
    
    /*
     *  Set the attributes. For barb/hash, use the first 
     *  barb/hash's attributes if no barb/hash is selected.		
     */           
    if ( curJetExist ) {
	splclr = cur_jet.elem.jet.line.splcol;
	splwid = cur_jet.elem.jet.line.spl.info.splwid;
	smooth = cur_jet.hdr.smooth;
    
	sel = pgjet_getSelectedSub ();
        
	/* 
	 *  Start with the first valid barb and hash's attributes.
	 */
	if ( cur_jet.elem.jet.nbarb > 0 ) {	    
	    brbclr = cur_jet.elem.jet.barb[0].wndcol;
	    brbwid = cur_jet.elem.jet.barb[0].wnd.info.width;
	    brbsiz = cur_jet.elem.jet.barb[0].wnd.info.size;
	}
	else {
	    brbclr = el.elem.jet.barb[0].wndcol;
	    brbwid = el.elem.jet.barb[0].wnd.info.width;
	    brbsiz = el.elem.jet.barb[0].wnd.info.size;	    	
	}
	
	if ( cur_jet.elem.jet.nhash > 0 ) {
	    hshclr = cur_jet.elem.jet.hash[0].wndcol;
	    hshwid = cur_jet.elem.jet.hash[0].wnd.info.width;
	    hshsiz = cur_jet.elem.jet.hash[0].wnd.info.size;	    
	}
	else {
	    hshclr = el.elem.jet.hash[0].wndcol;
	    hshwid = el.elem.jet.hash[0].wnd.info.width;
	    hshsiz = el.elem.jet.hash[0].wnd.info.size;	    	
	}	

	/* 
	 *  If still work on the same jet, use the last_saved attributes.
	 *  Note - last_saved attributes are those of the last-selected
	 *         barb and hash's. 
	 */
	if ( pgjet_isUp() ) {
	    brbclr = _attrBarbColr;
	    brbwid = _attrBarbWdth;
	    brbsiz = _attrBarbSize;	    	
	    
	    hshclr = _attrHashColr;
	    hshwid = _attrHashWdth;
	    hshsiz = _attrHashSize;	    		    
	}
	
	/* 
	 *  If a barb OR hash has been selected, use its attributes.
	 */
	if (  sel >= 0 ) {
	    if ( _jetSubtyp == SUB_BARB ) {
	        brbclr = cur_jet.elem.jet.barb[sel].wndcol;
	        brbwid = cur_jet.elem.jet.barb[sel].wnd.info.width;
	        brbsiz = cur_jet.elem.jet.barb[sel].wnd.info.size;
	    }	
	    else if ( _jetSubtyp == SUB_HASH ) {
	        hshclr = cur_jet.elem.jet.hash[sel].wndcol;
	        hshwid = cur_jet.elem.jet.hash[sel].wnd.info.width;
	        hshsiz = cur_jet.elem.jet.hash[sel].wnd.info.size;	    	
	    }
	}
       
	pgjet_setAttr ( splclr, splwid, smooth,
		        brbclr, brbwid, brbsiz,
		        hshclr, hshwid, hshsiz );
    }


    /*
     *  Allow modification to "smooth" for jet line only and "size"
     *  for barb/hash only.
     */
    if ( _jetSubtyp == SUB_BARB || _jetSubtyp == SUB_HASH ) {
        XtSetSensitive ( _smth_frmW, False );
        XtSetSensitive ( _size_frmW, True );
    }
    else {
        XtSetSensitive ( _smth_frmW, True );
        XtSetSensitive ( _size_frmW, False );    
    }
    
    /*
     *   Allow the control of clearing of the barb.
     */
    if ( _jetSubtyp == SUB_BARB ) {
        XtManageChild ( _clearRcW );                
        NxmLabel_setStr ( _clearLblW, "Clear: " );
    }
    else {
        XtUnmanageChild ( _clearRcW );                
        NxmLabel_setStr ( _clearLblW, "Color: " );
    }
    
    /*
     *  Change the button to indicate "Hide Attributes".
     */
    xmstr = XmStringCreateLtoR ( " Hide Attributes ",
                                 XmFONTLIST_DEFAULT_TAG );
    XtVaSetValues ( _jetAttrBtn, XmNlabelString, xmstr, NULL );
    XmStringFree ( xmstr );
    
}

/*=====================================================================*/

void pgjet_popdownAttr ( void )
/************************************************************************
 * pgjet_popdownAttr							*
 *									*
 * This function pops down the jet attribute edit window.		*
 *									*
 * void pgjet_popdownAttr ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03	initail coding	         		*
 * J. Wu/SAIC		12/03   adjust for GUI change			*
 * J. Wu/SAIC		12/03   pop down color pallette			*
 ***********************************************************************/
{
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    if ( XtIsManaged ( _pgjet_attrW ) ) {
        NxmClrW_popdown();    	
	XtUnmanageChild ( _pgjet_attrW );
    }
    
    xmstr = XmStringCreateLtoR ( " Show Attributes ",
                                 XmFONTLIST_DEFAULT_TAG );
    XtVaSetValues ( _jetAttrBtn, XmNlabelString, xmstr, NULL );
    XmStringFree ( xmstr );

}

/*=====================================================================*/

static Boolean pgjet_isUpAttr ( void )
/************************************************************************
 * pgjet_isUpAttr							*
 *									*
 * Queries whether the JET attribute edit window is managed or not.	*
 *									*
 * static Boolean pgjet_isUpAttr ( void )				*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	pgjet_isUpAttr()	Boolean	    True -- managed (up)	*
 *					    False -- down		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03	initial colding				*
 ***********************************************************************/
{
    return ( XtIsManaged ( _pgjet_attrW ) );
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_clearCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgjet_clearCb                                                      	*
 *                                                                      *
 * Callback for the clear button widget.                            	*
 *                                                                      *
 * static void pgjet_clearCb ( wid, which, cbs )    	             	*
 *									*
 * Input parameters:                                                    *
 *   wid	    Widget	    	Widget ID			*
 *   which    	    long	    		which button			*
 *   cbs      XtPointer		callback struct			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC        05/04  initial coding				*
 ***********************************************************************/
{  
/*---------------------------------------------------------------------*/
    
    if ( which == 0 ) {		/* On */
        _barbClear = True;
        _attrBarbWdth = _attrBarbWdth % 10 + 800;      
	XmToggleButtonGadgetSetState ( _clearBtnW[0], TRUE, FALSE );
        XmToggleButtonGadgetSetState ( _clearBtnW[1], FALSE, FALSE );
    }
    else {			/* Off */
        _barbClear = False;
        _attrBarbWdth = _attrBarbWdth % 10;      
	XmToggleButtonGadgetSetState ( _clearBtnW[0], FALSE, FALSE );
        XmToggleButtonGadgetSetState ( _clearBtnW[1], TRUE, FALSE );    
    }
    
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_colorCb ( Widget wid, XtPointer clnt, XtPointer cbs ) 
/************************************************************************
 * pgjet_colorCb							*
 *									*
 * Callback for attribute color button widget.				*
 *									*
 * static void pgjet_colorCb ( wid, clnt, cbs )		*
 *									*
 * Input parameters:							*
 *   wid		Widget			Widget ID		*
 *   clnt	XtPointer       	color			*
 *   cbs	XtPointer	callback struct		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03	initial coding				*
 ***********************************************************************/
{
    int		*color;
/*---------------------------------------------------------------------*/
    
    if ( _jetSubtyp == SUB_BARB ) {
        color = &_attrBarbColr;
    }
    else if ( _jetSubtyp == SUB_HASH ) {
        color = &_attrHashColr;
    }
    else {
        color = &_attrSplColr;
    }
    
    XtVaSetValues ( _attr_clrW,
		XmNbackground,		NxmColrP_getColorPixel ( *color ),
		XmNtopShadowColor,	NxmColrP_getColorPixel ( *color ),
		XmNbottomShadowColor,	NxmColrP_getColorPixel ( *color ),
		NULL);

    clnt = (XtPointer) color;
    NxmClrW_popup ( wid, clnt, (XtPointer)cbs );

}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_widSldCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgjet_widSldCb                                                       *
 *                                                                      *
 * Callback for attribute width scale widget.				*
 *                                                                      *
 * static void pgjet_widSldCb ( w, clnt, cbs )	                *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	   Widget ID                    *
 *   clnt    XtPointer       	   not used                     *
 *   *cbs	    XmScaleCallbackStruct  callback struct              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/03	initial coding				*
 * J. Wu/SAIC           05/04   add "clear" control on barb		*
 ***********************************************************************/
{
    int		wid;		
    char	txtstr[5];
/*---------------------------------------------------------------------*/

    wid = cbs->value;
    XmScaleSetValue ( w, wid );
    sprintf ( txtstr, "%i", wid );
    XmTextFieldSetString ( _width_txtW, txtstr );

    if ( cbs->reason == XmCR_VALUE_CHANGED ) {
        if ( _jetSubtyp == SUB_BARB ) {
	    _attrBarbWdth  = wid;
            if ( _barbClear )  _attrBarbWdth += 800;
        }
        else if ( _jetSubtyp == SUB_HASH ) {
            _attrHashWdth = wid;
        }
        else {
	    _attrSplWdth = wid;
        }
    }     
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_widTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgjet_widTxtCb							*
 *                                                                      *
 * Callback for attribute width text widget.				*
 *                                                                      *
 * static void pgjet_widTxtCb ( w, clnt, cbs )	                *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/03	initial coding				*
 * J. Wu/SAIC           05/04   add "clear" control on barb		*
 ***********************************************************************/
{
    char	*ss;
    int		slval, wid;
/*---------------------------------------------------------------------*/

    /*
     * Confirm there is an event.  If not, this text has already
     * been set up.
     */
    if ( !cbs->event ) 
	return;

    /*
     * if the value on corresponding slider is different, set the sliders
     * value accordingly.
     */
    XmScaleGetValue ( _width_sldW, &slval );
    ss = XmTextFieldGetString( _width_txtW );
    wid = atoi ( ss );
    XtFree ( ss );

    if ( MIN_WIDTH_SCALE <= wid && wid <= MAX_WIDTH_SCALE ) {
        if ( wid != slval ) {
            XmScaleSetValue ( _width_sldW, wid );

            if ( _jetSubtyp == SUB_BARB ) {
                _attrBarbWdth  = wid;
                if ( _barbClear )  _attrBarbWdth += 800;            
	    }
            else if ( _jetSubtyp == SUB_HASH ) {
                _attrHashWdth = wid;
            }
            else {
	        _attrSplWdth = wid;
            }
        }
    }    
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_sizSldCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgjet_sizSldCb                                                      	*
 *                                                                      *
 * Callback for attribute size scale widget.				*
 *                                                                      *
 * static void pgjet_sizSldCb( w, clnt, cbs )            	*
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget           Widget ID                          *
 *   clnt    XtPointer        not used                           *
 *   *cbs     XmScaleCallbackStruct  callback struct 		        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/03	initial coding				*
 ***********************************************************************/
{
    float	siz;	
    char	txtstr[5];
/*---------------------------------------------------------------------*/

    siz = (float)cbs->value / 10.0F;
    sprintf ( txtstr, "%.1f", siz );
    XmTextFieldSetString ( _size_txtW, "" );
    XmTextFieldSetString ( _size_txtW, txtstr );

    if ( cbs->reason == XmCR_VALUE_CHANGED ) {
        if ( _jetSubtyp == SUB_BARB ) {
            _attrBarbSize  = siz;
        }
        else if ( _jetSubtyp == SUB_HASH ) {
            _attrHashSize = siz;
        }
    }    
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_sizTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgjet_sizTxtCb							*
 *                                                                      *
 * Callback for attribute size text widget.				*
 *                                                                      *
 * static void pgjet_sizTxtCb( w, clnt, cbs )	                *
 *                                                                      *
 * Input parameters:                                                    *
 *   w              Widget          	  Widget ID                     *
 *   clnt    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/03	initial coding				*
 ***********************************************************************/
{
    char        *ss;
    int         slval;
    float	siz;
/*---------------------------------------------------------------------*/

    /*
     * Confirm there is an event.  If not, this text has already
     * been set up.
     */
    if ( !cbs->event ) 
	return;

    /* if the value on corresponding slider is different, set the sliders
     * value accordingly.
     */
    XmScaleGetValue ( _size_sldW, &slval );
    ss = XmTextFieldGetString ( _size_txtW );
    siz = (float) ( atof ( ss ) );
    XtFree ( ss );

    if ( MIN_SIZE_SCALE <= siz && siz <= MAX_SIZE_SCALE ) {
        if ( (int) (siz * 10.0F) != slval) {
            XmScaleSetValue ( _size_sldW, (int) (siz * 10.0F) );

            if ( _jetSubtyp == SUB_BARB ) {
                _attrBarbSize  = siz;
            }
            else if ( _jetSubtyp == SUB_HASH ) {
                _attrHashSize = siz;
            }
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_smthPbCb ( Widget ww, long clnt, XtPointer cbs )
/************************************************************************
 * pgjet_smthPbCb							*
 *									*
 * Callback of attribute smooth level widget.				*
 *									*
 * static void pgjet_smthPbCb ( ww, clnt, cbs )		*
 *									*
 * Input parameters:							*
 *	ww	Widget			widget ID		*
 *	clnt	long			client data		*
 *	cbs	XtPointer	callback struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _attrSmth = (int)clnt;

}

/*=====================================================================*/

static void pgjet_switchClr ( void )
/************************************************************************
 * pgjet_switchClr							*
 *									*
 * Sets proper background/foreground color for subtype/action buttons.	*
 *									*
 * static void pgjet_switchClr ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03	initial coding				*
 * J. Wu/SAIC		12/03	adjust foreground/background colors	*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/
    
    /*
     *  initiate all subtype buttons with blue background
     */
    for ( ii = 0; ii < MAX_SUBTYPS; ii++ ) {
	XtVaSetValues ( _subtypBtns[ii],
		XmNbackground,			_bluePix,
		XmNforeground,			_whitePix,
		NULL );
    }        
    
    /*
     *  set the selected subtype buttons with green background
     */
    if ( _jetSubtyp >= 0 ) {
        XtVaSetValues ( _subtypBtns[_jetSubtyp],
		XmNforeground,			_blackPix,
		XmNbackground,			_greenPix,
		NULL );
    }
    
    /*
     *  set the action buttons under the selected subtype button
     *  with GREEN background. For other action buttons, set 
     *  BLUE background. Set the selected active action button 
     *  with YELLOW background. 
     */
    if ( _jetSubtyp == SUB_BARB ) {  /* Barb */
        for ( ii = 0; ii < MAX_ACTIONS; ii++ ) {        
            XtVaSetValues ( _barbActBtns[ii],
		XmNbackground,			_greenPix,
		XmNforeground,			_blackPix,
		NULL );

            XtVaSetValues ( _hashActBtns[ii],
		XmNbackground,			_bluePix,
		XmNforeground,			_whitePix,
		NULL );
        
	}
        
	XtVaSetValues ( _barbActBtns[_jetBarbAct],
		XmNforeground,			_blackPix,
		XmNbackground,			_yellowPix,
		NULL );
    }
    else if ( _jetSubtyp == SUB_HASH ) {  /* Hash */
        for ( ii = 0; ii < MAX_ACTIONS; ii++ ) {        
            XtVaSetValues ( _barbActBtns[ii],
		XmNbackground,			_bluePix,
		XmNforeground,			_whitePix,
		NULL );
        
            XtVaSetValues ( _hashActBtns[ii],
		XmNbackground,			_greenPix,
		XmNforeground,			_blackPix,
		NULL );

	}
        
	XtVaSetValues ( _hashActBtns[_jetHashAct],
		XmNforeground,			_blackPix,
		XmNbackground,			_yellowPix,
		NULL );    
    }
    else {  /* none */
        for ( ii = 0; ii < MAX_ACTIONS; ii++ ) {        
            XtVaSetValues ( _barbActBtns[ii],
		XmNbackground,			_bluePix,
		XmNforeground,			_whitePix,
		NULL );
        
            XtVaSetValues ( _hashActBtns[ii],
		XmNbackground,			_bluePix,
		XmNforeground,			_whitePix,
		NULL );
	}    
    }

}

/*=====================================================================*/

void pgjet_getAttr ( int *splcol, int *splwdth, char *smth,
		     int *brbcol, int *brbwdth, float *brbsiz,
		     int *hshcol, int *hshwdth, float *hshsiz )
/************************************************************************
 * pgjet_getAttr							*
 *									*
 * This function gets the attributes in the jet edit window.		*
 *									*
 * void pgjet_getAttr ( splcol, splwdth, smth, brbcol, brbwdth, brbsiz,	*
 *		        hshcol, hshwdth, hshsiz )			*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	*splcol		int		jet line color			*
 *	*splwdth	int		jet line width			*
 *	*smth		char		jet line smoothing level	*
 *	*brbcol		int		barb color			*
 *	*brbwdth	int		barb width			*
 *	*brbsiz		float		barb size			*
 *	*hshcol		int		hash color			*
 *	*hshwdth	int		hash width			*
 *	*hshsiz		float		hash size			*
 *									*
 **									*
 * J. Wu/SAIC		10/03	initial coding				*
 ***********************************************************************/
{
    *splcol	= _attrSplColr;
    *splwdth	= _attrSplWdth;
    *smth	= (char)_attrSmth;
    *brbcol	= _attrBarbColr;
    *brbwdth	= _attrBarbWdth;
    *brbsiz	= _attrBarbSize;
    *hshcol	= _attrHashColr;
    *hshwdth	= _attrHashWdth;
    *hshsiz	= _attrHashSize;
}

/*=====================================================================*/

static void pgjet_setAttr ( int splcol, int splwdth, char smth,
		     int brbcol, int brbwdth, float brbsiz,
		     int hshcol, int hshwdth, float hshsiz )
/************************************************************************
 * pgjet_setAttr							*
 *									*
 * This function sets the jet attributes.				*
 *									*
 * static void pgjet_setAttr ( splcol, splwdth, smth,			*
 *                             brbcol, brbwdth, brbsiz,			*
 *		               hshcol, hshwdth, hshsiz )		*
 *									*
 * Input parameters:							*
 *	splcol		int		jet line color			*
 *	splwdth		int		jet line width			*
 *	smth		char		jet line smoothing level	*
 *	brbcol		int		barb color			*
 *	brbwdth		int		barb width			*
 *	brbsiz		float		barb size			*
 *	hshcol		int		hash color			*
 *	hshwdth		int		hash width			*
 *	hshsiz		float		hash size			*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/03	initial coding				*
 * J. Wu/SAIC		11/03	clean up width text field		*
 * J. Wu/SAIC           05/04   add "clear" control on barb		*
 ***********************************************************************/
{
    int			color, width, tmpwdth;
    float		size;
    char		str[5];
/*---------------------------------------------------------------------*/

    if ( _jetSubtyp == SUB_BARB ) {
        color = brbcol;
	width = brbwdth % 10;	/* adjusting for clearing control */
	size  = brbsiz;
    }
    else if ( _jetSubtyp == SUB_HASH ) {
        color = hshcol;
	width = hshwdth;
	size  = hshsiz;
    }
    else {
        color = splcol;
	width = splwdth;
        size  = 1.0;
    }

    /*
     * color
     */
    _attrSplColr = splcol;
    _attrBarbColr = brbcol;
    _attrHashColr = hshcol;

    XtVaSetValues ( _attr_clrW,
		XmNbackground,		NxmColrP_getColorPixel ( color ),
		XmNtopShadowColor,	NxmColrP_getColorPixel ( color ),
		XmNbottomShadowColor,	NxmColrP_getColorPixel ( color ),
		NULL );

    /* 
     * width
     */    
    if ( MIN_WIDTH_SCALE <= splwdth && splwdth <= MAX_WIDTH_SCALE ) {
        _attrSplWdth = splwdth;    
    }
    
    tmpwdth = brbwdth % 10;
    if ( MIN_WIDTH_SCALE <= tmpwdth && tmpwdth <= MAX_WIDTH_SCALE ) {
        _attrBarbWdth = brbwdth;    
    }
    
    if ( MIN_WIDTH_SCALE <= hshwdth && hshwdth <= MAX_WIDTH_SCALE ) {
        _attrHashWdth = hshwdth;    
    }

    if ( MIN_WIDTH_SCALE <= width && width <= MAX_WIDTH_SCALE ) {
	sprintf ( str, "%i", width );
        XmTextFieldSetString ( _width_txtW, "" );
        XmTextFieldSetString ( _width_txtW, str );
        XmScaleSetValue ( _width_sldW, width );
    } 


    /*
     * size
     */
    if ( MIN_SIZE_SCALE <= brbsiz && brbsiz <= MAX_SIZE_SCALE ) {
        _attrBarbSize = brbsiz;    
    }

    if ( MIN_SIZE_SCALE <= hshsiz && hshsiz <= MAX_SIZE_SCALE ) {
        _attrHashSize = hshsiz;    
    }
    

    if ( MIN_SIZE_SCALE <= size && size <= MAX_SIZE_SCALE ) {
	sprintf ( str, "%.1f", size );
        XmTextFieldSetString ( _size_txtW, "" );
        XmTextFieldSetString ( _size_txtW, str );
        XmScaleSetValue ( _size_sldW, (int) (size * 10.0F) );
    }


    /*
     * smooth
     */
    _attrSmth = (int) smth;   

    XtVaSetValues ( _smth_optW, 
		   XmNmenuHistory, _smth_pbW [ _attrSmth ],
		   NULL );

}
 
/*=====================================================================*/

int pgjet_getCurAction ( void )
/************************************************************************
 * pgjet_getCurAction							*
 *									*
 * Gets the current action choice on the jet line.			*
 *									*
 * int pgjet_getCurAction ( void )					*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	pgjet_getCurAction()	int	    1 - MOVE barb		*
 *					    2 - ADD barb		*
 *					    3 - DELETE barb		*
 *					    4 - MOVE hash		*
 *					    5 - ADD hash		*
 *					    6 - DELETE hash		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/03	initial colding				*
 ***********************************************************************/
{  
    int		cur_action;
/*---------------------------------------------------------------------*/

    cur_action = 0;
    
    if ( _jetSubtyp == SUB_BARB && _jetBarbAct == ACT_MOVE ) {    
        cur_action = MOVE_BARB;
    }
    else if ( _jetSubtyp == SUB_BARB && _jetBarbAct == ACT_ADD ) {
        cur_action = ADD_BARB;
    }
    else if ( _jetSubtyp == SUB_BARB && _jetBarbAct == ACT_DELETE ) {
        cur_action = DELETE_BARB;
    }
    else if ( _jetSubtyp == SUB_HASH && _jetHashAct == ACT_MOVE ) {    
        cur_action = MOVE_HASH;
    }
    else if ( _jetSubtyp == SUB_HASH && _jetHashAct == ACT_ADD ) {
        cur_action = ADD_HASH;
    }
    else if ( _jetSubtyp == SUB_HASH && _jetHashAct == ACT_DELETE ) {
        cur_action = DELETE_HASH;
    }

    return ( cur_action );

}

/*=====================================================================*/

int pgjet_getSelectedSub ( void )
/************************************************************************
 * pgjet_getSelectedSub							*
 *									*
 * Gets the index of the current selected barb or hash. 		*
 *									*
 * int pgjet_getSelectedSub ( void )					*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	pgjet_getSelectedSub()	int	  selected barb/hash's index	*
 *					    				*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/03	initial colding				*
 ***********************************************************************/
{  
    return ( _selectedSub );
}

/*=====================================================================*/

static int pgjet_getNearestSub ( VG_DBStruct el, float xx, float yy, int act )
/************************************************************************
 * pgjet_getNearestSub							*
 *									*
 * Gets the index of the nearest barb/hash to the click point.		*
 *									*
 * static int pgjet_getNearestSub ( el, xx, yy, act )			*
 *									*
 * Input parameters:							*
 *	el		VG_DBStruct	current jet			*
 *	xx		float		X coordinate of fixed point	*
 *	yy		float		Y coordinate of fixed point	*
 *	act		int		current jet action		*
 *									*
 * Output parameters:							*
 *	pgjet_getNearestSub()	int	nearest barb/hash's index	*
 *					    				*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/03	initial colding				*
 ***********************************************************************/
{  
    int		ii, ier, nn, closest_idx, second, hot_pt;
    float	dx[MAXPTS], dy[MAXPTS], nx, ny;
    float	distance, closest_distance;
/*---------------------------------------------------------------------*/
    
    nn = 1;
    closest_idx = -1;    
    closest_distance = CLOSEST_DIST;
    
    if ( act == MOVE_BARB || act == DELETE_BARB ) {
	for ( ii = 0; ii < el.elem.jet.nbarb; ii++ ) {
	    gtrans ( sys_M, sys_D, &nn,
		     &el.elem.jet.barb[ii].wnd.data.latlon[0],
		     &el.elem.jet.barb[ii].wnd.data.latlon[1],
		     dx, dy, &ier, strlen(sys_M), strlen(sys_D) );
	        
	    if ( ier == 0 ) {
	        cgr_segdist( &nn, dx, dy, &xx, &yy, &distance, &hot_pt, 
			     &second, &nx, &ny, &ier );
		    if ( distance <= closest_distance ) {
		        closest_idx = ii;
		        closest_distance = distance;
		    }
	     }
        }
    }
    else if ( act == MOVE_HASH || act == DELETE_HASH ) {
        for ( ii = 0; ii < el.elem.jet.nhash; ii++ ) {
	    gtrans ( sys_M, sys_D, &nn,
		     &el.elem.jet.hash[ii].wnd.data.latlon[0],
		     &el.elem.jet.hash[ii].wnd.data.latlon[1],
		     dx, dy, &ier, strlen(sys_M), strlen(sys_D) );
	        
	    if ( ier == 0 ) {
	        cgr_segdist( &nn, dx, dy, &xx, &yy, &distance, &hot_pt, 
			     &second, &nx, &ny, &ier );
		    if ( distance <= closest_distance ) {
		        closest_idx = ii;
		        closest_distance = distance;
		    }
	     }
        }
    }

    return ( closest_idx );
        
}

/*=====================================================================*/

static void pgjet_eventHandler ( void )
/************************************************************************
 * pgjet_pgjet_eventHandler						*
 *									*
 * Handles the drawing/editing events after a jet is selected.		*
 *									*
 * static void pgjet_eventHandler ( void )				*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	none								*
 *					    				*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/03	initial colding				*
 ***********************************************************************/
{  
    int		cur_action; 
/*---------------------------------------------------------------------*/
    
    cur_action = pgjet_getCurAction();
    
    if ( cur_action >= MOVE_BARB &&  cur_action <= DELETE_HASH ) {
        
	mcanvw_disarmDynamic ();
	
	switch ( cur_action ) {
	
	    case MOVE_BARB:
	    case DELETE_BARB:
	    case MOVE_HASH:
	    case DELETE_HASH:
	        mcanvw_setPressFunc ( (XtEventHandler)&pgjet_mvdelHandler, CURS_DEFAULT );
	        mbotw_mouseSet ( LMHINT_SELECT, MMHINT_EXIT );     
	    break;

	    case ADD_BARB:
	    case ADD_HASH:
	        mcanvw_setPressFunc ( (XtEventHandler)&pgjet_addHandler, CURS_POINT_SELECT );
	        mbotw_mouseSet ( ACHINT_ADD, MMHINT_DONE );     
	    break;	    
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_addHandler ( Widget wid, XtPointer clnt, XEvent *event,
							Boolean *ctdr )
/************************************************************************
 * pgjet_addHandler							*
 *									*
 * Handler function to add new barbs and hashs onto the selected jet.	*
 *									*
 * static void pgjet_addHandler ( wid, clnt, event )			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	not used				*
 *	*event	XEvent		button event				*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	11/03   	initial coding				*
 * J. Wu/SAIC	12/03   	use same color for jet's barb & text	*
 * J. Wu/SAIC	02/04   	add delta into barb FL text		*
 * J. Wu/SAIC	02/04   	do not save deltas if both are 0	*
 * J. Wu/SAIC	03/04   	set default offset for FL text		*
 * J. Wu/SAIC	09/04   	allow to set text type in settings.tbl	*
 * M. Li/SAIC	10/05		Checked for delta format		*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * J. Lewis/AWC 03/07           Remove check for delta format           *
 * J. Lewis/AWC 03/07           Set yoffset=3 if deltas not equal to 0  *
 * J. Lewis/AWC 03/07           If deltas are zero don't write to       *
 *                                 text string
 ***********************************************************************/
{

    int		nloc = 1, np, ier, loco, xoff, yoff, el_location;
    int		cur_act, *num, lwfactor, yoffset;
    float	xx, yy, lat, lon, llx, lly, urx, ury, *xpts, *ypts;
    char	str[MAX_TEXT];
    VG_DBStruct	el, ces_el;
    BarbAttr	*new; 
/*---------------------------------------------------------------------*/
    
    if ( event->xbutton.button == Button1 ) {

	cur_act = pgjet_getCurAction ();
		
	/* 
	 *  Check if there is space for adding more barb/hashs.
	 */
	el_location = pgactv_getElmLoc ();
	cvg_rdrec ( cvg_getworkfile(), el_location, &el, &ier );

	if ( cur_act == ADD_BARB ) {
	    num = &el.elem.jet.nbarb;
	}
	else {
	    num = &el.elem.jet.nhash;
        }
	    	
	if ( *num < MAX_JETPTS ) {
	 
	    /* 
	     *  Retrieve default settings for JET_ELM.
	     */
            ces_el.hdr.vg_type = JET_ELM;
            ces_el.hdr.vg_class = CLASS_MET;

            ces_get ( -99, &ces_el, &ier );	    
	    
	    
	    /* 
	     *  Add new barb/hashs to the end of the barb/hash list.
	     */
	    if ( cur_act == ADD_BARB ) {
	        new = (BarbAttr*) &el.elem.jet.barb[*num];
	    }
	    else {
	        new = (BarbAttr*) &el.elem.jet.hash[*num];
            }
	    
	    xgtoff ( &xoff, &yoff, &ier );
	    xx = (float) (event->xbutton.x + xoff);
	    yy = (float) (event->xbutton.y + yoff);
	
	    gtrans ( sys_D, sys_M, &nloc, &xx, &yy, &lat, &lon, 
		    &ier, strlen(sys_D), strlen(sys_M) );
	    
	    pgactv_getDevPts ( &np, &xpts, &ypts );
            
	    pgutls_prepNew ( el_location, &el, &llx, &lly, &urx, &ury, &ier );
	    
	    pgundo_newStep ();
	    pgundo_storeThisLoc ( el_location, UNDO_DEL, &ier );

	    new->wnd.info.numwnd = 1;
	    new->wnd.info.hdsiz = 0.0F;
	    new->wnd.data.latlon[0] = lat;
	    new->wnd.data.latlon[1] = lon;
	    new->wnd.data.spddir[1] = 360.0F; /* doesn't matter, will snap */
            	    
	    if ( cur_act == ADD_BARB ) {
		new->wndcol = _attrBarbColr;
		new->wnd.info.width = _attrBarbWdth;
		new->wnd.info.size = _attrBarbSize;
	        new->wnd.info.wndtyp = 114;		/* Barb */
	        new->wnd.data.spddir[0] = _barbSpeed;
		
	        /*
		 *  Note: Flight level text always use its associated barb's color.
		 */
		new->spt.info.lat = lat;
	        new->spt.info.lon = lon;                
	        
		new->sptcol = _attrBarbColr;		
		new->spt.info.sztext = ces_el.elem.jet.barb[*num].spt.info.sztext;
		new->spt.info.itxfn = ces_el.elem.jet.barb[*num].spt.info.itxfn;
		new->spt.info.ithw = ces_el.elem.jet.barb[*num].spt.info.ithw;
		new->spt.info.iwidth = ces_el.elem.jet.barb[*num].spt.info.iwidth;
		new->spt.info.ialign= ces_el.elem.jet.barb[*num].spt.info.ialign;
		new->spt.info.rotn = ces_el.elem.jet.barb[*num].spt.info.rotn;
		new->spt.info.turbsym = ces_el.elem.jet.barb[*num].spt.info.turbsym;
		new->spt.info.txtcol = ces_el.elem.jet.barb[*num].spt.info.txtcol;
		new->spt.info.filcol = ces_el.elem.jet.barb[*num].spt.info.filcol;
		new->spt.info.lincol= ces_el.elem.jet.barb[*num].spt.info.lincol;
		new->spt.info.sptxtyp = ces_el.elem.jet.barb[*num].spt.info.sptxtyp;
			
		pgjet_formatFL ( _barbLevel, _barbDelta1, _barbDelta2, str );


		/*
		 *  Set default text offset. It is slightly adjusted from wind
		 *  barb point.
		 */
		new->spt.info.offset_x = 0;
		
		lwfactor = (int)(el.elem.jet.line.spl.info.splwid / 14.0);
		
		if ( ( _barbDelta1 != 0) && ( _barbDelta2 != 0) ) {
		    yoffset = 3;
		}
		else {
		    yoffset = 2;
		}               
	        
		if ( lat >= 0.0 ) {	/* Northern Hemisphere */	
		    new->spt.info.offset_y = -yoffset - lwfactor;
                }
		else {			/* Southern Hemisphere */
		    new->spt.info.offset_y = yoffset + lwfactor;		
		}

		strcpy ( new->spt.text, str );		
		
		pgjet_setSpdLvl ( _barbSpeed, _barbLevel );
		pgjet_setDelta ( _barbDelta1, _barbDelta2 );
	    
	    }
	    else {	        
		new->wndcol = _attrHashColr;
	        new->wnd.info.width = _attrHashWdth;
	        new->wnd.info.size = _attrHashSize;
	        new->wnd.data.spddir[0] = 0.0F;
	        new->wnd.info.wndtyp = 1;		/* Hash */
	    }
	    
	    (*num)++;
	    	    
	    pgvgf_saveNewElm ( NULL, sys_D, &el, np, xpts, ypts, TRUE,
	    		&loco, &ier );
	    pgundo_storeThisLoc ( loco, UNDO_ADD, &ier );
            pgundo_endStep ();
	
	    pgutls_redraw ( loco, &el, &ier );

	}
    }
    else {

	pgjet_popdownBarbInfo ();
        
	_jetSubtyp = -1;
	pgjet_switchClr ();

	/*
	 *  Update the attribute to the jet lines, not the barb/hash's.
	 */ 	    
        if ( pgjet_isUpAttr() ) {
	    pgjet_popupAttr ();
	}
	
	mcanvw_disarmDynamic ();
	mcanvw_setPressFunc ( (XtEventHandler)&pgevt_selectHdl, CURS_DEFAULT ); 
	mbotw_mouseSet ( LMHINT_MOVEPOINT, MMHINT_DONE );	
    }
        
}

/*=====================================================================*/
/* ARGSUSED */
static void pgjet_mvdelHandler ( Widget wid, XtPointer clnt, XEvent *event,
						Boolean *ctdr )
/************************************************************************
 * pgjet_mvdelHandler							*
 *									*
 * Handler function to move/delete a barb/hash on the selected jet.	*
 * 									*
 * static void pgjet_mvdelHandler ( wid, clnt, event )		*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	not used				*
 *	*event	XEvent		button event				*
 *									*
 * Output parameters:							*
 *	none								*
 **									*
 * Log:									*
 * J. Wu/SAIC	11/03   	initial coding				*
 * J. Wu/SAIC	02/04   	decode delta from barb FL text		*
 * J. Wu/SAIC	02/04   	default deltas to 0 before decoding	*
 * M. Li/SAIC	10/05		Checked for new format of delta		*
 * J. Lewis/AWC 03/07           Remove check for delta format           *
 * J. Lewis/AWC 03/07           Set deltas to 0 if input is invalid     *
 ***********************************************************************/
{
    int		nloc = 1, ier, xoff, yoff, el_location;
    int		cur_act, num, lvl, dta1, dta2;
    float	xx, yy, lat, lon, spd;
    VG_DBStruct	el; 
/*---------------------------------------------------------------------*/

    if ( event->xbutton.button == Button1 ) {
        
	cur_act = pgjet_getCurAction ();

	xgtoff ( &xoff, &yoff, &ier );
	xx = (float) (event->xbutton.x + xoff);
	yy = (float) (event->xbutton.y + yoff);
	
	gtrans ( sys_D, sys_M, &nloc, &xx, &yy, &lat, &lon, 
		&ier, strlen(sys_D), strlen(sys_M) );
	
            	          
	/*
	 *  Locate the nearest barb or hash on the jet.
	 */ 	    
	el_location = pgactv_getElmLoc ();                        
	cvg_rdrec ( cvg_getworkfile(), el_location, &el, &ier );
        _selectedSub = pgjet_getNearestSub ( el, xx, yy, cur_act );
                                    
	if ( _selectedSub >= 0 ) {
	    
	    if ( cur_act == MOVE_BARB ) {	        
	        _attrBarbColr = el.elem.jet.barb[_selectedSub].wndcol;
	        _attrBarbWdth = el.elem.jet.barb[_selectedSub].wnd.info.width;
	        _attrBarbSize = el.elem.jet.barb[_selectedSub].wnd.info.size;

		spd = el.elem.jet.barb[_selectedSub].wnd.data.spddir[0];
		lvl = _barbLevel; 
	      
		    dta1 = 0; 
		    dta2 = 0; 

		if ( strlen ( el.elem.jet.barb[_selectedSub].spt.text ) > (size_t)2 ) {
			sscanf( el.elem.jet.barb[_selectedSub].spt.text,
                            "FL%d\n%d/%d", &lvl, &dta2, &dta1 );
		    }

		pgjet_setSpdLvl ( spd, lvl );

	     	if ( strlen ( el.elem.jet.barb[_selectedSub].spt.text ) > (size_t)5 ) {
                    if ( dta1 < 0 ) {
                        dta1 = 0;
                        dta2 = 0;
                    }

		    _barbDelta1 = dta1;
		    _barbDelta2 = dta2;
	      	}
	      	else {
                    _barbDelta1 = 0;
		    _barbDelta2 = _barbDelta1;
	        } 
		
		pgjet_setDelta ( _barbDelta1, _barbDelta2 );
		
		pgjet_popupBarbInfo ();
	        
		mbotw_mouseSet ( LMHINT_MOVE, MMHINT_DONE );
	    }
	    else { 
	        
		pgjet_popdownBarbInfo ();	
		
		if ( cur_act == MOVE_HASH ) {
	            _attrHashColr = el.elem.jet.hash[_selectedSub].wndcol;
	            _attrHashWdth = el.elem.jet.hash[_selectedSub].wnd.info.width;
	            _attrHashSize = el.elem.jet.hash[_selectedSub].wnd.info.size;
	        }
	        
	        if ( cur_act == MOVE_HASH ) {	        
		    mbotw_mouseSet ( LMHINT_MOVE, MMHINT_DONE );
	        }
		else { 
		    mbotw_mouseSet ( LMHINT_DELETE, MMHINT_DONE );		
		}	        
	    }	

	    
	    /*
	     *  Remove the handbar on the jet line and place it on
	     *  the selected barb or hash.
	     *  Note: the active element is still the jet element. 
	     */ 	    
            crg_getinx ( el_location, &num, &ier);
	    pghdlb_deselectEl ( num, TRUE );
	    pghdlb_select ( &el, el_location );
            	    
	    
	    /*
	     *  Update the attribute to the selected barb/hash's.
	     */ 	    
            if ( pgjet_isUpAttr() ) {
	        pgjet_popupAttr ();
	    }


	    /*
	     *  Now dispatch the next mouse event.
	     */ 	    
	    mcanvw_disarmDynamic ();
	    mcanvw_setPressFunc ( (XtEventHandler)&pgjet_mvdelSub, CURS_POINT_SELECT );
	}		
    }    
    else {	

	_jetSubtyp = -1;
	pgjet_switchClr ();

	/*
	 *  Update the attribute to the jet lines, not the barb/hash's.
	 */ 	    
        if ( pgjet_isUpAttr() ) {
	    pgjet_popupAttr ();
	}
	
	mcanvw_disarmDynamic ();
	mcanvw_setPressFunc ( (XtEventHandler)&pgevt_selectHdl, CURS_DEFAULT ); 
	mbotw_mouseSet ( LMHINT_MOVEPOINT, MMHINT_DONE );	    
    }
}    
/*=====================================================================*/

/* ARGSUSED */
static void pgjet_mvdelSub ( Widget wid, XtPointer clnt, XEvent *event,
							Boolean *ctdr )
/************************************************************************
 * pgjet_mvdelSub							*
 *									*
 * This function actually moves or deletes a barb/hash on a jet line.	*
 * 									*
 * static void pgjet_mvdelSub ( wid, clnt, event )			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	not used				*
 *	*event	XEvent		button event				*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	11/03   	initial coding				*
 * J. Wu/SAIC	11/03   	propogate width/size for DELETE	action	*
 * J. Wu/SAIC	12/03   	use same color for jet's barb & text	*
 * J. Wu/SAIC	02/04   	add delta into barb FL text		*
 * J. Wu/SAIC	02/04   	reset speed/level/deltas after moving	*
 * J. Wu/SAIC	03/04   	reset default text pos. after moving	*
 * J. Wu/SAIC	04/04   	correct text offset when deleting barb	*
 * M. Li/SAIC	10/05		checked for delta format		*		
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * J. Lewis/AWC 03/07           remove check for delta format           *
 * J. Lewis/AWC 03/07           change conditions for yoffset = 3       *		
 ***********************************************************************/
{
    int		nloc = 1, ii, np, ier, loco, xoff, yoff, el_location;
    int		cur_act, selected, lwfactor, yoffset;
    float	xx, yy, lat, lon, llx, lly, urx, ury, *xpts, *ypts;
    char	str[MAX_TEXT];
    VG_DBStruct	el; 
/*---------------------------------------------------------------------*/
    
    if ( event->xbutton.button == Button1 ) {
        	
	xgtoff ( &xoff, &yoff, &ier );
	xx = (float) (event->xbutton.x + xoff);
	yy = (float) (event->xbutton.y + yoff);
	
	gtrans ( sys_D, sys_M, &nloc, &xx, &yy, &lat, &lon, 
		&ier, strlen(sys_D), strlen(sys_M) );
	
        pgactv_getDevPts ( &np, &xpts, &ypts );

        el_location = pgactv_getElmLoc ();
	pgutls_prepNew ( el_location, &el, &llx, &lly, &urx, &ury, &ier );
	pgundo_newStep ();
	pgundo_storeThisLoc ( el_location, UNDO_DEL, &ier );
        
	cur_act = pgjet_getCurAction ();
        selected = pgjet_getSelectedSub ();
			
	switch( cur_act ) {

            case MOVE_BARB:				        
	        el.elem.jet.barb[selected].wndcol = _attrBarbColr;
		el.elem.jet.barb[selected].wnd.info.width = _attrBarbWdth;
	        el.elem.jet.barb[selected].wnd.info.size = _attrBarbSize;
		el.elem.jet.barb[selected].wnd.data.latlon[0] = lat;
	        el.elem.jet.barb[selected].wnd.data.latlon[1] = lon;
	        el.elem.jet.barb[selected].wnd.data.spddir[0] = _barbSpeed;

	        el.elem.jet.barb[selected].sptcol = _attrBarbColr;
	        el.elem.jet.barb[selected].spt.info.lat = lat;
	        el.elem.jet.barb[selected].spt.info.lon = lon;

		pgjet_formatFL ( _barbLevel, _barbDelta1, _barbDelta2, str );

		
		/*
		 *  Reset to default text position. It is slightly adjusted from
		 *  wind barb point.
		 */
                el.elem.jet.barb[selected].spt.info.offset_x = 0;
		
		lwfactor = (int)(el.elem.jet.line.spl.info.splwid / 14.0);
		
		if ( ( _barbDelta1 != 0 ) || ( _barbDelta2 != 0 ) ) {
		    yoffset = 3;
		}
		else {
		    yoffset = 2;
		}               
	        
		if ( lat >= 0.0 ) {	/* Northern Hemisphere */
		    el.elem.jet.barb[selected].spt.info.offset_y = -yoffset - lwfactor;
                }
		else {			/* Southern Hemisphere */
		    el.elem.jet.barb[selected].spt.info.offset_y = yoffset + lwfactor;
		}

		strcpy ( el.elem.jet.barb[selected].spt.text, str );

		pgjet_setSpdLvl ( _barbSpeed, _barbLevel );
	    
	    break;
          
	    case DELETE_BARB:	    		
	        for ( ii = selected; ii < el.elem.jet.nbarb-1; ii++ ) {
	            el.elem.jet.barb[ii].wndcol =
		            el.elem.jet.barb[ii+1].wndcol;
	            el.elem.jet.barb[ii].wnd.info.width =
		            el.elem.jet.barb[ii+1].wnd.info.width;
	            el.elem.jet.barb[ii].wnd.info.size = 
		            el.elem.jet.barb[ii+1].wnd.info.size;
	            el.elem.jet.barb[ii].wnd.data.latlon[0] =
		            el.elem.jet.barb[ii+1].wnd.data.latlon[0];
	            el.elem.jet.barb[ii].wnd.data.latlon[1] = 
		            el.elem.jet.barb[ii+1].wnd.data.latlon[1];
	            el.elem.jet.barb[ii].wnd.data.spddir[0] = 
		            el.elem.jet.barb[ii+1].wnd.data.spddir[0];
	            el.elem.jet.barb[ii].wnd.data.spddir[1] = 
		            el.elem.jet.barb[ii+1].wnd.data.spddir[1];
	            	        		    
		    el.elem.jet.barb[ii].sptcol =
		            el.elem.jet.barb[ii+1].sptcol ;
	            el.elem.jet.barb[ii].spt.info.lat = 
		            el.elem.jet.barb[ii+1].spt.info.lat ;
	            el.elem.jet.barb[ii].spt.info.lon = 
		            el.elem.jet.barb[ii+1].spt.info.lon;
		    el.elem.jet.barb[ii].spt.info.offset_x = 
		    	    el.elem.jet.barb[ii+1].spt.info.offset_x;
		    el.elem.jet.barb[ii].spt.info.offset_y = 
		    	    el.elem.jet.barb[ii+1].spt.info.offset_y;
                    strcpy ( el.elem.jet.barb[ii].spt.text, 
		                el.elem.jet.barb[ii+1].spt.text );		
		}		
	        
		el.elem.jet.barb[el.elem.jet.nbarb-1].wnd.info.numwnd = 0;
		(el.elem.jet.nbarb)--;		
	    
	    break;

	    case MOVE_HASH:	
	        el.elem.jet.hash[selected].wndcol = _attrHashColr;
	        el.elem.jet.hash[selected].wnd.info.width = _attrHashWdth;
	        el.elem.jet.hash[selected].wnd.info.size = _attrHashSize;
	        el.elem.jet.hash[selected].wnd.data.latlon[0] = lat;
	        el.elem.jet.hash[selected].wnd.data.latlon[1] = lon;
	    break;
	  
	    case DELETE_HASH:	
	        for ( ii = selected; ii < el.elem.jet.nhash-1; ii++ ) {
	            el.elem.jet.hash[ii].wndcol =
		            el.elem.jet.hash[ii+1].wndcol;
	            el.elem.jet.hash[ii].wnd.info.width =
		            el.elem.jet.hash[ii+1].wnd.info.width;
	            el.elem.jet.hash[ii].wnd.info.size = 
		            el.elem.jet.hash[ii+1].wnd.info.size;
	            el.elem.jet.hash[ii].wnd.data.latlon[0] =
		            el.elem.jet.hash[ii+1].wnd.data.latlon[0];
	            el.elem.jet.hash[ii].wnd.data.latlon[1] = 
		            el.elem.jet.hash[ii+1].wnd.data.latlon[1];
	            el.elem.jet.hash[ii].wnd.data.spddir[0] = 
		            el.elem.jet.hash[ii+1].wnd.data.spddir[0];
	            el.elem.jet.hash[ii].wnd.data.spddir[1] = 
		            el.elem.jet.hash[ii+1].wnd.data.spddir[1];
		}		
	        
		el.elem.jet.hash[el.elem.jet.nhash-1].wnd.info.numwnd = 0;
		(el.elem.jet.nhash)--;		
	    break;
	  	
	}
			
	pgvgf_saveNewElm ( NULL, sys_D, &el, np, xpts, ypts, TRUE, &loco, 
			&ier );
	pgundo_storeThisLoc ( loco, UNDO_ADD, &ier );
        pgundo_endStep ();
	pgutls_redraw ( loco, &el, &ier );
            
	
	/*
	 *  For MOVE, keep the barb/hash selected for continuous MOVE
	 *  until the user hits middle mouse to end the MOVE. For DELETE,
	 *  the action is done. So reset mouse for next selection.
	 */ 	    
        if ( cur_act == DELETE_BARB || cur_act == DELETE_HASH ) {

            pgjet_refresh ();

	    mcanvw_disarmDynamic ();
	    mcanvw_setPressFunc ( (XtEventHandler)&pgjet_mvdelHandler, CURS_DEFAULT ); 
	    mbotw_mouseSet ( LMHINT_SELECT, MMHINT_DONE );	
	}
	else if ( cur_act == MOVE_BARB || cur_act == MOVE_HASH ) {
            pghdlb_select ( &el, loco );	    
	}
	
    }
    
    else {
        
	pgjet_popdownBarbInfo ();
		
        pgjet_refresh ();
		
	mcanvw_disarmDynamic ();
	mcanvw_setPressFunc ( (XtEventHandler)&pgjet_mvdelHandler, CURS_DEFAULT ); 
	mbotw_mouseSet ( LMHINT_SELECT, MMHINT_DONE );	
    }
}

/*=====================================================================*/

static void pgjet_refresh ( void )
/************************************************************************
 * pgjet_refresh							*
 *									*
 * Refresh the display and place handbar on the current jet line.	*
 *									*
 * static void pgjet_refresh ( )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		11/03	initial coding				*
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 * J. Lewis/AWC         03/07   remove check for delta format           *   
 ***********************************************************************/
{  
    int		num, ier, el_layer, location;
    float	llx, lly, urx, ury;
    VG_DBStruct	el;
    filter_t	filter;
/*---------------------------------------------------------------------*/

    /*
     *  Cancel the selection of the barb/hash on the jet line.
     */
    _selectedSub = -1; 		

    /*
     *  Deselect the jet and refresh the display.
     */
    location = pgactv_getElmLoc ();
    cvg_rdrec ( cvg_getworkfile(), location, &el, &ier );
    crg_getinx ( location, &num, &ier );
    if (ier == 0) {
	pghdlb_deselectEl ( num, TRUE );

        crg_get( num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);
	if (ier == 0) {
	    xpgpaste ( llx,  lly, urx,  ury, &ier );
	    cvg_rfrsh( NULL, llx, lly, urx, ury, &ier);
	}
    }
    
    /*
     *  Place handbar on the jet line.
     */
    pghdlb_select ( &el, location );

}

/*=====================================================================*/

static void pgjet_formatFL ( int level, int delta1, int delta2, char *flstr )
/************************************************************************
 * pgjet_formatFL							*
 *									*
 * Formats the flight level text in the form of "FLxxx".		*
 *									*
 * static void pgjet_formatFL ( level, delta1, delta2, flstr )		*
 *									*
 * Input parameters:							*
 *	level		int	Flight level				*
 *	delta1		int	Delta level 1				*
 *	delta2		int	Delta level 2				*
 *									*
 * Output parameters:							*
 *	*flstr		char	Formatted flight level text string	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding				*
 * M. Li/SAIC		10/05	checked for delta format		*
 * J. Lewis/AWC         03/07   remove check for delta format           *
 ***********************************************************************/
{  
    char	tmpstr[16];
/*---------------------------------------------------------------------*/
    
    if ( flstr != NULL ) {    
        
	if ( level < 1 ) {
            strcpy ( flstr, "FL300" ); 
        }    
        else if ( level < 10 ) {
            sprintf ( flstr, "FL00%i", _barbLevel ); 
        }
        else if ( level < 100 ) {
            sprintf ( flstr, "FL0%i", _barbLevel );
        }
        else {
            sprintf ( flstr, "FL%i", _barbLevel );
        }

	if ( (delta1 != 0) && (delta2 != 0) ) {
	     sprintf ( tmpstr, "\n%i/%i", delta2, delta1 );
             strcat ( flstr, tmpstr );
        }
    }
        
}

/*=====================================================================*/
