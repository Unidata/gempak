#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_mainw.h"	      /* MAPW_NAME */
#include "nmap_data.h"
#include "nmapprm.h"

#define ICON_DIR    	"$NAWIPS/icons/nmap"
#define ICON_FGNAME 	"blue"
#define ICON_BGNAME 	"white"


static Widget 		_mapPopW;      	/* map selection popup window */
static Widget 		_mapwCanvasW;  	/* map drawing area in map window */

static WidgetList 	_mapAreaBtn;   	/* pre-defined map toggle buttons */
static Widget 		_showImageBtn; 	/* show image button */
static Widget   	_ctlBtn[3];

static WidgetList 	_mapBtn;   	/* map overlay check buttons */
static WidgetList 	_ovlBtn;	/* map ovelay push buttons   */


static float  		_latlonInc[2];  /* latitude(0)/longitude(1) increment */
static Widget		_latW, _lonW;   /* text input widgets for lat/lon */

                                        /* display image data beneath the map */
static Boolean		_showImage[MAX_LOOP];  	/* show image flag	*/ 

static int		_lpId;		/* loop index */
static int		_currOvl;	/* current overlay index */
static Boolean		_changeMade;
static Boolean		_zoomActv;
static Boolean  	_roamWasUp;	/* roam control box state */
static int		_whichMap = 0; 	/* loaded map index    */
static int		_mapSelected = 0; /* selected map index  */

static int		_lblfreq[MAX_LOOP][2];	 /*lat/lon labeling frequency*/
static float		_ltlnstep[MAX_LOOP][2];  /*lat/lon increment frequency*/

static  int    		_ifnt  = 1;
static  int    		_ihwsw = 2;
static  int    		_ibrdr = 111;
static  float  		_tsize = 1.0F;
static  int    		_iwid  = 1;
static  int    		_irot  = 1;
static  int    		_ijust = 1;
static	int    		_maxmap = 30;

static NxmLineA_t  	_line;
static NxmMarkA_t  	_mark;
static NxmScaleA_t 	_scale;

/*
 *  Private callback functions
 */
void mapw_canvasExposeCb  ( Widget, XtPointer, XmDrawingAreaCallbackStruct* );
void mapw_latlonBtnCb 	  ( Widget, String, XtPointer );
void mapw_latlonTxtCb 	  ( Widget, long, XtPointer );
void mapw_overlayCheckbCb ( Widget, long, XtPointer );
void mapw_overlayPushbCb  ( Widget, long, XtPointer );
void mapw_pdfToggleCb 	  ( Widget, long, XtPointer );
void mapw_previewCb 	  ( Widget, XtPointer, XtPointer );
void mapw_showImageCb 	  ( Widget, XtPointer, XmToggleButtonCallbackStruct* );
void mapw_mapsetCb	  ( Widget, long, XtPointer );

/*
 *  Private functions
 */
Widget mapw_createLatlonOpt ( Widget parent);
void mapw_latlonApply 	    ( void );
void mapw_latlonInit 	    ( void );
void mapw_rgstr 	    ( void );
void mapw_setLnMk 	    ( int lp, int ovl );
 
/************************************************************************
 * nmap_mapw.c                                                          *
 *                                                                      *
 * This module creates the map selection popup window and defines the   *
 * callback functions for nmap. 					*
 *                                                                      *
 * CONTENTS:                                                            *
 *      mapw_readAreaTbl()   read predefined map area info table.    	*
 *      mapw_readOvlTbl()    read predefined map overlay table.    	*
 *                                                                      *
 *      mapw_create()   creates the map editing popup window.           *
 *      mapw_rgstr()    registers the map drawing window to GEMPAK.     *
 *      mapw_popup()    pop up the map editing window.           	*
 *      mapw_popdown()  pop down the map editing window.           	*
 *      mapw_pdfToggleCb()     callback for pre-defined geographic area.*
 *      mapw_previewCb()       callback for preview button 		*
 *      mapw_overlayCheckbCb() callback for overlay check buttons.   	*
 *	mapw_showImageCb()	callback for show image button		*
 *      mapw_overlayPushbCb()  callback for overlay push buttons.   	*
 *      mapw_canvasExposeCb()  expose callback for map drawing area.   	*
 *      mapw_ctlBtnCb()        callback of control buttons.    		*
 *                                                                      *
 *      mapw_redrawMap()       redraw the map.    			*
 *      mapw_isUp()       checks whether the map editing window is up.  *
 *      mapw_updtOvlBtns() update the state of overlay check buttons.	*
 *	mapw_updtMapBtns() update the state of map area buttons.	*
 *      mapw_createLatlonOpt() creates the lat/lon increment selection  *
 *						panel.			*
 *      mapw_latlonTxtCb()   callback function for lat/lon text input.  *
 *      mapw_latlonBtnCb()   callback function for lat/lon easy buttons.*
 *      mapw_latlonApply()   specific latlon function for APPLY button. *
 *      mapw_latlonInit()    specific latlon function when pops up  	*
 *                                                                      *
 *      mapw_ctlBtnUpd()        Update control buttons                  *
 ***********************************************************************/

/*=====================================================================*/

Widget mapw_create ( Widget parent )
/************************************************************************
 * mapw_create                                              		*
 *                                                                      *
 * This function creates the map editing popup window.    		*
 *                                                                      *
 * Widget mapw_create(parent)                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent form widget ID                          *
 * Output parameters:                                                   *
 * mapw_create	Widget   map editing popup window widget ID             *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96  						*
 * S. Schotz/NCEP  	 7/97  Replaced Roads with Marine regional map	*
 * C. Lin/EAI      	07/97  Modified to add more overlay features	*
 *				add preview button			*
 * S. Wang/GSC		09/97  	replace attrw_*() functions with nxm	*
 *				library functions NxmLineA_*()		*
 * S. Wang/GSC	   	09/97  	add NxmMarkA_create()			*
 * G. Krueger/EAI  	10/97  	NxmFrameLabel->NxmLabel_createFrameLbl	*
 * G. Krueger/EAI  	10/97  	NxmControlBtn->NxmCtlBtn_create 	*
 * S. Law/GSC		06/99  	added Show Image button			*
 * H. Zeng/EAI  	10/99  	update for multiple loops of nmap2	*
 * H. Zeng/EAI          10/99  	used static _showImageBtn widget        *
 * H. Zeng/EAI          01/00  	Increased the sizes of overlay buttons  *
 * M. Li/GSC            11/00  	Combined "LOAD" and "Accept"            *
 * M. Li/GSC		03/01  	added NMP library			*
 * E. Safford/GSC	06/01	rm bottom attachment on subpane		*
 * M. Li/GSC		06/01	Added initialization of _showImage	*
 * E. Safford/GSC	06/01	scale map to 2/3rds of main canvas	*
 * E. Safford/GSC	07/01	move show_image init to before return() *
 * M. Li/SAIC		12/01	added apply/get map setting buttons	*
 * M. Li/SAIC		01/02	replace bitmaps with push buttons	*
 * E. Safford/SAIC	02/02	make mapset window a child of _mapPopW  *
 * M. Li/SAIC		12/02	radio box -> check Box			*
 * R. Tian/SAIC         01/03   add True flag to NxmBxmBtn_create(Multi)*
 * J. Wu/SAIC         	12/03   make GUI compact & re-align code layout	*
 * H. Zeng/SAIC		08/04	added a new overlay button		*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    int		nn, lp, iret, ier, intnum, intattr[5], ovlnum, itype;
    int		wdth, hght, btn_hght = 20;
    Widget 	form, pane, rc, form1, subpane, frame1, pane_fr1;
    Widget 	rc2, frame2, pane_fr2, rc_form, area_scrlw, ovl_scrlw;
    Widget 	subform, rc1, prevbtn, getMap_btn, appMap_btn;

    char	*btnstr[] = { "LOAD", "Help", "Cancel" };
    char   	iconfile[256];
    long   	ii, ignore;

    nmpovlstr_t	ovlattr, ovlnms[MAX_OVL];
    nmpstr_t	mapnms[MAX_MAP];

    static char	preview_label[30];

/*---------------------------------------------------------------------*/

    /*
     * create dialog shell
     */
    _mapPopW = XmCreateFormDialog ( parent, "mapw_popup", NULL, 0 );
    XtVaSetValues ( _mapPopW, 
		XmNnoResize,			True, 
		NULL );
    XtVaSetValues ( XtParent(_mapPopW),
		XmNtitle,			"Map Selection",
                NULL );

    /*
     * create a parent pane widget
     */
    pane = XtVaCreateWidget ( "mapw_pane",
		xmPanedWindowWidgetClass,	_mapPopW,
		XmNsashWidth,			1,
		XmNsashHeight,			1,
		NULL);

    form = XtVaCreateWidget ( "mapw_topForm",
		xmFormWidgetClass,		pane,
		NULL );

    /*
     * create the predefined geographic area 
     */
    frame1 = XtVaCreateWidget ( "mapw_pdfFrame",
		xmFrameWidgetClass,		form,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNbottomAttachment,		XmATTACH_FORM,
		NULL );
    pane_fr1 = XtVaCreateWidget("mapw_pdfFrame",
		xmPanedWindowWidgetClass,  	frame1,
		XmNsashWidth,			1,
		XmNsashHeight,			1,
		NULL);

    NxmLabel_createFrameLbl("Predefined Area", pane_fr1, frame1);

    area_scrlw = XtVaCreateWidget ( "ovl_scrollwin",
		xmScrolledWindowWidgetClass,	pane_fr1,
		XmNscrollingPolicy,		XmAUTOMATIC,
		XmNscrollBarDisplayPolicy,	XmAS_NEEDED,
		XmNscrollBarPlacement,		XmBOTTOM_LEFT,
		XmNtraversalOn,			FALSE,
		XmNwidth,			150,
		XmNspacing,			0,
		NULL );				

    rc = XtVaCreateWidget ( "rc",
		xmRowColumnWidgetClass,		area_scrlw,
		XmNorientation,			XmVERTICAL,
		XmNradioBehavior,		False,
		XmNtraversalOn,			False,
		XmNspacing,			0,
		NULL );

    /*
     * create the toggle buttons for predefined area
     */
    nmp_gmapnum ( &nn, &ier );
    nmp_gmapnms ( mapnms, &ier );
    _maxmap = nn;

    _mapAreaBtn = (WidgetList) XtMalloc( (size_t)nn * sizeof(Widget) );
    for ( ii = 0; ii < nn; ii++ ) {
	_mapAreaBtn[ii] = XtVaCreateManagedWidget ( mapnms[ii],
		xmToggleButtonWidgetClass,	rc,
		XmNheight,			btn_hght,
		XmNmarginHeight,		0,
		XmNmarginWidth,			0,
		NULL );

	XtAddCallback ( _mapAreaBtn[ii], XmNarmCallback,
		(XtCallbackProc)mapw_pdfToggleCb, (XtPointer)ii );
    }

    XtManageChild ( rc );
    XtManageChild ( area_scrlw );
    XtManageChild ( pane_fr1 );
    XtManageChild ( frame1 );

/*    _projPanelW = frame1;  NOT used */


    /*
     * create pane widget as parent of map feature and drawing area
     */
    subpane = XtVaCreateWidget ( "pane",
		xmPanedWindowWidgetClass,	form,
		XmNsashWidth,			1,
		XmNsashHeight,			1,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			frame1,
		NULL );

    subform = XtVaCreateWidget ( "mapw_subForm",
		xmFormWidgetClass,		subpane,
		NULL );


    /*
     * create the first two buttons for map feature overlay
     */
    rc = XtVaCreateWidget ("rc",
		xmRowColumnWidgetClass,		subform,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			15,
		XmNorientation,			XmHORIZONTAL,
		XmNspacing,			15,
		XmNtraversalOn,			False,
		NULL );

    nmp_govlnum ( &ovlnum, &ier );
    nmp_govlnms ( ovlnms, &ier );

    _mapBtn = (WidgetList) XtMalloc ( (size_t)ovlnum * sizeof(Widget) );
    _ovlBtn = (WidgetList) XtMalloc ( (size_t)ovlnum * sizeof(Widget) );

    for ( ii = 0; ii < 3; ii++ ) {

        form1 = XtVaCreateWidget( "rc", xmFormWidgetClass, rc, NULL );

	_mapBtn[ii] = XtVaCreateManagedWidget ( " ",
		xmToggleButtonGadgetClass,	form1,
		XmNindicatorType,		XmN_OF_MANY,
		XmNset,				(ii != 2),
		XmNleftAttachment,		XmATTACH_FORM,
		XmNheight,			btn_hght,
		XmNmarginHeight,		0,
		XmNmarginWidth,			0,
		NULL );
	XtAddCallback ( _mapBtn[ii], XmNvalueChangedCallback,
		(XtCallbackProc)mapw_overlayCheckbCb, (XtPointer)ii);

	_ovlBtn[ii] = XtVaCreateManagedWidget ( ovlnms[ii],
		xmPushButtonGadgetClass,	form1,
		XmNshadowThickness,		1,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_mapBtn[ii],
		XmNheight,			btn_hght,
		NULL );
	XtAddCallback ( _ovlBtn[ii], XmNactivateCallback,
		(XtCallbackProc)mapw_overlayPushbCb, (XtPointer)ii);

	XtManageChild ( form1 );
    }


    /*
     * create Show Image toggle
     */
    form1 = XtVaCreateWidget ( "rc", xmFormWidgetClass,	rc, NULL );

    _showImageBtn = XtVaCreateManagedWidget ( " ",
		xmToggleButtonGadgetClass,	form1,
		XmNindicatorType,		XmN_OF_MANY,
		XmNset,				FALSE,
		XmNheight,			btn_hght,
		XmNmarginHeight,		0,
		XmNmarginWidth,			0,
		NULL );
    XtAddCallback ( _showImageBtn, XmNvalueChangedCallback,
		(XtCallbackProc)mapw_showImageCb, (XtPointer) NULL );

    XtVaCreateManagedWidget ( "Show Image",
		xmLabelWidgetClass,		form1,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_showImageBtn,
		XmNheight,			btn_hght,
		NULL );

    XtManageChild ( form1 );
    XtManageChild ( rc );

    /*
     * create preview button
     */
    cfl_inqr ( "preview.xbm", ICON_DIR, &ignore, iconfile, &iret );

    strcpy ( preview_label, "preview" );

    prevbtn = NxmBxmBtn_create ( subform, "preview", NULL, 32, 32,
		ICON_FGNAME , ICON_BGNAME, NULL, iconfile, preview_label,
		True, (XtCallbackProc)mapw_previewCb, NULL );

    /*
     * create zoom buttons
     */
    rc1 = zoomw_create ( subform );
    XtVaSetValues ( rc1,
		XmNrightAttachment,		XmATTACH_FORM,
		XmNrightOffset,			20,
		NULL );

    XtVaSetValues ( prevbtn,
		XmNtopAttachment,		XmATTACH_OPPOSITE_WIDGET,
		XmNtopWidget,			rc1,
		XmNtopOffset,			3,
		XmNrightAttachment,		XmATTACH_WIDGET,
		XmNrightWidget,			rc1,
		NULL );


    /*
     * create Get Map Settings button
     */
    getMap_btn = XtVaCreateManagedWidget ( "Get Settings",
		xmPushButtonWidgetClass,	subform,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			rc,
		XmNrightAttachment,		XmATTACH_WIDGET,
		XmNrightWidget,			prevbtn,
		XmNrightOffset,			10,
		XmNheight,			btn_hght+6,
		XmNtraversalOn,			False,
		NULL );

    XtAddCallback ( getMap_btn, XmNactivateCallback,
		(XtCallbackProc)mapw_mapsetCb, (XtPointer) MAPSET_GET );

    /*
     * create Apply Map Settings button
     */
    appMap_btn = XtVaCreateManagedWidget ( "Apply Settings",
		xmPushButtonWidgetClass,	subform,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			rc,
		XmNrightAttachment,		XmATTACH_WIDGET,
		XmNrightWidget,			getMap_btn,
		XmNheight,			btn_hght+6,
		XmNtraversalOn,			False,
		NULL );

    XtAddCallback ( appMap_btn, XmNactivateCallback,
		(XtCallbackProc)mapw_mapsetCb, (XtPointer) MAPSET_APPLY );

    XtManageChild ( subform );

    /*
     * create the map drawing area 
     *
     *	This should be in proportion to the main window's drawing
     *	area so that zoom yields the same results for the two areas.
     *	Width is the limiting factor for the map window, so use that
     *	as a check.  If width of the main window is > 850 pixels, use 
     *	2/3rds the width for the map window, else use the actual 
     *	width of the main window for the map window.
     */
    mcanvw_getDims ( &wdth, &hght );

    if ( wdth > 850 ) {
        wdth = (int) ( (double)wdth * .667 );
        hght = (int) ( (double)hght * .667 );
    }

    _mapwCanvasW = XtVaCreateManagedWidget ( "mapw_canvas",
		xmDrawingAreaWidgetClass,	subpane,
		XmNwidth,			wdth,
		XmNheight,			hght,
		NULL );

    XtAddCallback ( _mapwCanvasW, XmNexposeCallback, 
		(XtCallbackProc)mapw_canvasExposeCb, NULL );

    XtManageChild ( subpane );
    XtManageChild ( form );


    /*
     * create the rest of overlay features if necessary
     */
    if ( ovlnum > 2 ) {

        /*
         * create the rest of predefined overlay feature
         */
        frame2 = XtVaCreateWidget ( "mapw_ovlFrame",
		xmFrameWidgetClass,		form,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			subpane,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNbottomAttachment,		XmATTACH_FORM,
		XmNrightAttachment,		XmATTACH_FORM,
		NULL );

        pane_fr2 = XtVaCreateWidget ( "mapw_pdfFrame",
		xmPanedWindowWidgetClass,	frame2,
		XmNsashWidth,			1,
		XmNsashHeight,			1,
		NULL );

        NxmLabel_createFrameLbl ( "Overlays", pane_fr2, frame2 );

        ovl_scrlw = XtVaCreateWidget ( "ovl_scrollwin",
		xmScrolledWindowWidgetClass,	pane_fr2,
		XmNscrollingPolicy,		XmAUTOMATIC,
		XmNscrollBarDisplayPolicy,	XmAS_NEEDED,
		XmNtraversalOn,			FALSE,
		XmNwidth,			190,
		XmNspacing,			0,
		NULL );				
	   
        rc2 = XtVaCreateWidget ( "ovl_rc",
		xmRowColumnWidgetClass,		ovl_scrlw,
		XmNorientation,			XmVERTICAL,
		XmNtraversalOn,			False,
		XmNspacing,			0,
		NULL );

        /*
         * create the buttons for each overlay
         */
        for ( ii = 3; ii < ovlnum; ii++ ) {

            form1 = XtVaCreateWidget ( "rc",
		xmFormWidgetClass,		rc2,
		NULL );

            _mapBtn[ii] = XtVaCreateManagedWidget ( " ",
		xmToggleButtonWidgetClass,	form1,
		XmNindicatorType,		XmN_OF_MANY,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,		   	0,
		XmNheight,			btn_hght,
		XmNmarginHeight,		0,
		XmNmarginWidth,			0,
		XmNset,				False,
		NULL );
            
	    XtAddCallback ( _mapBtn[ii], XmNvalueChangedCallback,
		(XtCallbackProc)mapw_overlayCheckbCb, (XtPointer)ii);

            _ovlBtn[ii] = XtVaCreateManagedWidget ( ovlnms[ii],
		xmPushButtonWidgetClass,	form1,
		XmNshadowThickness,		1,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			_mapBtn[ii],
		XmNheight,			btn_hght,
		XmNsensitive,			False,
		NULL );
            
	    XtAddCallback ( _ovlBtn[ii], XmNactivateCallback,
		(XtCallbackProc)mapw_overlayPushbCb, (XtPointer)ii);

            XtManageChild ( form1 );
	}

        XtManageChild ( rc2 );
        XtManageChild ( ovl_scrlw );
        XtManageChild ( pane_fr2 );
        XtManageChild ( frame2 );

    }
    else {
        XtVaSetValues ( subpane,
		XmNrightAttachment,		XmATTACH_FORM,
		NULL);
    }


    /*
     * create control buttons
     */
    nn = XtNumber ( btnstr );
    rc_form = XtVaCreateWidget ( "mapw_ctlBtn",
		xmFormWidgetClass,		pane,
		XmNorientation,			XmHORIZONTAL,
		XmNfractionBase,		4*nn,
		XmNtopAttachment,		XmATTACH_WIDGET,
		XmNtopWidget,			subpane,
		NULL );

    for ( ii = 0; ii < nn; ii++ )  {
        _ctlBtn[ii] = XtVaCreateManagedWidget ( btnstr[ii],
		xmPushButtonWidgetClass,	rc_form,
		XmNleftAttachment,		XmATTACH_POSITION,
		XmNleftPosition,		4*ii+1,
		XmNrightAttachment,		XmATTACH_POSITION,
		XmNrightPosition,		4*ii+3,
		NULL );

        XtAddCallback ( _ctlBtn[ii], XmNactivateCallback,
		(XtCallbackProc)mapw_ctlBtnCb, (XtPointer) ii );
    }

    XtVaSetValues ( rc_form, XmNmarginHeight, 15, NULL );

    XtManageChild ( rc_form );
    XtManageChild ( pane );

    /*
     * initialize LAT/LON label frequency and steps
     */
    lp = 0;
    for ( ii = 0; ii < ovlnum; ii++ ) {

        nmp_govlattr ( ii, lp, &itype, ovlattr, &ier );

        if ( itype == 0 ) {

            cst_ilst ( ovlattr, ' ', 1, 5, intattr, &intnum, &ier );

            _ltlnstep[0][0] = _ltlnstep[0][1] = (float)intattr[3];
            _lblfreq[0][0]  = _lblfreq[0][1]  = intattr[4];
            break;
        }
    }

    for ( lp = 1; lp < MAX_LOOP; lp++ ) {
        _ltlnstep[lp][0] = _ltlnstep[lp][1] = _ltlnstep[0][0];
        _lblfreq[lp][0]  = _lblfreq[lp][1]  = _lblfreq[0][0];
    }


    /*
     * create customized map definition popup panel
     */
    mpcstw_create ( _mapPopW );


    /*
     * create map attribute window
     */
    NxmLineA_create ( _mapPopW );


    /*
     * create marker attribute editing window
     */
    NxmMarkA_create ( _mapPopW );


    /*
     * create scale attribute editing window
     */
    NxmScaleA_create ( _mapPopW );


    /* 
     * Initialize the show image flag  
     */
    for ( ii = 0; ii < MAX_LOOP; ii++ ) {
        _showImage[ii] = FALSE;
    }


    /*
     *  Create the Map get/apply windows
     */
    mapset_create ( _mapPopW );


    return ( _mapPopW );

}

/*=====================================================================*/

void mapw_rgstr ( void )
/************************************************************************
 * mapw_rgstr                                                           *
 *                                                                      *
 * This function registers the canvas widget of the map drawing window  *
 * as a GEMPAK window.                                                  *
 *                                                                      *
 * void mapw_rgstr()                                                    *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      4/96                                                 *
 * T. Piper/SAIC	07/03	replaced gmpk_rgstr with NxmGmpkRgstr	*
 ***********************************************************************/
{
char wname[10];

/*---------------------------------------------------------------------*/

        strcpy(wname, MAPW_NAME);
        NxmGmpkRgstr(_mapwCanvasW, wname, NULL);

}

/*=====================================================================*/

void mapw_popup ( void ) 
/************************************************************************
 * mapw_popup 								*
 *                                                                      *
 * This function pops up the map editing popup window.    		*
 *                                                                      *
 * void mapw_popup()                   					*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96  						*
 * C. Lin/EAI      	06/97 	add gmpk_initTxt() 			*
 * C. Lin/EAI      	07/97 	modified to use new data structs 	*
 * C. Lin/EAI      	08/97 	add roam control window checking 	*
 * S. Wang/GSC	   	09/97	remove mapw_copyObj()			*
 * E. Safford/GSC  	06/99	disable auto-update			*
 * S. Jacobs/NCEP  	10/99	Added nindex to checks for SAT or RAD	*
 * H. Zeng/EAI     	10/99   Added show image button status check    *
 * E. Safford/GSC  	10/99   dataw_getCurLoop -> loop_getCurLoop     *
 * E. Safford/GSC	04/00	turn off loop selection btns 		*
 * E. Safford/GSC	08/00	added call to zoomw_isZoomActv		*
 * M. Li/GSC       	11/00   added mapw_ctlBtnUpd                    *
 * M. Li/GSC		03/01	added NMP library			*
 * E. Safford/GSC	05/01	added dataw_disableLoop & set _whichMap *
 * M. Li/GSC		05/01	added nmp_save				*
 * S. Jacobs/NCEP	 7/01	Removed call to gmpk_initTxt		*
 * M. Li/SAIC		01/02	Added loop_saveRoamVal			*
 * E. Safford/SAIC	02/02	add nmp_rstrproj() to fix map navigation*
 * M. Li/SAIC           12/02   radio box -> check Box                  *
 * T. Piper/SAIC	04/05	removed zoomw_zoomFlagSave; never used	*
 * E. Safford/SAIC	04/05	disable roam menu when map pops up      *
 ***********************************************************************/
{
int 		nindex, nn, ier, ii;
char    	wname[30], *str;
XmString	xm_str;
nmpstr_t    	mapDrp, proj, gareaDrp[2], mapnms[MAX_MAP];

static char first = 1;
/*---------------------------------------------------------------------*/
 
    if ( mapw_isUp() ) {
        return;
    }

    _roamWasUp = False;
    _lpId = loop_getCurLoop(); 

    /*
     * check roam control window, make sure it is down
     */
    if ( roamw_isUp() ) {
        roamw_popdown();
	_roamWasUp = True;
    }
    roamw_disableMenu();

    mbtnw_zoomSensitive(False);
    mbtnw_loopSetSensitive(False);
    loopw_sensitive(False);
    auto_stopAutoUpdt();

    /*
     * If the data window is not up save a copy of map overlay attribute 
     * to be edited
     *
     * If the data window is up, then order its loop menu disabled so the
     * user can't change loops on us while the mapw window is up.
     */
    if ( !dataw_isUp() ) {
        nmp_save (&ier);
	loop_saveRoamVal();
    }
    else {
	dataw_disableLoop();
    }


    /*
     * save a copy of the zoom flag
     */
    _zoomActv = zoomw_isZoomActv(_lpId);

    /*
     * Update control buttons "Load" or "Accept"
     */
    mapw_ctlBtnUpd();

    XtManageChild(_mapPopW);

    /*
     * change current window to this window
     */
    if (first) {
	mapw_rgstr();
	first = 0;
    }
    strcpy(wname, MAPW_NAME);
    gslwin(wname, &ier, strlen(wname));
    if ( ier != 0 )
	NxmErr_update();

    /*
     *  Restore the starting navigation information.  This may
     *  have been reset for the main window already, but the remembered 
     *  settings are dimensioned by window.  So, reset them for the
     *  map window.	
     */
    nmp_rstrproj ( _lpId, &ier);

    /*
     * set pre-defined map area when necessary
     */
    nmp_gmapnum(&nn, &ier);
    nmp_gmapnms(mapnms, &ier);
    nmp_gmapattr(_lpId, mapDrp, proj, gareaDrp, &ier);


    if ( !dataw_isSatSelect(_lpId, &nindex) ) {

        /*
         * gray out the SAT button
         */
        XtSetSensitive(_mapAreaBtn[nn-SAT_BTN], False);

        /*
         * if previous projection is satellite
         * change it to default setting 
         */ 
        if ( strcmp(proj, "SAT") == 0 ) {
	    for (ii = 0; ii < _maxmap; ii++ ) {
		if ( ii == 0 )  
            	    XmToggleButtonSetState(_mapAreaBtn[ii], True, True);
	 	else
		    XmToggleButtonSetState(_mapAreaBtn[ii], False, False);
	    }
   	    mapw_pdfToggleCb(_mapAreaBtn[0], 0, NULL);
        }
    }
    else { 

	/*
	 * make the SAT button sensitive
	 */
        XtSetSensitive(_mapAreaBtn[nn-SAT_BTN], True);
    }

    /*
     * locate and set the correct predefined toggle button
     */
     _whichMap = 0;
     for (ii=0; ii < nn; ii++) {
	XtVaGetValues (_mapAreaBtn[ii],
	 	XmNlabelString,			&xm_str,
		NULL);
	XmStringGetLtoR ( xm_str, XmFONTLIST_DEFAULT_TAG, &str);
	XmStringFree (xm_str);

	if ( strcmp (str, mapDrp) == 0 ) {
	    _whichMap = ii;
	    XtFree (str);
	    break;
	}
	XtFree (str);
    }

    for (ii = 0; ii < _maxmap; ii++ ) {
      	if ( ii == _whichMap )
            XmToggleButtonSetState(_mapAreaBtn[ii], True, True);
        else
            XmToggleButtonSetState(_mapAreaBtn[ii], False, False);
    }


    /*
     * update the status of map overlay check buttons
     */
    mapw_updtOvlBtns();

    /*
     *  validate the status of show image button
     */

    if (_showImage[_lpId]) {
	XmToggleButtonGadgetSetState(_showImageBtn, True, False);
    }
    else {
	XmToggleButtonGadgetSetState(_showImageBtn, False, False);
    }

    /*
     * set the map area and redraw map window
     */
    mapw_setLnMk(_lpId, _currOvl);
    mapw_redrawMap();

    _changeMade = FALSE;

}

/*=====================================================================*/

void mapw_popdown ( void )
/************************************************************************
 * mapw_popdown                                             		*
 *                                                                      *
 * This function pops down the map editing popup window.    		*
 *                                                                      *
 * void mapw_popdown()                   				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI           04/96  						*
 * E. Safford/GSC	06/99	enable auto-update if dataw isn't up	*
 * E. Safford/GSC	04/00	enable loop buttons  			*
 * E. Safford/GSC	05/01	add dataw_enableLoop() 			*
 * E. Safford/GSC	05/01   do gslwin() before restarting looping   *
 * M. Li/SAIC		01/01	added mapset_popdown			*
 * E. Safford/GSC	04/05   reset roam menu                         *
 ***********************************************************************/
{
int 	ier;
char    wname[10];

/*---------------------------------------------------------------------*/

    XtUnmanageChild(_mapPopW);

    /*
     * change back to the current window
     */
    strcpy(wname, MCANVW_NAME);
    gslwin(wname, &ier, strlen(wname));

    if ( ier != 0 ) {
	NxmErr_update();
    }	

    if ( !dataw_isUp() ) {
	mbtnw_zoomSensitive(True);
	mbtnw_loopSetSensitive(TRUE);
	loopw_sensitive(True);
	auto_startAutoUpdt();
    }
    else {
	dataw_enableLoop();
    }

    if (mapset_isUp()) {
	mapset_popdown();
    }

    /*
     *  reset roam menu and window
     */
    roamw_enableMenu();
    roamw_setup( loop_getCurLoop(), TRUE );
    if( _roamWasUp ) {
        roamw_popup();
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_pdfToggleCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * mapw_pdfToggleCb                                                     *
 *                                                                      *
 * Callback function for predefined geographic area toggle buttons.     *
 *                                                                      *
 * void mapw_pdfToggleCb(w, which, call)                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        button index                               *
 *  call          XtPointer  not used     				*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96                                           *
 * C. Lin/EAI      	07/97   modify to use new data structs		*
 * E. Safford/GSC  	10/99   dataw_getCurLoop -> loop_getCurLoop     *
 * M. Li/GSC            03/01   added NMP library                       *
 * E. Safford/GSC	05/01	remove special SAT case			*
 * E. Safford/GSC	05/01	use zoom garea if available		*
 * E. Safford/SAIC	02/02	move _changeMade flag to catch custom   *
 * M. Li/SAIC           12/02   radio box -> check Box                  *
 * T. Piper/SAIC	10/04	added nmp_sovlattr for auto lat		*
 ***********************************************************************/
{
int		ier, ignore, mapnum, idx, ii;
nmpstr_t	mapnms[MAX_MAP], mapDrp, proj, garea[2];
nmpovlstr_t     ovlattr;

/*---------------------------------------------------------------------*/
    nmp_gmapnum(&mapnum, &ier);
    nmp_gmapnms(mapnms, &ier);
    nmp_gmapattr(_lpId, mapDrp, proj, garea, &ier);

    _changeMade = TRUE;

/*
 *  Update the map selection if this isn't the currently selected map
 */
    if ( strcmp (mapDrp, mapnms[which]) != 0 ) {
	nmp_setmap(mapnms[which], _lpId, 0, &ier);
}

    for (ii = 0; ii < _maxmap; ii++ ) {
        XmToggleButtonSetState(_mapAreaBtn[ii], False, False);
    }

    if ( which == (mapnum - CUSTOM_BTN)) { 

/*
 *  If there is a zoom area, use that in the custom window
 */
        idx = ( strlen(garea[1]) > (size_t)0 ) ?  1 : 0;
	mpcstw_popup(proj, garea[idx]); 
	return;
    }

    XmUpdateDisplay(XtParent(w));

/*
 * draw map
 */
    gclear( &ignore );
    if ( _scale.lat_opt == 0 ) {  /*  Auto mode for latitude  */
	sprintf(ovlattr, "%d %d %d %2.2f %d %s %d %d %2.2f %d",
                      _scale.color, _scale.unit, _scale.lat_opt,
                      RMISSD, _scale.val_opt, _scale.value_txt, 
		      _scale.pos, _scale.font, _scale.size, _scale.style );
	nmp_sovlattr(_lpId, _currOvl, ovlattr, &ier);
	}

    mapw_redrawMap();
    
    _mapSelected = (int)which;

}

/*=====================================================================*/
/* ARGSUSED */
void mapw_previewCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * mapw_previewCb                                                       *
 *                                                                      *
 * Callback function for preview map overlay feature.     		*
 *                                                                      *
 * void mapw_previewCb(w, clnt, call)                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *	w		Widget			widget ID               *
 *	clnt		XtPointer		client data		*
 *	call		XtPointer		not used     		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      07/97                                                *
 ***********************************************************************/
{
    mapw_redrawMap();
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_mapsetCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * mapw_mapsetCb                                                       	*
 *                                                                      *
 * Callback function for Apply/Get Map Settings buttons			* 
 *                                                                      *
 * void mapw_mapsetCb (w, which, call)                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        button index                               *
 *  call          XtPointer  not used                                   *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		01/02						*
 * M. Li/SAIC		01/02	added function id			*
 ***********************************************************************/
{
    int func;
/*---------------------------------------------------------------------*/
    func = (int)which;
    mapset_popup(func);
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_overlayCheckbCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * mapw_overlayCheckbCb                                                 *
 *                                                                      *
 * Callback function for overlaying check buttons.     			*
 *                                                                      *
 * void mapw_overlayCheckbCb(w, which, call)                            *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        button index                               *
 *  call          XtPointer  callback data struct     			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96                                           *
 * C. Lin/EAI      	04/97 	use $MAPFIL                             *
 * C. Lin/EAI      	07/97   modified to set the ovlflag only        *
 * H. Zeng/EAI     	08/99   change "mask" to "mask[2]" in order to  *
 *                              allow more overlays                     *
 * H. Zeng/EAI     	10/99	update for multiple loops of nmap2	*
 * M. Li/GSC       	03/01  	added NMP library                       *
 * E. Safford/GSC	05/01	remove mask[2] 				*
 ***********************************************************************/
{
XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;

    int	ier;
/*---------------------------------------------------------------------*/

    if ( cbs->set == True ) {
	nmp_sovlflg(_lpId, (int)which, TRUE, &ier);

	XtSetSensitive(_ovlBtn[which], True);
    }
    else {
	nmp_sovlflg(_lpId, (int)which, FALSE, &ier);

	XtSetSensitive(_ovlBtn[which], False);
    }
    _changeMade = TRUE;
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_overlayPushbCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * mapw_overlayPushbCb                                                  *
 *                                                                      *
 * Callback function for overlaying push buttons.     			*
 *                                                                      *
 * void mapw_overlayPushbCb(w, which, call)                             *
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        button index                               *
 *  call          XtPointer  callback data struct     			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 * C. Lin/EAI      07/97	modified to use new data structure      *
 * S. Wang/GSC	   09/97	add NxmMarkA_popup()			*
 * H. Zeng/EAI     10/99	update for multiple loops of nmap2	*
 * M. Li/GSC	   03/01   	added NMP library                       *
 * M. Li/SAIC	   02/01	added _changeMade			*
 * T. Piper/SAIC	10/04	added NxmScaleA_popup			*
 ***********************************************************************/
{
char    	title[40];
int		ier, itype;
nmpovlstr_t	ovlnms[MAX_OVL], ovlattr;

/*---------------------------------------------------------------------*/

	nmp_govlnms(ovlnms, &ier);
	nmp_govlattr((int)which, _lpId, &itype, ovlattr, &ier);

	sprintf(title, "%s Attributes", ovlnms[which]);

	_currOvl = (int)which;

	mapw_setLnMk(_lpId, (int)which);  

	switch (itype ) {
	    case 0:
	        NxmLineA_popUp((_NXMattr*)&_line, 
			title, mapw_latlonApply, mapw_createLatlonOpt, 
			mapw_latlonInit);
		break;
	    case 1:
	        NxmLineA_popUp((_NXMattr*)&_line, 
			title, mapw_redrawMap, NULL, NULL);
		break;
	    case 2:
		NxmMarkA_popup(&_mark, mapw_redrawMap );
		break;
	    case 5:
		NxmScaleA_popup(&_scale, mapw_redrawMap );
		break;
	}
	_changeMade = TRUE;
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_showImageCb ( Widget wid, XtPointer clnt, 
					XmToggleButtonCallbackStruct *cbs )
/************************************************************************
 * mapw_showImageCb							*
 *									*
 * Callback function for the show image button.				*
 *									*
 * void mapw_showImageCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	button index				*
 *	*cbs	XmToggleButtonCallbackStruct				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		06/99	initial coding				*
 * E. Safford/GSC  	10/99   dataw_getCurLoop -> loop_getCurLoop     *
 * E. Safford/GSC  	02/02   rm loop_getCurLoop, use _lpId           *
 ***********************************************************************/
{
    _showImage[_lpId] = (Boolean)cbs->set;
    mapw_redrawMap ();
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_canvasExposeCb ( Widget w, XtPointer clnt, 
				XmDrawingAreaCallbackStruct *call )
/************************************************************************
 * mapw_canvasExposeCb                                                  *
 *                                                                      *
 * Callback function for expose event of map editing window drawing     *
 * area.      								*
 *                                                                      *
 * void mapw_canvasExposeCb(w, clnt, call )                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  clnt          XtPointer  not used                                   *
 *  *call    XmDrawingAreaCallbackStruct   callback data struct    	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 ***********************************************************************/
{
XEvent          *event;
/*---------------------------------------------------------------------*/
    event = call->event;
    if ( event->xexpose.count == 0 ) {
	xmexpo(event);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_ctlBtnCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * mapw_ctlBtnCb                                                  	*
 *                                                                      *
 * Callback function for control buttons at the bottom.     		*
 * area.      								*
 *                                                                      *
 * void mapw_ctlBtnCb(w, which, call)                              	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long        which button                               *
 *  call          XtPointer  not used     				*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96                                           *
 * C. Lin/EAI      	02/97  	add fadeflg parm to dataw_loadFrame     *
 * S. Jacobs/NCEP  	06/97  	Added write flag of TRUE to cvg_load	*
 * C. Lin/EAI      	07/97  	Added color of 0 to cvg_load            *
 * C. Lin/EAI      	07/97  	modify to use mapw_ functions 		*
 * C. Lin/EAI      	08/97  	bug fix for Accept case                 *
 * E. Wehner/EAi   	08/97  	Remove display of centroids through cpg	*
 * C. Lin/EAi      	08/97  	Call mapw_setMapstrs() in Load		*
 * G. Krueger/EAI  	09/97  	Changed NxmWarning -> NxmWarn_show	*
 * E. Wehner/EAi   	09/97  	Stopped passing GrInfo record           *
 * D.W.Plummer/NCEP 	 9/97  	Changed &icolor to icolor in to cvg_load*
 * S. Wang/GSC	   	09/97  	remove mapw_restore()			*
 * C. Lin/EAI	   	10/97  	changed product generation functions	*
 * C. Lin/EAI	   	12/97  	dataw_resetMapCopy->dataw_restoreMap	*
 * G. Krueger/EAI  	11/97	Renamed NxmHelp functions               *
 * C. Lin/EAI      	01/98	take out extra cvg_load                 *
 * E. Safford/GSC  	10/99	updated for nmap2			*
 * H. Zeng/EAI  	10/99	update for multiple loops of nmap2	*
 * E. Safford/GSC  	10/99   dataw_getCurLoop -> loop_getCurLoop     *
 * E. Safford/GSC	01/00	add call to loop_setDataChngd    	*
 * E. Safford/GSC	01/00	fix crash on accept w/ no data & pg up 	*
 * E. Safford/GSC	04/00	set data changed flag on zoom		*
 * S. Law/GSC		06/00	removed loop parameter from xpgsvfrm2	*
 * E. Safford/GSC	07/00	always signal a reload for load & accpt *
 * E. Safford/GSC	07/00	use dsp_reloadLoop for Accept           *
 * S. Jacobs/NCEP	 7/00	Added another check for dataw_isUp	*
 * E. Safford/GSC	08/00	restore zoom area on cancel             *
 * M. Li/GSC            11/00   Combined Load and Accept buttons        *
 * M. Li/GSC       	03/01   added NMP library                       *
 * M. Li/GSC		05/01	added nmp_restore			*
 * E. Safford/GSC	07/01	if !dataw_isUp use dataw_loadData only	*
 * E. Safford/GSC	07/01	add call to pgpalw_setupOper()		*
 * M. Li/SAIC		01/02	added loop_restoreRoamVal, and		*
 *				      mapset_settingChngd		*
 * M. Li/SAIC		02/02	Added _changeMade condition		*
 * E. Safford/GSC  	02/02   rm loop_getCurLoop, use _lpId           *
 * H. Zeng/EAI          05/02   changed para list for zoomw_setZoom     *
 ***********************************************************************/
{
    int		ii, ier;
/*---------------------------------------------------------------------*/

    switch (which) {

        case 0:		/* Load  or Accept */

/*
 * set MAPFILE and MAP string
 */
	    for(ii = 0; ii < MAX_LOOP; ii++) {
		if ( ii == _lpId || mapset_settingChngd (ii) ) { 
                    nmp_setmapstr(ii, &ier);
		}
	    }

            mapw_popdown();

            if (_changeMade) loop_setDataChngd (_lpId, TRUE);

/*
 * If map was started from main window, load the data. 
 */
            if ( !dataw_isUp() ) { 
                dataw_loadData(); 
            }

	    _whichMap = _mapSelected;

	    break;

        case 1:     	/* Help */
	    NxmHelp_helpBtnCb(w, 3, NULL);
            break;

	case 2:		/* Cancel */

	    if (!dataw_isUp()) {
		loop_setDataChngd (_lpId, FALSE);
	        nmp_restore (&ier);
		loop_restoreRoamVal();
	    }

	    if ( _zoomActv ) {
		zoomw_setZoom (_lpId); 
	    }
	    
	    mapw_popdown();

	    if ( pgpalw_isUp() ) {
	        pgpalw_setupOper();
	    }
	    break;
    }

    if (!dataw_isUp()) {
	if (which != 1) {
	    mbtnw_zoomSensitive(TRUE);
	    loopw_sensitive(TRUE);
	}
    }
}

/*=====================================================================*/

void mapw_redrawMap ( void )
/************************************************************************
 * mapw_redrawMap                                              		*
 *                                                                      *
 * This function redraws the map in the map canvas area.        	*
 *                                                                      *
 * void mapw_redrawMap()                   				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96  						*
 * C. Lin/EAI      	04/97	call mapw_setMapstrs()  		*
 * C. Lin/EAI      	12/97	modified for RADAR  			*
 * C. Lin/EAI      	04/98	pass image data name in image_loadData  *
 * S. Law/GSC		06/99	added _showImage check			*
 * S. Jacobs/NCEP	10/99	Added nindex to checks for SAT or RAD	*
 * E. Safford/GSC	12/99	add display image			*
 * E. Safford/GSC	02/00	fix incorrect image display bug    	*
 * M. Li/GSC         	03/01   added NMP library                       *
 * E. Safford/GSC	06/01	return line/mark setup (rmvd in cleanup)*
 * A. Hardy/GSC          7/01   added queries and saves for text attr.  *
 * T. Piper/SAIC	12/01	corrected type for ftime		*
 * E. Safford/GSC  	02/02   rm loop_getCurLoop, use _lpId           *
 * J. Wu/SAIC  		08/03   encode correct marker size in attr str	*
 * T. Lee/SAIC		08/03	added time range/interval to nim_plot	*
 * B. Yin/SAIC          03/04   changed css_gtim calling sequences      *
 * T. Lee/SAIC		 7/04	get TIME_MATCH from pref. table		*
 * T. Piper/SAIC	10/04	added scale legend support ( itype=5)	*
 * m.gamazaychikov/SAIC 04/06   change way time matching scheme is set  *
 ***********************************************************************/
{
int 		ignore, drvr_idx, frm, lfrm, title, min, sel, ier;
int     	txt_size_id, ltln_int, which, itype, itime = 1;
int         	jtxfn, jtxhw, jtxwid, jbrdr, jrrotn, jjust;
float       	ssztxt;
Pixmap		pxm;

char 		img_garea[256];

Boolean 	sat, rad;
Boolean 	ovlflg[MAX_OVL];

dattm_t		ctime;
dttms_t 	ftime, selarry[MAX_FRAME];

nmpovlstr_t 	ovlattr, ovlattrDrp;
nmpstr_t   	mapDrp, projDrp, gareaDrp[2];
dsrc_t  *dom;
/*---------------------------------------------------------------------*/

    NxmCursor_setCursor(_mapwCanvasW, CURS_BUSY);
    gclear(&ignore);
 
/*
 * get itype and overlay active flag
 */
    nmp_govlflg(_lpId, ovlflg, &ier);
    nmp_govlattr(_currOvl, _lpId, &itype, ovlattrDrp, &ier);

/*
 * set line/mark attributes
 */
    if (ovlflg[_currOvl]) {
        if (_line.style > 20) _line.style -= 20;

	if (itype == 0 ) {
	    ltln_int = (int)_ltlnstep[_lpId][0];
            sprintf(ovlattr, "%d %d %d %d %d", _line.color, _line.style,
                        _line.width, ltln_int, _lblfreq[_lpId][0] );
        }
        else if (itype == 1 ) {
            sprintf(ovlattr, "%d %d %d",  _line.color, _line.style,
			_line.width);
        }
        else if (itype == 2) {
            ctb_fszfnd(_mark.txt_size, &txt_size_id, &ier);

            sprintf(ovlattr, "%d %d %2.1f %d %d %d", _mark.color, 
			_mark.type_id, _mark.size, _mark.state, 
			txt_size_id, _mark.width);
	}
	else if (itype == 5) {
	    sprintf(ovlattr, "%d %d %d %2.2f %d %s %d %d %2.2f %d", 
			_scale.color, _scale.unit, _scale.lat_opt, 
			_scale.lat, _scale.val_opt, _scale.value_txt, 
			_scale.pos, _scale.font, _scale.size, _scale.style );
	}

        nmp_sovlattr(_lpId, _currOvl, ovlattr, &ier);
    }


    if (_showImage[_lpId]) { 
  	sat = rad = FALSE;

        if ( dataw_isSatSelect(_lpId, &drvr_idx) ) {
	    sat = TRUE;
	}
        else if ( dataw_isRadSelect(_lpId, &drvr_idx) ) {
	    rad = TRUE;
	}

	if (sat || rad) {

	    ftime[0] = '\0';
	
/* 
 *  ftime, or frame time is taken from the timeline if
 *  the dataw window is open, or from the loaded loop 
 *  information.   
 */
	    if ( dataw_isUp() ) {
    		tmln_getSelected(&sel, selarry);
		if (sel > 0) {
		    strcpy (ftime, selarry[sel-1]);
		}
	    }
	    else { 
	        frm = loop_getNumFrames (_lpId);
	        lfrm = (frm > 1)? frm-2 : frm;
	        loop_getFrameTm(_lpId, lfrm, ftime);
	    }

/*
 *  If ftime is still blank, use the current time, 
 *  displaying the latest image.
 */
	    if (ftime[0] == '\0') {
		css_gtim (&itime, ftime, &ier);
	    }


	    nmp_gmapattr(_lpId, mapDrp, projDrp, gareaDrp, &ier);
   	   
	    if (strlen(gareaDrp[1]) > (size_t)0) {
	 	which = 1;
	    } else {
		which = 0;
	    }

	    strcpy(img_garea, gareaDrp[which]);
	   	

	    title = -2;
	    min   = 2880;	/* 2 days */
	    css_gtim (&itime, ctime, &ier); 
	    pxm = 0;
            dom    = (dsrc_t *)dataw_getDomSrc(_lpId);

            gqtext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, 
		        &jrrotn, &jjust, &ier );
	    gstext (&_ifnt, &_ihwsw, &_tsize, &_iwid, &_ibrdr, 
		        &_irot, &_ijust, &ier );

	    nim_plot (pxm, drvr_idx, img_garea, "ALL", ftime, ctime,
	    		min, -1, dom->domtmmtch, 0, title, &ier);
	    gstext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, 
		        &jrrotn, &jjust, &ier );

	}
    }

/*
 * set MAPFILE and MAP string
 */
    nmp_setmapstr(_lpId, &ier);
    gqtext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, 
		        &jrrotn, &jjust, &ier );
    gstext (&_ifnt, &_ihwsw, &_tsize, &_iwid, &_ibrdr, 
		        &_irot, &_ijust, &ier );
    nmp_plot(_lpId, 0, "ALL", &ier);
    gstext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, 
		        &jrrotn, &jjust, &ier );

    if ( (itype == 5) && ( _scale.lat_opt == 0 ) &&
             ( NxmScaleA_isUp() ) ) {  /* Update the latitude value in GUI  */
	NxmScaleA_updtLat ();
    }

    geplot(&ier);

    NxmCursor_setCursor(_mapwCanvasW, CURS_DEFAULT);
}

/*=====================================================================*/

int mapw_isUp ( void )
/************************************************************************
 * mapw_isUp                                              		*
 *                                                                      *
 * This function checks whether the map editing window is up.        	*
 *                                                                      *
 * int mapw_isUp()                   					*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * mapw_isUp	int     1 = yes (up ), 0 = no (down)                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96  						*
 ***********************************************************************/
{
    if ( XtIsManaged(_mapPopW) )
	return (1);
    else
	return(0);
}

/*=====================================================================*/

void mapw_updtOvlBtns ( void )
/************************************************************************
 * mapw_updtOvlBtns                                             	*
 *                                                                      *
 * This function checks the map overlay buttons against the overlay 	*
 * flag in current _mapObj and sets them correctly.        		*
 *                                                                      *
 * void mapw_updtOvlBtns()             					*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	10/96  						*
 * C. Lin/EAI      	 7/97   modify to use _mapOvl struct  		*
 * H. Zeng/EAI     	08/99   change "mask" to "mask[2]" in order to  *
 *                          		allow more overlays             *
 * H. Zeng/EAI	   	10/99   update for multiple loops of nmap2      *
 * M. Li/GSC       	03/01   added NMP library                   	*
 * E. Safford/GSC	05/01	rename from mapw_validateOvlCheckBtns	*
 ***********************************************************************/
{
int 	ii, ovlnum, ier; 
Boolean ovlflg[MAX_OVL];
/*---------------------------------------------------------------------*/

    nmp_govlnum(&ovlnum, &ier);
    nmp_govlflg(_lpId, ovlflg, &ier);

    for ( ii = 0; ii < ovlnum; ii++) {

        if ( ovlflg[ii] ) {
   	    XmToggleButtonSetState(_mapBtn[ii], True, False);
	    XtSetSensitive(_ovlBtn[ii], True);
	}
	else {
	    XmToggleButtonSetState(_mapBtn[ii], False, False);
	    XtSetSensitive(_ovlBtn[ii], False);
	}
    }
}

/*=====================================================================*/

void mapw_updtMapBtns ( int lp )
/************************************************************************
 * mapw_updtMapBtns                                                     *
 *                                                                      *
 * This function sets the map toggle buttons based on the setting in	*
 * the specific loop.   						*
 *                                                                      *
 * void mapw_updtMapBtns(lp)                                            *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp		int	loop index				*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		01/02   					* 
 * M. Li/SAIC           12/02   radio box -> check Box                  *
 ***********************************************************************/
{
int     ii, nn, mapInx, ier;
char	*str;
XmString    xm_str;
nmpstr_t    mapDrp, proj, garea[2];
/*---------------------------------------------------------------------*/

     nmp_gmapnum(&nn, &ier);
     nmp_gmapattr(lp, mapDrp, proj, garea, &ier);

     mapInx = 0;
     for (ii=0; ii < nn; ii++) {
        XtVaGetValues (_mapAreaBtn[ii],
                XmNlabelString,                 &xm_str,
                NULL);
        XmStringGetLtoR ( xm_str, XmFONTLIST_DEFAULT_TAG, &str);
        XmStringFree (xm_str);

        if ( strcmp (str, mapDrp) == 0 ) {
            mapInx = ii;
            XtFree (str);
            break;
        }
        XtFree (str);
    }

    for (ii = 0; ii < _maxmap; ii++ ) {
        if ( ii == mapInx )
            XmToggleButtonSetState(_mapAreaBtn[ii], True, True);
        else
            XmToggleButtonSetState(_mapAreaBtn[ii], False, False);
    }
}

/*=====================================================================*/

Widget mapw_createLatlonOpt ( Widget parent )
/************************************************************************
 * mapw_createLatlonOpt                                                 *
 *                                                                      *
 * This function creates the sub-panel for selecting the latitude and   *
 * longitude overlay intervals.                                         *
 *                                                                      *
 * Widget mapw_createLatlonOpt(parent)                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent   Widget     parent widget ID                                *
 *                                                                      *
 * Output parameters:                                                   *
 * mapw_createLatlonOpt	Widget ID of the highest level bulletin board   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       3/94                                                *
 * C. Lin/EAI       2/95 add comments, clean up                         *
 * C. Lin/EAI      12/95 clean up                                       *
 * C. Lin/EAI       5/96 change lat/lon increment from integer to float *
 * S. Wang/GSC      8/96 adapted from NSAT                              *
 ***********************************************************************/
{
int    i, iret;
char   name[15];
Widget bb, rc, button;
XmString xmstr;
char   *latlons[] = { "1.00", "2.00", "5.00", "10.00", "15.00", 
						"20.00", "30.00"};
char   str[10];
/*---------------------------------------------------------------------*/

        bb = XtVaCreateManagedWidget("bb",
                xmBulletinBoardWidgetClass, parent,
                NULL);

        xmstr = XmStringCreateLocalized("Increments");

         XtVaCreateManagedWidget("LatlonOverlayOptionLb",
                xmLabelWidgetClass,     bb,
                XmNlabelString,         xmstr,
                XmNx,                   25,
                XmNy,                   20,
                NULL);

        XmStringFree(xmstr);

/*
 * create the buttons for frequently used lat/lons
 */
        rc = XtVaCreateManagedWidget("latlonSelectionRc",
                xmRowColumnWidgetClass, bb,
                XmNorientation,         XmHORIZONTAL,
                XmNx,                   135,
                XmNy,                   15,
                XmNnumColumns,          1,
                NULL);

        for ( i = 0; i < (int)XtNumber(latlons); i++ ) {

	    cst_ncpy(str, latlons[i], (int)strlen(latlons[i])-3, &iret);
            button = XtVaCreateManagedWidget(str,
                xmPushButtonWidgetClass,        rc,
                NULL);

            XtAddCallback( button, XmNactivateCallback, 
	    	(XtCallbackProc)mapw_latlonBtnCb, (XtPointer)(latlons[i]) );

        }

        xmstr = XmStringCreateLocalized("Latitude: ");

        XtVaCreateManagedWidget("LatIncLb",
                xmLabelWidgetClass,     bb,
                XmNlabelString,         xmstr,
                XmNx,                   25,
                XmNy,                   85,
                NULL);

        XmStringFree(xmstr);

        sprintf(name, "%5.2f", _latlonInc[0] );

         _latW = XtVaCreateManagedWidget("LatlonInc1",
                xmTextFieldWidgetClass, bb,
                XmNvalue,               name,
                XmNcolumns,             5,
                XmNx,                   105,
                XmNy,                   75,
                NULL);

        XtAddCallback( _latW, XmNactivateCallback, 
		(XtCallbackProc)mapw_latlonTxtCb, (XtPointer) 0 );

        xmstr = XmStringCreateLocalized("Longitude: ");

        XtVaCreateManagedWidget("LonIncLb",
                xmLabelWidgetClass,     bb,
                XmNlabelString,         xmstr,
                XmNx,                   190,
                XmNy,                   85,
                NULL);

        XmStringFree(xmstr);

        sprintf(name, "%5.2f", _latlonInc[1] );
        _lonW = XtVaCreateManagedWidget("LatlonInc2",
                xmTextFieldWidgetClass, bb,
                XmNvalue,               name,
                XmNcolumns,             5,
                XmNx,                   290,
                XmNy,                   75,
                NULL);

        XtAddCallback( _lonW, XmNactivateCallback, 
		(XtCallbackProc)mapw_latlonTxtCb, (XtPointer) 1 );


        return(bb);

}

/*=====================================================================*/
/* ARGSUSED */
void mapw_latlonTxtCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * mapw_latlonTxtCb                                                     *
 *                                                                      *
 * Callback function for map set lat/lon increments.                    *
 *                                                                      *
 * void mapw_latlonTxtCb(w, which, call)                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  w        Widget     parent widget ID                                *
 *  which    long        lat/lon increment                               *
 *  call     XtPointer  not used                                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       3/94                                                *
 * C. Lin/EAI      12/95  clean up                                      *
 * C. Lin/EAI       5/96  atoi -> atof                                  *
 ***********************************************************************/
{
String text;
float  value;
/*---------------------------------------------------------------------*/
        XtVaGetValues(w, XmNvalue, &text, NULL);

        if ( text != NULL ) {
            value = (float)atof(text) ;
            if ( value > 0.1F )
                _latlonInc[which] = value ;

            else {
                sprintf( text, "%5.2f", _latlonInc[which] ) ;
                XtVaSetValues( w, XmNvalue, text, NULL ) ;
            }
	    XtFree(text);
        }
}

/*=====================================================================*/
/* ARGSUSED */
void mapw_latlonBtnCb ( Widget w, String which, XtPointer call )
/************************************************************************
 * mapw_latlonBtnCb                                                     *
 *                                                                      *
 * Callback function for frequently used option buttons of lat/lon      *
 * increment.                                                           *
 *                                                                      *
 * void mapw_latlonBtnCb(w, which, call)                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  w        Widget     parent widget ID                                *
 *  which    String     lat/lon increment                               *
 *  call     XtPointer  not used                                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       3/94                                                *
 * C. Lin/EAI      12/95  clean up                                      *
 ***********************************************************************/
{
/*
 * display the value on the text-input field
 */
    XtVaSetValues( _latW, XmNvalue, which, NULL );
    XtVaSetValues( _lonW, XmNvalue, which, NULL );

/*
 * read the value from the text-input field
 */
    mapw_latlonTxtCb( _latW, 0, NULL );
    mapw_latlonTxtCb( _lonW, 1, NULL );

}

/*=====================================================================*/

void mapw_latlonApply ( void )
/************************************************************************
 * mapw_latlonApply                                                     *
 *                                                                      *
 * Lat/lon specific callback function for Apply button in lat/lon 	*
 * window.								*
 *                                                                      *
 * void mapw_latlonApply()                                		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       9/96                                                *
 * M. Li/GSC	    3/01	added _ltlnstep				*
 * M. Li/SAIC	    2/02	added _changeMade			*
 ***********************************************************************/
{
 /*--------------------------------------------------------------------*/
    mapw_latlonTxtCb( _latW, 0, NULL );
    mapw_latlonTxtCb( _lonW, 1, NULL );

    _ltlnstep[_lpId][0] = _latlonInc[0];
    _ltlnstep[_lpId][1] = _latlonInc[1];


    _changeMade = TRUE;
    mapw_redrawMap();

    return;

}

/*=====================================================================*/

void mapw_latlonInit ( void )
 /***********************************************************************
 * mapw_latlonInit                                                      *
 *                                                                      *
 * This function initializes the latlon increment editing text field.   *
 *                                                                      *
 * void  mapw_latlonInit()                                              *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            9/96                                           *
 * C. Lin/EAI            7/97   use mapw_ function instead of map_      *
 * E. Safford/GSC  	10/99   dataw_getCurLoop -> loop_getCurLoop     *
 * M. Li/GSC             3/01        added _ltlnstep                    *
 * E. Safford/GSC  	02/02   rm loop_getCurLoop, use _lpId           *
 ***********************************************************************/
{
char   str[10];
/*--------------------------------------------------------------------*/

    _latlonInc[0] = _ltlnstep[_lpId][0];
    _latlonInc[1] = _ltlnstep[_lpId][1];

/*
 * display the value on the text-input field
 */
    sprintf(str, "%5.2f", _latlonInc[0]);
    XtVaSetValues( _latW, XmNvalue, str, NULL );
    sprintf(str, "%5.2f", _latlonInc[1]);
    XtVaSetValues( _lonW, XmNvalue, str, NULL );

}

/*=====================================================================*/

void mapw_setLnMk ( int lp, int ovl )
/************************************************************************
 * mapw_setLnMk                                                         *
 *                                                                      *
 * This function sets the line, mark, or scale legend values.		*
 *                                                                      *
 * void mapw_setLnMk(lp, ovl)                                           *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	    loop index					*
 *  ovl         int         overlay index                               *
 *                                                                      *
 * Input/Output parameters:                                             *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		02/01						*
 * J. Wu/SAIC		08/03	recode to allow float number in attr str*
 * T. Piper/SAIC	08/04	Added itype of 5			*
 ***********************************************************************/
{
int     	itype, txt_size_id, ier;
nmpovlstr_t	ovlattr;
/*---------------------------------------------------------------------*/

    nmp_govlattr(ovl, lp, &itype, ovlattr, &ier);

    if ( ier == 0 ) {

	if ( itype < 2 ) {
	    sscanf ( ovlattr, "%d %d %d", &_line.color, &_line.style,
	            &_line.width );
	}
	else if ( itype == 2 ) {  /* Marker type */
	    sscanf ( ovlattr, "%d %d %f %d %d %d",
			&_mark.color, &_mark.type_id, &_mark.size,
			&_mark.state, &txt_size_id, &_mark.width );
	    
/*
 * get txt_size from table fontsz.tbl 
 */
             ctb_fszval(txt_size_id, &(_mark.txt_size), &ier); 
	}
	else if ( itype == 5 ) {
	    sscanf ( ovlattr, "%d %d %d %f %d %s %d %d %f %d", 
			&_scale.color, &_scale.unit, &_scale.lat_opt, 
			&_scale.lat, &_scale.val_opt, _scale.value_txt, 
			&_scale.pos, &_scale.font, &_scale.size, &_scale.style );
	}
    }
}

/*=====================================================================*/

void mapw_ctlBtnUpd ( void )
/************************************************************************
 * mapw_ctlBtnUpd                                                       *
 *                                                                      *
 * This function updates the control buttons.                           *
 *                                                                      *
 * void mapw_ctlBtnUpd()                                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li                11/00   Created                                 *
 ***********************************************************************/
{
    XmString        xmstr;
/*---------------------------------------------------------------------*/

        if (dataw_isUp () ) {
            xmstr = XmStringCreateLocalized("Accept");
        }
        else {
            xmstr = XmStringCreateLocalized("LOAD");
        }

        XtVaSetValues(_ctlBtn[0],
                        XmNlabelString,          xmstr,
                        NULL);
        XmStringFree(xmstr);
 }
