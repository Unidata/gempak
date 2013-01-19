#include "geminc.h"
#include "gemprm.h"

#include "Nxm.h"
#include "nmap_mainw.h"	      /* MAPW_NAME */
#include "nmap_data.h"
#include "nmapprm.h"


#define MAP		0	/* Map Selection button  */
#define	ZOOM		1	/* Zoomed button	 */
#define	OVERLAY		2	/* Overlays		 */
#define	ROAM		3	/* Roam setting		 */
#define	OPTIONS		4	/* Number of options	 */

static	Widget		_mapsetW, _fromWid, _toWid;
static  Widget		_selLpWid, _allLpWid;
static  WidgetList	_options, _lpOptions, _fromlp;	

static	Boolean		_settingChngd[MAX_LOOP];

/*
 *  Private callback functions
 */
static void mapset_ctlBtnCb ( Widget w, long which, XtPointer call );
static void mapset_loopCb ( Widget w, long which, XtPointer call );

/*
 *  Private functions
 */
static void mapset_options ( Widget parent );
static void mapset_createApplyTo ( Widget parent ); 
static void mapset_createFromLp ( Widget parent );
static void mapset_init ( void );
static void mapset_update ( void );
static void mapset_applyTo ( void );
static void mapset_applyFrom ( void );


/************************************************************************
 * nmap_mapset.c                                                        *
 *                                                                      *
 * This module provides a mean to apply the map and map related settings*
 * from one loop to another.						*
 *                                                                      *
 * CONTENTS:                                                            *
 *	mapset_create()	  creates the 'apply map settings' popup window.*
 *	mapset_init()	  initialize the settings for the options.	*
 *	mapset_popup()	  popup the Apply Map Settings window.		*
 * 	mapset_popdown()  popdown the Apply Map Settings window.	*
 * 	mapset_isUp()	  check if the Apply Map Settings window is up.	*	
 *	mapset_options()  create the options selection.			*
 *	mapset_createApplyTo()create the selections for the target loops*
 *      mapset_createFromLp() create the selections for the source loops*
 *	mapset_ctlBtnCb() callback function for Apply Map Settings.	* 	
 *      mapset_loopCb()   callback function for loop selections         *
 *	mapset_applyTo()  apply the setting to the target loops.	*
 *  	mapset_applyFrom() get the setting from the specific loop.     	*
 *	mapset_settingChngd() return a setting-changed flag.		*
 *	mapset_update()	  update the settings for the loop selections	*
 ***********************************************************************/

/*=====================================================================*/

Widget mapset_create ( Widget parent )
/************************************************************************
 * mapset_create                                              		*
 *                                                                      *
 * This function creates the 'apply map setting' popup window.		*
 *                                                                      *
 * Widget mapset_create(parent)                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent           Widget  parent form widget ID                      *
 * Output parameters:							*
 * Return parameters:							*
 *  mapset_create    Widget  Apply map setting popup window widget ID	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		12/01						*
 * M. Li/SAIC		01/02	Added mapset_init, mapset_createFromLp	*
 ***********************************************************************/
{
Widget      pane, ctlBtn;
XmString    title_string;
char        *btnstr[] = {"Apply", "Cancel"};
/*---------------------------------------------------------------------*/

    _mapsetW = XmCreateFormDialog(parent, "mapset_popup", NULL, 0);

    title_string = XmStringCreateLocalized ("Apply Map Settings");

    XtVaSetValues(_mapsetW,
                  XmNnoResize,          True,
                  XmNdialogTitle,       title_string,
                  NULL);
    XmStringFree (title_string);

    pane = XtVaCreateManagedWidget("prtpane",
                xmPanedWindowWidgetClass,       _mapsetW,
                XmNsashWidth,                   1,
                XmNsashHeight,                  1,
                NULL);

    /*
     * Create the setting and target loops options
     */
    mapset_options(pane);
    mapset_createApplyTo(pane);
    mapset_createFromLp(pane);

    /*
     * create control buttons
     */
    ctlBtn = NxmCtlBtn_create(pane, 1,  "mapset_ctlBtn", XtNumber(btnstr),
                            btnstr, (XtCallbackProc)mapset_ctlBtnCb, NULL);
    XtVaSetValues(ctlBtn, XmNmarginHeight, 15, NULL);
    
    /*
     * Initialize the settings of toggle buttons
     */
     mapset_init();
 
    return(_mapsetW);

}

/*=====================================================================*/

static void mapset_init ( void )
/************************************************************************
 * mapset_init                                                          *
 *                                                                      *
 * This function initialize the settings of the toggle buttons.       	*
 *                                                                      *
 * void mapset_init()                                              	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 ** Log:                                                                *
 * M. Li/SAIC           01/02                                           *
 ***********************************************************************/
{
int ii;
/*---------------------------------------------------------------------*/

    /*
     * Set toggle buttons states in the Map Settings window
     */
    for (ii = 0; ii < OPTIONS; ii++) {
        XmToggleButtonSetState(_options[ii], True, True);
    }
    XmToggleButtonSetState(_allLpWid, True, True);

    /*
     * Initialize _settingChngd flag to False
     */
    for (ii = 0; ii< MAX_LOOP; ii++) {
        _settingChngd[ii] = False;
    }

    XmToggleButtonSetState(_fromlp[0], True, True);
}

/*=====================================================================*/

void mapset_popup ( int func )
/************************************************************************
 * mapset_popup								*
 *                                                                      *
 * This function popups the Apply Map Settings window			*
 *                                                                      *
 * void mapset_popup(func)                                          	*
 *                                                                      *
 * Input parameters:                                                    *
 *	func		int	get or apply map settings		*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 ** Log:                                                                *
 * M. Li/SAIC           12/01                                           *
 * M. Li/SAIC		01/02	Added function id and _fromWid window	*
 ***********************************************************************/
{
XmString  title_string;
/*---------------------------------------------------------------------*/

    mapset_update();

    switch (func) {
	case MAPSET_APPLY:		/* Apply Map settings 	*/	
	    title_string = XmStringCreateLocalized ("Apply Map Settings");
    	    XtVaSetValues(_mapsetW,
                  XmNdialogTitle,       title_string,
                  NULL);
	    XmStringFree (title_string);

	    if (!XtIsManaged(_toWid)) {
		XtManageChild(_toWid);
	    }
	    if (XtIsManaged(_fromWid)) {
		XtUnmanageChild(_fromWid);
	    }
	    break;

	case MAPSET_GET:		/* Get Map settings	*/ 
	    title_string = XmStringCreateLocalized ("Get Map Settings");
            XtVaSetValues(_mapsetW,
                  XmNdialogTitle,       title_string,
                  NULL);
	    XmStringFree (title_string);

            if (!XtIsManaged(_fromWid)) {
                XtManageChild(_fromWid);
            }
            if (XtIsManaged(_toWid)) {
                XtUnmanageChild(_toWid);
            }
            break;
    }

    XtManageChild(_mapsetW);
}
/*=====================================================================*/

void mapset_popdown ( void )
/************************************************************************
 * mapset_popdown                                                       *
 *                                                                      *
 * This function pops down the Apply Map Settings window.               *
 *                                                                      *
 * void mapset_popdown()                                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		12/01						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    XtUnmanageChild(_mapsetW);
}
/*=====================================================================*/

Boolean mapset_isUp ( void )
/************************************************************************
 * mapset_isUp                                                          *
 *                                                                      *
 * This function checks whether the Apply Map Settings window is up.	*
 *                                                                      *
 * Boolean mapset_isUp()                                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	NONE								*
 * Return parameters:							*
 * 	mapset_isUp    	Boolean		True = up, False = down		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           12/01                                           *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

        return(XtIsManaged(_mapsetW));
}
/*=====================================================================*/

static void mapset_options ( Widget parent )
/************************************************************************
 * mapset_options                                                      	*
 *                                                                      *
 * This functions create the options selection for "Apply Map Settings"	*
 *                                                                      *
 * void mapset_options(parent)                                         	*
 *                                                                      *
 * Input parameters:                                                    *
 *      parent          Widget          parent widget ID                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 ** Log:                                                                *
 * M. Li/SAIC		12/01                                           *
 ***********************************************************************/
{
int    	ii, nn;
Widget	frame, pane_fr, rc;
char	*option_list[]  = { "Map Selection (Base GAREA)", "Zoomed Area", 
	                    "Overlays", "Roam Setting" };

/*---------------------------------------------------------------------*/

    frame = XtVaCreateManagedWidget("frameCol",
                 xmFrameWidgetClass, parent,
                 NULL);

    pane_fr = XtVaCreateManagedWidget( "pane_fr",
                xmPanedWindowWidgetClass, frame,
                XmNsashWidth,             1,
                XmNsashHeight,            1,
                XmNx,                     10,
                NULL );

    NxmLabel_createFrameLbl("Options", pane_fr, frame );

    rc = XtVaCreateManagedWidget("SelectRc",
                xmRowColumnWidgetClass, pane_fr,
                XmNorientation,         XmVERTICAL,
                XmNnumColumns,          1,
                NULL);

    nn = XtNumber(option_list);;

    _options = (WidgetList) XtMalloc( (size_t)nn * sizeof(Widget));

    for (ii=0; ii<nn; ii++){

        _options[ii] = XtVaCreateManagedWidget (option_list[ii],
                xmToggleButtonGadgetClass,      rc,
                XmNtraversalOn,                 False,
                XmNtopAttachment,               XmATTACH_FORM,
                NULL);
    }

}

/*=====================================================================*/

static void mapset_createApplyTo ( Widget parent )
/************************************************************************
 * mapset_createApplyTo                                                 *
 *                                                                      *
 * This function creates the selections for "Apply Map Settings"   	*
 *                                                                      *
 * void mapset_createApplyTo(parent)                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *      parent          Widget          parent widget ID                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 ** Log:                                                                *
 * M. Li/SAIC           12/01                                           *
 * M. Li/SAIC		01/02	Added mapset_loopCb, _toWid		*
 * E. Safford/SAIC	04/04	construct loopList dynamically		*
 ***********************************************************************/
{
int     ii;
Widget  pane_fr, radio_box, rc;
char	loopName[10];

/*---------------------------------------------------------------------*/

    _toWid = XtVaCreateManagedWidget("frameCol",
                 xmFrameWidgetClass, parent,
                 NULL);

    pane_fr = XtVaCreateManagedWidget( "pane_fr",
                xmPanedWindowWidgetClass, _toWid,
                XmNsashWidth,             1,
                XmNsashHeight,            1,
                XmNx,                     10,
                NULL );

    NxmLabel_createFrameLbl("Apply To", pane_fr, _toWid);

    radio_box = XmCreateRadioBox( pane_fr, "applyTo", NULL, 0 );

    _allLpWid= XtVaCreateManagedWidget("All Loops",
                xmToggleButtonGadgetClass, radio_box,
                NULL);
    XtAddCallback (_allLpWid, XmNarmCallback,
                       (XtCallbackProc)mapset_loopCb, (XtPointer) 0);
    
    _selLpWid= XtVaCreateManagedWidget("Selected Loops",
                xmToggleButtonGadgetClass, radio_box,
                NULL);

    XtAddCallback (_selLpWid, XmNarmCallback,
                       (XtCallbackProc)mapset_loopCb, (XtPointer) 1);


    XtManageChild(radio_box);

    /* 
     * Create loops options for the 'Selected Loops'
     */
    rc = XtVaCreateManagedWidget("SelectRc",
                xmRowColumnWidgetClass, pane_fr,
		XmNpacking,		XmPACK_COLUMN,
                XmNorientation,         XmVERTICAL,
		XmNnumColumns,          2,
		XmNmarginWidth,		40,
                NULL);

    _lpOptions = (WidgetList) XtMalloc( (size_t)MAX_LOOP * sizeof(Widget));

    for (ii=0; ii<MAX_LOOP; ii++){
        sprintf( loopName, "Loop%d", ii+1 ); 

        _lpOptions[ii] = XtVaCreateManagedWidget (loopName,
                xmToggleButtonGadgetClass,      rc,
                XmNtraversalOn,                 False,
                XmNtopAttachment,               XmATTACH_FORM,
                NULL);
    }
 
}

/*=====================================================================*/

static void mapset_createFromLp ( Widget parent )
/************************************************************************
 * mapset_createFromLp                                                  *
 *                                                                      *
 * This function creates the selections of source loops.                *
 *                                                                      *
 * void mapset_createFromLp (parent)                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *      parent          Widget          parent widget ID                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 ** Log:                                                                *
 * M. Li/SAIC           01/02                                           *
 * E. Safford/SAIC	04/04	construct loopList dynamically		*
 ***********************************************************************/
{
int     ii;
Widget  pane_fr, rc;
char	loopName[10];

/*---------------------------------------------------------------------*/

    _fromWid = XtVaCreateManagedWidget("frameCol",
                 xmFrameWidgetClass, parent,
                 NULL);

    pane_fr = XtVaCreateManagedWidget( "pane_fr",
                xmPanedWindowWidgetClass, _fromWid,
                XmNsashWidth,             1,
                XmNsashHeight,            1,
                XmNx,                     10,
                NULL );

    NxmLabel_createFrameLbl("From Loop", pane_fr, _fromWid );


    /*
     * Create loops options
     */
    rc = XtVaCreateManagedWidget("SelectRc",
                xmRowColumnWidgetClass, pane_fr,
                XmNpacking,             XmPACK_COLUMN,
                XmNorientation,         XmVERTICAL,
		XmNradioBehavior,       True,
                XmNtraversalOn,         False,
                XmNnumColumns,          2,
                XmNmarginWidth,         40,
                NULL);


    _fromlp = (WidgetList) XtMalloc( (size_t)MAX_LOOP * sizeof(Widget));

    for (ii=0; ii<MAX_LOOP; ii++){
        sprintf( loopName, "Loop%d", ii+1 ); 
        _fromlp[ii] = XtVaCreateManagedWidget (loopName,
                xmToggleButtonGadgetClass,      rc,
                NULL);
    }

}

/*=====================================================================*/
/* ARGSUSED */
static void mapset_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * mapset_ctlBtnCb                                                      *
 *                                                                      *
 * Callback function for Apply Map Settings                   		*
 *                                                                      *
 * void mapset_ctlBtnCb(w, which, call)                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *  wid           Widget	widget ID                               *
 *  which         long		button index                            *
 *  call          XtPointer	not used                                *
 *                                                                      *
 * Output parameters:                                                   * 
 * Return parameters:                                                   * 
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		12/01						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    switch (which) {

        case 0:         /* Apply */
	    if (XtIsManaged(_toWid)) {
	        mapset_applyTo();
	    }
	    else if (XtIsManaged(_fromWid)) {
                mapset_applyFrom();
            }

	    mapset_popdown();
            break;

        case 1:         /* Cancel */
	    mapset_popdown();
            break;
    }

}

/*=====================================================================*/
/* ARGSUSED */
static void mapset_loopCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * mapset_loopCb                                                      	*
 *                                                                      *
 * Callback function for loop selections                             	*
 *                                                                      *
 * void mapset_loopCb(w, which, call)                                 	*
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
 * M. Li/SAIC           01/02                                           *
 ***********************************************************************/
{
int	ii, loop;
Boolean	on_flag;
/*---------------------------------------------------------------------*/
    if (which) {
	on_flag = True;
    }
    else {
	on_flag = False;
    }
    
    loop = loop_getCurLoop();
    for (ii = 0; ii< MAX_LOOP; ii++) {
	if (on_flag && ii == loop) {
	    XtSetSensitive(_lpOptions[loop], False);
	}
	else {
            XtSetSensitive(_lpOptions[ii], (int)on_flag);
	}
    }

}

/*=====================================================================*/

static void mapset_applyTo ( void )
/************************************************************************
 * mapset_applyTo							*
 *                                                                      *
 * This function applies the selections to the target loops		*
 *                                                                      *
 * void mapset_applyTo()                                             	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           12/01                                           *
 * M. Li/SAIC		01/02	Renamed function			*
 * M. Li/SAIC		02/02	Set map to Custom if source is img.	*
 * H. Zeng/EAI          05/02   changed para list for zoomw_setZoom     *
 * T. Piper/SAIC	07/03	treat all unremapped images the same	*
 * T. Piper/SAIC	04/05	updated for nmp_smapattr CSC		*
 ***********************************************************************/
{ 
int 	    ii, jj, nn, ier;
int 	    loop, ovlnum, itype, roamVal;
Boolean     changes_made, ovlflg[MAX_OVL], proj_ok;
nmpstr_t    smap, sproj, sgarea[2], tmap, tproj, tgarea[2];
nmpovlstr_t ovlattr[MAX_OVL];

/*---------------------------------------------------------------------*/

    /* 
     * If no selection in the options frame, do nothing
     */
    nn = 0;
    for (ii = 0; ii < OPTIONS; ii++) {
        nn += (int) XmToggleButtonGetState(_options[ii]);
    }

    if (nn == 0) return;

    loop = loop_getCurLoop();

    /*
     * Get map settings
     */
    if( XmToggleButtonGetState(_options[MAP])  || 
	XmToggleButtonGetState(_options[ZOOM]) ) { 
	nmp_gmapattr ( loop, smap, sproj, sgarea, &ier );
	if (dataw_isImgInLoop(loop)) {
	    nmp_gtruattr(loop, sproj, sgarea, &ier);
	}
    }

    /* 
     * If sproj contains "SAT", as it does for some unremapped images,
     * it can not be applied to other loops.  Flag this condition.
     */
    proj_ok = TRUE;
    if ( strstr(sproj, "SAT") != (char *)NULL ) {
	proj_ok = FALSE;
    }

    /*
     * Get overlay attributes 
     */
    if(XmToggleButtonGetState(_options[OVERLAY])) {
	nmp_govlflg(loop, ovlflg, &ier);
	nmp_govlnum(&ovlnum, &ier);
	
	for ( ii = 0; ii < ovlnum; ii++ ) {
	    if (ovlflg[ii]) {
		nmp_govlattr(ii, loop, &itype, ovlattr[ii], &ier);
	    }
	}
    }

    /*
     * Get the roam value
     */
    if(XmToggleButtonGetState(_options[ROAM])) {
	roamVal = loop_getRoamVal(loop);
    }
  
    /*
     * Process all the target loops
     */
    for (ii = 0; ii < MAX_LOOP; ii++) {
        if ( XmToggleButtonGetState(_allLpWid) ||
	    (XmToggleButtonGetState(_selLpWid) &&
	     XmToggleButtonGetState(_lpOptions[ii])) ) {	

	    if (loop == ii) continue;
	    changes_made = False;

	    /* 
	     * Set map
	     */
	    if (XmToggleButtonGetState(_options[MAP])) {

		/*
		 * If image in target loop, apply source garea only
		 */
		nmp_gmapattr ( ii, tmap, tproj, tgarea, &ier );
		if (dataw_isImgInLoop(ii)) {
		    nmp_smapattr(ii, tmap, tproj, sgarea, False, &ier);
		}
		else {
		    /*
		     * No image in target loop.  Set map to "Custom", 
		     * apply source garea and appropriate projection.
		     */  
		    if (dataw_isImgInLoop(loop)) {  /* Image in source loop */
			if ( proj_ok ) {  /* Remapped image in source loop, use its projection */
			    nmp_smapattr(ii, "Custom", sproj, sgarea, False, &ier);
			}
		        else {  /* Unremapped image in source loop, use target loop projection */
			    nmp_smapattr(ii, "Custom", tproj, sgarea, False, &ier);
			}
		    }
		    else {  /*  No image in source or target loop, use all source info */
			nmp_smapattr(ii, smap, sproj, sgarea, False, &ier);
		    }
		}

		changes_made = True;
	    }

	    /*
	     * Set zoomed area
	     */

	    if (XmToggleButtonGetState(_options[ZOOM])) {
		nmp_szoom ( ii, sgarea[1], &ier );
		zoomw_setZoom (ii);
		changes_made = True;
	    }

	    /*
	     * Set overlays
	     */

	    if (XmToggleButtonGetState(_options[OVERLAY])) {
		for ( jj = 0; jj < ovlnum; jj++ ) {
		    nmp_sovlflg(ii, jj, ovlflg[jj], &ier);
		
		    if (ovlflg[jj]) {
			nmp_sovlattr(ii, jj, ovlattr[jj], &ier);
		    }
		}
	
		changes_made = True;
	    }

	    /* 
	     * Set roam settings
	     */
	    if (XmToggleButtonGetState(_options[ROAM])) {
		loop_setRoamVal(ii, roamVal);
		changes_made = True;
	    }

	    _settingChngd[ii] = changes_made;

	    if (changes_made) {
		loop_setDataChngd(ii, TRUE);
	    }

	} 
    }

}

/*=====================================================================*/

static void mapset_applyFrom ( void )
/************************************************************************
 * mapset_applyFrom                                                     *
 *                                                                      *
 * This function gets the selected settings from the specific loop	*
 * and set the current loop accordingly.				*
 *                                                                      *
 * void mapset_applyFrom()                                              *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           01/02                                           *
 * M. Li/SAIC           02/02   Set map to Custom if source is img.     *
 * H. Zeng/EAI          05/02   changed para list for zoomw_setZoom     *
 * T. Piper/SAIC        07/03   treat all unremapped images the same    *
 * T. Piper/SAIC	04/05	updated for nmp_smapattr CSC		*
 ***********************************************************************/
{
int         ii, jj, nn, ier;
int         loop, cur_lp, ovlnum, itype, roamVal;
Boolean     changes_made, ovlflg[MAX_OVL], proj_ok;
nmpstr_t    smap, sproj, sgarea[2], tmap, tproj, tgarea[2];
nmpovlstr_t ovlattr[MAX_OVL];

/*---------------------------------------------------------------------*/
/*
 * If no selection in the options frame, do nothing
 */
    nn = 0;
    for (ii = 0; ii < OPTIONS; ii++) {
        nn += (int) XmToggleButtonGetState(_options[ii]);
    }

    if (nn == 0) return;

/*
 * Search the source loop
 */
    for (ii = 0; ii < MAX_LOOP; ii++) {
	if (XmToggleButtonGetState(_fromlp[ii])) {
	    loop = ii;
	    break;
	}
    }

/*
 * Get map settings
 */
    if( XmToggleButtonGetState(_options[MAP])  ||
	XmToggleButtonGetState(_options[ZOOM]) ) {
	nmp_gmapattr ( loop, smap, sproj, sgarea, &ier );
	if (dataw_isImgInLoop(loop)) {
	    nmp_gtruattr(loop, sproj, sgarea, &ier);
        }
    }

/*
 * If proj contains "SAT", as it does for some unremapped images,
 * it can not be applied to other loops.  Flag this condition.
 */
    proj_ok = TRUE;
    if ( strstr(sproj, "SAT") != (char *)NULL ) {
        proj_ok = FALSE;
    }

/*
 * Get overlay attributes
 */
    if(XmToggleButtonGetState(_options[OVERLAY])) {
        nmp_govlflg(loop, ovlflg, &ier);
        nmp_govlnum(&ovlnum, &ier);

	for ( ii = 0; ii < ovlnum; ii++ ) {
	    if (ovlflg[ii]) {
	        nmp_govlattr(ii, loop, &itype, ovlattr[ii], &ier);
	    }
	}
    }

/*
 * Get the roam value
 */
    if(XmToggleButtonGetState(_options[ROAM])) {
        roamVal = loop_getRoamVal(loop);
    }

/*
 * Set the current loop 
 */
    changes_made = False;
    cur_lp = loop_getCurLoop();

/*
 * Set map
 */
    if (XmToggleButtonGetState(_options[MAP])) {

/*
 * If image in current loop, apply source garea only
 */
	nmp_gmapattr ( cur_lp, tmap, tproj, tgarea, &ier );
	if (dataw_isImgInLoop(cur_lp)) {
	    nmp_smapattr(cur_lp, tmap, tproj, sgarea, False, &ier);
	}
	else {
/*
 * No image in current loop.  Set map to "Custom",
 * apply source garea and appropriate projection.
 */
	    if (dataw_isImgInLoop(loop)) {  /* Image in source loop */
	        if ( proj_ok ) {  /* Remapped image in source loop, use its projection */
	            nmp_smapattr(cur_lp, "Custom", sproj, sgarea, False, &ier);
	        }
	        else {  /* Unremapped image in source loop, use target loop projection */
	            nmp_smapattr(cur_lp, "Custom", tproj, sgarea, False, &ier);
	        }
	    }
	    else {  /*  No image in source or target loop, use all source info */
	        nmp_smapattr(cur_lp, smap, sproj, sgarea, False, &ier);
	    }
	}

        changes_made = True;
    }

/*
 * Set zoomed area
 */
    if (XmToggleButtonGetState(_options[ZOOM])) {
        nmp_szoom ( cur_lp, sgarea[1], &ier );
	zoomw_setZoom (cur_lp);
        changes_made = True;
    }

/*
 * Set overlays
 */
    if (XmToggleButtonGetState(_options[OVERLAY])) {
        for ( jj = 0; jj < ovlnum; jj++ ) {
	    nmp_sovlflg(cur_lp, jj, ovlflg[jj], &ier);

	    if (ovlflg[jj]) {
		nmp_sovlattr(cur_lp, jj, ovlattr[jj], &ier);
	    }
        }

	changes_made = True;
    }

/*
 * Set roam settings
 */
    if (XmToggleButtonGetState(_options[ROAM])) {
        loop_setRoamVal(cur_lp, roamVal);
        changes_made = True;
    }

    _settingChngd[cur_lp] = changes_made;

    if (changes_made) {
/*
 * Set toggle buttons for the map and overlays on the map window
 */
	mapw_updtMapBtns(loop);
	mapw_updtOvlBtns();

	loop_setDataChngd(cur_lp, TRUE);
	
/*
 * Draw map and overlays with new changes on the map window
 */
	gclear(&ier);
	nmp_setmapstr(cur_lp, &ier);
	nmp_plot(cur_lp, 0, "ALL", &ier);
	geplot(&ier);
    }
}

/*=====================================================================*/

Boolean mapset_settingChngd ( int loop )
/************************************************************************
 * mapset_settingChngd							*
 *                                                                      *
 * This function returns a setting-changed flag for a specific loop.	*
 *                                                                      *
 * Boolean mapset_settingChngd(loop)                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	loop		int	loop index				*
 * Output parameters:  		                                        *
 * Return parameters:                                                   *
 *  	mapset_settingChngd   	Boolean	    Value of setting changed	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           01/02                                           *
 ***********************************************************************/
{
    return _settingChngd[loop];
}

/*=====================================================================*/

static void mapset_update ( void )               
/************************************************************************
 * mapset_update                                                        *
 *                                                                      *
 * This function updates the settings of the loop selection toggles.    *
 *                                                                      *
 * void mapset_update()                                       		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:   	                                        *
 * Return parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           01/02                                           *
 ***********************************************************************/
{
int     ii, loop;
Boolean lp_flag;
/*---------------------------------------------------------------------*/
    loop = loop_getCurLoop();

    /*
     * Inactivate the loop choices if "Apply to" all loops
     */
    if (XmToggleButtonGetState(_allLpWid)) {
        for (ii = 0; ii< MAX_LOOP; ii++) {
            XtSetSensitive(_lpOptions[ii], False);
        }
    }
    /*
     * Grey out current loop
     */
    else if (XmToggleButtonGetState(_selLpWid)) {
        for (ii = 0; ii< MAX_LOOP; ii++) {
            if (XmToggleButtonGetState(_lpOptions[ii])) {
                XmToggleButtonSetState(_lpOptions[ii], False, False);
            }

            if (ii == loop) {
                lp_flag = False;
            }
            else {
                lp_flag = True;
            }

            XtSetSensitive(_lpOptions[ii], (int)lp_flag);
        }
    }

    /*
     * make current loop insensitive in the From Loop window
     */
    for(ii = 0; ii< MAX_LOOP; ii++) {
	if (ii == loop) {
            lp_flag = False;
        }
        else {
            lp_flag = True;
        }

        XtSetSensitive(_fromlp[ii], (int)lp_flag);
    }
}
