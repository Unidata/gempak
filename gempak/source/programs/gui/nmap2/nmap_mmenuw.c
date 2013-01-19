#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"
#include "nmapprm.h"


#define ENHANCEMENT	 0
#define DWELL_RATE	 1
#define ICON_ON		 2
#define ICON_OFF	 3
#define LOCATOR_EDIT	 4
#define CURSOR_EDIT	 5
#define BAD_FRAME	 6
#define EXTENDEDZOOM_ON	 7
#define EXTENDEDZOOM_OFF 8
#define ZOOM_CORNER 	 9
#define ZOOM_CENTER 	10
#define DRAW_NOAA_LOGO	11
#define DRAW_NWS_LOGO	12 
#define LOOPSTOP_CUR	13
#define LOOPSTOP_END	14
#define ROAMSHARE_ON	15
#define ROAMSHARE_OFF	16
#define LLL_ON		17
#define LLL_OFF		18


static Widget _dwellPopW;	/* dwell rate set pop up */
static int    *_dwellRate;   	/* printer to dwell rates */

static Boolean _extendedZoom;		/* extended zoom option */
static Boolean _cornerZoom;		/* zoom modes option */
static Boolean _loopStop;		/* loop stop option */
static Boolean _roamShare;		/* roam share option */
static Boolean _lllOn;			/* loop layer link option */
static Boolean _exitNMAP = FALSE;	/* if the "EXIT" is initiated */

WidgetList icon_mb;		/* Widget IDs for icon tips buttons */
WidgetList loop_mb;		/* Widget IDs for loop stop buttons */
WidgetList roam_mb;		/* Widget IDs for roam share buttons */
WidgetList zoom_mb;		/* Widget IDs for extended zoom buttons */
WidgetList zoomod_mb;		/* Widget IDs for zoom mode buttons */ 
WidgetList  lll_mb;		/* Widget IDs for loop layer link buttons */

static int    _selLogo = NOAA_LOGO;     /* user selection of logo */

/*
 *  Private Callback Functions
 */
void mmenuw_badFrmCb ( Widget, XtPointer, XtPointer );
void mmenuw_exitCancelCb ( Widget, XtPointer, XtPointer );
void mmenuw_exitCb ( Widget, XtPointer, XtPointer );
void mmenuw_fileCb ( Widget,  long which, XtPointer );
void mmenuw_optionCb ( Widget, long which, XtPointer );

/************************************************************************
 * nmap_mmenuw.c                                                        *
 *									*
 * This module creates the menubar widgets in the main window and       *
 * defines the callback functions for nmap.				*
 *									*
 * CONTENTS:                                                            *
 *	mmenuw_create()		creates the menubar widgets.		*
 *	mmenuw_fileCb()		callback function for FILE.		*
 *	mmenuw_optionCb()	callback function for OPTION.		*
 *	mmenuw_exitCb()		callback function for EXIT.		*
 *	mmenuw_exitNMAPCb 	callback for Ok button upon EXIT.	*
 *	mmenuw_exitCancelCb	callback for Cancel button upon EXIT.	*
 *	mmenuw_badFrmCb()	callback function for Bad Frame.	*
 *	mmenuw_extendedZoomSet()set zoom option				*
 *	mmenuw_zoomModeGet()	Get zoom mode 				*
 *      mmenuw_loopStopGet()    get loop stop value                     *
 *	mmenuw_roamShareGet()	get roam share value			*
 *	mmenuw_lllValGet()	get loop layer link option value	*
 *	mmenuw_getLogoName()	get selected logo's name		*
 *	mmenuw_setExitNMAP()	set _exitNMAP flag			*
 ***********************************************************************/

/*=====================================================================*/

void mmenuw_create ( Widget parent )
/************************************************************************
 * mmenuw_create                                              		*
 *                                                                      *
 * This function creates the menubar of the main window.    		*
 *                                                                      *
 * void mmenuw_create(parent)                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                               *
 *                                                                      *
 * Output parameters:                                                   *
 * 	none.                                                           *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		03/96  						*
 * S. Wang/GSC		05/97	add auto text on/off sub-menu		*
 * C. Lin/EAI		08/97	add roam menu				*
 * G. Krueger/EAI	11/97	Renamed NxmHelp functions		*
 * C. Lin/EAI		08/98	change name for icon tip option		*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL for LINUX		*
 * E. Safford/GSC	12/98	check XmVERSION with XmREVISION		*
 * S. Law/GSC		02/99	added locator options			*
 * E. Safford/GSC	04/99   eliminate return value			*
 * D.W.Plummer/NCEP	 4/99	add Marine and Coastal types to seek	*
 * D.W.Plummer/NCEP  	 5/99   Re-order Anchor and VOR                 *
 * S. Law/GSC		06/99	moved roam menu to mmenuw_roamSetup	*
 * S. Law/GSC		10/99	mmenuw_roamSetup -> roamw_createMenu	*
 * S. Jacobs/NCEP	12/99	Added lat/lon in degrees and minutes	*
 * A. Hardy/GSC         01/00   Added loop for locator menu             *
 * H. Zeng/EAI          01/00   Changed locator to push button          *
 * E. Safford/GSC	05/00	add bad frame option, & defined values  *
 * S. Jacobs/NCEP	 5/00	Added new menu item for the NOAA logo	*
 * H. Zeng/EAI          05/00   Added Cursor push button                *
 * T. Lee/GSC		01/01	Added zoom option			*
 * J. Wu/GSC	  	04/01	change logo-drawing to be loop-specific *
 * J. Wu/GSC	  	05/01	add NWS logo to logo menu   		*
 * T. Piper/SAIC	11/01	freed logolist and opt_mb		*
 * T. Piper/SAIC        02/02   Freed logoOption_menu.label and         *
 *                                              _botOption_menu.label   *
 * T. Piper/SAIC	06/03	added roam share option			*
 * T. Piper/SAIC	06/03	added sensitizing on/off buttons 	*
 * M. Li/SAIC		07/03   Remove _botOption_menu & locator.tbl	*
 * T. Piper/SAIC	03/05	Removed XmVERSION/XmREVISION check	*
 * C. Bailey/HPC        03/05   Added Export to Gif Option in file_menu	*
 * H. Zeng/SAIC		03/07   added loop layer link option		*
 * M. Li/SAIC		12/07   added zoom modes			* 
 ***********************************************************************/
{
    _NXMmenuItem   file_menu[] = {
	{"Data Source", &xmPushButtonGadgetClass, (char)NULL, 
	 "Ctrl<KeyPress>D", "Ctrl + D", (XtCallbackProc)mmenuw_fileCb,
	 0, NULL, NULL},
        {"Export to Gif", &xmPushButtonGadgetClass, (char)NULL,
         (char *)NULL, (char *)NULL, (XtCallbackProc)mmenuw_fileCb, 1,
		 NULL, NULL},
        {NULL, NULL, (char)NULL, (char *)NULL, (char *)NULL, NULL,
		(int)NULL, NULL, NULL}
    };

    static _NXMmenuItem   tipOption_menu[] = {
	{ "On    ", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
	  (char *)NULL, (XtCallbackProc)mmenuw_optionCb, ICON_ON, NULL, NULL},
	{ "Off   ", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
	  (char *)NULL, (XtCallbackProc)mmenuw_optionCb, ICON_OFF, NULL, NULL},
	{NULL, NULL, (char)NULL, (char *)NULL, (char *)NULL, NULL,
	        (int)NULL, NULL, NULL}
    };

    static _NXMmenuItem   zoomOption_menu[] = {
	{ "On    ", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
	  (char *)NULL, (XtCallbackProc)mmenuw_optionCb, EXTENDEDZOOM_ON, 
	  NULL, NULL},
	{ "Off   ", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
	  (char *)NULL, (XtCallbackProc)mmenuw_optionCb, EXTENDEDZOOM_OFF, 
	  NULL, NULL},
        {NULL, NULL, (char)NULL, (char *)NULL, (char *)NULL, NULL,
                (int)NULL, NULL, NULL}
    };

    static _NXMmenuItem   zoomMode_menu[] = {
        { "Corner ", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
          (char *)NULL, (XtCallbackProc)mmenuw_optionCb, ZOOM_CORNER,
          NULL, NULL},
        { "Center ", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
          (char *)NULL, (XtCallbackProc)mmenuw_optionCb, ZOOM_CENTER,
          NULL, NULL},
        {NULL, NULL, (char)NULL, (char *)NULL, (char *)NULL, NULL,
                (int)NULL, NULL, NULL}
    };

    static _NXMmenuItem   loopOption_menu[] = {
        { "Current", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
          (char *)NULL, (XtCallbackProc)mmenuw_optionCb, LOOPSTOP_CUR, NULL, NULL},
        { "End    ", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
           (char *)NULL, (XtCallbackProc)mmenuw_optionCb, LOOPSTOP_END, NULL, NULL},
        {NULL, NULL, (char)NULL, (char *)NULL, (char *)NULL, NULL,
                (int)NULL, NULL, NULL}
      };

    static _NXMmenuItem   roamOption_menu[] = {
        { "On    ", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
          (char *)NULL, (XtCallbackProc)mmenuw_optionCb, ROAMSHARE_ON, NULL, NULL},
        { "Off   ", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
          (char *)NULL, (XtCallbackProc)mmenuw_optionCb, ROAMSHARE_OFF, NULL, NULL},
        {NULL, NULL, (char)NULL, (char *)NULL, (char *)NULL, NULL,
                (int)NULL, NULL, NULL}
    };

    static _NXMmenuItem   logoOption_menu[LOGONUM];

    static _NXMmenuItem   logoNameOption_menu[] = {
	{ "NOAA Logo", &xmCascadeButtonGadgetClass, (char)NULL, (char *)NULL,
	  (char *)NULL, (XtCallbackProc)mmenuw_optionCb, DRAW_NOAA_LOGO, 
	  logoOption_menu, NULL},
	{ "NWS  Logo", &xmCascadeButtonGadgetClass, (char)NULL, (char *)NULL,
	  (char *)NULL, (XtCallbackProc)mmenuw_optionCb, DRAW_NWS_LOGO, 
	  logoOption_menu, NULL},
        {NULL, NULL, (char)NULL, (char *)NULL, (char *)NULL, NULL,
                (int)NULL, NULL, NULL}
    };

    static _NXMmenuItem   lllOption_menu[] = {
	{ "On    ", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
	  (char *)NULL, (XtCallbackProc)mmenuw_optionCb, LLL_ON, NULL, NULL},
	{ "Off   ", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
	  (char *)NULL, (XtCallbackProc)mmenuw_optionCb, LLL_OFF, NULL, NULL},
	{NULL, NULL, (char)NULL, (char *)NULL, (char *)NULL, NULL,
		(int)NULL, NULL, NULL}
    };

    _NXMmenuItem   option_menu[] = {
	{ "Enhancement", &xmPushButtonGadgetClass, (char)NULL, 
	  "Ctrl<KeyPress>E", NULL, (XtCallbackProc)mmenuw_optionCb, 
	  ENHANCEMENT, NULL, NULL},
	{ "Dwell Rate", &xmPushButtonGadgetClass, (char)NULL, NULL,
	  NULL, (XtCallbackProc)mmenuw_optionCb, DWELL_RATE, NULL, NULL},
	{ "Locator", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
	      (char *)NULL, (XtCallbackProc)mmenuw_optionCb, LOCATOR_EDIT,
		      NULL, NULL},
	{ "Cursor", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
	      (char *)NULL, (XtCallbackProc)mmenuw_optionCb, CURSOR_EDIT, 
	      NULL, NULL},
	{ "Bad Frame", &xmPushButtonGadgetClass, (char)NULL, (char *)NULL,
	      (char *)NULL, (XtCallbackProc)mmenuw_optionCb, BAD_FRAME, NULL, NULL},
	{ "Logos", &xmCascadeButtonGadgetClass, (char)NULL, (char *)NULL,
	      (char *)NULL, NULL, (int)NULL, logoNameOption_menu, NULL},
        { "Icon Tips", &xmCascadeButtonGadgetClass, (char)NULL, (char *)NULL,
          (char *)NULL, NULL, (int)NULL, tipOption_menu, NULL},
	{ "Extended Zoom", &xmCascadeButtonGadgetClass, (char)NULL, (char *)NULL,
	  (char *)NULL, NULL, (int)NULL, zoomOption_menu, NULL},
        { "Zoom Mode", &xmCascadeButtonGadgetClass, (char)NULL, (char *)NULL,
          (char *)NULL, NULL, (int)NULL, zoomMode_menu, NULL},
        { "Loop Stop", &xmCascadeButtonGadgetClass, (char)NULL, (char *)NULL,
          (char *)NULL, NULL, (int)NULL, loopOption_menu, NULL},
        { "Roam Share", &xmCascadeButtonGadgetClass, (char)NULL, (char *)NULL,
	  (char *)NULL, NULL, (int)NULL, roamOption_menu, NULL},
        { "Loop layer link", &xmCascadeButtonGadgetClass, (char)NULL, (char *)NULL,
          (char *)NULL, NULL, (int)NULL, lllOption_menu, NULL},
        { NULL, NULL, (char)NULL, (char *)NULL, (char *)NULL, NULL,
                (int)NULL, NULL, NULL}
    };

    int         nlogo, jj, ier;
    Pixel	bg;
    Widget	menubar, button;
    char	*logolist[LOGOLEN], loop_prefs[6], roam_prefs[6];
    String	dialog_name="DwellPanel";
 
    WidgetList	opt_mb; 

/*---------------------------------------------------------------------*/

    menubar = XmCreateMenuBar(parent, "mmenuw_menubar", NULL, 0);
    XtVaGetValues(parent, XmNbackground, &bg, NULL);
    XtVaSetValues(menubar, XmNbackground, bg, NULL);

    /*
     * create File menu
     */
    NxmMenuPulldownBuild(menubar, NULL, "File", 'F', file_menu);

    /*
     * Create Logo-Option menu
     */
    logo_getNames ( &nlogo, logolist );
    for  ( jj = 0; jj < nlogo; jj++ )  {
	logoOption_menu[jj].label = (char *)malloc((strlen(logolist[jj])+1) * sizeof(char));
	strcpy ( logoOption_menu[jj].label, logolist[jj] );
	logoOption_menu[jj].class = &xmPushButtonGadgetClass;
	logoOption_menu[jj].mnemonic = 0;
	logoOption_menu[jj].accelerator = NULL;
	logoOption_menu[jj].accel_text = NULL;
	logoOption_menu[jj].callback = (XtCallbackProc)&logo_selectCb;
	logoOption_menu[jj].which_widget = (long)jj;
	logoOption_menu[jj].subitems = NULL;
	logoOption_menu[jj].sub_buttons = NULL;
	free(logolist[jj]);
    }

    /*
     * create Option menu
     */
    icon_mb = (WidgetList) XtMalloc (XtNumber (tipOption_menu) * sizeof (Widget));
    option_menu[6].sub_buttons = icon_mb;
    zoom_mb = (WidgetList) XtMalloc (XtNumber (zoomOption_menu) * sizeof (Widget));
    option_menu[7].sub_buttons = zoom_mb;
    zoomod_mb = (WidgetList) XtMalloc (XtNumber (zoomMode_menu) * sizeof (Widget));
    option_menu[8].sub_buttons = zoomod_mb;
    loop_mb = (WidgetList) XtMalloc (XtNumber (loopOption_menu) * sizeof (Widget));
    option_menu[9].sub_buttons = loop_mb;
    roam_mb = (WidgetList) XtMalloc (XtNumber (roamOption_menu) * sizeof (Widget));
    option_menu[10].sub_buttons = roam_mb;
    opt_mb = (WidgetList) XtMalloc (XtNumber (option_menu) * sizeof (Widget));
    lll_mb = (WidgetList) XtMalloc (XtNumber (lllOption_menu) * sizeof (Widget));
    option_menu[11].sub_buttons = lll_mb;

    NxmMenuPulldownBuild(menubar, opt_mb, "Option", 'O', option_menu);

    for (jj = 0; jj < nlogo; jj++)  free(logoOption_menu[jj].label);

    /*  
     * Set initial values for icon tips, extended zoom, loop stop, and roam share.
     */
    XtSetSensitive(icon_mb[1], FALSE); 
    NxmBxmBtn_enableLabel(FALSE);
    XtSetSensitive(zoom_mb[0], FALSE);
    _extendedZoom = TRUE;
    XtSetSensitive(zoomod_mb[0], FALSE);
    _cornerZoom = TRUE;
    ctb_pfstr ( "LOOP_STOP", loop_prefs, &ier);    
    if ( strcmp(loop_prefs, "TRUE") == 0 ) {
            XtSetSensitive(loop_mb[0], FALSE);   
            _loopStop = TRUE;
    }
    else {
            XtSetSensitive(loop_mb[1],  FALSE); 
            _loopStop = FALSE;
    }
    ctb_pfstr ( "ROAM_SHR", roam_prefs, &ier);
    if ( strcmp(roam_prefs, "TRUE") == 0 ) {
	XtSetSensitive(roam_mb[0], FALSE);
	_roamShare = TRUE;
    }
    else {
	XtSetSensitive(roam_mb[1],  FALSE);
	_roamShare = FALSE;
    }
    XtSetSensitive(lll_mb[1], FALSE); 
    _lllOn = FALSE;

    XtFree((XtPointer)opt_mb);

    /*
     * create Roam menu
     */
    roamw_createMenu (menubar);

    /*
     * create Exit button
     */
    button = XmCreateCascadeButton(menubar, "Exit", NULL, 0);
    XtVaSetValues(button, XmNbackground, bg, 
			  XmNmnemonic, 'E',
			  NULL);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback,
		  (XtCallbackProc)mmenuw_exitCb, NULL);

    /*
     * create help button
     */
    button = XmCreateCascadeButton(menubar, "Help", NULL, 0);
    XtVaSetValues(button, XmNbackground, bg, 
			XmNmnemonic, 'H',
			NULL);
    XtVaSetValues(menubar, XmNmenuHelpWidget, button, NULL);
    XtAddCallback(button, XmNactivateCallback, (XtCallbackProc)NxmHelp_helpBtnCb,
		  (XtPointer)1);
    XtManageChild(button);

    XtManageChild(menubar);

    /*
     * create dwell rate popup
     */
    _dwellPopW = NxmDwell_popupCreate(parent,dialog_name);
    if ( _dwellPopW == NULL ) {
	printf(" cannot create dwell setting panel.\n");
	exit(0);
    }

    _dwellRate = NxmDwell_getDwellPtr();
    xmloopSetDwellPtr(_dwellRate);
}

/*=====================================================================*/
/* ARGSUSED */
void mmenuw_fileCb ( Widget wdgt, long which, XtPointer call )
/************************************************************************
 * mmenuw_fileCb                                              		*
 *                                                                      *
 * Callback function for FILE button on the menubar of the main window. *
 *                                                                      *
 * void mmenuw_fileCb(wdgt, which, call)                   		*
 *                                                                      *
 * Input parameters:                                                    *
 *  wdgt       Widget     parent widget ID                                 *
 *  which   long        which item                               	*
 *  call    XtPointer  not used						*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96  						*
 ***********************************************************************/
{

	switch( which ) {
		case 0:		/* DATA */
			dataw_popup(); 
			break;
		case 1:
			NxmGif_gifWPopup();
        		break;
	}
}

/*=====================================================================*/
/* ARGSUSED */
void mmenuw_optionCb ( Widget wdgt, long which, XtPointer call )
/************************************************************************
 * mmenuw_optionCb                                              	*
 *                                                                      *
 * Callback function for OPTION menu on the menubar of the main window. *
 *                                                                      *
 * void mmenuw_optionCb(wdgt, which, call)                   		*
 *                                                                      *
 * Input parameters:                                                    *
 *	wdgt	Widget		parent widget ID                        *
 *	which	long		button ID                      		*
 *	call	XtPointer	callback structure (not used)		*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96  						*
 * S. Wang/GSC	   	04/97	add auto text label on/off 		*
 * C. Lin/EAI      	12/97 	change calling to  NxmEnhw_popup() 	*
 * S. Jacobs/NCEP  	10/99	Added variables to checks for SAT & RAD	*
 * E. Safford/GSC	10/99	dataw_getCurLoop -> loop_getCurLoop	*
 * H. Zeng/EAI          01/00   added locator callback                  *
 * E. Safford/GSC	05/00	add bad frame, and defined values       *
 * H. Zeng/EAI          05/00   added cursor  callback                  *
 * H. Zeng/EAI          02/01   added new para. to NxmExit_create()     *
 * J. Wu/GSC		05/01	add DRAW_(NOAA/NWS)_LOGO cases		*
 * H. Zeng/SAIC		01/07	changed arg. list for locfmtw_popup()	*
 * H. Zeng/SAIC		03/07   added loop layer link callback		*
 * M. Li/SAIC		12/07	Added zoom modes			*
 ***********************************************************************/
{
int	loop, nindex;
char	badfrm_title[] = "Bad Frame Confirmation";
char	badfrm_msg[]   = "Do you really want to tag this as a bad frame?";
char	badfrm_err[]   = "The base map cannot be tagged as a bad frame.";
/*---------------------------------------------------------------------*/

    loop = loop_getCurLoop ();
    
    
    switch(which) {

        case ENHANCEMENT:

	    if (dataw_isRadSelect(loop,&nindex)) {
	        NxmEnhw_popup(2);
	    }
	    else  {
	        NxmEnhw_popup(1);
	    }
	    break;

	case DWELL_RATE:  
	    XtManageChild(_dwellPopW);
	    break;

	case ICON_ON:     
            XtSetSensitive(icon_mb[0], FALSE);
            XtSetSensitive(icon_mb[1], TRUE);
	    NxmBxmBtn_enableLabel(1);
	    break;

	case ICON_OFF:     
            XtSetSensitive(icon_mb[0], TRUE);
            XtSetSensitive(icon_mb[1], FALSE);
	    NxmBxmBtn_enableLabel(0);
	    break;

	case LOCATOR_EDIT:  
	    locfmtw_popup ();
	    break;

	case CURSOR_EDIT:
	    cursorw_popup();
	    break;

	case BAD_FRAME:

	    if (loopw_isLoopActv()) {
	        loopw_stop();
	    }

     	    if ( loop_getNumFrames(loop) <= 1 || xmloop_atBlankPxm() ) { 
		NxmWarn_show(mcanvw_getDrawingW(), badfrm_err);
    	    }  
	    else { 
                NxmExit_create(mcanvw_getDrawingW(), badfrm_title, 
			       badfrm_msg, (XtCallbackProc)mmenuw_badFrmCb, NULL ); 
  	    }
	    break;
	
	case EXTENDEDZOOM_ON:
	    XtSetSensitive(zoom_mb[0], FALSE);
            XtSetSensitive(zoom_mb[1], TRUE);
	    _extendedZoom = TRUE;
	    break;

	case EXTENDEDZOOM_OFF:
	    XtSetSensitive(zoom_mb[0], TRUE);
	    XtSetSensitive(zoom_mb[1], FALSE);
	    _extendedZoom = FALSE;
	    break;

        case ZOOM_CORNER:
            XtSetSensitive(zoomod_mb[0], FALSE);
            XtSetSensitive(zoomod_mb[1], TRUE);
            _cornerZoom = TRUE;
            break;

	case ZOOM_CENTER:
            XtSetSensitive(zoomod_mb[0], TRUE);
            XtSetSensitive(zoomod_mb[1], FALSE);
            _cornerZoom = FALSE;
            break;

	case DRAW_NOAA_LOGO:
	    _selLogo = NOAA_LOGO;
	    break;

	case DRAW_NWS_LOGO:
	    _selLogo = NWS_LOGO;
	    break;

        case LOOPSTOP_CUR:
            XtSetSensitive(loop_mb[0], FALSE); 
            XtSetSensitive(loop_mb[1], TRUE);
            _loopStop = TRUE; 
            break;

        case LOOPSTOP_END:
            XtSetSensitive(loop_mb[0], TRUE);
            XtSetSensitive(loop_mb[1], FALSE); 
             _loopStop = FALSE;
            break;

	case ROAMSHARE_ON:
            XtSetSensitive(roam_mb[0], FALSE);
            XtSetSensitive(roam_mb[1], TRUE);
 	    _roamShare = TRUE;
	    break;

	case ROAMSHARE_OFF:
            XtSetSensitive(roam_mb[0], TRUE);
            XtSetSensitive(roam_mb[1], FALSE);
	    _roamShare = FALSE;
	    break;

	case LLL_ON:     
            XtSetSensitive(lll_mb[0], FALSE);
            XtSetSensitive(lll_mb[1], TRUE);
	    _lllOn = TRUE;
	    break;

	case LLL_OFF:     
            XtSetSensitive(lll_mb[0], TRUE);
            XtSetSensitive(lll_mb[1], FALSE);
	    _lllOn = FALSE;
	    break;

    }

}

/*=====================================================================*/
/* ARGSUSED */
void mmenuw_exitCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * mmenuw_exitCb                                              		*
 *                                                                      *
 * Callback function for EXIT button on the menubar of the main window. *
 *                                                                      *
 * void mmenuw_exitCb(wdgt, clnt, call)                			*
 *                                                                      *
 * Input parameters:                                                    *
 *      wdgt    Widget          parent widget ID                        *
 *      clnt    XtPointer       client data (not used)                  *
 *      call    XtPointer       callback structure (not used)           *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96  						*
 * G. Krueger/EAI  09/97	NxmExitDialog->NxmExit_create		*
 * C. Lin/EAI      12/97	add call to cvg_rndef			*
 * C. Lin/EAI      04/98	add product generation checking		*
 * E. Safford/GSC  05/98	add endUndo call               		*
 * E. Safford/GSC  09/98	change pgpalw_isPgen to pgpalw_isUp  	*
 * H. Zeng/EAI     02/01        added new para. to NxmExit_create()     *
 * J. Wu/SAIC      03/02        unify with exiting from layering     	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    
    mmenuw_setExitNMAP ( TRUE );
    
    if ( pglayer_getChngLayer (0) < 0 ) {	
        NxmExit_create(wdgt, "Exit Confirmation", "OK to EXIT from nmap?",
		       mmenuw_exitNMAPCb, mmenuw_exitCancelCb );
    }
    else {
	pglayrxt_popup ();
    }

}

/*=====================================================================*/
/* ARGSUSED */
void mmenuw_exitNMAPCb ( Widget wdgt, XtPointer clnt, XtPointer call)
/************************************************************************
 * mmenuw_exitNMAPCb                         	                        *
 *                                                                      *
 * Callback function for the Ok button on the EXIT window.		*
 *                                                                      *
 * void  mmenuw_exitNMAPCb ( wdgt, clnt, call )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      wdgt    Widget          parent widget ID                        *
 *      clnt    XtPointer       client data (not used)                  *
 *      call    XtPointer       callback structure (not used)           *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Log:                                                                 *
 * H. Zeng/EAI          11/00     changed for the new undo design       *
 * J. Wu/SAIC           03/02     rename & unify with exiting layering	*
 ***********************************************************************/
 {
    Display *dpy;
/*---------------------------------------------------------------------*/
    
    if ( _exitNMAP ) {

	if ( pgpalw_isUp() ) {
	    cvg_rndef();
	}
	
	dpy = XtDisplay(wdgt);
        XFlush(dpy);
        XtCloseDisplay(dpy);

        exit(0);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mmenuw_badFrmCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * mmenuw_badFrmCb                                            		*
 *                                                                      *
 * Callback function for OK button on the bad frame confirmation window.*
 *                                                                      *
 * void mmenuw_badFrmCb(wdgt, clnt, call)              			*
 *                                                                      *
 * Input parameters:                                                    *
 * 	wdgt		Widget		parent widget ID                *
 *  	clnt		XtPointer	client data (not used)		*
 *  	call		XtPointer	callback structure (not used)	*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	05/00	initial coding                       	*
 ***********************************************************************/
{
int	lp, frm;
/*---------------------------------------------------------------------*/

    lp  = loop_getCurLoop();
    frm = loop_getCurFrame();

    xmfrmtg_setFrmTag(lp, frm, TRUE);

    /*
     *  And step to the next frame.
     */
    loopw_frwd();

}

/*=====================================================================*/

void mmenuw_extendedZoomSet ( Boolean *zoomtyp )
/************************************************************************
 * mmenuw_extendedZoomSet						*
 *									*
 * This subroutine sets zoom option type.				*
 *									*
 * void mmenuw_extendedZoomSet ( zoomtyp )				*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*zoomtyp	Boolean		zoom option type		*
 **									*
 * Log:									*
 * T. Lee/GSC		01/01	initial coding				*
 ***********************************************************************/
{
    *zoomtyp = _extendedZoom;
}

/*=====================================================================*/

Boolean mmenuw_zoomModeGet ( void )
/************************************************************************
 * mmenuw_ZoomModeGet                                               	*
 *                                                                      *
 * This subroutine gets zoom mode.                               	*
 *                                                                      *
 * void mmenuw_zoomModeGet ( void)                              	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      mmenuw_zoomModeGet 	Boolean		zoome mode		*
 *						 TRUE  = corner zoom	*
 *						 FALSE = center zoom	*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		12/07	Created					*
 ***********************************************************************/
{
    return ( _cornerZoom ); 
}

/*=====================================================================*/


Boolean mmenuw_loopStopGet ( void )
/************************************************************************
 * mmenuw_loopStopGet                                                   *
 *                                                                      *
 * This subroutine returns the roam share value.                        *
 *                                                                      *
 * Boolean mmenuw_loopStopGet ( void )                                  *
 *                                                                      *
 * Input parameters:    NONE                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * mmenuw_loopStopGet          Boolean         loop stop value          *
 *						TRUE = current frame	*
 *						FALSE = end of loop	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC        06/03   created                                 *
 ***********************************************************************/
{
    return(_loopStop);
}

/*=====================================================================*/

Boolean mmenuw_roamShareGet ( void )
/************************************************************************
 * mmenuw_roamShareGet							*
 * 									*
 * This subroutine returns the roam share value.			*
 *  									*
 * Boolean mmenuw_roamShareGet ( void )					*
 * 									*
 * Input parameters:	NONE                                            *
 * 									*
 * Output parameters:                                                   *
 *									*
 * mmenuw_roamShareGet		Boolean		roam share value	*
 *									*
 **									*
 * Log:									*
 * T. Piper/SAIC	06/03	created					*
 ***********************************************************************/
{
    return(_roamShare);
}

/*=====================================================================*/

int mmenuw_getLogoName ( void )
/************************************************************************
 * mmenuw_getLogoName							*
 *									*
 * This subroutine returns currently-selected logo's name.		*
 *									*
 * int mmenuw_getLogoName ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	 None								*
 *									*
 * return parameters:							*
 *	mmenuw_getLogoName	int	logo name number		*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		5/01	create					*
 ***********************************************************************/
{
    return ( _selLogo );
}

/*=====================================================================*/

Boolean mmenuw_lllValGet ( void )
/************************************************************************
 * mmenuw_lllValGet							*
 *                                                                      *
 * This subroutine returns the lll option value.                        *
 *                                                                      *
 * Boolean mmenuw_lllValGet ( void )					*
 *                                                                      *
 * Input parameters:    NONE                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * mmenuw_lllValGet            Boolean         loop layer link opt. val.*
 *						TRUE =  ON		*
 *						FALSE = OFF		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		03/07	initial coding				*
 ***********************************************************************/
{
    return(_lllOn);
}

/*=====================================================================*/

void mmenuw_setExitNMAP ( Boolean flag )
/************************************************************************
 * mmenuw_setExitNMAP							*
 *									*
 * Sets _exitNMAP2 flag.						*
 *									*
 * void mmenuw_setExitNMAP ( flag )					*
 *									*
 * Input parameters:							*
 *	flag		Boolean		True - Exit NMAP2		*
 *					False - Do not exit NMAP2	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/02	initial coding				*
 ***********************************************************************/
{
    _exitNMAP = flag;
}

/*=====================================================================*/
/* ARGSUSED */
void mmenuw_exitCancelCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * mmenuw_exitCancelCb  	                       	   	     	*
 *                                                                      *
 * Callback function for the cancel button on the EXIT window.		*
 *                                                                      *
 * void  mmenuw_exitCancelCb ( widgt, clnt, call )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	wdgt	Widget		parent widget ID                        *
 *	clnt	XtPointer	client data (not used)                  *
 *	call	XtPointer	callback structure (not used)           *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Log:                                                                 *
 * J. Wu/SAIC          	03/02   initail coding				*
 * E. Safford/SAIC	03/02	add pgpalw_setupOper()			*
 ***********************************************************************/
{
    _exitNMAP = FALSE;
    pgpalw_setupOper();
}
