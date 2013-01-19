#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"

#define ICON_DIR "$NAWIPS/icons/nmap"

static Widget _pageW;	    /* label widget to display current page(frame)*/
static Widget _timeW;	    /* label widget to display current frame time */
static Widget _latlonW;     /* label widget to display lat/lon */
static Widget _fadeScale;   /* scale bar for fade effect */
static Widget _hintW;	    /* gives hint of what to do next */
static Widget _pgfileW;     /* product generation file name */

static XmFontList  _fontList; /* fontlist for characters in label strings */
static Widget   _topW;      /* TopLevel widget */
static Widget	_mainw_form;
static Widget   _drawW;	    /* Main Drawing Area widget */
static Widget  *_locDsplWid, *_locDsplLbl;  /* multiple loc. display
					       overriding widgets &
					       labels.		          */
static int    _posNum;	    /* total number of locator positions          */

static int _nfadeC, _fadeRed[256], _fadeGreen[256], _fadeBlue[256];
		            /* # of fade colors and its RGB value */
static Boolean _fadeAway = FALSE;  /* image status, displayed or faded */

static char _hintAction[30], _hintClass[30];
static char _hintMouseL[30], _hintMouseM[30];

static Boolean	_loadingLoop = FALSE;
static int	_activeLoop  = 0;

/*
 *  private callback functions
 */
static void mbotw_fadeBtnCb   ( Widget, long, XtPointer );
static void mbotw_fadeScaleCb ( Widget, XtPointer, XtPointer );
static void mbotw_locOptionCb (	Widget, XtPointer, XtPointer );

/************************************************************************
 * nmap_mbotw.c                                                         *
 *                                                                      *
 * This module creates the widgets at the bottom frame of the main      *
 * window and defines the callback functions for nmap.			*
 *                                                                      *
 * CONTENTS:                                                            *
 *      mbotw_create()      creates the bottom frame.           	*
 *                                                                      *
 *      mbotw_fadeBtnCb()    callback for image fade buttons.	        *
 *      mbotw_fadeScaleCb()  callback for image fade scale.	        *
 *	mbotw_locOptionCb()  callback for locator options		*
 *                                                                      *
 *      mbotw_pageSet()      set the frame (page) number.	        *
 *      mbotw_loadingLoopSet() set the _loadingLoop flag and activeLoop	*
 *	mbotw_startLocDspl()   creates widgets for multi. loc. display	*
 *      mbotw_updateLocDspl()  set the lat/lon info.	        	*
 *      mbotw_endLocDspl()     clear the lat/lon info area.	        *
 *      mbotw_getFadeColor() get the image color indices for fading.	*
 *	mbotw_reloadFade ()  reload fade indexes from color bank	*
 *	mbotw_hintRedraw()   update the hints display.			*
 *	mbotw_disabledSet()  hint that product generation is disabled.	*
 *	mbotw_enabledSet()   hint that product generation is enabled.	*
 *	mbotw_actionSet()    set the action hint message.		*
 *	mbotw_classSet()     set the class hint message.		*
 *	mbotw_mouseSet()     set the mouse hint messages.		*
 *	mbotw_actionClear()  clear the action hint message.		*
 *	mbotw_classClear()   clear the class hint message.		*
 *	mbotw_mouseClear()   clear the mouse hint messages.		*
 *	mbotw_pgfileSet()    set the product generation file name.	*
 *	mbotw_pgfileClear()  clear the product generation file name.	*
 *									*
 *	mbotw_fadeReset()    fade reset wrapper function		*
 *      mbotw_fadeToggle()   fade toggle wrapper function         	*
 *      mbotw_setFade()      set the fade level 			*
 *      mbotw_restoreFade()  restore fade setting for a given loop	*
 ***********************************************************************/

/*=====================================================================*/

Widget mbotw_create ( Widget parent )
/************************************************************************
 * mbotw_create                                              		*
 *                                                                      *
 * This function creates the bottom area for the main window.    	*
 *                                                                      *
 * Widget mbotw_create ( parent )                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                               *
 *                                                                      *
 * Output parameters:                                                   *
 * mbotw_create	Widget	Frame container widget ID                       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI	04/96  							*
 * C. Lin/EAI	12/97	add VGF file name  				*
 * C. Lin/EAI	06/98	add frame time info  				*
 * C. Lin/EAI	07/98	adjust positions of the bottom widgets  	*
 * W. Li /EAI	04/99	add locator stype button at bottom of main win. *
 * D.W.Plummer/NCEP	 4/99	add Marine and Coastal types		*
 * D.W.Plummer/NCEP	 5/99	Re-order Anchor and VOR			*
 * S. Jacobs/NCEP	12/99	Added lat/lon in degrees and minutes	*
 * A. Hardy/GSC		01/00   Added file open for location types      *
 * H. Zeng/EAI          02/00   Added edit button into option menu      *
 * A. Hardy/GSC         02/00   Increased the width of the locator      *
 * S. Law/GSC		05/00	tightened up a bit			*
 * S. Law/GSC		05/00	returned latlon boxes to previous size	*
 * H. Zeng/EAI          05/00   added separator before edit button      *
 * T. Piper/SAIC	11/01	freed opt_mb				*
 * T. Piper/SAIC	02/02	freed option_menu.label			*
 * S. Chiswell/Unidata	 9/02	Added check for depth for fade callback	*
 * R. Tian/SAIC    01/03        add True flag to NxmBxmBtn_create(Multi)*
 * T. Piper/SAIC	07/03	removed xwcmn.h, added XtDisplay()	*
 * M. Li/SAIC		07/03	Changed the width of _hintW		*
 * M. Li/SAIC		07/03	Allow to accept unlimit entries in 	*
 *				table "locator.tbl"			*
 * E. Safford/SAIC	02/04	use mcanvw_getDpth			*
 * T. Piper/SAIC	03/05	Removed XmVERSION/XmREVISION check	*
 * T. Piper/SAIC	10/05	declared ii long			*
 * H. Zeng/SAIC		01/07	removed locator table reading		*
 ***********************************************************************/
{
    int		jj, iret, hoff = 5, num_loc;
    long	ii, ignore;
    char	iconf[256], iconfile[256];
    char	*fadebtns[] = {"fadeoff", "fadehalf"};
    char	*fade_label[] = {"fade out image", "no fade"};

    char	fontname[] = 
                     "-adobe-courier-bold-r-normal-*-*-120-*-*-m-*-*-*";
    Display	*dsp;
    XmFontListEntry flentry;

    Widget	frame, form, txtframe, button, rc1;
    Widget	pageframe, menubarw, button_id;
    WidgetList	opt_mb;
    XmString	xmstr;

    static char	text_label[2][30];
    static _NXMmenuItem *option_menu;
/*---------------------------------------------------------------------*/
    _mainw_form = parent;
/*
 *  Get total number of locator types.
 */
    num_loc = locfmtw_getLocNum();

    option_menu =
        (_NXMmenuItem *) XtMalloc ((size_t)(num_loc + 3) * sizeof (_NXMmenuItem));
    for ( ii = 0; ii < num_loc; ii++ ) {
	option_menu[ii].label = 
	  (char *) malloc ((strlen (locfmtw_getLocStr(ii)) + 1) * sizeof (char));
	strcpy ( option_menu[ii].label, locfmtw_getLocStr(ii));
	option_menu[ii].class =  &xmPushButtonGadgetClass ;
	option_menu[ii].mnemonic = 0;
	option_menu[ii].accelerator = NULL;
	option_menu[ii].accel_text = NULL;
	option_menu[ii].callback = &mbotw_locOptionCb ;
	option_menu[ii].which_widget = (long)ii;
	option_menu[ii].subitems = NULL ;
	option_menu[ii].sub_buttons =  NULL ;
    }

/*
 *  Add separator into option_menu.
 */
    option_menu[ii].label = (char *)malloc((strlen("Separator")+1) 
                                           * sizeof(char));
    strcpy ( option_menu[ii].label, "Separator" );
    option_menu[ii].class =  &xmSeparatorGadgetClass ;
    option_menu[ii].mnemonic = 0;
    option_menu[ii].accelerator = NULL;
    option_menu[ii].accel_text = NULL;
    option_menu[ii].callback = NULL;
    option_menu[ii].which_widget = (long)ii;
    option_menu[ii].subitems = NULL ;
    option_menu[ii].sub_buttons =  NULL ;

/*
 *  Add Edit button into option_menu.
 */
    ii++;
    option_menu[ii].label = (char *)malloc((strlen("EDIT...")+1) 
                                           * sizeof(char));
    strcpy ( option_menu[ii].label, "EDIT..." );
    option_menu[ii].class =  &xmPushButtonGadgetClass ;
    option_menu[ii].mnemonic = 0;
    option_menu[ii].accelerator = NULL;
    option_menu[ii].accel_text = NULL;
    option_menu[ii].callback = &mbotw_locOptionCb ;
    option_menu[ii].which_widget = (long)ii;
    option_menu[ii].subitems = NULL ;
    option_menu[ii].sub_buttons =  NULL ;

    ii++;
    option_menu[ii].label = NULL; 

/*---------------------------------------------------------------------*/

    frame = XtVaCreateWidget("mbotw_frame",
			     xmFrameWidgetClass,  parent,
			     XmNleftAttachment,   XmATTACH_FORM,
			     XmNrightAttachment,  XmATTACH_FORM,
			     XmNbottomAttachment, XmATTACH_FORM,
			     NULL);

    form = XtVaCreateWidget("mbotw_form",
			    xmFormWidgetClass, frame,
			    NULL);

/*
 *  Create page number widget
 */
    pageframe = XtVaCreateManagedWidget("mbotw_pageFrame",
					xmFrameWidgetClass,  form,
					XmNtopAttachment,    XmATTACH_FORM,
					XmNleftAttachment,   XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNshadowType,       XmSHADOW_IN,
					NULL);

    _pageW = XtVaCreateManagedWidget("mbotw_pageLabel",
				     xmLabelWidgetClass,  pageframe,
				     XmNrecomputeSize,    False,
				     XmNwidth,            60,
				     NULL);

/*
 *  Create date/time label
 */
    _timeW = XtVaCreateManagedWidget("mbotw_timeLabel",
				     xmLabelWidgetClass,  form,
				     XmNrecomputeSize,    False,
				     XmNleftAttachment,   XmATTACH_WIDGET,
				     XmNleftWidget,       pageframe,
				     XmNleftOffset,       hoff,
				     XmNbottomAttachment, XmATTACH_FORM,
				     XmNbottomOffset,     6,
				     XmNwidth,            140,
				     XmNalignment,	XmALIGNMENT_BEGINNING,
				     NULL);

    mbotw_pageSet(1, 1);

/*
 *  Create product generation file name label
 */
    xmstr = XmStringCreateLocalized("");
    _pgfileW = XtVaCreateManagedWidget("mbotw_pgfileLabel",
				       xmLabelWidgetClass,  form,
				       XmNlabelString,      xmstr,
				       XmNrecomputeSize,    False,
				       XmNtopAttachment,    XmATTACH_FORM,
				       XmNbottomAttachment, XmATTACH_FORM,
				       XmNleftAttachment,   XmATTACH_WIDGET,
				       XmNleftWidget,       _timeW, 
				       XmNleftOffset,       hoff,
				       XmNwidth,            225,
				       XmNalignment,	XmALIGNMENT_BEGINNING,
				       NULL);
    XmStringFree(xmstr);

/*
 *  Create popup menu for locator options
 */
    menubarw = XmCreateMenuBar(form, "menubarw", NULL, 0);

    XtVaSetValues(menubarw,
		  XmNtopAttachment,    	XmATTACH_FORM,
		  XmNrightAttachment,   XmATTACH_FORM,
		  XmNbottomAttachment, 	XmATTACH_FORM,
		  XmNmarginHeight,	0,
		  XmNmarginWidth,	0,
		  XmNborderWidth,	0,
		  NULL);

    opt_mb = (WidgetList)XtMalloc((num_loc+3) *sizeof(Widget));

    button_id = NxmMenuPulldownBuild(menubarw, opt_mb, "", 
				     0, option_menu);	

    XtManageChild(menubarw);

    for (jj = 0; jj < ii; jj++)  free(option_menu[jj].label);
    XtFree((XtPointer)option_menu);

    sprintf(iconf, "%s.xbm", "locator");
    cfl_inqr(iconf, ICON_DIR, &ignore, iconfile, &iret);

    NxmBxmBtn_addBxmLabel(button_id, 20, 20,
			  "white", "blue", iconfile, "Locator");

    XtVaSetValues(button_id,
		  XmNmarginHeight,	0,
		  XmNmarginWidth,	0,
		  XmNborderWidth,	0,
		  NULL);

    XtFree((XtPointer)opt_mb);

/*
 *  Create location info display widget.
 */
    txtframe = XtVaCreateManagedWidget("mbotw_latlonFrame",
				       xmFrameWidgetClass,	form,
				       XmNshadowType,		XmSHADOW_IN,
				       XmNtopAttachment,    	XmATTACH_FORM,
				       XmNbottomAttachment, 	XmATTACH_FORM,
				       XmNrightAttachment,   	XmATTACH_WIDGET,
				       XmNrightWidget,		menubarw,
				       XmNrightOffset,		hoff,
				       NULL);

    dsp = XtDisplay (txtframe);
    flentry = XmFontListEntryLoad (dsp, fontname, XmFONT_IS_FONT, "TAG1");
    _fontList = XmFontListAppendEntry (NULL, flentry);
    XmFontListEntryFree(&flentry);

    _latlonW = XtVaCreateManagedWidget(" ",
			 xmLabelWidgetClass,  	txtframe,
			 XmNfontList,	        _fontList,
			 XmNrecomputeSize,    	False,
			 XmNwidth,    	     	130,
			 NULL);

/*
 *  Create error pushbutton
 */
    button = NxmErr_btCreate(form);

    XtVaSetValues(button,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNbottomOffset,     3,
		  XmNrightAttachment,  XmATTACH_WIDGET,
		  XmNrightWidget,      txtframe,
		  XmNrightOffset,      hoff,
		  NULL);

/*
 *  Create error message popup
 */
    NxmErr_createPopup(parent);

/*
 *  Create fade scale
 */
    rc1 = XtVaCreateWidget( "mbotw_scaleRc",
			    xmRowColumnWidgetClass, form,
			    XmNorientation,         XmHORIZONTAL,
			    XmNrightAttachment,     XmATTACH_WIDGET,
			    XmNbottomAttachment,    XmATTACH_FORM,
			    XmNrightWidget,         button,
			    XmNrightOffset,         hoff,
			    NULL);
    XtVaCreateManagedWidget( "Fade: ",
			     xmLabelGadgetClass,     rc1,
			     NULL);
    for ( ii = 0; ii < (long)XtNumber(fadebtns); ii++ ) {
	sprintf(iconf, "%s.xbm", fadebtns[ii]);
	cfl_inqr(iconf, ICON_DIR, &ignore, iconfile, &iret);
	
	strcpy( text_label[ii], fade_label[ii]);

	NxmBxmBtn_create(rc1, "fade", NULL, 15, 15,
			 "white", "blue", NULL, iconfile, text_label[ii], True,
                         (XtCallbackProc)mbotw_fadeBtnCb, (XtPointer)ii);
    }

/*
 *  Set the xdpth for slider behavior. For non-8 bit, only change fader
 *  after setting value.
 */

    _fadeScale = XtVaCreateManagedWidget( "mbtnw_fadeScale",
					  xmScaleWidgetClass,     rc1,
					  XmNorientation,         XmHORIZONTAL,
					  XmNprocessingDirection, XmMAX_ON_RIGHT,
					  XmNshowValue,           False,
					  XmNminimum,             0,
					  XmNmaximum,             100,
					  XmNvalue,               50,
					  XmNwidth,               80,
					  NULL);
    if ( mcanvw_getDpth() == _8_BIT ) XtAddCallback(_fadeScale, XmNdragCallback,
		  mbotw_fadeScaleCb, NULL);
    XtAddCallback(_fadeScale, XmNvalueChangedCallback,
		  mbotw_fadeScaleCb, NULL);
    XtManageChild(rc1);

/*
 *  Create hint widget 
 */
    xmstr = XmStringCreateLocalized("");
    _hintW = XtVaCreateManagedWidget("mbotw_hintLabel",
				     xmLabelWidgetClass,  form,
				     XmNlabelString,      xmstr,
				     XmNrecomputeSize,    False,
				     XmNtopAttachment,    XmATTACH_FORM,
				     XmNbottomAttachment, XmATTACH_FORM,
				     XmNrightAttachment,  XmATTACH_WIDGET,
				     XmNrightWidget,      rc1, 
				     XmNalignment,	  XmALIGNMENT_BEGINNING,
				     XmNwidth,            280,
				     NULL);
    XmStringFree(xmstr);
    mbotw_disabledSet();

    XtManageChild(form);
    XtManageChild(frame);
    return(frame);
}

/*=====================================================================*/
/* ARGSUSED */
static void mbotw_fadeBtnCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * mbotw_fadeBtnCb                                                      *
 *                                                                      *
 * Callback function for fade button to reset to nominal fade value or  *
 * black out the image.    						*
 *                                                                      *
 * void mbotw_fadeBtnCb ( w, which, cbs )	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget     widget ID                                    *
 *  which       long       the button index                             *
 *  cbs		XtPointer  not used                                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	06/96                                           *
 * J. Wu/GSC       	05/01   add I/i hotkeys to toggle fade action 	*
 * M. Li/GSC	   	05/01   mbotw_fadeScaleCb -> mbotw_setFade	*
 * E. Safford/SAIC	04/02	add params to mbot_setFade		*
 ***********************************************************************/
{
float	ratio;

/*---------------------------------------------------------------------*/

    switch( which ) {
	case 0:	/* image set to 0 */
	    ratio = 0.0F;
	    _fadeAway = TRUE;
	break;

	case 1:
	default: 	/* ratio = 50% */
	    ratio = 50.0F;
	    _fadeAway = FALSE;
	break;
    }

    XmScaleSetValue(_fadeScale, (int)ratio);
    ratio = ratio/50.0F;
    mbotw_setFade ( loop_getCurLoop(), ratio, True );
}

/*=====================================================================*/
/* ARGSUSED */
static void mbotw_fadeScaleCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * mbotw_fadeScaleCb                                                    *
 *                                                                      *
 * Callback function for fading effect.                                 *
 *                                                                      *
 * void mbotw_fadeScaleCb ( w, clnt, call )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget     widget ID                                    *
 *  clnt        XtPointer  not used                             	*
 *  call        XtPointer  callback struct                              *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	05/96                                           *
 * C. Lin/EAI      	12/97	add RADAR                               *
 * T. Piper/GSC	   	05/99  	add dataw_isSatSelect and return cases	*
 * S. Jacobs/NCEP  	10/99  	Add variables to checks for SAT and RAD	*
 * E. Safford/GSC  	10/99  	dataw_getCurLoop -> loop_getCurLoop	*
 * M. Li/GSC	   	06/01  	move most of routine to mbotw_setFade()	*
 * E. Safford/SAIC	02/04	add param to mbotw_setFade         	*
 ***********************************************************************/
{
    float ratio;
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
/*---------------------------------------------------------------------*/

    ratio = (float)cbs->value/50.0F;
    mbotw_setFade ( loop_getCurLoop(), ratio, True );
}

/*=====================================================================*/
/* ARGSUSED */
static void mbotw_locOptionCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * mbotw_locOptionCb							*
 *									*
 * Callback function locator options button on the top options menu.	*
 *									*
 * void mbotw_locOptionCb ( wid, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	client data				*
 *	cbs	XtPointer	callback struct				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		02/99	intial coding				*
 * H. Zeng/EAI          02/00   added edit button callback              *
 * H. Zeng/SAIC		01/07	changed arg. list for locfmtw_popup()	*
 ***********************************************************************/
{
    int  choice;
/*---------------------------------------------------------------------*/

    choice = (long)clnt;

    if( choice >= locfmtw_getLocNum() ) {
      locfmtw_popup ();
    }
    else {
      locfmtw_setCurrPos (0);
      locfmtw_locOptCb (NULL, (XtPointer)(long)choice, NULL);
    }
}

/*=====================================================================*/

void mbotw_pageSet ( int current, int total )
/************************************************************************
 * mbotw_pageSet                                              		*
 *                                                                      *
 * This function set the current frame (page) number and total number   *
 * of frames (pages) information.     					*
 *                                                                      *
 * void mbotw_pageSet ( current, total )                   		*
 *                                                                      *
 * Input parameters:                                                    *
 *  current       int  	current page (frame) number                     *
 *  total         int  	total number of frames                    	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96  						*
 * C. Lin/EAI      	06/98	add displaying frame time 		*
 * E. Safford/GSC	12/98	avoid resetting time for frame 0	*
 * E. Safford/GSC	10/99	update for nmap2, use dattm_t         	*
 * E. Safford/GSC	10/99	use nmap_loop for current loop & time	*
 * E. Safford/GSC	02/00	add loop loading message		*
 * H. Zeng/EAI          04/01   moved blank map before the 1st pixmap   *
 * M. Li/SAIC		06/03	Added day of week and forecast hour	*
 * M. Li/SAIC		07/03	Added a case for the "blank" loop	*
 * E. Safford/SAIC	12/03	added check on alias before strcpy	*
 * T. Lee/SAIC		 9/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS          *
 * F. J. Yen/NCEP	 4/08	Add bin minutes & mstrct to ctb_dtget CS*
 * L. Hinson/AWC         6/08   Add comparison of cycle to "000101/0000"*
 *                               to display day/dattim only             *
 * K. Tyle/UAlbany	11/10	Increase dimension of dayw		*
 ***********************************************************************/
{
char		pagestr[20], loopstr[20], dayw[4], cycle[20], strdt[30];
char        	asrc[256], *alias, d1[256], d2[256], tmpstr[256];
dattm_t		dttm;
int             iarray[5], len, lp, idayw, itarr[5], ier, iret, dtmch;
int         	isbcat, d3, d4, d5, d6, d7, d7m, d8, d8m, mstrct, nmin,
		ifhr, dionoff;
XmString	xmstr;
time_t		tt;
struct tm	*ts;
dsrc_t		*dom;
/*---------------------------------------------------------------------*/


    if (_loadingLoop) {
/*
 *  Display loading message
 */
	sprintf (loopstr, "Loading %2d", _activeLoop+1);

	xmstr = XmStringCreateLocalized (loopstr);
	XtVaSetValues (_timeW, XmNlabelString, xmstr, NULL);

	XmStringFree (xmstr);
	XmUpdateDisplay(_timeW);

    }
    else if (current >= 0) {
/*
 *  Display time info
 */
        lp = loop_getCurLoop();
        loop_getFrameTm (lp, current, dttm);

        if ( dttm[0] == '\0' ) {
            tt = time(NULL);
            ts = gmtime(&tt);
            iarray[0] = 1900 + ts->tm_year;
            iarray[1] = ts->tm_mon + 1;
            iarray[2] = ts->tm_mday;
            iarray[3] = ts->tm_hour;
            iarray[4] = ts->tm_min;
            ti_itoc(iarray, dttm, &iret, sizeof(dttm));
            dttm[19] = '\0';
            cst_rmbl(dttm, dttm, &len, &iret);
        }

/*
 *  Compute the day of the week.
 */
	ti_ctoi ( dttm, itarr, &ier, strlen(dttm) );
	ti_dayw ( itarr, &idayw, &ier);

	switch( idayw ) {

            case 1:     strcpy ( dayw, "SUN" );
                        break;

            case 2:     strcpy ( dayw, "MON" );
                        break;

            case 3:     strcpy ( dayw, "TUE" );
                        break;

            case 4:     strcpy ( dayw, "WED" );
                        break;

            case 5:     strcpy ( dayw, "THU" );
                        break;

            case 6:     strcpy ( dayw, "FRI" );
                        break;

            case 7:     strcpy ( dayw, "SAT" );
                        break;
        }

/*
 *  Compute forecast hour.
 */
	cycle[0] = '\0';
	ifhr = 0;
	isbcat = IMISSD;
	dom   = (dsrc_t *)dataw_getDomSrc(loop_getCurLoop());

	if (dom != NULL) {
            strcpy (asrc, dom->path);
            alias  = strtok(asrc, "/");
            alias  = strtok(NULL, "/");

	    if ( alias != NULL ) {
                strcpy ( tmpstr, alias );
 
                ctb_dtget ( tmpstr, d1, d2, &d3, &isbcat, &d4, &d5, &d6, 
			    &dionoff, &d7, &d7m, &d8, &d8m, &mstrct,
			    &dtmch, &ier );
 
 	        if  ( isbcat == SCAT_FCT ) {
                    dslw_getGrdCycle ( cycle ); 
                }
                else if ( isbcat == SCAT_SFF || isbcat == SCAT_SNF )  {
                    dslw_getMosCycle ( cycle ); 
                }
            }
	}

	if ( cycle != '\0' || strcmp(cycle,"000101/0000")!=0 ) {
            cst_rpst ( cycle, "_", "/", cycle, &ier );
            ti_diff ( dttm, cycle, &nmin, &ier, strlen(dttm), strlen(cycle) );
            ifhr = (int) nmin / 60 ;
        }

        if ( (isbcat == SCAT_SFF || isbcat == SCAT_SNF ||
	     isbcat == SCAT_FCT) && strcmp(cycle,"000101/0000")!=0 ) {
            sprintf(strdt, "%s %sV%03d", dayw, dttm, ifhr);
        }
        else {
            sprintf(strdt, "%s %s", dayw, dttm);
        }

        xmstr = XmStringCreateLocalized(strdt);
        XtVaSetValues(_timeW, XmNlabelString, xmstr, NULL);
        XmStringFree(xmstr);

        XmUpdateDisplay(_timeW);
    }

/*
 *  Display page info
 */
    sprintf(pagestr, "%2d of %2d", current, total);
    xmstr = XmStringCreateLocalized(pagestr);
    XtVaSetValues(_pageW, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);
	
    XmUpdateDisplay(_pageW);
}

/*=====================================================================*/

void mbotw_loadingLoopSet ( Boolean flag, int lp )
/************************************************************************
 * mbotw_loadingLoopSet                                       		*
 *                                                                      *
 * This function sets the _loadingLoop flag                             *
 *                                                                      *
 * void mbotw_loadingLoopSet ( flag, lp )                     		*
 *                                                                      *
 * Input parameters:                                                    *
 *  flag        Boolean 	value for _loadLoop flag               	*
 *  lp		int		Loop number 				*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	02/00	initial coding                       	*
 ***********************************************************************/
{
    _loadingLoop = flag;
    _activeLoop  = (_loadingLoop) ? lp : 0;
}

/*=====================================================================*/

void mbotw_startLocDspl ( void )
/************************************************************************
 * mbotw_startLocDspl                                                   *
 *                                                                      *
 * This function creates the overriding widgets and their associated    *
 * label widgets for multiple locator types display.			*
 *									*
 * NOTE:  This function is called whenever the mouse enters the drawing	*
 *	  area.  See mcanvw_evntHandler.				*
 *                                                                      *
 * void mbotw_startLocDspl ( void )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		01/07	initial coding				*
 ***********************************************************************/
{
    int         ii;
    Position	pos_x, pos_y, new_pos_y;
    Dimension   w_width, w_height;
    static Boolean   first_time=TRUE;
/*---------------------------------------------------------------------*/
/*
 *  In the first time, obtain the total position number and allocate
 *  memory for the pointers.
 */
    if ( first_time ) {

      _posNum = locfmtw_getPosNum ();
      _locDsplWid  = (Widget *) malloc (_posNum * sizeof(Widget));
      _locDsplLbl  = (Widget *) malloc (_posNum * sizeof(Widget));
      for ( ii = 0; ii < _posNum; ii++ ) {

        _locDsplWid[ii] = NULL;
	_locDsplLbl[ii] = NULL;
      } 
      _locDsplLbl[0] = _latlonW; 
 
      _topW    = mainw_getToplevel ();
      _drawW   = mcanvw_getDrawingW();

      first_time = FALSE;
    }

/*
 *  Create locator type overriding display widgets and their 
 *  associated labels.
 */
    XtVaGetValues (_topW, XmNx, &pos_x, XmNy, &pos_y, NULL);
    XtVaGetValues (_drawW,
                   XmNwidth,     &w_width, 
                   XmNheight,    &w_height,
                   NULL);

    pos_x += (Position)((float)w_width  - 163);
    pos_y += (Position)((float)w_height + 62 );
			
    for ( ii = 1; ii < _posNum; ii++ ) {

      if ( locfmtw_getPosOnoff(ii) ) {

        if ( _locDsplWid[ii] != NULL ) {

           XtDestroyWidget (_locDsplWid[ii]);
           _locDsplWid[ii] = NULL;
	   _locDsplLbl[ii] = NULL;
        }

        new_pos_y = pos_y - (Position)(34 * (ii-1));
        _locDsplWid[ii] = XtVaCreateManagedWidget("loc_dspl_wid",
			  overrideShellWidgetClass, _mainw_form,
			  XmNheight,	33,
			  XmNwidth,	130,
			  XmNx,		pos_x,
			  XmNy,		new_pos_y,
			  NULL);
        _locDsplLbl[ii] = XtVaCreateManagedWidget("  ",
			  xmLabelWidgetClass,  _locDsplWid[ii],  
			  XmNfontList,	       _fontList,
			  NULL);
	}
    }
}

/*=====================================================================*/

void mbotw_updateLocDspl ( float lat, float lon )
/************************************************************************
 * mbotw_updateLocDspl                                                  *
 *                                                                      *
 * This function sets the locator info. base on the latitude and        *
 *longitude of the current mouse location.				*
 *                                                                      *
 * void mbotw_updateLocDspl ( lat, lon )                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      lat     float   latitude of the mouse location                  *
 *      lon     float   longitude of the mouse location                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI           04/96                                           *
 * S. Law/GSC           02/99   added other types of information        *
 * D.W.Plummer/NCEP      3/99   added dist & dir to VOR and ANCHOR      *
 * D.W.Plummer/NCEP      4/99   add Marine and Coastal types            *
 * M. Li/GSC            10/99   replaced clo_compass code               *
 * S. Jacobs/NCEP       12/99   Added lat/lon in degrees and minutes    *
 * D.W.Plummer/NCEP     12/99   Add check for missing lat/lon           *
 * A. Hardy/GSC		01/00   Added new function clo_format           *
 * H. Zeng/EAI          01/00   used locfmtw_getLocFmt()                *
 * A. Hardy/GSC		 8/00   renamed clo_format to clo_ddenc		*
 * H. Zeng/SAIC		01/07	changed the function name		*
 ***********************************************************************/
{
    int         ii, loc_idx, loc_fmt, ier;
    char        str[20], name[32];
    XmString    xmstr;
/*---------------------------------------------------------------------*/
    ier = 0;

    for ( ii = 0; ii < _posNum; ii++ ) {

      if ( !locfmtw_getPosOnoff(ii) )  continue;

      if ( !ERMISS(lat) && !ERMISS(lon) )  {

         locfmtw_getPosInfo(ii, &loc_idx, &loc_fmt);
         strcpy ( name, locfmtw_getLocId(loc_idx) );
	 clo_ddenc ( name, loc_fmt, lat, lon, str, &ier);
      }
      else  {

        strcpy( str, "-" );
      }

      xmstr = XmStringCreateLocalized(str);
      if ( _locDsplLbl[ii] != NULL ) {
         XtVaSetValues(_locDsplLbl[ii], XmNlabelString, xmstr, NULL);
      }
      XmStringFree(xmstr);

    } /* the end of for (... */
}

/*=====================================================================*/

void mbotw_endLocDspl ( void )
/************************************************************************
 * mbotw_endLocDspl                                              	*
 *                                                                      *
 * This function clears the location information of current mouse	*
 * location.								*
 *                                                                      *
 * void mbotw_endLocDspl ()                   				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96  						*
 * H. Zeng/SAIC	   01/07	changed for multi. locator display	*
 ***********************************************************************/
{
    int      ii;
    XmString xmstr;
/*---------------------------------------------------------------------*/

/*
 *  Wipe off the primary (the first) locator display.
 */
    xmstr = XmStringCreateLocalized(" ");
    XtVaSetValues(_latlonW, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);

/*
 *  Destroy the overriding widgets for any additional locator display.
 */
    for ( ii = 1; ii < _posNum; ii++ ) {

      if ( _locDsplWid[ii] != NULL ) {

         XtDestroyWidget (_locDsplWid[ii]);
         _locDsplWid[ii] = NULL;
	 _locDsplLbl[ii] = NULL;
      }
    }
}

/*=====================================================================*/

void mbotw_getFadeColor ( void )
/************************************************************************
 * mbotw_getFadeColor                                                   *
 *                                                                      *
 * This function gets color indices for the image colors for use 	*
 * in fading. 								*
 *                                                                      *
 * void mbotw_getFadeColor()                                            *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96                                           *
 * C. Lin/EAI      	12/97 	add RADAR                               *
 * T. Piper/GSC    	05/99	added dataw_isSatSelect and return cases*
 * S. Jacobs/NCEP  	10/99	Added variables to checks for SAT & RAD	*
 * E. Safford/GSC	10/99	dataw_getCurLoop -> loop_getCurLoop	*
 * E. Safford/GSC	06/01	remove mbotw_fadeBtnCb()		*
 * E. Safford/GSC	02/04	add param to mbotw_reloadFade		*
 ***********************************************************************/
{   int	lp;
/*---------------------------------------------------------------------*/
    lp = loop_getCurLoop ();

    mbotw_reloadFade( lp );
    mbotw_fadeReset();
}

/*=====================================================================*/

void mbotw_reloadFade ( int lp )
/************************************************************************
 * mbotw_reloadFade                                                     *
 *                                                                      *
 * This function gets reloads the color indices from the appropriate    *
 * color bank for fading.             					*
 *                                                                      *
 * void mbotw_reloadFade( lp )                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp		int	loop to reload				*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	06/01	moved functionality from _getFadeColor  *
 * E. Safford/SAIC	02/04	add lp param				*
 ***********************************************************************/
{
int	bank, ncolors, nindex, iret;

/*---------------------------------------------------------------------*/

    if ( dataw_isRadSelect(lp, &nindex) )
        bank = 2;
    else if ( dataw_isSatSelect(lp, &nindex) )
        bank = 1;
    else
        return;

    xqcmps(&bank, &ncolors, _fadeRed, _fadeGreen, _fadeBlue, &iret);

    if ( iret == 0 ) {
        _nfadeC = ncolors;
    }
}

/*=====================================================================*/

void mbotw_hintRedraw ( void )
/************************************************************************
 * mbotw_hintRedraw							*
 *                                                                      *
 * This function redraws the contents of the hint box.			*
 *                                                                      *
 * void mbotw_hintRedraw()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI  06/98 Uniform status hints				*
 * M. Li/SAIC	   08/03 redraw only when pgpalw_isUp is true		*
 ***********************************************************************/
{
    char hintString[60];
    XmString xmstr;
/*---------------------------------------------------------------------*/

    if ( strcmp(_hintClass, "CLASS") == 0 ) _hintClass[0] = '\0';
    
    sprintf( hintString,
	     "%8s %-6.6s <L> %-7.7s <M> %-6.6s",
	     _hintAction, _hintClass, _hintMouseL, _hintMouseM );
    xmstr = XmStringCreateLocalized(hintString);
    if ( pgpalw_isUp() ) {
        XtVaSetValues(_hintW, XmNlabelString, xmstr, NULL);
    }
    XmStringFree(xmstr);
}

/*=====================================================================*/

void mbotw_disabledSet ( void )
/************************************************************************
 * mbotw_disabledSet							*
 *                                                                      *
 * This function hints that the Product Generator is disabled in the	*
 * hint box.								*
 *                                                                      *
 * void mbotw_disabledSet()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * G. Krueger/EAI	10/98	Removed right mouse button option	*
 * E. Safford/SAIC	08/01	add preceeding blanks to string		*
 ***********************************************************************/
{
    XmString xmstr;

/*---------------------------------------------------------------------*/

    xmstr = XmStringCreateLocalized("   Product Generation Inactive");
    XtVaSetValues(_hintW, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);
}

/*=====================================================================*/

void mbotw_enabledSet ( void )
/************************************************************************
 * mbotw_enabledSet							*
 *									*
 * This function hints that the Product Generator is enabled in the	*
 * hint box.								*
 *									*
 * void mbotw_enabledSet()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI  	06/98 	Uniform status hints			*
 * E. Safford/GSC	09/98	clean up				*
 * G. Krueger/EAI	10/98	Removed right mouse button option	*
 ***********************************************************************/
{
    char uname[30];
    int	 ier;
/*---------------------------------------------------------------------*/

    _hintAction[0] = '\0';
    cst_ncpy(uname, pgpalw_getCurClassName(), 29, &ier);

    cst_lcuc(uname, uname, &ier);
    mbotw_classSet(uname);

    _hintMouseL[0] = '\0';
    _hintMouseM[0] = '\0';

    mbotw_hintRedraw();
}

/*=====================================================================*/

void mbotw_actionSet ( char *aname )
/************************************************************************
 * mbotw_actionSet							*
 *									*
 * This function indicates the current action in the hint box.		*
 *									*
 * void mbotw_actionSet ( aname )					*
 *									*
 * Input parameters:							*
 *	*aname		char 	Action Name				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI  	06/98 	Uniform status hints			*
 * E. Safford/GSC	09/98	change cst_ncpy call to lstr-1		*
 ***********************************************************************/
{
int	 lstr, ier;
/*---------------------------------------------------------------------*/

	lstr = sizeof(_hintAction);
	cst_ncpy(_hintAction, aname, lstr-1, &ier);

	cst_lcuc( _hintAction, _hintAction, &ier );

	if ( strcmp( _hintAction, "UNDO" ) == 0 ||
	     strcmp( _hintAction, "REDO" ) == 0 ||
	     strcmp( _hintAction, "PGEN" ) == 0 )
	    _hintAction[0] = '\0';

	mbotw_hintRedraw();
}

/*=====================================================================*/

void mbotw_classSet ( char *cname )
/************************************************************************
 * mbotw_classSet							*
 *									*
 * This function indicates the current class in the hint box.		*
 *									*
 * void mbotw_classSet ( cname )					*
 *									*
 * Input parameters:							*
 *	*cname		char 	Class Name				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI  06/98 Uniform status hints				*
 ***********************************************************************/
{
    int		ier;
    size_t	lstr;

/*---------------------------------------------------------------------*/

    lstr = sizeof(_hintClass);
    cst_ncpy( _hintClass, cname, lstr, &ier );
    cst_lcuc( _hintClass, _hintClass, &ier );
    mbotw_hintRedraw();
}

/*=====================================================================*/

void mbotw_mouseSet ( char *lstr, char *mstr )
/************************************************************************
 * mbotw_mouseSet							*
 *									*
 * This function indicates the current mouse status in the hint box.	*
 * Any string pointer that is null will not change the current status	*
 * display for the associated mouse button.				*
 *									*
 * void mbotw_mouseSet ( lstr, mstr )					*
 *									*
 * Input parameters:							*
 * *lstr		char 	Left mouse button function		*
 * *mstr		char 	Middle mouse button function		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI  	06/98 	Uniform status hints			*
 * E. Safford/GSC	09/98	change cst_ncpy call          		*
 * G. Krueger/EAI	10/98	Removed right mouse button option	*
 ***********************************************************************/
{
    int	 llstr, lmstr, ier;
/*---------------------------------------------------------------------*/

    if ( lstr != NULL ) {
	llstr = sizeof(_hintMouseL);
	cst_ncpy(_hintMouseL, lstr, llstr-1, &ier);
    }

    if ( mstr != NULL ) {
	lmstr = sizeof(_hintMouseM);
	cst_ncpy(_hintMouseM, mstr, lmstr-1, &ier);
    }
    mbotw_hintRedraw();
}

/*=====================================================================*/

void mbotw_actionClear ( void )
/************************************************************************
 * mbotw_actionClear							*
 *									*
 * This function clears the action portion of the hint box.		*
 *									*
 * void mbotw_actionClear()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI  06/98 Uniform status hints				*
 ***********************************************************************/
{
    _hintAction[0] = '\0';
    mbotw_hintRedraw();
}

/*=====================================================================*/

void mbotw_classClear ( void )
/************************************************************************
 * mbotw_classClear							*
 *									*
 * This function clears the class portion of the hint box.		*
 *									*
 * void mbotw_classClear()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI  06/98 Uniform status hints		 		*
 ***********************************************************************/
{
    _hintClass[0] = '\0';
    mbotw_hintRedraw();
}

/*=====================================================================*/

void mbotw_mouseClear ( void )
/************************************************************************
 * mbotw_mouseClear							*
 *									*
 * This function clears the mouse portion of the hint box.		*
 *									*
 * void mbotw_mouseClear()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI	06/98	Uniform status hints		 	*
 * G. Krueger/EAI	10/98	Removed right mouse button option	*
 ***********************************************************************/
{
    _hintMouseL[0] = '\0';
    _hintMouseM[0] = '\0';
    mbotw_hintRedraw();
}

/*=====================================================================*/

void mbotw_pgfileSet ( char *fname )
/************************************************************************
 * mbotw_pgfileSet							*
 *									*
 * This function sets the product generation file (VGF) in the label	*
 * widget.								*
 *									*
 * void mbotw_pgfileSet ( fname )					*
 *									*
 * Input parameters:							*
 * *fname	char 	file name					*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	   12/97						*
 ***********************************************************************/
{
    XmString xmstr;
/*---------------------------------------------------------------------*/

    xmstr = XmStringCreateLocalized(fname);
    XtVaSetValues(_pgfileW, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);
}

/*=====================================================================*/

void mbotw_pgfileClear ( void )
/************************************************************************
 * mbotw_pgfileClear							*
 *									*
 * This function clears the product generation file label.		*
 *									*
 * void mbotw_pgfileClear()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	   12/97						*
 ***********************************************************************/
{
    XmString xmstr;
/*---------------------------------------------------------------------*/

    xmstr = XmStringCreateLocalized(" ");
    XtVaSetValues(_pgfileW, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);
}

/*=====================================================================*/

void mbotw_fadeReset ( void )
/************************************************************************
 * mbotw_fadeReset							*
 *									*
 * Reset the fade to the default value.					*
 *									*
 * mbotw_fadeReset ( )							*
 *									*
 **									*
 * Log: 								*
 * S. Jacobs/NCEP	 7/00	Created					*
 ***********************************************************************/
{
    mbotw_fadeBtnCb ( NULL, 1, NULL );
}

/*=====================================================================*/

void mbotw_fadeToggle ( void )
/************************************************************************
 * mbotw_fadeToggle							*
 *									*
 * Wrapper function to facilitate hot keys to toggle fade button action.*
 *									*
 * void mbotw_fadeToggle ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * J. Wu/NCEP	05/01	Created						*
 ***********************************************************************/
{
/*
 *  Toggle the display of images and fade flag.
 */    
    if ( _fadeAway ) {
	mbotw_fadeReset ();
	_fadeAway = FALSE;
	
    }
    else {
	mbotw_fadeBtnCb ( NULL, 0, NULL );
        _fadeAway = TRUE;
    }
}

/*=====================================================================*/

void mbotw_setFade ( int lp, float ratio, Boolean updtDsply )
/************************************************************************
 * mbotw_setFade                                                     	*
 *                                                                      *
 * This function sets the fade level for the loop.			*
 *                                                                      *
 * void mbotw_setFade ( ratio )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	lp	    	int	loop					*
 *	ratio          	float   fade ratio				*
 *	updtDsply	Boolean	update display flag			*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		06/01	created					*
 * E. Safford/SAIC	02/04	add updtDsply and lp params		*
 * H. Zeng/SAIC		04/04	added call to xmfrmtg_saveFrmTag()	*
  * T Piper/SAIC	02/09	Added NxmBusy_setStopBtn to fix bug	*
 ***********************************************************************/
{
int 	ii, bank, iret, icolrs[256], red[256], green[256], blue[256];
int     nindex;
float	oratio;

/*---------------------------------------------------------------------*/

    if ( lp < 0 || lp >= MAX_LOOP ) {
        return;
    }

    if ( dataw_isRadSelect(lp, &nindex) )
        bank = 2;
    else if ( dataw_isSatSelect(lp, &nindex) )
        bank = 1;
    else
        return;

    loop_getFadeRatio ( lp, &oratio );
    loop_setFadeRatio ( lp, ratio );

    for (ii = 0; ii < _nfadeC; ii++) {

        red[ii]   = (int)((float)_fadeRed[ii] * ratio);
        green[ii] = (int)((float)_fadeGreen[ii] * ratio);
        blue[ii]  = (int)((float)_fadeBlue[ii] * ratio);
        red[ii]   = (red[ii]   < 255) ? red[ii]:255;
        green[ii] = (green[ii] < 255) ? green[ii]:255;
        blue[ii]  = (blue[ii]  < 255) ? blue[ii]:255;

        icolrs[ii] = ii;
    }

    gsbrgb(&bank, &_nfadeC, icolrs, red, green, blue, &iret);

    if ( ( mcanvw_getDpth() > _8_BIT ) &&
           !G_DIFF(ratio, oratio) && updtDsply ) {

        xmfrmtg_saveFrmTag ( lp, &iret );
	NxmBusy_setStopBtn(0);
        dsp_reloadLoop ( lp, &iret );
	NxmBusy_setStopBtn(1);
        xmfrmtg_restoreFrmTag ( lp, &iret );	
	dsp_updtDisplay();
    }
}

/*=====================================================================*/

void mbotw_restoreFade ( int lp, Boolean updtDsply )
/************************************************************************
 * mbotw_restoreFade                                                    *
 *                                                                      *
 * This function resets the appropriate fade level for the loop.        *
 *                                                                      *
 * void mbotw_restoreFade ( lp, updtDsply )                           	*
 *                                                                      *
 * Input parameters:                                                    *
 *	lp		int	loop number				*	
 *	updtDsply	Boolean	update display flag			*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		06/01	created					*
 * E. Safford/SAIC	02/04	add params				*
 ***********************************************************************/
{
float	ratio;
/*---------------------------------------------------------------------*/

    if ( lp < 0 || lp >= MAX_LOOP ) {
        return;
    }

    loop_getFadeRatio(lp, &ratio);

    mbotw_setFade ( lp, ratio, updtDsply );
    if ( updtDsply) {
       XmScaleSetValue(_fadeScale, (int)(ratio*50.0F));	
    }
}
