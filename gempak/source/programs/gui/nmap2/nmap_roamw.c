#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"

#define ROAMCANVAS_WDTH	 150
#define ROAMCANVAS_HGHT	 112

#define ROAMW_LINE_WDTH	   0

#define PXMW_MARGIN	   2	/* margin width of pixmap rectangle */

#define ROAM_INCREMENT	  50.0F

#define ROAM_TBL	"nmap_roam.tbl"
#define CTL_PANEL	9999
#define MAX_ROAMVAL	  22

#define DEFAULT_LEVEL	   0

/*
 * define pixmap window attributes
 */
#define PXMW_COLOR	 "blue"
#define SCREEN_COLOR	 "white"

static Widget   _roamPopW;
static Widget   _roamCanvasW;
static Window   _roamWin;
static Display *_roamDpy;

static GC  _roamwGc;   /* GC for drawing pixmap window and roam window */
static GC  _rubberGc;  /* GC for drawing rubber band */

static Pixel _pxmwColr;	  /* color for pixmap rectangle */
static Pixel _scrColr;	  /* color for screen rectangle */

static float _winRatio; /* the ration between real window and control */

static int _pxmwX;      /* Upper left X-coordinate of roam box */
static int _pxmwY;      /* Upper left Y-coordinate of roam box */
static int _pxmwHght;   /* Height of roam box, which is proportional to actual pixmap height  */
static int _pxmwWdth;   /* Width  of roam box, which is proportional to actual pixmap width  */

static int _screenX;    /* screen rectangle data */
static int _screenY;
static Cardinal _screenWdth; /* Width  of ghost box in roam box, which is proportional to actual width  of view area  */
static Cardinal _screenHght; /* Height of ghost box in roam box, which is proportional to actual height of view area  */

static int _rbRefX;	/* rubber band data */
static int _rbRefY;
static int _rbStartX;
static int _rbStartY;
static int _rbLastX;
static int _rbLastY;
static Boolean _rbFlag;


static Widget 		_imgRoamBtn;	/* Roam button for full image size */
static Widget 		_roamCtlBtn;	/* menu button for roam control */
static WidgetList	_roamMb; 	/* roam menu items */
static int		_numRoam;	/* number of roam buttons */
static int		_roamValues[MAX_ROAMVAL]; /* Roam values array */
static Boolean		_roamWinUp = TRUE;
					/* User wants Roam Control Window */



/*
 *  private functions -- callback
 */
static void roamw_arrowCb (  Widget, long which, XtPointer);
static void roamw_ctlBtnCb ( Widget, long which, XtPointer);
static void roamw_exposeCb ( Widget, XtPointer, 
				XmDrawingAreaCallbackStruct *call);
static void roamw_menuCb ( Widget, long which, XtPointer call);
static void roamw_motionCb ( Widget, XtPointer, XEvent *event);

/*
 * private functions -- action
 */
static void roamw_endRband ( void );
static void roamw_newXY ( int ix, int iy );
static void roamw_redraw ( void );
static void roamw_startRband ( int x, int y );
static void roamw_trackRband ( int x, int y);


/************************************************************************
 * nmap_roamw.c								*
 *									*
 * This module creates the roam control popup window and defines the	*
 * callback functions.							*
 *									*
 * CONTENTS:								*
 *	roamw_create()		creates the roam control popup window.	*
 *	roamw_popup()		pop up the roam control window.		*
 *	roamw_popdown()		pop down the roam control window.	*
 *	roamw_createMenu()	creates the roam menu.			*
 *	roamw_sensitive()       set sensitivity of the roam control win	*
 *	roamw_setWinSize()	set pixmap and screen window sizes.	*
 *	roamw_setScreenXY()	set screen origin.			*
 *	roamw_setup		sets up the roam environment		*
 *      roamw_ctlBtnSnstiv()    set the sensitivity of _roamCtlBtn      *
 *									*
 *	roamw_isUp()	        TRUE if room ctrl window is up          *
 *	roamw_disableMenu()	make all the roam menu insensitive	*
 *	roamw_enableMenu()	make all the roam menu sensitive	*
 *									*
 *	roamw_menuCb()		callback function for ROAM menu. 	*
 *	roamw_exposeCb()	expose callback for drawing canvas.	*
 *	roamw_motionCb()	motion callback for drawing canvas.	*
 *	roamw_arrowCb()		arrow button callback.			*
 *	roamw_ctlBtnCb()	control button (close) callback.	*
 *									*
 *	roamw_redraw()		redraw the roam control canvas.		*
 *	roamw_startRband()	start the rubber banding.		*
 *	roamw_trackRband()	start the rubber banding.		*
 *	roamw_endRband()	start the rubber banding.		*
 *	roamw_newXY()	        set new ul corner of 'S' in GEMPAK.	*
 ***********************************************************************/

/*=====================================================================*/

Widget roamw_create ( Widget parent )
/************************************************************************
 * roamw_create								*
 *									*
 * This function creates the roam control popup window.			*
 *									*
 * Widget roamw_create (parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent form widget ID			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 * roamw_create		Widget	roam control popup window widget ID	*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		08/97						*
 * G. Krueger/EAI	10/97	NxmControlBtn->NxmCtlBtn_create 	*
 * C. Lin/EAI		05/98	rearrange widgets to make window smaller*
 * S. Law/GSC		10/99	added loop level variables		*
 * H. Zeng/EAI          11/99   used loop_setRoamVal()                  *
 * E. Safford/GSC	12/99   add roamw_ctlBtnSnstiv                  *
 * T. Lee/GSC		03/01	changed call seq. of _set & _save roam	*
 * T. Piper/SAIC	04/03	removed roamw_ctlBtnSnstiv		*
 ***********************************************************************/
{
    Widget	pane, frame, form, rc1, rc2, arrow[2];
    int		ii;
    char	*btnstr[]={"Close"};
/*---------------------------------------------------------------------*/
/*
 * create dialog shell
 */
    _roamPopW = XmCreateFormDialog(parent, "roamw_popup", NULL, 0);
    XtVaSetValues (_roamPopW, XmNnoResize, True, NULL);
    XtVaSetValues (XtParent(_roamPopW), XmNtitle, "Roam Control", NULL);

    _roamDpy = XtDisplay(_roamPopW);

/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("roamw_pane",
			    xmPanedWindowWidgetClass,	_roamPopW,
			    XmNsashWidth,	1,
			    XmNsashHeight,	1,
			    NULL);

/*
 * create the roam control area 
 */
    form = XtVaCreateWidget("roamw_controlForm", 
			    xmFormWidgetClass, pane, 
			    NULL);
/*
 * create vertical arrow button control
 */
    rc1 = XtVaCreateWidget("dataw_vertRc",
			   xmRowColumnWidgetClass, form,
			   XmNorientation,	XmVERTICAL,
			   XmNmarginHeight,	0,
			   XmNmarginWidth,	0,
			   XmNleftAttachment,	XmATTACH_FORM,
			   XmNtopAttachment,	XmATTACH_POSITION,
			   XmNtopPosition,	5,
			   NULL);

    arrow[0] = XtVaCreateManagedWidget("dataw_vArrowUp",
				       xmArrowButtonWidgetClass, rc1,
				       XmNarrowDirection, XmARROW_UP,
				       NULL);

    XtAddCallback (arrow[0], XmNactivateCallback, 
			(XtCallbackProc)roamw_arrowCb, (XtPointer)0);

    arrow[1] = XtVaCreateManagedWidget("dataw_vArrowDown",
				       xmArrowButtonWidgetClass, rc1,
				       XmNarrowDirection, XmARROW_DOWN,
				       NULL);

    XtAddCallback (arrow[1], XmNactivateCallback, 
			(XtCallbackProc)roamw_arrowCb, (XtPointer)1);

    XtManageChild(rc1);

/*
 * create horizontal arrow button control
 */
    rc2 = XtVaCreateWidget("dataw_horzRc",
			   xmRowColumnWidgetClass, form,
			   XmNorientation,	XmVERTICAL,
			   XmNmarginHeight,	0,
			   XmNmarginWidth,	0,
			   XmNleftAttachment,	XmATTACH_FORM,
			   XmNtopAttachment,	XmATTACH_WIDGET,
			   XmNtopWidget,	rc1,
			   XmNtopOffset,	15,
			   NULL);

    arrow[0] = XtVaCreateManagedWidget("dataw_hArrowLeft",
				       xmArrowButtonWidgetClass, rc2,
				       XmNarrowDirection, XmARROW_LEFT,
				       NULL);

    XtAddCallback (arrow[0], XmNactivateCallback, 
			(XtCallbackProc)roamw_arrowCb, (XtPointer)2);

    arrow[1] = XtVaCreateManagedWidget("dataw_hArrowRight",
				       xmArrowButtonWidgetClass, rc2,
				       XmNarrowDirection, XmARROW_RIGHT,
				       NULL);

    XtAddCallback (arrow[1], XmNactivateCallback, 
			(XtCallbackProc)roamw_arrowCb, (XtPointer)3);

    XtManageChild(rc2);

/*
 * create the roam control canvas 
 */
    frame = XtVaCreateWidget ("roamw_controlFrame",
			      xmFrameWidgetClass, form,
			      XmNshadowType,		XmSHADOW_IN,
			      XmNmarginWidth,		0,
			      XmNmarginHeight,		0,
			      XmNleftAttachment,	XmATTACH_WIDGET,
			      XmNleftWidget,		rc1,
			      XmNrightAttachment,	XmATTACH_FORM,
			      XmNtopAttachment,		XmATTACH_FORM,
			      NULL);

    _roamCanvasW = XtVaCreateManagedWidget("roamw_canvas",
					   xmDrawingAreaWidgetClass, frame,
					   XmNwidth,	ROAMCANVAS_WDTH,
					   XmNheight,	ROAMCANVAS_HGHT,
					   NULL);

    XtAddCallback(_roamCanvasW, XmNexposeCallback, 
			(XtCallbackProc)roamw_exposeCb, NULL);

    XtAddEventHandler (_roamCanvasW,
		       ButtonMotionMask | ButtonReleaseMask | ButtonPressMask,
		       FALSE, (XtEventHandler) roamw_motionCb, NULL);

    XtManageChild(frame);

    XtManageChild(form);

/*
 * create control buttons
 */
    form = NxmCtlBtn_create (pane, 1, "roamw_ctlBtn", XtNumber(btnstr), 
			     btnstr, (XtCallbackProc)roamw_ctlBtnCb, NULL);
    XtVaSetValues(form, XmNmarginHeight, 15, NULL);

    XtManageChild(pane);

    _roamWin  = (Window)NULL;
    _pxmwHght = 0;

    for (ii = 0; ii < MAX_LOOP; ii++) {
        loop_setRoamVal(ii, DEFAULT_LEVEL);
    }

    loop_saveRoamVal();

    return(_roamPopW);
}

/*=====================================================================*/

void roamw_popup ( void )
/************************************************************************
 * roamw_popup 								*
 *                                                                      *
 * This function pops up the roam control popup window.    		*
 *                                                                      *
 * void roamw_popup()                   				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	08/97  						*
 * E. Safford/GSC	03/01	Use redraw for refresh, not popdown/up  *
 * T. Piper/SAIC	05/03	replaced XAllocNamedColor w/xsncolr	*
 ***********************************************************************/
{
    int		ier;
    Pixel	fg, bg;
    XGCValues	gcval;
/*---------------------------------------------------------------------*/
/*
 *  If roam window is already up, just redraw drag box.
 */
    if (XtIsManaged (_roamPopW)) {
        roamw_redraw();
	return;
    } 

/*
 * check other drawing windows, make sure the main window is 
 * the only window up
 */
    if (mapw_isUp()) mapw_popdown();

    XtManageChild(_roamPopW);
    XtSetSensitive (_roamCtlBtn, FALSE);


    if (!_roamWin) {
	_roamWin = XtWindow(_roamCanvasW);

/*
 * create GC for pixmap window and screen window
 */
	gcval.line_width = ROAMW_LINE_WDTH;
	xsncolr(PXMW_COLOR, &_pxmwColr, &ier);
	gcval.foreground = _pxmwColr;

	_roamwGc = XCreateGC( _roamDpy, _roamWin,
			      GCLineWidth|GCForeground, &gcval );

/*
 * get color for screen window
 */
	xsncolr(SCREEN_COLOR, &_scrColr, &ier);

/*
 * create GC for rubber banding
 */
	XtVaGetValues( _roamCanvasW, XmNbackground, &bg, NULL );

	xsncolr(SCREEN_COLOR, &fg, &ier);
	_rubberGc = XCreateGC( _roamDpy, _roamWin, 0, NULL );

	XSetFunction( _roamDpy, _rubberGc, GXxor );
	XSetForeground( _roamDpy, _rubberGc, fg^bg );
    }

}

/*=====================================================================*/

void roamw_popdown ( void )
/************************************************************************
 * roamw_popdown                                             		*
 *                                                                      *
 * This function pops down the roam control popup window.    		*
 *                                                                      *
 * void roamw_popdown()                   				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      08/97  						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_roamPopW)) {
        XtUnmanageChild(_roamPopW);
        XtSetSensitive (_roamCtlBtn, TRUE);
    }
}

/*=====================================================================*/

void roamw_createMenu ( Widget parent )
/************************************************************************
 * roamw_createMenu							*
 *									*
 * Reads the roam defaults file then sets up the roam menu.		*
 *									*
 * void roamw_createMenu ( parent )  	                 		*
 *									*
 * Input parameters:                                                    *
 *	parent		Widget	ID of menubar to attach menu		*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		06/99	initial coding				*
 * M. Li/GSC		02/00	malloc(len)->malloc(len+1)		*
 * T. Piper/GSC		 6/01	freed menu_items			*
 * T. Piper/SAIC	04/03	changed values to global _roamValues	*
 * T. Piper/SAIC	04/03	removed to XtSetSensitive calls		*
 ***********************************************************************/
{
    int		ii, loglev, state, iret, ier, ncount, nctl;
    int		curr_val;
    char	record[256], grp[10];
    size_t	len;
    Boolean	not_done;
    FILE	*fp;
    _NXMmenuItem *menu_items;
/*---------------------------------------------------------------------*/

    not_done = TRUE;
    ncount = 2;		/* skip over "Fit to Window" and "Size of Image" */

/*
 * Attempt to open the file.
 */
    if ((fp = cfl_tbop (ROAM_TBL, "nmap", &ier)) == NULL) {
	loglev = 2;
        iret = -1;
	strcpy(grp, "NMAP_ROAM");
        er_lmsg (&loglev, grp, &iret, ROAM_TBL, &ier, strlen (grp),
                 strlen (ROAM_TBL));

	not_done = FALSE;
}

/* 
 * For every line in the table, read in the record,
 * parse out the fields, and compare to input type.
 */
    while (not_done) {
	cfl_trln (fp, sizeof(record), record, &state );

	switch (state) {
	  case 4:	/* EOF */
	    not_done = FALSE;
	    break;

	  case -3:	/* read failure */
	  case -6:	/* no file has been opened */
	  default:
	    not_done = FALSE;
	    break;

	  case 1:	/* line is too long */
	  case -24:	/* line is really too long */
	    /* do nothing */
	    break;

	  case 0:	/* good record */
	    sscanf (record, "%d", &curr_val);

	    if (0 < curr_val && curr_val < 21) {
		_roamValues[ncount++] = curr_val;

		if (ncount == (MAX_ROAMVAL - 1)) not_done = FALSE;
	    }

	    break;
	}
    }

    if (state != -6) {	/* no file has been opened */
	cfl_clos(fp, &ier);
    }

    nctl = ncount++;	/* add in control panel */
    menu_items = 
	(_NXMmenuItem *) XtMalloc ((size_t)(ncount + 1) * sizeof (_NXMmenuItem));

    for (ii = 0; ii < ncount; ii++) {
	if (ii == 0) {
	    strcpy (record, "Fit to Screen");
	    _roamValues[ii] = 0;
	}
	else if (ii == 1) {
	    strcpy (record, "Size of Image");
	    _roamValues[ii] = 1;
	}
	else if (ii == (nctl)) {
	    strcpy (record, "Control Panel");
	    _roamValues[ii] = CTL_PANEL;
	}
	else {
	    sprintf (record, "%d x Screen Size", _roamValues[ii]);
	}

	len = strlen (record);
	menu_items[ii].label = (char *) malloc (len + 1);
	strcpy (menu_items[ii].label, record);
	menu_items[ii].class = &xmPushButtonGadgetClass;
	menu_items[ii].mnemonic = 0;
	menu_items[ii].accelerator = NULL;
	menu_items[ii].accel_text = NULL;
	menu_items[ii].callback = (XtCallbackProc)roamw_menuCb;
	menu_items[ii].which_widget = (long)_roamValues[ii];
	menu_items[ii].subitems = NULL;
	menu_items[ii].sub_buttons = NULL;
    }

    menu_items[ncount].label = NULL;

/*
 * create Roam menu
 */
    _numRoam = ncount;
    _roamMb = (WidgetList) XtMalloc ((size_t)ncount * sizeof (Widget));
    NxmMenuPulldownBuild (parent, _roamMb, "Roam", 'R', menu_items);
    for (ii = 0; ii < ncount; ii++) {
        free(menu_items[ii].label);
    }
    XtFree((XtPointer)menu_items);

    _imgRoamBtn = _roamMb[1];
    _roamCtlBtn = _roamMb[nctl];
}

/*=====================================================================*/

void roamw_sensitive ( Boolean state )
/************************************************************************
 * roamw_sensitive                                             		*
 *                                                                      *
 * This function sets the sensitivity of the roam control window.    	*
 *                                                                      *
 * void roamw_sensitive ( state )              				*
 *                                                                      *
 * Input parameters:                                                    *
 *  state       Boolean   sensitivity                         		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI           08/97  						*
 * E. Safford/GSC	02/99	add check on _rbFlag for active drag    *
 ***********************************************************************/
{
/*
 *  End any "rubber band" or zoom box drag 
 */
    if ( _rbFlag )  {
        roamw_endRband();
    }
  
    if ( XtIsManaged(_roamPopW) ) {
        XtSetSensitive(_roamPopW, (int)state);
	XtSetSensitive(_roamCanvasW, (int)state);
    }
}

/*=====================================================================*/

void roamw_setWinSize ( int pxmw_w, int pxmw_h, int screen_w, int screen_h )
/************************************************************************
 * roamw_setWinSize                                              	*
 *                                                                      *
 * This function gets the real size of pixmap and screen window from    *
 * input, then calculates the window sizes and their left coordinates   *
 * in roam canvas area for each window, assuming the screen window is   *
 * at the upper left origin of the pixmap window.			*
 *                                                                      *
 * void roamw_setWinSize(pxmw_w, pxmw_h, screen_w, screen_h)            *
 *                                                                      *
 * Input parameters:                                                    *
 *  pxmw_w       int  pixmap window width                          	*
 *  pxmw_h       int  pixmap window hight                          	*
 *  screen_w     int  screen window width                          	*
 *  screen_h     int  screen window hight                          	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      08/97  						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( !pxmw_w || !pxmw_h || !screen_w || !screen_h) {
	    return;
    }

	if ( (float)pxmw_w/(float)pxmw_h > 
		(float)ROAMCANVAS_WDTH/(float)ROAMCANVAS_HGHT ) {

/*
 * fixed width
 */
	    _pxmwX    = PXMW_MARGIN;
	    _pxmwWdth = ROAMCANVAS_WDTH - 2*PXMW_MARGIN;

	    _winRatio   = (float)_pxmwWdth/(float)pxmw_w;

	    _pxmwHght = (int) ((float)pxmw_h * _winRatio);
	    _pxmwY    = (int) ((float)(ROAMCANVAS_HGHT - _pxmwHght)/2.0F);

	}
	else {

/*
 * fixed height
 */
	    _pxmwY    = PXMW_MARGIN;
	    _pxmwHght = ROAMCANVAS_HGHT - 2*PXMW_MARGIN;

	    _winRatio   = (float)_pxmwHght/(float)pxmw_h;

	    _pxmwWdth = (int) ((float)pxmw_w * _winRatio);
	    _pxmwX    = (int) ((float)(ROAMCANVAS_WDTH - _pxmwWdth)/2.0F);

	}

	_screenX    = _pxmwX;
	_screenY    = _pxmwY;
	_screenWdth = (Cardinal) ((float)screen_w * _winRatio);
	_screenHght = (Cardinal) ((float)screen_h * _winRatio);

	_rbLastX = _screenX;
	_rbLastY = _screenY;

}

/*=====================================================================*/

void roamw_setScreenXY ( int screen_x, int screen_y )
/************************************************************************
 * roamw_setScreenXY                                              	*
 *                                                                      *
 * This function gets the upper left origin of the screen window    	*
 *                                                                      *
 * void roamw_setScreenXY(screen_x, screen_y)            		*
 *                                                                      *
 * Input parameters:                                                    *
 *  screen_x     int  upper left screen window X                        *
 *  screen_y     int  upper left screen window Y                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      08/97  						*
 * J. Wu/SAIC      04/02	correct the rounding error		*
 ***********************************************************************/
{
    double dbltmp;

/*---------------------------------------------------------------------*/

	dbltmp = floor( (double)screen_x * (double)_winRatio + 0.5 );
	_screenX = _pxmwX + (int)dbltmp;
	dbltmp = floor( (double)screen_y * (double)_winRatio + 0.5 );
	_screenY = _pxmwY + (int)dbltmp;

	_rbLastX = _screenX;
	_rbLastY = _screenY;
}

/*=====================================================================*/

void roamw_setup ( int loop, Boolean view )
/************************************************************************
 * roamw_setup								*
 *									*
 * This function sets up the roam environment.				*
 *									*
 * void roamw_setup (loop, view)					*
 *									*
 * Input parameters:							*
 *	loop		int	the loop for which to set the value	*
 *	view		Boolean	to view or not to view			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		10/99	moved from mmenu_roamCb			*
 * H. Zeng/EAI          11/99   used loop_setRoamVal()                  *
 * S. Law/GSC		12/99	added check for seek window		*
 * E. Safford/GSC	04/00	add call to groam 			*
 * S. Law/GSC		05/00	removed seek window check		*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * E. Safford/GSC	03/01   reposition popdown call			*
 * T. Lee/GSC		03/01	changed call seq. of _set & _save roam	*
 * E. Safford/GSC	04/01   allow roam to size of image		*
 * E. Safford/GSC	04/01   make popdown conditional on view	*
 * H. Zeng/XTRIA	02/03   added checking of _roamWinOff		*
 * T. Piper/SAIC	04/03	Fix roam menu sensitivities logic	*
 * T. Piper/SAIC	04/03	Allow Size of Image for all images 	*
 * T. Piper/SAIC	04/03	Put gqbnd & roam calls inside view test	*
 * T. Piper/SAIC	06/06	Put back break in "Size of Image" case	*
 ***********************************************************************/
{
    float	xl, yb, xr, yt, ratio, fx, fy;
    int		dh, dw, ii, iret, irmflg, ityp, sw, sh, value, xx, yy;
/*---------------------------------------------------------------------*/
/*
 * get screen size
 */
    gqbnd(sys_S, &xl, &yb, &xr, &yt, &iret, strlen(sys_S));
    sw = (int) (xr - xl);
    sh = (int) (yb - yt);

    value = loop_getRoamVal(loop);
    loop_saveRoamVal(); 

    switch (value) {

      case 0:	/* Fit to Screen (no roam) */

	irmflg = 0;
	gsroam (&irmflg, &sw, &sh, &iret);
	break;

      case 1:     /* SIZE of IMAGE */

	if (!dataw_isImgInLoop(loop)) {
	    return;
	}
	else {
/*
 * get image size
 */
	    im_qsiz (&dw, &dh, &iret);
	    if (iret != 0 ) {
		return;
	    }
/*
 * check whether roam is needed
 */
	    irmflg = (dw > sw || dh > sh) ? 1 : 0;

	    gsroam (&irmflg, &dw, &dh, &iret);

	}

	break;

      default:		/* size multiplier */
	ratio = (float) sqrt ((double) value);
	dw = (int) ((float)sw * ratio);
	dh = (int) ((float)sh * ratio);

	irmflg = 1;
	gsroam (&irmflg, &dw, &dh, &iret);

	break;
    }

/*
 * The following code sets up the roam menu buttons and control panel.
 * It is only relevant to the currently viewed image.
 */
    if ( view ) {
	if (!irmflg) {
/* 
 * By definition, if no roam, then no roam control; hence call roam_popdown.
 * By definition, if no roam control, then set Control Panel button insensitive.
 * If in 'Fit to Screen' mode, then set button insensitive, else if in
 *	'Size of Image' mode, then set it insensitive.
 */
	    roamw_popdown();
	    roamw_enableMenu();
	    if ( value == 0 ) {  /* Fit to Screen (no roam) */
		XtSetSensitive (_roamMb[0], FALSE);
		if ( !dataw_isImgInLoop(loop) ) {
		    XtSetSensitive (_imgRoamBtn, FALSE);
		}
	    }
	    else { 
		XtSetSensitive (_imgRoamBtn, FALSE);
	    }
	    XtSetSensitive (_roamCtlBtn, FALSE);
	}
	else {
	    gqbnd (sys_D, &xl, &yb, &xr, &yt, &iret, strlen (sys_D));
	    dw = abs ((int) (xr - xl));
	    dh = abs ((int) (yt - yb));

	    xmroam_getPos (&xx, &yy);

	    roamw_setWinSize (dw, dh, sw, sh);
	    roamw_setScreenXY ((int) xx, (int) yy);

	    ityp = 0;
	    fx = (float)xx;
	    fy = (float)yy;
	    groam(&ityp, sys_D, &fx, &fy, &iret, strlen(sys_D));

	    if ( _roamWinUp == TRUE ) {
		roamw_popup();
		roamw_sensitive(TRUE);
	    }
/* 
 * Set sensitivity of Roam Buttons appropriately
 */
	
	    for ( ii = 0; ii < _numRoam; ii++ ) {
		if ( _roamValues[ii] != value ) { 
		    XtSetSensitive(_roamMb[ii], TRUE);
		}
		else {
		    XtSetSensitive(_roamMb[ii], FALSE);
		}
	    }
	    if ( !dataw_isImgInLoop(loop) ) {
		   XtSetSensitive (_imgRoamBtn, FALSE);
	    }
	    if ( roamw_isUp() ) {
		XtSetSensitive (_roamCtlBtn, FALSE);
	    }
	}
    }
}
/*=====================================================================*/

void roamw_ctlBtnSnstiv ( Boolean state )
/************************************************************************
 * roamw_ctlBtnSnstiv                                                   *
 *                                                                      *
 * This function sets the sensitivity of the "Control Panel" button     *
 * in the menu.    							*
 *                                                                      *
 * void roamw_ctlBtnSnstiv(state)                   		        *
 *                                                                      *
 * Input parameters:                                                    *
 *  state       Boolean   True = sensitive, False = not sensitive       *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI      11/99  						*
 ***********************************************************************/
{
    XtSetSensitive (_roamCtlBtn, (int)state);
}

/*=====================================================================*/

Boolean roamw_isUp ( void )
/************************************************************************
 * roamw_isUp                                             		*
 *                                                                      *
 * This function checks whether the roam control popup window is up.    *
 *                                                                      *
 * Boolean roamw_isUp()                   				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 * roamw_isUp	Boolean     True -- up,	False -- down                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      08/97  						*
 ***********************************************************************/
{
    return( XtIsManaged(_roamPopW) );
}

/*=====================================================================*/

void roamw_disableMenu ( void )
/************************************************************************
 * roamw_disableMenu                                          		*
 *                                                                      *
 * This function makes the entire roam menu insensitive.                *
 *                                                                      *
 * void roamw_disableMenu()                   				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/01	initial coding				*
 ***********************************************************************/
{
    int ii;
/*---------------------------------------------------------------------*/

    for (ii=0; ii < _numRoam; ii++) {
	XtSetSensitive (_roamMb[ii], FALSE);
    }
}

/*=====================================================================*/

void roamw_enableMenu ( void )
/************************************************************************
 * roamw_enableMenu                                          		*
 *                                                                      *
 * This function makes the entire roam menu sensitive.                	*
 *                                                                      *
 * void roamw_enableMenu()                   				*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/01	initial coding				*
 * E. Safford/GSC	04/01	mod for roam to size of image		*
 * T. Piper/SAIC	04/03	removed special case handling		*
 ***********************************************************************/
{
    int ii;
/*---------------------------------------------------------------------*/

    for (ii=0; ii < _numRoam; ii++) {
	XtSetSensitive (_roamMb[ii], TRUE);
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void roamw_menuCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * roamw_menuCb								*
 *									*
 * Callback function for ROAM menu on the menubar of the main window.	*
 *									*
 * static void roamw_menuCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		parent widget ID			*
 *	which	long		button ID				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI      	08/97  						*
 * C. Lin/EAI      	10/97 	add call to mcanvw_disarmDynamic()  	*
 * E. Safford/GSC	10/99	update for nmap2			*
 * S. Law/GSC		10/99	moved from mmenu_roamCb			*
 * E. Safford/GSC	10/99	dataw_getCurLoop -> loop_getCurLoop	*
 * E. Safford/GSC	11/99	use dsp_reloadLoop                      *
 * H. Zeng/EAI          11/99   use loop_setRoamVal()                   *
 * E. Safford/GSC	12/99	param change to dsp_reloadLoop		*
 * E. Safford/GSC	04/00	add call to dsp_updtDisplay		*
 * S. Law/GSC		05/00	added calls to seekw_saveGhost		*
 * E. Safford/GSC	05/00	update for frame tags         		*
 * E. Safford/GSC	07/00	add dsp_setBusy()             		*
 * T. Lee/GSC		03/01	changed call seq. of loop_setRoamVal	*
 * H. Zeng/XTRIA	02/03   added setting of _roamWinOff		*
 * M. Li/SAIC		12/03	Added a check for pgnumb_isUp		*
 * T. Piper/SAIC	12/04	Added aodtw_refresh & cldhgtw_refresh	*
 ***********************************************************************/
{
int		lp, ier;
/*---------------------------------------------------------------------*/

    dsp_setBusy (TRUE);

/*
 * disable the mouse button setups
 */
    mcanvw_disarmDynamic ();
    if (!pgnumb_isUp()) pgpalw_classPopdown ();
    pgpalw_setupOper ();

    seekw_saveGhost (TRUE);

    if (mapw_isUp ()) mapw_ctlBtnCb (NULL, 3, NULL);

    if (which == CTL_PANEL) {
        _roamWinUp = TRUE;
	roamw_popup();
    }
    else {
	lp = loop_getCurLoop();

/*
 *  Save bad frame tag settings
 */
	xmfrmtg_saveFrmTag ( lp, &ier );
        loop_setRoamVal(lp, (int)which);
	dsp_reloadLoop(lp, &ier);

/*
 *  Restore bad frame tag settings
 */
	xmfrmtg_restoreFrmTag ( lp, &ier );

	dsp_updtDisplay();

    }

    aodtw_refresh(TRUE);
    cldhgtw_refresh(TRUE);
    seekw_saveGhost (FALSE);

    dsp_setBusy (FALSE);
}

/*=====================================================================*/
/* ARGSUSED */
static void roamw_exposeCb ( Widget w, XtPointer clnt, 
				XmDrawingAreaCallbackStruct *call )
/************************************************************************
 * roamw_exposeCb                                                 	*
 *                                                                      *
 * Callback function for expose event of roam control window drawing    *
 * area.      								*
 *                                                                      *
 * static void roamw_exposeCb(w, clnt, call )                     	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  clnt          XtPointer  not used                                   *
 *  *call    XmDrawingAreaCallbackStruct  callback data struct     	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      08/97                                                *
 ***********************************************************************/
{
    XEvent	*event;
/*---------------------------------------------------------------------*/

    event = call->event;

    if(event->xexpose.count == 0) {
	roamw_redraw();
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void roamw_motionCb ( Widget w, XtPointer clnt, XEvent *event )
/************************************************************************
 * roamw_motionCb                                                 	*
 *                                                                      *
 * Callback function for mouse button press, release and motion in the  *
 * drawing canvas area of roam control window.				*
 *                                                                      *
 * static void roamw_motionCb(w, clnt, event )                         	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget	widget ID                               *
 *  clnt          XtPointer	not used                                *
 *  *event        XEvent	X event data struct    			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	08/97                                           *
 * C. Lin/EAI      	06/98   use xmotion.state (S.Danz/AWC)		*
 * E. Safford/GSC	02/99	add check on _rbFlag (Release & Motion) *
 * S. Law/GSC		05/00	added calls to seekw_saveGhost		*
 * E. Safford/SAIC	10/01	added call to pggst_redrawGhost		*
 * T. Piper/SAIC	12/04	added aodtw_refresh & cldhgtw_refresh	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    switch ( event->type ) {

        case ButtonPress:

/*
 * Check that ButtonPress is Button1.
 */
            if  ( event->xbutton.button  == Button1 ) {

                roamw_startRband( event->xbutton.x, event->xbutton.y );
            }
            break;

        case ButtonRelease:

/*
 * Check that ButtonRelease is Button1.
 */
            if  ( event->xbutton.button  == Button1 && _rbFlag ) {

		seekw_saveGhost (TRUE);

                roamw_endRband ();

		aodtw_refresh(TRUE);
		cldhgtw_refresh(TRUE);
		seekw_saveGhost (FALSE);

/*
 *  If in pgen and mid-draw (active dynamics), adjusting
 *  for the roam has wiped the ghost line -- redraw it.
 */
		if ( pgpalw_isUp() && mcanvw_getDynActFlag() ) {
		    pggst_redrawGhost();
		}
            }
            break;

        case MotionNotify:

            if  ( event->xmotion.state & Button1Mask && _rbFlag ) {

                roamw_trackRband( event->xmotion.x, event->xmotion.y );
            }
            break;
	}
}

/*=====================================================================*/
/* ARGSUSED */
static void roamw_arrowCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * roamw_arrowCb                                                 	*
 *                                                                      *
 * Callback function for arrow buttons. 				*
 *                                                                      *
 * static void roamw_arrowCb(w, which, call)                          	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w             Widget     widget ID                                  *
 *  which         long  	     which arrow button 			*
 *  call          XtPointer  not used     				*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      08/97                                                *
 * E. Safford/SAIC	10/01	added call to pggst_redrawGhost		*
 * T. Piper/SAIC	03/03	fixed the calculation of ix, iy		*
 ***********************************************************************/
{
int interval, sx, sy, ix, iy;

/*---------------------------------------------------------------------*/

	sx = _screenX;
	sy = _screenY;

	if ( which == 0 || which == 1 ) {

	    interval = (int)((float)_pxmwHght/ROAM_INCREMENT); 

	    if ( which == 0 ) {
		_screenY -= interval;
		if ( _screenY < _pxmwY)
			_screenY = _pxmwY;
	    }
	    else {
		_screenY += interval;
		if ( _screenY > _pxmwY + _pxmwHght - (int)_screenHght)
			_screenY = _pxmwY + _pxmwHght - (int)_screenHght;
	    }

	    _rbLastY = _screenY;
		    
	}
	else {

            interval = (int)((float)_pxmwWdth/ROAM_INCREMENT);

	    if ( which == 2 ) {
		_screenX -= interval;
		if ( _screenX < _pxmwX)
			_screenX = _pxmwX;
	    }
	    else {
		_screenX += interval;
		if ( _screenX > _pxmwX + _pxmwWdth - (int)_screenWdth )
			_screenX = _pxmwX + _pxmwWdth - (int)_screenWdth;
	    }

	    _rbLastX = _screenX;
	}

	if ( sx != _screenX || sy != _screenY ) {

/*
 * erase screen window using pixmap window color
 */
            XSetForeground( _roamDpy, _roamwGc, _pxmwColr );
            XDrawRectangle( _roamDpy, _roamWin, _roamwGc,
                        sx, sy, _screenWdth, _screenHght );

/*
 * draw new screen window
 */
            XSetForeground( _roamDpy, _roamwGc, _scrColr );
            XDrawRectangle( _roamDpy, _roamWin, _roamwGc,
                        _screenX, _screenY, _screenWdth, _screenHght );

	}

/*
 * reset the x and y of the view area in pixmap
 */
	ix = (int)((float)(_screenX - _pxmwX)/_winRatio);
	iy = (int)((float)(_screenY - _pxmwY)/_winRatio);
	roamw_newXY(ix, iy);

/*
 *  If in pgen and mid-draw (active dynamics), adjusting
 *  for the roam has wiped the ghost line -- redraw it.
 */
	if ( pgpalw_isUp() && mcanvw_getDynActFlag() ) {
	    pggst_redrawGhost();
	}
}

/*=====================================================================*/
/* ARGSUSED */
static void roamw_ctlBtnCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * roamw_ctlBtnCb                                                  	*
 *                                                                      *
 * Callback function for control buttons at the bottom area.      	*
 *                                                                      *
 * static void roamw_ctlBtnCb(w, which, call)                           *
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
 * C. Lin/EAI      08/97                                                *
 * H. Zeng/XTRIA   02/03    added setting of _roamWinOff		*
 ***********************************************************************/
{
/*
 * close button
 */
    _roamWinUp = FALSE;
    roamw_popdown ();
}

/*=====================================================================*/

static void roamw_redraw ( void )
/************************************************************************
 * roamw_redraw                                                  	*
 *                                                                      *
 * This function redraws the roam control area.      			*
 *                                                                      *
 * static void roamw_redraw()                          			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      08/97                                                *
 * T. Piper/SAIC	04/03	Added XClearWindow			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    if ( !_pxmwHght )
	return;

/*
 * draw pixmap window
 */
    XSetForeground( _roamDpy, _roamwGc, _pxmwColr );
    XClearWindow(_roamDpy, _roamWin);
    XFillRectangle( _roamDpy, _roamWin, _roamwGc, 
                    _pxmwX-1, _pxmwY-1, (Cardinal)_pxmwWdth+2,
                    (Cardinal)_pxmwHght+2 );

/*
 * draw screen window
 */
    XSetForeground( _roamDpy, _roamwGc, _scrColr );
    XDrawRectangle( _roamDpy, _roamWin, _roamwGc, 
		    _screenX, _screenY, _screenWdth, _screenHght );
}

/*=====================================================================*/

static void roamw_startRband ( int x, int y )
/************************************************************************
 * roamw_startRband                                                  	*
 *                                                                      *
 * This function starts the rubber banding.      			*
 *                                                                      *
 * static void roamw_startRband(x, y)                          		*
 *                                                                      *
 * Input parameters:                                                    *
 *  x         int        X coordinate of starting point                 *
 *  y         int        Y coordinate of starting point                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      08/97                                                *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	if( x >= _screenX && x <= (_screenX + (int)_screenWdth)
		&& y >= _screenY && y <= (_screenY + (int)_screenHght) ) {

/*
 * erase screen window using pixmap window color
 */
	    XSetForeground( _roamDpy, _roamwGc, _pxmwColr );
            XDrawRectangle( _roamDpy, _roamWin, _roamwGc,
                        _screenX, _screenY, _screenWdth, _screenHght );

/*
 * draw rubber band window
 */
            XDrawRectangle( _roamDpy, _roamWin, _rubberGc,
                        _screenX, _screenY, _screenWdth, _screenHght );

/*
 * set the reference point
 */
	    _rbRefX = x;
	    _rbRefY = y;

	    _rbStartX = _rbLastX;
	    _rbStartY = _rbLastY;

	    _rbFlag = True;

	}
	else
	    _rbFlag = False;

}

/*=====================================================================*/

static void roamw_trackRband ( int x, int y )
/************************************************************************
 * roamw_trackRband                                                  	*
 *                                                                      *
 * This function tracks the rubber banding.      			*
 *                                                                      *
 * static void roamw_trackRband(x, y)                          		*
 *                                                                      *
 * Input parameters:                                                    *
 *  x         int        X coordinate of current point                  *
 *  y         int        Y coordinate of current point                  *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	08/97                                           *
 * E. Safford/GSC	10/99	add geplot call				*
 * T. Piper/SAIC	03/03	fixed the calulation of xx, yy		*
 ***********************************************************************/
{
int     iret;
int	tempx, tempy;
int     xx, yy;
/*---------------------------------------------------------------------*/

	if ( !_rbFlag )
		return;

/*
 * erase the previous rectangle
 */
        XDrawRectangle( _roamDpy, _roamWin, _rubberGc,
                        _rbLastX, _rbLastY, _screenWdth, _screenHght );

	_rbLastX = _rbStartX + x - _rbRefX;
	_rbLastY = _rbStartY + y - _rbRefY;

/*
 * check boundary in X dimension
 */
	tempx =  _pxmwX + _pxmwWdth - (int)_screenWdth;
	if ( _rbLastX < _pxmwX ) 
	    _rbLastX = _pxmwX;
	else if ( _rbLastX > tempx ) 
	    _rbLastX = tempx;

/*
 * check boundary in Y dimension
 */
	tempy = _pxmwY + _pxmwHght - (int)_screenHght;
	if ( _rbLastY < _pxmwY ) 
	    _rbLastY = _pxmwY;
	else if ( _rbLastY > tempy ) 
	    _rbLastY = tempy;

/*
 * draw new rectangle
 */
        XDrawRectangle( _roamDpy, _roamWin, _rubberGc,
                        _rbLastX, _rbLastY, _screenWdth, _screenHght );

/*
 * roam the picture
 */
	xx = (int)((float)(_rbLastX - _pxmwX)/_winRatio);
	yy = (int)((float)(_rbLastY - _pxmwY)/_winRatio);
	xmroam_setPos (xx, yy);
	geplot(&iret);

}

/*=====================================================================*/

static void roamw_endRband ( void )
/************************************************************************
 * roamw_endRband                                                  	*
 *                                                                      *
 * This function ends the rubber banding.      				*
 *                                                                      *
 * static void roamw_endRband()                        			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	08/97                                           *
 * E. Safford/GSC	02/99	switch off _rbFlag 			*
 * T. Piper/SAIC	03/03	fixed the calculation of ix, iy		*
 ***********************************************************************/
{
int ix, iy;
/*---------------------------------------------------------------------*/

	if ( !_rbFlag )
		return;

/*
 * erase the previous screen rectangle
 */
        XDrawRectangle( _roamDpy, _roamWin, _rubberGc,
                        _rbLastX, _rbLastY, _screenWdth, _screenHght );

/*
 * draw new screen rectangle
 */
	XSetForeground( _roamDpy, _roamwGc, _scrColr );
        XDrawRectangle( _roamDpy, _roamWin, _roamwGc,
                        _rbLastX, _rbLastY, _screenWdth, _screenHght );

	_screenX = _rbLastX;
	_screenY = _rbLastY;

/*
 * reset the x and y of the view area in pixmap 
 */
        ix = (int)((float)(_rbLastX - _pxmwX)/_winRatio);
	iy = (int)((float)(_rbLastY - _pxmwY)/_winRatio);
	roamw_newXY(ix, iy);

	_rbFlag = FALSE;

}

/*=====================================================================*/

static void roamw_newXY ( int ix, int iy ) 
/************************************************************************
 * roamw_newXY                                                  	*
 *                                                                      *
 * This function resets the upper left corner device coordinates of     *
 * the screen based on the position of the screen window.		*
 *                                                                      *
 * static void roamw_newXY ( ix, iy )                 			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * ix	int	upper left corner device x coordinate			*
 * iy	int	upper left corner device y coordinate			*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	08/97                                           *
 * E. Safford/GSC	10/99	add geplot call				*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 ***********************************************************************/
{
    int   ityp, iret;
    float xx, yy;
/*---------------------------------------------------------------------*/

    xx = (float)ix;
    yy = (float)iy;

    ityp = 0;
    groam(&ityp, sys_D, &xx, &yy, &iret, strlen(sys_D));
    geplot(&iret); 

}

/*=====================================================================*/
/* ARGSUSED */
void roamw_arrowUp ( Widget w, XEvent *event, String *params, 
						Cardinal *num_params )
/************************************************************************
 * roamw_arrowUp                                             		*
 *                                                                      *
 * This function execute roamw_arrowCb() with client data 0.            *
 *                                                                      *
 * void  roamw_arrowUp ( w, event, params, num_params )                 *
 *                                                                      *
 * Input parameters:                                                    *
 *  w	     Widget     widget that caused action to be called		*
 *  *event    XEvent	event that caused action to be called		*
 *  *params   String    pointer to list of strings specified as action  *
 *                      arguments (not used)                            *
 *  *num_params  Cardinal   number of strings (not used)                *
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI      04/01    initial coding  				*
 * H. Zeng/EAI      05/01    added parameter list                       *
 * H. Zeng/SAIC	    01/07    changed to use mbotw_updateLocDspl		*
 ***********************************************************************/
{
int  np, iret;
float x, y, plat, plon;
/*---------------------------------------------------------------------*/
	roamw_arrowCb(NULL, 0, NULL);

/*
 * Update the Lat/Lon if the mouse focuses on the main drawing area.
 */
        if( w == mcanvw_getDrawingW() ) {

/*
 * get device coordinates
 */
            x = (float)event->xkey.x;
            y = (float)event->xkey.y;

/*
 * get lat/lon 
 */
	    np = 1;
            gtrans(sys_S, sys_M, &np, &x, &y, &plat, &plon,
                   &iret, strlen(sys_S), strlen(sys_M));

/*
 * display the lat/lon on the bottom of the main window 
 */
	    mbotw_updateLocDspl (plat, plon);

        }
}

/*=====================================================================*/
/* ARGSUSED */
void roamw_arrowDown ( Widget w, XEvent *event, String *params, 
						Cardinal *num_params )
/************************************************************************
 * roamw_arrowDown                                             		*
 *                                                                      *
 * This function execute roamw_arrowCb() with client data 1.            *
 *                                                                      *
 * void  roamw_arrowDown( w, event, params, num_params )                *
 *                                                                      *
 * Input parameters:                                                    *
 *  w	     Widget     widget that caused action to be called		*
 *  *event    XEvent	event that caused action to be called		*
 *  *params   String    pointer to list of strings specified as action  *
 *                      arguments (not used)                            *
 *  *num_params  Cardinal   number of strings (not used)                *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI      04/01    initial coding  				*
 * H. Zeng/EAI      05/01    added parameter list                       *
 * H. Zeng/SAIC	    01/07    changed to use mbotw_updateLocDspl		*
 ***********************************************************************/
{
int  np, iret;
float x, y, plat, plon;
/*---------------------------------------------------------------------*/
	roamw_arrowCb(NULL, 1, NULL);

/*
 * Update the Lat/Lon if the mouse focuses on the main drawing area.
 */
        if( w == mcanvw_getDrawingW() ) {

/*
 * get device coordinates
 */
            x = (float)event->xkey.x;
            y = (float)event->xkey.y;

/*
 * get lat/lon 
 */
	    np = 1;
            gtrans(sys_S, sys_M, &np, &x, &y, &plat, &plon,
                   &iret, strlen(sys_S), strlen(sys_M));

/*
 * display the lat/lon on the bottom of the main window 
 */
	    mbotw_updateLocDspl (plat, plon);

        }
}

/*=====================================================================*/
/* ARGSUSED */
void roamw_arrowLeft ( Widget w, XEvent *event, String *params, 
						Cardinal *num_params )
/************************************************************************
 * roamw_arrowLeft                                           		*
 *                                                                      *
 * This function execute roamw_arrowCb() with client data 2.            *
 *                                                                      *
 * void  roamw_arrowLeft ( w, event, params, num_params )               *
 *                                                                      *
 * Input parameters:                                                    *
 *  w	     Widget     widget that caused action to be called		*
 *  *event    XEvent	event that caused action to be called		*
 *  *params   String    pointer to list of strings specified as action  *
 *                      arguments (not used)                            *
 *  *num_params  Cardinal   number of strings (not used)                *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI      04/01    initial coding  				*
 * H. Zeng/EAI      05/01    added parameter list                       *
 * H. Zeng/SAIC	    01/07    changed to use mbotw_updateLocDspl		*
 ***********************************************************************/
{
int  np, iret;
float x, y, plat, plon;
/*---------------------------------------------------------------------*/
	roamw_arrowCb(NULL, 2, NULL);

/*
 * Update the Lat/Lon if the mouse focuses on the main drawing area.
 */
        if( w == mcanvw_getDrawingW() ) {

/*
 * get device coordinates
 */
            x = (float)event->xkey.x;
            y = (float)event->xkey.y;

/*
 * get lat/lon 
 */
	    np = 1;
            gtrans(sys_S, sys_M, &np, &x, &y, &plat, &plon,
                   &iret, strlen(sys_S), strlen(sys_M));

/*
 * display the lat/lon on the bottom of the main window 
 */
	    mbotw_updateLocDspl (plat, plon);

        }
}

/*=====================================================================*/
/* ARGSUSED */
void roamw_arrowRight ( Widget w, XEvent *event, String *params, 
						Cardinal *num_params )
/************************************************************************
 * roamw_arrowRight                                             	*
 *                                                                      *
 * This function execute roamw_arrowCb() with client data 3.            *
 *                                                                      *
 * void  roamw_arrowRight ( w, event, params, num_params )              *
 *                                                                      *
 * Input parameters:                                                    *
 *  w	     Widget     widget that caused action to be called		*
 *  event    XEvent*	event that caused action to be called		*
 *  params   String*    pointer to list of strings specified as action  *
 *                      arguments (not used)                            *
 *  num_params  Cardinal*   number of strings (not used)                *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI      04/01    initial coding  				*
 * H. Zeng/EAI      05/01    added parameter list                       *
 * H. Zeng/SAIC	    01/07    changed to use mbotw_updateLocDspl		*
 ***********************************************************************/
{
int  np, iret;
float x, y, plat, plon;
/*---------------------------------------------------------------------*/
	roamw_arrowCb(NULL, 3, NULL);

/*
 * Update the Lat/Lon if the mouse focuses on the main drawing area.
 */
        if( w == mcanvw_getDrawingW() ) {

/*
 * get device coordinates
 */
            x = (float)event->xkey.x;
            y = (float)event->xkey.y;

/*
 * get lat/lon 
 */
	    np = 1;
            gtrans(sys_S, sys_M, &np, &x, &y, &plat, &plon,
                   &iret, strlen(sys_S), strlen(sys_M));

/*
 * display the lat/lon on the bottom of the main window 
 */
	    mbotw_updateLocDspl (plat, plon);

        }
}
