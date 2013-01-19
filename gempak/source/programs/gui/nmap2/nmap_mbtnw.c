#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "nmap_mainw.h"
#include "nmap_data.h"


#define ICON_WIDTH  32  /* action icon width, hight */
#define ICON_HEIGHT 32  

#define LOOP_ICON_W 24
#define LOOP_ICON_H 24

#define ICON_FGNAME       "white"
#define ICON_BGNAME       "blue"
#define ICON_WORK_BGNAME  "red"

#define LOOP_ICON_FG      "yellow"
#define LOOP_ICON_BG      "black"

#define ICON_DIR    "$NAWIPS/icons/nmap"

#define CTL_BTNS    	 10 
#define MAX_FNAME_SZ	256

#define CLICK_OUT	  0
#define CLICK_IN   	  1


typedef struct {
    Widget      wid;
    char        name[MAX_FNAME_SZ];
} button_t;

static button_t		_ctlBtns[CTL_BTNS];

static struct pxmBuf	_autoPxm[3];
static Boolean		_autoUpdt;
static Boolean		_autoUpdtLock = FALSE;
static Boolean		_loopKeysActive = TRUE;

static Widget 		_mbtnZoomRc, _loopOptn;
static WidgetList	_loopBtn;

static Pixel		_armColor, _bgColor, _topColor, _botColor;

Widget			_rc1;

static char		_aodtvers[10][LLMXLN];

/*
 *  Private callback functions
 */
void    mbtnw_clearLoopCb  ( Widget, XtPointer, XtPointer );
void 	mbtnw_loopSelBtnCb ( Widget, XtPointer, XtPointer );
void 	mbtnw_pushbCb      ( Widget, XtPointer, XtPointer );
void	mbtnw_autoMenuCb   ( Widget, XtPointer, XtPointer );
void 	mbtnw_aodtVerCb    ( Widget, XtPointer, XtPointer );

/*
 * private functions
 */
void	mbtnw_autoUpdtSnstv ( Boolean state );
Widget	mbtnw_mapBtnCreate ( Widget parent );
void	mbtnw_setBtnClick ( Widget btn, int state );
void    mbtnw_setLoopKeys ( Boolean state );
void	mbtnw_setupAutoUpdt ( Boolean sensitive, int value );


/************************************************************************
 * nmap_mbtnw.c								*
 *									*
 * This module creates the push_button widgets and defines the callback	*
 * functions in the main window for nmap.				*
 *									*
 * CONTENTS:								*
 *      mbtnw_create		create the iconized push buttons.	*
 *      mbtnw_zoomSensitive	set ZOOM/UNZOOM button snsitiv/insnsitiv*
 *	mbtnw_setMbtns		set the loop buttons & auto update	*
 *	mbtnw_loopSetSensitive	sets the data loop buttons' sensitvity	*
 *									*
 *      mbtnw_pushbCb		callback function for the push buttons.	*
 *      mbtnw_loopSelBtnCb	callback function for frame loop buttons*
 *      mbtnw_clearLoopCb       callback for loop clear confirmation    *
 *									*
 *	mbtnw_setLoopOne	sets the first loop			*
 *	mbtnw_setLoopTwo	sets the second loop			*
 *	mbtnw_setLoopThree	sets the third loop			*
 *	mbtnw_setLoopFour	sets the fourth loop			*
 *	.	.	.	.	.	.			*
 *	.	.	.	.	.	.			*
 *	.	.	.	.	.	.			*
 *	mbtnw_setLoopSixteen	sets the sixteen loop			*
 *									*
 *      mbtnw_mapBtnCreate	create the map buttons. 		*
 *      mbtnw_setupAutoUpdt	set auto update button state & sensitv	*
 *      mbtnw_autoUpdtSnstv	set auto update button snsitiv/insnsitiv*
 *	mbtnw_setLoopKeys     	sets the loop key (F1-F4) availability	*
 *	mbtnw_autoMenuCb	callback function for auto menu		*
 *	mbtnw_autoUpdtToggle	auto update hot key function		*
 *	mbtnw_autoLockToggle	auto update lock hot key function	*
 ***********************************************************************/

/*=====================================================================*/

Widget mbtnw_create ( Widget parent )
/************************************************************************
 * mbtnw_create                                              		*
 *                                                                      *
 * This function creates the icon push_buttons for the main window.    	*
 *                                                                      *
 * Widget mbtnw_create(parent)                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                               *
 *                                                                      *
 * Output parameters:                                                   *
 * mbtnw_create	Widget  Widget ID of the form container                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		04/96						*
 * C. Lin/EAI		02/97	use NxmBusy_createBtns for busy/stop	*
 * S. Wang/GSC		03/97	add print button			*
 * C. Lin/EAI		07/97	add clear data button			*
 * S. Law/GSC		01/99	added seek widget creation		*
 * E. Safford/GSC	03/99	added auto_update button & clean up	*
 * E. Safford/GSC	04/99	initialize auto-update to insensitive	*
 * E. Safford/GSC	04/99	add reload button                    	*
 * E. Safford/GSC	09/99	add loop buttons                     	*
 * E. Safford/GSC	03/00	temporarily comment out the wipe btn 	*
 * E. Safford/GSC	03/00	add call to cvg_load                 	*
 * E. Safford/GSC	04/00	don't make loop btns insensitive      	*
 * E. Safford/GSC	05/00	change lp controls to popup menu      	*
 * R. Tian/SAIC		11/02	add auto update lock			*
 * R. Tian/SAIC		01/03	add flag to NxmBxmBtn_create(Multi)	*
 * H. Zeng/XTRIA	01/04	added AODT button			*
 * M. Li/SAIC		06/05	Added AODT version selection		*
 * M. Li/SAIC		07/05	free -> G_FREE				*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    char	iconfile[256], *name_ptr, iconf[256]; 
    char	on_file[256], off_file[256], lock_file[256], insens_file[256];
    char	label[25];
    int		jj, iret, nv, mlen;
    long	ii, ignore;
    Widget	form, rc2, loopframe; 
    Widget	loop_pulldn;
    struct bxmInfo  auto_bxm[3];

    char 	*ctl_btns[]   = { "data", "auto", "map", "pgen", "print",
    				  "wipe", "reload", "seek", "cldhgt",
				  "aodt" };
    char	*ctl_label[]  = { "data selection", "auto update", 
    			  	  "map editing", "product generation", 
				  "print", "clear data", 
				  "reload this frame", "seek",
				  "cloud height", "hurricane track" };
    char	*auto_items[] =	{ "Auto-Update Lock", 
                                  "Auto-Update Unlock" };
    char	**temp;
    Boolean	isnAODT = True;
/*---------------------------------------------------------------------*/

    form = XtVaCreateWidget( "mbtnw_form",
	    	xmFormWidgetClass, 		parent,
		XmNfractionBase,		50,
	        NULL);

/*
 *  Create first group of icons
 */
    _rc1 = XtVaCreateWidget( "mbtnw_pushb1Rc",
	   	xmRowColumnWidgetClass, form,
		XmNtopAttachment,       XmATTACH_FORM,
		XmNleftAttachment,      XmATTACH_FORM,
		XmNbottomAttachment,    XmATTACH_FORM,
		XmNorientation,         XmHORIZONTAL,
		NULL);

    for (ii=0; ii < CTL_BTNS; ii++) {

	strcpy ( _ctlBtns[ii].name, ctl_label[ii] );
	name_ptr = _ctlBtns[ii].name;

	if (ii == 2) {
	    mbtnw_mapBtnCreate(_rc1);
	}
	else if (ii == 1) {
	    cfl_inqr("insns.xbm", ICON_DIR, 
	    			&ignore, insens_file, &iret);
	    cfl_inqr ("auto_on.xbm", ICON_DIR, 
	    			&ignore, on_file, &iret);
	    cfl_inqr ("auto_off.xbm", ICON_DIR, 
	    			&ignore, off_file, &iret);
	    cfl_inqr ("auto_lock.xbm", ICON_DIR, 
	    			&ignore, lock_file, &iret);

	    for (jj=0; jj < 2; jj++) {
	        strcpy (auto_bxm[jj].fgcolor, ICON_FGNAME);
	        strcpy (auto_bxm[jj].bgcolor, ICON_BGNAME);
	    }
	    strcpy (auto_bxm[2].fgcolor, ICON_FGNAME);
	    strcpy (auto_bxm[2].bgcolor, ICON_WORK_BGNAME);

	    auto_bxm[0].sens_bits = off_file;
	    auto_bxm[1].sens_bits = on_file;
	    auto_bxm[2].sens_bits = lock_file;
  	    auto_bxm[0].insens_bits = insens_file; 
	    auto_bxm[1].insens_bits = insens_file; 
	    auto_bxm[2].insens_bits = insens_file; 

	    _ctlBtns[ii].wid = (Widget) NxmBxmBtn_createMulti ( _rc1,
	    		name_ptr, NULL, ICON_WIDTH, ICON_HEIGHT,
  			auto_bxm, 3, name_ptr, False,
                        (XtCallbackProc)mbtnw_pushbCb, (XtPointer)1, _autoPxm);

	    NxmPushBtnMenu_create(_ctlBtns[ii].wid, 0L, auto_items, 2,
				  (XtCallbackProc)mbtnw_autoMenuCb); 
	}
	else {
	    sprintf ( iconf, "%s.xbm", ctl_btns[ii] );
	    cfl_inqr ( iconf, ICON_DIR, &ignore, iconfile, &iret ); 
	    if ( ii == 9 ) isnAODT = False;
	    
	    _ctlBtns[ii].wid = (Widget) NxmBxmBtn_create (
		    _rc1, name_ptr, NULL, ICON_WIDTH, ICON_HEIGHT,
		    ICON_FGNAME, ICON_BGNAME, NULL,
		    iconfile, name_ptr, isnAODT, (XtCallbackProc)mbtnw_pushbCb, 
                    (XtPointer)ii);	

	    if ( ii == 9 ) {
		aodtw_getnumvers ( &nv, &mlen );
		aodtw_getversnm ( &temp );

		for ( jj = 0; jj < nv; jj++ ) {
		    cst_rxbl ( temp[jj], temp[jj], &mlen, &iret );
                    strcpy ( _aodtvers[jj], temp[jj] );
                }

                NxmPushBtnMenu_create(_ctlBtns[ii].wid, 0L, temp, nv,
                                  (XtCallbackProc)mbtnw_aodtVerCb);

		for ( jj = 0; jj < nv; jj++ ) {
		    G_FREE ( temp[jj], char );
                }
		G_FREE ( temp, char* );
	    }
	}
    }

    XtManageChild(_rc1);

/*
 *  Create loop popup menu
 */
    loop_pulldn   = XmCreatePulldownMenu (form, "loop_pulldn", NULL, 0);
    _loopOptn     = XmCreateOptionMenu (form, "_loopOptn", NULL, 0);
    _loopBtn      = (WidgetList)XtMalloc(MAX_LOOP*sizeof(Widget));

    for (ii=0; ii < MAX_LOOP; ii++) {
	sprintf (label, " %ld ", ii+1);
	_loopBtn[ii] = XtVaCreateManagedWidget(label,
		xmPushButtonWidgetClass,	loop_pulldn,
		NULL);
	XtAddCallback(_loopBtn[ii],		XmNactivateCallback,
		(XtCallbackProc)mbtnw_loopSelBtnCb, (XtPointer)ii);
    }

    XtVaSetValues (_loopOptn,
    		XmNsubMenuId,			loop_pulldn,
       		XmNleftAttachment,		XmATTACH_POSITION,  
  		XmNleftPosition,		21,  
  		XmNtopAttachment,		XmATTACH_FORM, 
  		XmNtopOffset,			6, 
		NULL);
    NxmLabel_setStr(_loopOptn, "Loop:  ");
    XtManageChild(_loopOptn);


/*
 *  Create zoom, unzoom buttons 
 */
    _mbtnZoomRc = zoomw_create(form);
    XtVaSetValues(_mbtnZoomRc, 
		XmNrightAttachment,    		XmATTACH_POSITION,
		XmNrightPosition,         	31,
		XmNtopAttachment,   		XmATTACH_FORM,
		NULL);
    XtManageChild(_mbtnZoomRc); 

/*
 *  Create second group of icons
 */
    rc2 = XtVaCreateWidget( "mbtnw_pushb2Rc",
		xmRowColumnWidgetClass, 	form,
		XmNtopAttachment,       	XmATTACH_FORM,
		XmNrightAttachment,     	XmATTACH_FORM,
		XmNbottomAttachment,    	XmATTACH_FORM,
		XmNorientation,         	XmHORIZONTAL,
		NULL);

/*
 *  Create loop control buttons
 */
    loopframe = XtVaCreateManagedWidget("mbtnw_loopFrame",
					xmFrameWidgetClass,  form,
					XmNrightAttachment,  XmATTACH_POSITION,
					XmNrightPosition,    44,
					XmNtopAttachment,    XmATTACH_FORM,
					XmNtopOffset,        6,
					NULL);
    loopw_create(loopframe); 
    mbtnw_loopSetSensitive(TRUE);

/*
 *  Create stop/busy buttons
 */
    NxmBusy_createBtns(rc2);
    _autoUpdt = TRUE; 
    NxmBxmBtn_setPxm (_ctlBtns[1].wid,
			_autoPxm[(int)_autoUpdt].snstv, 
			_autoPxm[(int)_autoUpdt].insnstv);
    mbtnw_autoUpdtSnstv (FALSE);
    XtManageChild(rc2); 

    XtManageChild(form);

    return(form);
}

/*=====================================================================*/

void mbtnw_zoomSensitive ( Boolean state )
/************************************************************************
 * mbtnw_zoomSensitive                                              	*
 *                                                                      *
 * This function makes the zoom in & out buttons sensitive/insensitive.	*
 *                                                                      *
 * void mbtnw_zoomSensitive( state )                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *	state	Boolean	 True=sensitive, False=Insensitive		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      11/96  						*
 ***********************************************************************/
{
    XtSetSensitive(_mbtnZoomRc, state);
    XmUpdateDisplay(_mbtnZoomRc);
}

/*=====================================================================*/

void mbtnw_setMbtns ( void )
/************************************************************************
 * mbtnw_setMbtns                                               	*
 *                                                                      *
 * This function updates the state of the loop selection buttons and  	*
 * the auto update settings for the current loop.			*
 *                                                                      *
 * void mbtnw_setMbtns ( )                        			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	09/99	initial coding				*
 * E. Safford/GSC	10/99	dataw_getCurLoop -> loop_getCurLoop	*
 * E. Safford/GSC	02/00	dsp_isLoopActv -> dsp_isLoopLoaded 	*
 * E. Safford/GSC	04/00	don't make loop btns insensitive    	*
 * E. Safford/GSC	05/00	use dataw_isImgDom, not _isImgInLoop	*
 * E. Safford/GSC	05/00	change lp controls to popup menu	*
 * E. Safford/GSC	05/01	add dataw_useRefTm to set auto updt     *
 ***********************************************************************/
{
int	cur_lp;
Boolean	auto_avail, auto_status;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();

/*
 *  Set the loop menu 
 */
    XtVaSetValues (_loopOptn,
		XmNmenuHistory,		_loopBtn[cur_lp],
		NULL);

/*
 *  Set Auto-Update
 *     the auto-update control is available (true) if there is a dominant 
 *		image source and the data window is using the current time,
 *		not a ref time
 *     the control is set to on (true) if the data window was set
 *		on for auto-update and the control is determined to be 
 *		available. 
 */
    auto_avail  = dataw_isImgDom(cur_lp) && !dataw_useRefTm(cur_lp);
    auto_status = loop_getAutoUpdt(cur_lp) && auto_avail;

    mbtnw_setupAutoUpdt (auto_avail, auto_status);
}

/*=====================================================================*/

void mbtnw_loopSetSensitive ( Boolean state )
/************************************************************************
 * mbtnw_loopSetSensitive                                              	*
 *                                                                      *
 * This function makes the data loop selection buttons sensitive or    	*
 * insensitive.								*
 *                                                                      *
 * void mbtnw_loopSetSensitive( state )               			*
 *                                                                      *
 * Input parameters:                                                    *
 *	state	Boolean	 True=sensitive, False=Insensitive		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding				*
 * E. Safford/GSC	05/00	make loop control a popup menu		*
 ***********************************************************************/
{
    XtSetSensitive (_loopOptn, state);
    mbtnw_setLoopKeys(state); 
}

/*=====================================================================*/
/* ARGSUSED */
void mbtnw_pushbCb ( Widget wid, XtPointer which, XtPointer call )
/************************************************************************
 * mbtnw_pushbCb							*
 *									*
 * Callback function for push buttons.					*
 *									*
 * void mbtnw_pushbCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	XtPointer	which button				*
 *	call	XtPointer	callback struct				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		05/96						*
 * C. Lin/EAI		02/97	take out case for stop/busy buttons     *
 * S. Wang/GSC		03/97	add print button			*
 * C. Lin/EAI		07/97	add clear data button			*
 * C. Lin/EAI		07/97	allow printing without data		*
 * C. Lin/EAI		10/97	NxmPaletteSh->pgpalw_popup		*
 * C. Lin/EAI		04/98	add WORK_FILE checking, and message	*
 * C. Lin/EAI		04/98	pop down the data window for clear_data *
 * S. Law/GSC		12/98	added parameter to dataw_clearData	*
 * S. Law/GSC		12/98	added seek popup			*
 * E. Safford/GSC	03/99	added auto update ctl btn		*
 * E. Safford/GSC	04/99	turn off auto-update with clear data	*
 * E. Safford/GSC	04/99	add frame reload                    	*
 * T. Piper/GSC		05/99	removed _autoUpdt = FALSE in case 5	*
 * H. Zeng/EAI          09/99   added cloud height                      *
 * E. Safford/GSC	10/99	use loop_setAutoUpdt                	*
 * S. Jacobs/NCEP	11/99	Reactivated the print button		*
 * S. Jacobs/NCEP	11/99	Reactivated the reload button		*
 * S. Law/GSC		12/99	added seek popdowns			*
 * E. Safford/GSC	02/00	force pxm refresh on Reloads		*
 * E. Safford/GSC	03/00	temporarily renumber & comment out Wipe *
 * E. Safford/GSC	05/00	return wipe				*
 * E. Safford/GSC	07/00	add dsp_setBusy calls			*
 * E. Safford/GSC	08/00	add geplot on frame reload 		*
 * E. Safford/GSC	09/00	add reorder & check for blank to reload *
 * E. Safford/GSC	02/01	update param list on NxmExit_create     *
 * E. Safford/GSC	05/01   fix bug in reload on last frame		*
 * J. Wu/SAIC		11/01	add param in cvg_load()	calling		*
 * J. Wu/SAIC		12/01	add layer in cvg_load()	call		*
 * J. Wu/SAIC		12/01	replace cvg_load() with cvg_redraw()	*
 * R. Tian/SAIC		11/02	add _autoUpdtLock check			*
 * H. Zeng/XTRIA        12/02   modified to call loopw_stopLooping()    *
 * T. Lee/SAIC		11/03	added user directory to work_file	*
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * T. Lee/SAIC		12/03	enabled environ. variable in prefs.tbl	*
 * H. Zeng/XTRIA	01/04   added AODT button			*
 * H. Zeng/SAIC		04/04   added version for aodtw_popup()		*
 * M. Li/SAIC		03/05	removed dataw_getIRInfo for case 8 & 9	*
 * M. Li/SAIC		06/05	Added AODT version selection		*
 ***********************************************************************/
{
    Widget	draw_w, drawingw;
    char	mesg1[] = "Unable to invoke PRODUCT GENERATION.\n\n"
			"1) Product generation already in use.\n"
	    		" or \n"
			"2) Cleanup of previous NMAP error required.\n"
		    	"    type <cleanvgf> in current directory\n";
    
    char        clear_msg[256], newpath[LLPATH];
    char	clear_title[] = "Clear Loop Confirmation";
    int  	cur_lp, ifrm, ier, nfrm;
    struct stat buf;
    Boolean     lpflg;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop();

    switch((long)which) {

      case 0:     	/* DATA */
	seekw_popdown ();
    	dataw_popup();   
	break;

      case 1:     	/* Auto Update */
	if(_autoUpdtLock)
	    break;

        _autoUpdt = !_autoUpdt;
	NxmBxmBtn_setPxm (_ctlBtns[1].wid,
	    		  _autoPxm[(int)_autoUpdt].snstv, 
			  _autoPxm[(int)_autoUpdt].insnstv);

	loop_setAutoUpdt(cur_lp, _autoUpdt);

	auto_startAutoUpdt();
        break;

      case 2:     	/* MAP */
	seekw_popdown ();
  	mapw_popup(); 
	break;

      case 3:    	/* PRODUCT GENERATION */
/*
 *  If WORK_FILE already exists, do not 
 *  activate product generation
 */
	css_envr ( cvg_getworkfile(), newpath, &ier );
  	if ( stat(newpath, &buf) != 0 ) {
  	    pgpalw_popup();
	}
	else {
	    draw_w = (Widget)mcanvw_getDrawingW();
	    NxmWarn_show(draw_w, mesg1);
	}
	break;

      case 4:		/* PRINT */

/*
 *  If there is no data in the current loop, stop any existing 
 *  looping to avoid the double print bug.
 */
        lpflg = dataw_isLoopActv ( cur_lp );
        if ( !lpflg ) {
             loopw_stopLooping();
        }

	seekw_popdown (); 
	NxmPrt_prtWPopup(); 
	break;
  
      case 5:  		/* CLEAR DATA */
        sprintf (clear_msg, 
	         "Do you really want to remove all data in loop %d?", 
						(loop_getCurLoop() + 1));

        NxmExit_create (wid, clear_title, clear_msg, 
			mbtnw_clearLoopCb, NULL);
	break;

      case 6:      	/* Reload Frame */

	ifrm = loop_getCurFrame ();
	nfrm = loop_getNumFrames(cur_lp);

/*
 *   No reload if current frame is the base map (frame 0) or if it's
 *   out of range.              
 */
	if (ifrm <= 0 || ifrm >= nfrm) {
            return;
	}

        dsp_setBusy(TRUE);

	dsp_reloadFrame ( cur_lp, ifrm );

	seekw_popdown ();

	if ( pgpalw_isUp() ) {
	    cvg_redraw ( NULL, &ier );
        }
	
	geplot(&ier);
        dsp_setBusy(FALSE);

        break;

      case 7:  		/* SEEK */
  	seekw_popup (); 
	break;

      case 8:  		/* CLOUD HEIGHT */
	seekw_popdown ();
	cldhgtw_popup();
	break;

      case 9:  		/* AODT */
	aodtw_setvers ( _aodtvers[0], &ier );
	if ( !aodtw_isUp() ) {
    	    drawingw = (Widget)mcanvw_getDrawingW();
    	    aodtw_create ( drawingw );
    	    aodtw_popup ();
	}
	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mbtnw_autoMenuCb ( Widget wid, XtPointer which, XtPointer call )
/************************************************************************
 * mbtnw_autoMenuCb							*
 *									*
 * Callback function for auto menu.					*
 *									*
 * void mbtnw_autoMenuCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	XtPointer	which button				*
 *	call	XtPointer	callback struct				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * R. Tian/SAIC		10/02						*	
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    switch((long)which) {

      case 0:     	/* Auto Update Lock */
	if(_autoUpdtLock)
	    break;

	auto_stopAutoUpdt();

	_autoUpdtLock = TRUE;
	loop_saveAutoUpdt();
	for ( ii = 0; ii < MAX_LOOP; ++ii ) {
	    loop_setAutoUpdt(ii, FALSE);
	}

	NxmBxmBtn_setPxm (_ctlBtns[1].wid,
			_autoPxm[2].snstv, 
			_autoPxm[2].insnstv);
        break;

      case 1:     	/* Auto Update Unlock */
	if(!_autoUpdtLock)
	    break;

	_autoUpdtLock = FALSE;
	loop_restoreAutoUpdt();
	_autoUpdt = loop_getAutoUpdt(loop_getCurLoop());

	NxmBxmBtn_setPxm (_ctlBtns[1].wid,
			_autoPxm[(int)_autoUpdt].snstv, 
			_autoPxm[(int)_autoUpdt].insnstv);

	auto_startAutoUpdt();
        break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mbtnw_aodtVerCb ( Widget wid, XtPointer which, XtPointer call )
/************************************************************************
 * mbtnw_aodtVerCb                                                      *
 *                                                                      *
 * Callback function for aodt versions menu.                            *
 *                                                                      *
 * void mbtnw_aodtVerCb (wid, which, call)                              *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      which   XtPointer       which button                            *
 *      call    XtPointer       callback struct                         *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC          	06/05                                           *       
 ***********************************************************************/
{
    int		ier;
    Widget	drawingw;
/*---------------------------------------------------------------------*/
    aodtw_setvers ( _aodtvers[(long)which], &ier );
    if ( !aodtw_isUp() ) {
    	drawingw = (Widget)mcanvw_getDrawingW();
    	aodtw_create ( drawingw );
    	aodtw_popup ();
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mbtnw_loopSelBtnCb ( Widget wid, XtPointer which, XtPointer call )
/************************************************************************
 * mbtnw_loopSelBtnCb							*
 *									*
 * Callback function for frame loop selection buttons.			*
 *									*
 * void mbtnw_loopSelBtnCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	XtPointer	which button				*
 *	call	XtPointer	callback struct				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	09/99	initial coding                      	*
 * S. Jacobs/NCEP	10/99	Added reset of LUT for images		*
 * E. Safford/GSC	10/99	fix bug in page frame time update	*
 * S. Law/GSC		10/99	changed xmloop_switchLoop parameters	*
 * E. Safford/GSC	12/99	add call to xpgrfrsh if in prod gen	*
 * S. Law/GSC		01/00	added a check to dsp_isLoopActv		*
 * E. Safford/GSC	02/00	dsp_isLoopActv -> dsp_isLoopLoaded 	*
 * E. Safford/GSC	03/00	use loop_restoreLut to reset LUT   	*
 * E. Safford/GSC	04/00	mod for default map in all loops      	*
 * E. Safford/GSC	05/00	mod to correctly handle pgen       	*
 * S. Law/GSC		05/00	added calls to seekw_saveGhost		*
 * S. Law/GSC		06/00	rebuild mstr pixmaps on a loop change	*
 * S. Law/GSC		06/00	added call to mcanvw_setLatlon		*
 * S. Law/GSC		06/00	moved functionality to loop_changeLoop	*
 * E. Safford/GSC	07/00	added CurLoop check on which		*
 ***********************************************************************/
{
    if ( loop_getCurLoop() != (long)which) {
        loop_changeLoop ((long)which);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mbtnw_clearLoopCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * mbtnw_clearLoopCb 							*
 *									*
 * Callback function for the clear loop button confirmation.		*
 *									*
 * void mbtnw_clearLoopCb (wid, clnt, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	client data			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/GSC	05/00	initial coding                      	*
 * E. Safford/GSC	05/00	param change on dsp_setProj		*
 * E. Safford/GSC	07/00	add dsp_setBusy calls			*
 * E. Safford/GSC	08/00	call loop_changeLoop() to reset		*
 * M. Li/GSC		09/00	added zoomw_clearZoom			*
 * M. Li/GSC		03/01	dsp_setProj -> nmp_setmap		*
 * E. Safford/SAIC	10/01	reset map only if img src in data	*
 ***********************************************************************/
{
int	 cur_lp, ier;
nmpstr_t mapnms[MAX_MAP];
Boolean	 had_img;
/*---------------------------------------------------------------------*/

    dsp_setBusy(TRUE);

    cur_lp  = loop_getCurLoop();
    had_img = dataw_isImgInLoop(cur_lp);

/*
 *  Disable auto-update and stop any active looping
 */
    mbtnw_autoUpdtSnstv( FALSE );
    loopw_stop();

    seekw_popdown ();
    dataw_popdown();

/*
 *  Clear all loop data
 */
    dataw_clearLoop (cur_lp);  

/*
 *  Reload default base map -- only if img src was in loop
 */
    if (had_img) {
  	nmp_sdefmap (cur_lp, &ier);
	nmp_sproj (cur_lp, &ier);

        zoomw_clearZoom(cur_lp);
        nmp_gmapnms(mapnms, &ier);
        nmp_setmap(mapnms[0], cur_lp, False, &ier);
    }

    dsp_reloadLoop (cur_lp, &ier);
    loop_changeLoop(cur_lp); 
    dsp_setBusy(FALSE);
}

/*=====================================================================*/

/************************************************************************
 * mbtnw_setLoopOne, mbtnw_setLoopTwo, mbtnw_setLoopThree,		*
 * mbtnw_setLoopFour...mbtnw_setLoopSixteen				*
 *									*
 * Set of wrapper functions to facilitate the definition of the loop	*
 * selection shortcut keys.						*
 *									*
 * mbtnw_setLoopOne	-- set loop one					*
 * mbtnw_setLoopTwo	-- set loop two					*
 * mbtnw_setLoopThree	-- set loop three				*
 * mbtnw_setLoopFour	-- set loop four				*
 * mbtnw_setLoopFive	-- set loop five				*
 * mbtnw_setLoopSix	-- set loop six					*
 * mbtnw_setLoopSeven	-- set loop seven				*
 * mbtnw_setLoopEight	-- set loop eight				*
 * mbtnw_setLoopNine	-- set loop nine	       			*
 * mbtnw_setLoopTen	-- set loop ten					*
 * mbtnw_setLoopEleven	-- set loop eleven				*
 * mbtnw_setLoopTwelve	-- set loop twelve				*
 * mbtnw_setLoopThirteen-- set loop thirteen				*
 * mbtnw_setLoopFourteen-- set loop fourteen				*
 * mbtnw_setLoopFifteen	-- set loop fifteen				*
 * mbtnw_setLoopSixteen	-- set loop sixteen				*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/00	initial coding				*
 * E. Safford/GSC	04/00	add check to _loopKeysActive		*
 * E. Safford/GSC	05/00	add wrappers for 5-8        		*
 * S. Law/GSC		06/00	changed to use loop_changeLoop		*
 * E. Safford/GSC	10/00	add check on current loop		*
 * H. Zeng/XTRIA	03/04	added more loops			*
 * H. Zeng/SAIC		03/07	added loop layer link capability	*
 ***********************************************************************/
void mbtnw_setLoopOne ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 0) {
	    loop_changeLoop (0);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(0) ) {
        pglayrw_setActvLayer( 0, &iret );
    }
}

void mbtnw_setLoopTwo ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 1) {
	    loop_changeLoop (1);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(1) ) {
        pglayrw_setActvLayer( 1, &iret );
    }
}

void mbtnw_setLoopThree ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 2) {
	    loop_changeLoop (2);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(2) ) {
        pglayrw_setActvLayer( 2, &iret );
    }
}

void mbtnw_setLoopFour ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 3) {
	    loop_changeLoop (3);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(3) ) {
        pglayrw_setActvLayer( 3, &iret );
    }
}

void mbtnw_setLoopFive ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 4) {
	    loop_changeLoop (4);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(4) ) {
        pglayrw_setActvLayer( 4, &iret );
    }
}

void mbtnw_setLoopSix ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 5) {
	    loop_changeLoop (5);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(5) ) {
        pglayrw_setActvLayer( 5, &iret );
    }
}

void mbtnw_setLoopSeven ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 6) {
	    loop_changeLoop (6);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(6) ) {
        pglayrw_setActvLayer( 6, &iret );
    }
}

void mbtnw_setLoopEight ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 7) {
	    loop_changeLoop (7);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(7) ) {
        pglayrw_setActvLayer( 7, &iret );
    }
}

void mbtnw_setLoopNine ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 8) {
	    loop_changeLoop (8);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(8) ) {
        pglayrw_setActvLayer( 8, &iret );
    }
}

void mbtnw_setLoopTen ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 9) {
	    loop_changeLoop (9);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(9) ) {
        pglayrw_setActvLayer( 9, &iret );
    }
}

void mbtnw_setLoopEleven ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 10) {
	    loop_changeLoop (10);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(10) ) {
        pglayrw_setActvLayer( 10, &iret );
    }
}

void mbtnw_setLoopTwelve ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 11) {
	    loop_changeLoop (11);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(11) ) {
        pglayrw_setActvLayer( 11, &iret );
    }
}

void mbtnw_setLoopThirteen ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 12) {
	    loop_changeLoop (12);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(12) ) {
        pglayrw_setActvLayer( 12, &iret );
    }
}

void mbtnw_setLoopFourteen ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 13) {
	    loop_changeLoop (13);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(13) ) {
        pglayrw_setActvLayer( 13, &iret );
    }
}

void mbtnw_setLoopFifteen ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 14) {
	    loop_changeLoop (14);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(14) ) {
        pglayrw_setActvLayer( 14, &iret );
    }
}

void mbtnw_setLoopSixteen ( void )
{
    int  	iret;
/*---------------------------------------------------------------------*/
    if (_loopKeysActive) {
        if ( loop_getCurLoop() != 15) {
	    loop_changeLoop (15);
	}
    }

    if ( mmenuw_lllValGet() && pglayrw_isUp() && pglayer_getInUse(15) ) {
        pglayrw_setActvLayer( 15, &iret );
    }
}

/*=====================================================================*/

Widget mbtnw_mapBtnCreate ( Widget parent )
/************************************************************************
 * mbtnw_mapBtnCreate                                              	*
 *                                                                      *
 * This function creates the icon push_button for the map editing.    	*
 *                                                                      *
 * Widget mbtnw_mapBtnCreate(parent)                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                               *
 *                                                                      *
 * Output parameters:                                                   *
 * mbtnw_mapBtnCreate	Widget    Widget ID of the push button        	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI  		04/96  						*
 * R. Tian/SAIC		01/03	add flag to NxmBxmBtn_create(Multi)	*
 ***********************************************************************/
{
Widget button;
char   iconfile[256];
int    iret;
long   ignore;
static char map_label[30];

/*---------------------------------------------------------------------*/

    cfl_inqr("map.xbm", ICON_DIR, &ignore, iconfile, &iret);
    strcpy( map_label, "map editing");

    button = NxmBxmBtn_create( parent,
		"map", NULL, 32, 32, 
		ICON_FGNAME , ICON_BGNAME, NULL, 
		iconfile, map_label, True,
		(XtCallbackProc)mbtnw_pushbCb, (XtPointer)2 );
    return(button);
}

/*=====================================================================*/

void mbtnw_setupAutoUpdt ( Boolean sensitive, int value )
/************************************************************************
 * mbtnw_setupAutoUpdt                                              	*
 *                                                                      *
 * This function sets the auto update button to sensitive/insensitive 	*
 * and sets the value to on, off, or no change.				*
 *                                                                      *
 * void mbtnw_setupAutoUpdt( sensitive, value )       			*
 *                                                                      *
 * Input parameters:                                                    *
 *  sensitive 	Boolean		True = sensitive, False = insensitive	*
 *  value	int		-1 = no change, 0 = Off, 1 = On		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding				*
 * R. Tian/SAIC		11/02	add _autoUpdtLock check			*
 ***********************************************************************/
{
    if (value > -1 && value < 2) { 
    	if(_autoUpdtLock) {
	    NxmBxmBtn_setPxm (_ctlBtns[1].wid,
			_autoPxm[2].snstv, 
			_autoPxm[2].insnstv);
    	    _autoUpdt = FALSE;
	}
	else {
            NxmBxmBtn_setPxm (_ctlBtns[1].wid,
			    _autoPxm[value].snstv, 
			    _autoPxm[value].insnstv);
    	    _autoUpdt = value;
	}
    }
    mbtnw_autoUpdtSnstv (sensitive);
}

/*=====================================================================*/

void mbtnw_autoUpdtSnstv ( Boolean state )
/************************************************************************
 * mbtnw_autoUpdtSnstv                                              	*
 *                                                                      *
 * This function sets the auto update button to sensitive/insensitive.	*
 *                                                                      *
 * void mbtnw_autoUpdtSnstv( state )                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *	state	Boolean		True = sensitive, False = insensitive	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/99	initial coding				*
 ***********************************************************************/
{
    XtSetSensitive (_ctlBtns[1].wid, state); 
    XmUpdateDisplay (_ctlBtns[1].wid); 
}

/*=====================================================================*/

void mbtnw_setBtnClick ( Widget btn, int state )
/************************************************************************
 * mbtnw_setBtnClick                                              	*
 *                                                                      *
 * This function sets a loop button to either the clicked "in" or       *
 * clicked "out" state.                                       		*
 *                                                                      *
 * void mbtnw_setBtnClick ( btn, state )               			*
 *                                                                      *
 * Input parameters:                                                    *
 *	btn		Widget		button to be modified		*
 *	state		int		O = CLICK_OUT, 1 = CLICK_IN	*
 *									*
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	09/99	initial coding				*
 ***********************************************************************/
{
    if (state == CLICK_OUT) {
	XtVaSetValues( btn,
		XmNarmColor,			_armColor,
		XmNbackground,			_bgColor,
		XmNtopShadowColor,		_topColor,
		XmNbottomShadowColor,		_botColor,
		NULL);
    }
    else {
	XtVaSetValues( btn,
		XmNarmColor,			_bgColor,
		XmNbackground,			_armColor,
		XmNtopShadowColor,		_botColor,
		XmNbottomShadowColor,		_topColor,
		NULL);
    }
}

/*=====================================================================*/

void mbtnw_setLoopKeys ( Boolean state )
/************************************************************************
 * mbtnw_setLoopKeys                                               	*
 *                                                                      *
 * This function updates the state of the loop selection buttons and  	*
 * the auto update settings for the current loop.			*
 *                                                                      *
 * void mbtnw_setLoopKeys ( state )                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *	state		Boolean	  value for loop keys (F1-F8 loop keys)	*
 *				  True means available, False disables  *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	04/00	initial coding				*
 ***********************************************************************/
{
    _loopKeysActive = state;
}

/*=====================================================================*/

void mbtnw_autoUpdtToggle ( void )
/************************************************************************
 * mbtnw_autoUpdtToggle                                                 *
 *                                                                      *
 * Wrapper function to facilitate hot keys to toggle auto update.	*
 *                                                                      *
 * void mbtnw_autoUpdtToggle ( void )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		11/02 						*
 ***********************************************************************/
{
    if(_autoUpdtLock)
        return;

    _autoUpdt = !_autoUpdt;
    NxmBxmBtn_setPxm (_ctlBtns[1].wid,
                      _autoPxm[(int)_autoUpdt].snstv,
                      _autoPxm[(int)_autoUpdt].insnstv);

    loop_setAutoUpdt(loop_getCurLoop(), _autoUpdt);

    auto_startAutoUpdt();
}

/*=====================================================================*/

void mbtnw_autoLockToggle ( void )
/************************************************************************
 * mbtnw_autoLockToggle                                                 *
 *                                                                      *
 * Wrapper function to facilitate hot keys to toggle auto update lock.	*
 *                                                                      *
 * void mbtnw_autoLockToggle ( void )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		11/02 						*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    _autoUpdtLock = !_autoUpdtLock;

    if(_autoUpdtLock) {
	auto_stopAutoUpdt();
	loop_saveAutoUpdt();
	for ( ii = 0; ii < MAX_LOOP; ++ii ) {
	    loop_setAutoUpdt(ii, FALSE);
	}

	NxmBxmBtn_setPxm (_ctlBtns[1].wid,
			_autoPxm[2].snstv, 
			_autoPxm[2].insnstv);
    }
    else {
	loop_restoreAutoUpdt();
	_autoUpdt = loop_getAutoUpdt(loop_getCurLoop());

	NxmBxmBtn_setPxm (_ctlBtns[1].wid,
			_autoPxm[(int)_autoUpdt].snstv, 
			_autoPxm[(int)_autoUpdt].insnstv);

	auto_startAutoUpdt();
    }
}

/*=====================================================================*/

void mbtnw_CtlBtnsSetSensitive ( Boolean state )
/************************************************************************
 * mbtnw_CtlBtnsSetSensitive                                           	*
 *                                                                      *
 * This function makes the control buttons sensitive or insensitive.	*
 *                                                                      *
 * void mbtnw_CtlBtnsSetSensitive ( state )               		*
 *                                                                      *
 * Input parameters:                                                    *
 *	state	Boolean	 True=sensitive, False=Insensitive		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	02/07						*
 ***********************************************************************/
{
    XtSetSensitive ( _rc1, state );
}
