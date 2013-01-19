#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"	/* frame info */


#define ICON_DIR "$NAWIPS/icons/nmap"

#define BUTTON_WDTH	24
#define BUTTON_HGHT	24
#define MAX_FNAME_SZ	30

#define NOT_LOOPING	-1

#define LOOP_BTNS	8
static char _loopLabels[LOOP_BTNS][20];

static Widget	_hidePb;
static Widget	_floopPb;
static Widget	_bloopPb;
static Widget	_rockPb;

static Widget	_loopRc;   /* widget ID for loop button 
					RowColumn container */

static struct pxmBuf	_hidePxm[2];

static Pixel	_armColor;
static Pixel	_bgdColor;
static Pixel	_topColor;
static Pixel	_botColor;

static Boolean 	_hideFlag	= FALSE;
static Boolean	_suspdFlag	= FALSE;

static int	_loopingCmd	= NOT_LOOPING;


/*
 *  private callback functions
 */
void loopw_loopBtnCb ( Widget wid, long which, XtPointer call );

/*
 *  private functions
 */
void loopw_setHide ( Boolean hflag );
void loopw_setShadow ( Boolean setflg );
void loopw_suspend ( Boolean sflag );


/************************************************************************
 * nmap_loopw.c                                                         *
 *									*
 * This module creates the loop buttons and defines the callback        *
 * functions for nmap.							*
 *									*
 * CONTENTS:                                                            *
 *	loopw_create()		creates the loop buttons.           	*
 *	loopw_sensitive()	set sensitivity for the loop buttons.	*
 *	loopw_resetHide()	reset the hide/show button & flag 	*
 *	loopw_stop()		halt any looping operation		*
 *									*
 *	loopw_loopBtnCb()	callback function of loop buttons.	*
 *      loopw_isLoopActv()      true if an active loop is in progress   *
 *	loopw_stopLooping()     stops any active looping command        *
 *      loopw_isHideActv()      true if the current loop is hidden      *
 *									*
 *	loopw_back()		step back wrapper function.		*
 *	loopw_frwd()		step foreward wrapper function.		*
 *	loopw_lpfd()		loop foreward wrapper function.		*
 *	loopw_rock()		rock wrapper function.			*
 *	loopw_last()		last frame wrapper function.		*
 *									*
 *	loopw_setShadow()	sets the pushbutton's shadow.		*
 *	loopw_suspend()		sets the suspend flag			*
 *	loopw_setHide()		set the hide/show button & flag.	*
 ***********************************************************************/

/*=====================================================================*/

void loopw_create ( Widget parent )
/************************************************************************
 * loopw_create                                              		*
 *                                                                      *
 * This function creates the loop buttons row column.    		*
 *                                                                      *
 * void loopw_create(parent)    	               			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                               *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	11/96   initial coding				*
 * E. Safford/GSC	11/98	add hide/show to frame loop		*
 * E. Safford/GSC	12/98	revamp loop buttons, fix hide/show label*
 * E. Safford/GSC	02/99	fix appearance of hide/show buttons	*
 * E. Safford/GSC	04/99	fix insensitive state of hide/show	*
 * S. Law/GSC		01/00	rearranged button creation		*
 * R. Tian/SAIC 	01/03   add True flag to NxmBxmBtn_create(Multi)*
 ***********************************************************************/
{
    int		ii, iret;
    long	ignore;
    char	iconfile[256], insnsfile[256], alt_iconfile[256];
    char	*name_ptr;
    Widget	wid;
    struct bxmInfo	hide_bxm[2];
/*---------------------------------------------------------------------*/

    /*
     * create looping buttons
     */
    _loopRc = XtVaCreateWidget("loopw_loopRc",
			       xmRowColumnWidgetClass,  parent,
			       XmNorientation,          XmHORIZONTAL,
			       XmNspacing,              0,
			       XmNmarginHeight,         0,
			       NULL);

    cfl_inqr("loopinsns.xbm", ICON_DIR, &ignore, insnsfile, &iret);

    /* 
     *  Load each of the loop control buttons.  The only trick is
     *  with Hide/Show, which are alternate pixmaps for button 0.
     */ 

    ii = 0;

    /* 
     * the Hide/Show button
     */
    strcpy (_loopLabels[ii], "hide frames");
    name_ptr = _loopLabels[ii];

    cfl_inqr ("hide_loop.xbm", ICON_DIR, &ignore, iconfile, &iret);
    cfl_inqr ("show_loop.xbm", ICON_DIR, &ignore, alt_iconfile, &iret);

    strcpy (hide_bxm[0].fgcolor, "black" ); 
    strcpy (hide_bxm[1].fgcolor, "black" ); 
    strcpy (hide_bxm[0].bgcolor, "yellow" ); 
    strcpy (hide_bxm[1].bgcolor, "yellow" ); 

    hide_bxm[0].sens_bits   = iconfile;
    hide_bxm[1].sens_bits   = alt_iconfile;
    hide_bxm[0].insens_bits = insnsfile;
    hide_bxm[1].insens_bits = insnsfile; 

    _hidePb = 
	(Widget) NxmBxmBtn_createMulti (_loopRc, name_ptr, NULL, 
					BUTTON_WDTH, BUTTON_HGHT,
					hide_bxm, 2, name_ptr, True,
                                        (XtCallbackProc)loopw_loopBtnCb,
					(XtPointer)ANIM_HIDE_DATA, _hidePxm);

    XtVaSetValues(_hidePb,
		  XmNmarginHeight,        0,
		  XmNmarginWidth,         0,
		  XmNshadowThickness,     3,
		  NULL);
    ii++;

    /* 
     * the rest of the buttons
     */
    strcpy (_loopLabels[ii], "show first frame");
    name_ptr = _loopLabels[ii];

    cfl_inqr("firstfm.xbm", ICON_DIR, &ignore, iconfile, &iret);

    wid = 
	(Widget) NxmBxmBtn_create(_loopRc, name_ptr, NULL, 
				  BUTTON_WDTH, BUTTON_HGHT, 
				  "black" , "yellow", insnsfile, 
				  iconfile, name_ptr, True,
                                  (XtCallbackProc)loopw_loopBtnCb, 
				  (XtPointer) ANIM_FRST_FRAM);

    XtVaSetValues(wid,
		  XmNmarginHeight,        0,
		  XmNmarginWidth,         0,
		  XmNshadowThickness,     3,
		  NULL);
	
    ii++;

    strcpy (_loopLabels[ii], "loop backward");
    name_ptr = _loopLabels[ii];

    cfl_inqr("loopback.xbm", ICON_DIR, &ignore, iconfile, &iret);

    _bloopPb = 
	(Widget) NxmBxmBtn_create(_loopRc, name_ptr, NULL, 
				  BUTTON_WDTH, BUTTON_HGHT, 
				  "black" , "yellow", insnsfile, 
				  iconfile, name_ptr, True,
                                  (XtCallbackProc)loopw_loopBtnCb, 
				  (XtPointer)ANIM_LOOP_BACK);

    XtVaSetValues(_bloopPb,
		  XmNmarginHeight,        0,
		  XmNmarginWidth,         0,
		  XmNshadowThickness,     3,
		  NULL);
	
    ii++;

    strcpy (_loopLabels[ii], "step backward");
    name_ptr = _loopLabels[ii];

    cfl_inqr("stepback.xbm", ICON_DIR, &ignore, iconfile, &iret);

    wid = 
	(Widget) NxmBxmBtn_create(_loopRc, name_ptr, NULL, 
				  BUTTON_WDTH, BUTTON_HGHT, 
				  "black" , "yellow", insnsfile, 
				  iconfile, name_ptr, True,
                                  (XtCallbackProc)loopw_loopBtnCb, 
				  (XtPointer)ANIM_STEP_BACK);

    XtVaSetValues(wid,
		  XmNmarginHeight,        0,
		  XmNmarginWidth,         0,
		  XmNshadowThickness,     3,
		  NULL);
	
    ii++;

    strcpy (_loopLabels[ii], "step forward");
    name_ptr = _loopLabels[ii];

    cfl_inqr("stepfrwd.xbm", ICON_DIR, &ignore, iconfile, &iret);

    wid = 
	(Widget) NxmBxmBtn_create(_loopRc, name_ptr, NULL, 
				  BUTTON_WDTH, BUTTON_HGHT, 
				  "black" , "yellow", insnsfile, 
				  iconfile, name_ptr, True,
                                  (XtCallbackProc)loopw_loopBtnCb, 
				  (XtPointer)ANIM_STEP_FORW);

    XtVaSetValues(wid,
		  XmNmarginHeight,        0,
		  XmNmarginWidth,         0,
		  XmNshadowThickness,     3,
		  NULL);
	
    ii++;

    strcpy (_loopLabels[ii], "loop forward");
    name_ptr = _loopLabels[ii];

    cfl_inqr("loopfrwd.xbm", ICON_DIR, &ignore, iconfile, &iret);

    _floopPb = 
	(Widget) NxmBxmBtn_create(_loopRc, name_ptr, NULL, 
				  BUTTON_WDTH, BUTTON_HGHT, 
				  "black" , "yellow", insnsfile, 
				  iconfile, name_ptr, True,
                                  (XtCallbackProc)loopw_loopBtnCb, 
				  (XtPointer)ANIM_LOOP_FORW);

    XtVaSetValues(_floopPb,
		  XmNmarginHeight,        0,
		  XmNmarginWidth,         0,
		  XmNshadowThickness,     3,
		  NULL);
	
    ii++;

    strcpy (_loopLabels[ii], "rock");
    name_ptr = _loopLabels[ii];

    cfl_inqr("rock.xbm", ICON_DIR, &ignore, iconfile, &iret);

    _rockPb = 
	(Widget) NxmBxmBtn_create(_loopRc, name_ptr, NULL, 
				  BUTTON_WDTH, BUTTON_HGHT, 
				  "black" , "yellow", insnsfile, 
				  iconfile, name_ptr, True,
                                  (XtCallbackProc)loopw_loopBtnCb, 
				  (XtPointer)ANIM_ROCK_LOOP);

    XtVaSetValues (_rockPb,
		   XmNmarginHeight,        0,
		   XmNmarginWidth,         0,
		   XmNshadowThickness,     3,
		   NULL);
	
    ii++;

    strcpy (_loopLabels[ii], "show last frame");
    name_ptr = _loopLabels[ii];

    cfl_inqr("lastfm.xbm", ICON_DIR, &ignore, iconfile, &iret);

    wid = 
	(Widget) NxmBxmBtn_create(_loopRc, name_ptr, NULL, 
				  BUTTON_WDTH, BUTTON_HGHT, 
				  "black" , "yellow", insnsfile, 
				  iconfile, name_ptr, True,
                                  (XtCallbackProc)loopw_loopBtnCb, 
				  (XtPointer)ANIM_LAST_FRAM);

    XtVaSetValues(wid,
		  XmNmarginHeight,        0,
		  XmNmarginWidth,         0,
		  XmNshadowThickness,     3,
		  NULL);
	
    ii++;


    XtManageChild(_loopRc);

    XtVaGetValues (wid,
		   XmNarmColor,			&_armColor,
		   XmNbackground,		&_bgdColor,
		   XmNtopShadowColor,		&_topColor,
		   XmNbottomShadowColor,	&_botColor,
		   NULL);


}

/*=====================================================================*/

void loopw_sensitive ( Boolean state )
/************************************************************************
 * loopw_sensitive                                                  	*
 *                                                                      *
 * This function turns on/off the loop control buttons as per the value *
 * of state.                                                         	*
 *                                                                      *
 * void loopw_sensitive(state)                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *	state	Boolean		True=sensitive, False=insensitive	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	11/96                                           *
 * E. Safford/GSC	04/00	stop & resume looping as per state      *
 * E. Safford/GSC	06/01	no stop/resume loop if _hideFlag is true*
 * T. Piper/SAIC	07/03	renamed xmloop -> xmloop_loop		*
 * T. Piper/SAIC	07/03	added mmenuw_loopStopGet		*
 ***********************************************************************/
{

    XtSetSensitive(_loopRc, (int)state);
    XmUpdateDisplay(_loopRc);

    if ( (_loopingCmd != NOT_LOOPING) && (!_hideFlag) ) {
	
        if ( state ) {
            xmloop_loop(_loopingCmd, mmenuw_loopStopGet());
        }
        else {
    	    xmloop_loop(ANIM_STOP_LOOP, mmenuw_loopStopGet()); 
        }
    }
}

/*=====================================================================*/

void loopw_resetHide ( void )
/************************************************************************
 * loopw_resetHide							*
 *									*
 * This function sets the hide/show icon to the default (hide) setting.	*
 *									*
 * void loopw_resetHide ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *                      NONE                                            *
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	10/00	initial coding				*
 * H. Zeng/EAI          06/01   added _hideFlag                         *
 ***********************************************************************/
{
    if (_hideFlag || _suspdFlag) { 
        loopw_loopBtnCb (NULL, ANIM_HIDE_DATA, NULL);
    }
}

/*=====================================================================*/

void loopw_stop ( void )
/************************************************************************
 * loopw_stop                                                    	*
 *                                                                      *
 * This function halts any active looping command.  It differs from     *
 * loopw_suspend, in that the _suspdFlag flag is not set (indeed if it  *
 * has been the suspended command is wiped out).                        *
 *                                                                      *
 * void loopw_stop ( )                                                  *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	01/00	initial coding                          *
 * S. Law/GSC		01/00	changed to xwprm defines		*
 * T. Piper/SAIC	07/03	renamed xmloop -> xmloop_loop		*
 * T. Piper/SAIC        07/03   added mmenuw_loopStopGet                *
 ***********************************************************************/
{
    xmloop_loop(ANIM_STOP_LOOP, mmenuw_loopStopGet());
    _suspdFlag = FALSE;
}

/*=====================================================================*/
/* ARGSUSED */
void loopw_loopBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * loopw_loopBtnCb							*
 *									*
 * Callback function for loop buttons.					*
 *									*
 * void loopw_loopBtnCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		button index				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI      	04/96   initial coding                          *
 * E. Safford/GSC	11/98	add Hide/Show and clean up		*
 * E. Safford/GSC	12/98	convert to global _hideFlag		*
 * E. Safford/GSC	12/98	update _loopBtn params to ClickIn/Out	*
 * E. Safford/GSC	02/99	reverse flag logic, add suspend/resume	*
 * S. Law/GSC		01/00	general cleanup				*
 * E. Safford/GSC	04/00	do nothing if loop has no data        	*
 * E. Safford/GSC	04/00	redisplay seek line as needed		*
 * S. Law/GSC		05/00	changed to use seekw_saveGhost		*
 * S. Law/GSC		06/00	removed unused cwin			*
 * S. Law/GSC		06/00	added additional loopw_setShadow call	*
 * E. Safford/GSC	10/00	removed check on loaded data		*
 * T. Piper/SAIC        07/03   renamed xmloop -> xmloop_loop           *
 * T. Piper/SAIC        07/03   added mmenuw_loopStopGet                *
 * T. Piper/SAIC        12/04   added aodtw_refresh & cldhgtw_refresh   *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    /*
     * turn off any current looping
     */
    if (_loopingCmd != NOT_LOOPING) {
	loopw_setShadow (FALSE);
	xmloop_loop(ANIM_STOP_LOOP, mmenuw_loopStopGet());

	_suspdFlag = (Boolean)(which == ANIM_HIDE_DATA);

	aodtw_refresh(FALSE);
	cldhgtw_refresh(FALSE);
	seekw_saveGhost (FALSE);

	if (_loopingCmd == which) {
	    _loopingCmd = NOT_LOOPING;
	    return;
	}
    }

    seekw_saveGhost (TRUE);

    /*
     * set _loopingCmd, if not suspended
     */
    if (!_suspdFlag) {
	if (which == ANIM_LOOP_FORW || which == ANIM_LOOP_BACK || 
	    which == ANIM_ROCK_LOOP) {

	    _loopingCmd = (int)which;

	    loopw_setShadow (TRUE);
	}
	else {
	    _loopingCmd = NOT_LOOPING;
	}
    }

    /*
     * hide or show the data
     */
    if (which == ANIM_HIDE_DATA) {
    
        loopw_setHide ((Boolean)(!_hideFlag));
  
	if (!_hideFlag && _suspdFlag) {
	    _suspdFlag = FALSE;
	    which = (long)_loopingCmd;
	    loopw_setShadow (TRUE);
	}
    }
    else if (_hideFlag) {
	loopw_setHide (FALSE);
    }

    xmloop_loop((int)which, mmenuw_loopStopGet());

    /*
     *  Redisplay seek line as needed
     */
    if (_loopingCmd == NOT_LOOPING) {
	aodtw_refresh(FALSE);
	cldhgtw_refresh(FALSE);
	seekw_saveGhost (FALSE);
    }

}

/*=====================================================================*/

Boolean loopw_isLoopActv ( void )
/************************************************************************
 * loopw_isLoopActv                                                    	*
 *                                                                      *
 * This function returns TRUE if there is an active loop command in     *
 * progress.  An active loop means the user has selected loop forward,  *
 * loop backward, or rock.  FALSE is returned if none of these buttons  *
 * are currently set.							*
 *                                                                      *
 * Boolean loopw_isLoopActv ( )                                         *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	loopw_isLoopActv	Boolean		True - loop active	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	05/00	initial coding                          *
 ***********************************************************************/
{
    return ((Boolean)(_loopingCmd != NOT_LOOPING));
}


/*=====================================================================*/

void loopw_stopLooping ( void )
/************************************************************************
 * loopw_stopLooping                                                 	*
 *                                                                      *
 * This function stops any current active loop command. An active loop  *
 * means the user has selected loop forward, loop backward, or rock.    *
 *                                                                      *
 * void loopw_stopLooping ( )                                           *
 *                                                                      *
 * Input parameters :							*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	             NONE						*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA        12/02   initial coding                          *
 ***********************************************************************/
{
   if (_loopingCmd != NOT_LOOPING) {
        loopw_loopBtnCb (NULL, _loopingCmd, NULL);

   }

}

/*=====================================================================*/

Boolean loopw_isHideActv ( void )
/************************************************************************
 * loopw_isHideActv                                                    	*
 *                                                                      *
 * This function returns TRUE if the current loop has been switched     *
 * to the Hide state.							*
 *                                                                      *
 * Boolean loopw_isHideActv ( )                                         *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	loopw_isHideActv	Boolean		True - loop active	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	06/01	initial coding                          *
 ***********************************************************************/
{
    return (_hideFlag);
}


/*=====================================================================*/

void loopw_back ( void )
/************************************************************************
 * loopw_back, loopw_frwd, loopw_lpfd, loopw_rock, loopw_last           *
 * loopw_hide           						*
 *                                                                      *
 * Set of wrapper functions to facilitate the definition of animation   *
 * shortcut key.                                                        *
 *                                                                      *
 * loopw_back -- step back                                              *
 * loopw_frwd -- step foreward                                          *
 * loopw_lpfd -- loop foreward                                          *
 * loopw_rock -- rock                                                   *
 * loopw_last -- last							*
 * loopw_hide -- hide/show data toggle					*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI           10/96	initial coding                          *
 * E. Safford/GSC	11/98	add #defines to wrappers		*
 * E. Safford/GSC	12/98	fix loopw_back				*
 * E. Safford/GSC	02/99	add loopw_last				*
 * S. Law/GSC		01/00	changed to xwprm defines		*
 * J. Wu/GSC		04/01	add loopw_hide				*
 ***********************************************************************/
{
        loopw_loopBtnCb(NULL, ANIM_STEP_BACK, NULL);
}

void loopw_frwd ( void )
{
        loopw_loopBtnCb(NULL, ANIM_STEP_FORW, NULL);
}

void loopw_lpfd ( void )
{
        loopw_loopBtnCb(NULL, ANIM_LOOP_FORW, NULL);
}

void loopw_rock ( void )
{
        loopw_loopBtnCb(NULL, ANIM_ROCK_LOOP, NULL);
}

void loopw_last ( void )
{
        loopw_loopBtnCb(NULL, ANIM_LAST_FRAM, NULL);
}

void loopw_hide ( void )
{
        loopw_loopBtnCb(NULL, ANIM_HIDE_DATA, NULL);
}

/*=====================================================================*/

void loopw_setShadow ( Boolean setflg )
/************************************************************************
 * loopw_setShadow							*
 *									*
 * This function sets the shadow of the pushbutton to make it look like	*
 * clicked in (TRUE) or out (FALSE).					*
 *									*
 * void loopw_setShadow (setflg)					*
 *									*
 * Input parameters:							*
 *	setflg		Boolean		set shadow flag			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		01/00	initial coding				*
 ***********************************************************************/
{
    Widget	wid;
/*---------------------------------------------------------------------*/

    switch (_loopingCmd) {
      case ANIM_LOOP_FORW:
	wid = _floopPb;
	break;

      case ANIM_LOOP_BACK:
	wid = _bloopPb;
	break;

      case ANIM_ROCK_LOOP:
	wid = _rockPb;
	break;

      default:
	return;
    }

    if (setflg) {
	XtVaSetValues (wid,
		       XmNarmColor,		_bgdColor,
		       XmNbackground,		_armColor,
		       XmNtopShadowColor,	_botColor,
		       XmNbottomShadowColor,	_topColor,
		       NULL);
    }
    else {
	XtVaSetValues (wid,
		       XmNarmColor,		_armColor,
		       XmNbackground,		_bgdColor,
		       XmNtopShadowColor,	_topColor,
		       XmNbottomShadowColor,	_botColor,
		       NULL);
    }
}

/*=====================================================================*/

void loopw_suspend ( Boolean sflag )
/************************************************************************
 * loopw_suspend							*
 *									*
 * This function sets the suspend flag.					*
 *									*
 * void loopw_suspend (sflag)						*
 *									*
 * Input parameters:							*
 *	sflag	Boolean		to suspend or not to suspend		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	02/99	initial coding				*
 * S. Law/GSC		01/00	combined with loopw_resume		*
 ***********************************************************************/
{
    _suspdFlag = sflag;
}

/*=====================================================================*/

void loopw_setHide ( Boolean hflag )
/************************************************************************
 * loopw_setHide							*
 *									*
 * This function sets the hide/show icon.				*
 *									*
 * void loopw_setHide (hflag)						*
 *									*
 * Input parameters:							*
 *	hflag		Boolean		hide state flag			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *                      NONE                                            *
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	12/98	initial coding				*
 * E. Safford/GSC	12/98	remove nameptr				*
 * S. Law/GSC		01/00	augmented from loopw_resetHide		*
 * S. Law/GSC		06/00	turned off auto update when hiding data	*
 ***********************************************************************/
{
    _hideFlag = hflag;

    if (hflag) {
	NxmBxmBtn_setPxm (_hidePb, _hidePxm[1].snstv, (Pixmap)0);
	strcpy (_loopLabels[ANIM_HIDE_DATA], "show frames");
	auto_stopAutoUpdt ();
    }
    else {
	NxmBxmBtn_setPxm (_hidePb, _hidePxm[0].snstv, (Pixmap)0);
	strcpy (_loopLabels[ANIM_HIDE_DATA], "hide frames");
	auto_startAutoUpdt ();
    }
}

/*=====================================================================*/
