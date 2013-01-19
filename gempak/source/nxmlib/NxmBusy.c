#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"


#define ICON_WIDTH  32  /* stop icon width, hight */
#define ICON_HEIGHT 32  


#define ICON_DIR    "$NAWIPS/icons/busy"
#define ICON_FGNAME       "white"
#define ICON_BGNAME       "blue"
#define ICON_WORK_BGNAME  "red"

static Widget _stopButton;
static Widget _cursorW;	/* the wiget where cursor changes */
static struct pxmBuf   _stopPxm[2];  /* stop button pixmaps */

static char  *_interruptFlag = NULL; /* flag to signal interrupt */
static char  _stopBtnFlag = 1; /* stop button enable/disable flag */

/*
 *  Private callback functions
 */
void NxmBusy_pushbCb ( Widget, long which, XtPointer );


/************************************************************************
 * NxmBusy.c                                                         	*
 *                                                                      *
 * This module takes care of creating BUSY/STOP buttons and the         *
 * callbacks for them.							*
 *                                                                      *
 * CONTENTS:                                                            *
 *      NxmBusy_createBtns()    create stop button.			*
 *      NxmBusy_invoke()        set cursor and stop to busy mode.	*
 *      NxmBusy_checkStopBtn()  check whether stop button is pressed.   *
 *      NxmBusy_animateFinish() cleanup function when finish animation. *
 *	NxmBusy_setStopBtn()	enable/disable stop buttons		*
 *									*
 *      NxmBusy_pushbCb()       callback function for the push buttons. *
 ***********************************************************************/

/*=====================================================================*/

void NxmBusy_createBtns ( Widget rc )
/************************************************************************
 * NxmBusy_createBtns                                              	*
 *                                                                      *
 * This function creates the stop icon push_button.  The parent widget	*
 *	is expected to be a Row Column widget.    			*
 *                                                                      *
 * void NxmBusy_createBtns(rc)                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  rc       Widget      parent RC widget ID                            *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 * C. Lin/EAI      02/97  	modified, move from NMAP to NXMLIB  	*
 * S. Wang/GSC     04/97  	add auto-labels for stop/busy button  	*
 * S. Wang/GSC     10/97  	add insensitive bits for stop button	*
 * R. Tian/SAIC	   01/03	add True flag to NxmBxmBtn_create(Multi)*
 * T. Piper/SAIC	07/03	removed the creation of the busy button	*
 ***********************************************************************/
{
char   iconfile[256], ins_iconfile[256];
int    iret;
long   ignore;
struct bxmInfo  stopbxm[2];

static  char  label_1[30];
/*---------------------------------------------------------------------*/

	/*
	 * create stop button
	 */
	cfl_inqr("stop.xbm", ICON_DIR, &ignore, iconfile, &iret);
	cfl_inqr("stpins.xbm", ICON_DIR, &ignore, ins_iconfile, &iret);

        strcpy( stopbxm[0].fgcolor, ICON_FGNAME );
        strcpy( stopbxm[1].fgcolor, ICON_FGNAME );
        strcpy( stopbxm[0].bgcolor, ICON_BGNAME );
        strcpy( stopbxm[1].bgcolor, ICON_WORK_BGNAME );

        stopbxm[0].sens_bits   = iconfile;
        stopbxm[1].sens_bits   = iconfile;
        stopbxm[0].insens_bits = ins_iconfile;
        stopbxm[1].insens_bits = NULL;

        strcpy( label_1, "stop action" );
        _stopButton = NxmBxmBtn_createMulti( rc, "stopBtn", NULL,
                         ICON_WIDTH, ICON_HEIGHT, stopbxm, 2, label_1, 
                         True, (XtCallbackProc)NxmBusy_pushbCb, 0, _stopPxm );

	NxmBxmBtn_setPxm( _stopButton, (Pixmap)0, _stopPxm[0].insnstv );

}

/*=====================================================================*/

void NxmBusy_invoke ( Widget cursorw, char *sflag )
/************************************************************************
 * NxmBusy_invoke                                              		*
 *                                                                      *
 * This function starts up  busy process.    				*
 *                                                                      *
 * void NxmBusy_invoke(cursorw, sflag )               			*
 *                                                                      *
 * Input parameters:                                                    *
 *	cursorw		Widget	a widget where cursor changes   	*
 *	*sflag		char	the flag to signal there is an interrupt*
 *									*
 * Output parameters:                                                   *
 * NxmBusy_invoke	pid_t    PID of busy process                    *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 * C. Lin/EAI      02/97  	modified, move from NMAP to NXMLIB  	*
 * C. Lin/EAI      04/97  	solve problem with SUNOS -- some widget *
 *		                parms and local parms doesn't carry over*
 *				to child process, use local globals.	*
 * E. Safford/GSC  04/99	fix irix6 compiler warning		*
 * H. Zeng/EAI     04/00        changed cursor change funtion           *
 * T. Piper/SAIC	04/03	removed _busyButtonH & _busyButtonW	*
 * T. Piper/SAIC	07/03	removed the invocation of Busy		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
/*
 * change the cursor to be busy in the drawing window
 */
	if ( cursorw ) {
		_cursorW = cursorw;
                NxmCursor_setCursor(cursorw, CURS_BUSY);
	}
	else
		_cursorW = NULL;
	
	_interruptFlag = sflag;

/*
 * change the stop button foreground color to red
 */
	NxmBxmBtn_setPxm( _stopButton, _stopPxm[1].snstv, (Pixmap)0 );
}

/*=====================================================================*/

void NxmBusy_checkStopBtn ( void )
/************************************************************************
 * NxmBusy_checkStopBtn                                              	*
 *                                                                      *
 * This function checks whether the stop button has been pressed.    	*
 *                                                                      *
 * void NxmBusy_checkStopBtn()                   			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 * C. Lin/EAI      02/97  	modified from NMAP  			*
 * S. Wang/GSC     10/97  	add _stopBtnFlag			*
 ***********************************************************************/
{
XEvent event;
Display *dpy = XtDisplay(_stopButton);

/*---------------------------------------------------------------------*/

    if ( _stopBtnFlag == 1 ) {

	XSync(dpy, 0);

  	while (XCheckMaskEvent(dpy, (ButtonPressMask|ButtonReleaseMask),
                         &event)) {
	    XButtonEvent *bevent = &(event.xbutton);
	    if (bevent->window == XtWindow(_stopButton)) { 
		XtDispatchEvent(&event);
            }
    	}
    }
}

/*=====================================================================*/

void NxmBusy_animateFinish ( void )
/************************************************************************
 * NxmBusy_animateFinish                                              	*
 *                                                                      *
 * This function cleans up after finishing animation.    		*
 *                                                                      *
 * void NxmBusy_animateFinish()                   			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 * C. Lin/EAI      02/97	modified, move from NMAP to NXMLIB  	*
 * H. Zeng/EAI     04/00        changed cursor change funtion           *
 * T. Piper/SAIC	07/03	removed the killing of the busy process	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
/*
 * change the cursor back to normal in the widget invoking  
 * the busy process
 */
	if ( _cursorW )
		NxmCursor_setCursor(_cursorW, CURS_DEFAULT);

/*
 * change the stop button foreground color back 
 */
	NxmBxmBtn_setPxm( _stopButton, _stopPxm[0].snstv, (Pixmap)0 );

}

/*=====================================================================*/

void NxmBusy_setStopBtn ( int flag )
 /***********************************************************************
 * NxmBusy_setStopBtn                                                   *
 *                                                                      *
 * This function enables/disables the stop button			*
 *                                                                      *
 * void NxmBusy_setStopBtn(flag)                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *   flag       int     1 enable stop button, 0 disable stop button    	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      10/97                                               *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	_stopBtnFlag = (char)flag;

	if ( _stopBtnFlag == 0 )
	    XtSetSensitive( _stopButton, False );
	else 	
	    XtSetSensitive( _stopButton, True );

}

/*=====================================================================*/
/* ARGSUSED */
void NxmBusy_pushbCb ( Widget w, long which, XtPointer cbs )
/************************************************************************
 * NxmBusy_pushbCb                                                      *
 *                                                                      *
 * Callback function for push buttons.        				*
 *                                                                      *
 * void NxmBusy_pushbCb(w, which, cbs)                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget     widget ID                                    *
 *  which       long  	   which button  				*
 *  cbs        XtPointer  callback struct                              *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      05/96                                                *
 * C. Lin/EAI      02/97  	modified from NMAP  			*
 * C. Lin/EAI      05/97  	check _interruptFlag and _busyPid  	*
 * S. Wang/GSC     10/97  	add _stopBtnFlag			*
 * H. Zeng/EAI     05/01        disabled BUSY callback                  *
 * T. Piper/SAIC	07/03	removed the check on _busyPid		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    if ( _stopBtnFlag == 1 ) {
	switch(which) {

	    case 0:	/* STOP */
		if ( _interruptFlag ) {
			*_interruptFlag = 1;
			NxmBusy_animateFinish();
		}
		break;

	    case 1:	/* BUSY */
                break;
	}
    }
}
