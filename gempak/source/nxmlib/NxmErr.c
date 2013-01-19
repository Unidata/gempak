#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmInit.h"
#include "proto_xw.h"


#define VISIBLE_NUM	 15
#define FIELD_WIDTH	 950	  /* width of the error popup window */
#define ICON_FGNAME      "white"
#define ICON_BGNAME      "blue"
#define ICON_EMG_BGNAME  "red"
#define BLUE_LABEL	 "no error message"
#define RED_LABEL	 "new error reported"

/* error button icon bitmap */
static unsigned char _NxmErrBits[] = {
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f,
   0xff, 0xff, 0xff, 0x3f, 0x21, 0x0c, 0xc7, 0x30, 0x21, 0x08, 0x82, 0x20,
   0x39, 0x49, 0x82, 0x24, 0x21, 0x08, 0x92, 0x20, 0x21, 0x0c, 0x93, 0x30,
   0x39, 0x49, 0x82, 0x24, 0x21, 0x49, 0x82, 0x24, 0x21, 0x49, 0xc6, 0x24,
   0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xff, 0x3f, '\0' };


static  Widget 	_ErrorPopup; 
static  Widget 	_ErrorList;
static  Widget	_ErrBtn;
static  int     _popupFlg; 	/* flag is set after clicking Error button */

static char  _errTxtLabel[30];

struct pxmBuf _errPxm[2];		/* pixmap label buffer	    */

/*
 *  Private functions
 */
void _errorBtCb ( Widget, XtPointer, XtPointer );
void _errorCloseCb ( Widget, XtPointer, XtPointer );
void NxmErr_popup ( void );


/************************************************************************
 * NxmErr.c                                                        	*
 *                                                                      *
 * This module creates an error button and the corresponding error 	*
 * message display popup window. The error messages listed in the	*
 * pop-up window can be saved to a file when nenessary.                 *
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 * 	NxmErr_createPopup()	create the error message window         *
 *	NxmErr_btCreate()	create the error button			*
 *	NxmErr_update()		update the error message popup window	*
 *      NxmErr_popup()		pops up the error message window	*
 *	_errorBtCb()		callback of error button		*
 *	_errorCloseCb()		callback of close button		*
 ***********************************************************************/

/*=====================================================================*/

Widget NxmErr_createPopup ( Widget parent )
/************************************************************************
 * NxmErr_createPopup                                                   *
 *                                                                      *
 * This function creates the error display popup widget.                *
 *                                                                      *
 * Widget NxmErr_createPopup(parent)			                *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent          Widget     ID of parent widget.                     *
 *                                                                      *
 * Return parameters:                                                   *
 *	NxmErr_createPopup	Widget		The dialog widget       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC	   07/96                                                *
 * C. Lin/EAI      01/97  take out the save and help button		*
 * I. Durham/GSC   05/98  changed underscore decl. to an include	*
 * T. Piper/SAIC	05/03	replaced XAllocNamedColor w/xsncolr	*
 ***********************************************************************/
{
Widget         	pane, frame_list, bb, button;
Cardinal       	argcnt;
Arg            	args[10];
Pixel		color_temp;
XmString	str;
int             xpos;

/*---------------------------------------------------------------------*/

	xsncolr("white", &color_temp, &xpos);

	str = XmStringCreateLocalized("Error Messages");

	argcnt = 0;
	XtSetArg(args[argcnt], XmNallowOverlap, False);  argcnt++;
	XtSetArg(args[argcnt], XmNnoResize, True); 	 argcnt++;
	XtSetArg(args[argcnt], XmNdialogTitle, str);     argcnt++;
	_ErrorPopup = XmCreateFormDialog(parent, 
				"Error", args, argcnt); 

	XmStringFree(str);
	
	pane = XtVaCreateManagedWidget( "pane",
			xmPanedWindowWidgetClass, _ErrorPopup, 
			XmNsashWidth,             1,
			XmNsashHeight,            1,
			NULL);

	frame_list = XtVaCreateManagedWidget( "frame_list",
			xmFrameWidgetClass,       pane, 
			NULL);

	argcnt = 0;
	XtSetArg(args[argcnt], XmNscrollBarDisplayPolicy, 
					XmAS_NEEDED); argcnt++;
	XtSetArg(args[argcnt], XmNwidth, 	  
					FIELD_WIDTH); argcnt++;
	XtSetArg(args[argcnt], XmNscrollingPolicy, 	  
					XmAUTOMATIC); argcnt++;
	XtSetArg(args[argcnt], XmNbackground,        
					color_temp); argcnt++;
	XtSetArg(args[argcnt], XmNlistSizePolicy,        
					XmCONSTANT); argcnt++;
	_ErrorList = XmCreateScrolledList( frame_list, "sc", 
					args, argcnt );
	XtManageChild(_ErrorList);
	
        bb = XtVaCreateManagedWidget("Acknowledged",
                        xmBulletinBoardWidgetClass, pane,
			XmNmarginHeight,	    3,	
                        NULL );

	xpos = (int)((float)FIELD_WIDTH/2.0F) - 40;
        button = XtVaCreateManagedWidget("Acknowledged",
                        xmPushButtonGadgetClass, bb,
			XmNx,	                 xpos,	
                        NULL );
        XtAddCallback(button, XmNactivateCallback,
                        (XtCallbackProc)_errorCloseCb, NULL);

	_popupFlg  = 0;
	return(_ErrorPopup);

}

/*=====================================================================*/

Widget NxmErr_btCreate ( Widget parent )
/************************************************************************
 * NxmErr_btCreate							*
 *                                                                      *
 * This function create the error button			 	*
 *                                                                      *
 * Widget   NxmErr_btCreate(parent)					*
 *                                                                      *
 * Input parameters:                                                    *
 *	parent		Widget	parent id                               *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	NxmErr_btCreate	Widget                                          *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      09/96                                               *
 * R. Tian/SAIC	    01/03	add True flag to NxmBxmBtn_create(Multi)*
 ***********************************************************************/
{
struct	bxmInfo	errBxm[2];

/*---------------------------------------------------------------------*/

 	strcpy( errBxm[0].fgcolor, ICON_FGNAME );
        strcpy( errBxm[1].fgcolor, ICON_FGNAME );
        strcpy( errBxm[0].bgcolor, ICON_BGNAME );
        strcpy( errBxm[1].bgcolor, ICON_EMG_BGNAME );

        errBxm[0].sens_bits   = (char *)_NxmErrBits;
        errBxm[1].sens_bits   = (char *)_NxmErrBits;
        errBxm[0].insens_bits = (char *)NULL;
        errBxm[1].insens_bits = (char *)NULL;

	strcpy( _errTxtLabel, BLUE_LABEL );
	_ErrBtn = NxmBxmBtn_createMulti( parent, "errBtn", NULL,
                       	30, 15, (struct bxmInfo *)errBxm, 2, 
			_errTxtLabel, True, _errorBtCb, (XtPointer)2, 
			(struct pxmBuf *)_errPxm );

	return( _ErrBtn );
}

/*=====================================================================*/

void NxmErr_update ( void )
/************************************************************************
 * NxmErr_update							*
 *                                                                      *
 * This function checks the common error message buffer defined in      *
 * ercmn.h, updates the error message popup window or error button      *
 * when necessary.							*
 *                                                                      *
 * void	NxmErr_update()							*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      		NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       01/97                                               *
 * S. Wang/GSC	    04/97  	add NxmBxmBtn_changeLabel()		*
 * J. Wu/SAIC	    01/04  	add call to er_gnumerr & er_gerrmsg	*
 * J. Wu/SAIC	    02/04  	change call to er_gerrmsg		*
 * T. Piper/SAIC	10/06	Increased errmsg to 513			*
 ***********************************************************************/
{
int		ii, nermsg, ier;
XmStringTable	xmstrs;
Boolean         winflg;
char		errmsg[513];

/*---------------------------------------------------------------------*/

	er_gnumerr ( &nermsg, &ier );
	 
	if ( nermsg == 0 )
		return;

	winflg = XtIsManaged(_ErrorPopup);

	if ( winflg || _popupFlg ) { 

	    if (winflg) 
	    	XtUnmanageChild(_ErrorPopup);

	    xmstrs = (XmStringTable)XtMalloc((size_t)nermsg*sizeof(XmString));

	    for ( ii = 0; ii < nermsg; ii++ ) {
		er_gerrmsg ( &ii, errmsg, &ier );
		xmstrs[ii] = XmStringCreateLocalized(errmsg);
	    }

	    XtVaSetValues(_ErrorList, 
		XmNitems, 	        xmstrs,
		XmNitemCount,		nermsg,
		XmNvisibleItemCount,    VISIBLE_NUM,
		NULL);
				
	    for( ii=0; ii<nermsg; ii++ )
		XmStringFree( xmstrs[ii] );
	    XtFree((XtPointer)xmstrs);

	    XtManageChild(_ErrorPopup);

	    if ( _popupFlg )
		_popupFlg = 0;

	}
	else { 	/* set the error button red */
	    if ( _ErrBtn ) {
        	NxmBxmBtn_setPxm( _ErrBtn, _errPxm[1].snstv, (Pixmap)0 );
		NxmBxmBtn_changeLabel(_errTxtLabel, RED_LABEL);
	    }
	}
}

/*=====================================================================*/

void NxmErr_popup ( void )
/************************************************************************
 * NxmErr_popup				 	                        *
 *                                                                      *
 * This function pops up the error display window.			*
 *                                                                      *
 * void NxmErr_popup()				                        *
 *                                                                      *
 * Input parameters:                                             	*
 * Output parameters:                                             	*
 * Return parameters:                                                   *
 *      		NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      07/96                                               *
 * C. Lin/EAI	    01/97	use ercmn.h instead of local link-list  *
 * S. Wang/GSC      04/97	remove warning message			*
 * J. Wu/SAIC	    01/04  	add call to er_gnumerr			*
 ***********************************************************************/
{
    int   nermsg, ier; 
/*---------------------------------------------------------------------*/
	
	er_gnumerr ( &nermsg, &ier );
	
	if ( nermsg == 0 ) 
		return;	
	else {	
		_popupFlg = 1;
		NxmErr_update();
	}
}

/*=====================================================================*/
/* ARGSUSED */
void _errorBtCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * _errorBtCb								*
 *                                                                      *
 * This is the callback function for error button		 	*
 *                                                                      *
 * void   _errorBtCb(w, clnt, call)					*
 *                                                                      *
 * Input parameters:                                                    *
 * 	w	Widget							*
 *	clnt	XtPointer						*
 *	call	XtPointer						*
 * 									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      		NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      09/96                                               *
 ***********************************************************************/
{
/*
 * set button to blue
 */
    NxmBxmBtn_setPxm( w, _errPxm[0].snstv, (Pixmap)0 );

/*
 * pop-up the error message widget
 */
    NxmErr_popup();
}

/*=====================================================================*/
/* ARGSUSED */
void _errorCloseCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * _errorCloseCb							*
 *                                                                      *
 * This is the callback function for closing the error popup window	*
 *                                                                      *
 * void _errorCloseCb(w, clnt, call)					*
 *                                                                      *
 * Input parameters:                                                    *
 *      w        Widget     	ID of the calling widget                *
 *	clnt	 XtPointer	never used				*
 *	call	 XtPointer	never used				*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *      		NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC      07/96   	                                        *
 * C. Lin/EAI       01/97	call er_init(), take out save, help     *
 * S. Wang/GSC	    04/97  	add NxmBxmBtn_changeLabel()		*
 ***********************************************************************/
{
int 	iret;

/*---------------------------------------------------------------------*/
/*
 *  Change text label to no error.
 */
    NxmBxmBtn_changeLabel(_errTxtLabel, BLUE_LABEL);
    XtUnmanageChild(_ErrorPopup);
    er_init( &iret );
}
