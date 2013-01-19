#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "pgprm.h"
#include "proto_xw.h"

#define MAX_MENU	 10

#define DIST_SM		  0	/* use statute  miles */
#define DIST_NM		  1	/* use nautical miles */
#define DIST_KM		  2	/* use kilometers */
#define DIR_16PT	  0	/* use 16 point compass */
#define DIR_DEG		  1	/* use degrees */

#define LABEL_WIDTH	  115
#define LABEL_HEIGHT	  25

static int	_distType	= DIST_NM;
static char	*_distLabel[]	= {"sm", "nm", "km"};
static int	_dirType	= DIR_16PT;
static char	*_dirLabel[]	= {"16-pt", "DEG"};
static Boolean	_distDplOn;

static Widget	_mainWin;
static Widget	_displayBtn;
static Widget	_optForm;
static Widget	_topW;		/* Toplevel widget */
static Widget	_drawW;		/* Drawing area widget */
static Widget	_distWid;
static Widget	_distLbl;

static float	_iniLat;
static float	_iniLon;

static Boolean	_stopUpdate;

/*
 * Private callback functions
 */
void pgdist_ctlBtnCb	( Widget, long, XtPointer );
void pgdist_dirCb	( Widget, long, XtPointer );
void pgdist_distCb	( Widget, long, XtPointer );
void pgdist_displayCb	( Widget, XtPointer, XtPointer );

/************************************************************************
 * nmap_pgdist.c							*
 *									*
 * This module defines a distance options popup window.			*
 *									*
 * CONTENTS:                                                            *
 * pgdist_create()	creates the distance options popup widget	*
 * pgdist_popup()	manages the distance options popup widget	*
 * pgdist_popdown()	unmanages the distance options popup widget	*
 *									*
 * pgdist_isUp()	query if the distance options window is up 	*
 * pgdist_isDplOn()	query if Display Options on or off		* 
 *									*
 * pgdist_ctlBtnCb()	callback for the control buttons		*
 * pgdist_distCb()	callback for the distance unit menu		*
 * pgdist_dirCb()	callback for the direction unit menu		*
 * pgdist_displayCb()	callback for Display Options on/off button	*
 ***********************************************************************/

/*=====================================================================*/

Widget pgdist_create ( Widget parent )
/************************************************************************
 * pgdist_create							*
 *									*
 * This function creates the distance options window.			*
 *									*
 * Widget pgdist_create(parent)						*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget				*
 *									*
 * Output parameters:							*
 * pgdist_create	Widget	Widget ID of the distance options window*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		07/06	intial coding				*
 ***********************************************************************/
{
    Widget	pane, label, tempmenu;
    Widget	displayform, distform, dirform;
    Widget	btnarr[MAX_MENU];
    WidgetList	btnw;
    int		nitems;
    char	*ctlstrs[] = {"Close"};
/*---------------------------------------------------------------------*/
/*
 * create dialog shell
 */
    _mainWin = XmCreateFormDialog(parent, "pgdist_popup",
				    NULL, 0);
    XtVaSetValues(_mainWin, 
		  XmNnoResize,        True, 
		  XmNdefaultPosition, False, 
		  NULL);
    XtVaSetValues(XtParent(_mainWin),
		  XmNtitle, "DISTANCE OPTIONS",
		  NULL);

/*
 * create a parent pane widget
 */
    pane = XtVaCreateWidget("distw_pane",
			xmPanedWindowWidgetClass,	_mainWin,
			XmNallowResize,                 True,
			XmNmarginHeight,                0,
			XmNmarginWidth,                 0,
			XmNsashHeight,			1,
			XmNsashWidth,                   1,
			XmNspacing,                     1,
			NULL);

/*
 * create distance display on button
 */
    displayform  = XtVaCreateWidget("display_form",
			     xmFormWidgetClass, pane,
			     NULL);

    _displayBtn = XtVaCreateManagedWidget("Distance Display Off",
                xmPushButtonWidgetClass, displayform,
                XmNleftAttachment,       XmATTACH_FORM,
                XmNleftOffset,           45,     
		XmNlabelType,		 XmSTRING,
                XmNheight,		 25,
                NULL);

    XtAddCallback( _displayBtn, XmNactivateCallback, 
		      (XtCallbackProc)pgdist_displayCb, NULL);
    XtManageChild(displayform);
    _distDplOn = FALSE;

/*
 * create options form 
 */
    _optForm = XtVaCreateWidget("pgdist_optform",
			    xmFormWidgetClass,	pane,
			    NULL);

/*
 * dist button
 */
    nitems = XtNumber (_distLabel);
    if (nitems > MAX_MENU) nitems = MAX_MENU;
    pgutls_createOptionMenu (_optForm, nitems, (XtPointer)&_distType, 
			     "Dist\nUnits", (XtCallbackProc)pgdist_distCb, &distform,
			     &label, &(tempmenu), 
			     btnarr, _distLabel);

    XtVaSetValues (distform, 
		   XmNleftAttachment,	XmATTACH_FORM,
		   XmNtopAttachment,	XmATTACH_FORM,
		   NULL);

    XtVaSetValues (label, 
		   XmNtopAttachment,	XmATTACH_FORM,
		   XmNtopOffset,	5,
		   NULL);

/*
 * dir button
 */
    nitems = XtNumber (_dirLabel);
    if (nitems > MAX_MENU) nitems = MAX_MENU;
    pgutls_createOptionMenu (_optForm, nitems, (XtPointer)&_dirType, 
			     "Dir\nUnits", (XtCallbackProc)pgdist_dirCb, &dirform,
			     &label, &(tempmenu), 
			     btnarr, _dirLabel);

    XtVaSetValues (dirform, 
		   XmNleftAttachment,	XmATTACH_WIDGET,
		   XmNleftWidget,	distform,
		   XmNleftOffset,	10,
		   XmNtopAttachment,	XmATTACH_FORM,
		   NULL);

    XtVaSetValues (label, 
		   XmNtopAttachment,	XmATTACH_FORM,
		   XmNtopOffset,	5,
		   NULL);

    XtManageChild(_optForm);
    XtSetSensitive (_optForm, FALSE);
    
/*
 * create control buttons.
 */
    nitems = XtNumber (ctlstrs);
    btnw   = (WidgetList)XtMalloc((size_t)nitems*sizeof(Widget));
    NxmCtlBtn_create(pane, 1, "pgdist_ctlBtn", nitems,
		     ctlstrs, (XtCallbackProc)pgdist_ctlBtnCb, btnw);

    XtFree((XtPointer)btnw);

    XtManageChild (pane);

    return(_mainWin);

}

/*=====================================================================*/

void pgdist_popup ( void )
/************************************************************************
 * pgdist_popup								*
 *									*
 * This function pops up distance options window.			*
 *									*
 * void pgdist_popup ()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		07/06	initial coding				*
 ***********************************************************************/
{
    XtManageChild (_mainWin);
}

/*=====================================================================*/

void pgdist_popdown ( void )
/************************************************************************
 * pgdist_popdown							*
 *									*
 * This function pops down the distance options window.			*
 *									*
 * void pgdist_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		07/06	initial coding				*
 ***********************************************************************/
{
    XtUnmanageChild (_mainWin);
    if ( pgpalw_isUp() ) pgpalw_setupOper();
}

/*=====================================================================*/

Boolean pgdist_isUp ( void ) 
/************************************************************************
 * pgdist_isUp								*
 *									*
 * This function queries whether the distance option window is up.	*
 *									*
 * Boolean pgdist_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * seekw_isUp	Boolean		True -- up,  False -- down		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		07/06	initial coding				*
 ***********************************************************************/
{
    return (XtIsManaged (_mainWin));
}

/*=====================================================================*/

Boolean pgdist_isDplOn ( void ) 
/************************************************************************
 * pgdist_isDplOn							*
 *									*
 * This function queries whether the distance display feature is ON.	*
 *									*
 * Boolean pgdist_isDplOn ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgdist_isDplOn	Boolean		True -- on,  False -- off	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		07/06	initial coding				*
 ***********************************************************************/
{
    return ( _distDplOn );
}

/*=====================================================================*/
/* ARGSUSED */
void pgdist_displayCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgdist_displayCb							*
 *									*
 * Callback for distance display on/off button widget			*
 *									*
 * void pgdist_displayCb (wid, clnt, call)				*
 *									*
 * Input parameters:							*
 *	wid		Widget			Widget ID		*
 *	clnt		XtPointer		color			*
 *	call		XtPointer		callback struct		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		07/06	initial coding				*
 ***********************************************************************/
{
    XmString     xmstr;
/*---------------------------------------------------------------------*/

    if ( _distDplOn == TRUE ) {

	 xmstr = XmStringCreateLocalized ("Distance Display Off");
	 XtVaSetValues(_displayBtn, XmNlabelString, xmstr, NULL);
	 XmStringFree (xmstr);
	 XtSetSensitive (_optForm, FALSE);
	 _distDplOn = FALSE;
    }
    else {

	 xmstr = XmStringCreateLocalized ("Distance Display On");
	 XtVaSetValues(_displayBtn, XmNlabelString, xmstr, NULL);
	 XmStringFree (xmstr);
	 XtSetSensitive (_optForm, TRUE);
	 _distDplOn = TRUE;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgdist_distCb ( Widget wid, long unit, XtPointer cbs )
/************************************************************************
 * pgdist_distCb							*
 *									*
 * This is the callback function for the button to change the distance	*
 * units.								*
 *									*
 * void pgdist_distCb (wid, unit, cbs)					*
 *									*
 * Input parameters:							*
 *	wid	Widget		the widget calling this function	*
 *	unit	long		which unit				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		07/06	initial coding				*
 ***********************************************************************/
{
    _distType = (int)unit;
}

/*=====================================================================*/
/* ARGSUSED */
void pgdist_dirCb ( Widget wid, long unit, XtPointer cbs )
/************************************************************************
 * pgdist_dirCb								*
 *									*
 * This is the callback function for the button to change the direction	*
 * units.								*
 *									*
 * void pgdist_dirCb (wid, unit, cbs)					*
 *									*
 * Input parameters:							*
 *	wid	Widget		the widget calling this function	*
 *	unit	long		which unit				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		07/06	initial coding				*
 ***********************************************************************/
{
    _dirType = (int)unit;
}

/*=====================================================================*/
/* ARGSUSED */
void pgdist_ctlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pgdist_ctlBtnCb							*
 *									*
 * This is the callback function for the control buttons.		*
 *									*
 * void pgdist_ctlBtnCb (wid, which, cbs)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		the widget calling this function	*
 *	which	long		which button				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		07/06	initial coding				*
 ***********************************************************************/
{
    switch (which) {

      case 0:		/* close */
	pgdist_popdown ();
	break;
    }
}

/*=====================================================================*/

void pgdist_start ( int event_x, int event_y )
/************************************************************************
 * pgdist_start								*
 *									*
 * This function pops up an overriding widget for LINE, FRONT and CIRCLE*
 *									*
 * void pgdist_start (event_x, event_y)					*
 *									*
 * Input parameters:							*
 *	event_x	int	X device coord of cursor			*
 *	event_y	int	Y device coord of cursor			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		08/06   initial coding				*
 * H. Zeng/SAIC		09/06	added local global _topW & _drawW	*
 ***********************************************************************/
{
    int		npts, ier, xoff, yoff;
    float	xx[1], yy[1];
    char	loco[20];
    XmString	xmstr;
    Position	pos_x, pos_y, center_x, center_y;
    Dimension   w_width, w_height;
/*---------------------------------------------------------------------*/

    xgtoff (&xoff, &yoff, &ier);

    xx[0] = (float) (event_x + xoff);
    yy[0] = (float) (event_y + yoff);

    npts = 1;
    gtrans (sys_D, sys_M, &npts, xx, yy, &_iniLat, &_iniLon, 
	    &ier, strlen(sys_D), strlen(sys_M) );

/*
 * Destroy any previous widget that _distWid points to.
 */
    if ( _distWid != NULL ) {

         XtDestroyWidget (_distWid);
         _distWid = NULL;
	 _distLbl = NULL;
    }

/*
 * create dialog shell
 */
    _topW   = mainw_getToplevel ( );
    _distWid = XtVaCreateWidget("dist_popup",
			 overrideShellWidgetClass, _topW,
			 XmNheight, LABEL_HEIGHT,
			 XmNwidth,  LABEL_WIDTH,
			 NULL);
    _distLbl = XtVaCreateManagedWidget("      0",
			 xmLabelWidgetClass,  _distWid,  NULL);
        
    _drawW   = mcanvw_getDrawingW();
    XtVaGetValues (_drawW,
                   XmNwidth,     &w_width, 
                   XmNheight,    &w_height,
                   NULL);

    center_x = (Position)(w_width  / 2);
    center_y = (Position)(w_height / 2);

    XtVaGetValues (_topW, XmNx, &pos_x, XmNy, &pos_y, NULL);

    pos_x += (Position)((float)center_x +
		        (float)(event_x - center_x)*0.80F); 
    pos_y += (Position)((float)center_y +
		        (float)(event_y - center_y)*1.0F );

    XtVaSetValues (_distWid, 
		   XmNx,	pos_x,
		   XmNy,	pos_y,
		   NULL);

    sprintf (loco, "    0 %s", _distLabel[_distType] );
    xmstr = XmStringGenerate(loco, XmFONTLIST_DEFAULT_TAG, 
			     XmCHARSET_TEXT, NULL);
    XtVaSetValues (_distLbl, XmNlabelString, xmstr, NULL);
    XmStringFree  (xmstr);
    XtManageChild (_distWid);
    
    _stopUpdate = FALSE;

}

/*=====================================================================*/

void pgdist_update ( int event_x, int event_y )
/************************************************************************
 * pgdist_update							*
 *									*
 * This function updates the dist.&dir. info on the overriding widget	*
 * for LINES, FRONTS and CIRCLE.					*
 *									*
 * void pgdist_update ( event_x, event_y )				*
 *									*
 * Input parameters:							*
 *	event_x	int	X device coord of cursor			*
 *	event_y	int	Y device coord of cursor			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		08/06	initial coding				*
 * H. Zeng/SAIC		09/06	updating the pos. of overriding widget  *
 ***********************************************************************/
{
    int		npts, ier, xoff, yoff, icmp;
    float	xx[1], yy[1], lat, lon, dist, dir;
    char	loco[20], rose[10];
    XmString	xmstr;  
    Position	pos_x, pos_y, center_x, center_y;
    Dimension   w_width, w_height; 
/*---------------------------------------------------------------------*/
/*
 * Check if the overriding widget really exists.
 */
    if ( _distWid == NULL )  return;

/*
 * Check if _stopUpdate is set to be TRUE.
 */
    if ( _stopUpdate )       return;    


    xgtoff (&xoff, &yoff, &ier);

    xx[0] = (float) (event_x + xoff);
    yy[0] = (float) (event_y + yoff);

    npts = 1;
    gtrans (sys_D, sys_M, &npts, xx, yy, &lat, &lon, 
	    &ier, strlen(sys_D), strlen(sys_M) );

/*
 * Calculate the new distance and direction based on new lat&lon info
 */
    npts = 1;
    clo_dist (&_iniLat, &_iniLon, &npts, &lat, &lon, &dist, &ier);

    switch (_distType) {
      case DIST_SM:
	dist *= M2SM;
	break;

      case DIST_NM:
	dist *= M2NM;
	break;

      case DIST_KM:
	dist /= 1000.0F;
	break;
    }

    dist = (float)G_NINT (dist);

    if (dist < 1.0F) {
	dir = 0.0F;
	strcpy (rose, "-");
    }
    else {
	clo_direct (&_iniLat, &_iniLon, &lat, &lon, &dir, &ier);

	if (_dirType == DIR_16PT) {
	    clo_compass (&dir, rose, &icmp, &ier);
	}
	else {
	    sprintf (rose, "%3ddeg", (int) dir);
	}
    }

    sprintf (loco, "%6d%s %-3s", 
	     (int) dist, _distLabel[_distType], rose);

    xmstr = XmStringGenerate(loco,  XmFONTLIST_DEFAULT_TAG, 
			            XmCHARSET_TEXT, NULL);
    XtVaSetValues (_distLbl, 
		   XmNlabelString,  xmstr,
		   NULL);
    XmStringFree (xmstr);

/*
 * Update the position of the overrideing widget so it will move
 * with the cursor.
 */
    XtVaGetValues (_drawW,
                   XmNwidth,     &w_width, 
                   XmNheight,    &w_height,
                   NULL);

    center_x = (Position)(w_width  / 2);
    center_y = (Position)(w_height / 2);

    XtVaGetValues (_topW, XmNx, &pos_x, XmNy, &pos_y, NULL);

    pos_x += (Position)((float)center_x +
		        (float)(event_x - center_x)*0.80F); 
    pos_y += (Position)((float)center_y +
		        (float)(event_y - center_y)*1.0F );

    XtVaSetValues (_distWid, 
		   XmNx,	pos_x,
		   XmNy,	pos_y,
		   NULL); 
}

/*=====================================================================*/

void pgdist_stop ( void )
/************************************************************************
 * pgdist_stop								*
 *									*
 * This function destroys the overriding widget that shows dist.& dir.  *
 * info for LINES, FRONTS and CIRCLE.					*
 *									*
 * void pgdist_stop ( void )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		08/06	initial coding				*
 ***********************************************************************/
{
/*
 * Destroy the widget that _distWid points to.
 */
    if ( _distWid != NULL ) {

         XtDestroyWidget (_distWid);
         _distWid = NULL;
	 _distLbl = NULL;
    }
    _stopUpdate = TRUE;
}

/*=====================================================================*/

void pgdist_stopUpdate ( void )
/************************************************************************
 * pgdist_stopUpdate							*
 *									*
 * This function sets _stopUpdate flag which sends a sigal to stop      *
 * updating dist.& dir.  info on the overriding widget for LINES, FRONTS*
 * and CIRCLE.								*
 *									*
 * void pgdist_stopUpdate ( void )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		08/06	initial coding				*
 ***********************************************************************/
{
    _stopUpdate = TRUE;
}
