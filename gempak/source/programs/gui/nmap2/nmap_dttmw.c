#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"


Widget	_dttmEditW;	/* date/time edit popup */
Widget	_dayW[6][7];	/* pushbutton gadget for each day in a month */
Widget  _monListW;	/* list widget of the months */
Widget  _yearW;		/* year widget */
Widget  _hhmmW[2];	/*  hour/min scale widgets( hour=0,minutes=1 )*/

dttmi_t	_dttmEdit;	/* point to the dttm structure to be modified */
dttmi_t _dttmCopy;      /* copy of the original dttm struct */

char    *_monthStr[]={"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
			"Aug", "Sep", "Oct", "Nov", "Dec"};

int	_lastDay[]={-1, -1}; /* i, j of last selected day widget */

static int _lastMon = 0; /* The last selected month - counted from 0 to 11 */ 

/*
 * Private callback functions
 */

void dttmw_yearCb   ( Widget, long, XtPointer );
void dttmw_monthCb  ( Widget, XtPointer, XtPointer );
void dttmw_dayCb    ( Widget, long, XtPointer );
void dttmw_hhmmCb   ( Widget, long, XtPointer );
void dttmw_ctlBtnCb ( Widget, long, XtPointer );

/*
 *  Private functions
 */

void dttmw_yearSet ( void );
void dttmw_monthSet ( void );
void dttmw_hhmmSet ( void );
Boolean dttmw_isUp ( void );


/************************************************************************
 * nmap_dttmw.c                                                        	*
 *                                                                      *
 * This module defines the date/time editting popup widget for nmap.	* 
 *                                                                      *
 * CONTENTS:                                                            *
 *      dttmw_create()      creates the date/time editting popup.       *
 *      dttmw_popup()       pop up the dttm editting window.       	*
 *	dttmw_isUp()	    query if calendar edit window is up.	*
 *      dttmw_yearCb()      arrow button callback function for year.  	*
 *      dttmw_monthCb()     callback function for selecting the month.  *
 *      dttmw_dayCb()       callback function for selecting the day.    *
 *      dttmw_hhmmCb()      scale callback function for hour/min.  	*
 *      dttmw_ctlBtnCb()    callback function for ctrl-buttns at bottom.*
 *      dttmw_monthSet()    set the month defined in *_dttmEdit.    	*
 *      dttmw_hhmmSet()     set the hour/min scales based on       	*
 *					*_dttmEdit.    			*
 ***********************************************************************/

/*=====================================================================*/

Widget dttmw_create ( Widget parent )
/************************************************************************
 * dttmw_create                                                         *
 *                                                                      *
 * This function creates the date/time editting  window.                *
 *                                                                      *
 * Widget dttmw_create(parent)                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                               *
 *                                                                      *
 * Output parameters:                                                   *
 *  dttmw_create	Widget      Widget ID of the popup              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 * G. Krueger/EAI  10/97	NxmControlBtn->NxmCtlBtn_create 	*
 * C. Lin/EAI      06/98	add ProcessDirection for scales 	*
 ***********************************************************************/
{
Widget   pane, rc0, frame, rc, rc01, rc02, rc1, rc2; 
Widget   rc3, rc4, rc5, form, arrow; 
XmString xmstr[12];
int      jj, n;
long	ii;
char     *btnstr[] = {"Accept", "Close"};

/*---------------------------------------------------------------------*/

	_dttmEditW = XmCreateFormDialog(parent, "dttmw_editPop",
			NULL, 0);
	XtVaSetValues(_dttmEditW, XmNnoResize, True, NULL); 
	XtVaSetValues(XtParent(_dttmEditW), 
			XmNtitle, "Select Date and Time", 
			NULL);

        pane = XtVaCreateWidget("pane",
                        xmPanedWindowWidgetClass,  _dttmEditW,
			XmNsashWidth,              1, 
			XmNsashHeight,             1, 
                        NULL);

        rc0 = XtVaCreateWidget("rc",
                        xmRowColumnWidgetClass,  pane,
			XmNorientation,          XmVERTICAL, 
                        NULL);

	/*
	 * create arrow button widgets for year
	 */
        rc01 = XtVaCreateWidget("rc",
                        xmRowColumnWidgetClass,  rc0,
			XmNorientation,          XmHORIZONTAL, 
			XmNspacing,              30, 
                        NULL);
        XtVaCreateManagedWidget("Year",
                        xmLabelGadgetClass,  rc01,
                        NULL);
        rc02 = XtVaCreateWidget("rc",
                        xmRowColumnWidgetClass,  rc01,
			XmNorientation,          XmHORIZONTAL, 
                        NULL);
        arrow = XtVaCreateManagedWidget("arrow",
                        xmArrowButtonWidgetClass,  rc02,
                        XmNarrowDirection,         XmARROW_UP,
                        NULL);
	XtAddCallback(arrow, XmNactivateCallback, (XtCallbackProc)dttmw_yearCb,
				(XtPointer)0);

        _yearW = XtVaCreateManagedWidget("year",
                        xmLabelGadgetClass,  rc02,
                        NULL);

        arrow = XtVaCreateManagedWidget("arrow",
                        xmArrowButtonWidgetClass,  rc02,
                        XmNarrowDirection,         XmARROW_DOWN,
                        NULL);
	XtAddCallback(arrow, XmNactivateCallback, (XtCallbackProc)dttmw_yearCb,
				(XtPointer)1);
	XtManageChild(rc02);

	XtManageChild(rc01);

	/*
	 * create month editting widgets
	 */
        rc = XtVaCreateWidget("rc",
                        xmRowColumnWidgetClass,  rc0,
			XmNorientation,          XmHORIZONTAL, 
                        NULL);

        frame = XtVaCreateWidget("frame",
                        xmFrameWidgetClass,  rc,
                        NULL);
	
        rc1 = XtVaCreateWidget("rc",
                        xmRowColumnWidgetClass,  frame,
                        NULL);
	
        XtVaCreateManagedWidget("Sun Mon Tue  Wed  Thu   Fri    Sat",
                        xmLabelGadgetClass,  rc1,
                        NULL);

        rc2 = XtVaCreateWidget("dttmw_dayRc",
                        xmRowColumnWidgetClass,  rc1,
			XmNorientation,          XmHORIZONTAL, 
			XmNnumColumns,           6, 
			XmNpacking,              XmPACK_COLUMN, 
			XmNtraversalOn,          False, 
                        NULL);

	for ( ii = 0; ii < 6; ii++) { 
	    for ( jj = 0; jj < 7; jj++) { 
		_dayW[ii][jj] = XtVaCreateManagedWidget(" ",
                                xmPushButtonGadgetClass,  rc2,
			        XmNshadowThickness,       0,
			        XmNuserData,              jj,
                                NULL); 
		XtAddCallback(_dayW[ii][jj], XmNactivateCallback,
				(XtCallbackProc)dttmw_dayCb, (XtPointer)ii);
	    }
	}

	xmstr[0] = XmStringCreateLocalized("00");
	XtVaSetValues(_dayW[5][6], 
			XmNmappedWhenManaged, False,
			XmNlabelString,       xmstr[0],
			NULL);
	XmStringFree(xmstr[0]);

	XtManageChild(rc2);
	XtManageChild(rc1);
	XtManageChild(frame);

	/*
	 * create list for the months 
	 */
	n = XtNumber(_monthStr);
	for (ii = 0; ii < n; ii++)
		xmstr[ii] = XmStringCreateLocalized(_monthStr[ii]);

	_monListW=XmCreateScrolledList(rc, "dttmw_monList", NULL, 0);
	XtVaSetValues(_monListW,
			XmNitems,            xmstr, 
			XmNitemCount,        n, 
			XmNvisibleItemCount, n, 
			NULL);
	XtAddCallback(_monListW, XmNbrowseSelectionCallback, 
				dttmw_monthCb, NULL);
	for (ii = 0; ii < n; ii++)
		XmStringFree(xmstr[ii]);
	XtManageChild(_monListW);

	XtManageChild(rc);

	/*
	 * create  scale widget for hour
	 */
        rc3 = XtVaCreateWidget("rc",
                        xmRowColumnWidgetClass,  rc0,
			XmNorientation,          XmHORIZONTAL, 
			XmNspacing,              20, 
                        NULL);
        rc4 = XtVaCreateWidget("rc",
                        xmRowColumnWidgetClass,  rc3,
			XmNorientation,          XmVERTICAL, 
                        NULL);
        _hhmmW[0] = XtVaCreateManagedWidget("scale",
                        xmScaleWidgetClass,     rc4,
                        XmNorientation,         XmHORIZONTAL,
                        XmNprocessingDirection, XmMAX_ON_RIGHT,
                        XmNshowValue,           True,
                        XmNminimum,             0,
                        XmNmaximum,             23,
                        XmNvalue,               0,
                        NULL);
	XtAddCallback(_hhmmW[0], XmNdragCallback, 
				(XtCallbackProc)dttmw_hhmmCb, (XtPointer)1);
        XtAddCallback(_hhmmW[0], XmNvalueChangedCallback,
                                (XtCallbackProc)dttmw_hhmmCb, (XtPointer)1);

        XtVaCreateManagedWidget("Hour",
                        xmLabelGadgetClass,  rc4,
                        NULL);
	XtManageChild(rc4);

	/*
	 * create  scale widget for minutes
	 */
        rc5 = XtVaCreateWidget("rc",
                        xmRowColumnWidgetClass,  rc3,
			XmNorientation,          XmVERTICAL, 
                        NULL);
        _hhmmW[1] = XtVaCreateManagedWidget("scale",
                        xmScaleWidgetClass,     rc5,
                        XmNorientation,         XmHORIZONTAL,
                        XmNprocessingDirection, XmMAX_ON_RIGHT,
                        XmNshowValue,           True,
                        XmNminimum,             0,
                        XmNmaximum,             59,
                        XmNvalue,               0,
                        XmNwidth,               150,
                        NULL);
	XtAddCallback(_hhmmW[1], XmNdragCallback, 
		      (XtCallbackProc)dttmw_hhmmCb, (XtPointer)2);
        XtAddCallback(_hhmmW[1], XmNvalueChangedCallback,
                      (XtCallbackProc)dttmw_hhmmCb, (XtPointer)2);

        XtVaCreateManagedWidget("Minute",
                        xmLabelGadgetClass,  rc5,
                        NULL);
	XtManageChild(rc5);

	XtManageChild(rc3);

	XtManageChild(rc0);

	form = NxmCtlBtn_create(pane, 1, "dttmw_ctlB", 2, 
				btnstr, (XtCallbackProc)dttmw_ctlBtnCb, NULL);
	XtVaSetValues(form, XmNmarginHeight, 20, NULL);

	XtManageChild(pane);

	return(_dttmEditW);

}

/*=====================================================================*/

void dttmw_popup ( Widget parent, dttmi_t *dttm )
/************************************************************************
 * dttmw_popup								*
 *									*
 * This function pops up the date/time editting window.			*
 *									*
 * void dttmw_popup(parent, dttm)						*
 *									*
 * Input parameters:                                                    *
 *  parent 	widget ID					*
 *  *dttm	dttmi_t pointer to the dttm structure to be modified    *
 *									*
 * Output parameters:                                                   *
 *			NONE						*
 * Return parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI      04/96						*
 * J. Wu/SAIC      01/03   reset the last selected month		*
 * T. Lee/SAIC	   05/04   copied dttm to _dttmEdit			*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	dttm_copy(&_dttmEdit, dttm);
	dttm_copy(&_dttmCopy, dttm);

	_lastMon = _dttmCopy.mon;   /* reset the last seleced mon. */
	dttmw_yearSet();
 	dttmw_monthSet();
 	dttmw_hhmmSet();
	XtManageChild(_dttmEditW);

}

/*=====================================================================*/
/* ARGSUSED */
void dttmw_yearCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * dttmw_yearCb                                                         *
 *                                                                      *
 * Callback function for selecting the year (Arrow Buttons).            *
 *                                                                      *
 * void dttmw_yearCb(w, which, call)                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *  w		Widget		widget ID    				*
 *  which	long		up = 0, down = 1     			*
 *  call	XtPointer	not used     				*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 * T. Piper/SAIC	03/04	Removed checks on 1980 and 2100		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	if (which == 0) {
	    _dttmEdit.year ++;
	}
	else if (which == 1) {
	    _dttmEdit.year --;
	}

	dttmw_yearSet();
	dttmw_monthSet();

}
/*=====================================================================*/
/* ARGSUSED */
void dttmw_monthCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * dttmw_monthCb                                                        *
 *                                                                      *
 * Callback function for selecting the month (List).               	*
 *                                                                      *
 * void dttmw_monthCb(w, clnt, call)                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *  w		Widget     widget ID    				*
 *  clnt	XtPointer  not used     				*
 *  call	XtPointer  not used     				*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 * J. Wu/SAIC      01/03   record the last selected month		*
 ***********************************************************************/
{
XmListCallbackStruct *cbs = (XmListCallbackStruct *) call;

/*---------------------------------------------------------------------*/

	if ( cbs->item_position == ( _lastMon + 1 ) )
		return;

	_lastMon = cbs->item_position - 1;

	_dttmEdit.mon = _lastMon;

	dttmw_monthSet();

}

/*=====================================================================*/
/* ARGSUSED */
void dttmw_dayCb ( Widget w, long row, XtPointer call )
/************************************************************************
 * dttmw_dayCb                                                          *
 *                                                                      *
 * Callback function for selecting the day (Pushbutton).               	*
 *                                                                      *
 * void dttmw_dayCb(w, row, call)                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *  w		Widget		widget ID    				*
 *  row		long		the row number where the button at     	*
 *  call	XtPointer	not used     				*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 * C. Lin/EAI      03/97	add free xmstr                          *
 * C. Lin/EAI      06/98	use XtFree 				*
 * m.gamazaychikov 04/06	change col type to long			*
 ***********************************************************************/
{
long     col;
XmString xmstr;
char     *day;

/*---------------------------------------------------------------------*/

	if ( _lastDay[0] != -1)
	    XtVaSetValues(_dayW[_lastDay[0]][_lastDay[1]],
			XmNsensitive, 	    True, 
			XmNshadowThickness, 0, 
			NULL);

	XtVaGetValues(w, XmNuserData,    &col,
			 XmNlabelString, &xmstr,
			 NULL);
	XmStringGetLtoR(xmstr, XmFONTLIST_DEFAULT_TAG, &day);
	XmStringFree(xmstr);

	_lastDay[0]    = (int)row;
	_lastDay[1]    = (int)col;
	_dttmEdit.day = atoi(day);

	XtFree(day);

	XtVaSetValues(w, XmNsensitive,       False, 
			 XmNshadowThickness, 2, 
			 NULL);

}

/*=====================================================================*/
/* ARGSUSED */
void dttmw_hhmmCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * dttmw_hhmmCb                                                         *
 *                                                                      *
 * Callback function for selecting the year/hour/minute (Scale).        *
 *                                                                      *
 * void dttmw_hhmmCb(w, which, call)                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *  w		Widget		widget ID    				*
 *  which	long		1=hour, 2=minutes     			*
 *  call	XtPointer	not used     				*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 ***********************************************************************/
{
XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;

/*---------------------------------------------------------------------*/

	switch (which) {

	    case 0:	/* year */
			_dttmEdit.year = cbs->value;
			dttmw_monthSet();
			break;

	    case 1:	/* hour */
			_dttmEdit.hour = cbs->value;
			break;

	    case 2:     /* minutes */
			_dttmEdit.min = cbs->value;
			break;

	}

}

/*=====================================================================*/
/* ARGSUSED */
void dttmw_ctlBtnCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * dttmw_ctlBtnCb                                                       *
 *                                                                      *
 * Callback function for the control buttons at the bottom.             *
 *                                                                      *
 * void dttmw_ctlBtnCb(w, which, call)                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *  w		Widget     widget ID    				*
 *  which	long        which button     				*
 *  call	XtPointer  not used     				*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96                                           *
 * E. Safford/GSC	10/99	remove timeline reset 			*
 * E. Safford/GSC	03/00	add updt flag to dtmbw_dttmSet     	*
 * S. Jacobs/NCEP	10/01	Added call to set the image navigation	*
 * T. Lee/SAIC		05/04	called dataw_setNewRefTm		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	switch( which ) {

	    case 0:	/* Accept */

			/* 
			 * set the date/time for the reference time.
			 */

			dataw_setNewRefTm (&_dttmEdit);

			/*
			 * If an image is present, set the navigation.
			 */
			dataw_setImageNav ( );

			break;

	    case 1:	/* Close */
			
			/*
			 * retrieve the original copy
			 */
			_dttmEdit.year = _dttmCopy.year;
			_dttmEdit.mon  = _dttmCopy.mon;
			_dttmEdit.day  = _dttmCopy.day;
			_dttmEdit.hour = _dttmCopy.hour;
			_dttmEdit.min  = _dttmCopy.min;

			break;
	}

	/*
	 * close the date/time editting window
	 */
	XtUnmanageChild(_dttmEditW);

}

/*=====================================================================*/

void dttmw_yearSet ( void )
/************************************************************************
 * dttmw_yearSet                                                        *
 *                                                                      *
 * Set the year label.               					*
 *                                                                      *
 * void dttmw_yearSet()                                     		*
 *                                                                      *
 * Input parameters:                                                    *
 *                      NONE                                            *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 ***********************************************************************/
{
char     str[5];
XmString xmstr;

/*---------------------------------------------------------------------*/

        sprintf(str, "%4d", _dttmEdit.year);
        xmstr = XmStringCreateLocalized(str);
        XtVaSetValues(_yearW, XmNlabelString, xmstr, NULL);
        XmStringFree(xmstr);

}

/*=====================================================================*/

void dttmw_monthSet ( void )
/************************************************************************
 * dttmw_monthSet                                                        *
 *                                                                      *
 * Set the month defined in the *_dttmEdit.               		*
 *                                                                      *
 * void dttmw_monthSet( )                                      		*
 *                                                                      *
 * Input parameters:                                                    *
 *                      NONE                                            *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96                                           *
 * E. Safford/GSC	03/99	clean up             			*
 * S. Jacobs/NCEP	 1/01	Fixed encoding of day into a string	*
 * J. Wu/GSC       	05/01	free XmStrings    			*
 ***********************************************************************/
{
int      idtarr[5], idayw, itotal, day, iret;
int      ii, jj;
char     str[3];
XmString xmstr, blank;
/*---------------------------------------------------------------------*/

	idtarr[0] = _dttmEdit.year;
	idtarr[1] = _dttmEdit.mon +1;
	idtarr[2] = 1;
	idtarr[3] = 0;
	idtarr[4] = 0;

	XmListSelectPos(_monListW, idtarr[1], False);

	ti_daym(&idtarr[0], &idtarr[1], &itotal, &iret);
	if ( iret != 0 )
		NxmErr_update();
	ti_dayw(idtarr, &idayw, &iret);
	if ( iret != 0 )
		NxmErr_update();
	idayw--;

	if (_lastDay[0] != -1) 
		XtVaSetValues(_dayW[_lastDay[0]][_lastDay[1]],
			XmNsensitive,         True,
			XmNshadowThickness,   0,
			NULL);

	day = 0;
	blank = XmStringCreateLocalized("  ");
	for ( ii = 0; ii < 6; ii++ ) {
	    for (jj = 0; jj < 7; jj++) {
		if ((ii == 0 && jj >= idayw) || (ii > 0 && day < itotal)) { 
			day++;
			if ( day > 9 ) {
			    sprintf(str, "%2d", day);
			}
			else {
			    sprintf(str, " %d", day);
			}

			xmstr = XmStringCreateLocalized(str);
			if ( day == _dttmEdit.day ) {
			    XtVaSetValues(_dayW[ii][jj],
				XmNlabelString,       xmstr,
				XmNsensitive,         False,
				XmNshadowThickness,   2,
				NULL);
			    _lastDay[0] = ii;
			    _lastDay[1] = jj;
			}
			else
			    XtVaSetValues(_dayW[ii][jj],
				XmNlabelString, xmstr,
				XmNsensitive,   True,
				NULL);
			XmStringFree(xmstr);
		}
		else {
			XtVaSetValues(_dayW[ii][jj],
				XmNlabelString, blank,
				XmNsensitive,   False,
				NULL);
		}
	    }
	}
	
	XmStringFree(blank);			

}

/*=====================================================================*/

void dttmw_hhmmSet ( void ) 
/************************************************************************
 * dttmw_hhmmSet                                                        *
 *                                                                      *
 * Set the hour/minutes scale.               				*
 *                                                                      *
 * void dttmw_hhmmSet()                                     		*
 *                                                                      *
 * Input parameters:                                                    *
 *                      NONE                                            *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      04/96                                                *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	XtVaSetValues(_hhmmW[0], XmNvalue, _dttmEdit.hour, NULL);
	XtVaSetValues(_hhmmW[1], XmNvalue, _dttmEdit.min, NULL);


}

/*=====================================================================*/

Boolean dttmw_isUp ( void )
/************************************************************************
 * dttmw_isUp								*
 *									*
 * This function returns the current state of the calendar edit window. *
 *									*
 * Boolean dttmw_isUp()							*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * dttmw_isUp           Boolean         TRUE if dataw window is up      *
 *									*
 **									*
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/SAIC		05/04	initial coding				*
 ***********************************************************************/
 {
    return ( XtIsManaged(_dttmEditW) );
 }

/*=====================================================================*/

void dttmw_popdown ( void )
/************************************************************************
 * dttmw_popdown                                                        *
 *									*
 * This function pops down the calendar edit window.			*
 *									*
 * void dataw_popdown()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:                                                                 *
 * T. Lee/SAIC		05/04	initial coding				*
 ***********************************************************************/
{
    if ( dttmw_isUp() ) {
	XtUnmanageChild (_dttmEditW);
   }
}
/*=====================================================================*/
