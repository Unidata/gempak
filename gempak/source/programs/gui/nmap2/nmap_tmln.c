#include "geminc.h"
#include "gemprm.h"
#include "nmapprm.h"
#include "nmap_dttm.h"
#include "nmap_data.h"
#include "ctbcmn.h"


#define MAX_AVAIL	(MAX_PIXMAP + 5)
#define WIN_HEIGHT	115
#define WIN_WIDTH	660

/*
 * date line
 */
#define DL_START	 15
#define DL_END		(WIN_WIDTH - DL_START)
#define DL_XOFF		  5
#define DL_YTOP		  0		/* top edge */
#define DL_YMID		(DL_YTOP + 10)	/* middle */
#define DL_YBOT		(DL_YMID + 15)	/* bottom edge */
#define DL_SPLIT	 10		/* breakpoint for spliting month/day */

/*
 * time line
 */
#define TL_START	 15
#define TL_END		(WIN_WIDTH - TL_START)
#define TL_WIDTH	(TL_END - TL_START)
#define TL_XMID		(TL_START + (TL_WIDTH / 2))
#define TL_YMID		 72		/* middle */
#define TL_YTOP		(TL_YMID - 15)	/* top edge */
#define TL_YBOT		(TL_YMID + 14)	/* bottom edge */
#define TL_TICK		  5
#define TL_TICK2	  7

/*
 * available or selected time marks
 */
#define MARK_WD		  4
#define MARK_XOFF	(MARK_WD / 2)
#define MARK_HT		  8
#define MARK_YTOP	(TL_YMID - (MARK_HT / 2))

/*
 * font adjustments
 */
#define FONT_XOFF2	  7	/* adjust for 2 characters */
#define FONT_XOFF5	 15	/* adjust for 5 characters */
#define FONT_YOFF	 12

/*
 * foot note
 */
#define FN_MK1X		  5		/* first mark location */
#define FN_TX1X		(FN_MK1X + 15)	/* first text location */
#define FN_MK2X		(FN_MK1X + 140)	/* second mark location */
#define FN_TX2X		(FN_MK2X + 15)	/* second text location */
#define FN_YBOT		(WIN_HEIGHT - 2)
#define FN_YTOP		(FN_YBOT - 7)

/*
 * selection box
 */
#define BOX_YTOP	(TL_YTOP + 4)
#define BOX_HT		((TL_YMID - BOX_YTOP) * 2)
#define BOX_MIN		(TL_START - 5)
#define BOX_MAX		(TL_END + 5)
#define MOTION_CHECK	  4

#define MinToWin(a)	(((a * TL_WIDTH) / _totalMin) + TL_START)
#define WinToMin(a)	(((a - TL_START) * _totalMin) / TL_WIDTH)

struct dtg_xcoor {
    int x;	/* x coordinate */
    int i1;	/* value 1 */
    int i2;	/* value 2 */
};

struct dtg_xcoor _monDay[MAX_AVAIL];	/* value 1 = month, value 2 = day */
struct dtg_xcoor _hrMin[MAX_AVAIL];	/* value 1 = hour,  value 2 = minute */
struct dtg_xcoor _tickMark[MAX_AVAIL];	/* value 1 = hour,  value 2 = minute */

static Widget	_tmlnDrawA;		/* drawing canvas widget */
static Window	_tmlnWin;

static GC	_drawGc;
static GC	_boxGc;

static dttms_t	_availTimes[MAX_AVAIL];
static Boolean	_selectTimes[MAX_AVAIL];
static dttms_t  _lastim[MAX_LOOP];

static Boolean	_timeInfoFlag 	= FALSE; /* flag for initilizing globals */
static Boolean	_splitDate	= FALSE;
static Boolean	_minCheck;
static Boolean	_directionFlag;		/* prepend or append new frames */

static int	_totalMin	= 1;
static int	_totalFrames	= 1;
static int	_nAvailable 	= 0;
static int	_nDates 	= 1;
static int	_skipFactor	= 0;

static int	_boxStart;
static int	_boxEnd;
static int	_boxOffset;
static int	_icatg;

XFontStruct	*font = NULL;

/* 
 *  private callback functions
 */
void tmln_exposeCb ( Widget, XtPointer, XtPointer );
void tmln_selectCb ( Widget, XtPointer, XEvent* );

/*
 *  private functions
 */
void tmln_drawBox ( int firstx, int secondx );
void tmln_selectRange ( int startx, int endx, int state, Boolean set_flag );
void tmln_setupGc ( int flag );


/************************************************************************
 * nmap_tmln.c								*
 *									*
 * This module defines the dominate time line object for nmap.		*
 *									*
 * CONTENTS:								*
 *									*
 *	tmln_createCanvas()	create the drawing canvas		*
 *	tmln_setTimeInfo()	set the currently available times	*
 *	tmln_setTotalFrame()	set the current total number of frames	*
 *	tmln_setSkipFactor()	set the number of frames to skip	*
 *	tmln_clearTimeInfo()	clear time info flag			*
 *	tmln_drawClear()	clear the drawing canvas		*
 *	tmln_redraw()		redraw the drawing canvas		*
 *									*
 *	tmln_getTimeInfo()	return the currently set times		*
 *	tmln_getSelected()	return the currently selected times	*
 *	tmln_getSkipFactor()	returns the current skip factor		*
 *	tmln_getNewTimes()	returns the updated set of times	*
 *									*
 *	tmln_exposeCb()		callback function for expose event	*
 *	tmln_selectCb()		controls the selection process		*
 *									*
 *	tmln_selectRange()	selects a range of times		*
 *	tmln_setupGc()		sets up the various GCs			*
 *	tmln_drawBox()		draws the selection box			*
 ***********************************************************************/

/*=====================================================================*/

Widget tmln_createCanvas ( Widget parent )
/************************************************************************
 * tmln_createCanvas							*
 *									*
 * This function create the drawing canvas.				*
 *									*
 * Widget tmln_createCanvas (parent)					*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget ID			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *	tmln_createCanvas 	Widget	???				*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial work				*
 ***********************************************************************/
{

    _tmlnDrawA = XtVaCreateManagedWidget("tmln_canvas",
					 xmDrawingAreaWidgetClass, parent,
					 XmNwidth,  WIN_WIDTH, 
					 XmNheight, WIN_HEIGHT, 
					 NULL);

    XtAddEventHandler(_tmlnDrawA, ExposureMask, FALSE,
		      (XtEventHandler) tmln_exposeCb, NULL);

    XtAddEventHandler(_tmlnDrawA, 
		      ButtonMotionMask | ButtonReleaseMask | ButtonPressMask, 
		      FALSE, (XtEventHandler) tmln_selectCb, NULL);

    return(_tmlnDrawA);
}

/*=====================================================================*/

void tmln_setTimeInfo ( Boolean direction, int ntimes, dttms_t *tarry, 
							Boolean *select )
/************************************************************************
 * tmln_setTimeInfo							*
 *									*
 * This function receives the available times and which of those times	*
 * are currently selected.						*
 *									*
 * void tmln_setTimeInfo (direction, ntimes, tarry, select)		*
 *									*
 * Input parameters:							*
 *	direction	Boolean	0 = backwards, 1 = forwards		*
 *	ntimes		int	number of available times		*
 *	*tarry		dttms_t	array of available times		*
 *	*select		Boolean	array of selection flags		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial coding				*
 * S. Law/GSC		09/98	Made changes to filling _monDay array	*
 * S. Law/GSC		12/98	switched to using dttms_t		*
 * T. Lee/SAIC		01/04	corrected time line label		*
 ***********************************************************************/
{
    int		ii, jj, kk, iret, iarray[5], min, tintv, moncheck;
    int		delta, dayarray[5];
    dttms_t	dtgstr, daystr;
/*---------------------------------------------------------------------*/

    _directionFlag = direction;
    _boxStart = _boxEnd = 0;

    if (ntimes > MAX_AVAIL) {
	_nAvailable = MAX_AVAIL;
	kk = ntimes - MAX_AVAIL;
    }
    else {
	_nAvailable = ntimes;
	kk = 0;
    }

    ti_diff (tarry[(_nAvailable-1)], tarry[0], 
	     &_totalMin, &iret, (DTTMS_SIZE-1), (DTTMS_SIZE-1));

    if (iret != 0) {	/* bad date(s) */
	ti_diff (tarry[(_nAvailable-1)], tarry[1], 
		 &_totalMin, &iret, (DTTMS_SIZE-1), (DTTMS_SIZE-1));
    }

    if (iret != 0) {	/* more bad date(s) */
	ti_diff (tarry[(_nAvailable-2)], tarry[0], 
		 &_totalMin, &iret, (DTTMS_SIZE-1), (DTTMS_SIZE-1));
    }


    if (iret != 0) {		/* too many bad date(s), using default */
	_totalMin = 1440;	/* one day */
    }

    _nDates = 0;
    _totalFrames = 0;
    _boxOffset = WIN_WIDTH;

/*
 * ii is where the time is being placed
 * jj is where the time is coming from
 */
    for (ii = 0, jj = kk; ii < _nAvailable; ii++, jj++) {
	strcpy (_availTimes[ii], tarry[jj]);
	_selectTimes[ii] = select[jj];
	if (_selectTimes[ii]) _totalFrames++;

	ti_ctoi (_availTimes[ii], iarray, &iret, (DTTMS_SIZE-1));
 
	if (iret != 0) {	/* skip invalid date */
	    _nAvailable--;
	    if (_selectTimes[ii]) _totalFrames--;
	    ii--;
	}
	else {

/*
 * Set time line x values
 */
	    ti_diff (_availTimes[ii], _availTimes[0], 
		     &min, &iret, (DTTMS_SIZE-1), (DTTMS_SIZE-1));

	    if (_nAvailable == 1) {
		_totalMin = 1440;
		_hrMin[ii].x = TL_XMID;
		_hrMin[ii].x = MinToWin (min);
	    }
	    else {
		_hrMin[ii].x = MinToWin (min);
	    }

	    _hrMin[ii].i1 = iarray[3];	/* set hour */
	    _hrMin[ii].i2 = iarray[4];	/* set minute */

	    if (ii == 0) {
		_minCheck = FALSE;	
	    }
	    else {
		if (_hrMin[ii].i2 != _hrMin[0].i2 ||
		    _hrMin[ii].i2 != _hrMin[ii-1].i2 ||
		    _icatg == CAT_IMG ) {
		    _minCheck = TRUE;
		}
		delta = _hrMin[ii].x - _hrMin[ii-1].x;
		if (delta < _boxOffset) _boxOffset = delta;
	    }

	    if (_nAvailable == 1) {
		_boxOffset = 10;
	    }

/*
 * Set initial date line x values
 */
	    if  (ii == 0) {
		_monDay[_nDates].x = DL_START;
		_monDay[_nDates].i1 = iarray[1];	/* set month */
		_monDay[_nDates].i2 = iarray[2];	/* set day */
		_nDates++;
		dayarray[0] = iarray[0];
		dayarray[1] = iarray[1];
		dayarray[2] = iarray[2];
		dayarray[3] = 0;
		dayarray[4] = 0;
	    }
	}
    }

    _boxOffset /= 2;

    moncheck = FALSE;
    while (_monDay[_nDates - 1].x < DL_END && _nDates != MAX_AVAIL) {
	ti_addd (dayarray, dayarray, &iret);
	ti_itoc (dayarray, daystr, &iret, (DTTMS_SIZE-1));
	ti_diff (daystr, _availTimes[0], &min, &iret, 
		 (DTTMS_SIZE-1), (DTTMS_SIZE-1));

	_monDay[_nDates].x = MinToWin (min);
	_monDay[_nDates].i1 = dayarray[1];	/* set month */
	_monDay[_nDates].i2 = dayarray[2];	/* set day */
	_nDates++;
	if (_monDay[_nDates - 1].i1 != _monDay[_nDates].i1) {
	    moncheck = TRUE;
	}
    }
    _nDates--;

    _timeInfoFlag = TRUE;

    if (_minCheck) { 
	tintv = 15;	/* 1/4 hour intervals */
	if ((_totalMin / tintv) > (MAX_AVAIL-10)) tintv =  30;	/* 1/2 hour */
	if ((_totalMin / tintv) > (MAX_AVAIL-10)) tintv =  60;	/*   1 hour */
    }
    else {
	tintv = 60;	/* 1 hour intervals */
    }

    if ((_totalMin / tintv) > MAX_AVAIL) tintv =  180;		/*  3 hours */
    if ((_totalMin / tintv) > MAX_AVAIL) tintv =  360;		/*  6 hours */
    if ((_totalMin / tintv) > MAX_AVAIL) tintv =  720;		/* 12 hours */
    if ((_totalMin / tintv) > MAX_AVAIL) tintv = 1440;		/* 24 hours */

    strncpy (dtgstr, _availTimes[0], 7);
    dtgstr[7] = '\0';
    strcat  (dtgstr, "0000\0");
    ti_diff (dtgstr, _availTimes[0], 
	     &min, &iret, (DTTMS_SIZE-1), (DTTMS_SIZE-1));
    ti_ctoi (dtgstr, iarray, &iret, (DTTMS_SIZE-1));

    if (_nAvailable > 1) {
	while (min < 0) {
	    ti_addm (iarray, &tintv, iarray, &iret);
	    ti_itoc (iarray, dtgstr, &iret, (DTTMS_SIZE-1));
	    dtgstr[DTTMS_SIZE - 1] = '\0';
	    ti_diff (dtgstr, _availTimes[0], 
		     &min, &iret, (DTTMS_SIZE-1), (DTTMS_SIZE-1));
	}
    }

    for (ii = 0; ii < MAX_AVAIL; ii++) {
	_tickMark[ii].x = MinToWin (min);
	_tickMark[ii].i1 = iarray[3];	/* set hour */
	_tickMark[ii].i2 = iarray[4];	/* set minute */

	ti_addm (iarray, &tintv, iarray, &iret);
	ti_itoc (iarray, dtgstr, &iret, (DTTMS_SIZE-1));
	dtgstr[DTTMS_SIZE - 1] = '\0';
	ti_diff (dtgstr, _availTimes[0], 
		 &min, &iret, (DTTMS_SIZE-1), (DTTMS_SIZE-1));
    }

    _splitDate = (Boolean)((moncheck && _nDates > DL_SPLIT));

}

/*=====================================================================*/

void tmln_setTotalFrame ( int nframes )
/************************************************************************
 * tmln_setTotalFrame							*
 *									*
 * This function sets the current total number of frames.		*
 *									*
 * void tmln_setTotalFrame (nframes)					*
 *									*
 * Input parameters:							*
 *	nframes		int	total number of frames			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial coding				*
 * S. Law/GSC		06/00	removed parameter from dataw_updSelTimes*
 ***********************************************************************/
{
    int current, ii, first, last, direction, ndir, skip;
/*---------------------------------------------------------------------*/

    _totalFrames = nframes;

    current = 0;
    first = -1;
    last = 0;
    for (ii = 0; ii < _nAvailable; ii++) {
	if (_selectTimes[ii]) {
	    if (first == -1) first = ii;
	    last = ii;
	    current++;
	}
    }

    direction = (int)_directionFlag;
    ndir = 0;
    skip = _skipFactor + 1;

    while (current != _totalFrames) {

/*
 * add frame to start
 */
	if (current < _totalFrames && direction == 0) {
	    if ((first - skip) < 0) {
		direction = (direction == 1) ? 0 : 1;
		ndir++;
	    }
	    else {
		first -= skip;
		_selectTimes[first] = TRUE;
		current++;
	    }
	}

/*
 * add frame to end
 */
	else if (current < _totalFrames && direction == 1) {
	    if ((last + skip) >= _nAvailable) {
		direction = (direction == 1) ? 0 : 1;
		ndir++;
	    }
	    else {
		last += skip;
		_selectTimes[last] = TRUE;
		current++;
	    }
	}

/*
 * remove frame from start
 */
	else if (current > _totalFrames && direction == 0) {
	    _selectTimes[first] = FALSE;
	    current--;
	    while (!_selectTimes[first]) first++;
	}

/*
 * remove frame from end
 */
	else if (current > _totalFrames && direction == 1) {
	    _selectTimes[last] = FALSE;
	    current--;
	    while (!_selectTimes[last]) last--;
	}

	if (ndir > 1) {
	    _totalFrames = current;
  	    dataw_updSelTimes (); 
	}
    }
    _boxStart = _boxEnd = 0;
    tmln_redraw (0);
}

/*=====================================================================*/

void tmln_setSkipFactor ( int skip, Boolean adjust )
/************************************************************************
 * tmln_setSkipFactor							*
 *									*
 * This function sets the number of frames to skip between selected	*
 * frames.								*
 *									*
 * void tmln_setSkipFactor (skip, adjust)				*
 *									*
 * Input parameters:							*
 *	skip		int	number of frames to skip		*
 *	adjust		Boolean	whether to adjust current selections	*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial coding				*
 * S. Law/GSC		09/98	Removed direction checking		*
 * S. Law/GSC		12/98	Added adjust parameter			*
 * S. Law/GSC		06/00	removed parameter from dataw_updSelTimes*
 ***********************************************************************/
{
    int	ii, first, last;
/*---------------------------------------------------------------------*/

    if (_skipFactor != skip) {
	_skipFactor = skip;

	if (adjust && _nAvailable > 0) {
	    first = -99;
	    for (ii = 0; ii < _nAvailable; ii++) {
		if (_selectTimes[ii]) {
		    if (first == -99) first = ii;
		    last = ii;
		    _selectTimes[ii] = FALSE;
		}
	    }

	    tmln_selectRange (_boxStart, _boxEnd, 1, TRUE);

	    _totalFrames = 0;
	    for (ii = 0; ii < _nAvailable; ii++) {
		if (_selectTimes[ii]) _totalFrames++;
	    }

	    if (_totalFrames <= 0) {
		ii = (_directionFlag == 0) ? last : first;
		_selectTimes[ii] = TRUE;
		_totalFrames = 1;
	    }

  	    dataw_updSelTimes ();

	    tmln_redraw (0);
	}
    }
}

/*=====================================================================*/

void tmln_clearTimeInfo ( void )
/************************************************************************
 * tmln_clearTimeInfo							*
 *									*
 * This function clears the time_info flag, therefore it will not try	*
 * to draw the time line.						*
 *									*
 * void tmln_clearTimeInfo ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		10/96						*
 ***********************************************************************/
{
    _timeInfoFlag = FALSE;
}

/*=====================================================================*/

void tmln_drawClear ( void )
/************************************************************************
 * tmln_drawClear							*
 *									*
 * This function clear the drawing area.				*
 *									*
 * void tmln_drawClear ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		04/96						*
 * T. Piper/SAIC	05/03	removed xwcmn.h and added XtDisplay()	*
 ***********************************************************************/
{
    if (XtDisplay(_tmlnDrawA) && _tmlnWin) {
	XClearArea (XtDisplay(_tmlnDrawA), _tmlnWin, 0, 0, 
		    WIN_WIDTH, WIN_HEIGHT, FALSE);
    }
}

/*=====================================================================*/

void tmln_redraw ( int box_motion )
/************************************************************************
 * tmln_redraw								*
 *									*
 * This function redraws the drawing area.				*
 *									*
 * void tmln_redraw(box_motion)						*
 *									*
 * Input parameters:							*
 *	box_motion	int	x offset for box motion			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NULL						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial coding				*
 * S. Law/GSC		09/98	Various cleanup				*
 * T. Piper/SAIC        05/03   removed xwcmn.h and added XtDisplay()  *
 ***********************************************************************/
{
    int		ii, jj, tmpx, tmpy, time, timex, isize;
    int		lastminx, lasttickx, fontxoff, tlabel;
    char	label[20];
    char	*month_names[]= {"Jan", "Feb", "Mar", "Apr", 
				     "May", "Jun", "Jul", "Aug", 
				     "Sep", "Oct", "Nov", "Dec" };
/*---------------------------------------------------------------------*/

    if (!_timeInfoFlag) return;

    if (!_tmlnWin) tmln_setupGc (-1);

    tmln_drawClear ();

/*
 * Draw date line
 */
    tmln_setupGc (0);
    XDrawLine(XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc, 
	      DL_START, DL_YBOT, DL_END, DL_YBOT);

    fontxoff = (_splitDate) ? FONT_XOFF2 : FONT_XOFF5;
    for (ii = 0; ii < _nDates; ii++) {
	if (ii == 0) {
	    time  = _monDay[ii].i1;
	    timex = _monDay[ii].x;
	}
	else {
	    if (_splitDate && 
		(time != _monDay[ii].i1 || 
		 ii == (_nDates - 1))) {
		tmpx = ((_monDay[ii].x - timex) / 2) + timex - FONT_XOFF2;
		sprintf (label, "%s", month_names[time-1]);

		if ((_monDay[ii].x - timex) > 30) {
		    XDrawString(XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc, tmpx, 
				DL_YMID, label, (int)strlen (label));
		}

		timex = _monDay[ii].x;
	    }

	    if (time == _monDay[ii].i1) {	/* short line for same day */
		tmpy = DL_YMID;
	    }
	    else {				/* tall line for next day */
		tmpy = DL_YTOP;
		time = _monDay[ii].i1;
	    }

	    XDrawLine(XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc, 
		      _monDay[ii].x, tmpy, _monDay[ii].x, DL_YBOT);
	}

	if (_splitDate) {
	    sprintf (label, "%2.2d", _monDay[ii].i2);
	    isize = 20;
	}
	else {
	    sprintf (label, "%s%2.2d", month_names[_monDay[ii].i1-1], 
		     _monDay[ii].i2);
	    isize = 40;
	}

	if (ii == (_nDates-1)) {
	    tmpx =  ((DL_END - _monDay[ii].x) / 2) + 
		_monDay[ii].x - fontxoff;
	    if ((DL_END - _monDay[ii].x) < isize) isize = -1;
	}
	else {
	    tmpx = ((_monDay[ii+1].x - _monDay[ii].x) / 2) + 
		_monDay[ii].x - fontxoff;
	    if ((_monDay[ii+1].x - _monDay[ii].x) < isize) isize = -1;
	}

	if (isize > 0) {
	    XDrawString(XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc, tmpx, 
			DL_YBOT, label, (int)strlen (label));
	}
    }

/*
 * Draw time line
 */
    XDrawLine (XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc, 
	      TL_START, TL_YMID, TL_END, TL_YMID);

/*
 * Draw tick marks
 */
    ii = 0;
    tlabel = _hrMin[0].i1;
    lasttickx = -99;
    while (_tickMark[ii].x < TL_END && ii != MAX_AVAIL) {
	if (_minCheck) {
	    isize = (_tickMark[ii].i2 == 0) ? TL_TICK2 : TL_TICK;
	}
	else {
	    isize = ((_tickMark[ii].i1 % 3) == 0) ? TL_TICK2 : TL_TICK;
	}

	XDrawLine (XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc, 
		   _tickMark[ii].x, (TL_YMID + isize), 
		   _tickMark[ii].x, (TL_YMID - isize));

	if ((ii == 0 || tlabel != _tickMark[ii].i1) &&
	    isize == TL_TICK2 &&
	    (_tickMark[ii].x - lasttickx) > 20) {

	    tlabel = _tickMark[ii].i1;
	    sprintf (label, "%2.2d", tlabel);
	    XDrawString(XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc, 
			_tickMark[ii].x - FONT_XOFF2, TL_YTOP, 
			label, (int)strlen (label));

	    lasttickx = _tickMark[ii].x;
	}

	ii++;
    }

/*
 * Draw marks for available (blue) and selected (red) times
 */
    lastminx = -99;
    for (ii = 0; ii < _nAvailable; ii++) {
	tlabel = (_minCheck) ? _hrMin[ii].i2 : _hrMin[ii].i1;
	sprintf (label, "%2.2d", tlabel);

	if ((_hrMin[ii].x - lastminx) > 20) {
	    tmln_setupGc (0);
	    XDrawString(XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc, 
			_hrMin[ii].x - FONT_XOFF2, TL_YBOT + FONT_YOFF, 
			label, (int)strlen (label));

	    lastminx = _hrMin[ii].x;
	}

	jj = (_selectTimes[ii]) ? 1 : 2;
	tmln_setupGc (jj);
	XFillRectangle (XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc,
			_hrMin[ii].x - MARK_XOFF, MARK_YTOP, 
			MARK_WD, MARK_HT);
    }

/*
 * Draw footnote
 */
    tmln_setupGc (2);
    XFillRectangle (XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc,
		    FN_MK1X, FN_YTOP, MARK_WD, MARK_HT);

    tmln_setupGc (1);
    XFillRectangle (XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc,
		    FN_MK2X, FN_YTOP, MARK_WD, MARK_HT);

    tmln_setupGc (0);
    sprintf (label, "available frame");
    XDrawString(XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc, 
		FN_TX1X, FN_YBOT, label, (int)strlen (label));

    sprintf (label, "selected frame");
    XDrawString(XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc, 
		FN_TX2X, FN_YBOT, label, (int)strlen (label));

/*
 * Draw selection box
 */
    if (_boxStart == 0 && _boxEnd == 0) {
	for (ii = 0; ii < _nAvailable; ii++) {
	    if (_selectTimes[ii]) {
		if (_boxStart == 0) _boxStart = _hrMin[ii].x;
		_boxEnd = _hrMin[ii].x;
	    }
	}

	if (_boxStart != 0) {
	    _boxStart -= _boxOffset;
	    _boxEnd   += _boxOffset;

	    tmln_drawBox (_boxStart, _boxEnd);
	}
    }
    else {
	_boxStart += box_motion;
	_boxEnd   += box_motion;

	tmln_drawBox (_boxStart, _boxEnd);
    }
}

/*=====================================================================*/

void tmln_getTimeInfo ( int *ntimes, dttms_t *tarry )
/************************************************************************
 * tmln_getTimeInfo							*
 *									*
 * This function returns the currently set times.			*
 *									*
 * void tmln_getTimeInfo (ntimes, tarry)				*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*ntimes		int	number of selected times		*
 *	*tarry		dttms_t	array of selected times			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial coding				*
 * S. Law/GSC		12/98	Added _timeInfoFlag test		*
 ***********************************************************************/
{
    int	ii;
/*---------------------------------------------------------------------*/

    if (_timeInfoFlag) {
	for (ii = 0; ii < _nAvailable; ii++) {
	    strcpy (tarry[ii], _availTimes[ii]);
	}

	*ntimes = _nAvailable;
    }
    else {
	*ntimes = 0;
    }
}

/*=====================================================================*/

void tmln_getSelected ( int *ntimes, dttms_t *tarry )
/************************************************************************
 * tmln_getSelected							*
 *									*
 * This function returns the currently selected times.			*
 *									*
 * void tmln_getSelected (ntimes, tarry)				*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*ntimes		int	number of selected times		*
 *	*tarry		dttms_t	array of selected times			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial coding				*
 * S. Law/GSC		12/98	switched to using dttms_t		*
 * S. Law/GSC		06/00	clear strings past selected times	*
 ***********************************************************************/
{
    int	ii, nn;
/*---------------------------------------------------------------------*/

    nn = 0;

    if (_timeInfoFlag) {
	for (ii = 0; ii < _nAvailable; ii++) {
	    if (_selectTimes[ii]) {
		strcpy (tarry[nn], _availTimes[ii]);
		nn++;
	    }
	}
    }

    *ntimes = nn;

    for (ii = nn; ii < _nAvailable; ii++) {
	strcpy (tarry[ii], "");
    }
}

/*=====================================================================*/

void tmln_getSkipFactor ( int *skip )
/************************************************************************
 * tmln_getSkipFactor							*
 *									*
 * This function returns the current number of frames to skip between	*
 * selected frames.							*
 *									*
 * void tmln_getSkipFactor (skip)					*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*skip		int	number of frames to skip		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial coding				*
 ***********************************************************************/
{
    *skip = _skipFactor;
}

/*=====================================================================*/

int tmln_getNewTimes ( dsrc_t *source, char *time, int *ntimes, 
						dttms_t new_times[] )
/************************************************************************
 * tmln_getNewTimes							*
 *									*
 * This function returns an updated list of times.			*
 *									*
 * int tmln_getNewTimes (source, time, ntimes, new_times)		*
 *									*
 * Input parameters:							*
 *	*source		dsrc_t	current source				*
 *	*time		char	gempak time for data frame selection	*
 *									*
 * Output parameters:							*
 *	*ntimes		int	number of times found, up to MAX_FRAME	*
 *	new_times[]	dttms_t	time array (minimum size is MAX_FRAME)	*
 *	tmln_getNewTimes	int	nframes from datatype.tbl	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		06/00	Initial coding				*
 * S. Law/GSC		06/00	fixed CAT_SFC/SFF			*
 * E. Safford/GSC	06/00	fix bug in CAT_VGF 			*
 * S. Jacobs/NCEP	 7/00	Removed address from strings in		*
 *				call to ctb_dtget			*
 * S. Jacobs/NCEP	 7/00	Added CAT_SNF & SCAT_FCT spec handling	*
 * E. Safford/GSC	01/01	round msc source times to last 10 min   *
 * E. Safford/GSC	05/01	reset times to 0 if bad ier from *gtim()*
 * J. Wu/SAIC		07/03	get times based on sources' range/intv	*
 * T. Lee/SAIC		08/03	add time interval to n**_gtim		*
 * T. Piper/SAIC	02/04	removed unused variable two_days	*
 * T. Lee/SAIC		02/04	added reference time			*
 * T. Lee/SAIC		04/04	added delta reference time		*
 * T. Lee/SAIC		09/04	added bin hours to calling sequences	*
 * m.gamazaychikov/SAIC 12/04   add ionoff flag to ctb_dtget CS         *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS          *
 * T. Piper/SAIC	04/07	Modified for nim_gtim CSC		*
 * T. Piper/SAIC	10/07	Explicitly initialize **timarr		*
 * M. Li/SAIC		02/08	Added CAT_ENS				*
 * F. J. Yen/NCEP	 4/08	Add bin mins and mstrct to ctb_dtget CSC*
 ***********************************************************************/
{
    int		ier, ii, times, start, tm_arry[5], tmcnt, t_times;
    char	*pstr, tmp[12], src_str[LLPATH], **timarr=NULL;
    Boolean	bflag, bauto;
    dattm_t	mtmarry[MXNMFL], m_tm;
    dttms_t	tmarry[MXNMFL], base_t;
    DTinfo	dti;
    int		range, intv, lp;
/*---------------------------------------------------------------------*/

    _icatg      = source->catg;	

/*
 *  Get the subcat code and default # of frames for this source
 */
    if (_icatg == CAT_VGF) {
	strcpy (tmp, "VGF");
    }
    else if ( _icatg == CAT_ENS ) {
  	dslw_getFrstMod ( source->path, tmp );
    }
    else {
        strcpy (src_str, source->path);
        pstr = strtok(src_str, "/");
        pstr = strtok(NULL, "/");
	strcpy (tmp, pstr);
    }

    cst_lcuc (tmp, dti.alias, &ier);
    ctb_dtget (dti.alias, dti.path, dti.template, &(dti.catgry), 
	       &(dti.subcat), &(dti.nframe), &(dti.range),
	       &(dti.intrvl), &(dti.ionoff), &(dti.hrsbfr),
	       &(dti.mnsbfr), &(dti.hraftr), &(dti.mnaftr),
	       &(dti.mstrct), &(dti.domtmmtch),  &ier);

/*
 * Get auto-update and Reference time flags for the loop.
 */
    lp = loop_getCurLoop();
    bauto = auto_getAutoUpdt (lp);
    bflag = dataw_useRefTm (lp);

/*
 *  Get the available times for the source
 */
    switch (_icatg) {

      case CAT_IMG:
	dataw_getBaseTm ( lp, base_t );
	nim_gtim(source->attridx, time, source->range, source->interval, 
		    bflag, bauto, base_t, &t_times, &timarr, &ier);
	source->delta_rt = -1;
	if (ier < G_NORMAL || t_times <= 0 ) {
	    times = 0;
	}
	else {
	    times = t_times;
	    strcpy ( _lastim[lp], timarr[times-1] );
	}

/*
 * the last frame is the closest to time, the rest are earlier
 */
	if (times > MAX_FRAME) {
	    start = times - MAX_FRAME;
	    times = MAX_FRAME;
	}
	else {
	    start = 0;
	}

	for (ii = start, tmcnt = 0; tmcnt < times; ii++, tmcnt++) {
	    strcpy (new_times[tmcnt], timarr[ii]);
	    G_FREE(timarr[ii], char);
	}
	for ( ii = 0; ii < t_times; ii++) {
	    G_FREE(timarr[ii], char);
	}
	if ( timarr != NULL ) G_FREE(timarr, char*);
	break;
	
      case CAT_SFC:
      case CAT_SFF:
      case CAT_SND:
      case CAT_SNF:  
	switch (dti.subcat) {
	  case SCAT_SND:
	    nsn_gtim(source->attridx, time, source->range, source->interval,
			bflag, &(source->delta_rt), &times, tmarry, &ier);
	    break;

	  case SCAT_SNF:
	    nsn_gtim(source->attridx, time, source->range, source->interval, 
			bflag, &(source->delta_rt), &times, tmarry, &ier);
	    break;

	  case SCAT_SFF:
	    nsf_gtim(source->attridx, time, source->range, source->interval, 
			bflag, &(source->delta_rt), &times, tmarry, &ier);
	    break;

	  default:
	    nsf_gtim(source->attridx, time, source->range, source->interval, 
			bflag, &(source->delta_rt), &times, tmarry, &ier);
	    break;
	}

	if (ier < G_NORMAL) {
	    times = 0;
	}

	start = 0;
	if (times > MAX_FRAME) {

/*
 * the first frame is the closest to time, the rest are later
 */
	    if (_icatg == CAT_SFF || _icatg == CAT_SNF) {
		times = MAX_FRAME;
	    }

/*
 * the last frame is the closest to time, the rest are earlier
 */
	    else {
		start = times - MAX_FRAME;
		times = MAX_FRAME;
	    }
	}

	for (ii = start, tmcnt = 0; tmcnt < times; ii++, tmcnt++) {
	    strcpy (new_times[tmcnt], tmarry[ii]);
	}
	break;

      case CAT_ENS:
      case CAT_GRD:
	ngd_gtim(source->attridx, time, source->range, source->interval, 
		    bflag, &(source->delta_rt), &times, mtmarry, &ier);
	if (ier < G_NORMAL) {
	    times = 0;
	}

	start = 0;
	if (times > MAX_FRAME) {

/*
 * the first frame is the closest to time, the rest are later
 */
	    if (dti.subcat == SCAT_FCT) {
		times = MAX_FRAME;
	    }

/*
 * the last frame is the closest to time, the rest are earlier
 */
	    else {
		start = times - MAX_FRAME;
		times = MAX_FRAME;
	    }
	}


	for (ii = start, tmcnt = 0; tmcnt < times; ii++, tmcnt++) {
/*
 *  Strip the VXXXX from the end of the time string.
 */
	    pstr = strchr(mtmarry[ii], 'V');

	    if (pstr != NULL) {
		*pstr = '\0';
	    }

	    strcpy (new_times[tmcnt], mtmarry[ii]);
	}
	break;

      case CAT_MSC:
	range = source->range;
	intv = source->interval;
	
/*
 *  checks for invalid interval values.
 */
	if ( intv <= 0 ) {	
	    times = 10;
	    intv = range/times;
	}    
	
	if ( intv == 0 ) intv = 1;
	
	times = range/intv;
	
	source->interval = intv;
	
	if (times > MAX_FRAME) times = MAX_FRAME;

	strcpy (m_tm, time);
	
/*
 *  round the initial time to the last 10 min interval
 */
	ti_ctoi (m_tm, tm_arry, &ier, strlen (m_tm) );	
	tm_arry[4] = ( (int)(tm_arry[4]/10) ) * 10;
	ti_itoc (tm_arry, m_tm, &ier, strlen (m_tm) );


	for (tmcnt = times - 1; tmcnt >= 0; tmcnt--) {
	    strcpy (new_times[tmcnt], m_tm);

/*
 *  subtract the interval from the time
 */
	    ti_ctoi (m_tm, tm_arry, &ier, strlen (m_tm) );	
	    ti_subm (tm_arry, &intv, tm_arry, &ier);
	    ti_itoc (tm_arry, m_tm, &ier, strlen (m_tm) );
	}
	break;

      case CAT_VGF:
	strcpy (new_times[0], time);
	times = 1;
	break;
    }

    if (times < MAX_FRAME) {
	new_times[times][0] = '\0';
    }
    *ntimes = times;
    return (dti.nframe);
}

/*=====================================================================*/
/* ARGSUSED */
void tmln_exposeCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * tmln_exposeCb							*
 *									*
 * This callback function directs expose callbacks to tmln_redraw	*
 *									*
 * void tmln_exposeCb (wid, clnt, event)				*
 *									*
 * Input parameters:							*
 *	wid		Widget	drawing area window ID			*
 *	clnt		XtPointer	clint data			*
 *	cbs		XtPointer	callback structure		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NULL						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial work				*
 ***********************************************************************/
{
    tmln_redraw(0);
}

/*=====================================================================*/
/* ARGSUSED */
void tmln_selectCb ( Widget wid, XtPointer clnt, XEvent *event )
/************************************************************************
 * tmln_selectCb							*
 *									*
 * This function controls the selection process by interpreting the	*
 * button callbacks							*
 *									*
 * void tmln_selectCb (wid, clnt, event)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	clint data			*
 *	*event		XEvent		X event structure		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial coding				*
 * S. Law/GSC		09/98	Various cleanup				*
 * E. Safford/GSC	11/99	dataw_updtSelTimes on any btn release   *
 * S. Law/GSC		06/00	removed parameter from dataw_updSelTimes*
 * T. Lee/GSC		02/01	added check for single time flag	*
 * T. Lee/GSC		03/01	changed call seq. for loop_getTmMode	*
 * E. Safford/GSC	06/01	fix crash on release w/ no times avail  *
 * E. Safford/GSC	07/01	really fix crash                        *
 ***********************************************************************/
{
    static int		startx, endx, outx;
    int			ii, dx, current, cur_lp;
    Boolean		tm_flag;
    static Boolean	active, motion, boundary, outside;
/*---------------------------------------------------------------------*/

    if (!_timeInfoFlag) return;

/*
 *  Get single time flag for the current loop.
 */
    cur_lp =  loop_getCurLoop ();
    tm_flag = loop_getTmMode (cur_lp);

    if (event->type == ButtonPress ) {
	if (active) tmln_redraw (0);

/*
 * Check if outside vertical boudaries
 */
	if (event->xbutton.y < (TL_YTOP - FONT_YOFF) ||
	    event->xbutton.y > (TL_YBOT + FONT_YOFF)) {
	    active = FALSE;
	    outside = FALSE;
	    boundary = FALSE;
	}

/*
 * Check if outside horizontal boudaries
 */
	else if (event->xbutton.x < (_boxStart - (_boxOffset * 2)) ||
		 (_boxEnd + (_boxOffset * 2)) < event->xbutton.x) {
	    motion = FALSE;
	    active = FALSE;
	    outside = TRUE;
	    outx = event->xbutton.x;
	    boundary = FALSE;
	}
	else {
	    motion = FALSE;
	    active = TRUE;
	    outside = FALSE;

	    if (event->xbutton.x <= _boxStart ||
		event->xbutton.x <= BOX_MIN) {
		startx = endx = _boxStart;
		boundary = TRUE;
	    }
	    else if (_boxEnd <= event->xbutton.x ||
		     BOX_MAX <= event->xbutton.x) {
		startx = endx = _boxEnd;
		boundary = TRUE;
	    }
	    else {
		startx = endx = event->xbutton.x;
		boundary = FALSE;
	    }
	}

	if (active && boundary ) {
	    tmln_setupGc (0);
	    XDrawLine (XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc,
		       startx, BOX_YTOP, startx, (BOX_YTOP + BOX_HT));
	}

    }
    else if (event->type == ButtonRelease) {
    	if ( outside && (_nAvailable > 0) ) {
	    if (abs (event->xbutton.x - outx) < MOTION_CHECK) {
		active = TRUE;
	    }
	}

	if (active) {
	
	    if (motion && boundary) {
		current = (_boxStart < endx && endx < _boxEnd) ? 0 : 1;
		tmln_selectRange (startx, endx, current, TRUE);

		if (startx == _boxStart) {
		    _boxStart = endx;
		}
		if (startx == _boxEnd) {
		    _boxEnd = endx;
		}
	    }

	    if (!motion) {
		endx = event->xbutton.x;
		dx = abs (endx - _hrMin[0].x);
		current = 0;
		for (ii = 1; ii < _nAvailable; ii++) {
		    if (dx > abs (endx - _hrMin[ii].x)) {
			dx = abs (endx - _hrMin[ii].x);
			current = ii;
		    }
		}

		if (_selectTimes[current] == FALSE || _totalFrames > 1) {
		    _selectTimes[current] = (Boolean)((_selectTimes[current]) ? 
			FALSE : TRUE);
		    if ( tm_flag ) {
			for (ii = 0; ii < _nAvailable; ii++) {
		            _selectTimes[ii] = FALSE;
			}
		        _selectTimes[current] = TRUE;
		    }
		}

		if (outside || boundary) {
		    if (endx < _boxStart) {
			_boxStart = _hrMin[current].x - _boxOffset;
		    }
		    if (endx > _boxEnd) {
			_boxEnd   = _hrMin[current].x + _boxOffset;
		    }
		}
	    }

	    current = 0;
	    for (ii = 0; ii < _nAvailable; ii++) {
		if (_selectTimes[ii]) current++;
	    }

	    if (current <= 0) {
		dx = WIN_WIDTH;
		ii = -1;
		for (ii = 0; ii < _nAvailable; ii++) {
		    if (dx > abs (endx - _hrMin[ii].x)) {
			dx = abs (endx - _hrMin[ii].x);
			current = ii;
		    }
		}

		_selectTimes[current] = TRUE;
		current = 1;

		_boxStart = _boxEnd = 0;
	    }

	    if (current != _totalFrames) {
		_totalFrames = current;
	    }

  	    dataw_updSelTimes (); 
	    tmln_redraw (0);
	    active = FALSE;
	}
    }
    else if ( !tm_flag )  {	/* button motion */
	if (active && abs (event->xmotion.x - startx) > MOTION_CHECK) {
	    motion = TRUE;
	    if (boundary) {
		tmln_selectRange (startx, endx, -1, FALSE);

		endx = event->xmotion.x;

		if (startx == _boxStart && endx >= _boxEnd) {
		    startx = _boxEnd;
		}
		if (startx == _boxEnd && endx <= _boxStart) {
		    startx = _boxStart;
		}

		current = (_boxStart < endx && endx < _boxEnd) ? 0 : 1;
		tmln_selectRange (startx, endx, current, FALSE);
	    }
	    else {
		tmln_selectRange (_boxStart, _boxEnd, 0, TRUE);

		current = event->xmotion.x - startx;
		tmln_selectRange (_boxStart + current, _boxEnd + current,
				  1, TRUE);

		tmln_redraw (current);

		startx += current;
	    }
	}
    }
}

/*=====================================================================*/

void tmln_selectRange ( int startx, int endx, int state, Boolean set_flag )
/************************************************************************
 * tmln_selectRange							*
 *									*
 * This function marks a range of times					*
 *									*
 * void tmln_selectRange (startx, endx, state, set_flag)		*
 *									*
 * Input parameters:							*
 *	startx		int	first  x coordinate			*
 *	endx		int	second x coordinate			*
 *	state		int	-1 = revert, 0 = unselect, 1 = select	*
 *	set_flag	Boolean	set values in _selectTimes		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial coding				*
 * S. Law/GSC		09/98	Cleaned up skipping			*
 ***********************************************************************/
{
    int		x1, x2, ii, jj, kk, *pint, skip;
    Boolean	select;
/*---------------------------------------------------------------------*/

    if (endx > startx) {
	x1 = startx;
	x2 = endx;
    }
    else {
	x1 = endx;
	x2 = startx;
    }

/*
 * Dragging _boxEnd
 */
    if (startx == _boxEnd) {
	pint = &ii;
    }

/*
 * Dragging _boxStart
 */
    else if (endx == _boxStart) {
	pint = &jj;
    }

/* 
 * Moving box, skip starts from front
 */
    else if (_directionFlag) {
	pint = &ii;
    }

/* 
 * Moving box, skip starts from back
 */
    else {
	pint = &jj;
    }

    if (!set_flag) {
	tmln_drawBox (x1, x2);
    }

    skip = _skipFactor;
    select = FALSE;
    kk = 2;
    for (ii = 0, jj = (_nAvailable - 1); ii < _nAvailable; ii++, jj--) {
	if (x1 <= _hrMin[*pint].x && _hrMin[*pint].x <= x2) {
	    if (state == -1) {
		kk = (_selectTimes[*pint]) ? 1 : 2;
	    }
	    else if (state == 1) {
		if (skip >= _skipFactor) {
		    skip = 0;
		    kk = 1;
		    select = TRUE;
		}
		else {
		    skip++;
		    kk = 2;
		    select = FALSE;
		}
	    }

	    if (state != -1 && set_flag) {
		_selectTimes[*pint] = select;
	    }

	    tmln_setupGc (kk);
	    XFillRectangle (XtDisplay(_tmlnDrawA), _tmlnWin, _drawGc,
			    _hrMin[*pint].x - MARK_XOFF, MARK_YTOP, 
			    MARK_WD, MARK_HT);
	}
	else {
	    if (_selectTimes[*pint]) {
		skip = 0;
	    }
	    else {
		skip++;
	    }
	}
    }
}

/*=====================================================================*/

void tmln_setupGc ( int flag )
/************************************************************************
 * tmln_setupGc								*
 *									*
 * This function initially sets up each GC.  It also sets the		*
 * foreground color in _drawGc						*
 *									*
 * void tmln_setupGc (flag)						*
 *									*
 * Input parameters:							*
 *	flag		int	-1 = intial setup			*
 *				 0 = set foreground color to black	*
 *				 1 = set foreground color to red	*
 *				 2 = set foreground color to blue	*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		08/98	Initial coding				*
 * T. Piper/SAIC	01/02	Freed font when necessary		*
 * T. Piper/SAIC	05/03	replaced XAllocNamedColor w/xsncolr	*
 ***********************************************************************/
{
    XGCValues		gcval;
    char		fontname[256];
    Pixel		bg, fg;
    static Pixel	blackpix, redpix, bluepix;
    static Display	*dpy;
    int			ier;
/*---------------------------------------------------------------------*/
    if (flag == -1) {
	_tmlnWin = XtWindow (_tmlnDrawA);
	dpy = XtDisplay (_tmlnDrawA);

	xsncolr("red",  &redpix,  &ier);
	xsncolr("blue", &bluepix, &ier);
	xsncolr("black", &blackpix, &ier);
	gcval.foreground = blackpix;
	_drawGc = XCreateGC (dpy, _tmlnWin, GCForeground, &gcval);

	XtVaGetValues (_tmlnDrawA, XmNbackground, &bg, NULL );
	gcval.line_width = 2;
	gcval.foreground = blackpix^bg;
	_boxGc = XCreateGC (dpy, _tmlnWin, 
			    GCLineWidth|GCForeground, &gcval);
	XSetFunction (dpy, _boxGc, GXxor);

	strcpy(fontname,"-adobe-times-bold-i-normal--14-140-*");
	if ( font ) XFreeFont ( dpy, font );
	font = XLoadQueryFont( dpy, fontname );

	if (font) { 
	    XSetFont (dpy, _drawGc, font->fid);
	}
    }
    else {
	fg = (flag == 0) ? blackpix : (flag == 1) ? redpix : bluepix;
	XSetForeground (dpy, _drawGc, fg);
    }
}

/*=====================================================================*/

void tmln_drawBox ( int firstx, int secondx )
/************************************************************************
 * tmln_drawBox								*
 *									*
 * This function draws the selection box.				*
 *									*
 * void tmln_drawBox ( firstx, secondx )				*
 *									*
 * Input parameters:							*
 *	firstx		int	first  x coordinate of box		*
 *	secondx		int	second x coordinate of box		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/98	Initial coding				*
 * T. Lee/GSC		02/01	No box drawn for single time loop	*
 * T. Lee/GSC		03/01	changed call seq. of loop_getTmMode	*
 * T. Piper/SAIC        05/03   removed xwcmn.h and added XtDisplay()   *
 ***********************************************************************/
{
    int		startx, endx, cur_lp;
    unsigned int	dx;
/*---------------------------------------------------------------------*/

    cur_lp = loop_getCurLoop ();

/* 
 *  No box for single time loop.
 */ 
    if (loop_getTmMode (cur_lp)) return;
 
/*
 *  Draw the box.
 */
    if (secondx > firstx) {
	startx = firstx;
	endx   = secondx;
    }
    else {
	startx = secondx;
	endx   = firstx;
    }

    if (startx < BOX_MIN) startx = BOX_MIN;
    if (startx > BOX_MAX) startx = BOX_MAX;
    if (endx   < BOX_MIN) endx   = BOX_MIN;
    if (endx   > BOX_MAX) endx   = BOX_MAX;

    dx = (unsigned int)(endx - startx);

    XDrawRectangle (XtDisplay(_tmlnDrawA), _tmlnWin, _boxGc,
		    startx, BOX_YTOP, dx, BOX_HT);
}

/*=====================================================================*/

void tmln_getLastTm ( int lp, dttms_t ltime )
/************************************************************************
 * tmln_getLastTm							*
 *									*
 * This function get time for the last tick mark.                       *
 *									*
 * void tmln_getLastTm ( lp, ltime, )					*
 *									*
 * Input parameters:                                                    *
 *	lp		int	loop #					*
 *									*
 * Output parameters:                                                   *
 *	ltime		char	last time				*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		01/04	Initial coding				*
 ***********************************************************************/
{
    if ( lp > MAX_LOOP || lp < 0 )  {
	ltime = NULL;
    }
    else {
	strcpy ( ltime, _lastim [lp] );   
    }
}
