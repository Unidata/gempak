#include "xwgui.h"

#define MIN_DWELL 100
#define DIR_BACK   -1
#define DIR_FORW    1

XtAppContext _xmApp;

static int  	_xmloopPxms[MAX_LOOP][MAX_PIXMAP];    /* pxm indecies */

static int   	_pxIdx[MAX_LOOP]; 	/* current pixmap index */
static int   	_currentCommand; 	/* current command */
static int   	*_dwPtr; 		/* pointer to the dwell rates */
					/*  (in milliseconds) */

void (*_loopFunc)(int, int);    	/* label displaying function */
XtIntervalId _xmloopTimeoutId;

static	Boolean	_xmloopDsply;
static  int	_loopfrbkInc = DIR_FORW;


/************************************************************************
 * xmloop.c                                                             *
 *                                                                      *
 * This module takes care the animation for MOTIF window.               *
 *                                                                      *
 * CONTENTS:                                                            *
 *      xmloopSetDwellPtr()     creates the main window.                *
 *      xmloopSet()             sets loop parameters.                   *
 *	xmloop_loopSet()	sets up the animation for a loop	*
 *      xmloop_switchLoop()	switches between loops			*
 *      xmloop_loop()           executes the loop command.              *
 *									*
 *	xmloop_getPxmInfo()	gets pixmap information for loop	*
 *	xmloop_atBlankPxm()	TRUE if the blank pxm is displayed	*
 *									*
 *      xmloop_timeoutProc()	internal timeout procedure		*
 *	xmloop_getGoodPxm()	get the first valid pixmap		*
 *									*
 **									*
 * Log:									*
 * T. Piper/GSC		3/01	Removed xwprm.h, in xwcmn.h		*
 * H. Zeng/EAI         04/01    Replaced _lpIdx with _lp                *
 ***********************************************************************/

/*=====================================================================*/

void xmloopSetDwellPtr ( int *dwellptr )
/************************************************************************
 * xmloopSetDwellPtr                                                    *
 *                                                                      *
 * This subroutine sets the pointer to the dwell rates.                 *
 *                                                                      *
 * void xmloopSetDwellPtr ( dwellptr )         				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *dwellptr       int             pointer to dwell rate (ms)      *
 *                                                                      *
 * Output parameters:                                                   *
 *			NONE						*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            5/96                                  		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

	_dwPtr = dwellptr;

}

/*=====================================================================*/

void xmloop_loopSet ( Widget wid, int loop, Boolean loopcur, int curpxm, 
			int npxm, void (*func)(int,int), int *iret )
/************************************************************************
 * xmloop_loopSet                                                       *
 *                                                                      *
 * This subroutine sets animation sequence info for MOTIF windows.  It  *
 * is similar to xmloopSet, but it also includes a loop variable (range *
 * 0-3) for use with nmap2. 						*
 *                                                                      *
 * void xmloop_loopSet ( wid, loop, loopcur, curpxm, npxm, func, iret)	*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid         Widget         parent widget                    	*
 *      loop	    int            loop which is to be animated		*
 *	loopcur	    Boolean	   make this loop the active loop	*
 *      curpxm      int            current pixmap      			*
 *      npxm        int            # of the pixmaps in the loop not	*
 *					counting blank		    	*
 *      *func()     void           additional function if any      	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*iret	    int		0 = normal, -1 = bad pixmap value	*
 *					    -2 = bad loop value		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	09/99	initial coding - copied xmloopSet    	*
 * S. Law/GSC		12/99	added clearing of pixmaps if npxm == 0	*
 * S. Law/GSC		01/00	added clearing of master pixmaps	*
 * S. Law/GSC		01/00	changed curpxm and _pxIdx to arrays	*
 * E. Safford/GSC	04/00	load default maps in all loops (nmap2)  *
 * E. Safford/GSC	04/00	fix bug in current pixmap setting       *
 * S. Law/GSC		06/00	mstr array only for current loop	*
 * H. Zeng/EAI          04/01   moved blank map before the 1st pixmap   *
 * E. Safford/SAIC	08/01	remove pxmarry[] param			*
 * E. Safford/SAIC	09/01   use loopcur on check for Free of mstr   *
 ***********************************************************************/
{
    int		ii, dum1, dum2, dum3, dum4, ier;
    Window_str	*cwin;
/*---------------------------------------------------------------------*/

	
    *iret = 0;

    if (loop < 0 || loop > MAX_LOOP) {
        *iret = -1;
	return;
    }

    if (!_xmApp) {
        _xmApp = XtWidgetToApplicationContext(wid);
    }

    cwin = &(gemwindow[current_window]);

    if (npxm == 0) {

	for (ii = 0; ii < MAX_PIXMAP; ii++) {

	    if (cwin->pxms[loop][ii] != (Pixmap) NULL) {
		XFreePixmap (gemdisplay, cwin->pxms[loop][ii]);
		cwin->pxms[loop][ii] = (Pixmap) NULL;

		if (loopcur &&
		    cwin->mstr[ii] != (Pixmap) NULL) {
		    XFreePixmap (gemdisplay, cwin->mstr[ii]);
		    cwin->mstr[ii] = (Pixmap) NULL;
		}

		XSync (gemdisplay, False);

		cwin->npxms--;
	    }
	}

	cwin->curpxm[loop] = 0;
	xclear (&dum1, &dum2, &dum3, &dum4, &ier);

	_loopSet[loop]  = TRUE;
	_fstPxm[loop]   = 0; 
	_lstPxm[loop]   = 0;
	_blankPxm[loop] = 0;
    }
    else {

	_fstPxm[loop] = 1;   
	_lstPxm[loop] = npxm;

	for ( ii = 0; ii <= npxm; ii++)  {
	    _xmloopPxms[loop][ii] = ii;
	}
	
	_blankPxm[loop] = 0;
        _loopSet[loop]  = TRUE;

    }

    _numPxm[loop] = npxm;
    _xmloopDsply = FALSE;
    
    if ( curpxm <= npxm && curpxm > 0 ) {
        _pxIdx[loop] = curpxm;
    }
    else {
        _pxIdx[loop] = 0;
    }
        
    if (loopcur) {
        _lp = loop;
	_xmloopDsply = TRUE;

	if (func) { 
	    _loopFunc = func;
	}
    }

}

/*=====================================================================*/

void xmloop_switchLoop ( int loop, Boolean update )
/************************************************************************
 * xmloop_switchLoop							*
 *									*
 * This subroutine changes the current loop (_lp in xwgui.h and         *
 * curr_loop in the appropriate gemwindow structure) value.  If update  *
 * is TRUE, the displayed animation sequence is changed to the new      *
 * value of loop and the _viewedLoop value is updated.  If FALSE is     *
 * supplied the current loop value is updated but the _viewedLoop       *
 * (xwgui.h) and animation sequence are not changed.  This reflects an  *
 * internal loop change that is not made visible to the user.  This is  *
 * most commonly done when loading loops while the user is looking at a *
 * different loop.							*
 *									*
 * void xmloop_switchLoop (loop, update)				*
 *									*
 * Input parameters:							*
 *	loop		int	loop to change to			*
 *	update		Boolean	update flag				*
 *									*
 * Output parameters:							*
 *	None								*
 **									*
 * Log:									*
 * E. Safford/GSC	09/99	initial coding				*
 * S. Law/GSC		10/99	added update flag			*
 * E. Safford/GSC	01/00	make update flag a boolean, fix header  *
 * S. Law/GSC		01/00	changed animation defines		*
 * S. Law/GSC		01/00	ANIM_LAST_FRAM -> ANIM_CURR_FRAM	*
 * E. Safford/GSC	04/00	add check on _loopSet                   *
 * E. Safford/SAIC	01/02	add _viewedLoop assignment		*
 * T. Piper/SAIC	07/03	renamed and added parameter to xmloop	*
 ***********************************************************************/
{
    if (loop < 0 || loop > MAX_LOOP-1) {
	return;
    }

    _lp = gemwindow[current_window].curr_loop = loop;

    if (update) {
	_viewedLoop = loop;
    }

    if (_loopSet[_lp]) {

        if (_currentCommand != ANIM_LOOP_FORW && 
	    _currentCommand != ANIM_LOOP_BACK &&
	    _currentCommand != ANIM_ROCK_LOOP) {
	    _currentCommand = ANIM_CURR_FRAM;
        }

        if (update) {
	    xmloop_loop(_currentCommand, 0);
        }
    }

}

/*=====================================================================*/

void xmloop_loop ( int command, Boolean loopstop )
/************************************************************************
 * xmloop_loop                                                          *
 *                                                                      *
 * This subroutine executes the animation command for MOTIF windows.    *
 *                                                                      *
 * void xmloop_loop ( command )                                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *      command		int		command               		*
 *	loopstop	Boolean		stop at current or end		*
 *					TRUE - stop at current		*
 *					FALSE - stop at end		*
 *                                                                      *
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI            5/96                                  		*
 * E. Safford/GSC	11/98	add hide/show frame loop                *
 * E. Safford/GSC	12/98	clean up loopFunc logic			*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL for LINUX		*
 * E. Safford/GSC	04/99	don't loop single data frames		*
 * E. Safford/GSC	09/99	update for new _xmloopPxms dimension	*
 * E. Safford/GSC	10/99	Fixed bug in _loopfunc call          	*
 * E. Safford/GSC	01/00	_xmloopSet -> _loopSet[]           	*
 * S. Law/GSC		01/00	changed animation defines		*
 * S. Law/GSC		01/00	xg2win -> xpxm2win,added ANIM_CURR_FRAM	*
 * S. Law/GSC		01/00	changed _pxIdx to an array		*
 * E. Safford/GSC	04/00	avoid looping on a single frame loop    *
 * E. Safford/GSC	05/00	use xmloop_getGoodPxm                   *
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 * H. Zeng/EAI          04/01   modified to use _fstPxm & _lstPxm       *
 * E. Safford/SAIC	09/01	rm check on total -- make looping halt  *
 *				  when only 1 frame in loop		*
 * J. Wu/SAIC		11/01	add param in cvg_load()	calling		*
 * J. Wu/SAIC		12/01	add _pgLayer in cvg_load() call		*
 * J. Wu/SAIC		12/01	replace cvg_load() with cvg_redraw()	*
 * H. Zeng/XTRIA	12/02	modified to set "total=1" when "total=0"*
 * T. Piper/SAIC	07/03	renamed xmloop -> xmloop_loop		*
 * T. Piper/SAIC	07/03	added loopstop parameter		*
 * T. Piper/SAIC	11/03	added check for last frame when loopstop*
 * T. Piper/SAIC	12/03	fixed check for last frame for backward	*
 ***********************************************************************/
{
    int		total, ipxm, ier, istat;
    static int	prev_frame, prev_index;
    char	refresh;
    Window_str	*cwin;
/*---------------------------------------------------------------------*/

    if ( _loopSet[_lp] != TRUE ) {
	return;
    }

    cwin = &(gemwindow[current_window]);
    total = _numPxm[_lp];

    /*
     * When there is no pixmap, set total=1 because of the blank pixmap.
     */
    if ( total == 0 ) {
	total = 1;
    }

    _currentCommand = command;


    if (_allFrmsBad[_lp]) {
	ipxm = _blankPxm[_lp];
    }
    else {
	switch( command ) {

	case ANIM_HIDE_DATA:
	    if (_xmloopDsply) {
	        prev_frame = _xmloopPxms[_lp][_pxIdx[_lp]];
                prev_index = _pxIdx[_lp];
	        ipxm = _blankPxm[_lp]; 
	        _pxIdx[_lp] = _blankPxm[_lp];
	        _xmloopDsply = FALSE;
	    }
	    else {
	        ipxm = prev_frame;
	        _pxIdx[_lp] = prev_index;
	        _xmloopDsply = TRUE;
	    }

	    break;

	case ANIM_STOP_LOOP:    /* stop */
	    if (_xmloopTimeoutId != (XtIntervalId)NULL) {
	        XtRemoveTimeOut(_xmloopTimeoutId);
	        _xmloopTimeoutId = (XtIntervalId)NULL;
	    }
	    if ( loopstop ) {
		_pxIdx[_lp] = xmloop_getGoodPxm (_lp,
 				cwin->curpxm[_lp], DIR_FORW);
	    }
	    else { 
		_pxIdx[_lp] = _lstPxm[_lp];
		_pxIdx[_lp] = xmloop_getGoodPxm (_lp,
					_pxIdx[_lp], DIR_BACK); 
	    }
	    ipxm = _xmloopPxms[_lp][_pxIdx[_lp]];
	    break;

	case ANIM_STEP_FORW:    /* forward step */
	    _pxIdx[_lp] = ( _pxIdx[_lp] +1 ) > _lstPxm[_lp] ? 
			_pxIdx[_lp]+1-total : _pxIdx[_lp]+1;
	    _pxIdx[_lp] = xmloop_getGoodPxm (_lp, 
					_pxIdx[_lp], DIR_FORW); 
	    ipxm = _xmloopPxms[_lp][_pxIdx[_lp]];
	    break;

	case ANIM_STEP_BACK:    /* backward step */
	    _pxIdx[_lp] =( _pxIdx[_lp] -1 ) < _fstPxm[_lp] ?
	                  _pxIdx[_lp]-1+total : _pxIdx[_lp]-1;
	    _pxIdx[_lp] = xmloop_getGoodPxm (_lp, 
	    				_pxIdx[_lp], DIR_BACK); 
	    ipxm = _xmloopPxms[_lp][_pxIdx[_lp]];
	    break;

	case ANIM_LOOP_FORW:    /* forward loop */
	case ANIM_LOOP_BACK:    /* reverse loop */
	case ANIM_ROCK_LOOP:    /* rock loop */

	    /*
	     * set starting pixmap index 
	     */
            if ( loopstop  && 
  		( (command == ANIM_LOOP_FORW  &&  cwin->curpxm[_lp] != _lstPxm[_lp])  ||
		  (command == ANIM_LOOP_BACK  &&  cwin->curpxm[_lp] != _fstPxm[_lp])  ||
		  (command == ANIM_ROCK_LOOP  &&  cwin->curpxm[_lp] != _lstPxm[_lp]) ) ) {
    		    _pxIdx[_lp] = cwin->curpxm[_lp];

	    }
	    else {
		if ( command == ANIM_LOOP_BACK ) {
		    _pxIdx[_lp] = _lstPxm[_lp];
		    _loopfrbkInc = DIR_BACK;
		} 
		else { 
		    _pxIdx[_lp] = _fstPxm[_lp];
		    _loopfrbkInc = DIR_FORW;
	        }
	    }	    

	    /*
	     *  Then find first good pxm.
	     */
	    _pxIdx[_lp] = xmloop_getGoodPxm (_lp, 
	    				_pxIdx[_lp], _loopfrbkInc); 


	    ipxm = _xmloopPxms[_lp][_pxIdx[_lp]];

	    /*
	     *  Don't loop if there is only one frame
	     */
	    if (total > 1) {
	        if ( _xmloopTimeoutId != (XtIntervalId)NULL ) {
		    XtRemoveTimeOut(_xmloopTimeoutId);
		    _xmloopTimeoutId = (XtIntervalId)NULL;
	        }

	        _xmloopTimeoutId = 
		    XtAppAddTimeOut(_xmApp, 1L, 
				(XtTimerCallbackProc)xmloop_timeoutProc, 
				(XtPointer)NULL);
	    }

	    break;

          case ANIM_FRST_FRAM: /* go to first frame */
	    _pxIdx[_lp] = _fstPxm[_lp];
	    _pxIdx[_lp] = xmloop_getGoodPxm (_lp, 
	    				_pxIdx[_lp], DIR_FORW); 
	    ipxm = _pxIdx[_lp];
	    break;

          case ANIM_LAST_FRAM: /* go to last frame */
	    _pxIdx[_lp] = _lstPxm[_lp]; 
	    _pxIdx[_lp] = xmloop_getGoodPxm (_lp, 
	    				_pxIdx[_lp], DIR_BACK); 
	    ipxm = _pxIdx[_lp];
	    break;

          case ANIM_CURR_FRAM: /* go to current frame */
	    _pxIdx[_lp] = xmloop_getGoodPxm (_lp, 
	    				_pxIdx[_lp], DIR_FORW); 
	    ipxm = _xmloopPxms[_lp][_pxIdx[_lp]];
	    break;

          default:    /* wrong command */
	    return;

        }   /* end switch */

    }       /* end else */


    xpxm2win (ipxm, current_window, &ier);

    refresh = cwin->xw_rfrsh[cwin->curr_loop][ipxm];
    cwin->xw_rfrsh[cwin->curr_loop][ipxm] = FALSE;

    if (_pgpalwIsUp && refresh) { 
	cvg_redraw(NULL, &istat); 
    }

    /*
     *  If a loop control button has broken out of hide, then update
     *  the _xmloopDsply flag to TRUE
     */ 
    if (command != ANIM_HIDE_DATA && !_xmloopDsply) {
        _xmloopDsply = TRUE; 
    }

    if (_loopFunc) {
	_loopFunc( _pxIdx[_lp], total);

    }

}

/*=====================================================================*/

void xmloop_getPxmInfo ( int loop, int *npxm, int *fpxm, int *lpxm, 
                                                int *bpxm, int *iret )
/************************************************************************
 * xmloop_getPxmInfo							*
 *									*
 * This subroutine gets the pixmap information for the given loop.	*
 *									*
 * void xmloop_getPxmInfo (loop, npxm, fpxm, lpxm, bpxm, iret)		*
 *									*
 * Input parameters:							*
 *	loop		int	which loop				*
 *									*
 * Output parameters:							*
 *	*npxm		int	number of pixmaps in loop		*
 *	*fpxm		int	index of first pixmap in loop		*
 *	*lpxm		int	index of last  pixmap in loop		*
 *	*bpxm		int	index of blank pixmap in loop		*
 *	*iret		int	return value (0 = OK, -1 = bad loop)	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		10/99	initial coding				*
 * E. Safford/GSC	05/00	add bpxm to output list			*
 ***********************************************************************/
{
    if (loop < 0 || MAX_LOOP <= loop) {
	*iret = -1;
	return;
    }

    *iret = 0;

    *npxm = _numPxm[loop];
    *fpxm = _fstPxm[loop];
    *lpxm = _lstPxm[loop];
    *bpxm = _blankPxm[loop];

}

/*=====================================================================*/

Boolean xmloop_atBlankPxm ( void )          
/************************************************************************
 * xmloop_atBlankPxm							*
 *									*
 * This subroutine returns true if the current pxm is the blank pxm for	*
 * the current loop.  							*
 *									*
 * Boolean xmloop_atBlankPxm ( )                               		*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	xmloop_atBlankPxm	Boolean		TRUE if the blank 	*
 *						pixmap is in view	*
 **									*
 * Log:									*
 * E. Safford/GSC	05/00	initial coding          		*
 ***********************************************************************/
{
    return ((Boolean)(!_xmloopDsply));
}

/*=====================================================================*/
/* ARGSUSED */
XtTimerCallbackProc xmloop_timeoutProc ( XtIntervalId  *id ) 
/************************************************************************
 * xmloop_timeoutProc                                                   *
 *                                                                      *
 * This function controls the animation.                                *
 *                                                                      *
 * XtTimerCallbackProc xmloop_timeoutProc ( id )                        *
 *                                                                      *
 * Input parameters:                                                    *
 *  id       XtIntervalId*  timer ID ( not used )                       *
 *                                                                      *
 * Output parameters:                                                   *
 *  xmloop_timeoutProc  XtTimerCallbackProc                             *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       	05/96                                           *
 * E. Safford/GSC	04/99	fix irix6 compile warning		*
 * E. Safford/GSC	09/99	update for new _xmloopPxms dimension	*
 * T. Piper/GSC  	10/99	fix rock problem			*
 * S. Law/GSC		01/00	changed animation defines		*
 * S. Law/GSC		01/00	xg2win -> xpxm2win			*
 * S. Law/GSC		01/00	changed _pxIdx to an array		*
 * E. Safford/GSC	04/00	mod _loopFunc call                      *
 * E. Safford/GSC	05/00	use xmloop_getGoodPxm                   *
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 * H. Zeng/EAI          04/01   moved blank map before the 1st pixmap   *
 * E. Safford/GSC	07/01	add initial wait value			*
 * J. Wu/SAIC		11/01	add param in cvg_load()	calling		*
 * J. Wu/SAIC		12/01	add _pgLayer in cvg_load() call		*
 * J. Wu/SAIC		12/01	replace cvg_load() with cvg_redraw()	*
 * H. Zeng/EAI          10/02   add first&last frame pause for rocking  *
 ***********************************************************************/
{
    unsigned long	wait = 100;
    int			ipxm, iret, total, first, last, istat, frms;
    char		refresh;
    Window_str		*cwin;
/*---------------------------------------------------------------------*/

    total   = _numPxm[_lp];
    ipxm    = _xmloopPxms[_lp][_pxIdx[_lp]];
    first   =  xmloop_getGoodPxm (_lp, _fstPxm[_lp], DIR_FORW); 
    last    =  xmloop_getGoodPxm (_lp, _lstPxm[_lp], DIR_BACK); 

    cwin    = &(gemwindow[current_window]);

    if (_loopFunc) {
	frms = (total == 0)?  1 : total;
	_loopFunc( _pxIdx[_lp], frms);
    }

    xpxm2win (ipxm, current_window, &iret);

    refresh = cwin->xw_rfrsh[cwin->curr_loop][ipxm];
    cwin->xw_rfrsh[cwin->curr_loop][ipxm] = FALSE;

    if (_pgpalwIsUp && refresh) { 
	cvg_redraw(NULL, &istat); 
    }


    switch ( _currentCommand ) {

      case ANIM_LOOP_FORW:

	/*
	 *  Set dwell based on current pixmap
	 */
	if( ipxm == first )  {
	    wait = (unsigned long)_dwPtr[0];
	}
	else if( ipxm == last )  {
	    wait = (unsigned long)_dwPtr[2];
	}
	else {
	    wait = (unsigned long)_dwPtr[1];
	}	  

	_loopfrbkInc = DIR_FORW;

	break;

      case ANIM_LOOP_BACK:

	/*
	 *  Set dwell based on current pixmap
	 */
	if( ipxm == first )  {
	    wait = (unsigned long)_dwPtr[2];
	}
	else if( ipxm == last )  {
	    wait = (unsigned long)_dwPtr[0];
	}
	else {
	    wait = (unsigned long)_dwPtr[1];
	}	

	_loopfrbkInc = DIR_BACK;

	break;

      case ANIM_ROCK_LOOP:

	if( ipxm == first || ipxm == last )  {
	    wait = (unsigned long)_dwPtr[2];
	}
	else {
	    wait = (unsigned long)_dwPtr[1];
	}

	if( ipxm == first )  {
	    _loopfrbkInc =  DIR_FORW;
	}
	else if( ipxm == last )  {
	    _loopfrbkInc =  DIR_BACK;
	}

	break;
    }

    if (total > 0) { 

	/*
	 *  move 1 pixmap and then find the first good pixmap
	 */
        if( _loopfrbkInc == DIR_FORW ) {
	    _pxIdx[_lp] =( _pxIdx[_lp] +1 ) > _lstPxm[_lp] ?
	                  _pxIdx[_lp]+1-total : _pxIdx[_lp]+1;
        }
        else {
	    _pxIdx[_lp] =( _pxIdx[_lp] -1 ) < _fstPxm[_lp] ?
	                  _pxIdx[_lp]-1+total : _pxIdx[_lp]-1;
        }

        _pxIdx[_lp] =  
		xmloop_getGoodPxm (_lp, _pxIdx[_lp], _loopfrbkInc); 

    }

    _xmloopTimeoutId = XtAppAddTimeOut(_xmApp, wait, 
			(XtTimerCallbackProc)xmloop_timeoutProc, 
			(XtPointer)NULL);

    return (XtTimerCallbackProc) NULL;
}

/*=====================================================================*/

int xmloop_getGoodPxm ( int lp, int start, int dir )
/************************************************************************
 * xmloop_getGoodPxm                                                    *
 *                                                                      *
 * This function returns the index of the next pixmap that has NOT been *
 * identified as bad.  It will look first at the start index, and then  *
 * move according to dir.  Dir should be either 1 or -1.  As soon as a  *
 * pixmap is located that is not bad, that index number is returned.	*
 *                                                                      *
 * int xmloop_getGoodPxm ( lp, start, dir )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *  	lp		int		loop number			*
 *	start		int		starting pixmap	index		*
 *	dir		int		DIR_FORW or DIR_BACK		*
 *                                                                      *
 * Output parameters:                                                   *
 *	xmloop_getGoodPxm	int	pixmap index number or -1 on 	*
 *								error	*
 **                                                                     *
 * E. Safford/GSC	05/00	initial coding                          *
 * H. Zeng/EAI          04/01   moved blank map before the 1st pixmap   *
 * E. Safford/SAIC	12/03	use #defines for dir check		*
 ***********************************************************************/
{
int		pxm;
Window_str	*cwin;
/*---------------------------------------------------------------------*/

    if ( (dir != DIR_FORW && dir != DIR_BACK)   || 
         (lp < 0 || lp > MAX_LOOP) || (_allFrmsBad[lp]) ) {

	return (-1);
    }

    cwin = &(gemwindow[current_window]);

    pxm = start;

    while ( cwin->bad_frm[lp][pxm] ) {
        if( dir == DIR_FORW ) {
	    pxm =( pxm +1 ) > _lstPxm[lp] ?
	                  pxm+1-_numPxm[lp] : pxm+1;
        }
        else {
	    pxm =( pxm -1 ) < _fstPxm[lp] ?
	                  pxm-1+_numPxm[lp] : pxm-1;
        }
    }

    return (pxm);

}
