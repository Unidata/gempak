#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "proto_xw.h"

static  GC      _drawGc;                /* drawing graphics Context (GC *) */
static	GC	_drawXORGc;		/* drawing xor graphics context */
static	Widget	_draw_area;

static	Boolean	_textFlag = FALSE;
static	Boolean	_circleFlag = FALSE;
static	Boolean	_pointObj = FALSE;
static	int	_textLen  = 0;
static	float	_txtRotn;
static	float	_textX[5];
static	float	_textY[5];
static	float	_circleX[2];
static	float	_circleY[2];
static	float	_oldPtX[2] = {-99.0F, -99.0F};
static	float	_oldPtY[2] = {-99.0F, -99.0F};

static  int	_ghostPts, _drawPt, _nPts=LLMXPT;
static	float   *_ghostX = (float *)NULL,  *_ghostY = (float *)NULL;
static  Boolean _closedFig, _chngdGhost;
static	Boolean	_ghostShow = FALSE;	/* whether user function has turned
					   ghost on */
static	Boolean	_ghostVeil = TRUE;	/* whether to veil the ghost when
					   mouse leaves canvas */
static	Boolean	_ghostEnabled = FALSE;	/* whether the ghost is currently
					   veiled */
static	Boolean	_ghostInhibit = FALSE;	/* whether to prevent user function
					   from removing a ghost that has
					   already been removed by
					   veiling */	 
static	Boolean	_ghostArrowFlg = FALSE;	/* whether to draw a ghost arrowhead 
					   at cursor point for closed or open
					   ghost lines of CLASS_LINES */	 
static  char    _smoothLvl;
static  int    _smooth_pts;
static	float	*_smooth_x = (float *)NULL, *_smooth_y = (float *)NULL;


/*
 *  Private functions
 */
static void pggst_drawTemp ( void );
static void pggst_getSmthPts ( int max_pts, int *pts, 
			float smth_x[], float smth_y[] );
static void pggst_ghostLine ( int np, float rx[], float ry[] );


/************************************************************************
 * nmap_pggst.c								*
 *									*
 * This module contains the functions for drawing ghost lines in	*
 * product generation.							*
 *									*
 * This module also maintains an array of "ghost points" for use in     *
 * ghosting vgf elements.  This is the only file that should set or     *
 * manipulate the _ghostX & _ghostY coordinate arrays.  DO NOT ever     *
 * manipulate them elsewhere.						*
 *									*
 * CONTENTS:								*
 * pggst_initGc()		set up environment for ghost lines	*
 * pggst_drawGhost()		routine for drawing ghosts		*
 * pggst_tempLine()		draw 'dropped' lines			*
 * pggst_setText()		sets the text string			*
 *									*
 * pggst_cursorGhost()		draw a single point (cursor) ghost 	*
 * pggst_addGhostPts()		add new points to the ghost arrays	*
 * pggst_replaceGhostPts()      replace points in ghost arrays		*
 * pggst_clearGhost()   	reset the ghost arrays to 0		*
 * pggst_enableGhost()		make ghosting visible (mouse on canvas)	*
 * pggst_disableGhost()		make ghosting invisible (off canvas)	*
 * pggst_veilGhost()		select whether to veil mouse off canvas	*
 * pggst_redrawGhost()		redraw the existing ghost line		*
 *									*
 * pggst_ghostLine()		internal routine for drawing ghosts	*
 * pggst_getSmthPts()		calculate the pts - new, smoothed lines *
 * pggst_drawTemp()             draw part of line to the Gc             *
 * pggst_touchGhost()           sets _chngdGhost flag to TRUE           *
 ***********************************************************************/


/*=====================================================================*/

void pggst_initGc ( void )
/************************************************************************
 * pggst_initGc								*
 *                                                                      *
 * This function sets the values for _drawXORGc and _drawGc.  It should *
 * be called only once per nmap instance.				*
 *                                                                      *
 * void pggst_initGc ( )                                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      None                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       05/98   Initial coding                          *
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * H. Zeng/EAI          06/00   added parameter to pggst_clearGhost()   *
 * M. Li/GSC		08/00	added line width and xw_gxor		*
 * T. Piper/SAIC	07/03	made draw_area global			*
 * J. Wu/SAIC		06/05	allocate _ghostX/_ghostY dynamically	*
 ***********************************************************************/
{
    int			istat, icolr, colbnk, ier;
    XGCValues		vals;
    Pixel		fg, bg;
    static Boolean	first_time = TRUE;
/*---------------------------------------------------------------------*/

     if (!first_time) 
         return;

     first_time = FALSE;

     /* 
      * Allocate memory for storing ghost points
      */
    G_MALLOC (   _ghostX, float, _nPts, "pggst_initGc" );
    G_MALLOC (   _ghostY, float, _nPts, "pggst_initGc" );
    G_MALLOC ( _smooth_x, float, _nPts, "pggst_initGc" ); 
    G_MALLOC ( _smooth_y, float, _nPts, "pggst_initGc" );

     /* get the Widget ID of the drawing area */
     _draw_area = (Widget)mcanvw_getDrawingW();

     colbnk = 0;

     xw_gxor(&icolr, &ier);

     xqcolr( &colbnk, &icolr, &fg, &istat );
     icolr = 0;
     xqcolr( &colbnk, &icolr, &bg, &istat );

     /*
      * set up the gc for temporary lines
      */
     vals.foreground = fg;
     vals.background = bg;
     vals.line_width = 2;
     _drawGc = XtGetGC(_draw_area, GCForeground|GCBackground|GCLineWidth, &vals);

     /*
      * set up the gc for ghost lines
      * (it seems that the 'foreground' and 'background' colors
      * are XORed to get the source color when XOR function is set)
      */
     vals.foreground = fg ^ bg;
     vals.function = GXxor;
     _drawXORGc = XtGetGC(_draw_area,
                GCForeground|GCBackground|GCFunction|GCLineWidth, &vals);

     pggst_clearGhost(TRUE);
}


/*=====================================================================*/

void pggst_drawGhost ( char ghost_typ )
/************************************************************************
 * pggst_drawGhost                                                      *
 *                                                                      *
 * This is the public function to draw the current ghost line to the    *
 * screen.                                                              *
 *                                                                      *
 * void pggst_drawGhost ( ghost_typ )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *	ghost_typ	char	type of ghosting			*
 *                                                                      *
 * Output parameters:                                                   *
 *                      None                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       08/98   Initial coding                          *
 * E. Safford/GSC       08/98   clean up & add ghost_typ                *
 * E. Safford/GSC       10/00   MAXGHOST -> LLMXPT                      *
 * J. Wu/SAIC       	06/05   manage memory dynamically		*
 ***********************************************************************/
{
int		ii, idx, ier, i_term, f_term;
/*---------------------------------------------------------------------*/

    if (_chngdGhost) {

	if (_ghostPts > 2)  {

	    switch (ghost_typ) {
		case GST_NORMAL:
                    pgutls_getSmoothPts (_ghostPts, _ghostX, _ghostY,
                             (int)_smoothLvl, _nPts, 0, 0, &_smooth_pts,
                                                _smooth_x, _smooth_y, &ier);
		    break;

		case GST_NEW_LINE:

		    /*
		     *  First update the tempLine if necessary
		     */

		    if ((!_closedFig && _ghostPts - 3 > _drawPt) ||
			     (_closedFig && _ghostPts - 5 > _drawPt)){
		        pggst_drawTemp();
		    }

		    /*
		     *  If smoothed then calculate the smoothed points
		     *  for the line/front, else transfer the points 
		     *  from the _ghostX/Y arrays.
		     */
		    if (_smoothLvl) {			/* smoothed */
		        pggst_getSmthPts(_nPts, &_smooth_pts, 
		    				_smooth_x, _smooth_y);
		    }
		    else {				/* non-smoothed */
			idx=0;
			for (ii=_drawPt; ii < _ghostPts; ii++, idx++) {
			    _smooth_x[idx] = _ghostX[ii];
			    _smooth_y[idx] = _ghostY[ii];
			}

			_smooth_pts = _ghostPts - _drawPt;

			/*
			 *  If anything is drawn to tempLine then add the
			 *  segment from 0 to 1 (tempLine will start at 1.
			 */
  			if (_closedFig && _drawPt) { 
			    _smooth_x[_smooth_pts] = _ghostX[1];
			    _smooth_y[_smooth_pts] = _ghostY[1];
			    _smooth_pts++;
			}
		    }
		    break;

		case GST_INIT_TRNC:
		case GST_FINAL_TRNC:
		case GST_BOTH_TRNC:
	 	    if (ghost_typ == GST_INIT_TRNC || 
		    	ghost_typ == GST_BOTH_TRNC)
		        i_term = 1;
		    else
		        i_term = 0;

	 	    if (ghost_typ == GST_FINAL_TRNC || 
		    	ghost_typ == GST_BOTH_TRNC)
		        f_term = _ghostPts - 2;
		    else
		        f_term = _ghostPts - 1;

                    pgutls_getSmoothPts (_ghostPts, _ghostX, _ghostY,
                             (int)_smoothLvl, _nPts, i_term, f_term, 
		     		&_smooth_pts, _smooth_x, _smooth_y, &ier); 
		    break;
	
		default: 
		    break;
	    }
    	}
        else {		 	/* two points or less */
	    for (ii=0; ii<_ghostPts; ii++) {
	        _smooth_x[ii] = _ghostX[ii];
	        _smooth_y[ii] = _ghostY[ii];
	    }
	    _smooth_pts = _ghostPts;
 	}
    }

    pggst_ghostLine (_smooth_pts, _smooth_x, _smooth_y);
    _chngdGhost = FALSE;

}

/*=====================================================================*/

void pggst_tempLine ( int np, float rx[], float ry[] )
/************************************************************************
 * pggst_tempLine                                                       *
 *                                                                      *
 * This function draws a line to the _drawGc context                    *
 *                                                                      *
 * void pggst_tempLine ( np, rx, ry )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *      np      int     number of points to draw                        *
 *      rx[]    float   array of x coordinates                          *
 *      ry[]    float   array of y coordinates                          *
 *                                                                      *
 * Output parameters:                                                   *
 *                      None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       05/98   copied pggst_ghostLine                  *
 * E. Safford/GSC       08/98   add start & end and smoothed line draws *
 * E. Safford/GSC       10/99   update for new xwcmn.h                  *
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * T. Piper/SAIC	06/03	added XtDisplay and XtWindow		*
 ***********************************************************************/
{
    int	ii, xoff, yoff, ier;
/*---------------------------------------------------------------------*/

    xgtoff (&xoff, &yoff, &ier);

    if (np > 1) {
        for (ii = 0; ii< (np - 1); ii++) {
            XDrawLine(XtDisplay(_draw_area), XtWindow(_draw_area), _drawGc,
                      (int)rx[ii] - xoff,
                      (int)ry[ii] - yoff,
                      (int)rx[ii+1] - xoff, 
                      (int)ry[ii+1] - yoff); 
	}
    }

}

/*=====================================================================*/

void pggst_setText ( VG_DBStruct *el )
/************************************************************************
 * pggst_setText							*
 *									*
 * This function sets up the ghost text box.				*
 *									*
 * void pggst_setText (el)						*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	Element containing text		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/98	Copied from crg_settxt			*
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * W. Li/EAI		10/98	Added gosting box for north relative	*
 * E. Safford/GSC	10/98   mod to treat offset text as a single pt *
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * G. Krueger/EAI	06/99	Fixed ghost flash problem		*
 * S. Law/GSC		03/00	Cleanup					*
 * S. Law/GSC		04/00	Fixed cleanup				*
 ***********************************************************************/
{
    int	np;
/*---------------------------------------------------------------------*/

    np = 1;

    if (el->hdr.vg_class == 0 || 
	  (el->hdr.vg_type == TEXT_ELM && 
	     (el->elem.txt.info.offset_x  || el->elem.txt.info.offset_y)) ||
	  (el->hdr.vg_type == SPTX_ELM && 
	     (el->elem.spt.info.offset_x  || el->elem.spt.info.offset_y))) { 

	if (_oldPtX[0] >= 0.0F) {	/* erase previous ghost, if any */
	    pggst_ghostLine (np, _oldPtX, _oldPtY);
	}

	_oldPtX[0] = _oldPtY[0] = -99.0F;

	_textFlag = FALSE;
	_ghostInhibit = FALSE;
    }
    else {
	if (_oldPtX[0] >= 0.0F) {	/* erase previous ghost, if any */
	    pggst_ghostLine (np, _oldPtX, _oldPtY);
	}

	_textFlag = TRUE;
	_circleFlag = FALSE;

	if (el->hdr.vg_type == SPTX_ELM) {
	    _textLen = (int)strlen (el->elem.spt.text);
	}
	else {
	    _textLen = (int)strlen (el->elem.txt.text);
	}

        _txtRotn  = el->elem.spt.info.rotn;

	crg_gettxtbox (el, 0, _textX, _textY);

	/* close box */
	_textX[4] = _textX[0];
	_textY[4] = _textY[0];

	_oldPtX[0] = _textX[0];
	_oldPtY[0] = _textY[0];
    }
}


/*=====================================================================*/

void pggst_setCircle ( VG_DBStruct *el )
/************************************************************************
 * pggst_setCircle							*
 *									*
 * This function sets up the circle ghost.				*
 *									*
 * void pggst_setCircle (el)						*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	Element containing circle	*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	05/99	Copied from pggst_setText		*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * H. Zeng/SAIC		09/06	added call to pgdist_isDplOn()		*
 ***********************************************************************/
{
    int		np, ier;
/*---------------------------------------------------------------------*/

    np = 2;

    /*
     * Erase the previous ghost.
     */
    if ( _oldPtX[0] >= 0.0F && !pgdist_isDplOn() ) { 
	 pggst_ghostLine (np, _oldPtX, _oldPtY);
    }

    if (el->hdr.vg_class == 0) { 
	/*
	 * finished with ghost circling
	 */
	_oldPtX[0] = _oldPtY[0] = -99.0F;
	_oldPtX[1] = _oldPtY[1] = -99.0F;
	_circleFlag = FALSE;
	_ghostInhibit = FALSE;
    }
    else {
	_circleFlag = TRUE;
	_textFlag = FALSE;

	gtrans ( sys_M, sys_D, &np, &el->elem.cir.data.latlon[0],
		 &el->elem.cir.data.latlon[np], _circleX, _circleY,
		 &ier, strlen(sys_M), strlen(sys_D) );

	_oldPtX[0] = _circleX[0];
	_oldPtY[0] = _circleY[0];
	_oldPtX[1] = _circleX[1];
	_oldPtY[1] = _circleY[1];
	pggst_ghostLine (1, _oldPtX, _oldPtY); /* draw new ghost */
    }
}

/*=====================================================================*/

void pggst_setLineAttr ( char smooth, Boolean closed )
/************************************************************************
 * pggst_setLineAttr                                                    *
 *                                                                      *
 * This function updates the attributes of smoothed and closed.  These  *
 * attributes only have meaning if the number of ghost points is        *
 * greater than three.							*
 *                                                                      *
 * void pggst_setLineAttr ( smooth, closed )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	smooth	char	smoothing level         			*
 *	closed	Boolean	closed figure flag             			*
 *									*
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	08/98	initial coding				*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 ***********************************************************************/
{
    _smoothLvl = smooth;
    _closedFig = closed;    
}


/*=====================================================================*/

void pggst_cursorGhost ( float *xx, float *yy, int *iret )
/************************************************************************
 * pggst_cursorGhost							*
 *									*
 * This is a convenience routine to draw a single cursor point.		*
 * 									*
 * void pggst_cursorGhost ( xx, yy, iret )				*
 * 									*
 * Input parameters:                                                    *
 *  *xx		float	x coordinate of new ghost point        		*
 *  *yy		float	y coordinate of new ghost point        		*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret	int	return code                                     *
 *			 0 = normal					*
 *			-1 = MAXGHOST exceeded				*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	12/04	created					*
 ************************************************************************/ 
{
    pggst_clearGhost (TRUE);
    pggst_addGhostPts (1, xx, yy, iret);
    pggst_drawGhost (GST_NORMAL);
}

/*=====================================================================*/

void pggst_addGhostPts ( int np, float *xx, float *yy, int *iret )
/************************************************************************
 * pggst_addGhostPts                                                    *
 *                                                                      *
 * This function adds points to the ghost point coordinate arrays.      *
 *                                                                      *
 * void pggst_addGhostPts ( np, xx, yy, iret)				*
 *                                                                      *
 * Input parameters:                                                    *
 *	np		int	number of new points			*
 *	*xx		float	x coordinates of new ghost point	*
 *	*yy		float	y coordinates of new ghost point	*
 *									*
 * Output parameters:                                                   *
 *      *iret	int    return code		                        *
 *			   0 = normal					*
 *			  -1 = MAXGHOST exceeded			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	08/98	initial coding				*
 * E. Safford/GSC	09/98	mod to handle closed figures		*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * E. Safford/GSC       10/00   MAXGHOST -> LLMXPT                      *
 ***********************************************************************/
{
int	ii; 
/*---------------------------------------------------------------------*/
    if (_ghostPts + np > _nPts - 1) {
        *iret = -1;
	_chngdGhost = FALSE;
    }
    else {
        *iret = 0;

        if (_closedFig && _ghostPts > 2)
            _ghostPts--;

	for (ii=0; ii<np; ii++, _ghostPts++) {
	    _ghostX[_ghostPts] = xx[ii];
	    _ghostY[_ghostPts] = yy[ii];
	}

        /*
         *  Connect the last point to the first if this is a closed figure
         */
        if (_closedFig && _ghostPts > 2) {
            _ghostX[_ghostPts] = _ghostX[0];
            _ghostY[_ghostPts] = _ghostY[0];
            _ghostPts++;
        } 
  
        _chngdGhost = TRUE;
    }
}

/*=====================================================================*/

void pggst_replaceGhostPts ( int np, float *xx, float *yy, int *iret )
/************************************************************************
 * pggst_replaceGhostPts                                                *
 *                                                                      *
 * This function replaces the last np points in the arrays of ghost     *
 * coordinates.                                                         *
 *                                                                      *
 * void  pggst_replaceGhostPts( np, xx, yy, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	np		int		number of replacement points	*
 *	*xx		float		x coordinate of point		*
 *	*yy		float		y coordinate of point		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*iret		int	return value				*
 *				  0 = normal				*
 *				 -1 = LLMXPT exceeded			*
 *				 -2 = np greater than _ghostPts		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	08/98	initial coding				*
 * E. Safford/GSC	09/98	mod to handle closed figures		*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * E. Safford/GSC       10/00   MAXGHOST -> LLMXPT                      *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    if ( np > _ghostPts )
        *iret = -2; 
    else if (_ghostPts < _nPts - 1)
	*iret = 0;
    else 
	*iret = -1;

    if (*iret == 0) {
        if (_closedFig && _ghostPts > 2)
            _ghostPts--;

        for (ii=0; ii < np; ii++) {
	    _ghostX[_ghostPts - np + ii] = xx[ii];
	    _ghostY[_ghostPts - np + ii] = yy[ii];
        }

        if (_closedFig && _ghostPts > 2) {
    	    _ghostX[_ghostPts] = _ghostX[0];
    	    _ghostY[_ghostPts] = _ghostY[0];
	    _ghostPts++;
        }

        _chngdGhost = TRUE;
    } 
}


/*=====================================================================*/

void pggst_clearGhost ( Boolean reset_attr )
/************************************************************************
 * pggst_clearGhost                                                     *
 *                                                                      *
 * This function resets the _ghostPts counter to 0 and the smooth and   *
 * closed flags to their defaults.                     			*
 *                                                                      *
 * void  pggst_clearGhost( reset_attr )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	reset_attr	Boolean	switch to reset smooth & closed values  *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	08/98	initial coding				*
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * S. Law/GSC		04/00	Cleanup					*
 * J. Wu/SAIC		06/05	free memory allocated for smoothing	*
 ***********************************************************************/
{

    if (_ghostShow && _ghostVeil) {
	if ( _pointObj ) {
	    if ( _oldPtX[0] >= 0.0F ) {
		pggst_ghostLine (1, _oldPtX, _oldPtY);
	    }
	}
	else {
	    pggst_drawGhost(GST_NORMAL);
	}
    }

    _oldPtX[0] = -99.0F;
    _oldPtY[0] = -99.0F;
    _oldPtX[1] = -99.0F;
    _oldPtY[1] = -99.0F;

    _ghostShow = FALSE;

    _drawPt = _ghostPts = 0;
    _chngdGhost = TRUE;
    _ghostInhibit = FALSE;

    if (reset_attr) {
        _smoothLvl = 0;
        _closedFig = FALSE;
    }

    if (_textFlag || _circleFlag) {
	_oldPtX[0] = -99.0F;
	_oldPtY[0] = -99.0F;
	_oldPtX[1] = -99.0F;
	_oldPtY[1] = -99.0F;
    }
}

/*=====================================================================*/

void pggst_enableGhost ( void )
/************************************************************************
 * pggst_enableGhost							*
 *                                                                      *
 * This function enables the ghost line function.  Called by		*
 * mcanvw_eventHandler when the mouse enters the canvas.  This function	*
 * is not intended to be called by any other functions.			*
 *                                                                      *
 * void pggst_enableGhost ( )		 				*
 *                                                                      *
 * Input parameters:                                                    *
 *			None						*
 *                                                                      *
 * Output parameters:                                                   *
 *			None						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI	09/98						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    _ghostEnabled = TRUE;

}

/*=====================================================================*/

void pggst_disableGhost ( void )
/************************************************************************
 * pggst_disableGhost							*
 *                                                                      *
 * This function disables the ghost line function.  Called by		*
 * mcanvw_eventHandler when the mouse leaves the canvas.  This function	*
 * is not intended to be called by any other functions.			*
 *                                                                      *
 * void pggst_disableGhost ( )		 				*
 *                                                                      *
 * Input parameters:                                                    *
 *			None						*
 *									*
 * Output parameters:                                                   *
 *			None						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI	09/98						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
 
    if (_ghostVeil) {

	/* Erase previous ghost, if any. */
	if (_ghostShow) {
	    if (_pointObj) {
		if ( _oldPtX[0] >= 0.0F ) {
		    pggst_ghostLine (1, _oldPtX, _oldPtY);
		    _oldPtX[0] = -99.0F;
		    _oldPtY[0] = -99.0F;
		    _oldPtX[1] = -99.0F;
		    _oldPtY[1] = -99.0F;
		}
	    }
	    else {
		pggst_drawGhost(GST_NORMAL);
	    }
	    /* Inhibit attempt to erase ghost that we just erased. */
	    _ghostInhibit = TRUE;
	}
	/* Disable ghosting. */
	_ghostEnabled = FALSE;
    }

}

/*=====================================================================*/

void pggst_veilGhost ( Boolean veil_on )
/************************************************************************
 * pggst_veilGhost							*
 *                                                                      *
 * This function selects whether the ghost is to be veiled when the	*
 * mouse leaves the canvas.						*
 *                                                                      *
 * void pggst_veilGhost (veil_on)	 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	veil_on		Boolean		enable/disable veiling.		*
 *									*
 * Output parameters:                                                   *
 *			None						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI	09/98						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
    _ghostVeil = veil_on;
}

/*=====================================================================*/

void pggst_redrawGhost ( void )
/************************************************************************
 * pggst_redrawGhost                                                    *
 *                                                                      *
 * This function redraws the existing ghost line.  			*
 * 									*
 * First the part of the line that doesn't change is drawn using the    *
 * pggst_tempLine function.  On a non-smoothed line this is all but the *
 * last segement (_ghostsPts-1 to _ghostPts).  For a smoothed line      *
 * this is all but the last 3 points starting at 0 if not closed, or 1  *
 * if closed.								*
 *									*
 * Then the actual ghosted portion of the line is drawn by calling the  *
 * pggst_drawGhost function.						*
 *                                                                      *
 *									*
 * void pggst_redrawGhost ( )                                      	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * 	none                                           			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	01/01	initial coding            		*
 * J. Wu/SAIC		06/05	manage memory dynamically            	*
 ***********************************************************************/
{

int	ghost_pts, ghost_strt, smth_strt, smth_stop, temp_pts, ii, ier;
int	start;
float	*temp_x = (float *)NULL, *temp_y = (float *)NULL;
/*---------------------------------------------------------------------*/
        
    G_MALLOC ( temp_x, float, _nPts, "pggst_redrawGhost" );
    G_MALLOC ( temp_y, float, _nPts, "pggst_redrawGhost" );
    
    /*
     *  A non-smoothed line is only actually "ghosted" for the last
     *  point.  The rest of the line does not change in response to a 
     *  cursor drag, so it is drawn to the Gc using pggst_tempLine.
     *
     *  The last 3 points of a smoothed line are ghosted.  Any points more
     *  than that (which won't change with cursor movement) are drawn using 
     *  pggst_templine.
     */

    if ( _smoothLvl > 0 && _ghostPts > 3 ) {

	start = (_closedFig)? 1 : 0;

	for (ii = start; ii < _drawPt; ii++) {
	    /*
	     *  The smoothed section begins two points before the last drawn 
	     *  point (_drawPt) and is up to 6 points long.  The smoothed 
	     *  section is then truncated between the _drawPt and the next 
	     *  vertex (smth_strt & smth_stop).
	     */
            ghost_strt = (ii > 2)? ii-2 : 0;
	    ghost_pts  = (_ghostPts > 5) ?  6 : _ghostPts;
	
	    smth_strt  = ii - ghost_strt;
	    smth_stop  =  smth_strt + 1;

	    pgutls_getSmoothPts (ghost_pts, &_ghostX[ghost_strt], 
		    &_ghostY[ghost_strt], (int)_smoothLvl, _nPts, smth_strt,
			 smth_stop, &temp_pts, temp_x, temp_y, &ier);

            pggst_tempLine (temp_pts, temp_x, temp_y);
	}

    }
    else if ( _smoothLvl == 0 ) { 
	for (ii = 0; ii <= _drawPt+1; ii++) {
	    temp_x[ii] = _ghostX[ii];
	    temp_y[ii] = _ghostY[ii];
        }

        pggst_tempLine (_drawPt+1, temp_x, temp_y);
    }

    pggst_drawGhost ( GST_NORMAL ); 

    G_FREE ( temp_x, float );
    G_FREE ( temp_y, float );

}

/*=====================================================================*/

static void pggst_ghostLine ( int np, float rx[], float ry[] )
/************************************************************************
 * pggst_ghostLine                                                      *
 *                                                                      *
 * This function erases the previous ghostLine or draws the next using  *
 * the XOR function.  This is the internal routine for drawing ghosts   *
 * and should be called only from within nmap_pggst.c			*
 *                                                                      *
 * static void pggst_ghostLine ( np, rx, ry )                           *
 *                                                                      *
 * Input parameters:                                                    *
 * 	np	int	number of points to draw			*
 * 	rx[]	float	array of x coordinates				*
 * 	ry[]	float	array of y coordinates				*
 *                                                                      *
 * Output parameters:                                                   *
 *                      None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Law/GSC           05/98   Initial coding                          *
 * E. Safford/GSC	05/98	moved to nmap_pggst, added params	*
 * S. Law/GSC		07/98	Added ghosting for text			*
 * E. Safford/GSC	08/98	renamed to _pggst_ghostLine		*
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * W. Li/EAI		10/98	Added gosting box for north relative	*
 * E. Safford/GSC	10/98	rename gadzrm to gp_azdr		*
 * W. Li/EAI		03/99	Removed call to _pggst_drawLatLong	*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * G. Krueger/EAI	06/99	Fixed circle roam and edge of world	*
 * E. Safford/GSC	10/99	update for new xwcmn.h  		*
 * H. Zeng/EAI          12/99   fixed a bug on AIX4                     *
 * M. Li/GSC		 1/00	Used string variables in gtrans		*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * T. Piper/SAIC        06/03   added XtDisplay and XtWindow            *
 * S. Danz/AWC          03/06   Updated to use new crg_gettxtbox results*
 * H. Zeng/SAIC		03/06	added an arrowhead for lines		*
 * E. Safford/SAIC	07/06	correct offset problem with text box	*
 ***********************************************************************/
{
    int		ii, ier, nnp, xoff, yoff, ncirc, cur_class;
    int		idx, ghost_idx, halfline = 10;
    char	tag_val[128];
    float	tstx[2], tsty[2], tstlat[2], tstlon[2], px[2], py[2];
    float	pxl, pyb, pxr, pyt, dxl, dyb, dxr, dyt, xdiff;
    float	radrotn, bsin, bcos, rotn, srotn, nrotn, nrx, nry;
    float	rad, ang, ang1, ang2, arr_r, arr_x[2], arr_y[2];
    XPoint  	points[21];
/*---------------------------------------------------------------------*/

    if ( np <= 0 )  return;

    xgtoff (&xoff, &yoff, &ier);

    if ( _ghostEnabled ) {

	if ( ! _ghostInhibit ) {
	    _oldPtX[0] = rx[0];
	    _oldPtY[0] = ry[0];

	    if ( np == 1 )
		_pointObj = TRUE;
	    else {
	    	_pointObj = FALSE;
		_oldPtX[1] = rx[1];
		_oldPtY[1] = ry[1];
	    }

	    if (_textFlag) {
		if (_textLen > 0) {
		    if ((_txtRotn >= 0.00F) && (_txtRotn < 1000.00F)) {
			/* screen relative */
		        srotn = _txtRotn;
			radrotn = (float)((double)srotn * DTR);
		    }
		    else {
			/* north relative */
			nnp = 1;
    			gtrans (sys_D, sys_M, &nnp, &rx[0], &ry[0],
				&nrx, &nry, &ier,
				strlen(sys_D), strlen(sys_M) );
    			rotn = 0.0F;
      			gp_azdr (&rotn, &nrx, &nry, &nrotn, &ier ); 

    			srotn = _txtRotn - 1000.00F;
			radrotn = (float)((double)(srotn-nrotn) * DTR);
		    }

		    bcos = (float) cos ((double)radrotn);
		    bsin = (float) sin ((double)radrotn);

		    points[0].x = (short) ( rx[0] + (_textX[0] * bcos - _textY[0] * bsin) - xoff );
		    points[0].y = (short) ( ry[0] - (_textX[0] * bsin + _textY[0] * bcos) - yoff );
		    points[1].x = (short) ( rx[0] + (_textX[3] * bcos - _textY[3] * bsin) - xoff );
		    points[1].y = (short) ( ry[0] - (_textX[3] * bsin + _textY[3] * bcos) - yoff );
		    points[2].x = (short) ( rx[0] + (_textX[2] * bcos - _textY[2] * bsin) - xoff );
		    points[2].y = (short) ( ry[0] - (_textX[2] * bsin + _textY[2] * bcos) - yoff );
		    points[3].x = (short) ( rx[0] + (_textX[1] * bcos - _textY[1] * bsin) - xoff );
		    points[3].y = (short) ( ry[0] - (_textX[1] * bsin + _textY[1] * bcos) - yoff );
		    points[4].x = points[0].x;
		    points[4].y = points[0].y;

		    XDrawLines (XtDisplay(_draw_area), XtWindow(_draw_area), _drawXORGc,
		    		points, 5, 0);
		}
	    }
	    else if (_circleFlag) {
		/*
		 * Figure out whether the circle straddles a wrap line.
		 */
		nnp = 2;
		gtrans ( sys_D, sys_P, &nnp, _circleX, _circleY, px, 
		         py, &ier, strlen(sys_D), strlen(sys_P) );
		gqbnd ( sys_P, &pxl, &pyb, &pxr, &pyt, &ier, strlen(sys_P) );

		tstx[0] = pxr;
		tsty[0] = py[0];
		tstx[1] = pxl;
		tsty[1] = py[0];
		gtrans ( sys_P, sys_M, &nnp, tstx, tsty, tstlat, tstlon, 
		         &ier, strlen(sys_P), strlen(sys_M) );
		xdiff = _circleX[1] - _circleX[0];
		if ( G_DIFF(tstlon[1], tstlon[0]) ) {
		    /*
		     * Circle straddles a wrap line.  Make sure the
		     * radius point remains in the near half of the
		     * world.
		     */
		    gqbnd ( sys_D, &dxl, &dyb, &dxr, &dyt, &ier, strlen(sys_D) );
		    if ( px[1] - px[0] > .5F ) xdiff -= dxr - dxl;
		    if ( px[0] - px[1] > .5F ) xdiff += dxr - dxl;
		}
		rad = (float)(sqrt ( pow((double)xdiff, 2.0) +
			     pow((double)(_circleY[1] - _circleY[0]), 2.0)));
		/*
		 * Calculate points around the circle.
		 */
		ncirc = 20;
		for ( ii = 0; ii <= ncirc - 1; ii++ ) {
		    points[ii].x = (short)(rad * (float)cos((double)ii * TWOPI / (double)ncirc) + rx[0] - (float)xoff);
		    points[ii].y = (short)(rad * (float)sin((double)ii * TWOPI / (double)ncirc) + ry[0] - (float)yoff);
		}
		points[ncirc].x = points[0].x;
		points[ncirc].y = points[0].y;

		/*
		 * Draw circle.
		 */
		XDrawLines (XtDisplay(_draw_area), XtWindow(_draw_area), _drawXORGc,
			    points, ncirc+1, 0);
	    }
	    else if (np > 1) {
		for (ii = 0; ii< (np - 1); ii++) {
		    XDrawLine (XtDisplay(_draw_area), XtWindow(_draw_area), _drawXORGc,
			       (int)rx[ii] - xoff, 
			       (int)ry[ii] - yoff, 
			       (int)rx[ii+1] - xoff, 
			       (int)ry[ii+1] - yoff); 
		}

		/*
		 * Query the value of GHOST_ARROW_HD in prefs.tbl.
		 */
		ctb_pfstr ( "GHOST_ARROW_HD", tag_val, &ier );
		if ( ier == 0 ) {
		   if ( strcasecmp(tag_val,"ON") == 0 ) _ghostArrowFlg = TRUE;
		   else _ghostArrowFlg = FALSE;
		}
		else {
		   _ghostArrowFlg = FALSE;
		}

		/*
                 * If _ghostArrowFlg == TRUE, add an arrowhead at the cursor 
		 * point on the ghost line for LINES.
                 */
		cur_class = pgpalw_getCurClassId();
		if ( cur_class == CLASS_LINES && _ghostArrowFlg ) {

	          /*
                   * Determine in rx&ry array the index of the 
		   * point where the cursor is currently at.
                   */
                  if ( !_closedFig ) {

		    /*
                     * If the line is open, the last point in rx&ry
		     * array should be the point in question.
                     */
		    idx = np-1;
                  }
		  else {

		    /* 
                     * If the line is closed, we need to first
		     * determine in _ghostX&Y array the index of
		     * point where the cursor is currently at.
		     */
		    if ( fabs(_ghostX[_ghostPts-1] - _ghostX[0]) < 1.0E-5 &&
		         fabs(_ghostY[_ghostPts-1] - _ghostY[0]) < 1.0E-5   ) {
			
		      ghost_idx = _ghostPts-2;
		    }
		    else {
		      ghost_idx = _ghostPts-1;
	    	    }

		    /*
		     * From index (np-1) backward, find the point in
		     * question in rx&ry array.
		     */
		    idx = np-1;
		    for ( ii = np-1; ii >=0; ii-- ) {

		      if ( fabs(rx[ii] - _ghostX[ghost_idx]) < 1.0E-5 &&
			   fabs(ry[ii] - _ghostY[ghost_idx]) < 1.0E-5   ) {

		        idx = ii;
		      }
		    }

		  }

		  /*
		   * Prevent the situation of two points being in the same
		   * position. If that is the case, back off to use the 
		   * previous point.
		   */
		  if (  fabs(rx[idx-1] - rx[idx]) < 1.0E-5 &&
			fabs(ry[idx-1] - ry[idx]) < 1.0E-5    ) {
			
			idx = idx - 1;
	    	  }

		  /*
		   * Apply some trigonometry calculations and draw the 
		   * arrowhead.
		   */
		  if ( idx > 0 ) {

		    ang = (float)atan( (ry[idx-1]-ry[idx])/(rx[idx-1]-rx[idx]) );

	            if ( (rx[idx-1]-rx[idx]) < 0.0F )       ang = ang + PI;
                    else if ( (ry[idx-1]-ry[idx]) < 0.0F )  ang = ang + 2.0*PI;

		    ang1  = ang + PI / 6.0;
	            ang2  = ang - PI / 6.0;
		    arr_r = 20.0F;

	            arr_x[0] = rx[idx] + arr_r * cos(ang1);
	            arr_y[0] = ry[idx] + arr_r * sin(ang1);
	            arr_x[1] = rx[idx] + arr_r * cos(ang2);
	            arr_y[1] = ry[idx] + arr_r * sin(ang2);

		    XDrawLine (XtDisplay(_draw_area), XtWindow(_draw_area), 
			       _drawXORGc,
			       (int)arr_x[0] - xoff, 
			       (int)arr_y[0] - yoff, 
			       (int)rx[idx]  - xoff, 
			       (int)ry[idx]  - yoff ); 

		    XDrawLine (XtDisplay(_draw_area), XtWindow(_draw_area), 
			       _drawXORGc,
			       (int)arr_x[1] - xoff, 
			       (int)arr_y[1] - yoff, 
			       (int)rx[idx]  - xoff, 
			       (int)ry[idx]  - yoff );

	          } /* the end of if ( idx > 0 ) ... */

                } /* the end of if ( cur_class == CLASS_LINES ... */

	    } /* the end of else if (np > 1) ... */
	    else {
		XDrawLine (XtDisplay(_draw_area), XtWindow(_draw_area), _drawXORGc,
			   (int) rx[0] - xoff - halfline,
			   (int) ry[0] - yoff, 
			   (int) rx[0] - xoff + halfline,
			   (int) ry[0] - yoff);

		XDrawLine (XtDisplay(_draw_area), XtWindow(_draw_area), _drawXORGc,
			   (int) rx[0] - xoff,
			   (int) ry[0] - yoff - halfline,
			   (int) rx[0] - xoff,
			   (int) ry[0] - yoff + halfline);
	    }
	    _ghostShow = (Boolean)(! _ghostShow);

	}
	_ghostInhibit = FALSE;
    }
}

/*=====================================================================*/

static void pggst_getSmthPts ( int max_pts, int *pts, 
					float smth_x[], float smth_y[] )
/************************************************************************
 * pggst_getSmthPts                                                     *
 *                                                                      *
 * This function calculates the arrays of intermediate points on        *
 * smoothed lines.  It is used for drawing new, smoothed lines/fronts,  *
 * and excludes the sections of line drawn to the Gc component via      *
 * pggst_tempLine.							*
 *                                                                      *
 * static void pggst_getSmthPts ( max_pts, pts, smth_x, smth_y )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	max_pts	int	maximum number of points to return		*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*pts		int		number of points to draw	*
 * 	smth_x[]	float		array of x coordinates		*
 * 	smth_y[]	float		array of y coordinates		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	08/98	initial coding            		*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * E. Safford/GSC       10/00   MAXGHOST -> LLMXPT                      *
 * J. Wu/SAIC       	06/05   manage memory dynamically               *
 ***********************************************************************/
{
int	tmp_ghost, strt_idx, ii, ier, closed_pts;
int	start, stop;
float 	*closed_x, *closed_y;	
/*---------------------------------------------------------------------*/

    if (_closedFig) {          	/* closed figures */
        if (_drawPt < 2) {
	    /*
	     *  For fewer than 5 points the whole figure is drawn as a
	     *  ghost -- nothing is drawn using tempLine.  Add one more
	     *  point to connect the ends and get the smth_ points.
	     */
            _ghostX[_ghostPts] = _ghostX[0];
            _ghostY[_ghostPts] = _ghostY[0];
	   
            pgutls_getSmoothPts (_ghostPts + 1, _ghostX, _ghostY, 
	      (int)_smoothLvl, max_pts, 0, 0, pts, smth_x, smth_y, &ier);
        }
	else {
	    /*
	     *  Build a list of closed points which is a subset of 
	     *  _ghostPts, and excludes the drawn (non-ghosted) portion
	     *  of the temporary line (drawn using tempLine).  
	     */
	    closed_pts = 0;
	    G_MALLOC ( closed_x, float, _ghostPts+1, "pggst_getSmthPts" );
	    G_MALLOC ( closed_y, float, _ghostPts+1, "pggst_getSmthPts" );
	    for (ii=_drawPt -1; ii < _ghostPts -1; ii++, closed_pts++) {
		closed_x[closed_pts] = _ghostX[ii];	
		closed_y[closed_pts] = _ghostY[ii];	
	    }

	    /*
	     *  Add in the _ghostX/Y points 0, 1, and 2 so that the segment
	     *  from 0 to 1 can be ghosted.
	     */
	    for (ii=0; ii < 3; ii++, closed_pts++) {
		closed_x[closed_pts] = _ghostX[ii];	
		closed_y[closed_pts] = _ghostY[ii];	
	    }

	    /*
	     *  Start and stop are the truncation points for smth_ arrays.
	     */
 	    start = 1;
	    stop  = closed_pts - 2;
	  
            pgutls_getSmoothPts (closed_pts, closed_x, closed_y, 
		(int)_smoothLvl, max_pts, start, stop, pts, smth_x, smth_y, &ier); 
	    G_FREE ( closed_x, float );
	    G_FREE ( closed_y, float );
  
	}
    }
    else {			/* non-closed figure */

        /*
         * In order to calculate the intermediate smoothed points
         * we take the last 5 _ghostPts and calculate the smoothed
         * points for only those.
         */
        tmp_ghost = (_ghostPts < 5)? _ghostPts : 5;
        strt_idx  = _ghostPts - tmp_ghost; 
   
        if (_ghostPts < 4)
            start = 0;
	else if (_ghostPts == 4)
	    start = 1;
	else
            start = 2;

        pgutls_getSmoothPts (tmp_ghost, &_ghostX[strt_idx], 
		    		&_ghostY[strt_idx], (int)_smoothLvl, _nPts, 
				start, 0, pts, smth_x, smth_y, &ier);

    }

}

/*=====================================================================*/

static void pggst_drawTemp ( void ) 
/************************************************************************
 * pggst_drawTemp                                                       *
 *                                                                      *
 * This function draws part of the ghost line to the Gc using tempLine. *
 * Doing this saves space and time on new lines/fronts, and allows      *
 * full ghosting of smoothed lines/fronts.                              *
 *                                                                      *
 * static void pggst_drawTemp ( )                                   	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * 	none                                           			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	08/98	initial coding            		*
 * E. Safford/GSC	04/99	fix irix6 compiler warning		*
 * E. Safford/GSC       10/00   MAXGHOST -> LLMXPT                      *
 * J. Wu/SAIC           06/05   manage memory dynamically               *
 ***********************************************************************/
{
int	ghost_pts, ghost_strt, smth_strt, smth_stop, temp_pts, ii, ier;
float	temp_2x[2], temp_2y[2], *temp_x, *temp_y;
/*---------------------------------------------------------------------*/

    if (_closedFig && !_drawPt) {
        _drawPt = 1; 
	return;
    }

    ghost_pts = (_closedFig)? _ghostPts - 1 : _ghostPts;
    
    if (_smoothLvl) {
	/*
	 *  The smoothed section begins two points before the last drawn 
	 *  point (_drawPt) and is up to 6 points long.  The smoothed 
	 *  section is then truncated between the _drawPt and the next 
	 *  vertex (smth_strt & smth_stop).
	 */
        ghost_strt = (_drawPt > 2)? _drawPt-2 : 0;
	ghost_pts  = (ghost_pts > 5) ?  6 : ghost_pts;

	smth_strt  = _drawPt - ghost_strt;
	smth_stop  =  smth_strt + 1;
	G_MALLOC ( temp_x, float, _nPts, "pggst_drawTemp" );
	G_MALLOC ( temp_y, float, _nPts, "pggst_drawTemp" );

	pgutls_getSmoothPts (ghost_pts, &_ghostX[ghost_strt], 
		&_ghostY[ghost_strt], (int)_smoothLvl, _nPts, smth_strt,
			 smth_stop, &temp_pts, temp_x, temp_y, &ier);

	pggst_tempLine (temp_pts, temp_x, temp_y);
	G_FREE ( temp_x, float );
	G_FREE ( temp_y, float );
    }
    else { 
	for (ii = 0; ii < 2; ii++) {
	    temp_2x[ii] = _ghostX[_drawPt + ii];
	    temp_2y[ii] = _ghostY[_drawPt + ii];
        }
	temp_pts = 2;
	pggst_tempLine (temp_pts, temp_2x, temp_2y);
    }

    _drawPt++;

}

/*=====================================================================*/

void pggst_touchGhost ( void )
/************************************************************************
 * pggst_touchGhost							*
 *                                                                      *
 * This function sets the _chngdGhost flag to TRUE although nothing has *
 * changed to the current ghost line.					*
 *                                                                      *
 * void pggst_touchGhost ( )	 				        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			None						*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02      initial coding                       *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    _chngdGhost = TRUE;

}
