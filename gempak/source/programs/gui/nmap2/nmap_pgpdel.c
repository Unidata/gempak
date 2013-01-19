#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "hints.h"
#include "proto_xw.h"

#define DIST_LIMIT	15.0F		/* distance check limit */
#define THRESHOLD       0.5             /* minimum x,y distance 
					   for two points       */

#define TAIL		 0
#define HEAD		 1
#define BOTH		 2

#define AR_EMPTY_HEAD		 4	/* empty-head arrow */
#define AR_FILLED_HEAD		 6	/* filled-head arrow */
#define AR_FILLED_BALL_CHAIN	10	/* arrow, filled ball-chain */
#define AR_EMPTY_FILLED_BOX	13	/* arrow, empty box-filled box */
#define AR_BALL_CHAIN		16	/* arrow, ball-chain */
#define AR_DASHED_EMPTY_HEAD	20	/* dashed empty-head arrow */
#define AR_DASHED_FILLED_HEAD	21	/* dashed filled-head arrow */

#define CONTOUR			 1	/* contour */
#define FILLED_BALL		 9	/* filled ball */
#define BOX_BALL		 7	/* box-ball */
#define BALL_CHAIN		 1	/* ball-chain */
#define DASHED			 2	/* basic dashed line */

static float    *_dcX, *_dcY;
static int      _dcN;

typedef struct {		/* partial delete points buffer */
    float	x;		/* x and y coordinates for closest */
    float	y;		/* point on element to click point */
    int		vert1;		/* closest vertex to click point */
    int		vert2;		/* next closest vertex */
} PDELSeg;

static PDELSeg  _pdelSeg[2];

static int	_firstPDelPt;	/* which point is first in _dcX/Y */ 
static Widget	_genPopup = NULL;
static int   	_smoothLvl;
static Boolean	_closedFig;
static Boolean	_allowClose;
static Boolean	_gotArrow = FALSE;
static int	_useArrow = HEAD;

static int	_highEnd = 0;		/* endpoint flag  */
static int	_lowEnd  = 0;		/* endpoint flag  */
static int	_endEx   = 0;		/* endpoint index */

static VG_DBStruct _pDelEl;


/*
 *   Private Callback Functions
 */
static void pgpdel_destroyWidCb  ( Widget, XtPointer, XtPointer );
static void pgpdel_partDeleteEh  ( Widget, XtPointer, XEvent*, Boolean* );
static void pgpdel_whichArrowsCb ( Widget, XtPointer, XtPointer );

/*
 *  Private Functions
 */
static Boolean pgpdel_checkDist ( float x1, float y1, float x2,
			    		float y2, float limit );
static void pgpdel_markRed ( float *xx, float *yy );
static void pgpdel_partDelDropClosed ( void );
static void pgpdel_partDelDropOpen ( void );


/************************************************************************
 * nmap_pgpdel.c							*
 *									*
 * This module contains the callback and support functions that are	*
 * used to accomplish partial deletes of lines and fronts.		*
 *									*
 * CONTENTS:								*
 * pgpdel_startPDel()		initialize required variables		*
 *									*
 * pgpdel_partDeleteEh()	event handler for mouse button presses	*
 * pgpdel_whichArrowCb()	callback for arrow check callback	*
 * pgpdel_destroyWidCb()	destroys the generic widget		*
 *									*
 * pgpdel_partDelDropOpen()	completion of partial delete		*
 * pgpdel_partDelDropClosed()	completion for closed lines		*
 * pgpdel_markRed()		mark coordinates with red		*
 * pgpdel_checkDist()		check if distance is under limit	*
 ***********************************************************************/

/*=====================================================================*/

void pgpdel_startPDel ( VG_DBStruct *el )
/************************************************************************
 * pgpdel_startPDel                                                     *
 *                                                                      *
 * Setup for partial delete operation.                                  *
 *                                                                      *
 * void pgpdel_startPDel  ( el )                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *	*el	VG_DBStruct	element to be partially deleted		*
 *                                                                      *
 * Output parameters:                                                   *
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC      06/98   intial coding                           *
 *  G. Krueger/EAI	06/98	Uniform status hints			*
 *  E. Safford/GSC      07/98   add _smoothLvl setup                    *
 *  G. Krueger/EAI	10/98	Using table for hints			*
 *  S. Law/GSC		09/99	add _allowClose				*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 *  J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 *  J. Wu/GSC		03/04	add GFA_ELM				*
 ***********************************************************************/
{
    mcanvw_setDynActFlag (FALSE);
    mcanvw_setPressFunc((XtEventHandler)&pgpdel_partDeleteEh, CURS_DEFAULT);

    _pDelEl = *el;
    _closedFig = _pDelEl.hdr.closed;
    _allowClose = (el->hdr.vg_class == CLASS_LINES || 
		   el->hdr.vg_class == CLASS_SIGMETS ||
		   el->hdr.vg_type == GFA_ELM );

    if (_pDelEl.hdr.closed) pgactv_addClosedPt ();

    _smoothLvl = el->hdr.smooth;

    pgactv_getDevPts (&_dcN, &_dcX, &_dcY);

    mbotw_mouseSet(LMHINT_START, MMHINT_DONE);
}


/*=====================================================================*/

static void pgpdel_partDeleteEh ( Widget wid, XtPointer clnt, 
					XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgpdel_partDeleteEh							*
 *									*
 * Callback for the partial delete button presses.			*
 *									*
 * static void pgpdel_partDeleteEh (wid, clnt, event, ctdr )		*
 *									*
 * Input parameters:							*
 *	wid		Widget	Calling widget				*
 *	clnt		XtPointer					*
 *	*event		XEvent						*
 *									*
 **									*
 * Log:									*
 *  E. Safford/GSC	02/98	intial coding				*
 *  E. Safford/GSC	06/98	move to nmap_pgpdel.c			*
 *  G. Krueger/EAI	06/98	Uniform status hints			*
 *  E. Safford/GSC	07/98	partial delete smoothed lines		*
 *  E. Safford/GSC	08/98	modified getSmoothPts call		*
 *  G. Krueger/EAI	10/98	Using table for hints			*
 *  S. Law/GSC		02/99	moved open/closed test from drop to here*
 *  S. Law/GSC		02/99	_pgpdel_getSmoothedPt -> pgutls_ and	*
 *				moved endpoint checks from dropOpen	*
 *  E. Safford/GSC	03/99	fixed erroneous delete			*
 *  S. Law/GSC		09/99	changed to use _allowClose		*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  E. Safford/GSC	10/99	updated for new xwcmn.h			*
 *  S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 *  H. Zeng/EAI         11/00   changed for the new undo design         *
 *  H. Zeng/EAI         12/00   modified for multiple undo steps        *
 *  E. Safford/GSC	02/01	add check on distance to line		*
 *  J. Wu/GSC		03/01	added "EXIT" hint to bottom panel       *
 *  W.D.Plummer/NCEP	12/02	chg call sequence of cgr_segdist	*
 ***********************************************************************/
{
    int		nearest, ier, ii, dummy, start, end, xoff, yoff;
    int         el_location;
    float	xx, yy, startx, starty, stopx, stopy;
    float	dist, dist0, dist1, llx, lly, urx, ury;
    Boolean	low_test, hi_test;
/*---------------------------------------------------------------------*/

    if (event->xbutton.button == Button1) {
	xgtoff (&xoff, &yoff, &ier);
        xx = (float) (event->xbutton.x + xoff); 
        yy = (float) (event->xbutton.y + yoff); 


        if (mcanvw_getDynActFlag()) {     /* dynamics are active */

            cgr_segdist (&_dcN, _dcX, _dcY, &xx, &yy, &dist,
			 &_pdelSeg[1].vert1, &_pdelSeg[1].vert2, 
			 &stopx, &stopy, &ier);
	    if ( _smoothLvl == 0 && dist > LINE_TIEIN ) {
	        return;
	    }

 	    start = _pdelSeg[1].vert1;
	    end   = _pdelSeg[1].vert2;

	    if (_smoothLvl) {
		if (_closedFig) {
  	            if (start == _dcN-1) {
	                start = 0;
		    }
	            else if (end == _dcN-1) {
	                end   = 0;
		    }
		}

	        if (start > end) {
	            dummy  = start;
		    start  = end;
		    end    = dummy;
	        }

    	        pgutls_getSmoothedPt( xx, yy, start, end, _closedFig,
				     _smoothLvl, &stopx, &stopy, &ier);
		if ( ier > 0 ) {
		    return;
		}
	    }

	    pgpdel_markRed (&stopx, &stopy);

            _pdelSeg[1].x = stopx;
            _pdelSeg[1].y = stopy;

	    /*
	     *  Determine the order of the two new points, relative to the
	     *  fill order of the line (0 thru _dcN-1) by comparing the
	     *  closest vertices to both points.
	     */

            if (_pdelSeg[0].vert1 < _pdelSeg[1].vert1) {
                _firstPDelPt = 0;
	    }
            else if (_pdelSeg[0].vert1 > _pdelSeg[1].vert1) {
                _firstPDelPt = 1;
	    }
            else if (_pdelSeg[0].vert2 < _pdelSeg[1].vert2) {
                _firstPDelPt = 0;
	    }
            else if (_pdelSeg[0].vert2 > _pdelSeg[1].vert2) {
                _firstPDelPt = 1;
	    }
            else {
		/*
		 *  If equal then compare the respective distances of each
		 *  point to the shared closest vertex.  Then check the order
		 *  of the end points of one of the segments.  If the 0 segment
		 *  is greater than the 1, then the points are nearing a vertex
		 *  so the first line ends at the point furthest from the
		 *  shared vertex, and thus the first point is the more distant.
		 */

                ii = _pdelSeg[0].vert1;

                cgr_dist (1, (_dcX+ii), (_dcY+ii), _pdelSeg[0].x,
			  _pdelSeg[0].y, &dist0, &nearest, &ier);

                cgr_dist (1, (_dcX+ii), (_dcY+ii), _pdelSeg[1].x,
			  _pdelSeg[1].y, &dist1, &nearest, &ier);
	
                if (dist0 < dist1) {
                    _firstPDelPt = 0;
		}
                else {
                    _firstPDelPt = 1;
		}

                if (_pdelSeg[0].vert1 > _pdelSeg[0].vert2) {
                    _firstPDelPt = !(_firstPDelPt);
		}
            }
	    

	    /*
	     *  Test for selection of both end points.  If both have been
	     *  selected the line will be deleted entirely.
	     */
	    _lowEnd = _highEnd = 0;
	    for (ii=0; ii<2; ii++) {

	        low_test = hi_test = FALSE;
		low_test = pgpdel_checkDist (_pdelSeg[ii].x, _pdelSeg[ii].y,
			       *(_dcX), *(_dcY), DIST_LIMIT);
		hi_test  = pgpdel_checkDist (_pdelSeg[ii].x, _pdelSeg[ii].y,
			       *(_dcX + _dcN-1), *(_dcY + _dcN-1), DIST_LIMIT);

		cgr_dist(_dcN, _dcX, _dcY, _pdelSeg[ii].x, _pdelSeg[ii].y,
		    		&dist, &nearest, &ier);


 		if (low_test && nearest == 0) {
		    _lowEnd = ii+1;
		    _endEx = _lowEnd % 2;     /* ie 0 if 2, 1 if 1, since */
  		}
		if (hi_test && nearest == _dcN-1) {
		    _highEnd = ii+1;
		    _endEx = _highEnd % 2;   /* you want the opposite vertex */
  		}   
	    }

	    if (_lowEnd && _highEnd) {
		pgdel_deletEh (wid, clnt, event, ctdr);
	    }
	    else {
	        el_location = pgactv_getElmLoc();
		pgutls_prepNew (el_location, &_pDelEl, &llx, &lly, 
                                &urx, &ury, &ier);
		pgundo_newStep();
		pgundo_storeThisLoc(el_location, UNDO_DEL, &ier);

		if (_allowClose && _closedFig) {
		    pgpdel_partDelDropClosed ();
		}
		else {
		    pgpdel_partDelDropOpen ();
		}
                pgundo_endStep();

	    }

            mcanvw_setDynActFlag(False);

	    mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
        }
        else {                            /* dynamics are inactive */

            cgr_segdist (&_dcN, _dcX, _dcY, &xx, &yy, &dist,
			 &_pdelSeg[0].vert1, &_pdelSeg[0].vert2, 
			 &startx, &starty, &ier);
	    if ( _smoothLvl == 0 && dist > LINE_TIEIN ) {
	        return;
	    }

	    start = _pdelSeg[0].vert1;
	    end   = _pdelSeg[0].vert2;

	    if (_smoothLvl) {
		if (_closedFig) {
  	            if (start == _dcN-1) {
	                start = 0;
		    }
	            else if (end == _dcN-1) {
	                end   = 0;
		    }
		}
  
	        if (start > end) {
	            dummy  = start;
		    start  = end;
		    end    = dummy;
	        }

    	        pgutls_getSmoothedPt (xx, yy, start, end, _closedFig,
				      _smoothLvl, &startx, &starty, &ier);
		if ( ier > 0 ) {
		    return;
		}

	    }

            _pdelSeg[0].x = startx;
            _pdelSeg[0].y = starty;

	    pgpdel_markRed (&startx, &starty);

            mcanvw_setDynActFlag(TRUE);

	    mbotw_mouseSet(LMHINT_END, MMHINT_DONE);
        }
    }
    else {          /*  Mouse button != Button1  */

        mcanvw_disarmDynamic ();
        mcanvw_setDynActFlag(False);

        pghdlb_deselectAll();

        mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);

        mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
    }

}


/*=====================================================================*/
/* ARGSUSED */
static void pgpdel_whichArrowsCb ( Widget wid, XtPointer which, XtPointer cbs )
/************************************************************************
 * pgpdel_whichArrowsCb							*
 *									*
 * Callback for the partial delete button presses.			*
 *									*
 * static void pgpdel_whichArrowsCb (wid, which, cbs)		*
 *									*
 * Input parameters:							*
 *	wid		Widget	Calling widget				*
 *	which		XtPointer which segments get arrow heads	*
 *	cbs	XtPointer					*
 *									*
 **									*
 * Log:									*
 *  S. Law/GSC		02/99	intial coding				*
 *  S. Law/GSC		02/99	added call to XtDestroyWidget		*
 *  E. Safford/SAIC	04/04	add check on _genPopup			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    if ( _genPopup != NULL ) XtDestroyWidget (_genPopup);
    
    _genPopup = NULL;
    _useArrow = (long)which;
    _gotArrow = TRUE;

    pgpdel_partDelDropOpen ();

}

/*=====================================================================*/
/* ARGSUSED */
static void pgpdel_destroyWidCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgpdel_destroyWidCb							*
 *									*
 * Callback for the partial delete button presses.			*
 *									*
 * static void pgpdel_destroyWidCb (wid, clnt, cbs)		*
 *									*
 * Input parameters:							*
 *	wid		Widget		Calling widget			*
 *	clnt		XtPointer	not used			*
 *	cbs	XtPointer	not used			*
 *									*
 **									*
 * Log:									*
 *  S. Law/GSC		02/99	intial coding				*
 *  E. Safford/SAIC	04/04	add check on _genPopup			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    if ( _genPopup != NULL ) XtDestroyWidget (_genPopup);

    _genPopup = NULL;
    _gotArrow = TRUE;

    pgpdel_partDelDropOpen ();

}

/*=====================================================================*/

static void pgpdel_partDelDropOpen ( void )
/************************************************************************
 * pgpdel_partDelDropOpen						*
 *									*
 * Routine for finishing a partial delete				*
 *									*
 * static void pgpdel_partDelDropOpen ()				*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 *  E. Safford/GSC      02/98   copied from modifyDrag                  *
 *  E. Safford/GSC      04/98   revise refresh scheme                   *
 *  E. Safford/GSC      05/98   update undo                             *
 *  E. Safford/GSC      06/98   move to nmap_pgpdel.c	                *
 *  E. Safford/GSC      07/98   clean up             	                *
 *  S. Law/GSC		02/99	added distance check and arrow prompts	*
 *  S. Law/GSC		02/99	moved endpoint checks to partDeleteCb	*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 *  H. Zeng/EAI         11/00   changed for the new undo design         *
 *  A. Hardy/GSC        11/00   renamed coordinate system declaration   *
 *  J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 *  J. Wu/SAIC		09/01	cast "second_pt" from Boolean to int	*
 *  J. Wu/SAIC		12/01	add layer in crg_set() call		*
 *  J. Wu/SAIC		01/02	add layer in crg_get() call		*
 *  E. Safford/SAIC	04/04	calling sequence change to NxmGeneric	*
 *  J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 *  S. Danz/AWC         08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		location, ier,  np, ii, start, stop, num, layer, el_layer;
    Boolean	second_pt;
    float	llx, lly, urx, ury, rx[MAXPTS], ry[MAXPTS];
    float	llx2, lly2, urx2, ury2;
    static Widget	mock_parent;

    VG_DBStruct	dropArrowEl, *pel1, *pel2;
    char	*buttonStrings[] = { "Tail only", "Head only", "Both" };
    filter_t	filter;
/*---------------------------------------------------------------------*/

    second_pt = !(_firstPDelPt);

    /*
    *  One of the end points is to be deleted - truncate line/fnt & resave
    */
    layer = pglayer_getCurLayer( );
    if (_lowEnd || _highEnd) {
        if (_highEnd) {

            start = 0;
	    stop = (_pdelSeg[_endEx].vert2 > _pdelSeg[_endEx].vert1) ? 
		_pdelSeg[_endEx].vert1 : _pdelSeg[_endEx].vert2;
            np = 0;
        }
        else {
	    start = (_pdelSeg[_endEx].vert2 < _pdelSeg[_endEx].vert1) ? 
		_pdelSeg[_endEx].vert1 : _pdelSeg[_endEx].vert2;
            stop = _dcN-1;

	    rx[0] = _pdelSeg[_endEx].x;
	    ry[0] = _pdelSeg[_endEx].y;

	    /* 
	     * if too close to vertex, ignore
	     */
	    np = (pgpdel_checkDist (rx[0], ry[0], 
				     *(_dcX+start), *(_dcY+start), 
				     DIST_LIMIT)) ? 0 : 1;
        }

        for (ii=start; ii<= stop; ii++, np++) {
            rx[np] = *(_dcX+ii);
            ry[np] = *(_dcY+ii);
        }

        if (_highEnd &&
	    !(pgpdel_checkDist (_pdelSeg[_endEx].x, _pdelSeg[_endEx].y, 
				 *(_dcX+stop-1), *(_dcY+stop-1), 
				 DIST_LIMIT))) {
	    rx[np] = _pdelSeg[_endEx].x;
	    ry[np] = _pdelSeg[_endEx].y;
            np++;
        }

        pgvgf_saveNewElm (NULL, sys_D, &_pDelEl, np, rx, ry, TRUE, 
			&location, &ier);
        pgundo_storeThisLoc (location, UNDO_ADD, &ier);

	crg_set (&_pDelEl, location, layer, &ier);
        crg_getinx (location, &num, &ier);
        crg_get(num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);
    }

    /*
     *  Deleted segment is mid-span and will result in two seperate elems
     */
    else {
	if (!_gotArrow &&
	    _pDelEl.hdr.vg_class == CLASS_LINES && 
	    _pDelEl.hdr.vg_type  == SPLN_ELM &&
	    (_pDelEl.elem.spl.info.spltyp == AR_EMPTY_HEAD ||
	     _pDelEl.elem.spl.info.spltyp == AR_FILLED_HEAD)) {

	    if (!mock_parent) {
		mock_parent = (Widget) mcanvw_getDrawingW ();
	    }

	    _genPopup = NxmGeneric_show 
		(mock_parent, "Partial Delete", 
		 "Which segments should have an arrow head?", 
		 XtNumber( buttonStrings ), buttonStrings,
		 (XtCallbackProc)pgpdel_whichArrowsCb);

            mcanvw_setPressFunc ((XtEventHandler)&pgpdel_destroyWidCb, CURS_DEFAULT);
	    return;
	}

	if (_gotArrow && _useArrow != BOTH) {
	    dropArrowEl.hdr.delete	= _pDelEl.hdr.delete;
	    dropArrowEl.hdr.vg_type	= LINE_ELM;
	    dropArrowEl.hdr.vg_class	= _pDelEl.hdr.vg_class;
	    dropArrowEl.hdr.filled	= _pDelEl.hdr.filled;
	    dropArrowEl.hdr.closed	= _pDelEl.hdr.closed;
	    dropArrowEl.hdr.smooth	= _pDelEl.hdr.smooth;
	    dropArrowEl.hdr.version	= _pDelEl.hdr.version;
	    dropArrowEl.hdr.grptyp	= _pDelEl.hdr.grptyp;
	    dropArrowEl.hdr.maj_col	= _pDelEl.hdr.maj_col;
	    dropArrowEl.hdr.min_col	= _pDelEl.hdr.min_col;
	    dropArrowEl.hdr.recsz	= _pDelEl.hdr.recsz;
	    dropArrowEl.hdr.range_min_lat	= _pDelEl.hdr.range_min_lat;
	    dropArrowEl.hdr.range_min_lon	= _pDelEl.hdr.range_min_lon;
	    dropArrowEl.hdr.range_max_lat	= _pDelEl.hdr.range_max_lat;
	    dropArrowEl.hdr.range_max_lon	= _pDelEl.hdr.range_max_lon;

	    dropArrowEl.elem.lin.info.numpts	= _pDelEl.elem.spl.info.numpts;
	    dropArrowEl.elem.lin.info.lintyp	= CONTOUR;
	    dropArrowEl.elem.lin.info.lthw	= 0;
	    dropArrowEl.elem.lin.info.width	= _pDelEl.elem.spl.info.splwid;
	    dropArrowEl.elem.lin.info.lwhw	= 0;

	    for (ii = 0; ii < (_pDelEl.elem.spl.info.numpts * 2); ii++) {
		dropArrowEl.elem.lin.latlon[ii] =  _pDelEl.elem.spl.latlon[ii];
	    }

	    pel1 = (_useArrow == TAIL || _useArrow == BOTH) ? 
		&_pDelEl : &dropArrowEl;
	    pel2 = (_useArrow == HEAD || _useArrow == BOTH) ? 
		&_pDelEl : &dropArrowEl;
	}
	else {
	    pel1 = pel2 = &_pDelEl;
	}

	_gotArrow = FALSE;

        start = 0;

        if (_pdelSeg[_firstPDelPt].vert1 < _pdelSeg[_firstPDelPt].vert2 ) {
            stop = _pdelSeg[_firstPDelPt].vert1;
	}
        else {
            stop = _pdelSeg[_firstPDelPt].vert2;
	}

        for (np=start; np<= stop; np++) {
            rx[np] = *(_dcX+np);
            ry[np] = *(_dcY+np);
        }

	/* if too close to vertex, ignore */
	if (!(pgpdel_checkDist (_pdelSeg[_firstPDelPt].x, 
				 _pdelSeg[_firstPDelPt].y, 
				*(_dcX+stop-1), *(_dcY+stop-1), 
				DIST_LIMIT))) {
	    rx[np] = _pdelSeg[_firstPDelPt].x;
	    ry[np] = _pdelSeg[_firstPDelPt].y;
	    np++;
	}

        pgvgf_saveNewElm (NULL, sys_D, pel1, np, rx, ry, TRUE, 
			&location, &ier);
        pgundo_storeThisLoc (location, UNDO_ADD, &ier);
 
        crg_set (pel1, location, layer, &ier);
        crg_getinx (location, &num, &ier);
        crg_get(num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);
 
        stop = _dcN -1;
        if (_pdelSeg[(int)second_pt].vert1 > _pdelSeg[(int)second_pt].vert2) {
            start = _pdelSeg[(int)second_pt].vert1;
	}
        else {
            start = _pdelSeg[(int)second_pt].vert2;
	}

	np = 0;
	/* if too close to vertex, ignore */
	if (!(pgpdel_checkDist (_pdelSeg[_firstPDelPt].x, 
				 _pdelSeg[_firstPDelPt].y, 
				*(_dcX+stop-1), *(_dcY+stop-1), 
				DIST_LIMIT))) {
	    rx[0] = _pdelSeg[(int)second_pt].x;
	    ry[0] = _pdelSeg[(int)second_pt].y;
	    np = 1;
	}

        for (ii=start; ii<= stop; ii++, np++) {
            rx[np] = *(_dcX+ii);
            ry[np] = *(_dcY+ii);
        }

        pgvgf_saveNewElm (NULL, sys_D, pel2, np, rx, ry, TRUE,
			&location, &ier);
        pgundo_storeThisLoc (location, UNDO_ADD, &ier);

        crg_set (pel2, location, layer, &ier);
        crg_getinx (location, &num, &ier);
        crg_get(num, &el_layer, filter, &llx2, &lly2, &urx2, &ury2, &ier);

        if (llx > llx2) llx = llx2;
        if (lly > lly2) lly = lly2;
        if (urx < urx2) urx = urx2;
        if (ury < ury2) ury = ury2;
    }
    cvg_rfrsh(NULL, llx, lly, urx, ury, &ier);

    mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);

}


/*=====================================================================*/

static void pgpdel_partDelDropClosed ( void )
/************************************************************************
 * pgpdel_partDelDropClosed                                             *
 *                                                                      *
 * Routine for finishing a partial delete on a closed line              *
 *                                                                      *
 * static void pgpdel_partDelDropClosed( )                              *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *    none                                                              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC  	02/98   copied from _pgevt_partDelDrop          *
 *  E. Safford/GSC  	04/98   revise refresh scheme                   *
 *  E. Safford/GSC      06/98   move to nmap_pgpdel.c	                *
 *  E. Safford/GSC      07/98   clean up             	                *
 *  S. Law/GSC		09/99	added SIGMET check			*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  S. Law/GSC		02/00	added CCF				*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 *  H. Zeng/EAI         11/00   changed for the new undo design         *
 *  A. Hardy/GSC        11/00   renamed coordinate system declaration   *
 *  J. Wu/SAIC		09/01	cast "second_pt" from Boolean to int	*
 *  J. Wu/SAIC		12/01	add layer in crg_set() call		*
 *  J. Wu/SAIC		12/01	add layer in crg_get() call		*
 *  H. Zeng/XTRIA       03/03   added check for the order of pts	*
 *  J. Wu/SAIC		03/04	add GFA_ELM				*
 *  J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 *  B. Yin/SAIC		02/05	add a call to snap GFA			*
 *  E. Safford/SAIC	06/05	allow smear to snap smaller on edit	*
 *  S. Danz/AWC         08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          02/07   Add logic to update GFA centroid 	*
 ***********************************************************************/
{
    int		location, ier, num, layer, el_layer;
    int		np, ii, jj, start, stop, one = 1;
    int		exclude_stop, exclude_start, excluded_pts;
    Boolean	second_pt, trav_dir;
    float	rx[MAXPTS], ry[MAXPTS], llx, lly, urx, ury;
    float       buf[MAXPTS], dis_x, dis_y;
    float       x_cntr, y_cntr, c_lat, c_lon, area;
    char	value[32];
    filter_t	filter;
/*---------------------------------------------------------------------*/


    second_pt = !(_firstPDelPt);

    if ( _pDelEl.hdr.vg_type != GFA_ELM ) {
        _pDelEl.hdr.closed = _closedFig = FALSE;
    }
    
    if (_pDelEl.hdr.vg_class == CLASS_SIGMETS) {
	if (_pDelEl.hdr.vg_type == SIGCCF_ELM) {
	    _pDelEl.elem.ccf.info.subtype = SIGTYP_LINE;
	}
	else {
	    _pDelEl.elem.sig.info.subtype = SIGTYP_LINE;
	}
    }
    
    /*
     *  convert any values of _dcN-1 to 0  and restore _dcN to
     *  original value
     */
    for (ii=0; ii<2; ii++) {
	if (_pdelSeg[ii].vert1 == _dcN-1) _pdelSeg[ii].vert1 = 0;
	if (_pdelSeg[ii].vert2 == _dcN-1) _pdelSeg[ii].vert2 = 0;
    }

    _dcN--;
    trav_dir = PREV;

    /*
     *  Determine the start and stop vertices from the original figure.
     *  The first case is two points on the same line segment.
     */
    if ( (_pdelSeg[_firstPDelPt].vert1 == _pdelSeg[(int)second_pt].vert1 &&
          _pdelSeg[_firstPDelPt].vert2 == _pdelSeg[(int)second_pt].vert2)       ||
	(_pdelSeg[_firstPDelPt].vert1 == _pdelSeg[(int)second_pt].vert2 &&
	 _pdelSeg[(int)second_pt].vert1    == _pdelSeg[_firstPDelPt].vert2) ) {

        if (_pdelSeg[_firstPDelPt].vert1 ==
	    pgutls_nextIndex (_dcN, _pdelSeg[_firstPDelPt].vert2, PREV)) {
            start = _pdelSeg[_firstPDelPt].vert1;
            stop  = _pdelSeg[_firstPDelPt].vert2;
        }
        else  {
            start = _pdelSeg[_firstPDelPt].vert2;
            stop  = _pdelSeg[_firstPDelPt].vert1;
        }
    }
    else {    /*  Two points not on the same line segment */

        if (_pdelSeg[_firstPDelPt].vert1 ==
	    pgutls_nextIndex (_dcN, _pdelSeg[_firstPDelPt].vert2, PREV)) {
            exclude_start = _pdelSeg[_firstPDelPt].vert1;
	}
        else {
            exclude_start = _pdelSeg[_firstPDelPt].vert2;
	}

        if (_pdelSeg[(int)second_pt].vert1 ==
	    pgutls_nextIndex (_dcN, _pdelSeg[(int)second_pt].vert2, PREV)) {
            exclude_stop = _pdelSeg[(int)second_pt].vert2;
	}
        else {
            exclude_stop = _pdelSeg[(int)second_pt].vert1;
	}


	/*
	 *  Count the vertices that would be excluded in a PREV traversal
	 *  from exclude_start to exclude_stop.
	 */
        excluded_pts = 1;

        for (ii=exclude_start; ii!= exclude_stop;
	     ii=pgutls_nextIndex (_dcN, ii, PREV)) {
            excluded_pts++;
        }


	/*
	 *  Use the traversal direction that eliminates the fewest
	 *  vertices, and set the traversal direction accordingly.
	 *  A special case is start & stop == 0.  Look for this first.
	 */
        if (exclude_start == 0 && exclude_stop == 0) {
            start = 1;
            stop  = _dcN-1;
            trav_dir = NEXT;
        }
        else if ((excluded_pts > 1) &&
		 (excluded_pts < _dcN - excluded_pts)) {
            start    = pgutls_nextIndex(_dcN, exclude_start, NEXT);
            stop     = pgutls_nextIndex(_dcN, exclude_stop, PREV);
            trav_dir = NEXT;
        }
        else {
            start    = exclude_start;
            stop     = exclude_stop;
        }
    }

    
    /*
     * Load latlon points for new element.
     *  Note: make a 10 times larger THRESHOLD for GFA_ELM
     *        to eliminate too close points.
     */
    np = 0;

    dis_x = *(_dcX+start) - _pdelSeg[_firstPDelPt].x;
    dis_y = *(_dcY+start) - _pdelSeg[_firstPDelPt].y;
    if ( _pDelEl.hdr.vg_type == GFA_ELM ) {
        dis_x = dis_x/10.0;
        dis_y = dis_y/10.0;
    }
    
    if( fabs((double)dis_x) >= THRESHOLD  ||
            fabs((double)dis_y) >= THRESHOLD      ) {
            
        /*
         *  Load the _firstPDelPt
         */
        rx[np] = _pdelSeg[_firstPDelPt].x;
        ry[np] = _pdelSeg[_firstPDelPt].y;
        np++;
    }     

    /*
     *  Then load from start to stop
     */

    for (ii = start; ii!= stop; 
	 ii= pgutls_nextIndex (_dcN, ii, trav_dir), np++) {
        rx[np] = *(_dcX+ii);
        ry[np] = *(_dcY+ii);
    }

    /*
     *  Load stop
     *  Note: make a 10 times larger THRESHOLD for GFA_ELM
     *        to eliminate too close points.
     */
    rx[np] = *(_dcX + stop);
    ry[np] = *(_dcY + stop);
    np++;

    dis_x = _pdelSeg[(int)second_pt].x - *(_dcX + stop);
    dis_y = _pdelSeg[(int)second_pt].y - *(_dcY + stop);

    if ( _pDelEl.hdr.vg_type == GFA_ELM ) {
        dis_x = dis_x/10.0;	   
        dis_y = dis_y /10.0;
    }

    if( fabs((double)dis_x) >= THRESHOLD  ||
            fabs((double)dis_y) >= THRESHOLD      ) {
            
        /*
         *  Load second_pt
         */
        rx[np] = _pdelSeg[(int)second_pt].x;
        ry[np] = _pdelSeg[(int)second_pt].y;
        np++;
    }     


    /*
     * Change the order of the new latlon points when necessary.
     */
    if ( trav_dir == PREV ) {

         for ( ii = 0, jj = np-1; ii < np; ii++, jj-- ) {
               buf[jj] = rx[ii];
         }
         for ( ii = 0; ii < np; ii++ ) {
               rx[ii] = buf[ii];
         }

         for ( ii = 0, jj = np-1; ii < np; ii++, jj-- ) {
               buf[jj] = ry[ii];
         }
         for ( ii = 0; ii < np; ii++ ) {
               ry[ii] = buf[ii];
         }

    }

    /*
     *  GFA_ELM must have at least 3 points. If not, do not
     *  change it. 
     */
    if ( _pDelEl.hdr.vg_type == GFA_ELM && np < 3 ) {
       np = _pDelEl.elem.gfa.info.npts;
       gtrans ( sys_M, sys_D, &np, &(_pDelEl.elem.gfa.latlon[0]), 
	    &(_pDelEl.elem.gfa.latlon[np]), rx, ry, 
	    &ier, strlen (sys_M), strlen (sys_D) );
    }
    else if ( _pDelEl.hdr.vg_type == GFA_ELM ) {
	_pDelEl.elem.gfa.info.npts = np;
       	gtrans ( sys_D, sys_M, &np, rx, ry, 
		 &(_pDelEl.elem.gfa.latlon[0]), &(_pDelEl.elem.gfa.latlon[np]), &ier,
 		 strlen(sys_D), strlen(sys_M) );

 	pgsmear_snapEl ( FALSE, &_pDelEl, &ier );

	np = _pDelEl.elem.gfa.info.npts;

	cvg_todev( &_pDelEl, &np, rx, ry, &ier );

	if ( pggfaw_isClosed() ) {
	    cgr_centroid( rx, ry, &np, &x_cntr, &y_cntr, &area, &ier );
	} else {
	    x_cntr = rx[ 0 ];
	    y_cntr = ry[ 0 ];
	}
	gtrans( sys_D, sys_M, &one, &x_cntr, &y_cntr, &c_lat, &c_lon, &ier, 
	    strlen(sys_D), strlen(sys_M) 
	    );

	sprintf ( value, "%7.2f", c_lat );
	cvg_setFld ( &_pDelEl, TAG_GFA_ARROW_LAT, value, &ier );
	sprintf ( value, "%7.2f", c_lon );
	cvg_setFld ( &_pDelEl, TAG_GFA_ARROW_LON, value, &ier );
    }

    pgvgf_saveNewElm (NULL, sys_D, &_pDelEl, np, rx, ry, TRUE,
    		&location, &ier);
    pgundo_storeThisLoc (location, UNDO_ADD, &ier);

    layer = pglayer_getCurLayer( );
    crg_set (&_pDelEl, location, layer, &ier);
    crg_getinx (location, &num, &ier);
    crg_get(num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);

    cvg_rfrsh(NULL, llx, lly, urx, ury, &ier);

    mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
}

/*=====================================================================*/

static void pgpdel_markRed ( float *xx, float *yy )
/************************************************************************
 * pgpdel_markRed                                                       *
 *                                                                      *
 * Routine for finishing a partial delete on a closed line              *
 *                                                                      *
 * static void pgpdel_markRed ( xx, yy )                                *
 *                                                                      *
 * Input parameters:                                                    *
 *	*xx	float	x coordinate for mark				*
 *	*yy	float	y coordinate for mark				*
 * Output parameters:                                                   *
 *    none                                                              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC      02/98   copied from _pgevt_partDelDrop          *
 *  E. Safford/GSC      04/98   revise refresh scheme                   *
 *  E. Safford/GSC      06/98   move to nmap_pgpdel.c                   *
 *  A. Hardy/GSC        11/00   renamed coordinate system declaration   *
 ***********************************************************************/
{
int	mrkclr, mrknp, ier;
/*---------------------------------------------------------------------*/

    mrkclr = 2;
    mrknp  = 1;
    gscolr ( &mrkclr, &ier);
    gmark (sys_D, &mrknp, xx, yy, &ier, strlen(sys_D));
    geplot( &ier );
}

/*=====================================================================*/

static Boolean pgpdel_checkDist ( float x1, float y1, float x2, float y2, 
							float limit )
/************************************************************************
 * pgpdel_checkDist							*
 *									*
 * This function checks whether the distance between two points is	*
 * below the given limit.						*
 *									*
 * static Boolean  pgpdel_checkDist (x1, y1, x2, y2, limit)		*
 *									*
 * Input parameters:							*
 *	x1	float		x coordinate for first point		*
 *	y1	float		y coordinate for first point		*
 *	x2	float		x coordinate for second point		*
 *	y2	float		y coordinate for second point		*
 *	limit	float		distance limit				*
 *									*
 * Output parameters:							*
 *                 	Boolean		True =  below limit		*
 **									*
 * Log:									*
 * S. Law/GSC		02/99	initial coding				*
 ***********************************************************************/
{
    float xx, yy, xy;
/*---------------------------------------------------------------------*/

    xx = (x2 - x1) * (x2 - x1);
    yy = (y2 - y1) * (y2 - y1);

    xy = (float) sqrt ((double) (xx + yy));

    return (xy < limit);
}

/*=====================================================================*/
