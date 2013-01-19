#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "proto_xw.h"


static int	_nearPt, _numVerts;
static float   	*_dcX, *_dcY;
static float	_altX[7], _altY[7];

static Boolean  _closedFig;
static int	_activePt;
static char	_smoothLvl;

/*
 *  Arrays for holding WATCH pgram coords
 */
static float   _watchX[11], _watchY[11];


/*
 *  private callback functions
 */
static void pgmodv_pgramDrag  ( Widget, XtPointer, XEvent* );
static void pgmodv_vertexDrag ( Widget, XtPointer, XEvent* );
static void pgmodv_vertexDrop ( Widget, XtPointer, XEvent* );


/************************************************************************
 * nmap_pgmodv.c                                                        *
 *                                                                      *
 * This module contains the select drag and drop callback functions for *
 * lines, fronts, and watches.    					*
 *                                                                      *
 * CONTENTS:                                                            *
 * pgmodv_start()	   set up for modifying vertices		*
 *                                                                      *
 * pgmodv_vertexDrag()    'SELECT' state -- drag  (motion)              *
 * pgmodv_pgramDrag()	   parallelogram drag				*
 * pgmodv_vertexDrop()    'SELECT' state -- drop  (mouse release)       *
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void pgmodv_start ( int funcflg, int smooth_lvl, char closed )
/************************************************************************
 * pgmodv_start	                                                        *
 *                                                                      *
 * Function to initialize select variables.                             *
 *                                                                      *
 * void pgmodv_start( funcflg, smooth_lvl, closed )                     *
 *                                                                      *
 * Input parameters:                                                    *
 * funcflg	int		Flag indicating which function to do	*
 * smooth_lvl	int		smoothing level of line/front		*
 * closed	char		closed figure flag			*
 *									*
 * Output parameters:                                                   *
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC	05/98	initial coding				*
 *  D.W.Plummer/NCEP	06/98	changes for pgram watch box		*
 *  E. Safford/GSC      07/98   change ghost of closed lines add smooth *
 *  G. Krueger/EAI	09/98	Added ghost veiling			*
 *  D.W.Plummer/NCEP	11/98	Changes for editing pgram watch edges	*
 *  S. Law/GSC		11/98	Changed current object device coords	*
 *				to be consistent with other functions	*
 *  S. Law/GSC		02/99	started using NEXT and PREV		*
 *  G. Krueger/EAI	05/99	Added circle draw function		*
 *  G. Krueger/EAI	06/99	Corrected circle ghosting		*
 *  S. Law/GSC		09/99	set ghost to always use non-closed line	*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  E. Safford/GSC	12/99	fix param list in pggst_addGhostPts 	*
 ***********************************************************************/
{
    int		ii, xy_idx, np, ier;
    Boolean	reset_attr;
/*---------------------------------------------------------------------*/
/*
 * get the point that causes the element to be selected
 */
    _nearPt = pgactv_getNearPt();
    pgactv_getDevPts (&np, &_dcX, &_dcY);
  
    _closedFig = (Boolean)((closed)? TRUE : FALSE);
    _smoothLvl = (char)smooth_lvl;

    pggst_veilGhost (FALSE);
    reset_attr = FALSE;
    pggst_clearGhost(reset_attr);

    pggst_setLineAttr (_smoothLvl, FALSE);

    if ( funcflg == 0 )  {

/*
 *  Normal line
 *
 *  Determine which vertices of the line/front are going to be 
 *  modified in this operation.  Set _numVerts to equal that number
 *  and advance the _dcX and _dcY pointers to the starting
 *  vertices.
 */
	if (!smooth_lvl) {

            if (np > 1) {
		if  (closed) {
		    _numVerts = 3;

/*
 *  Include the vert on either side of _nearPt
 */
		    xy_idx = pgutls_nextIndex (np, _nearPt, PREV);
		    for (ii=0; ii<3; ii++, 
		    		xy_idx = pgutls_nextIndex(np, xy_idx, NEXT)) {
                        _altX[ii] = *(_dcX + xy_idx);
                        _altY[ii] = *(_dcY + xy_idx);
		    }

/*
 *  Point _dcX & _dcY to the _altX/Y arrays
 */
                    _dcX = &_altX[0];
                    _dcY = &_altY[0];
		}
                else if ( _nearPt == 0 )  {
                    _numVerts = 2;
                }
                else if ( _nearPt == np - 1 )  {
                    _numVerts = 2;
                    _dcX = _dcX + np - 2;
                    _dcY = _dcY + np - 2;
                }
                else { 
	            _numVerts = 3;
                    _dcX = _dcX + _nearPt - 1;
                    _dcY = _dcY + _nearPt - 1;
                }
            }
            else
	        _numVerts = 1;
	}
	else {			/* smoothed lines */

	    xy_idx = 0;

	    if (closed && np < 6) { 		/* closed & np < 6 */

/*
 *  Ghost the whole figure, back up 1 from _nearPt to start
 */
		xy_idx = pgutls_nextIndex (np, _nearPt, PREV);
		_activePt = 1;
		_numVerts = np + 1;
	    }
	    else if (closed) {			/* all other closed figs */
		_numVerts = 7;
		_activePt = 3;
		xy_idx = _nearPt;

/*
 *  Back up 3 vertices
 */
		for (ii=0; ii<3; ii++) {
	    	    xy_idx = pgutls_nextIndex(np, xy_idx, PREV);
		}
	    }
	    else if (np < 7) { 	        /* ghost all points if < 7 total */
		_activePt = _nearPt;
		_numVerts = np;
	    }
	    else if (_nearPt == 0) { 		/* active is first pt */
		_activePt = 0;
		_numVerts = (np > 3)?  4 : np;
	    }
	    else if (_nearPt == np-1) {		/* active is last pt */
	        _numVerts = (np > 3)?  4 : np;
	        _activePt = _numVerts - 1;
		xy_idx = np - _numVerts;
	    }
	    else if (_nearPt == 1) {		/* active is 2nd pt */
		_numVerts = (np > 4)?  5 : np;
		_activePt = 1;
	    }
	    else if (_nearPt == np-2) {		/* active is 2nd to last pt */
		_numVerts = (np > 4)?  5 : np;
		_activePt = _numVerts - 2;
		xy_idx = np - _numVerts;
	    }
	    else if (_nearPt == 2) {		/* active is 3rd pt */
		_numVerts = (np > 7)?  6 : np;
		_activePt = 2;
	    }
	    else if (_nearPt == np-3) {		/* active is 3rd to last pt */
		_numVerts = (np > 7)?  6 : np;
		_activePt = _numVerts - 3;
		xy_idx = np - _numVerts;
	    }
	    else {				/* all other cases */
		_numVerts = 7;
		_activePt = 3;
		xy_idx = _nearPt - 3;
	    }

/*
 *  Load the _altX/Y arrays from _dcX & _dcY.  These are
 *  the points used to build the ghostlines.
 */
	    for (ii=0; ii <_numVerts; ii++, 
	    			xy_idx = pgutls_nextIndex(np, xy_idx, NEXT)) {
                _altX[ii] = *(_dcX + xy_idx);
                _altY[ii] = *(_dcY + xy_idx);
	    }

/*
 *  Reassign the _dcX/_dcY to point to the loaded verts
 */
            _dcX = &_altX[0];
            _dcY = &_altY[0];

 	}

	pggst_addGhostPts (_numVerts, _dcX, _dcY, &ier);

        mcanvw_setDragFunc((XtEventHandler)&pgmodv_vertexDrag, CURS_DEFAULT);

    }
    else if ( funcflg == 1 )  {

/*
 *  Watch box (parallelogram)
 */
	for ( ii = 0; ii < 8; ii++ )  {
	    _watchX[ii] = _dcX[ii];
	    _watchY[ii] = _dcY[ii];
	}
	_watchX[8] = _watchX[0];
	_watchY[8] = _watchY[0];
	_watchX[9] = ( _watchX[2] + _watchX[6] ) / 2.0F;
	_watchY[9] = ( _watchY[2] + _watchY[6] ) / 2.0F;
	_watchX[10] = _watchX[4];
	_watchY[10] = _watchY[4];

	pggst_addGhostPts (11, _watchX, _watchY, &ier);

        mcanvw_setDragFunc((XtEventHandler)&pgmodv_pgramDrag, CURS_DEFAULT);

    }

    if ( funcflg == 2 ) {
	pgrad_setCircPlActv (FALSE);
	if ( _nearPt == 0 ) {
	    pgcirc_setLocation (_dcX[0], _dcY[0], _dcX[1], _dcY[1]);
	    pgcirc_setGhostFlag (TRUE, pggst_setCircle);
	    pgcirc_updateGstCirc ();
	    pgmvcp_start(0, 0, 0, _dcX[0], _dcY[0], 0);
	}
	else {
	    pgrad_radiusStart(_dcX[0], _dcY[0]);
	}
    }
    else {
	mcanvw_setDropFunc((XtEventHandler)&pgmodv_vertexDrop, CURS_DEFAULT);
	pggst_drawGhost(GST_NORMAL);
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgmodv_vertexDrag ( Widget w, XtPointer clnt, 
							XEvent *event )
/************************************************************************
 * pgmodv_vertexDrag                                                    *
 *                                                                      *
 * Internal function for SELECT drag mode called by pgevt_selectHdl to  *
 * move individual vertices of lines, fronts and watches.		*
 *                                                                      *
 *                                                                      *
 * static void pgmodv_vertexDrag( w, clnt, event)                	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w          		Widget          Calling widget                  *
 *  clnt		XtPointer                                       *
 *  *event		XEvent                                          *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Wehner/Eai               Initial coding                          *
 *  D.W.Plummer/NCEP     9/97   Combine into NxmDraw for new vgstruct.h *
 *  E. Wehner/EAi               Remove graphics info record             *
 *  C. Lin/EAi          10/97   rename from NxmDrMvDragCb, cleanup      *
 *  E. Safford/GSC	05/98	moved to nmap_pgvmov, rewrite        	*
 *  E. Safford/GSC	07/98   change ghost of closed lines		*
 *  E. Safford/GSC	07/98   change ghost of smoothed lines		*
 *  S. Law/GSC		11/98	Changed current object device coords	*
 *				to be consistent with other functions	*
 *  E. Safford/GSC	10/99   update for new xwcmn.h        		*
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 ***********************************************************************/
{
    int		ier, xoff, yoff;
    float	xx, yy;
/*---------------------------------------------------------------------*/
/*
 * erase old ghost line
 */
    pggst_drawGhost(GST_NORMAL);

/*
 * Move the selected vertex the specified amount
 */
    xgtoff (&xoff, &yoff, &ier);
    xx = (float) (event->xbutton.x + xoff); 
    yy = (float) (event->xbutton.y + yoff); 

    pgactv_modPt (_nearPt, xx, yy);

    if (_closedFig && _smoothLvl <= 0) {
        _altX[1] = xx; 
        _altY[1] = yy;  
    }
    else if (_smoothLvl) {
        _altX[_activePt] = xx; 
        _altY[_activePt] = yy;  
    }

    pggst_replaceGhostPts (_numVerts, _dcX, _dcY, &ier);

/*
 * redraw the ghost line
 */
    pggst_drawGhost(GST_NORMAL);
}

/*=====================================================================*/
/* ARGSUSED */
static void pgmodv_pgramDrag ( Widget w, XtPointer clnt, 
							XEvent *event )
/************************************************************************
 * pgmodv_pgramDrag                                                     *
 *                                                                      *
 * Internal function for SELECT drag mode for parallelogram called by   *
 * pgevt_selectHdl.                                                     *
 *                                                                      *
 *                                                                      *
 * static void pgmodv_pgramDrag( w, clnt, event)                 	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w           	Widget          Calling widget                  *
 *  clnt		XtPointer                                       *
 *  *event       	XEvent                                          *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  D.W.Plummer/NCEP	 6/98						*
 *  E. Safford/GSC	07/98	changed ghosting			*
 *  D.W.Plummer/NCEP	11/98	Changes for editing pgram watch edges	*
 *  S. Law/GSC		11/98	moved pgwbxw_gpts -> pgwpts_get		*
 *  E. Safford/GSC	10/99   update for new xwcmn.h        		*
 *  M. Li/GSC		 1/00	Used string variables in gtrans		*
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 *  A. Hardy/GSC        11/00   renamed coordinate system declarations  *
 *  H. Zeng/XTRIA	01/03   modifed arguments to pgwbxw_getAttr     *
 *  H. Zeng/SAIC	10/04	added two para. for pgwbxw_getAttr	*
 ***********************************************************************/
{
    int		ii, nearest, np, wcolr, wstyle, wshape, ier;
    int		mtype, mwidth, cnty_colr, xoff, yoff;
    signed char	cnty_fill;
    float	rx[8], ry[8], rx2[8], ry2[8], tlat[8], tlon[8], msize;
/*---------------------------------------------------------------------*/
/*
 * Erase the ghost line
 */
    pggst_drawGhost(GST_NORMAL);

    for (ii = 0; ii < 8; ii++) {
	rx[ii] = _watchX[ii];
	ry[ii] = _watchY[ii];
    }

/*
 * get the point that causes the element to be selected
 */
    nearest = pgactv_getNearPt();

    xgtoff (&xoff, &yoff, &ier);
    rx[nearest] = (float) (event->xbutton.x + xoff); 
    ry[nearest] = (float) (event->xbutton.y + yoff); 

/*
 * get the new corner points
 */
    pgwbxw_getAttr(&wcolr, &wstyle, &wshape,
		   &mtype, &msize,  &mwidth,
		   &cnty_fill, &cnty_colr    );

    np = 8;

    gtrans (sys_D, sys_M, &np, rx, ry,  tlat,  tlon, &ier, 
            strlen(sys_D), strlen(sys_M));
    pgwpts_get(nearest, wshape, tlat, tlon, tlat, tlon, &ier);
    gtrans (sys_M, sys_D, &np,  tlat,  tlon,  rx2,  ry2, &ier,
            strlen(sys_M), strlen(sys_D));

/*
 * assign the new values
 */
    for (ii = 0; ii < 8; ii++) {
	_watchX[ii] = rx2[ii];
	_watchY[ii] = ry2[ii];
	pgactv_modPt( ii, _watchX[ii], _watchY[ii] );
    }
    _watchX[8] = _watchX[0];
    _watchY[8] = _watchY[0];
    _watchX[9] = ( _watchX[2] + _watchX[6] ) / 2.0F;
    _watchY[9] = ( _watchY[2] + _watchY[6] ) / 2.0F;
    _watchX[10] = _watchX[4];
    _watchY[10] = _watchY[4];

    pggst_replaceGhostPts (11, _watchX, _watchY, &ier);

/*
 * redraw the ghost line
 */
    pggst_drawGhost(GST_NORMAL);
}

/*=====================================================================*/

static void pgmodv_vertexDrop ( Widget wid, XtPointer clnt, 
							XEvent *event )
/************************************************************************
 * pgmodv_vertexDrop                                                    *
 *                                                                      *
 * Internal function for SELECT drop mode called by pgevt_selectHdl to  *
 * move individual vertices of lines, fronts, and watches.		*
 *                                                                      *
 *                                                                      *
 * static void pgmodv_vertexDrop (wid, clnt, event)              	*
 *                                                                      *
 * Input parameters:                                                    *
 *  wid         Widget          Calling widget                          *
 *  clnt	XtPointer                                               *
 *  *event      XEvent                                                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Wehner/Eai               Initial coding                          *
 *  D.W.Plummer/NCEP     9/97   Combine into NxmDraw for new vgstruct.h *
 *  E. Wehner/EAi        9/97   Remove graphics info record             *
 *  C. Lin/EAi          10/97   rename from NxmDrMvDropCb, cleanup      *
 *  C. Lin/EAi          10/97   add WBOX_ELEM related functions         *
 *  C. Lin/EAi          11/97   further cleanup                         *
 *  E. Safford/GSC      02/98   add _storedEl & _displayLoc for undo    *
 *  E. Safford/GSC      05/98   mod for new Undo routines               *
 *  E. Safford/GSC	05/98	moved to nmap_pgvmov                	*
 *  E. Safford/GSC	07/98	changed ghosting                    	*
 *  G. Krueger/EAI	08/98	Cleaned up ghosting			*
 *  S. Law/GSC		12/98	Added call to pgwbx_setWlst		*
 *  D.W.Plummer/NCEP	 4/99	remove call to pgwlst_update		*
 *  E. Safford/GSC	04/99	make call to setWlst conditional on el  *
 *  S. Law/GSC		05/99	added CLASS_TRACKS			*
 *  S. Law/GSC		06/99	set the snap flag for watches		*
 *  S. Law/GSC		07/99	added CLASS_SIGMETS			*
 *  S. Law/GSC		08/99	pgsigf -> pgsigw			*
 *  S. Law/GSC		02/00	Added CCF catch				*
 *  S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 *  M. Li/GSC		05/00	added pgsigw_setState			*
 *  M. Li/GSC		06/00	removed pgsigw_setState			*
 *  M. Li/GSC		10/00	insert anchor pt info into element	*
 *  E. Safford/GSC	11/00	wipe county list when resizing watches	*
 *  H. Zeng/EAI         11/00   changed for the new undo design         *
 *  A. Hardy/GSC        11/00   renamed coordinate system declarations  *
 *  H. Zeng/EAI         12/00   modified for multiple undo steps        *
 *  E. Safford/GSC	03/01	add mcanvw_disarmDrop()               	*
 *  H. Zeng/XTRIA	06/03   added check to county lock status	*
 *  J. Wu/SAIC		02/04	set "From Line" for GFA_ELM             *
 *  J. Wu/SAIC		10/04	free GFA block memory			*
 *  B. Yin/SAIC		02/05	add a call to snap			*
 *  E. Safford/SAIC	06/05	allow smear to get smaller on edit	*
 *  H. Zeng/SAIC	01/06	added call to pgwatch_clrNumCwas()	*
 *  S. Danz/AWC         08/06   New flag to pgvgf_saveNewElm to place el*
 *  E. Safford/SAIC	01/07	use pggfaw_reorderGFA() for GFA elements*
 *  S. Danz/AWC         02/07   Add logic to update GFA centroid 	*
 *  E. Safford/SAIC	06/07	handle a failure on save 		*
 *  B. Yin/SAIC		03/08	warn user FBBA states list may have 	*
 *				changed					*
 ***********************************************************************/
{
    int		location, ier, np, el_location, ii, areaType, one = 1;
    int		subtype;
    float	llx, lly, urx, ury, *dx, *dy, *tmpLat, *tmpLon;
    float	x_cntr, y_cntr, c_lat, c_lon, area;
    char	system[2], tagStr[32], subtypeStr[ 32 ];

    VG_DBStruct	el;
    Boolean	saveOk = False;
/*---------------------------------------------------------------------*/

    el_location = pgactv_getElmLoc();
    pgutls_prepNew (el_location, &el, &llx, &lly, &urx, &ury, &ier);

    if (el.hdr.vg_type == WBOX_ELM) {
	pgwpts_setSnap (TRUE);

	pgmodv_pgramDrag (wid, clnt, event);

	pgwpts_setSnap (FALSE);
    }

    pggst_clearGhost(TRUE);

    mcanvw_disarmDrag();
    mcanvw_disarmDrop();

    pgactv_getDevPts (&np, &dx, &dy);

    strcpy (system, sys_D);

    if (el.hdr.vg_class == CLASS_TRACKS && 
	_nearPt < el.elem.trk.info.nipts) {

	np = el.elem.trk.info.nipts;

	gtrans(sys_D, sys_M, &np, dx, dy, 
	       &(el.elem.trk.latlon[0]), &(el.elem.trk.latlon[np]), &ier,
	       strlen(sys_D), strlen(sys_M));

	pgtrkw_extrapolate (&(el.elem.trk.latlon[0]), 
			    &(el.elem.trk.latlon[np]), &el);

	strcpy (system, sys_M);

	np = 0;
    }
    else if (el.hdr.vg_class == CLASS_SIGMETS && el.hdr.vg_type != SIGCCF_ELM) {

	np = el.elem.sig.info.npts;

	gtrans(sys_D, sys_M, &np, dx, dy, 
	       &(el.elem.sig.latlon[0]), &(el.elem.sig.latlon[np]), &ier,
	       strlen(sys_D), strlen(sys_M));

	pgsigw_setFrom (np, &(el.elem.sig.latlon[0]), 
			&(el.elem.sig.latlon[np]));

    }
    else if ( el.hdr.vg_type == GFA_ELM ) {

        subtypeStr[ 0 ]= '\0';
        cvg_getFld ( &el, TAG_GFA_SUBTYPE, subtypeStr, &ier );
                                                                              
        subtype = atoi( subtypeStr ) % 10;
                                                                               
        /*
         *  Activate the states button for FBBA to warn the user
	 *  the states list may have changed.
        */
        if ( subtype == GFA_FBBA_AIRMET || 
             subtype == GFA_FBBA_OUTLOOK )  {

	   pggfawp_setStatesBtn( True );
	}

	np = el.elem.gfa.info.npts;

	G_MALLOC( tmpLat, float, np, "PGMODV_VERTEXDROP: tmpLat" );
	G_MALLOC( tmpLon, float, np, "PGMODV_VERTEXDROP: tmpLon" );

	gtrans(sys_D, sys_M, &np, dx, dy, tmpLat, tmpLon,
	       &ier, strlen(sys_D), strlen(sys_M));

	cvg_getFld ( &el, TAG_GFA_AREATYPE, tagStr, &ier );
        areaType =  pggfaw_getHazardType ( tagStr );

        pggfaw_reorderGFA ( areaType, np, tmpLat, tmpLon, &ier );

        for (ii = 0; ii < np; ii++) {
            el.elem.gfa.latlon[ii]    = tmpLat[ii];
            el.elem.gfa.latlon[ii+np] = tmpLon[ii];
        }    

	G_FREE( tmpLat, float );            
	G_FREE( tmpLon, float );            

	pgsmear_snapEl ( FALSE, &el, &ier );

	np = el.elem.gfa.info.npts;

	cvg_todev( &el, &np, dx, dy, &ier );

	pggfaw_setFrom (np, &(el.elem.gfa.latlon[0]), 
			&(el.elem.gfa.latlon[np]));

	if ( pggfaw_isClosed() ) {
	    cgr_centroid( dx, dy, &np, &x_cntr, &y_cntr, &area, &ier );
	} else {
	    x_cntr = dx[ 0 ];
	    y_cntr = dy[ 0 ];
	}
	gtrans( sys_D, sys_M, &one, &x_cntr, &y_cntr, &c_lat, &c_lon, &ier, 
		strlen(sys_D), strlen(sys_M) 
		);

	sprintf ( tagStr, "%7.2f", c_lat );
	cvg_setFld ( &el, TAG_GFA_ARROW_LAT, tagStr, &ier );
	sprintf ( tagStr, "%7.2f", c_lon );
	cvg_setFld ( &el, TAG_GFA_ARROW_LON, tagStr, &ier );

    }
    else if (el.hdr.vg_type == WBOX_ELM) {

	pgwbxw_getAnchor ( 0, el.elem.wbx.info.w_a0id, 
			&el.elem.wbx.info.w_a0lt, &el.elem.wbx.info.w_a0ln, 
			&el.elem.wbx.info.w_a0dis, el.elem.wbx.info.w_a0dir, 
			&ier );

	pgwbxw_getAnchor ( 1, el.elem.wbx.info.w_a1id, 
			&el.elem.wbx.info.w_a1lt, &el.elem.wbx.info.w_a1ln, 
			&el.elem.wbx.info.w_a1dis, el.elem.wbx.info.w_a1dir, 
			&ier );

/*
 *  If county lock status if FALSE, wipe the county list.
 */
        if ( !pgwlst_getCtlkStatus() ) {

	   el.elem.wbx.info.numcnty = 0;
        }
    }

/*
 *  Check the return code from pgvgf_saveNewElm.  If it worked ok then
 *  store the new element as an undo step.  
 *
 *  If the write failed then undelete the original element.  The most 
 *  likely cause of a write failure is a GFA element that results in too 
 *  few points (because of snapping vertices to the same point).
 */
    pgvgf_saveNewElm (NULL, system, &el, np, dx, dy, TRUE, &location, &ier);

    if( ier >= 0 ) {
        pgundo_newStep();
        pgundo_storeThisLoc(el_location, UNDO_DEL, &ier);
	saveOk = True;
        pgundo_storeThisLoc (location, UNDO_ADD, &ier);
        pgundo_endStep();
    }
    else {
	cvg_undel( cvg_getworkfile(), el_location, TRUE, &ier );
    }

/*
 * Free TCA/GFA memory
 */
    if ( el.hdr.vg_type == TCA_ELM ) {
        cvg_freeBkpts ( &el );
    }
    else if ( el.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &el );
    }
    
/*
 * REDISPLAY -- Redisplay the newly created/stored element
 *              by reading the record, setting the range, and displaying
 *
 *           -- or redisplay the original element in the event of a 
 *              failure to create a new element.
 */
    if( saveOk ) {
        pgutls_redraw(location, &el, &ier);
        pgactv_setActvElm (&el, location);
    }
    else {
        pgutls_redraw(el_location, &el, &ier);
	pgactv_setActvElm( &el, el_location );
    }

    if (el.hdr.vg_type == WBOX_ELM) {
        pgwatch_clrNumCwas ();
        pgwbxw_setWlst (location, FALSE);
    }

/*
 * Free TCA/GFA memory
 */
    if ( el.hdr.vg_type == TCA_ELM ) {
        cvg_freeBkpts ( &el );
    }
    else if ( el.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &el );
    }
}
