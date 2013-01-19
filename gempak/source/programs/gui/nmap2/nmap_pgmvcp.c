#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "hints.h"
#include "proto_xw.h"


#define	 EXTRA	5.0F
#define	 DRAG_COUNT     3	

static Boolean  _midDrag = FALSE;
static int	_dragCount;
static int	_nearPt;

static float    _dragX, _dragY;
static float    _origX, _origY;
static float    _goffX, _goffY;

static float    *_dcX, *_dcY;
static int	_dcN;
static Boolean  _closedFig;

static float	_gpcX[5], _gpcY[5];
static int	_smoothLvl;
static int	_wboxElm;

/*
 *  Private functions
 */
static void _pgmvcp_elDragEh    ( Widget, XtPointer, XEvent*, Boolean* );
static void _pgmvcp_elDropEh    ( Widget, XtPointer, XEvent*, Boolean* );
static void _pgmvcp_groupDragEh ( Widget, XtPointer, XEvent*, Boolean* );
static void _pgmvcp_groupDropEh ( Widget, XtPointer, XEvent*, Boolean* );
static void _pgmvcp_wboxCalc  ( void );
static void _pgmvcp_jetCalc ( VG_DBStruct *el, float dx, float dy, Boolean grp );
static void _pgmvcp_ccfCalc ( VG_DBStruct *el, float dx, float dy, Boolean grp );
static void _pgmvcp_gfaCalc ( VG_DBStruct *el, float dx, float dy, Boolean grp );


/************************************************************************
 * nmap_pgmvcp.c                                                        *
 *                                                                      *
 * This module contains the callback functions for move and copy	*
 * operations.								*
 *                                                                      *
 * CONTENTS:                                                            *
 * pgmvcp_start()         'MOVE' or 'COPY' state -- start (mouse press) *
 *                                                                      *
 * _pgmvcp_elDragEh()     'MOVE' or 'COPY' state -- single element drag*
 * _pgmvcp_groupDragEh()  'MOVE' or 'COPY' state -- group drag         *
 * _pgmvcp_elDropEh()     'MOVE' or 'COPY' state -- element drop       *
 * _pgmvcp_groupDropEh()  'MOVE' or 'COPY' state -- group drop         *
 * _pgmvcp_wboxCalc()	   MOVE/COPY watch box point/anchor calculation	*
 * _pgmvcp_jetCalc()       MOVE/COPY jet barb/hash position calculation	*
 * _pgmvcp_gfaCalc()       MOVE/COPY gfa attr. box position calculation	*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void pgmvcp_start ( char grptyp, int grpnum, char closed, float xx, 
						float yy, int smooth )
/************************************************************************
 * pgmvcp_start                                                         *
 *                                                                      *
 * Function for starting MOVE/COPY drag mode called by pgevt_mvcpHdl.   *
 *                                                                      *
 * void pgmvcp_start ( grptyp, grpnum, closed, xx, yy, smooth )         *
 *                                                                      *
 * Input parameters:                                                    *
 * 	grptyp		char	group type of selected element		*
 *	grpnum		int	group number of selected element	*
 *      closed          char    figure close flag                       *
 *	xx		float	x coordinate of cursor			*
 *	yy		float	y coordinate of cursor			*
 *	smooth		int	smoothing level				*
 *									*
 * Output parameters:                                                   *
 *	none								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Wehner/Eai               Initial coding                          *
 *  C. Lin/EAi          10/97   rename from NxmDrawSlide2Cb, cleanup    *
 *  C. Lin/EAi          11/97   reset the hot point when necessary      *
 *  E. Safford/GSC      03/98   add pgsymb_popdown                      *
 *  E. Safford/GSC      04/98   remove check on class                   *
 *  S. Law/GSC          04/98   added copy function                     *
 *  S. Law/GSC          05/98   cleaned up drag, added group box        *
 *  S. Law/GSC          05/98   replaced pgpalw_mvcp with _setupOper    *
 *  S. Law/GSC          05/98   added call to pgevt_unsetOper           *
 *  E. Safford/GSC      05/98   move to nmap_pgmvcp.c                   *
 *  G. Krueger/EAI      06/98   Uniform status hints                    *
 *  E. Safford/GSC      07/98   modify ghost of closed figures          *
 *  E. Safford/GSC      07/98   modify ghost of smoothed figures        *
 *  C. Lin/EAi          08/98   add _dragCount to improve jiggle effect *
 *  G. Krueger/EAI	09/98	Added ghost veiling			*
 *  E. Safford/GSC      09/98   modify to handle mode (GRP/OBJ)         *
 *  G. Krueger/EAI	10/98	Using table for hints			*
 *  W. Li/EAI		01/99	NxmTxtA_XXX --> pgtxt_XXX		*
 *  G. Krueger/EAI	05/99	Added circle draw function		*
 *  G. Krueger/EAI	06/99	Simplified circle move			*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 *  E. Safford/GSC	02/01	add check on cvg_rdsel return code	*
 *  E. Safford/GSC	02/01	add param to cvg_rdsel               	*
 *  J. Wu/SAIC		10/04	free GFA block memory               	*
 ***********************************************************************/
{
    int         selected, nearest, ier;
    float       llx, lly, urx, ury, dummy;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    _midDrag = FALSE;
    _nearPt = pgactv_getNearPt();  
    
    pggst_veilGhost (FALSE);
    if (grptyp && grpnum && 
       		(grptyp == GRPTYP_COMSYM || pgpalw_getMode() == TYPE_GRP)) {
        crg_ggbnd(grptyp, grpnum, &llx, &urx, &ury, &lly, &ier);
        _dragX = _origX = llx;
        _dragY = _origY = lly;
        _goffX = xx - llx;
        _goffY = yy - lly;

	_gpcX[0] = llx - _goffX;
        _gpcY[0] = lly - _goffY;
        _gpcX[1] = urx - _goffX;
        _gpcY[1] = lly - _goffY;
        _gpcX[2] = urx - _goffX;
        _gpcY[2] = ury - _goffY;
        _gpcX[3] = llx - _goffX;
        _gpcY[3] = ury - _goffY;
        _gpcX[4] = llx - _goffX;
        _gpcY[4] = lly - _goffY;

        mcanvw_setDragFunc((XtEventHandler)&_pgmvcp_groupDragEh, CURS_DEFAULT);
        mcanvw_setDropFunc((XtEventHandler)&_pgmvcp_groupDropEh, CURS_DEFAULT);
    }
    else { 
        _closedFig =  (closed)?  TRUE : FALSE; 

        pgactv_getDevPts (&_dcN, &_dcX, &_dcY);
  	selected = pgactv_getElmLoc();

	if (selected != -1) {
	    cvg_rdsel(NULL, selected, xx, yy, &nearest, &dummy, &el, &ier);
	    
/*
 * Free TCA/GFA memory
 */
            if ( el.hdr.vg_type == TCA_ELM ) {
                cvg_freeBkpts ( &el );
            }
            else if ( el.hdr.vg_type == GFA_ELM ) {
                cvg_freeElPtr ( &el );
            }

	    if ( ier != 0 ) {
		return;
	    }

	    if ( el.hdr.vg_type == CIRCLE_ELM ) _nearPt = 0;
	    _wboxElm = ( el.hdr.vg_type == WBOX_ELM );
	}

        _dragX = _origX = *(_dcX + _nearPt);
        _dragY = _origY = *(_dcY + _nearPt);
        _goffX = 0.0F;
        _goffY = 0.0F;
        mcanvw_setDragFunc((XtEventHandler)&_pgmvcp_elDragEh, CURS_DEFAULT);
        mcanvw_setDropFunc((XtEventHandler)&_pgmvcp_elDropEh, CURS_DEFAULT);
    } 

    _dragCount = 0;
    _smoothLvl = smooth;

    pgtxt_setGhostFlag (FALSE, NULL);

    mbotw_mouseSet(LMHINT_DRAG, MMHINT_DONE);
}

/*=====================================================================*/
/* ARGSUSED */
static void _pgmvcp_elDragEh ( Widget w, XtPointer clnt, XEvent *event,
							Boolean *ctdr )
/************************************************************************
 * _pgmvcp_elDragEh                                                     *
 *                                                                      *
 * Internal function for MOVE/COPY drag mode  for single elements       *
 * called by pgevt_mvcpHdl.  						*
 *                                                                      *
 * static void _pgmvcp_elDragEh (w, clnt, event, ctdr)              	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget          Calling widget                          *
 *  clnt	XtPointer                                               *
 *  *event      XEvent 	                                                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Wehner/Eai               Initial coding                          *
 *  C. Lin/EAi          10/97   rename from NxmDrSlDragCb, cleanup      *
 *  S. Law/GSC          04/98   added copy function                     *
 *  S. Law/GSC          05/98   cleaned up drag, added group box        *
 *  E. Safford/GSC      05/98   move to nmap_pgmvcp.c, renamed          *
 *  G. Krueger/EAI      06/98   Uniform status hints                    *
 *  E. Safford/GSC	07/98	mod to ghost smoothed lines		*
 *  E. Safford/GSC	08/98	clean up ghost line setup		*
 *  C. Lin/EAI		08/98	add _dragCount to improve jiggling effe.*
 *  G. Krueger/EAI	10/98	Using table for hints			*
 *  G. Krueger/EAI	05/99	Added circle draw function		*
 *  G. Krueger/EAI	06/99	Simplified circle move			*
 *  E. Safford/GSC	10/99	update for new xwcmn.h			*
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 ***********************************************************************/
{
    int           ii, ier, xoff, yoff;
    float         delx, dely;
    Boolean	  reset_attr;
/*---------------------------------------------------------------------*/

    if ( _dragCount < DRAG_COUNT ) {
	_dragCount++;
	return;
    }

/*
 * Erase the ghost line
 */
    if (_midDrag) {
	pggst_drawGhost(GST_NORMAL);
    }
    else { 
/*
 * first time get into dragging mode
 */
        _midDrag = TRUE;
	reset_attr = TRUE;

        pggst_clearGhost(reset_attr); 
        pggst_setLineAttr (_smoothLvl, _closedFig); 

        pggst_addGhostPts ( _dcN, _dcX,  _dcY, &ier);
    }

/*
 * set "delta" amounts...
 */
    xgtoff (&xoff, &yoff, &ier);
    delx = (float)event->xbutton.x + (float)xoff - _dragX;
    dely = (float)event->xbutton.y + (float)yoff - _dragY;
  
    _dragX += delx;
    _dragY += dely;
  
/*
 * Move the vertices the specified amount
 */
    if ( _wboxElm )  {
	pgwpts_setSnap (FALSE);
	_pgmvcp_wboxCalc ( );
    }

    for (ii = 0; ii< _dcN; ii++) {
        pgactv_modPt (ii, *(_dcX +ii) + delx, *(_dcY + ii) + dely);
    }

/*
 *  Rebuild the ghostpoint arrays
 */ 
    pggst_replaceGhostPts ( _dcN, _dcX,  _dcY, &ier);

/*
 * redraw the ghost line
 */
    pggst_drawGhost(GST_NORMAL);

    mbotw_mouseSet(LMHINT_PUT, MMHINT_DONE);
}

/*=====================================================================*/
/* ARGSUSED */
static void _pgmvcp_groupDragEh ( Widget w, XtPointer clnt, XEvent *event,
						Boolean *ctdr )
/************************************************************************
 * _pgmvcp_groupDragEh                                                  *
 *                                                                      *
 * Internal function for MOVE/COPY drag mode for groups.                *
 *                                                                      *
 * static void _pgmvcp_groupDragEh (w, clnt, event, ctdr)		*
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget          Calling widget                          *
 *  clnt	XtPointer                                               *
 *  *event      XEvent 		                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC      05/98   copied from _pgmvcp_elDragEh            *
 *  G. Krueger/EAI	06/98	Uniform status hints			*
 *  E. Safford/GSC      08/98   mod ghosting and clean up               *
 *  E. Safford/GSC	08/98	fix ghost line problem			*
 *  C. Lin/EAI		08/98	add _dragCount to improve jiggling effe.*
 *  G. Krueger/EAI	10/98	Using table for hints			*
 *  E. Safford/GSC	10/99   update for new xwcmn.h        		*
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 ***********************************************************************/
{
    int		ii, np, ier, xoff, yoff;
    float	delx, dely;
    Boolean	reset_attr;
/*---------------------------------------------------------------------*/

    if ( _dragCount < DRAG_COUNT ) {
        _dragCount++;
        return;
    }   

/*
 * Erase the ghost line
 */
    np = 5;
    if (_midDrag) {
	pggst_drawGhost (GST_NORMAL);
    }
    else {

/*
 * first time into drag mode...
 */
        _midDrag = TRUE;
	reset_attr = TRUE;
        pggst_clearGhost(reset_attr); 
	pggst_addGhostPts ( np, _gpcX, _gpcY, &ier);
    }

/*
 * set "delta" amounts...
 */
    xgtoff (&xoff, &yoff, &ier);
    delx = (float)event->xbutton.x + (float)xoff - _dragX;
    dely = (float)event->xbutton.y + (float)yoff - _dragY;
  
    _dragX += delx;
    _dragY += dely;
  
/*
 * Move the vertices the specified amount
 */
    for (ii = 0; ii< np; ii++) {
	_gpcX[ii] = _gpcX[ii] + delx;
	_gpcY[ii] = _gpcY[ii] + dely;
    }
 
    pggst_replaceGhostPts ( np, _gpcX, _gpcY, &ier);

/*
 * redraw the ghost line
 */
    pggst_drawGhost (GST_NORMAL);

    mbotw_mouseSet(LMHINT_PUT, MMHINT_DONE);

}

/*=====================================================================*/
/* ARGSUSED */
static void _pgmvcp_elDropEh ( Widget w, XtPointer clnt, XEvent *event,
							Boolean *ctdr )
/************************************************************************
 * _pgmvcp_elDropEh                                                     *
 *                                                                      *
 * This function is the callback for a drop on a selected element.      *
 *                                                                      *
 * static void _pgmvcp_elDropEh (w, clnt, event, ctdr)			*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt		XtPointer       State information record        *
 *      *event          XEvent          Button press event record       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       06/97   Modified to handle Special Text         *
 * E. Wehner/EAi        07/97   Remove offsets when replacing text.     *
 * E. Safford/GSC       07/97   Fixed drag with special text problem    *
 * E. Wehner/EAi        08/97   Remove watch box slide                  *
 * C. Lin/EAI            8/97   Add offsets for 'S' coord(roam)         *
 * D.W.Plummer/NCEP      9/97   Combine into NxmDraw for new vgstruct.h *
 * E. Wehner/EAi         9/97   Remove graphics info record             *
 * C. Lin/EAi           10/97   rename from NxmDrSlDropCb, cleanup      *
 * C. Lin/EAi           10/97   add WBOX_ELEM related functions         *
 * C. Lin/EAi           11/97   further cleanup                         *
 * E. Safford/GSC       02/98   add _storedEl for undo function         *
 * S. Law/GSC           04/98   added copy function                     *
 * E. Safford/GSC       04/98   added FUNC_SELECT to FUNC_MOVE ops      *
 * S. Law/GSC           05/98   cleaned up drag, added group box        *
 * E. Safford/GSC       05/98   mod for new undo routines               *
 * E. Safford/GSC       05/98   move to nmap_pgmvcp.c                   *
 * E. Safford/GSC       06/98   split from mvcpDrop.c                   *
 * G. Krueger/EAI       06/98   Uniform status hints                    *
 * E. Safford/GSC       07/98   reset _dcN for closed figures           *
 * C. Lin/EAI       	08/98   fix ghosting problem & reset _dragCount *
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * E. Safford/GSC	12/98	modify refresh to limit area affected	*
 * D.W.Plummer/NCEP	 4/99	remove call to pgwlst_update		*
 * E. Safford/GSC	11/00	wipe the county list for watches     	*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters          *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * J. Wu/SAIC		12/01	add layer in crg_set() call		*
 * J. Wu/SAIC		01/02	add layer in crg_get() call		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		11/03	adjust jet barb/hash position		*
 * J. Wu/SAIC		02/04	adjust gfa attribute box position	*
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 * J. Wu/SAIC		07/04	free GFA block memory			*
 * B. Yin/SAIC		02/05	add a call to snap for GFA		*
 * E. Safford/SAIC	06/05	allow smear to get smaller on edit	*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 * S. Danz/AWC          02/07   Add logic to update GFA centroid 	*
 * L. Hinson/AWC        07/09   Add code to update CCF centroid         *
 ***********************************************************************/
{
int         location, ier, currfunc, new_location, num, layer, el_layer;
int         found, update_crg, one = 1;
float       llx, lly, urx, ury; 
float       x_cntr, y_cntr, c_lat, c_lon, area;
float       o_llx, o_lly, o_urx, o_ury, inf_bbox[4];
char	    value[32];
VG_DBStruct el, del_el;
filter_t    filter;
/*---------------------------------------------------------------------*/

    _dragCount = 0;
    mcanvw_disarmDrag();
    mcanvw_disarmDrop();

    if ( _wboxElm )  {
	pgwpts_setSnap (TRUE);
	_pgmvcp_wboxCalc ( );
    }

    pggst_clearGhost(TRUE);
    if (!_midDrag)
	return;

    _midDrag = FALSE;

    update_crg = 0;

    currfunc = pgpalw_getCurOperId();

    pgundo_newStep();
    location = pgactv_getElmLoc();
    cvg_rdrec(cvg_getworkfile(), location, &el, &ier);

    crg_getinx (location, &num, &ier);
    crg_get (num, &el_layer, filter, &o_llx, &o_lly, &o_urx, &o_ury, &ier);
    
    pghdlb_deselectEl (location, FALSE); 

    if ((currfunc == FUNC_MOVE) || (currfunc == FUNC_SELECT)) {

/*
 * Mark elements in placement that are effected by
 * the delete, and get the area of influence back
 */
        cvg_rdrec(cvg_getworkfile(), location, &del_el, &ier);
        cvg_checkplace(&del_el, 1, location, &found, inf_bbox, &ier);
        if (found > 0) {

/*
 * Update the refresh extent if the area impacted by
 * placement was bigger than the area passed in
 */
            o_llx = G_MIN(o_llx, inf_bbox[0]);
            o_lly = G_MIN(o_lly, inf_bbox[2]);
            o_urx = G_MAX(o_urx, inf_bbox[1]);
            o_ury = G_MAX(o_ury, inf_bbox[3]);
            update_crg = 1;
        }

/*
 * Free TCA/GFA memory
 */
        if ( del_el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &del_el );
        }
        else if ( del_el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &del_el );
        }

/* 
 *  delete old element
 */  
	cvg_delet (cvg_getworkfile(), location, TRUE, &ier);
	crg_clear (num, &ier);
        pgundo_storeThisLoc(location, UNDO_DEL, &ier);
    }

    if ( el.hdr.vg_type == WBOX_ELM )  {

        pgwbxw_getAnchor ( 0, el.elem.wbx.info.w_a0id,
                        &el.elem.wbx.info.w_a0lt, &el.elem.wbx.info.w_a0ln,
                        &el.elem.wbx.info.w_a0dis, el.elem.wbx.info.w_a0dir,
                        &ier );

        pgwbxw_getAnchor ( 1, el.elem.wbx.info.w_a1id,
                        &el.elem.wbx.info.w_a1lt, &el.elem.wbx.info.w_a1ln,
                        &el.elem.wbx.info.w_a1dis, el.elem.wbx.info.w_a1dir,
                        &ier );

/*
 *  Wipe the county list
 */
 	el.elem.wbx.info.numcnty = 0;
    }

/*
 *  adjust jet barb/hash position accordingly
 */ 
    if ( el.hdr.vg_type == JET_ELM )  {
        _pgmvcp_jetCalc ( &el, 0, 0, False );
    }
     
    if ( el.hdr.vg_type == SIGCCF_ELM ) {
      _pgmvcp_ccfCalc ( &el, 0, 0, False );
      gtrans ( sys_D, sys_M, &_dcN, _dcX, _dcY, 
		 &(el.elem.ccf.latlon[0]), &(el.elem.ccf.latlon[_dcN]), &ier,
		 strlen(sys_D), strlen(sys_M) );
      _dcN = el.elem.ccf.info.npts;
      cvg_todev ( &el, &_dcN, _dcX, _dcY, &ier );
      
      if ( el.hdr.closed ) {
        cgr_centroid ( _dcX, _dcY, &_dcN, &x_cntr, &y_cntr, &area, &ier );
      } else {
        x_cntr = _dcX[0] ;
        y_cntr = _dcY[0] ;
      }
      gtrans( sys_D, sys_M, &one, &x_cntr, &y_cntr, &c_lat, &c_lon, &ier, 
	    strlen(sys_D), strlen(sys_M) 
	    );
      el.elem.ccf.info.arrowlat = c_lat;
      el.elem.ccf.info.arrowlon = c_lon;
    }
/*
 *  adjust GFA attribute box position accordingly
 */ 
    if ( el.hdr.vg_type == GFA_ELM ) {

        _pgmvcp_gfaCalc ( &el, 0, 0, False );

	gtrans ( sys_D, sys_M, &_dcN, _dcX, _dcY, 
		 &(el.elem.gfa.latlon[0]), &(el.elem.gfa.latlon[_dcN]), &ier,
		 strlen(sys_D), strlen(sys_M) );

	pgsmear_snapEl ( FALSE, &el, &ier );

	_dcN = el.elem.gfa.info.npts;

	cvg_todev( &el, &_dcN, _dcX, _dcY, &ier );

	if ( pggfaw_isClosed() ) {
	    cgr_centroid( _dcX, _dcY, &_dcN, &x_cntr, &y_cntr, &area, &ier );
	} else {
	    x_cntr = _dcX[ 0 ];
	    y_cntr = _dcY[ 0 ];
	}
	gtrans( sys_D, sys_M, &one, &x_cntr, &y_cntr, &c_lat, &c_lon, &ier, 
	    strlen(sys_D), strlen(sys_M) 
	    );

	sprintf ( value, "%7.2f", c_lat );
	cvg_setFld ( &el, TAG_GFA_ARROW_LAT, value, &ier );
	sprintf ( value, "%7.2f", c_lon );
	cvg_setFld ( &el, TAG_GFA_ARROW_LON, value, &ier );
    }
    
/*
 *  save new element
 */
    pgvgf_saveNewElm(NULL, sys_D, &el, _dcN, _dcX, _dcY, FALSE, 
    		&new_location, &ier);
    pgundo_storeThisLoc (new_location, UNDO_ADD, &ier);
    pgundo_endStep();
    
/*
 * Free TCA/GFA memory
 */
    if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
    }
    else if ( el.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &el );
    }

    cvg_rdrec(cvg_getworkfile(), new_location, &el, &ier);
    layer = pglayer_getCurLayer( );
    crg_set (&el, new_location, layer, &ier);
    pgactv_setActvElm (&el, new_location);

    crg_getinx (new_location, &num, &ier);
    crg_get(num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);

    if (o_llx < llx)
        llx = o_llx;
    if (o_lly < lly)
        lly = o_lly;
    if (o_urx > urx)
        urx = o_urx;
    if (o_ury > ury)
        ury = o_ury;

/*
 * Mark elements in placement that are effected by
 * the new element, and get the area of influence back
 */
    cvg_checkplace(&el, 0, new_location, &found, inf_bbox, &ier);
    if (found > 0) {

/*
 * Update the refresh extent if the area impacted by
 * placement was bigger than the area passed in
 */
        llx = G_MIN(llx, inf_bbox[0]);
        lly = G_MIN(lly, inf_bbox[2]);
        urx = G_MAX(urx, inf_bbox[1]);
        ury = G_MAX(ury, inf_bbox[3]);
        update_crg = 1;
    }

    xpgpaste (llx, lly, urx, ury, &ier);
    cvg_rfrsh (NULL, llx, lly, urx, ury, &ier);

/*
 * If we may have impacted other elements with placement
 * we will need to rebuild the range records
 */
    if (update_crg) {
        crg_rebuild();
    }

    pghdlb_select (&el, new_location);

/*
 * Free TCA/GFA memory
 */
    if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
    }
    else if ( el.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &el );
    }

    mbotw_mouseSet(LMHINT_DRAG, MMHINT_DONE);
}

/*=====================================================================*/
/* ARGSUSED */
static void _pgmvcp_groupDropEh ( Widget w, XtPointer clnt, 
					XEvent *event, Boolean *ctdr )
/************************************************************************
 * _pgmvcp_groupDropEh                                                  *
 *                                                                      *
 * This function is the callback for a drop on a group.                 *
 *                                                                      *
 * static void _pgmvcp_groupDropEh (w, clnt, event, ctdr)		*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt		XtPointer       State information record        *
 *      *event          XEvent          Button press event record       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       06/97   Modified to handle Special Text         *
 * E. Wehner/EAi        07/97   Remove offsets when replacing text.     *
 * E. Safford/GSC       07/97   Fixed drag with special text problem    *
 * E. Wehner/EAi        08/97   Remove watch box slide                  *
 * C. Lin/EAI            8/97   Add offsets for 'S' coord(roam)         *
 * D.W.Plummer/NCEP      9/97   Combine into NxmDraw for new vgstruct.h *
 * E. Wehner/EAi         9/97   Remove graphics info record             *
 * C. Lin/EAi           10/97   rename from NxmDrSlDropCb, cleanup      *
 * C. Lin/EAi           10/97   add WBOX_ELEM related functions         *
 * C. Lin/EAi           11/97   further cleanup                         *
 * E. Safford/GSC       02/98   add _storedEl for undo function         *
 * S. Law/GSC           04/98   added copy function                     *
 * E. Safford/GSC       04/98   added FUNC_SELECT to FUNC_MOVE ops      *
 * S. Law/GSC           05/98   cleaned up drag, added group box        *
 * E. Safford/GSC       05/98   mod for new undo routines               *
 * E. Safford/GSC       05/98   move to nmap_pgmvcp.c                   *
 * E. Safford/GSC       06/98   split from mvcpDrop.c                   *
 * E. Safford/GSC       06/98   added call to cgr_grfrsh.c              *
 * G. Krueger/EAI       06/98   Uniform status hints                    *
 * C. Lin/EAI       	08/98   fix ghosting problem & reset _dragCount *
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * E. Safford/GSC	12/98	modify refresh to limit area affected	*
 * D.W.Plummer/NCEP	 4/99	remove call to pgwlst_update		*
 * E. Safford/GSC	10/99   update for new xwcmn.h        		*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters          *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * J. Wu/SAIC		12/01	add layer in crg_set() call		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		11/03	adjust jet barb/hash position		*
 * J. Wu/SAIC		02/04	adjust gfa attribute box position	*
 * J. Wu/SAIC		10/04	free GFA block memory			*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 ***********************************************************************/
{
    int		location, ier, nelm, ii, jj, *inxarry, layer, update_crg;
    int		currfunc, newnum, old_location, xoff, yoff, found;
    float	llx, lly, urx, ury, delx, dely;
    float	o_llx, o_lly, o_urx, o_ury, inf_bbox[4];
    char	newtyp;
    VG_DBStruct	el, del_el;
/*---------------------------------------------------------------------*/

    _dragCount = 0;
    mcanvw_disarmDrag();
    mcanvw_disarmDrop();

    pggst_clearGhost(TRUE);
    if (!_midDrag)
	return;

    _midDrag = FALSE;

    update_crg = 0;

    currfunc = pgpalw_getCurOperId();

    old_location = pgactv_getElmLoc();

    cvg_rdrec(cvg_getworkfile(), old_location, &el, &ier);
    crg_ggnel(el.hdr.grptyp, el.hdr.grpnum, &nelm, &ier);

    if (nelm <= 0)
        return;

    inxarry = (int *)malloc(nelm*sizeof(int));
    crg_gginx (el.hdr.grptyp, el.hdr.grpnum, nelm, inxarry, &nelm, &ier);

    newtyp = el.hdr.grptyp;
    newnum = el.hdr.grpnum;
    crg_ggbnd (newtyp, newnum, &o_llx, &o_urx, &o_ury, &o_lly, &ier);

    if (currfunc == FUNC_COPY)
        crg_ggnxt (el.hdr.grptyp, &newnum, &ier);

/*
 * set "delta" amounts...
 */
    xgtoff (&xoff, &yoff, &ier);
    delx = (float)event->xbutton.x + (float)xoff - _dragX;
    dely = (float)event->xbutton.y + (float)yoff - _dragY;

    _dragX += delx;
    _dragY += dely;
    delx = _dragX - _origX - _goffX;
    dely = _dragY - _origY - _goffY;

    pghdlb_deselectEl (old_location, FALSE); 

/*
 * Free TCA/GFA memory
 */
    if ( el.hdr.vg_type == TCA_ELM ) {
        cvg_freeBkpts ( &el );
    }
    else if ( el.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &el );
    }

    pgundo_newStep();
    layer = pglayer_getCurLayer( );
    for (ii = 0; ii < nelm; ii++) {
        crg_goffset(inxarry[ii], &location, &ier);

        cvg_rdrec(cvg_getworkfile(), location, &el, &ier);
	pgactv_setActvElm ( &el, location);
        pgactv_getDevPts (&_dcN, &_dcX, &_dcY);

        for (jj = 0; jj < _dcN; jj++) {
            pgactv_modPt (jj, *(_dcX + jj) + delx, *(_dcY + jj) + dely);
        }

        if ((currfunc == FUNC_MOVE) || (currfunc == FUNC_SELECT)) {

/*
 * Mark elements in placement that are effected by
 * the delete, and get the area of influence back
 */
            cvg_rdrec(cvg_getworkfile(), location, &del_el, &ier);
            cvg_checkplace(&del_el, 1, location, &found, inf_bbox, &ier);
            if (found > 0) {

/*
 * Update the refresh extent if the area impacted by
 * placement was bigger than the area passed in
 */
                o_llx = G_MIN(o_llx, inf_bbox[0]);
                o_lly = G_MIN(o_lly, inf_bbox[2]);
                o_urx = G_MAX(o_urx, inf_bbox[1]);
                o_ury = G_MAX(o_ury, inf_bbox[3]);
                update_crg = 1;
            }

/*
 * Free TCA/GFA memory
 */
            if ( del_el.hdr.vg_type == TCA_ELM ) {
                cvg_freeBkpts ( &del_el );
            }
            else if ( del_el.hdr.vg_type == GFA_ELM ) {
                cvg_freeElPtr ( &del_el );
            }
	  
/*
 *  delete old element 
 */ 
	    cvg_delet (cvg_getworkfile(), location, TRUE, &ier);
	    crg_clear (inxarry[ii], &ier);
 
            pgundo_storeThisLoc(location, UNDO_DEL, &ier);
        }

/*
 *  adjust jet barb/hash position accordingly
 */ 
        if ( el.hdr.vg_type == JET_ELM ) {
            _pgmvcp_jetCalc ( &el, delx, dely, True );
        }

/*
 *  adjust GFA attribute box position accordingly
 */ 
        if ( el.hdr.vg_type == GFA_ELM ) {
            _pgmvcp_gfaCalc ( &el, delx, dely, True );
        }

/*
 *  save new element
 */
        el.hdr.grptyp = newtyp;
        el.hdr.grpnum = newnum;

        pgvgf_saveNewElm(NULL, sys_D, &el, _dcN, _dcX, _dcY, FALSE,
			&location, &ier);
        pgundo_storeThisLoc (location, UNDO_ADD, &ier);

/*
 * Free TCA/GFA memory
 */
        if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
        }
        else if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }

        cvg_rdrec(cvg_getworkfile(), location, &el, &ier);
        crg_set (&el, location, layer, &ier);

/*
 * Mark elements in placement that are effected by
 * the new element, and get the area of influence back
 */
        cvg_checkplace(&el, 0, location, &found, inf_bbox, &ier);
        if (found > 0) {

/*
 * Update the refresh extent if the area impacted by
 * placement was bigger than the area passed in
 */
            o_llx = G_MIN(o_llx, inf_bbox[0]);
            o_lly = G_MIN(o_lly, inf_bbox[2]);
            o_urx = G_MAX(o_urx, inf_bbox[1]);
            o_ury = G_MAX(o_ury, inf_bbox[3]);
            update_crg = 1;
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

    }  /* for */
    pgundo_endStep();

    pgactv_setActvElm (&el, location);

    crg_ggbnd (newtyp, newnum, &llx, &urx, &ury, &lly, &ier); 

    free (inxarry);

    o_llx -= EXTRA;
    o_lly -= EXTRA;
    o_urx += EXTRA;
    o_ury += EXTRA;

    if (o_llx < llx)
        llx = o_llx;
    if (o_lly < lly)
        lly = o_lly;
    if (o_urx > urx)
        urx = o_urx;
    if (o_ury > ury)
        ury = o_ury;

    xpgpaste (llx, lly, urx, ury, &ier);
    cvg_rfrsh (NULL, llx, lly, urx, ury, &ier);

/*
 * If we may have impacted other elements with placement
 * we will need to rebuild the range records
 */
    if (update_crg) {
        crg_rebuild();
    }

    pghdlb_select (&el, location);

    mbotw_mouseSet(LMHINT_DRAG, MMHINT_DONE);
}
 
/*=====================================================================*/

static void _pgmvcp_wboxCalc ( void )
/************************************************************************
 * _pgmvcp_wboxCalc                                                     *
 *                                                                      *
 * Internal function for MOVE/COPY watch box point/anchor calculation	*
 *                                                                      *
 * static void _pgmvcp_wboxCalc ()                 			*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  D.W.Plummer/NCEP	11/00						*
 *  A. Hardy/GSC        11/00   renamed coordinate system declarations  *
 *  H. Zeng/XTRIA	01/03   modified arguments to pgwbxw_getAttr	*
 *  H.Zeng/SAIC		10/04	added two para. to pgwbxw_getAttr	*
 ***********************************************************************/
{
    int           ii, np, cnty_colr, ier;
    signed char	  cnty_fill;
    int           nearest, wcolr, wstyle, wshape, mtype, mwidth;
    float	  rx[8], ry[8], rx2[8], ry2[8], tlat[8], tlon[8], msize;
/*---------------------------------------------------------------------*/
/*
 * get the new corner points (by calculating anchor points)
 */
        pgwbxw_getAttr(&wcolr, &wstyle, &wshape, 
		       &mtype, &msize,  &mwidth,
		       &cnty_fill, &cnty_colr    );

        np = 8;

	for ( ii = 0; ii < np; ii++ )  {
	    rx[ii] = *(_dcX +ii);
	    ry[ii] = *(_dcY +ii);
	}
        gtrans (sys_D, sys_M, &np, rx, ry, tlat, tlon, &ier,
                strlen(sys_D), strlen(sys_M));
	nearest = 0;
        pgwpts_get(nearest, wshape, tlat, tlon, tlat, tlon, &ier);
        gtrans (sys_M, sys_D, &np, tlat, tlon, rx2, ry2, &ier,
                strlen(sys_M), strlen(sys_D));

/*
 * assign the new values
 */
        for (ii = 0; ii < np; ii++) {
            _dcX[ii] = rx2[ii];
            _dcY[ii] = ry2[ii];
            pgactv_modPt( ii, _dcX[ii], _dcY[ii] );
        }
        _dcX[np+1] = _dcX[0];
        _dcY[np+1] = _dcY[0];
        _dcX[np+2] = ( _dcX[2] + _dcX[6] ) / 2.0F;
        _dcY[np+2] = ( _dcY[2] + _dcY[6] ) / 2.0F;
        _dcX[np+3] = _dcX[4];
        _dcY[np+3] = _dcY[4];

}

/*=====================================================================*/

static void _pgmvcp_jetCalc ( VG_DBStruct *el, float dx, float dy, Boolean grp )
/************************************************************************
 * _pgmvcp_wboxCalc                                                     *
 *                                                                      *
 * Internal function for MOVE/COPY jet's barbs/hashs			*
 *                                                                      *
 * static void _pgmvcp_jetCalc ()                  			*
 *      *el               VG_DBStruct	Pointer to jet element		*
 *       dx               float		Increment on X axis		*
 *       dy               float		Increment on Y axis		*
 *       grp              Boolean	Group mode or not		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		11/03	initial coding				*
 ***********************************************************************/
{
    int		ii, np, ier;
    float	delx, dely, xx, yy;
/*---------------------------------------------------------------------*/
/*
 * For group drag/drop, use the given dx, dy as the "delta" increment.
 */    
    delx = dx;
    dely = dy;
    
/*
 * For single element drag/drop, recalculate the "delta" increment. 
 */    
    if ( !grp && _dcN > 0 ) {
        delx = *(_dcX + _nearPt) - _origX;
        dely = *(_dcY + _nearPt) - _origY;
    }
    
/*
 * Adjust barb/hash locations.
 */
    np = 1;
    for ( ii = 0; ii < el->elem.jet.nbarb; ii++ ) {
	gtrans ( sys_M, sys_D, &np,
		 &el->elem.jet.barb[ii].wnd.data.latlon[0],
		 &el->elem.jet.barb[ii].wnd.data.latlon[1],
		 &xx, &yy, &ier, strlen(sys_M), strlen(sys_D) );
        xx += delx;
	yy += dely;
	gtrans ( sys_D, sys_M, &np, &xx, &yy,
		 &el->elem.jet.barb[ii].wnd.data.latlon[0],
		 &el->elem.jet.barb[ii].wnd.data.latlon[1],
		 &ier, strlen(sys_D), strlen(sys_M) );    
    }
    
    for ( ii = 0; ii < el->elem.jet.nhash; ii++ ) {
	gtrans ( sys_M, sys_D, &np,
		 &el->elem.jet.hash[ii].wnd.data.latlon[0],
		 &el->elem.jet.hash[ii].wnd.data.latlon[1],
		 &xx, &yy, &ier, strlen(sys_M), strlen(sys_D) );
        xx += delx;
	yy += dely;
	gtrans ( sys_D, sys_M, &np, &xx, &yy,
		 &el->elem.jet.hash[ii].wnd.data.latlon[0],
		 &el->elem.jet.hash[ii].wnd.data.latlon[1],
		 &ier, strlen(sys_D), strlen(sys_M) );    
    }    
}

/*=====================================================================*/
static void _pgmvcp_ccfCalc ( VG_DBStruct *el, float dx, float dy, Boolean grp )
/******************************************************************************
  _pgmvcp_ccfCalc
  
  Internal function for MOVE/COPY CCF's attribute box location
  
  static void _pgmvcp_ccfCalc ()
    Input parameters:
      *el    VG_DBStruct Pointer to ccf element
      dx     float Increment on X axis
      dy     float Increment on Y axis
      grp    Boolean Group mode or not
**
  Log:
  L. Hinson  07/09  initial coding
*******************************************************************************/      
{
    int		np, ier;
    float	delx, dely, xx, yy, nlat, nlon;
    char	value[32];
    delx = dx;
    dely = dy;
    if ( !grp && _dcN > 0 ) {
        delx = *(_dcX + _nearPt) - _origX;
        dely = *(_dcY + _nearPt) - _origY;
    }
    np = 1;
    nlat = el->elem.ccf.info.textlat;
    nlon = el->elem.ccf.info.textlon;
    gtrans ( sys_M, sys_D, &np, &nlat, &nlon,
		 &xx, &yy, &ier, strlen(sys_M), strlen(sys_D) );
    xx += delx;
    yy += dely;
    gtrans ( sys_D, sys_M, &np, &xx, &yy, &nlat,&nlon,
		 &ier, strlen(sys_D), strlen(sys_M) );
    el->elem.ccf.info.textlat = nlat;
    el->elem.ccf.info.textlon = nlon;
}

/*=====================================================================*/

/*=====================================================================*/

static void _pgmvcp_gfaCalc ( VG_DBStruct *el, float dx, float dy, Boolean grp )
/************************************************************************
 * _pgmvcp_gfaCalc                                                      *
 *                                                                      *
 * Internal function for MOVE/COPY GFA's atribute box location.		*
 *                                                                      *
 * static void _pgmvcp_gfaCalc ()                 			*
 *      *el               VG_DBStruct	Pointer to gfa element		*
 *       dx               float		Increment on X axis		*
 *       dy               float		Increment on Y axis		*
 *       grp              Boolean	Group mode or not		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		10/04	Access GFA attr with cvg_getFld()	*
 ***********************************************************************/
{
    int		np, ier;
    float	delx, dely, xx, yy, nlat, nlon;
    char	value[32];
/*---------------------------------------------------------------------*/
/*
 * For group drag/drop, use the given dx, dy as the "delta" increment.
 */    
    delx = dx;
    dely = dy;
    
/*
 * For single element drag/drop, recalculate the "delta" increment. 
 */    
    if ( !grp && _dcN > 0 ) {
        delx = *(_dcX + _nearPt) - _origX;
        dely = *(_dcY + _nearPt) - _origY;
    }
    
/*
 * Adjust attribute text box location.
 */
    np = 1;
    cvg_getFld ( el, TAG_GFA_LAT, value, &ier );
    nlat = atof ( value );
    cvg_getFld ( el, TAG_GFA_LON, value, &ier );
    nlon = atof ( value );
    
    gtrans ( sys_M, sys_D, &np, &nlat, &nlon,
		 &xx, &yy, &ier, strlen(sys_M), strlen(sys_D) );
    xx += delx;
    yy += dely;
    gtrans ( sys_D, sys_M, &np, &xx, &yy, &nlat,&nlon,
		 &ier, strlen(sys_D), strlen(sys_M) );    
    
    sprintf ( value, "%7.2f", nlat );
    cvg_setFld ( el, TAG_GFA_LAT, value, &ier );

    sprintf ( value, "%7.2f", nlon );
    cvg_setFld ( el, TAG_GFA_LON, value, &ier );
}
