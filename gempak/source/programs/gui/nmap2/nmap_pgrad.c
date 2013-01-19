#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "proto_xw.h"

static float    _dragPtX[2], _dragPtY[2];
static VG_DBStruct	_tmpel;
static Boolean _circPlActv;


/*
 *  private callback functions
 */
void _pgrad_radiusDropEh   ( Widget, XtPointer, XEvent*, Boolean* );
void _pgrad_radiusMotionEh ( Widget, XtPointer, XEvent*, Boolean* );


/************************************************************************
 * nmap_pgrad.c 							*
 *									*
 * This module contains the callback functions for radius extension	*
 * operations in product generation.					*
 *									*
 * CONTENTS:								*
 * pgrad_radiusStart()	  'RADIUS' state -- start  (mouse press)	*
 *									*
 * _pgrad_radiusMotionEh()'RADIUS' state -- motion (motion)		*
 * _pgrad_radiusDropEh()  'RADIUS' state -- drop   (mouse release)	*
 * pgrad_setCircPlActv()  set value of _circPlActv flag			*
 ***********************************************************************/

/*=====================================================================*/

void pgrad_radiusStart ( float x, float y )
/************************************************************************
 * pgrad_radiusStart                                                    *
 *                                                                      *
 * Internal function for start RADIUS mode called by pgevt_radiusHdl.   *
 *                                                                      *
 * void pgrad_radiusStart( x, y )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  x		float		current x coordinate			*
 *  y		float		current y coordinate			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI	05/99	Copied from pgrot_rotateStart		*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * H. Zeng/SAIC		09/06	changed argument for pggst_veilGhost()	*
 ***********************************************************************/
{
    int		np, ier;
/*---------------------------------------------------------------------*/

    pggst_clearGhost(TRUE);
    pggst_veilGhost (FALSE);

    _dragPtX[0] = x;
    _dragPtY[0] = y;
    _dragPtX[1] = x;
    _dragPtY[1] = y;
    pgcirc_setLocation(_dragPtX[0], _dragPtY[0], _dragPtX[1], _dragPtY[1]);

    _tmpel.hdr.vg_class = CLASS_CIRCLE;
    _tmpel.elem.cir.info.numpts = 2;
    np = 2;
    gtrans (sys_D, sys_M, &np, _dragPtX, _dragPtY, 
	    &_tmpel.elem.cir.data.latlon[0],
	    &_tmpel.elem.cir.data.latlon[np], &ier, 
	    strlen(sys_D), strlen(sys_M) );
    pggst_setCircle (&_tmpel);
    mcanvw_setDropFunc((XtEventHandler)&_pgrad_radiusDropEh, CURS_DEFAULT);
    mcanvw_setDragFunc((XtEventHandler)&_pgrad_radiusMotionEh, CURS_DEFAULT);
}

/*=====================================================================*/
/* ARGSUSED */
void _pgrad_radiusMotionEh ( Widget w, XtPointer clnt, XEvent *event,
					Boolean *ctdr )
/************************************************************************
 * _pgrad_radiusMotionEh						*
 *                                                                      *
 * Callback function for RADIUS motion mode.  This mode does not	*
 * require the user to hold the mouse button to change the radius of	*
 * the object.								*
 *                                                                      *
 * void _pgrad_radiusMotionEh ( w, clnt, event)				*
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget          Calling widget                          *
 *  clnt	XtPointer                                               *
 *  *event      XEvent                                                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI	05/99	Copied from pgrot_rotateMotion		*
 * E. Safford/GSC	10/99	update for new xwcmn.h			*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * H. Zeng/SAIC		08/06	added check of dist. display flag	*
 * H. Zeng/SAIC		09/06	removed call to pggst_clearGhost()	*
 ***********************************************************************/
{
    int		ier, np, xoff, yoff;
/*---------------------------------------------------------------------*/
/*
 * Check distance display option flag.
 */
    if ( pgdist_isDplOn() ) {

	 pgdist_update (event->xbutton.x, event->xbutton.y);
	 XmUpdateDisplay( mcanvw_getDrawingW() );
    }

    xgtoff (&xoff, &yoff, &ier);
    _dragPtX[1] = (float) (event->xbutton.x + xoff); 
    _dragPtY[1] = (float) (event->xbutton.y + yoff); 

    pgcirc_setLocation(_dragPtX[0], _dragPtY[0], _dragPtX[1], _dragPtY[1]);

    np = 2;
    gtrans (sys_D, sys_M, &np, _dragPtX, _dragPtY, 
	    &_tmpel.elem.cir.data.latlon[0],
	    &_tmpel.elem.cir.data.latlon[np], &ier, 
	    strlen(sys_D), strlen(sys_M) );

    pggst_setCircle (&_tmpel);
}

/*=====================================================================*/
/* ARGSUSED */
void _pgrad_radiusDropEh ( Widget w, XtPointer clnt, XEvent *event,
							Boolean *ctdr )
/************************************************************************
 * _pgrad_radiusDropEh                                                  *
 *                                                                      *
 * Callback function for RADIUS drop mode.                              *
 *                                                                      *
 * _pgrad_radiusDropEh ( w, clnt, event, ctdr )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt		XtPointer       State information record        *
 *      *event          XEvent          Button press event record       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI	05/99	Copied from pgrot_rotateDrop		*
 * G. Krueger/EAI	06/99	Fixed doubled circle problem		*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * E. Safford/GSC	10/99	update for new xwcmn.h			*
 * S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 * S. Law/GSC		03/00	fixed erroneous relabeling		*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * H. Zeng/EAI          09/01   revised for new GROUP functionality     *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * H. Zeng/SAIC		08/06	added check of dist. display flag	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		location, ier, np, iobj, xoff, yoff;
    int         el_location;
    float	lats[2], lons[2], llx, lly, urx, ury;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    el.hdr.vg_class = 0;
    pggst_setCircle (&el);
    pggst_clearGhost (TRUE);

/*
 * Check distance display option flag.
 */
    if ( pgdist_isDplOn() ) {

	 pgdist_stop ( );
    }

    el.hdr.vg_class = CLASS_CIRCLE;

    xgtoff (&xoff, &yoff, &ier);
    _dragPtX[1] = (float) (event->xbutton.x + xoff); 
    _dragPtY[1] = (float) (event->xbutton.y + yoff); 

    pgcirc_setLocation(_dragPtX[0], _dragPtY[0], _dragPtX[1], _dragPtY[1]);
    np = 2;
    gtrans (sys_D, sys_M, &np, _dragPtX, _dragPtY, lats, lons, 
            &ier, strlen(sys_D), strlen(sys_M) );

    el_location = pgactv_getElmLoc();
    pgutls_prepNew (el_location, &el, &llx, &lly, 
                    &urx, &ury, &ier);

    iobj = pgpalw_getCurObjId();
    pglabel_saveOldInfor(CLASS_CIRCLE, iobj);

    pgundo_newStep();
    pgundo_storeThisLoc(el_location, UNDO_DEL, &ier);

    el.elem.cir.data.latlon[0] = _tmpel.elem.cir.data.latlon[0];
    el.elem.cir.data.latlon[1] = _tmpel.elem.cir.data.latlon[1];
    el.elem.cir.data.latlon[2] = _tmpel.elem.cir.data.latlon[2];
    el.elem.cir.data.latlon[3] = _tmpel.elem.cir.data.latlon[3];
    pgvgf_saveNewElm(NULL, sys_M, &el, 0, NULL, NULL, TRUE,
    		&location, &ier);

    pgundo_storeThisLoc (location, UNDO_ADD, &ier);
    pgundo_endStep();

/*
 * REDISPLAY -- Redisplay the newly created/stored element
 *              by reading the record, setting the range, and displaying
 */
    pgutls_redraw(location, &el, &ier);

    cvg_rdrec (cvg_getworkfile(), location, &el, &ier);
    pgactv_setActvElm(&el, location);
    pghdlb_select( &el, location);

    if (_circPlActv) {
	if (pglabel_getLabFlag()) {
	    pglabel_txtpopup();
	}
	else {
	    pghdlb_deselectAll();
	    pgnew_setArmDynamic ();
	    mcanvw_setDynActFlag (TRUE);
	}
    } else {
	mcanvw_disarmDynamic ();
	mcanvw_setPressFunc ((XtEventHandler)&pgevt_selectHdl, CURS_DEFAULT);
    }

/*
 * If GROUP is active, redraw the group boundary so the new element 
 * is included.
 */
    if ( pgpalw_isGrpActv() ) {

         if ( pggrpw_getEmptFlg() ) {
              pggrpw_setEmptFlg(FALSE);
         }
         pghdlb_deselectGrp( (char)pggrpw_getGrpType(),
                                   pggrpw_getGrpNum()   );
         pghdlb_selectGrp( (char)pggrpw_getGrpType(),
                                 pggrpw_getGrpNum()   ); 
    }
}

/*=====================================================================*/

void pgrad_setCircPlActv ( Boolean status ) 
/************************************************************************
 * pgrad_setCircPlActv                                                  *
 *                                                                      *
 * Utility for setting the value of _circPlActv flag                    *
 *                                                                      *
 * void pgrad_setCircPlActv ( status )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  status          Boolean     New value of _circPlActv flag           *
 *                                                                      *
 * Ouput parameters:                                                    *
 *  none                                                                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * G. Krueger/EAI  05/99        Copied from pgevt_setWindPlActv		*
 ***********************************************************************/
{
    _circPlActv = status;
}
