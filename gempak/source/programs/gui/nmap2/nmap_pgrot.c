#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "proto_xw.h"


static float    _dragPtX[2], _dragPtY[2];


/*
 *  private callback functions
 */
void _pgrot_rotateDrop	( Widget, XtPointer, XEvent* );
void _pgrot_rotateMotion( Widget, XtPointer, XEvent* );


/************************************************************************
 * nmap_pgrot.c                                                         *
 *                                                                      *
 * This module contains the callback functions for rotation operations  *
 * in product generation.                                               *
 *                                                                      *
 * CONTENTS:                                                            *
 * pgrot_rotateStart()    'ROTATE' state -- start  (mouse press)	*
 *                                                                      *
 * _pgrot_rotateMotion()  'ROTATE' state -- motion (motion)		*
 * _pgrot_rotateDrop()    'ROTATE' state -- drop   (mouse release)	*
 ***********************************************************************/

/*=====================================================================*/

void pgrot_rotateStart ( float x, float y )
/************************************************************************
 * pgrot_rotateStart                                                    *
 *                                                                      *
 * Internal function for start ROTATE mode called by pgevt_rotateHdl.   *
 *                                                                      *
 * void pgrot_rotateStart( x, y )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  x		float		current x coordinate			*
 *  y		float		current y coordinate			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Wehner/Eai               Initial coding                          *
 *  C. Lin/EAi          10/97   rename from rotDrag2Cb, cleanup         *
 *  E. Safford/GSC      12/97   modify for intial wind rotation         *
 *  S. Law/GSC          05/98   replaced pgpalw_rotate with _setupOper  *
 *  S. Law/GSC          05/98   added call to pgevt_unsetOper           *
 *  E. Safford/GSC	05/98	moved to nmap_pgrot.c, cleaned up	*
 *  S. Law/GSC		07/98	added call to NxmTxtA_setGhostFlag	*
 *  G. Krueger/EAI	09/98	Added ghost veiling			*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 ***********************************************************************/
{
    int		np, ier;
    float	*x_coords, *y_coords;
/*---------------------------------------------------------------------*/

    pggst_clearGhost(TRUE);
    pggst_veilGhost(TRUE);

    pgactv_getDevPts (&np, &x_coords, &y_coords);
    _dragPtX[0] = *x_coords;
    _dragPtY[0] = *y_coords;
    _dragPtX[1] = x;
    _dragPtY[1] = y;
    pggst_addGhostPts (2, _dragPtX, _dragPtY, &ier);
    pggst_drawGhost (GST_NORMAL);

    mcanvw_setDropFunc((XtEventHandler)&_pgrot_rotateDrop, CURS_DEFAULT);
    mcanvw_setDragFunc((XtEventHandler)&_pgrot_rotateMotion, CURS_DEFAULT);
}

/*=====================================================================*/
/* ARGSUSED */
void _pgrot_rotateMotion ( Widget w, XtPointer clnt, XEvent *event )
/************************************************************************
 * _pgrot_rotateMotion							*
 *                                                                      *
 * Callback function for ROTATE motion mode.  This mode does not	*
 * require the user to hold the mouse button to change the rotation	*
 * angle of the object.							*
 *                                                                      *
 * void _pgrot_rotateMotion( w, clnt, event)				*
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget          Calling widget                          *
 *  clnt	XtPointer                                               *
 *  *event      XEvent                                                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Wehner/Eai               Initial coding                          *
 *  C. Lin/EAi          10/97   rename from rotDragCb, cleanup          *
 *  E. Safford/GSC	05/98	moved to nmap_pgrot.c, cleaned up	*
 *  E. Safford/GSC	08/98	updated ghosting                 	*
 *  G. Krueger/EAI	09/98	Added ghost veiling			*
 *  E. Safford/GSC	10/99	update for new xwcmn.h           	*
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 ***********************************************************************/
{
    int		ier, xoff, yoff;
/*---------------------------------------------------------------------*/
/*
 * Erase the last ghost line
 */
    pggst_drawGhost (GST_NORMAL);
    
    xgtoff (&xoff, &yoff, &ier);
    _dragPtX[1] = (float) (event->xbutton.x + xoff); 
    _dragPtY[1] = (float) (event->xbutton.y + yoff); 

    pggst_replaceGhostPts (1, &_dragPtX[1], &_dragPtY[1], &ier);
    pggst_drawGhost(GST_NORMAL);
}

/*=====================================================================*/
/* ARGSUSED */
void _pgrot_rotateDrop ( Widget w, XtPointer clnt, XEvent *event )
/************************************************************************
 * _pgrot_rotateDrop                                                    *
 *                                                                      *
 * Callback function for ROTATE drop mode.                              *
 *                                                                      *
 * _pgrot_rotateDrop  ( w, clnt, event)                          	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt		XtPointer       State information record        *
 *      *event          XEvent          Button press event record       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       06/97   Modified to handle Special Text         *
 * E. Wehner/EAi        07/97   Add rotation to special text            *
 * E. Safford/GSC       07/97   Updated for changes to cvg_svstx        *
 * E. WEhner/EAi        08/97   Don't pass grinfo to handlebar          *
 * D.W.Plummer/NCEP     09/97   Combine into NxmDraw for new vgstruct.h *
 * E. Wehner/EAi        09/97   Remove graphics info record             *
 * C. Lin/EAi           10/97   rename from rotDropCb, cleanup          *
 * C. Lin/EAi           11/97   further cleanup                         *
 * E. Safford/GSC       12/97   modify for intial wind rotation         *
 * A. Hardy/GSC         02/98   inserted comma in setAttr calling seq.  *
 * S. Law/GSC           03/98   cleaned up wind normang calculation     *
 * W. Li/EAI            04/98   Add darr and hash in CLASS_WINDS        *
 * S. Law/GSC           04/98   arranged normang statements properly    *
 * E. Safford/GSC       05/98   update undo                             *
 * E. Safford/GSC	05/98	moved to nmap_pgrot.c, cleaned up	*
 * E. Safford/GSC	06/98	add call to NxmTxtA_setRotn      	*
 * W. Li/EAI		08/98	Added rotation type for text		*
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * W. Li/EAI		10/98	Corrected negative angle problems	*
 * I. Durham/GSC	10/98	changed gazdrm to gp_azdr		*
 * I. Durham/GSC	10/98	changed gdrazm to gp_draz		*
 * W. Li/EAI		01/99	NxmTxtA_XXX --> pgtxt_XXX		*
 * S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 * S. Law/GSC		03/00	added GRPTYP_CCF check			*
 * S. Law/GSC		03/00	changed to use pgccfw_saveEdit		*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * H. Zeng/EAI          09/01   revised for new GROUP functionality     *
 * D.W.Plummer/NCEP	08/03	chg calling seq for cgr_dang to ptrs	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * L. Hinson/AWC        07/09   Removed GRPTYP_CCF conditional process  *
 ***********************************************************************/
{
    int		location, el_location, ier;
    float	dang; 			  /* device angle */
    float	scrang, normang, nthrang;
    float	llx, lly, urx, ury;
    float	wndang;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    pggst_clearGhost(TRUE);

    pgundo_newStep();
    el_location = pgactv_getElmLoc();    
    pgutls_prepNew (el_location, &el, &llx, &lly, &urx, &ury, &ier);
    pgundo_storeThisLoc(el_location, UNDO_DEL, &ier);

    cgr_dang(&(_dragPtX[0]), &(_dragPtY[0]), 
	     &(_dragPtX[1]), &(_dragPtY[1]), &dang, &ier);
    scrang = dang * -1.0F;

    if (scrang < 0.0F){
        scrang  += 360.0F;
    }

    switch (el.hdr.vg_type) {

      case TEXT_ELM:
      case TEXTC_ELM:
	if ((int)el.elem.txt.info.rotn >= 1000){
	    gp_azdr (&scrang,  &el.elem.txt.info.lat,
                    &el.elem.txt.info.lon, &nthrang, &ier );
	    if (nthrang < 0.0F){
		nthrang  += 360.0F;
	    }
	    el.elem.txt.info.rotn = nthrang + 1000.0F;
	}
	else {
	    el.elem.txt.info.rotn = scrang;
	}
        break;

      case SPTX_ELM:
	if ((int)el.elem.spt.info.rotn >= 1000){
	    gp_azdr (&scrang,  &el.elem.spt.info.lat,
                    &el.elem.spt.info.lon, &nthrang, &ier );
	    if (nthrang < 0.0F){
		nthrang  += 360.0F;
	    }
	    el.elem.spt.info.rotn = nthrang + 1000.0F;
	}
	else{
	    el.elem.spt.info.rotn = scrang;
	}

        break;

      case BARB_ELM:
      case ARROW_ELM:
      case DARR_ELM:
      case HASH_ELM:
        if ((el.hdr.vg_type == BARB_ELM) ||
            (el.hdr.vg_type == HASH_ELM))
            normang = (-1.0F * scrang ) + 90.0F;
        else
            normang = (-1.0F * scrang ) - 90.0F;

        while (normang < 0.0F) normang += 360.0F;

        gp_draz( &normang, &el.elem.wnd.data.latlon[0],
               &el.elem.wnd.data.latlon[1],
	       &wndang, &ier);

        el.elem.wnd.data.spddir[1] = wndang;
        break;

    }

    pgvgf_saveNewElm(NULL, sys_D, &el, 0, NULL, NULL, TRUE,
		    &location, &ier);

    pgutls_redraw(location, &el, &ier);

    if (el.hdr.vg_class == CLASS_WINDS) {
	pgwndw_setAttr (el.elem.wnd.data.spddir[1], 
			-1.0F, -1.0F, -1, -1.0F, -1);
    }
    else if (el.hdr.vg_class == CLASS_TEXT) {
	pgtxt_setRotn ((int) el.elem.spt.info.rotn);
    }

    pgundo_storeThisLoc (location, UNDO_ADD, &ier);
    pgundo_endStep();

/*
 * If GROUP is active, redraw the group boundary so the new element 
 *is included.
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


    pggst_addGhostPts (2, _dragPtX, _dragPtY, &ier);
    pggst_drawGhost (GST_NORMAL);

}
