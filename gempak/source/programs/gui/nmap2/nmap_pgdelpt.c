#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "hints.h"
#include "proto_xw.h"

static float		*_dcX, *_dcY;
static int		_dcN;
static Boolean		_selectFlag;

static VG_DBStruct	_selectEl;


/*
 *  Private Callback Functions
 */
static void pgdelpt_selectCb ( Widget, XtPointer, XEvent* );


/************************************************************************
 * nmap_pgdelpt.c							*
 *									*
 * This module contains the routines for the point deletion function.	*
 *									*
 * CONTENTS:								*
 * pgdelpt_start	Initialize and original selection		*
 * pgdelpt_selectCb	Selection callback				*
 ***********************************************************************/

/*=====================================================================*/

void pgdelpt_start ( VG_DBStruct *el )
/************************************************************************
 * pgdelpt_start							*
 *									*
 * Initial setup and original selection					*
 *									*
 * void pgdelpt_start (el)						*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	selected element		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 *  S. Law/GSC		08/98	Initial coding				*
 *  G. Krueger/EAI	10/98	Using table for mouse hints		*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 ***********************************************************************/
{
    _selectFlag = FALSE;
    mcanvw_setPressFunc((XtEventHandler)&pgdelpt_selectCb, CURS_DEFAULT);

    _selectEl = *el;

    pgactv_getDevPts (&_dcN, &_dcX, &_dcY);

    mbotw_mouseSet(LMHINT_PTSELECT, MMHINT_DONE);

}

/*=====================================================================*/
/* ARGSUSED */
static void pgdelpt_selectCb ( Widget wid, XtPointer clnt, 
							XEvent *event )
/************************************************************************
 * pgdelpt_selectCb							*
 *									*
 * This function handles the selection callbacks			*
 *									*
 * static void pgdelpt_selectCb (wid, clnt, event)			*
 *									*
 * Input parameters:							*
 *	wid		Widget	calling widget				*
 *	clnt		XtPointer					*
 *	*event		XEvent						*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 *  S. Law/GSC		08/98	Initial coding				*
 *  G. Krueger/EAI	10/98	Using table for mouse hints		*
 *  E. Safford/GSC	04/99	fix irix6 compile warning		*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  E. Safford/GSC	10/99	update for new xwcmn.h			*
 *  S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 *  H. Zeng/EAI         11/00   changed for the new undo design         *
 *  A. Hardy/GSC        11/00   renamed coordinate system declaration   *
 *  H. Zeng/EAI         12/00   modified for the new undo design        *
 *  E. Safford/GSC	02/01	add tie-in dist check on pt selection   *
 *				  and rename as a callback func		*
 * J. Wu/GSC		03/01	added "EXIT" hint to bottom panel       *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		03/04	add GFA_ELM				*
 * B. Yin/SAIC		02/05	add a call to snap GFA			*
 * E. Safford/SAIC	06/05	allow smear to get smaller on edit	*
 * B. Yin/SAIC		02/06	allow open line GFA less than 3 points	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          02/07   Add logic to update GFA centroid 	*
 ***********************************************************************/
{
    float	distance, xx, yy, llx, lly, urx, ury;
    float	x_cntr, y_cntr, c_lat, c_lon, area;
    int		mrkclr, mrknp, location, ier, xoff, yoff;
    int         el_location, one = 1;
    char	value[32];
    static int	nearest;

/*---------------------------------------------------------------------*/

    if (event->xbutton.button == Button1) {
        if (_selectFlag) {     /* confirming selected point */
	    pgundo_newStep();

	    el_location = pgactv_getElmLoc();
	    pgutls_prepNew (el_location, &_selectEl, &llx, &lly, 
                            &urx, &ury, &ier);
	    pgundo_storeThisLoc (el_location, UNDO_DEL, &ier);

	    pgactv_getDevPts (&_dcN, &_dcX, &_dcY);
	    if ( _selectEl.hdr.vg_type != GFA_ELM ||
	         ( (_selectEl.hdr.vg_type == GFA_ELM) &&
		   ( ( _selectEl.hdr.closed == 1 && _dcN > 3) ||
		     ( _selectEl.hdr.closed == 0 && _dcN > 2))) ) { 		
	        pgactv_deletePt (nearest);
	    }
	    
	    pgactv_getDevPts (&_dcN, &_dcX, &_dcY);

	    if (_dcN > 1) {

/*
 *  Snap GFA 
 */
		if ( _selectEl.hdr.vg_type == GFA_ELM ) {
		   _selectEl.elem.gfa.info.npts = _dcN;
		   gtrans ( sys_D, sys_M, &_dcN, _dcX, _dcY, 
			    &(_selectEl.elem.gfa.latlon[0]), 
			    &(_selectEl.elem.gfa.latlon[_dcN]), &ier,
			    strlen(sys_D), strlen(sys_M) );

		   pgsmear_snapEl ( FALSE, &_selectEl, &ier );

		   _dcN = _selectEl.elem.gfa.info.npts;

		   cvg_todev( &_selectEl, &_dcN, _dcX, _dcY, &ier );

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
		   cvg_setFld ( &_selectEl, TAG_GFA_ARROW_LAT, value, &ier );
		   sprintf ( value, "%7.2f", c_lon );
		   cvg_setFld ( &_selectEl, TAG_GFA_ARROW_LON, value, &ier );
		}

		pgvgf_saveNewElm(cvg_getworkfile(), sys_D, &_selectEl, 
				_dcN, _dcX, _dcY, TRUE, &location, &ier );

		pgutls_redraw(location, &_selectEl, &ier);

		pgundo_storeThisLoc (location, UNDO_ADD, &ier);

		_selectFlag = FALSE;
		mbotw_mouseSet(LMHINT_PTSELECT, MMHINT_DONE);
	    }
	    else {
		mcanvw_disarmDynamic ();
		_selectFlag = FALSE;

		pghdlb_deselectAll();

		mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);

		_selectFlag = FALSE;
		mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_EXIT);
	    }
            pgundo_endStep();

	}
	else {	/* selecting point */
	    xgtoff (&xoff, &yoff, &ier);
	    xx = (float) (event->xbutton.x + xoff);
	    yy = (float) (event->xbutton.y + yoff);

	    cgr_dist (_dcN, _dcX, _dcY, xx, yy, &distance, &nearest, &ier);
	    if ( distance > VERTEX_TIEIN ) {
	        return;
	    }

	    xx = _dcX[nearest];
	    yy = _dcY[nearest];

	    mrkclr = 2;
	    mrknp  = 1;
	    gscolr (&mrkclr, &ier);
	    gmark  (sys_D, &mrknp, &xx, &yy, &ier, strlen(sys_D) );
	    geplot (&ier);

	    _selectFlag = TRUE;
	    mbotw_mouseSet(LMHINT_CONFIRM, MMHINT_CANCEL);
	}
    }
    else {
        if (_selectFlag) {     /* unselecting point */
	    pghdlb_displayAllSel ();

	    _selectFlag = FALSE;
	    mbotw_mouseSet(LMHINT_PTSELECT, MMHINT_DONE);
	}
	else {	/* unselecting element */
	    mcanvw_disarmDynamic ();
	    _selectFlag = FALSE;

	    pghdlb_deselectAll();

	    mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);

	    _selectFlag = FALSE;
	    mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_EXIT);
	}
    }
}
