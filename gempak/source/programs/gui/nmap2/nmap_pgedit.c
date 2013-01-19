#include "geminc.h"
#include "gemprm.h"
#include "NxmTxt.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "hints.h"

#define NO_VALUE	-99              /* no valid value for _subTyp */

#define	 BOX		0
#define	 SIZE		1
#define	 FONT		2
#define	 STYLE		3
#define	 ALIGN		4
#define	 TURB		5
#define  ICNG           6
#define	 TEXT		7
#define	 COLOR		8
#define	 ROTN		9

#define W_DIR 		0
#define W_SPD   	1
#define W_SIZE          2
#define W_WIDTH         3
#define W_A_SIZ         4
#define W_COLOR         5


#define	 RED_COLOR	2

#define  MAX_GRP_MEM    100

static float    *_dcX, *_dcY;
static int      _dcN;

static int     _vgType;

static int     _subTyp;
static int     _pipDr;


/************************************************************************
 * nmap_pgedit.c                                                        *
 *                                                                      *
 * This module contains the event_handling/callback functions that are  *
 * used to edit existing vgf elements in product generation             *
 * CONTENTS:                                                            *
 * pgedit_editStart()      'EDIT'   state -- start (mouse press)        *
 * pgedit_isActive()	   indicates state of attribute editing		*
 *                                                                      *
 * pgedit_editCb()        callback for edit apply/cancel buttons        *
 * pgedit_multiEditCb()   callback for multi-edit apply/cancel buttons  *
 ***********************************************************************/

/*=====================================================================*/

void pgedit_editStart ( VG_DBStruct *el )
/************************************************************************
 * pgedit_editStart                                                     *
 *                                                                      *
 * Initialization routine for beginning attribute editing.              *
 *                                                                      *
 * void pgedit_editStart( el )                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	Selected element to be edited	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       11/97   copied from  _pgevt_modifyStart         *
 * E. Safford/GSC       01/98   fixed pip direction error on fronts     *
 * C. Lin/EAI           01/98   set dash_line type to 5                 *
 * E. Safford/GSC       03/98   added assignment to _vgType             *
 * W. Li/EAI            03/98   add selected symbol editing window      *
 * S. Law/GSC           03/98   added call to pgwndw_setColor           *
 * C. Lin/EAI           04/98   modify for line and front class         *
 * F. J. Yen/NCEP       04/98   set _subTyp for symbols                 *
 * W. Li/EAI            04/98   Add darr and hash in CLASS_WINDS        *
 * W. Li/EAI            04/98   Removed stroke from CLASS_LINE          *
 * F. J. Yen/NCEP       05/98   Removed pip stroke; Renamed with subTyp *
 * E. Safford/GSC	06/98	moved to nmap_pgedit.c			*
 * E. Safford/GSC	06/98	removed reassignment of _subTyp on Lines*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * W. Li/EAI		07/98	Added color for front			*
 * E. Safford/GSC	09/98	remove call to mbotw_actionSet		*
 * W. Li/EAI		09/98	Added clear type for vect		*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * W. Li/EAI		11/98	Added FUNC_NUMB_EDIT 			*
 * S. Law/GSC		11/98	Seperated CLASS_WATCHES from _PRODUCTS	*
 * W. Li/EAI		12/98	excluded character in number editor	*
 * S. Law/GSC		12/98	Added call to pgwbx_setWlst		*
 * E. Safford/GSC	12/98	removed FUNC_NUMB_EDIT			*
 * W. Li/EAI		12/98	Added attribute edit in pgtxt_editPopup	*
 * W. Li/EAI		01/99	Moved BOX,.., ROTN from Nxm.h to pgedit	*
 * W. Li/EAI		03/99	Added latitude/longitude editting	*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * S. Law/GSC		05/99	Added CLASS_TRACKS			*
 * S. Law/GSC		07/99	Added CLASS_SIGMETS			*
 * S. Law/GSC		08/99	pgsigf -> pgsigw			*
 * S. Law/GSC		09/99	changed pgsigw_popup calling		*
 * S. Law/GSC		07/99	Added CCF				*
 * H. Zeng/EAI          07/00   added check of possible label for line  *
 *                              front, circle and symbol                *
 * D.W.Plummer/NCEP	10/00	set watch info before popping edit box	*
 * H. Zeng/EAI          01/01   added call to pgwatch_editcnty()        *
 * E. Safford/GSC	01/01	add grptyp param to pgline_popup	*
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * E. Safford/GSC	03/01	add param to pgline_popup, frtw & symb	*
 * E. Safford/SAIC	09/01	increase value of MAX_GRP_MEM 		*
 * J. Wu/SAIC		10/01	add param kpos to pgline_popup		*
 * D.W.Plummer/NCEP	12/01	chg call seq to pgwatch_editcnty	*
 * J. Wu/SAIC		07/02	show "SYMBOL" for a symbol-labeled line	*
 * J. Wu/SAIC		11/02	add CLASS_LIST				*
 * J. Wu/SAIC		11/02	allow edit on multiple list objects	*
 * J. Wu/SAIC		11/02	add color attr for list			*
 * H. Zeng/XTRIA        01/03   modified arguments to pgwbxw_popup      *
 * H. Zeng/XTRIA	07/03	added volcano element			*
 * H. Zeng/XTRIA	09/03	added ash cloud element			*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		11/03	add JET_ELM				*
 * J. Wu/SAIC		02/04	add GFA_ELM				*
 * B. Yin/SAIC		04/04	add TCA_ELM				*
 * H. Zeng/SAIC		10/04	added two new para. for pgwbxw_popup	*
 * H. Zeng/SAIC		01/06	added call to pgwatch_clrNumCwas()	*
 * E. Safford/SAIC	03/07	call pggfawp_* when GFA' is active	*
 * E. Safford/SAIC	04/07	fix bug in calling GFA' edit      	*
 * J. Wu/SAIC		03/08	use GFA' to edit smears			*
 * S. Jacobs/NCEP	04/08	Handle dotted lines - line type 10	*
 * X.Guo/CWS		01/10	Handle multi select GFA elements	*
 ***********************************************************************/
{
    int		width, size, kpos, oper, symb_code, nelm, np, iret;
    int         inxarry[MAX_GRP_MEM], joffset, ii, grp_typ;
    int		sub, haz, cat, ier,gfatype;
    char        *label_ptr, symb_ptr[] = "SYMBOL", subtype[128];
    float	hdsiz;
    VG_DBStruct text_el;
    Boolean	useGfa = False;
/*---------------------------------------------------------------------*/

    pgactv_getDevPts (&_dcN, &_dcX, &_dcY);
    _vgType = (int)el->hdr.vg_type;

    oper = pgpalw_getCurOperId();
    
    mbotw_mouseSet(LMHINT_MOVEPOINT, MMHINT_DONE);

    switch (el->hdr.vg_class) {

      case CLASS_LINES:
	width = TRUE;
	size = FALSE;
	kpos = FALSE;
	if (el->hdr.vg_type == LINE_ELM) {
/*
 *  _subTyp is contained in the first digit of lintyp
 *  (the 10s digit alters the compression of dashed lines)
 *
 *  Line types that are multiples of 10 are special.
 *  Check for multiples of 10, greater than 0, and set the base
 *  subtype to 10. For all other values find the base subtype
 *  from the line type value.
 */
	    if ( el->elem.lin.info.lintyp > 0 &&
		 (el->elem.lin.info.lintyp % 10) == 0 ) {
		_subTyp = 10;
	    }
	    else {
		_subTyp = el->elem.lin.info.lintyp - 
		    ((el->elem.lin.info.lintyp/10) * 10);
	    }
	}
	else {
	    _pipDr  = el->elem.spl.info.spldir;
	    _subTyp = el->elem.spl.info.spltyp;
	    size    = TRUE;
	    if ( _subTyp == 24 || _subTyp == 25 ) { /* kink line */
	        kpos = TRUE;
	    }
	}

/*
 * Obtain possible label string in the group.
 */
        label_ptr = NULL;
	grp_typ   = (int)el->hdr.grptyp;
        nelm = 0;
        if(el->hdr.grptyp != 0 && el->hdr.grpnum != 0) {
            crg_gginx(el->hdr.grptyp, el->hdr.grpnum, MAX_GRP_MEM, 
                      inxarry, &nelm, &iret );

        }

        for(ii = 0; ii < nelm; ii++) {
            crg_goffset(inxarry[ii], &joffset, &iret );
            cvg_rdrec(cvg_getworkfile(), joffset, &text_el, &iret );

            if ( text_el.hdr.vg_class == CLASS_TEXT ) {
               label_ptr = text_el.elem.spt.text;
               break;
            }
	    else if ( text_el.hdr.vg_class == CLASS_SYMBOLS ||
		      text_el.hdr.vg_class == CLASS_COMSYM ||
		      text_el.hdr.vg_class == CLASS_MARKER ) {
                label_ptr = symb_ptr;
                break;
            }	    
        }

/*
 * Popup line attribute window.
 */
	if (oper == FUNC_MULTISEL) {
	    pgline_popup (_subTyp, (int)el->hdr.vg_type,
			  width, size, kpos, TRUE, (XtCallbackProc)pgedit_multiEditCb,
                          label_ptr, grp_typ, FALSE );
	    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);
	}
	else {
	    pgline_popup (_subTyp, (int)el->hdr.vg_type,
			  width, size, kpos, TRUE, (XtCallbackProc)pgedit_editCb,
                          label_ptr, grp_typ, FALSE );
	}

/*
 * Set line attributes
 */
	if (el->hdr.vg_type == LINE_ELM) {
	    pgline_setAttr (el->hdr.filled, el->hdr.closed,
			    el->hdr.maj_col, el->elem.lin.info.width,
			    (float)0, 1, el->hdr.smooth );
	}
	else {
	    if ( _subTyp == 24 || _subTyp == 25 ) { /* kink line */
	        pgline_setAttr (el->hdr.filled, el->hdr.closed,
			    el->hdr.maj_col, el->elem.spl.info.splwid,
			    el->elem.spl.info.splsiz,
			    el->elem.spl.info.splstr,
			    el->hdr.smooth );	    
	    }	    
	    else {
	        pgline_setAttr (el->hdr.filled, el->hdr.closed,
			    el->hdr.maj_col, el->elem.spl.info.splwid,
			    el->elem.spl.info.splsiz, 1,
			    el->hdr.smooth );
            }
	}

	break;

      case CLASS_CIRCLE:
	width = TRUE;
	_subTyp = el->elem.cir.info.lintyp;

/*
 * Obtain possible label string in the group.
 */
        label_ptr = NULL;
        nelm = 0;
        if(el->hdr.grptyp != 0 && el->hdr.grpnum != 0) {
            crg_gginx(el->hdr.grptyp, el->hdr.grpnum, MAX_GRP_MEM, 
                      inxarry, &nelm, &iret );

        }
        for(ii = 0; ii < nelm; ii++) {
            crg_goffset(inxarry[ii], &joffset, &iret );
            cvg_rdrec(cvg_getworkfile(), joffset, &text_el, &iret );
            if(text_el.hdr.vg_class == CLASS_TEXT) {
               label_ptr = text_el.elem.spt.text;
               break;
            }
        }

/*
 * Popup the circle attribute window.
 */
	if (oper == FUNC_MULTISEL) {
	    pgcirc_popup (_subTyp, (int)el->hdr.vg_type,
			  width, TRUE, (XtCallbackProc)pgedit_multiEditCb, label_ptr);
	}
	else {
	    pgcirc_popup (_subTyp, (int)el->hdr.vg_type, 
			  width, TRUE, (XtCallbackProc)pgedit_editCb, label_ptr);
	}

/*
 * Set the circle attributes.
 */
	pgcirc_setAttr(el->hdr.maj_col, el->elem.cir.info.width);

	mbotw_mouseSet(LMHINT_APPLY, MMHINT_DONE);

	break;

      case CLASS_SYMBOLS:
	symb_code = (int)(el->elem.sym.data.code[0]);
	_subTyp = symb_code;

/*
 * Obtain possible label string in the group.
 */
        label_ptr = NULL;
        nelm = 0;
        if(el->hdr.grptyp != 0 && el->hdr.grpnum != 0) {
            crg_gginx(el->hdr.grptyp, el->hdr.grpnum, MAX_GRP_MEM, 
                      inxarry, &nelm, &iret );

        }
        for(ii = 0; ii < nelm; ii++) {
            crg_goffset(inxarry[ii], &joffset, &iret );
            cvg_rdrec(cvg_getworkfile(), joffset, &text_el, &iret );
            if(text_el.hdr.vg_class == CLASS_TEXT) {
               label_ptr = text_el.elem.spt.text;
               break;
            }
        }

/*
 * Popup the symbol attribute window.
 */
	if (oper == FUNC_MULTISEL || el->hdr.grptyp == GRPTYP_COMSYM) {
	    pgsymb_popup (symb_code, (int)el->hdr.vg_type,
			  TRUE, (XtCallbackProc)pgedit_multiEditCb, label_ptr, FALSE);
	}
	else {
	    pgsymb_popup (symb_code, (int)el->hdr.vg_type, 
			  TRUE, (XtCallbackProc)pgedit_editCb, label_ptr, FALSE);
	}

	pgsymb_setAttr(el->hdr.maj_col, el->elem.sym.info.size,
		       el->elem.sym.info.width );

	pgsymb_setLatLon(el->elem.sym.data.latlon[0], 
			 el->elem.sym.data.latlon[1]);

	mbotw_mouseSet(LMHINT_NOACTION, MMHINT_DONE);

	break;

      case CLASS_FRONTS:

/*
 * Obtain possible label string in the group.
 */
        label_ptr = NULL;
        nelm = 0;
        if(el->hdr.grptyp != 0 && el->hdr.grpnum != 0) {
            crg_gginx(el->hdr.grptyp, el->hdr.grpnum, MAX_GRP_MEM, 
                      inxarry, &nelm, &iret );

        }
        for(ii = 0; ii < nelm; ii++) {
            crg_goffset(inxarry[ii], &joffset, &iret );
            cvg_rdrec(cvg_getworkfile(), joffset, &text_el, &iret );
            if(text_el.hdr.vg_class == CLASS_TEXT) {
               label_ptr = text_el.elem.spt.text;
               break;
            }
        }


	if (oper == FUNC_MULTISEL) {
	    pgfrtw_popup (el->elem.frt.info.fcode,
			  TRUE, TRUE, TRUE, (XtCallbackProc)pgedit_multiEditCb,
                          label_ptr, FALSE );
	    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);
	}
	else {
	    pgfrtw_popup (el->elem.frt.info.fcode,
			  TRUE, TRUE, TRUE, (XtCallbackProc)pgedit_editCb,
                          label_ptr, FALSE );
	}

	_subTyp = el->elem.frt.info.fcode;
	_pipDr  = el->elem.frt.info.fpipdr;

	pgfrtw_setAttr(el->elem.frt.info.fcode, el->elem.frt.info.fpipsz,
		       el->hdr.smooth, el->hdr.maj_col, el->hdr.min_col);

	break;

      case CLASS_WATCHES:

        np = el->elem.wbx.info.numcnty;
        pgwatch_editcnty(4, el, np, el->elem.wbx.info.cn_fips );
	if (oper == FUNC_MULTISEL) {
	    pgwbxw_popup (el->elem.wbx.info.w_style, 
			  el->hdr.maj_col,
			  el->elem.wbx.info.w_shape, 
			  el->elem.wbx.info.w_mrktyp,
			  el->elem.wbx.info.w_mrksiz,
			  el->elem.wbx.info.w_mrkwid,
			  el->hdr.filled,
			  el->hdr.min_col,
			  TRUE, 
			  (XtCallbackProc)pgedit_multiEditCb );

	    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);
	}
	else {
	    pgwlst_setShowFlg (TRUE);
	    pgwatch_clrNumCwas();
	    pgwbxw_setWlst (pgactv_getElmLoc(), FALSE);

	    pgwbxw_popup (el->elem.wbx.info.w_style, 
			  el->hdr.maj_col,
			  el->elem.wbx.info.w_shape, 
			  el->elem.wbx.info.w_mrktyp,
			  el->elem.wbx.info.w_mrksiz,
			  el->elem.wbx.info.w_mrkwid,
			  el->hdr.filled,
			  el->hdr.min_col,
			  TRUE, 
			  (XtCallbackProc)pgedit_editCb );
	}

	_subTyp = el->elem.wbx.info.w_style;

	break;

      case CLASS_WINDS:

	if (el->hdr.vg_type == ARROW_ELM) {
	    _subTyp = OBJ_WINDARRW;
	    hdsiz   = el->elem.wnd.info.hdsiz;
	}
	else if (el->hdr.vg_type == BARB_ELM) {
	    _subTyp = OBJ_WINDBARB;
	    hdsiz   = 0.0F;
	}
	else if (el->hdr.vg_type == DARR_ELM) {
	    _subTyp = OBJ_WINDDARR;
	    hdsiz   = el->elem.wnd.info.hdsiz;
	}
	else if (el->hdr.vg_type == HASH_ELM) {
	    _subTyp = OBJ_WINDHASH;
	    hdsiz   = 0.0F;
	}

	if (oper == FUNC_MULTISEL) {
	    pgwndw_popup (_subTyp, TRUE, (XtCallbackProc)pgedit_multiEditCb);
	    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);
	}
	else {
	    pgwndw_popup (_subTyp, TRUE, (XtCallbackProc)pgedit_editCb);
	}

	pgwndw_setAttr (el->elem.wnd.data.spddir[1],
			el->elem.wnd.data.spddir[0],
			el->elem.wnd.info.size,
			el->elem.wnd.info.width,
			hdsiz, el->elem.wnd.info.wndtyp);
	pgwndw_setColr (el->hdr.maj_col);
	break;

      case CLASS_TEXT:
        if (oper == FUNC_MULTISEL)
            pgtxt_editPopup (el, TRUE, (XtCallbackProc)pgedit_multiEditCb);
        else
            pgtxt_editPopup (el, FALSE, (XtCallbackProc)pgedit_editCb);

	mbotw_mouseSet(LMHINT_APPLY, MMHINT_DONE);

	_subTyp = (el->hdr.vg_type == SPTX_ELM) ? 
	    el->elem.spt.info.sptxtyp : 0;

	break;

      case CLASS_TRACKS:
	pgtrkw_extrapolate (NULL, NULL, el);

	if (oper == FUNC_MULTISEL) {
	    pgtrkw_popup ((int)el->hdr.vg_type, TRUE, (XtCallbackProc)pgedit_multiEditCb);
	    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);
	}
	else {
	    pgtrkw_popup ((int)el->hdr.vg_type, TRUE, (XtCallbackProc)pgedit_editCb);
	}

	_subTyp = el->elem.trk.info.subtype;
	pgtrkw_setAttr (el);
	break;

      case CLASS_SIGMETS:
	if (oper == FUNC_MULTISEL) {
	    if (el->hdr.vg_type == SIGCCF_ELM) {
		pgccfw_popup (el, (XtCallbackProc)pgedit_multiEditCb);
	    }
	    else if (el->hdr.vg_type == VOLC_ELM) {
		pgvolw_editPopup (el);
	    }
	    else if (el->hdr.vg_type == ASHCLD_ELM) {
		pgvacw_popup ( el );
	    }
	    else {
		pgsigw_popup (el, (XtCallbackProc)pgedit_multiEditCb);
	    }

	    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);
	}
	else {
	    if (el->hdr.vg_type == SIGCCF_ELM) {
		pgccfw_popup (el, (XtCallbackProc)pgedit_editCb);
	    }
	    else if (el->hdr.vg_type == VOLC_ELM) {
		pgvolw_editPopup (el);
	    }
	    else if (el->hdr.vg_type == ASHCLD_ELM) {
		pgvacw_popup ( el );
	    }
	    else {
		pgsigw_popup (el, (XtCallbackProc)pgedit_editCb);
	    }
	}

	break;
      
      case CLASS_LIST:

	_subTyp = el->elem.lst.info.subtyp;	

	if ( oper == FUNC_MULTISEL ) {
	    pglist_popup ( _subTyp, TRUE, (XtCallbackProc)pgedit_multiEditCb );
	    mbotw_mouseSet ( LMHINT_TOGGLE, MMHINT_DONE );
        }
	else {
	    pglist_popup ( _subTyp, TRUE, (XtCallbackProc)pgedit_editCb );	
	}
	
	pglist_setAttr ( el->elem.lst.info.mrktyp,
			 el->hdr.maj_col,
			 el->elem.lst.info.mrksiz,
			 el->elem.lst.info.mrkwid );			 
        break;
	
      case CLASS_MET:

	_subTyp = -99;	
        
        if ( el->hdr.vg_type == JET_ELM ) {	
	    if ( oper == FUNC_SELECT ) {
	        pgjet_popup ( (XtCallbackProc)pgedit_editCb );	
	    }
	}
        else if ( el->hdr.vg_type == GFA_ELM ) {	
	    if ( ( oper == FUNC_SELECT ) || (oper == FUNC_MULTISEL ) ) {

	        cvg_getFld ( el, TAG_GFA_SUBTYPE, subtype, &ier );
		if( ier < 0 ) {
		    break;
		}

                useGfa = False;
                sub = atoi( subtype );
                pggfawp_getHazCat( sub, &haz, &cat, &ier );

                if( ier < 0 ) {
                    useGfa = True;
                }

		if( pgpalw_isGFAp_Actv() && !useGfa ) {
		    if ( oper == FUNC_MULTISEL ) {
			/*check GFA and see if it's a snapshot for multi-select*/
			gfatype = atoi( subtype ) - atoi( subtype )/10 * 10;  
	            	if ( gfatype == GFA_SNAPSHOT ) {
			    pggfawp_popup ( el, (XtCallbackProc)pgedit_multiEditCb );
			}
		    }
                    else {
			pggfawp_popup ( el, (XtCallbackProc)pgedit_editCb );
                    }	
		}
		else {
		    if ( oper == FUNC_MULTISEL ) {
			/*check GFA and see if it's a snapshot for multi-select*/
			gfatype = atoi( subtype ) - atoi( subtype )/10 * 10;
			if ( gfatype == GFA_SNAPSHOT ) {
	            	    pggfaw_popup ( el, (XtCallbackProc)pgedit_multiEditCb );
			}
		    }
		    else {
			pggfaw_popup ( el, (XtCallbackProc)pgedit_editCb );
		    }	
		}
	    }
        }
        else if ( el->hdr.vg_type == TCA_ELM ) {	
	    if ( oper == FUNC_SELECT ) {
	        pgtca_popup ( el );	
	    }
        }
		
        break;

      default:
	break;
    }
}

/*=====================================================================*/

Boolean pgedit_isActive ( void )
/************************************************************************
 * pgedit_isActive                                                      *
 *                                                                      *
 * Test to determine if attribute editing has been started for any      *
 * class of VG element.							*
 *                                                                      *
 * Boolean pgedit_isActive( )                                           *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:							*
 *	none								*
 *                                                                      *
 * Return value:							*
 *  pgedit_isActive	Boolean 	Attribute editing started	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       11/98   initial coding                          *
 * W. Li/EAI		12/98	added pgnumb_isUp			*
 * W. Li/EAI		01/99	NxmTxtA_XXX --> pgtxt_XXX		*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * S. Law/GSC		05/99	Added CLASS_TRACKS			*
 * S. Law/GSC		07/99	Added CLASS_SIGMETS			*
 * S. Law/GSC		08/99	pgsigf -> pgsigw			*
 * X. Guo/CWS		01/10   Added pggfaw_isUp and pggfawp_isUp	*
 ***********************************************************************/
{
    if ( pgline_isUp() || pgcirc_isUp() || pgfrtw_isUp() ||
    	 pgwndw_isUp() || pgsymb_isUp() || pgtxt_isUp()  ||
	 pgnumb_isUp() || pgtrkw_isUp() || pgsigw_isUp() ||
         pggfaw_isUp() || pggfawp_isUp()) {

	return TRUE;
    }
    else {
    	return FALSE;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgedit_editCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgedit_editCb                                                        *
 *                                                                      *
 * Callback for the apply/cancel buttons on the attribute edit windows. *
 *                                                                      *
 * void pgedit_editCb ( w, clnt, call )				*
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget          Calling widget                          *
 *  clnt	XtPointer                                               *
 *  call	XtPointer                                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       11/97   copied from _pgevt_modifyStart          *
 * E. Safford/GSC       01/98   fixed pip direction error on fronts     *
 * S. Law/GSC           03/98   nmap_pgwndw_getAttr -> .._getData       *
 * W. Li/EAI            03/98   add symbol editing callback, popdown    *
 * S. Law/GSC           04/98   Updated/added pgline/pgfrtw_getAttr     *
 * E. Safford/GSC       04/98   fixed pip direction error on lines      *
 * F. J. Yen/NCEP       04/98   Updated with new ces function names     *
 * F. J. Yen/NCEP       05/98   Renamed _gemType with _subTyp           *
 * E. Safford/GSC       05/98   update undo                             *
 * S. Law/GSC           05/98   added call to pgevt_unsetOper           *
 * E. Safford/GSC	06/98	moved to nmap_pgedit.c			*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * W. Li/EAI		07/98	Added color for front			*
 * E. Safford		09/98	modify for mode (GRP/OBJ)		*
 * W. Li/EAI		10/98	modify for color edit			*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * S. Law/GSC		11/98	Seperated CLASS_WATCHES from _PRODUCTS	*
 * S. Law/GSC		12/98	Added call to pgwbx_setWlst		*
 * W. Li/EAI		01/99	Added non text value check		*
 * W. Li/EAI		01/99	Changed call _getCurClass to pgtxt_isUp	*
 * W. Li/EAI		03/99	Added latitude/longitude editting	*
 * S. Law/GSC		03/99	Update parameters for pgline_getAttr	*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * S. Law/GSC		05/99	Added CLASS_TRACKS			*
 * S. Law/GSC		07/99	Added CLASS_SIGMETS			*
 * S. Law/GSC		08/99	pgsigf -> pgsigw			*
 * S. Law/GSC		09/99	added call to pgactv_getDevPts		*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * M. Li/GSC		 1/00	Used string variables in gtrans		*
 * S. Law/GSC		02/00	Added CCF				*
 * S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 * S. Law/GSC		03/00	added call to pgccfw_saveEdit		*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * E. Safford/GSC	03/01   removed ces_get on lines, frnts & symbs *
 * E. Safford/GSC	03/01   param change for pgline_getAttr         *
 * E. Safford/GSC	03/01   return ces_get on frnts & symbs         *
 * E. Safford/GSC	04/01   use grpnum & grptyp from deleted elem   *
 * J. Wu/SAIC		10/01   param change for pgline_getAttr         *
 * E. Safford/SAIC	02/02	add pgline_saveAttr()			*
 * J. Wu/SAIC		11/02   add CLASS_LIST				*
 * J. Wu/SAIC		11/02	add color attr for list			*
 * H. Zeng/XTRIA        01/03   modifed arguments to pgwbxw_getAttr     *
 * H. Zeng/XTRIA	02/03   wiped the cnty list for watch shape chng*
 * S. Jacobs/NCEP	 5/03	Removed ces_get from CLASS_WATCHES;	*
 *				Moved reset of w_type if shape changes	*
 * J. Wu/SAIC		11/03	add JET_ELM				*
 * J. Wu/SAIC		12/03	change subtyp for JET_ELM		*
 * J. Wu/SAIC		12/03	use same color for jet's barb & text	*
 * J. Wu/SAIC		02/04	add GFA_ELM				*
 * H.Zeng/SAIC		10/04	added two para. for pgwbxw_getAttr	*
 * J. Wu/SAIC		10/04	free TCA/GFA memory			*
 * H. Zeng/SAIC		12/04	checked cnty lock status		*
 * B. Yin/SAIC		03/05	snap GFA if hour attr is a time span	*
 * H. Zeng/SAIC		01/06	added call to pgwatch_clrNumCwas()	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * M. Li/SAIC		09/06	add increment to pgwndw_getData		*
 * M. Li/SAIC		10/06	add check for wind speed/direction limit*
 * E. Safford/SAIC	03/07	use pggfaw_* when GFA' is active	*
 * E. Safford/SAIC	04/07	fix bug in calling gfa' edit and in	*
 *				  GFA subtype determination		*
 * E. Safford/SAIC	06/07	check return code from pgvgf_saveNewElm	*
 * J. Wu/SAIC		03/08	use GFA' to edit smears			*
 ***********************************************************************/
{
    int		ier, location, shape, el_location, sel, jet_subtyp, incr;
    int		splcol, splwid, brbcol, brbwid, hshcol, hshwid;
    int		f_type, str, front_code, np, ii, grpnum, dummy1, maxspd;
    int		haz, cat;
    float	brbsiz, hshsiz;
    float	wndspd, wnddir;
    float	tempdir;
    float	llx, lly, urx, ury, *plat, *plon, xx[8], yy[8];
    float	new_xx, new_yy, dummy;
    char	text[MAX_TEXT], dummyc, grptyp, smooth, subtype[ 128 ];
    Boolean	not_ccf;
    Boolean	useGfa = False;

    textattrib_t    attribs;
    VG_DBStruct el;
/*---------------------------------------------------------------------*/

    if ((long)clnt == 0) {    /*  Accept selected  */

	if (pgtxt_isUp()){
	    pgtxt_getAttr (&attribs, text);
	    if (strlen(text) <= (size_t)0) {
        	return;
	    }
	}

	el_location = pgactv_getElmLoc();
        pgutls_prepNew (el_location, &el, &llx, &lly, &urx, &ury, &ier);
	grptyp = el.hdr.grptyp;
	grpnum = el.hdr.grpnum;

	not_ccf = (Boolean)((el.hdr.grptyp != GRPTYP_CCF));


        pgundo_newStep();

	if (not_ccf) {
	    pgundo_storeThisLoc(el_location, UNDO_DEL, &ier);
	}

        switch (el.hdr.vg_class) {

	  case CLASS_LINES:
	    if (el.hdr.vg_type == SPLN_ELM) {
		el.elem.spl.info.spldir = _pipDr;
                if ( _subTyp == 24 || _subTyp == 25 ) { /* kink lines */
		    pgline_getAttr (&(el.hdr.filled), &(el.hdr.closed),
				&(el.hdr.maj_col), 
				&(el.elem.spl.info.splwid),
				&(el.elem.spl.info.splsiz),
				&(el.elem.spl.info.splstr),
				&(el.hdr.smooth),
				&(dummyc));
	        }
		else {
		    pgline_getAttr (&(el.hdr.filled), &(el.hdr.closed),
				&(el.hdr.maj_col), 
				&(el.elem.spl.info.splwid),
				&(el.elem.spl.info.splsiz),
				&(dummy1), &(el.hdr.smooth), 
				&(dummyc));		
		}
	    }
	    else {
		pgline_getAttr (&(el.hdr.filled), &(el.hdr.closed),
				&(el.hdr.maj_col), 
				&(el.elem.lin.info.width),
				&(dummy), &(dummy1), &(el.hdr.smooth),
				&(dummyc));
	    }
	    pgline_saveAttr();
	    break;

	  case CLASS_CIRCLE:
	    ces_get (_subTyp, &el, &ier);
	    pgcirc_getAttr(&(el.hdr.maj_col), &(el.elem.cir.info.width));
	    pgcirc_saveAttr();
	    break;

	  case CLASS_SYMBOLS:
	    ces_get (_subTyp, &el, &ier);
	    pgsymb_getColor(&(el.hdr.maj_col));
	    pgsymb_getXxYyCoord(&new_xx, &new_yy);
	    _dcX[0]=new_xx;
	    _dcY[0]=new_yy;
	    pgsymb_saveAttr();
	    break;

	  case CLASS_FRONTS:

/*
 *  ces_get needs the front type, which is the 100s 
 *  digit of the original fcode value, which is stored 
 *  in _subTyp for fronts
 */
  	    pgfrtw_parseFcode (_subTyp, &f_type, &str, &front_code);
	    ces_get (_subTyp, &el, &ier);

	    pgfrtw_getAttr (&(el.hdr.smooth), &(el.hdr.maj_col),
			    &(el.hdr.min_col));
	    pgfrtw_setmajColor();
	    pgfrtw_setminColor();

/*
 *  build new fcode by taking front_code from original
 *  fcode and adding the new strength (width) code to it.
 */

	    el.elem.frt.info.fcode = front_code +
		(el.elem.frt.info.fwidth *10);
	    el.elem.frt.info.fpipdr = _pipDr;

	    break;

	  case CLASS_WATCHES:

	    shape = el.elem.wbx.info.w_shape;
	    pgwbxw_getAttr (&(el.hdr.maj_col), 
			    &(el.elem.wbx.info.w_style),
			    &(el.elem.wbx.info.w_shape), 
			    &(el.elem.wbx.info.w_mrktyp),
                            &(el.elem.wbx.info.w_mrksiz),
			    &(el.elem.wbx.info.w_mrkwid),
			    &(el.hdr.filled),
			    &(el.hdr.min_col)             );

	    if (shape != el.elem.wbx.info.w_shape) {
		np = 8;
		plat = &(el.elem.wbx.latlon[0]);
		plon = &(el.elem.wbx.latlon[np]);

		pgwpts_save (plat, plon);
		pgwpts_get (0, el.elem.wbx.info.w_shape, 
			    plat, plon, plat, plon, &ier);

		gtrans (sys_M, sys_D, &np, plat, plon, xx, yy, 
		        &ier, strlen (sys_M), strlen (sys_D) );
		for (ii = 0; ii < np; ii++) {
		    pgactv_modPt (ii, xx[ii], yy[ii]);
		}

/*
 *  Wipe the county list and reset the watch type
 *  if county lock button is OFF.
 */
                if ( !pgwlst_getCtlkStatus() ) {

	           el.elem.wbx.info.numcnty = 0;
		   el.elem.wbx.info.w_type = UNDWTCH;
                }

	    }

	    break;

	  case CLASS_WINDS:
	    ces_get (NO_VALUE, &el, &ier);
	    pgwndw_getData( &wnddir, &wndspd, &(el.hdr.maj_col), &incr);
	    maxspd = (_subTyp == OBJ_WINDBARB) ? 400 : 200;
	    if ( incr == 1 ) {
		el.elem.wnd.data.spddir[1] += wnddir;
		el.elem.wnd.data.spddir[0] += wndspd;

	        if ( el.elem.wnd.data.spddir[1] > 360.0F )
		    el.elem.wnd.data.spddir[1] = (float)((int)el.elem.wnd.data.spddir[1] % 360);
		if ( el.elem.wnd.data.spddir[1] < 0.0F ) {
		    tempdir = (float)((int)G_ABS(el.elem.wnd.data.spddir[1]) % 360);
		    el.elem.wnd.data.spddir[1] = 360.0F - tempdir;
		}

		if ( el.elem.wnd.data.spddir[0] < 0.0F ) el.elem.wnd.data.spddir[0] = 0.0F;
		if ( el.elem.wnd.data.spddir[0] > maxspd ) el.elem.wnd.data.spddir[0] = (float)maxspd;
	    }
	    else {
		el.elem.wnd.data.spddir[1] = wnddir;
                el.elem.wnd.data.spddir[0] = wndspd;
	    }
	    pgwndw_saveColor();
	    break;

	  case CLASS_TEXT:

	    if ((el.hdr.vg_type == TEXT_ELM) ||
		(el.hdr.vg_type == TEXTC_ELM)) {
		el.elem.txt.info.itxfn    = attribs.gemfont;
		el.elem.txt.info.ithw     = attribs.fontithw;
		el.elem.txt.info.iwidth   = attribs.iwidth;
		el.elem.txt.info.ialign   = attribs.align + 2;
		el.elem.txt.info.rotn     = attribs.frotn;
		el.elem.txt.info.sztext   = attribs.fsize;
		strcpy(el.elem.txt.text, text);
		el.hdr.maj_col = attribs.colr;
		el.hdr.min_col = attribs.colr;
	    }
	    else {
		el.elem.spt.info.sptxtyp  = attribs.sptxtyp;
		el.elem.spt.info.itxfn    = attribs.gemfont;
		el.elem.spt.info.ithw     = attribs.fontithw;
		el.elem.spt.info.iwidth   = attribs.iwidth;
		el.elem.spt.info.ialign   = attribs.align;
		el.elem.spt.info.turbsym  = attribs.turb;
		el.elem.spt.info.rotn     = attribs.frotn;
		el.elem.spt.info.sztext   = attribs.fsize;
		strcpy(el.elem.spt.text, text);
		el.elem.spt.info.txtcol = attribs.colr;
		el.elem.spt.info.lincol = attribs.colr;
		el.elem.spt.info.filcol = attribs.colr;
		el.hdr.maj_col = attribs.colr;
		el.hdr.min_col = attribs.colr;
	    }
	    break;

	  case CLASS_TRACKS:
	    ces_get (_subTyp, &el, &ier);

	    pgtrkw_getAttr (&el);
	    pgtrkw_extrapolate (NULL, NULL, &el);

	    _dcN = 0;
	    break;

	  case CLASS_SIGMETS:
	    ces_get (_subTyp, &el, &ier);

	    if (el.hdr.vg_type == SIGCCF_ELM) {
		pgccfw_getAttr (&el);
	    }
	    else {
		pgsigw_getAttr (&el);
	    }

	    pgactv_getDevPts (&_dcN, &_dcX, &_dcY);

	    break;
	  
	  case CLASS_LIST:
	    ces_get ( _subTyp, &el, &ier );
	    pglist_getAttr ( &(el.elem.lst.info.mrktyp),
	    		     &(el.hdr.maj_col),
	    		     &(el.elem.lst.info.mrksiz),
	    		     &(el.elem.lst.info.mrkwid) );
	    pglist_saveAttr();
	    pglist_setCurList ( &el );
	    
	    break;
	
	  case CLASS_MET:
	    
	    if ( el.hdr.vg_type == JET_ELM ) {
	        pgjet_getAttr ( &splcol, &splwid, &smooth,
	    		    &brbcol, &brbwid, &brbsiz,
	    		    &hshcol, &hshwid, &hshsiz );
	    
	        sel = pgjet_getSelectedSub ();
	        jet_subtyp = pgjet_getCurSubtyp ();
	    
	        if ( jet_subtyp == 0 ) { /* Barb */
	            if ( sel >= 0 ) {
		        el.elem.jet.barb[sel].wndcol = brbcol;
	                el.elem.jet.barb[sel].wnd.info.width = brbwid;
	                el.elem.jet.barb[sel].wnd.info.size = brbsiz;	    
		    
		        el.elem.jet.barb[sel].sptcol = brbcol;
	            }
		    else {
		        for ( ii = 0; ii < el.elem.jet.nbarb; ii++ ) {
		            el.elem.jet.barb[ii].wndcol = brbcol;
	                    el.elem.jet.barb[ii].wnd.info.width = brbwid;
	                    el.elem.jet.barb[ii].wnd.info.size = brbsiz;

		            el.elem.jet.barb[ii].sptcol = brbcol;
		        }
		    }
	        }
	        else if ( jet_subtyp == 1 ) { /* Hash */
	            if ( sel >= 0 ) {
	                el.elem.jet.hash[sel].wndcol = hshcol;
	                el.elem.jet.hash[sel].wnd.info.width = hshwid;
	                el.elem.jet.hash[sel].wnd.info.size = hshsiz;
	            }
		    else {
		        for ( ii = 0; ii < el.elem.jet.nhash; ii++ ) {
		            el.elem.jet.hash[ii].wndcol = hshcol;
	                    el.elem.jet.hash[ii].wnd.info.width = hshwid;
	                    el.elem.jet.hash[ii].wnd.info.size = hshsiz;
		        }
		    }		
	        }
	        else {  /* Line */
	            el.elem.jet.line.splcol = splcol;
		    el.elem.jet.line.spl.info.splwid = splwid;
		    el.hdr.smooth = smooth;
	        }
	    }
	    else if ( el.hdr.vg_type == GFA_ELM ) {

	        cvg_getFld ( &el, TAG_GFA_SUBTYPE, subtype, &ier );		
		if( ier < 0 ) {
		    break;
		}

                useGfa = False;
                _subTyp = atoi( subtype );

                pggfawp_getHazCat( _subTyp, &haz, &cat, &ier );
                if( ier < 0 ) {
                    useGfa = True;
                }
		
		if( pgpalw_isGFAp_Actv() && !useGfa ) {
		    pggfawp_getAttr ( &el );		    
		    pggfawp_checkHours ( &el );
		}
		else {
		    pggfaw_getAttr ( &el );
		    pggfaw_checkHours ( &el );
		}
	    }	    
	    
	    break;
	
	}  /* switch */

/*
 *  ces_get returns the default grptyp for the element.  We don't
 *  want that -- we want what the element _originally_ was.
 */
	el.hdr.grptyp = grptyp;
	el.hdr.grpnum = grpnum;


	if ( el.hdr.vg_type == GFA_ELM ) {
/*
* GFA may have been snapped, so the points in the element 
* could be different from those points in the map.
*/
	   pgvgf_saveNewElm ( NULL, sys_M, &el, el.elem.gfa.info.npts, 
	       		      &(el.elem.gfa.latlon[ 0 ]), 
			      &(el.elem.gfa.latlon[ el.elem.gfa.info.npts ]), 
			      TRUE, &location, &ier);
	}
	else {
	   pgvgf_saveNewElm (NULL, sys_D, &el, _dcN, _dcX, _dcY, 
			  TRUE, &location, &ier);
	}

/*
*  Free TCA_GFA memory.
*/
	if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
        }
	else if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }

        if( ier >= 0 ) {
	    pgutls_redraw (location, &el, &ier);

	    pghdlb_select (&el, location);
	    pgundo_storeThisLoc (location, UNDO_ADD, &ier);
            pgundo_endStep();	    	
	}

/*
*  Free TCA_GFA memory.
*/
	if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
        }
	else if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }
	if (el.hdr.vg_class == CLASS_WATCHES) {
	    pgwatch_clrNumCwas ();
	    pgwbxw_setWlst (location, FALSE);
	}
    }
    else {    /*  Cancel selected  */
        if ( !pgjet_isUp() ) {
	    pgevt_unsetOper (FALSE);

            mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);

	    mbotw_mouseSet(LMHINT_SELECT, MMHINT_TOSELECTOPER);
        }
	else {
	    pgjet_popdownAttr();
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgedit_multiEditCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgedit_multiEditCb                                                   *
 *                                                                      *
 * Callback for the apply/cancel buttons on the attribute edit windows  *
 * when in editing using the multiple select function.                  *
 *                                                                      *
 * void pgedit_multiEditCb ( w, clnt, call )                     	*
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget          Calling widget                          *
 *  clnt	XtPointer                                               *
 *  call	XtPointer                                               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC      03/98   copied from _pgevt_editCb               *
 *  S. Law/GSC          04/98   Updated/added pgline/pgfrtw_getAttr     *
 *  E. Safford/GSC      04/98   added smoothing to lines/fronts         *
 *  F. J. Yen/NCEP      04/98   Updated with new ces function names     *
 *  W. Li/EAI           04/98   Add darr and hash in CLASS_WINDS        *
 *  W. Li/EAI           04/98   Removed stroke from CLASS_LINE          *
 *  F. J. Yen/NCEP      05/98   Removed pip stroke; renamed with subTyp *
 *  E. Safford/GSC      05/98   update undo                             *
 *  S. Law/GSC          05/98   added call to pgevt_unsetOper           *
 *  E. Safford/GSC      06/98   moved to nmap_pgedit.c                  *
 *  E. Safford/GSC      06/98   updated call to pghdlb_deselectEl       *
 *  G. Krueger/EAI	06/98	Uniform status hints			*
 *  W. Li/EAI		07/98	Added color for front			*
 *  E. Safford/GSC	09/98	modify for mode (OBJ/GRP)		*
 *  W. Li/EAI		10/98	modify for color edit			*
 *  E. Safford/GSC	10/98	modify for param change to cvg_rdrec	*
 *  G. Krueger/EAI	10/98	Using table for hints			*
 *  W. Li/EAI		11/98	Added FUNC_NUMB_EDIT 			*
 *  W. Li/EAI		12/98	Added call to  pgutls_isNumber		*
 *  E. Safford/GSC	12/98	change NUMB_EDIT to INC_DEC, cleanup    *
 *  W. Li/EAI		12/98	Added attribute edit for text 		*
 *  W. Li/EAI		01/99	Added text value check for multi_select *
 *  W. Li/EAI		01/99	Changed call _getCurClass to pgtxt_isUp	*
 *  S. Law/GSC		03/99	Updated parameters for pgline_getAttr	*
 *  W. Li/EAI		03/99	Removed number changer type		*
 *  W. Li/EAI		04/99	Modified for Mark color change		*
 *  G. Krueger/EAI	05/99	Added circle draw function		*
 *  S. Law/GSC		05/99	Added CLASS_TRACKS			*
 *  W. Li/EAI		06/99	Removed duplicate selection for INC/DEC	*
 *  S. Law/GSC		06/99	Fixed CLASS_TRACKS			*
 *  S. Law/GSC		07/99	Added CLASS_SIGMETS			*
 *  S. Law/GSC		09/99	Cleaned up CLASS_SIGMETS		*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  S. Law/GSC		02/00	Added CCF				*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 *  H. Zeng/EAI         11/00   changed for the new undo design         *
 *  H. Zeng/EAI         12/00   modified for multiple undo steps        *
 *  J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 *  E. Safford/GSC	03/01	param change for pgline_getAttr		*
 *  M. Li/SAIC		10/01	Added MARKER				*
 *  J. Wu/SAIC		10/01	add param to pgline_getAttr()		*
 *  M. Li/SAIC		11/01	Added ICNG				*
 *  J. Wu/SAIC		12/01	add layer in crg_set() call		*
 *  J. Wu/SAIC		01/02	add layer in crg_get() call		*
 * T. Piper/SAIC	 2/02	fixed memory leak with new_offsets	*
 * J. Wu/SAIC		11/02   add CLASS_LIST				*
 * J. Wu/SAIC		11/02	add color attr for list			*
 * J. Wu/SAIC		03/03	do not change MCLOUD text contents if 	*
 *				selected together with other text types	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * E. Safford/SAIC	06/04	fix LINE & SPLN edit bug		*
 * E. Safford/SAIC	06/04	fix special line pattern size bug	*
 * J. Wu/SAIC         	07/04   add filter param to crg_get		*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 * M. Li/SAIC		09/06	add increment to pgwndw_getData		*
 * M. Li/SAIC		10/06	add check for wind speed/direction limit*
 * M. Li/SAIC		09/07	add  wind attribute editing selections	*	
 * X.Guo/CWS		01/10   add code to handle GFA elements	        *
 ***********************************************************************/
{
    float	llx, lly, urx, ury, dummy;
    float	m_llx, m_lly, m_urx, m_ury;
    float	wind_dir, wind_speed, tempdir, inf_bbox[4];

    int		ier, location, num_selected, *new_offsets, incr;
    int		f_type, str, front_code, el_num, ii, num_elms, dummy1;
    int		new_location, oper, class, prev_el, found, update_crg;
    int		numb_valu, layer, el_layer, multi_sel, mixed_sel, maxspd;

    int		linWidth = -1;
    float	splSize = -1.0F;
    Boolean	edit_flags[10];
    Boolean	w_editflg[6];
    char  	text[MAX_TEXT], dummyc,subtype[ 128 ],value1[64],value2[64],grptyp;
    textattrib_t    attribs;
    VG_DBStruct	el, tmp_el, del_el;
    filter_t	dsplyFilter;
    Boolean     useGfa = False;
    int         sub, haz, cat,grpnum;
    float 	lx, ly, ux, uy;
/*---------------------------------------------------------------------*/

    update_crg = 0;

    if ((long)clnt == 0) {    /*  Accept selected  */

        if (!pghdlb_elemSelected()) {
            return;
	}

/*
 *  set up tmp_el as the template for the update to
 *  all selected elements
 */
	oper = pgpalw_getCurOperId();
        class = pgpalw_getCurClassId ();

        if (class == CLASS_COMSYM || class == CLASS_MARKER) {
            class = CLASS_SYMBOLS;
	}

        tmp_el.hdr.vg_class = (char)class;
        tmp_el.hdr.vg_type  = (char)_vgType;
	
	switch ( class ) {

	  case CLASS_LINES:
	    ces_get (_subTyp, &tmp_el, &ier);

	    if (tmp_el.hdr.vg_type == LINE_ELM) {
		pgline_getAttr (&(tmp_el.hdr.filled), 
				&(tmp_el.hdr.closed),
				&(tmp_el.hdr.maj_col), 
				&(tmp_el.elem.lin.info.width),
				&dummy, &dummy1, &(tmp_el.hdr.smooth), 
				&(dummyc));

	        linWidth = tmp_el.elem.lin.info.width;
	    }
	    else {
		if ( _subTyp == 24 || _subTyp == 25 ) { /* kink lines */
		    pgline_getAttr (&(tmp_el.hdr.filled), 
				&(tmp_el.hdr.closed),
				&(tmp_el.hdr.maj_col), 
				&(tmp_el.elem.spl.info.splwid),
				&(tmp_el.elem.spl.info.splsiz),
				&(tmp_el.elem.spl.info.splstr),
				&(tmp_el.hdr.smooth),
				&(dummyc));

	        }
		else {
		    pgline_getAttr (&(tmp_el.hdr.filled), 
				&(tmp_el.hdr.closed),
				&(tmp_el.hdr.maj_col), 
				&(tmp_el.elem.spl.info.splwid),
				&(tmp_el.elem.spl.info.splsiz),
				&(dummy1),
				&(tmp_el.hdr.smooth),
				&(dummyc));		
		}

	        linWidth = tmp_el.elem.spl.info.splwid;
	        splSize  = tmp_el.elem.spl.info.splsiz;
	    }

	    break;

	  case CLASS_CIRCLE:
	    ces_get(NO_VALUE, &tmp_el, &ier);
	    pgcirc_getAttr(&(tmp_el.hdr.maj_col),
			   &(tmp_el.elem.cir.info.width));
	    pgcirc_saveAttr();
	    break;

	  case CLASS_FRONTS:
	    pgfrtw_parseFcode (_subTyp, &f_type, &str, &front_code);
	    ces_get (f_type, &tmp_el, &ier);
	    pgfrtw_getAttr (&(tmp_el.hdr.smooth), &(tmp_el.hdr.maj_col),
			    &(tmp_el.hdr.min_col));
	    pgfrtw_setmajColor();
	    pgfrtw_setminColor();
	    break;

	  case CLASS_WINDS:
	    ces_get(NO_VALUE, &tmp_el, &ier);
	    pgwndw_getData (&wind_dir, &wind_speed, 
			    &(tmp_el.hdr.maj_col), &incr);
	    pgwndw_saveColor();
	    pgwndw_getEditFlag(w_editflg);
	    break;

	  case CLASS_TEXT:
	    if (pgtxt_isUp()){
		pgtxt_getAttr(&attribs, text);
		if (strlen(text) <= (size_t)0) {
		    return;
		}
		pgtxt_getEditFlag(edit_flags);
	        pgtxt_getSelMcloud ( &multi_sel, &mixed_sel );
	    }
	    break;

	  case CLASS_SYMBOLS:
	  case CLASS_COMSYM:
	  case CLASS_MARKER:
	    ces_get (_subTyp, &tmp_el, &ier);
	    pgsymb_getColor(&(tmp_el.hdr.maj_col));
	    pgsymb_saveAttr();
	    break;

	  case CLASS_SIGMETS:
	    ces_get (_subTyp, &tmp_el, &ier);

	    if (_vgType == SIGCCF_ELM) {
		pgccfw_getAttr (&tmp_el);
	    }
	    else {
		pgsigw_getAttr (&tmp_el);
	    }
	    break;
	  
	  case CLASS_LIST:
	    pglist_getAttr ( &(tmp_el.elem.lst.info.mrktyp),
	    		     &(tmp_el.hdr.maj_col),
	    		     &(tmp_el.elem.lst.info.mrksiz),
	    		     &(tmp_el.elem.lst.info.mrkwid) );
	    pglist_saveAttr();
	    
	    break;
        }

/*
 *  process selected elements
 */
        m_llx = 999999.0F;
        m_lly = 999999.0F;
        m_urx = 0.0F;
        m_ury = 0.0F;

        num_selected = pghdlb_elemSelected();
        new_offsets = (int *) malloc ((size_t)num_selected * sizeof(int));
        prev_el = -1;

	num_elms = 0;

        pgundo_newStep();
	layer = pglayer_getCurLayer( );
        for (ii = 0; ii < num_selected; ii++) {
            pghdlb_getNextIndex(prev_el, &el_num, &location, &ier);
            cvg_rdrec(cvg_getworkfile(), location, &el, &ier);


	    if (el.hdr.grptyp != GRPTYP_COMSYM) {

	        pgactv_setActvElm (&el, location);
	        pgactv_getDevPts (&_dcN, &_dcX, &_dcY);

	        crg_get (el_num, &el_layer, dsplyFilter, &llx, &lly, &urx, &ury, &ier);
                if (llx < m_llx) m_llx = llx;
                if (lly < m_lly) m_lly = lly;
                if (urx > m_urx) m_urx = urx;
                if (ury > m_ury) m_ury = ury;

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
		    m_llx = G_MIN(m_llx, inf_bbox[0]);
		    m_lly = G_MIN(m_lly, inf_bbox[2]);
		    m_urx = G_MAX(m_urx, inf_bbox[1]);
		    m_ury = G_MAX(m_ury, inf_bbox[3]);
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
		/*refresh selected GFA element*/		
		if ( el.hdr.vg_class == CLASS_MET && el.hdr.vg_type == GFA_ELM ) {
		    pgutls_prepNew ( location, &del_el, &lx, &ly, &ux, &uy, &ier );
                    if ( del_el.hdr.vg_type == TCA_ELM ) {
                    	cvg_freeBkpts ( &del_el );
                    }
                    else if ( del_el.hdr.vg_type == GFA_ELM ) {
                    	cvg_freeElPtr ( &del_el );
                    }
		}
	        cvg_delet (cvg_getworkfile(), location, TRUE, &ier);
	        pghdlb_deselectEl (el_num, TRUE);
	        crg_clear (el_num, &ier);
 
                pgundo_storeThisLoc (location, UNDO_DEL, &ier);

                switch (el.hdr.vg_class) {
		  case CLASS_LINES:

		    el.hdr.filled  = tmp_el.hdr.filled;
		    el.hdr.closed  = tmp_el.hdr.closed;
		    el.hdr.maj_col = tmp_el.hdr.maj_col;
		    el.hdr.smooth  = tmp_el.hdr.smooth;

		    if (el.hdr.vg_type == LINE_ELM) {
			el.elem.lin.info.width = linWidth;
		    }
		    else {
			el.elem.spl.info.splwid = linWidth;

			if ( splSize >= 0 ) {  
			    el.elem.spl.info.splsiz = splSize;
                        }
		    }
		    break;

		  case CLASS_CIRCLE:
		    el.elem.cir.info.width = tmp_el.elem.cir.info.width;
		    el.hdr.maj_col         = tmp_el.hdr.maj_col;
		    break;

		  case CLASS_FRONTS:
		    pgfrtw_parseFcode (el.elem.frt.info.fcode, &f_type,
				       &str, &front_code);

		    el.hdr.maj_col = tmp_el.hdr.maj_col;
		    el.hdr.min_col = tmp_el.hdr.min_col;
		    el.hdr.smooth = tmp_el.hdr.smooth;
		    el.elem.frt.info.fwidth = tmp_el.elem.frt.info.fwidth;
		    el.elem.frt.info.fpipsz = tmp_el.elem.frt.info.fpipsz;

		    el.elem.frt.info.fcode = front_code +
			(el.elem.frt.info.fwidth *10);
		    break;
               
		  case CLASS_WINDS:
		    if ((el.hdr.vg_type == ARROW_ELM &&
			 tmp_el.hdr.vg_type == ARROW_ELM) ||
			(el.hdr.vg_type == DARR_ELM &&
			 tmp_el.hdr.vg_type == DARR_ELM)) {
			if (w_editflg[W_A_SIZ])
			    el.elem.wnd.info.hdsiz = tmp_el.elem.wnd.info.hdsiz;
		    }

		    if (w_editflg[W_WIDTH])
		        el.elem.wnd.info.width = tmp_el.elem.wnd.info.width;
		    if (w_editflg[W_SIZE])
		        el.elem.wnd.info.size  = tmp_el.elem.wnd.info.size;

	    	    maxspd = (_subTyp == OBJ_WINDBARB) ? 400 : 200;
		    if ( incr == 1 ) {
			if (w_editflg[W_SPD])
		            el.elem.wnd.data.spddir[0] += wind_speed;
			if (w_editflg[W_DIR])
		            el.elem.wnd.data.spddir[1] += wind_dir;

			if ( el.elem.wnd.data.spddir[1] > 360.0F )
                    	    el.elem.wnd.data.spddir[1] = (float)((int)el.elem.wnd.data.spddir[1] % 360);
                	if ( el.elem.wnd.data.spddir[1] < 0.0F ) {
                    	    tempdir = (float)((int)G_ABS(el.elem.wnd.data.spddir[1]) % 360);
                    	    el.elem.wnd.data.spddir[1] = 360.0F - tempdir;
                	}

		        if ( el.elem.wnd.data.spddir[0] < 0.0F ) el.elem.wnd.data.spddir[0] = 0.0F;
		 	if ( el.elem.wnd.data.spddir[0] > maxspd ) el.elem.wnd.data.spddir[0] = (float)maxspd;
		    }
		    else {
			if (w_editflg[W_SPD])
			    el.elem.wnd.data.spddir[0] = wind_speed;
			if (w_editflg[W_DIR])
                            el.elem.wnd.data.spddir[1] = wind_dir;
                    }
		    if (w_editflg[W_COLOR])
			el.hdr.maj_col = tmp_el.hdr.maj_col;
		    break;
 
		  case CLASS_TEXT:
		    if (oper == FUNC_INC_DEC){	
			pgnumb_getNumb(&numb_valu);

			if (el.hdr.vg_type == SPTX_ELM) {
			    sprintf(text, "%i", (atoi(el.elem.spt.text) +
						 numb_valu));
			    strcpy (el.elem.spt.text, text);
			}
			else if ((el.hdr.vg_type == TEXT_ELM) || 
				 (el.hdr.vg_type == TEXTC_ELM)) {
			    sprintf(text, "%i", (atoi(el.elem.txt.text) +
						 numb_valu));
			    strcpy (el.elem.txt.text, text);
			}

		    } 
		    else if ((el.hdr.vg_type == TEXT_ELM) ||
			     (el.hdr.vg_type == TEXTC_ELM)) {
			if (edit_flags[FONT] || edit_flags[STYLE]){
			    el.elem.txt.info.itxfn    = attribs.gemfont;
			    el.elem.txt.info.ithw     = attribs.fontithw;
			    el.elem.txt.info.iwidth   = attribs.iwidth;
			}
			if (edit_flags[ALIGN]) {
			    el.elem.txt.info.ialign   = attribs.align + 2;
			}
			if (edit_flags[ROTN]) {
			    el.elem.txt.info.rotn     = attribs.frotn;
			}
			if (edit_flags[SIZE]) {
			    el.elem.txt.info.sztext   = attribs.fsize;
			}
			if (edit_flags[TEXT]) {
			    strcpy(el.elem.txt.text, text);
			}
			if (edit_flags[COLOR]){
			    el.hdr.maj_col = attribs.colr;
			    el.hdr.min_col = attribs.colr;
			}
		    }
		    else {

			if (edit_flags[BOX]) {
			    el.elem.spt.info.sptxtyp = attribs.sptxtyp; 
			}
			if (edit_flags[FONT] || edit_flags[STYLE]){
			    el.elem.spt.info.itxfn   = attribs.gemfont;
			    el.elem.spt.info.ithw    = attribs.fontithw;
			    el.elem.spt.info.iwidth  = attribs.iwidth;
			}

			if (edit_flags[ALIGN]) {
			    el.elem.spt.info.ialign  = attribs.align;
			}

			if (edit_flags[TURB] || edit_flags[ICNG]){
			    el.elem.spt.info.turbsym = attribs.turb;
			    el.elem.spt.info.sptxtyp = attribs.sptxtyp;
			}

			if (edit_flags[TEXT]) {
			    if ( el.elem.spt.info.sptxtyp != 15 ) 
			        strcpy(el.elem.spt.text, text);
			}
			
			if (edit_flags[ROTN]) {
			    el.elem.spt.info.rotn    = attribs.frotn;
			}
			if (edit_flags[SIZE]) {
			    el.elem.spt.info.sztext  = attribs.fsize;
			}
			if (edit_flags[COLOR]){
			    el.elem.spt.info.txtcol  = attribs.colr;
			    el.elem.spt.info.lincol  = attribs.colr;
			    el.elem.spt.info.filcol  = attribs.colr;
			    el.hdr.maj_col	       = attribs.colr;
			    el.hdr.min_col	       = attribs.colr;
			}
		    }
		    break;

		  case CLASS_SYMBOLS:
		    if (oper != FUNC_INC_DEC) {
			el.elem.sym.info.width = tmp_el.elem.sym.info.width;
			el.elem.sym.info.size  = tmp_el.elem.sym.info.size;
			el.hdr.maj_col         = tmp_el.hdr.maj_col;
		    }
		    else  {
			el.hdr.maj_col = RED_COLOR;
			el.hdr.min_col = RED_COLOR; 
		    } 

		    break;

		  case CLASS_TRACKS:
		    pgtrkw_getAttr (&el);
		    pgtrkw_extrapolate (NULL, NULL, &el);

		    _dcN = 0;
		    break;

		  case CLASS_SIGMETS:
		    el.hdr.maj_col = tmp_el.hdr.maj_col;

		    if (el.hdr.vg_type == SIGCCF_ELM) {
			el.elem.ccf.info.subtype = 
			    tmp_el.elem.ccf.info.subtype;
			el.elem.ccf.info.cover = 
			    tmp_el.elem.ccf.info.cover;
			el.elem.ccf.info.tops = 
			    tmp_el.elem.ccf.info.tops;
			el.elem.ccf.info.prob = 
			    tmp_el.elem.ccf.info.prob;
			el.elem.ccf.info.growth = 
			    tmp_el.elem.ccf.info.growth;
			el.elem.ccf.info.spd = 
			    tmp_el.elem.ccf.info.spd;
			el.elem.ccf.info.dir = 
			    tmp_el.elem.ccf.info.dir;
		    }
		    else {
			el.elem.sig.info.subtype = 
			    tmp_el.elem.sig.info.subtype;
			el.elem.sig.info.distance = 
			    tmp_el.elem.sig.info.distance;
			el.elem.sig.info.sol = tmp_el.elem.sig.info.sol;
		    }

		    break;

                    case CLASS_LIST:
		        el.elem.lst.info.mrktyp = tmp_el.elem.lst.info.mrktyp;
		        el.hdr.maj_col = tmp_el.hdr.maj_col;
		        el.elem.lst.info.mrksiz = tmp_el.elem.lst.info.mrksiz;
		        el.elem.lst.info.mrkwid = tmp_el.elem.lst.info.mrkwid;
		        pglist_setCurList ( &el );
		    
		    break;
          	    case CLASS_MET:

                	if ( el.hdr.vg_type == GFA_ELM ) {
			    cvg_getFld ( &el, TAG_GFA_SUBTYPE, subtype, &ier );
			    if ( ier < 0 ) {
				break;
			    }    
                	    useGfa = False;
                	    sub = atoi( subtype );
                	    pggfawp_getHazCat( sub, &haz, &cat, &ier );
                	    if( ier < 0 ) {
                    		useGfa = True;
                	    }
			    cvg_getFld ( &el, TAG_GFA_LAT, value1, &ier );
			    cvg_getFld ( &el, TAG_GFA_LON, value2, &ier );	    
                	    grptyp = el.hdr.grptyp;
			    grpnum = el.hdr.grpnum;
			    if( pgpalw_isGFAp_Actv() && !useGfa ) {
                        	pggfawp_getAttr ( &el );
                        	pggfawp_checkHours ( &el );
                    	    }
                    	    else {
                        	pggfaw_getAttr ( &el );
                        	pggfaw_checkHours ( &el );
                   	    }
                            el.hdr.grptyp = grptyp;
                            el.hdr.grpnum = grpnum;
                            cvg_setFld ( &el, TAG_GFA_LAT, value1, &ier );
                            cvg_setFld ( &el, TAG_GFA_LON, value2, &ier );     
                	}
            		break;

		}   /* switch */

/*
 * Add the element, but don't place it until later since
 * we have more than one element to deal with
 */
        	if ( el.hdr.vg_type == GFA_ELM ) {
/*
* GFA may have been snapped, so the points in the element 
 could be different from those points in the map.
*/
           	    pgvgf_saveNewElm ( cvg_getworkfile(), sys_M, &el, el.elem.gfa.info.npts,
                                  	&(el.elem.gfa.latlon[ 0 ]),
                              		&(el.elem.gfa.latlon[ el.elem.gfa.info.npts ]),
                              		FALSE, &new_location, &ier);
		}
		else {
                    pgvgf_saveNewElm (cvg_getworkfile(), sys_D, &el, _dcN, _dcX, 
				  	_dcY, FALSE, &new_location, &ier);
		}
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
                    m_llx = G_MIN(m_llx, inf_bbox[0]);
                    m_lly = G_MIN(m_lly, inf_bbox[2]);
                    m_urx = G_MAX(m_urx, inf_bbox[1]);
                    m_ury = G_MAX(m_ury, inf_bbox[3]);
                    update_crg = 1;
                }

		if (update_crg == 0) {
                    crg_set (&el, new_location, layer, &ier);
		}

	        new_offsets[num_elms] = new_location;
                num_elms++;
                pgundo_storeThisLoc (new_location, UNDO_ADD, &ier);
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

	    prev_el = el_num;

        }       /* for */

        pgundo_endStep();

	if (oper == FUNC_INC_DEC){
	    cvg_rfrsh (NULL, m_llx, m_lly, m_urx, m_ury, &ier);
	}
	else {
            for (ii = 0; ii < num_elms; ii++) {
		pghdlb_setSelect (new_offsets[ii]);
	    }

            cvg_rfrsh (NULL, m_llx, m_lly, m_urx, m_ury, &ier);
            pghdlb_displayAllSel();

	}
	free (new_offsets);    

/*
 * If we may have impacted other elements with placement
 * we will need to rebuild the range records
 */
	if (update_crg) {
	    crg_rebuild();
	}

	if ( class > 0 ) {
	    if (oper == FUNC_INC_DEC) {
	        mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_APPLY);
	    }
	    else {
	        mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);
	    }
	} 
	else {
	    mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
	}
    }
    else {                               /*  Cancel selected  */
        pgevt_unsetOper (FALSE);
	mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_NOACTION);
        mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
    }
}
