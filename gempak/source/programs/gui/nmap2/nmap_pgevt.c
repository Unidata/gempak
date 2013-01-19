#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmTxt.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "hints.h"
#include "proto_xw.h"

#define TIE_DIST_LINES		(300.0F)


#define MAX_ALLOW_PTS  (MAXPTS - 1)  /* functional limit for lines/fronts */
#define NONE_CURRENT   0             /* no current selection */

#define STNMDL	       10	     /* this corresponds to the STNMDL
					entry in the grptyp.tbl.  Any change
					to the STNMDL value on that table
					will necessitate a change to this
					define statement	       */

#define _one 1
#define KINKLN1		24	/* identity of kink lines */
#define KINKLN2		25
static Boolean _windPlActv;

/*
 * Private Callback Functions
 */
void    pgevt_confirmCancelCb	( Widget, XtPointer, XtPointer );
void	pgevt_mvcpHdl		( Widget, XtPointer, XEvent*, Boolean* );
void    pgevt_flipHdl		( Widget, XtPointer, XEvent*, Boolean* );
void    pgevt_txtEditHdl	( Widget, XtPointer, XEvent*, Boolean* );
void    pgevt_extrapHdl		( Widget, XtPointer, XEvent*, Boolean* );
void	pgevt_listEditHdl	( Widget, XtPointer, XEvent*, Boolean* );
 
/************************************************************************
 * nmap_pgevt.c                                                         *
 *									*
 * This module contains the event_handling/callback functions that are  *
 * hung from the mouse during product generation			*
 *									*
 * CONTENTS:                                                            *
 * pgevt_locateElmCb()	  callback for locate element			*
 * pgevt_selectElmEh()    event handler for SELECT when GROUP is active *
 * pgevt_selectHdl()	  event handler for SELECT mode			*
 * pgevt_multiSelHdl()    event handler for MULTIPLE SELECT mode        *
 * pgevt_extrapHdl()	  event handler for EXTRAP mode			*
 * pgevt_flipHdl()	  event handler for FLIP mode			*
 * pgevt_mvcpHdl()	  event handler for MOVE and COPY modes		*
 * pgevt_rotateHdl()	  event handler for ROTATE mode			*
 * pgevt_radiusHdl()	  event handler for RADIUS mode			*
 * pgevt_txtEditHdl()	  event handler for TEXT EDIT mode		*
 * pgevt_listEditHdl()	  event handler for LIST EDIT mode		*
 * pgevt_changeType()	  change element type				*
 * pgevt_ungroup()	  break up selected group(s)			*
 * pgevt_unsetOper()	  various disarm and popdowns calls		*
 * pgevt_setWindPlActv()  set value of _windPlActv flag			*
 * pgevt_confirmCancelCb()callback for cancel button of confirm. window *
 ***********************************************************************/

/*=====================================================================*/
/* ARGSUSED */
void pgevt_locateElmCb ( Widget w, XtPointer clnt, XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgevt_locateElmCb							*
 *									*
 * This function locates elements based on the active lyaer, class and 	*
 * function, then places handlebars on it.				*
 *                                                                      *
 * pgevt_locateElmCb ( w, clnt, event, ctdr )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	w			Widget					*
 *	clnt			XtPointer				*
 *	*event			XEvent					*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         4/97   Created.                                *
 * E. Wehner/EAi         8/97   SLide functions for symbols consolidt	*
 * E. Safford/GSC        9/97   Replaced grP->cmd with cmd_ routines 	*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * E. Wehner/EAi	 9/97	Removed GrInfo record			*
 * C. Lin/EAI	        10/97	rename from NxmDrawLocate, cleanup	*
 * C. Lin/EAI	        10/97	add WBOX_ELEM				*
 * C. Lin/EAI	        11/97	cleanup					*
 * E. Safford/GSC       11/97   add MODIFY function                   	*
 * E. Safford/GSC       12/97   add attrib edit setup			*
 * E. Safford/GSC       12/97   modify for initial wind rotation	*
 * E. Safford/GSC       01/98   modify for closed line modification     *
 * E. Safford/GSC       02/98   modify for undo reset                   *
 * C. Lin/EAI	        02/98	add event handler for text edit		*
 * E. Safford/GSC       02/98	add partial delelete function  		*
 * E. Safford/GSC       03/98	remove undo initialization     		*
 * E. Safford/GSC       03/98	add multiple select            		*
 * W. Li/EAI		03/98	modify for select symbol editing	*
 * E. Safford/GSC       04/98	modify to use class any        		*
 * C. Lin/EAI       	04/98	add FUNC_GROUP with FUNC_MULTISEL       *
 * E. Safford/GSC       04/98	add param to hdlb_select calls 		*
 * E. Safford/GSC       04/98	mod to handle CLASS_COMSYM 		*
 * S. Law/GSC		04/98	add FUNC_COPY				*
 * E. Safford/GSC	06/98	add else if for group popdowns		*
 * E. Safford/GSC	06/98	add check on attrib edit for FUNC_GROUP	*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * S. Law/GSC		06/98	added call to pggst_setText		*
 * C. Lin/EAI		08/98	remove  move with SELECT vect, sym...   *
 * S. Law/GSC		08/98	added FUNC_DELPOINT			*
 * C. Lin/EAI		09/98	modify for new logic in GROUP/UNGROUP   *
 * S. Law/GSC		09/98	added FUNC_CONNECT			*
 * C. Lin/EAI		09/98	use M mouse button finish GROUP   	*
 * E. Safford/GSC	09/98	mod for mode function & clean up       	*
 * D.W.Plummer/NCEP      9/98   added pgwbxw_setAttr and pgwbxw_spts    *
 * G. Krueger/EAI	09/98	Remove redundant MBOTW_ACTIONSETs	*
 * G. Krueger/EAI	10/98	Allow GROUP cancel			*
 * W. Li/EAI		10/98	renamed pgdel_eraseAll->pgdel_deletAll	*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * E. Safford/GSC       11/98   fix inconsistant text ghosting          *
 * E. Safford/GSC	11/98	mod for gempak level combosymbols	*
 * W. Li/EAI		11/98	added FUNC_NUMB_EDIT			*
 * S. Law/GSC		11/98	seperated CLASS_WATCHES from _PRODUCTS	*
 *				and moved pgwbxw_spts -> pgwpts_save	*
 * S. Law/GSC		11/98	prevent edit popup during group mode	*
 * W. Li/EAI		12/98	added call to pgutls_isNumber		*
 * E. Safford/GSC	12/98	don't use setText on group move/copy	*
 * E. Safford/GSC	12/98	move FUNC_NUMB_EDIT to multiEditHdl	*
 * S. Law/GSC		01/99	added check watchbox format window	*
 * S. Law/GSC		01/99	removed unneeded pgwlist_update calls	*
 * D.W.Plummer/NCEP	 4/99	remove call to pgwlst_update		*
 * S. Law/GSC		04/99	added call to pgwatch_save		*
 * G. Krueger/EAI	 5/99	Added circle draw function		*
 * S. Law/GSC		05/99	added CLASS_TRACKS			*
 * E. Safford/GSC	06/99	mod FUNC_DEL_OBJ hints & exit      	*
 * S. Law/GSC		07/99	added CLASS_SIGMETS			*
 * S. Law/GSC		09/99	added check for SIGTYP_ISOL		*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * E. Safford/GSC	11/99	updated for new xwcmn.h			*
 * E. Safford/GSC	12/99	added OBJ_WATCHLN special case		*
 * S. Law/GSC		12/99	added watch status checks		*
 * E. Safford/GSC	12/99	fix watch attribute popup problem	*
 * S. Law/GSC		01/00	removed call to pgwatch_savecnty	*
 * S. Law/GSC		02/00	added CCF				*
 * H. Zeng/EAI          03/00   added confirmation window for WATCH     *
 * S. Law/GSC		03/00	added CCF warning			*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * J. Wu/GSC		03/01	added EXIT for actions via middle mouse *
 * M. Li/SAIC		10/01	Added CLASS_MARKER			*
 * J. Wu/SAIC		10/01	forbid MODIFY/CONNECT for OBJ_KINKLN1,2	*
 * J. Wu/SAIC		10/01	Initialize "is_kink" to False		*
 * J. Wu/SAIC		01/02	select only the elem. on current layer	*
 * M. Li/SAIC		04/02	Removed FUNC_LABEL			*
 * E. Safford/SAIC	04/02	check ier from cvgscan to avoid UMR	*
 * J. Wu/SAIC           10/02   add FUNC_EXTRAP  			*
 * J. Wu/SAIC           11/02   add CLASS_LIST & allow MULTISEL on it	*
 * H. Zeng/XTRIA        01/03   added arguments to pgwbxw_setAttr	*
 * H. Zeng/XTRIA	07/03	added volcano element			*
 * J. Wu/SAIC           10/03   add CLASS_MET -> JET_ELM		*
 * H. Zeng/XTRIA	10/03   added ash cloud element			*
 * H. Zeng/XTRIA	11/03   modified warning meg when doing copy	*
 * H. Zeng/XTRIA	01/04	added more ash cloud types		*
 * J. Wu/SAIC           02/04   add CLASS_MET -> GFA_ELM		*
 * B. Yin/SAIC          04/04   add CLASS_MET -> TCA_ELM                *
 * J. Wu/SAIC           04/04   remove pgevt_listEditHdl		*
 * B. Yin/SAIC          07/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   Changed pgtca_freeBkpts to cvg_freeBkpts*
 * H. Zeng/SAIC		11/04	changed para. list for pgwbxw_setAttr	*
 * J. Wu/SAIC           11/04   free GFA block memory			*
 * m.gamazaychikov/SAIC 11/04   changed call pgwbxw_setUpWtchLn to 	*
 *				pgwbxw_updtWcnFlag			*
 * T. Piper/SAIC	10/05	declared data long			*
 * X. Guo/CWS		01/10   add FUNC_ADDPOINT			*
 ***********************************************************************/
{
    float	xx, yy;
    int		iclass, iopr, vg_type, vg_class, obj_id, xoff, yoff;
    int		ier, selected, nearest, cur_layer;
    long	data;
    char  	draw_mode;
    char	warn_mes[] = 
                "Volcanoes cannot be moved or copied.";
    char	wtch_msg[] = 
    		"You need to choose an issued watch for the status line.";
    char        cfm_mesg[] = "Watch has no counties!\n"
                             "Set default counties?" ;
    char        ccf_mesg[] = "Changes to the text attributes can be made here.\nChanges to the text itself should be made in the CCF element." ;

    Boolean	ln_frt, el_grpd, is_kink;
    Widget      draw_w;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
    
    iopr = pgpalw_getCurOperId(); 
    is_kink = False;
    
    cur_layer = pglayer_getCurLayer ();         
    if (event->xbutton.button == Button1 && iopr != FUNC_DEL_OBJ) {
/*
 * get device coordinates
 */
	xgtoff (&xoff, &yoff, &ier);
        xx = (float) (event->xbutton.x + xoff);
        yy = (float) (event->xbutton.y + yoff);

/* 
 * get current operation ID and class ID
 */
	iclass = pgpalw_getCurClassId();
        if (iclass == NONE_CURRENT) return;

        if (iclass == CLASS_COMSYM || iclass == CLASS_MARKER) {
            iclass = CLASS_SYMBOLS;
	}

/* 
 * Set up a masking element.   A Masking element says that each 
 * element returned must have the same data in fields that are 
 * used for the masking.  Initially, this is only used for the 
 * "hdr.vg_class" field, but could be used for any other field
 * in the VG_DBStruct record.
 */
	el.hdr.vg_class = iclass;
	cvg_scan (NULL, cur_layer, (char) iclass, xx, yy, 0,
                  &el, &selected, &nearest, &ier);

        if ( ier >= 0 ) {

	    if (el.hdr.vg_class == CLASS_LINES || 
	        el.hdr.vg_class == CLASS_FRONTS||
		el.hdr.vg_type == JET_ELM ||
		el.hdr.vg_type == GFA_ELM  ) {     
	        ln_frt = TRUE;
	        if ( el.hdr.vg_type == SPLN_ELM ) {
	            if ( el.elem.spl.info.spltyp == KINKLN1 || 
	                 el.elem.spl.info.spltyp == KINKLN2 ) {
		        is_kink = True;
		    }
	        }
	    }
	    else if (el.hdr.vg_class == CLASS_SIGMETS) {

	        if (el.hdr.vg_type == SIGCCF_ELM) {
		    ln_frt = TRUE;
	        }
		else if (el.hdr.vg_type == VOLC_ELM) {
		    ln_frt = FALSE;
		}
		else if (el.hdr.vg_type == ASHCLD_ELM) {
		    ln_frt = (el.elem.ash.info.subtype == ASHCLD_AREA || 
                              el.elem.ash.info.subtype == ASHCLD_LINE   );
		}
	        else {
		    ln_frt = (el.elem.sig.info.subtype != SIGTYP_ISOL);
	        }
	    }
	    else {
	        ln_frt = FALSE;
	    }

/*
 *  el_grpd indicates the grouped status of the element, while
 *  draw_mode indicates the OBJ/GRP selection in the pg gui. 
 */
	    el_grpd = (el.hdr.grpnum && el.hdr.grptyp && 
				el.hdr.grptyp != GRPTYP_WATCH);
	    draw_mode = pgpalw_getMode();
	    
/* 
 * set VG type group 
 */
	    vg_type = el.hdr.vg_type;
	    vg_class = el.hdr.vg_class;

	    pgactv_setActvElm (&el, selected);

	    if (el.hdr.vg_type == CIRCLE_ELM) {
		nearest = 0;
	    }

	    pgactv_setNearPt  (nearest);

	    mcanvw_disarmPress();

            switch (iopr) {

	      case FUNC_SELECT:
		pghdlb_select( &el, selected);

		if (pgwfmt_isUp ()) {
		    mcanvw_setPressFunc((XtEventHandler)&pgwfmt_selectEh, CURS_DEFAULT); 
		    mbotw_mouseSet(LMHINT_NOACTION, MMHINT_DONE);
                    if (el.elem.wbx.info.numcnty != 0) {
		        pgwatch_save (&el);
		        pgwfmt_setWatch (&el);
                    }
                    else {
	                draw_w = (Widget)mcanvw_getDrawingW();
                        data = 3;
	                NxmConfirm_show(draw_w, cfm_mesg, 
                                     (XtCallbackProc)pgwlst_cmOptCb,
                                     pgevt_confirmCancelCb,
                                     (XtPointer)data, &ier);        
                    }

		}
		else if (pgwsmw_isUp ()) {
		    if (el.elem.wbx.info.w_issued == 0) {
			NxmWarn_show (mcanvw_getDrawingW(), 
				      "This watch has not been issued!");

			pghdlb_deselectAll ();
			mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, 
					     CURS_DEFAULT);
		    }
		    else {
			mcanvw_setPressFunc((XtEventHandler)&pgwsmw_selectEh, CURS_DEFAULT); 
			mbotw_mouseSet(LMHINT_NOACTION, MMHINT_DONE);
			pgwatch_save (&el);
			pgwsmw_setWatch (&el);
		    }
		}
		else if ((draw_mode == TYPE_OBJ) ||
			 (draw_mode == TYPE_GRP && el_grpd)) {
		    obj_id = pgpalw_getCurObjId();

		    if (draw_mode == TYPE_OBJ) {
		    	if ( vg_class != CLASS_WATCHES ||
			    (vg_class == CLASS_WATCHES && 
				obj_id != OBJ_WATCHLN) )  {
			    pgedit_editStart (&el); 
			}
		    }

/* 
 *  set press function 
 */
		    if (!el_grpd) {
		        switch (vg_class) {
			  case CLASS_LINES:
			  case CLASS_FRONTS:
			  case CLASS_PRODUCTS:
			  case CLASS_WATCHES:
			  case CLASS_TRACKS:
			  case CLASS_SIGMETS:
			  case CLASS_MET:

			    if (vg_class == CLASS_WATCHES && 
			    		obj_id == OBJ_WATCHLN) {

  				if (el.elem.wbx.info.w_issued < WATCH_ISSUED) {
				    NxmWarn_show (w, wtch_msg);
				    pghdlb_deselectAll();
				    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, 
				    				CURS_DEFAULT);
				}
				else {
				    pgwbxw_updtWcnFlag(selected);
				}
			    }
			    else if (vg_class == CLASS_SIGMETS && 
			    	     vg_type  == VOLC_ELM ) {

			        mcanvw_setPressFunc((XtEventHandler)&pgevt_selectHdl, 
						CURS_DEFAULT); 
			        mbotw_mouseSet(LMHINT_NOACTION, MMHINT_DONE);
			    }
			    else if (vg_class == CLASS_SIGMETS && 
			    	     vg_type  == ASHCLD_ELM    &&
			             el.elem.ash.info.subtype 
						!= ASHCLD_AREA &&
			             el.elem.ash.info.subtype 
						!= ASHCLD_LINE    ) {

			        mcanvw_setPressFunc((XtEventHandler)&pgevt_selectHdl, 
						CURS_DEFAULT); 
			        mbotw_mouseSet(LMHINT_NOACTION, MMHINT_DONE);
			    }
                            else if ( vg_type == TCA_ELM ){}
			    else {
				mcanvw_setPressFunc((XtEventHandler)&pgevt_selectHdl, 
						    CURS_DEFAULT); 
				mbotw_mouseSet(LMHINT_MOVEPOINT,
					       MMHINT_DONE);
			    }
			    break;

			  case CLASS_TEXT:
			    mcanvw_setPressFunc((XtEventHandler)&pgevt_txtEditHdl, 
						CURS_DEFAULT);
			    mbotw_mouseSet(LMHINT_APPLY, MMHINT_DONE);

			    break;

			  case CLASS_CIRCLE:
			    mcanvw_setPressFunc((XtEventHandler)&pgevt_selectHdl, 
						CURS_DEFAULT); 
			    mbotw_mouseSet(LMHINT_MOVEPOINT,
					   MMHINT_DONE);

			    break;
			  
			  case CLASS_LIST:			    
	                    pglist_setCurList ( &el );

			    break;

			  default:			    
			    mcanvw_setPressFunc((XtEventHandler)&pgevt_selectHdl, 
						CURS_DEFAULT); 
			    mbotw_mouseSet(LMHINT_NOACTION, MMHINT_DONE);

			    break;
		        }
		    }
		    else {
			if (el.hdr.grptyp == GRPTYP_CCF &&
			    el.hdr.vg_class == CLASS_TEXT && 
			    strstr (el.elem.spt.text, "Hght:") != NULL) {
			    NxmWarn_show (w, ccf_mesg);
			}

			if ( vg_type != TCA_ELM ) {
  		           mcanvw_setPressFunc((XtEventHandler)&pgevt_selectHdl, 
					    CURS_DEFAULT); 
		           mbotw_mouseSet(LMHINT_NOACTION, MMHINT_DONE);
			}
		    }
  		}   			/* sel_match */
  		else {
		    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, 
					CURS_DEFAULT);
		}   

		break;

	      case FUNC_MULTISEL:
		if ( (draw_mode == TYPE_OBJ) ||
		     (draw_mode == TYPE_GRP && el_grpd) ) {

		    if (vg_type == WBOX_ELM )  {
                        pgwbxw_setAttr (el.hdr.maj_col,
                                        el.elem.wbx.info.w_style,
                                        el.elem.wbx.info.w_shape,
					el.elem.wbx.info.w_mrktyp,
					el.elem.wbx.info.w_mrksiz,
					el.elem.wbx.info.w_mrkwid,
					el.hdr.filled,
					el.hdr.min_col		   );
		    }
		    else if (iclass != CLASS_ANY && draw_mode == TYPE_OBJ)
		        pgedit_editStart (&el);

		    mcanvw_setPressFunc((XtEventHandler)&pgevt_multiSelHdl, 
					CURS_DEFAULT);
		    pghdlb_select(&el, selected);

		    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);
		}
		else {
		    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, 
					CURS_DEFAULT);
		}

		break;

	      case FUNC_GROUP:
	      case FUNC_UNGROUP:
		if (vg_type == WBOX_ELM ) {
                    pgwbxw_setAttr (el.hdr.maj_col,
                                    el.elem.wbx.info.w_style,
                                    el.elem.wbx.info.w_shape,
				    el.elem.wbx.info.w_mrktyp,
				    el.elem.wbx.info.w_mrksiz,
				    el.elem.wbx.info.w_mrkwid,
				    el.hdr.filled,
				    el.hdr.min_col	       );
                     pgwpts_save (&(el.elem.wbx.latlon[0]),
                                 &(el.elem.wbx.latlon[el.elem.wbx.info.numpts]));
		}
		else if (iclass != CLASS_ANY && !el_grpd 
					&& iopr != FUNC_GROUP
					&& iopr != FUNC_UNGROUP) {
		    pgedit_editStart (&el);
		}

		mcanvw_setPressFunc((XtEventHandler)&pgevt_multiSelHdl, CURS_DEFAULT);

		if ( iopr == FUNC_UNGROUP ) {
		    if ( el.hdr.grpnum && 
				el.hdr.grptyp != GRPTYP_COMSYM ) {
		         pghdlb_select(&el, selected);

		         mbotw_mouseSet(LMHINT_CONFIRM, MMHINT_DONE);
		    }
		    else
		         mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
		}
		else {
		    if ( iopr == FUNC_GROUP ) pghdlb_deselectAll ();

		    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);
		    pghdlb_select(&el, selected);
		}

		break;

	      case FUNC_MOVE:
	      case FUNC_COPY:
		if (((draw_mode == TYPE_OBJ) ||
		     (draw_mode == TYPE_GRP && el_grpd)) &&
		      el.hdr.vg_class != CLASS_LIST &&
	  	      el.hdr.vg_type != TCA_ELM ) {

/*
 * volcano elements cannot be moved or copied. A
 * warning message will pop up.
 */
		    if ( el.hdr.vg_type == VOLC_ELM ) {

			 NxmWarn_show(mcanvw_getDrawingW(), warn_mes);
			 mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, 
							CURS_DEFAULT);
			 break; /* leave case MOVE&COPY */

		    }


		    if (el.hdr.vg_type == WBOX_ELM ) {
                        pgwbxw_setAttr (el.hdr.maj_col,
                                        el.elem.wbx.info.w_style,
                                        el.elem.wbx.info.w_shape,
					el.elem.wbx.info.w_mrktyp,
					el.elem.wbx.info.w_mrksiz,
					el.elem.wbx.info.w_mrkwid,
				        el.hdr.filled,
					el.hdr.min_col		   );
		    }

		    pghdlb_select( &el, selected);

		    mcanvw_setPressFunc((XtEventHandler)&pgevt_mvcpHdl, CURS_DEFAULT);

		    if (el.hdr.vg_class == CLASS_TEXT && !el_grpd) {
		        pggst_setText (&el);
		    }

		    pgrad_setCircPlActv (False);
		    if ( el.hdr.vg_class == CLASS_CIRCLE &&
		    	 ! (draw_mode == TYPE_GRP && el_grpd) ) {
		        pggst_setCircle (&el);
		    }
		    mbotw_mouseSet(LMHINT_DRAG, MMHINT_DONE);
   	 	}
		else {
		    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
		} 

		break;

	      case FUNC_FLIP:
		if ((ln_frt || vg_class == CLASS_WATCHES) && 
		    draw_mode == TYPE_OBJ &&
		    el.hdr.vg_type != TCA_ELM ) {
                   
		    pghdlb_select( &el, selected);
		    mcanvw_setPressFunc((XtEventHandler)&pgevt_flipHdl, CURS_DEFAULT);

		    mbotw_mouseSet(LMHINT_FLIP, MMHINT_TOSELECTOPER);
		}
		else {
		    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
		}
		break;

	      case FUNC_MODIFY:
		if (ln_frt && draw_mode == TYPE_OBJ && !is_kink
		    && el.hdr.vg_type != TCA_ELM ) {
		    pghdlb_select( &el, selected);

		    mcanvw_setPressFunc((XtEventHandler)&pgmdfy_modifyStartEh, CURS_DEFAULT); 

		    mbotw_mouseSet(LMHINT_START, MMHINT_DONE);
		}
		else {
		    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
		}
		break;

	      case FUNC_DELETE:
		if ((draw_mode == TYPE_OBJ) ||
		    (draw_mode == TYPE_GRP && el_grpd)) {
		    pghdlb_select( &el, selected);
		    mcanvw_setPressFunc((XtEventHandler)&pgdel_deletEh, CURS_DEFAULT);

		    if ( iclass > 0 ) 
			mbotw_mouseSet(LMHINT_DELETE, MMHINT_DONE);
		}
		else {
		    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
		}
		break;

	      case FUNC_ROTATE:
		if ((vg_class == CLASS_WINDS || 
		     vg_class == CLASS_TEXT) && draw_mode == TYPE_OBJ
		     && el.hdr.vg_type != TCA_ELM ) {

		    pghdlb_select( &el, selected);
		    _windPlActv = False; 

    		    mcanvw_setPressFunc((XtEventHandler)&pgevt_rotateHdl, CURS_DEFAULT);

		    mbotw_mouseSet(LMHINT_ROTATE, MMHINT_TOSELECTOPER);
		}
		else {
		    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
		}

		break;

	      case FUNC_PARTDELETE:
		if (ln_frt && draw_mode == TYPE_OBJ
		    && el.hdr.vg_type != TCA_ELM ) {
		    pghdlb_select( &el, selected);
		    pgpdel_startPDel (&el);
		}
		else {
		    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
		}
		break; 

	      case FUNC_DELPOINT:
		if (ln_frt && draw_mode == TYPE_OBJ
		    && el.hdr.vg_type != TCA_ELM ) {
		    pghdlb_select( &el, selected);
		    pgdelpt_start (&el);
		}
		else {		    
		    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
		}
		break;

	      case FUNC_CONNECT:
		if (ln_frt && draw_mode == TYPE_OBJ && !is_kink
		    && el.hdr.vg_type != TCA_ELM ) {
		    pghdlb_select( &el, selected);
		    pgconn_start (&el, xx, yy);
		}
		else {
		    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
		}
		break;
	      
	      case FUNC_EXTRAP:

		if ( ( ( draw_mode == TYPE_OBJ ) ||
		       ( draw_mode == TYPE_GRP && el_grpd ) ) &&
		         el.hdr.vg_class != CLASS_LIST        &&
			 el.hdr.vg_type  != TCA_ELM ) {
                  
		    if ( el.hdr.vg_type == WBOX_ELM ) {
                        pgwbxw_setAttr ( el.hdr.maj_col,
                                         el.elem.wbx.info.w_style,
                                         el.elem.wbx.info.w_shape,
					 el.elem.wbx.info.w_mrktyp,
					 el.elem.wbx.info.w_mrksiz,
					 el.elem.wbx.info.w_mrkwid,
					 el.hdr.filled,
					 el.hdr.min_col		   );
		    }

		    pghdlb_select( &el, selected );

		    mcanvw_setPressFunc ( (XtEventHandler)&pgevt_extrapHdl, CURS_DEFAULT ); 

		    if ( el.hdr.vg_class == CLASS_TEXT && !el_grpd ) {
		        pggst_setText ( &el );
		    }

	            if ( pgextrap_getMode() == 0 ) {
	                mbotw_mouseSet ( LMHINT_COPY, MMHINT_EXIT );
	            }
	            else {
	                mbotw_mouseSet ( LMHINT_MOVE, MMHINT_EXIT );		    
	            }
		    
   	 	}
		else {
		    mcanvw_setPressFunc ( (XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT );
		}
		
		break;	      
              case FUNC_ADDPOINT:
                if (ln_frt && draw_mode == TYPE_OBJ
                    && el.hdr.vg_type != TCA_ELM ) {
                    pghdlb_select( &el, selected);
                    pgaddpt_start (&el);
                }
                else {
                    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
                }
                break;
	    
	    } /* switch */

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
    }
    else if (event->xbutton.button == Button2) {
        pgevt_unsetOper (TRUE);
    }          
    else {
	pgevt_unsetOper (FALSE);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgevt_selectElmEh ( Widget w, XtPointer clnt, XEvent *event, 
							Boolean *ctdr )
/************************************************************************
 * pgevt_selectElmEh							*
 *									*
 * This function select elements based on the active layer & class. 	*
 * This is the handler for FUNC_SELECT when GROUP is active.		*
 *                                                                      *
 * pgevt_selectElmEh ( w, clnt, event, ctdr )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	w			Widget					*
 *	clnt			XtPointer				*
 *	*event			XEvent					*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          09/01   initial coding                          *
 * J. Wu/SAIC           01/02   select only the elem. on current layer  *
 * T. Piper/SAIC	02/04	removed unused variable iopr		*
 * J. Wu/SAIC           03/04   block selection of GFA_ELM into group	*
 * J. Wu/SAIC           11/04   free GFA block memory			*
 ***********************************************************************/
{
    float	xx, yy;
    int		iclass, xoff, yoff, ier, selected; 
    int         err_code, ignore, nearest, cur_layer;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
               
    cur_layer = pglayer_getCurLayer ();
    if (event->xbutton.button == Button1) {

/*
 * get device coordinates
 */
	xgtoff (&xoff, &yoff, &ier);
        xx = (float) (event->xbutton.x + xoff);
        yy = (float) (event->xbutton.y + yoff);

/* 
 * Get current class ID.
 */
	iclass = pgpalw_getCurClassId();
        if (iclass == NONE_CURRENT) return;

        if (iclass == CLASS_COMSYM) {
            iclass = CLASS_SYMBOLS;
	}

/* 
 * Set up a masking element.   A Masking element says that each 
 * element returned must have the same data in fields that are 
 * used for the masking.  Initially, this is only used for the 
 * "hdr.vg_class" field, but could be used for any other field
 * in the VG_DBStruct record.
 */
	el.hdr.vg_class = iclass;
	cvg_scan (NULL, cur_layer, (char) iclass, xx, yy, 0,
                  &el, &selected, &nearest, &ier);

        if (ier >= 0) {

/*
 * Add the selected element into the active GROUP.
 */
	    if (  el.hdr.vg_class != CLASS_WATCHES &&
                  el.hdr.vg_class != CLASS_TRACKS  &&
                  el.hdr.vg_class != CLASS_SIGMETS &&
                  el.hdr.vg_type != GFA_ELM &&
                  el.hdr.grptyp   <  90            &&
                 (el.hdr.grptyp != (char)pggrpw_getGrpType() ||
                  el.hdr.grpnum !=       pggrpw_getGrpNum()    )
               ) {

                pghdlb_deselectGrp( (char)pggrpw_getGrpType(),
                                          pggrpw_getGrpNum()   );
                pggrpw_addtoGrp( &el, selected, &ier );
                pghdlb_selectGrp( (char)pggrpw_getGrpType(),
                                        pggrpw_getGrpNum()   );

		mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT); 

            }
            else if ( el.hdr.grptyp >= 90 ) {
                err_code = 4;
                er_wmsg ("pgen", &err_code, NULL, &ignore, 4, 0);
	        NxmErr_update(); 
            }
            else if ( el.hdr.vg_class == CLASS_SIGMETS ) {
                err_code = 3;
                er_wmsg ("pgen", &err_code, NULL, &ignore, 4, 0);
	        NxmErr_update(); 
            }
            else if ( el.hdr.vg_class == CLASS_TRACKS ) {
                err_code = 2;
                er_wmsg ("pgen", &err_code, NULL, &ignore, 4, 0);
	        NxmErr_update(); 
            }
            else if ( el.hdr.vg_class == CLASS_WATCHES ) {
                err_code = 1;
                er_wmsg ("pgen", &err_code, NULL, &ignore, 4, 0);
	        NxmErr_update(); 
            }

	} /* if (ier >=0... */

    } 
    else if ( event->xbutton.button == Button2 ) {

        if ( pghdlb_elemSelected() >= 1 ) {

/*
 * If there is a selected group on screen, first deselect it.
 */
             pghdlb_deselectGrp( (char)pggrpw_getGrpType(),
                                       pggrpw_getGrpNum()   );

             pggrpw_startGrp();	     
        }
        else {

/*
 * Use the middle mouse button to exit group mode.
 */
             pgpalw_inactvGrp();
             pgpalw_setCurOper( (Widget) NULL );
             pgpalw_setupOper();
             pgpalw_classPopdown ();
        }

    }  
    else {
	pgevt_unsetOper (TRUE);
    }
    
/*
 *  Free TCA/GFA memory.
 */
    if ( el.hdr.vg_type == TCA_ELM ) {
        cvg_freeBkpts ( &el );
    }
    else if ( el.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &el );
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgevt_selectHdl ( Widget w, XtPointer clnt, XEvent *event,
						Boolean *ctdr )
/************************************************************************
 * pgevt_selectHdl                                                      *
 *                                                                      *
 * Event handler for SELECT state (LINES, FRONTS, WATCHES, and CIRCLES	*
 * only). 								*
 *                                                                      *
 *                                                                      *
 * void pgevt_selectHdl( w, clnt, event, ctdr)				*
 *                                                                      *
 * Input parameters:                                                    *
 *  w		Widget		Calling widget				*
 *  clnt	XtPointer                                               *
 *  event       XEvent *                                                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Wehner/Eai		Initial coding				*
 *  E. Safford/GSC	09/97	replaced grP->cmd with cmd_ routines    *
 *  D.W.Plummer/NCEP	09/97	Combine into NxmDraw for new vgstruct.h	*
 *  E. Wehner/EAi	09/97	Remove graphics info record		*
 *  C. Lin/EAi	 	10/97	rename from NxmDrawMoveCb, cleanup      *
 *  C. Lin/EAi	 	10/97	add OBJ_WBOX processing      		*
 *  E. Safford/GSC	12/97	add popdowns to attrib edit windows     *
 *  W. Li/EAI		03/98	add popdown to symbol edit windows	*
 *  E. Safford/GSC	04/98	no drag/drop for grouped elements       *
 *  S. Law/GSC		05/98	replaced pgpalw_select with _setupOper	*
 *  S. Law/GSC		05/98	added call to pgevt_unsetOper		*
 *  S. Law/GSC		05/98	added check for pghot_getElmLoc		*
 *  D.W.Plummer/NCEP	06/98	changes for pgram watch box		*
 *  G. Krueger/EAI	06/98	Uniform status hints			*
 *  E. Safford/GSC	07/98	change pgmodv_start call		*
 *  C. Lin/EAI		08/98	modify to handle non-vertex-mod class 	*
 *  D.W.Plummer/NCEP	09/98	added pgwbxw_setAttr and pgwbxw_spts	*
 *  G. Krueger/EAI	09/98	Remove redundant MBOTW_ACTIONSETs	*
 *  G. Krueger/EAI	10/98	Using table for hints			*
 *  S. Law/GSC		11/98	moved pgwbxw_spts -> pgwpts_save	*
 *  D.W.Plummer/NCEP	04/99	remove call to pgwlst_update		*
 *  G. Krueger/EAI	05/99	Added circle draw function		*
 *  S. Law/GSC		05/99	added CLASS_TRACKS			*
 *  S. Law/GSC		07/99	added CLASS_SIGMETS			*
 *  S. Law/GSC		09/99	added SIGTYP_ISOL check			*
 *  E. Safford/GSC	11/99	updated for new xwcmn.h			*
 *  S. Law/GSC		02/00	added CCF				*
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 *  E. Safford/GSC	02/01	add check on cvg_rdsel return code	*
 *  J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 *  E. Safford/GSC	05/01	added param to cvg_rdsel		*
 *  H. Zeng/XTRIA	01/03   added arguments to pgwbxw_setAttr       *
 *  H. Zeng/XTRIA	07/03	added volcano element			*
 *  J. Wu/SAIC		10/03	add JET_ELM				*
 *  H. Zeng/XTRIA	10/03	added ash cloud element			*
 *  H. Zeng/XTRIA	01/04   added more ash cloud types		*
 *  J. Wu/SAIC		02/04	add GFA_ELM				*
 *  H. Zeng/SAIC	10/04	changed para. list for pgwbxw_setAttr	*
 *  J. Wu/SAIC		11/04	free TCA/GFA memory			*
 ***********************************************************************/
{
    float	xx, yy, distance;
    int		selected, nearest, ier, xoff, yoff;
    char        vgclass;
    Boolean     not_group, is_line, area_line;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
/*
 * get device coordinates
 */
    xgtoff (&xoff, &yoff, &ier);
    xx = (float) (event->xbutton.x + xoff);
    yy = (float) (event->xbutton.y + yoff);

    if (event->xbutton.button == Button1) {
        
/* 
 * set nearest point of element 
 */
        selected = pgactv_getElmLoc();
	if (selected == -1) {
	    return;
	}

        cvg_rdsel(NULL, selected, xx, yy, &nearest, &distance, &el, &ier);

/*
 *  Here are the requirements for this oddity:  If the 
 *  element class is CLASS_LINES, then allow vertex dragging for 
 *  great whopping distances.  If the class is any other, then the 
 *  vertex drag must start from virtually on a vertex.
 *
 *  cvg_rdsel returns an code in ier of 1 if the distance to the 
 *  nearest point is determined to be greater than the VERTEX_TIEIN 
 *  value (pgprm.h).  So, we need to figure out if we have a ier 
 *  code > 0, and then what the class type is, and if it's CLASS_LINE, 
 *  then check to see if the actual distance is within the 
 *  TIE_DIST_LINES tolerance.
 */
	if ( ier < 0 ) {

/*
 *  Free TCA_GFA memory.
 */
	    if ( el.hdr.vg_type == TCA_ELM ) {
                cvg_freeBkpts ( &el );
            }
	    else if ( el.hdr.vg_type == GFA_ELM ) {
                cvg_freeElPtr ( &el );
            }
	    
	    return;
	}

  	if ( ier > 0 ) {
	    is_line = el.hdr.vg_class == CLASS_LINES;

	    if ( (!is_line) || (is_line && distance > TIE_DIST_LINES) ) {
		return;
	    }
	}

        pgactv_setNearPt(nearest);

	vgclass = el.hdr.vg_class;

  	if (pgpalw_getMode() == TYPE_OBJ && el.hdr.grptyp != GRPTYP_COMSYM) {
	    not_group = TRUE;
	}
	else {
	    not_group = (!el.hdr.grpnum && !el.hdr.grptyp);
	}

/*
 * if it is OBJ_WBOX, set the hotlist
 */
    	if ((int) el.hdr.vg_type == WBOX_ELM ) {
            pgwbxw_setAttr (el.hdr.maj_col,
			    el.elem.wbx.info.w_style,
			    el.elem.wbx.info.w_shape,
			    el.elem.wbx.info.w_mrktyp,
			    el.elem.wbx.info.w_mrksiz,
			    el.elem.wbx.info.w_mrkwid,
			    el.hdr.filled,
			    el.hdr.min_col             );
            pgwpts_save (&(el.elem.wbx.latlon[0]),
                         &(el.elem.wbx.latlon[el.elem.wbx.info.numpts]));
	}

        area_line = el.elem.ash.info.subtype == ASHCLD_AREA ||
		    el.elem.ash.info.subtype == ASHCLD_LINE;

  	if ( not_group && (vgclass == CLASS_LINES || 
		           vgclass == CLASS_FRONTS ||
		           vgclass == CLASS_TRACKS ||
		           (vgclass == CLASS_SIGMETS &&
			    el.hdr.vg_type == SIGCCF_ELM) ||
		           (vgclass == CLASS_SIGMETS &&
			    el.hdr.vg_type == ASHCLD_ELM && area_line) ||
		           (vgclass == CLASS_SIGMETS &&
			    el.hdr.vg_type != VOLC_ELM &&
			    el.elem.sig.info.subtype != SIGTYP_ISOL) ||
		           vgclass == CLASS_WATCHES ||
			   vgclass == CLASS_CIRCLE ||  
			   (int)el.hdr.vg_type == JET_ELM ||
			   (int)el.hdr.vg_type == GFA_ELM ) 
			   ) {

	    if ( vgclass == CLASS_WATCHES &&
		el.elem.wbx.info.w_style == PGRAM ) {
            	pgmodv_start(1, 0, el.hdr.closed);
            }  
            else if ( vgclass == CLASS_CIRCLE ) {
            	pgmodv_start(2, 0, 0);
	    }
            else {
                pgmodv_start(0, el.hdr.smooth, el.hdr.closed);
            }	    
	    mbotw_mouseSet(LMHINT_MOVEPOINT, MMHINT_DONE);	    
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
    }  
    else {
	pgevt_unsetOper (TRUE);
	if (pgpalw_getCurClassId() > 0) {
	    mbotw_mouseSet(LMHINT_SELECT, MMHINT_NOACTION);
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgevt_multiSelHdl ( Widget w, XtPointer clnt, XEvent *event,
						Boolean *ctdr )
/************************************************************************
 * pgevt_multiSelHdl                                                    *
 *                                                                      *
 * Event handler for MULTIPLE SELECT state.                             *
 *                                                                      *
 *                                                                      *
 * void pgevt_multiSelHdl( w, clnt, event, ctdr)                      *
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget          Calling widget                          *
 *  clnt	XtPointer                                               *
 *  event       XEvent *                                                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC      03/98   initial coding                          *
 *  E. Safford/GSC      04/98   modify scan operation                   *
 *  E. Safford/GSC      04/98	add param to hdlb_select calls 		*
 *  S. Law/GSC		05/98	replaced pgpalw_multiSelect w/ setupOper*
 *  S. Law/GSC		05/98	added call to pgevt_unsetOper		*
 *  E. Safford/GSC      06/98   updated pghdlb_deselectEl call          *
 *  G. Krueger/EAI	06/98	Uniform status hints			*
 *  C. Lin/EAI		09/98	new logic (UN)GROUP,take comsym as obj  *
 *				remove mbotw_actionSet			*
 *  C. Lin/EAI		09/98	use M mouse button to finish GROUP  	*
 *  E. Safford/GSC      09/98   modify for mode (OBJ/GRP)               *
 *  G. Krueger/EAI	10/98	Allow GROUP cancel			*
 *  G. Krueger/EAI	10/98	Using table for hints			*
 *  E. Safford/GSC	11/98	add multi-select by drag		*
 *  E. Safford/GSC	11/98	fix edit window start problem		*
 *  S. Law/GSC		11/98	call to pghdlb_deselectAll if grouping	*
 *  W. LI/EAI		12/98	added hint for number editor		*
 *  E. Safford/GSC	12/98	make INC_DEC work with non-special text *
 *  E. Safford/GSC	12/98	make INC_DEC exit correctly via MB2     *
 *  W. Li/EAI		01/99	added NULL string check			*
 *  E. Safford/GSC	02/99	added INC_DEC toggle for STNMDL grps    *
 *  W. Li/EAI		03/99	added deselection for Inc/Dec Button2	*
 *  W. Li/EAI		04/99	modified for Mark color 		*
 *  E. Safford/GSC	11/99	updated for new xwcmn.h			*
 *  S. Law/GSC		04/00	set num_str if MARK_ELM			*
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 *  H. Zeng/EAI		11/00   changed cvg_rdrec() parameters		*
 *  J. Wu/GSC		03/01	added "EXIT" hint for UNGROUP           *
 *  M. Li/SAIC          10/01   Added CLASS_MARKER                      *
 *  H. Zeng/EAI         10/01   revised GROUP functionality             *
 *  J. Wu/SAIC          01/02   select only the elem. on current layer	*
 *  H. Zeng/EAI         04/02   removed pggrpw_reset()                  *
 *  T. Lee/SAIC		11/03   added user directory to work_file       *
 *  T. Lee/SAIC		11/03	used cvg_getworkfile			*
 *  J. Wu/SAIC          03/04   block GROUP action on GFA_ELM		*
 *  B. Yin/SAIC         08/04   Added code to free TCA memory           *
 *  B. Yin/SAIC         08/04   Changed pgtca_freeBkpts to cvg_freeBkpts*
 *  J. Wu/SAIC          10/04   free GFA block memory			*
 ***********************************************************************/
{
    char        sel, grptyp, draw_mode, *num_str;
    float       xx, yy;
    int         grpnum, selected, nearest, iclass, iopr, el_num;
    int		nexp, nelm, inxarry[MAX_EDITABLE_ELEMS], ii, rng_loc;
    int		xoff, yoff, ier, fl_pos, err_code, ignore, cur_layer;
    Boolean	not_group, stn_mdl;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    cur_layer = pglayer_getCurLayer ();
    if (event->xbutton.button == Button1) {

/*
 * get device coordinates
 */
	xgtoff (&xoff, &yoff, &ier);
        xx = (float) (event->xbutton.x + xoff); 
        yy = (float) (event->xbutton.y + yoff); 

/*
 * get current operation ID and class ID
 */
        iclass = pgpalw_getCurClassId();
        if (iclass == NONE_CURRENT) return;

        if (iclass == CLASS_COMSYM || iclass == CLASS_MARKER)
            iclass = CLASS_SYMBOLS;

	el.hdr.vg_class = iclass;

        cvg_scan (NULL, cur_layer, (char) iclass, xx, yy, 0,
                  &el, &selected, &nearest, &ier);

	if (ier >= 0) {

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
 *  toggle STNMDL groups on/off as a unit
 */
	    if (el.hdr.grptyp == STNMDL) {
		stn_mdl = TRUE;
	        crg_ggnel(el.hdr.grptyp, el.hdr.grpnum, &nexp, &ier);
	        crg_gginx(el.hdr.grptyp, el.hdr.grpnum,
			    	nexp, inxarry, &nelm, &ier);
		sel = 0;
		for (ii=0; ii < nelm && !sel; ii++) {
		    crg_gsel(inxarry[ii], &sel, &ier);
		}
	    }
	    else {		/* not a STNMDL */
		nelm    = 1;
	        stn_mdl = FALSE;
	        crg_getinx (selected, &el_num, &ier);
	        crg_gsel (el_num, &sel, &ier);
            }

            iopr = pgpalw_getCurOperId();

	    if (sel) {		/* deselect element */
		if (ier >= 0) {
		    if ( iopr == FUNC_UNGROUP ) {
           		pgevt_ungroup();
		    }		   
		    else if (iopr == FUNC_INC_DEC) {

/*
 *  check for STNMDL elements and deselect all within each group
 */
			crg_goffset(el_num, &fl_pos, &ier);
			cvg_rdrec (cvg_getworkfile(), fl_pos, &el, &ier);

			if ( stn_mdl ) {

			    rng_loc = inxarry[0];
			}
			else {
			    rng_loc = el_num;
			}

			for (ii=0; ii<nelm-1; ii++) {
			    pghdlb_deselectEl( rng_loc, FALSE );

/*  
 *  get next rng_loc in the STNMDL group
 */			
			    if (ii+1 < nelm) {
			    	rng_loc = inxarry[ii+1];
			    }
			}

/*
 *  TRUE in this last call updates the display
 */
			pghdlb_deselectEl( rng_loc, TRUE );

			pghdlb_displayAllSel ();

			if ( pghdlb_elemSelected() > 0 )
			    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_APPLY);
			else{
			     mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);
			     pgnumb_updateBtns (FALSE);
			}
		    
/*
 *  Free GFA block memory
 */
			if ( el.hdr.vg_type == GFA_ELM ) {
                            cvg_freeElPtr ( &el );
                        }  			   
		    }
		    else {

/*
 * If currently in a GROUP process, remove the element from the group.
 */
		        if ( pgpalw_isGrpActv() ) {

                             pghdlb_deselectGrp( (char)pggrpw_getGrpType(),
                                                       pggrpw_getGrpNum()  );
                             pggrpw_rmvfrmGrp( &el, selected, &ier );
                             pghdlb_selectGrp( (char)pggrpw_getGrpType(),
                                                     pggrpw_getGrpNum()  );  
                        }
                        else {
			     pghdlb_deselectEl( el_num, TRUE );
			     pghdlb_displayAllSel ();
                        }

			if ( pgpalw_isGrpActv() )
			    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_EXIT);
			else if ( iopr == FUNC_GROUP && 
                                  pghdlb_elemSelected() >= 1 )
			    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_OK);
                        else
			    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);

		    }
		}
	    }
	    else {                                        /* select element */
		if ( iopr == FUNC_UNGROUP ) {
	    	    crg_ggrp(el_num, &grptyp, &grpnum, &ier);
		    if ( grpnum && grptyp != GRPTYP_COMSYM ) {
			pghdlb_select( &el, selected);
	        	mbotw_mouseSet(LMHINT_CONFIRM, MMHINT_DONE);
		    }
		    else {
			mbotw_mouseSet(LMHINT_SELECT, MMHINT_NOACTION);
		    }
		}
		else if ( iopr == FUNC_INC_DEC ) {
		    if ( !pgnumb_isUp() ) { 
			pgnumb_popup();
		    }

		    if (pgpalw_getMode() == TYPE_OBJ) {
			if (stn_mdl) {
			    crg_goffset (inxarry[0], &fl_pos, &ier);
			    cvg_rdrec (cvg_getworkfile(), fl_pos, &el, &ier);
			}
			else {
			    fl_pos = selected;
			}

			if (el.hdr.grptyp == STNMDL && el.hdr.grpnum) {
			    crg_ggnel(el.hdr.grptyp, el.hdr.grpnum,
				    &nexp, &ier);
  			    crg_gginx(el.hdr.grptyp, el.hdr.grpnum,
				    nexp, inxarry, &nelm, &ier);
			}
			    
			for (ii=0; ii < nelm; ii++) {

			    if ( el.hdr.vg_type == SPTX_ELM ) {
				num_str =  el.elem.spt.text;
			    }
			    else if ( el.hdr.vg_type == TEXT_ELM ||
			    	  el.hdr.vg_type == TEXTC_ELM ) {
				num_str =  el.elem.txt.text;
			    }
			    else if ( el.hdr.vg_type == MARK_ELM ) {
				pghdlb_select( &el, fl_pos); 
			        num_str = NULL;
			    }
			    else {
			        num_str = NULL;
			    }

			    if (num_str){
			        if( pgutls_isNumber(num_str) ) {
	                            pghdlb_select( &el, fl_pos);
				    pgnumb_updateBtns (TRUE);
			        }
			    }

/*
 *  Get next STNMDL grp element 
 */
			    if (ii+1 < nelm) {
			       crg_goffset (inxarry[ii+1], &fl_pos, &ier);
			       cvg_rdrec (cvg_getworkfile(), fl_pos, &el, &ier);
			    }
			    
/*
 *  Free GFA block memory 
 */
			    if ( el.hdr.vg_type == GFA_ELM ) {
			        cvg_freeElPtr ( &el );
                            }

			}  /* for */

		    }  
		}
	       	else {

/*
 * If currently in a GROUP process, add the element to
 * the group.(Watches, Tracks and Sigmets are excluded.)
 */
		    if ( pgpalw_isGrpActv() ) {

		         if (el.hdr.vg_class != CLASS_WATCHES &&
                             el.hdr.vg_class != CLASS_TRACKS  &&
                             el.hdr.vg_class != CLASS_SIGMETS &&
                             el.hdr.vg_type != GFA_ELM &&
                             el.hdr.grptyp   <  90              ) {

                             pghdlb_deselectGrp( (char)pggrpw_getGrpType(),
                                                       pggrpw_getGrpNum() );
                             pggrpw_addtoGrp( &el, selected, &ier );
                             pghdlb_selectGrp( (char)pggrpw_getGrpType(),
                                                     pggrpw_getGrpNum() ); 
                         }
                         else if (el.hdr.grptyp >= 90) {
                             err_code = 4;
                             er_wmsg ("pgen", &err_code, NULL, &ignore, 4, 0);
	                     NxmErr_update(); 
                         }
                         else if ( el.hdr.vg_class == CLASS_SIGMETS ) {
                             err_code = 3;
                             er_wmsg ("pgen", &err_code, NULL, &ignore, 4, 0);
	                     NxmErr_update(); 
                         }
                         else if ( el.hdr.vg_class == CLASS_TRACKS ) {
                             err_code = 2;
                             er_wmsg ("pgen", &err_code, NULL, &ignore, 4, 0);
	                     NxmErr_update(); 
                         }
                         else if ( el.hdr.vg_class == CLASS_WATCHES ) {
                             err_code = 1;
                             er_wmsg ("pgen", &err_code, NULL, &ignore, 4, 0);
	                     NxmErr_update(); 
                         }
                         else if ( el.hdr.vg_type == GFA_ELM ) {
                             err_code = 8;
                             er_wmsg ("pgen", &err_code, NULL, &ignore, 4, 0);
	                     NxmErr_update(); 
                         }
	         
      		         mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_EXIT);
                        

                    }
		    else if ( ( iopr == FUNC_MULTISEL ) 
                            &&( el.hdr.vg_type != TCA_ELM ) ) {
	                draw_mode = pgpalw_getMode();
  	                if (draw_mode == TYPE_OBJ && 
		    		el.hdr.grptyp != GRPTYP_COMSYM) {
	                    not_group = TRUE;
		        }
	                else 
	                    not_group = (!el.hdr.grpnum && !el.hdr.grptyp);

		        if ((draw_mode == TYPE_OBJ && not_group)  || 
		            (draw_mode == TYPE_GRP && !not_group) ||
		            (el.hdr.grptyp == GRPTYP_COMSYM && el.hdr.grpnum)) {
		
	                    pghdlb_select( &el, selected);
			    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_DONE);
		        }

/*
 * start edit window if it isn't already
 */
                        if ( (pgpalw_getCurClassId() != CLASS_ANY) && 
             			(pgpalw_getMode() == TYPE_OBJ) && 
						!pgedit_isActive() ) {
	    		    cvg_rdrec (cvg_getworkfile(), selected, &el, &ier);
                            pgedit_editStart (&el);
            		    
/*
 *  Free GFA block memory 
 */
			    if ( el.hdr.vg_type == GFA_ELM ) {
			        cvg_freeElPtr ( &el );
			    }
        		}

		    }
	        }
	    } /* else (select element)... */
	} /* if (ier >=0... */		
    } /* if (event->xbutton.button == Button1... */
    else {

	iopr = pgpalw_getCurOperId();

	if ((iopr == FUNC_INC_DEC) && (event->xbutton.button == Button2)){
            if (!pghdlb_elemSelected()){
		pgedit_multiEditCb( NULL, (XtPointer)_one, NULL); /* cancel */
	        pgpalw_setCurBtns (FUNC_SELECT, CLASS_ANY, -1);
	    	pgpalw_setupOper ();
		pgpalw_rfrshObjPal();
	    }
	    else{
		pgedit_multiEditCb(NULL, 0, NULL);   /*  apply  */
		pghdlb_deselectAll();
		pgnumb_updateBtns(FALSE);
	    }

	}
        else if ( pgpalw_isGrpActv() && (event->xbutton.button == Button2) ) {

            if ( pghdlb_elemSelected() >= 1 ) {

/*
 * If there is a selected group on screen, first deselect it.
 */
                 pghdlb_deselectGrp( (char)pggrpw_getGrpType(),
                                           pggrpw_getGrpNum() );

                 pggrpw_startGrp();	     
            }
            else {

/*
 * Use the middle mouse button to exit group mode.
 */
                 pgpalw_inactvGrp();
                 pgpalw_setCurOper( (Widget) NULL );
                 pgpalw_setupOper();
                 pgpalw_classPopdown ();
            }
        }
	else {
	     pgevt_unsetOper (FALSE);

	     if ( pgpalw_getCurClassId() > 0 ) {
		 if ( iopr == FUNC_UNGROUP )
		     mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
		 else
		     mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_NOACTION);
	     }
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgevt_mvcpHdl ( Widget w, XtPointer clnt, XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgevt_mvcpHdl							*
 *									*
 * Event handler for MOVE and COPY states.				*
 *									*
 * void pgevt_mvcpHdl (w, clnt, event, ctdr)				*
 *									*
 * Input parameters:							*
 *  w		Widget		Calling widget				*
 *  clnt	XtPointer						*
 *  event       XEvent *						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/Eai		Initial coding				*
 * C. Lin/EAi		10/97	rename from NxmDrawSlideCb, cleanup	*
 * C. Lin/EAi		10/97	add OBJ_WBOX processing			*
 * E. Safford/GSC	12/97	add popdowns to attribute edit windows	*
 * E. Safford/GSC	03/98	add pgsymb_popdown			*
 * S. Law/GSC		04/98	added copy function			*
 * S. Law/GSC		05/98	cleaned up drag, added group box	*
 * S. Law/GSC		05/98	replaced pgpalw_mvcp with _setupOper	*
 * S. Law/GSC		05/98	added call to pgevt_unsetOper		*
 * S. Law/GSC		05/98	added check for pghot_getElmLoc		*
 * E. Safford/GSC	06/98	fixed drag problem on non-grouped elems *
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC	07/98	add closed & smooth to pgmvcp_start     *
 * C. Lin/EAi		08/98	limit move to drag from the vicinity  	*
 * E. Safford/GSC	09/98	modify to handle mode (GRP/OBJ)         *
 * D.W.Plummer/NCEP	09/98   added pgwbxw_setAttr and pgwbxw_spts	*
 * E. Safford/GSC	10/98   make drag pt lat/lon for offset text    *
 * G. Krueger/EAI	10/98	Using table for hints			*
 * S. Law/GSC		11/98	moved pgwbxw_spts -> pgwpts_save	*
 * D.W.Plummer/NCEP	04/99	remove call to pgwlst_update		*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * E. Safford/GSC	11/99	updated for new xwcmn.h			*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * J. Wu/GSC		03/01	added "EXIT" hint to bottom panel       *
 * E. Safford/GSC	05/01	added param to cvg_rdsel		*
 * J. Wu/SAIC		01/02	add layer param in crg_get() call	*
 * H. Zeng/XTRIA	01/03   added arguments to pgwbxw_setAttr	*
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 * H. Zeng/SAIC		11/04	changed para list for pgwbxw_setAttr	*
 * J. Wu/SAIC		11/04	free TCA/GFA block memory		*
 ***********************************************************************/
{
    float	xx, yy, llx, lly, urx, ury, dummy;
    int		selected, nearest, elnum, ier, xoff, yoff, el_layer;
    char	closed_flag;
    VG_DBStruct	el;
    filter_t	filter;
/*---------------------------------------------------------------------*/
/*
 * get device coordinates
 */
    xgtoff (&xoff, &yoff, &ier);
    xx = (float) (event->xbutton.x + xoff); 
    yy = (float) (event->xbutton.y + yoff); 

    if (event->xbutton.button == Button1) {

/* 
 * set nearest point of element 
 */
  	selected = pgactv_getElmLoc();
	if (selected == -1)
	    return;

        cvg_rdsel(NULL, selected, xx, yy, &nearest, &dummy, &el, &ier);

	if (el.hdr.grptyp && el.hdr.grpnum && 
	   (el.hdr.grpnum == GRPTYP_COMSYM || pgpalw_getMode() == TYPE_GRP)) {

	    crg_ggbnd(el.hdr.grptyp, el.hdr.grpnum, 
				 &llx, &urx, &ury, &lly, &ier);
	}
	else {		/* non grouped element */

/*
 *  If element is offset text then get don't use the range
 *  record, re-range the lat/lon point only
 */ 
	    if ((el.hdr.vg_type == TEXT_ELM) &&
	           (el.elem.txt.info.offset_x || el.elem.txt.info.offset_y)) {
		crg_rngpt (el.elem.txt.info.lat, el.elem.txt.info.lon, 
		    		&llx, &lly, &urx, &ury, &ier);
	    }
	    else if ((el.hdr.vg_type == SPTX_ELM) && 
    		   (el.elem.spt.info.offset_x || el.elem.spt.info.offset_y)) {
		crg_rngpt (el.elem.spt.info.lat, el.elem.spt.info.lon, 
		    		&llx, &lly, &urx, &ury, &ier);
	    }
	    else {

/*
 *  Use range record to establish llx, lly, urx, ury
 */
	        crg_getinx(selected, &elnum, &ier);
	        crg_get(elnum, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);
	    }
	}

/*
 * if the cursor is not close enough, return
 */
	if ( xx < llx  || urx < xx || yy < lly || ury < yy) {
	        return;
	}

	if (el.hdr.vg_type == CIRCLE_ELM) nearest = 0;
        pgactv_setNearPt(nearest);

/*
 * if it is OBJ_WBOX, set the hotlist
 */
    	if (el.hdr.vg_type == WBOX_ELM ) {
            pgwbxw_setAttr (el.hdr.maj_col,
                            el.elem.wbx.info.w_style,
                            el.elem.wbx.info.w_shape,
			    el.elem.wbx.info.w_mrktyp,
			    el.elem.wbx.info.w_mrksiz,
			    el.elem.wbx.info.w_mrkwid,
			    el.hdr.filled,
			    el.hdr.min_col            );
            pgwpts_save (&(el.elem.wbx.latlon[0]),
			&(el.elem.wbx.latlon[el.elem.wbx.info.numpts]));
	    closed_flag = 1;
	}
	else
	    closed_flag = el.hdr.closed;	

	pgmvcp_start(el.hdr.grptyp, el.hdr.grpnum, closed_flag, 
					xx, yy, el.hdr.smooth); 

	mbotw_mouseSet(LMHINT_DRAG, MMHINT_DONE);
	
/*
 *  Free TCA_GFA memory.
 */
	if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
        }
	else if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }
    }
    else {
	pgevt_unsetOper (FALSE);
	mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgevt_flipHdl ( Widget w, XtPointer clnt, XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgevt_flipHdl							*
 *									*
 * This is the event handler for FLIP mode.				*
 *									*
 * pgevt_flipHdl ( w, clnt, event, ctdr )				*
 *									*
 * Input parameters:							*
 *	w		Widget		Parent widget			*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	*event		XEvent		Event that triggered callback	*
 *									*
 * Output parameters:							*
 *			NULL						*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	 6/97						*
 * D. Keiser/GSC	 6/97		Fixed warm front flipping bug	*
 * E. Wehner/Eai	 7/97		Added filled and closed flags	*
 * E. Wehner/EAi	 8/97 		Remove grinfo from handlebars 	*
 * C. Lin/EAI		 8/97		Add offsets for 'S' coord (roam)*
 * E. Wehner/EAi	 8/97		Use drwpaste instead of Xlib	*
 * D.W.Plummer/NCEP	 9/97	Combine into NxmDraw for new vgstruct.h	*
 * E. Wehner/EAi	 9/97	Remove graphics info record		*
 * C. Lin/EAi	 	10/97	rename from drw_flpcb, cleanup     	*
 * C. Lin/EAi	 	10/97	further cleanup     			*
 * C. Lin/EAi	 	02/98	bug fix, save unflippable element     	*
 * E. Safford/GSC	02/98	add undo capability		 	*
 * S. Law/GSC		05/98	replaced pgpalw_flip with _setupOper	*
 * E. Safford/GSC	05/98	update undo            			*
 * S. Law/GSC		05/98	added call to pgevt_unsetOper		*
 * S. Law/GSC		05/98	added check for pghot_getElmLoc		*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * C. Lin/EAI		08/98	reset mouse func to FLIP for Button2	*
 * G. Krueger/EAI	09/98	Remove redundant MBOTW_ACTIONSETs	*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * J. Wu/GSC		03/01	added "EXIT" hint to bottom panel       *
 * E. Safford/GSC	05/01	added param to cvg_rdsel		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		11/04	free TCA/GFA memory			*
 * B. Yin/SAIC		04/06	flip GFA FZLVL				*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    float       xx, yy, llx, lly, urx, ury, *x_coords, *y_coords, dummy;
    float	tmp;
    int         ii, location, *dir, near_pt, ier, np, xoff, yoff;
    char	hazard[ 32 ];
    VG_DBStruct el;
/*---------------------------------------------------------------------*/
/*
 * Get device coordinates.
 */
    xgtoff (&xoff, &yoff, &ier);
    xx = (float) (event->xbutton.x + xoff); 
    yy = (float) (event->xbutton.y + yoff); 

    if ( event->xbutton.button == Button1 ) {

        location = pgactv_getElmLoc();
	if (location == -1) 
	    return;
	pgactv_getDevPts (&np, &x_coords, &y_coords);

        cvg_rdsel(NULL, location, xx, yy, &near_pt, &dummy, &el, &ier);

/*
 *  Free TCA_GFA memory.
 */
	if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
        }
	else if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }

	pgundo_newStep();

	pgutls_prepNew (location, &el, &llx, &lly, &urx, &ury, &ier);
	pgundo_storeThisLoc(location, UNDO_DEL, &ier);

/*
 * Switch on the VG type to flip, and store and display the 
 *  flipped VG element in the file.
 */
	if (el.hdr.vg_type == FRONT_ELM) 
	    dir = &el.elem.frt.info.fpipdr;
	else if (el.hdr.vg_type == SPLN_ELM) 
	    dir = &el.elem.spl.info.spldir;

	else if ( el.hdr.vg_type == GFA_ELM ) {

	    dir = NULL;

            cvg_getFld ( &el, TAG_GFA_AREATYPE, hazard, &ier );

	    if ( strcasecmp( hazard, "FZLVL" ) == 0 ) {

/*
 *  Change FZLVL direction.
 */
               for ( ii = 0; ii < np / 2; ii++ ) {
                                                                                    
                   tmp = x_coords[ ii ];
                   x_coords[ ii ] = x_coords[ np - ii - 1 ];
                   x_coords[ np - ii - 1 ] = tmp;
                                                                      
                   tmp = y_coords[ ii ];
                   y_coords[ ii ] = y_coords[ np - ii - 1 ];
                   y_coords[ np - ii - 1 ] = tmp;
               }
	    }
	}
	else
	    dir = NULL;

	if ( dir ) {
	    if (*dir < 0)
		*dir = 1;
	    else
		*dir = -1;
	}
        pgvgf_saveNewElm(cvg_getworkfile(), sys_D, &el, 
				np, x_coords, y_coords, TRUE,
				&location, &ier );

/*
 *  Free TCA_GFA memory.
 */
	if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
        }
	else if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }
	
	pgutls_redraw(location, &el, &ier);
	pgundo_storeThisLoc (location, UNDO_ADD, &ier);
        pgundo_endStep();
	
/*
 *  Free TCA_GFA memory.
 */
	if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
        }
	else if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }

	mbotw_mouseSet(LMHINT_FLIP, MMHINT_TOSELECTOPER);
    }
    else {
	pgevt_unsetOper (FALSE);
	mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgevt_rotateHdl ( Widget w, XtPointer clnt, XEvent *event, 
						Boolean *ctdr )
/************************************************************************
 * pgevt_rotateHdl							*
 *									*
 * This is the event handler for ROTATE mode.				*
 *									*
 * pgevt_rotateHdl ( w, clnt, event, ctdr )				*
 *									*
 * Input parameters:							*
 *	w		Widget		Parent widget			*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	*event		XEvent		Event that triggered callback	*
 *									*
 * Output parameters:							*
 *			NULL						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/Eai	 7/97	initial coding				*
 * C. Lin/EAi	 	10/97	rename from drw_rotCb, cleanup     	*
 * E. Safford/GSC	12/97	modified for intial wind rotation	*
 * S. Law		04/98	modified _mPtX & _mPtY to proper point	*
 * S. Law/GSC		05/98	replaced pgpalw_rotate with _setupOper	*
 * S. Law/GSC		05/98	added call to pgevt_unsetOper		*
 * S. Law/GSC		05/98	added check for pghot_getElmLoc		*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * C. Lin/EAI	        08/98	add CLASS_ANY				*
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * W. Li/EAI		01/99	NxmTxtA_XXX --> pgtxt_XXX		*
 * W. Li/EAI		02/99	Fixed a totation problem for label	*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * E. Safford/GSC	11/99	updated for new xwcmn.h			*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * J. Wu/GSC		03/01	added "EXIT" hint to bottom panel       *
 * H. Zeng/EAI          09/01   revised for new GROUP functionality     *
 * M. Li/SAIC		04/02	Added pglabel_getLabelPending		*
 ***********************************************************************/
{
    float       xx, yy;
    int         location, iclass, ier, xoff, yoff;
    VG_DBStruct el;
/*---------------------------------------------------------------------*/
/*
 * get device coordinates
 */
    xgtoff (&xoff, &yoff, &ier);
    xx = (float) (event->xbutton.x + xoff); 
    yy = (float) (event->xbutton.y + yoff); 

    if (event->xbutton.button == Button1) {
        location = pgactv_getElmLoc();
	if (location == -1) {
	    return;
	}

	iclass = pgpalw_getCurClassId();

	if ( iclass == CLASS_TEXT || 
	     iclass == CLASS_WINDS || 
	     iclass == CLASS_ANY ) {
	    mcanvw_disarmDynamic ();
	    pgtxt_setGhostFlag (FALSE, NULL);
	    pgrot_rotateStart (xx, yy);
            mcanvw_setPressFunc((XtEventHandler)&pgevt_rotateHdl, CURS_DEFAULT);
	}
  	else {
	    pgpalw_setupOper();
	}
    }
    else {
        if (_windPlActv ) {
	    if ( pglabel_getLabelPending() ){
		pghdlb_displayAllSel();
		el.hdr.vg_class = 0;
		pggst_setText (&el);
	    }
	    else{
                pghdlb_deselectAll ();
	    }

	    mcanvw_disarmPress ();
	
	    if (pgpalw_getCurClassId() == CLASS_TEXT ) {
		pgtxt_setGhostFlag (TRUE, pggst_setText);
		pgtxt_updateGstTxt ();
            }

	    pgnew_setArmDynamic ();
	    mcanvw_setDynActFlag (True); 
        }
        else {
	    pgevt_unsetOper (FALSE);
        }
	mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);

/*
 * If GROUP is active, redraw the group boundary so the 
 * new element is included.
 */
        if ( pgpalw_isGrpActv() ) {
             pghdlb_deselectGrp( (char)pggrpw_getGrpType(),
                                       pggrpw_getGrpNum()   );
             pghdlb_selectGrp( (char)pggrpw_getGrpType(),
                                     pggrpw_getGrpNum()   ); 
        }
    } /* the end of Event Hdl for Button2 Press. */
}

/*=====================================================================*/
/* ARGSUSED */
void pgevt_radiusHdl ( Widget w, XtPointer clnt, XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgevt_radiusHdl							*
 *									*
 * This is the event handler for RADIUS mode.				*
 *									*
 * pgevt_radiusHdl ( w, clnt, event, ctdr )				*
 *									*
 * Input parameters:							*
 *	w		Widget		Parent widget			*
 *	clnt		XtPointer	Pointer to client data (GrInfo)	*
 *	*event		XEvent		Event that triggered callback	*
 *									*
 * Output parameters:							*
 *			NULL						*
 *									*
 **									*
 * Log:									*
 * G. Krueger/EAI	 5/99	Copied from pgevt_rotateHdl		*
 * E. Safford/GSC	11/99	updated for new xwcmn.h			*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * J. Wu/GSC		03/01	added "EXIT" hint to bottom panel       *
 ***********************************************************************/
{
    float       xx, yy;
    int         location, iclass, ier, xoff, yoff;
    Boolean	label_flag = FALSE;
/*---------------------------------------------------------------------*/
/*
 * get device coordinates
 */
    xgtoff (&xoff, &yoff, &ier);
    xx = (float) (event->xbutton.x + xoff); 
    yy = (float) (event->xbutton.y + yoff);

    if (event->xbutton.button == Button1) {
	    location = pgactv_getElmLoc();
	    if (location == -1) {
		return;
	    }

	    iclass = pgpalw_getCurClassId();

	    if ( iclass == CLASS_CIRCLE || iclass == CLASS_ANY ) {
		label_flag = pglabel_getLabFlag();
		if (label_flag){
		     mbotw_mouseSet(LMHINT_MOVEPOINT, MMHINT_LABEL);
		}
		else {
		     mbotw_mouseSet(LMHINT_MOVEPOINT, MMHINT_DONE);
		}
		mcanvw_disarmDynamic ();
		pgtxt_setGhostFlag (FALSE, NULL);
		pgrad_radiusStart (xx, yy);
	    }
    }
    else {
	pgevt_unsetOper (FALSE);
        mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgevt_txtEditHdl ( Widget w, XtPointer clnt, XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgevt_txtEditHdl                                                     *
 *                                                                      *
 * Event handler for text edit state.                                   *
 *                                                                      *
 * void pgevt_txtEditHdl( w, clnt, event, ctdr)                       *
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget          Calling widget                          *
 *  clnt	XtPointer                                               *
 *  event       XEvent *                                                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  C. Lin/EAi          02/98                                           *
 *  G. Krueger/EAI	06/98	Uniform status hints			*
 *  G. Krueger/EAI	10/98	Using table for hints			*
 ***********************************************************************/
{
    if (event->xbutton.button == Button1) {
         pgedit_editCb(NULL, 0, NULL);

	 mbotw_mouseSet(LMHINT_APPLY, MMHINT_DONE);
    }
    else {
         pgedit_editCb(NULL, (XtPointer)_one, NULL);

	 mbotw_mouseSet(LMHINT_SELECT, MMHINT_NOACTION);
    }
}

/*=====================================================================*/

int pgevt_changeType ( int obj )
/************************************************************************
 * pgevt_changeType							*
 *									*
 * Changes the type of the current element.				*
 *									*
 * int pgevt_changeType (obj)						*
 *									*
 * Input parameters:							*
 *	obj		int		new object ID			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return:								*
 *	pgevt_changeType	int	Status code			*
 *					   0 = sucessful		*
 *					-1 = no element selected	*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/98	Created					*
 * E. Safford/GSC	04/98	Add popdown if attrib active for lines  *
 * F. J. Yen/NCEP	04/98	Updated with new ces function names	*
 * S. Law/GSC		04/98	Removed unneeded location check		*
 * F. J. Yen/NCEP	05/98	Removed front pip stroke		*
 * E. Safford/GSC	05/98	return without change if el is in group *
 * E. Safford/GSC	05/98	update undo                             *
 * S. Law/GSC		05/98	added check for pghot_getElmLoc		*
 * E. Safford/GSC	06/98	keep original front/line str & pip size *
 * E. Safford/GSC	10/98	convert cvg_rdsel call to cvg_rdrec     *
 * E. Safford/GSC	04/99	fix irix6 compiler warning              *
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * H. Zeng/SAIC		04/04	applied to multiple elements		*
 * E. Safford/SAIC	04/04	fix bug in front strength w/ >1 front	*
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 * J. Wu/SAIC		11/04	free TCA/GFA memory			*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		class, elm, gem, sub, location, frntStrength, np, ier, ier2; 
    int		direction, width, el_num, new_num, el_layer;
    int		extra = 5, cur_index;
    float       llx, lly, urx, ury, m_llx, m_lly, m_urx, m_ury;
    float	x_coords[MAXPTS], y_coords[MAXPTS], pattern_sz;
    VG_DBStruct	el, el2, el_first;
    Boolean	first;
    filter_t	filter;
/*---------------------------------------------------------------------*/

    m_llx = 999999.0F;
    m_lly = 999999.0F;
    m_urx = 0.0F;
    m_ury = 0.0F;

    first = TRUE;

/*****************/
/* new type info */
/*****************/
    class = pgpalw_getCurClassId();
    if (class == NONE_CURRENT) return (-1);

    pgobj_getId (class, obj, &elm, &gem, &sub);

/*****************************/
/* get defaults for new type */
/*****************************/
    el2.hdr.vg_class = class;
    el2.hdr.vg_type  = elm;
    ces_get (gem, &el2, &ier);

/*
 * Check each selected element and change the type 
 * when necessary.
 */
    pgundo_newStep();

    cur_index = -1;
    pghdlb_getNextIndex ( cur_index, &el_num, &location, &ier2 );

    while ( ier2 == 0 ) {

       if ( location == -1 ) {

	    return (-1);
       }

       cvg_rdrec(cvg_getworkfile(), location, &el, &ier);

       if ( el.hdr.grptyp && el.hdr.grpnum && 
            (pgpalw_getMode() == TYPE_GRP)    ) {

	    return (-1);
       }
       else if (class != el.hdr.vg_class) {

	    return (-1);
       }
   
       cvg_todev (&el, &np, x_coords, y_coords, &ier);

       pgutls_prepNew (location, &el, &llx, &lly, &urx, &ury, &ier);

       if (m_llx > llx)
           m_llx = llx;
       if (m_lly > lly)
           m_lly = lly;
       if (m_urx < urx)
           m_urx = urx;
       if (m_ury < ury)
           m_ury = ury;

       pgundo_storeThisLoc (location, UNDO_DEL, &ier);

/****************************************/
/* change to new type  and add defaults */
/****************************************/
       switch (class) {
         case CLASS_FRONTS:

/***************************************************************/
/* add in the current strength which is the middle of 3 digits */
/* (the fcode contains both the front type and the strength -- */
/*  we need to change the type but keep the strength)	  */
/***************************************************************/
	   frntStrength = ((el.elem.frt.info.fcode % 100) / 10) * 10;
	   el.elem.frt.info.fcode = sub + frntStrength;
	   break;
         case CLASS_LINES:

	   if (el.hdr.vg_type == LINE_ELM) {
	       width = el.elem.lin.info.width;
  	       if (el2.hdr.vg_type == SPLN_ELM) {
		   pattern_sz = el2.elem.spl.info.splsiz;
		   direction  = el2.elem.spl.info.spldir;
	       } 
           }
	   else {
	       width = el.elem.spl.info.splwid;
	       pattern_sz = el.elem.spl.info.splsiz;
	       direction  = el.elem.spl.info.spldir;
	   }
	
	   if (elm == LINE_ELM) {
	       el.hdr.vg_type = LINE_ELM;
  	       el.elem.lin.info.lintyp = gem;   
  	       el.elem.lin.info.width  = width; 
	       el.elem.lin.info.lthw   = el2.elem.lin.info.lthw;
	       el.elem.lin.info.lwhw   = el2.elem.lin.info.lwhw; 
	   }
	   else {
	       el.hdr.vg_type = SPLN_ELM;
	       el.elem.spl.info.spltyp = gem;
  	       el.elem.spl.info.splwid = width; 
	       el.elem.spl.info.splstr = el2.elem.spl.info.splstr;
	       el.elem.spl.info.spldir = direction; 
	       el.elem.spl.info.splsiz = pattern_sz; 
	   }
	   break;
         default:
	   return (-1);
       }

       el.hdr.maj_col = el2.hdr.maj_col;
       el.hdr.min_col = el2.hdr.min_col;

       pgvgf_saveNewElm (NULL, sys_D, &el, np, 
			 x_coords, y_coords, TRUE, &location, &ier); 
       pghdlb_select(&el, location);

       crg_getinx (location, &new_num, &ier);
       crg_get(new_num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);

       if (m_llx > llx)
           m_llx = llx;
       if (m_lly > lly)
           m_lly = lly;
       if (m_urx < urx)
           m_urx = urx;
       if (m_ury < ury)
           m_ury = ury;

       pgundo_storeThisLoc (location, UNDO_ADD, &ier);

/*
 * Remember the first selected element.
 */
       if ( first == TRUE ) {

	  el_first = el;
          pgactv_setActvElm(&el, location);
          first = FALSE;
       }

       cur_index = el_num;
       pghdlb_getNextIndex ( cur_index, &el_num, &location, &ier2 );

/*
 *  Free TCA_GFA memory.
 */
       if ( el.hdr.vg_type == TCA_ELM ) {
           cvg_freeBkpts ( &el );
       }
       else if ( el.hdr.vg_type == GFA_ELM ) {
           cvg_freeElPtr ( &el );
       }

    } /* the end of while (   */

    pgundo_endStep();

    m_llx -= (float)extra;
    m_lly -= (float)extra;
    m_urx += (float)extra;
    m_ury += (float)extra;
    
    cvg_rfrsh (NULL, m_llx, m_lly, m_urx, m_ury, &ier2); 

/*
 * Pop up the edit window for the first element.
 */
    if (class == CLASS_LINES) {
	if (pgline_isUp()) {
	    pgline_popdown();
	    pgedit_editStart (&el_first);	
	} 
    }
    else if (class == CLASS_FRONTS) {
	if (pgfrtw_isUp()) {
	    pgfrtw_popdown();
	    pgedit_editStart (&el_first);	
	} 
    }
    return (0);
}

/*=====================================================================*/

void pgevt_ungroup ( void )
/************************************************************************
 * pgevt_ungroup							*
 *									*
 * Releases the selected elements from their group			*
 *									*
 * void pgevt_ungroup ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return:								*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	04/98	initial coding				*
 * S. Law/GSC		04/98	Added xpgpaste				*
 * E. Safford/GSC	06/98	added crg_grfrsh			*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * C. Lin/EAI		08/98	Add unmanage object panel		*
 * C. Lin/EAI		09/98	check COMBOSYM and modify for new logic	*
 * E. Safford/GSC	09/98	add undo capabilities			*
 * G. Krueger/EAI	09/98	Correct MOUSESET hint			*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * E. Safford/GSC	12/98	Limit area of refresh			*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters		*
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * J. Wu/SAIC		12/01	add layer in crg_set() call		*
 * J. Wu/SAIC		01/02	add layer in crg_get() call		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   Changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           11/04   free GFA block memory			*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 ***********************************************************************/
{
    int		index,  el_num, new_num, el_loc, ier1, found, update_crg;
    int		grpnum, ier2, elN, new_location, layer, iret, el_layer;
    float	llx, lly, urx, ury, m_llx, m_lly, m_urx, m_ury;
    float       *elX, *elY, extra = 5.0F, inf_bbox[4];
    char	grptyp;
    Boolean     done;
    VG_DBStruct	el;
    filter_t	filter;
/*---------------------------------------------------------------------*/

    m_llx = 999999.0F;
    m_lly = 999999.0F;
    m_urx = 0.0F;
    m_ury = 0.0F;

    update_crg = 0;

    index = -1;	
    pghdlb_getNextIndex (index, &el_num, &el_loc, &ier1);

    done  = False;

    pgundo_newStep();
    layer = pglayer_getCurLayer( );
    while (ier1 >= 0) {

 	crg_ggrp (el_num, &grptyp, &grpnum, &ier2);	

	if (grpnum && grptyp != GRPTYP_COMSYM) {

	    cvg_rdrec (cvg_getworkfile(), el_loc, &el, &ier2);

/*
 * Create a copy of the element with no group info,
 */
	    pgactv_setActvElm ( &el, el_loc);
            pgactv_getDevPts (&elN, &elX, &elY);
            pgvgf_saveNewElm(NULL, sys_D, &el, 
                  elN, elX, elY, FALSE, &new_location, &iret);
            cvg_setginf(cvg_getworkfile(), new_location, 0, 0, &iret);

/*
 * Free TCA break point/GFA block memory
 */
            if ( el.hdr.vg_type == TCA_ELM ) {
               cvg_freeBkpts ( &el );
            }
	    else if ( el.hdr.vg_type == GFA_ELM ) {
               cvg_freeElPtr ( &el );
            }

            cvg_rdrec(cvg_getworkfile(), new_location, &el, &iret);
            crg_set (&el, new_location, layer, &iret);
	    crg_getinx (new_location, &new_num, &iret);
	    crg_get(new_num, &el_layer, filter, &llx, &lly, &urx, &ury, &iret);

/*
 * Mark elements in placement that are effected by
 * the new element, and get the area of influence back
 */
	    cvg_checkplace(&el, 0, new_location, &found, inf_bbox, &iret);
            if (found > 0) {

/*
 * Update the refresh extent if the area impacted by
 * placement was bigger than the current area
 */
                m_llx = G_MIN(m_llx, inf_bbox[0]);
                m_lly = G_MIN(m_lly, inf_bbox[2]);
                m_urx = G_MAX(m_urx, inf_bbox[1]);
                m_ury = G_MAX(m_ury, inf_bbox[3]);

                update_crg = 1;
            }

/*
 * Free TCA break point memory
 */
            if ( el.hdr.vg_type == TCA_ELM ) {
               cvg_freeBkpts ( &el );
            }

/*
 * Free GFA block memory
 */
            if ( el.hdr.vg_type == GFA_ELM ) {
               cvg_freeElPtr ( &el );
            }

	    if (m_llx > llx)
                m_llx = llx;
            if (m_lly > lly)
                m_lly = lly;
            if (m_urx < urx)
                m_urx = urx;
            if (m_ury < ury)
                m_ury = ury;

	    pgundo_storeThisLoc(new_location, 
                                UNDO_ADD, &iret);

/*
 * Mark elements in placement that are effected by
 * the delete, and get the area of influence back
 */
            cvg_rdrec(cvg_getworkfile(), el_loc, &el, &iret);
	    cvg_checkplace(&el, 1, el_loc, &found, inf_bbox, &iret);
            if (found > 0) {
/*
 * Update the refresh extent if the area impacted by
 * placement was bigger than the current area
 */
                m_llx = G_MIN(m_llx, inf_bbox[0]);
                m_lly = G_MIN(m_lly, inf_bbox[2]);
                m_urx = G_MAX(m_urx, inf_bbox[1]);
                m_ury = G_MAX(m_ury, inf_bbox[3]);

                update_crg = 1;
            }

/* 
 * Mark the original element as deleted.
 */
            cvg_delet(cvg_getworkfile(), el_loc, TRUE, &ier2);
	    crg_get (el_num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier2);		
	
	    if (m_llx > llx)
                m_llx = llx;
            if (m_lly > lly)
                m_lly = lly;
            if (m_urx < urx)
                m_urx = urx;
            if (m_ury < ury)
                m_ury = ury;

            crg_clear(el_num, &ier2);
	    pgundo_storeThisLoc (el_loc, UNDO_DEL, &ier2);

	    done = True;

	}
	index = el_num;	
        pghdlb_getNextIndex (index, &el_num, &el_loc, &ier1);

    }		/* while */
    pgundo_endStep();

    m_llx -= extra;
    m_lly -= extra;
    m_urx += extra;
    m_ury += extra;
    
    xpgpaste (m_llx, m_lly, m_urx, m_ury, &ier1);
    cvg_rfrsh (NULL, m_llx, m_lly, m_urx, m_ury, &ier1); 

/*
 * If we may have impacted other elements with placement
 * we will need to rebuild the range records
 */
    if (update_crg) {
        crg_rebuild();
    }

    pghdlb_displayAllSel ();

    if (done)
	mbotw_mouseSet(LMHINT_SELECT, MMHINT_DONE);
    else
	mbotw_mouseSet(LMHINT_SELECT, MMHINT_NOACTION);
}

/*=====================================================================*/

void pgevt_unsetOper ( Boolean reset )
/************************************************************************
 * pgevt_unsetOper							*
 *									*
 * Does the various disarm and popdowns for Button2 or 3 events		*
 *									*
 * void pgevt_unsetOper (reset)						*
 *									*
 * Input parameters:							*
 *	reset		Boolean	flag telling to reset operation		*
 *									*
 * Output parameters:							*
 * Return:								*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/98	initial copying				*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * S. Law/GSC		03/00	cleanup					*
 ***********************************************************************/
{
    mcanvw_disarmDynamic ();

    if (reset) {
	pgpalw_setCurBtns (0, -1, -1);
    }

    pgpalw_classPopdown ();
    pgpalw_setupOper ();
}

/*=====================================================================*/

void pgevt_setWindPlActv ( Boolean status ) 
/************************************************************************
 * pgevt_setWindPlActv                                                  *
 *                                                                      *
 * Utility for setting the value of _windPlActv flag                    *
 *                                                                      *
 * void pgevt_setWindPlActv ( status )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  status          Boolean     New value of _windPlActv flag           *
 *                                                                      *
 * Ouput parameters:                                                    *
 *  none                                                                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC  06/98        Initial coding                          *
 ***********************************************************************/
{
    _windPlActv = status;
}

/*=====================================================================*/
/* ARGSUSED */
void pgevt_confirmCancelCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgevt_confirmCancelCb						*
 *									*
 * Callback function for cancel button at the bottom of confirmation    *
 * window.							        *
 *									*
 * void pgevt_confirmCancelCb (wid, clnt, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	clnt	XtPointer	not used				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		03/00	initial coding				*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 ***********************************************************************/
{
    pgwfmt_setWatch (NULL);
    mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT); 
    mbotw_mouseSet (LMHINT_SELECT, MMHINT_NOACTION);
}

/*=====================================================================*/
/* ARGSUSED */
void pgevt_extrapHdl ( Widget w, XtPointer clnt, XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgevt_extrapHdl							*
 *									*
 * Event handler for EXTRAP state.					*
 *									*
 * void pgevt_extrapHdl ( w, clnt, event, ctdr )			*
 *									*
 * Input parameters:							*
 *  w		Widget		Calling widget				*
 *  clnt	XtPointer						*
 *  event       XEvent *						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		10/02	modify from pgevt_mvcpHdl()		*
 * H. Zeng/XTRIA	01/03   added arguments to pgwbxw_setAttr	*
 * T. Piper/SAIC	02/04	removed unused variable closed_flag	*
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 * H. Zeng/SAIC		11/04	changed para. list for pgwbxw_setAttr	*
 * J. Wu/SAIC		11/04	free TCA/GFA memory			*
 ***********************************************************************/
{
    float	xx, yy, llx, lly, urx, ury, dummy;
    int		selected, nearest, elnum, ier, xoff, yoff, el_layer;
    VG_DBStruct	el;
    filter_t	filter;
/*---------------------------------------------------------------------*/
/*
 * get device coordinates
 */
    xgtoff (&xoff, &yoff, &ier);
    xx = (float) (event->xbutton.x + xoff); 
    yy = (float) (event->xbutton.y + yoff); 

    if (event->xbutton.button == Button1) {

/* 
 * set nearest point of element 
 */
  	selected = pgactv_getElmLoc();
	if (selected == -1)
	    return;

        cvg_rdsel(NULL, selected, xx, yy, &nearest, &dummy, &el, &ier);

	if (el.hdr.grptyp && el.hdr.grpnum && 
	   (el.hdr.grpnum == GRPTYP_COMSYM || pgpalw_getMode() == TYPE_GRP)) {

	    crg_ggbnd(el.hdr.grptyp, el.hdr.grpnum, 
				 &llx, &urx, &ury, &lly, &ier);
	}
	else {					/* non grouped element */
/*
 *  If element is offset text then get don't use the range
 *  record, re-range the lat/lon point only
 */ 
	    if ((el.hdr.vg_type == TEXT_ELM) &&
	           (el.elem.txt.info.offset_x || el.elem.txt.info.offset_y)) {
		crg_rngpt (el.elem.txt.info.lat, el.elem.txt.info.lon, 
		    		&llx, &lly, &urx, &ury, &ier);
	    }
	    else if ((el.hdr.vg_type == SPTX_ELM) && 
    		   (el.elem.spt.info.offset_x || el.elem.spt.info.offset_y)) {
		crg_rngpt (el.elem.spt.info.lat, el.elem.spt.info.lon, 
		    		&llx, &lly, &urx, &ury, &ier);
	    }
	    else {
/*
 *  Use range record to establish llx, lly, urx, ury
 */
	        crg_getinx ( selected, &elnum, &ier );
	        crg_get ( elnum, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);
	    }
	}

	if ( el.hdr.vg_type == CIRCLE_ELM ) nearest = 0;
        pgactv_setNearPt ( nearest );

/*
 * if it is OBJ_WBOX, set the hotlist
 */
    	if ( el.hdr.vg_type == WBOX_ELM ) {
            pgwbxw_setAttr ( el.hdr.maj_col,
                             el.elem.wbx.info.w_style,
                             el.elem.wbx.info.w_shape,
			     el.elem.wbx.info.w_mrktyp,
			     el.elem.wbx.info.w_mrksiz,
			     el.elem.wbx.info.w_mrkwid,
			     el.hdr.filled,
			     el.hdr.min_col	        );
            pgwpts_save ( &(el.elem.wbx.latlon[0]),
			  &(el.elem.wbx.latlon[el.elem.wbx.info.numpts]) );
	}

	pgextrap_start ( el.hdr.grptyp, el.hdr.grpnum ); 
			
	mcanvw_setPressFunc ( (XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT );
	
	mbotw_mouseSet ( LMHINT_SELECT, MMHINT_EXIT );
		
/*
 *  Free TCA_GFA memory.
 */
	if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
        }
	else if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }
    }
    else {
	pgevt_unsetOper (FALSE);
	mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
    }
}
