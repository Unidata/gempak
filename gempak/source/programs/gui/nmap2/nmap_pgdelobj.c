#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "hints.h"

/*
 *  private functions
 */
void pgdelobj_cancelCb ( void );
void pgdelobj_createDialog ( Widget w, char *message );
void pgdelobj_mouseCb( Widget, XtPointer, XEvent* );
void pgdelobj_okCb ( void );

static Widget	_delConfW;


/************************************************************************
 * nmap_pgdelobj.c 							*
 * 									*
 * This module contains the function to delete elements by object type	*
 * in product generation.						*
 *                                                                      *
 * CONTENTS:                                                            *
 * pgdelobj_deletStart	selects elements by object type 		*
 * pgdelobj_createDialog() creates the delete confirmation window	*
 * pgdelobj_okCb()	deletes all selected elements in the WORK_FILE	*
 * pgdelobj_cancelCb	cancels deletion				*
 * pgdelobj_mouseCb	checks for MB2 press events   			*
 ***********************************************************************/

/*=====================================================================*/
/* ARGSUSED */
void pgdelobj_deletStart ( Widget w, int class, int obj )
/************************************************************************
 * pgdelobj_deletStart							*
 *									*
 * This function reads a VG file and selects any element on the current *
 * layer which object type is equal to the user selected object type 	*
 * and popup a confirmation window.					*
 * 									*
 * operation popup window.						*
 *									*
 * void pgdelobj_deletStart(w, class, obj)				*
 *									*
 * Input parameters:							*
 *    w		Widget		parent widget				*
 *    class	int		class id				*
 *    obj	int		object id				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * W. Li/EAI		05/99						*
 * G. Krueger/EAI       05/99   Added circle draw function              *
 * W. Li/EAI		05/99	added CLASS_TRACKS and fixed a bug	*
 * E. Safford/GSC	06/99	use locally defined dialog box		*
 * S. Law/GSC		09/99	added CLASS_SIGMETS and fixed lines	*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 * M. Li/SAIC		10/01	Added CLASS_MARKER			*
 * M. Li/SAIC		10/01	Added OBJ_TEXTICNG			*
 * J. Wu/SAIC		01/02	delete only the obj. on current layer	*
 * J. Wu/SAIC		11/02	delete LIST object			*
 * J. Wu/SAIC		02/03	delete midlevel cloud text object	*
 * J. Wu/SAIC		10/03	add JET object				*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		02/04	add AIRMET (GFA_ELM) object		*
 * J. Wu/SAIC		03/04	add NCONSIG (GFA_ELM) object		*
 * J. Wu/SAIC		05/04	add OBJ_GFA (GFA_ELM) 			*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   Changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC		09/04	remove OBJ_AIRMET & OBJ_NCONSIG		*
 * J. Wu/SAIC		10/04	free GFA block memory			*
 * J. Wu/SAIC		05/08	add OBJ_GFA_P				*
 ***********************************************************************/
{
    int		vg_type, gem_type, sub_type, oper, ier;
    int		total_select, offsets, cur_layer, el_layer; 
    long        size;
    int		flag, ltype;
    char	message[100], newfil[133];
    FILE 	*fp;
    Boolean	select;
    VG_DBStruct	el;
    Widget	canvas;
/*---------------------------------------------------------------------*/

    if (pghdlb_elemSelected()){	    
       pghdlb_deselectAll();
    }

    pgobj_getId (  class, obj, &vg_type, &gem_type, &sub_type );

    oper = pgpalw_getCurOperId();
    pgpalw_setCurBtns(oper, class, obj);

/*
 * get offsets and total elements in WORK_FILE
 */
    cfl_inqr(cvg_getworkfile(), NULL, &size, newfil, &ier);

    fp = (FILE *) cfl_uopn(cvg_getworkfile(), &ier);

    if (( ier != 0 ) || ( fp == NULL )) {
        return;
    }

    offsets = 0;
    cur_layer = pglayer_getCurLayer ();
    while ((long)offsets < size ) {

	cvg_rdhdr(cvg_getworkfile(), fp, offsets, (int)size, &el, 
		  &flag, &ier);        	

	cvg_rdele(&el, offsets, el.hdr.recsz, fp, &ier); 

/*
 *  Only delete objects on the current layer.
 */
	el_layer = crg_getLayer ( offsets );
        if ( (el_layer == cur_layer) && (el.hdr.vg_type == vg_type) ) {

	    select = FALSE;
	    switch(class) {
	      case CLASS_FRONTS:
		if (sub_type == ((el.elem.frt.info.fcode/100)*100 + 
				 el.elem.frt.info.fcode%10)) {
		    select = TRUE;
		}
		break;

	      case CLASS_LINES:
		ltype = (el.hdr.vg_type == LINE_ELM) ? 
		    el.elem.lin.info.lintyp : el.elem.spl.info.spltyp;

		if (gem_type == ltype) {
		    select = TRUE;
		}
		break;

	      case CLASS_SYMBOLS:
	      case CLASS_COMSYM:
	      case CLASS_MARKER:
		if (sub_type == (int)el.elem.sym.data.code[0]){
		    select = TRUE;
		}
		break;

	      case CLASS_TEXT:
		if (obj == OBJ_TEXTGEN && 
		    (el.elem.spt.info.sptxtyp == 0 ||
		     el.elem.spt.info.sptxtyp == 1 ||
		     el.elem.spt.info.sptxtyp == 2 ||
		     el.elem.spt.info.sptxtyp == 3 ||
		     el.elem.spt.info.sptxtyp == 4 ||
		     el.elem.spt.info.sptxtyp == 5 ||
		     el.elem.spt.info.sptxtyp == 10||
		     el.elem.spt.info.sptxtyp == 11)) {

		    select = TRUE;
		}
		else if (obj == OBJ_TEXTFZL && 
			 el.elem.spt.info.sptxtyp == 6) {

		    select = TRUE;
		}
		else if (obj == OBJ_TEXTTURB && 
			 (el.elem.spt.info.sptxtyp == 7 ||
			  el.elem.spt.info.sptxtyp == 9)) {

		    select = TRUE;
		}
		else if (obj == OBJ_TEXTCLD && 
			 el.elem.spt.info.sptxtyp ==8 ) {

		    select = TRUE;
		}
		else if (obj == OBJ_TEXTICNG &&
                         el.elem.spt.info.sptxtyp ==12) {

                    select = TRUE;
		}
		else if (obj == OBJ_TEXTMCLOUD &&
                         el.elem.spt.info.sptxtyp ==15) {

                    select = TRUE;
                }
		break;

	      case CLASS_WATCHES:
		if (obj == OBJ_WBCOUNTY && 
		    el.elem.wbx.info.w_style == WBC) {
		    select = TRUE;
		}
		else if (obj == OBJ_WBPARALL && 
			 el.elem.wbx.info.w_style == PGRAM) {
		    select = TRUE;
		}
		break;

	      case CLASS_TRACKS:
	      case CLASS_CIRCLE:
	      case CLASS_WINDS:
	      case CLASS_SIGMETS:
		select = TRUE;
		break;
	      
	      case CLASS_LIST:
		if ( sub_type == el.elem.lst.info.subtyp ) {
		    select = TRUE;
		}
		break;

	      case CLASS_MET:
		if ( obj == OBJ_JET ) {
		    select = TRUE;
		}
		else if ( obj == OBJ_GFA ) {
		    select = TRUE;		
		}
		else if ( obj == OBJ_GFA_P ) {
		    select = TRUE;		
		}
		break;

	    } /* end of: switch */

	    if(select) {

		if (!el.hdr.delete) {
		    pghdlb_setSelect (offsets);
		}
	    }
	}

/*
 * Gets next element offset
 */
	offsets = offsets + el.hdr.recsz;

/*
 * Free TCA/GFA memory
 */
        if ( el.hdr.vg_type == TCA_ELM ) {
           cvg_freeBkpts ( &el );
        }
	else if ( el.hdr.vg_type == GFA_ELM ) {
           cvg_freeElPtr ( &el );
        }
	
    } /* end of while  */

    cfl_clos(fp, &ier);

    pghdlb_showAllSel ();

    total_select =  pghdlb_elemSelected(); 

    if (total_select>0){
	sprintf(message, " %d %s\n%s", total_select, 
		"selected element(s) will be deleted !!", 
		"Are you sure you want to delete them?"); 
  	canvas = (Widget)mcanvw_getDrawingW();
 	pgdelobj_createDialog (canvas, message); 

	mbotw_mouseSet(LMHINT_CONFIRM, MMHINT_CANCEL);

  	mcanvw_setPressFunc ((XtEventHandler)&pgdelobj_mouseCb, CURS_DEFAULT);
    }
    else {
	mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_EXIT);
    }
}

/*=====================================================================*/

void pgdelobj_createDialog ( Widget w, char *message )
/************************************************************************
 * pgdelobj_createDialog						*
 *									*
 * This function creates and manages the dialog popup to confirm the 	*
 * delete by type operation.						*
 *									*
 * void pgdelobj_createDialog(w, message)				*
 *									*
 * Input parameters:							*
 *	w		Widget	parent widget for dialog		*
 *	*message	char	message to display in window		*
 *		 	    						*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * E. Safford/GSC	06/99						*
 * J. Wu/GSC		05/01	free XmStrings				*
 * E. Safford/GSC	06/01	fix crash - mv XmStringFree to bottom	*
 ***********************************************************************/
{
Arg		args[10];
int		argcnt;
XmString	conf_string;
Widget		child;
/*---------------------------------------------------------------------*/

    conf_string = XmStringCreateLtoR (message, 
					XmFONTLIST_DEFAULT_TAG);
    argcnt = 0;
    XtSetArg ( args[argcnt], XmNdialogType, XmDIALOG_QUESTION );
    argcnt++;

    XtSetArg ( args[argcnt], XmNmessageAlignment, XmALIGNMENT_CENTER ); 
    argcnt++;

    XtSetArg ( args[argcnt], XmNmessageString, conf_string ); 
    argcnt++;
    
    XtSetArg ( args[argcnt], XmNdialogStyle, XmDIALOG_MODELESS ); 
    argcnt++;
	
    _delConfW = XmCreateQuestionDialog (w, "dialog", args, argcnt);	

    XtAddCallback ( _delConfW, XmNokCallback, (XtCallbackProc)pgdelobj_okCb, NULL );
    XtAddCallback ( _delConfW, XmNcancelCallback, (XtCallbackProc)pgdelobj_cancelCb,
		                       				NULL ); 

    child = XmMessageBoxGetChild ( _delConfW, XmDIALOG_HELP_BUTTON );
    XtUnmanageChild ( child );

    XtManageChild ( _delConfW );

    XmStringFree ( conf_string );  

}

/*=====================================================================*/

void pgdelobj_okCb ( void )
/************************************************************************
 * pgdelobj_okCb							*
 *									*
 * This function is the callback for OK on the object delete confirm	*
 * operation popup window.						*
 *									*
 * void pgdelobj_okCb ()						*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * W. Li/EAI		05/99						*
 * W. Li/EAI		05/99	added call to crg_clear()		*
 * S. Law/GSC		03/00	changed to use pgutls_prepNew		*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * J. Wu/SAIC		02/02	set changes_made to TRUE on curr. layer	*
 * J. Wu/SAIC		10/04	free TCA/GFA memory			*
 ***********************************************************************/
{
    int		cur_index, sel_num, sel_loc, ier;
    float	llx, lly, urx, ury;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
/*
 * Get each selected element and delete it.
 */
    pgundo_newStep();
    cur_index = -1;    /* start */

    pghdlb_getNextIndex (cur_index, &sel_num, &sel_loc, &ier);

    while (ier >= 0) {
	pgutls_prepNew (sel_loc, &el, &llx, &lly, &urx, &ury, &ier);
	pgundo_storeThisLoc (sel_loc, UNDO_DEL, &ier);

	cur_index = sel_num;
        pghdlb_getNextIndex (cur_index, &sel_num, &sel_loc, &ier);
	
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
    pgundo_endStep();
    
    pglayer_setChngMade( pglayer_getCurLayer(), TRUE );
    
    pgpalw_setupOper();
    mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_EXIT);
}

/*=====================================================================*/

void pgdelobj_cancelCb ( void )
/************************************************************************
 * pgdelobj_cancelCb							*
 *									*
 * This function is the callback for cancel on the object delete 	*
 * confirm operation popup window.					*
 *									*
 * void pgdelobj_cancelCb ()						*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * W. Li/EAI	05/99							*
 ***********************************************************************/
{
    mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_EXIT);
   
    if (pghdlb_elemSelected()){	    
       pghdlb_deselectAll();
    }
}
/*=====================================================================*/
/* ARGSUSED */
void pgdelobj_mouseCb ( Widget w, XtPointer clnt, XEvent *event )
/************************************************************************
 * pgdelobj_mouseCb							*
 *									*
 * This function checks for Mouse Button events which will function	*
 * to accept/cancel the dialog box (in addition to the dialog control).	*
 *									*
 * void pgdelobj_mouseCb (w, clnt, event)				*
 *									*
 * Input parameters:							*
 *	w			Widget					*
 *	clnt			XtPointer				*
 *	*event			XEvent					*
 *		 	    						*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * E. Safford/GSC	06/99						*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 ***********************************************************************/
{
    if (event->xbutton.button == Button3) {
	return;
    }

    XtDestroyWidget (_delConfW);

    if (event->xbutton.button == Button1) {
	pgdelobj_okCb();
    }
    else if (event->xbutton.button == Button2) {
        pghdlb_deselectAll();
    }

    mcanvw_disarmPress();
    mcanvw_setPressFunc((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
    mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_EXIT);
}
