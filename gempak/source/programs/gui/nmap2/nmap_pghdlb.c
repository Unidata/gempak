#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "drwids.h"
#include "proto_xw.h"

#define NO_VALUE	-99

#define SEL_ELM		  0
#define SEL_GRP		  1
#define SEL_INC_DEC	  2
#define SEL_SMEAR	  3
#define SEL_TCA		  4

#define GRP_COLOR	  5	
#define GRP_MARK	  3	

#define ELM_COLOR	  1	
#define ELM_MARK	 17	

#define INC_DEC_COLOR     7 
#define INC_DEC_MARK	  2

#define SMEAR_MARK	 (19)
#define SMEAR_COLOR      ( 2)

#define TCA_COLOR	 26	
#define TCA_MARK	 2	

#define EXTRA		  5.0F


static int	_numSelected;

/*
 *  Private functions
 */
static void pghdlb_applyHdlb ( VG_DBStruct *el, int *iret );
static void pghdlb_clearHdlb ( int el_num, float *llx, float *lly,
			float *urx, float *ury, int *iret );
static void pghdlb_deselGrp (  Boolean grptyp, int grpnum, Boolean updt_dsply );
static void pghdlb_displayGrp ( Boolean grptyp, int grpnum );
static void pghdlb_drawGrpbnd ( float rl, float rr, float rt, float rb );
static void pghdlb_selGrp ( Boolean grptyp, int grpnum );
static void pghdlb_setMark ( int type, int *iret );
static void pghdlb_undisplayGrp ( Boolean grptyp, int grpnum );
static void pghdlb_updateSelection ( char selected, int el_num, int *iret );

static void pghdlb_chkSelGfaHazType ( int location, int *iret );
/************************************************************************
 * nmap_pghdlb.c							*
 *									*
 * This module contains the handle bar related functions.		*
 *									*
 * CONTENTS:								*
 *									*
 *	Public Functions						*
 *   pghdlb_select()		mark element or group as selected	*
 *   pghdlb_selectGrp()         mark a group as selected                *
 *   pghdlb_selectAll()		marks all elements or groups as selected*
 *   pghdlb_setSelect()		mark element as selected in rng rec only*
 *   pghdlb_deselectEl()	deselect one specific element		*
 *   pghdlb_deselectGrp()       deselect a group                        *
 *   pghdlb_deselectAll()	deselect all elements			*
 *   pghdlb_displayAllSel()	redisplay all selected elements		*
 *   pghdlb_showAllSel()	display all selected elms w/o deselect	*
 *   pghdlb_grpAllSel()	        group all selected element(s) 		*
 *   pghdlb_elemSelected()	return number of elements selected	*
 *   pghdlb_getNextIndex()	return index to next selected element	*
 *									*
 *	Protected Functions						*
 *   pghdlb_setMark()		set gmark attributes for handlebars	*
 *   pghdlb_selGrp()		selects all the elements of a group	*
 *   pghdlb_deselGrp()		deselects all the elements of a group	*
 *   pghdlb_displayGrp()	displays all the elements of a group	*
 *   pghdlb_undisplayGrp()	undisplays all the elements of a group	*
 *   pghdlb_applyHdlb()		place handle bar on a specific element	*
 *   pghdlb_clearHdlb()		remove handle bars from an element	*
 *   pghdlb_drawGrpbnd()	draw a box to indicate group boundary	*
 *   pghldb_updateSelection()	keeps track of the selected elements	*
 *   pghdlb_chkSelGfaHazType()  check selected GFA Hazard-type		*
 ***********************************************************************/

/*=====================================================================*/

void pghdlb_select ( VG_DBStruct *el, int location )
/************************************************************************
 * pghdlb_select							*
 *									*
 * Show the handlebar for the selected element.				*
 *									*
 * void pghdlb_select( el, location )					*
 *									*
 * Input parameters:							*
 *  *el		VG_DBStruct	the selected element			*
 *  location	int		range record location of element	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *		None.							*
 *									*
 **									*
 * Log:									*
 * E. Wehner/Eai		Initial coding                         	*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * E. Wehner/EAi	 9/97	Remove include file			*
 * C. Lin/EAI		10/97	rename from NxmHandleBar, cleanup	*
 *				use fixed attributed marker		*
 * E. Safford/GSC	03/98	mod to use _applyHdlb, cleanup		*
 * E. Safford/GSC	04/98	add location param			*
 * S. Law/GSC		04/98	cleanup, added calls to _selectGrp	*
 *				and _updateSelection			*
 * I. Durham/GSC	05/98	changed underscore decl. to an include	*
 * E. Safford/GSC	09/98	mod to handle modes (OBJ/GRP)		*
 * E. Safford/GSC	11/98	add new hdlb display for inc/dec	*
 * E. Safford/GSC	12/98	rename FUNC_NUMB_EDIT to FUNC_INC_DEC	*
 * S. Law/GSC		01/99	added watch box updates			*
 * D.W.Plummer/NCEP	 4/99	remove call to pgwbxw_setWlst		*
 * S. Law/GSC		04/00	added GRPTYP_CCF check			*
 * H. Zeng/EAI          08/01   revised group functionality             *
 * B. Yin/SAIC          07/04   added handling of TCA elements          *
 * X.Guo/CWS		01/10   Handle multi-select GFA elements	*
 ***********************************************************************/
{
int	iret, el_num, nelm,iopr;
char	mode;
/*---------------------------------------------------------------------*/

    mode = pgpalw_getMode();
    iopr = pgpalw_getCurOperId();
    if (el->hdr.grpnum &&
	(el->hdr.grptyp == GRPTYP_COMSYM ||
	 (el->hdr.grptyp == GRPTYP_CCF && 
	  pgpalw_getCurOperId() == FUNC_DELETE) ||
	 (el->hdr.grptyp && mode == TYPE_GRP))) {

        crg_ggnel(el->hdr.grptyp, el->hdr.grpnum, &nelm, &iret);
	if (nelm <= 0)
	    return;

	pghdlb_selGrp (el->hdr.grptyp, el->hdr.grpnum);
    }
    else {	
	if ((_numSelected + 1) >= MAX_EDITABLE_ELEMS) 
	    return;

	if ( iopr == FUNC_INC_DEC ) {
            pghdlb_setMark (SEL_INC_DEC, &iret);
        }
        else if ( pgpalw_isGrpActv() ) {
            pghdlb_setMark (SEL_GRP, &iret);
        }
	else if ( el->hdr.vg_type == TCA_ELM ) {
	    pghdlb_setMark (SEL_TCA, &iret);
	}
	else {
	    /*check GFA hazard type for multi select GFA elements*/
            if ( ( el->hdr.vg_type == GFA_ELM) && 
                 (iopr == FUNC_MULTISEL) ) {
		pghdlb_chkSelGfaHazType ( location, &iret );
		if ( iret < 0 ) {
		    return;
		}
	    }
	    pghdlb_setMark (SEL_ELM, &iret);
        }

        pghdlb_applyHdlb (el, &iret);

	if (el->hdr.vg_type == WBOX_ELM) {

	    pgwlst_setShowFlg (TRUE);
	    pgwpts_save (&(el->elem.wbx.latlon[0]),
			 &(el->elem.wbx.latlon[el->elem.wbx.info.numpts]));
	}

	if (iret >= 0) {
	    crg_getinx (location, &el_num, &iret);
	    pghdlb_updateSelection (TRUE, el_num, &iret);
	}
    }

    geplot( &iret );

}

/*=====================================================================*/

void pghdlb_selectGrp ( char grptyp, int grpnum )
/************************************************************************
 * pghdlb_selectGrp							*
 *									*
 * Show the handlebar for the selected group.				*
 *									*
 * void pghdlb_selectGrp( grptyp, grpnum )				*
 *									*
 * Input parameters:							*
 *  grptyp	char	        the selected group type			*
 *  grpnum	int		the selected group number       	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *		None.							*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          09/01   initial coding                          *
 ***********************************************************************/
{
int	iret, nelm;
/*---------------------------------------------------------------------*/

    crg_ggnel(grptyp, grpnum, &nelm, &iret);

    if (nelm <= 0) {
	   return;
    }

    pghdlb_selGrp (grptyp, grpnum);
  
    geplot( &iret );

}

/*=====================================================================*/

void pghdlb_selectAll ( int sflag )
/************************************************************************
 * pghdlb_selectAll							*
 *									*
 * Sets the select all the elements, groups, or both based on sflag.	*
 * This does not display the handlbars of the newly selected items.	*
 *									*
 * void pghdlb_selectAll (sflag)					*
 *									*
 * Input parameters:							*
 *	sflag		int	0 = elements only, 1 = both, 2 = groups	*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/98	Initial coding				*
 * J. Wu/SAIC		01/02	select only elem. on current layer	*
 * E. Safford/SAIC	02/02	correct logic in final if statement	*
 * E. Safford/SAIC	02/02	fixed handling of wiped range recs 	*
 ***********************************************************************/
{
    int		ii, ioff, iret, grpnum, cur_layer, el_layer;
    char	grptyp;
    Boolean	elm_flag, grp_flag, is_elm, is_grp;
/*---------------------------------------------------------------------*/

    elm_flag = FALSE;
    grp_flag = FALSE;

    if (sflag <= 1) elm_flag = TRUE;
    if (sflag >= 1) grp_flag = TRUE;

    cur_layer = pglayer_getCurLayer ();
    for (ii = 0; ii < MAX_EDITABLE_ELEMS; ii++) {
	crg_goffset (ii, &ioff, &iret);

	if (ioff < 0) {
	    el_layer = -1;
	}
	else {
            el_layer = crg_getLayer ( ioff );
	}
	
	if (ioff >= 0 && el_layer == cur_layer) {
	    crg_ggrp (ii, &grptyp, &grpnum, &iret);

	    if (grpnum && grptyp != GRPTYP_COMSYM) {
		is_elm = FALSE;
		is_grp = TRUE;
	    }
	    else {
		is_elm = TRUE;
		is_grp = FALSE;
	    }
	}
	else {
	    is_elm = FALSE;
	    is_grp = FALSE;
	}

	if ( (elm_flag && is_elm) || (grp_flag && is_grp) ) {
	    pghdlb_updateSelection (TRUE, ii, &iret);
	}
    }

}

/*=====================================================================*/

void pghdlb_setSelect ( int location )
/************************************************************************
 * pghdlb_setSelect                                                     *
 *                                                                      *
 * set the range record for the element to selected.  No update of the  *
 * displayed element is included.					*
 *                                                                      *
 * void pghdlb_setSelect( location )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  location    int             range record location of element        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *              None.                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       06/98   initial coding                          *
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 ***********************************************************************/
{
    int         iret, el_num;
/*---------------------------------------------------------------------*/

    if ((_numSelected + 1) >= MAX_EDITABLE_ELEMS)
        return;

    crg_getinx (location, &el_num, &iret);
    pghdlb_updateSelection (TRUE, el_num, &iret);
}


/*=====================================================================*/

void pghdlb_deselectEl ( int el_num, Boolean updt_dsply )
/************************************************************************
 * pghdlb_deselectEl							*
 *									*
 * This function unselects elements by clearing handlebars and		*
 * repainting one specific element					*
 *									*
 * void pghdlb_deselectEl (el_num, updt_dsply)				*
 *									*
 * Input parameters:							*
 *  el_num	int	element to be deselected			*
 *  updt_dsply	Boolean	element to be deselected			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *		None.							*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	03/98	copied from pghdlb_deselect		*
 * E. Safford/GSC	03/98	cleanup					*
 * S. Law/GSC		04/98	cleanup, added calls to _deselectGrp	*
 *				and _updateSelection			*
 * E. Safford/GSC	06/98	add updt_dsply parameter 		*
 * G. Krueger/EAI	05/99	Cleanup unused variables		*
 ***********************************************************************/
{
    int		iret, grpnum;
    char	grptyp;
    float	llx, lly, urx, ury;
/*---------------------------------------------------------------------*/

    crg_ggrp (el_num, &grptyp, &grpnum, &iret);

    if (grpnum && grptyp && (grptyp == GRPTYP_COMSYM ||
    				pgpalw_getMode() == TYPE_GRP)) {
	pghdlb_deselGrp (grptyp, grpnum, updt_dsply);
    }
    else {
	pghdlb_updateSelection (FALSE, el_num, &iret);

	if (updt_dsply) {
	    /*
	     * copy the background images, then refresh the VGF elements
	     */
            pghdlb_clearHdlb (el_num, &llx, &lly, &urx, &ury, &iret);
	    pgutls_refresh (llx, lly, urx, ury, &iret);
	}
    }
}

/*=====================================================================*/

void pghdlb_deselectGrp ( char grptyp, int grpnum )
/************************************************************************
 * pghdlb_deselectGrp							*
 *									*
 * This function unselects a group.                                     *
 *									*
 * void pghdlb_deselectGrp (grptyp, grpnum)				*
 *									*
 * Input parameters:							*
 *  grptyp	char	the deselected group type        		*
 *  grpnum	int	the deselected group number			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *		None.							*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          09/01   initial coding                          *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
 
    pghdlb_deselGrp (grptyp, grpnum, TRUE);

}

/*=====================================================================*/

void pghdlb_deselectAll ( void )
/************************************************************************
 * pghdlb_deselectAll							*
 *									*
 * This function unselects all elements by clearing handlebars and	*
 * repainting								*
 *									*
 * void pghdlb_deselectAll ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			None.						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	03/98	intial coding (copied pghdlb_deselect)	*
 * E. Safford/GSC	03/98	cleanup					*
 * C. Lin/GSC		04/98	add group				*
 * S. Law/GSC		04/98	cleanup, added calls to _deselectGrp	*
 *				and _updateSelection			*
 * S. Law/GSC		05/98	reduced to one large refresh		*
 * S. Law/GSC		07/98	added call to pggst_setText		*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 ***********************************************************************/
{
    int		iret;
    float	llx, lly, urx, ury;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    if (_numSelected) {

	/*
	 * clear each element
	 */

	crg_deselect (&llx, &lly, &urx, &ury);
	_numSelected = 0;

	/*
	 * Unset ghost text & circle
	 */
	el.hdr.vg_class = 0;
	pggst_setText (&el);
	pggst_setCircle (&el);

	/*
	 * copy background images, then refresh the VGF elements
	 */
	pgutls_refresh (llx, lly, urx, ury, &iret);
    }
}

/*=====================================================================*/

void pghdlb_displayAllSel ( void )
/************************************************************************
 * pghdlb_displayAllSel							*
 *									*
 * Deselect all elements then display all selected elements.		*
 *									*
 * void pghdlb_displayAllSel( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *		None.							*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	03/98	copied from pghdlb_select		*
 * E. Safford/GSC	03/98	cleanup					*
 * S. Law/GSC		04/98	cleanup					*
 * E. Safford/GSC	09/98	modify redisplayl for mode (OBJ/GRP)	*
 * E. Safford/GSC	10/98	modify for param change to cvg_rdrec	*
 * E. Safford/GSC 	11/98	add new hdlb display for inc/dec	*
 * E. Safford/GSC	02/99	mod to handle FUNC_SHOW_GRPS		*
 * W. Li/EAI            03/99   modify undisplayGrp for FUNC_INC_DEC    *
 * E. Safford/GSC	03/99	use pghdlb_showAllSel			*
 * G. Krueger/EAI	05/99	Cleanup unused variables		*
 ***********************************************************************/
{
int		ii, jj, iret, num_els, grpnum;
int		grpid, *grplist, max_grps, cur_oper;
float		llx, lly, urx, ury;
char		sel_flag, grptyp;
/*---------------------------------------------------------------------*/

    if (_numSelected <= 0) {
        return;
    }

    cur_oper = pgpalw_getCurOperId();
    max_grps = pggrpw_grpCount ();
    grplist = (int *) malloc (max_grps * sizeof (int));

    /*
     *  initialize
     */
    for (jj = 0; jj < max_grps; jj++)  {
        grplist[jj] = -1;
    }

    num_els = _numSelected;

    /*
     *  undisplay all currently selected elements
     */
    for (ii=0; (num_els && (ii < MAX_EDITABLE_ELEMS)); ii++) {
  	crg_gsel (ii, &sel_flag, &iret);
	 
	if (sel_flag) {
	    crg_ggrp (ii, &grptyp, &grpnum, &iret);

	    if (grpnum && grptyp) {
	        jj = 0;
		grpid = (grpnum * 100) + grptyp;
		while (jj < max_grps && grplist[jj] != -1) {
		    if (grpid == grplist[jj++])  {
		        grpid = 0;
		    }
		}

		if (grpid > 0) {
		    if (jj < max_grps) { 
		        grplist[jj] = grpid;
		    }

		    if (cur_oper != FUNC_INC_DEC) {
		        pghdlb_undisplayGrp(grptyp, grpnum);
		    }
		}
	    }
	    else {	
                pghdlb_clearHdlb (ii, &llx, &lly, &urx, &ury, &iret);
	    }
	    num_els--;
	}
    }

    free (grplist);
    pghdlb_showAllSel(); 
}

/*=====================================================================*/

void pghdlb_showAllSel ( void )
/************************************************************************
 * pghdlb_showAllSel   							*
 *									*
 * Draw the handlebar for all selected elements.  The selected elements	*
 * are not de-selected first (as is done by displayAllSel).		*
 *									*
 * void pghdlb_showAllSel ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *		None.							*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	03/99	copied from pghdlb_displayAllSel	*
 * G. Krueger/EAI	05/99	Cleanup unused variables		*
 * H. Zeng/EAI          08/01   revised group functionality             *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * E. Safford/SAIC	11/03	added case for handlebars during smear  *
 * J. Wu/SAIC		03/04	add FUNC_INTERP				*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block pointers			*
 ***********************************************************************/
{
int		location, ii, jj, iret, num_els, grpnum;
int		grpid, *grplist, max_grps, cur_oper, actvElemLoc;
char		sel_flag, grptyp;
VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    if (_numSelected <= 0) {
        return;
    }

    cur_oper = pgpalw_getCurOperId();
    max_grps = pggrpw_grpCount ();
    grplist = (int *) malloc (max_grps * sizeof (int));

    /*
     *  initialize
     */
    for (jj = 0; jj < max_grps; jj++)  {
        grplist[jj] = -1;
    }

    num_els = _numSelected;

    /*
     *  display the handlebars on all selected elements
     */
    for (ii=0; (num_els && ii < MAX_EDITABLE_ELEMS); ii++) {
  	crg_gsel (ii, &sel_flag, &iret);	 

	if (sel_flag) {
	    crg_ggrp (ii, &grptyp, &grpnum, &iret);

	    if ( grpnum && grptyp && (grptyp == GRPTYP_COMSYM || 
		 		pgpalw_getMode() == TYPE_GRP || 
		 		cur_oper == FUNC_SHOW_GRPS) ) {
	        jj = 0;
		grpid = (grpnum * 100) + grptyp;

		while (jj < max_grps && grplist[jj] != -1) {
		    if (grpid == grplist[jj++])  {
		        grpid = 0;
		    }
		}

		if (grpid > 0) {
		    if (jj < max_grps)  {
		        grplist[jj] = grpid; 
		    }
		    pghdlb_displayGrp(grptyp, grpnum);
		}
	    }
	    else {	
	        crg_goffset (ii, &location, &iret);

		if (pgpalw_getCurOperId() == FUNC_INC_DEC) {
		    pghdlb_setMark (SEL_INC_DEC, &iret);
		}
                else if ( pgpalw_isGrpActv() ) {
		    pghdlb_setMark (SEL_GRP, &iret);
                }
		else if ( (pgpalw_getCurOperId() == FUNC_SMEAR) ||
		          (pgpalw_getCurOperId() == FUNC_INTERP) ) {

		    /*
		     *  Use standard handlebar on initial selection,
		     *  and smear handlebars on all confirmed 
		     *  selections. Same rules apply to INTERP.
		     */
		    actvElemLoc = pgactv_getElmLoc( );

		    if ( location != actvElemLoc ) {
                       pghdlb_setMark( SEL_SMEAR, &iret );
                    }
		    else {
		       pghdlb_setMark (SEL_ELM, &iret);
                    }
                }
		else {
		    pghdlb_setMark (SEL_ELM, &iret);
		}

	        cvg_rdrec(cvg_getworkfile(), location, &el, &iret);
                pghdlb_applyHdlb (&el, &iret);

                /*
                  * Free TCA break point/GFA block memory
                  */
                if ( el.hdr.vg_type == TCA_ELM ) {
                   cvg_freeBkpts ( &el );
                }
                else if ( el.hdr.vg_type == GFA_ELM ) {
                    cvg_freeElPtr ( &el );
                }
		
	    }
	    num_els--;
	}
    }
    
    geplot( &iret );
    free (grplist);

}
/*=====================================================================*/

void pghdlb_grpAllSel ( char grptyp, int grpnum )
/************************************************************************
 * pghdlb_grpAllSel							*
 *									*
 * Groups all selected element(s).					*
 *									*
 * void pghdlb_grpAllSel(grptyp, grpnum)				*
 *									*
 * Input parameters:							*
 *  grptyp	char		Group type				*
 *  grpnum	int		Group number				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *		None.							*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		04/98						*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 ***********************************************************************/
{
int     i, n, location, iret;
char	sel_flag;
/*---------------------------------------------------------------------*/

    i = 0;
    n = _numSelected; 
    while (n && (i < MAX_EDITABLE_ELEMS)) {
  	crg_gsel (i, &sel_flag, &iret);	 

	if (sel_flag) {
	    crg_goffset(i, &location, &iret);
	    cvg_setginf(cvg_getworkfile(), location, grptyp, grpnum, &iret);
	    n--;
	}

	i++;
    }


}

/*=====================================================================*/

int pghdlb_elemSelected ( void )
/************************************************************************
 * pghdlb_elemSelected                                                  *
 *                                                                      *
 * Return the number of elements are currently selected.                *
 *                                                                      *
 * int pghdlb_elemSelected ()                                           *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * 	None.								*
 *									*
 * Return parameters:                                                   *
 *	pghdlb_elemSelected	int	 	True if elmnts selected	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       03/98   copied from pghdlb_deselect             *
 * C. Lin/EAI           04/98   modified to return the number selected  *
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 ***********************************************************************/
{ 
    if (_numSelected <= 0)
        return (0);
    else
        return (_numSelected);
}

/*=====================================================================*/

void pghdlb_getNextIndex ( int cur_index, int *sel_el, int *sel_loc, int *iret )
/************************************************************************
 * pghdlb_getNextIndex                                                  *
 *                                                                      *
 * Returns the range array index number of the next currently selected  *
 * element.								*
 *                                                                      *
 * void pghdlb_getNextIndex (cur_index, sel_el, sel_loc, iret)          *
 *                                                                      *
 * Input parameters:                                                    *
 *   	cur_index	int	index of starting element for search	*
 *				  use -1 to start at first element	*
 *									*
 * Output parameters:                                                   *
 *	*sel_el		int	range array index number                *
 *	*sel_loc	int	location of next selected elment        *
 *	*iret		int	return code				*
 *				  -1 	no more elements selected	*
 * Return parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       03/98   initial coding                          *
 * G. Krueger/EAI	05/99	Cleanup prologue & unused variables	*
 ***********************************************************************/
{
int	i;
char	sel_flag;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (!(_numSelected)) {
	*iret = -1;
    }
    else {
	i = cur_index;
	do { 
	    i++;
	    crg_gsel (i, &sel_flag, iret); 
	}  while (!(sel_flag) && (i < MAX_EDITABLE_ELEMS));  

	if (sel_flag) {
	   crg_goffset (i, sel_loc, iret);
	   *sel_el  = i; 
	}
	else {
	   *iret = -1;
	}   
    }
}

/*=====================================================================*/

static void pghdlb_updateSelection ( char selected, int el_num, int *iret ) 
/************************************************************************
 * pghdlb_updateSelection						*
 *									*
 * Updates whether an element is selected in the crg range list and	*
 * adjusts _numSelected							*
 *									*
 * static void pghdlb_updateSelection (selected, el_num, iret)		*
 *									*
 * Input parameters:							*
 *	selected	char	selected flag				*
 *	el_num		int	element number				*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 *	*iret		int	return value				*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/98	initial coding				*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * E. Safford/SAIC	10/01	rm init '_' in func name & make static  *
 ***********************************************************************/
{
    char oldsel;
/*---------------------------------------------------------------------*/

    crg_gsel (el_num, &oldsel, iret);
    crg_ssel (el_num, selected, iret);

    if (oldsel != selected) {
	if (selected == TRUE) {
	    _numSelected++;
	}
	else {
	    _numSelected--;
	}
    }
}

/*=====================================================================*/

static void pghdlb_setMark ( int type, int *iret )
/************************************************************************
 * pghdlb_setMark                                                       *
 *                                                                      *
 * Set the gmark attributes for handlebars.                             *
 *                                                                      *
 * static void pghdlb_setMark ( type, iret )                            *
 *                                                                      *
 * Input parameters:                                                    *
 *  type       int              SEL_ELM - element, SEL_GRP - group      *
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret       int             return code                             *
 *                                                                      *
 * Return parameters:                                                   *
 *             None.                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       03/98   initial coding                          *
 * C. Lin/EAI       	04/98   add type into calling sequence,rearrange*
 * E. Safford/GSC 	11/98	add new hdlb display for inc/dec	*
 * E. Safford/GSC 	04/99	change inc/dec hdlb width & color    	*
 * G. Krueger/EAI	05/99	Cleanup unused variables		*
 * E. Safford/SAIC	10/01	rm init '_' in func name & make static  *
 * E. Safford/SAIC	11/03	add handling of SEL_SMEAR               *
 * B. Yin/SAIC		07/04	add handling of SEL_TCA                 *
 ***********************************************************************/
{
float       mksiz;
int         imkcolr, imktyp, imkhw, imkwid;
/*---------------------------------------------------------------------*/

    if ( type == SEL_ELM ) { 
        imkcolr = ELM_COLOR;
        imktyp  = ELM_MARK; 
    }
    else if ( type == SEL_INC_DEC) {
	imkcolr = INC_DEC_COLOR;
	imktyp  = INC_DEC_MARK;
    }
    else if ( type == SEL_SMEAR ) {
        imkcolr = SMEAR_COLOR;
        imktyp  = SMEAR_MARK;
    }
    else if ( type == SEL_TCA ) {
        imkcolr = TCA_COLOR;
        imktyp  = TCA_MARK;
    }
    else {
	imkcolr = GRP_COLOR;
        imktyp  = GRP_MARK;
    }

    imkhw   = 1;
    mksiz   = 1.0F;
    imkwid  = 2;

    gscolr(&imkcolr, iret);
    gsmrkr(&imktyp, &imkhw, &mksiz, &imkwid, iret);

}

/*=====================================================================*/

static void pghdlb_selGrp ( char grptyp, int grpnum )
/************************************************************************
 * pghdlb_selGrp							*
 *									*
 * Display the specified group.						*
 *									*
 * static void pghdlb_selGrp (grptyp, grpnum)				*
 *									*
 * Input parameters:							*
 *	grptyp		char	group type				*
 *	grpnum		int	group number				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		04/98						*
 * S. Law/GSC		04/98	cleanup					*
 * E. Safford/GSC	10/98	modify for param change to cvg_rdrec	*
 * G. Krueger/EAI	05/99	Cleanup unused variables		*
 * H. Zeng/EAI          09/01   removed unused variables                *
 * E. Safford/SAIC	11/01	rename and make static			*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * B. Yin/SAIC          07/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block pointers			*
 ***********************************************************************/
{
int	ii, iret;
int	location, nelm, *inxarry;
float   rl, rr, rt, rb;
VG_DBStruct el;
/*---------------------------------------------------------------------*/

    crg_ggnel(grptyp, grpnum, &nelm, &iret);
    if ( nelm <= 0 ) {
	return;
    }

    if ((_numSelected + nelm) >= MAX_EDITABLE_ELEMS) 
	return;

    crg_ggbnd(grptyp, grpnum, &rl, &rr, &rt, &rb, &iret);
    if ( iret != 0 ) {
	return;
    }

    /*
     * change marker on each element
     */
    pghdlb_setMark (SEL_GRP, &iret);

    inxarry = (int *)malloc(nelm*sizeof(int));
    crg_gginx(grptyp, grpnum, nelm, inxarry, &nelm, &iret);

    for ( ii = 0; ii < nelm; ii++ ) {
	crg_goffset(inxarry[ii], &location, &iret);
        cvg_rdrec(cvg_getworkfile(), location, &el, &iret);
        pghdlb_applyHdlb(&el, &iret);
	pghdlb_updateSelection (TRUE, inxarry[ii], &iret);

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
    free(inxarry);

    /*
     *  draw group bound
     */
    if ( iret == 0 ) {
	pghdlb_drawGrpbnd( rl, rr, rt, rb );
    }
}

/*=====================================================================*/

static void pghdlb_deselGrp ( char grptyp, int grpnum, Boolean updt_dsply )
/************************************************************************
 * pghdlb_deselGrp							*
 *									*
 * This function clears handlebars for a group.				*
 *									*
 * static void pghdlb_deselGrp (grptyp, grpnum, updt_dsply)		*
 *									*
 * Input parameters:                                                    *
 *  grptyp	char	group type					*
 *  grpnum	int	group number					*
 *  updt_dsply  Boolean control for updating the display		*
 *									*
 * Output parameters:							*
 * Return parameters:                                                   *
 *		None.							*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/98	moved from pghdlb_deselectEl		*
 * E. Safford/GSC	06/98	add updt_dsply parameter 		*
 * E. Safford/SAIC	11/01	rename and make static			*
 ***********************************************************************/
{
    float	llx, lly, urx, ury;
    int		nelm, iret, *inxarry, ii;
/*---------------------------------------------------------------------*/

    crg_ggnel(grptyp, grpnum, &nelm, &iret);
    if (nelm <= 0) 
	return;

    inxarry = (int *)malloc(nelm*sizeof(int));
    crg_gginx (grptyp, grpnum, nelm, inxarry, &nelm, &iret);

    for (ii = 0; ii < nelm; ii++ ) {
	if (updt_dsply) {
	    pghdlb_clearHdlb (inxarry[ii], &llx, &lly, &urx, &ury, &iret);
	}
	pghdlb_updateSelection (FALSE, inxarry[ii], &iret);
    }

    free(inxarry);

    if (updt_dsply) {
        crg_ggbnd (grptyp, grpnum, &llx, &urx, &ury, &lly, &iret);
        if (iret != 0)
	    return;

        llx -= EXTRA;
        lly -= EXTRA;
        urx += EXTRA;
        ury += EXTRA;

        /*
         * copy the background images, then refresh the VGF elements
         */
        pgutls_refresh (llx, lly, urx, ury, &iret);
    }

}

/*=====================================================================*/

static void pghdlb_displayGrp ( char grptyp, int grpnum )
/************************************************************************
 * pghdlb_displayGrp							*
 *									*
 * This function displays the handlebars for a group.			*
 *									*
 * static void pghdlb_displayGrp (grptyp, grpnum)			*
 *									*
 * Input parameters:                                                    *
 *  grptyp	char	group type					*
 *  grpnum	int	group number					*
 *									*
 * Output parameters:							*
 * Return parameters:                                                   *
 *		None.							*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/98	initial coding				*
 * E. Safford/GSC	10/98	modify for param change to cvg_rdrec	*
 * E. Safford/SAIC	10/01	rm init '_' in func name & make static  *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block pointers			*
 ***********************************************************************/
{
    int		nelm, iret, *inxarry, ii;
    int		location;
    float	rl, rr, rt, rb;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    crg_ggnel(grptyp, grpnum, &nelm, &iret);
    if (nelm <= 0) 
	return;

    inxarry = (int *)malloc(nelm*sizeof(int));
    crg_gginx (grptyp, grpnum, nelm, inxarry, &nelm, &iret);

    crg_ggbnd (grptyp, grpnum, &rl, &rr, &rt, &rb, &iret);
    if (iret != 0)
	return;

    pghdlb_setMark (SEL_GRP, &iret);

    for ( ii = 0; ii < nelm; ii++ ) {
	crg_goffset(inxarry[ii], &location, &iret);
        cvg_rdrec(cvg_getworkfile(), location, &el, &iret);
        pghdlb_applyHdlb(&el, &iret);

        /*
         * Free TCA break point/GFA block  memory
         */
        if ( el.hdr.vg_type == TCA_ELM ) {
           cvg_freeBkpts ( &el );
        }
        else if ( el.hdr.vg_type == GFA_ELM ) {
           cvg_freeElPtr ( &el );
        }
    }

    free(inxarry);

    if ( iret == 0 ) {
	pghdlb_drawGrpbnd(rl, rr, rt, rb);
    }
}

/*=====================================================================*/

static void pghdlb_undisplayGrp ( char grptyp, int grpnum )
/************************************************************************
 * pghdlb_undisplayGrp							*
 *									*
 * This function removes the handlebars for a group.			*
 *									*
 * static void pghdlb_undisplayGrp (grptyp, grpnum)			*
 *									*
 * Input parameters:                                                    *
 *  grptyp	char	group type					*
 *  grpnum	int	group number					*
 *									*
 * Output parameters:							*
 * Return parameters:                                                   *
 *		None.							*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/98	initial coding				*
 * E. Safford/SAIC	10/01	rm init '_' in func name & make static  *
 ***********************************************************************/
{
    float	llx, lly, urx, ury;
    int		nelm, iret, *inxarry, ii;
/*---------------------------------------------------------------------*/

    crg_ggnel(grptyp, grpnum, &nelm, &iret);
    if (nelm <= 0) 
	return;

    inxarry = (int *)malloc(nelm*sizeof(int));
    crg_gginx (grptyp, grpnum, nelm, inxarry, &nelm, &iret);

    for (ii = 0; ii < nelm; ii++ ) {
	pghdlb_clearHdlb (inxarry[ii], &llx, &lly, &urx, &ury, &iret);
    }

    crg_ggbnd (grptyp, grpnum, &llx, &urx, &ury, &lly, &iret);
    if (iret != 0)
	return;

    llx -= EXTRA;
    lly -= EXTRA;
    urx += EXTRA;
    ury += EXTRA;

    /*
     * copy the background images, then refresh the VGF elements
     */
    pgutls_refresh (llx, lly, urx, ury, &iret);

    free(inxarry);

}

/*=====================================================================*/

static void pghdlb_applyHdlb ( VG_DBStruct *el, int *iret )
/************************************************************************
 * pghdlb_applyHdlb                                                     *
 *                                                                      *
 * Apply the handlebar to the element.					*
 *                                                                      *
 * static void pghdlb_applyHdlb ( el, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *  *el		VG_DBStruct	the selected element			*
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret	int		return code				*
 *									*
 * Return parameters:                                                   *
 *             None.                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/98	initial coding				*
 * E. Safford/GSC	03/98	cleanup					*
 * S. Law/GSC		11/98	added call to pgwlst_update		*
 * S. Law/GSC		01/99	moved watch list updates to _select	*
 * S. Law/GSC		05/99	added TRKSTORM_ELM			*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * S. Law/GSC		08/99	added SIGINTL_ELM			*
 * S. Law/GSC		08/99	added remaining SIGMETs			*
 * S. Law/GSC		02/00	Added CCF				*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * E. Safford/SAIC	10/01	rm init '_' in func name & make static  *
 * J. Wu/SAIC		11/02	add LIST_ELM				*
 * H. Zeng/XTRIA	07/03	added volcano element			*
 * J. Wu/SAIC		10/03	add JET_ELM				*
 * H. Zeng/XTRIA	10/03   added ash cloud element			*
 * J. Wu/SAIC		11/03	put handbar on a selected jet barb/hash	*
 * J. Wu/SAIC		02/04	add GFA_ELM				*
 * B. Yin/SAIC		04/04	add TCA_ELM				*
 * B. Yin/SAIC		05/04	Changed the color of TCA handle bars 	*
 * B. Yin/SAIC		07/04	Modified to handle TCA water and islands*
 * J. Wu/SAIC		10/04	access GFA attr with cvg_getFld()	*
 ***********************************************************************/
{
    float	plat, plon;
    int		ii, jj, np, cur_act, sel, ier;
    char	value[32];    
/*---------------------------------------------------------------------*/

    np = 1;
    if ((el->hdr.vg_class == CLASS_SYMBOLS ) ||
        (el->hdr.vg_class == CLASS_TEXT )    ||
	(el->hdr.vg_class == CLASS_WINDS )) {

        plat = el->hdr.range_min_lat;
        plon = el->hdr.range_min_lon;
        gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
    }
    else {
        switch(el->hdr.vg_type) {

	  case FRONT_ELM:
	    for (ii = 0; ii < el->elem.frt.info.numpts; ii++) {
		plat = el->elem.frt.latlon[ii];
		plon = el->elem.frt.latlon[ii+el->elem.frt.info.numpts];
		gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M) );
	    }

	    break;

	  case LINE_ELM:
	    for (ii = 0; ii < el->elem.lin.info.numpts; ii++) {
		plat = el->elem.lin.latlon[ii];
		plon = el->elem.lin.latlon[ii+el->elem.lin.info.numpts];
		gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
	    }

	    break;

	  case SPLN_ELM:
	    for (ii = 0; ii < el->elem.spl.info.numpts; ii++) {
		plat = el->elem.spl.latlon[ii];
		plon = el->elem.spl.latlon[ii+el->elem.spl.info.numpts];
		gmark( sys_M, &np, &plat, &plon, iret, strlen (sys_M));
	    }

	    break;

	  case WBOX_ELM:
	    for (ii = 0; ii < el->elem.wbx.info.numpts; ii++) {              
		plat = el->elem.wbx.latlon[ii];
		plon = el->elem.wbx.latlon[ii+el->elem.wbx.info.numpts];
		gmark( sys_M, &np, &plat, &plon, iret, strlen (sys_M));
	    }

	    break;

	  case TRKSTORM_ELM:
	    for (ii = 0; ii < el->elem.trk.info.npts; ii++) {
		plat = el->elem.trk.latlon[ii];
		plon = el->elem.trk.latlon[ii+el->elem.trk.info.npts];
		gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
	    }

	    break;

	  case SIGAIRM_ELM:
	  case SIGCONV_ELM:
	  case SIGINTL_ELM:
	  case SIGNCON_ELM:
	  case SIGOUTL_ELM:
	    for (ii = 0; ii < el->elem.sig.info.npts; ii++) {
		plat = el->elem.sig.latlon[ii];
		plon = el->elem.sig.latlon[ii+el->elem.sig.info.npts];
		gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
	    }

	    break;

	  case SIGCCF_ELM:
	    for (ii = 0; ii < el->elem.ccf.info.npts; ii++) {
		plat = el->elem.ccf.latlon[ii];
		plon = el->elem.ccf.latlon[ii+el->elem.ccf.info.npts];
		gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
	    }

	    break;

	  case VOLC_ELM:
            plat = el->hdr.range_min_lat;
            plon = el->hdr.range_min_lon;
            gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));

	    break;

	  case ASHCLD_ELM:
	    for (ii = 0; ii < el->elem.ash.info.npts; ii++) {
		plat = el->elem.ash.latlon[ii];
		plon = el->elem.ash.latlon[ii+el->elem.ash.info.npts];
		gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
	    }

	    break;

	  case CIRCLE_ELM:
	    for ( ii = 0; ii < el->elem.cir.info.numpts; ii++ ) {
		plat = el->elem.cir.data.latlon[ii];
		plon = el->elem.cir.data.latlon[ii+el->elem.cir.info.numpts];
		gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
	    }

            break;

	  case LIST_ELM:
	    for (ii = 0; ii < el->elem.lst.data.nitems; ii++) {
		plat = el->elem.lst.data.lat[ii];
		plon = el->elem.lst.data.lon[ii];
		gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
	    }

            break;

	  case JET_ELM:
	    
	    cur_act = pgjet_getCurAction ();
	    sel = pgjet_getSelectedSub ();
	    
	    if ( sel >= 0 )  {
	        if ( cur_act == 1 ||  cur_act == 3 ) {
		    plat = el->elem.jet.barb[sel].wnd.data.latlon[0];
		    plon = el->elem.jet.barb[sel].wnd.data.latlon[1];
		    gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
		}
		else if ( cur_act == 4 ||  cur_act == 6 ) {
		    plat = el->elem.jet.hash[sel].wnd.data.latlon[0];
		    plon = el->elem.jet.hash[sel].wnd.data.latlon[1];
		    gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
		}
	    }
	    else {
	        for (ii = 0; ii < el->elem.jet.line.spl.info.numpts; ii++) {
		    plat = el->elem.jet.line.spl.latlon[ii];
		    plon = el->elem.jet.line.spl.latlon[ii +
				el->elem.jet.line.spl.info.numpts];
		    gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
	        }
	    }

            break;

	  case GFA_ELM:
	    
	    if ( !pggfaw_isTxtActive() ) {
	        for (ii = 0; ii < el->elem.gfa.info.npts; ii++) {
		    plat = el->elem.gfa.latlon[ii];
		    plon = el->elem.gfa.latlon[ii + el->elem.gfa.info.npts];
		    gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
	        }
            }
	    else {
	        cvg_getFld ( el, TAG_GFA_LAT, value, &ier );		
		plat = atof ( value );
	        cvg_getFld ( el, TAG_GFA_LON, value, &ier );		
		plon = atof ( value );
		gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));	    
	    }
	    
            break;

	  case TCA_ELM:

	    if ( !pgtca_isNewSeg() ) {

	       for ( ii = 0; ii < el->elem.tca.info.wwNum; ii++){
		   for ( jj = 0; jj < el->elem.tca.info.tcaww[ ii ].numBreakPts; jj++ ) {
		       plat = el->elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lat;
		       plon = el->elem.tca.info.tcaww[ ii ].breakPnt[ jj ].lon;
		       gmark( sys_M, &np, &plat, &plon, iret, strlen(sys_M));
		   }
 	       }
	    }

	    break;
	
	  default:
	    break;
	}
    }
}

/*=====================================================================*/

static void pghdlb_clearHdlb ( int el_num, float *llx, float *lly, 
					float *urx, float *ury, int *iret )
/************************************************************************
 * pghdlb_clearHdlb							*
 *                                                                      *
 * This function clears handlebars for one element and returns the      *
 * display coordinates from the range array.				*
 *                                                                      *
 * static void pghdlb_clearHdlb (el_num, llx, lly, urx, ury, iret)	*
 *                                                                      *
 * Input parameters:                                                    *
 *  el_num	int	element to be deselected			*
 *									*
 * Output parameters:                                                   *
 *	*llx		float		lower left x coordinate		*
 *	*lly		float		lower left y coordinate		*
 *	*urx		float		upper right x coordinate	*
 *	*ury		float		upper right y coordinate	*
 *	*iret		int		return code			*
 *									*
 * Return parameters:                                                   *
 * 	None.								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/98	copied from pghdlb_deselect             *
 * E. Safford/GSC	10/98	modify for param change to cvg_rdrec	*
 * D.W.Plummer/NCEP	 4/99	remove call to pgwlst_clear		*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 * E. Safford/SAIC	10/01	rm init '_' in func name & make static  *
 * J. Wu/SAIC		01/02	add layer param in crg_get() call	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		07/04	add filter param to crg_get()		*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   remove redundant call to cvg_rdrec()	*
 ***********************************************************************/
{
    int		offset, el_layer;
    filter_t	filter;
/*---------------------------------------------------------------------*/

    crg_goffset( el_num, &offset, iret );

    crg_get( el_num, &el_layer, filter, llx, lly, urx, ury, iret);

}

/*=====================================================================*/

static void pghdlb_drawGrpbnd ( float rl, float rr, float rt, float rb )
/************************************************************************
 * pghdlb_drawGrpbnd							*
 *									*
 * Draw a box to indicate group boundary.				*
 *									*
 * static void pghdlb_drawGrpbnd (rl, rr, rt, rb)			*
 *									*
 * Input parameters:							*
 *	rl	float	left x coordinate				*
 *	rr	float	right x coordinate				*
 *	rt	float	top y coordinate				*
 *	rb	float	bottom y coordinate				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			None.						*
 **									*
 * Log:									*
 * C. Lin/EAI		05/98    					*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * E. Safford/SAIC	10/01	rm init '_' in func name & make static  *
 ***********************************************************************/
{
int   np, ityp, ilthw, iwdth, ilwhw, iret;
float x[5], y[5];
/*---------------------------------------------------------------------*/

        ityp  = 1;
        ilthw = 0;
        iwdth = 1;
        ilwhw = 0;
        gsline(&ityp, &ilthw, &iwdth, &ilwhw, &iret);

        np = 5;
        x[0] = rl;   y[0] = rb;
        x[1] = rr;   y[1] = rb;
        x[2] = rr;   y[2] = rt;
        x[3] = rl;   y[3] = rt;
        x[4] = x[0]; y[4] = y[0];

        gline(sys_D, &np, x, y, &iret, strlen(sys_D));

}

/*=====================================================================*/

static void pghdlb_chkSelGfaHazType ( int selected, int *iret )

/************************************************************************
 * pghdlb_chkSelGfaHazType						*
 *									*
 * Check selected GFA element and see if all selected elements with the	*
 * same hazard type.							*
 *									*
 * static void pghdlb_chkSelGfaHazType ( )				*
 *									*
 * Input parameters:                                                    *
 *  selected         int     the selected element location              *
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret       int             return code                             *
 *                                                                      *
 * Return parameters:                                                   *
 *             None.                                                    *
 *									*
 **									*
 * Log:									*
 * X.Guo/CWS           01/10   Initial coding				*
 ***********************************************************************/
{
int		location, ii, num_els,ier,areaType,areaType1,subtype;
char		sel_flag;
char		tagstr[64],tagstr1[64],value[32];
VG_DBStruct	el,el1;
/*---------------------------------------------------------------------*/
    *iret = -1;
    num_els = _numSelected;
    cvg_rdrec(cvg_getworkfile(), selected, &el, &ier);

    cvg_getFld ( &el, TAG_GFA_SUBTYPE, value, &ier );
    subtype = atoi( value ) - atoi( value )/10 * 10;
    if ( subtype != GFA_SNAPSHOT ) {
        /*
         * Free TCA break point/GFA block memory
         */
        if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
        }
        else if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
        }
	return;
    }

    if (_numSelected <= 0) {
        *iret = 1;
        return;
    }
    num_els = _numSelected;

    cvg_getFld ( &el, TAG_GFA_AREATYPE, tagstr, &ier );
    areaType = pggfawp_getHazardType ( tagstr );
    /*
     * Free TCA break point/GFA block memory
     */
    if ( el.hdr.vg_type == TCA_ELM ) {
        cvg_freeBkpts ( &el );
    }
    else if ( el.hdr.vg_type == GFA_ELM ) {
               cvg_freeElPtr ( &el );
    }

    if ( areaType < 0 ) {
        return;
    }
    /*
     * check on all selected elements
     */
    for (ii=0; (num_els && ii < MAX_EDITABLE_ELEMS); ii++) {
  	crg_gsel (ii, &sel_flag, &ier);	 

	if (sel_flag) {
	    crg_goffset (ii, &location, &ier);

	    cvg_rdrec(cvg_getworkfile(), location, &el1, &ier);
            if ( ier >= 0 ) {
	        break;  	    
	    }
            /*
             * Free TCA break point/GFA block memory
            */
           if ( el1.hdr.vg_type == TCA_ELM ) {
               cvg_freeBkpts ( &el1 );
           }
           else if ( el1.hdr.vg_type == GFA_ELM ) {
               cvg_freeElPtr ( &el1 );
           }
		
	   num_els--;
	}
    }
    if ( ier >= 0 ) {
	cvg_getFld ( &el1, TAG_GFA_AREATYPE, tagstr1, &ier );
        if ((areaType1 = pggfawp_getHazardType ( tagstr1 ) ) > 0 ) {
	    if ( areaType == areaType1 ) {
		*iret = 1;
	    }
        }
    }
    /*
     * Free TCA break point/GFA block memory
     */
    if ( el1.hdr.vg_type == TCA_ELM ) {
        cvg_freeBkpts ( &el1 );
    }
    else if ( el1.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &el1 );
    }
}
/*=====================================================================*/
