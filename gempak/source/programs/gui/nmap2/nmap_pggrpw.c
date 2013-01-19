#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "hints.h"
#include "proto_xw.h"

static Widget		_group_dlgW;
static Widget		_group_typeW;
static WidgetList	_group_buttonW;

static int	_numGrp = 0, _curIndex = 0;
static int      _selectTyp = 1;
static int      _selectNum;
static Boolean  _grpEmpty = TRUE;

/*
 *  private functions
 */
void pggrpw_setGrpType ( void );

/*
 *  private callback functions
 */
void pggrpw_grpActionCb( Widget, long, XtPointer );

/************************************************************************
 * nmap_pggrpw.c							*
 *									*
 * This module creates and displays the VG group selection box. It also	*
 * contains the callbacks for the box.					*
 *									*
 * CONTENTS:								*
 *	pggrpw_create()		Creates VG group selection window	*
 *	pggrpw_popup()		Shows the VG group selection box.	*
 *	pggrpw_popdown()	Unmanage VG group selection box.	*
 *	pggrpw_isUp()	        check if VG group selection box is up.	*
 *                                                                      *
 *	pggrpw_grpCount()	return the current number of groups	*
 *      pggrpw_getGrpType()     query the current group type choice     *
 *      pggrpw_setGrpType()     set group type for GROUP window         *
 *      pggrpw_getGrpNum()      query the active group number           *
 *      pggrpw_getEmptFlg()     query the value of group empty flag     *
 *      pggrpw_setEmptFlg()     set the value of group empty flag       *
 *      pggrpw_startGrp()       generate group from selected elements   *
 *      pggrpw_addtoGrp()       adds a element to a group               *
 *      pggrpw_rmvfrmGrp()      removes a element from a group          *
 *									*
 *	pggrpw_grpActionCb()	Callback for GROUP menu grp action gui	*
 ***********************************************************************/

/*=====================================================================*/

Widget pggrpw_create ( Widget parent )
/************************************************************************
 * pggrpw_create							*
 *									*
 * This function creates a VG group selection window only and call	*
 * two creation function to create type box and control buttons		*
 *									*
 * Widget pggrpw_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 * pggrpw_create	Widget		ID of the group popup window	*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	 6/97						*
 * E. Wehner/EAi	 8/97	Remove reference to MAX_EDIT		*
 * C. Lin/EAI	        10/97	rename from NxmGroupCr, cleanup		*
 * C. Lin/EAI	        04/98	use group table structure, add ctrl btns*
 * C. Lin/EAI	        09/98	replace "Close" with "Cancel"		*
 * W. Li/EAI		05/99	split creation to three parts		*
 * H. Zeng/EAI          03/01   removed pggrpw_rtbl()                   *
 * H. Zeng/EAI          10/01   revised for new GROUP functionality     *
 * H. Zeng/EAI          04/02   removed pggrpw_createTypeBox()          *
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
Widget 	 pane, pulldown;
XmString xmstr;
int      grptyp, iret;
long	ii;
char     *names = NULL;
char     grpnam[64];
/*---------------------------------------------------------------------*/

    /*
     * Create the VG group selection box.
     */
    _group_dlgW = XmCreateFormDialog ( parent, "group_selection",
				       NULL, 0 );
    xmstr = XmStringCreateLocalized("VG Group Selection");
    XtVaSetValues(_group_dlgW,
	XmNnoResize,     True,
        XmNautoUnmanage, FALSE,
        XmNdialogTitle,  xmstr, 
        NULL);
    XmStringFree(xmstr);

    /*
     * create a parent pane widget
     */
    pane = (Widget)XtVaCreateManagedWidget("grp_pane",
	   xmPanedWindowWidgetClass, _group_dlgW,
           XmNsashWidth,             1,
           XmNsashHeight,            1,
           NULL);

    /*
     * Build group type option menu.
     */
    ces_gtggrps(&_numGrp, &names, &iret);   
    if ( names != NULL )  free (names);

    pulldown = XmCreatePulldownMenu(pane, "menuW", NULL, 0);
    _group_typeW = XmCreateOptionMenu(pane, "option_menu", NULL, 0 ); 
    _group_buttonW = (WidgetList)XtMalloc(_numGrp * sizeof(Widget));

    for( ii = 0; ii < _numGrp; ii++ ) { 

        /*
         * Get group name from index.
         */
        grptyp = ces_gtgmsid ( ii );
        ces_gtgnam (grptyp, grpnam, &iret);

        _group_buttonW[ii] = XtVaCreateManagedWidget(grpnam,
             xmPushButtonWidgetClass,		pulldown,
             NULL);

	XtAddCallback (_group_buttonW[ii], XmNactivateCallback, 
		       (XtCallbackProc)pggrpw_grpActionCb, (XtPointer)ii);

    }

    xmstr = XmStringCreateLocalized("Group Type");
    XtVaSetValues(_group_typeW,
	XmNsubMenuId, 		pulldown,
	XmNlabelString, 	xmstr, 
	NULL );
    XmStringFree(xmstr);

    XtManageChild(_group_typeW);
 
    return(_group_dlgW);    

}

/*=====================================================================*/

void pggrpw_popup ( void )
/************************************************************************
 * pggrpw_popup								*
 *									*
 * This function shows a VG group selection box.			*
 *									*
 * void pggrpw_popup ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	 6/97						*
 * C. Lin/EAI	        10/97	rename from NxmGroupSh, cleanup		*
 * C. Lin/EAI	        04/98	pop up windown only			*
 * C. Lin/EAI	        09/98	modify for new GROUP logic		*
 * E. Safford/GSC	06/99	remove reset on _selectTyp & set menu	*
 * H. Zeng/EAI          03/01   rewrote for new group type table        *
 * H. Zeng/EAI          10/01   revised for new GROUP functionality     *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    pggrpw_setGrpType ();
    XtManageChild( _group_dlgW );
}

/*=====================================================================*/

void pggrpw_popdown ( void )
/************************************************************************
 * pggrpw_popdown                                                       *
 *                                                                      *
 * This function unmanages the groups dialog box                        *
 *                                                                      *
 * void pggrpw_popdown ()                                               *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97                                           *
 * C. Lin/EAI	        10/97	rename from NxmGroupSh, cleanup header	*
 ***********************************************************************/
{
    if ( XtIsManaged(_group_dlgW) )
    	XtUnmanageChild(_group_dlgW);
}

/*=====================================================================*/

Boolean pggrpw_isUp ( void )
/************************************************************************
 * pggrpw_isUp                                                       	*
 *                                                                      *
 * This function returns a boolean value specifying whether the group  	*
 * dialog is managed or not.                                            *
 *                                                                      *
 * Boolean pggrpw_isUp ( )                                              *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * pggrpw_isUp	Boolean		True (up), 	False (down)		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97                                           *
 * C. Lin/EAI	        10/97	rename from NxmGroupManaged, cleanup hdr*
 ***********************************************************************/
{
    return ( XtIsManaged(_group_dlgW) );
}

/*=====================================================================*/

void pggrpw_setGrpType ( void )
/************************************************************************
 * pggrpw_setGrpType							*
 *									*
 * This function sets the group type choice in the group action window. *
 * This function is only called when GROUP process is active.		*
 * 									*
 * void pggrpw_setGrpType()					        *
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		10/01	initial coding				*
 * E. Safford/SAIC	03/02	use pglayer_getDefGrp, clean up		*
 * H. Zeng/EAI          04/02   removed pggrpw_getGrpIndex()            *
 * H. Zeng/XTRIA	03/03   added check to the value of def_grp     *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		03/04	block GFA_ELM				*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block memory			*
 ***********************************************************************/
{
int     	ii, nelm, location, iret;
int		cur_layer, def_grp, grptyp, idx;
char		sel_flag;
VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    /*
     *  If no elements are selected, or elements selected are not in any
     *  groups-- group menu starts with the default group type for this 
     *  layer.  Use 0 for the idx if the def_grp value somehow is bad.
     */ 
    cur_layer = pglayer_getCurLayer();
    def_grp   = pglayer_getDefGrp(cur_layer);

    if ( def_grp == NON_GRPID ) {
         idx = 0;
    }
    else {
         idx = ces_gtgavid (def_grp);
    }

    if ( pghdlb_elemSelected() >= 1 ) {
        /*
         *  If some elements are selected already, then examine them for
         *  group types.  As soon as a group type is found, use this value
         *  as the starting value of the group menu.
         */
        ii = 0;
        nelm = pghdlb_elemSelected();

        while (nelm && (ii < MAX_EDITABLE_ELEMS)) {

            crg_gsel(ii, &sel_flag, &iret);
            if (sel_flag) {

                crg_goffset(ii, &location, &iret);
	        cvg_rdrec (cvg_getworkfile(), location, &el, &iret);

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
		 *  Toss out any non-groupable elements or internal group
		 *  numbers.
		 */
	        if (el.hdr.vg_class != CLASS_WATCHES &&
                    el.hdr.vg_class != CLASS_TRACKS  &&
                    el.hdr.vg_class != CLASS_SIGMETS &&
                    el.hdr.vg_type != GFA_ELM &&
                    el.hdr.grptyp   <  90            &&
                    el.hdr.grptyp   >  0                ) {

                    grptyp = el.hdr.grptyp;
		    idx = ces_gtgavid (grptyp);
		    if (idx >= 0) {
		        break;
                    }  

	        }
                nelm--;

            } /* the end of if (sel_flag... */

	    ii++;

        }   /* the end of while (nelm... */

    }	

    if ( idx < 0 || idx >= _numGrp ) idx = 0;

    _curIndex =  idx;
    _selectTyp = ces_gtgmsid (idx);

    XtVaSetValues (_group_typeW,
		  XmNmenuHistory,    _group_buttonW[idx], 
		  NULL);

}

/*=====================================================================*/

int pggrpw_getGrpType ( void )
/************************************************************************
 * pggrpw_getGrpType							*
 *									*
 * This function queries the current group type choice  	        *
 * 									*
 *									*
 * int pggrpw_getGrpType()					        *
 *									*
 * Input parameters:							*
 *	        	NONE				                *
 *									*
 * Output parameters:							*
 * pggrpw_getGrpType   	int	current group choice			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		04/00						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    return (_selectTyp);

}

/*=====================================================================*/

int pggrpw_getGrpNum ( void )
/************************************************************************
 * pggrpw_getGrpNum							*
 *									*
 * This function queries the active group number.  	                *
 * 									*
 *									*
 * int pggrpw_getGrpNum ()					        *
 *									*
 * Input parameters:							*
 *	        	NONE				                *
 *									*
 * Return parameters:							*
 * pggrpw_getGrpNum   	int	active group number			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		08/01						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    return (_selectNum);

}

/*=====================================================================*/

int pggrpw_grpCount ( void )
/************************************************************************
 * pggrpw_grpCount							*
 *									*
 * Returns the current total number of groups				*
 *									*
 * int pggrpw_grpCount ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pggrpw_grpCount	int		number of groups		*
 *									*
 **									*
 * Log:									*
 *  S. Law/GSC		04/98	Initial coding				*
 *  H. Zeng/EAI         03/01   modified to use ces functions           *
 *  E. Safford/SAIC	03/02	param change for ces_gtggrps()		*
 ***********************************************************************/
{
int    count, number, ngrp, grpid, iret;
char   *names = NULL, *ptr;
/*---------------------------------------------------------------------*/

    count = 0;
    ces_gtggrps(&ngrp, &names, &iret);

    ptr = strtok(names, ";");
    while ( ptr != (char *)NULL ) {
       ces_gtgid(ptr, &grpid, &iret);
       crg_ggnxt ( (char)grpid, &number, &iret );
       count += number;

       ptr = strtok(NULL, ";" );

    }

    if ( names != NULL )  free (names);

    return (count);

}

/*=====================================================================*/

/* ARGSUSED */
void pggrpw_grpActionCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pggrpw_grpActionCb							*
 *									*
 * Callback function for option menu of GROUP window.			*
 *									*
 * void pggrpw_grpActionCb (w, which, call )				*
 *									*
 * Input parameters:							*
 *	w	Widget		option button widget ID			*
 *	which	long		which button				*
 *	data	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          10/01   initial coding                          *
 * E. Safford/SAIC	03/02	rename from pggrpw_option2Cb		*
 * H. Zeng/EAI          04/02   modified to use ces_gtgmsid()           *
 * M. Li/SAIC		05/02   added pg*_updtGrpMenu			*
 ***********************************************************************/
{
int grpid, cur_layer;
/*---------------------------------------------------------------------*/

    if ( (int)which == _curIndex ) {
         return;
    }

    _curIndex = (int)which;
    grpid = ces_gtgmsid (_curIndex);
    _selectTyp = grpid;

    cur_layer = pglayer_getCurLayer();
    pglayer_setDefGrp (cur_layer, grpid);
 
    /*
     * Change group type for all selected elements.
     */
    pggrpw_startGrp();

    /*
     * If the line, front, or symbol attribute window is up, synchronize
     * the two group menus.
     */
    if ( pgline_isUp() ) {
	pgline_updtGrpMenu (_curIndex);
    }
   
    if ( pgfrtw_isUp() ) {
        pgfrtw_updtGrpMenu (_curIndex);
    }
  
    if (pgsymb_isUp() ) {
        pgsymb_updtGrpMenu (_curIndex);
    }

}      

/*=====================================================================*/

/* ARGSUSED */
void pggrpw_startGrp ( void )
/************************************************************************
 * pggrpw_startGrp							*
 *									*
 * Function that generates initial group from selected elements.	*
 *									*
 * void pggrpw_startGrp (void)				                *
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		04/98						*
 * S. Law/GSC		04/98	Add return to multiSelect after popdown	*
 * S. Law/GSC		05/98	replace pgpalw_multiSelect w/ _setupOper*
 * E. Safford/GSC	06/98	return to multiSelect on ok       	*
 * C. Lin/EAI		09/98	modify to prevent grouping combo-symbol *
 * E. Safford/GSC	09/98	add undo to grouping operation		*
 * C. Lin/EAI		09/98	replace "Close" with "Cancel" 		*
 * E. Safford/GSC	09/98	moved isComsym to pgutls      		*
 * G. Krueger/EAI	10/98	Allow GROUP cancel			*
 * E. Safford/GSC	10/98	mod for param change to cvg_rdrec	*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * S. Law/GSC		11/98	Add pghdlb_deselectAll when grouped	*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * H. Zeng/EAI          08/01   revised&renamed for new group func.     *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		03/04	block GFA_ELM				*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block memory			*
 ***********************************************************************/
{
int     ii, nelm, grpnum, location, iret, err_code, ignore;
char	grptyp, sel_flag;
char    message[]="cannot group combo-symbols";
VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    /*
     * First set the group empty flag.
     */
    _grpEmpty = TRUE;

    /*
     * Assign a new group number based on current group type.
     */
    grptyp = (char)_selectTyp;
    crg_ggnxt(grptyp, &grpnum, &iret);
    _selectNum = grpnum;


    if ( pghdlb_elemSelected() < 1 ) {
	 return;
    }

    if ( pgutls_isComsym() ) {
	NxmWarn_show(mcanvw_getDrawingW(), message);
    }
    else {
	ii = 0;
        nelm = pghdlb_elemSelected();

        while (nelm && (ii < MAX_EDITABLE_ELEMS)) {

            crg_gsel(ii, &sel_flag, &iret);

            if (sel_flag) {

               crg_goffset(ii, &location, &iret);
	       cvg_rdrec (cvg_getworkfile(), location, &el, &iret);

	       if (el.hdr.vg_class != CLASS_WATCHES &&
                   el.hdr.vg_class != CLASS_TRACKS  &&
                   el.hdr.vg_class != CLASS_SIGMETS &&
                   el.hdr.vg_type != GFA_ELM &&
                   el.hdr.grptyp   <  90              ) {

		   if ( _grpEmpty ) {
                        _grpEmpty = FALSE;
                   }
                   pggrpw_addtoGrp ( &el, location, &iret );

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
               else if ( el.hdr.vg_type == GFA_ELM ) {
                  err_code = 8;
                  er_wmsg ("pgen", &err_code, NULL, &ignore, 4, 0);
	          NxmErr_update(); 
               }

               nelm--;

               /*
                * Free TCA memory
                */
               if ( el.hdr.vg_type == TCA_ELM ) {
                  cvg_freeBkpts ( &el );
               }
               else if ( el.hdr.vg_type == GFA_ELM ) {
                   cvg_freeElPtr ( &el );
               }

            }   /* the end of if (sel_flag... */

	    ii++;

        }   /* the end of while (nelm... */

	pghdlb_deselectAll();
        pghdlb_selectGrp( (char)pggrpw_getGrpType(),
                                pggrpw_getGrpNum()   ); 

    }  /* the end of else */

}

/*=====================================================================*/

void pggrpw_addtoGrp ( VG_DBStruct *el, int location, int *iret )
/************************************************************************
 * pggrpw_addtoGrp							*
 *									*
 * The function tries to add an element(or the group the selected       *
 * element is in) to a group.		                                *
 *									*
 * void pggrpw_addtoGrp ( el, location, iret )				*
 *									*
 * Input parameters:							*
 *	el	   VG_DBStruct*    point to the element			*
 *	location   int		   file location of the element		*
 *	iret	   int*            return value				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          09/01   initial coding                          *
 * J. Wu/SAIC		12/01	add layer in crg_set() call		*
 * J. Wu/SAIC		01/02	add layer in crg_get() call		*
 * H. Zeng/EAI          04/02   modified to use ces_gtgmsid()           *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC         	07/04   add filter param to crg_get		*
 * B. Yin/SAIC          07/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block memory			*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 ***********************************************************************/
{
int     new_location, new_num, el_grpnum, actv_grpnum, grp_typ, layer, found;
int     el_num, elN, extra = 5, grp_mem, *inxarry, ii, grp_loc, ier, el_layer;
int     update_crg;
char    el_grptyp, actv_grptyp, grpnam[20], grp_nam[64];
float	llx, lly, urx, ury, m_llx, m_lly, m_urx, m_ury;
float   *elX, *elY, inf_bbox[4];
VG_DBStruct    new_el, grp_el, del_el;
filter_t	filter;
/*---------------------------------------------------------------------*/

       *iret = 0;

       update_crg = 0;

       actv_grptyp = (char)_selectTyp;
       actv_grpnum = _selectNum;

       el_grptyp = el->hdr.grptyp;
       el_grpnum = el->hdr.grpnum;

       /*
        * Check if this is the first element to be grouped.
        */
       if ( _grpEmpty ) {

	    if ( el_grptyp > 0 ) {

	         _selectTyp = el_grptyp;
	         crg_ggnxt(el_grptyp, &_selectNum, &ier);

                 ces_gtgnam(_selectTyp, grpnam, &ier);
                 _curIndex = 0;
                 for( ii = 0; ii < _numGrp; ii++ ) {

                    /*
                     * Get group name from index.
                     */
                    grp_typ = ces_gtgmsid ( ii );
                    ces_gtgnam (grp_typ, grp_nam, &ier);

                    if( strcmp(grp_nam, grpnam) == 0) {
                        _curIndex = ii;
                        break;
                    }
                 }  

                 XtVaSetValues (_group_typeW,
		             XmNmenuHistory,	  _group_buttonW[_curIndex], 
		             NULL);

                 actv_grptyp = (char)_selectTyp;
                 actv_grpnum = _selectNum;

	    }
            _grpEmpty = FALSE; 
          
       }

       /*
        * Stop if the element already belongs to the active group.
        */
       if (el_grptyp == actv_grptyp && el_grpnum == actv_grpnum) {
           return;
       }


       m_llx = 999999.0F;
       m_lly = 999999.0F;
       m_urx = 0.0F;
       m_ury = 0.0F;

       grp_mem = 0;
       if (el_grptyp != 0 && el_grpnum != 0) {
           crg_ggnel(el_grptyp, el_grpnum, &grp_mem, &ier);
       }

       layer = pglayer_getCurLayer( );
       if (grp_mem == 0) {

	  /*
           * The selected element does not belong to any group.
           */
          pgundo_newStep();

          /*
           * Create a copy of the element with the active group info.
           */
          pgactv_setActvElm ( el, location);
          pgactv_getDevPts (&elN, &elX, &elY);
          cvg_rdrec(cvg_getworkfile(), location, &new_el, &ier);
          pgvgf_saveNewElm(NULL, sys_D, &new_el, 
                    elN, elX, elY, FALSE, &new_location, &ier);

          /*
           * Free TCA/GFA memory
           */
          if ( new_el.hdr.vg_type == TCA_ELM ) {
             cvg_freeBkpts ( &new_el );
          }
          else if ( new_el.hdr.vg_type == GFA_ELM ) {
              cvg_freeElPtr ( &new_el );
          }

          cvg_rdrec(cvg_getworkfile(), new_location, &new_el, &ier);
          crg_set (&new_el, new_location, layer, &ier);
          crg_getinx (new_location, &new_num, &ier);
          crg_get(new_num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);

          if (m_llx > llx)
              m_llx = llx;
          if (m_lly > lly)
              m_lly = lly;
          if (m_urx < urx)
              m_urx = urx;
          if (m_ury < ury)
              m_ury = ury;

          /*
           * Mark elements in placement that are effected by
           * the new element, and get the area of influence back
           */
   	  cvg_checkplace(&new_el, 0, new_location, &found, inf_bbox, &ier);
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

          cvg_setginf(cvg_getworkfile(), new_location, actv_grptyp, 
                                               actv_grpnum, &ier);  
          pgundo_storeThisLoc(new_location, UNDO_ADD, &ier);

          /*
           * Free TCA/GFA memory
           */
          if ( new_el.hdr.vg_type == TCA_ELM ) {
             cvg_freeBkpts ( &new_el );
          }
          else if ( new_el.hdr.vg_type == GFA_ELM ) {
              cvg_freeElPtr ( &new_el );
          }

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

          /* 
           * Mark the original element as deleted.
           */
          cvg_delet(cvg_getworkfile(), location, TRUE, &ier);
          crg_getinx (location, &el_num, &ier);
          crg_get (el_num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);
	
          if (m_llx > llx)
              m_llx = llx;
          if (m_lly > lly)
              m_lly = lly;
          if (m_urx < urx)
              m_urx = urx;
          if (m_ury < ury)
              m_ury = ury;

          crg_clear(el_num, &ier);
          pgundo_storeThisLoc(location, UNDO_DEL, &ier);
 	  
          pgundo_endStep();

       }  /* if (grp_mem == 0... */
       else {
    
	  /*
           * The selected element belongs to a group. Add all group
           * members.
           */
          pgundo_newStep();

          inxarry = (int *)malloc(grp_mem*sizeof(int));
          crg_gginx(el_grptyp, el_grpnum, grp_mem, inxarry, 
                                         &grp_mem, &ier);

          for ( ii = 0; ii < grp_mem; ii++ ) {

	      crg_goffset(inxarry[ii], &grp_loc, &ier);
              cvg_rdrec(cvg_getworkfile(), grp_loc, &grp_el, &ier);

	      pgactv_setActvElm ( &grp_el, grp_loc);
              pgactv_getDevPts (&elN, &elX, &elY);

              /*
               * Free TCA/GFA memory
               */
              if ( grp_el.hdr.vg_type == TCA_ELM ) {
                 cvg_freeBkpts ( &grp_el );
              }
              else if ( grp_el.hdr.vg_type == GFA_ELM ) {
                 cvg_freeElPtr ( &grp_el );
              }

              cvg_rdrec(cvg_getworkfile(), grp_loc, &new_el, &ier);
              pgvgf_saveNewElm(NULL, sys_D, &new_el, 
                    elN, elX, elY, FALSE, &new_location, &ier);

              /*
               * Free TCA/GFA memory
               */
              if ( new_el.hdr.vg_type == TCA_ELM ) {
                 cvg_freeBkpts ( &new_el );
              }
              else if ( new_el.hdr.vg_type == GFA_ELM ) {
                 cvg_freeElPtr ( &new_el );
              }


              cvg_rdrec(cvg_getworkfile(), new_location, &new_el, &ier);
              crg_set (&new_el, new_location, layer, &ier);
	      crg_getinx (new_location, &new_num, &ier);
	      crg_get(new_num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);

	      if (m_llx > llx)
                  m_llx = llx;
              if (m_lly > lly)
                  m_lly = lly;
              if (m_urx < urx)
                  m_urx = urx;
              if (m_ury < ury)
                  m_ury = ury;

              /*
               * Mark elements in placement that are effected by
               * the new element, and get the area of influence back
               */
	      cvg_checkplace(&new_el, 0, new_location, &found, inf_bbox, &ier);
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

  	      cvg_setginf(cvg_getworkfile(), new_location, actv_grptyp, 
				     actv_grpnum, &ier);  
	      pgundo_storeThisLoc(new_location, 
                                     UNDO_ADD, &ier);

              /*
               * Free TCA/GFA memory
               */
              if ( new_el.hdr.vg_type == TCA_ELM ) {
                 cvg_freeBkpts ( &new_el );
              }
              if ( new_el.hdr.vg_type == GFA_ELM ) {
                 cvg_freeElPtr ( &new_el );
              }

              /*
               * Mark elements in placement that are effected by
               * the delete, and get the area of influence back
               */
              cvg_rdrec(cvg_getworkfile(), grp_loc, &del_el, &ier);
	      cvg_checkplace(&del_el, 1, grp_loc, &found, inf_bbox, &ier);

              /*
               * Free TCA/GFA memory
               */
              if ( del_el.hdr.vg_type == TCA_ELM ) {
                 cvg_freeBkpts ( &del_el );
              }
              if ( del_el.hdr.vg_type == GFA_ELM ) {
                 cvg_freeElPtr ( &del_el );
              }

              /* 
               * Mark the original element as deleted.
               */
              cvg_delet(cvg_getworkfile(), grp_loc, TRUE, &ier);
	      crg_getinx (grp_loc, &el_num, &ier);
	      crg_get (el_num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);
	
	      if (m_llx > llx)
                  m_llx = llx;
              if (m_lly > lly)
                  m_lly = lly;
              if (m_urx < urx)
                  m_urx = urx;
              if (m_ury < ury)
                  m_ury = ury;

              crg_clear(el_num, &ier);
	      pgundo_storeThisLoc(grp_loc, UNDO_DEL, &ier);

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
              if ( del_el.hdr.vg_type == GFA_ELM ) {
                 cvg_freeElPtr ( &del_el );
              }
          
          }   /* for ( ii = 0 ... */
	  
          pgundo_endStep();
          free(inxarry);

       }  /* else ... */
                
       m_llx -= (float)extra;
       m_lly -= (float)extra;
       m_urx += (float)extra;
       m_ury += (float)extra;
    
       xpgpaste (m_llx, m_lly, m_urx, m_ury, &ier);
       cvg_rfrsh (NULL, m_llx, m_lly, m_urx, m_ury, &ier); 

       /*
        * If we may have impacted other elements with placement
        * we will need to rebuild the range records
        */
       if (update_crg) {
           crg_rebuild();
       }

}

/*=====================================================================*/

void pggrpw_rmvfrmGrp ( VG_DBStruct *el, int location, int *iret )
/************************************************************************
 * pggrpw_rmvfrmGrp							*
 *									*
 * The function tries to remove an element from a group.		*
 *									*
 * void pggrpw_rmvfrmGrp ( el, location, iret )				*
 *									*
 * Input parameters:							*
 *	el	   VG_DBStruct*    point to the element			*
 *	location   int		   file location of the element		*
 *	iret	   int*            return value				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          09/01   initial coding                          *
 * J. Wu/SAIC		12/01	add layer in crg_set() call		*
 * J. Wu/SAIC		01/02	add layer in crg_get() call		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC         	07/04   add filter param to crg_get		*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block memory			*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 ***********************************************************************/
{
int     new_location, new_num, layer, el_layer;
int     el_num, elN, extra = 5, ier, found, update_crg;
float	llx, lly, urx, ury, m_llx, m_lly, m_urx, m_ury;
float   *elX, *elY, inf_bbox[4];
VG_DBStruct    new_el;
filter_t	filter;
/*---------------------------------------------------------------------*/

       *iret = 0;

       m_llx = 999999.0F;
       m_lly = 999999.0F;
       m_urx = 0.0F;
       m_ury = 0.0F;

       update_crg = 0;

       pgundo_newStep();

       /*
        * Create a copy of the element with no group info.
        */
       pgactv_setActvElm ( el, location);
       pgactv_getDevPts (&elN, &elX, &elY);
       cvg_rdrec(cvg_getworkfile(), location, &new_el, &ier);
       pgvgf_saveNewElm(NULL, sys_D, &new_el, 
                 elN, elX, elY, FALSE, &new_location, &ier);

       /*
        * Free TCA break point memory
        */
       if ( new_el.hdr.vg_type == TCA_ELM ) {
          cvg_freeBkpts ( &new_el );
       }
       if ( new_el.hdr.vg_type == GFA_ELM ) {
          cvg_freeElPtr ( &new_el );
       }

       cvg_rdrec(cvg_getworkfile(), new_location, &new_el, &ier);
       layer = pglayer_getCurLayer( );
       crg_set (&new_el, new_location, layer, &ier);
       crg_getinx (new_location, &new_num, &ier);
       crg_get(new_num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);

       if (m_llx > llx)
           m_llx = llx;
       if (m_lly > lly)
           m_lly = lly;
       if (m_urx < urx)
           m_urx = urx;
       if (m_ury < ury)
           m_ury = ury;

       cvg_setginf(cvg_getworkfile(), new_location, 0, 0, &ier);  
       pgundo_storeThisLoc(new_location, UNDO_ADD, &ier);

       /*
        * Mark elements in placement that are effected by
        * the new element, and get the area of influence back
        */
       cvg_checkplace(&new_el, 0, new_location, &found, inf_bbox, &ier);
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
        * Free TCA break point/GFA block memory
        */
       if ( new_el.hdr.vg_type == TCA_ELM ) {
          cvg_freeBkpts ( &new_el );
       }
       if ( new_el.hdr.vg_type == GFA_ELM ) {
           cvg_freeElPtr ( &new_el );
       }

       /*
        * Mark elements in placement that are effected by
        * the delete, and get the area of influence back
        */
       cvg_rdrec(cvg_getworkfile(), location, &new_el, &ier);
       cvg_checkplace(&new_el, 1, location, &found, inf_bbox, &ier);
       /*
        * Free TCA break point/GFA block memory
        */
       if ( new_el.hdr.vg_type == TCA_ELM ) {
          cvg_freeBkpts ( &new_el );
       }
       if ( new_el.hdr.vg_type == GFA_ELM ) {
           cvg_freeElPtr ( &new_el );
       }

       /* 
        * Mark the original element as deleted.
        */
       cvg_delet(cvg_getworkfile(), location, TRUE, &ier);
       crg_getinx (location, &el_num, &ier);
       crg_get (el_num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);
	
       if (m_llx > llx)
           m_llx = llx;
       if (m_lly > lly)
           m_lly = lly;
       if (m_urx < urx)
           m_urx = urx;
       if (m_ury < ury)
           m_ury = ury;

       crg_clear(el_num, &ier);
       pgundo_storeThisLoc(location, UNDO_DEL, &ier);

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
 	  
       pgundo_endStep();

       m_llx -= (float)extra;
       m_lly -= (float)extra;
       m_urx += (float)extra;
       m_ury += (float)extra;
    
       xpgpaste (m_llx, m_lly, m_urx, m_ury, &ier);
       cvg_rfrsh (NULL, m_llx, m_lly, m_urx, m_ury, &ier); 
      /*
       * If we may have impacted other elements with placement
       * we will need to rebuild the range records
       */
      if (update_crg) {
          crg_rebuild();
      }

}

/*=====================================================================*/

Boolean pggrpw_getEmptFlg ( void )
/************************************************************************
 * pggrpw_getEmptFlg							*
 *									*
 * This function queries the current group empty flag _grpEmpty. 	*
 * 									*
 * Boolean pggrpw_getEmptFlg()					        *
 *									*
 * Input parameters:							*
 *	        	NONE				                *
 *									*
 * Return parameters:							*
 * pggrpw_getEmptFlg   	Boolean    value of _grpEmpty			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		10/01						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    return (_grpEmpty);

}

/*=====================================================================*/

void pggrpw_setEmptFlg ( Boolean val )
/************************************************************************
 * pggrpw_setEmptFlg							*
 *									*
 * This function sets _grpEmpty based on the passed in value.	        *
 *									*
 * void pggrpw_setEmptFlg ( val )					*
 *									*
 * Input parameters:							*
 *	 val	        Boolean    TRUE or FALSE	   		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *                      NONE						*
 *									*
 **									*
 * Log:									*
 *  H. Zeng/EAI         10/01      initial coding                       *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _grpEmpty = val;

}

/*=====================================================================*/
