#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "hints.h"
#include "proto_xw.h"

#define FindNearEnd(a, b, c)	(((b - a) < (c - b)) ? a : c)

#define TOP			( 0 )
#define BOTTOM			( 1 )

static VG_DBStruct	_primaryEl;
static int              _primaryLoc;
static Boolean		_selectFlag;
static Boolean		_isGFA;
static Boolean		_isOpenFzlvl;
static Boolean		_drawGhost;  /* True for non-GFAs and open FZLVLs
					   False for closed-GFAs */


static float		_ghostX[4];
static float		_ghostY[4];
static int		_ghostN;


/*
 *  Private Callback Functions
 */
static void pgconn_ghost  ( Widget, XtPointer, XEvent*, Boolean* );
static void pgconn_select ( Widget, XtPointer, XEvent*, Boolean* );
static Boolean pgconn_verifyType ( VG_DBStruct *first_el, 
				   VG_DBStruct *second_el );

static void pgconn_smear ( int		loc1,  
			  int 		loc2, 
			  VG_DBStruct  *elout,
			  int		*out, 
			  float 	*outX, 
			  float 	*outY );

/************************************************************************
 * nmap_pgconn.c							*
 *									*
 * This module contains the routines for connecting lines together	*
 *									*
 * CONTENTS:								*
 * pgconn_start		Initialize and original selection		*
 * pgconn_select	Press callback					*
 * pgconn_ghost		Drag callback					*
 * pgconn_verifyType	Confirm the two elements are same type/subtype  *
 ***********************************************************************/

/*=====================================================================*/

void pgconn_start ( VG_DBStruct *el, float currx, float curry )
/************************************************************************
 * pgconn_start								*
 *									*
 * Initial setup and original selection					*
 *									*
 * void pgconn_start (el, currx, curry)					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	selected element		*
 *	currx		float		current x coordinate		*
 *	curry		float		current y coordinate		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 *  S. Law/GSC		09/98	Initial coding				*
 *  G. Krueger/EAI	09/98	Added ghost veiling			*
 *  G. Krueger/EAI	10/98	Using table for mouse hints		*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 *  H. Zeng/EAI         11/00   added _primaryLoc                       *
 *  J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 *  J. Wu/SAIC		06/07	connect GFAs with same hazard type &	*
 *				forecast hour				*
 ***********************************************************************/
{
    float	*dcx, *dcy;
    int		dcn, near, ii, jj, ier;
    char	hazard[32];
/*---------------------------------------------------------------------*/
    
    _primaryEl  = *el;
    _primaryLoc = pgactv_getElmLoc();
    
/*
 *  Check the type of first selected element - GFAs (except open
 *  FZLVL) are handle differently than other types of VG elements.
 */
    _isGFA = (_primaryEl.hdr.vg_type == GFA_ELM) ? True:False;

    _isOpenFzlvl = False;	           
    if ( _isGFA ) {
           
       _primaryEl.elem.gfa.info.nblocks = el->elem.gfa.info.nblocks;

       G_MALLOC ( _primaryEl.elem.gfa.info.blockPtr[ 0 ],
	          gfaBlock_t, el->elem.gfa.info.nblocks, 
	          "pgconn_start: _primaryEl.blockPtr" );

       memcpy ( _primaryEl.elem.gfa.info.blockPtr[ 0 ], 
	        el->elem.gfa.info.blockPtr[ 0 ], 
	        el->elem.gfa.info.nblocks * STD_STRLEN * sizeof ( char ) );      
       
       cvg_getFld ( el, TAG_GFA_AREATYPE, hazard, &ier );
       
       if ( strcasecmp( hazard, "FZLVL" ) == 0 ) {
           if ( _primaryEl.hdr.closed == 0 ) {
               _isOpenFzlvl = True;	           	   
	   }
       }
       
    }
    
    _drawGhost = True;
    if ( _isGFA && !_isOpenFzlvl ) {
        _drawGhost = False;    
    }

/*
 *  Start another mouse click.
 */
    pggst_veilGhost ( FALSE );    
    mcanvw_setPressFunc((XtEventHandler)&pgconn_select, CURS_DEFAULT);

/*
 *  No drag function for closed GFAs - except open FZLVLs.
 */
    if ( _drawGhost ) {
        mcanvw_setDragFunc((XtEventHandler)&pgconn_ghost, CURS_DEFAULT);
    }
          
    pgactv_getDevPts (&dcn, &dcx, &dcy);
    
/*
 *  Ghost only for non-GFAs and open FZLVLs.
 */
    if ( _drawGhost ) {
        near = pgactv_getNearPt ();
        _ghostN = (dcn < 3) ? dcn : 3;
        if (FindNearEnd (0, near, (dcn - 1))) {	/* ghost last 2 or 3 points */
	    jj = 0;
	    for (ii = (dcn - _ghostN); ii < dcn; ii++) {
	        _ghostX[jj] = *(dcx + ii);
	        _ghostY[jj] = *(dcy + ii);
	        jj++;
	    }
        }
        else {					/* ghost first 2 or 3 points */
	    jj = 0;
	    for (ii = (_ghostN - 1); ii > -1; ii--) {
	        _ghostX[jj] = *(dcx + ii);
	        _ghostY[jj] = *(dcy + ii);
	        jj++;
	    }
        }

        _ghostX[jj] = currx;
        _ghostY[jj] = curry;
    
        pggst_clearGhost (TRUE);
        pggst_setLineAttr (_primaryEl.hdr.smooth, 
		       (Boolean) _primaryEl.hdr.closed);
        pggst_addGhostPts ( (_ghostN+1), _ghostX, _ghostY, &ier );

        pggst_drawGhost (GST_NORMAL);
    }
    
    mbotw_mouseSet(LMHINT_NEXT, MMHINT_DONE);
}

/*=====================================================================*/
/* ARGSUSED */
static void pgconn_select ( Widget wid, XtPointer clnt, XEvent *event,
						Boolean *ctdr )
/************************************************************************
 * pgconn_select							*
 *									*
 * This function handles the selection callbacks			*
 *									*
 * static void pgconn_select (wid, clnt, event, ctdr )			*
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
 *  S. Law/GSC		09/98	Initial coding				*
 *  S. Law/GSC		09/98	Removed unnecessary refreshes		*
 *  E. Safford/GSC	09/98	fix ghost problem conn to 2 pt lines    *
 *  G. Krueger/EAI	10/98	Using table for mouse hints		*
 *  S. Law/GSC		09/98	added CLASS_SIGMETS			*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  E. Safford/GSC	10/99	update for new xwcmn.h			*
 *  S. Law/GSC		02/00	added CCF				*
 *  S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 *  H. Zeng/EAI         11/00   modified for the new undo design        *
 *  A. Hardy/GSC        11/00   renamed coordinate system declaration   *
 *  H. Zeng/EAI         12/00   modified for multiple undo steps        *
 *  J. Wu/GSC		03/01	added "EXIT" hint to bottom panel       *
 *  J. Wu/SAIC		01/02	scan the current PGEN layer only	*
 *  E. Safford/SAIC	03/02	rename from _pgconn_select, add         *
 *				  pgutls_regroup(), clean up		*
 *  E. Safford/SAIC	04/02	check ier from cvg_scan to avoid UMR	*
 *  T. Lee/SAIC         11/03   added user directory to work_file       *
 *  T. Lee/SAIC		11/03	used cvg_getworkfile			*
 *  S. Danz/AWC         08/06   New flag to pgvgf_saveNewElm to place el*
 *  J. Wu/SAIC		06/07	connect GFAs      			*
 ***********************************************************************/
{ 
    float		xx, yy, llx, lly, urx, ury;
    float		*primary_x, *primary_y;
    float		secondary_x[MAXPTS], secondary_y[MAXPTS];
    int			primary_n, primary_loc, secondary_n;
    int			ier, near, test, xoff, yoff, cur_layer;
    int			start, grp_num1, grp_num2;
    char		grp_typ1, grp_typ2, warnMsg[256], value[10];
    char		mesg[] = {"Maximum vertices exceeded."};
    Boolean		primary_grouped = FALSE;
    Boolean		secondary_grouped = FALSE;

    static int		secondary_elnum;
    static int          secondary_loc;
    static Boolean	reverse_flag = FALSE;
    static VG_DBStruct	secondary_el;
    static VG_DBStruct	GFA_el;
    
    int			GFA_pts, GFA_newloc;
    float		GFA_x[MAXPTS], GFA_y[MAXPTS];
/*---------------------------------------------------------------------*/

    xgtoff (&xoff, &yoff, &ier);
    cur_layer = pglayer_getCurLayer ();

    if (event->xbutton.button == Button1) {

        if (_selectFlag) {     /* confirming secondary element */
	    	    	    
	    grp_typ1 = _primaryEl.hdr.grptyp;
	    grp_num1 = _primaryEl.hdr.grpnum;

	    if ( grp_typ1 > 0 && grp_num1 > 0 ) {
		primary_grouped = TRUE;
	    }

	    cvg_todev (&secondary_el, &secondary_n, secondary_x, 
		       secondary_y, &ier);

	    grp_typ2 = secondary_el.hdr.grptyp;
	    grp_num2 = secondary_el.hdr.grpnum;

	    if ( grp_typ2 > 0 && grp_num2 > 0 ) {
		secondary_grouped = TRUE;
	    }
	    	             
	    if ( _drawGhost ) {
	        near = pgactv_getNearPt ();
	        pgactv_getDevPts (&primary_n, &primary_x, &primary_y);

	        start = (FindNearEnd (0, near, (primary_n - 1)) == 0) ? 
		         0 : primary_n;


	        if (reverse_flag) {
		    pgutls_fReverseArray (secondary_n, secondary_x,
				      secondary_y);
	        }

	        pgactv_addPts (secondary_x, secondary_y, secondary_n, 
			       start, &ier);
            }
	    
	    if (ier == 0) {
		
		if ( _drawGhost ) {
		    pgactv_getDevPts (&primary_n, &primary_x, &primary_y);
                }		

/*
 *  Grouping the new element:
 *
 *  If the primary element was grouped, the new element stays in 
 *  that group. If the primary element was not grouped, but the 
 *  secondary element was, then put the new element in the 
 *  secondary element's group. 
 */
	        if ( !primary_grouped && secondary_grouped ) {
		    _primaryEl.hdr.grptyp = grp_typ2;
		    _primaryEl.hdr.grpnum = grp_num2;
		}
		
/*
 *  Save the new element:
 *  
 *  For non-GFAs or open FZLVLs, use points of both the primary
 *  and secondary elements.
 *  
 *  For closed GFAs,  use points resulting from smearing the primary
 *  and secondary elements.  
 *
 *  Note: The new GFA element will use the the primary element's 
 *        forecast hour, color, and line attributes. 
 */		
		if ( _drawGhost ) {
		    
		    pgundo_newStep();
		    
		    pgundo_storeThisLoc ( _primaryLoc, UNDO_DEL, &ier );
		    pgundo_storeThisLoc ( secondary_loc, UNDO_DEL, &ier );

		    pgutls_prepNew ( -1, &_primaryEl,  &llx, &lly, &urx, &ury,
				     &ier);
		    
		    pgvgf_saveNewElm ( cvg_getworkfile(), sys_D, &_primaryEl, 
				 primary_n, primary_x, primary_y, 
				 TRUE, &primary_loc, &ier );
		    
		    pgutls_redraw ( primary_loc, &_primaryEl, &ier );

		    pgundo_storeThisLoc ( primary_loc, UNDO_ADD, &ier);
                    
		}
		else {		
		    		    		    		    		    
		    pgvgf_saveNewElm ( cvg_getworkfile(), sys_M, &GFA_el, 
		                      0, GFA_x, GFA_y, 
		                      TRUE, &GFA_newloc, &ier );
		    		    		     		    
		    if ( ier >= 0 ) {
		        pgundo_newStep();
		    
		        pgundo_storeThisLoc ( _primaryLoc, UNDO_DEL, &ier );
		        pgundo_storeThisLoc ( secondary_loc, UNDO_DEL, &ier );
		        
		        pgactv_setActvElm ( &_primaryEl,  _primaryLoc );
		        pgutls_prepNew ( _primaryLoc, &_primaryEl,  
			                 &llx, &lly, &urx, &ury, &ier);
			
			pgutls_redraw ( GFA_newloc, &GFA_el, &ier );
		        
			pgundo_storeThisLoc ( GFA_newloc, UNDO_ADD, &ier );
		    }
		    else if ( ier == -28 ) { /* too few points in resulting smear */
	                cvg_getFld ( &GFA_el, TAG_GFA_TAG, value, &ier );
	                sprintf( warnMsg, "Unable to connect tag %s.\n It has too few points after smearing and/or snapping.\n", value );
	                NxmWarn_show( mcanvw_getDrawingW(), warnMsg );
		    }
		    
		    cvg_freeElPtr ( &GFA_el );

		}
		
/* 
 * set the secondary element as the active
 * one, so that prepNew can remove it
 */		
		pgactv_setActvElm (&secondary_el, secondary_loc);
		pgutls_prepNew (secondary_loc, &secondary_el, 
				&llx, &lly, &urx, &ury, &ier);
	        		
/*
 *  If both elements were grouped, then move all elements
 *  grouped with the secondary element into the primary
 *  element's group.
 */
		if ( primary_grouped && secondary_grouped ) {
  		    pgutls_regroup ( grp_typ2, grp_num2, 
		    		     grp_typ1, grp_num1, &ier ); 
		}

                pgundo_endStep();
                
/*
 *  Clean up for next connection.
 */
                if ( _isGFA ) {
		    cvg_freeElPtr ( &secondary_el );		
		    cvg_freeElPtr ( &_primaryEl );		
		}
		
		pghdlb_deselectAll ();
                pgactv_clearActv ();
	        pggst_clearGhost ( FALSE );
		
	    }
	    else {
		NxmWarn_show (wid, mesg);
	    }

	    mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
	    if ( _drawGhost ) mcanvw_disarmDrag ();

	    _selectFlag = FALSE;
	    mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
	}
	else {	/* selecting secondary element */
 
 	    xx = (float) (event->xbutton.x + xoff); 
	    yy = (float) (event->xbutton.y + yoff);

	    test = False;
	    cvg_scan (NULL, cur_layer, (char)_primaryEl.hdr.vg_class, xx, yy,
		      0, &secondary_el, &secondary_loc, &near, &ier);

/*
 *  Verify elements only if no error returned from cvg_scan.
 */	
	    if ( ier >= 0 ) {
	        test = pgconn_verifyType ( &_primaryEl, &secondary_el); 
	    }

/* 
 *  Verify there are actually 2 elements, not the same element.
 */
	    if (test) {
		if (secondary_loc == pgactv_getElmLoc () ||
		    secondary_loc == 0) {
		    test = FALSE;
		}
	    }
	    

	    if ( test ) {
				
		crg_getinx ( secondary_loc, &secondary_elnum, &ier );
 	        
/*
 *  Get the points for the potential connect/join and use it
 *  to draw the ghost line so the user can have a feel of the
 *  result before making the confirmation.
 *  
 *  For non-GFAs or open FZLVLs, connect points of both the primary
 *  and secondary elements.
 *  
 *  For closed GFAs,  get points by smearing the primary and
 *  secondary elements. 
 *
 */
		if ( !_drawGhost ) {	        
		    
/*
 *  Select the second GFA.
 */
		    pghdlb_select ( &secondary_el, secondary_loc );
                                        
/*
 *  Smear two GFA polygons.
 */		    
	            GFA_pts = 0;
		    GFA_el.elem.gfa.info.nblocks = 0; 
		    pgconn_smear ( _primaryLoc, secondary_loc,		  
		  		   &GFA_el, &GFA_pts, GFA_x, GFA_y );
   		    
/*
 *  Ghost the resulting polygon.
 */
		    pggst_clearGhost (TRUE);
		    		    
                    pggst_setLineAttr ( _primaryEl.hdr.smooth, 
		                        (Boolean)_primaryEl.hdr.closed );
                    
		    pggst_addGhostPts ( GFA_pts, GFA_x, GFA_y, &ier );
		    
		    
		    pggst_drawGhost ( GST_NORMAL );
		                        
		}
		else { 
		
		    pggst_drawGhost (GST_NORMAL);

		    mcanvw_disarmDrag ();
						
		    cvg_todev (&secondary_el, &secondary_n, secondary_x, 
			       secondary_y, &ier);

		    if (FindNearEnd (0, near, (secondary_n - 1))) {
		        reverse_flag = TRUE;
		        pgutls_fReverseArray (secondary_n, secondary_x,
					      secondary_y);
		    }
		    else {
		        reverse_flag = FALSE;
		    }

		    pggst_replaceGhostPts (1, &secondary_x[0], 
				           &secondary_y[0], &ier);

		    pggst_addGhostPts ((secondary_n - 1), &secondary_x[1],
				        &secondary_y[1], &ier);

		    pggst_drawGhost (GST_NORMAL);

	        }
		   		
		_selectFlag = TRUE;
		mbotw_mouseSet(LMHINT_CONFIRM, MMHINT_CANCEL);
	    }
	}
    }
    else {
        if (_selectFlag) {     /* unselecting secondary element */
	    pghdlb_deselectEl ( secondary_elnum, TRUE );
	    pghdlb_displayAllSel ();

	    if ( _drawGhost ) {
	        xx = (float) (event->xbutton.x + xoff); 
	        yy = (float) (event->xbutton.y + yoff); 

	        _ghostX[_ghostN] = xx;
	        _ghostY[_ghostN] = yy;
  
	        pggst_clearGhost (FALSE);

	        pggst_addGhostPts ((_ghostN + 1), _ghostX, _ghostY, &ier);

	        pggst_drawGhost (GST_NORMAL);

	    }
	     
	    _selectFlag = FALSE;
	    mbotw_mouseSet(LMHINT_NEXT, MMHINT_DONE);
  
	    if ( _drawGhost ) {
	        mcanvw_setDragFunc((XtEventHandler)&pgconn_ghost, CURS_DEFAULT);
	    }
             
	    if ( _isGFA ) {
		cvg_freeElPtr ( &secondary_el );		
		if ( !_drawGhost ) cvg_freeElPtr ( &GFA_el );		
	    }

	}
	else {	/* unselecting primary element */
	    
	    if ( _isGFA ) {
		cvg_freeElPtr ( &_primaryEl );		
	    }
	    
	    mcanvw_disarmDynamic ();
	    _selectFlag = FALSE;

	    pghdlb_deselectAll();

	    mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);

	    _selectFlag = FALSE;
	    mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);	    
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgconn_ghost ( Widget wid, XtPointer clnt, XEvent *event,
				Boolean *ctdr )
/************************************************************************
 * pgconn_ghost								*
 *									*
 * This function handles the selection callbacks			*
 *									*
 * static void pgconn_ghost (wid, clnt, event, ctdr)			*
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
 *  S. Law/GSC		09/98	Initial coding				*
 *  E. Safford/GSC	10/99	update for new xwcmn.h			*
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 *  E. Safford/SAIC	03/02	rename from _pgconn_ghost, mk static	*
 ***********************************************************************/
{
    float	xx, yy;
    int		ier, xoff, yoff;
/*---------------------------------------------------------------------*/

    xgtoff (&xoff, &yoff, &ier);
    xx = (float) (event->xbutton.x + xoff); 
    yy = (float) (event->xbutton.y + yoff);

    pggst_drawGhost (GST_NORMAL);
    pggst_replaceGhostPts (1, &xx, &yy, &ier);
    pggst_drawGhost (GST_NORMAL);

}
/*=====================================================================*/

static Boolean pgconn_verifyType ( VG_DBStruct *first_el, 
				   VG_DBStruct *second_el )
/************************************************************************
 * pgconn_verifyType							*
 *									*
 * This function examines the two elements to see if they may be 	*
 * connected.  True is returned if they are the same type/subtype.	* 
 *									*
 * static Boolean pgconn_verifyType ( first_el, second_el )		*
 *									*
 * Input parameters:							*
 *	*first_el	VG_DBStruct	first element to be examined	*
 *	*second_el	VG_DBStruct	second element to be examined	*
 * Output parameters:							*
 *			NONE						*
 * Return:								*
 *			Boolean		True if the two elements are of *
 *					 the same type/subtype		*
 **									*
 * Log:									*
 *  E. Safford/GSC	03/02	moved out of pgconn_select()		*
 *  J. Wu/SAIC		10/03	connect jets				*
 *  J. Wu/SAIC		05/07	connect GFAs with same hazard type and	*
 *				forecast hour				*
 ***********************************************************************/
{
    int		ier;
    char	hazard1[32], fcsthr1[32], hazard2[32], fcsthr2[32];

    Boolean	test = False;
/*---------------------------------------------------------------------*/
/*
 *  Test to insure that elements are the same type and subtype 
 *  where appropriate.
 */
    if (first_el->hdr.vg_class == second_el->hdr.vg_class) {

	if ( first_el->hdr.vg_class == CLASS_FRONTS) {
	    if ((first_el->elem.frt.info.fcode / 100) ==
	        	(second_el->elem.frt.info.fcode / 100)) {
	        test = TRUE;
	    }
	}
	else if (first_el->hdr.vg_type == SPLN_ELM &&
		 second_el->hdr.vg_type == SPLN_ELM) {
	    if (first_el->elem.spl.info.spltyp == 
			second_el->elem.spl.info.spltyp) {
		test = TRUE;
	    }
	}
	else if (first_el->hdr.vg_type == LINE_ELM &&
		 second_el->hdr.vg_type == LINE_ELM) {
	    if (first_el->elem.lin.info.lintyp == 
			second_el->elem.lin.info.lintyp) {
		test = TRUE;
	    }
	}
	else if (first_el->hdr.vg_type == JET_ELM &&
		 second_el->hdr.vg_type == JET_ELM) {
	    if (first_el->elem.jet.line.spl.info.spltyp == 
			second_el->elem.jet.line.spl.info.spltyp) {
		test = TRUE;
	    }
	}
	else if (first_el->hdr.vg_type == GFA_ELM &&
		 second_el->hdr.vg_type == GFA_ELM) {
            
/*
 *  To be connected, two GFAs must have same hazard type, 
 *  forecast hour,  and both closed or both open.
 */
	    cvg_getFld ( first_el, TAG_GFA_AREATYPE, hazard1, &ier );
            cvg_getFld ( second_el, TAG_GFA_AREATYPE, hazard2, &ier );
            
	    if ( strcasecmp( hazard1, hazard2 ) == 0 &&
	         first_el->hdr.closed == second_el->hdr.closed ) {
                
		cvg_getFld ( first_el, TAG_GFA_FCSTHR, fcsthr1, &ier );
                cvg_getFld ( second_el, TAG_GFA_FCSTHR, fcsthr2, &ier );
                
		if ( strcasecmp( fcsthr1, fcsthr2 ) == 0 ) {	            
		    test = TRUE;
		}
	    }
	}
	
	if (first_el->hdr.vg_class == CLASS_SIGMETS &&
	    first_el->hdr.vg_type == second_el->hdr.vg_type) {

	    if (first_el->hdr.vg_type == SIGCCF_ELM &&
		first_el->elem.ccf.info.subtype == SIGTYP_LINE &&
		second_el->elem.ccf.info.subtype == SIGTYP_LINE) {
		test = TRUE;
	    }
	    else if (first_el->hdr.vg_type != SIGCCF_ELM &&
		     first_el->elem.sig.info.subtype == SIGTYP_LINE &&
		     second_el->elem.sig.info.subtype == 
		     SIGTYP_LINE) {
		test = TRUE;
	    }
	}	
    }
    return (test);
}

/*=====================================================================*/

static void pgconn_smear ( int loc1,  int loc2, VG_DBStruct *elOut,
			  int *nout, float *outX, float *outY )
/************************************************************************
 * pgconn_smear	                                          		*
 *                                                                      *
 * This function smears two selected GFA elements.  If the two GFAs	*
 * intersect, they are combined using "union";  otherwise, they are	*
 * combined using "shrink-wrap" algorithm.				*
 *									*
 * static void pgconn_smear ( loc1, loc2, elOut, nout, outX, outY )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	loc1		int		Location of first GFA element	*
 *	loc2		int		Location of second GFA element	*
 *									*
 * Output parameters:                                             	*
 *      *elOut		VG_DBStruct	GFA element			*
 *	*nout		int		number of smear points		*
 *	outX[]		float		x of smear points in sys_D	*
 *	outY[]		float		y of smear points in sys_D	*
 *									*
 * Return parameters:                                             	*
 *    			None			       		 	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		06/07	initial coding				*
 * J. Wu/SAIC		09/07	add "unionOnly" into pgsmear_smear	*
 ***********************************************************************/
{
    int		ii, jj, npts, ier, GFA_loc[2], ntmp;
    char	value[10];
    float	*xtmp, *ytmp, *lat, *lon;
    Boolean	repeat;
/*---------------------------------------------------------------------*/
/*
 *  Set computational projection
 */
    ncw_set ( );
    ncw_sproj ( "PREFS" );
    
/*
 *  Smear
 *  Note: pgsmear_smear() needs following options for "join" GFAs:
 *        "skipFzlvl"	- False: closed FZLVL could be joined.
 *        "useAllShapShots"	- True:  snapshots' status doesn't matter.
 *        "reducePts"	- False: no point reduction performed.
 */
    GFA_loc[ 0 ] = loc1;	    
    GFA_loc[ 1 ] = loc2;	    
    pgsmear_smear ( 2, GFA_loc, False, _primaryEl.hdr.maj_col, 0, 
		    NULL, False, False, True, False, True,		  
		    elOut, nout, outX, outY );
    
/*
 *  Remove repeated points
 */
    ntmp = *nout;
    G_MALLOC ( xtmp, float, ntmp, "pgsmear_doSnap xtmp" );
    G_MALLOC ( ytmp, float, ntmp, "pgsmear_doSnap ytmp" );
    G_MALLOC ( lat, float, ntmp, "pgconn_select: lat" );
    G_MALLOC ( lon, float, ntmp, "pgconn_select: lon" );
    
    npts = 0;
    for ( ii = 0; ii < *nout; ii++ ) {

	repeat = False;

	for ( jj = ii + 1; jj < ntmp; jj++ ) {

	    if ( ( fabs ( outX[ ii ] - outX[ jj ] ) < .0001 ) &&

	         ( fabs ( outY[ ii ] - outY[ jj ] ) < .0001 ) ) {

	       repeat = True;
	       break;

	    }
	}

	if ( !repeat ) {

	   xtmp[ npts ] = outX[ ii ];
	   ytmp[ npts ] = outY[ ii ];
	   npts++;
	}
    }
    
/*
 *  Convert device (x,y) to (lat,lon) and put into el. 
 */
    gtrans ( sys_D, sys_M, &npts, xtmp, ytmp, lat, lon, &ier,
    	     strlen(sys_D), strlen(sys_M) );

    if ( npts > 0 ) {

       elOut->elem.gfa.info.npts = npts;

       for ( ii = 0; ii < npts; ii++ ) {

           elOut->elem.gfa.latlon[ ii ] 	 = lat[ ii ];
           elOut->elem.gfa.latlon[ ii + npts ]   = lon[ ii ];

       }
    }
   
/*
 *  Unset computational projection and back to current projection
 */
    ncw_unset ( );
    
/*
 *  Snap if the first polygon is an airmet or outlook.
 */		                         
    cvg_getFld ( &_primaryEl, TAG_GFA_FCSTHR, value, &ier );
    		    
    if ( strchr ( value, '-' ) ) {                                              
	pgsmear_snapEl ( True, elOut, &ier );			
    }
    
/*
 *  Convert into device for ghosting in current projection.
 */		                         
    *nout = elOut->elem.gfa.info.npts;
		        
    gtrans ( sys_M, sys_D, nout, elOut->elem.gfa.latlon,
	     &(elOut->elem.gfa.latlon[*nout]), outX, outY,
	     &ier, strlen(sys_M), strlen(sys_D) );
		    
    
    G_MALLOC ( xtmp, float, ntmp, "pgconn_smear xtmp" );
    G_MALLOC ( ytmp, float, ntmp, "pgconn_smear ytmp" );
    G_MALLOC ( lat, float, ntmp, "pgconn_smear lat" );
    G_MALLOC ( lon, float, ntmp, "pgconn_smear lon" );

/*
 *  Adopt the first selected GFA's attributes.
 */
    cvg_getFld ( &_primaryEl, TAG_GFA_FCSTHR, value, &ier );
    cvg_setFld ( elOut, TAG_GFA_FCSTHR, value, &ier );
                    
    cvg_getFld ( &_primaryEl, TAG_GFA_SUBTYPE, value, &ier );
    cvg_setFld ( elOut, TAG_GFA_SUBTYPE, value, &ier );		    
    ces_get( atoi( value ), elOut, &ier );
		    
    elOut->hdr.maj_col = _primaryEl.hdr.maj_col;
    elOut->hdr.min_col = _primaryEl.hdr.min_col;
}
