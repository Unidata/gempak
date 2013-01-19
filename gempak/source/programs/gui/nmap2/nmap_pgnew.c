#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "hints.h"
#include "NxmTxt.h"			/* to get SOFTWARE define */
#include "proto_xw.h"

extern XtAppContext _appContext;

#define NONE_CURRENT   0

static int	_maxAllowPts	= (MAXPTS - 1);	/* functional limit */

static float	_xPtsBuf[MAXPTS];
static float	_yPtsBuf[MAXPTS];
static int	_activePt;		/* active_point */
static char	_ghostTyp;
static int	_currClass;
static int	_currObject;
static Boolean	_multiPoint;

static Boolean	_quitRotate;

static Boolean  _watchLine = FALSE;

static char 	_labelGrptyp;
static int  	_labelGrpnum;


/*
 *  private callback functions
 */
static void pgnew_dynamicDrag ( Widget, XtPointer, XEvent*, Boolean* );
static void pgnew_dynamicDrop ( Widget, XtPointer, XEvent*, Boolean* );
static void pgnew_dynamicEnd  ( Widget, XtPointer, XEvent*, Boolean* );
static void pgnew_dynamicStart( Widget, XtPointer, XEvent*, Boolean* );

/*
 * private functions
 */
static void pgnew_setSmthCls ( void );


/************************************************************************
 * nmap_pgnew.c                                                         *
 *                                                                      *
 * This module contains the event_handling/callback functions that are  *
 * used to draw new vgf objects.	                                *
 *                                                                      *
 * CONTENTS:								*
 * pgnew_setArmDynamic()  set callbacks for 'Arm Dynamic' state.        *
 * pgnew_resetMouse()     Activated when clicking middle mouse button   *
 * pgnew_setMultiPt()	  set the value for _multiPoint			*
 * pgnew_setWatchLn()	  set the value for _watchLine			*
 * pgnew_setGrpInfo()	  set the label group type and group number	*
 *									*
 * pgnew_dynamicStart()  'Arm Dynamic' state -- start (mouse press)     *
 * pgnew_dynamicDrag()   'Arm Dynamic' state -- drag  (motion)		*
 * pgnew_dynamicDrop()   'Arm Dynamic' state -- drop  (mouse release)   *
 * pgnew_dummyDrop  ()    mouse drop callback for volcano element	*
 * pgnew_dynamicEnd()     finish drawing new vgf elements		*
 *									*
 * pgnew_setSmthCls()	  set the smooth level and closed flag		*
 * pgnew_getGrpInfo()	  get the label group type and group number	*
 ***********************************************************************/

/*=====================================================================*/

void pgnew_setArmDynamic ( void )
/************************************************************************
 * pgnew_setArmDynamic                                                  *
 *                                                                      *
 * This function associates callback functions with the mouse		*
 * actions for product generation.                                      *
 *                                                                      *
 * void pgnew_setArmDynamic( )                                          *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAi      	10/97                                           *
 * E. Safford/GSC	06/98	moved to nmap_pgnew.c			*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * G. Krueger/EAI       10/98   Using table for hints                   *
 * S. Law/GSC		07/99	added _multipoint setup			*
 * S. Law/GSC		08/99	added adjusted max points for SIGMETs	*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * S. Law/GSC		02/00	added CCF check				*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 * M. Li/SAIC		10/01	Added CLASS_MARKER			*
 * E. Safford/SAIC	11/01	added call to mcanvw_getCurPos		*
 * J. Wu/SAIC		11/02	add CLASS_LIST				*
 * J. Wu/SAIC		10/03	add CLASS_MET				*
 * H. Zeng/XTRIA	10/03   added Volcano Ash Cloud type		*
 * H. Zeng/XTRIA	01/04   added more ash cloud types		*
 * J. Wu/SAIC		04/04	move CLASS_LIST case to nmap_pglist.c	*
 ***********************************************************************/
{
    float	xx, yy;
    int		xoff, yoff, ier;
/*---------------------------------------------------------------------*/


    _currClass  = pgpalw_getCurClassId();
    _currObject = pgpalw_getCurObjId();
    
    switch (_currClass) {

      case CLASS_LINES:
      case CLASS_FRONTS:
      case CLASS_WATCHES:
      case CLASS_TRACKS:
      case CLASS_MET:
	_multiPoint = TRUE;
	_maxAllowPts = MAXPTS - 1;
	pggst_veilGhost (FALSE);

	break;

      case CLASS_SIGMETS:

	_maxAllowPts = MAX_SIGMET - 1;

	if (_currObject == OBJ_SIGCCF) {

	    _multiPoint = TRUE;
	    pggst_veilGhost (FALSE);
	}
        else if (_currObject == OBJ_SIGVAC) {

             if (pgvacw_getSubType() != ASHCLD_AREA &&
		 pgvacw_getSubType() != ASHCLD_LINE    ) {

	         _multiPoint = FALSE;
	         pggst_veilGhost (TRUE);
	     }
	     else {

	         _multiPoint = TRUE;
	         pggst_veilGhost (FALSE);
	     }
        }
	else if ( pgsigw_getSubType () == SIGTYP_ISOL ) {

	     _multiPoint = FALSE;
	     pggst_veilGhost (TRUE);
	}
	else {

	     _multiPoint = TRUE;
	     pggst_veilGhost (FALSE);
	}

	break;

      case CLASS_SYMBOLS:
      case CLASS_COMSYM:
      case CLASS_TEXT:
      case CLASS_WINDS:
      case CLASS_CIRCLE:
      case CLASS_MARKER:

	_multiPoint = FALSE;
	_maxAllowPts = MAXPTS - 1;
	pggst_veilGhost (TRUE);
	break;

    }

    mcanvw_setDynamicFunc( (XtEventHandler)&pgnew_dynamicStart, 
	    		   (XtEventHandler)&pgnew_dynamicDrag,
			   (XtEventHandler)&pgnew_dynamicDrop, CURS_DEFAULT );

    mbotw_actionSet(ACHINT_ADD);
    mbotw_mouseSet(LMHINT_PUT, MMHINT_TOSELECTOPER);

    _activePt = 0;

    mcanvw_getCurPos( &xx, &yy );
    xgtoff (&xoff, &yoff, &ier);
    
    _xPtsBuf[0] = (float)xoff + xx;
    _yPtsBuf[0] = (float)yoff + yy;

}

/*=====================================================================*/

void pgnew_resetMouse ( Widget w, XtPointer clnt, XEvent *event, 
						Boolean *ctdr )
/************************************************************************
 * pgnew_resetMouse                                                     *
 *                                                                      *
 * Callback for resetting with the mouse                                *
 *                                                                      *
 * void pgnew_resetMouse ( w, clnt, event, ctdr )                     	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Widget that activated callback  *
 *      clnt		XtPointer       Pointer to client data          *
 *      *event          XEvent          Event that triggered callback   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         4/97   Created                                 *
 * D. Keiser/GSC         5/97   Major clean up - remove G-level calls   *
 * D. Keiser/GSC         6/97   Added switch on  gr->cmd.class          *
 * E. Wehner/Eai         7/97   Stay in draw mode for lines/frn         *
 * E. Wehner/EAi         7/97   Unmanage palettes                       *
 * E. Wehner/EAi         9/97   stop passing grinfo record              *
 * C. Lin/EAI           10/97   cmd_getClass->pgpalw_getCurClassId      *
 * E. Safford/GSC       10/97   added text handling functionality       *
 * C. Lin/EAI           10/97   modified for Watch Box                  *
 * E. Safford/GSC       12/97   added calls to popdowns for attr edits  *
 * E. Safford/GSC       12/97   modified to handle initial wind rotn    *
 * C. Lin/EAI           04/98   modified to handle label function       *
 * E. Safford/GSC       04/98   disable undo for COMSYM & rmv cvg_scan  *
 * E. Safford/GSC       05/98   fix deselect on new lables & new undo   *
 * E. Safford/GSC	06/98	moved to nmap_pgnew.c			*
 * T. Lee/GSC		06/98	Eliminated end points for special lines	*
 * D.W.Plummer/NCEP	06/98	changes for watch boxes			*
 * E. Safford/GSC	06/98	Eliminated starting end pts for spl lns	*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC	07/98	cleanup                                	*
 * C. Lin/EAI	        08/98	remove the distance checking in arrows  *
 * C. Lin/EAI	        08/98	modify Act&mouse hints for(_activePt<=1)*
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * G. Krueger/EAI	09/98	Remove redundant MBOTW_ACTIONSETs	*
 * E. Safford/GSC	10/98	mod for param change to cvg_rdrec      	*
 * G. Krueger/EAI       10/98   Using table for hints                   *
 * E. Safford/GSC       11/98   mod for gempak level combosyms          *
 * D.W.Plummer/NCEP	11/98	changes for editing pgram watch edges	*
 * S. Law/GSC		11/98	moved pgwbxw_init -> pgwpts_init	*
 * S. Law/GSC		12/98	added call to pgwbxw_setWlst		*
 * W. Li/EAI		02/99	fixed rotation problem for FUNC_LABEL	*
 * W. Li/EAI		02/99	fixed label reselection prob. for lines	*
 * S. Law/GSC		05/99	added CLASS_TRACKS			*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * W. Li/EAI		05/99	added call to  pggrpw_getGrpTypName	*
 * S. Law/GSC		07/99	changed to use _multiPoint		*
 * E. Safford/GSC	08/99	clean up reset after label drop		*
 * S. Law/GSC		09/99	moved various calls to pgnew_dynamicEnd	*
 * E. Safford/GSC	12/99	add _watchLine check           		*
 * M. Li/GSC		 1/00	Used string variables in gtrans		*
 * S. Law/GSC		02/00	changed to use _currObject		*
 * S. Law/GSC		03/00	added call to pgccfw_saveNew		*
 * H. Zeng/EAI          08/00   added validation of track times         *
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * H. Zeng/EAI          12/00   modified for the undo new design        *
 * H. Zeng/EAI          03/01   modified to use ces_gtgid()             *
 * J. Wu/GSC            03/01   removed handle bar on the drawn watches	*
 * E. Safford/GSC	04/01	use pgnew_getGroupInfo for grptyp & num *
 * M. Li/GSC		07/01	removed pgnew_getGrpInfo		* 
 * E. Safford/GSC	07/01	fix bug - wrong grptyp for label action *
 * H. Zeng/EAI          09/01   revised GROUP functionality             *
 * M. Li/SAIC		11/01	Added OBJ_TEXTICNG			*
 * J. Wu/SAIC	        12/01   rebuild range record after xpgrfrsh()	*
 * E. Safford/SAIC	04/02	fix unnecessary grpnum incr w/ labels   *
 * M. Li/SAIC		04/02	add pglabel_setLabelPending		*
 * M. Li/SAIC		06/02	Moved _labelGrptyp and grpnum to module	*
 * J. Wu/SAIC	        08/02   allow to label a line with symbol	*
 * J. Wu/SAIC	        08/02   check label type when drawing a symbol	*
 * J. Wu/SAIC	        11/02   add CLASS_LIST				*
 * J. Wu/SAIC	        11/02   allow creating LIST from a closed line	*
 * H. Zeng/XTRIA	01/03   modifed arguments to pgwbxw_getAttr	*
 * J. Wu/SAIC		03/03	add OBJ_TEXTMCLOUD			*
 * S. Jacobs/NCEP	 3/03	Added group type and num for LIST	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		02/04	add CLASS_MET->OBJ_AIRMET (GFA_ELM)	*
 * J. Wu/SAIC		03/04	add CLASS_MET->OBJ_NCONSIG (GFA_ELM)	*
 * E. Safford/SAIC	04/04	add "else" to the CLASS_MET if test	*
 * J. Wu/SAIC		04/04	move CLASS_LIST case to nmap_pglist.c	*
 * J. Wu/SAIC		05/04	add CLASS_MET->OBJ_GFA (GFA_ELM)	*
 * J. Wu/SAIC		08/04	set proper mouse hints for drawing GFA	*
 * J. Wu/SAIC		09/04	remove OBJ_AIRMET & OBJ_NCONSIG		*
 * H. Zeng/SAIC		10/04	modified arguments to pgwbxw_getAttr	*
 * E. Safford/SAIC	03/07	add drawGfaP flag for FBBA GFA elements *
 * L. Hinson/AWC        07/09   add drawCCF flag for CCF objects, to    *
 *                              use middle mouse click to place text    *
 ***********************************************************************/
{
    char	mesg[128], grptyp; 
    signed char	cnty_fill;
    int		cnty_colr, ier;
    int		location, grpnum, elnum, np;
    int		wcolr, wstyle, wshape, mtype, mwidth;
    float	tlat[10], tlon[10], msize;
    float	lat[MAXPTS], lon[MAXPTS];
    Widget      draw_w;
    char        mesg2[] = "Unable to draw TRACK.\n\n"
                          "Please check First&Second times!\n" ;
    
    VG_DBStruct	el;
    Boolean	drawGFA = False, drawGfaP = False, drawCCF = False;
/*---------------------------------------------------------------------*/

    pggst_clearGhost(TRUE);
    _activePt++;

/*
 * Check if we are drwing a GFA element.
 */
    drawGFA  = ( _currClass == CLASS_MET &&  _currObject == OBJ_GFA );
    drawGfaP = ( _currClass == CLASS_MET &&  _currObject == OBJ_GFA_P );
    drawCCF  = ( _currClass == CLASS_SIGMETS && _currObject == OBJ_SIGCCF );
    
/*
 * Load up the points for the element.
 */
    if (_multiPoint) {

	mcanvw_setDynActFlag(False);

	if (_activePt > 1) {

	    if (_currClass == CLASS_WATCHES) {
		pgwbxw_getAttr(&wcolr, &wstyle, &wshape,
			       &mtype, &msize,  &mwidth,
			       &cnty_fill, &cnty_colr    );
	    }
	    else {
		wstyle = -1;
	    }

	    if ( wstyle == PGRAM ) {
		np = 2;
		gtrans (sys_D, sys_M, &np, _xPtsBuf, _yPtsBuf, 
			tlat, tlon, &ier, strlen(sys_D), strlen(sys_M) );

		pgwpts_init (tlat, tlon, wshape, lat, lon, &ier);
		np = 8;
	    }
	    else {
		np = _activePt;
		gtrans (sys_D, sys_M, &np, _xPtsBuf, _yPtsBuf, 
			lat, lon, &ier, strlen(sys_D), strlen(sys_M) );
	    }

	    if (_currClass == CLASS_SIGMETS && _currObject == OBJ_SIGCCF) {
                pgundo_newStep();
		pgccfw_saveNew (np, lat, lon);
                pgundo_endStep();
	    }
	    else if ( drawGFA ) {
	    	pgundo_newStep();
		pggfaw_saveNew ( np, lat, lon );
                pgundo_endStep();
	    }
	    else if( drawGfaP ) {
	    	pgundo_newStep();
		pggfawp_saveNew ( np, lat, lon );
                pgundo_endStep();
            }
            else if(_currClass == CLASS_TRACKS && 
                    !( pgtrkw_validateTimes() )   ) {
  
/*
 * For track, if First&Second times are not valid, Popup the
 * warning window and skip saving the element.
 */
	        draw_w = (Widget)mcanvw_getDrawingW();
	        NxmWarn_show(draw_w, mesg2);

            }
	    else {
                pgnew_getGrpInfo ( &grptyp, &grpnum );
		_labelGrptyp = grptyp;
		_labelGrpnum = grpnum;
		pgvgf_save(grptyp, grpnum, np, lat, lon, &location, &ier );
		pglabel_setLabelPending ( pglabel_getLabFlag() );
	
		if (_watchLine) { 
		    pgwbxw_addWtchLn(location); 
		}

		pglabel_saveOldInfor (_currClass, _currObject);

		if (ier == 0) {
		    pgundo_newStep();
		    pgundo_storeThisLoc (location, UNDO_ADD, &ier);
                    pgundo_endStep();
		}
		else if ( ier != 0 ) {
		    sprintf(mesg, "Draw func is undetermined -- %i ", 
			    _currObject);
		    NxmWarn_show(w, mesg);
		}

	    }

	    _activePt = 0;

/*
 *  If drawing a GFA or CCF,  pressing middle mouse will place the 
 *  text box and DONE the drawing.  For drawing other elements,
 *  pressing middle mouse will reset the action.
 */
	    if ( drawGFA || drawGfaP || drawCCF ) {
	        mbotw_mouseSet(LMHINT_PUT, MMHINT_DONE);
	    }
	    else {	
	        mbotw_mouseSet(LMHINT_PUT, MMHINT_TOSELECTOPER);
            }

	}
	else {
            pgnew_dynamicEnd(w, clnt, event, ctdr);
	    
/*
 *  Set hints to SELECT after finishing drawing a GFA.
 */
	    if ( drawGFA || drawGfaP ) {
	        mbotw_mouseSet(LMHINT_SELECT, MMHINT_NOACTION);
	    }
	}
    }
    else {
	mbotw_mouseSet(LMHINT_PUT, MMHINT_TOSELECTOPER);

	if ( _activePt == 1 ) {
	    np = _activePt;
	    gtrans (sys_D, sys_M, &np, _xPtsBuf, _yPtsBuf, 
		    lat, lon, &ier, strlen(sys_D), strlen(sys_M) );

	    if ( pglabel_getLabelPending() &&
		 ( _currClass == CLASS_TEXT || 
		     ( _currClass == CLASS_SYMBOLS && 
		       pglabel_getLabType() == 2 ) ||
		   _currClass == CLASS_COMSYM ||
		   _currClass == CLASS_MARKER  ) ) { /* LABEL FUNCTION */

		pgvgf_save(_labelGrptyp, _labelGrpnum, np, lat, lon, 
						&location, &ier);

		if (ier == 0) {
		    pgundo_newStep();
		    pgundo_storeThisLoc(location, UNDO_ADD, &ier);
                    pgundo_endStep();
		    cvg_rdrec (cvg_getworkfile(), location, &el, &ier);
		    pgactv_setActvElm(&el, location);

		    if ( _currClass == CLASS_TEXT ) {
		        if (el.elem.spt.info.ithw == SOFTWARE &&
			    (_currObject != OBJ_TEXTTURB && 
			     _currObject != OBJ_TEXTICNG &&
			     _currObject != OBJ_TEXTMCLOUD) ) { 
			    pgactv_setActvElm(&el, location);
			    pghdlb_select( &el, location);
		        }
		    } 

		    crg_getinx (location, &elnum, &ier); 
		    pghdlb_setSelect(location);
		    pghdlb_displayAllSel();

		    mbotw_mouseClear();
		    pgpalw_setupOper();
		}
		else if ( ier != -2 ) {
		    /* failure was other than no text */
		    sprintf(mesg, "Draw func is undetermined -- %i ",
			    _currObject);
		    NxmWarn_show(w, mesg);

		    mbotw_mouseClear();
		    mcanvw_disarmDynamic();
		}
	    }
	    else {
                pgnew_getGrpInfo (&grptyp, &grpnum);

		_labelGrptyp = grptyp;
		_labelGrpnum = grpnum;

		pglabel_setLabelPending ( pglabel_getLabFlag() );
		pgvgf_save(grptyp, grpnum, np, lat, lon, &location, &ier );
		    
		pglabel_saveOldInfor (_currClass, _currObject);

		if ( ier == 0 ) {
		        
		    pgundo_newStep();
		    pgundo_storeThisLoc (location, UNDO_ADD, &ier);
                    pgundo_endStep();

		    if (_currClass == CLASS_WINDS ||
			_currClass == CLASS_TEXT) { /* SET ROTATION MODE */
			cvg_rdrec (cvg_getworkfile(), location, &el, &ier);
			pgactv_setActvElm(&el, location);

			if (_currClass != CLASS_TEXT ||
                            (_currClass == CLASS_TEXT &&
			     el.elem.spt.info.ithw == SOFTWARE &&
			     (_currObject != OBJ_TEXTTURB &&
			      _currObject != OBJ_TEXTICNG &&
			      _currObject != OBJ_TEXTMCLOUD) ) ) {
			    pgactv_setActvElm(&el, location);
			    pghdlb_select( &el, location);

			    mbotw_mouseSet(LMHINT_ROTATE, MMHINT_DONE);
			}
		    }
		    else if (_currClass == CLASS_CIRCLE) { /* SET RADIUS MODE */
			cvg_rdrec (cvg_getworkfile(), location, &el, &ier);
			pgactv_setActvElm(&el, location);
		    }
		}
		else if ( ier != -2 ) {
		    /* failure was other than no text */
		    sprintf(mesg, "Draw func is undetermined -- %i ",
			        _currObject);
		    NxmWarn_show(w, mesg);

		    mbotw_mouseClear();

		    mcanvw_disarmDynamic();
	        } 
	    }
	}	
	else {
	    pgpalw_classPopdown ();
	    
	    mbotw_actionClear();
	    mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
	}
    }    
    _activePt = 0;
    xpgrfrsh(); 
    crg_rebuild(); 

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

void pgnew_setMultiPt ( Boolean mp_state )
/************************************************************************
 * pgnew_setMultiPt							*
 *									*
 * Sets the state of _multiPoint.					*
 *									*
 * pgnew_setMultiPt ( mp_state )					*
 *									*
 * Input parameters							*
 *	mp_state	Boolean		new state for _multiPoint	*
 *									*
 * Output parameters							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		07/99	initial coding				*
 ***********************************************************************/
{

    if (mp_state == _multiPoint) {
	return;
    }

    _multiPoint = mp_state;

    if (mcanvw_getDynActFlag ()) {
	if (!_multiPoint) {
	    pggst_drawGhost(_ghostTyp);
	}

	pggst_clearGhost (TRUE);
	_activePt = 0;
	mcanvw_setDynActFlag (FALSE);
    }

    pggst_veilGhost (!_multiPoint);
}

/*=====================================================================*/

void pgnew_setWatchLn ( Boolean wl_state )
/************************************************************************
 * pgnew_setWatchLn							*
 *									*
 * Sets the state of _watchLine.					*
 *									*
 * pgnew_setWatchLn ( wl_state )					*
 *									*
 * Input parameters							*
 *	wl_state	Boolean		new state for _multiPoint	*
 *									*
 * Output parameters							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	12/99	initial coding				*
 ***********************************************************************/
{
    _watchLine = wl_state;
}

/*=====================================================================*/

static void pgnew_dynamicStart ( Widget w, XtPointer clnt, 
				XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgnew_dynamicStart                                                   *
 *                                                                      *
 * Internal callback for clicking with the mouse in 'Arm Dynamic' state *
 *                                                                      *
 * static void pgnew_dynamicStart( w, clnt, event)               	*
 *                                                                      *
 * Input parameters:                                                    *
 *	w               Widget      Widget that activated the callback  *
 *	clnt		XtPointer   Pointer to client data		*
 *	*event          XEvent      Event that triggered the callback   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi    	 4/97   Created                                 *
 * D. Keiser/GSC    	 6/97   Reorganized                             *
 * C. Lin/EAI        	 8/97   Added offsets for 'S' coord (roam)      *
 * E. Wehner/EAi         9/97   stop passing grinfo record      	*
 * C. Lin/EAI           10/97   rename from NxmMouStart                 *
 * C. Lin/EAI           10/97   add CLASS_WATCHES processing            *
 * E. Safford/GSC       02/98   mofify mesg handling for max point check*
 * E. Safford/GSC	06/98	moved to nmap_pgnew.c			*
 * D.W.Plummer/NCEP	 6/98	added watch box processing		*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC	07/98	ghost smoothed lines 			*
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * G. Krueger/EAI	09/98	Remove redundant MBOTW_ACTIONSETs	*
 * G. Krueger/EAI       10/98   Using table for hints                   *
 * S. Law/GSC		05/99	added CLASS_TRACKS			*
 * S. Law/GSC		07/99	changed to use _multiPoint		*
 * S. Law/GSC		08/99	MAX_ALLOW_PTS -> _maxAllowPts		*
 * E. Safford/GSC	10/99	updated for new xwcmn.h			*
 * S. Law/GSC		02/00	removed pgnew_setSmthCls parameter	*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * S. Law/GSC		07/00	added calls to pgtrkw_setFrmTime	*
 * H. Zeng/XTRIA        01/03   modified arguments to pgwbxw_getAttr    *
 * H. Zeng/SAIC		10/04	modified arguments to pgwbxw_getAttr	*
 * B. Yin/SAIC		05/05	set GFA from line text to blank		*
 * H. Zeng/SAIC		08/06	added distance display option check for *
 *				CLASS_FRONTS&CLASS_LINES		*
 * E. Safford/SAIC	03/07	add GFA_OBJ_P check 			*
 ***********************************************************************/
{
    int   	wcolr, wstyle, wshape, mtype, mwidth;
    int		cnty_colr, ii, ier, xoff, yoff;
    float	xx, yy, msize;
    char	hint_str[128], mesg[] = {"Maximum vertices exceeded."};
    signed char	cnty_fill;
/*---------------------------------------------------------------------*/
/*
 * Set GFA from line text to an empty string
 */
    if ( _currClass == CLASS_MET && _currObject == OBJ_GFA ) {
	pggfaw_setFrom ( 0, NULL, NULL );
    }
    else if( _currClass == CLASS_MET && _currObject == OBJ_GFA_P ) {
	pggfawp_setFrom ( 0, NULL, NULL );
    }

/*
 * Make sure event triggered off left button
 */
    if (event->xbutton.button == Button1) {
	wstyle = -1;
        if (_currClass == CLASS_WATCHES )	{
            pgwbxw_getAttr(&wcolr, &wstyle, &wshape,
			   &mtype, &msize,  &mwidth,
			   &cnty_fill, &cnty_colr    );
	}

	if (_currClass == NONE_CURRENT) return;

	xgtoff (&xoff, &yoff, &ier);
	xx = (float) (event->xbutton.x + xoff); 
	yy = (float) (event->xbutton.y + yoff); 

/*
 * if dynamics are already active, draw to point
 */
        if (mcanvw_getDynActFlag()) {

	    if (_multiPoint) {
		if (_activePt + 2  >= _maxAllowPts) {
		    NxmWarn_show(w, mesg);
		    mbotw_mouseSet(LMHINT_ERROR, MMHINT_DONE);
		}
		else {
		    _activePt++;
		    _xPtsBuf[_activePt +1] = _xPtsBuf[_activePt] = xx;
		    _yPtsBuf[_activePt +1] = _yPtsBuf[_activePt] = yy;

		    pggst_drawGhost(_ghostTyp);
		    pggst_replaceGhostPts (1, &xx, &yy, &ier);

		    pggst_addGhostPts (1, &xx, &yy, &ier);
		    pggst_drawGhost(_ghostTyp);

		    if (_currClass == CLASS_TRACKS) {
			pgtrkw_setFrmTime (FALSE);
		    }

		    if ( (_activePt == 1 && wstyle == PGRAM ) ||
			(_activePt == 5 && wstyle == WBC ) ) {
			pgnew_resetMouse(w, clnt, event, ctdr);
		    }

		    if ( _currClass == CLASS_WATCHES && wstyle == WBC ) {
			if ( _activePt == 0 ) {
			    mbotw_mouseSet(LMHINT_PUT, MMHINT_TOSELECTOPER);
			}
			else {
			    sprintf( hint_str, "%d MORE", 5 - _activePt );
			    mbotw_mouseSet(hint_str, MMHINT_TOSELECTOPER);
			}
		    }
		}
	    }
	    else if (_currClass == CLASS_WINDS || _currClass == CLASS_TEXT) {
		pgevt_setWindPlActv (TRUE);
            }
        }
        else {

            _activePt = 0;

	    if (_multiPoint) {

		pggst_clearGhost (TRUE);	
		pgnew_setSmthCls ();

		mcanvw_setDynActFlag(True);

/*
 *  Set the ghost pointers & PtsBuf array
 */
		for (ii = 0; ii < 2; ii++) {
		    *(_xPtsBuf + ii) = xx;
		    *(_yPtsBuf + ii) = yy;
		} 
		
		pggst_addGhostPts (2, _xPtsBuf, _yPtsBuf, &ier); 
		_ghostTyp = GST_NEW_LINE;
		pggst_drawGhost(_ghostTyp);

		if (_currClass == CLASS_TRACKS) {
		    pgtrkw_setFrmTime (TRUE);
		}

		if (_currClass == CLASS_WATCHES) {
		    if (wstyle == WBC) {
			mbotw_mouseSet(LMHINT_5MORE, MMHINT_TOSELECTOPER);
		    }
		    else {
			mbotw_mouseSet(LMHINT_END, MMHINT_TOSELECTOPER);
		    }
		}

/*
 * For LINE&FRONT check distance display option flag.
 */
		if ( (_currClass == CLASS_LINES || 
		      _currClass == CLASS_FRONTS  ) && 
		     pgdist_isDplOn() ) {

		     pgdist_start (event->xbutton.x, event->xbutton.y);
		}
            }
	    else {
		_ghostTyp = GST_NORMAL;
	    }
        }
    }
    else {	/* middle or third mouse button */

        if (!mcanvw_getDynActFlag()) {
	    pgpalw_setCurObj((Widget)NULL);
	}
	pggst_clearGhost(TRUE);

/*
 * For LINE&FRONT check distance display option flag.
 */
	if ( (_currClass == CLASS_LINES || 
	      _currClass == CLASS_FRONTS  ) && 
	     pgdist_isDplOn() ) {

	     pgdist_stopUpdate ( );
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgnew_dynamicDrag ( Widget w, XtPointer clnt, 
				XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgnew_dynamicDrag                                                    *
 *                                                                      *
 * Callback for dragging with the mouse                                 *
 *                                                                      *
 * static void pgnew_dynamicDrag( w, clnt, event, ctdr )		*
 *                                                                      *
 * Input parameters:                                                    *
 *  w          	    Widget      Widget that activated the callback      *
 *  clnt	    XtPointer   Pointer to client data (graphics info)  *
 *  *event          XEvent      Event that triggered the callback       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi    	04/97   Created                                 *
 * D. Keiser/GSC    	06/97   Added CLASS_SYMBOLS, "+" marker drawing *
 * E. Safford/GSC   	07/97   Added CLASS_TEXT                        *
 * C. Lin/EAI       	08/97   Added offsets for 'S' coord (roam)      *
 * E. Wehner/EAi    	09/97   stop passing grinfo record              *
 * C. Lin/EAI           10/97   rename from NxmMouDrag                  *
 * E. Safford/GSC	06/98	moved to nmap_pgnew.c			*
 * E. Safford/GSC	07/98	ghost smoothed lines 			*
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * S. Law/GSC		05/99	added CLASS_TRACKS			*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * S. Law/GSC		07/99	changed to use _multiPoint		*
 * E. Safford/GSC	10/99	updated for new xwcmn.h			*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 ***********************************************************************/
{

    int		ier, xoff, yoff;
    float	xx, yy;
/*---------------------------------------------------------------------*/

    xgtoff (&xoff, &yoff, &ier);
    xx = (float) (event->xbutton.x + xoff); 
    yy = (float) (event->xbutton.y + yoff); 

/*
 * if dynamics are active...................
 */
    if (mcanvw_getDynActFlag()) {

	if (_multiPoint) {

/*
 *  Erase the previous line
 */
	    pggst_drawGhost(_ghostTyp);

/*
 *  Load the new point(s)
 */
	    _xPtsBuf[_activePt + 1] = xx;
	    _yPtsBuf[_activePt + 1] = yy;

	    pggst_replaceGhostPts (1, &_xPtsBuf[_activePt +1],
				   &_yPtsBuf[_activePt +1], &ier);	

/*
 *  Draw the new ghost line
 */
	    pggst_drawGhost(_ghostTyp);

/*
 * For LINE&FRONT check distance display option flag.
 */
	    if ( (_currClass == CLASS_LINES || 
		  _currClass == CLASS_FRONTS  ) && 
		 pgdist_isDplOn() ) {

		 pgdist_update (event->xbutton.x, event->xbutton.y);
	    }

	}
	else {

	    pggst_clearGhost(TRUE);
	    pggst_addGhostPts ( 1, &xx, &yy,  &ier);
	    pggst_drawGhost(_ghostTyp);

	    _xPtsBuf[0] = xx;
	    _yPtsBuf[0] = yy;
        }
    }
    else if (!_multiPoint) {
	pggst_clearGhost (TRUE);
	pggst_addGhostPts ( 1, &xx, &yy, &ier);
  	pggst_drawGhost(_ghostTyp);
	mcanvw_setDynActFlag (TRUE);
    }
}

/*=====================================================================*/

static void pgnew_dynamicDrop ( Widget wid, XtPointer clnt, 
				XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgnew_dynamicDrop							*
 *									*
 * Callback for clicking with the mouse					*
 *									*
 * static void pgnew_dynamicDrop (wid, clnt, event, ctdr )		*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data		*
 *	*event		XEvent		Event that triggered callback	*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	10/96	Created					*
 * D. Keiser/GSC	 3/97	Clean up				*
 * D. Keiser/GSC	 6/97	Added CLASS_SYMBOLS			*
 * E. Safford/GSC	 7/97	Added CLASS_TEXT			*
 * E. Wehner/EAi	 9/97	stop passing grinfo record		*
 * C. Lin/EAI		10/97	rename from NxmMouDrop			*
 * E. Safford/GSC	10/97	modify TEXT handling to resetMouse	*
 * C. Lin/EAI		10/97	add CLASS_WATCHES processing		*
 * C. Lin/EAI		11/97	further cleanup				*
 * E. Safford/GSC	12/97	modify for intial wind rotation		*
 * E. Safford/GSC	02/98	mod message for use w/ MAXPNT check	*
 * W. Li/EAI		03/98	add call to pgsymb_popdown		*
 * C. Lin/EAI		04/98	modify to handle LABEL function		*
 * S. Law/GSC		05/98	replaced pgpalw_multSelect w/ _setupOper*
 * E. Safford/GSC	06/98	moved to nmap_pgnew.c			*
 * E. Safford/GSC	06/98	fixed overwrite on mesg			*
 * E. Safford/GSC	06/98	fixed dynamic on labels & reset on symb	*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC	08/98	update ghosting				*
 * C. Lin/EAI		08/98	cleanup the hints			*
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * G. Krueger/EAI	09/98	Remove redundant MBOTW_ACTIONSETs	*
 * E. Safford/GSC	10/98	mod for param change to cvg_rdrec	*
 * G. Krueger/EAI	10/98	Using table for hints			*
 * W. Li/EAI		01/99	NxmTxtA_XXX --> pgtxt_XXX		*
 * W. Li/EAI		02/99	fixed label rotation problem		*
 * W. Li/EAI		02/99	fixed label problems			*
 * S. Law/GSC		05/99	added CLASS_TRACKS			*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * W. Li/EAI		05/99	changed deselection sequence		*
 * E. Safford/GSC	06/99	Added call to pgnew_exitLabel		*
 * S. Law/GSC		07/99	added CLASS_TRACKS			*
 * S. Law/GSC		07/99	added CLASS_SIGMETS			*
 * S. Law/GSC		09/99	moved various calls to pgnew_dynamicEnd	*
 * E. Safford/GSC	12/99	add _watchLine check and special exit	*
 * S. Law/GSC		02/00	changed to use _currObject		*
 * M. Li/SAIC		10/01	Added CLASS_MARKER			*
 * J. Wu/SAIC		10/01	add OBJ_KINKLN1, 2 - kink arrow lines	*
 * M. Li/SAIC		11/01	Added OBJ_TEXTICNG			*
 * M. Li/SAIC		04/02	Added pglabel_getLabelPending		*
 * J. Wu/SAIC	        08/02   allow to label a line with symbol	*
 * J. Wu/SAIC	        08/02   check label type when putting a symbol	*
 * J. Wu/SAIC	        11/02   add CLASS_LIST				*
 * J. Wu/SAIC		03/03	add OBJ_TEXTMCLOUD			*
 * J. Wu/SAIC	        10/03   add CLASS_MET				*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * E. Safford/SAIC	12/03	check for MAX_TRACKS exceeded           *
 * J. Wu/SAIC		04/04	move CLASS_LIST case to nmap_pglist.c	*
 * B. Yin/SAIC		06/05	check if GFA top/bottom field is empty	*
 * B. Yin/SAIC		06/05	add button2 action for GFAs		*
 * L. Hinson/AWC        09/05   Revised logic to delay check of Top/Base*
 *                                till after polygon is rendered with   *
 *                                middle mouse click.                   *
 * H. Zeng/SAIC		08/06	added distance display option check for *
 *				CLASS_CIRCLE				*
 * E. Safford/SAIC	04/07	add pggfawp_* calls			*
 * B. Yin/SAIC          09/07   fully cancel out for GFA warning box    *
 ***********************************************************************/
{
    int         	location, ier;
    Boolean		label_flag, active_flag;
    VG_DBStruct	el;
    static Boolean 	gfaPointsDrawn = False;

    Boolean		useGfaPrime = False, cancel = False; 
    
/*---------------------------------------------------------------------*/
/* 
 *  If current object is GFA, check if the top/bottom field is empty.
 */
    if ( _currClass == CLASS_MET && 
    		( _currObject == OBJ_GFA || _currObject == OBJ_GFA_P )) {

/*
 *  The new GFA GUI is called GFA' or GFA Prime.  Check to see if
 *  we're using the old GFA GUI or the GFA' GUI.
 */
        useGfaPrime = ( _currObject == OBJ_GFA_P );
	cancel = False; 

        if (event->xbutton.button == Button1) {
            gfaPointsDrawn = True; 
        }


        if ( event->xbutton.button == Button2  && !gfaPointsDrawn ) {
/*
 * No points drawn
 */
            pggst_clearGhost (TRUE);
	    pgpalw_classPopdown ();
	    if ( pgpalw_isGrpActv() ) {
	       pgpalw_setCurBtns (FUNC_GROUP, -1, -1);
	    }
	    else {
	       pgpalw_setCurBtns (FUNC_SELECT, -1, -1);
	    }

	    pgpalw_setupOper (); 
            gfaPointsDrawn = False;

	    return;
        }
        
        if (event->xbutton.button == Button2) {

/*
 *  Check if OK to proceed.  For certain hazards the top/bottom
 *  values must be filled in.  Before completing the GFA make sure
 *  these values are present.
 */
            if ( (useGfaPrime && !pggfawp_okToDraw( True )) || 
	         (!useGfaPrime && !pggfaw_okToDraw( True ))) {

	        if( useGfaPrime ) {
                    pggfawp_setOkOnWarning( False );
                    pggfawp_setCancelOnWarning( False );

/* 
 *  Wait Until Top/Bottom Info filled && OK clicked on 
 *  Warning before proceeding 
 */
                    while ( !pggfawp_getOkOnWarning() || 
		    	    XtAppPending(_appContext) ) {
                        XtAppProcessEvent(_appContext,XtIMAll);
                        if ( pggfawp_getCancelOnWarning() ) break;
                    }

		    if( pggfawp_getCancelOnWarning() ) {
			cancel = True;
		    }
	        }
	        else {
                    pggfaw_setOkOnWarning( False );
                    pggfaw_setCancelOnWarning( False );

/* 
 *  Wait Until Top/Bottom Info filled && OK clicked on 
 *  Warning before proceeding 
 */
                    while ( !pggfaw_getOkOnWarning() || 
		    	    XtAppPending(_appContext) ) {
                        XtAppProcessEvent(_appContext,XtIMAll);
                        if ( pggfaw_getCancelOnWarning() ) break;
                    }

		    if( pggfaw_getCancelOnWarning() ) {
			cancel = True;
		    }
	        }


		if( cancel ) {
/*
 * Drop the Airmet if Cancel on Warning Box
 */
                    pggst_clearGhost (TRUE);
                    pgpalw_classPopdown ();

	            if ( pgpalw_isGrpActv() ) {
	                pgpalw_setCurBtns (FUNC_GROUP, -1, -1);
	            }
	            else {
	                pgpalw_setCurBtns (FUNC_SELECT, -1, -1);
	            }
	            pgpalw_setupOper ();
                } 
                else {
                    pgnew_resetMouse(wid, clnt, event, ctdr);            
                }
                gfaPointsDrawn = False;             
                return; 
            }

            gfaPointsDrawn = False;
        }
    }

    switch (_currClass) {
      case CLASS_LINES:
      case CLASS_FRONTS:
      case CLASS_TRACKS:
      case CLASS_SIGMETS:
      case CLASS_MET:
	label_flag = pglabel_getLabFlag();
	active_flag = (_activePt > 0);
	if (event->xbutton.button == Button1) {
	    if (label_flag && active_flag){
		mbotw_mouseSet(LMHINT_NEXT, MMHINT_LABEL);
	    }
	    else {
		mbotw_mouseSet(LMHINT_NEXT, MMHINT_DONE);
	    }

	    if (_currClass == CLASS_SIGMETS && !_multiPoint) {
		pgnew_resetMouse (wid, clnt, event, ctdr);
	    }

	    if ( ( _currObject == OBJ_KINKLN1 || 
	           _currObject == OBJ_KINKLN2 ) )  {
		if ( _activePt >= 1 ) {
		    pgnew_resetMouse(wid, clnt, event, ctdr);
	        }
	    }	

/*
 *  Stop adding track points when MAX available points is hit.
 */
	    if ( _currClass == CLASS_TRACKS ) {
		if ( _activePt + 1 >= MAX_TRACKS - pgtrkw_getNumExtra() ) {
	            pgnew_resetMouse( wid, clnt, event, ctdr );	
                }
            }
	}
	else if (event->xbutton.button == Button2) {	    
	    if (_currClass == CLASS_SIGMETS && !_multiPoint) {
		if (label_flag && active_flag){
		    pglabel_txtpopup();
		}
		else{
		    pgnew_dynamicEnd (wid, clnt, event, ctdr);
		}
	    }
	    else {
		pgnew_resetMouse(wid, clnt, event, ctdr);

/*
 * For LINE&FRONT check distance display option flag.
 */
	        if ( (_currClass == CLASS_LINES || 
		      _currClass == CLASS_FRONTS  ) && 
		     pgdist_isDplOn() ) {

		     pgdist_stop ( );
	        }

		if (label_flag && active_flag) {
		    if ( _currClass == CLASS_LINES && 
		         pgline_getLabType() == 2 ) {
		        pglabel_symbpopup();
		    }
		    else {
		        pglabel_txtpopup();		    
		    }
		}
    		else if ( _watchLine ) {
	 	    pgline_popdown();
		    pghdlb_deselectAll();
		    pgpalw_dsplyObjPal (CLASS_WATCHES);
    	            pgpalw_setCurBtns (-1, CLASS_WATCHES, OBJ_WATCHLN); 
		    pgpalw_setupOper();
                    pgnew_setWatchLn (FALSE); 
		}

	    }
	}
	else if (event->xbutton.button == Button3) {

/*
 * For LINE&FRONT check distance display option flag.
 */
	    if ( (_currClass == CLASS_LINES || 
		  _currClass == CLASS_FRONTS  ) && 
		 pgdist_isDplOn() ) {

		 pgdist_stop ( );
	    }
	}	    
	break;

      case CLASS_WATCHES:
	if ( event->xbutton.button == Button2 ) {
	    if( _activePt >= 6) {
		pgnew_resetMouse(wid, clnt, event, ctdr);
		mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
	    }
	    else {
		_activePt = 0;
		pgnew_dynamicEnd(wid, clnt, event, ctdr);
	    }
	}
	break;

      case CLASS_SYMBOLS:
	label_flag  = pglabel_getLabFlag();
	active_flag = (pghdlb_elemSelected() > 0);
	if (event->xbutton.button == Button1) {
	    if (label_flag && active_flag){
		mbotw_mouseSet(LMHINT_NEXT, MMHINT_LABEL);
	    }
	    else {
		mbotw_mouseSet(LMHINT_NEXT, MMHINT_DONE);
	    }
	    pgnew_resetMouse(wid, clnt, event, ctdr);
	    
	    if ( pglabel_getLabelPending() && 
	         pglabel_getLabType() == 2 )   {
		pgnew_dynamicEnd(wid, clnt, event, ctdr);
	    }
        }
	else if ( event->xbutton.button == Button2 ) {
	    if (label_flag && active_flag){
		pglabel_txtpopup();
	    }
	    else{
		pgnew_dynamicEnd(wid, clnt, event, ctdr);
	    }
	}
	break;

      case CLASS_COMSYM:
      case CLASS_MARKER:
	if (event->xbutton.button == Button1) {
	    pgnew_resetMouse(wid, clnt, event, ctdr);
	    
	    if ( pglabel_getLabelPending() ) {
		pgnew_dynamicEnd(wid, clnt, event, ctdr);
	    }
	}
	else if ( event->xbutton.button == Button2 ) {
	    pgnew_dynamicEnd(wid, clnt, event, ctdr);
	}
	break;

      case CLASS_TEXT:
	pggst_clearGhost(TRUE);

	if (event->xbutton.button == Button1) {
	    pgnew_resetMouse(wid, clnt, event, ctdr);

	    location = pgactv_getElmLoc();
	    cvg_rdrec (cvg_getworkfile(), location, &el, &ier);

	    mbotw_mouseSet(LMHINT_PUT, MMHINT_TOSELECTOPER);

	    if ( (_currObject != OBJ_TEXTTURB && 
	          _currObject != OBJ_TEXTICNG &&
	          _currObject != OBJ_TEXTMCLOUD )
		 && el.elem.spt.info.ithw == SOFTWARE) {
		mcanvw_disarmPress ();
		pgevt_rotateHdl (wid, clnt, event, ctdr);
		_quitRotate = TRUE;
		mbotw_mouseSet(LMHINT_ROTATE, MMHINT_DONE);
	    }

	    if ( pglabel_getLabelPending() ) {
		pgnew_dynamicEnd(wid, clnt, event, ctdr);
	    }
	}
	else if ( event->xbutton.button == Button2 ) {

	    if (_quitRotate) {
		_quitRotate = False;
	    }
	    else {
		pgnew_dynamicEnd(wid, clnt, event, ctdr);
	    }
	}

	break;

      case CLASS_WINDS:
	if (event->xbutton.button == Button1) {
	    pgnew_resetMouse(wid, clnt, event, ctdr);
	    mcanvw_disarmPress ();
	    pgevt_rotateHdl (wid, clnt, event, ctdr);
	    _quitRotate = TRUE;
	}
	else if ( event->xbutton.button == Button2 ) {
	    if (_quitRotate) {
		_quitRotate = False;
		mbotw_mouseSet(LMHINT_PUT, MMHINT_TOSELECTOPER);
		mcanvw_setDynActFlag (FALSE);
	    }
	    else {
		pgnew_dynamicEnd(wid, clnt, event, ctdr);
	    }
	}
	break;

      case CLASS_CIRCLE:
	pggst_clearGhost(TRUE);
	label_flag  = pglabel_getLabFlag();
	active_flag = (pghdlb_elemSelected() > 0);
	if (event->xbutton.button == Button1) {
	    pgrad_setCircPlActv (TRUE);
	    if (label_flag && active_flag) {
		mbotw_mouseSet(LMHINT_DRAG, MMHINT_LABEL);
	    }
	    else {
		mbotw_mouseSet(LMHINT_DRAG, MMHINT_DONE);
	    }
	    pgnew_resetMouse(wid, clnt, event, ctdr);
	    mcanvw_disarmPress ();
	    pgevt_radiusHdl (wid, clnt, event, ctdr);
	    pgcirc_saveAttr ();

/*
 * Check distance display option flag.
 */
	    if ( pgdist_isDplOn() ) {

		 pgdist_start (event->xbutton.x, event->xbutton.y);
	    }
	}
	else if (event->xbutton.button == Button2) {
	    _activePt = 0;
	    pgnew_dynamicEnd(wid, clnt, event, ctdr);
	}
	break;

      default:
	break;
    }
}
 
/*=====================================================================*/

void pgnew_dummyDrop ( Widget wid, XtPointer clnt, 
					XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgnew_dummyDrop							*
 *									*
 * Callback for clicking with the mouse	for volcano element.		*
 *									*
 * void pgnew_dummyDrop (wid, clnt, event, ctdr)			*
 *									*
 * Input parameters:							*
 *	wid		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data		*
 *	*event		XEvent		Event that triggered callback	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA	09/03   initial coding				*
 ***********************************************************************/
{
    if (event->xbutton.button == Button2) {
	pgnew_dynamicEnd (wid, clnt, event, ctdr);
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgnew_dynamicEnd ( Widget w, XtPointer clnt, 
				XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgnew_dynamicEnd                                                     *
 *                                                                      *
 * Internal function for ending the mouse dynamic state.                *
 *                                                                      *
 * static void pgnew_dynamicEnd  ( w, clnt, event, ctdr)		*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      clnt		XtPointer       State information record        *
 *      *event          XEvent          Button press event record       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAi           11/97   based on _pgdyn_dynamicDrop             *
 * E. Safford/GSC       12/97   modify for intial wind rotation         *
 * E. Safford/GSC	06/98	moved to nmap_pgnew.c			*
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC	07/98	clean up & add clearGhost		*
 * S. Law/GCS		09/99	moved various cleanup routines to here	*
 * M. Li/SAIC		04/02	Added pglabel_getLabelPending		*
 * M. Li/SAIC		04/02	Added call to pgpalw_isGrpActv		*
 * E. Safford/SAIC	05/02	don't remove handlebars if Grouping     *
 * J. Wu/SAIC		08/02	update for label line with symbols      *
 ***********************************************************************/
{
    pggst_clearGhost (TRUE);

    if ( pglabel_getLabelPending() ) {
	if (pglabel_ifpopup()) {
	    pglabel_popupOldWin();
	}
	else {
	    pgtxt_popdown();
	    pgpalw_setCurBtns (FUNC_SELECT, CLASS_ANY, -1);
	}

/*
 *  Handlebars are used on the element that is being labeled.
 *  When the label is placed, turn these handlebars off unless
 *  the Grouping Action is currently ongoing.
 */
	if ( !pggrpw_isUp() ) {
	    pghdlb_deselectAll();
  	}
	pglabel_setLabelPending(False);
    }
    else {
	pglabel_setLabType ( 1 );
	pgpalw_classPopdown ();
	if ( pgpalw_isGrpActv() ) {
	    pgpalw_setCurBtns (FUNC_GROUP, -1, -1);
	}
	else {
	    pgpalw_setCurBtns (FUNC_SELECT, -1, -1);
	}

	pgpalw_setupOper ();    
    }
}

/*=====================================================================*/

static void pgnew_setSmthCls ( void )
/************************************************************************
 * pgnew_setSmthCls                                                     *
 *                                                                      *
 * Private function for setting the smoothing level and closed flag	*
 *									*
 * static void pgnew_setSmthCls ()					*
 *									*
 * Input parameters							*
 * Output parameters							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	07/98	initial coding				*
 * S. Law/GSC		03/99	Updated parameters for pgline_getAttr	*
 * S. Law/GSC		02/00	added CCF and removed parameter		*
 * S. Law/GSC		05/00	fixed non-CCF SIGMETs			*
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * E. Safford/GSC	03/01	param change for pgline_getAttr		*
 * J. Wu/SAIC		10/01	param change for pgline_getAttr		*
 * J. Wu/SAIC		10/03	add CLASS_MET				*
 * H. Zeng/XTRIA	10/03   added Volcano Ash Cloud element		*
 * J. Wu/SAIC		02/04	add CLASS_MET->OBJ_AIRMET (GFA_ELM)	*
 * J. Wu/SAIC		03/04	add CLASS_MET->OBJ_NCONSIG (GFA_ELM)	*
 * J. Wu/SAIC		05/04	add CLASS_MET->OBJ_GFA (GFA_ELM)	*
 * J. Wu/SAIC		09/04	remove OBJ_AIRMET & OBJ_NCONSIG		*
 * B. Yin/SAIC		12/05	check if GFA is closed			*
 * E. Safford/SAIC	04/07	fix freeing blocks for GFAs		*
 ***********************************************************************/
{
    char	close, smooth, grptyp;
    signed char	fill;
    int		maj_col, min_col, width, kpos, elmid, ier, subtyp, gemtyp;
    int		one = 1;
    float	size;
    Boolean	closed;
    VG_DBStruct	*element;
/*---------------------------------------------------------------------*/

    if (_currClass == CLASS_LINES) {
        pgline_getAttr (&fill, &close, &maj_col, &width, &size, &kpos,
						&smooth, &grptyp);
	closed = (close == 1);
        if ( _currObject == OBJ_KINKLN1 || _currObject == OBJ_KINKLN2 ) {
	    smooth = 0;
	    closed = FALSE;
	}
    }
    else if (_currClass == CLASS_FRONTS) {
	pgfrtw_getAttr (&smooth, &maj_col, &min_col);
	closed = FALSE;
    }
    else if (_currClass == CLASS_SIGMETS && _currObject == OBJ_SIGCCF) {
	G_CALLOC(element, VG_DBStruct, one, "pgnew_setSmthCls:  VG_DBStruct");
	pgccfw_getAttr (element);
	smooth = element->hdr.smooth;
	closed = (element->hdr.closed == 1);
	G_FREE(element, VG_DBStruct);
    }
    else if (_currClass == CLASS_SIGMETS && _currObject == OBJ_SIGVAC) {

        if (pgvacw_getSubType () == ASHCLD_AREA) {

 	    smooth = 0;
	    closed = TRUE;
        }
        else {
 	    smooth = 0;
	    closed = FALSE;
        }
    }
    else if (_currClass == CLASS_SIGMETS && 
	     pgsigw_getSubType () == SIGTYP_AREA) {
 	smooth = 0;
	closed = TRUE;
    }
    else if ( _currClass == CLASS_MET && _currObject == OBJ_JET ) {
	G_CALLOC(element, VG_DBStruct, one, "pgnew_setSmthCls:  VG_DBStruct");
	pgobj_getId ( _currClass, _currObject, &elmid, &gemtyp, &subtyp );
	element->hdr.vg_type = elmid;

	element->hdr.vg_class = CLASS_MET;
	ces_get ( subtyp, element, &ier );
 	
	smooth = element->hdr.smooth;
	closed = FALSE;
	G_FREE(element, VG_DBStruct);
    }
    else if ( _currClass == CLASS_MET && _currObject == OBJ_GFA
              && pggfaw_isClosed() ) {
	G_CALLOC(element, VG_DBStruct, one, "pgnew_setSmthCls:  VG_DBStruct");
	pggfaw_getAttr ( element );
	smooth = element->hdr.smooth;
	closed = TRUE;    
	cvg_freeElPtr( element );
	G_FREE(element, VG_DBStruct);
    }
    else if ( _currClass == CLASS_MET && _currObject == OBJ_GFA_P
              && pggfawp_isClosed() ) {
	G_CALLOC(element, VG_DBStruct, one, "pgnew_setSmthCls:  VG_DBStruct");
	pggfawp_getAttr ( element );
	smooth = element->hdr.smooth;
	closed = TRUE;    
	cvg_freeElPtr( element );
	G_FREE(element, VG_DBStruct);
    }
    else {
 	smooth = 0;
	closed = FALSE;
    }
    pggst_setLineAttr (smooth, closed);
}

/*=====================================================================*/

void pgnew_getGrpInfo ( char *grptyp, int *grpnum )
/************************************************************************
 * pgnew_getGrpInfo                                                     *
 *                                                                      *
 * Retrieve the group type and number from the current attribute edit	*
 * window, or the previous grptyp and grpnum for a label.		*
 *									*
 * void pgnew_getGrpInfo ( grptyp, grpnum )				*
 *									*
 * Input parameters							*
 * Output parameters							*
 *	*grptyp		char	group type number			*
 *	*grpnum		int	group number				*
 **									*
 * Log:									*
 * E. Safford/GSC	04/01	initial coding				*
 * H. Zeng/EAI          09/01   revised for new GROUP functionality     *
 * M. Li/SAIC		04/02	Added pglabel_getLabelPending		*
 * J. Wu/SAIC		08/02	update for labeling line with symbols	*
 * H. Zeng/XTRIA	07/03   added volcano element type		*
 * H. Zeng/XTRIA	09/03   added volcanic ash cloud type		*
 ***********************************************************************/
{
int		ier;
static char	type;
static int	num;
/*---------------------------------------------------------------------*/
/*
 *  On a label function, return the stored type and num.  
 *  For all other situations, query the current element attribute 
 *  window.
 */
    if ( pglabel_getLabelPending() && 
         ( _currClass == CLASS_TEXT || 
	   _currClass == CLASS_COMSYM || 
	   _currClass == CLASS_MARKER ||
	   (_currClass == CLASS_SYMBOLS && 
	    pglabel_getLabType() == 2 ) ) ) {
	*grptyp = type;
	*grpnum = num;
    }
    else {	

/*
 *  Default condition is no grptyp and no grpnum.
 */
        *grptyp = 0;
        *grpnum = 0;

/*
 *  By type, query the element attribute window for a grptyp.
 */
        switch ( _currClass ) {

	    case CLASS_LINES:
	        *grptyp = pgline_getGrptyp(); 
	        break;

	    case CLASS_FRONTS:
		*grptyp = pgfrtw_getGrptyp();
	        break;

	    case CLASS_SYMBOLS:
		*grptyp = pgsymb_getGrptyp();
	        break;

	    case CLASS_CIRCLE:
  		*grptyp = pgcirc_getGrptyp(); 
	        break;

	    case CLASS_SIGMETS:

	        if ( _currObject == OBJ_SIGVOL ) {

  		     *grptyp = pgvolw_getGrptyp(); 
		}
		else if ( _currObject == OBJ_SIGVAC ) {

  		     *grptyp = pgvacw_getGrptyp(); 
		}

	        break;

	    default:
	        break;
        }

	if ( *grptyp > 0 ) {
	    crg_ggnxt ( *grptyp, grpnum, &ier );
	    if ( ier < 0 ) {
	        *grpnum = 0;
	        *grptyp = 0;
	    }
	}

/*
 * In the end, if GROUP is active, the above will be overriden
 * and grptyp&grpnum will take currently active ones.
 */
        if ( pgpalw_isGrpActv() ) {
             *grptyp = (char)pggrpw_getGrpType();
             *grpnum = pggrpw_getGrpNum();
        }
    }

/*
 *  Save the grptyp and grpnum for a possible label.
 */
    type = *grptyp;
    num  = *grpnum;
}

/*=====================================================================*/

void pgnew_setGrpInfo ( char grptyp, int grpnum )
/************************************************************************
 * pgnew_setGrpInfo                                                     *
 *                                                                      *
 * Set the group type and number for a label.				*
 *                                                                      *
 * void pgnew_setGrpInfo ( grptyp, grpnum )                      	*
 *                                                                      *
 * Input parameters                                                     *
 *      grptyp         char    group type number                      	*
 *      grpnum         int     group number                            	*
 * Output parameters                                                    *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           06/02  						* 
 ***********************************************************************/
{
    _labelGrptyp = grptyp;
    _labelGrpnum = grpnum;
}
