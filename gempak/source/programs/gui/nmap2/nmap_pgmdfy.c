#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "hints.h"
#include "proto_xw.h"

#define  THRESHOLD    0.0001

static int	_maxAllowPts = MAXPTS;	        /* functional limit */

static float	*_dcX, *_dcY;
static int	_dcN;

static int      _newN = 0;                      /* new element total pts */
static float    _newX[MAXPTS], _newY[MAXPTS];	/* new element coordinates */

static int	_clickPt = 0;		/* click points */
static float    _clickX[MAXPTS], _clickY[MAXPTS];	
                                        /* click pts coordinates */

static int	_startPt, _endPt;

static int	_smoothLvl;
static int	_closedFig;

/*
 *  private functions
 */
void pgmdfy_modifyDone ( void );
void pgmdfy_modifyDragEh ( Widget, XtPointer, XEvent*, Boolean* );
void pgmdfy_updateGhost ( Boolean first_time );


/************************************************************************
 * nmap_pgmdfy.c                                                        *
 *                                                                      *
 * This module contains the callback functions that are used in the     *
 * the modify operation.                                                *
 *                                                                      *
 * CONTENTS:                                                            *
 * pgmdfy_modifyStartEh()    'MODIFY' state -- start (mouse press)      *
 *									*
 * pgmdfy_updateGhost()	   update the array of ghost points		*
 * pgmdfy_modifyDragEh()    'MODIFY' state -- drag  (motion)            *
 * pgmdfy_modifyDone()    save the modification                         *
 ***********************************************************************/

/*=====================================================================*/
/* ARGSUSED */
void pgmdfy_modifyStartEh ( Widget w, XtPointer clnt, XEvent *event,
						Boolean *ctdr )
/************************************************************************
 * pgmdfy_modifyStartEh                                                 *
 *                                                                      *
 * Set up for MODIFY operation.   	                                *
 *                                                                      *
 * void pgmdfy_modifyStartEh ( w, clnt, event, ctdr )                 *
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget          Calling widget                          *
 *  clnt	XtPointer                                               *
 *  event       XEvent *                                                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC      11/97   copied from  _pgevt_mvcpStart           *
 *  E. Safford/GSC      02/98   added check on MAXPTS                   *
 *  S. Law/GSC          05/98   added call to pgevt_unsetOper           *
 *  G. Krueger/EAI	06/98	Uniform status hints			*
 *  E. Safford/GSC	07/98	change ghosting				*
 *  G. Krueger/EAI	09/98	Added ghost veiling			*
 *  G. Krueger/EAI	10/98	Using table for hints			*
 *  S. Law/GSC		03/99	cleanup					*
 *  S. Law/GSC		08/99	MAX_ALLOW_PTS -> _maxAllowPts		*
 *  S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 *  E. Safford/GSC	10/99	update for new xwcmn.h			*
 *  H. Zeng/EAI         04/00   changed cursor name                     *
 *  S. Law/GSC		06/00	changed to use xgtoff			*
 *  H. Zeng/EAI         09/00   rewrote for new cv_mdfy()               *
 *  J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 *  E. Safford/GSC	03/01	use mcanvw_getDynActFlag not local flag *
 *  J. Wu/GSC		03/01	added "EXIT" hint to bottom panel       *
 *  T. Lee/SAIC		11/03   added user directory to work_file       *
 *  T. Lee/SAIC		11/03	used cvg_getworkfile			*
 *  J. Wu/GSC		10/04	free TCA/GFA memory      		*
 ***********************************************************************/
{
    int		    ier, location, xoff, yoff;
    float	    xx, yy;
    char	    mesg[128];
    VG_DBStruct	    el;
    Boolean	    mod_active;
/*---------------------------------------------------------------------*/

    mod_active = mcanvw_getDynActFlag();

/*
 * Initialization before modifying
 */
    if( !mod_active ) {
  
        location = pgactv_getElmLoc();
        cvg_rdrec(cvg_getworkfile(), location, &el, &ier);

        _smoothLvl = (int) el.hdr.smooth;
        _closedFig = (el.hdr.closed)? 1 : 0; 

        if (_closedFig == 1) {
	    pgactv_addClosedPt();
        }
        pgactv_getDevPts (&_dcN, &_dcX, &_dcY);

       _maxAllowPts = (pgpalw_getCurClassId () == CLASS_SIGMETS) ?
	              (MAX_SIGMET) : (MAXPTS);
       _clickPt = 0;
       _newN    = 0;
       pggst_veilGhost (FALSE);

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

    xgtoff (&xoff, &yoff, &ier);

    if (_newN >= _maxAllowPts) {

/*
 * Popup a warning window if the max pts is reached.
 */
        pgevt_unsetOper (FALSE);	    
        mcanvw_setDynActFlag(FALSE);
        mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);

        strcpy(mesg, "Maximum vertices exceeded");
	NxmWarn_show(w, mesg);
	mbotw_mouseSet(LMHINT_ERROR, MMHINT_DONE);
    }
    else {

/*
 * Identify which button is pressed and handle the event accordingly.
 */
        if (event->xbutton.button == Button1) {
	    xx = (float) (event->xbutton.x + xoff); 
	    yy = (float) (event->xbutton.y + yoff); 
            _clickX[_clickPt] = xx;
            _clickY[_clickPt] = yy;
            _clickPt++;

            cv_mdfy(&_dcN, _dcX, _dcY, &_clickPt, _clickX, _clickY,
                    &_smoothLvl, &_closedFig, &_maxAllowPts, &_newN, 
                     _newX, _newY, &_startPt, &_endPt, &ier );

            if(_clickPt == 1) {

/*
 * Draw the ghost line for the first time and set the dynamic drag function.
 */
	       pgmdfy_updateGhost(TRUE);
               mcanvw_setDragFunc((XtEventHandler)&pgmdfy_modifyDragEh, CURS_DEFAULT);
               mcanvw_setDynActFlag(TRUE);

            }
	    pgmdfy_updateGhost(FALSE);
 	    mbotw_mouseSet(LMHINT_NEXT, MMHINT_DONE);

	}
        else {	    /*  Mouse button != Button1  */

            if (mcanvw_getDynActFlag()) {
                mcanvw_disarmPress();
                mcanvw_disarmDrag();
                mcanvw_setDynActFlag(FALSE);

	        pgmdfy_modifyDone();
		mcanvw_setPressFunc((XtEventHandler)&pgmdfy_modifyStartEh, CURS_DEFAULT); 
	        mbotw_mouseSet(LMHINT_START, MMHINT_DONE);
            }
            else {
                pgevt_unsetOper (FALSE);	    
                mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);
	        mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
            }
	}
    }
}

/*=====================================================================*/

void pgmdfy_updateGhost ( Boolean first_time )
/************************************************************************
 * pgmdfy_updateGhost                                                   *
 *                                                                      *
 * Update the array of ghost points in response to either a drag or     *
 * drop action.								*
 *                                                                      *
 * void pgmdfy_updateGhost(first_time)                                  *
 *                                                                      *
 * Input Parameters:                                                    *
 *        first_time    Boolean   if first time drawing ghost line?     *
 * Output Parameters:                                                   *
 * Return Parameters:                                                   *
 *                      None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	07/98	initial coding                          *
 * S. Law/GSC		03/99	cleanup					*
 * E. Safford/GSC	03/99	check _modDirSet for both closed & open	*
 * H. Zeng/EAI          09/00   rewrote for new cv_mdfy()               *
 * H. Zeng/EAI          11/00   made ghost line closed for closed elem. *
 ***********************************************************************/
{
float    *temp_x, *temp_y;
int       temp_point, ii, iret;
/*---------------------------------------------------------------------*/

    if( !first_time ) {

/*
 *  Erase previous ghost line
 */
       pggst_drawGhost (GST_NORMAL);

    }

/*
 * Set up new ghost points and attributes.
 */
    pggst_clearGhost(TRUE);

    if(_closedFig == 0) {
       
/*
 * For non-closed element.
 */
      temp_point = _endPt + 1 - _startPt;
  
      temp_x = (float*)malloc(temp_point*sizeof(float));
      temp_y = (float*)malloc(temp_point*sizeof(float));
   
      for(ii = 0; ii < temp_point; ii++) {
          temp_x[ii] = _newX[_startPt+ii];
          temp_y[ii] = _newY[_startPt+ii];
      }
      pggst_setLineAttr((char)_smoothLvl, FALSE);
      pggst_addGhostPts(temp_point, temp_x, temp_y, &iret);
      free(temp_x);
      free(temp_y);
  
      pggst_drawGhost (GST_NORMAL);

    }
    else {

/*
 * For closed element.
 */
      temp_point = _endPt + 1 - _startPt;
      if(temp_point <= 0) {
         temp_point = temp_point + _newN;
      } 

      temp_x = (float*)malloc(temp_point*sizeof(float));
      temp_y = (float*)malloc(temp_point*sizeof(float));
   
      for(ii = 0; ii < temp_point; ii++) {
          temp_x[ii] = _newX[(_startPt+ii)%(_newN)];
          temp_y[ii] = _newY[(_startPt+ii)%(_newN)];
      }

/*
 * If we are ghosting the whole new line, make the ghost line
 * closed, otherwise make it open.
 */
      if( temp_point == _newN ) {
        pggst_setLineAttr((char)_smoothLvl, TRUE);
      }
      else {
        pggst_setLineAttr((char)_smoothLvl, FALSE);
      }

      pggst_addGhostPts(temp_point, temp_x, temp_y, &iret);
      free(temp_x);
      free(temp_y);
      
      pggst_drawGhost (GST_NORMAL);

    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgmdfy_modifyDragEh ( Widget w, XtPointer clnt, XEvent *event,
						Boolean *ctdr )
/************************************************************************
 * pgmdfy_modifyDragEh                                                  *
 *                                                                      *
 * Callback for dragging with the mouse                                 *
 *                                                                      *
 * void pgmdfy_modifyDragEh ( w, clnt, event, ctdr )			*
 *                                                                      *
 * Input parameters:                                                    *
 *  w           Widget		Widget that activated the callback      *
 *  clnt	XtPointer	Pointer to client data (graphics info)  *
 *  *event      XEvent		Event that triggered the callback       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	11/97	copied from dynamicDrag			*
 * E. Safford/GSC	01/98	add initial hi-lite for lines/fronts	*
 * E. Safford/GSC	02/98	fixed offsets for hi-lites		*
 * E. Safford/GSC	04/98	removed switch on class			*
 * E. Safford/GSC	07/98	using pgmdfy_updateGhost now		*
 * E. Safford/GSC	10/99	update for new xwcmn.h			*
 * S. Law/GSC		06/00	changed to use xgtoff			*
 * H. Zeng/EAI          09/00   rewrote for new cv_mdfy()               *
 ***********************************************************************/
{
    int		ier, xoff, yoff, ghost_point;
    float	xx, yy, dis_x, dis_y;
/*---------------------------------------------------------------------*/

    if (mcanvw_getDynActFlag()) {

/*
 * Get current cursor position.
 */ 
	xgtoff (&xoff, &yoff, &ier);
	xx = (float) (event->xbutton.x + xoff); 
	yy = (float) (event->xbutton.y + yoff); 

/*
 * If the current position is close enough to the last click
 * point position, ignore it.
 */
        dis_x = xx - _clickX[_clickPt-1];
        dis_y = yy - _clickY[_clickPt-1];
        if( fabs((double)dis_x) < THRESHOLD  &&
            fabs((double)dis_y) < THRESHOLD      ) {
            
            return;
        }     


        _clickX[_clickPt] = xx;
        _clickY[_clickPt] = yy;
        ghost_point = _clickPt + 1;

        cv_mdfy(&_dcN, _dcX, _dcY, &ghost_point, _clickX, _clickY,
                &_smoothLvl, &_closedFig, &_maxAllowPts, &_newN, 
                 _newX, _newY, &_startPt, &_endPt, &ier );

	pgmdfy_updateGhost(FALSE);

    }
}

/*=====================================================================*/

void pgmdfy_modifyDone ( void )
/************************************************************************
 * pgmdfy_modifyDone                                                    *
 *                                                                      *
 * This function saves the modification.                                *
 *                                                                      *
 * void pgmdfy_modifyDone()                                             *
 *                                                                      *
 * Input  parameters:                                                   *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       11/97   copied from _pgevt_mvcpDrop             *
 * E. Safford/GSC       12/97   changed handling of closed lines        *
 * E. Safford/GSC       01/98   added ability to drop the end of lines  *
 * E. Safford/GSC       01/98   fixed end of line/front bug & cleanup   *
 * E. Safford/GSC       01/98   added end to end drawing capability     *
 * E. Safford/GSC       02/98   added undo capability                   *
 * E. Safford/GSC       02/98   added MAX_ALLOW_PTS check and cleanup   *
 * E. Safford/GSC       03/98   mod to limit closed line conversion     *
 * E. Safford/GSC       05/98   update undo                             *
 * E. Safford/GSC	06/98	fixed existing vert proximity calc err  *
 * G. Krueger/EAI	06/98	Uniform status hints			*
 * E. Safford/GSC	07/98	changed ghosting                        *
 * E. Safford/GSC	10/98	mod for param change to cvg_rdrec       *
 * G. Krueger/EAI	10/98	Using table for hints			*
 * S. Law/GSC		09/99	changed to use _allowClose, changed	*
 *				pgmdfy_getNewPts parameters		*
 * S. Law/GSC		09/99	added parameter to mcanvw_set* functions*
 * S. Law/GSC		02/00	added CCF				*
 * S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 * H. Zeng/EAI          04/00   changed cursor name                     *
 * H. Zeng/EAI          09/00   rewrote and renamed for new cv_mdfy()   *
 * E. Safford/GSC	10/00	no save if line has not changed         *
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * T. Lee/SAIC		10/04	free TCA/GFA memory			*
 * B. Yin/SAIC		02/05	add a call to snap GFAs			*
 * E. Safford/SAIC	06/05	allow smear to get smaller on edit	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * E. Safford/SAIC	01/07	add call to pggfaw_reorderGFA()		*
 * S. Danz/AWC          02/07   Add logic to update GFA centroid 	*
 ***********************************************************************/
{
    int         location, ier, areaType, ii, one = 1;
    char	tagStr[ 64 ];

    float       llx, lly, urx, ury, *tmpLat, *tmpLon;
    float       x_cntr, y_cntr, c_lat, c_lon, area;
    Boolean	badGfa = False;
    VG_DBStruct el;
/*---------------------------------------------------------------------*/

    location = pgactv_getElmLoc();
    cvg_rdrec (cvg_getworkfile(), location, &el, &ier);
    
/*
 * Free TCA/GFA memory
 */
    if ( el.hdr.vg_type == TCA_ELM ) {
        cvg_freeBkpts ( &el );
    }
    else if ( el.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &el );
    }

    if (_clickPt > 1) {

/*
 * Save modification.
 */
        pgundo_newStep();
        pgundo_storeThisLoc(location, UNDO_DEL, &ier);

        pgutls_prepNew (location, &el, &llx, &lly, &urx, &ury, &ier);
        cv_mdfy(&_dcN, _dcX, _dcY, &_clickPt, _clickX, _clickY,
                &_smoothLvl, &_closedFig, &_maxAllowPts, &_newN, 
                 _newX, _newY, &_startPt, &_endPt, &ier );

 	if ( el.hdr.vg_type == GFA_ELM ) {

	   el.elem.gfa.info.npts = _newN;

/*
 *  Ensure point order is CW (except FZLVLs).
 */
	   G_MALLOC( tmpLat, float, _newN, "PGMDFY_MODIFYDONE: tmpLat" );
	   G_MALLOC( tmpLon, float, _newN, "PGMDFY_MODIFYDONE: tmpLon" );

	   gtrans ( sys_D, sys_M, &_newN, _newX, _newY, tmpLat, tmpLon,
		    &ier, strlen(sys_D), strlen(sys_M) );

	   cvg_getFld ( &el, TAG_GFA_AREATYPE, tagStr, &ier );
           areaType =  pggfaw_getHazardType ( tagStr );

	   if( ier >= 0 ) {
               pggfaw_reorderGFA ( areaType, _newN, tmpLat, tmpLon, &ier );

               for (ii = 0; ii < _newN; ii++) {
                   el.elem.gfa.latlon[ii]       = tmpLat[ii];
                   el.elem.gfa.latlon[ii+_newN] = tmpLon[ii];
	       }
	   }

	   G_FREE( tmpLat, float );
	   G_FREE( tmpLon, float );

	   pgsmear_snapEl ( FALSE, &el, &ier );

	   _newN = el.elem.gfa.info.npts;
        
            
	   cvg_todev( &el, &_newN, _newX, _newY, &ier );

	   if ( pggfaw_isClosed() ) {
	       cgr_centroid( _newX, _newY, &_newN, &x_cntr, &y_cntr, &area, &ier );
	   } else {
	       x_cntr = _newX[ 0 ];
	       y_cntr = _newY[ 0 ];
	   }
	   gtrans( sys_D, sys_M, &one, &x_cntr, &y_cntr, &c_lat, &c_lon, &ier, 
		   strlen(sys_D), strlen(sys_M) 
		   );

	   sprintf ( tagStr, "%7.2f", c_lat );
	   cvg_setFld ( &el, TAG_GFA_ARROW_LAT, tagStr, &ier );
	   sprintf ( tagStr, "%7.2f", c_lon );
	   cvg_setFld ( &el, TAG_GFA_ARROW_LON, tagStr, &ier );

	}

/*
 * Update the element if more than 2 pts are clicked.
 */
        pgvgf_saveNewElm (NULL, sys_D, &el, _newN, _newX, _newY,
		         TRUE, &location, &ier);

	if ( ier < 0 ) {
	   badGfa = True;
 	}
	else {
           pgundo_storeThisLoc (location, UNDO_ADD, &ier);
           pgundo_endStep();
        }
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

    if ( !badGfa ) {
       pgutls_redraw(location, &el, &ier);
       pgactv_setActvElm (&el, location);
    }
    
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
