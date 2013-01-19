#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "hints.h"
#include "proto_xw.h"


#define         MAX_UNDO_STEPS  10

/*
 *   The locations of the elements that are potentially to be returned to 
 *   the display on an UNDO operation are stored in an array _undoDelItms.  
 *   The variable _undoDelItmsNum indicates the current number of elements 
 *   in that array.
 *
 *   The locations of the elements presently displayed to the user's screen 
 *   that will be removed in an UNDO operation are tracked by the array 
 *   _undoAddItms. The variable _undoAddItmsNum indicates the current number 
 *   of elements in that array.
 */

static int	_undoAddItms[MAX_UNDO_STEPS][MAX_EDITABLE_ELEMS];
static int      _undoDelItms[MAX_UNDO_STEPS][MAX_EDITABLE_ELEMS];   
static int	_undoAddItmsNum[MAX_UNDO_STEPS], 
                _undoDelItmsNum[MAX_UNDO_STEPS];
static int      _curStep, _totalUndoSteps, _totalRedoSteps;


/*
 * private functions
 */
static void pgundo_updtRfrshArea ( float llx, float lly, float urx, 
				   float ury, float *m_llx, float *m_lly,
				   float *m_urx, float *m_ury );


/************************************************************************
 * nmap_pgundo.c                                                        *
 *                                                                      *
 * This module contains the functions related to "undoing" product      *
 * generation operations.                                               *
 *                                                                      *
 * CONTENTS:                                                            *
 *									*
 * pgundo_initUndo()       initialize undo variables & file             *
 * pgundo_newStep()        increase _curStep by 1                       *
 * pgundo_endStep()        update undo info. upon completion            *
 * pgundo_storeThisLoc()   write location of displayed elem to array	*
 * pgundo_undo()           undo function                                *
 * pgundo_redo()           redo function                                *
 * pgundo_undoIsActv()     check if UNDO is active                      *
 * pgundo_redoIsActv()     check if REDO is active                      *
 * pgundo_getCurStep()     returns the current step             	*
 *									*
 * pgundo_updtRfrshArea    update the max/min coord values for elements	*
 ***********************************************************************/

/*=====================================================================*/

void pgundo_initUndo ( void )
/************************************************************************
 * pgundo_initUndo                                                      *
 *                                                                      *
 * Initialize undo related variables                                    *
 *                                                                      *
 * void pgundo_initUndo   ( )                                           *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *  None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC      02/98   initial coding                          *
 *  E. Safford/GSC      05/98   move to nmap_pgundo add UNDO_FILE       *
 *  E. Safford/GSC      09/98   change undo toggle                      *
 *  G. Krueger/EAI	05/99	Cleanup unused variables		*
 *  H. Zeng/EAI         11/00   rewrote for the new undo design         *
 *  H. Zeng/EAI         12/00   modified for multiple undo steps        *
 ***********************************************************************/
{
int	ii, undo_step;
/*---------------------------------------------------------------------*/
/*
 *  Clear location array
 */
   for(undo_step = 0; undo_step < MAX_UNDO_STEPS; undo_step++) {

      for (ii=0; ii< MAX_EDITABLE_ELEMS; ii++) {
	   _undoAddItms[undo_step][ii] = -1;
           _undoDelItms[undo_step][ii] = -1;
      }
    
      _undoAddItmsNum[undo_step] = 0;
      _undoDelItmsNum[undo_step] = 0;

   }

/*
 * Clear undo steps
 */
   _curStep = IMISSD;
   _totalUndoSteps = 0;
   _totalRedoSteps = 0;
   
   pgfrom_clearUndoStep( );
   
/*
 * Make GUI adjustments
 */
   pgpalw_setBtnSntv(FUNC_UNDO, FALSE);
   pgpalw_setBtnSntv(FUNC_REDO, FALSE);
}

/*=====================================================================*/

void pgundo_newStep ( void )
/************************************************************************
 * pgundo_newStep                                                       *
 *                                                                      *
 * Increase _curStep by 1.                                              *
 *                                                                      *
 * void pgundo_newStep()                                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *  None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  H. Zeng/EAI         12/00   initial coding                          *
 *  J. Wu/SAIC		09/07   erase previously_saved step in "FROM"	*
 ***********************************************************************/
{
int     ii, fromStep;
/*---------------------------------------------------------------------*/
/*
 * Take a step forward
 */
   if ( _curStep == IMISSD ) {
       _curStep = 0;
   }
   else {
      
       fromStep = pgfrom_getUndoStep();
       if ( fromStep == IMISSD || fromStep != _curStep )  {
           _curStep = (_curStep + 1) % MAX_UNDO_STEPS;
       }
       else {
           _totalUndoSteps -= 1;     
       }
   } 

/*
 * Clear location array
 */
   if (_undoAddItmsNum[_curStep] > 0) { 
        ii = 0;
        while (_undoAddItms[_curStep][ii] != -1) {
	    _undoAddItms[_curStep][ii] = -1;
	    ii++;
        }
        _undoAddItmsNum[_curStep] = 0;
   }

   if (_undoDelItmsNum[_curStep] > 0) {
        ii = 0;
        while (_undoDelItms[_curStep][ii] != -1) {
	    _undoDelItms[_curStep][ii] = -1;
	    ii++;
        }
        _undoDelItmsNum[_curStep] = 0;
   }   

   pgfrom_clearUndoStep( );
}

/*=====================================================================*/

void pgundo_endStep ( void )
/************************************************************************
 * pgundo_endStep                                                       *
 *                                                                      *
 * Update undo step info. upon completion of each undo step.            *
 *                                                                      *
 * void pgundo_endStep ()                                               *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *  None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  H. Zeng/EAI         12/00   initial coding                          *
 ***********************************************************************/
{
   _totalUndoSteps += 1;
   if(_totalUndoSteps > MAX_UNDO_STEPS) {
      _totalUndoSteps = MAX_UNDO_STEPS;
   }

   _totalRedoSteps = 0;
   pgpalw_setBtnSntv (FUNC_REDO, FALSE);

/*
 * Enable UNDO button
 */
   pgpalw_setBtnSntv (FUNC_UNDO, TRUE);
   pgsymb_setUnRedo(TRUE);
}

/*=====================================================================*/

void pgundo_storeThisLoc ( int loc, int purpose, int *iret )
/************************************************************************
 * pgundo_storeThisLoc                                                  *
 *                                                                      *
 * Store this location (of a vgf element) for possible undo.            *
 *                                                                      *
 * void pgundo_storeThisLoc ( loc, purpose, iret )                      *
 *                                                                      *
 * Input parameters:                                                    *
 *  loc		int		element to record                       *
 *  purpose     int             want to add it or delete it?            *
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret	int    	return code 	                                *
 *				  -1 = reached max number of elems	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC      05/98   initial coding                          *
 *  G. Krueger/EAI	05/99	Cleanup prologue			*
 *  H. Zeng/EAI         11/00   rewrote for the new undo design         *
 *  H. Zeng/EAI         12/00   modified for multiple undo steps        *
 ***********************************************************************/
{
   int  next_item; 
/*---------------------------------------------------------------------*/

    switch(purpose) {
       case UNDO_DEL:

          if (_undoDelItmsNum[_curStep] < MAX_EDITABLE_ELEMS -1) {
              *iret = 0;
              next_item = _undoDelItmsNum[_curStep];
              _undoDelItms[_curStep][next_item] = loc;
              _undoDelItmsNum[_curStep]++;
          }
          else {
              *iret = -1;
          }
          break;

       case UNDO_ADD:

          if (_undoAddItmsNum[_curStep] < MAX_EDITABLE_ELEMS -1) {
              *iret = 0;
              next_item = _undoAddItmsNum[_curStep];
              _undoAddItms[_curStep][next_item] = loc;
              _undoAddItmsNum[_curStep]++;
          }
          else {
              *iret = -1;
          }
          break;
    } /* the end of switch */
}

/*=====================================================================*/

void pgundo_undo ( void )
/************************************************************************
 * pgundo_undo                                                          *
 *                                                                      *
 * Undo function "undoes" the last save operation.              	*
 *                                                                      *
 * void pgundo_undo  ( )                                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *  None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC      01/98   initial coding                          *
 *  E. Safford/GSC      02/98   mod to handle partial delete undo/redo  *
 *  E. Safford/GSC      03/98   mod to handle all elm types & operations*
 *  E. Safford/GSC      04/98   revise refresh scheme                   *
 *  F. J. Yen/NCEP	05/98	Changed cds_dspvg to cds_dspelm		*
 *  E. Safford/GSC	06/98	added call to crg_grfrsh		*
 *  G. Krueger/EAI	06/98	Uniform status hints			*
 *  E. Safford/GSC	09/98	unhighlight selected obj pallete item   *
 *  E. Safford/GSC	10/98	mod to use cvg_qkopen			*
 *  G. Krueger/EAI	10/98	Using table for hints			*
 *  E. Safford/GSC	12/98	limit area of refresh			*
 *  S. Law/GSC		05/00	added parameter to pgfilw_getFileName	*
 *  H. Zeng/EAI         11/00   rewrote for the new undo design         *
 *  H. Zeng/EAI         12/00   modified for multiple undo steps        *
 *  J. Wu/SAIC		12/01	add layer in crg_set() call		*
 *  J. Wu/SAIC		12/01	rebuild range record after cvg_rfrsh()	*
 *  J. Wu/SAIC		01/02	add layer in crg_get() call		*
 *  T. Lee/SAIC		11/03   added user directory to work_file       *
 *  T. Lee/SAIC		11/03	used cvg_getworkfile			*
 *  J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 *  B. Yin/SAIC		07/04	Added code to free TCA memory		*
 *  B. Yin/SAIC         08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 *  J. Wu/SAIC          10/04   free GFA block memory			*
 *  S. Danz/AWC		07/06	Added new cvg_delet and cvg_undo        *
 *                              flag to pass updates to placement       *
 *  S. Danz/AWC         08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 *  J. Wu/SAIC		09/07   Do not redo the "undo"ed step in FROM	*
 *  J. Wu/SAIC          05/08   refresh completely for GFA elements	*
 ***********************************************************************/
{
    int		tmp_cnt, ii, iret, layer, el_layer, found;
    int		num, tmp_dsply[MAX_EDITABLE_ELEMS], fromStep;
    float	llx, lly, urx, ury, m_llx, m_lly, m_urx, m_ury, inf_bbox[4];
    float	llx1, lly1, urx1, ury1;
    VG_DBStruct	el;
    filter_t	filter;
/*---------------------------------------------------------------------*/

    m_llx = 999999.0F;
    m_lly = 999999.0F;
    m_urx = 0.0F;
    m_ury = 0.0F;

/*
 * If undo is currently valid, de-highlight obj button.
 */
    if ( (_undoDelItmsNum[_curStep] != 0) ||
         (_undoAddItmsNum[_curStep] != 0)   ) {

        if(pgpalw_getCurObjId()) {
	    pgpalw_setCurBtns (-1, -1, 0);
        }
    }

    layer = pglayer_getCurLayer( );
    for (ii = 0; ii < _undoDelItmsNum[_curStep]; ii++) {
	cvg_undel(cvg_getworkfile(), _undoDelItms[_curStep][ii], TRUE, &iret);

        cvg_rdrec(cvg_getworkfile(), _undoDelItms[_curStep][ii], &el, &iret);
        crg_set (&el, _undoDelItms[_curStep][ii], layer, &iret);
	crg_getinx (_undoDelItms[_curStep][ii], &num, &iret);
	crg_get( num, &el_layer, filter, &llx, &lly, &urx, &ury, &iret);
	
	if ( iret == 0 ) {
	    
	    crg_getsecrange( num, &llx1, &lly1, &urx1, &ury1, &iret );            
	    if ( iret == 0 ) {
		llx = G_MIN( llx, llx1 );
                lly = G_MIN( lly, lly1 );
                urx = G_MAX( urx, urx1 );
                ury = G_MAX( ury, ury1 );                
	    }
	}
	
	pgundo_updtRfrshArea (llx, lly, urx, ury, 
			      &m_llx, &m_lly, &m_urx, &m_ury);

        tmp_dsply[ii] = _undoDelItms[_curStep][ii];

/*
 * Mark elements in placement that are effected by
 * the undel, and get the area of influence back
 */
        cvg_checkplace(&el, 0, _undoDelItms[_curStep][ii], &found, inf_bbox, &iret);
        if (found > 0) {
/*
 * Update the refresh extent if the area impacted by
 * placement is bigger
 */
            m_llx = G_MIN(m_llx, inf_bbox[0]);
            m_lly = G_MIN(m_lly, inf_bbox[2]);
            m_urx = G_MAX(m_urx, inf_bbox[1]);
            m_ury = G_MAX(m_ury, inf_bbox[3]);
        }
	
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
    tmp_cnt = _undoDelItmsNum[_curStep];


    for (ii = 0; ii < _undoAddItmsNum[_curStep]; ii++) {
	cvg_delet (cvg_getworkfile(), _undoAddItms[_curStep][ii], TRUE, &iret);

	crg_getinx (_undoAddItms[_curStep][ii], &num, &iret);
	crg_get( num, &el_layer, filter, &llx, &lly, &urx, &ury, &iret);
	
	if ( iret == 0 ) {
	    
	    crg_getsecrange( num, &llx1, &lly1, &urx1, &ury1, &iret );            
	    if ( iret == 0 ) {
		llx = G_MIN( llx, llx1 );
                lly = G_MIN( lly, lly1 );
                urx = G_MAX( urx, urx1 );
                ury = G_MAX( ury, ury1 );                
	    }
	}
	
	pgundo_updtRfrshArea (llx, lly, urx, ury, 
			      &m_llx, &m_lly, &m_urx, &m_ury);
        crg_clear(num, &iret);

	_undoDelItms[_curStep][ii] = _undoAddItms[_curStep][ii];

/*
 * Mark elements in placement that are effected by
 * the delete, and get the area of influence back
 */
        cvg_rdrec(cvg_getworkfile(), _undoAddItms[_curStep][ii], &el, &iret);
        cvg_checkplace(&el, 1, _undoAddItms[_curStep][ii], &found, inf_bbox, &iret);
        if (found > 0) {
/*
 * Update the refresh extent if the area impacted by
 * placement is bigger
 */
            m_llx = G_MIN(m_llx, inf_bbox[0]);
            m_lly = G_MIN(m_lly, inf_bbox[2]);
            m_urx = G_MAX(m_urx, inf_bbox[1]);
            m_ury = G_MAX(m_ury, inf_bbox[3]);
        }
	
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
    while( ii < _undoDelItmsNum[_curStep] ) {
        _undoDelItms[_curStep][ii] = -1;
        ii++;
    }
    _undoDelItmsNum[_curStep] = _undoAddItmsNum[_curStep];

/*
 * transfer tmp_dsply to _undoAddItms[_curStep] and fill in -1 for 
 * unused records. 
 */
    for (ii = 0; ii < tmp_cnt; ii++) {
	_undoAddItms[_curStep][ii] = tmp_dsply[ii];
    }
    while( ii < _undoAddItmsNum[_curStep] ) {
        _undoAddItms[_curStep][ii] = -1;
        ii++;
    }
    _undoAddItmsNum[_curStep] = tmp_cnt;

/*
 * Refresh the drawing area.
 */
    xpgpaste(m_llx, m_lly, m_urx, m_ury, &iret);
    cvg_rfrsh(cvg_getworkfile(), m_llx, m_lly, m_urx, m_ury, &iret);
    crg_rebuild();
        
/*
 * Go back to the previous undo step.
 */
    _totalUndoSteps -= 1;
    if(_totalUndoSteps == 0) {
      pgpalw_setBtnSntv (FUNC_UNDO, FALSE);
    }
    
    fromStep = pgfrom_getUndoStep();
    if ( fromStep == IMISSD || fromStep != _curStep ) {
        _totalRedoSteps += 1;
        pgpalw_setBtnSntv (FUNC_REDO, TRUE);
    }
    else {
        pgfrom_clearUndoStep( );
    }
    
    _curStep -= 1;
    if(_curStep < 0) {
       _curStep += MAX_UNDO_STEPS;
    }

    mbotw_actionClear();
    mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
}

/*=====================================================================*/

void pgundo_redo ( void )
/************************************************************************
 * pgundo_redo                                                          *
 *                                                                      *
 * Redo function "undoes" the last undo action.          	        *
 *                                                                      *
 * void pgundo_redo  ( )                                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *  None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  H. Zeng/EAI         12/00   initial coding                          *
 *  J. Wu/SAIC		12/01	add layer in crg_set() call		*
 *  J. Wu/SAIC		01/02	add layer in crg_get() call		*
 *  T. Lee/SAIC		11/03   added user directory to work_file       *
 *  T. Lee/SAIC		11/03	used cvg_getworkfile			*
 *  J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 *  B. Yin/SAIC		07/04	Added code to free TCA memory		*
 *  B. Yin/SAIC         08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 *  J. Wu/SAIC          10/04   free GFA block memory			*
 *  S. Danz/AWC		07/06	Added new cvg_delet and cvg_undo        *
 *                              flag to pass updates to placement       *
 *  S. Danz/AWC         08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 *  J. Wu/SAIC          05/08   refresh completely for GFA elements	*
 ***********************************************************************/
{
    int		tmp_cnt, ii, iret, layer, el_layer, found;
    int		num, tmp_dsply[MAX_EDITABLE_ELEMS];
    float	llx, lly, urx, ury, m_llx, m_lly, m_urx, m_ury, inf_bbox[4];
    float	llx1, lly1, urx1, ury1;
    VG_DBStruct	el;
    filter_t	filter;
/*---------------------------------------------------------------------*/
/*
 * Move to the next undo step.
 */
    _totalRedoSteps -= 1;
    if(_totalRedoSteps == 0) {
        pgpalw_setBtnSntv (FUNC_REDO, FALSE);
    }
    
    _totalUndoSteps += 1;
    pgpalw_setBtnSntv (FUNC_UNDO, TRUE);

    _curStep += 1;
    if(_curStep >= MAX_UNDO_STEPS) {
       _curStep -= MAX_UNDO_STEPS;
    }

    m_llx = 999999.0F;
    m_lly = 999999.0F;
    m_urx = 0.0F;
    m_ury = 0.0F;

/*
 * If undo is currently valid, de-highlight obj button.
 */
    if ( (_undoDelItmsNum[_curStep] != 0) ||
         (_undoAddItmsNum[_curStep] != 0)   ) {

        if(pgpalw_getCurObjId()) {
	    pgpalw_setCurBtns (-1, -1, 0);
        }
    }

    layer = pglayer_getCurLayer( );
    for (ii = 0; ii < _undoDelItmsNum[_curStep]; ii++) {
	cvg_undel(cvg_getworkfile(), _undoDelItms[_curStep][ii], TRUE, &iret);

        cvg_rdrec(cvg_getworkfile(), _undoDelItms[_curStep][ii], &el, &iret);
        crg_set (&el, _undoDelItms[_curStep][ii], layer, &iret);
	crg_getinx (_undoDelItms[_curStep][ii], &num, &iret);
	crg_get( num, &el_layer, filter, &llx, &lly, &urx, &ury, &iret);
	
	if ( iret == 0 ) {
	    
	    crg_getsecrange( num, &llx1, &lly1, &urx1, &ury1, &iret );            
	    if ( iret == 0 ) {
		llx = G_MIN( llx, llx1 );
                lly = G_MIN( lly, lly1 );
                urx = G_MAX( urx, urx1 );
                ury = G_MAX( ury, ury1 );                
	    }
	}

	
	pgundo_updtRfrshArea (llx, lly, urx, ury, 
			      &m_llx, &m_lly, &m_urx, &m_ury);

        tmp_dsply[ii] = _undoDelItms[_curStep][ii];

/*
 * Mark elements in placement that are effected by
 * the change, and get the area of influence back
 */
        cvg_checkplace(&el, 0, _undoDelItms[_curStep][ii], &found, inf_bbox, &iret);
        if (found > 0) {
/*
 * Update the refresh extent if the area impacted by
 * placement is bigger
 */
            m_llx = G_MIN(m_llx, inf_bbox[0]);
            m_lly = G_MIN(m_lly, inf_bbox[2]);
            m_urx = G_MAX(m_urx, inf_bbox[1]);
            m_ury = G_MAX(m_ury, inf_bbox[3]);
        }
	
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
    tmp_cnt = _undoDelItmsNum[_curStep];


    for (ii = 0; ii < _undoAddItmsNum[_curStep]; ii++) {
/*
 * Mark elements in placement that are effected by
 * the change, and get the area of influence back
 */
        cvg_rdrec(cvg_getworkfile(), _undoAddItms[_curStep][ii], &el, &iret);
        cvg_checkplace(&el, 1, _undoAddItms[_curStep][ii], &found, inf_bbox, &iret);
        if (found > 0) {
/*
 * Update the refresh extent if the area impacted by
 * placement is bigger
 */
            m_llx = G_MIN(m_llx, inf_bbox[0]);
            m_lly = G_MIN(m_lly, inf_bbox[2]);
            m_urx = G_MAX(m_urx, inf_bbox[1]);
            m_ury = G_MAX(m_ury, inf_bbox[3]);
        }
	
/*
 * Free TCA break point/GFA block memory
 */
        if ( el.hdr.vg_type == TCA_ELM ) {
           cvg_freeBkpts ( &el );
        }
        else if ( el.hdr.vg_type == GFA_ELM ) {
           cvg_freeElPtr ( &el );
        }

	cvg_delet (cvg_getworkfile(), _undoAddItms[_curStep][ii], TRUE, &iret);

	crg_getinx (_undoAddItms[_curStep][ii], &num, &iret);
	crg_get( num, &el_layer, filter, &llx, &lly, &urx, &ury, &iret);
	
	if ( iret == 0 ) {
	    
	    crg_getsecrange( num, &llx1, &lly1, &urx1, &ury1, &iret );            
	    if ( iret == 0 ) {
		llx = G_MIN( llx, llx1 );
                lly = G_MIN( lly, lly1 );
                urx = G_MAX( urx, urx1 );
                ury = G_MAX( ury, ury1 );                
	    }
	}
	
	
	pgundo_updtRfrshArea (llx, lly, urx, ury, 
			      &m_llx, &m_lly, &m_urx, &m_ury);
        crg_clear(num, &iret);

	_undoDelItms[_curStep][ii] = _undoAddItms[_curStep][ii];
    } 
    while( ii < _undoDelItmsNum[_curStep] ) {
        _undoDelItms[_curStep][ii] = -1;
        ii++;
    }
    _undoDelItmsNum[_curStep] = _undoAddItmsNum[_curStep];

/*
 * transfer tmp_dsply to _undoAddItms[_curStep] and fill in -1 for 
 * unused records. 
 */
    for (ii = 0; ii < tmp_cnt; ii++) {
	_undoAddItms[_curStep][ii] = tmp_dsply[ii];
    }
    while( ii < _undoAddItmsNum[_curStep] ) {
        _undoAddItms[_curStep][ii] = -1;
        ii++;
    }
    _undoAddItmsNum[_curStep] = tmp_cnt;

/*
 * Refresh the drawing area.
 */
    xpgpaste(m_llx, m_lly, m_urx, m_ury, &iret);
    cvg_rfrsh(cvg_getworkfile(), m_llx, m_lly, m_urx, m_ury, &iret);
    crg_rebuild();

    mbotw_actionClear();
    mbotw_mouseSet(LMHINT_NOACTION, MMHINT_NOACTION);
}

/*=====================================================================*/

static void pgundo_updtRfrshArea ( float llx, float lly, float urx, 
				   float ury, float *m_llx, float *m_lly, 
				   float *m_urx, float *m_ury )
/************************************************************************
 * pgundo_updtRfrshArea                                                 *
 *                                                                      *
 * Check the input refresh area coordinates and move the input max/min  *
 * values if the new values are outside the established max/min		*
 *                                                                      *
 * static void pgundo_updtRfrshArea  ( llx, lly, urx, ury		*
 *					m_llx, m_lly, m_urx, m_ury )    *
 *                                                                      *
 * Input parameters:                                                    *
 *   llx	float		lower left x				*
 *   lly	float		lower left y				*
 *   urx	float		upper right x				*
 *   ury	float		upper right y				*
 *									*
 * Input/Output parameters:                                             *
 *   *m_llx	float		lower left x				*
 *   *m_lly	float		lower left y				*
 *   *m_urx	float		upper right x				*
 *   *m_ury	float		upper right y				*
 **                                                                     *
 * Log:                                                                 *
 *  E. Safford/GSC      05/98   initial coding                          *
 *  G. Krueger/EAI	05/99	Cleanup prologue			*
 ***********************************************************************/
{
    if (*m_llx > llx)
        *m_llx = llx;
    if (*m_lly > lly)
        *m_lly = lly;
    if (*m_urx < urx)
        *m_urx = urx;
    if (*m_ury < ury)
        *m_ury = ury;
}

/*=====================================================================*/

Boolean pgundo_undoIsActv ( void )
/************************************************************************
 * pgundo_undoIsActv                                                    *
 *                                                                      *
 * Returns UNDO button status.                                    	*
 *                                                                      *
 * Boolean pgundo_undoIsActv  ( )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *  None								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  J. Wu/GSC      04/01   create                         		*
 ***********************************************************************/
{
   return ( _totalUndoSteps > 0 ); 
}

/*=====================================================================*/

Boolean pgundo_redoIsActv ( void )
/************************************************************************
 * pgundo_redoIsActv                                                    *
 *                                                                      *
 * Returns REDO button status                                   	*
 *                                                                      *
 * Boolean pgundo_redoIsActv  ( )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *  pgundo_redoIsActv	Boolean		Is redo activve?		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *  J. Wu/GSC      04/01   create                         		*
 ***********************************************************************/
{
    return ( _totalRedoSteps > 0 ); 
}

/*=====================================================================*/

int pgundo_getCurStep ( void )
/************************************************************************
 * pgundo_getCurStep                                                    *
 *                                                                      *
 * Returns the value of _curStep                                   	*
 *                                                                      *
 * int pgundo_getCurStep ( )                               		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *  pgundo_getCurStep	int		value of _curStep		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC      09/07   create                         		*
 ***********************************************************************/
{
    return ( _curStep ); 
}
