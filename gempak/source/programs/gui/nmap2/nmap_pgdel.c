#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "hints.h"
#include "proto_xw.h"

#define EXTRA          5.0F          /* extra distance for group delete */


static int pgdel_deleteElms ( void );


/************************************************************************
 * nmap_pgdel.c                                                         *
 *                                                                      *
 * This module contains the function to delete elements in product      *
 * generation.                                                          *
 *                                                                      *
 * CONTENTS:                                                            *
 * pgdel_deletEh()          delete from delete palette button           *
 * pgdel_keyboardDelete() handles deletes via the keyboard shortcuts	*
 * pgdel_deletAll()       delete all elements in the WORK_FILE          *
 *									*
 * pgdel_deleteElms()	  performs actual delete operation		*
 ***********************************************************************/

/*=====================================================================*/
/* ARGSUSED */
void pgdel_deletEh ( Widget wid, XtPointer clnt, XEvent *event,
						Boolean *ctdr )
/************************************************************************
 * pgdel_deletEh                                                        *
 *                                                                      *
 * Delete the selected element.                                         *
 *                                                                      *
 * void pgdel_deletEh (wid, clnt, event, ctdr )                       	*
 *                                                                      *
 * Input parameters:                                                    *
 *  wid         Widget          callback widget                         *
 *  clnt	XtPointer       client data                             *
 *  *event      XEvent                                                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner                    Initial coding                          *
 * E. Safford            9/97   Replaced grP->cmd with cmd_ routines    *
 * D.W.Plummer/NCEP      9/97   Changes for new vgstruct header file    *
 * E. Wehner/EAi         9/97   Remove graphics info record             *
 * C. Lin/EAi           10/97   rename from NxmDrawErase, cleanup       *
 * E. Safford/GSC       02/98   added undo capability                   *
 * F. Yen/NCEP           1/98   Updated calls for crg library cleanup   *
 * S. Law/GSC           04/98   added pghdlb_deselectEl                 *
 * E. Safford/GSC       04/98   mod to handle groups                    *
 * S. Law/GSC           05/98   replaced pgpalw_delete with _setupOper  *
 * E. Safford/GSC       05/98   updated for new undo functions          *
 * S. Law/GSC           05/98   added check for pghot_getElmLoc         *
 * E. Safford/GSC	06/98	moved to nmap_pgdel.c			*
 * E. Safford/GSC	06/98	updated pghdlb_deselectEl call		*
 * E. Safford/GSC	06/98	added call to crg_grfrsh		*
 * E. Safford/GSC	09/98	modify to handle mode (OBJ/GRP)		*
 * W. Li/EAI		10/98	renamed pgdel_eraseXX -> pgdel_deletXX	*
 * E. Safford/GSC	10/98	modify for param change to cvg_rdrec 	*
 * E. Safford/GSC	12/98	limit area of refresh			*
 * S. Law/GSC		03/00	changed to use pgutls_prepNew		*
 * E. Safford/GSC	03/00	fixed group delete crash		*
 * S. Law/GSC		03/00	added GRPTYP_CCF check			*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * J. Wu/GSC		03/01	added "EXIT" hint to bottom panel       *
 * J. Wu/SAIC		02/02	set changes_made to TRUE on curr. layer	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * E. Safford/SAIC	04/04	moved most code to pgdel_deleteElms()   *
 ***********************************************************************/
{
    int		location = -1;
/*---------------------------------------------------------------------*/

    location = pgactv_getElmLoc();
    if( location < 0 ) return;

    if (event->xbutton.button == Button1) {
        pgdel_deleteElms( );
    }

    mcanvw_disarmPress();
    pgpalw_setupOper();
    mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
}

/*=====================================================================*/

void pgdel_keyboardDelete ( void )
/************************************************************************
 * pgdel_keyboardDelete                                                 *
 *                                                                      *
 * This function handles deletes via the keyboard shortcuts.            *
 *                                                                      *
 * void pgdel_keyboardDelete ( )       	                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      none                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	04/04	initial coding				*
 ***********************************************************************/
{
    int		iopr = 0, numDeleted = 0;  
/*---------------------------------------------------------------------*/
/*
 *  Only allow keyboard deletes when in either of the selection modes.
 */ 
    iopr = pgpalw_getCurOperId();
    if( iopr != FUNC_SELECT && iopr != FUNC_MULTISEL ) {
        return;
    }
    else {
        numDeleted = pgdel_deleteElms( );
	if ( numDeleted ) {
            pgpalw_classPopdown();
	    pgpalw_setupOper();
        }
    }
}

/*=====================================================================*/

void pgdel_deletAll ( int *iret )
/************************************************************************
 * pgdel_deletAll                                                       *
 *                                                                      *
 * This function marks every record in a VG file as deleted and stores  *
 * each deleted element in the undo file.            			*
 *                                                                      *
 * void pgdel_deletAll ( iret )  	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      none                                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	07/98	initial coding				*
 * E. Safford/GSC	10/98	mod to use pgundo_storeFile & cvg_deall	*
 * H. Zeng/EAI          11/00   modified for the new undo design        *
 * A. Hardy/GSC         01/01   removed int cast from file pointer, fp  *
 * E. Safford/GSC	02/01	use cvg_deall instead of cvg_delet	*
 * J. Wu/SAIC		01/02	delete only all elem. on current layer	*
 * J. Wu/SAIC		02/02	record only elem. on current layer	*
 * J. Wu/SAIC		02/02	set changes_made to TRUE on curr. layer	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * S. Danz/AWC		07/06	Added new cvg_deall placement argument	*
 ***********************************************************************/
{
   int    maxbytes, location, flag, ier, cur_layer, el_layer;
   int    record_size;
   FILE   *fp;
   VG_DBStruct	el;
/*---------------------------------------------------------------------*/
/*
 *  NOTE:  Deleting all elements must use cvg_deall, not cvg_delet.
 *  cvg_deall does one file open, then reads each header, changes the
 *  delete flag, and writes the element header back to the same 
 *  file location.  cvg_delet does a file open, read, write, and file
 *  close.  So using cvg_delet to delete all elements will introduce
 *  _significant_ file overhead.  Take my advice -- don't do it.  - ES
 */
    *iret = 0;
    cur_layer = pglayer_getCurLayer ();    

    cvg_qkopen (cvg_getworkfile(), &fp, &maxbytes, &ier);

    pgundo_newStep();
    location  = 0;
    while ( location < maxbytes ) {
        cvg_rdhdr (cvg_getworkfile(), fp, location, maxbytes, &el, 
		   &flag, &ier);
        el_layer = crg_getLayer ( location );
        
	if( !el.hdr.delete  &&  el.hdr.vg_type != FILEHEAD_ELM &&
	    el_layer == cur_layer ) {
            pgundo_storeThisLoc(location, UNDO_DEL, &ier);
        }         
	record_size = el.hdr.recsz;

	location += record_size;

    }
    
    pgundo_endStep();
    cvg_clos (fp, iret); 
    
    cvg_deall( cvg_getworkfile(), cur_layer, TRUE, &ier );
    pglayer_setChngMade( cur_layer, TRUE );
}

/*=====================================================================*/

static int pgdel_deleteElms ( void )
/************************************************************************
 * pgdel_deleteElms                                                     *
 *                                                                      *
 * This function deletes all the currently selected elements.           *
 *                                                                      *
 * static void pgdel_deleteElms ( )   	                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      none                                                            *
 *                                                                      *
 * Return:                                                              *
 *      		int	number of deleted elements		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	04/04	initial coding				*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   Changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block pointers			*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 ***********************************************************************/
{
    int         num      = 0,  ier      = 0, found, update_crg; 
    int		grpnum   = 0,  ii       = 0, 	nelm   = 0;
    int		curIndex = -1, selIndex = 0, 	selLoc = 0;
    int		iret     = 0,  *inxarry = NULL, count  = 0;
    char        grptyp   = '0';
    float       llx = 0, lly = 0, urx = 0, ury = 0, inf_bbox[4];
    VG_DBStruct el;
/*---------------------------------------------------------------------*/

    pghdlb_getNextIndex( curIndex, &selIndex, &selLoc, &iret );

    update_crg = 0;
    while ( iret >= 0 ) {

/*
 *  If this is the first deletion, start the undo step
 */
	if( count == 0 ) {
            pgundo_newStep();	    
        }

        crg_getinx(selLoc, &num, &ier); 
        crg_ggrp (num, &grptyp, &grpnum, &ier);

/*
 *  If deleting _by_ group, process the whole group 
 */
        if (grptyp && grpnum && (grptyp == GRPTYP_COMSYM || 
	    grptyp == GRPTYP_CCF || pgpalw_getMode() == TYPE_GRP)) {

            crg_ggbnd (grptyp, grpnum, &llx, &urx, &ury, &lly, &ier);
	    llx -= EXTRA;
	    lly -= EXTRA;
	    urx += EXTRA;
	    ury += EXTRA;

            crg_ggnel(grptyp, grpnum, &nelm, &ier);

            inxarry = (int *)malloc(nelm*sizeof(int));
            crg_gginx(grptyp, grpnum, nelm, inxarry, &nelm, &ier);

            for ( ii = 0; ii < nelm; ii++ ) {

/*
 * Mark elements in placement that are effected by
 * the delete, and get the area of influence back
 */
                cvg_rdrec ( cvg_getworkfile(), selLoc, &el, &ier );
                cvg_checkplace(&el, 1, selLoc, &found, inf_bbox, &ier);
                if (found > 0) {

/*
 * Update the refresh extent if the area impacted by placement is bigger
 */
                    llx = G_MIN(llx, inf_bbox[0]);
                    lly = G_MIN(lly, inf_bbox[2]);
                    urx = G_MAX(urx, inf_bbox[1]);
                    ury = G_MAX(ury, inf_bbox[3]);

                    update_crg = 1;
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

                crg_goffset(inxarry[ii], &selLoc, &ier);
	        pgundo_storeThisLoc(selLoc, UNDO_DEL, &ier);
	        cvg_delet(cvg_getworkfile(), selLoc, TRUE, &ier);

		count++;
	        crg_clear (inxarry[ii], &ier);
            }
            free(inxarry);
            xpgpaste(llx, lly, urx, ury, &ier);

/*
 *  The deleted elements are deselected here so we won't 
 *  try to process them again in the outer while loop.
 */
	    pghdlb_deselectEl (num, TRUE);
	    cvg_rfrsh(NULL, llx, lly, urx, ury, &ier);
        }
        else {				/* non-group mode */
            pgutls_prepNew (selLoc, &el, &llx, &lly, &urx, &ury, &ier);
            pgundo_storeThisLoc(selLoc, UNDO_DEL, &ier);

/*
 * Free TCA break point/GFA block memory
 */
            if ( el.hdr.vg_type == TCA_ELM ) {
               cvg_freeBkpts ( &el );
            }
	    else if ( el.hdr.vg_type == GFA_ELM ) {
               cvg_freeElPtr ( &el );
            }

	    count++;
        }

/*
 *  Check for the next selected element
 */
	curIndex = selIndex;
        pghdlb_getNextIndex( curIndex, &selIndex, &selLoc, &iret );
    }

    if( count > 0 ) {
        pgundo_endStep();
        pglayer_setChngMade( pglayer_getCurLayer(), TRUE );
        pgactv_clearActv();
    }

/*
 * If we may have impacted other elements with placement
 * we will need to rebuild the range records
 */
    if (update_crg) {
        crg_rebuild();
    }

    return ( count );
}
