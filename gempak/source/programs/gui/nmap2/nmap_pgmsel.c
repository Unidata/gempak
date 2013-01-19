#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmTxt.h"
#include "pgprm.h"
#include "hints.h"
#include "drwids.h"
#include "vgstruct.h"


#define		NONE_CURRENT	(  0 )
#define		ONE		(  1 ) 
#define		STNMDL		( 10 )

/************************************************************************
 * nmap_pgmsel.c                                                        *
 *									*
 * This module contains the routines for handling multiple selection    *
 * when used in the context of the Multi-Select, Inc/Dec, and Grouping  *
 * functions in PGEN.                                                   *
 *									*
 * CONTENTS:                                                            *
 * pgmsel_multiPtSel	handles multiple selections			*
 * pgmsel_singlePtSel	handles single point selections			*
 * pgmsel_endMultiSel	handles termination of multiple selection	*
 ***********************************************************************/
     
/*=====================================================================*/

void pgmsel_multiPtSel ( int np, const float xx[], const float yy[] )
/************************************************************************
 * pgmsel_multiPtSel    						*
 *									*
 * Function to handle selection of a multiple points/elements within    *
 * multi-select.							*
 *									*
 * void pgmsel_multiPtSel( )                     			*
 *									*
 * Input parameters:							*
 *	np		int	number of points in the xx yy arrays	*
 *  	xx[]		float	X points				*
 *  	yy[]		float	Y points				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	05/04	initial coding (copied out of pgdsel.c)	*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block memory			*
 * E. Safford/SAIC	02/05	don't group GFA elements		*
 ***********************************************************************/
{
   int	 	numSel, nelm, offsets[MAX_EDITABLE_ELEMS], grp_off;
   int	 	ignore, ii, jj, ier, iopr, nexp, inxarry[MAX_EDITABLE_ELEMS];
   int 		cur_layer, errCode = 0;
   char		sel_grpd, class, *num_str;
   float	xPts[MAX_EDITABLE_ELEMS], yPts[MAX_EDITABLE_ELEMS];
   VG_DBStruct	el, grp_el;

/*----------------------------------------------------------------------*/

   /*
    *  Exit if we don't have a closed figure.
    */
   if ( np < 3 ) return;


   for (ii=0; ii<np; ii++) {
      xPts[ii] = xx[ii];
      yPts[ii] = yy[ii];
   }

   cur_layer = pglayer_getCurLayer ();
   class     = pgpalw_getCurClassId();
   iopr      = pgpalw_getCurOperId();

   /*
    *  Determine if we're selecting grouped elements.
    */
   sel_grpd  = pgpalw_getMode();
   if (iopr == FUNC_GROUP) {
      sel_grpd   = 0;
   }

   cvg_fscan(cvg_getworkfile(), cur_layer, class, sel_grpd, np, 
       		    	     xPts, yPts, &numSel, offsets, &ier);

   /*
    *  Loop through all the returned elements.  
    */
   for (ii=0; ii < numSel; ii++)  {
      cvg_rdrec (cvg_getworkfile(), offsets[ii], &el, &ier);

      if (iopr == FUNC_INC_DEC) {
	 if ((el.hdr.vg_type == TEXT_ELM) || 
	     (el.hdr.vg_type == TEXTC_ELM)){
            num_str = el.elem.txt.text;
         }
	 else if (el.hdr.vg_type == SPTX_ELM) {
	    num_str = el.elem.spt.text;
    	 }
         else {
	    num_str = NULL;
	 }

	 if (num_str) {
            if (pgutls_isNumber (num_str)) {
	       pgnumb_updateBtns (TRUE);
               pghdlb_setSelect (offsets[ii]);

	       if (el.hdr.grptyp == STNMDL && el.hdr.grpnum) {
		  crg_ggnel(el.hdr.grptyp, el.hdr.grpnum, &nexp, &ier);  
		  crg_gginx(el.hdr.grptyp, el.hdr.grpnum,
		  			   nexp, inxarry, &nelm, &ier); 
                  
                  for(jj = 0; jj < nelm; jj++){
                     crg_goffset(inxarry[jj] , &grp_off, &ier );
		     cvg_rdrec (cvg_getworkfile(), grp_off, &grp_el, &ier);

		     if (grp_el.hdr.vg_type == MARK_ELM) {
			pghdlb_setSelect (grp_off);
			break;
                     }
                  }
               }
	    }
	 }
      }			
      else {			/* not INC/DEC */
	 errCode = 0;

	 if (  pgpalw_isGrpActv()               &&
	       el.hdr.delete   == 0             &&
	       el.hdr.vg_class != CLASS_WATCHES &&
	       el.hdr.vg_class != CLASS_TRACKS  &&
	       el.hdr.vg_class != CLASS_SIGMETS &&
               el.hdr.vg_type  != GFA_ELM       &&
	       el.hdr.grptyp   <  90            &&
      	      (el.hdr.grptyp != (char)pggrpw_getGrpType() ||
      	       el.hdr.grpnum !=       pggrpw_getGrpNum()    ) ) {

            pggrpw_addtoGrp( &el, offsets[ii], &ier );

	 }
	 else if ( !pgpalw_isGrpActv() ) {
	    pghdlb_setSelect (offsets[ii]);
	 }
	 else if ( el.hdr.vg_class == CLASS_SIGMETS ) {
	    errCode = 3;
	 }
	 else if ( el.hdr.vg_class == CLASS_TRACKS ) {
	    errCode = 2;
	 }
	 else if ( el.hdr.vg_class == CLASS_WATCHES ) {
	    errCode = 1;
	 }
         else if ( el.hdr.vg_type == GFA_ELM ) {
            errCode = 8;
         }
	 else if ( el.hdr.grptyp >= 90 ) {
	    errCode = 4;
	 }
      }

      if( errCode != 0 ) {
         er_wmsg ("pgen", &errCode, NULL, &ignore, 4, 0);
	 NxmErr_update(); 
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

   }		/* end for loop */


   if ( pgpalw_isGrpActv() ) {

      pghdlb_deselectGrp( (char)pggrpw_getGrpType(), pggrpw_getGrpNum()   );
      pghdlb_selectGrp( (char)pggrpw_getGrpType(), pggrpw_getGrpNum()   ); 

   }
   else {
      pghdlb_displayAllSel();
   }

   if ( (pgpalw_getCurClassId() != CLASS_ANY) && (iopr != FUNC_INC_DEC) &&
        (pgpalw_getMode() == TYPE_OBJ) &&  numSel && !pgedit_isActive()&&
        !pgpalw_isGrpActv()  ) {

      cvg_rdrec (cvg_getworkfile(), offsets[0], &el, &ier);
      pgedit_editStart (&el);

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

}

/*=====================================================================*/

void pgmsel_singlePtSel ( float xx, float yy )
/************************************************************************
 * pgmsel_singlePtSel    						*
 *									*
 * Function to handle selection of a single point/element within        *
 * multi-select.							*
 *									*
 * void pgmsel_singlePtSel( )                      			*
 *									*
 * Input parameters:							*
 *  	xx		float	X location of mouse button release	*
 *  	yy		float	Y location of mouse button release	*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	05/04	initial coding (copied out of pgdsel.c)	*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC           10/04   free GFA block memory			*
 ***********************************************************************/
{
   char        	sel, grptyp, draw_mode, *num_str;
   int         	grpnum, selected, nearest, iclass, iopr, el_num;
   int		nexp, nelm, inxarry[MAX_EDITABLE_ELEMS], ii, rng_loc;
   int		ier, fl_pos, err_code, ignore, cur_layer;
   Boolean	not_group, stn_mdl;
   VG_DBStruct	el;
/*---------------------------------------------------------------------*/

   /*
    * get current operation ID and class ID
    */
   iclass = pgpalw_getCurClassId();
   if (iclass == NONE_CURRENT) return;

   cur_layer = pglayer_getCurLayer ();

   if (iclass == CLASS_COMSYM || iclass == CLASS_MARKER)
      iclass = CLASS_SYMBOLS;

   el.hdr.vg_class = iclass;

   cvg_scan (NULL, cur_layer, (char) iclass, xx, yy, 0,
                  &el, &selected, &nearest, &ier);

      if (ier >= 0) {

         /*
          * Free TCA break point memory
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


	 if (sel) {	/* deselect element */
 	    if (ier >= 0) {
	       if ( iopr == FUNC_UNGROUP ) {
	          pgevt_ungroup();
	       }		   
	       else if (iopr == FUNC_INC_DEC) {

                  /*
   	           *  check for STNMDL elements and deselect all
		   *  within each group
		   */
		  crg_goffset(el_num, &fl_pos, &ier);
		  cvg_rdrec (cvg_getworkfile(), fl_pos, &el, &ier);

  	          /*
                    * Free TCA break point memory
                    */
                  if ( el.hdr.vg_type == TCA_ELM ) {
                      cvg_freeBkpts ( &el );
                  }
                  else if ( el.hdr.vg_type == GFA_ELM ) {
                      cvg_freeElPtr ( &el );
                  }

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
	       }   
	       else {

	          /*
		   * If currently in a GROUP process, remove the element from
		   * the group.
		   */
		  if ( pgpalw_isGrpActv() ) {

                     pghdlb_deselectGrp( (char)pggrpw_getGrpType(),
		                               pggrpw_getGrpNum()   );
		     pggrpw_rmvfrmGrp( &el, selected, &ier );
		     pghdlb_selectGrp( (char)pggrpw_getGrpType(),
		                               pggrpw_getGrpNum()   );  
		  }
		  else {
		     pghdlb_deselectEl( el_num, TRUE );
		     pghdlb_displayAllSel ();
		  }

      		  if ( pgpalw_isGrpActv() )
		     mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_EXIT);
		  else if ( iopr == FUNC_GROUP && pghdlb_elemSelected() >= 1 )
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
		  crg_ggnel(el.hdr.grptyp, el.hdr.grpnum, &nexp, &ier);
  	          crg_gginx(el.hdr.grptyp, el.hdr.grpnum,
				    nexp, inxarry, &nelm, &ier);
	       }
  	       
	       /*
                 * Free TCA break point memory
                 */
               if ( el.hdr.vg_type == TCA_ELM ) {
                   cvg_freeBkpts ( &el );
               }
               else if ( el.hdr.vg_type == GFA_ELM ) {
                   cvg_freeElPtr ( &el );
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
	              
		      /*
                         * Free TCA break point memory
                         */
                      if ( el.hdr.vg_type == TCA_ELM ) {
                          cvg_freeBkpts ( &el );
                      }
                      else if ( el.hdr.vg_type == GFA_ELM ) {
                          cvg_freeElPtr ( &el );
                      }
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
                                            pggrpw_getGrpNum()   );
                  pggrpw_addtoGrp( &el, selected, &ier );
                  pghdlb_selectGrp( (char)pggrpw_getGrpType(),
                                          pggrpw_getGrpNum()   ); 
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
  	       if (draw_mode == TYPE_OBJ && el.hdr.grptyp != GRPTYP_COMSYM) {
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
                    (pgpalw_getMode() == TYPE_OBJ) && !pgedit_isActive() ) {
	          cvg_rdrec (cvg_getworkfile(), selected, &el, &ier);
            	  pgedit_editStart (&el);
               
                    /*
                      * Free TCA break point memory
                      */
                    if ( el.hdr.vg_type == TCA_ELM ) {
                        cvg_freeBkpts ( &el );
                    }
                    else if ( el.hdr.vg_type == GFA_ELM ) {
                       cvg_freeElPtr ( &el );
                    }
	       
	       }

	    }
	 }
      } /* else (select element)... */

   } /* if (ier >=0... */

}


/*=====================================================================*/

void pgmsel_endMultiSel ( void )
/************************************************************************
 * pgmsel_endMultiSel							*
 *									*
 * Function to handle termination of multi-select                       *
 *									*
 * void pgmsel_endMultiSel( )                      			*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	05/04	initial coding (copied out of pgdsel.c)	*
 ***********************************************************************/
{
    int         iopr;
/*---------------------------------------------------------------------*/

   iopr = pgpalw_getCurOperId();

   if ( iopr == FUNC_INC_DEC ) {	
      if (!pghdlb_elemSelected()){
         pgedit_multiEditCb( NULL, (XtPointer)ONE, NULL); /* cancel */
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
   else if ( iopr == FUNC_GROUP ) {

      /*
       *  Group uses a 2-stage exit.  If anything is selected, first
       *  de-select it but allow grouping to continue.  If nothing
       *  is selected, then exit.
       */
      if ( pghdlb_elemSelected() >= 1 ) {

         pghdlb_deselectGrp( (char)pggrpw_getGrpType(), pggrpw_getGrpNum() );

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
   else if (iopr == FUNC_MULTISEL )  {		
      pgevt_unsetOper (FALSE);

      if ( pgpalw_getCurClassId() > 0 ) {
         if ( iopr == FUNC_UNGROUP )
	    mbotw_mouseSet(LMHINT_SELECT, MMHINT_EXIT);
	 else
	    mbotw_mouseSet(LMHINT_TOGGLE, MMHINT_NOACTION);
      }
   }
   
}
