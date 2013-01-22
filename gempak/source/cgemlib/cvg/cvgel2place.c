#include "cvgcmn.h"
#include "proto_uka.h"

static void cvg_ashregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_ccfpregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_circleregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_frontregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_gfaregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_sgwxregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_jetregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_lineregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_listregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_sigmetregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_splnregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_symbregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_tcaregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_textregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_trkstmregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_vectregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_volcregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_wboxregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_checkgrp(Handle id, VG_DBStruct *el, int *iret);
static void cvg_unregpl(Handle id, VG_DBStruct *el, int *iret);
static void cvg_checkrefmatch(Handle obj, int grptyp, int grpnum, int *iret);
static void cvg_checkobjmatch(Handle ref, int grptyp, int grpnum, int *iret);
static void registerBarbsAndHashes (Handle id, VG_DBStruct *el);

void cvg_el2place(int elpos, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_el2place
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *                                -1 = Invalid pointer in arguments
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Removed test to check for deleted objects
 ****************************************************************************/
{
    Handle  id;
/*---------------------------------------------------------------------*/

    if (!el) {
        *iret = -1;
        return;
    }

    *iret = 0;

    /*
     * If placement hasn't been setup yet, just leave
     */
    if (!cvg_metadata || !cvg_placements) {
        return;
    }

    id = cvg_el2hndl(elpos, el, iret);

    /*
     * Based on the type of object, send it to the function that will
     * break it down appropriately to be added/deleted to the meta data
     * and  also determine if any of the 'parts' need to be added to the 
     * placement set
     */
    switch ( el->hdr.vg_type) {
        case FRONT_ELM:
            cvg_frontregpl(id, el, iret);
        break;

        case SPLN_ELM:
            cvg_splnregpl(id, el, iret);
        break;

        case LINE_ELM:
            cvg_lineregpl(id, el, iret);
        break;

        case TEXT_ELM:
        case TEXTC_ELM:
        case SPTX_ELM:
            cvg_textregpl(id, el, iret);
        break;

        case BARB_ELM:
        case ARROW_ELM:
        case DARR_ELM:
        case HASH_ELM:
            cvg_vectregpl(id, el, iret);
        break;

        case MARK_ELM:
        case WXSYM_ELM:
        case CTSYM_ELM:
        case ICSYM_ELM:
        case PTSYM_ELM:
        case PWSYM_ELM:
        case SKSYM_ELM:
        case SPSYM_ELM:
        case TBSYM_ELM:
        case CMBSY_ELM:
            cvg_symbregpl(id, el, iret);
        break;

        case WBOX_ELM:
            cvg_wboxregpl(id, el, iret);
        break;

        case CIRCLE_ELM:
            cvg_circleregpl(id, el, iret);
        break;

        case TRKSTORM_ELM:
            cvg_trkstmregpl(id, el, iret);
        break;

        case SIGAIRM_ELM:
        case SIGCONV_ELM:
        case SIGINTL_ELM:
        case SIGNCON_ELM:
        case SIGOUTL_ELM:
            cvg_sigmetregpl(id, el, iret);
        break;

        case SIGCCF_ELM:
            cvg_ccfpregpl(id, el, iret);
        break;

        case VOLC_ELM:
            cvg_volcregpl(id, el, iret);
        break;

        case ASHCLD_ELM:
            cvg_ashregpl(id, el, iret);
        break;

        case LIST_ELM:
            cvg_listregpl(id, el, iret);
        break;

        case JET_ELM:
            cvg_jetregpl(id, el, iret);
        break;

        case GFA_ELM:
            cvg_gfaregpl(id, el, iret);
        break;

        case SGWX_ELM:
            cvg_sgwxregpl(id, el, iret);
        break;

        case TCA_ELM:
            cvg_tcaregpl(id, el, iret);
        break;

        default:
            /* Error? */
        break;
    }

    /*
     * See if its grouped and track it if it is
     */
    cvg_checkgrp(id, el, iret);

    return;
}


static void cvg_ashregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_ashregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int         np;
    float       dx[MAXPTS], dy[MAXPTS];
/*---------------------------------------------------------------------*/
        
    /*
     * FIXME: Is there a text box here?  If so it needs to be extracted from
     * the VG element.
     */

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        cvg_todev2(el, &np, dx, dy, iret);

        /*
         * For closed elements, repeat the first point to the end. 
         */
        if ( el->hdr.closed && np < MAXPTS ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
        }

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
    }
}


static void cvg_ccfpregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_ccfpregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 * L. Hinson/AWC        07/09   Fixed to make placement functional
 ****************************************************************************/
{
    int         np;
    Handle      txt_id;
    float       dx[MAXPTS], dy[MAXPTS];
    VG_DBStruct txt_el;
    int subtype;
/*---------------------------------------------------------------------*/

    txt_id = id + 1;
    
    /*
     * FIXME: Is there a text box here?  If so it needs to be extracted from
     * the VG element.  It looks like today there is a separate text box that
     * is grouped with the CCFP element... 
     */

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
        cvg_unregpl(txt_id, el, iret);
    } else {
        cvg_todev2(el, &np, dx, dy, iret);

        /*
         * For closed elements, repeat the first point to the end. 
         */
        if ( el->hdr.closed && np < MAXPTS ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
        }

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
        
        /* Get the text information and create a box to be placed. */
        
        cds_ccftxt(el, &txt_el, iret);
        
        if (*iret == 0) {
          cvg_textregpl(txt_id, &txt_el, iret);
          if (cvg_cap_by_group[GRPTYP_CCF].enabled) {
            cvg_subtyp(el, &subtype, iret);
            if (cvg_cap_by_group[GRPTYP_CCF].obj_type ==
              txt_el.hdr.vg_type &&
                (cvg_cap_by_group[GRPTYP_CCF].obj_subtype == subtype ||
                 cvg_cap_by_group[GRPTYP_CCF].obj_subtype == -1)) {
              /* Determine additional conditions... */
              cap_psaddpl(cvg_placements, txt_id, id,
                cvg_cap_by_group[GRPTYP_CCF].inside,
                cvg_cap_by_group[GRPTYP_CCF].both_sides,
                cvg_cap_by_group[GRPTYP_CCF].attempts,
                cvg_cap_by_group[GRPTYP_CCF].increment,
                cvg_cap_by_group[GRPTYP_CCF].offset,
                IMMEDIATE,
                cvg_cap_by_group[GRPTYP_CCF].point_center, iret
              );
            }
            
          }
        }        
    }
}


static void cvg_circleregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_circleregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int         np;
    float       dx[MAXPTS], dy[MAXPTS];
    VG_DBStruct line_el;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        /*
         * Convert the circle to a polygon with 8 points, that's enough
         */
        cvg_cir2lin(el, 45, &line_el, iret);

        /*
         * Now convert the object to device coordinates
         */
        cvg_todev2(&line_el, &np, dx, dy, iret);

        /*
         * For closed elements, repeat the first point to the end. 
         */
        if ( line_el.hdr.closed && np < MAXPTS ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
        }

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
    }
}


static void cvg_frontregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_frontregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int         np;
    float       dx[MAXPTS], dy[MAXPTS];
/*---------------------------------------------------------------------*/

    /*
     * FIXME: add direction arrows and speed text
     */

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        cvg_todev2(el, &np, dx, dy, iret);

        /*
         * For closed elements, repeat the first point to the end. 
         */
        if ( el->hdr.closed && np < MAXPTS ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
        }

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
    }
}


static void cvg_gfaregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_gfaregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 * L.Hinson/AWC         12/06   Add check on iret status for 3 after
 *                              calling cds_gfatxt.  This implies
 *                              that NIL has been set in text layout
 *                              string, and that we want to bypass
 *                              placement.
 ****************************************************************************/
{
    int         np, subtype;
    Handle      txt_id;
    float       dx[MAXPTS], dy[MAXPTS];
    VG_DBStruct txt_el;
/*---------------------------------------------------------------------*/

    txt_id = id + 1;
    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
        cvg_unregpl(txt_id, el, iret);
    } else {
        cvg_todev2(el, &np, dx, dy, iret);
        cvg_subtyp(el, &subtype, iret);

        /*
         * For closed elements, repeat the first point to the end. 
         */
        if ( el->hdr.closed && np < MAXPTS ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
        }

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);

        /*
         * Get the text information and create a box to be placed.
         */
        cds_gfatxt(el, &txt_el, iret);
	if (*iret == 3) {  /*exit routine if ier set to 3 */
	    *iret = 0;     /* on NIL set in layout string */
	    return;
	}

        if (*iret == 0) {
            cvg_textregpl(txt_id, &txt_el, iret);
            
            /*
             * If the text is in the placement table, add it to the placement set
             */
            if (cvg_cap_by_type[GFA_ELM].enabled) {
                if (cvg_cap_by_type[GFA_ELM].obj_type == txt_el.hdr.vg_type &&
                    (cvg_cap_by_type[GFA_ELM].obj_subtype == subtype ||
                     cvg_cap_by_type[GFA_ELM].obj_subtype == -1)) {
                    cap_psaddpl(cvg_placements, txt_id, id, 
                            cvg_cap_by_type[GFA_ELM].inside, 
                            cvg_cap_by_type[GFA_ELM].both_sides, 
                            cvg_cap_by_type[GFA_ELM].attempts, 
                            cvg_cap_by_type[GFA_ELM].increment, 
                            cvg_cap_by_type[GFA_ELM].offset, 
                            IMMEDIATE, 
                            cvg_cap_by_type[GFA_ELM].point_center, iret
                        );
                }
            }
        }
    }
}

static void cvg_sgwxregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_sgwxregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * L. Hinson/AWC        01/12   Created
 ****************************************************************************/
{
  int         np;
  Handle      txt_id;
  float       dx[MAXPTS*2], dy[MAXPTS*2];
  VG_DBStruct txt_el;
  int subtype;
  txt_id = id + 1;
  *iret = 0;
  if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
        cvg_unregpl(txt_id, el, iret);
  } else {
    cvg_todev2(el, &np, dx, dy, iret);
    if ( el->hdr.closed && np < MAXPTS*2 ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
    }
    cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
    /* Get the text information and create a box to be placed. */
    cds_sgwxtxt(el, &txt_el, iret);
    if (*iret == 0) {
       cvg_textregpl(txt_id, &txt_el, iret);
       if (cvg_cap_by_group[GRPTYP_SGWX].enabled) {
         cvg_subtyp(el, &subtype, iret);
         if (cvg_cap_by_group[GRPTYP_SGWX].obj_type ==
           txt_el.hdr.vg_type &&
             (cvg_cap_by_group[GRPTYP_SGWX].obj_subtype == subtype ||
              cvg_cap_by_group[GRPTYP_SGWX].obj_subtype == -1)) {
           /* Determine additional conditions... */
           cap_psaddpl(cvg_placements, txt_id, id,
             cvg_cap_by_group[GRPTYP_SGWX].inside,
             cvg_cap_by_group[GRPTYP_SGWX].both_sides,
             cvg_cap_by_group[GRPTYP_SGWX].attempts,
             cvg_cap_by_group[GRPTYP_SGWX].increment,
             cvg_cap_by_group[GRPTYP_SGWX].offset,
             IMMEDIATE,
             cvg_cap_by_group[GRPTYP_SGWX].point_center, iret
           );
         }

       }
    }
  }
}


static void cvg_jetregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_jetregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 * L. Hinson/AWC        01/12   Add barbs, text, and hash marks.
 ****************************************************************************/
{
    int     np;
    float   dx[MAXPTS], dy[MAXPTS];
    
/*---------------------------------------------------------------------*/

    *iret = 0;
    /* idcount = id; */
    
    if (el->hdr.delete) {
        /* printf("Jet id=%d is flagged with delete\n",id);
	printf("Number of barbs=%d\n", el->elem.jet.nbarb); */
        cvg_unregpl(id, el, iret);
	/* While the jet barbs and hashes should technically be unregistered...
	   there is a bug with the el->elem structure as it was not populated.
	   See cvgdelet.c which calls cvg_rdhdr only to populate the header.
	   Consequently, the number of barbs and number of hashes could be
	   set to huge large numbers, causing problems with NMAP2 management
	   of the jets and other software going into a long loop.
	*/
	/*
        for (i = 0; i < el->elem.jet.nbarb; i++) {
  	  if (el->elem.jet.nbarb > 20) break;
          idcount++;
          cvg_unregpl(idcount, el, iret);
          idcount++;
          cvg_unregpl(idcount, el, iret);
        }
        for (i = 0; i< el->elem.jet.nhash; i++) {
	  if (el->elem.jet.nhash > 20) break;
          idcount++;
          cvg_unregpl(idcount, el, iret);
 
        }
        */
	
    } else {
        cvg_todev2(el, &np, dx, dy, iret);
        
        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
        if (*iret != 0) {
          printf("Jet id=%d not registered\n",id);
        }
        registerBarbsAndHashes(id,el);        
    }
}

static void registerBarbsAndHashes (Handle id, VG_DBStruct *el) 
{
    VG_DBStruct barb_el, text_el, hash_el;
    VG_DBStruct *new_el;
    int i, idcount, iret;
    iret = 0;
    idcount = id+1;
    /**********************************/
    /* Now we tweak the jets, such that under the condition that
       there are only hash marks for that jet are in the view
       window, that we register [and plot] at least one wind barb.
    */
    /* alterBarbsAndHashes(el,new_el);*/
    new_el = el;
    
    
    /*******************************************/
     /* Add Wind barb... */     
    for (i = 0; i < new_el->elem.jet.nbarb; i++) {
      barb_el.hdr.vg_type = BARB_ELM;
      barb_el.elem.wnd.info.numwnd = 1;
      barb_el.elem.wnd.info.size = new_el->elem.jet.barb[i].wnd.info.size;
      barb_el.elem.wnd.data.spddir[0] = new_el->elem.jet.barb[i].wnd.data.spddir[0];
      barb_el.elem.wnd.data.spddir[1] = new_el->elem.jet.barb[i].wnd.data.spddir[1];
      barb_el.elem.wnd.data.latlon[0] = new_el->elem.jet.barb[i].wnd.data.latlon[0];
      barb_el.elem.wnd.data.latlon[1] = new_el->elem.jet.barb[i].wnd.data.latlon[1]; 
      cvg_vectregpl(idcount, &barb_el, &iret);
      idcount++;

      /* Add associative text with the barb... */
      text_el.hdr.vg_type = SPTX_ELM;
      text_el.hdr.delete = 0;
      sprintf(text_el.elem.spt.text,"%s",new_el->elem.jet.barb[i].spt.text);
      text_el.elem.spt.info.sztext = new_el->elem.jet.barb[i].spt.info.sztext;
      text_el.elem.spt.info.ithw = new_el->elem.jet.barb[i].spt.info.ithw;
      text_el.elem.spt.info.itxfn = new_el->elem.jet.barb[i].spt.info.itxfn;
      text_el.elem.spt.info.ialign = new_el->elem.jet.barb[i].spt.info.ialign;
      text_el.elem.spt.info.offset_x = new_el->elem.jet.barb[i].spt.info.offset_x;
      text_el.elem.spt.info.offset_y = new_el->elem.jet.barb[i].spt.info.offset_y;
      text_el.elem.spt.info.rotn = new_el->elem.jet.barb[i].spt.info.rotn;
      text_el.elem.spt.info.sptxtyp = new_el->elem.jet.barb[i].spt.info.sptxtyp;
      text_el.elem.spt.info.lat = new_el->elem.jet.barb[i].spt.info.lat;
      text_el.elem.spt.info.lon = new_el->elem.jet.barb[i].spt.info.lon;
      cvg_textregpl(idcount,&text_el,&iret);
      idcount++;

    }
    for (i = 0; i< new_el->elem.jet.nhash; i++) {
      hash_el.hdr.vg_type = HASH_ELM;
      hash_el.elem.wnd.info.numwnd = 1;
      hash_el.elem.wnd.info.size = new_el->elem.jet.hash[i].wnd.info.size;
      hash_el.elem.wnd.data.spddir[0] = new_el->elem.jet.hash[i].wnd.data.spddir[0];
      hash_el.elem.wnd.data.spddir[1] = new_el->elem.jet.hash[i].wnd.data.spddir[1];
      hash_el.elem.wnd.data.latlon[0] = new_el->elem.jet.hash[i].wnd.data.latlon[0];
      hash_el.elem.wnd.data.latlon[1] = new_el->elem.jet.hash[i].wnd.data.latlon[1];
      cvg_vectregpl(idcount, &hash_el, &iret);
      idcount++;
    }
}

static void cvg_lineregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_lineregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int     np;
    float   dx[MAXPTS], dy[MAXPTS];
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        cvg_todev2(el, &np, dx, dy, iret);

        /*
         * For closed elements, repeat the first point to the end. 
         */
        if ( el->hdr.closed && np < MAXPTS ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
        }

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
    }
}


static void cvg_listregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_listregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int     i, np;
    float   dx[MAXPTS], dy[MAXPTS], bx[5], by[5], minx, maxx, miny, maxy;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        cvg_todev2(el, &np, dx, dy, iret);
        
        /*
         * Create a box for the area enclosed by the list
         * Or should this be a separate box for each symbol in the list?
         * That could use more than the 100 id we have left in the Handle info
         * for 'sub-id' numbers.
         */
        minx = maxx = dx[0];
        miny = maxy = dy[0];
        for (i = 1; i < np; i++) {
            if (minx > dx[i]) {
                minx = dx[i];
            }
            if (miny > dy[i]) {
                miny = dy[i];
            }
            if (maxx < dx[i]) {
                maxx = dx[i];
            }
            if (maxy < dy[i]) {
                maxy = dy[i];
            }
        }
        bx[0] = minx;
        bx[1] = maxx;
        bx[2] = maxx;
        bx[3] = minx;
        bx[4] = bx[0];

        by[0] = miny;
        by[1] = miny;
        by[2] = maxy;
        by[3] = maxy;
        by[4] = by[0];

        cmd_osaddob(cvg_metadata, id, bx, by, 5, iret);
    }
}


static void cvg_sigmetregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_sigmetregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int     np;
    float   dx[MAXPTS], dy[MAXPTS];
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        cvg_todev2(el, &np, dx, dy, iret);

        /*
         * For closed elements, repeat the first point to the end. 
         */
        if ( el->hdr.closed && np < MAXPTS ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
        }

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
    }
}


static void cvg_splnregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_splnregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int     np;
    float   dx[MAXPTS], dy[MAXPTS];
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        cvg_todev2(el, &np, dx, dy, iret);

        /*
         * For closed elements, repeat the first point to the end. 
         */
        if ( el->hdr.closed && np < MAXPTS ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
        }

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
    }
}


static void cvg_symbregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_symbregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    float   dx[MAXPTS], dy[MAXPTS];
    float   rx[1], ry[1];
    float	llx, lly, urx, ury;
    int 	ier, np;
    float   szmk, sztx, szwb, szws, szab, szah;
    float	szwsx; 
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        cvg_todev2(el, &np, rx, ry, iret);

        /*
         *  Get symbol size
         */

        gqsizd ( &szmk, &sztx, &szwb, &szws, &szab, &szah, &ier );

        /*
         *  To get actual pixel size, multiply szws by size multiplier
         */

        szwsx = szws * el->elem.sym.info.size;

        /* 
         *  Coordinates based on the dimension of the element 
         */

        lly = ry[0] - szwsx/2.0;
        llx = rx[0] - szwsx/2.0;
        urx = rx[0] + szwsx/2.0;
        ury = ry[0] + szwsx/2.0;

        dx[0] = llx;
        dx[1] = urx;
        dx[2] = urx;
        dx[3] = llx;
        dx[4] = dx[0];

        dy[0] = lly;
        dy[1] = lly;
        dy[2] = ury;
        dy[3] = ury;
        dy[4] = dy[0];

        np = 5;

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
    }
}


static void cvg_tcaregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_tcaregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int     np;
    float   dx[MAXPTS], dy[MAXPTS];
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        cvg_todev2(el, &np, dx, dy, iret);

        /*
         * For closed elements, repeat the first point to the end. 
         */
        if ( el->hdr.closed && np < MAXPTS ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
        }

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
    }
}


static void cvg_textregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_textregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int     pt, np, flip;
    float   x[5], y[5], dx[5], dy[5], dxl, dxr, dyt, dyb, cx, cy;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        /*
         * Get the device coordinates and see if the y needs to be flipped
         */
        gqbnd( sys_D, &dxl, &dyb, &dxr, &dyt, iret, strlen(sys_D) );

        if (dyt > dyb) {
            flip = 0;
        } else {
            flip = 1;
        }

        cvg_todev2(el, &np, &cx, &cy, iret);
        crg_gettxtbox (el, 2, x, y);

        /*
         * Adjust the text box to the appropriate location based on the x/y of
         * the control point for the box
         */
        for (pt=0; pt < 4; pt++) {
            dx[pt] = x[pt] + cx;
            if (flip) {
                dy[pt] = -y[pt] + cy;
            } else {
                dy[pt] = y[pt] + cy;
            }
        }
        dx[4] = dx[0];
        dy[4] = dy[0];

        cmd_osaddob(cvg_metadata, id, dx, dy, 5, iret);
    }
}


static void cvg_trkstmregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_trkstmregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int     np;
    float   dx[MAXPTS], dy[MAXPTS];
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        cvg_todev2(el, &np, dx, dy, iret);

        /*
         * For closed elements, repeat the first point to the end. 
         */
        if ( el->hdr.closed && np < MAXPTS ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
        }

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
    }
}


static void cvg_vectregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_vectregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 * L. Hinson/AWC        01/12   Fixed math and logic for registering hashes
 *                              and barbs on a jet.
 ****************************************************************************/
{
    int     np;
    float   px[2], py[2], dx[MAXPTS], dy[MAXPTS];
    float   szmk, sztx, szwb, szws, szab, szah;
    float   rx, ry, angle, srotn, nrotn, speed, linelen;
    float   offsetx, offsety;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        /*
         *  Get hash, wind barb and wind arrow sizes...
         */
        gqsizd ( &szmk, &sztx, &szwb, &szws, &szab, &szah, iret );

        /*
         * Have the lat/lon for the base of the object converted
         * to device
         */
        cvg_todev2(el, &np, dx, dy, iret);

        /*
         * Compute the length of the line for the arrow
         */
        speed = 1.0;
        if (el->hdr.vg_type == ARROW_ELM || (el->hdr.vg_type == DARR_ELM)) {
            /* Use speed to determine length */
            if (el->hdr.vg_type == DARR_ELM) {
                speed = 10.0;
            } else {
                speed = el->elem.wnd.data.spddir[0];
                if (speed < 1) {
                    speed = 1.0;
                }
            }
            linelen = speed * el->elem.wnd.info.size * szab;
            linelen += el->elem.wnd.info.hdsiz * szah * 0.85;
        } else {
            linelen = el->elem.wnd.info.size * szwb;
        }

        srotn = 0.0F;
        rx = el->elem.wnd.data.latlon[0];
        ry = el->elem.wnd.data.latlon[1];
        gp_azdr (&srotn, &rx, &ry, &nrotn, iret );
        angle = (el->elem.wnd.data.spddir[1] - nrotn) * DTR;
	
        if (el->hdr.vg_type == HASH_ELM) {
            /* Hash marks extend to both sides of the point */
	    /* Here we recompute the angle, since the spddir[1] sub-element
	       is the direction of the vector from, and not to.  The previous
	       angle cacluation subtracted the rotation, which messed up the
	       math calculations*/
            angle = (el->elem.wnd.data.spddir[1] + nrotn) * DTR;
            px[0] = dx[0] - cos(angle) * linelen/2.0;
            py[0] = dy[0] - sin(angle) * linelen/2.0;
            px[1] = dx[0] + cos(angle) * linelen/2.0;
            py[1] = dy[0] + sin(angle) * linelen/2.0;
        } else {
            if (el->hdr.vg_type == BARB_ELM) {
	      /* Here we recompute the angle, since the spddir[1] sub-element
	       is the direction of the vector from, and not to.  The previous
	       angle cacluation subtracted the rotation, which messed up the
	       math calculations*/
              angle = (el->elem.wnd.data.spddir[1] + nrotn) * DTR;
              /* Here we offset the Windbarb by 10 pixels displacement
	         from its original location. This is so that we
		 register a line tangent to the flags of the barb, 
		 rather than a line tangent to the jet itself which has
		 already been registered.
		 
		 NOTE:  This modification is specific to wind barbs
		 on the jet. 
		 		 
		 If we are in the southern
		 hemisphere, we multiply these offsets by -1.
	      */
		 
              offsetx = cos(angle)*10;
              offsety = sin(angle)*10;
              if (rx < 0) {
                offsetx *= -1.0;
                offsety *= -1.0;
              }              
              px[0] = dx[0] + offsetx - sin(angle) * linelen;
              py[0] = dy[0] + offsety + cos(angle) * linelen;
              px[1] = dx[0] + offsetx + sin(angle) * linelen;
              py[1] = dy[0] + offsety - cos(angle) * linelen;
            } else {
              /* Everything else starts at the point and extends */
              px[0] = dx[0];
              py[0] = dy[0];
              px[1] = dx[0] + cos(angle) * linelen;
              py[1] = dy[0] + sin(angle) * linelen;
            }
        }

        cmd_osaddob(cvg_metadata, id, px, py, 2, iret);
    }
}


static void cvg_volcregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_volcregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int     np;
    float   dx[MAXPTS], dy[MAXPTS], bx[5], by[5];
    float   szmk, sztx, szwb, szws, szab, szah, totalsize;
/*---------------------------------------------------------------------*/

    *iret = 0;
    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        /* 
         * Get device coordiates for the symbol
         */
        cvg_todev2(el, &np, dx, dy, iret);

        /*
         * Get size info
         */
        gqsizd ( &szmk, &sztx, &szwb, &szws, &szab, &szah, iret );
        
        /*
         * Compute complete scaled size
         */
        totalsize = szws * el->elem.vol.info.size;

        bx[0] = dx[0] - totalsize/2.0;
        bx[1] = dx[0] + totalsize/2.0;
        bx[2] = dx[0] + totalsize/2.0;
        bx[3] = dx[0] - totalsize/2.0;
        bx[4] = bx[0];

        by[0] = dy[0] - totalsize/2.0;
        by[1] = dy[0] - totalsize/2.0;
        by[2] = dy[0] + totalsize/2.0;
        by[3] = dy[0] + totalsize/2.0;
        by[4] = by[0];

        cmd_osaddob(cvg_metadata, id, bx, by, 5, iret);
    }
}


static void cvg_wboxregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_wboxregpl
 * 
 * Processes the given VG element for use in the currently active CMD and CAP 
 * data structures.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 * S.Danz/AWC           07/06   Remove element(s) if marked deleted
 ****************************************************************************/
{
    int     np;
    float   dx[MAXPTS], dy[MAXPTS];
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.delete) {
        cvg_unregpl(id, el, iret);
    } else {
        cvg_todev2(el, &np, dx, dy, iret);

        /*
         * For closed elements, repeat the first point to the end. 
         */
        if ( el->hdr.closed && np < MAXPTS ) {
            dx[np]=dx[0];
            dy[np]=dy[0];
            np++;       
        }

        cmd_osaddob(cvg_metadata, id, dx, dy, np, iret);
    }
}


static void cvg_checkgrp(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_checkgrp
 * 
 * Checks if the given object is in a group, and if it is, checks if its one
 * of the ones allowed to be placed.  If it is allowed for placement, sees
 * if the object is the reference or the object to place and then looks to
 * see if the other half of the equation is in the CVG global group list.
 * If the object is used from the group list, it is removed.
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *  *el         VG_DBStruct     VG element to process for CMD/CAP
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           03/06   Created
 ****************************************************************************/
{
    int     grpid, found, index, subtype;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (el->hdr.grptyp != 0 && el->hdr.delete != 1) {
        if (cvg_cap_by_group[(int)el->hdr.grptyp].enabled) {
            cvg_subtyp(el, &subtype, iret);

            if (cvg_cap_by_group[(int)el->hdr.grptyp].obj_type == el->hdr.vg_type &&
                (cvg_cap_by_group[(int)el->hdr.grptyp].obj_subtype == subtype ||
                 cvg_cap_by_group[(int)el->hdr.grptyp].obj_subtype == -1)) {

                cvg_checkrefmatch(id, el->hdr.grptyp, el->hdr.grpnum, iret);
            } else if (cvg_cap_by_group[(int)el->hdr.grptyp].ref_type == el->hdr.vg_type &&
                (cvg_cap_by_group[(int)el->hdr.grptyp].ref_subtype == subtype ||
                cvg_cap_by_group[(int)el->hdr.grptyp].ref_subtype == -1)) {
                cvg_checkobjmatch(id, el->hdr.grptyp, el->hdr.grpnum, iret);
            }
        }
    } else if (el->hdr.grptyp != 0) {
        grpid = el->hdr.grpnum * 100 + el->hdr.grptyp;

        /*
         * If we are deleting a grouped element, be sure that its not still in
         * set of refs or objs waiting to be paired up, if so, take it out of
         * the set
         */
        for (index = 0, found = -1; 
                index < cvg_group_check->objs_used && found<0; index++) {
            if (cvg_group_check->objs[index].groupid == grpid &&
                cvg_group_check->objs[index].handle_id == id) {
                found = index;
            }
        }

        if (found >= 0) {
            cvg_group_check->objs_used--;
            if (found < cvg_group_check->objs_used) {
                memmove(&(cvg_group_check->objs[found]),
                        &(cvg_group_check->objs[found+1]),
                        sizeof(cvg_el_group_info)*
                        (cvg_group_check->objs_used - found)
                    );
            }
        }

        for (index = 0, found = -1; 
                index < cvg_group_check->refs_used && found<0; index++) {
            if (cvg_group_check->refs[index].groupid == grpid &&
                cvg_group_check->refs[index].handle_id == id) {
                found = index;
            }
        }

        if (found >= 0) {
            cvg_group_check->refs_used--;
            if (found < cvg_group_check->refs_used) {
                memmove(&(cvg_group_check->refs[found]),
                        &(cvg_group_check->refs[found+1]),
                        sizeof(cvg_el_group_info)*
                        (cvg_group_check->refs_used - found)
                    );
            }
        }
    }
}


static void cvg_unregpl(Handle id, VG_DBStruct *el, int *iret)
/*****************************************************************************
 * cvg_unregpl
 * 
 * Un-registers the object referenced by the id given from the CAP and CMD
 *
 * Input parameters:
 *  id          Handle          CMD/CAP handle coresponding to the VG element
 *                              to remove
 *  *el         VG_DBStruct     VG element to unregister
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           07/06   Created
 ****************************************************************************/
{
    Handle      ref, plid;
    Placement   place;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * First, remove it from the meta-data (it better be there)
     * Then, remove the handle from the placement (doesn't have to be there)
     * Last, check all the placement objects and any that use this object
     *       as the reference need to be removed since you can't place without
     *       a reference.
     */
    cmd_osdelob(cvg_metadata, id, iret);
    if (*iret == 0) {
        /*
         * Before we delete anything, if this was a grouped object
         * and it was placed, then we need to put its ref on the 
         * set of refs in the group check list so if/when the object
         * is added back, the two will pair up again.  
         */
        if (el->hdr.grptyp != 0) {
            cap_psgetpl(cvg_placements, id, &place, iret);
            if (place) {
                cap_plgetref(place, &ref, iret);
                if (ref) {
                    cvg_checkobjmatch(ref, el->hdr.grptyp, el->hdr.grpnum, iret);
                }
            }
        }

        cap_psdelpl(cvg_placements, id, iret);
        cap_psiterinit(cvg_placements, iret);

        cap_psiternext(cvg_placements, &place, iret);
        while (place) {
            cap_plgetref(place, &ref, iret);
            if (ref == id) {
                cap_plgetid(place, &plid, iret);
                cap_psdelpl(cvg_placements, plid, iret);
                /*
                 * Now that we dropped it from placement, if the element was
                 * a reference that was grouped, then this placed object needs
                 * to be put into the set of objects in the group check list 
                 * so if/when a reference is added back, the two will pair up 
                 * again.  The grpid of the ref and placed object were the 
                 * same, so we can use the ref's grpid to register with
                 */
                if (el->hdr.grptyp != 0) {
                    cvg_checkrefmatch(plid, el->hdr.grptyp, el->hdr.grpnum, iret);
                }
            }

            cap_psiternext(cvg_placements, &place, iret);
        }
    }
}


static void cvg_checkrefmatch(Handle obj, int grptyp, int grpnum, int *iret)
/*****************************************************************************
 * cvg_checkrefmatch
 * 
 * Checks if the grpid matches anything in the set of refs waiting for a
 * matching object.  If so, then register the id given with placement
 * and take the ref out of the set of pending refs.
 * If not, put the id in the set of objects waiting for a matching ref.
 *
 * Input parameters:
 *  obj         Handle          CMD/CAP handle to match with a ref
 *  grptyp      int             Group type for the object represented by id
 *  grpnum      int             Group number for the object represented by id
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           07/06   Created
 ****************************************************************************/
{
    int     index, found, grpid;
/*---------------------------------------------------------------------*/
    grpid = grpnum * 100 + grptyp;

    /*
     * See if we have a ref defined already 
     */
    for (index = 0, found = -1; 
            index < cvg_group_check->refs_used && found<0; index++) {
        if (cvg_group_check->refs[index].groupid == grpid) {
            found = index;
        }
    }

    /*
     * If we found the ref, register the placement object and
     * drop the ref from the list
     * NOTE: We are saying here there is a 1-to-1 relation between
     *       refs and objects to place.
     * Otherwise, put the object on the list of objects and hope
     * for a ref later on in the file
     */
    if (found >= 0) {
        cap_psaddpl(cvg_placements, 
                obj, 
                cvg_group_check->refs[found].handle_id, 
                cvg_cap_by_group[grptyp].inside, 
                cvg_cap_by_group[grptyp].both_sides, 
                cvg_cap_by_group[grptyp].attempts, 
                cvg_cap_by_group[grptyp].increment, 
                cvg_cap_by_group[grptyp].offset, 
                IMMEDIATE, 
                cvg_cap_by_group[grptyp].point_center, 
                iret
            );
        cvg_group_check->refs_used--;
        if (found < cvg_group_check->refs_used) {
            memmove(&(cvg_group_check->refs[found]),
                    &(cvg_group_check->refs[found+1]),
                    sizeof(cvg_el_group_info)*
                    (cvg_group_check->refs_used - found)
                );
        }
    } else {
        cvg_group_check->objs[cvg_group_check->objs_used].groupid = grpid;
        cvg_group_check->objs[cvg_group_check->objs_used].handle_id = obj;
        cvg_group_check->objs_used++;
    }
}


static void cvg_checkobjmatch(Handle ref, int grptyp, int grpnum, int *iret)
/*****************************************************************************
 * cvg_checkobjmatch
 * 
 * Checks if the grpid matches anything in the set of objects waiting for a
 * matching reference.  If so, then register the object found with placement
 * and take it out of the list of pending objects.
 * If not, put the ref in the set of refs waiting for a matching object.
 *
 * Input parameters:
 *  ref         Handle          CMD/CAP handle to match with an object
 *  grptyp      int             Group type for the object represented by id
 *  grpnum      int             Group number for the object represented by id
 *
 * Output parameters:
 *  *iret       int             Return code
 *                                 0 = Function successful
 *
 * Return value:
 *  None
 **
 * Log:
 * S.Danz/AWC           07/06   Created
 ****************************************************************************/
{
    int     index, found, grpid;
/*---------------------------------------------------------------------*/
    grpid = grpnum * 100 + grptyp;

    /*
     * See if an object is waiting for this ref
     */
    for (index = 0, found = -1; 
            index < cvg_group_check->objs_used && found<0; index++) {
        if (cvg_group_check->objs[index].groupid == grpid) {
            found = index;
        }
    }

    /*
     * If we found an object, register the placement object and
     * drop the object from the list
     * NOTE: We are saying here there is a 1-to-1 relation between
     *       refs and objects to place.
     * Otherwise, put the ref on the list of refs and hope
     * for an object later on in the file
     */
    if (found >= 0) {
        cap_psaddpl(cvg_placements, 
                cvg_group_check->objs[found].handle_id, 
                ref, 
                cvg_cap_by_group[grptyp].inside, 
                cvg_cap_by_group[grptyp].both_sides, 
                cvg_cap_by_group[grptyp].attempts, 
                cvg_cap_by_group[grptyp].increment, 
                cvg_cap_by_group[grptyp].offset, 
                IMMEDIATE, 
                cvg_cap_by_group[grptyp].point_center, 
                iret
            );
        cvg_group_check->objs_used--;
        if (found < cvg_group_check->objs_used) {
            memmove(&(cvg_group_check->objs[found]),
                    &(cvg_group_check->objs[found+1]),
                    sizeof(cvg_el_group_info)*
                    (cvg_group_check->objs_used - found)
                );
        }
    } else {
        cvg_group_check->refs[cvg_group_check->refs_used].groupid = grpid;
        cvg_group_check->refs[cvg_group_check->refs_used].handle_id = ref;
        cvg_group_check->refs_used++;
    }
}
