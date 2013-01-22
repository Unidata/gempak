#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cescmn.h"


void ces_get ( int subtyp, VG_DBStruct *el, int *iret )
/************************************************************************
 * ces_get								*
 *									*
 * This function gets settings for a type of element into the header	*
 * of the passed in element.  The passed in element is also queried to	*
 * determine what type of element to retrieve settings for.		*
 *									*
 * ces_get ( subtyp, el, iret )						*
 *									*
 * Input parameters:							*
 *	subtyp		int		Element subtype			*
 *									*
 * Input/Output parameters:						*
 *	*el		VG_DBStruct	Element as mask and returned	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -2 = Unable to get settings	*
 **									*
 * Log:									*
 * E. Wehner/EAi	 8/97	Created					*
 * C. Lin/EAi		10/97	Add WBOX_ELM				*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * C. Lin/EAi		11/97	use CLASS_WATCHES for WBOX_ELM		*
 * C. Lin/EAi		11/97	add front strenth, wind line width	*
 * S. Law/GSC		03/98	loads size and hdsiz for CLASS_WINDS	*
 * F.J. Yen/NCEP	 4/98	Rename from ces_elset.Clean up. Add text*
 * F.J. Yen/NCEP	 4/98	Set hsiz for BARB_ELM			*
 * W. Li/EAI		04/98	Add DARR and HASH in CLASS_WIND		*
 * W. Li/EAI		07/98	Added txt_attrib's text value setting	*
 * C. Lin/EAI		09/98	Added smoothing level hdr.unused1	*
 * A. Hardy/GSC		12/98	Added CLASS_CIRCLE			*
 * W. Li/EAI		03/99	Added latitude/longitude for symbols	*
 * S. Law/GSC		03/99	added filled and closed			*
 * S. Law/GSC		05/99	Added CLASS_TRACKS			*
 * S. Law/GSC		07/99	Added CLASS_SIGMETS			*
 * S. Law/GSC		08/99	updated element				*
 * S. Law/GSC		02/00	added CFF catch to SIGMETS		*
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * H. Zeng/EAI		02/01	added group type info.			*
 * H. Zeng/EAI		03/01	modified to allow invalid group name	*
 * J. Wu/SAIC		11/02	add class LIST				*
 * H. Zeng/XTRIA	01/03	added marker info. for Watch		*
 * H. Zeng/XTRIA	03/03	special handling for grp_typ		*
 * H. Zeng/XTRIA	07/03	added VOLC_ELM and ASHCLD_ELM		*
 * J. Wu/SAIC		09/03	add CLASS_MET -> JET_ELM		*
 * J. Wu/SAIC		01/04	add CLASS_MET -> GFA_ELM		*
 * B. Yin/SAIC		02/04	added CLASS_MET -> TCA_ELM		*
 * J. Wu/SAIC		10/04	remove line width from GFA_ELM		*
 * T. Piper/SAIC	12/05	redone for NEW Setting_t structure	*
 * B. Yin/SAIC		12/05	add line width for GFA			*
 * B. Yin/SAIC		07/06	add line type and line element for GFA	*
 * L. Hinson/AWC        12/06   add text color, size, font, hw, width,  *
 *                              alignment for GFA                       *
 * L. Hinson/AWC        06/07   add arrow size for GFA                  *
 * L. Hinson/AWC        07/09   add fills, linetype, szarrow, text size *
 *                              font, hw, width, alignment, and text    *
 *                              Layout to CCF                           *
 * L. Hinson/AWC        01/12   Add CLASS_MET -> SGWX_ELM               *
 ***********************************************************************/
{
    char	logstr[10], grp[4], grp_typ[25], *ptr, widthStr[8];
    char	lineType[ 8 ], lineElm[ 8 ];
    int		ier1, ier2, ii, indx, loglev, grpid;
    char        arrowSize[6];
    char        txtColr[2], txtFont[2], txtHw[2], txtWidth[2];
    char        txtAlign[2], txtSize[6];

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    /* 
     * Retrieve the index to this type of element in the settings
     * array by making a call to ces_getinx.
     */
    ces_getinx(el, subtyp, &indx, iret);
    if (*iret == -2)
    {
	loglev = 2;
	strcpy(grp, "CES");
	sprintf(logstr, "%i", subtyp);
	er_lmsg ( &loglev, grp, iret, logstr, &ier1, strlen(grp),
			strlen(logstr) );
	return;
    }

    /* 
     * Using the index, retrieve the settings for the element from
     * the settings array.  Key off the vg_type field in the VG
     * element.  
     */
    el->hdr.maj_col = set[indx].maj_col;
    el->hdr.min_col = set[indx].min_col;
    el->hdr.smooth  = 0;

    strcpy ( grp_typ, set[indx].grp_typ );
    ptr = strtok ( grp_typ, "/" );

    ces_gtgid( ptr, &grpid, &ier2 );
    if( ier2 == 0 ) {
      el->hdr.grptyp  = (char)grpid;
    }
    else {
      el->hdr.grptyp  = NON_GRPID;
    }


    switch (el->hdr.vg_class) {

      case CLASS_FRONTS:

	/* FRONT_ELM */
	el->elem.frt.info.fcode		= set[indx].subtyp;
	el->elem.frt.info.fpipsz	= set[indx].info.frt->fpipsz;
	el->elem.frt.info.fpipst	= set[indx].info.frt->fpipst;
	el->elem.frt.info.fpipdr	= set[indx].info.frt->fpipdr;
	el->elem.frt.info.fwidth	= set[indx].info.frt->fwidth;
	el->hdr.smooth			= set[indx].smooth;
	break;

      case CLASS_WATCHES:

	/* WBOX_ELM */
	el->elem.wbx.info.w_type	= set[indx].info.wbx->w_type;
	el->elem.wbx.info.w_number	= set[indx].info.wbx->w_number;
	el->elem.wbx.info.w_mrktyp	= set[indx].info.wbx->w_mrktyp;
	el->elem.wbx.info.w_mrksiz	= set[indx].info.wbx->w_mrksiz;
	el->elem.wbx.info.w_mrkwid	= set[indx].info.wbx->w_mrkwid;
	break;

      case CLASS_CIRCLE:

	/* CIRCLE_ELM */
	el->elem.cir.info.lintyp	= set[indx].info.cir->lintyp;
	el->elem.cir.info.lthw		= set[indx].info.cir->lthw;
	el->elem.cir.info.width		= set[indx].info.cir->width;
	el->elem.cir.info.lwhw		= set[indx].info.cir->lwhw;
	break;

      case CLASS_LINES:

	switch (el->hdr.vg_type) {
	  case LINE_ELM:
	    el->elem.lin.info.lintyp	= set[indx].info.lin->lintyp;
	    el->elem.lin.info.lthw	= set[indx].info.lin->lthw;
	    el->elem.lin.info.width	= set[indx].info.lin->width;
	    el->elem.lin.info.lwhw	= set[indx].info.lin->lwhw;
	    el->hdr.smooth		= set[indx].smooth;
	    el->hdr.filled		= set[indx].filled;
	    el->hdr.closed		= set[indx].closed;
	    break;

	  case SPLN_ELM:
	    el->elem.spl.info.spltyp	= set[indx].info.spl->spltyp;
	    el->elem.spl.info.splstr	= set[indx].info.spl->splstr;
	    el->elem.spl.info.spldir	= set[indx].info.spl->spldir;
	    el->elem.spl.info.splsiz	= set[indx].info.spl->splsiz;
	    el->elem.spl.info.splwid	= set[indx].info.spl->splwid;
	    el->hdr.smooth		= set[indx].smooth;
	    el->hdr.filled		= set[indx].filled;
	    el->hdr.closed		= set[indx].closed;
	    break;

	  default:
	    *iret = -2;
	    break;
	    }
	break;

      case CLASS_SYMBOLS:

	el->elem.sym.info.width 	= set[indx].info.sym->info.width;
	el->elem.sym.info.size		= set[indx].info.sym->info.size;
	el->elem.sym.data.latlon[0]	= set[indx].info.sym->data.latlon[0];
	el->elem.sym.data.latlon[1]	= set[indx].info.sym->data.latlon[1];
	break;

      case CLASS_WINDS:

	switch (el->hdr.vg_type) {
	  case ARROW_ELM:		/* WIND ARROWS */
	  case BARB_ELM:		/* WIND BARBS */
	  case DARR_ELM:		/* DIRECTION */
	  case HASH_ELM:		/* HASH MAKE */		
	    el->elem.wnd.info.wndtyp	= set[indx].info.wnd->wndtyp;
	    el->elem.wnd.info.width	= set[indx].info.wnd->width;
	    el->elem.wnd.info.size	= set[indx].info.wnd->size;
	    el->elem.wnd.info.hdsiz	= set[indx].info.wnd->hdsiz;
	    break;

	  default:
	    *iret = -2;
	    break;
	}
	break;

      case CLASS_TEXT:

	switch (el->hdr.vg_type) {
	  case TEXT_ELM:
	  case TEXTC_ELM:
	    el->elem.txt.info.rotn	= set[indx].info.txt->info.rotn;
	    el->elem.txt.info.sztext	= set[indx].info.txt->info.sztext;
	    el->elem.txt.info.itxfn	= set[indx].info.txt->info.itxfn;
	    el->elem.txt.info.ithw	= set[indx].info.txt->info.ithw;
	    el->elem.txt.info.iwidth	= set[indx].info.txt->info.iwidth;
	    el->elem.txt.info.ialign	= set[indx].info.txt->info.ialign;
	    strcpy(el->elem.txt.text, set[indx].info.txt->text);
	    break;

	  case SPTX_ELM:
	    el->elem.spt.info.rotn	= set[indx].info.spt->text.info.rotn; 
	    el->elem.spt.info.sztext	= set[indx].info.spt->text.info.sztext;
	    el->elem.spt.info.sptxtyp	= set[indx].info.spt->text.info.sptxtyp;
	    el->elem.spt.info.turbsym	= set[indx].info.spt->text.info.turbsym;
	    el->elem.spt.info.itxfn	= set[indx].info.spt->text.info.itxfn;
	    el->elem.spt.info.ithw	= set[indx].info.spt->text.info.ithw;
	    el->elem.spt.info.iwidth	= set[indx].info.spt->text.info.iwidth;
	    el->elem.spt.info.ialign	= set[indx].info.spt->text.info.ialign;
	    strcpy(el->elem.spt.text, set[indx].info.spt->text.text);
	    break;

	  default:
	    *iret = -2;
	    break;
	}
	break;

      case CLASS_TRACKS:
      
	el->elem.trk.info.ltype1	= set[indx].info.trk->ltype1;
	el->elem.trk.info.ltype2	= set[indx].info.trk->ltype2;
	el->elem.trk.info.mtype1	= set[indx].info.trk->mtype1;
	el->elem.trk.info.mtype2	= set[indx].info.trk->mtype2;
	el->elem.trk.info.width		= set[indx].info.trk->width;
	el->elem.trk.info.incr		= set[indx].info.trk->incr;
	break;

      case CLASS_SIGMETS:

	if (el->hdr.vg_type == VOLC_ELM) {
	    el->elem.vol.info.width	= set[indx].info.vol->width;
	    el->elem.vol.info.size	= set[indx].info.vol->size;
	}
	else if (el->hdr.vg_type != SIGCCF_ELM &&
		 el->hdr.vg_type != ASHCLD_ELM    ) {
	    el->elem.sig.info.lintyp	= set[indx].info.sig->lintyp;
	    el->elem.sig.info.linwid	= set[indx].info.sig->linwid;

	    el->elem.sig.info.status	= 0;
	    el->elem.sig.info.seqnum	= 0;

	    strcpy (el->elem.sig.info.remarks, "");
	} else if (el->hdr.vg_type == SIGCCF_ELM) {
          el->elem.ccf.info.fillhi = set[indx].info.ccf->fillhi;
          el->elem.ccf.info.fillmed = set[indx].info.ccf->fillmed;
          el->elem.ccf.info.filllow = set[indx].info.ccf->filllow;
          el->elem.ccf.info.linetype = set[indx].info.ccf->linetype;
          el->elem.ccf.info.szarrow = set[indx].info.ccf->szarrow;
          el->elem.ccf.spt.info.sztext = set[indx].info.ccf->info.sztext;
          el->elem.ccf.spt.info.itxfn = set[indx].info.ccf->info.itxfn;
          el->elem.ccf.spt.info.ithw = set[indx].info.ccf->info.ithw;
          el->elem.ccf.spt.info.iwidth = set[indx].info.ccf->info.iwidth;
          el->elem.ccf.spt.info.ialign = set[indx].info.ccf->info.ialign;
          strcpy(el->elem.ccf.textLayout, set[indx].info.ccf->textLayout);
        }
	break;

      case CLASS_LIST:
      
	el->elem.lst.info.subtyp	= set[indx].info.lst->subtyp;
	el->elem.lst.info.mrktyp	= set[indx].info.lst->mrktyp;
	el->elem.lst.info.mrksiz	= set[indx].info.lst->mrksiz;
	el->elem.lst.info.mrkwid	= set[indx].info.lst->mrkwid;
	break;

      case CLASS_MET:

	switch (el->hdr.vg_type) {
	  case JET_ELM:
	    for ( ii = 0; ii < MAX_JETPTS; ii++ ) {
		el->elem.jet.barb[ii].wndcol		= set[indx].info.jet->barb[ii].wndcol;
		el->elem.jet.barb[ii].wnd.info.size	= set[indx].info.jet->barb[ii].wnd.info.size;
		el->elem.jet.barb[ii].wnd.info.width	= set[indx].info.jet->barb[ii].wnd.info.width;
		el->elem.jet.barb[ii].wnd.info.wndtyp	= set[indx].info.jet->barb[ii].wnd.info.wndtyp;
		el->elem.jet.barb[ii].wnd.info.hdsiz	= set[indx].info.jet->barb[ii].wnd.info.hdsiz;

		el->elem.jet.barb[ii].sptcol		= set[indx].info.jet->barb[ii].sptcol;
		el->elem.jet.barb[ii].spt.info.sztext	= set[indx].info.jet->barb[ii].spt.info.sztext;
		el->elem.jet.barb[ii].spt.info.itxfn	= set[indx].info.jet->barb[ii].spt.info.itxfn;
		el->elem.jet.barb[ii].spt.info.ithw	= set[indx].info.jet->barb[ii].spt.info.ithw;
		el->elem.jet.barb[ii].spt.info.iwidth	= set[indx].info.jet->barb[ii].spt.info.iwidth;
		el->elem.jet.barb[ii].spt.info.ialign	= set[indx].info.jet->barb[ii].spt.info.ialign;
		el->elem.jet.barb[ii].spt.info.rotn	= set[indx].info.jet->barb[ii].spt.info.rotn;
		el->elem.jet.barb[ii].spt.info.sptxtyp	= set[indx].info.jet->barb[ii].spt.info.sptxtyp;
		el->elem.jet.barb[ii].spt.info.turbsym	= set[indx].info.jet->barb[ii].spt.info.turbsym;
		el->elem.jet.barb[ii].spt.info.txtcol	= set[indx].info.jet->barb[ii].spt.info.txtcol;
		el->elem.jet.barb[ii].spt.info.filcol	= set[indx].info.jet->barb[ii].spt.info.filcol;
		el->elem.jet.barb[ii].spt.info.lincol	= set[indx].info.jet->barb[ii].spt.info.lincol;

		strcpy(el->elem.jet.barb[ii].spt.text, set[indx].info.jet->barb[ii].spt.text);

		el->elem.jet.hash[ii].wndcol		= set[indx].info.jet->hash[ii].wndcol;
		el->elem.jet.hash[ii].wnd.info.size	= set[indx].info.jet->hash[ii].wnd.info.size;
		el->elem.jet.hash[ii].wnd.info.width	= set[indx].info.jet->hash[ii].wnd.info.width;
		el->elem.jet.hash[ii].wnd.info.wndtyp	= set[indx].info.jet->hash[ii].wnd.info.wndtyp;
		el->elem.jet.hash[ii].wnd.info.hdsiz	= set[indx].info.jet->hash[ii].wnd.info.hdsiz;
	    }
	    el->hdr.smooth			= set[indx].smooth;
	    el->hdr.filled			= set[indx].filled;
	    el->hdr.closed			= set[indx].closed;
	    el->elem.jet.line.splcol		= set[indx].info.jet->splcol;
	    el->elem.jet.line.spl.info.spltyp	= set[indx].info.jet->line.spltyp;
	    el->elem.jet.line.spl.info.splsiz	= set[indx].info.jet->line.splsiz;
	    el->elem.jet.line.spl.info.splwid	= set[indx].info.jet->line.splwid;
	    el->elem.jet.line.spl.info.splstr	= set[indx].info.jet->line.splstr;
	    el->elem.jet.line.spl.info.spldir	= set[indx].info.jet->line.spldir;
	    break;
            
	  case GFA_ELM:

	      sprintf ( widthStr, "%d", set[indx].info.gfa->linwid );
	      cvg_setFld ( el, TAG_GFA_LINEWIDTH, widthStr, &ier1 );

	      sprintf ( lineElm, "%d", set[indx].info.gfa->linelm );
	      cvg_setFld ( el, TAG_GFA_LINELM, lineElm, &ier1 );

	      sprintf ( lineType, "%d", set[indx].info.gfa->lintyp );
	      cvg_setFld ( el, TAG_GFA_LINTYP, lineType, &ier1 );
              
              sprintf ( arrowSize, "%4.2f", set[indx].info.gfa->szarrow );
              cvg_setFld ( el, TAG_GFA_ARROWSZ, arrowSize, &ier1 );

              sprintf ( txtColr, "%d", set[indx].info.gfa->info.txtcol );
              cvg_setFld ( el, TAG_GFA_TXTCLR, txtColr, &ier1 );
              
              sprintf ( txtSize, "%4.2f", set[indx].info.gfa->info.sztext );
              cvg_setFld ( el, TAG_GFA_TXTSZ, txtSize, &ier1 );
              
              sprintf ( txtFont, "%d", set[indx].info.gfa->info.itxfn );
              cvg_setFld ( el, TAG_GFA_TXTFN, txtFont, &ier1 );
              
              sprintf ( txtHw, "%d", set[indx].info.gfa->info.ithw);
              cvg_setFld ( el, TAG_GFA_TXTHW, txtHw, &ier1 );
              
              sprintf ( txtWidth, "%d", set[indx].info.gfa->info.iwidth);
              cvg_setFld ( el, TAG_GFA_TXTWDTH, txtWidth, &ier1 );
              
              sprintf ( txtAlign, "%d", set[indx].info.gfa->info.ialign);
              cvg_setFld ( el, TAG_GFA_TXTALGN, txtAlign, &ier1 );
              
              cvg_setFld ( el, TAG_GFA_TXTLYT, set[indx].info.gfa->textLayout,
                           &ier1 );

	      break;

          case SGWX_ELM:
            el->elem.sgwx.info.lineelm  = set[indx].info.sgwx->lineelm;
            el->elem.sgwx.info.linetype = set[indx].info.sgwx->linetype;
            el->elem.sgwx.info.linewidth = set[indx].info.sgwx->linewidth;
            el->elem.sgwx.info.szarrow = set[indx].info.sgwx->szarrow;
            el->elem.sgwx.spt.info.sztext = set[indx].info.sgwx->info.sztext;
            el->elem.sgwx.spt.info.itxfn = set[indx].info.sgwx->info.itxfn;
            el->elem.sgwx.spt.info.ithw = set[indx].info.sgwx->info.ithw;
            el->elem.sgwx.spt.info.iwidth = set[indx].info.sgwx->info.iwidth;
            el->elem.sgwx.spt.info.ialign = set[indx].info.sgwx->info.ialign;
            el->elem.sgwx.spt.info.filcol = 0;
            el->elem.sgwx.spt.info.txtcol = set[indx].maj_col;
            el->hdr.smooth = 1;
            el->hdr.closed = 1;
            break;
	    
	  case TCA_ELM:
	      break;

	  default:
	    *iret = -2;
	    break;
	}  /*  End of  'switch (el->hdr.vg_type)'  */
	break;

      default:
	*iret = -2;
	break;
    }  /*  End of  'switch (el->hdr.vg_class)'  */
    
    if (*iret !=0) {
      loglev = 2;
      strcpy(grp, "CES");
      sprintf(logstr, "%i", subtyp);
      er_lmsg ( &loglev, grp, iret, logstr, &ier1, strlen(grp),
                strlen(logstr) );
    }

}
