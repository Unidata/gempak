#include "geminc.h"
#include "gemprm.h"
#include "cescmn.h"


void ces_set ( int subtyp, VG_DBStruct *el, int *iret )
/************************************************************************
 * ces_set								*
 *									*
 * This function sets the settings for one element in the drawing 	*
 * table.								*
 *									*
 * ces_set ( subtyp, el, iret )						*
 *									*
 * Input parameters:							*
 *	subtyp		int		GEMPAK type for this element	*
 *	*el		VG_DBStruct	Vector data structure		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -2 = unable to modify settings	*
 **									*
 * Log:									*
 * E. Wehner/EAi	 8/97	Created					*
 * C. Lin/EAi		10/97	add WBOX_ELM				*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * C. Lin/EAi		11/97	add line width in WIND CLASS		*
 * S. Law/GSC		03/98   saves size and hdsiz for CLASS_WINDS    *
 * F.J. Yen/NCEP	 4/98	Rename from ces_chng. Clean up. Add text*
 * F.J. Yen/NCEP	 4/98	Set hdsiz for BARB_ELM			*
 * W. Li/EAI		04/98	Add DARR_ELM and HASH_ELM		*
 * W. Li/EAI		07/98	Added txt_attrib's text value setting	*
 * C. Lin/EAI		09/98	Added smooth level			*
 * A. Hardy/GSC		10/98   Added CMBSY_ELM                         *
 * A. Hardy/GSC		12/98   Added CIRCLE_ELM                        *
 * W. Li/EAI		03/99	Added latitude/longitude for symbols	*
 * S. Law/GSC		03/99	added filled and closed			*
 * W. Li/EAI		04/99	added MARK_ELM for symbols		*
 * S. Law/GSC		05/99	Added CLASS_TRACKS			*
 * S. Law/GSC		07/99	Added CLASS_SIGMETS			*
 * E. Safford/GSC	08/99   increase text size to MAX_TEXT		*
 * S. Law/GSC		08/99	added remaining SIGMETs			*
 * S. Law/GSC		02/00	added CCF				*
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * H. Zeng/EAI		02/01   added group type info.                  *
 * H. Zeng/EAI		03/01   modified to check group type id         *
 * E. Safford/SAIC	02/02	clean up group type, strcpy -> cst_ncpy	*
 * J. Wu/SAIC		11/02	add class LIST				*
 * H. Zeng/XTRIA	01/03   added marker info. for Watch            *
 * H. Zeng/XTRIA	03/03   changed para. of cst_ncpy		*
 * J. Wu/SAIC		09/03	add CLASS_MET->JET_ELM			*
 * J. Wu/SAIC		01/04	add CLASS_MET->GFA_ELM			*
 * B. Yin/SAIC		02/04	added CLASS_MET->TCA_ELM		*
 * J. Wu/SAIC		05/04   add barb/hash color into JET_ELM	*
 * J. Wu/SAIC		10/04   remove line width from GFA_ELM		*
 * T. Piper/SAIC	12/05	redone for new Sewtting_t structure	*
 * T. Piper/SAIC	12/05	fixed bug where splcol not set from el	*
 * B. Yin/SAIC		12/05	add line width for GFA			*
 * L. Hinson/AWC        01/12   Add CLASS_MET->SGWX_ELM                 *
 ***********************************************************************/
{
    char        grp[4], logstr[10], group_name[20], widthStr[32];
    int         loglev, grpid, ier1, ier2;
    int		indx, ii;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /* get the index of the element we are modifying */
    ces_getinx( el, subtyp, &indx, iret);

    if (*iret == -2)
    {
	loglev = 2;
	cst_ncpy (grp, "CES", sizeof(grp), &ier2);
	sprintf(logstr, "%i", subtyp);
	er_lmsg ( &loglev, grp, iret, logstr, &ier1, strlen(grp),
			strlen(logstr) );
	return;
    }

    /*
     * Copy the data from the passed in element to the array
     * member that contains the data for the element.
     */

    switch (el->hdr.vg_type)
    {
      case FRONT_ELM:
	set[indx].info.frt->fpipsz	= el->elem.frt.info.fpipsz;
	set[indx].info.frt->fpipst	= el->elem.frt.info.fpipst;
	set[indx].info.frt->fpipdr	= el->elem.frt.info.fpipdr;
	set[indx].info.frt->fcode	= el->elem.frt.info.fcode;
	set[indx].info.frt->fwidth	= el->elem.frt.info.fwidth;
	set[indx].smooth		= el->hdr.smooth;
	break;

      case CIRCLE_ELM:
	set[indx].info.cir->lintyp	= el->elem.cir.info.lintyp;
	set[indx].info.cir->lthw	= el->elem.cir.info.lthw;
	set[indx].info.cir->width	= el->elem.cir.info.width;
	set[indx].info.cir->lwhw	= el->elem.cir.info.lwhw;
	break;

      case LINE_ELM:
	set[indx].info.lin->lintyp	= el->elem.lin.info.lintyp;
	set[indx].info.lin->lthw	= el->elem.lin.info.lthw;
	set[indx].info.lin->width	= el->elem.lin.info.width;
	set[indx].info.lin->lwhw	= el->elem.lin.info.lwhw;
	set[indx].smooth		= el->hdr.smooth;
	set[indx].filled		= el->hdr.filled;
	set[indx].closed		= el->hdr.closed;
	break;

      case SPLN_ELM:
	set[indx].info.spl->spltyp	= el->elem.spl.info.spltyp;
	set[indx].info.spl->splstr	= el->elem.spl.info.splstr;
	set[indx].info.spl->spldir	= el->elem.spl.info.spldir;
	set[indx].info.spl->splsiz	= el->elem.spl.info.splsiz;
	set[indx].info.spl->splwid	= el->elem.spl.info.splwid;
	set[indx].smooth		= el->hdr.smooth;
	set[indx].filled		= el->hdr.filled;
	set[indx].closed		= el->hdr.closed;
	break;

      case WXSYM_ELM:
      case CTSYM_ELM:
      case ICSYM_ELM:
      case PTSYM_ELM:
      case PWSYM_ELM:
      case SKSYM_ELM:
      case SPSYM_ELM:
      case TBSYM_ELM:
      case CMBSY_ELM:
      case  MARK_ELM:
	set[indx].info.sym->info.width		= el->elem.sym.info.width;
	set[indx].info.sym->info.size		= el->elem.sym.info.size;    
	set[indx].info.sym->data.latlon[0]	= el->elem.sym.data.latlon[0];
	set[indx].info.sym->data.latlon[1]	= el->elem.sym.data.latlon[1];
	break;

      case ARROW_ELM:
      case BARB_ELM:
      case DARR_ELM:
      case HASH_ELM:
	set[indx].info.wnd->wndtyp = el->elem.wnd.info.wndtyp;
	set[indx].info.wnd->width  = el->elem.wnd.info.width;
	set[indx].info.wnd->size   = el->elem.wnd.info.size;
	set[indx].info.wnd->hdsiz  = el->elem.wnd.info.hdsiz;
	break;

      case WBOX_ELM:
	set[indx].info.wbx->w_type   = el->elem.wbx.info.w_type;
	set[indx].info.wbx->w_number = el->elem.wbx.info.w_number;
	set[indx].info.wbx->w_mrktyp = el->elem.wbx.info.w_mrktyp;
	set[indx].info.wbx->w_mrksiz = el->elem.wbx.info.w_mrksiz;
	set[indx].info.wbx->w_mrkwid = el->elem.wbx.info.w_mrkwid;
	break;

      case TEXT_ELM:
      case TEXTC_ELM:
	set[indx].info.txt->info.rotn   = el->elem.txt.info.rotn;
	set[indx].info.txt->info.sztext = el->elem.txt.info.sztext;
	set[indx].info.txt->info.itxfn  = el->elem.txt.info.itxfn;
	set[indx].info.txt->info.ithw   = el->elem.txt.info.ithw;
	set[indx].info.txt->info.iwidth = el->elem.txt.info.iwidth;
	set[indx].info.txt->info.ialign = el->elem.txt.info.ialign;
	strncpy(set[indx].info.txt->text, el->elem.txt.text, MAX_TEXT-1);
	set[indx].info.txt->text[MAX_TEXT-1] = '\0';
	break;

      case SPTX_ELM:
	set[indx].info.spt->text.info.rotn    = el->elem.spt.info.rotn;
	set[indx].info.spt->text.info.sztext  = el->elem.spt.info.sztext;
	set[indx].info.spt->text.info.sptxtyp = el->elem.spt.info.sptxtyp;
	set[indx].info.spt->text.info.turbsym = el->elem.spt.info.turbsym;
	set[indx].info.spt->text.info.itxfn   = el->elem.spt.info.itxfn;
	set[indx].info.spt->text.info.ithw    = el->elem.spt.info.ithw;
	set[indx].info.spt->text.info.iwidth  = el->elem.spt.info.iwidth;
	set[indx].info.spt->text.info.ialign  = el->elem.spt.info.ialign;
	strncpy(set[indx].info.spt->text.text, el->elem.spt.text, MAX_TEXT-1);
	set[indx].info.spt->text.text[MAX_TEXT-1] = '\0'; 
	break;

      case TRKSTORM_ELM:
	set[indx].info.trk->ltype1 = el->elem.trk.info.ltype1;
	set[indx].info.trk->ltype2 = el->elem.trk.info.ltype2;
	set[indx].info.trk->mtype1 = el->elem.trk.info.mtype1;
	set[indx].info.trk->mtype2 = el->elem.trk.info.mtype2;
	set[indx].info.trk->width  = el->elem.trk.info.width;
	set[indx].info.trk->incr   = el->elem.trk.info.incr;
	break;

      case SIGAIRM_ELM:
      case SIGCONV_ELM:
      case SIGINTL_ELM:
      case SIGNCON_ELM:
      case SIGOUTL_ELM:
	set[indx].info.sig->lintyp = el->elem.sig.info.lintyp;
	set[indx].info.sig->linwid = el->elem.sig.info.linwid;
	break;

      case SIGCCF_ELM:
	break;
      
      case LIST_ELM:
	set[indx].subtyp 	   = el->elem.lst.info.subtyp;
	set[indx].info.lst->subtyp = el->elem.lst.info.subtyp;
	set[indx].info.lst->mrktyp = el->elem.lst.info.mrktyp;
	set[indx].info.lst->mrksiz = el->elem.lst.info.mrksiz;
	set[indx].info.lst->mrkwid = el->elem.lst.info.mrkwid;
	break;

      case JET_ELM:
	for ( ii = 0; ii < MAX_JETPTS; ii++ ) {
	    set[indx].info.jet->barb[ii].wndcol = el->elem.jet.barb[ii].wndcol;
	    set[indx].info.jet->barb[ii].wnd.info.size = el->elem.jet.barb[ii].wnd.info.size;
	    set[indx].info.jet->barb[ii].wnd.info.width = el->elem.jet.barb[ii].wnd.info.width;
	    set[indx].info.jet->barb[ii].wnd.info.wndtyp = el->elem.jet.barb[ii].wnd.info.wndtyp;
	    set[indx].info.jet->barb[ii].wnd.info.hdsiz = el->elem.jet.barb[ii].wnd.info.hdsiz;
	        
	    set[indx].info.jet->barb[ii].sptcol = el->elem.jet.barb[ii].sptcol;
	    set[indx].info.jet->barb[ii].spt.info.sztext = el->elem.jet.barb[ii].spt.info.sztext;
	    set[indx].info.jet->barb[ii].spt.info.itxfn = el->elem.jet.barb[ii].spt.info.itxfn;
	    set[indx].info.jet->barb[ii].spt.info.ithw = el->elem.jet.barb[ii].spt.info.ithw;
	    set[indx].info.jet->barb[ii].spt.info.iwidth= el->elem.jet.barb[ii].spt.info.iwidth;
	    set[indx].info.jet->barb[ii].spt.info.ialign = el->elem.jet.barb[ii].spt.info.ialign;
	    set[indx].info.jet->barb[ii].spt.info.rotn = el->elem.jet.barb[ii].spt.info.rotn;
	    set[indx].info.jet->barb[ii].spt.info.sptxtyp = el->elem.jet.barb[ii].spt.info.sptxtyp;
	    set[indx].info.jet->barb[ii].spt.info.turbsym = el->elem.jet.barb[ii].spt.info.turbsym;
	    set[indx].info.jet->barb[ii].spt.info.txtcol = el->elem.jet.barb[ii].spt.info.txtcol;
	    set[indx].info.jet->barb[ii].spt.info.filcol = el->elem.jet.barb[ii].spt.info.filcol;
	    set[indx].info.jet->barb[ii].spt.info.lincol = el->elem.jet.barb[ii].spt.info.lincol;

	    strncpy(set[indx].info.jet->barb[ii].spt.text, el->elem.jet.barb[ii].spt.text, MAX_TEXT-1);
	    set[indx].info.jet->barb[ii].spt.text[MAX_TEXT-1] = '\0';

	    set[indx].info.jet->hash[ii].wndcol = el->elem.jet.hash[ii].wndcol;
	    set[indx].info.jet->hash[ii].wnd.info.size = el->elem.jet.hash[ii].wnd.info.size;
	    set[indx].info.jet->hash[ii].wnd.info.width = el->elem.jet.hash[ii].wnd.info.width;
	    set[indx].info.jet->hash[ii].wnd.info.wndtyp = el->elem.jet.hash[ii].wnd.info.wndtyp;
	    set[indx].info.jet->hash[ii].wnd.info.hdsiz = el->elem.jet.hash[ii].wnd.info.hdsiz;
	}
	
	set[indx].smooth = el->hdr.smooth;
	set[indx].filled = el->hdr.filled;
	set[indx].closed = el->hdr.closed;
	set[indx].info.jet->splcol = el->elem.jet.line.splcol;
	set[indx].info.jet->line.spltyp = el->elem.jet.line.spl.info.spltyp;
	set[indx].info.jet->line.splsiz = el->elem.jet.line.spl.info.splsiz;
	set[indx].info.jet->line.splwid = el->elem.jet.line.spl.info.splwid;
	set[indx].info.jet->line.splstr = el->elem.jet.line.spl.info.splstr;
	set[indx].info.jet->line.spldir = el->elem.jet.line.spl.info.spldir;
	break;

      case GFA_ELM:

	widthStr[0] = '\0';
	cvg_getFld ( el, TAG_GFA_LINEWIDTH, widthStr, &ier1 );

	/*
	 * If no line width, put a default value.
	 */
	if ( ier1 != 0 || strlen( widthStr ) == (size_t)0 ) {
	   set[indx].info.gfa->linwid = 2;
	}
	else {
	   set[indx].info.gfa->linwid = atoi ( widthStr );
	}

	break;

      case SGWX_ELM:
        set[indx].info.sgwx->lineelm = el->elem.sgwx.info.lineelm;
        set[indx].info.sgwx->linetype = el->elem.sgwx.info.linetype;
        set[indx].info.sgwx->linewidth = el->elem.sgwx.info.linewidth;
	set[indx].info.sgwx->szarrow = el->elem.sgwx.info.szarrow;
        break;
	
      case TCA_ELM:
	break;

      default:
	loglev = 2;
	cst_ncpy (grp, "CES", sizeof(grp), &ier2);
	sprintf(logstr, "%i", subtyp);
	*iret = -2;
	er_lmsg ( &loglev, grp, iret, logstr, &ier1, strlen(grp),
			strlen(logstr) );
	break;
    }

    if (*iret >= 0) {
	set[indx].maj_col = el->hdr.maj_col;
	set[indx].min_col = el->hdr.min_col;

	/*
	 *  Save group type in the set[] record
	 */
        grpid = (int)(el->hdr.grptyp);
	if (grpid >= 0) {
	    ces_gtgnam(grpid, group_name, &ier2);
	}

	if (grpid >= 0 && ier2 >= 0) {
	    cst_ncpy (set[indx].grp_typ, group_name, 
			sizeof(set[indx].grp_typ)-1, &ier2);
	}
	else {
	    set[indx].grp_typ[0] = '\0';
	}

    }

}
