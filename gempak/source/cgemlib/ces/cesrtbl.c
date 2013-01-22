#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#define CES_GLOBAL
#include "cescmn.h"


void ces_rtbl ( int *iret )
/************************************************************************
 * ces_rtbl								*
 *									*
 * This function reads the setting table and loads the settings		*
 * structure.								*
 *									*
 * ces_rtbl ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  2 = Bad read.  Ignore record. *
 *					  1 = Setting array siz exceeded*
 *					 -1 = No setting table		*
 **									*
 * Log:									*
 * D. Keiser/GSC	 7/97	Rewrote using union structure		*
 * E. Wehner/EAi	 8/97	Adopted from cpg_rstng			*
 * C. Lin/EAi		 9/97	Change default table name		*
 * G. Krueger/EAI	10/97	CST_xLST: Removed RSPTB; Add str limit	*
 * C. Lin/EAi		10/97	modify to remove cst_clst(), cleanup	*
 * C. Lin/EAi	 	10/97	modify WBOX_ELM				*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * C. Lin/EAi	 	11/97	use CLASS_WATCHES for WBOX_ELM		*
 * C. Lin/EAi	 	11/97	add front strength and wind line width	*
 * S. Law/GSC		03/98	Changed scanned values for CLASS_WINDS  *
 * F. J. Yen/NCEP	 4/98	Renamed from ces_rstng.  Cleaned up.	*
 * F. J. Yen/NCEP	 4/98	Set hdsiz for BARB_ELM to 1.		*
 * W. Li/EAI		04/98	Add darr and hash in CLASS_WINDS	*
 * S. Jacobs/NCEP	 6/98	Changed front pip size to type float	*
 * W. Li/EAI		07/98	Added txt_attrib's text value setting	*
 * C. Lin/EAI		09/98	Add smoothing level for line/front	*
 * A. Hardy/GSC		10/98	Added CMBSY_ELM                         *
 * S. Jacobs/NCEP	12/98	Fixed typo				*
 * A. Hardy/GSC		12/98	Added CIRCLE_ELM                        *
 * E. Safford/GSC	02/99	set default for wndtyp to 114 (cleared) *
 * W. Li/EAI		03/99	added latitude/longitude for symbols	*
 * S. Law/GSC		03/99	added filled/closed = 0			*
 * W. Li/EAI		04/99	added MARK_ELM, removed lat/long symbol *
 * S. Law/GSC		05/99	Added CLASS_TRACKS			*
 * S. Law/GSC		07/99	Added CLASS_SIGMETS			*
 * S. Law/GSC		08/99	added remaining SIGMETs			*
 * S. Law/GSC		02/00	added CCF				*
 * H. Zeng/EAI		02/01	added group type info.			*
 * E. Safford/SAIC	02/02	added initialization for new_subtyp	*
 * J. Wu/SAIC		11/02	add class LIST				*
 * M. Li/SAIC		01/03	delete vgstruct.h			*
 * H. Zeng/XTRIA	01/03	added marker info. for Watch		*
 * H. Zeng/XTRIA	03/03	added layer_flag			*
 * D.W.Plummer/NCEP	06/03	added ASHCLD_ELM and VOLC_ELM		*
 * J. Wu/SAIC		09/03	add CLASS_MET -> JET_ELM		*
 * J. Wu/SAIC		01/04	add CLASS_MET -> GFA_ELM		*
 * B. Yin/SAIC		02/04	added CLASS_MET -> TCA_ELM		*
 * J. Wu/SAIC		05/04	add barb/hash color into JET_ELM	*
 * J. Wu/SAIC		05/04	add initialization for "ppid"		*
 * H. Zeng/SAIC		07/04	added check for ialign value		*
 * J. Wu/SAIC		09/04	add text type for jet barb text		*
 * J. Wu/SAIC		10/04   remove line width from GFA_ELM		*
 * T. Piper/SAIC	12/05	redone with new Setting_t structure	*
 * B. Yin/SAIC		12/05	added line width for GFA		*
 * S. Jacobs		03/06	add initialization of special text (bug)*
 * S. Danz/AWC		04/06	initialized jet->barb[]spt.text		*
 * B. Yin/SAIC		07/06	added line type and line elem for GFA	*
 * L. Hinson/AWC        12/06   added text color, size, font, hw, width *
 *                              alignment, and text Layout for GFA      *
 * L. Hinson/AWC        06/07   added arrow size for GFA                *
 * L. Hinson/AWC        07/09   Add color, fills, linetype, szarrow,    *
 *                              text size, font, hw, width, alignment,  *
 *                              and text Layout to CCF                  *
 * L. Hinson/AWC        01/12   Add CLASS_MET -> SGWX_ELM               *
 ***********************************************************************/
{
    char	tstr[256], vg_classstr[64], vg_typestr[64], tmpstr[256];
    int		one=1, smooth, loglev, quit, ier1, ier, jj, align_val;
    int		ii, tmpwid[3], tmpclr[3], ityp, ifnt, ithw, ialign, nn;
    char	grp[4], *ptr;
    float	tmpsiz, tmpsz[3];
    FILE	*fp;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * Open the setting table.  If not found, return an error.
     */

    if ( (fp = (FILE * )cfl_tbop(SETTING_TBL, "pgen", &ier)) == NULL) {
        *iret = -1;
	loglev = 2;
	strcpy(grp, "CES");
	er_lmsg (&loglev, grp, iret, SETTING_TBL, &ier1, strlen(grp),
		 strlen(SETTING_TBL));
        return;
    }

    /* 
     * For every line in the setting table list, read in the record,
     * parse out the fields, and compare to input type.
     */
    jj = num_set = 0;
    quit = G_FALSE;
    while ( ( !quit ) && ( jj < MAX_SET ) ) {

	cfl_trln ( fp, sizeof(tstr), tstr, &ier );

	if ( ier == 4 ) {
	    /*
	     * Here for end of file.
	     */
	    quit = G_TRUE;
	}

	else if ( ier != 0 ) {
	    /*
	     * Here for a bad read; record is ignored.
	     */
	    loglev = 2;
	    strcpy(grp, "CES");
	    ier = 2;
	    er_lmsg ( &loglev, grp, &ier, SETTING_TBL, &ier1,
		      strlen(grp), strlen(SETTING_TBL) );
	}

	else {
	    /*
	     * Here to process a good record.
	     */
	    sscanf(tstr, "%s %s", vg_classstr, vg_typestr);
	    num_set++;
	    G_REALLOC(set, Setting_t, num_set, "set");
	    set[jj].smooth = 0;
	    set[jj].filled = 0;
	    set[jj].closed = 0;

    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE FRONTS CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */

	    if ( strcmp(vg_classstr, "CLASS_FRONTS" ) == 0 ) {
		set[jj].vg_class = CLASS_FRONTS;
		G_MALLOC(set[jj].info.frt, FrontInfo, one, "set[jj].info.frt");

		sscanf(tstr, "%*s %*s %d %d %d %d %f %d %d %s",
			 &(set[jj].subtyp), &(set[jj].maj_col),
			 &(set[jj].min_col), &smooth, &tmpsiz,
			 &(set[jj].info.frt->fpipdr),
			 &(set[jj].info.frt->fwidth),
			   set[jj].grp_typ );
		
		set[jj].info.frt->fpipsz = (int)(tmpsiz * 100.0F);
		set[jj].info.frt->fcode = set[jj].subtyp;
		set[jj].info.frt->fpipst = 1;
	
		if ( smooth < 0 || smooth > 2 )
		    smooth = 0;
		set[jj].smooth = smooth;

		if ( strcmp(vg_typestr, "FRONT_ELM" ) == 0 ) {
		    set[jj].vg_type = FRONT_ELM;
		}
		else if ( strcmp(vg_typestr, "-99" ) == 0 ) {
		    set[jj].vg_type = DEFLTSET;
		}

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE CIRCLE CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_CIRCLE") == 0 ) {
		set[jj].vg_class = CLASS_CIRCLE;
		G_MALLOC(set[jj].info.cir, LineInfo, one, "set[jj].info.cir");

		if (strcmp(vg_typestr, "CIRCLE_ELM") == 0) {
		   set[jj].vg_type = CIRCLE_ELM;

		   sscanf(tstr, "%*s %*s %d %d %d %d %d %d %d %s", 
			 &(set[jj].subtyp), 
			 &(set[jj].maj_col), &(set[jj].min_col),
			 &(set[jj].info.cir->lintyp), 
			 &(set[jj].info.cir->lthw),
			 &(set[jj].info.cir->width), 
			 &(set[jj].info.cir->lwhw),
			   set[jj].grp_typ );

		}

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE LINES CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_LINES") == 0 ) {
		set[jj].vg_class = CLASS_LINES;

		if (strcmp(vg_typestr, "LINE_ELM") == 0) {
		    set[jj].vg_type = LINE_ELM;
		    G_MALLOC(set[jj].info.lin, LineInfo, one, "set[jj].info.lin");
		    sscanf(tstr, "%*s %*s %d %d %d %d %d %d %d %d %s",
			 &(set[jj].subtyp),
			 &(set[jj].maj_col), &(set[jj].min_col),
			 &smooth,
			 &(set[jj].info.lin->lintyp),
			 &(set[jj].info.lin->lthw),
			 &(set[jj].info.lin->width),
			 &(set[jj].info.lin->lwhw),
			   set[jj].grp_typ );

	        }
		else if (strcmp(vg_typestr, "SPLN_ELM") == 0) {
		    set[jj].vg_type = SPLN_ELM;
		    G_MALLOC(set[jj].info.spl, SpLineInfo, one, "set[jj].info.spl");
		    set[jj].info.spl->splstr = 1;

			sscanf(tstr, "%*s %*s %d %d %d %d %d %d %f %d %s",
			&(set[jj].subtyp), 
			&(set[jj].maj_col), &(set[jj].min_col),
			&smooth,
			&(set[jj].info.spl->spltyp), 
			&(set[jj].info.spl->spldir),
			&(set[jj].info.spl->splsiz), 
			&(set[jj].info.spl->splwid),
			set[jj].grp_typ );

		}

		if (smooth < 0 || smooth > 2)
			smooth = 0;
		set[jj].smooth = smooth;

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE SYMBOLS CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_SYMBOLS") == 0 ) {
		set[jj].vg_class = CLASS_SYMBOLS;
		G_MALLOC(set[jj].info.sym, SymType, one, "set[jj].info.sym");

		if ( strcmp(vg_typestr, "WXSYM_ELM") == 0) {
		    set[jj].vg_type = WXSYM_ELM;
		}
		else if ( strcmp(vg_typestr, "CTSYM_ELM") == 0) {
		    set[jj].vg_type = CTSYM_ELM;
		}
		else if ( strcmp(vg_typestr, "ICSYM_ELM") == 0) {
		    set[jj].vg_type = ICSYM_ELM;
		}
		else if ( strcmp(vg_typestr, "PTSYM_ELM") == 0) {
		    set[jj].vg_type = PTSYM_ELM;
		}
		else if ( strcmp(vg_typestr, "PWSYM_ELM") == 0) {
		    set[jj].vg_type = PWSYM_ELM;
		}
		else if ( strcmp(vg_typestr, "SKSYM_ELM") == 0) {
		    set[jj].vg_type = SKSYM_ELM;
		}
		else if ( strcmp(vg_typestr, "SPSYM_ELM") == 0) {
		    set[jj].vg_type = SPSYM_ELM;
		}
		else if ( strcmp(vg_typestr, "TBSYM_ELM") == 0) {
		    set[jj].vg_type = TBSYM_ELM;
		}
		else if ( strcmp(vg_typestr, "CMBSY_ELM") == 0) {
		    set[jj].vg_type = CMBSY_ELM;
		}
		else if ( strcmp(vg_typestr, "MARK_ELM" ) == 0 ) {
		    set[jj].vg_type = MARK_ELM;
		}
		else if (strcmp(vg_typestr, "-99" ) == 0 ) {
		    set[jj].vg_type = DEFLTSET;
		}
		sscanf(tstr, "%*s %*s %d %d %d %d %f %s", 
			&(set[jj].subtyp),
			&(set[jj].maj_col),
			&(set[jj].min_col),
			&(set[jj].info.sym->info.width),
			&(set[jj].info.sym->info.size),
			 set[jj].grp_typ );

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE WINDS CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_WINDS") == 0 ) {
		set[jj].vg_class = CLASS_WINDS;
		G_MALLOC(set[jj].info.wnd, WindInfo, one, "set[jj].info.wnd");

		if ( strcmp(vg_typestr, "ARROW_ELM") == 0 ) {
		    set[jj].vg_type = ARROW_ELM;
		    sscanf(tstr, "%*s %*s %d %d %d %d %f %f %s",
			&(set[jj].subtyp),
			&(set[jj].maj_col), &(set[jj].min_col),
			&(set[jj].info.wnd->width),
			&(set[jj].info.wnd->size),
			&(set[jj].info.wnd->hdsiz), set[jj].grp_typ );
		    set[jj].info.wnd->wndtyp = 114;
		}
		else if ( strcmp(vg_typestr, "BARB_ELM") == 0 ) {
		    set[jj].vg_type = BARB_ELM;
		    sscanf(tstr, "%*s %*s %d %d %d %d %f %s",
			 &(set[jj].subtyp), &(set[jj].maj_col),
			 &(set[jj].min_col), &(set[jj].info.wnd->width),
			 &(set[jj].info.wnd->size), set[jj].grp_typ );
		    set[jj].info.wnd->hdsiz = 1.0F;
		    set[jj].info.wnd->wndtyp = 114;

		}
		else if ( strcmp(vg_typestr, "DARR_ELM") == 0 ) {
		    set[jj].vg_type = DARR_ELM;
		    sscanf(tstr, "%*s %*s %d %d %d %d %f %f %s",
			&(set[jj].subtyp),
			&(set[jj].maj_col), &(set[jj].min_col),
			&(set[jj].info.wnd->width),
			&(set[jj].info.wnd->size),
			&(set[jj].info.wnd->hdsiz), set[jj].grp_typ );
		    set[jj].info.wnd->wndtyp = 114;
		}
		else if ( strcmp(vg_typestr, "HASH_ELM") == 0 ) {
		    set[jj].vg_type = HASH_ELM;
		    sscanf(tstr, "%*s %*s %d %d %d %d %f %s",
			&(set[jj].subtyp),
			&(set[jj].maj_col), &(set[jj].min_col),
			&(set[jj].info.wnd->width),
			&(set[jj].info.wnd->size),
			  set[jj].grp_typ );
		    set[jj].info.wnd->hdsiz = 1.0F;
		    set[jj].info.wnd->wndtyp = 1;
		}

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE WBOX CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_WATCHES") == 0 ) {
		set[jj].vg_class = CLASS_WATCHES;
		G_MALLOC(set[jj].info.wbx, WboxAttr, one, "set[jj].info.wbx");

		sscanf (tstr, "%*s %*s %d %d %d %d %d %f %d %s",
			&(set[jj].subtyp),
			&(set[jj].maj_col), &(set[jj].min_col),
			&(set[jj].info.wbx->w_type),
			&(set[jj].info.wbx->w_mrktyp),
			&(set[jj].info.wbx->w_mrksiz),
			&(set[jj].info.wbx->w_mrkwid), set[jj].grp_typ );

		set[jj].info.wbx->w_number = -9999;

		if (strcmp(vg_typestr, "WBOX_ELM") == 0) { 
		    set[jj].vg_type = WBOX_ELM;
		}
		else if (strcmp(vg_typestr, "-99") == 0) {
		    set[jj].vg_type = DEFLTSET;
		}

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE TEXT CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_TEXT") == 0 ) {
		set[jj].vg_class = CLASS_TEXT;

		if (strcmp(vg_typestr, "TEXT_ELM") == 0) {
		    set[jj].vg_type = TEXT_ELM;
		    G_MALLOC(set[jj].info.txt, TextType, one, "set[jj].info.txt");

		    sscanf (tstr, "%*s %*s %d %d %d %f %f %d %d %d %s", 
			&(set[jj].subtyp),
			&(set[jj].maj_col), &(set[jj].min_col),
			&(set[jj].info.txt->info.rotn),
			&(set[jj].info.txt->info.sztext),
			&(set[jj].info.txt->info.itxfn),
			&(set[jj].info.txt->info.iwidth), 
			&(set[jj].info.txt->info.ialign),
			  set[jj].grp_typ );

		    set[jj].info.txt->info.ithw = set[jj].subtyp;
		    set[jj].info.txt->text[0] = CHNULL;

		    /*
		     * Make sure ialign value is among (-1, 0, 1).
		     */
		    align_val = set[jj].info.txt->info.ialign;

		    if ( align_val != -1 && align_val != 0 &&
			align_val !=  1			) {

			set[jj].info.txt->info.ialign = 0;
		    }

		}
		else if (strcmp(vg_typestr, "TEXTC_ELM") == 0) {
		    set[jj].vg_type = TEXTC_ELM;
		    G_MALLOC(set[jj].info.txt, TextType, one, "set[jj].info.txt");

		    sscanf (tstr, "%*s %*s %d %d %d %f %f %d %d %d %s",
			&(set[jj].subtyp),
			&(set[jj].maj_col), &(set[jj].min_col),
			&(set[jj].info.txt->info.rotn),
			&(set[jj].info.txt->info.sztext),
			&(set[jj].info.txt->info.itxfn),
			&(set[jj].info.txt->info.iwidth),
			&(set[jj].info.txt->info.ialign),
			  set[jj].grp_typ );
		    set[jj].info.txt->info.ithw = set[jj].subtyp;
		    set[jj].info.txt->text[0] = CHNULL;

		    /*
		     * Make sure ialign value is among (-1, 0, 1).
		     */
		    align_val = set[jj].info.txt->info.ialign;

		    if ( align_val != -1 && align_val != 0 &&
			align_val !=  1			) {

			set[jj].info.txt->info.ialign = 0;
		    }

		}
		else if (strcmp(vg_typestr, "SPTX_ELM") == 0) { 
		    set[jj].vg_type = SPTX_ELM;
		    G_MALLOC(set[jj].info.spt, SpTxtAttr, one, "set[jj].info.spt");

		    sscanf (tstr, "%*s %*s %d %d %d %f %f %d %d %d %d %d %s",
			&(set[jj].subtyp),
			&(set[jj].maj_col), &(set[jj].min_col),
			&(set[jj].info.spt->text.info.rotn),
			&(set[jj].info.spt->text.info.sztext), 
			&(set[jj].info.spt->text.info.turbsym),
			&(set[jj].info.spt->text.info.itxfn), 
			&(set[jj].info.spt->text.info.ithw),
			&(set[jj].info.spt->text.info.iwidth), 
			&(set[jj].info.spt->text.info.ialign),
			  set[jj].grp_typ );
		    set[jj].info.spt->text.info.sptxtyp = set[jj].subtyp;
		    strcpy ( set[jj].info.spt->ppid, DEFLTPPID );
		    set[jj].info.spt->text.text[0] = CHNULL;
		    /*
		     * Make sure ialign value is among (-1, 0, 1).
		     */
		    align_val = set[jj].info.spt->text.info.ialign;

		    if ( align_val != -1 && align_val != 0 &&
			 align_val !=  1			) {

		         set[jj].info.spt->text.info.ialign = 0;
		    }

		}

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE TRACKS CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_TRACKS") == 0 ) {
		set[jj].vg_class = CLASS_TRACKS;
		G_MALLOC(set[jj].info.trk, TrackInfo, one, "set[jj].info.trk");

		if ( strcmp(vg_typestr, "TRKSTORM_ELM" ) == 0 ) {
		    set[jj].vg_type = TRKSTORM_ELM;
		}
		else if ( strcmp(vg_typestr, "-99" ) == 0 ) {
		    set[jj].vg_type = DEFLTSET;
		}

		sscanf(tstr, "%*s %*s %d %d %d %d %d %d %d %d %d %s",
		    &(set[jj].subtyp),
		    &(set[jj].maj_col), &(set[jj].min_col),
		    &(set[jj].info.trk->ltype1), &(set[jj].info.trk->ltype2),
		    &(set[jj].info.trk->mtype1), &(set[jj].info.trk->mtype2),
		    &(set[jj].info.trk->width), &(set[jj].info.trk->incr),
		    set[jj].grp_typ );
	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE SIGMETS CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_SIGMETS") == 0 ) {
		set[jj].vg_class = CLASS_SIGMETS;

		if ( strcmp(vg_typestr, "SIGAIRM_ELM" ) == 0 ) {
		    set[jj].vg_type = SIGAIRM_ELM;
		}
		else if ( strcmp(vg_typestr, "SIGCONV_ELM" ) == 0 ) {
		    set[jj].vg_type = SIGCONV_ELM;
		}
		else if ( strcmp(vg_typestr, "SIGINTL_ELM" ) == 0 ) {
		    set[jj].vg_type = SIGINTL_ELM;
		}
		else if ( strcmp(vg_typestr, "SIGNCON_ELM" ) == 0 ) {
		    set[jj].vg_type = SIGNCON_ELM;
		}
		else if ( strcmp(vg_typestr, "SIGOUTL_ELM" ) == 0 ) {
		    set[jj].vg_type = SIGOUTL_ELM;
		}
		else if ( strcmp(vg_typestr, "SIGCCF_ELM" ) == 0 ) {
		    set[jj].vg_type = SIGCCF_ELM;
		}
		else if ( strcmp(vg_typestr, "ASHCLD_ELM" ) == 0 ) {
		    set[jj].vg_type = ASHCLD_ELM;
		}
		else if ( strcmp(vg_typestr, "VOLC_ELM" ) == 0 ) {
		    set[jj].vg_type = VOLC_ELM;
		}
		else if ( strcmp(vg_typestr, "-99" ) == 0 ) {
		    set[jj].vg_type = DEFLTSET;
		}

		if (set[jj].vg_type == SIGCCF_ELM) {
                    G_MALLOC(set[jj].info.ccf, CcfAttr, one, "set[jj].info.ccf");		    
                    sscanf(tstr, "%*s %*s %d %d %d %d %d %d %d %d %f %f %d %d %d %d %s %s",
                      &(set[jj].subtyp), &(set[jj].smooth),
                      &(set[jj].maj_col), &(set[jj].min_col),
                      &(set[jj].info.ccf->fillhi),
                      &(set[jj].info.ccf->fillmed),
                      &(set[jj].info.ccf->filllow),
                      &(set[jj].info.ccf->linetype),
                      &(set[jj].info.ccf->szarrow),
                      &(set[jj].info.ccf->info.sztext),
                      &(set[jj].info.ccf->info.itxfn),
                      &(set[jj].info.ccf->info.ithw),
                      &(set[jj].info.ccf->info.iwidth),
                      &(set[jj].info.ccf->info.ialign),
                      set[jj].info.ccf->textLayout,
                      set[jj].grp_typ );
                    /* printf("subtyp = %d, smooth=%d, maj_col=%d, min_col=%d, fillhi=%d, fillmed=%d, filllow=%d\n", 
                      set[jj].subtyp, set[jj].smooth, set[jj].maj_col, set[jj].min_col, 
                      set[jj].info.ccf->fillhi, set[jj].info.ccf->fillmed, set[jj].info.ccf->filllow);
                    printf("linetype=%d, szarrow=%f, sztext=%f, itxfn=%d, ithw=%d, iwidth=%d ialign=%d\n",
                      set[jj].info.ccf->linetype, set[jj].info.ccf->szarrow, 
                      set[jj].info.ccf->info.sztext,
                      set[jj].info.ccf->info.itxfn, set[jj].info.ccf->info.ithw, set[jj].info.ccf->info.iwidth,
                      set[jj].info.ccf->info.ialign);
                    printf("tlayout=%s grptyp=%s\n",set[jj].info.ccf->textLayout, set[jj].grp_typ); */
                      
		}
		else if (set[jj].vg_type == ASHCLD_ELM) {
		    G_MALLOC(set[jj].info.ash, SigAttr, one, "set[jj].info.ash");
		    sscanf(tstr, "%*s %*s %d %d %d %d %s", 
			   &(set[jj].subtyp), &(set[jj].maj_col),
			   &(set[jj].min_col), &(set[jj].smooth),
			   set[jj].grp_typ );
		}
		else if (set[jj].vg_type == VOLC_ELM) {
		    G_MALLOC(set[jj].info.vol, VolAttr, one, "set[jj].info.vol");
		    sscanf(tstr, "%*s %*s %d %d %d %d %f %s",
			   &(set[jj].subtyp), &(set[jj].maj_col),
			   &(set[jj].min_col),&(set[jj].info.vol->width),
			   &(set[jj].info.vol->size), set[jj].grp_typ );
		}
		else {
		    G_MALLOC(set[jj].info.sig, SigAttr, one, "set[jj].info.sig");
		    sscanf(tstr, "%*s %*s %d %d %d %d %d %s",
			   &(set[jj].subtyp),
			   &(set[jj].maj_col), &(set[jj].min_col),
			   &(set[jj].info.sig->lintyp), 
			   &(set[jj].info.sig->linwid),
			     set[jj].grp_typ );
		}
	    }

    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE LIST CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_LIST") == 0 ) {
		set[jj].vg_class = CLASS_LIST;
		G_MALLOC(set[jj].info.lst, ListInfo, one, "set[jj].info.lst");

		if ( strcmp(vg_typestr, "LIST_ELM") == 0 ) {
		    set[jj].vg_type = LIST_ELM;
		    sscanf ( tstr, "%*s %*s %d %d %d %d %f %d %s",
			&(set[jj].subtyp),
			&(set[jj].maj_col),
			&(set[jj].min_col),
			&(set[jj].info.lst->mrktyp),
			&(set[jj].info.lst->mrksiz),
			&(set[jj].info.lst->mrkwid),
			  set[jj].grp_typ );

		     set[jj].info.lst->subtyp = set[jj].subtyp;
		}

	    }

    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE MET CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_MET") == 0 ) {
		set[jj].vg_class = CLASS_MET;

		if ( strcmp(vg_typestr, "JET_ELM") == 0 ) {
		    set[jj].vg_type = JET_ELM;
 		    G_MALLOC(set[jj].info.jet, JetInfo, one, "set[jj].info.jet");
		    /*
		       *  Check the number of items in setting.tbl.
		       *  ( 22 - version 5.7.4 & later; 21 for earlier versions.
		       */
		    strcpy ( tmpstr, tstr );
		    cst_nocc ( tmpstr, '!', 1, 0, &nn, &ier );

		    if ( ier == 0 ) {
		        tmpstr[nn] = '\0';
		        cst_lstr ( tmpstr, &nn, &ier );
		        tmpstr[nn] = '\0';
		    }

		    nn = 0;
		    ptr = strtok ( tmpstr, " " );
		    while ( (ptr != (char *)NULL) ) {
			ptr = strtok ( NULL, " " );
			nn++;
		    }

		    if ( nn < 22 ) {  /* Version 5.7.3 & before */
			sscanf ( tstr, "%*s %*s %d %d %d %d %d %f %d %d %f %d %f %d %d %d %d %d %f %d %s",
			    &(set[jj].subtyp),
			    &(set[jj].maj_col), &(set[jj].min_col), &smooth,
			    &(set[jj].info.jet->line.spltyp),
			    &(set[jj].info.jet->line.splsiz),
			    &(set[jj].info.jet->line.splwid),
			    &tmpclr[0], &tmpsz[0], &tmpwid[0],			/* Barb */
			    &tmpsz[1], &ifnt, &ithw, &tmpwid[1], &ialign,	/* Text */
			    &tmpclr[1], &tmpsz[2], &tmpwid[2],			/* Hash */
			    set[jj].grp_typ );

			    ityp = 0;
		    }
		    else {  /* Version 5.7.4 and later */
		        sscanf ( tstr, "%*s %*s %d %d %d %d %d %f %d %d %f %d %d %f %d %d %d %d %d %f %d %s",
			    &(set[jj].subtyp),
			    &(set[jj].maj_col), &(set[jj].min_col), &smooth,
			    &(set[jj].info.jet->line.spltyp),
			    &(set[jj].info.jet->line.splsiz),
			    &(set[jj].info.jet->line.splwid),
			    &tmpclr[0], &tmpsz[0], &tmpwid[0],				/* Barb */
			    &ityp, &tmpsz[1], &ifnt, &ithw, &tmpwid[1], &ialign,	/* Text */
			    &tmpclr[1], &tmpsz[2], &tmpwid[2],				/* Hash */
			    set[jj].grp_typ );
		    }
		    
		    if (smooth < 0 || smooth > 2) smooth = 0;
		    set[jj].smooth = smooth;
		    set[jj].info.jet->splcol = set[jj].maj_col;
		    set[jj].info.jet->line.splstr = 1;
		    set[jj].info.jet->line.spldir = 0;


		    for ( ii = 0; ii < MAX_JETPTS; ii++ ) {
		        set[jj].info.jet->barb[ii].wndcol = tmpclr[0];
		        set[jj].info.jet->barb[ii].wnd.info.size = tmpsz[0];
		        set[jj].info.jet->barb[ii].wnd.info.width = tmpwid[0];
		        set[jj].info.jet->barb[ii].wnd.info.wndtyp = 114;
		        set[jj].info.jet->barb[ii].wnd.info.hdsiz = 1.0F;
		        set[jj].info.jet->barb[ii].sptcol = tmpclr[0];
			set[jj].info.jet->barb[ii].spt.info.sztext = tmpsz[1];
		        set[jj].info.jet->barb[ii].spt.info.itxfn = ifnt;
		        set[jj].info.jet->barb[ii].spt.info.ithw = ithw;
		        set[jj].info.jet->barb[ii].spt.info.iwidth = tmpwid[1];
		        set[jj].info.jet->barb[ii].spt.info.ialign = ialign;
		        set[jj].info.jet->barb[ii].spt.info.rotn = 0.0F;
		        set[jj].info.jet->barb[ii].spt.info.sptxtyp = ityp;
		        set[jj].info.jet->barb[ii].spt.info.turbsym = 0;
		        set[jj].info.jet->barb[ii].spt.info.txtcol = tmpclr[0];
		        set[jj].info.jet->barb[ii].spt.info.filcol = tmpclr[0];
		        set[jj].info.jet->barb[ii].spt.info.lincol = tmpclr[0];
			set[jj].info.jet->barb[ii].spt.text[0] = CHNULL;
		        set[jj].info.jet->hash[ii].wndcol = tmpclr[1];
		        set[jj].info.jet->hash[ii].wnd.info.size = tmpsz[2];
		        set[jj].info.jet->hash[ii].wnd.info.width = tmpwid[2];
		        set[jj].info.jet->hash[ii].wnd.info.wndtyp = 1;	
		        set[jj].info.jet->hash[ii].wnd.info.hdsiz = 1.0F;			
		    }

		}

		else if ( strcmp(vg_typestr, "GFA_ELM") == 0 ) {
		    set[jj].vg_type = GFA_ELM;
		    G_MALLOC(set[jj].info.gfa, GfaAttr, one, "set[jj].info.gfa");
          
		    sscanf(tstr, "%*s %*s %d %d %d %d %d %d %f %d %f %d %d %d %d %s %s", 
			   &(set[jj].subtyp), &(set[jj].maj_col), 
			   &(set[jj].min_col), &(set[jj].info.gfa->linelm), 
			   &(set[jj].info.gfa->lintyp),&(set[jj].info.gfa->linwid),
                           &(set[jj].info.gfa->szarrow),
                           &(set[jj].info.gfa->info.txtcol),
                           &(set[jj].info.gfa->info.sztext),
                           &(set[jj].info.gfa->info.itxfn),
                           &(set[jj].info.gfa->info.ithw),
                           &(set[jj].info.gfa->info.iwidth),
                           &(set[jj].info.gfa->info.ialign),
                           set[jj].info.gfa->textLayout,
			   set[jj].grp_typ );
		}
                else if ( strcmp(vg_typestr, "SGWX_ELM") == 0 ) {
                  set[jj].vg_type = SGWX_ELM;
                  G_MALLOC(set[jj].info.sgwx, SgwxAttr, one, "set[jj].info.sgwx");
                  sscanf(tstr, "%*s %*s %d %d %d %d %d %d %d %f %f %d %d %d %d %s",
                    &(set[jj].subtyp), &(set[jj].smooth),
                    &(set[jj].maj_col), &(set[jj].min_col),
                    &(set[jj].info.sgwx->lineelm),
                    &(set[jj].info.sgwx->linetype),
                    &(set[jj].info.sgwx->linewidth),
                    &(set[jj].info.sgwx->szarrow),
                    &(set[jj].info.sgwx->info.sztext),
                    &(set[jj].info.sgwx->info.itxfn),
                    &(set[jj].info.sgwx->info.ithw),
                    &(set[jj].info.sgwx->info.iwidth),
                    &(set[jj].info.sgwx->info.ialign),
                    set[jj].grp_typ );
                }
		else if ( strcmp(vg_typestr, "TCA_ELM") == 0 ) {
		    set[jj].vg_type = TCA_ELM;
		    G_MALLOC(set[jj].info.tca, TcaInfo, one, "set[jj].info.tca");

		    sscanf(tstr, "%*s %*s %d %d %d %d %s",
                           &set[jj].subtyp, &set[jj].maj_col, &set[jj].min_col,
                           &set[jj].info.tca->wwNum, set[jj].grp_typ );
	}

	    }

	    /*
	     * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	     *    DIDN'T RECOGNIZE THE CLASS; BAD READ, IGNORE RECORD 
	     * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	     */
	    else {
		*iret = 2;
	        loglev = 2;
		strcpy(grp, "CES");
		er_lmsg (&loglev, grp, iret, SETTING_TBL, &ier1, strlen(grp),
		         strlen(SETTING_TBL));
	    }

        /*
         * Set layer_flag according to set[jj].grp_typ
         */
            cst_lcuc(set[jj].grp_typ, set[jj].grp_typ, &ier1);
	    if ( strstr ( set[jj].grp_typ, "/LAYER" ) != NULL ) {
                 set[jj].cds_or_ces.layer_flag = TRUE;
	    }
	    else {
		set[jj].cds_or_ces.layer_flag = FALSE;
	    }

	    /*
	     * Increment jj
	     */
	    jj++;

	}

    }

    if ( !quit ) {
	/*
	 * Setting array (set) size exceeded.  MAX_SET should
	 * be increased.
	 */
	*iret = 1;
	loglev = 2;
	strcpy(grp, "CES");
	er_lmsg (&loglev, grp, iret, SETTING_TBL, &ier1, strlen(grp),
		 strlen(SETTING_TBL));
    }

    /* close the settings file */
    cfl_clos(fp, &ier);
}
