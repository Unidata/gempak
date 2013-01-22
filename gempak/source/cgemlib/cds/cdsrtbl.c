#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"


void cds_rtbl ( char *attrfnam, int *iret )
/************************************************************************
 * cds_rtbl								*
 *									*
 * This function reads the user attribute table, attrfnam.  The table 	*
 * entries are compared against existing records in the cdsUattr array.	*
 * If found in the existing structure, update record with table values;	*
 * otherwise, create a new entry in the cdsUattr structure.		*
 *									*
 * cds_rtbl ( attrfnam, iret )						*
 *									*
 * Input parameters:							*
 *	*attrfnam	char		Name of User attribute file	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  2 = Bad read.  Ignore record. *
 *					  1 = Attrib. array siz exceeded*
 *					 -1 = No user attribute file.	*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	10/99	Based on ces_rtbl			*
 * F. J. Yen/NCEP	10/99	Changed loglev from 2 to 0		*
 * S. Law/GSC		02/00	Added CCF				*
 * E. Safford/SAIC	02/02	add new_subtyp, initialize grp_typ	*
 * M. Li/SAIC		01/03	delete vgstruct.h                       *
 * H. Zeng/XTRIA	05/03	added Watchbox marker info		*
 * J. Wu/SAIC		06/03	handle LIST class info			*
 * D.W.Plummer/NCEP	06/03	added ASHCLD_ELM and VOLC_ELM		*
 * J. Wu/SAIC		08/03	add CLASS_MET -> JET_ELM		*
 * J. Wu/SAIC		01/04	add CLASS_MET -> GFA_ELM		*
 * B. Yin/SAIC		02/04	added CLASS_MET -> TCA_ELM		*
 * J. Wu/SAIC		05/04	add barb/hash color into JET_ELM	*
 * J. Wu/SAIC		05/04	add post-processing flag for SPTX_ELM	*
 * H. Zeng/SAIC		07/04	added check for ialign value		*
 * J. Wu/SAIC		09/04	allow to set text type for jet barb text*
 * J. Wu/SAIC		10/04	remove line width from GFA_ELM		*
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 * B. Yin/SAIC		01/06	added line width for GFA		*
 * B. Yin/SAIC		07/06	added line type & line element for GFA	*
 * L. Hinson/AWC        12/06   added text color, size, font, hw, width *
 *                              alignment, and text Layout for GFA      *
 * m.gamazaychikov/SAIC	06/07	add code for TCERR, TCTRK, TCBKL elems	*
 * L. Hinson/AWC        06/07   added arrow size for GFA                *
 * L. Hinson/AWC        09/07   add color, fills, line type, arrowsize, *
 *                              text size, font, hw, width, alignment & *
 *                              text Layout for CCF                     *
 * L. Hinson/AWC        01/12   Add CLASS_MET -> SGWX_ELM               *
 ***********************************************************************/
{
    int		align_val, ialign, ier, ier1, ifnt, ii, indx, ithw;
    int		ityp, jj, loglev, nn, one=1, quit, smooth, subtyp;
    int		tmpclr[3], tmpwid[3], vg_class, vg_type;
    char	grp[4], *ptr, tmpstr[256], tstr[256];
    char	vg_classstr[64], vg_typestr[64];
    float	tmpsiz, tmpsz[3];
    FILE	*fp;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * Open the user setting table.  If not found, return an error.
     */

    if ( (fp = (FILE * )cfl_tbop(attrfnam, "pgen", &ier)) == NULL) {
        *iret = -1;
	loglev = 0;
	strcpy(grp, "CDS");
	er_lmsg (&loglev, grp, iret, attrfnam, &ier1, strlen(grp),
		 strlen(attrfnam));
        return;
    }

    /* 
     * For every line in the setting table list, read in the record,
     * parse out the fields, and compare to input type.
     */
    quit = G_FALSE;
    
    while ( ( !quit ) && ( numUset < MAX_SET ) ) {

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
	    loglev = 0;
    	    strcpy(grp, "CDS");
	    ier = 2;
	    er_lmsg ( &loglev, grp, &ier, attrfnam, &ier1,
		      strlen(grp), strlen(attrfnam) );
	}

	else {
/*
 * Here to process a good record.
 */
	    sscanf(tstr, "%s %s %d", vg_classstr, vg_typestr, &subtyp);
	    cds_class(vg_classstr, vg_typestr, &vg_class, &vg_type, &ier);
	    cds_match(vg_class, vg_type, subtyp, &indx, &ier);
/*
 *  If Match NOT found between user attribute table entry and
 *  cdsUattr structure set jj to numUset; otherwise use indx.
 */
	    if ( indx == -1 ) {
                jj = numUset;
		numUset++;
		G_REALLOC(cdsUattr, Setting_t, numUset, "cdsUattr");
		cdsUattr[jj].closed = 0;
		cdsUattr[jj].filled = 0;
		cdsUattr[jj].smooth = 0;
	    } else {
                jj = indx;
            }

/*
 *  Initialize the grp_typ.  This field of the struct isn't used 
 *  within  the cds library.
 */
	    cdsUattr[jj].grp_typ[0] = '\0';

/*
 *  Initialize the new_subtyp field.  It will be missing in old
 *  versions of the uattrib.tbl formatted files.
 */
	    cdsUattr[jj].cds_or_ces.new_subtyp = DEFLTSET;

    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE FRONTS CLASS
     * +++++++++++++++++++++++++++++++++++++++++++
     */
		if ( strcmp(vg_classstr, "CLASS_FRONTS" ) == 0 ) {
		cdsUattr[jj].vg_class = CLASS_FRONTS;
		if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.frt, FrontInfo, one, "cdsUattr[jj].info.frt");
		sscanf(tstr, "%*s %*s %d %d %d %d %f %d %d %d",
			&(cdsUattr[jj].subtyp), &(cdsUattr[jj].maj_col),
			&(cdsUattr[jj].min_col), &smooth, &tmpsiz,
			&(cdsUattr[jj].info.frt->fpipdr),
			&(cdsUattr[jj].info.frt->fwidth),
			&(cdsUattr[jj].cds_or_ces.new_subtyp));

		cdsUattr[jj].info.frt->fpipsz = (int)(tmpsiz * 100.0F);
		cdsUattr[jj].info.frt->fcode = cdsUattr[jj].subtyp;
		cdsUattr[jj].info.frt->fpipst = 1;

		if ( smooth < -1 || smooth > 2 ) smooth = 0;
		cdsUattr[jj].smooth = smooth;

		if ( strcmp(vg_typestr, "FRONT_ELM" ) == 0 ) {
		    cdsUattr[jj].vg_type = FRONT_ELM;
		}

		else {
		    if ( strcmp(vg_typestr, "-99" ) == 0 ) {
			cdsUattr[jj].vg_type = DEFLTSET;
		    }
		}

	    }
   /*
    * +++++++++++++++++++++++++++++++++++++++++++
    *    HANDLE CIRCLE CLASS 
    * +++++++++++++++++++++++++++++++++++++++++++
    */
		else if ( strcmp(vg_classstr, "CLASS_CIRCLE") == 0 ) {
		cdsUattr[jj].vg_class = CLASS_CIRCLE;

		if (strcmp(vg_typestr, "CIRCLE_ELM") == 0) {
		    cdsUattr[jj].vg_type = CIRCLE_ELM;

		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.cir, LineInfo, one, "cdsUattr[jj].info.cir");
		    sscanf(tstr, "%*s %*s %d %d %d %d %d %d %d %d",
			    &(cdsUattr[jj].subtyp),
			    &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col),
			    &(cdsUattr[jj].info.cir->lintyp),
			    &(cdsUattr[jj].info.cir->lthw),
			    &(cdsUattr[jj].info.cir->width),
			    &(cdsUattr[jj].info.cir->lwhw),
			    &(cdsUattr[jj].cds_or_ces.new_subtyp));

		}

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE LINES CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_LINES") == 0 ) {
		cdsUattr[jj].vg_class = CLASS_LINES;

		if (strcmp(vg_typestr, "LINE_ELM") == 0) {
		    cdsUattr[jj].vg_type = LINE_ELM;

		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.lin, LineInfo, one, "cdsUattr[jj].info.lin");
		    sscanf(tstr, "%*s %*s %d %d %d %d %d %d %d %d %d",
			    &(cdsUattr[jj].subtyp),
			    &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col),
			    &smooth,
			    &(cdsUattr[jj].info.lin->lintyp),
			    &(cdsUattr[jj].info.lin->lthw),
			    &(cdsUattr[jj].info.lin->width),
			    &(cdsUattr[jj].info.lin->lwhw),
			    &(cdsUattr[jj].cds_or_ces.new_subtyp));

		}
		else if (strcmp(vg_typestr, "SPLN_ELM") == 0) {
		    cdsUattr[jj].vg_type = SPLN_ELM;

		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.spl, SpLineInfo, one, "cdsUattr[jj].info.spl");
		    cdsUattr[jj].info.spl->splstr = 1;

		    sscanf(tstr, "%*s %*s %d %d %d %d %d %d %f %d %d",
			    &(cdsUattr[jj].subtyp),
			    &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col),
			    &smooth,
			    &(cdsUattr[jj].info.spl->spltyp),
			    &(cdsUattr[jj].info.spl->spldir),
			    &(cdsUattr[jj].info.spl->splsiz),
			    &(cdsUattr[jj].info.spl->splwid),
			    &(cdsUattr[jj].cds_or_ces.new_subtyp));

		}

		if (smooth < -1 || smooth > 2) smooth = 0;
		cdsUattr[jj].smooth = smooth;

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE SYMBOLS CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_SYMBOLS") == 0 ) {
		cdsUattr[jj].vg_class = CLASS_SYMBOLS;
		cdsUattr[jj].vg_type = vg_type;

		if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.sym, SymType, one, "cdsUattr[jj].info.sym");
		sscanf(tstr, "%*s %*s %d %d %d %d %f %d",
		       &(cdsUattr[jj].subtyp), 
		       &(cdsUattr[jj].maj_col), 
		       &(cdsUattr[jj].min_col),
		       &(cdsUattr[jj].info.sym->info.width),
		       &(cdsUattr[jj].info.sym->info.size),
		       &(cdsUattr[jj].cds_or_ces.new_subtyp));

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE WINDS CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_WINDS") == 0 ) {
		cdsUattr[jj].vg_class = CLASS_WINDS;

		if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.wnd, WindInfo, one, "cdsUattr[jj].info.wnd");
		if ( strcmp(vg_typestr, "ARROW_ELM") == 0 ) {
		    cdsUattr[jj].vg_type = ARROW_ELM;

		    sscanf(tstr, "%*s %*s %d %d %d %d %f %f %d",
			 &(cdsUattr[jj].subtyp), &(cdsUattr[jj].maj_col),
			 &(cdsUattr[jj].min_col),
			 &(cdsUattr[jj].info.wnd->width),
			 &(cdsUattr[jj].info.wnd->size),
		         &(cdsUattr[jj].info.wnd->hdsiz),
		         &(cdsUattr[jj].cds_or_ces.new_subtyp));

		    cdsUattr[jj].info.wnd->wndtyp = 114;
		}
		else if ( strcmp(vg_typestr, "BARB_ELM") == 0 ) {
		    cdsUattr[jj].vg_type = BARB_ELM;
		    sscanf(tstr, "%*s %*s %d %d %d %d %f %d",
			 &(cdsUattr[jj].subtyp), &(cdsUattr[jj].maj_col),
			 &(cdsUattr[jj].min_col),
			 &(cdsUattr[jj].info.wnd->width),
			 &(cdsUattr[jj].info.wnd->size),
		         &(cdsUattr[jj].cds_or_ces.new_subtyp));

		    cdsUattr[jj].info.wnd->hdsiz = 1.0F;
		    cdsUattr[jj].info.wnd->wndtyp = 114;

		}
		else if ( strcmp(vg_typestr, "DARR_ELM") == 0 ) {
		    cdsUattr[jj].vg_type = DARR_ELM;

		    sscanf(tstr, "%*s %*s %d %d %d %d %f %f %d",
			 &(cdsUattr[jj].subtyp), &(cdsUattr[jj].maj_col),
			 &(cdsUattr[jj].min_col),
			 &(cdsUattr[jj].info.wnd->width),
			 &(cdsUattr[jj].info.wnd->size),
			 &(cdsUattr[jj].info.wnd->hdsiz),
			 &(cdsUattr[jj].cds_or_ces.new_subtyp));

		    cdsUattr[jj].info.wnd->wndtyp = 114;
		}
		else if ( strcmp(vg_typestr, "HASH_ELM") == 0 ) {
		    cdsUattr[jj].vg_type = HASH_ELM;

		    sscanf(tstr, "%*s %*s %d %d %d %d %f %d",
			 &(cdsUattr[jj].subtyp), &(cdsUattr[jj].maj_col),
			 &(cdsUattr[jj].min_col),
			 &(cdsUattr[jj].info.wnd->width),
			 &(cdsUattr[jj].info.wnd->size),
			 &(cdsUattr[jj].cds_or_ces.new_subtyp));

		    cdsUattr[jj].info.wnd->hdsiz = 1.0F;
		    cdsUattr[jj].info.wnd->wndtyp = 1;
		}

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE WBOX CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr,"CLASS_WATCHES") == 0 ) {
		cdsUattr[jj].vg_class = CLASS_WATCHES;
		cdsUattr[jj].vg_type = vg_type;

		if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.wbx, WboxAttr, one, "cdsUattr[jj].info.wbx");
		sscanf (tstr, "%*s %*s %d %d %d %d %d %f %d %d",
			&(cdsUattr[jj].subtyp), &(cdsUattr[jj].maj_col),
			&(cdsUattr[jj].min_col),
			&(cdsUattr[jj].info.wbx->w_type),
			&(cdsUattr[jj].info.wbx->w_mrktyp),
			&(cdsUattr[jj].info.wbx->w_mrksiz),
			&(cdsUattr[jj].info.wbx->w_mrkwid),
			&(cdsUattr[jj].cds_or_ces.new_subtyp));

		cdsUattr[jj].info.wbx->w_number = -9999;

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE TEXT CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_TEXT") == 0 ) {
		cdsUattr[jj].vg_class = CLASS_TEXT;

		if (strcmp(vg_typestr, "TEXT_ELM") == 0) {
		    cdsUattr[jj].vg_type = TEXT_ELM;

		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.txt, TextType, one, "cdsUattr[jj].info.txt");

		    sscanf (tstr, "%*s %*s %d %d %d %f %f %d %d %d %d ",
			&(cdsUattr[jj].subtyp), &(cdsUattr[jj].maj_col),
			&(cdsUattr[jj].min_col),
			&(cdsUattr[jj].info.txt->info.rotn),
			&(cdsUattr[jj].info.txt->info.sztext),
			&(cdsUattr[jj].info.txt->info.itxfn),
			&(cdsUattr[jj].info.txt->info.iwidth),
			&(cdsUattr[jj].info.txt->info.ialign),
		        &(cdsUattr[jj].cds_or_ces.new_subtyp));

		    cdsUattr[jj].info.txt->info.ithw = cdsUattr[jj].subtyp;

		    /*
		    * Make sure ialign value is among (-1, 0, 1).
		    */
		    align_val = cdsUattr[jj].info.txt->info.ialign;

		    if ( align_val != -1 && align_val != 0 &&
			 align_val !=  1			) {

			 cdsUattr[jj].info.txt->info.ialign = 0;
		    }

		}
		else if (strcmp(vg_typestr, "TEXTC_ELM") == 0) { 
		    cdsUattr[jj].vg_type = TEXTC_ELM;

		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.txt, TextType, one, "cdsUattr[jj].info.txt");
		    sscanf (tstr, "%*s %*s %d %d %d %f %f %d %d %d %d ",
			&(cdsUattr[jj].subtyp), &(cdsUattr[jj].maj_col),
			&(cdsUattr[jj].min_col),
			&(cdsUattr[jj].info.txt->info.rotn),
			&(cdsUattr[jj].info.txt->info.sztext),
			&(cdsUattr[jj].info.txt->info.itxfn),
			&(cdsUattr[jj].info.txt->info.iwidth),
			&(cdsUattr[jj].info.txt->info.ialign),
			&(cdsUattr[jj].cds_or_ces.new_subtyp));

		    cdsUattr[jj].info.txt->info.ithw = cdsUattr[jj].subtyp;

		    /*
		     * Make sure ialign value is among (-1, 0, 1).
		     */
		    align_val = cdsUattr[jj].info.txt->info.ialign;

		    if ( align_val != -1 && align_val != 0 &&
			 align_val !=  1			) {

			cdsUattr[jj].info.txt->info.ialign = 0;
	    }

		}
		else if (strcmp(vg_typestr, "SPTX_ELM") == 0) { 
		    cdsUattr[jj].vg_type = SPTX_ELM;

		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.spt, SpTxtAttr, one, "cdsUattr[jj].info.spt");
		    sscanf (tstr, "%*s %*s %d %d %d %f %f %d %d %d %d %d %d %s",
			&(cdsUattr[jj].subtyp), &(cdsUattr[jj].maj_col),
			&(cdsUattr[jj].min_col),
			&(cdsUattr[jj].info.spt->text.info.rotn),
			&(cdsUattr[jj].info.spt->text.info.sztext),
			&(cdsUattr[jj].info.spt->text.info.turbsym),
			&(cdsUattr[jj].info.spt->text.info.itxfn),
			&(cdsUattr[jj].info.spt->text.info.ithw),
			&(cdsUattr[jj].info.spt->text.info.iwidth),
			&(cdsUattr[jj].info.spt->text.info.ialign),
			&(cdsUattr[jj].cds_or_ces.new_subtyp), cdsUattr[jj].info.spt->ppid );

		    cdsUattr[jj].info.spt->text.info.sptxtyp = cdsUattr[jj].subtyp;

		    /*
		     * Make sure ialign value is among (-1, 0, 1).
		     */
		    align_val = cdsUattr[jj].info.spt->text.info.ialign;

		    if ( align_val != -1 && align_val != 0 &&
			 align_val !=  1			) {

			cdsUattr[jj].info.spt->text.info.ialign = 0;
		    }

		}

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE TRACKS CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr, "CLASS_TRACKS") == 0 ) {
		cdsUattr[jj].vg_class = CLASS_TRACKS;
		cdsUattr[jj].vg_type = vg_type;

		if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.trk, TrackInfo, one, "cdsUattr[jj].info.trk");
		sscanf(tstr, "%*s %*s %d %d %d %d %d %d %d %d %d %d", 
		       &(cdsUattr[jj].subtyp),
		       &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col),
		       &(cdsUattr[jj].info.trk->ltype1),
		       &(cdsUattr[jj].info.trk->ltype2),
		       &(cdsUattr[jj].info.trk->mtype1),
		       &(cdsUattr[jj].info.trk->mtype2),
		       &(cdsUattr[jj].info.trk->width),
		       &(cdsUattr[jj].info.trk->incr),
		       &(cdsUattr[jj].cds_or_ces.new_subtyp));
	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE SIGMETS CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if (strcmp(vg_classstr, "CLASS_SIGMETS") == 0) {
		cdsUattr[jj].vg_class = CLASS_SIGMETS;
		cdsUattr[jj].vg_type = vg_type;

		if (cdsUattr[jj].vg_type == SIGCCF_ELM) {
                    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.ccf, CcfAttr, one, "cdsUattr[jj].info.ccf");                    
                    sscanf(tstr, "%*s %*s %d %d %d %d %d %d %d %d %f %f %d %d %d %d %s",
                      &(cdsUattr[jj].subtyp), &(cdsUattr[jj].smooth),
                      &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col),
                      &(cdsUattr[jj].info.ccf->fillhi),
                      &(cdsUattr[jj].info.ccf->fillmed),
                      &(cdsUattr[jj].info.ccf->filllow),
                      &(cdsUattr[jj].info.ccf->linetype),
                      &(cdsUattr[jj].info.ccf->szarrow),
                      &(cdsUattr[jj].info.ccf->info.sztext),
                      &(cdsUattr[jj].info.ccf->info.itxfn),
                      &(cdsUattr[jj].info.ccf->info.ithw),
                      &(cdsUattr[jj].info.ccf->info.iwidth),
                      &(cdsUattr[jj].info.ccf->info.ialign),
                      cdsUattr[jj].info.ccf->textLayout);
                     
                    /*Set the filled flag to true, to trigger load of cds table*/
                    cdsUattr[jj].filled = 1;
                    
                    /* printf("UA::subtyp = %d, smooth=%d, maj_col=%d, min_col=%d, fillhi=%d, fillmed=%d, filllow=%d\n", 
                      cdsUattr[jj].subtyp, cdsUattr[jj].smooth, cdsUattr[jj].maj_col, cdsUattr[jj].min_col, 
                      cdsUattr[jj].info.ccf->fillhi, cdsUattr[jj].info.ccf->fillmed, cdsUattr[jj].info.ccf->filllow);
                    printf("UA::linetype=%d, szarrow=%f, sztext=%f, itxfn=%d, ithw=%d, iwidth=%d\n",
                      cdsUattr[jj].info.ccf->linetype, cdsUattr[jj].info.ccf->szarrow, 
                      cdsUattr[jj].info.ccf->info.sztext,
                      cdsUattr[jj].info.ccf->info.itxfn, cdsUattr[jj].info.ccf->info.ithw, cdsUattr[jj].info.ccf->info.iwidth);
                    printf("UA::tlayout=%s grptyp=%s\n",cdsUattr[jj].info.ccf->textLayout, cdsUattr[jj].grp_typ); */
                    
		}
		else if (cdsUattr[jj].vg_type == ASHCLD_ELM) {
		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.ash, SigAttr, one, "cdsUattr[jj].info.ash");
		    sscanf(tstr, "%*s %*s %d %d %d %d %d",
			   &(cdsUattr[jj].subtyp),
			   &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col),
			   &(cdsUattr[jj].smooth), &(cdsUattr[jj].cds_or_ces.new_subtyp));
		}
		else if (cdsUattr[jj].vg_type == VOLC_ELM) {
		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.vol, VolAttr, one, "cdsUattr[jj].info.vol");
		    sscanf(tstr, "%*s %*s %d %d %d %d %f %d",
			   &(cdsUattr[jj].subtyp),
			   &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col),
			   &(cdsUattr[jj].info.vol->width),
			   &(cdsUattr[jj].info.vol->size),
			   &(cdsUattr[jj].cds_or_ces.new_subtyp));
		}
		else {
		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.sig, SigAttr, one, "cdsUattr[jj].info.sig");
		    sscanf(tstr, "%*s %*s %d %d %d %d %d %d", 
			   &(cdsUattr[jj].subtyp),
			   &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col),
			   &(cdsUattr[jj].info.sig->lintyp),
			   &(cdsUattr[jj].info.sig->linwid),
			   &(cdsUattr[jj].cds_or_ces.new_subtyp));
		}
	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE LIST CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr,"CLASS_LIST") == 0 ) {
		cdsUattr[jj].vg_class = CLASS_LIST;
		cdsUattr[jj].vg_type = vg_type;

		if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.lst, ListInfo, one, "cdsUattr[jj].info.lst");
		sscanf (tstr, "%*s %*s %d %d %d %d %f %d %d",
			&(cdsUattr[jj].subtyp), &(cdsUattr[jj].maj_col),
			&(cdsUattr[jj].min_col),
			&(cdsUattr[jj].info.lst->mrktyp),
			&(cdsUattr[jj].info.lst->mrksiz),
			&(cdsUattr[jj].info.lst->mrkwid),
			&(cdsUattr[jj].cds_or_ces.new_subtyp));

	    }
    /*
     * +++++++++++++++++++++++++++++++++++++++++++
     *    HANDLE MET CLASS 
     * +++++++++++++++++++++++++++++++++++++++++++
     */
	    else if ( strcmp(vg_classstr,"CLASS_MET") == 0 ) {
		cdsUattr[jj].vg_class = CLASS_MET;

		if (strcmp(vg_typestr, "JET_ELM") == 0) {
		    cdsUattr[jj].vg_type = JET_ELM;

		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.jet, JetInfo, one, "cdsUattr[jj].info.jet");
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
		        sscanf ( tstr, "%*s %*s %d %d %d %d %d %f %d %d %f %d %f %d %d %d %d %d %f %d %d",
			    &(cdsUattr[jj].subtyp),
			    &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col), &smooth,
			    &(cdsUattr[jj].info.jet->line.spltyp),
			    &(cdsUattr[jj].info.jet->line.splsiz),
			    &(cdsUattr[jj].info.jet->line.splwid),

			    &tmpclr[0], &tmpsz[0], &tmpwid[0],			/* Barb */
			    &tmpsz[1], &ifnt, &ithw, &tmpwid[1], &ialign,	/* Text */
			    &tmpclr[1], &tmpsz[2], &tmpwid[2],			/* Hash */
			    &cdsUattr[jj].cds_or_ces.new_subtyp );

			    ityp = 0;
		    }
		    else {  /* Version 5.7.4 and later */
		        sscanf ( tstr, "%*s %*s %d %d %d %d %d %f %d %d %f %d %d %f %d %d %d %d %d %f %d %d",
			    &(cdsUattr[jj].subtyp),
			    &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col), &smooth,

			    &(cdsUattr[jj].info.jet->line.spltyp),
			    &(cdsUattr[jj].info.jet->line.splsiz),
			    &(cdsUattr[jj].info.jet->line.splwid),

			    &tmpclr[0], &tmpsz[0], &tmpwid[0],				/* Barb */
			    &ityp, &tmpsz[1], &ifnt, &ithw, &tmpwid[1], &ialign,	/* Text */
			    &tmpclr[1], &tmpsz[2], &tmpwid[2],				/* Hash */
			    &cdsUattr[jj].cds_or_ces.new_subtyp );
		    }

		    if (smooth < -1 || smooth > 2) smooth = 0;
		    cdsUattr[jj].smooth = smooth;
		    cdsUattr[jj].info.jet->splcol = cdsUattr[jj].maj_col;

		    for ( ii = 0; ii < MAX_JETPTS; ii++ ) {
			cdsUattr[jj].info.jet->barb[ii].wndcol = tmpclr[0];
			cdsUattr[jj].info.jet->barb[ii].wnd.info.size = tmpsz[0];
			cdsUattr[jj].info.jet->barb[ii].wnd.info.width = tmpwid[0];
			cdsUattr[jj].info.jet->barb[ii].wnd.info.hdsiz = 1.0F;
			cdsUattr[jj].info.jet->barb[ii].wnd.info.wndtyp = 114;

			cdsUattr[jj].info.jet->barb[ii].sptcol = tmpclr[0];
			cdsUattr[jj].info.jet->barb[ii].spt.info.sztext = tmpsz[1];
			cdsUattr[jj].info.jet->barb[ii].spt.info.itxfn = ifnt;
			cdsUattr[jj].info.jet->barb[ii].spt.info.ithw = ithw;
			cdsUattr[jj].info.jet->barb[ii].spt.info.iwidth = tmpwid[1];
			cdsUattr[jj].info.jet->barb[ii].spt.info.ialign = ialign;
			cdsUattr[jj].info.jet->barb[ii].spt.info.sptxtyp = ityp;
			cdsUattr[jj].info.jet->barb[ii].spt.info.turbsym = 0;

			cdsUattr[jj].info.jet->hash[ii].wndcol = tmpclr[1];
			cdsUattr[jj].info.jet->hash[ii].wnd.info.size = tmpsz[2];
			cdsUattr[jj].info.jet->hash[ii].wnd.info.width = tmpwid[2];			
			cdsUattr[jj].info.jet->hash[ii].wnd.info.hdsiz = 1.0F;
			cdsUattr[jj].info.jet->hash[ii].wnd.info.wndtyp = 1;			
		    }
		}
		else if (strcmp(vg_typestr, "GFA_ELM") == 0) {
		    cdsUattr[jj].vg_type = GFA_ELM;
		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.gfa, GfaAttr, one, "cdsUattr[jj].info.gfa");

		    sscanf(tstr, "%*s %*s %d %d %d %d %d %d %f %d %f %d %d %d %d %s", 
			   &(cdsUattr[jj].subtyp),
			   &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col),
			   &(cdsUattr[jj].info.gfa->linelm),
			   &(cdsUattr[jj].info.gfa->lintyp),
			   &(cdsUattr[jj].info.gfa->linwid),
                           &(cdsUattr[jj].info.gfa->szarrow),
                           &(cdsUattr[jj].info.gfa->info.txtcol),
                           &(cdsUattr[jj].info.gfa->info.sztext),
                           &(cdsUattr[jj].info.gfa->info.itxfn),
                           &(cdsUattr[jj].info.gfa->info.ithw),
                           &(cdsUattr[jj].info.gfa->info.iwidth),
                           &(cdsUattr[jj].info.gfa->info.ialign),
                           cdsUattr[jj].info.gfa->textLayout);
		}
		else if (strcmp(vg_typestr,"SGWX_ELM") == 0) {
		  cdsUattr[jj].vg_type = SGWX_ELM;
		  if (indx == -1) G_MALLOC(cdsUattr[jj].info.sgwx, SgwxAttr, one, "cdsUattr[jj].info.sgwx");
		  sscanf(tstr, "%*s %*s %d %d %d %d %d %d %d %f %f %d %d %d %d",
		  &(cdsUattr[jj].subtyp), &(cdsUattr[jj].smooth),
                    &(cdsUattr[jj].maj_col), &(cdsUattr[jj].min_col),
                    &(cdsUattr[jj].info.sgwx->lineelm),
                    &(cdsUattr[jj].info.sgwx->linetype),
                    &(cdsUattr[jj].info.sgwx->linewidth),
                    &(cdsUattr[jj].info.sgwx->szarrow),
                    &(cdsUattr[jj].info.sgwx->info.sztext),
                    &(cdsUattr[jj].info.sgwx->info.itxfn),
                    &(cdsUattr[jj].info.sgwx->info.ithw),
                    &(cdsUattr[jj].info.sgwx->info.iwidth),
                    &(cdsUattr[jj].info.sgwx->info.ialign));
		}
		else if (strcmp(vg_typestr, "TCA_ELM") == 0) {
		    cdsUattr[jj].vg_type = TCA_ELM;
		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.tca, TcaInfo, one, "cdsUattr[jj].info.tca");

		    sscanf(tstr, "%*s %*s %d %d %d %d %d",
			   &cdsUattr[jj].subtyp,
			   &cdsUattr[jj].maj_col, &cdsUattr[jj].min_col,
		           &cdsUattr[jj].info.tca->wwNum, 
			   &cdsUattr[jj].cds_or_ces.new_subtyp);
		}
		else if (strcmp(vg_typestr, "TCERR_ELM") == 0) {
		    cdsUattr[jj].vg_type = TCERR_ELM;
		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.tce, TceAttr, one, "cdsUattr[jj].info.tce");

		    sscanf(tstr, "%*s %*s %d %d %d %d %d %d %d",
			   &cdsUattr[jj].subtyp,
			   &cdsUattr[jj].maj_col, &cdsUattr[jj].min_col,
			   &(cdsUattr[jj].info.tce->lincol),
			   &(cdsUattr[jj].info.tce->lintyp),
			   &(cdsUattr[jj].info.tce->filcol),
			   &(cdsUattr[jj].info.tce->filtyp));
		}
		else if (strcmp(vg_typestr, "TCTRK_ELM") == 0) {
		    cdsUattr[jj].vg_type = TCTRK_ELM;
		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.tct, TctAttr, one, "cdsUattr[jj].info.tct");

		    sscanf(tstr, "%*s %*s %d %d %d %d %d",
			   &cdsUattr[jj].subtyp,
			   &cdsUattr[jj].maj_col, &cdsUattr[jj].min_col,
			   &(cdsUattr[jj].info.tct->lincol),
			   &(cdsUattr[jj].info.tct->lintyp));
		}
		else if (strcmp(vg_typestr, "TCBKL_ELM") == 0) {
		    cdsUattr[jj].vg_type = TCBKL_ELM;
		    if ( indx == -1 ) G_MALLOC(cdsUattr[jj].info.tcb, TcbAttr, one, "cdsUattr[jj].info.tcb");

		    sscanf(tstr, "%*s %*s %d %d %d %d %d",
			   &cdsUattr[jj].subtyp,
			   &cdsUattr[jj].maj_col, &cdsUattr[jj].min_col,
			   &(cdsUattr[jj].info.tcb->lincol),
			   &(cdsUattr[jj].info.tcb->linwid));
		}
		else if (strcmp(vg_typestr, "-99") == 0) {
		    cdsUattr[jj].vg_type = DEFLTSET;
		}
	    }
    /*
     * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
     *    DIDN'T RECOGNIZE THE CLASS; BAD READ, IGNORE RECORD 
     * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
     */
	    else {
		*iret = 2;
	        loglev = 0;
		strcpy(grp, "CDS");
		er_lmsg (&loglev, grp, iret, attrfnam, &ier1, strlen(grp),
			 strlen(attrfnam));
	    }
	}
    }

    if ( !quit ) {
/*
 * User attribute array (cdsUattr) size exceeded.  MAX_SET should
 * be increased.
 */
	*iret = 1;
	loglev = 0;
	strcpy(grp, "CDS");
	er_lmsg (&loglev, grp, iret, attrfnam, &ier1, strlen(grp),
		 strlen(attrfnam));
    }

/* close the user attribute file */
    cfl_clos(fp, &ier);
}
