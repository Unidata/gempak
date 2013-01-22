#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"

void cds_atdeflt ( int *iret )
/************************************************************************
 * cds_atdeflt								*
 *									*
 * This function sets default values in the CDS_global variables used	*
 * to override element attributes.  The default values used are for no	*
 * changes.								*
 *									*
 * cds_atdeflt ( iret )							*
 *									*
 * Input parameters:							*
 *	None								*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * F. J. Yen/NCEP	10/99	Created.				*
 * S. Law/GSC		02/00	Added CCF				*
 * E. Safford/SAIC	02/02	added init of new_subtyp		*
 * H. Zeng/XTRIA	05/03   added Watchbox marker info		*
 * J. Wu/SAIC		06/03   added LIST info				*
 * H. Zeng/XTRIA	07/03	added volcano element			*
 * J. Wu/SAIC		09/03   added JET info				*
 * H. Zeng/XTRIA	09/03   added ash cloud info			*
 * J. Wu/SAIC		01/04   added GFA_ELM				*
 * B. Yin/SAIC		02/04	added TCA_ELMM				*
 * J. Wu/SAIC		05/04	add init of post-processing "ppid"	*
 * J. Wu/SAIC		10/04   remove line width from GFA_ELM		*
 * S. Gilbert/NCEP      11/05   added text_lat, text_lon, text_font     *
 *                              text_size, and text_width to TCA        *
 * T. Piper/SAIC	12/05	redone with new Setting_t structure	*
 * B. Yin/SAIC		01/06	added line width for GFA		*
 * S. Gilbert/NCEP	01/06	Changed TCA advisoryNum from int to char*
 * S. Danz/AWC		04/06	Changed G_MALLOCs to G_CALLOCs		*
 * B. Yin/SAIC		01/06	added line elm and type for GFA		*
 * B. Yin/SAIC		01/06	set default values to 0 for GFA		*
 * m.gamazaychikov/SAIC	07/07	added TCERR, TCTRK and TCBKL elements	*
 * L. Hinson/AWC        07/09   Added G_CALLOC for CCF                  *
 * L. Hinson/AWC        01/12   Added G_CALLOC for SGWX; Set numUset 26 *
 ***********************************************************************/
{
    int		jj, one=1;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    if ( numUset > 0 ) {
	for ( jj = 0; jj < numUset; jj++ ) {
	    G_FREE(cdsUattr[jj].info.jet, JetInfo );
	}
	G_FREE(cdsUattr, Setting_t);
    }
    
    numUset = 26;
    G_CALLOC(cdsUattr, Setting_t, numUset, "cdsUattr");

    for  ( jj = 0; jj <  numUset; jj++ ) {
	cdsUattr[jj].subtyp  = DEFLTSET;
	cdsUattr[jj].smooth  = -1;
	cdsUattr[jj].cds_or_ces.new_subtyp = DEFLTSET;
	}

    cdsUattr[0].vg_class = CLASS_FRONTS;
    cdsUattr[0].vg_type	 = DEFLTSET;
    G_CALLOC(cdsUattr[0].info.frt, FrontInfo, one, "cdsUattr[0].info.frt");

    cdsUattr[1].vg_class = CLASS_CIRCLE;
    cdsUattr[1].vg_type	 = CIRCLE_ELM;
    G_CALLOC(cdsUattr[1].info.cir, LineInfo, one, "cdsUattr[1].info.cir");

    cdsUattr[2].vg_class = CLASS_LINES;
    cdsUattr[2].vg_type	 = LINE_ELM;
    G_CALLOC(cdsUattr[2].info.lin, LineInfo, one, "cdsUattr[2].info.lin");

    cdsUattr[3].vg_class = CLASS_LINES;
    cdsUattr[3].vg_type	 = SPLN_ELM;
    G_CALLOC(cdsUattr[3].info.spl, SpLineInfo, one, "cdsUattr[3].info.spl");
			
    cdsUattr[4].vg_class = CLASS_SYMBOLS;
    cdsUattr[4].vg_type	 = DEFLTSET;
    G_CALLOC(cdsUattr[4].info.sym, SymType, one, "cdsUattr[4].info.sym");

    cdsUattr[5].vg_class = CLASS_WINDS;
    cdsUattr[5].vg_type	 = ARROW_ELM;
    G_CALLOC(cdsUattr[5].info.wnd, WindInfo, one, "cdsUattr[5].info.wnd");

    cdsUattr[6].vg_class = CLASS_WINDS;
    cdsUattr[6].vg_type	 = BARB_ELM;
    G_CALLOC(cdsUattr[6].info.wnd, WindInfo, one, "cdsUattr[6].info.wnd");

    cdsUattr[7].vg_class = CLASS_WINDS;
    cdsUattr[7].vg_type	 = DARR_ELM;
    G_CALLOC(cdsUattr[7].info.wnd, WindInfo, one, "cdsUattr[7].info.wnd");

    cdsUattr[8].vg_class = CLASS_WINDS;
    cdsUattr[8].vg_type	 = HASH_ELM;
    G_CALLOC(cdsUattr[8].info.wnd, WindInfo, one, "cdsUattr[8].info.wnd");

    cdsUattr[9].vg_class = CLASS_WATCHES;
    cdsUattr[9].vg_type	 = DEFLTSET;
    G_CALLOC(cdsUattr[9].info.wbx, WboxAttr, one, "cdsUattr[9].info.wbx");

    cdsUattr[10].vg_class = CLASS_TEXT;
    cdsUattr[10].vg_type  = TEXT_ELM;
    G_CALLOC(cdsUattr[10].info.txt, TextType, one, "cdsUattr[10].info.txt");

    cdsUattr[11].vg_class = CLASS_TEXT;
    cdsUattr[11].vg_type  = TEXTC_ELM;
    G_CALLOC(cdsUattr[11].info.txt, TextType, one, "cdsUattr[11].info.txt");

    cdsUattr[12].vg_class = CLASS_TEXT;
    cdsUattr[12].vg_type  = SPTX_ELM;
    G_CALLOC(cdsUattr[12].info.spt, SpTxtAttr, one, "cdsUattr[12].info.spt");
    strcpy ( cdsUattr[12].info.spt->ppid, DEFLTPPID );

    cdsUattr[13].vg_class = CLASS_TRACKS;
    cdsUattr[13].vg_type  = DEFLTSET;
    G_CALLOC(cdsUattr[13].info.trk, TrackInfo, one, "cdsUattr[13].info.trk");

    cdsUattr[14].vg_class = CLASS_SIGMETS;
    cdsUattr[14].vg_type  = DEFLTSET;
    G_CALLOC(cdsUattr[14].info.sig, SigAttr, one, "cdsUattr[14].info.sig");

    cdsUattr[15].vg_class = CLASS_SIGMETS;
    cdsUattr[15].vg_type  = SIGCCF_ELM;
    G_CALLOC(cdsUattr[15].info.ccf, CcfAttr, one, "cdsUattr[15].info.ccf");
    
    cdsUattr[16].vg_class = CLASS_SIGMETS;
    cdsUattr[16].vg_type  = VOLC_ELM;
    G_CALLOC(cdsUattr[16].info.vol, VolAttr, one, "cdsUattr[16].info.vol");

    cdsUattr[17].vg_class = CLASS_SIGMETS;
    cdsUattr[17].vg_type  = ASHCLD_ELM;
    G_CALLOC(cdsUattr[17].info.ash, SigAttr, one, "cdsUattr[17].info.ash");

    cdsUattr[18].vg_class = CLASS_LIST;
    cdsUattr[18].vg_type  = LIST_ELM;
    G_CALLOC(cdsUattr[18].info.lst, ListInfo, one, "cdsUattr[18].info.lst");

    cdsUattr[19].vg_class = CLASS_MET;
    cdsUattr[19].vg_type  = JET_ELM;
    G_CALLOC(cdsUattr[19].info.jet, JetInfo, one, "cdsUattr[19].info.jet");
    
    cdsUattr[20].vg_class = CLASS_MET;
    cdsUattr[20].vg_type  = GFA_ELM;
    G_CALLOC(cdsUattr[20].info.gfa, GfaAttr, one, "cdsUattr[20].info.gfa");

    cdsUattr[21].vg_class = CLASS_MET;
    cdsUattr[21].vg_type  = TCA_ELM;
    G_CALLOC(cdsUattr[21].info.tca, TcaInfo, one, "cdsUattr[21].info.tca");
    strcpy ( cdsUattr[21].info.tca->advisoryNum,	"1" );
    cdsUattr[21].info.tca->stormNum = 1;
    cdsUattr[21].info.tca->basin = ATLANTIC_BASIN;
    cdsUattr[21].info.tca->text_lat = 25.8F;
    cdsUattr[21].info.tca->text_lon = -80.4F;
    cdsUattr[21].info.tca->text_font = 1;
    cdsUattr[21].info.tca->text_size = 1.0F;
    cdsUattr[21].info.tca->text_width = 3;

    cdsUattr[22].vg_class = CLASS_MET;
    cdsUattr[22].vg_type  = TCTRK_ELM;
    G_CALLOC(cdsUattr[22].info.tct, TctAttr, one, "cdsUattr[22].info.tct");

    cdsUattr[23].vg_class = CLASS_MET;
    cdsUattr[23].vg_type  = TCERR_ELM;
    G_CALLOC(cdsUattr[23].info.tce, TceAttr, one, "cdsUattr[23].info.tce");

    cdsUattr[24].vg_class = CLASS_MET;
    cdsUattr[24].vg_type  = TCBKL_ELM;
    G_CALLOC(cdsUattr[24].info.tcb, TcbAttr, one, "cdsUattr[24].info.tcb");
    
    cdsUattr[25].vg_class = CLASS_MET;
    cdsUattr[25].vg_type = SGWX_ELM;
    G_CALLOC(cdsUattr[25].info.sgwx, SgwxAttr, one, "cdsUattr[25].info.sgwx");

}

