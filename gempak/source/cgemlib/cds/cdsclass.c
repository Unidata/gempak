#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"

void cds_class ( char *vg_classstr, const char *vg_typestr,
			int *vg_class, int *vg_type, int *iret )
/************************************************************************
 * cds_class								*
 *									*
 * This function converts VG_class string and vg_type string into their	*
 * corresponding integer value.						*
 *									*
 * cds_class ( vg_classstr, vg_typestr, vg_class, vg_type, iret )	*
 *									*
 * Input parameters:							*
 *	*vg_classstr	char		Name of VG class		*
 *	*vg_typestr	const char	Name of VG type			*
 *									*
 * Output parameters:							*
 *	*vg_class	int		VG class ID			*
 *	*vg_type	int		VG type ID			*
 *	*iret		int		Return code			*
 *					 -3 = Unknown VGF class string	*
 **									*
 * Log:									*
 * T. Piper/SAIC	12/05	Created					*
 * T. Piper/SAIC	12/05   redone with new Setting_t structure     *
 * m.gamazaychikov/SAIC	06/07	Added TCB, TCE, and TCT elements	*
 * L. Hinson/AWC        01/12   Add SGWX Element                        *
 ***********************************************************************/
{
    int		ier1, loglev;
    char	grp[4];
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    *vg_type = DEFLTSET;

    if ( strcmp(vg_classstr, "CLASS_FRONTS" ) == 0 ) {
	*vg_class = CLASS_FRONTS;
	if ( strcmp(vg_typestr, "FRONT_ELM" ) == 0 )
	    *vg_type = FRONT_ELM;
    }
    else if ( strcmp(vg_classstr, "CLASS_CIRCLE") == 0 ) {
	*vg_class = CLASS_CIRCLE;
	if (strcmp(vg_typestr, "CIRCLE_ELM") == 0) 
	    *vg_type = CIRCLE_ELM;
    }
    else if ( strcmp(vg_classstr, "CLASS_LINES") == 0 ) {
	*vg_class = CLASS_LINES;
	if (strcmp(vg_typestr, "LINE_ELM") == 0) 
	    *vg_type = LINE_ELM;
	else if (strcmp(vg_typestr, "SPLN_ELM") == 0) 
	    *vg_type = SPLN_ELM;
    }
    else if ( strcmp(vg_classstr, "CLASS_SYMBOLS") == 0 ) {
	*vg_class = CLASS_SYMBOLS;
	if ( strcmp(vg_typestr, "WXSYM_ELM") == 0)
	    *vg_type = WXSYM_ELM;
	else if ( strcmp(vg_typestr, "CTSYM_ELM") == 0)
	    *vg_type = CTSYM_ELM;
	else if ( strcmp(vg_typestr, "ICSYM_ELM") == 0)
	    *vg_type = ICSYM_ELM;
	else if ( strcmp(vg_typestr, "PTSYM_ELM") == 0)
	    *vg_type = PTSYM_ELM;
	else if ( strcmp(vg_typestr, "PWSYM_ELM") == 0)
	    *vg_type = PWSYM_ELM;
	else if ( strcmp(vg_typestr, "SKSYM_ELM") == 0)
	    *vg_type = SKSYM_ELM;
	else if ( strcmp(vg_typestr, "SPSYM_ELM") == 0)
	    *vg_type = SPSYM_ELM;
	else if ( strcmp(vg_typestr, "TBSYM_ELM") == 0)
	    *vg_type = TBSYM_ELM;
	else if ( strcmp(vg_typestr, "CMBSY_ELM") == 0)
	    *vg_type = CMBSY_ELM;
	else if ( strcmp(vg_typestr, "MARK_ELM" ) == 0 )
	    *vg_type = MARK_ELM;
    }
    else if ( strcmp(vg_classstr, "CLASS_WINDS") == 0 ) {
	*vg_class = CLASS_WINDS;
	if ( strcmp(vg_typestr, "ARROW_ELM") == 0 )
	    *vg_type = ARROW_ELM;
	else if ( strcmp(vg_typestr, "BARB_ELM") == 0 )
	    *vg_type = BARB_ELM;
	else if ( strcmp(vg_typestr, "DARR_ELM") == 0 )
	    *vg_type = DARR_ELM;
	else if ( strcmp(vg_typestr, "HASH_ELM") == 0 )
	    *vg_type = HASH_ELM;
    }
    else if ( strcmp(vg_classstr,"CLASS_WATCHES") == 0 ) {
	*vg_class = CLASS_WATCHES;
	if (strcmp(vg_typestr, "WBOX_ELM") == 0)
	    *vg_type = WBOX_ELM;
    }
    else if ( strcmp(vg_classstr, "CLASS_TEXT") == 0 ) {
	*vg_class = CLASS_TEXT;
	if (strcmp(vg_typestr, "TEXT_ELM") == 0)
	    *vg_type = TEXT_ELM;
	else if (strcmp(vg_typestr, "TEXTC_ELM") == 0)
	    *vg_type = TEXTC_ELM;
	else if (strcmp(vg_typestr, "SPTX_ELM") == 0)
	    *vg_type = SPTX_ELM;
    }
    else if ( strcmp(vg_classstr, "CLASS_TRACKS") == 0 ) {
	*vg_class = CLASS_TRACKS;
	if ( strcmp(vg_typestr, "TRKSTORM_ELM" ) == 0 )
	    *vg_type = TRKSTORM_ELM;
    }
    else if (strcmp(vg_classstr, "CLASS_SIGMETS") == 0) {
	*vg_class = CLASS_SIGMETS;
	if ( strcmp(vg_typestr, "SIGAIRM_ELM" ) == 0 )
	    *vg_type = SIGAIRM_ELM;
	else if ( strcmp(vg_typestr, "SIGCONV_ELM" ) == 0 )
	    *vg_type = SIGCONV_ELM;
	else if ( strcmp(vg_typestr, "SIGINTL_ELM" ) == 0 )
	    *vg_type = SIGINTL_ELM;
	else if ( strcmp(vg_typestr, "SIGNCON_ELM" ) == 0 )
	    *vg_type = SIGNCON_ELM;
	else if ( strcmp(vg_typestr, "SIGOUTL_ELM" ) == 0 )
	    *vg_type = SIGOUTL_ELM;
	else if ( strcmp(vg_typestr, "SIGCCF_ELM" ) == 0 )
	    *vg_type = SIGCCF_ELM;
	else if ( strcmp(vg_typestr, "VOLC_ELM" ) == 0 )
	    *vg_type = VOLC_ELM;
	else if ( strcmp(vg_typestr, "ASHCLD_ELM" ) == 0 )
	    *vg_type = ASHCLD_ELM;
    }
    else if ( strcmp(vg_classstr,"CLASS_LIST") == 0 ) {
	*vg_class = CLASS_LIST;
	if (strcmp(vg_typestr, "LIST_ELM") == 0)
	    *vg_type = LIST_ELM;
	else if (strcmp(vg_typestr, "-99") == 0)
	    *vg_type = DEFLTSET;
    }
    else if ( strcmp(vg_classstr,"CLASS_MET") == 0 ) {
	*vg_class = CLASS_MET;
	if (strcmp(vg_typestr, "JET_ELM") == 0)
	    *vg_type = JET_ELM;
	else if (strcmp(vg_typestr, "GFA_ELM") == 0)
	    *vg_type = GFA_ELM;
	else if (strcmp(vg_typestr, "SGWX_ELM") == 0)
	    *vg_type = SGWX_ELM;
	else if (strcmp(vg_typestr, "TCA_ELM") == 0)
	    *vg_type = TCA_ELM;
	else if (strcmp(vg_typestr, "TCERR_ELM") == 0)
	    *vg_type = TCERR_ELM;
	else if (strcmp(vg_typestr, "TCTRK_ELM") == 0)
	    *vg_type = TCTRK_ELM;
	else if (strcmp(vg_typestr, "TCBKL_ELM") == 0)
	    *vg_type = TCBKL_ELM;
    }
    /*
     * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
     *    DIDN'T RECOGNIZE THE CLASS; BAD READ, IGNORE RECORD 
     * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
     */
    else {
	*iret = -3;
        loglev = 2;
	strcpy(grp, "CDS");
	er_lmsg (&loglev, grp, iret, vg_classstr, &ier1, 
			strlen(grp), strlen(vg_classstr));
    }
}
