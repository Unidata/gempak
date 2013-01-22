#include "cvgcmn.h"

/* Local prototypes 	*/
void	tag2fhed ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2hdr ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2frt ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2lin ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2spl ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2wbx ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2wsm ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2sym ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2wnd ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2txt ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2spt ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2cir ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2trk ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2sig ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2ccf ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2lst ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2ash ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2vol ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2jet ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2gfa ( char *buffer, VG_DBStruct *el, int *iret );
void    tag2sgwx ( char *buffer, VG_DBStruct *el, int *iret );
void	tag2tca ( char *buffer, VG_DBStruct *el, int *iret );

/* Local global variables 	*/
char	*_Data;

int	cvg_t2v ( char *buffer, VG_DBStruct *el, int *iret )
/************************************************************************
 * cvg_t2v                                                          	*
 *                                                                      *
 * This function converts an element stored as an ASCII tag string to	*
 * a VGF element.							*
 * For example:								*
 * <vg_type>1<vg_class>1<delete>0 ... etc.				*
 *                                                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *      *buffer         char            Element in tag format		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *el             VG_DBStruct     PGEN Element			*
 *      *iret           int             Return code                     *
 **                                                                     *
 * D.W.Plummer/NCEP	06/03						*
 * D.W.Plummer/NCEP	07/03	Added tag2ash and tag2vol		*
 * J. Wu/SAIC		09/03	add tag2jet & modify tag2hdr		*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * H. Zeng/XTRIA	11/03   modified a field for ASHCLD		*
 * J. Wu/SAIC		01/03	add tag2gfa 				*
 * J. Wu/SAIC		02/04	added lat/lon text position for GFA_ELM	*
 * B. Yin/SAIC		02/04	added tag2tca				*
 * B. Yin/SAIC		03/04	added strom type in tca structre	*
 * B. Yin/SAIC		05/04	modified tag2tca to read out rcd size	*
 * J. Wu/SAIC		05/04	add weather type description in tag2gfa	*
 * B. Yin/SAIC		07/04	modified tag2tca for more than 2 bkpts	*
 * J. Wu/SAIC		10/04	set GFA fields with cvg_setFld()	*
 * B. Yin/SAIC		11/04	modified tag2gfa for any type of tags   *
 * B. Yin/SAIC		12/04	added timezone in TCAs			*
 * B. Yin/SAIC		04/05	added issueStatus in TCAs, removed year	*
 * S. Jacobs/NCEP	 5/05	Added checks for 0 points for lines	*
 * Q. Zhou/Chug	         3/10	Fixed Track-Times on notes were shifted	*
 * Q. Zhou/Chug	         6/10	Fixed Watch box-String wasn't displayed	*
 * Q. Zhou/Chug  	 7/10	Fixed Jet- Chaged line_latlon to latlon	*
 * Q. Zhou/Chug  	10/10	Fixed Ash- spt text wasn't displayed	*
 *                              Fixed Ccfp- text & arrow weren't displayed*
 ***********************************************************************/
{
int	ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    _Data = (char *)malloc( strlen(buffer) * sizeof(char) );
 
    cst_gtag ( "vg_type", buffer, "UNKNOWN", _Data, &ier );

    if ( strcmp(_Data,"UNKNOWN") == 0 || ier != 0 )  {
	*iret = -1;
    }
    else  {

	tag2hdr ( buffer, el, &ier );
 
	switch ( el->hdr.vg_type )  {

	    case	FILEHEAD_ELM:
		tag2fhed ( buffer, el, &ier );
		break;

	    case	FRONT_ELM:
		tag2frt ( buffer, el, &ier );
		break;

	    case	LINE_ELM:
		tag2lin ( buffer, el, &ier );
		break;

	    case	SPLN_ELM:
		tag2spl ( buffer, el, &ier );
		break;

	    case	WBOX_ELM:
		tag2wbx ( buffer, el, &ier );
		break;

	    case	WSM_ELM:
		tag2wsm ( buffer, el, &ier );
		break;

	    case	WXSYM_ELM:
	    case	CTSYM_ELM:
	    case	ICSYM_ELM:
	    case	PTSYM_ELM:
	    case	PWSYM_ELM:
	    case	SKSYM_ELM:
	    case	SPSYM_ELM:
	    case	TBSYM_ELM:
	    case	MARK_ELM:
	    case	CMBSY_ELM:
		tag2sym ( buffer, el, &ier );
		break;

	    case	BARB_ELM:
	    case	ARROW_ELM:
	    case	DARR_ELM:
	    case	HASH_ELM:
		tag2wnd ( buffer, el, &ier );
		break;

	    case	TEXT_ELM:
	    case	TEXTC_ELM:
		tag2txt ( buffer, el, &ier );
		break;

	    case	SPTX_ELM:
		tag2spt ( buffer, el, &ier );
		break;

	    case	CIRCLE_ELM:
		tag2cir ( buffer, el, &ier );
		break;

	    case	TRKSTORM_ELM:
		tag2trk ( buffer, el, &ier );
		break;

	    case	SIGINTL_ELM:
	    case	SIGNCON_ELM:
	    case	SIGCONV_ELM:
	    case	SIGOUTL_ELM:
	    case	SIGAIRM_ELM:
		tag2sig ( buffer, el, &ier );
		break;

	    case	SIGCCF_ELM:
		tag2ccf ( buffer, el, &ier );
		break;

	    case	LIST_ELM:
		tag2lst ( buffer, el, &ier );
		break;

	    case	ASHCLD_ELM:
		tag2ash ( buffer, el, &ier );
		break;

	    case	VOLC_ELM:
		tag2vol ( buffer, el, &ier );
		break;

	    case	JET_ELM:
		tag2jet ( buffer, el, &ier );
		break;

	    case	GFA_ELM:
		tag2gfa ( buffer, el, &ier );
		break;
            
	    case        SGWX_ELM:
                tag2sgwx ( buffer, el, &ier );
                break;

	    case	TCA_ELM:
		tag2tca ( buffer, el, &ier );
		break;
	    
	    default:
		*iret = -1;

	    break;

	}

    }

    free ( _Data );

    return ( 0 );

}

/*=====================================================================*/

void	tag2fhed ( char *buffer, VG_DBStruct *el, int *iret )
{
int     ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  File Header
     */

    cst_gtag ( "version", buffer, "", el->elem.fhed.version, &ier );

    cst_gtag ( "notes", buffer, "", el->elem.fhed.notes, &ier );

    el->hdr.recsz = ( sizeof(VG_HdrStruct) + sizeof(FileHeadType) );

    *iret = ier;

}

/*=====================================================================*/

void	tag2hdr ( char *buffer, VG_DBStruct *el, int *iret )
{
int	intg, ier, one = 1;
char	inch[16];
VG_DBStruct	*el_init;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  Get all the common header stuff first.
     *  (recsz and ranges will be computed later as part of
     *  each element's specific contents)
     */

    cst_gtag ( "vg_type", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &intg, &ier );
    el->hdr.vg_type = (char)intg;

    cst_gtag ( "vg_class", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &intg, &ier );
    el->hdr.vg_class = (char)intg;
    G_CALLOC(el_init, VG_DBStruct, one, "tag2hdr:  VG_DBStruct");
    el_init->hdr.vg_class = el->hdr.vg_class;
    el_init->hdr.vg_type = el->hdr.vg_type;    
    
    cvg_initelm ( el_init );  /* this function needs VG class & type */

    cst_inch ( (int)el_init->hdr.delete, inch, &ier );
    cst_gtag ( "delete", buffer, inch, _Data, &ier );
    cst_numb ( _Data, &intg, &ier );
    el->hdr.delete = (char)intg;

    cst_inch ( (int)el_init->hdr.filled, inch, &ier );
    cst_gtag ( "filled", buffer, inch, _Data, &ier );
    cst_numb ( _Data, &intg, &ier );
    el->hdr.filled = (char)intg;

    cst_inch ( (int)el_init->hdr.closed, inch, &ier );
    cst_gtag ( "closed", buffer, inch, _Data, &ier );    
    cst_numb ( _Data, &intg, &ier );
    el->hdr.closed = (char)intg;

    cst_inch ( (int)el_init->hdr.smooth, inch, &ier );
    cst_gtag ( "smooth", buffer, inch, _Data, &ier );
    cst_numb ( _Data, &intg, &ier );
    el->hdr.smooth = (char)intg;

    cst_inch ( (int)el_init->hdr.version, inch, &ier );
    cst_gtag ( "version", buffer, inch, _Data, &ier );
    cst_numb ( _Data, &intg, &ier );
    el->hdr.version = (char)intg;

    cst_inch ( (int)el_init->hdr.grptyp, inch, &ier );
    cst_gtag ( "grptyp", buffer, inch, _Data, &ier );
    cst_numb ( _Data, &intg, &ier );
    el->hdr.grptyp = (char)intg;

    cst_inch ( el_init->hdr.grpnum, inch, &ier );
    cst_gtag ( "grpnum", buffer, inch, _Data, &ier );
    cst_numb ( _Data, &(el->hdr.grpnum), &ier );

    cst_inch ( el_init->hdr.maj_col, inch, &ier );
    cst_gtag ( "maj_col", buffer, inch, _Data, &ier );
    cst_numb ( _Data, &(el->hdr.maj_col), &ier );

    cst_inch ( el_init->hdr.min_col, inch, &ier );
    cst_gtag ( "min_col", buffer, inch, _Data, &ier );
    cst_numb ( _Data, &(el->hdr.min_col), &ier );

    cst_gtag ( "range_min_lat", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->hdr.range_min_lat), &ier );

    cst_gtag ( "range_min_lon", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->hdr.range_min_lon), &ier );

    cst_gtag ( "range_max_lat", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->hdr.range_max_lat), &ier );

    cst_gtag ( "range_max_lon", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->hdr.range_max_lon), &ier );
    G_FREE(el_init, VG_DBStruct);
}

/*=====================================================================*/

void	tag2frt ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  FRONT_ELM
     */
    cst_gtag ( "numpts", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.frt.info.numpts), &ier );

    cst_gtag ( "fcode", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.frt.info.fcode), &ier );

    cst_gtag ( "fpipsz", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.frt.info.fpipsz), &ier );

    cst_gtag ( "fpipst", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.frt.info.fpipst), &ier );

    cst_gtag ( "fpipdr", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.frt.info.fpipdr), &ier );

    cst_gtag ( "fwidth", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.frt.info.fwidth), &ier );

    cst_gtag ( "frtlbl", buffer, "1", el->elem.frt.info.frtlbl, &ier );

    npts = el->elem.frt.info.numpts;
    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.frt.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    if  ( npts > 0 )  {
	el->hdr.range_min_lat =  FLT_MAX;
	el->hdr.range_max_lat = -FLT_MAX;
	el->hdr.range_min_lon =  FLT_MAX;
	el->hdr.range_max_lon = -FLT_MAX;
	for ( ii = 0; ii < npts; ii++ )  {
	    el->hdr.range_min_lat = 
		G_MIN(el->hdr.range_min_lat, el->elem.frt.latlon[ii]);
	    el->hdr.range_max_lat = 
		G_MAX(el->hdr.range_max_lat, el->elem.frt.latlon[ii]);
	    el->hdr.range_min_lon = 
		G_MIN(el->hdr.range_min_lon, el->elem.frt.latlon[ii+npts]);
	    el->hdr.range_max_lon = 
		G_MAX(el->hdr.range_max_lon, el->elem.frt.latlon[ii+npts]);
	}
    }

    el->hdr.recsz = (int)( (sizeof(float) * (size_t)(2 * npts)) + sizeof(VG_HdrStruct)
        + sizeof(FrontInfo) );

}

/*=====================================================================*/

void	tag2lin ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  LINE_ELM
     */
    cst_gtag ( "numpts", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.lin.info.numpts), &ier );

    cst_gtag ( "lintyp", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.lin.info.lintyp), &ier );

    cst_gtag ( "lthw", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.lin.info.lthw), &ier );

    cst_gtag ( "width", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.lin.info.width), &ier );

    cst_gtag ( "lwhw", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.lin.info.lwhw), &ier );

    npts = el->elem.lin.info.numpts;
    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.lin.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    if  ( npts > 0 )  {
	el->hdr.range_min_lat =  FLT_MAX;
	el->hdr.range_max_lat = -FLT_MAX;
	el->hdr.range_min_lon =  FLT_MAX;
	el->hdr.range_max_lon = -FLT_MAX;
	for ( ii = 0; ii < npts; ii++ )  {
	    el->hdr.range_min_lat = 
		G_MIN(el->hdr.range_min_lat, el->elem.lin.latlon[ii]);
	    el->hdr.range_max_lat = 
		G_MAX(el->hdr.range_max_lat, el->elem.lin.latlon[ii]);
	    el->hdr.range_min_lon = 
		G_MIN(el->hdr.range_min_lon, el->elem.lin.latlon[ii+npts]);
	    el->hdr.range_max_lon = 
		G_MAX(el->hdr.range_max_lon, el->elem.lin.latlon[ii+npts]);
	}
    }

    el->hdr.recsz = (int)( (sizeof(float) * (size_t)(2 * npts)) + sizeof(VG_HdrStruct)
        + sizeof(LineInfo) );

}

/*=====================================================================*/

void	tag2spl ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  SPLN_ELM
     */
    cst_gtag ( "numpts", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spl.info.numpts), &ier );

    cst_gtag ( "spltyp", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spl.info.spltyp), &ier );

    cst_gtag ( "splstr", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spl.info.splstr), &ier );

    cst_gtag ( "spldir", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spl.info.spldir), &ier );

    cst_gtag ( "splsiz", buffer, "1", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.spl.info.splsiz), &ier );

    cst_gtag ( "splwid", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spl.info.splwid), &ier );

    npts = el->elem.spl.info.numpts;
    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.spl.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    if  ( npts > 0 )  {
	el->hdr.range_min_lat =  FLT_MAX;
	el->hdr.range_max_lat = -FLT_MAX;
	el->hdr.range_min_lon =  FLT_MAX;
	el->hdr.range_max_lon = -FLT_MAX;
	for ( ii = 0; ii < npts; ii++ )  {
	    el->hdr.range_min_lat = 
		G_MIN(el->hdr.range_min_lat, el->elem.spl.latlon[ii]);
	    el->hdr.range_max_lat = 
		G_MAX(el->hdr.range_max_lat, el->elem.spl.latlon[ii]);
	    el->hdr.range_min_lon = 
		G_MIN(el->hdr.range_min_lon, el->elem.spl.latlon[ii+npts]);
	    el->hdr.range_max_lon = 
		G_MAX(el->hdr.range_max_lon, el->elem.spl.latlon[ii+npts]);
	}
    }

    el->hdr.recsz = (int)( (sizeof(float) * (size_t)(2 * npts)) + sizeof(VG_HdrStruct)
        + sizeof(SpLineInfo) );

}

/*=====================================================================*/

void	tag2wbx ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, *ivals, npts, nvals, ncnty, ier;
float	*fvals;
char	data[MAXPTS*2*12], delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  WBOX_ELM
     */
    cst_gtag ( "numpts", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.numpts), &ier );

    cst_gtag ( "w_style", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.w_style), &ier );

    cst_gtag ( "w_shape", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.w_shape), &ier );

    cst_gtag ( "w_mrktyp", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.w_mrktyp), &ier );

    cst_gtag ( "w_mrksiz", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.wbx.info.w_mrksiz), &ier );

    cst_gtag ( "w_mrkwid", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.w_mrkwid), &ier );

    cst_gtag ( "w_a0id", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_a0id, _Data );
    cst_gtag ( "w_a0lt", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.wbx.info.w_a0lt), &ier );
    cst_gtag ( "w_a0ln", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.wbx.info.w_a0ln), &ier );
    cst_gtag ( "w_a0dis", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.w_a0dis), &ier );
    cst_gtag ( "w_a0dir", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_a0dir, _Data );

    cst_gtag ( "w_a1id", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_a1id, _Data );
    cst_gtag ( "w_a1lt", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.wbx.info.w_a1lt), &ier );
    cst_gtag ( "w_a1ln", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.wbx.info.w_a1ln), &ier );
    cst_gtag ( "w_a1dis", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.w_a1dis), &ier );
    cst_gtag ( "w_a1dir", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_a1dir, _Data );

    cst_gtag ( "w_istat", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.w_istat), &ier );
    cst_gtag ( "w_number", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.w_number), &ier );
    cst_gtag ( "w_iss_t", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_iss_t, _Data );
    cst_gtag ( "w_exp_t", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_exp_t, _Data );
    cst_gtag ( "w_type", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.w_type), &ier );
    cst_gtag ( "w_severity", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.w_severity), &ier );

    cst_gtag ( "w_timezone", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_timezone, _Data );
    cst_gtag ( "w_hailsz", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_hailsz, _Data );
    cst_gtag ( "w_windg", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_windg, _Data );
    cst_gtag ( "w_tops", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_tops, _Data );
    cst_gtag ( "w_msmv_d", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_msmv_d, _Data );
    cst_gtag ( "w_msmv_s", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_msmv_s, _Data );
    cst_gtag ( "w_states", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_states, _Data );
    cst_gtag ( "w_adjarea", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_adjarea, _Data );
    cst_gtag ( "w_replw", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_replw, _Data );
    cst_gtag ( "w_fcstr", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_fcstr, _Data );
    cst_gtag ( "w_file", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.w_file, _Data );
    cst_gtag ( "w_issued", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.w_issued), &ier );

    cst_gtag ( "wsm_iss_t", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.wsm_iss_t, _Data );
    cst_gtag ( "wsm_exp_t", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.wsm_exp_t, _Data );
    cst_gtag ( "wsm_ref", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.wsm_ref, _Data );
    cst_gtag ( "wsm_from", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.wsm_from, _Data );
    cst_gtag ( "wsm_meso", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.wsm_meso, _Data );
    cst_gtag ( "wsm_fcstr", buffer, "0", _Data, &ier );
    strcpy ( el->elem.wbx.info.wsm_fcstr, _Data );

    cst_gtag ( "numcnty", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.numcnty), &ier );
    cst_gtag ( "cn_flag", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wbx.info.cn_flag), &ier );

    npts = el->elem.wbx.info.numpts;
    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.wbx.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    ncnty = el->elem.wbx.info.numcnty;
    ivals = (int *)malloc((size_t)ncnty*sizeof(int));
    cst_gtag ( "cn_fips", buffer, "0", _Data, &ier );
    cst_ilst ( _Data, delim, IMISSD, ncnty, ivals, &nvals, &ier );
    memmove ( el->elem.wbx.info.cn_fips, ivals, (size_t)nvals*sizeof(int) );
    free ( ivals );

    fvals = (float *)malloc((size_t)(ncnty*2)*sizeof(float));
    cst_gtag ( "cn_ltln", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, ncnty*2, fvals, &nvals, &ier );
    memmove ( el->elem.wbx.info.cn_ltln, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;

    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.wbx.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.wbx.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.wbx.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.wbx.latlon[ii+npts]);
    }

    for ( ii = 0; ii < ncnty; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.wbx.info.cn_ltln[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.wbx.info.cn_ltln[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.wbx.info.cn_ltln[ii+ncnty]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.wbx.info.cn_ltln[ii+ncnty]);
    }

    el->hdr.recsz = (int)( (sizeof(float) * (size_t)(2 * npts)) + sizeof(VG_HdrStruct)
        + sizeof(WatchBoxInfo) );

}

/*=====================================================================*/

void	tag2wsm ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  WSM_ELM
     */
    cst_gtag ( "numpts", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wsm.info.numpts), &ier );

    cst_gtag ( "spltyp", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wsm.info.spltyp), &ier );

    cst_gtag ( "splstr", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wsm.info.splstr), &ier );

    cst_gtag ( "spldir", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wsm.info.spldir), &ier );

    cst_gtag ( "splsiz", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.wsm.info.splsiz), &ier );

    cst_gtag ( "splwid", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wsm.info.splwid), &ier );

    npts = el->elem.wsm.info.numpts;
    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.wsm.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.wsm.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.wsm.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.wsm.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.wsm.latlon[ii+npts]);
    }

    el->hdr.recsz = (int)( (sizeof(float) * (size_t)(2 * npts)) + sizeof(VG_HdrStruct)
        + sizeof(SpLineInfo) );

}

/*=====================================================================*/

void	tag2sym ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, *ivals, nvals, ier;
float	*fvals;
char	delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  WSM_ELM
     */
    cst_gtag ( "numsym", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sym.info.numsym), &ier );

    cst_gtag ( "width", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sym.info.width), &ier );

    cst_gtag ( "size", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.sym.info.size), &ier );

    cst_gtag ( "ityp", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sym.info.ityp), &ier );

    cst_gtag ( "code", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, el->elem.sym.data.code, &ier );

    npts = el->elem.sym.info.numsym;
    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.sym.data.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    ivals = (int *)malloc((size_t)(npts*2)*sizeof(int));
    cst_gtag ( "offset_xy", buffer, "0", _Data, &ier );
    cst_ilst ( _Data, delim, IMISSD, npts*2, ivals, &nvals, &ier );
    memmove ( el->elem.sym.data.offset_xy, ivals, (size_t)nvals*sizeof(int) );
    free ( ivals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.sym.data.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.sym.data.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.sym.data.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.sym.data.latlon[ii+npts]);
    }

    el->hdr.recsz = ( sizeof(VG_HdrStruct)
        + sizeof(SymInfo) + sizeof(SymData) );

}

/*=====================================================================*/

void	tag2wnd ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  BARB_ELM, ARROW_ELM, DARR_ELM, HASH_ELM
     */
    cst_gtag ( "numwnd", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wnd.info.numwnd), &ier );

    cst_gtag ( "width", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wnd.info.width), &ier );

    cst_gtag ( "size", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.wnd.info.size), &ier );

    cst_gtag ( "wndtyp", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.wnd.info.wndtyp), &ier );

    cst_gtag ( "hdsiz", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.wnd.info.hdsiz), &ier );

    npts = el->elem.wnd.info.numwnd;

    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "spddir", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.wnd.data.spddir, fvals, (size_t)nvals*sizeof(float) );

    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.wnd.data.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.wnd.data.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.wnd.data.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.wnd.data.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.wnd.data.latlon[ii+npts]);
    }

    el->hdr.recsz = ( sizeof(VG_HdrStruct)
        + sizeof(WindInfo) + sizeof(WindData) );

}

/*=====================================================================*/

void	tag2txt ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  TEXT_ELM, TEXTC_ELM
     */
    cst_gtag ( "rotn", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.txt.info.rotn), &ier );

    cst_gtag ( "sztext", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.txt.info.sztext), &ier );

    cst_gtag ( "itxfn", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.txt.info.itxfn), &ier );

    cst_gtag ( "ithw", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.txt.info.ithw), &ier );

    cst_gtag ( "iwidth", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.txt.info.iwidth), &ier );

    cst_gtag ( "ialign", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.txt.info.ialign), &ier );

    cst_gtag ( "lat", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.txt.info.lat), &ier );

    cst_gtag ( "lon", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.txt.info.lon), &ier );

    cst_gtag ( "offset_x", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.txt.info.offset_x), &ier );

    cst_gtag ( "offset_y", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.txt.info.offset_y), &ier );

    cst_gtag ( "text", buffer, "0", el->elem.txt.text, &ier );
    while ( strstr(el->elem.txt.text,"$$") != (char *)NULL )
        cst_rpst ( el->elem.txt.text, "$$", "\n", el->elem.txt.text, &ier );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.txt.info.lat);
    el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.txt.info.lat);
    el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.txt.info.lon);
    el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.txt.info.lon);

    el->hdr.recsz = (int)( sizeof(VG_HdrStruct)
        + sizeof(TextInfo) + strlen(el->elem.txt.text)+1 );

}

/*=====================================================================*/

void	tag2spt ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  SPTX_ELM
     */
    cst_gtag ( "rotn", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.spt.info.rotn), &ier );

    cst_gtag ( "sztext", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.spt.info.sztext), &ier );

    cst_gtag ( "sptxtyp", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spt.info.sptxtyp), &ier );

    cst_gtag ( "turbsym", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spt.info.turbsym), &ier );

    cst_gtag ( "itxfn", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spt.info.itxfn), &ier );

    cst_gtag ( "ithw", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spt.info.ithw), &ier );

    cst_gtag ( "iwidth", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spt.info.iwidth), &ier );

    cst_gtag ( "txtcol", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spt.info.txtcol), &ier );

    cst_gtag ( "lincol", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spt.info.lincol), &ier );

    cst_gtag ( "filcol", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spt.info.filcol), &ier );

    cst_gtag ( "ialign", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spt.info.ialign), &ier );

    cst_gtag ( "lat", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.spt.info.lat), &ier );

    cst_gtag ( "lon", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.spt.info.lon), &ier );

    cst_gtag ( "offset_x", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spt.info.offset_x), &ier );

    cst_gtag ( "offset_y", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.spt.info.offset_y), &ier );

    cst_gtag ( "text", buffer, "0", el->elem.spt.text, &ier );
    while ( strstr(el->elem.spt.text,"$$") != (char *)NULL )
        cst_rpst ( el->elem.spt.text, "$$", "\n", el->elem.spt.text, &ier );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.spt.info.lat);
    el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.spt.info.lat);
    el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.spt.info.lon);
    el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.spt.info.lon);

    el->hdr.recsz = (int)( sizeof(VG_HdrStruct)
        + sizeof(SpTextInfo) + strlen(el->elem.spt.text)+1 );

}

/*=====================================================================*/

void	tag2cir ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  CIRCLE_ELM
     */
    cst_gtag ( "numpts", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.cir.info.numpts), &ier );

    cst_gtag ( "lintyp", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.cir.info.lintyp), &ier );

    cst_gtag ( "lthw", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.cir.info.lthw), &ier );

    cst_gtag ( "width", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.cir.info.width), &ier );

    cst_gtag ( "lwhw", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.cir.info.lwhw), &ier );

    npts = 2;

    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.cir.data.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.cir.data.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.cir.data.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.cir.data.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.cir.data.latlon[ii+npts]);
    }

    el->hdr.recsz = ( sizeof(VG_HdrStruct)
        + sizeof(LineInfo) + sizeof(CircData) );

}

/*=====================================================================*/

void	tag2trk ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	*cptr, delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  TRKSTORM_ELM
     */
    cst_gtag ( "subtype", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.subtype), &ier );

    cst_gtag ( "npts", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.npts), &ier );

    cst_gtag ( "nipts", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.nipts), &ier );

    cst_gtag ( "ltype1", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.ltype1), &ier );

    cst_gtag ( "ltype2", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.ltype2), &ier );

    cst_gtag ( "mtype1", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.mtype1), &ier );

    cst_gtag ( "mtype2", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.mtype2), &ier );

    cst_gtag ( "width", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.width), &ier );

    cst_gtag ( "speed", buffer, "1", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.trk.info.speed), &ier );

    cst_gtag ( "dir", buffer, "1", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.trk.info.dir), &ier );

    cst_gtag ( "incr", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.incr), &ier );

    cst_gtag ( "skip", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.skip), &ier );

    cst_gtag ( "itxfn", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.itxfn), &ier );

    cst_gtag ( "ithw", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.trk.info.ithw), &ier );

    cst_gtag ( "sztext", buffer, "1", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.trk.info.sztext), &ier );

    cst_gtag ( "times", buffer, "?", _Data, &ier );

    npts = el->elem.trk.info.npts;

    ii = 0;
    cptr = cst_split ( _Data, delim, sizeof(el->elem.trk.info.times[ii]), 
			el->elem.trk.info.times[ii], &ier );
    while ( ii < npts && cptr != (char *)NULL && ier == 0 )  {
        ii ++;
	cptr = cst_split ( cptr, delim, sizeof(el->elem.trk.info.times[ii]), 
			el->elem.trk.info.times[ii], &ier );
    }

    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.trk.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.trk.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.trk.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.trk.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.trk.latlon[ii+npts]);
    }

    el->hdr.recsz = ( sizeof(VG_HdrStruct) + sizeof(TrackType) );

}

/*=====================================================================*/

void	tag2sig ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  SIGINTL_ELM, SIGNCON_ELM, SIGCONV_ELM, SIGOUTL_ELM, SIGAIRM_ELM
     */
    cst_gtag ( "subtype", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.subtype), &ier );

    cst_gtag ( "npts", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.npts), &ier );

    cst_gtag ( "lintyp", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.lintyp), &ier );

    cst_gtag ( "linwid", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.linwid), &ier );

    cst_gtag ( "sol", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.sol), &ier );

    cst_gtag ( "area", buffer, "1", el->elem.sig.info.area, &ier );

    cst_gtag ( "fir", buffer, "1", el->elem.sig.info.fir, &ier );

    cst_gtag ( "status", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.status), &ier );

    cst_gtag ( "distance", buffer, "1", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.sig.info.distance), &ier );

    cst_gtag ( "msgid", buffer, "1", el->elem.sig.info.msgid, &ier );

    cst_gtag ( "seqnum", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.seqnum), &ier );

    cst_gtag ( "stime", buffer, "1", el->elem.sig.info.stime, &ier );

    cst_gtag ( "etime", buffer, "1", el->elem.sig.info.etime, &ier );

    cst_gtag ( "remarks", buffer, "1", el->elem.sig.info.remarks, &ier );

    cst_gtag ( "sonic", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.sonic), &ier );

    cst_gtag ( "phenom", buffer, "?", el->elem.sig.info.phenom, &ier );

    cst_gtag ( "phenom2", buffer, "?", el->elem.sig.info.phenom2, &ier );

    cst_gtag ( "phennam", buffer, "?", el->elem.sig.info.phennam, &ier );

    cst_gtag ( "phenlat", buffer, "?", el->elem.sig.info.phenlat, &ier );

    cst_gtag ( "phenlon", buffer, "?", el->elem.sig.info.phenlon, &ier );

    cst_gtag ( "pres", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.pres), &ier );

    cst_gtag ( "maxwind", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.maxwind), &ier );

    cst_gtag ( "freetext", buffer, "?", el->elem.sig.info.freetext, &ier );

    cst_gtag ( "trend", buffer, "?", el->elem.sig.info.trend, &ier );

    cst_gtag ( "move", buffer, "?", el->elem.sig.info.move, &ier );

    cst_gtag ( "obsfcst", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.obsfcst), &ier );

    cst_gtag ( "obstime", buffer, "?", el->elem.sig.info.obstime, &ier );

    cst_gtag ( "fl", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.fl), &ier );

    cst_gtag ( "spd", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.sig.info.spd), &ier );

    cst_gtag ( "dir", buffer, "?", el->elem.sig.info.dir, &ier );

    cst_gtag ( "tops", buffer, "?", el->elem.sig.info.tops, &ier );

    cst_gtag ( "fcstr", buffer, "?", el->elem.sig.info.fcstr, &ier );

    npts = el->elem.sig.info.npts;

    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.sig.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.sig.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.sig.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.sig.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.sig.latlon[ii+npts]);
    }

    el->hdr.recsz = (int)( (sizeof(float) * (size_t)(2 * npts)) + sizeof(VG_HdrStruct) 
		+ sizeof(SigmetInfo) );

}

/*=====================================================================*/

void	tag2ccf ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  SIGCCF_ELM
     */
    cst_gtag ( "subtype", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ccf.info.subtype), &ier );

    cst_gtag ( "npts", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ccf.info.npts), &ier );

    cst_gtag ( "cover", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ccf.info.cover), &ier );

    cst_gtag ( "tops", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ccf.info.tops), &ier );

    cst_gtag ( "prob", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ccf.info.prob), &ier );

    cst_gtag ( "growth", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ccf.info.growth), &ier );

    cst_gtag ( "spd", buffer, "1", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.ccf.info.spd), &ier );

    cst_gtag ( "dir", buffer, "1", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.ccf.info.dir), &ier );

    /*Added code to get spt text */
    
    cst_gtag ( "textlat", buffer, "0", _Data, &ier );
    cst_crnm  ( _Data, &(el->elem.ccf.info.textlat), &ier );
    cst_gtag ( "textlon", buffer, "0", _Data, &ier );
    cst_crnm  ( _Data, &(el->elem.ccf.info.textlon), &ier );

    cst_gtag ( "arrowlat", buffer, "0", _Data, &ier );
    cst_crnm  ( _Data, &(el->elem.ccf.info.arrowlat), &ier );
    cst_gtag ( "arrowlon", buffer, "0", _Data, &ier );
    cst_crnm  ( _Data, &(el->elem.ccf.info.arrowlon), &ier );

    cst_crnm  ( "fillhi", &(el->elem.ccf.info.fillhi), &ier );
    cst_crnm  ( "fillmed", &(el->elem.ccf.info.fillmed), &ier );
    cst_crnm  ( "filllow", &(el->elem.ccf.info.filllow), &ier );

    /*cst_gtag ( "sptxtyp", buffer, "0", _Data, &ier ); /* =0 */
    cst_numb ( "4", &(el->elem.ccf.spt.info.sptxtyp), &ier );

    cst_gtag ( "lat", buffer, "0", _Data, &ier );
    cst_crnm  ( _Data, &(el->elem.ccf.spt.info.lat), &ier );
    cst_gtag ( "lon", buffer, "0", _Data, &ier );
    cst_crnm  ( _Data, &(el->elem.ccf.spt.info.lon), &ier );
    cst_gtag ( "offset_x", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ccf.spt.info.offset_x), &ier );
    cst_gtag ( "offset_y", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ccf.spt.info.offset_y), &ier );
    cst_gtag ( "text", buffer, "0",  el->elem.ccf.spt.text, &ier );
    cst_gtag ( "textLayout", buffer, "0",  el->elem.ccf.textLayout, &ier );

    npts = el->elem.ccf.info.npts;

    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.ccf.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.ccf.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.ccf.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.ccf.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.ccf.latlon[ii+npts]);
    }

    el->hdr.recsz = ( sizeof(VG_HdrStruct) + sizeof(CCFType) );

}

/*======================================================================*/

void	tag2lst ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	*cptr, delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  LIST_ELM
     */
    cst_gtag ( "subtyp", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.lst.info.subtyp), &ier );

    cst_gtag ( "mrktyp", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.lst.info.mrktyp), &ier );

    cst_gtag ( "mrksiz", buffer, "1", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.lst.info.mrksiz), &ier );

    cst_gtag ( "mrkwid", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.lst.info.mrkwid), &ier );

    cst_gtag ( "nitems", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.lst.data.nitems), &ier );

    npts = el->elem.lst.data.nitems;

    cst_gtag ( "item", buffer, "0", _Data, &ier );
    ii = 0;
    cptr = cst_split ( _Data, delim, sizeof(el->elem.lst.data.item[ii]), 
			el->elem.lst.data.item[ii], &ier );
    while ( ii < npts && cptr != (char *)NULL && ier == 0 )  {
	cptr = cst_split ( cptr, delim, sizeof(el->elem.lst.data.item[ii]), 
			el->elem.lst.data.item[ii], &ier );
	ii++;
    }

    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "lat", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.lst.data.lat, fvals, (size_t)nvals*sizeof(float) );
    cst_gtag ( "lon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.lst.data.lon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.lst.data.lat[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.lst.data.lat[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.lst.data.lon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.lst.data.lon[ii+npts]);
    }

    el->hdr.recsz = ( sizeof(VG_HdrStruct) + 
		sizeof(ListInfo) + sizeof(ListData) );

}

/*======================================================================*/

void	tag2ash ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  ASHCLD_ELM
     */
    cst_gtag ( "subtype", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ash.info.subtype), &ier );

    cst_gtag ( "npts", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ash.info.npts), &ier );

    cst_gtag ( "distance", buffer, "10", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.ash.info.distance), &ier );

    cst_gtag ( "fhr", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ash.info.fhr), &ier );

    cst_gtag ( "lintyp", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ash.info.lintyp), &ier );

    cst_gtag ( "spd", buffer, "0", el->elem.ash.info.spds, &ier );

    cst_gtag ( "dir", buffer, "?", el->elem.ash.info.dir, &ier );

    cst_gtag ( "flvl1", buffer, "?",  el->elem.ash.info.flvl1, &ier );

    cst_gtag ( "flvl2", buffer, "?",  el->elem.ash.info.flvl2, &ier );

    /*Added code to get spt text */
    /*cst_gtag ( "sptxtyp", buffer, "0", _Data, &ier );  =0 */
    cst_numb ( "4", &(el->elem.ash.spt.info.sptxtyp), &ier );

    cst_gtag ( "lat", buffer, "0", _Data, &ier );
    cst_crnm  ( _Data, &(el->elem.ash.spt.info.lat), &ier );
    cst_gtag ( "lon", buffer, "0", _Data, &ier );
    cst_crnm  ( _Data, &(el->elem.ash.spt.info.lon), &ier );

    cst_gtag ( "offset_x", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ash.spt.info.offset_x), &ier );

    cst_gtag ( "offset_y", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.ash.spt.info.offset_y), &ier );

    cst_gtag ( "text", buffer, "0",  el->elem.ash.spt.text, &ier );
    

    npts = el->elem.ash.info.npts;

    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.ash.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );


    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.ash.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.ash.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.ash.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.ash.latlon[ii+npts]);
    }

    el->hdr.recsz = ( sizeof(VG_HdrStruct) + sizeof(AshType) );

}

/*======================================================================*/

void	tag2vol ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, *ivals, ier;
float	*fvals;
char	delim=',';
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  VOLC_ELM
     */
    cst_gtag ( "name", buffer, "?", el->elem.vol.info.name, &ier );

    cst_gtag ( "code", buffer, "0", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.vol.info.code), &ier );

    cst_gtag ( "size", buffer, "1", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.vol.info.size), &ier );

    cst_gtag ( "width", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.vol.info.width), &ier );

    cst_gtag ( "number", buffer, "?", el->elem.vol.info.number, &ier );

    cst_gtag ( "location", buffer, "?", el->elem.vol.info.location, &ier );

    cst_gtag ( "area", buffer, "?", el->elem.vol.info.area, &ier );

    cst_gtag ( "elev", buffer, "?", el->elem.vol.info.elev, &ier );

    cst_gtag ( "origstn", buffer, "?", el->elem.vol.info.origstn, &ier );

    cst_gtag ( "vaac", buffer, "?", el->elem.vol.info.vaac, &ier );

    cst_gtag ( "wmoid", buffer, "?", el->elem.vol.info.wmoid, &ier );

    cst_gtag ( "hdrnum", buffer, "?", el->elem.vol.info.hdrnum, &ier );

    cst_gtag ( "year", buffer, "?", el->elem.vol.info.year, &ier );

    cst_gtag ( "advnum", buffer, "?", el->elem.vol.info.advnum, &ier );

    cst_gtag ( "infosorc", buffer, "?", el->elem.vol.info.infosorc, &ier );

    cst_gtag ( "addlsorc", buffer, "?", el->elem.vol.info.addlsorc, &ier );

    cst_gtag ( "details", buffer, "?", el->elem.vol.info.details, &ier );

    cst_gtag ( "obsdate", buffer, "?", el->elem.vol.info.obsdate, &ier );

    cst_gtag ( "obstime", buffer, "?", el->elem.vol.info.obstime, &ier );

    cst_gtag ( "obsashcld", buffer, "?", el->elem.vol.info.obsashcld, &ier );

    cst_gtag ( "fcst_06", buffer, "?", el->elem.vol.info.fcst_06, &ier );

    cst_gtag ( "fcst_12", buffer, "?", el->elem.vol.info.fcst_12, &ier );

    cst_gtag ( "fcst_18", buffer, "?", el->elem.vol.info.fcst_18, &ier );

    cst_gtag ( "remarks", buffer, "?", el->elem.vol.info.remarks, &ier );

    cst_gtag ( "nextadv", buffer, "?", el->elem.vol.info.nextadv, &ier );

    cst_gtag ( "fcstrs", buffer, "?", el->elem.vol.info.fcstrs, &ier );

    npts = 1;

    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.vol.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );
    ivals = (int *)malloc((size_t)(npts*2)*sizeof(int));
    cst_gtag ( "offset_xy", buffer, "0", _Data, &ier );
    cst_ilst ( _Data, delim, IMISSD, npts*2, ivals, &nvals, &ier );
    memmove ( el->elem.vol.offset_xy, ivals, (size_t)nvals*sizeof(float) );
    free ( ivals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.vol.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.vol.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.vol.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.vol.latlon[ii+npts]);
    }

    el->hdr.recsz = ( sizeof(VG_HdrStruct) + sizeof(VolType) );

}

/*======================================================================*/

void	tag2jet ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	delim=',', tstr[2048], wnd_tag[16], txt_tag[16];
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  JET_ELM -> line
     */
    cst_gtag ( "splcol", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.jet.line.splcol), &ier );

    cst_gtag ( "numpts", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.jet.line.spl.info.numpts), &ier );

    cst_gtag ( "spltyp", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.jet.line.spl.info.spltyp), &ier );

    cst_gtag ( "splstr", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.jet.line.spl.info.splstr), &ier );

    cst_gtag ( "spldir", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.jet.line.spl.info.spldir), &ier );

    cst_gtag ( "splsiz", buffer, "1", _Data, &ier );
    cst_crnm ( _Data, &(el->elem.jet.line.spl.info.splsiz), &ier );

    cst_gtag ( "splwid", buffer, "1", _Data, &ier );
    cst_numb ( _Data, &(el->elem.jet.line.spl.info.splwid), &ier );

    npts = el->elem.jet.line.spl.info.numpts;

    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( "latlon", buffer, "0", _Data, &ier );

    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.jet.line.spl.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.jet.line.spl.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.jet.line.spl.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.jet.line.spl.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.jet.line.spl.latlon[ii+npts]);
    }


    /*  
     *  JET_ELM -> barbs
     */
    cst_gtag ( "nbarb", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.jet.nbarb), &ier );

    npts = el->elem.jet.nbarb;

    for ( ii = 0; ii < npts; ii++ ) {
	sprintf( wnd_tag, "jet_barb_%d", ii+1);
        cst_gtag ( wnd_tag, buffer, "\0", _Data, &ier );

	sscanf ( _Data, "%d,%d,%d,%f,%d,%f,%f,%f,%f,%f",
	    &el->elem.jet.barb[ii].wndcol,
	    &el->elem.jet.barb[ii].wnd.info.numwnd,
	    &el->elem.jet.barb[ii].wnd.info.width,
	    &el->elem.jet.barb[ii].wnd.info.size,
	    &el->elem.jet.barb[ii].wnd.info.wndtyp,
	    &el->elem.jet.barb[ii].wnd.info.hdsiz,
	    &el->elem.jet.barb[ii].wnd.data.spddir[0],
	    &el->elem.jet.barb[ii].wnd.data.spddir[1],
	    &el->elem.jet.barb[ii].wnd.data.latlon[0],
	    &el->elem.jet.barb[ii].wnd.data.latlon[1]
	); 	
    }

    /*  
     *  JET_ELM -> special texts
     */
    for ( ii = 0; ii < npts; ii++ ) {
	sprintf( txt_tag, "jet_text_%d", ii+1);
        cst_gtag ( txt_tag, buffer, "\0", _Data, &ier );
        
	sscanf ( _Data, "%d,%f,%f,%d,%d,%d,%d,%d,%d,%d,%d,%d,%f,%f,%d,%d,%s",
            &el->elem.jet.barb[ii].sptcol,
	    &el->elem.jet.barb[ii].spt.info.rotn,
	    &el->elem.jet.barb[ii].spt.info.sztext,
	    &el->elem.jet.barb[ii].spt.info.sptxtyp,
	    &el->elem.jet.barb[ii].spt.info.turbsym,
	    &el->elem.jet.barb[ii].spt.info.itxfn,
	    &el->elem.jet.barb[ii].spt.info.ithw,
	    &el->elem.jet.barb[ii].spt.info.iwidth,
	    &el->elem.jet.barb[ii].spt.info.txtcol,
	    &el->elem.jet.barb[ii].spt.info.lincol,
	    &el->elem.jet.barb[ii].spt.info.filcol,
	    &el->elem.jet.barb[ii].spt.info.ialign,
	    &el->elem.jet.barb[ii].spt.info.lat,
	    &el->elem.jet.barb[ii].spt.info.lon,
	    &el->elem.jet.barb[ii].spt.info.offset_x,
	    &el->elem.jet.barb[ii].spt.info.offset_y,
	    tstr    
        );
        
	while ( strstr(tstr,"$$") != (char *)NULL )
            cst_rpst ( tstr, "$$", "\n", tstr, &ier );
	    
	strcpy ( el->elem.jet.barb[ii].spt.text, tstr );    
    }
    
    for ( ii = 0; ii < npts; ii++ ) {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.jet.barb[ii].spt.info.lat);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.jet.barb[ii].spt.info.lat);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.jet.barb[ii].spt.info.lon);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.jet.barb[ii].spt.info.lon);
    }
    
    /*  
     *  JET_ELM -> hashs
     */
    cst_gtag ( "nhash", buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.jet.nhash), &ier );

    npts = el->elem.jet.nhash;

    for ( ii = 0; ii < npts; ii++ ) {
	sprintf( wnd_tag, "jet_hash_%d", ii+1);
        cst_gtag ( wnd_tag, buffer, "\0", _Data, &ier );
        
	sscanf ( _Data, "%d,%d,%d,%f,%d,%f,%f,%f,%f,%f",
	    &el->elem.jet.hash[ii].wndcol,
	    &el->elem.jet.hash[ii].wnd.info.numwnd,
	    &el->elem.jet.hash[ii].wnd.info.width,
	    &el->elem.jet.hash[ii].wnd.info.size,
	    &el->elem.jet.hash[ii].wnd.info.wndtyp,
	    &el->elem.jet.hash[ii].wnd.info.hdsiz,
	    &el->elem.jet.hash[ii].wnd.data.spddir[0],
	    &el->elem.jet.hash[ii].wnd.data.spddir[1],
	    &el->elem.jet.hash[ii].wnd.data.latlon[0],
	    &el->elem.jet.hash[ii].wnd.data.latlon[1]
	); 	
    }
    
    
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.jet.hash[ii].wnd.data.latlon[0]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.jet.hash[ii].wnd.data.latlon[0]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.jet.hash[ii].wnd.data.latlon[1]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.jet.hash[ii].wnd.data.latlon[1]);
    }

    el->hdr.recsz = (int)( sizeof(VG_HdrStruct) + sizeof(JetType) );

}

/*======================================================================*/

void	tag2gfa ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier, nblks;
float	*fvals;
char	delim=',', *ptr1, *ptr2;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  GFA_ELM
     */
    cst_gtag ( TAG_GFA_NBLOCKS, buffer, "0", _Data, &ier );
    cst_numb ( _Data, &nblks, &ier );
        
    el->elem.gfa.info.nblocks = 0;
    while ( el->elem.gfa.info.nblocks < nblks ) {
        cvg_allocGfaBlock ( el );    
    }
        
    /*
     *  Find tag <npts>
     */
    if ( !( ptr1 = strstr ( buffer, TAG_GFA_NPTS ) ) ) return;

    /*
     *  Find next tag
     */
    if ( !( ptr1 = strchr ( ptr1, '<' ) ) ) return;

    /* 
     *  Find tag <latlon>
     */
    if ( !( ptr2 = strstr ( ptr1, TAG_GFA_POINTS ) ) ) return;

    strncpy ( *el->elem.gfa.info.blockPtr[0], ptr1, ptr2 - ptr1 );
    el->elem.gfa.info.blockPtr[ ptr2 - ptr1 ] = '\0';

    cst_gtag ( TAG_GFA_NPTS, buffer, "0", _Data, &ier );
    cst_numb ( _Data, &npts, &ier );
    el->elem.gfa.info.npts = npts;
    fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
    cst_gtag ( TAG_GFA_POINTS, buffer, "0", _Data, &ier );
    cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
    memmove ( el->elem.gfa.latlon, fvals, (size_t)nvals*sizeof(float) );
    free ( fvals );

    el->hdr.range_min_lat =  FLT_MAX;
    el->hdr.range_max_lat = -FLT_MAX;
    el->hdr.range_min_lon =  FLT_MAX;
    el->hdr.range_max_lon = -FLT_MAX;
    for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.gfa.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.gfa.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.gfa.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.gfa.latlon[ii+npts]);
    }

    el->hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof (int) * 2 + 
                            sizeof(char) * STD_STRLEN * nblks  +
		            sizeof(float) * npts * 2 );

}

/*======================================================================*/

void	tag2sgwx ( char *buffer, VG_DBStruct *el, int *iret )
{
int	ii, npts, nvals, ier;
float	*fvals;
char	delim=',';
  *iret = 0;
  cst_gtag ( "subtype", buffer, "0", _Data, &ier );
  cst_numb ( _Data, &(el->elem.sgwx.info.subtype), &ier );

  cst_gtag ( "npts", buffer, "1", _Data, &ier );
  cst_numb ( _Data, &(el->elem.sgwx.info.npts), &ier );
  npts = el->elem.sgwx.info.npts;
  fvals = (float *)malloc((size_t)(npts*2)*sizeof(float));
  cst_gtag ( "latlon", buffer, "0", _Data, &ier );
  cst_rlst ( _Data, delim, RMISSD, npts*2, fvals, &nvals, &ier );
  memmove ( el->elem.sgwx.latlon, fvals, (size_t)nvals*sizeof(float) );
  free ( fvals );
  el->hdr.range_min_lat =  FLT_MAX;
  el->hdr.range_max_lat = -FLT_MAX;
  el->hdr.range_min_lon =  FLT_MAX;
  el->hdr.range_max_lon = -FLT_MAX;
  for ( ii = 0; ii < npts; ii++ )  {
        el->hdr.range_min_lat = 
            G_MIN(el->hdr.range_min_lat, el->elem.sgwx.latlon[ii]);
        el->hdr.range_max_lat = 
            G_MAX(el->hdr.range_max_lat, el->elem.sgwx.latlon[ii]);
        el->hdr.range_min_lon = 
            G_MIN(el->hdr.range_min_lon, el->elem.sgwx.latlon[ii+npts]);
        el->hdr.range_max_lon = 
            G_MAX(el->hdr.range_max_lon, el->elem.sgwx.latlon[ii+npts]);
  }
  el->hdr.recsz = ( sizeof(VG_HdrStruct) + sizeof(SGWXType) );
}

/*======================================================================*/

void	tag2tca ( char *buffer, VG_DBStruct *el, int *iret )
{
double	dtemp;
int	counter, ii, jj, ier, itemp;
char	tag_str[ STD_STRLEN ], *ptca, cmiss[20];
float       lat[9]={30., 30., 30., 25., 25., 25., 20., 20., 20. };
float       lon[9]={-85., -80., -75., -85., -80., -75., -85., -80., -75. };

/*---------------------------------------------------------------------*/

    *iret = 0;
    ier = 0;

    /*  
     *  TCA_ELM
     */
    cst_gtag ( TCA_STORMNUM_TAG, buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.tca.info.stormNum), &ier );

    cst_gtag ( TCA_ISSUESTATUS_TAG, buffer, "?", _Data, &ier );
    el->elem.tca.info.issueStatus = _Data[ 0 ];

    cst_gtag ( TCA_BASIN_TAG, buffer, "0", _Data, &ier );
    itemp = atoi( _Data );
    el->elem.tca.info.basin = (enum basin_t)itemp;

    cst_gtag ( TCA_ADVISORYNUM_TAG, buffer, "0", el->elem.tca.info.advisoryNum, &ier );

    cst_gtag ( TCA_STORMNAME_TAG, buffer, "?", el->elem.tca.info.stormName, &ier );

    cst_gtag ( TCA_STORMTYPE_TAG, buffer, "0", _Data, &ier );
    itemp = atoi( _Data );
    el->elem.tca.info.stormType = (enum storm_type_t)itemp;

    cst_gtag ( TCA_VALIDTIME_TAG, buffer, "?", el->elem.tca.info.validTime, &ier );

    cst_gtag ( TCA_TIMEZONE_TAG, buffer, "?", el->elem.tca.info.timezone, &ier );

    sprintf( cmiss, "%f", RMISSD);
    cst_gtag ( TCA_TEXTLAT_TAG, buffer, cmiss, _Data, &ier);
    dtemp = atof( _Data );
    el->elem.tca.info.text_lat = (float)dtemp;
    if ( ERMISS ( el->elem.tca.info.text_lat ) )
          el->elem.tca.info.text_lat = lat [ el->elem.tca.info.stormNum % 9 ];

    cst_gtag ( TCA_TEXTLON_TAG, buffer, cmiss, _Data, &ier);
    dtemp = atof( _Data );
    el->elem.tca.info.text_lon = (float)dtemp ;
    if ( ERMISS ( el->elem.tca.info.text_lon ) )
          el->elem.tca.info.text_lon = lon [ el->elem.tca.info.stormNum % 9 ];
    
    cst_gtag ( TCA_TEXTFONT_TAG, buffer, "1", _Data, &ier);
    cst_numb ( _Data, &(el->elem.tca.info.text_font), &ier );
    
    cst_gtag ( TCA_TEXTSIZE_TAG, buffer, "1.0", _Data, &ier);
    dtemp = atof( _Data );
    el->elem.tca.info.text_size = (float)dtemp;
    
    cst_gtag ( TCA_TEXTWIDTH_TAG, buffer, "3", _Data, &ier);
    cst_numb ( _Data, &(el->elem.tca.info.text_width), &ier );

    cst_gtag ( TCA_WWNUM_TAG, buffer, "0", _Data, &ier );
    cst_numb ( _Data, &(el->elem.tca.info.wwNum), &ier );
    
    for ( counter = 0; counter < el->elem.tca.info.wwNum; counter++ ) {
        sprintf ( tag_str, "%s_%d", TCA_TCAWWSTR_TAG, counter );
        cst_gtag ( tag_str, buffer, "0|0|0", _Data, &ier );

        itemp = atoi( strtok( _Data, "|" ));
	el->elem.tca.info.tcaww[ counter ].severity = (enum tca_sev_t)itemp;
	itemp = atoi( strtok( (char*)0, "|" ) );
        el->elem.tca.info.tcaww[ counter ].advisoryType = (enum tca_adv_t)itemp;
	itemp = atoi( strtok( (char*)0, "|" ) );
        el->elem.tca.info.tcaww[ counter ].specialGeog = (enum tca_sp_geog_t)itemp;

        sprintf ( tag_str, "%s_%d", TCA_NUMBKPTS_TAG, counter );
        cst_gtag ( tag_str, buffer, "0", _Data, &ier );
        el->elem.tca.info.tcaww[ counter ].numBreakPts = atoi( _Data );

	if ( !(el->elem.tca.info.tcaww[ counter ].breakPnt = malloc ( sizeof ( Breakpt_T ) *
			el->elem.tca.info.tcaww[ counter ].numBreakPts ))) return;

        sprintf ( tag_str, "%s_%d", TCA_BREAKPTS_TAG, counter );
        cst_gtag ( tag_str, buffer, "0|0|?|0|0|?", _Data, &ier );

        el->elem.tca.info.tcaww[ counter ].breakPnt[ 0 ].lat = atof( strtok( _Data, "|" ) );
        el->elem.tca.info.tcaww[ counter ].breakPnt[ 0 ].lon = atof( strtok( NULL, "|" ) );
        strcpy( el->elem.tca.info.tcaww[ counter ].breakPnt[ 0 ].breakPtName, strtok( NULL, "|" ) );

 	for ( ii = 1; ii < el->elem.tca.info.tcaww[ counter ].numBreakPts; ii++ ) {
            el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat = atof( strtok( NULL, "|" ) );
            el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon = atof( strtok( NULL, "|" ) );
            strcpy( el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].breakPtName, 
			strtok( NULL, "|" ) );
	}
    }

    if ( el->elem.tca.info.wwNum == 0 ) {
        el->hdr.range_min_lat =  el->elem.tca.info.text_lat;
        el->hdr.range_max_lat =  el->elem.tca.info.text_lat;
        el->hdr.range_min_lon =  el->elem.tca.info.text_lon;
        el->hdr.range_max_lon =  el->elem.tca.info.text_lon;
    }
    else {
        el->hdr.range_min_lat =  FLT_MAX;
        el->hdr.range_max_lat = -FLT_MAX;
        el->hdr.range_min_lon =  FLT_MAX;
        el->hdr.range_max_lon = -FLT_MAX;
        for ( counter = 0; counter < el->elem.tca.info.wwNum; counter++ )  {
            for ( jj = 0; jj < el->elem.tca.info.tcaww[counter].numBreakPts; jj++ )  {
               el->hdr.range_min_lat = 
               G_MIN(el->hdr.range_min_lat, el->elem.tca.info.tcaww[ counter ].breakPnt[ jj ].lat);
               el->hdr.range_max_lat = 
               G_MAX(el->hdr.range_max_lat, el->elem.tca.info.tcaww[ counter ].breakPnt[ jj ].lat);
               el->hdr.range_min_lon = 
               G_MIN(el->hdr.range_min_lon, el->elem.tca.info.tcaww[ counter ].breakPnt[ jj ].lon);
               el->hdr.range_max_lon = 
               G_MAX(el->hdr.range_max_lon, el->elem.tca.info.tcaww[ counter ].breakPnt[ jj ].lon);
            }
        }
    }

    el->hdr.recsz = (int)( sizeof(VG_HdrStruct) + 
			cvg_loadTcaFileInfo(el->elem.tca.info, &ptca,  &ier) );
    free( ptca );
}
/*======================================================================*/

