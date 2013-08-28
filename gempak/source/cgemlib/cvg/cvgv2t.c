#include "cvgcmn.h"

/* Local prototype */
void	fhed2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	hdr2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	frt2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	lin2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	spl2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	wbx2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	wsm2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	sym2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	wnd2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	txt2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	spt2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	cir2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	trk2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	sig2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	ccf2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	lst2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	ash2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	vol2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	jet2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	gfa2tag ( VG_DBStruct *el, char *buffer, int *iret );
void    sgwx2tag ( VG_DBStruct *el, char *buffer, int *iret );
void	tca2tag ( VG_DBStruct *el, char *buffer, int *iret );


int	cvg_v2t ( VG_DBStruct *el, char *buffer, int *iret )
/************************************************************************
 * cvg_v2t                                                          	*
 *                                                                      *
 * This function converts a VGF element into an ASCII tag string.	*
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
 * D.W.Plummer/NCEP	07/03	Added ash2tag and vol2tag		*
 * J. Wu/SAIC		09/03	Added jet2tag				*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * H. Zeng/XTRIA	11/03   modified a field for ASHCLD		*
 * J. Wu/SAIC		01/04	Added gfa2tag				*
 * J. Wu/SAIC		02/04	added lat/lon text position for GFA_ELM	*
 * B. Yin/SAIC		02/04	Added tca2tag				*
 * B. Yin/SAIC		03/04	Added storm type in tca structure	*
 * J. Wu/SAIC		05/04	add weather type description in gfa2tag	*
 * J. Wu/SAIC		10/04	access GFA attributes with cvg_getFld()	*
 * B. Yin/SAIC		11/04	removed unnecessary gfa tags		*
 * B. Yin/SAIC		12/04	added timezone in TCAs			*
 * B. Yin/SAIC		04/05	added issueStatus in TCAs, removed year	*
 * S. Jacobs/NCEP	 5/05	Added checks for 0 points in lines	*
 * S. Jacobs/NCEP	 5/05	Fixed check for multiple lines of text	*
 * S. Jacobs/NCEP	 4/10	Change < and > to &lt and &gt		*
 * L. Hinson/AWC         1/12   Added sgwx2tag                          *
 * L. Hinson/AWC         5/13   Fix jet2tag <line_latlon> to <latlon>   *
 ***********************************************************************/
{
int	ier;
/*---------------------------------------------------------------------*/

    *iret = 0;

    switch ( el->hdr.vg_type )  {

	case	FILEHEAD_ELM:
		fhed2tag ( el, buffer, &ier ); 
		break;

	case	LINE_ELM:
		lin2tag ( el, buffer, &ier );
		break;

	case	FRONT_ELM:
		frt2tag ( el, buffer, &ier ); 
		break;

	case	SPLN_ELM:
		spl2tag ( el, buffer, &ier ); 
		break;

	case	WBOX_ELM:
		wbx2tag ( el, buffer, &ier ); 
		break;

	case	WSM_ELM:
		wsm2tag ( el, buffer, &ier ); 
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
		sym2tag ( el, buffer, &ier ); 
		break;

	case	BARB_ELM:
	case	ARROW_ELM:
	case	DARR_ELM:
	case	HASH_ELM:
		wnd2tag ( el, buffer, &ier ); 
		break;

	case	TEXT_ELM:
	case	TEXTC_ELM:
		txt2tag ( el, buffer, &ier ); 
		break;

	case	SPTX_ELM:
		spt2tag ( el, buffer, &ier ); 
		break;

	case	CIRCLE_ELM:
		cir2tag ( el, buffer, &ier ); 
		break;

	case	TRKSTORM_ELM:
		trk2tag ( el, buffer, &ier ); 
		break;

	case	SIGINTL_ELM:
	case	SIGNCON_ELM:
	case	SIGCONV_ELM:
	case	SIGOUTL_ELM:
	case	SIGAIRM_ELM:
		sig2tag ( el, buffer, &ier ); 
		break;

	case	SIGCCF_ELM:
		ccf2tag ( el, buffer, &ier ); 
		break;
        
	case	LIST_ELM:
		lst2tag ( el, buffer, &ier ); 
		break;

	case	ASHCLD_ELM:
		ash2tag ( el, buffer, &ier ); 
		break;

	case	VOLC_ELM:
		vol2tag ( el, buffer, &ier ); 
		break;

	case	JET_ELM:
		jet2tag ( el, buffer, &ier ); 
		break;

	case	GFA_ELM:
		gfa2tag ( el, buffer, &ier ); 
		break;

	case    SGWX_ELM:
                sgwx2tag ( el, buffer, &ier );
                break;

	case	TCA_ELM:
		tca2tag ( el, buffer, &ier ); 
		break;

	default:
		*iret = -1;
	break;

    }

    return ( 0 );

}


void	hdr2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  ELEMENT HEADER
     */

    sprintf( buffer, 

	"<vg_type>%d"
	"<vg_class>%d"
	"<delete>%d"
	"<filled>%d"
	"<closed>%d"
	"<smooth>%d"
	"<version>%d"
	"<grptyp>%d"
	"<grpnum>%d"
	"<maj_col>%d"
	"<min_col>%d"
	"<recsz>%d"
	"<range_min_lat>%6.2f"
	"<range_min_lon>%7.2f"
	"<range_max_lat>%6.2f"
	"<range_max_lon>%7.2f",

	(int)el->hdr.vg_type,
	(int)el->hdr.vg_class,
	(int)el->hdr.delete,
	(int)el->hdr.filled,
	(int)el->hdr.closed,
	(int)el->hdr.smooth,
	(int)el->hdr.version,
	(int)el->hdr.grptyp,
	el->hdr.grpnum,
	el->hdr.maj_col,
	el->hdr.min_col,
	el->hdr.recsz,
	el->hdr.range_min_lat,
	el->hdr.range_min_lon,
	el->hdr.range_max_lat,
	el->hdr.range_max_lon

	);

}

void	fhed2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  FILEHEAD_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<version>%s"
	"<notes>%s",

	el->elem.fhed.version,
	el->elem.fhed.notes 

	);

}

/*=====================================================================*/

void	frt2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  FRONT_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<numpts>%d"
	"<fcode>%d"
	"<fpipsz>%d"
	"<fpipst>%d"
	"<fpipdr>%d"
	"<fwidth>%d"
	"<frtlbl>%s",

	el->elem.frt.info.numpts,
	el->elem.frt.info.fcode,
	el->elem.frt.info.fpipsz,
	el->elem.frt.info.fpipst,
	el->elem.frt.info.fpipdr,
	el->elem.frt.info.fwidth,
	el->elem.frt.info.frtlbl 

	);

    npts = el->elem.frt.info.numpts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.frt.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.frt.latlon[ii+npts] );
    }

    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

void	lin2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  LINE_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<numpts>%d"
	"<lintyp>%d"
	"<lthw>%d"
	"<width>%d"
	"<lwhw>%d",

	el->elem.lin.info.numpts,
	el->elem.lin.info.lintyp,
	el->elem.lin.info.lthw,
	el->elem.lin.info.width,
	el->elem.lin.info.lwhw 

	);

    npts = el->elem.lin.info.numpts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );

    if  ( npts > 0 )  {
	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "%6.2f,", el->elem.lin.latlon[ii] );
	}
	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "%7.2f,", el->elem.lin.latlon[ii+npts] );
	}

	/*  remove final comma 	*/
	blen = strlen(buffer);
	buffer[blen-1] = '\0';
    }

}

/*=====================================================================*/

void	spl2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  SPLN_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<numpts>%d"
	"<spltyp>%d"
	"<splstr>%d"
	"<spldir>%d"
	"<splsiz>%f"
	"<splwid>%d",

	el->elem.spl.info.numpts,
	el->elem.spl.info.spltyp,
	el->elem.spl.info.splstr,
	el->elem.spl.info.spldir,
	el->elem.spl.info.splsiz, 
	el->elem.spl.info.splwid 

	);

    npts = el->elem.spl.info.numpts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );

    if  ( npts > 0 )  {
	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "%6.2f,", el->elem.spl.latlon[ii] );
	}
	for ( ii = 0; ii < npts; ii++ )  {
	    blen = strlen(buffer);
	    sprintf( &(buffer[blen]), "%7.2f,", el->elem.spl.latlon[ii+npts] );
	}

	/*  remove final comma 	*/
	blen = strlen(buffer);
	buffer[blen-1] = '\0';
    }

}

/*=====================================================================*/

void	wbx2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  WBOX_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<numpts>%d"
	"<w_style>%d"
	"<w_shape>%d"
	"<w_mrktyp>%d"
	"<w_mrksiz>%f"
	"<w_mrkwid>%d"
	"<w_a0id>%s"
	"<w_a0lt>%6.2f"
	"<w_a0ln>%7.2f"
	"<w_a0dis>%d"
	"<w_a0dir>%s"
	"<w_a1id>%s"
	"<w_a1lt>%6.2f"
	"<w_a1ln>%7.2f"
	"<w_a1dis>%d"
	"<w_a1dir>%s"
	"<w_istat>%d"
	"<w_number>%d"
	"<w_iss_t>%s"
	"<w_exp_t>%s"
	"<w_type>%d"
	"<w_severiy>%d"
	"<w_timezone>%s"
	"<w_hailsz>%s"
	"<w_windg>%s"
	"<w_tops>%s"
	"<w_msmv_d>%s"
	"<w_msmv_s>%s"
	"<w_states>%s"
	"<w_adjarea>%s"
	"<w_replw>%s"
	"<w_fcstr>%s"
	"<w_file>%s"
	"<w_issued>%d"
	"<wsm_iss_t>%s"
	"<wsm_exp_t>%s"
	"<wsm_ref>%s"
	"<wsm_from>%s"
	"<wsm_meso>%s"
	"<wsm_fcstr>%s"
	"<numcnty>%d"
	"<cn_flag>%d",

	el->elem.wbx.info.numpts,
	el->elem.wbx.info.w_style,
	el->elem.wbx.info.w_shape,
	el->elem.wbx.info.w_mrktyp,
	el->elem.wbx.info.w_mrksiz, 
	el->elem.wbx.info.w_mrkwid, 
	el->elem.wbx.info.w_a0id, 
	el->elem.wbx.info.w_a0lt, 
	el->elem.wbx.info.w_a0ln, 
	el->elem.wbx.info.w_a0dis, 
	el->elem.wbx.info.w_a0dir, 
	el->elem.wbx.info.w_a1id, 
	el->elem.wbx.info.w_a1lt, 
	el->elem.wbx.info.w_a1ln, 
	el->elem.wbx.info.w_a1dis, 
	el->elem.wbx.info.w_a1dir, 
	el->elem.wbx.info.w_istat, 
	el->elem.wbx.info.w_number, 
	el->elem.wbx.info.w_iss_t, 
	el->elem.wbx.info.w_exp_t, 
	el->elem.wbx.info.w_type, 
	el->elem.wbx.info.w_severity, 
	el->elem.wbx.info.w_timezone, 
	el->elem.wbx.info.w_hailsz, 
	el->elem.wbx.info.w_windg, 
	el->elem.wbx.info.w_tops, 
	el->elem.wbx.info.w_msmv_d, 
	el->elem.wbx.info.w_msmv_s, 
	el->elem.wbx.info.w_states, 
	el->elem.wbx.info.w_adjarea, 
	el->elem.wbx.info.w_replw, 
	el->elem.wbx.info.w_fcstr, 
	el->elem.wbx.info.w_file, 
	el->elem.wbx.info.w_issued, 
	el->elem.wbx.info.wsm_iss_t, 
	el->elem.wbx.info.wsm_exp_t, 
	el->elem.wbx.info.wsm_ref, 
	el->elem.wbx.info.wsm_from, 
	el->elem.wbx.info.wsm_meso, 
	el->elem.wbx.info.wsm_fcstr, 
	el->elem.wbx.info.numcnty, 
	el->elem.wbx.info.cn_flag

	);

    npts = el->elem.wbx.info.numpts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.wbx.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.wbx.latlon[ii+npts] );
    }

    /*  remove final comma before continuing	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

    npts = el->elem.wbx.info.numcnty;
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<cn_fips>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%d,", el->elem.wbx.info.cn_fips[ii] );
    }

    /*  remove final comma before continuing	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<cn_ltln>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.wbx.info.cn_ltln[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.wbx.info.cn_ltln[ii+npts] );
    }

    /*  remove final comma before ending	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	wsm2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  WSM_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<numpts>%d"
	"<spltyp>%d"
	"<splstr>%d"
	"<spldir>%d"
	"<splsiz>%f"
	"<splwid>%d",

	el->elem.wsm.info.numpts,
	el->elem.wsm.info.spltyp,
	el->elem.wsm.info.splstr,
	el->elem.wsm.info.spldir,
	el->elem.wsm.info.splsiz,
	el->elem.wsm.info.splwid

	);

    npts = el->elem.wsm.info.numpts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.wsm.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.wsm.latlon[ii+npts] );
    }

    /*  remove final comma before ending	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	sym2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  WXSYM_ELM, CTSYM_ELM, ICSYM_ELM, PTSYM_ELM, PWSYM_ELM, 
     *  SKSYM_ELM, SPSYM_ELM, CMBSY_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<numsym>%d"
	"<width>%d"
	"<size>%f"
	"<ityp>%d"
	"<code>%f",

	el->elem.sym.info.numsym,
	el->elem.sym.info.width,
	el->elem.sym.info.size,
	el->elem.sym.info.ityp,
	el->elem.sym.data.code[0]

	);

    npts = el->elem.sym.info.numsym;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.sym.data.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.sym.data.latlon[ii+npts] );
    }

    /*  remove final comma before continuing	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<offset_xy>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%d,", el->elem.sym.data.offset_xy[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%d,", el->elem.sym.data.offset_xy[ii+npts] );
    }

    /*  remove final comma before ending	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	wnd2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  BARB_ELM, ARROW_ELM, DARR_ELM, HASH_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<numwnd>%d"
	"<width>%d"
	"<size>%f"
	"<wndtyp>%d"
	"<hdsiz>%f",

	el->elem.wnd.info.numwnd,
	el->elem.wnd.info.width,
	el->elem.wnd.info.size,
	el->elem.wnd.info.wndtyp,
	el->elem.wnd.info.hdsiz

	);

    npts = el->elem.wnd.info.numwnd;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<spddir>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.wnd.data.spddir[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.wnd.data.spddir[ii+npts] );
    }

    /*  remove final comma before continuing	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.wnd.data.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.wnd.data.latlon[ii+npts] );
    }

    /*  remove final comma before ending	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	txt2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ier;
char	*tstr, sstr[2];
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  TEXT_ELM, TEXTC_ELM
     */

    hdr2tag ( el, buffer, &ier );

    tstr = (char *)malloc( strlen(el->elem.txt.text)*3 * sizeof(char) );
    strcpy ( tstr, el->elem.txt.text );
    while ( strstr(tstr,"\n") != (char *)NULL )
        cst_rpst ( tstr, "\n", "$$", tstr, &ier );
    sstr[0] = CHLF;
    sstr[1] = CHNULL;
    while ( strstr(tstr,sstr) != (char *)NULL )
        cst_rpst ( tstr, sstr, "$$", tstr, &ier );
    sstr[0] = CHCR;
    sstr[1] = CHNULL;
    while ( strstr(tstr,sstr) != (char *)NULL )
        cst_rpst ( tstr, sstr, "$$", tstr, &ier );

    while ( strstr(tstr,"<") != (char *)NULL )
        cst_rpst ( tstr, "<", "&lt", tstr, &ier );
    while ( strstr(tstr,">") != (char *)NULL )
        cst_rpst ( tstr, ">", "&gt", tstr, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<rotn>%f"
	"<sztext>%f"
	"<itxfn>%d"
	"<ithw>%d"
	"<iwidth>%d"
	"<ialign>%d"
	"<lat>%f"
	"<lon>%f"
	"<offset_x>%d"
	"<offset_y>%d"
	"<text>%s",

	el->elem.txt.info.rotn,
	el->elem.txt.info.sztext,
	el->elem.txt.info.itxfn,
	el->elem.txt.info.ithw,
	el->elem.txt.info.iwidth,
	el->elem.txt.info.ialign,
	el->elem.txt.info.lat,
	el->elem.txt.info.lon,
	el->elem.txt.info.offset_x,
	el->elem.txt.info.offset_y,
	tstr

	);

    free ( tstr );

}

/*=====================================================================*/

void	spt2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ier;
char	*tstr, sstr[2];
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  SPTX_ELM
     */

    hdr2tag ( el, buffer, &ier );

    tstr = (char *)malloc( strlen(el->elem.spt.text)*3 * sizeof(char) );
    strcpy ( tstr, el->elem.spt.text );
    while ( strstr(tstr,"\n") != (char *)NULL ) 
        cst_rpst ( tstr, "\n", "$$", tstr, &ier );
    sstr[0] = CHLF;
    sstr[1] = CHNULL;
    while ( strstr(tstr,sstr) != (char *)NULL )
        cst_rpst ( tstr, sstr, "$$", tstr, &ier );
    sstr[0] = CHCR;
    sstr[1] = CHNULL;
    while ( strstr(tstr,sstr) != (char *)NULL )
        cst_rpst ( tstr, sstr, "$$", tstr, &ier );

    while ( strstr(tstr,"<") != (char *)NULL )
        cst_rpst ( tstr, "<", "&lt", tstr, &ier );
    while ( strstr(tstr,">") != (char *)NULL )
        cst_rpst ( tstr, ">", "&gt", tstr, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<rotn>%f"
	"<sztext>%f"
	"<sptxtyp>%d"
	"<turbsym>%d"
	"<itxfn>%d"
	"<ithw>%d"
	"<iwidth>%d"
	"<txtcol>%d"
	"<lincol>%d"
	"<filcol>%d"
	"<ialign>%d"
	"<lat>%f"
	"<lon>%f"
	"<offset_x>%d"
	"<offset_y>%d"
	"<text>%s",

	el->elem.spt.info.rotn,
	el->elem.spt.info.sztext,
	el->elem.spt.info.sptxtyp,
	el->elem.spt.info.turbsym,
	el->elem.spt.info.itxfn,
	el->elem.spt.info.ithw,
	el->elem.spt.info.iwidth,
	el->elem.spt.info.txtcol,
	el->elem.spt.info.lincol,
	el->elem.spt.info.filcol,
	el->elem.spt.info.ialign,
	el->elem.spt.info.lat,
	el->elem.spt.info.lon,
	el->elem.spt.info.offset_x,
	el->elem.spt.info.offset_y,
	tstr

	);

    free ( tstr );

}

/*=====================================================================*/

void	cir2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  CIRCLE_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<numpts>%d"
	"<lintyp>%d"
	"<lthw>%d"
	"<width>%d"
	"<lwhw>%d",

	el->elem.cir.info.numpts,
	el->elem.cir.info.lintyp,
	el->elem.cir.info.lthw,
	el->elem.cir.info.width,
	el->elem.cir.info.lwhw 

	);

    npts = 2;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.cir.data.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.cir.data.latlon[ii+npts] );
    }

    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	trk2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  TRKSTORM_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<subtype>%d"
	"<npts>%d"
	"<nipts>%d"
	"<ltype1>%d"
	"<ltype2>%d"
	"<mtype1>%d"
	"<mtype2>%d"
	"<width>%d"
	"<speed>%f"
	"<dir>%f"
	"<incr>%d"
	"<skip>%d"
	"<itxfn>%d"
	"<ithw>%d"
	"<sztext>%f",

	el->elem.trk.info.subtype,
	el->elem.trk.info.npts,
	el->elem.trk.info.nipts,
	el->elem.trk.info.ltype1,
	el->elem.trk.info.ltype2,
	el->elem.trk.info.mtype1,
	el->elem.trk.info.mtype2,
	el->elem.trk.info.width,
	el->elem.trk.info.speed,
	el->elem.trk.info.dir,
	el->elem.trk.info.incr,
	el->elem.trk.info.skip,
	el->elem.trk.info.itxfn,
	el->elem.trk.info.ithw,
	el->elem.trk.info.sztext

	);

    npts = el->elem.trk.info.npts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<times>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%s,", el->elem.trk.info.times[ii] );
    }

    /*  remove final comma before continuing	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.trk.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.trk.latlon[ii+npts] );
    }

    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	sig2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  SIGINTL_ELM, SIGNCON_ELM, SIGCONV_ELM, SIGOUTL_ELM, SIGAIRM_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<subtype>%d"
	"<npts>%d"
	"<lintyp>%d"
	"<linwid>%d"
	"<sol>%d"
	"<area>%s"
	"<fir>%s"
	"<status>%d"
	"<distance>%f"
	"<msgid>%s"
	"<seqnum>%d"
	"<stime>%s"
	"<etime>%s"
	"<remarks>%s"
	"<sonic>%d"
	"<phenom>%s"
	"<phenom2>%s"
	"<phennam>%s"
	"<phenlat>%s"
	"<phenlon>%s"
	"<pres>%d"
	"<maxwind>%d"
	"<freetext>%s"
	"<trend>%s"
	"<move>%s"
	"<obsfcst>%d"
	"<obstime>%s"
	"<fl>%d"
	"<spd>%d"
	"<dir>%s"
	"<tops>%s"
	"<fcstr>%s",

	el->elem.sig.info.subtype,
	el->elem.sig.info.npts,
	el->elem.sig.info.lintyp,
	el->elem.sig.info.linwid,
	el->elem.sig.info.sol,
	el->elem.sig.info.area,
	el->elem.sig.info.fir,
	el->elem.sig.info.status,
	el->elem.sig.info.distance,
	el->elem.sig.info.msgid,
	el->elem.sig.info.seqnum,
	el->elem.sig.info.stime,
	el->elem.sig.info.etime,
	el->elem.sig.info.remarks,
	el->elem.sig.info.sonic,
	el->elem.sig.info.phenom,
	el->elem.sig.info.phenom2,
	el->elem.sig.info.phennam,
	el->elem.sig.info.phenlat,
	el->elem.sig.info.phenlon,
	el->elem.sig.info.pres,
	el->elem.sig.info.maxwind,
	el->elem.sig.info.freetext,
	el->elem.sig.info.trend,
	el->elem.sig.info.move,
	el->elem.sig.info.obsfcst,
	el->elem.sig.info.obstime,
	el->elem.sig.info.fl,
	el->elem.sig.info.spd,
	el->elem.sig.info.dir,
	el->elem.sig.info.tops,
	el->elem.sig.info.fcstr

	);

    npts = el->elem.sig.info.npts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.sig.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.sig.latlon[ii+npts] );
    }

    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	ccf2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  SIGCCF_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<subtype>%d"
	"<npts>%d"
	"<cover>%d"
	"<tops>%d"
	"<prob>%d"
	"<growth>%d"
	"<spd>%f"
	"<dir>%f",

	el->elem.ccf.info.subtype,
	el->elem.ccf.info.npts,
	el->elem.ccf.info.cover,
	el->elem.ccf.info.tops,
	el->elem.ccf.info.prob,
	el->elem.ccf.info.growth,
	el->elem.ccf.info.spd,
	el->elem.ccf.info.dir

	);

    npts = el->elem.ccf.info.npts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.ccf.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.ccf.latlon[ii+npts] );
    }

    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	lst2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  LIST_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<subtyp>%d"
	"<mrktyp>%d"
	"<mrksiz>%f"
	"<mrkwid>%d"
	"<nitems>%d",

	el->elem.lst.info.subtyp,
	el->elem.lst.info.mrktyp,
	el->elem.lst.info.mrksiz,
	el->elem.lst.info.mrkwid,
	el->elem.lst.data.nitems

	);

    npts = el->elem.lst.data.nitems;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<item>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%s,", el->elem.lst.data.item[ii] );
    }
    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<lat>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.lst.data.lat[ii] );
    }
    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<lon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.lst.data.lon[ii] );
    }
    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';
}

/*=====================================================================*/

void	ash2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  ASHCLD_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<subtype>%d"
	"<npts>%d"
	"<distance>%f"
	"<fhr>%d"
	"<lintyp>%d"
	"<spd>%s"
	"<dir>%s"
	"<flvl1>%s"
	"<flvl2>%s",

	el->elem.ash.info.subtype,
	el->elem.ash.info.npts,
	el->elem.ash.info.distance,
	el->elem.ash.info.fhr,
	el->elem.ash.info.lintyp,
	el->elem.ash.info.spds,
	el->elem.ash.info.dir,
	el->elem.ash.info.flvl1,
	el->elem.ash.info.flvl2

	);

    npts = el->elem.ash.info.npts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.ash.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
	blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.ash.latlon[ii+npts] );
    }
    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	vol2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  VOLC_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<name>%s"
	"<code>%f"
	"<size>%f"
	"<width>%d"
	"<number>%s"
	"<location>%s"
	"<area>%s"
	"<elev>%s"
	"<origstn>%s"
	"<vaac>%s"
	"<wmoid>%s"
	"<hdrnum>%s"
	"<year>%s"
	"<advnum>%s"
	"<infosorc>%s"
	"<addlsorc>%s"
	"<details>%s"
	"<obsdattim>%s"
	"<obsdate>%s"
	"<obstime>%s"
	"<fcst_06>%s"
	"<fcst_12>%s"
	"<fcst_18>%s"
	"<remarks>%s"
	"<nextadv>%s"
	"<fcstrs>%s",

	el->elem.vol.info.name,
	el->elem.vol.info.code,
	el->elem.vol.info.size,
	el->elem.vol.info.width,
	el->elem.vol.info.number,
	el->elem.vol.info.location,
	el->elem.vol.info.area,
	el->elem.vol.info.elev,
	el->elem.vol.info.origstn,
	el->elem.vol.info.vaac,
	el->elem.vol.info.wmoid,
	el->elem.vol.info.hdrnum,
	el->elem.vol.info.year,
	el->elem.vol.info.advnum,
	el->elem.vol.info.infosorc,
	el->elem.vol.info.addlsorc,
	el->elem.vol.info.details,
	el->elem.vol.info.obsdate,
	el->elem.vol.info.obstime,
	el->elem.vol.info.obsashcld,
	el->elem.vol.info.fcst_06,
	el->elem.vol.info.fcst_12,
	el->elem.vol.info.fcst_18,
	el->elem.vol.info.remarks,
	el->elem.vol.info.nextadv,
	el->elem.vol.info.fcstrs

	);

    npts = 1;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.vol.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.vol.latlon[ii+npts] );
    }
    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<offset_xy>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%d,", el->elem.vol.offset_xy[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%d,", el->elem.vol.offset_xy[ii+npts] );
    }
    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	jet2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
char	*tstr, sstr[2];
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  JET_ELM -> line
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<splcol>%d"
	"<numpts>%d"
	"<spltyp>%d"
	"<splstr>%d"
	"<spldir>%d"
	"<splsiz>%f"
	"<splwid>%d",

	el->elem.jet.line.splcol,
	el->elem.jet.line.spl.info.numpts,
	el->elem.jet.line.spl.info.spltyp,
	el->elem.jet.line.spl.info.splstr,
	el->elem.jet.line.spl.info.spldir,
	el->elem.jet.line.spl.info.splsiz, 
	el->elem.jet.line.spl.info.splwid 

	);

    npts = el->elem.jet.line.spl.info.numpts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.jet.line.spl.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.jet.line.spl.latlon[ii+npts] );
    }

    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

    
    /*  
     *  JET_ELM -> barbs & texts
     */
    blen = strlen(buffer);

    sprintf( &(buffer[blen]), "<nbarb>%d", el->elem.jet.nbarb );

    npts = el->elem.jet.nbarb;

    for ( ii = 0; ii < npts; ii++ )  {
        	 
	blen = strlen(buffer);

        tstr = (char *)malloc( strlen(el->elem.jet.barb[ii].spt.text)*2 * sizeof(char) );
        strcpy ( tstr, el->elem.jet.barb[ii].spt.text );
        while ( strstr(tstr,"\n") != (char *)NULL ) 
            cst_rpst ( tstr, "\n", "$$", tstr, &ier );
	sstr[0] = CHLF;
	sstr[1] = CHNULL;
	while ( strstr(tstr,sstr) != (char *)NULL )
	    cst_rpst ( tstr, sstr, "$$", tstr, &ier );
	sstr[0] = CHCR;
	sstr[1] = CHNULL;
	while ( strstr(tstr,sstr) != (char *)NULL )
	    cst_rpst ( tstr, sstr, "$$", tstr, &ier );
	
	sprintf( &(buffer[blen]),
	    "<jet_barb_%d>%d,%d,%d,%4.1f,%d,%4.1f,%6.2f,%7.2f,%6.2f,%7.2f"
	    "<jet_text_%d>%d,%4.1f,%4.1f,%d,%d,%d,%d,%d,%d,%d,%d,%d,%6.2f,%7.2f,%d,%d,%s",
	    
	    ii+1,
	    el->elem.jet.barb[ii].wndcol,
	    el->elem.jet.barb[ii].wnd.info.numwnd,
	    el->elem.jet.barb[ii].wnd.info.width,
	    el->elem.jet.barb[ii].wnd.info.size,
	    el->elem.jet.barb[ii].wnd.info.wndtyp,
	    el->elem.jet.barb[ii].wnd.info.hdsiz,
	    el->elem.jet.barb[ii].wnd.data.spddir[0],
	    el->elem.jet.barb[ii].wnd.data.spddir[1],
	    el->elem.jet.barb[ii].wnd.data.latlon[0],
	    el->elem.jet.barb[ii].wnd.data.latlon[1],
            
	    ii+1,
	    el->elem.jet.barb[ii].sptcol,
	    el->elem.jet.barb[ii].spt.info.rotn,
	    el->elem.jet.barb[ii].spt.info.sztext,
	    el->elem.jet.barb[ii].spt.info.sptxtyp,
	    el->elem.jet.barb[ii].spt.info.turbsym,
	    el->elem.jet.barb[ii].spt.info.itxfn,
	    el->elem.jet.barb[ii].spt.info.ithw,
	    el->elem.jet.barb[ii].spt.info.iwidth,
	    el->elem.jet.barb[ii].spt.info.txtcol,
	    el->elem.jet.barb[ii].spt.info.lincol,
	    el->elem.jet.barb[ii].spt.info.filcol,
	    el->elem.jet.barb[ii].spt.info.ialign,
	    el->elem.jet.barb[ii].spt.info.lat,
	    el->elem.jet.barb[ii].spt.info.lon,
	    el->elem.jet.barb[ii].spt.info.offset_x,
	    el->elem.jet.barb[ii].spt.info.offset_y,
	    tstr
	);
    }
    
    
    /*  
     *  JET_ELM -> hashs
     */
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<nhash>%d", el->elem.jet.nhash );

    npts = el->elem.jet.nhash;

    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]),
	    "<jet_hash_%d>%d,%d,%d,%4.1f,%d,%4.1f,%6.2f,%7.2f,%6.2f,%7.2f",

	    ii+1,
	    el->elem.jet.hash[ii].wndcol,
	    el->elem.jet.hash[ii].wnd.info.numwnd,
	    el->elem.jet.hash[ii].wnd.info.width,
	    el->elem.jet.hash[ii].wnd.info.size,
	    el->elem.jet.hash[ii].wnd.info.wndtyp,
	    el->elem.jet.hash[ii].wnd.info.hdsiz,
	    el->elem.jet.hash[ii].wnd.data.spddir[0],
	    el->elem.jet.hash[ii].wnd.data.spddir[1],
	    el->elem.jet.hash[ii].wnd.data.latlon[0],
	    el->elem.jet.hash[ii].wnd.data.latlon[1]
	);
    }
            
}

/*=====================================================================*/

void	gfa2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*  
     *  GFA_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s%d", TAG_GFA_NBLOCKS, el->elem.gfa.info.nblocks );    
    
    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s%d", TAG_GFA_NPTS, el->elem.gfa.info.npts );

    for ( ii = 0; ii < el->elem.gfa.info.nblocks; ii++ ) {
	strcat ( buffer, *(el->elem.gfa.info.blockPtr[ ii ]) );
    }

    npts = el->elem.gfa.info.npts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "%s", TAG_GFA_POINTS );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.gfa.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.gfa.latlon[ii+npts] );
    }

    /*  remove final comma */
    blen = strlen(buffer);
    buffer[blen-1] = '\0';
    
}

/*=====================================================================*/

void	sgwx2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	ii, npts, ier;
size_t	blen;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*  
     *  SIGWX_ELM
     */

    hdr2tag ( el, buffer, &ier );

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), 

	"<subtype>%d"
	"<npts>%d",
	el->elem.sgwx.info.subtype,
	el->elem.sgwx.info.npts
	);

    npts = el->elem.sgwx.info.npts;

    blen = strlen(buffer);
    sprintf( &(buffer[blen]), "<latlon>" );
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%6.2f,", el->elem.sgwx.latlon[ii] );
    }
    for ( ii = 0; ii < npts; ii++ )  {
        blen = strlen(buffer);
	sprintf( &(buffer[blen]), "%7.2f,", el->elem.sgwx.latlon[ii+npts] );
    }

    /*  remove final comma 	*/
    blen = strlen(buffer);
    buffer[blen-1] = '\0';

}

/*=====================================================================*/

void	tca2tag ( VG_DBStruct *el, char *buffer, int *iret )
{
int	counter, ii, ier;
size_t	buf_len;
char	str_tmp[ STD_STRLEN ];
/*---------------------------------------------------------------------*/

    *iret = 0;
    ier = 0;
    str_tmp[ 0 ] = '\0';

    /*  
     *  TCA_ELM
     */

    hdr2tag ( el, buffer, &ier );

    buf_len = strlen( buffer );
    sprintf( &( buffer[ buf_len ] ), 

	"<%s>%d<%s>%c<%s>%d<%s>%s<%s>%s<%s>%d<%s>%s<%s>%s<%s>%f<%s>%f<%s>%d<%s>%f<%s>%d<%s>%d",

	TCA_STORMNUM_TAG,
	el->elem.tca.info.stormNum,
	TCA_ISSUESTATUS_TAG,
	el->elem.tca.info.issueStatus,
	TCA_BASIN_TAG,
	el->elem.tca.info.basin,
	TCA_ADVISORYNUM_TAG,
	el->elem.tca.info.advisoryNum,
	TCA_STORMNAME_TAG,
	el->elem.tca.info.stormName,
	TCA_STORMTYPE_TAG,
	el->elem.tca.info.stormType,
	TCA_VALIDTIME_TAG,
	el->elem.tca.info.validTime,
	TCA_TIMEZONE_TAG,
	el->elem.tca.info.timezone,
        TCA_TEXTLAT_TAG,
        el->elem.tca.info.text_lat,
        TCA_TEXTLON_TAG,
        el->elem.tca.info.text_lon,
        TCA_TEXTFONT_TAG,
        el->elem.tca.info.text_font,
        TCA_TEXTSIZE_TAG,
        el->elem.tca.info.text_size,
        TCA_TEXTWIDTH_TAG,
        el->elem.tca.info.text_width,
	TCA_WWNUM_TAG,
	el->elem.tca.info.wwNum

	);

    for ( counter = 0; counter < el->elem.tca.info.wwNum; counter++ ) {
        /*
         * Load TCA watches/warning string
         */
        sprintf( str_tmp, "<%s_%d>%d|%d|%d",
                          TCA_TCAWWSTR_TAG, counter,
                          el->elem.tca.info.tcaww[ counter ].severity,
                          el->elem.tca.info.tcaww[ counter ].advisoryType,
                          el->elem.tca.info.tcaww[ counter ].specialGeog );
        strcat( buffer, str_tmp );

        /*
         * Load TCA break points
         */
	sprintf( str_tmp, "<%s_%d>%d", TCA_NUMBKPTS_TAG, counter, 
			el->elem.tca.info.tcaww[ counter ].numBreakPts );
        strcat( buffer, str_tmp );

        sprintf( str_tmp, "<%s_%d>", TCA_BREAKPTS_TAG, counter );
        strcat( buffer, str_tmp );

	for ( ii = 0; ii < el->elem.tca.info.tcaww[ counter ].numBreakPts; ii++ ) {
            sprintf( str_tmp, "%f|%f|%s",
                          el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lat,
                          el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].lon,
                          el->elem.tca.info.tcaww[ counter ].breakPnt[ ii ].breakPtName );
	    if ( ii != el->elem.tca.info.tcaww[ counter ].numBreakPts - 1 ) {
	       strcat( str_tmp, "|" );
	    }
            strcat( buffer, str_tmp );
	}
    }
} 

/*=====================================================================*/
