#include "cvgcmn.h"

void cvg_todev2 ( VG_DBStruct *el, int *np, float *dx, float *dy, int *iret )
/************************************************************************
 * cvg_todev2								*
 *									*
 * This function converts the map based coordinates that are used in	*
 * the VGF file to a device based coordinate equivalent to the current	*
 * display. The difference with cvg_todev is that for non single point  *
 * objects (lines, polygons, etc) the cgr_range() is used instead of    *
 * gtrans() so that wrapping around the 'back' of the device is taken   *
 * into consideration and the polygon/lines generated have points that  *
 * stay consistent around the 'break'.  HOWEVER, this does not unroll   *
 * the object, but only returns the first representation of the object. *
 *									*
 * cvg_todev2 ( el, np, dx, dy, iret )					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 *									*
 * Output parameters:							*
 *	*np		int		Number of points		*
 *	*dx		float		Array of device coordinates	*
 *	*dy		float		Array of device coordinates	*
 *	*iret		int		Return code			*
 *					 -4 = unknown VG record type	*
 *									*
 **									*
 * Log:									*
 * S. Danz/AWC		03/06	Copied from cvg_todev to use cgr_range 	*
 *                              instead of gtrans to handle unrolling   *
 ***********************************************************************/
{
    char	grp[4];
    int		ier, ier1, qpoly, roll, nout, loglev, counter, ibkpts;
    float	lat[ MAXPTS ], lon[ MAXPTS ], xll, yll, xur, yur;
/*---------------------------------------------------------------------*/
    strcpy(grp, "CVG");
    loglev = 0;

    *iret = 0;

    if (el->hdr.closed) {
        qpoly = G_TRUE;
    } else {
        qpoly = G_FALSE;
    }

    switch ( el->hdr.vg_type)
    {
/*
 *      For point objects, just use the cvg_todev as it is today
 *      since there is no possible line/polygon for 'unrolling'
 */
        case TEXT_ELM:
        case TEXTC_ELM:
	case SPTX_ELM:
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
	case BARB_ELM:
	case ARROW_ELM:
	case DARR_ELM:
	case HASH_ELM:
        case VOLC_ELM:
            cvg_todev (el, np, dx, dy, iret);
	    break;

	case FRONT_ELM:

	    *np = el->elem.frt.info.numpts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, el->elem.frt.latlon,
                        &(el->elem.frt.latlon[*np]),
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case CIRCLE_ELM:

	    *np = el->elem.cir.info.numpts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, el->elem.cir.data.latlon,
                        &(el->elem.cir.data.latlon[*np]),
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case SPLN_ELM:

	    *np = el->elem.spl.info.numpts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, el->elem.spl.latlon,
                        &(el->elem.spl.latlon[*np]),
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case LINE_ELM:

            *np = el->elem.lin.info.numpts;
/*
 *          Convert all points in element to device...
 */
            cgr_range ( sys_M, np, el->elem.lin.latlon,
                        &(el->elem.lin.latlon[*np]),
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
            break;

	case WBOX_ELM:

	    *np = el->elem.wbx.info.numpts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, el->elem.wbx.latlon,
                        &(el->elem.wbx.latlon[*np]),
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case TRKSTORM_ELM:

	    *np = el->elem.trk.info.npts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.trk.latlon[0],
                        &el->elem.trk.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;


	case SIGAIRM_ELM:
	case SIGCONV_ELM:
	case SIGINTL_ELM:
	case SIGNCON_ELM:
	case SIGOUTL_ELM:

	    *np = el->elem.sig.info.npts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.sig.latlon[0],
                        &el->elem.sig.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case SIGCCF_ELM:

	    *np = el->elem.ccf.info.npts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.ccf.latlon[0],
                        &el->elem.ccf.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case ASHCLD_ELM:

	    *np = el->elem.ash.info.npts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.ash.latlon[0], 
                        &el->elem.ash.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case LIST_ELM:

	    *np = el->elem.lst.data.nitems;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.lst.data.lat[0],
                        &el->elem.lst.data.lon[0],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case JET_ELM:

	    *np = el->elem.jet.line.spl.info.numpts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.jet.line.spl.latlon[0],
                        &el->elem.jet.line.spl.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case GFA_ELM:

	    *np = el->elem.gfa.info.npts;

            cgr_range ( sys_M, np, &el->elem.gfa.latlon[0],
                        &el->elem.gfa.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case TCA_ELM:

	    *np = 0;
	    for ( counter = 0; counter < el->elem.tca.info.wwNum; counter++ ) {
		for ( ibkpts = 0; ibkpts < el->elem.tca.info.tcaww[ counter ].numBreakPts; ibkpts++ ){
		    lat[ *np ] = el->elem.tca.info.tcaww[ counter ].breakPnt[ ibkpts ].lat;
		    lon[ *np ] = el->elem.tca.info.tcaww[ counter ].breakPnt[ ibkpts ].lon;
		    (*np)++;
		}
	    }
            cgr_range ( sys_M, np, lat, lon,
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	default:
	    *iret = -4;
            er_lmsg( &loglev, grp, iret, NULL, &ier1, strlen(grp));

	    break;
    }
}
