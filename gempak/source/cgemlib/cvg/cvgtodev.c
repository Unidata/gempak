#include "cvgcmn.h"

void cvg_todev ( VG_DBStruct *el, int *np, float *dx, float *dy, int *iret )
/************************************************************************
 * cvg_todev								*
 *									*
 * This function converts the map based coordinates that are used in	*
 * the VGF file to a device based coordinate equivalent to the current	*
 * display.								*
 *									*
 * cvg_todev ( el, np, dx, dy, iret )					*
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
 * E. Wehner/EAi	11/96	Created					*
 * D. Keiser/GSC	 1/97	Clean up				*
 * D. Keiser/GSC	 3/97	Added contour type			*
 * D. Keiser/GSC	 4/97	Added special line			*
 * E. Safford/GSC        4/97   Added text type                 	*
 * E. Wehner/EAi	 5/97	Added symbols/lines			*
 * E. Wehner/EAi	 5/97	Added wind barbs			*
 * E. Safford/GSC	 6/97	Added special text			*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * E. Wehner/EAi	 9/97	REmoved xwdrawcmn.h and unused comments	*
 * W. Li/EAI		04/98	Add DARR_ELM, HASH_ELM			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * A. Hardy/GSC         10/98   Added CMBSY_ELM                         *
 * T. Piper/GSC		10/98	Prolog update				*
 * A. Hardy/GSC         11/98   Modified CIRCLE_ELM case statements     *
 * S. Law/GSC		05/99	Added TRKSTORM_ELM			*
 * G. Krueger/EAI	05/99	Modified circles for latlon array	*
 * S. Law/GSC		07/99	Added SIGINTL_ELM			*
 * S. Law/GSC		08/99	Added remaining SIGMETs			*
 * S. Law/GSC		02/00	Added SIGCCF_ELM			*
 * J. Wu/GSC		11/00	Removed unused CONTOUR_ELM case	        *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * J. Wu/SAIC		11/02	add LIST_ELM	        		*
 * H. Zeng/XTRIA	07/03   added VOLC_ELM				*
 * J. Wu/SAIC		10/03	add JET_ELM	        		*
 * H. Zeng/XTRIA	10/03   added ASHCLD_ELM			*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * J. Wu/SAIC		02/04	add GFA_ELM	        		*
 * B. Yin/SAIC		04/04	add TCA_ELM	        		*
 * B. Yin/SAIC		05/04	Modified TCA_ELM case statement.	*
 * B. Yin/SAIC		07/04	Modified TCA case for water and islands	*
 * S. Gilbert/NCEP	11/05	Added Zero WW case for TCA_ELM          *
 * L. Hinson/AWC        01/12   Add SGWX_ELM                            *
 ***********************************************************************/
{
    int		ier;
    char	grp[4];
    int		ier1;
    int		loglev, counter, ibkpts;
    float	lat[ MAXPTS ], lon[ MAXPTS ];
/*---------------------------------------------------------------------*/
    strcpy(grp, "CVG");
    loglev = 0;

    *iret = 0;

    switch ( el->hdr.vg_type)
    {
	case FRONT_ELM:

	    *np = el->elem.frt.info.numpts;
/*
 *	    Convert all points in element to device...
 */
	    gtrans(sys_M, sys_D, np, el->elem.frt.latlon, 
		   &(el->elem.frt.latlon[*np]),
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break;

	case CIRCLE_ELM:

	    *np = el->elem.cir.info.numpts;
/*
 *	    Convert all points in element to device...
 */
	    gtrans(sys_M, sys_D, np, el->elem.cir.data.latlon, 
		   &(el->elem.cir.data.latlon[*np]),
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break;

	case SPLN_ELM:

	    *np = el->elem.spl.info.numpts;
/*
 *	    Convert all points in element to device...
 */
	    gtrans(sys_M, sys_D, np, el->elem.spl.latlon, 
		   &(el->elem.spl.latlon[*np]),
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break;

	case LINE_ELM:

            *np = el->elem.lin.info.numpts;
/*
 *          Convert all points in element to device...
 */
            gtrans(sys_M, sys_D, np, el->elem.lin.latlon, 
		   &(el->elem.lin.latlon[*np]),
		   dx, dy, &ier,
                   strlen(sys_M), strlen(sys_D));
            break;

	case WBOX_ELM:

	    *np = el->elem.wbx.info.numpts;
/*
 *	    Convert all points in element to device...
 */
	    gtrans(sys_M, sys_D, np, el->elem.wbx.latlon, 
		   &(el->elem.wbx.latlon[*np]),
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break;

        case TEXT_ELM:
        case TEXTC_ELM:

            *np = 1;
            gtrans(sys_M, sys_D, np, 
		   &el->elem.txt.info.lat, &el->elem.txt.info.lon,
		   dx, dy, &ier,
                   strlen(sys_M), strlen(sys_D));
            break;

	case SPTX_ELM:
            *np = 1;
            gtrans(sys_M, sys_D, np, 
		   &el->elem.spt.info.lat, &el->elem.spt.info.lon,
		   dx, dy, &ier,
                   strlen(sys_M), strlen(sys_D));
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
/*
 *          Convert the point in the element to device coordinates.
 */
            *np = 1;
            gtrans(sys_M, sys_D, np, 
		   &el->elem.sym.data.latlon[0], &el->elem.sym.data.latlon[*np],
		   dx, dy, &ier,
                   strlen(sys_M), strlen(sys_D));
	    break;

	case BARB_ELM:
	case ARROW_ELM:
	case DARR_ELM:
	case HASH_ELM:
/*
 *          Convert the point in the element to device coordinates.
 */
            *np = 1;
            gtrans(sys_M, sys_D, np, 
		   &el->elem.wnd.data.latlon[0], &el->elem.wnd.data.latlon[*np],
		   dx, dy, &ier,
                   strlen(sys_M), strlen(sys_D));
            break;

	case TRKSTORM_ELM:

	    *np = el->elem.trk.info.npts;
/*
 *	    Convert all points in element to device...
 */
	    gtrans(sys_M, sys_D, np, &el->elem.trk.latlon[0], 
		   &el->elem.trk.latlon[*np],
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
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
	    gtrans(sys_M, sys_D, np, &el->elem.sig.latlon[0], 
		   &el->elem.sig.latlon[*np],
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break;

	case SIGCCF_ELM:

	    *np = el->elem.ccf.info.npts;
/*
 *	    Convert all points in element to device...
 */
	    gtrans(sys_M, sys_D, np, &el->elem.ccf.latlon[0], 
		   &el->elem.ccf.latlon[*np],
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break;

        case VOLC_ELM:
/*
 *          Convert the point in the element to device coordinates.
 */
            *np = 1;
            gtrans(sys_M, sys_D, np, 
		   &el->elem.vol.latlon[0], &el->elem.vol.latlon[*np],
		   dx, dy, &ier,
                   strlen(sys_M), strlen(sys_D));
	    break;

	case ASHCLD_ELM:

	    *np = el->elem.ash.info.npts;
/*
 *	    Convert all points in element to device...
 */
	    gtrans(sys_M, sys_D, np, &el->elem.ash.latlon[0], 
		   &el->elem.ash.latlon[*np],
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break;

	case LIST_ELM:

	    *np = el->elem.lst.data.nitems;
/*
 *	    Convert all points in element to device...
 */
	    gtrans(sys_M, sys_D, np, &el->elem.lst.data.lat[0], 
		   &el->elem.lst.data.lon[0],
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break;

	case JET_ELM:

	    *np = el->elem.jet.line.spl.info.numpts;
/*
 *	    Convert all points in element to device...
 */
	    gtrans(sys_M, sys_D, np, &el->elem.jet.line.spl.latlon[0], 
		   &el->elem.jet.line.spl.latlon[*np],
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break;

	case GFA_ELM:

	    *np = el->elem.gfa.info.npts;

	    gtrans(sys_M, sys_D, np, &el->elem.gfa.latlon[0], 
		   &el->elem.gfa.latlon[*np],
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break;

        case SGWX_ELM:
            *np = el->elem.sgwx.info.npts;
/*
 *	    Convert all points in element to device...
 */
	    gtrans(sys_M, sys_D, np, &el->elem.sgwx.latlon[0], 
		   &el->elem.sgwx.latlon[*np],
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break; 

	case TCA_ELM:

	    *np = 0;

            if ( el->elem.tca.info.wwNum == 0 ) {
               lat[0] = el->elem.tca.info.text_lat;
               lon[0] = el->elem.tca.info.text_lon;
               *np = 1;
            }
            else {
	        for ( counter = 0; counter < el->elem.tca.info.wwNum; counter++ ) {
		    for ( ibkpts = 0; ibkpts < el->elem.tca.info.tcaww[ counter ].numBreakPts; ibkpts++ ){
		        lat[ *np ] = el->elem.tca.info.tcaww[ counter ].breakPnt[ ibkpts ].lat;
		        lon[ *np ] = el->elem.tca.info.tcaww[ counter ].breakPnt[ ibkpts ].lon;
		        (*np)++;
		    }
	        }
	    }
	    gtrans(sys_M, sys_D, np, lat, lon, 
		   dx, dy, &ier,
		   strlen(sys_M), strlen(sys_D));
	    break;

	default:
	    *iret = -4;
            er_lmsg( &loglev, grp, iret, NULL, &ier1, strlen(grp));

	    break;
    }
}
