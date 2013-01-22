#include "cvgcmn.h"

void cvg_swap ( int flag, int readflg, VG_DBStruct elold, 
					VG_DBStruct *elnew, int *iret )
/************************************************************************
 * cvg_swap								*
 *									*
 * This function swaps the byte order of VG record structure	 	*
 *									*
 * cvg_swap ( flag, readflg, elold, elnew, iret )			*
 *									*
 * Input parameters:							*
 *	flag		int		which to swap			*
 *	readflg		int		read or write			*
 *	elold		VG_DBStruct	VG record structure		*
 *									*
 * Output parameters:							*
 *	*elnew		VG_DBStruct	Pointer to swapped VG structure	*
 *	*iret		int		Return code			*
 *					 0  = OK			*
 *					 >0 = # of invalid swap		*
 *									*
 **									*
 * Log:									*
 * M. Li/GSC		12/99	Created 				*
 * D.W.Plummer/NCEP	 1/00	Changes for expanded watch element	*
 * S. Law/GSC		02/00	Added CCF				*
 * F. J. Yen/NCEP	08/00	Changes for expanded sigmet element	*
 * D.W.Plummer/NCEP	10/00	Changes for expanded watch element	*
 * M. Li/GSC		10/00   Added skip and text font for TRACK	*
 * J. Wu/GSC		11/00	Removed unused CONTOUR_ELM case	        *
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * J. Wu/SAIC		09/02	add case LIST_ELM		 	*
 * J. Wu/SAIC		11/02	revise for LIST structure	 	*
 * H. Zeng/XTRIA        01/03   added new info for WatchBox             *
 * J. Wu/SAIC		09/03	add JET_ELM		 		*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * J. Wu/SAIC		01/04	add GFA_ELM		 		*
 * H. Zeng/XTRIA	03/04	added VOLC_ELM and ASHCLD_ELM		*
 * J. Wu/SAIC		10/04	change GFA structure	 		*
 * m.gamazaychikov/SAIC	05/07	add TCERR, TCBKL and TCTRK ELEMs	*
 * L. Hinson/AWC        01/12   Add SGWX_ELM                            * 
***********************************************************************/
{
    int		ier, nswp, npts, npts2, ii;
/*---------------------------------------------------------------------*/    

    ier   = 0;
    nswp  = 1;

    /* 
     * Swap byte order for int and float in the header sub-structure  
     */
    if ( flag == SWPALL || flag == SWPHDR ) {

	ier += mv_swp4( &nswp, &(elold.hdr.grpnum), &(elnew->hdr.grpnum) );
   	ier += mv_swp4( &nswp, &(elold.hdr.maj_col), &(elnew->hdr.maj_col) ); 
     	ier += mv_swp4( &nswp, &(elold.hdr.min_col), &(elnew->hdr.min_col) );
     	ier += mv_swp4( &nswp, &(elold.hdr.recsz), &(elnew->hdr.recsz) );

	ier += mv_swp4( &nswp, &(elold.hdr.range_min_lat), 		
			       &(elnew->hdr.range_min_lat) );
	ier += mv_swp4( &nswp, &(elold.hdr.range_min_lon), 		
			       &(elnew->hdr.range_min_lon) );
	ier += mv_swp4( &nswp, &(elold.hdr.range_max_lat), 		
			       &(elnew->hdr.range_max_lat) );
	ier += mv_swp4( &nswp, &(elold.hdr.range_max_lon), 		
			       &(elnew->hdr.range_max_lon) );
    }

    /* Swap byte order for int and float for all element types */
    if ( flag == SWPALL || flag == SWPINF ) {
	switch (elold.hdr.vg_type) {

	  case FRONT_ELM:	/* Front */

	    ier += mv_swp4( &nswp, &(elold.elem.frt.info.numpts), 
			    &(elnew->elem.frt.info.numpts) );
	    ier += mv_swp4( &nswp, &(elold.elem.frt.info.fcode), 
			    &(elnew->elem.frt.info.fcode) );
	    ier += mv_swp4( &nswp, &(elold.elem.frt.info.fpipsz), 
			    &(elnew->elem.frt.info.fpipsz) );
	    ier += mv_swp4( &nswp, &(elold.elem.frt.info.fpipst), 
			    &(elnew->elem.frt.info.fpipst) );
	    ier += mv_swp4( &nswp, &(elold.elem.frt.info.fpipdr), 
			    &(elnew->elem.frt.info.fpipdr) );
	    ier += mv_swp4( &nswp, &(elold.elem.frt.info.fwidth), 
			    &(elnew->elem.frt.info.fwidth) );

	    if (readflg == G_FALSE) {
		npts = 2 * (elold.elem.frt.info.numpts);
	    }
	    else {
		npts = 2 * (elnew->elem.frt.info.numpts);
	    }

	    ier += mv_swp4( &npts, (elold.elem.frt.latlon), 
			    (elnew->elem.frt.latlon) );
	    break;

	  case LINE_ELM:	/* Line   */

	    ier += mv_swp4( &nswp, &(elold.elem.lin.info.numpts), 
			    &(elnew->elem.lin.info.numpts) );
	    ier += mv_swp4( &nswp, &(elold.elem.lin.info.lintyp), 
			    &(elnew->elem.lin.info.lintyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.lin.info.lthw), 
			    &(elnew->elem.lin.info.lthw) );
	    ier += mv_swp4( &nswp, &(elold.elem.lin.info.width), 
			    &(elnew->elem.lin.info.width) );
	    ier += mv_swp4( &nswp, &(elold.elem.lin.info.lwhw), 
			    &(elnew->elem.lin.info.lwhw) );

	    if (readflg == G_FALSE) {		    
		npts = 2 * elold.elem.lin.info.numpts;
	    }else {
		npts = 2 * elnew->elem.lin.info.numpts;
	    }
	    ier += mv_swp4( &npts, (elold.elem.lin.latlon), 
			    (elnew->elem.lin.latlon) );
	    break;

	  case SPLN_ELM: 	/* Special Line	*/

	    ier += mv_swp4( &nswp, &(elold.elem.spl.info.numpts), 
			    &(elnew->elem.spl.info.numpts) );
	    ier += mv_swp4( &nswp, &(elold.elem.spl.info.spltyp), 
			    &(elnew->elem.spl.info.spltyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.spl.info.splstr), 
			    &(elnew->elem.spl.info.splstr) );
	    ier += mv_swp4( &nswp, &(elold.elem.spl.info.spldir), 
			    &(elnew->elem.spl.info.spldir) );
	    ier += mv_swp4( &nswp, &(elold.elem.spl.info.splsiz), 
			    &(elnew->elem.spl.info.splsiz) );
	    ier += mv_swp4( &nswp, &(elold.elem.spl.info.splwid), 
			    &(elnew->elem.spl.info.splwid) );

	    if (readflg == G_FALSE) {
		npts = 2 * (elold.elem.spl.info.numpts);
	    }
	    else {
		npts = 2 *(elnew->elem.spl.info.numpts);
	    }
	    ier += mv_swp4( &npts, (elold.elem.spl.latlon), 
			    (elnew->elem.spl.latlon) );
	    break;

	  case WBOX_ELM:	/* Watch box	*/

	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.numpts), 
			    &(elnew->elem.wbx.info.numpts) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_style), 
			    &(elnew->elem.wbx.info.w_style) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_type), 
			    &(elnew->elem.wbx.info.w_type) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_number), 
			    &(elnew->elem.wbx.info.w_number) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_shape), 
			    &(elnew->elem.wbx.info.w_shape) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_mrktyp), 
			    &(elnew->elem.wbx.info.w_mrktyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_mrksiz), 
			    &(elnew->elem.wbx.info.w_mrksiz) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_mrkwid), 
			    &(elnew->elem.wbx.info.w_mrkwid) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_istat), 
			    &(elnew->elem.wbx.info.w_istat) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_severity), 
			    &(elnew->elem.wbx.info.w_severity) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_issued), 
			    &(elnew->elem.wbx.info.w_issued) );

	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_a0lt), 
			    &(elnew->elem.wbx.info.w_a0lt) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_a0ln), 
			    &(elnew->elem.wbx.info.w_a0ln) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_a0dis), 
			    &(elnew->elem.wbx.info.w_a0dis) );

	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_a1lt), 
			    &(elnew->elem.wbx.info.w_a1lt) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_a1ln), 
			    &(elnew->elem.wbx.info.w_a1ln) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_a1dis), 
			    &(elnew->elem.wbx.info.w_a1dis) );

	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.numcnty), 
			    &(elnew->elem.wbx.info.numcnty) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.cn_flag), 
			    &(elnew->elem.wbx.info.cn_flag) );
	    if (readflg == G_FALSE) {
		npts = elold.elem.wbx.info.numcnty;
		npts2 = 2 * (elold.elem.wbx.info.numcnty);
	    }
	    else {
		npts = elnew->elem.wbx.info.numcnty;
		npts2 = 2 * (elnew->elem.wbx.info.numcnty);
	    }
	    ier += mv_swp4( &npts, (elold.elem.wbx.info.cn_fips), 
			    (elnew->elem.wbx.info.cn_fips) );
	    ier += mv_swp4( &npts2, (elold.elem.wbx.info.cn_ltln), 
			    (elnew->elem.wbx.info.cn_ltln) );

	    if (readflg == G_FALSE) {
		npts = 2 * (elold.elem.wbx.info.numpts);
	    }
	    else {
		npts = 2 * (elnew->elem.wbx.info.numpts);
	    }
	    ier += mv_swp4( &npts, (elold.elem.wbx.latlon), 
			    (elnew->elem.wbx.latlon) );	
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
	  case CMBSY_ELM:   /* Symbol Type	*/

	    ier += mv_swp4( &nswp, &(elold.elem.sym.info.numsym), 
			    &(elnew->elem.sym.info.numsym) );
	    ier += mv_swp4( &nswp, &(elold.elem.sym.info.width), 
			    &(elnew->elem.sym.info.width) );
	    ier += mv_swp4( &nswp, &(elold.elem.sym.info.size), 
			    &(elnew->elem.sym.info.size) );
	    ier += mv_swp4( &nswp, &(elold.elem.sym.info.ityp), 
			    &(elnew->elem.sym.info.ityp) );

	    if (readflg == G_FALSE) {
		npts = (elold.elem.sym.info.numsym);
	    }
	    else {
		npts = (elnew->elem.sym.info.numsym);
	    }
	    ier += mv_swp4( &npts, (elold.elem.sym.data.code), 
			    (elnew->elem.sym.data.code) );
	    npts = 2 * npts;
	    ier += mv_swp4( &npts, (elold.elem.sym.data.latlon), 
			    (elnew->elem.sym.data.latlon) );
	    ier += mv_swp4( &npts, (elold.elem.sym.data.offset_xy), 
			    (elnew->elem.sym.data.offset_xy) );
	    break;

	  case BARB_ELM:
	  case ARROW_ELM:
	  case DARR_ELM:
	  case HASH_ELM:     /* Wind Type	*/

	    ier += mv_swp4( &nswp, &(elold.elem.wnd.info.numwnd), 
			    &(elnew->elem.wnd.info.numwnd) );
	    ier += mv_swp4( &nswp, &(elold.elem.wnd.info.width), 
			    &(elnew->elem.wnd.info.width) );
	    ier += mv_swp4( &nswp, &(elold.elem.wnd.info.size), 
			    &(elnew->elem.wnd.info.size) );
	    ier += mv_swp4( &nswp, &(elold.elem.wnd.info.wndtyp), 
			    &(elnew->elem.wnd.info.wndtyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.wnd.info.hdsiz), 
			    &(elnew->elem.wnd.info.hdsiz) );

	    if (readflg == G_FALSE) {
		npts = 2 * (elold.elem.wnd.info.numwnd);
	    }
	    else {
		npts = 2 * (elnew->elem.wnd.info.numwnd);
	    }

	    ier += mv_swp4( &npts, (elold.elem.wnd.data.spddir), 
			    (elnew->elem.wnd.data.spddir) );
	    ier += mv_swp4( &npts, (elold.elem.wnd.data.latlon), 
			    (elnew->elem.wnd.data.latlon) );

	    break;

	  case TEXT_ELM:
	  case TEXTC_ELM:	/* Text Type	*/

	    ier += mv_swp4( &nswp, &(elold.elem.txt.info.rotn), 
			    &(elnew->elem.txt.info.rotn) );
	    ier += mv_swp4( &nswp, &(elold.elem.txt.info.sztext), 
			    &(elnew->elem.txt.info.sztext) );
	    ier += mv_swp4( &nswp, &(elold.elem.txt.info.itxfn), 
			    &(elnew->elem.txt.info.itxfn) );
	    ier += mv_swp4( &nswp, &(elold.elem.txt.info.ithw), 
			    &(elnew->elem.txt.info.ithw) );
	    ier += mv_swp4( &nswp, &(elold.elem.txt.info.iwidth), 
			    &(elnew->elem.txt.info.iwidth) );
	    ier += mv_swp4( &nswp, &(elold.elem.txt.info.ialign), 
			    &(elnew->elem.txt.info.ialign) );
	    ier += mv_swp4( &nswp, &(elold.elem.txt.info.lat), 
			    &(elnew->elem.txt.info.lat) );
	    ier += mv_swp4( &nswp, &(elold.elem.txt.info.lon), 
			    &(elnew->elem.txt.info.lon) );
	    ier += mv_swp4( &nswp, &(elold.elem.txt.info.offset_x), 
			    &(elnew->elem.txt.info.offset_x) );
	    ier += mv_swp4( &nswp, &(elold.elem.txt.info.offset_y), 
			    &(elnew->elem.txt.info.offset_y) );
	    break;

	  case SPTX_ELM:	/* Special Text	*/

	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.rotn), 
			    &(elnew->elem.spt.info.rotn) );	
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.sztext), 
			    &(elnew->elem.spt.info.sztext) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.sptxtyp), 
			    &(elnew->elem.spt.info.sptxtyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.turbsym), 
			    &(elnew->elem.spt.info.turbsym) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.itxfn), 
			    &(elnew->elem.spt.info.itxfn) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.ithw), 
			    &(elnew->elem.spt.info.ithw) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.iwidth), 
			    &(elnew->elem.spt.info.iwidth) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.txtcol), 
			    &(elnew->elem.spt.info.txtcol) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.lincol), 
			    &(elnew->elem.spt.info.lincol) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.filcol), 
			    &(elnew->elem.spt.info.filcol) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.ialign), 
			    &(elnew->elem.spt.info.ialign) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.lat), 
			    &(elnew->elem.spt.info.lat) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.lon), 
			    &(elnew->elem.spt.info.lon) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.offset_x), 
			    &(elnew->elem.spt.info.offset_x) );
	    ier += mv_swp4( &nswp, &(elold.elem.spt.info.offset_y), 
			    &(elnew->elem.spt.info.offset_y) );
	    break;

	  case CIRCLE_ELM:	/* Circle Type	*/

	    ier += mv_swp4( &nswp, &(elold.elem.cir.info.numpts), 
			    &(elnew->elem.cir.info.numpts) );
	    ier += mv_swp4( &nswp, &(elold.elem.cir.info.lintyp), 
			    &(elnew->elem.cir.info.lintyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.cir.info.lthw), 
			    &(elnew->elem.cir.info.lthw) );
	    ier += mv_swp4( &nswp, &(elold.elem.cir.info.width), 
			    &(elnew->elem.cir.info.width) );
	    ier += mv_swp4( &nswp, &(elold.elem.cir.info.lwhw), 
			    &(elnew->elem.cir.info.lwhw) );	

	    npts = 4; 
	    ier += mv_swp4( &npts, (elold.elem.cir.data.latlon), 
			    (elnew->elem.cir.data.latlon) );
	    break;

	  case TRKSTORM_ELM:	/* Track Type	*/

	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.subtype), 
			    &(elnew->elem.trk.info.subtype) );
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.npts),
			    &(elnew->elem.trk.info.npts) );		
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.nipts), 
			    &(elnew->elem.trk.info.nipts) );		
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.ltype1), 
			    &(elnew->elem.trk.info.ltype1) );
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.ltype2), 
			    &(elnew->elem.trk.info.ltype2) );
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.mtype1), 
			    &(elnew->elem.trk.info.mtype1) );
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.mtype2), 
			    &(elnew->elem.trk.info.mtype2) );
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.width), 
			    &(elnew->elem.trk.info.width) );		
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.speed), 
			    &(elnew->elem.trk.info.speed) );		
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.dir), 
			    &(elnew->elem.trk.info.dir) );		
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.incr), 
			    &(elnew->elem.trk.info.incr) );
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.skip),
                            &(elnew->elem.trk.info.skip) );
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.itxfn),
                            &(elnew->elem.trk.info.itxfn) );
	    ier += mv_swp4( &nswp, &(elold.elem.trk.info.ithw),
                            &(elnew->elem.trk.info.ithw) );

	    if (readflg == G_FALSE){
		npts = 2 * (elold.elem.trk.info.npts);  
	    }
	    else {
		npts = 2 * (elnew->elem.trk.info.npts);
	    }
	    ier += mv_swp4( &npts, (elold.elem.trk.latlon), 
			    (elnew->elem.trk.latlon) );	
	    break;

	  case SIGAIRM_ELM:
	  case SIGCONV_ELM:
	  case SIGINTL_ELM:
	  case SIGNCON_ELM:
	  case SIGOUTL_ELM:	/* Sigmet Type	*/
	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.subtype), 
			    &(elnew->elem.sig.info.subtype) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.npts), 
			    &(elnew->elem.sig.info.npts) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.lintyp), 
			    &(elnew->elem.sig.info.lintyp) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.linwid), 
			    &(elnew->elem.sig.info.linwid) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.sol), 
			    &(elnew->elem.sig.info.sol) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.status), 
			    &(elnew->elem.sig.info.status) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.distance), 
			    &(elnew->elem.sig.info.distance) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.seqnum), 
			    &(elnew->elem.sig.info.seqnum) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.sonic), 
			    &(elnew->elem.sig.info.sonic) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.pres), 
			    &(elnew->elem.sig.info.pres) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.maxwind), 
			    &(elnew->elem.sig.info.maxwind) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.obsfcst), 
			    &(elnew->elem.sig.info.obsfcst) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.fl), 
			    &(elnew->elem.sig.info.fl) );	
	    ier += mv_swp4( &nswp, &(elold.elem.sig.info.spd), 
			    &(elnew->elem.sig.info.spd) );	

	    if (readflg == G_FALSE) {
		npts = 2 * (elold.elem.sig.info.npts); 
	    }
	    else {
		npts = 2 * (elnew->elem.sig.info.npts);
	    }
	    ier += mv_swp4( &npts, (elold.elem.sig.latlon), 
			    (elnew->elem.sig.latlon) );
	    break;

	  case SIGCCF_ELM:	/* CCF Type	*/

	    ier += mv_swp4 (&nswp, &(elold.elem.ccf.info.subtype), 
			    &(elnew->elem.ccf.info.subtype));	
	    ier += mv_swp4 (&nswp, &(elold.elem.ccf.info.npts), 
			    &(elnew->elem.ccf.info.npts));	
	    ier += mv_swp4 (&nswp, &(elold.elem.ccf.info.cover), 
			    &(elnew->elem.ccf.info.cover));	
	    ier += mv_swp4 (&nswp, &(elold.elem.ccf.info.tops), 
			    &(elnew->elem.ccf.info.tops));	
	    ier += mv_swp4 (&nswp, &(elold.elem.ccf.info.prob), 
			    &(elnew->elem.ccf.info.prob));	
	    ier += mv_swp4 (&nswp, &(elold.elem.ccf.info.growth), 
			    &(elnew->elem.ccf.info.growth));	
	    ier += mv_swp4 (&nswp, &(elold.elem.ccf.info.spd), 
			    &(elnew->elem.ccf.info.spd));	
	    ier += mv_swp4 (&nswp, &(elold.elem.ccf.info.dir), 
			    &(elnew->elem.ccf.info.dir));	

	    if (readflg == G_FALSE) {
		npts = 2 * (elold.elem.ccf.info.npts);
	    }
	    else {
		npts = 2 * (elnew->elem.ccf.info.npts);
	    }
	    ier += mv_swp4 (&npts, (elold.elem.ccf.latlon), 
			    (elnew->elem.ccf.latlon));
	    break;

	  case WSM_ELM:	/* WATCH STATUS MESSAGE	*/

	    ier += mv_swp4( &nswp, &(elold.elem.wsm.info.numpts), 
			    &(elnew->elem.wsm.info.numpts) );
	    ier += mv_swp4( &nswp, &(elold.elem.wsm.info.spltyp), 
			    &(elnew->elem.wsm.info.spltyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.wsm.info.splstr), 
			    &(elnew->elem.wsm.info.splstr) );
	    ier += mv_swp4( &nswp, &(elold.elem.wsm.info.spldir), 
			    &(elnew->elem.wsm.info.spldir) );
	    ier += mv_swp4( &nswp, &(elold.elem.wsm.info.splsiz), 
			    &(elnew->elem.wsm.info.splsiz) );
	    ier += mv_swp4( &nswp, &(elold.elem.wsm.info.splwid), 
			    &(elnew->elem.wsm.info.splwid) );

	    if (readflg == G_FALSE) {
		npts = 2 * (elold.elem.wsm.info.numpts);
	    }
	    else {
		npts = 2 * (elnew->elem.wsm.info.numpts);
	    }
	    ier += mv_swp4( &npts, (elold.elem.wsm.latlon), 
			    (elnew->elem.wsm.latlon) );
	    break;
	  
	  case LIST_ELM:	/* LIST	*/

	    ier += mv_swp4( &nswp, &(elold.elem.lst.info.subtyp), 
			    &(elnew->elem.lst.info.subtyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.lst.info.mrktyp), 
			    &(elnew->elem.lst.info.mrktyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.lst.info.mrksiz), 
			    &(elnew->elem.lst.info.mrksiz) );
	    ier += mv_swp4( &nswp, &(elold.elem.lst.info.mrkwid), 
			    &(elnew->elem.lst.info.mrkwid) );

	    ier += mv_swp4( &nswp, &(elold.elem.lst.data.nitems), 
			    &(elnew->elem.lst.data.nitems) );

	    npts = MAXLISTITEMS;
	    
	    ier += mv_swp4( &npts, (elold.elem.lst.data.lat), 
			    (elnew->elem.lst.data.lat) );
	    ier += mv_swp4( &npts, (elold.elem.lst.data.lon), 
			    (elnew->elem.lst.data.lon) );
	    break;

	  case JET_ELM:	/* JET	*/

	    /*
	     *  Jet line
	     */
	    ier += mv_swp4( &nswp, &(elold.elem.jet.line.splcol), 
			    &(elnew->elem.jet.line.splcol) );
	    ier += mv_swp4( &nswp, &(elold.elem.jet.line.spl.info.numpts), 
			    &(elnew->elem.jet.line.spl.info.numpts) );
	    ier += mv_swp4( &nswp, &(elold.elem.jet.line.spl.info.spltyp), 
			    &(elnew->elem.jet.line.spl.info.spltyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.jet.line.spl.info.splstr), 
			    &(elnew->elem.jet.line.spl.info.splstr) );
	    ier += mv_swp4( &nswp, &(elold.elem.jet.line.spl.info.spldir), 
			    &(elnew->elem.jet.line.spl.info.spldir) );
	    ier += mv_swp4( &nswp, &(elold.elem.jet.line.spl.info.splsiz), 
			    &(elnew->elem.jet.line.spl.info.splsiz) );
	    ier += mv_swp4( &nswp, &(elold.elem.jet.line.spl.info.splwid), 
			    &(elnew->elem.jet.line.spl.info.splwid) );

	    if (readflg == G_FALSE) {
		npts = 2 * (elold.elem.jet.line.spl.info.numpts);
	    }
	    else {
		npts = 2 *(elnew->elem.jet.line.spl.info.numpts);
	    }

	    ier += mv_swp4( &npts, (elold.elem.jet.line.spl.latlon), 
			    (elnew->elem.jet.line.spl.latlon) );

	    
	    /*
	     *  Jet barbs
	     */
	    ier += mv_swp4( &nswp, &(elold.elem.jet.nbarb), 
			    &(elnew->elem.jet.nbarb) );

	    if (readflg == G_FALSE) {
		npts = elold.elem.jet.nbarb;
	    }
	    else {
		npts = elnew->elem.jet.nbarb;
	    }
	    
	    for ( ii = 0; ii < npts; ii++ ) {
	    
	        ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].wndcol), 
			    &(elnew->elem.jet.barb[ii].wndcol) );
	        ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].wnd.info.numwnd), 
			    &(elnew->elem.jet.barb[ii].wnd.info.numwnd) );
	        ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].wnd.info.width), 
			    &(elnew->elem.jet.barb[ii].wnd.info.width) );
	        ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].wnd.info.size), 
			    &(elnew->elem.jet.barb[ii].wnd.info.size) );
	        ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].wnd.info.wndtyp), 
			    &(elnew->elem.jet.barb[ii].wnd.info.wndtyp) );
	        ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].wnd.info.hdsiz), 
			    &(elnew->elem.jet.barb[ii].wnd.info.hdsiz) );

	        if (readflg == G_FALSE) {
		    npts2 = 2 * (elold.elem.jet.barb[ii].wnd.info.numwnd);
	        }
	        else {
		    npts2 = 2 * (elnew->elem.jet.barb[ii].wnd.info.numwnd);
	        }

	        ier += mv_swp4( &npts2, (elold.elem.jet.barb[ii].wnd.data.spddir), 
			    (elnew->elem.jet.barb[ii].wnd.data.spddir) );
	        ier += mv_swp4( &npts2, (elold.elem.jet.barb[ii].wnd.data.latlon), 
			    (elnew->elem.jet.barb[ii].wnd.data.latlon) );

	         /*
	          *  Jet texts
	          */	    
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].sptcol), 
			    &(elnew->elem.jet.barb[ii].sptcol) );	
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.rotn), 
			    &(elnew->elem.jet.barb[ii].spt.info.rotn) );	
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.sztext), 
			    &(elnew->elem.jet.barb[ii].spt.info.sztext) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.sptxtyp), 
			    &(elnew->elem.jet.barb[ii].spt.info.sptxtyp) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.turbsym), 
			    &(elnew->elem.jet.barb[ii].spt.info.turbsym) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.itxfn), 
			    &(elnew->elem.jet.barb[ii].spt.info.itxfn) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.ithw), 
			    &(elnew->elem.jet.barb[ii].spt.info.ithw) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.iwidth), 
			    &(elnew->elem.jet.barb[ii].spt.info.iwidth) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.txtcol), 
			    &(elnew->elem.jet.barb[ii].spt.info.txtcol) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.lincol), 
			    &(elnew->elem.jet.barb[ii].spt.info.lincol) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.filcol), 
			    &(elnew->elem.jet.barb[ii].spt.info.filcol) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.ialign), 
			    &(elnew->elem.jet.barb[ii].spt.info.ialign) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.lat), 
			    &(elnew->elem.jet.barb[ii].spt.info.lat) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.lon), 
			    &(elnew->elem.jet.barb[ii].spt.info.lon) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.offset_x), 
			    &(elnew->elem.jet.barb[ii].spt.info.offset_x) );
	         ier += mv_swp4( &nswp, &(elold.elem.jet.barb[ii].spt.info.offset_y), 
			    &(elnew->elem.jet.barb[ii].spt.info.offset_y) );

	    }
	    
	    
	    /*
	     *  Jet hashs
	     */
	    ier += mv_swp4( &nswp, &(elold.elem.jet.nhash), 
			    &(elnew->elem.jet.nhash) );

	    if (readflg == G_FALSE) {
		npts = elold.elem.jet.nhash;
	    }
	    else {
		npts = elnew->elem.jet.nhash;
	    }
	    
	    for ( ii = 0; ii < npts; ii++ ) {
	    
	        ier += mv_swp4( &nswp, &(elold.elem.jet.hash[ii].wndcol), 
			    &(elnew->elem.jet.hash[ii].wndcol) );
	        ier += mv_swp4( &nswp, &(elold.elem.jet.hash[ii].wnd.info.numwnd), 
			    &(elnew->elem.jet.hash[ii].wnd.info.numwnd) );
	        ier += mv_swp4( &nswp, &(elold.elem.jet.hash[ii].wnd.info.width), 
			    &(elnew->elem.jet.hash[ii].wnd.info.width) );
	        ier += mv_swp4( &nswp, &(elold.elem.jet.hash[ii].wnd.info.size), 
			    &(elnew->elem.jet.hash[ii].wnd.info.size) );
	        ier += mv_swp4( &nswp, &(elold.elem.jet.hash[ii].wnd.info.wndtyp), 
			    &(elnew->elem.jet.hash[ii].wnd.info.wndtyp) );
	        ier += mv_swp4( &nswp, &(elold.elem.jet.hash[ii].wnd.info.hdsiz), 
			    &(elnew->elem.jet.hash[ii].wnd.info.hdsiz) );

	        if (readflg == G_FALSE) {
		    npts2 = 2 * (elold.elem.jet.hash[ii].wnd.info.numwnd);
	        }
	        else {
		    npts2 = 2 * (elnew->elem.jet.hash[ii].wnd.info.numwnd);
	        }

	        ier += mv_swp4( &npts2, (elold.elem.jet.hash[ii].wnd.data.spddir), 
			    (elnew->elem.jet.hash[ii].wnd.data.spddir) );
	        ier += mv_swp4( &npts2, (elold.elem.jet.hash[ii].wnd.data.latlon), 
			    (elnew->elem.jet.hash[ii].wnd.data.latlon) );
	    }	    
	    
	    
	    break;

	  case GFA_ELM:	/* GFA Type */
	
	    ier += mv_swp4( &nswp, &(elold.elem.gfa.info.nblocks),
			    &(elnew->elem.gfa.info.nblocks) );
	    
	    ier += mv_swp4( &nswp, &(elold.elem.gfa.info.npts),
			    &(elnew->elem.gfa.info.npts) );
	    
	    /*
	      *  Swap latlon portion.
	      */
	    if (readflg == G_FALSE) {
                npts = 2 * (elold.elem.gfa.info.npts);
	    }
	    else {
                npts = 2 * (elnew->elem.gfa.info.npts);
	    }
	    ier += mv_swp4( &npts, (elold.elem.gfa.latlon), 
			    (elnew->elem.gfa.latlon) );
	    break;
	    
          case SGWX_ELM:    /* SIGWX Type */
            ier += mv_swp4 (&nswp, &(elold.elem.sgwx.info.subtype),
                            &(elnew->elem.sgwx.info.subtype));
            ier += mv_swp4 (&nswp, &(elold.elem.sgwx.info.npts),
                            &(elnew->elem.sgwx.info.npts));
            if (readflg == G_FALSE) {
              npts = 2 * (elold.elem.sgwx.info.npts);
            } else {
              npts = 2 * (elnew->elem.sgwx.info.npts);
            }
            ier += mv_swp4 (&npts, (elold.elem.sgwx.latlon),
                            &(elnew->elem.sgwx.latlon));
            break;	    

          case TCERR_ELM: /* TCE Type */
                                                                                                               
            ier += mv_swp4( &nswp, &(elold.elem.tce.cone.lincol),
                            &(elnew->elem.tce.cone.lincol) );
                                                                                                               
            ier += mv_swp4( &nswp, &(elold.elem.tce.cone.lintyp),
                            &(elnew->elem.tce.cone.lintyp) );
                                                                                                               
            ier += mv_swp4( &nswp, &(elold.elem.tce.cone.filcol),
                            &(elnew->elem.tce.cone.filcol) );
                                                                                                               
            ier += mv_swp4( &nswp, &(elold.elem.tce.cone.filtyp),
                            &(elnew->elem.tce.cone.filtyp) );
                                                                                                               
            ier += mv_swp4( &nswp, &(elold.elem.tce.cone.npts),
                            &(elnew->elem.tce.cone.npts) );
                                                                                                               
            /*
             *  Swap latlon portion.
             */
            if (readflg == G_FALSE) {
                npts = 2 * (elold.elem.tce.cone.npts);
            }
            else {
                npts = 2 * (elnew->elem.tce.cone.npts);
            }
            ier += mv_swp4( &npts, (elold.elem.tce.cone.latlon),
                            (elnew->elem.tce.cone.latlon) );
            break;

          case TCBKL_ELM: /* TCB Type */
                                                                                                               
            ier += mv_swp4( &nswp, &(elold.elem.tcb.lincol),
                            &(elnew->elem.tcb.lincol) );
                                                                                                               
            ier += mv_swp4( &nswp, &(elold.elem.tcb.linwid),
                            &(elnew->elem.tcb.linwid) );
                                                                                                               
            ier += mv_swp4( &nswp, &(elold.elem.tcb.tcww),
                            &(elnew->elem.tcb.tcww) );
                                                                                                               
            ier += mv_swp4( &nswp, &(elold.elem.tcb.numBkPts),
                            &(elnew->elem.tcb.numBkPts) );
                                                                                                               
            /*
             *  Swap latlon portion.
             */
            if (readflg == G_FALSE) {
                npts = elold.elem.tcb.numBkPts;
            }
            else {
                npts = elnew->elem.tcb.numBkPts;
            }

	    for ( ii = 0; ii < npts; ii++ ) {
	    
	        ier += mv_swp4( &nswp, &(elold.elem.tcb.bkPntLn[ii].lat), 
			    &(elnew->elem.tcb.bkPntLn[ii].lat) );
	        ier += mv_swp4( &nswp, &(elold.elem.tcb.bkPntLn[ii].lon), 
			    &(elnew->elem.tcb.bkPntLn[ii].lon) );

            }

            break;

          case TCTRK_ELM: /* TCT Type */
                                                                                  
            ier += mv_swp4( &nswp, &(elold.elem.tct.lincol),
                            &(elnew->elem.tct.lincol) );
                                                                        
            ier += mv_swp4( &nswp, &(elold.elem.tct.lintyp),
                            &(elnew->elem.tct.lintyp) );
                                                                                   
            ier += mv_swp4( &nswp, &(elold.elem.tct.numTrackPts),
                            &(elnew->elem.tct.numTrackPts) );
                                                                          
            /*
             *  No Swapping of latlon - they are written out as strings
             */

            break;

	  case VOLC_ELM:    /* Volcano Elm */

	    ier += mv_swp4( &nswp, &(elold.elem.vol.info.code), 
			    &(elnew->elem.vol.info.code) );
	    ier += mv_swp4( &nswp, &(elold.elem.vol.info.size), 
			    &(elnew->elem.vol.info.size) );
	    ier += mv_swp4( &nswp, &(elold.elem.vol.info.width), 
			    &(elnew->elem.vol.info.width) );

	    npts = 2;	    
	    ier += mv_swp4( &npts, (elold.elem.vol.latlon), 
			    (elnew->elem.vol.latlon) );

	    npts = 2;	    
	    ier += mv_swp4( &npts, (elold.elem.vol.offset_xy), 
			    (elnew->elem.vol.offset_xy) );

	    break;

	  case ASHCLD_ELM:    /* Ash Cloud Elm */

	    ier += mv_swp4( &nswp, &(elold.elem.ash.info.subtype), 
			    &(elnew->elem.ash.info.subtype) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.info.npts), 
			    &(elnew->elem.ash.info.npts) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.info.distance), 
			    &(elnew->elem.ash.info.distance) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.info.fhr), 
			    &(elnew->elem.ash.info.fhr) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.info.lintyp), 
			    &(elnew->elem.ash.info.lintyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.info.linwid), 
			    &(elnew->elem.ash.info.linwid) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.info.sol), 
			    &(elnew->elem.ash.info.sol) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.info.spd), 
			    &(elnew->elem.ash.info.spd) );

	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.rotn), 
			    &(elnew->elem.ash.spt.info.rotn) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.sztext), 
			    &(elnew->elem.ash.spt.info.sztext) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.sptxtyp), 
			    &(elnew->elem.ash.spt.info.sptxtyp) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.turbsym), 
			    &(elnew->elem.ash.spt.info.turbsym) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.itxfn), 
			    &(elnew->elem.ash.spt.info.itxfn) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.ithw), 
			    &(elnew->elem.ash.spt.info.ithw) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.iwidth), 
			    &(elnew->elem.ash.spt.info.iwidth) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.txtcol), 
			    &(elnew->elem.ash.spt.info.txtcol) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.lincol), 
			    &(elnew->elem.ash.spt.info.lincol) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.filcol), 
			    &(elnew->elem.ash.spt.info.filcol) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.ialign), 
			    &(elnew->elem.ash.spt.info.ialign) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.lat), 
			    &(elnew->elem.ash.spt.info.lat) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.lon), 
			    &(elnew->elem.ash.spt.info.lon) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.offset_x), 
			    &(elnew->elem.ash.spt.info.offset_x) );
	    ier += mv_swp4( &nswp, &(elold.elem.ash.spt.info.offset_y), 
			    &(elnew->elem.ash.spt.info.offset_y) );

	    if (readflg == G_FALSE) {
		npts = 2 * (elold.elem.ash.info.npts);
	    }
	    else {
		npts = 2 * (elnew->elem.ash.info.npts);
	    }

	    ier += mv_swp4( &npts, (elold.elem.ash.latlon), 
			    (elnew->elem.ash.latlon) );

	    break;
	    
        } /* End of Switch */    
    }
    
    *iret = ier;
}


