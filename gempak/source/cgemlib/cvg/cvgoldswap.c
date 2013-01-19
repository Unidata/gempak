#include "cvgcmn.h"
#include "drwids.h"
#include "pgprm.h"
#include "cvgoldstructs.h"

/************************************************************************
 * cvgoldswap.c                                                         *
 *                                                                      *
 * This module contains functions for swapping byte order of old        *
 * version VG elements, including version 0 WATCH BOX & SIGMET, 	*
 * version 1 and version 2 WATCH BOX.                                 	*
 *                                                                      *
 * CONTENTS:                                                            *
 * cvg_swap_v0()	Swap byte order of verion 0 WATCH BOX & SIGMET  *
 * cvg_swap_v1()	Swap byte order of verion 1 WATCH BOX record    *
 * cvg_swap_v2()	Swap byte order of verion 2 WATCH BOX record    *
 * cvg_swap_v3()	Swap byte order of verion 3 WATCH BOX record    *
 * cvg_swap_v4()	Swap byte order of verion 4 WATCH BOX record    *
 * cvg_swap_v5()	Swap byte order of verion 5 WATCH BOX record    *
 **									*
 * Log:									*
 * J. Wu/GSC		02/01	Created				        *
 * H. Zeng/XTRIA        01/03   Added WatchBox version 5                *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/


/*=====================================================================*/

void cvg_swap_v0 ( int flag, int readflg, v0_VG_DBStruct elold, 
					v0_VG_DBStruct *elnew, int *iret )
/************************************************************************
 * cvg_swap_v0								*
 *									*
 * This function swaps the byte order of a version 0 SIGMET and WATCH   *
 * BOX record structure.						*
 *									*
 * cvg_swap_v0 ( flag, readflg, elold, elnew, iret )			*
 *									*
 * Input parameters:							*
 *	flag		int		which to swap			*
 *	readflg		int		read or write			*
 *	elold		v0_VG_DBStruct	Version 0 VG record structure	*
 *									*
 * Output parameters:							*
 *	*elnew		v0_VG_DBStruct	Pointer to swapped VG structure	*
 *	*iret		int		Return code			*
 *					 0  = OK			*
 *					 >0 = # of invalid swap		*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		02/01	Copied from cvgswap.c	                *
 ***********************************************************************/
{    
    int	ier, nswp, npts;
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

    
    /* 
     * Swap byte order for int and float for all element types 
     */
    if ( flag == SWPALL || flag == SWPINF ) {
	
	switch (elold.hdr.vg_type) {

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
			    
	    if (readflg == G_FALSE) {
		npts = 2 * (elold.elem.wbx.info.numpts);
	    }
	    else {
		npts = 2 * (elnew->elem.wbx.info.numpts);
	    }
	    ier += mv_swp4( &npts, (elold.elem.wbx.latlon), 
			    (elnew->elem.wbx.latlon) );	
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

        } /* End of Switch */
	
    }

    *iret = ier;
}

/*=====================================================================*/
void cvg_swap_v1 ( int flag, int readflg, v1_VG_DBStruct elold, 
					v1_VG_DBStruct *elnew, int *iret )
/************************************************************************
 * cvg_swap_v1								*
 *									*
 * This function swaps the byte order of version 1 WATCH BOX record 	*
 *									*
 * cvg_swap_v1 ( flag, readflg, elold, elnew, iret )			*
 *									*
 * Input parameters:							*
 *	flag		int		which to swap			*
 *	readflg		int		read or write			*
 *	elold		v1_VG_DBStruct	Version 1 VG record structure	*
 *									*
 * Output parameters:							*
 *	*elnew		v1_VG_DBStruct	Pointer to swapped VG structure	*
 *	*iret		int		Return code			*
 *					 0  = OK			*
 *					 >0 = # of invalid swap		*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		02/01	Copied from cvgswap.c		        *
 ***********************************************************************/
{
    int	ier, nswp, npts;
/*---------------------------------------------------------------------*/    

    ier   = 0;
    nswp  = 1;

    
    /* 
     *  Swap byte order for int and float in the header sub-structure  
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

    
    /* 
     *  Swap byte order for int and float for all element types 
     */
    if ( flag == SWPALL || flag == SWPINF ) {
	
	switch (elold.hdr.vg_type) {
	  
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
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_istat), 
			    &(elnew->elem.wbx.info.w_istat) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_severity), 
			    &(elnew->elem.wbx.info.w_severity) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_issued), 
			    &(elnew->elem.wbx.info.w_issued) );

	    
	    if (readflg == G_FALSE) {
		npts = 2 * (elold.elem.wbx.info.numpts);
	    }
	    else {
		npts = 2 * (elnew->elem.wbx.info.numpts);
	    }
	    ier += mv_swp4( &npts, (elold.elem.wbx.latlon), 
			    (elnew->elem.wbx.latlon) );	
	    break;

        } /* End of Switch */
	
    }

    *iret = ier;
}


/*=====================================================================*/

void cvg_swap_v2 ( int flag, int readflg, v2_VG_DBStruct elold, 
					v2_VG_DBStruct *elnew, int *iret )
/************************************************************************
 * cvg_swap_v2								*
 *									*
 * This function swaps the byte order of a version 2 WATCH BOX record   *
 *									*
 * cvg_swap_v2 ( flag, readflg, elold, elnew, iret )			*
 *									*
 * Input parameters:							*
 *	flag		int		which to swap			*
 *	readflg		int		read or write			*
 *	elold		v2_VG_DBStruct	Version 2 VG record structure	*
 *									*
 * Output parameters:							*
 *	*elnew		v2_VG_DBStruct	Pointer to swapped VG structure	*
 *	*iret		int		Return code			*
 *					 0  = OK			*
 *					 >0 = # of invalid swap		*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		02/01	Copied from cvgswap.c		        *
 ***********************************************************************/
{
    int	ier, nswp, npts, npts2;
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

    /* 
     *  Swap byte order for int and float for all element types 
     */
    if ( flag == SWPALL || flag == SWPINF ) {
	switch (elold.hdr.vg_type) {

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
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_istat), 
			    &(elnew->elem.wbx.info.w_istat) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_severity), 
			    &(elnew->elem.wbx.info.w_severity) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_issued), 
			    &(elnew->elem.wbx.info.w_issued) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.numcnty), 
			    &(elnew->elem.wbx.info.numcnty) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.cn_flag), 
			    &(elnew->elem.wbx.info.cn_flag) );
	    
	    
	    if (readflg == G_FALSE) {
		npts  = elold.elem.wbx.info.numcnty;
		npts2 = 2 * (elold.elem.wbx.info.numcnty);
	    }
	    else {
		npts  = elnew->elem.wbx.info.numcnty;
		npts2 = 2 * (elnew->elem.wbx.info.numcnty);
	    }
	    ier += mv_swp4( &npts, (elold.elem.wbx.info.cn_stat), 
			    (elnew->elem.wbx.info.cn_stat) );
	    ier += mv_swp4( &npts2, (elold.elem.wbx.info.cn_ltln), 
			    (elnew->elem.wbx.info.cn_ltln) );

	    
	    if (readflg == G_FALSE) {
		npts2 = 2 * (elold.elem.wbx.info.numpts);
	    }
	    else {
		npts2 = 2 * (elnew->elem.wbx.info.numpts);
	    }
	    ier += mv_swp4( &npts2, (elold.elem.wbx.latlon), 
			    (elnew->elem.wbx.latlon) );	
	    break;

        } /* End of Switch */
	
    }

    *iret = ier;
}

/*=====================================================================*/

void cvg_swap_v3 ( int flag, int readflg, v3_VG_DBStruct elold, 
					v3_VG_DBStruct *elnew, int *iret )
/************************************************************************
 * cvg_swap_v3								*
 *									*
 * This function swaps the byte order of a version 3 WATCH BOX record   *
 *									*
 * cvg_swap_v3 ( flag, readflg, elold, elnew, iret )			*
 *									*
 * Input parameters:							*
 *	flag		int		which to swap			*
 *	readflg		int		read or write			*
 *	elold		v3_VG_DBStruct	Version 3 VG record structure	*
 *									*
 * Output parameters:							*
 *	*elnew		v3_VG_DBStruct	Pointer to swapped VG structure	*
 *	*iret		int		Return code			*
 *					 0  = OK			*
 *					 >0 = # of invalid swap		*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 8/01	Created from cvg_swap_v2		*
 ***********************************************************************/
{
    int	ier, nswp, npts, npts2;
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

    /* 
     *  Swap byte order for int and float for all element types 
     */
    if ( flag == SWPALL || flag == SWPINF ) {
	switch (elold.hdr.vg_type) {

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

	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_istat), 
			    &(elnew->elem.wbx.info.w_istat) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_severity), 
			    &(elnew->elem.wbx.info.w_severity) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.w_issued), 
			    &(elnew->elem.wbx.info.w_issued) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.numcnty), 
			    &(elnew->elem.wbx.info.numcnty) );
	    ier += mv_swp4( &nswp, &(elold.elem.wbx.info.cn_flag), 
			    &(elnew->elem.wbx.info.cn_flag) );
	    
	    
	    if (readflg == G_FALSE) {
		npts  = elold.elem.wbx.info.numcnty;
		npts2 = 2 * (elold.elem.wbx.info.numcnty);
	    }
	    else {
		npts  = elnew->elem.wbx.info.numcnty;
		npts2 = 2 * (elnew->elem.wbx.info.numcnty);
	    }
	    ier += mv_swp4( &npts, (elold.elem.wbx.info.cn_stat), 
			    (elnew->elem.wbx.info.cn_stat) );
	    ier += mv_swp4( &npts2, (elold.elem.wbx.info.cn_ltln), 
			    (elnew->elem.wbx.info.cn_ltln) );

	    
	    if (readflg == G_FALSE) {
		npts2 = 2 * (elold.elem.wbx.info.numpts);
	    }
	    else {
		npts2 = 2 * (elnew->elem.wbx.info.numpts);
	    }
	    ier += mv_swp4( &npts2, (elold.elem.wbx.latlon), 
			    (elnew->elem.wbx.latlon) );	
	    break;

        } /* End of Switch */
	
    }

    *iret = ier;
}

/*=====================================================================*/

void cvg_swap_v4 ( int flag, int readflg, v4_VG_DBStruct elold, 
					v4_VG_DBStruct *elnew, int *iret )
/************************************************************************
 * cvg_swap_v4								*
 *									*
 * This function swaps the byte order of a version 4 WATCH BOX record   *
 *									*
 * cvg_swap_v4 ( flag, readflg, elold, elnew, iret )			*
 *									*
 * Input parameters:							*
 *	flag		int		which to swap			*
 *	readflg		int		read or write			*
 *	elold		v4_VG_DBStruct	Version 4 VG record structure	*
 *									*
 * Output parameters:							*
 *	*elnew		v4_VG_DBStruct	Pointer to swapped VG structure	*
 *	*iret		int		Return code			*
 *					 0  = OK			*
 *					 >0 = # of invalid swap		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	06/02	Created from cvg_swap()			*
 ***********************************************************************/
{
    int	ier, nswp, npts, npts2;
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

    /* 
     *  Swap byte order for int and float for all element types 
     */
    if ( flag == SWPALL || flag == SWPINF ) {
	switch (elold.hdr.vg_type) {

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
	    ier += mv_swp4( &npts, (elold.elem.wbx.info.cn_stat), 
			    (elnew->elem.wbx.info.cn_stat) );
 	    ier += mv_swp4( &npts, (elold.elem.wbx.info.cn_fips), 
			    (elnew->elem.wbx.info.cn_fips) );
	    ier += mv_swp4( &npts2, (elold.elem.wbx.info.cn_ltln), 
			    (elnew->elem.wbx.info.cn_ltln) );

	    if (readflg == G_FALSE) {
		npts2 = 2 * (elold.elem.wbx.info.numpts);
	    }
	    else {
		npts2 = 2 * (elnew->elem.wbx.info.numpts);
	    }
	    ier += mv_swp4( &npts2, (elold.elem.wbx.latlon), 
			    (elnew->elem.wbx.latlon) );	

	    break;

        } /* End of Switch */
	
    }

    *iret = ier;
}

/*=====================================================================*/

void cvg_swap_v5 ( int flag, int readflg, v5_VG_DBStruct elold, 
					v5_VG_DBStruct *elnew, int *iret )
/************************************************************************
 * cvg_swap_v5								*
 *									*
 * This function swaps the byte order of a version 5 WATCH BOX record   *
 *									*
 * cvg_swap_v5 ( flag, readflg, elold, elnew, iret )			*
 *									*
 * Input parameters:							*
 *	flag		int		which to swap			*
 *	readflg		int		read or write			*
 *	elold		v5_VG_DBStruct	Version 5 VG record structure	*
 *									*
 * Output parameters:							*
 *	*elnew		v5_VG_DBStruct	Pointer to swapped VG structure	*
 *	*iret		int		Return code			*
 *					 0  = OK			*
 *					 >0 = # of invalid swap		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA        01/03   initial coding                          *
 ***********************************************************************/
{
    int	ier, nswp, npts, npts2;
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

    /* 
     *  Swap byte order for int and float for all element types 
     */
    if ( flag == SWPALL || flag == SWPINF ) {
	switch (elold.hdr.vg_type) {

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

        } /* End of Switch */
	
    }

    *iret = ier;
}

/*=====================================================================*/
