#include "cvgcmn.h"
#include "drwids.h"
#include "cvgoldstructs.h"

void cvg_v0_to_v1( v0_VG_DBStruct *v0_el, v1_VG_DBStruct *v1_el, int *iret );
void cvg_v1_to_v2( v1_VG_DBStruct *v1_el, v2_VG_DBStruct *v2_el, int *iret );
void cvg_v2_to_v3( v2_VG_DBStruct *v2_el, v3_VG_DBStruct *v3_el, int *iret );
void cvg_v3_to_v4( v3_VG_DBStruct *v3_el, v4_VG_DBStruct *v4_el, int *iret );
void cvg_v4_to_v5( v4_VG_DBStruct *v4_el, v5_VG_DBStruct *v5_el, int *iret );
void cvg_v5_to_v6( v5_VG_DBStruct *v5_el,    VG_DBStruct *v6_el, int *iret );
		   
/************************************************************************
 * cvgrdoldele.c							*
 *									*
 * This function readds old VGF element versions.			*
 *									*
 * CONTENTS:								*
 ***********************************************************************/
 
 /*=====================================================================*/

void cvg_rdoldele ( VG_DBStruct *el, int el_start, int el_size, 
						FILE *fp, int *iret )
/************************************************************************
 * cvg_rdoldele								*
 *									*
 * This function read an old version VG element from an open VG file.   *
 *									*
 * Note: 1. It is assumed that the *el contains a loaded header.	*
 *       2. Called only if VG element version is older than the latest. *
 *       3. The latest version # for different VG elements is different.*
 *									*
 * cvg_rdoldele ( el, el_start, el_size, fp, iret )			*
 *									*
 * Input parameters:							*
 *	*el             VG_DBStruct     Pointer to VG record structure  *
 *	el_start	int		File offset to start of element	*
 *	el_size		int		size of element to read in Bytes*
 *	*fp		FILE		Handle to open file		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -3 = seek to a bad location	*
 *					 -8 = no VG file is open	*
 *					-24 = no VG header loaded	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		02/01	Created                          	*
 * J. Wu/GSC		02/01	Modified 'unused2' to 'version' in VG   *
 * S. Jacobs/NCEP	 2/01	Added machine type MTLNUX		*
 * D.W.Plummer/NCEP	 8/01	Added new version for watch element	*
 * J. Wu/SAIC		06/02	add v4 to v5 conversion   		*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int 		ier, ierr, nbin, version;
    v0_VG_DBStruct	v0_el;
    v1_VG_DBStruct	v1_el;
    v2_VG_DBStruct	v2_el;
    v3_VG_DBStruct	v3_el;
    v4_VG_DBStruct	v4_el;
    v5_VG_DBStruct      v5_el;
    
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    version = (int)el->hdr.version;
    
    /* 
     *  If no VG header has been loaded or seek to a worng position,
     *  return with error.
     */    
    if ( fp == NULL )   *iret = -8;
    if ( el_size == 0 ) *iret = -24;
     
    cfl_seek( fp, (long)el_start, 0, &ier);
    if ( ier < 0 )  *iret = -3;  
    
    if ( *iret < 0 )    return;


    /* 
     *  Read the element into appropriate old VG version and swap
     *  it if necessary. 
     */    
    switch ( version ) {

        case 0: 
	    cfl_read( fp, el->hdr.recsz, (void *)(&v0_el), &nbin, &ier );
            
	    if ( MTMACH == MTULTX ||
	         MTMACH == MTALPH ||
		 MTMACH == MTLNUX ) {
 	        cvg_swap_v0( SWPALL, G_TRUE, v0_el, &v0_el, &ierr );  
	    }
	    
	    break;


        case 1: 
	    cfl_read( fp, el->hdr.recsz, (void *)(&v1_el), &nbin, &ier );            
	    
	    if ( MTMACH == MTULTX ||
	    	 MTMACH == MTALPH ||
	         MTMACH == MTLNUX ) {
 	        cvg_swap_v1( SWPALL, G_TRUE, v1_el, &v1_el, &ierr );  
	    }
	    
	    break;


        case 2: 
	    cfl_read( fp, el->hdr.recsz, (void *)(&v2_el), &nbin, &ier );

	    if ( MTMACH == MTULTX ||
	    	 MTMACH == MTALPH ||
	    	 MTMACH == MTLNUX ){
 	        cvg_swap_v2( SWPALL, G_TRUE, v2_el, &v2_el, &ierr );  
	    }
	    
	    break;
	            

        case 3: 
	    cfl_read( fp, el->hdr.recsz, (void *)(&v3_el), &nbin, &ier );

	    if ( MTMACH == MTULTX ||
	    	 MTMACH == MTALPH ||
	    	 MTMACH == MTLNUX ){
 	        cvg_swap_v3( SWPALL, G_TRUE, v3_el, &v3_el, &ierr );  
	    }
	    
	    break;

        case 4: 
	    cfl_read( fp, el->hdr.recsz, (void *)(&v4_el), &nbin, &ier );

	    if ( MTMACH == MTULTX ||
	    	 MTMACH == MTALPH ||
	    	 MTMACH == MTLNUX ){
	        cvg_swap_v4( SWPALL, G_TRUE, v4_el, &v4_el, &ierr );  
	    }
	    
	    break;

        case 5: 
	    cfl_read( fp, el->hdr.recsz, (void *)(&v5_el), &nbin, &ier );

	    if ( MTMACH == MTULTX ||
	    	 MTMACH == MTALPH ||
	    	 MTMACH == MTLNUX ){
	        cvg_swap_v5( SWPALL, G_TRUE, v5_el, &v5_el, &ierr );  
	    }
	    
	    break;
	            
    }
    

    /* 
     * Cascade the old version into the latest version. 
     */            
    switch ( version ) {

        case 0: 
	    cvg_v0_to_v1( &v0_el, &v1_el, &ier ); 

	case 1: 
	    cvg_v1_to_v2( &v1_el, &v2_el, &ier ); 

        case 2: 
	    cvg_v2_to_v3( &v2_el, &v3_el, &ier );         

        case 3: 
	    cvg_v3_to_v4( &v3_el, &v4_el, &ier );         

        case 4: 
           cvg_v4_to_v5( &v4_el, &v5_el, &ier );  
   
        case 5: 
	    cvg_v5_to_v6( &v5_el, el, &ier );         

	break;
	
    }
    	       		           
}

/*=====================================================================*/

void cvg_v0_to_v1 ( v0_VG_DBStruct *v0_el, v1_VG_DBStruct *v1_el, int *iret )
/************************************************************************
 * cvg_v0_to_v1								*
 *									*
 * This function converts a version 0 VG element into version 1 element *
 *									*
 * cvg_v0_to_v1 ( v0_el, v1_el, iret )			                *
 *									*
 * Input parameters:							*
 *	*v0_el		v0_VG_DBStruct	Pointer to version 0 VG element	*
 *									*
 * Output parameters:							*
 *	*v1_el		v1_VG_DBStruct	Pointer to version 1 VG element	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		02/01	Created                                 *
 * J. Wu/GSC		02/01	Modified 'unused2' to 'version' in VG   *
 ***********************************************************************/
{    
    int		ii;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    
    
    /*
     *  Pass on header information.
     */     
    v1_el->hdr = v0_el->hdr;


    /*
     *  Convert VG type-specific information and increase header size
     *  (recsz) and version number (version) if necessary.
     */            
    switch ( (int)v0_el->hdr.vg_type ) {

        case WBOX_ELM:        /* Watch Box Type */

	    v1_el->hdr.version = (char)( (int)v0_el->hdr.version + 1 );
	    v1_el->hdr.recsz = ( (sizeof(float) * 2 * v0_el->elem.wbx.info.numpts)
                 		+ sizeof(VG_HdrStruct) 
                 		+ sizeof(v1_WatchBoxInfo) );
	    v1_el->elem.wbx.info.numpts   = v0_el->elem.wbx.info.numpts;
	    v1_el->elem.wbx.info.w_style  = v0_el->elem.wbx.info.w_style;
	    v1_el->elem.wbx.info.w_shape  = v0_el->elem.wbx.info.w_shape;
	    v1_el->elem.wbx.info.w_number = v0_el->elem.wbx.info.w_number;
	    v1_el->elem.wbx.info.w_type   = v0_el->elem.wbx.info.w_type;
	    strcpy( v1_el->elem.wbx.info.w_file, v0_el->elem.wbx.info.w_file );

	    if ( v0_el->elem.wbx.info.w_type == 0 )  
	         v1_el->elem.wbx.info.w_type = UNDWTCH; 
	    for ( ii = 0; ii < v0_el->elem.wbx.info.numpts * 2; ii++ )  
		v1_el->elem.wbx.latlon[ii] = v0_el->elem.wbx.latlon[ii];


	    /* 
	     *  V1 Watch Box specific information goes here. 
	     */	    
	    v1_el->elem.wbx.info.w_istat = 0;
	    v1_el->elem.wbx.info.w_iss_t[0] = '\0';
	    v1_el->elem.wbx.info.w_exp_t[0] = '\0';
	    v1_el->elem.wbx.info.w_severity = 0;
	    v1_el->elem.wbx.info.w_timezone[0] = '\0';
	    v1_el->elem.wbx.info.w_hailsz[0] = '\0';
	    v1_el->elem.wbx.info.w_windg[0] = '\0';
	    v1_el->elem.wbx.info.w_tops[0] = '\0';
	    v1_el->elem.wbx.info.w_msmv_d[0] = '\0';
	    v1_el->elem.wbx.info.w_msmv_s[0] = '\0';
	    v1_el->elem.wbx.info.w_states[0] = '\0';
	    v1_el->elem.wbx.info.w_adjarea[0] = '\0';
	    v1_el->elem.wbx.info.w_replw[0] = '\0';
	    v1_el->elem.wbx.info.w_fcstr[0] = '\0';
	    v1_el->elem.wbx.info.w_issued = 0;
	    v1_el->elem.wbx.info.wsm_iss_t[0] = '\0';
	    v1_el->elem.wbx.info.wsm_exp_t[0] = '\0';
	    v1_el->elem.wbx.info.wsm_ref[0] = '\0';
	    v1_el->elem.wbx.info.wsm_from[0] = '\0';
	    v1_el->elem.wbx.info.wsm_meso[0] = '\0';
	    v1_el->elem.wbx.info.wsm_fcstr[0] = '\0';
	
	    break;
	    
	
	case SIGAIRM_ELM:
	case SIGCONV_ELM:
	case SIGINTL_ELM:
	case SIGNCON_ELM:
	case SIGOUTL_ELM:	/* Sigmet Type	*/
	
	    v1_el->hdr.version = (char)( (int)v0_el->hdr.version + 1 );
	    v1_el->hdr.recsz = ( (sizeof(float) * 2 * v0_el->elem.sig.info.npts)
                 		+ sizeof(VG_HdrStruct) 
                 		+ sizeof(SigmetInfo) );
	    v1_el->elem.sig.info.subtype  = v0_el->elem.sig.info.subtype;
	    v1_el->elem.sig.info.npts     = v0_el->elem.sig.info.npts;
	    v1_el->elem.sig.info.lintyp   = v0_el->elem.sig.info.lintyp;
	    v1_el->elem.sig.info.linwid   = v0_el->elem.sig.info.linwid;
	    v1_el->elem.sig.info.sol      = v0_el->elem.sig.info.sol;
	    strcpy(v1_el->elem.sig.info.area, v0_el->elem.sig.info.area);
	    strcpy(v1_el->elem.sig.info.fir, v0_el->elem.sig.info.fir);
	    v1_el->elem.sig.info.status    = v0_el->elem.sig.info.status;
	    v1_el->elem.sig.info.distance  = v0_el->elem.sig.info.distance;
	    strcpy(v1_el->elem.sig.info.msgid, v0_el->elem.sig.info.msgid);
	    v1_el->elem.sig.info.seqnum    = v0_el->elem.sig.info.seqnum;
	    strcpy(v1_el->elem.sig.info.stime, v0_el->elem.sig.info.stime);
	    strcpy(v1_el->elem.sig.info.etime, v0_el->elem.sig.info.etime);
	    strcpy(v1_el->elem.sig.info.remarks, v0_el->elem.sig.info.remarks);
	    v1_el->elem.sig.info.sonic     = v0_el->elem.sig.info.sonic;
	    strcpy(v1_el->elem.sig.info.phenom, v0_el->elem.sig.info.phenom);
	    
	    
	    /* 
	     *  V1 SIGMET specific information goes here. 
	     */	    
	    v1_el->elem.sig.info.phenom2[0]  = '\0';
	    v1_el->elem.sig.info.phennam[0]  = '\0';
	    v1_el->elem.sig.info.phenlat[0]  = '\0';
	    v1_el->elem.sig.info.phenlon[0]  = '\0';
	    v1_el->elem.sig.info.freetext[0] = '\0';
	    v1_el->elem.sig.info.pres 	  = IMISSD;
	    v1_el->elem.sig.info.maxwind  = IMISSD;	    
	    /* 
	     *  End of V1 SIGMET specific information 
	     */
	    
	    
	    strcpy(v1_el->elem.sig.info.trend, v0_el->elem.sig.info.trend);
	    strcpy(v1_el->elem.sig.info.move, v0_el->elem.sig.info.move);
	    v1_el->elem.sig.info.obsfcst  = v0_el->elem.sig.info.obsfcst;
	    strcpy(v1_el->elem.sig.info.obstime, v0_el->elem.sig.info.obstime);
	    v1_el->elem.sig.info.fl       = v0_el->elem.sig.info.fl;
	    v1_el->elem.sig.info.spd      = v0_el->elem.sig.info.spd;
	    strcpy(v1_el->elem.sig.info.dir, v0_el->elem.sig.info.dir);
	    strcpy(v1_el->elem.sig.info.tops, v0_el->elem.sig.info.tops);
	    strcpy(v1_el->elem.sig.info.fcstr, v0_el->elem.sig.info.fcstr);
	   
	    for ( ii = 0; ii < v0_el->elem.sig.info.npts * 2; ii++ )  
		v1_el->elem.sig.latlon[ii] = v0_el->elem.sig.latlon[ii];
	    
            	       
            break; 
	           	
    } 
       
}

/*=====================================================================*/

void cvg_v1_to_v2 ( v1_VG_DBStruct *v1_el, v2_VG_DBStruct *v2_el, int *iret )
/************************************************************************
 * cvg_v1_to_v2								*
 *									*
 * This function converts a version 1 VG element into version 2 element	*
 *									*
 * cvg_v1_to_v2 ( v1_el, v2_el, iret )					*
 *									*
 * Input parameters:							*
 *	*v1_el		v1_VG_DBStruct	Pointer to version 1 VG element	*
 *									*
 * Output parameters:							*
 *	*v2_el		v2_VG_DBStruct	Pointer to version 2 VG element	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		02/01	Created                                 *
 * J. Wu/GSC		02/01	Modified 'unused2' to 'version' in VG   *
 ***********************************************************************/
{    
    int		ii;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    

    /*
     *  Pass on header information. 
     */    
    v2_el->hdr = v1_el->hdr;
   

    /*
     *  Convert VG type-specific information and increase header size
     *  (recsz) and version number (version) if necessary.
     */          
    switch ( (int)v1_el->hdr.vg_type ) {

        case WBOX_ELM:         /* Watch Box Type */
	
    	    v2_el->hdr.version = (char)( (int)v1_el->hdr.version + 1 );
	    v2_el->hdr.recsz = ( (sizeof(float) * 2 * v1_el->elem.wbx.info.numpts)
                 		+ sizeof(VG_HdrStruct)
                 		+ sizeof(v2_WatchBoxInfo) );
	    v2_el->elem.wbx.info.numpts   = v1_el->elem.wbx.info.numpts;
	    v2_el->elem.wbx.info.w_style  = v1_el->elem.wbx.info.w_style;
	    v2_el->elem.wbx.info.w_shape  = v1_el->elem.wbx.info.w_shape;
	    v2_el->elem.wbx.info.w_number = v1_el->elem.wbx.info.w_number;
	    v2_el->elem.wbx.info.w_type   = v1_el->elem.wbx.info.w_type;
	    strcpy( v2_el->elem.wbx.info.w_file, v1_el->elem.wbx.info.w_file );

	    for ( ii = 0; ii < v1_el->elem.wbx.info.numpts * 2; ii++ )  
		v2_el->elem.wbx.latlon[ii] = v1_el->elem.wbx.latlon[ii];

	    v2_el->elem.wbx.info.w_istat = v1_el->elem.wbx.info.w_istat;
	    strcpy(v2_el->elem.wbx.info.w_iss_t, v1_el->elem.wbx.info.w_iss_t);
	    strcpy(v2_el->elem.wbx.info.w_exp_t, v1_el->elem.wbx.info.w_exp_t);
	    v2_el->elem.wbx.info.w_severity = v1_el->elem.wbx.info.w_severity;
	    strcpy(v2_el->elem.wbx.info.w_timezone,v1_el->elem.wbx.info.w_timezone);
	    strcpy(v2_el->elem.wbx.info.w_hailsz, v1_el->elem.wbx.info.w_hailsz);
	    strcpy(v2_el->elem.wbx.info.w_windg, v1_el->elem.wbx.info.w_windg);
	    strcpy(v2_el->elem.wbx.info.w_tops, v1_el->elem.wbx.info.w_tops);
	    strcpy(v2_el->elem.wbx.info.w_msmv_d, v1_el->elem.wbx.info.w_msmv_d);
	    strcpy(v2_el->elem.wbx.info.w_msmv_s, v1_el->elem.wbx.info.w_msmv_s);
	    strcpy(v2_el->elem.wbx.info.w_states, v1_el->elem.wbx.info.w_states);
	    strcpy(v2_el->elem.wbx.info.w_adjarea, v1_el->elem.wbx.info.w_adjarea);
	    strcpy(v2_el->elem.wbx.info.w_replw, v1_el->elem.wbx.info.w_replw);
	    strcpy(v2_el->elem.wbx.info.w_fcstr, v1_el->elem.wbx.info.w_fcstr);
	    v2_el->elem.wbx.info.w_issued = v1_el->elem.wbx.info.w_issued;
	    strcpy(v2_el->elem.wbx.info.wsm_iss_t, v1_el->elem.wbx.info.wsm_iss_t);
	    strcpy(v2_el->elem.wbx.info.wsm_exp_t, v1_el->elem.wbx.info.wsm_exp_t);
	    strcpy(v2_el->elem.wbx.info.wsm_ref, v1_el->elem.wbx.info.wsm_ref);
	    strcpy(v2_el->elem.wbx.info.wsm_from, v1_el->elem.wbx.info.wsm_from);
	    strcpy(v2_el->elem.wbx.info.wsm_meso, v1_el->elem.wbx.info.wsm_meso);
	    strcpy(v2_el->elem.wbx.info.wsm_fcstr, v1_el->elem.wbx.info.wsm_fcstr);


	    /* 
	     *  V2 Watch Box specific information goes here. 
	     */	    
	    v2_el->elem.wbx.info.numcnty = 0;
	    v2_el->elem.wbx.info.cn_flag = 1;
	
	    break;	


	case SIGAIRM_ELM:
	case SIGCONV_ELM:
	case SIGINTL_ELM:
	case SIGNCON_ELM:
	case SIGOUTL_ELM:	/* Sigmet Type	*/
	
	    v2_el->elem.sig = v1_el->elem.sig;
	    		
	    break;	
	
    } 
       
}

/*=====================================================================*/

void cvg_v2_to_v3 ( v2_VG_DBStruct *v2_el, v3_VG_DBStruct *v3_el, int *iret )
/************************************************************************
 * cvg_v2_to_v3								*
 *									*
 * This function converts a version 2 VG element into version 3 element	*
 *									*
 * cvg_v2_to_v3 (v2_el, v3_el, iret )			                *
 *									*
 * Input parameters:							*
 *	*v2_el		v2_VG_DBStruct	Pointer to version 2 VG element	*
 *									*
 * Output parameters:							*
 *	*v3_el		v3_VG_DBStruct	Pointer to version 3 VG element	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		02/01	Created                                 *
 * J. Wu/GSC		02/01	Modified 'unused2' to 'version' in VG   *
 ***********************************************************************/
{    
    int		ii;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    

    /*
     *  Pass on header information.
     */    
    v3_el->hdr = v2_el->hdr;
   

    /*
     *  Convert VG type-specific information and increase header size
     *  (recsz) and version number (version) if necessary.
     */        
    switch ( (int)v2_el->hdr.vg_type ) {
        
	case WBOX_ELM:          /* Watch Box Type */
    	    
	    v3_el->hdr.version = (char)( (int)v2_el->hdr.version + 1 );
	    v3_el->hdr.recsz = ( (sizeof(float) * 2 * v2_el->elem.wbx.info.numpts)
                 		+ sizeof(VG_HdrStruct)
                 		+ sizeof(v3_WatchBoxInfo) );
	    v3_el->elem.wbx.info.numpts   = v2_el->elem.wbx.info.numpts;
	    v3_el->elem.wbx.info.w_style  = v2_el->elem.wbx.info.w_style;
	    v3_el->elem.wbx.info.w_shape  = v2_el->elem.wbx.info.w_shape;
	    v3_el->elem.wbx.info.w_number = v2_el->elem.wbx.info.w_number;
	    v3_el->elem.wbx.info.w_type   = v2_el->elem.wbx.info.w_type;
	    strcpy( v3_el->elem.wbx.info.w_file, v2_el->elem.wbx.info.w_file );

	    for ( ii = 0; ii < v2_el->elem.wbx.info.numpts * 2; ii++ )  
		v3_el->elem.wbx.latlon[ii] = v2_el->elem.wbx.latlon[ii];

	    v3_el->elem.wbx.info.w_istat = v2_el->elem.wbx.info.w_istat;
	    strcpy(v3_el->elem.wbx.info.w_iss_t, v2_el->elem.wbx.info.w_iss_t);
	    strcpy(v3_el->elem.wbx.info.w_exp_t, v2_el->elem.wbx.info.w_exp_t);
	    v3_el->elem.wbx.info.w_severity = v2_el->elem.wbx.info.w_severity;
	    strcpy(v3_el->elem.wbx.info.w_timezone,v2_el->elem.wbx.info.w_timezone);
	    strcpy(v3_el->elem.wbx.info.w_hailsz, v2_el->elem.wbx.info.w_hailsz);
	    strcpy(v3_el->elem.wbx.info.w_windg, v2_el->elem.wbx.info.w_windg);
	    strcpy(v3_el->elem.wbx.info.w_tops, v2_el->elem.wbx.info.w_tops);
	    strcpy(v3_el->elem.wbx.info.w_msmv_d, v2_el->elem.wbx.info.w_msmv_d);
	    strcpy(v3_el->elem.wbx.info.w_msmv_s, v2_el->elem.wbx.info.w_msmv_s);
	    strcpy(v3_el->elem.wbx.info.w_states, v2_el->elem.wbx.info.w_states);
	    strcpy(v3_el->elem.wbx.info.w_adjarea, v2_el->elem.wbx.info.w_adjarea);
	    strcpy(v3_el->elem.wbx.info.w_replw, v2_el->elem.wbx.info.w_replw);
	    strcpy(v3_el->elem.wbx.info.w_fcstr, v2_el->elem.wbx.info.w_fcstr);
	    v3_el->elem.wbx.info.w_issued = v2_el->elem.wbx.info.w_issued;
	    strcpy(v3_el->elem.wbx.info.wsm_iss_t, v2_el->elem.wbx.info.wsm_iss_t);
	    strcpy(v3_el->elem.wbx.info.wsm_exp_t, v2_el->elem.wbx.info.wsm_exp_t);
	    strcpy(v3_el->elem.wbx.info.wsm_ref, v2_el->elem.wbx.info.wsm_ref);
	    strcpy(v3_el->elem.wbx.info.wsm_from, v2_el->elem.wbx.info.wsm_from);
	    strcpy(v3_el->elem.wbx.info.wsm_meso, v2_el->elem.wbx.info.wsm_meso);
	    strcpy(v3_el->elem.wbx.info.wsm_fcstr, v2_el->elem.wbx.info.wsm_fcstr);
	    v3_el->elem.wbx.info.numcnty = v2_el->elem.wbx.info.numcnty;
	    v3_el->elem.wbx.info.cn_flag = v2_el->elem.wbx.info.cn_flag;
	    for ( ii = 0; ii < v2_el->elem.wbx.info.numcnty; ii++ )  {
		v3_el->elem.wbx.info.cn_stat[ii] = v2_el->elem.wbx.info.cn_stat[ii];
		v3_el->elem.wbx.info.cn_ltln[ii] = v2_el->elem.wbx.info.cn_ltln[ii];
		v3_el->elem.wbx.info.cn_ltln[ii+v2_el->elem.wbx.info.numcnty] = 
		       v2_el->elem.wbx.info.cn_ltln[ii+v2_el->elem.wbx.info.numcnty];
            }
	    

	    /*
	     *  V3 Watch Box specific information goes here.
	     */	    
	    v3_el->elem.wbx.info.w_a0id[0] = '\0';
	    v3_el->elem.wbx.info.w_a0lt = RMISSD;
	    v3_el->elem.wbx.info.w_a0ln = RMISSD;
	    v3_el->elem.wbx.info.w_a0dis = IMISSD;
	    v3_el->elem.wbx.info.w_a0dir[0] = '\0';
	    v3_el->elem.wbx.info.w_a1id[0] = '\0';
	    v3_el->elem.wbx.info.w_a1lt = RMISSD;
	    v3_el->elem.wbx.info.w_a1ln = RMISSD;
	    v3_el->elem.wbx.info.w_a1dis = IMISSD;
	    v3_el->elem.wbx.info.w_a1dir[0] = '\0';
	
	    break;
	    
	case SIGAIRM_ELM:
	case SIGCONV_ELM:
	case SIGINTL_ELM:
	case SIGNCON_ELM:
	case SIGOUTL_ELM:	/* Sigmet Type	*/
	
	    v3_el->elem.sig = v2_el->elem.sig;
	    		
	    break;	
     }
         
}

/*=====================================================================*/

void cvg_v3_to_v4 ( v3_VG_DBStruct *v3_el, v4_VG_DBStruct *v4_el, int *iret )
/************************************************************************
 * cvg_v3_to_v4								*
 *									*
 * This function converts a version 3 VG element into version 4 element	*
 *									*
 * cvg_v3_to_v4 (v3_el, v4_el, iret )			                *
 *									*
 * Input parameters:							*
 *	*v3_el		v3_VG_DBStruct	Pointer to version 3 VG element	*
 *									*
 * Output parameters:							*
 *	*v4_el		v4_VG_DBStruct	Pointer to version 4 VG element	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 8/01	Created					*
 * J. Wu/SAIC	 	06/02	update for watch box version 5		*
 ***********************************************************************/
{    
    int		ii, ihot, intg, ier;
    char	strout[128], info[256], data[12];
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    

    /*
     *  Pass on header information.
     */    
    v4_el->hdr = v3_el->hdr;
   

    /*
     *  Convert VG type-specific information and increase header size
     *  (recsz) and version number (version) if necessary.
     */        
    switch ( (int)v3_el->hdr.vg_type ) {
        
	case WBOX_ELM:          /* Watch Box Type */
    	    
	    v4_el->hdr.version = (char)( (int)v3_el->hdr.version + 1 );
	    v4_el->hdr.recsz = ( (sizeof(float) * 2 * v3_el->elem.wbx.info.numpts)
                 		+ sizeof(VG_HdrStruct)
                 		+ sizeof(v4_WatchBoxInfo) );
	    v4_el->elem.wbx.info.numpts   = v3_el->elem.wbx.info.numpts;
	    v4_el->elem.wbx.info.w_style  = v3_el->elem.wbx.info.w_style;
	    v4_el->elem.wbx.info.w_shape  = v3_el->elem.wbx.info.w_shape;
	    v4_el->elem.wbx.info.w_number = v3_el->elem.wbx.info.w_number;
	    v4_el->elem.wbx.info.w_type   = v3_el->elem.wbx.info.w_type;
	    strcpy( v4_el->elem.wbx.info.w_file, v3_el->elem.wbx.info.w_file );

	    for ( ii = 0; ii < v3_el->elem.wbx.info.numpts * 2; ii++ )  
		v4_el->elem.wbx.latlon[ii] = v3_el->elem.wbx.latlon[ii];

	    v4_el->elem.wbx.info.w_istat = v3_el->elem.wbx.info.w_istat;
	    strcpy(v4_el->elem.wbx.info.w_iss_t, v3_el->elem.wbx.info.w_iss_t);
	    strcpy(v4_el->elem.wbx.info.w_exp_t, v3_el->elem.wbx.info.w_exp_t);
	    v4_el->elem.wbx.info.w_severity = v3_el->elem.wbx.info.w_severity;
	    strcpy(v4_el->elem.wbx.info.w_timezone,v3_el->elem.wbx.info.w_timezone);
	    strcpy(v4_el->elem.wbx.info.w_hailsz, v3_el->elem.wbx.info.w_hailsz);
	    strcpy(v4_el->elem.wbx.info.w_windg, v3_el->elem.wbx.info.w_windg);
	    strcpy(v4_el->elem.wbx.info.w_tops, v3_el->elem.wbx.info.w_tops);
	    strcpy(v4_el->elem.wbx.info.w_msmv_d, v3_el->elem.wbx.info.w_msmv_d);
	    strcpy(v4_el->elem.wbx.info.w_msmv_s, v3_el->elem.wbx.info.w_msmv_s);
	    strcpy(v4_el->elem.wbx.info.w_states, v3_el->elem.wbx.info.w_states);
	    strcpy(v4_el->elem.wbx.info.w_adjarea, v3_el->elem.wbx.info.w_adjarea);
	    strcpy(v4_el->elem.wbx.info.w_replw, v3_el->elem.wbx.info.w_replw);
	    strcpy(v4_el->elem.wbx.info.w_fcstr, v3_el->elem.wbx.info.w_fcstr);
	    v4_el->elem.wbx.info.w_issued = v3_el->elem.wbx.info.w_issued;
	    strcpy(v4_el->elem.wbx.info.wsm_iss_t, v3_el->elem.wbx.info.wsm_iss_t);
	    strcpy(v4_el->elem.wbx.info.wsm_exp_t, v3_el->elem.wbx.info.wsm_exp_t);
	    strcpy(v4_el->elem.wbx.info.wsm_ref, v3_el->elem.wbx.info.wsm_ref);
	    strcpy(v4_el->elem.wbx.info.wsm_from, v3_el->elem.wbx.info.wsm_from);
	    strcpy(v4_el->elem.wbx.info.wsm_meso, v3_el->elem.wbx.info.wsm_meso);
	    strcpy(v4_el->elem.wbx.info.wsm_fcstr, v3_el->elem.wbx.info.wsm_fcstr);
	    v4_el->elem.wbx.info.numcnty = v3_el->elem.wbx.info.numcnty;
	    v4_el->elem.wbx.info.cn_flag = v3_el->elem.wbx.info.cn_flag;
	    for ( ii = 0; ii < v3_el->elem.wbx.info.numcnty; ii++ )  {
		v4_el->elem.wbx.info.cn_stat[ii] = v3_el->elem.wbx.info.cn_stat[ii];
		v4_el->elem.wbx.info.cn_ltln[ii] = v3_el->elem.wbx.info.cn_ltln[ii];
		v4_el->elem.wbx.info.cn_ltln[ii+v3_el->elem.wbx.info.numcnty] = 
		       v3_el->elem.wbx.info.cn_ltln[ii+v3_el->elem.wbx.info.numcnty];
            }
	    
	    strcpy(v4_el->elem.wbx.info.w_a0id,
		v3_el->elem.wbx.info.w_a0id);
	    v4_el->elem.wbx.info.w_a0lt = v3_el->elem.wbx.info.w_a0lt;
	    v4_el->elem.wbx.info.w_a0ln = v3_el->elem.wbx.info.w_a0ln;
	    v4_el->elem.wbx.info.w_a0dis = v3_el->elem.wbx.info.w_a0dis;
	    strcpy(v4_el->elem.wbx.info.w_a0dir,
		v3_el->elem.wbx.info.w_a0dir);
	    strcpy(v4_el->elem.wbx.info.w_a1id,
		v3_el->elem.wbx.info.w_a1id);
	    v4_el->elem.wbx.info.w_a1lt = v3_el->elem.wbx.info.w_a1lt;
	    v4_el->elem.wbx.info.w_a1ln = v3_el->elem.wbx.info.w_a1ln;
	    v4_el->elem.wbx.info.w_a1dis = v3_el->elem.wbx.info.w_a1dis;
	    strcpy(v4_el->elem.wbx.info.w_a1dir,
		v3_el->elem.wbx.info.w_a1dir);

	    /*
	     *  For each county, determine the FIPS code.
	     */
	    ihot = 0;
	    clo_init ( &ier );
            for ( ii = 0; ii < v3_el->elem.wbx.info.numcnty; ii++ )  {
	        clo_tqbnd ( "CNTY_BNDS", v4_el->elem.wbx.info.cn_ltln[ii],
		  v4_el->elem.wbx.info.cn_ltln[ii+v3_el->elem.wbx.info.numcnty],
		  strout, &ier );
	        clo_bginfo ( "CNTY_BNDS", ihot, info, &ier );
		cst_gtag ( "FIPS", info, "99999", data, &ier );
		cst_numb ( data, &intg, &ier );
		v4_el->elem.wbx.info.cn_fips[ii] = intg;
	    }
		
	
	    break;
	    
	case SIGAIRM_ELM:
	case SIGCONV_ELM:
	case SIGINTL_ELM:
	case SIGNCON_ELM:
	case SIGOUTL_ELM:	/* Sigmet Type	*/
	
	    v4_el->elem.sig = v3_el->elem.sig;
	    		
	    break;	
     }
         
}

/*=====================================================================*/

void cvg_v4_to_v5 ( v4_VG_DBStruct *v4_el, v5_VG_DBStruct *v5_el, int *iret )
/************************************************************************
 * cvg_v4_to_v5								*
 *									*
 * This function converts a version 4 VG element into version 5 element	*
 *									*
 * cvg_v4_to_v5 (v4_el, v5_el, iret )			                *
 *									*
 * Input parameters:							*
 *	*v4_el		v4_VG_DBStruct	Pointer to version 4 VG element	*
 *									*
 * Output parameters:							*
 *	*v5_el		v5_VG_DBStruct	Pointer to version 5 VG element	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	06/02	modify from cvg_v3_to_v4		*
 * H. Zeng/XTRIA        01/03   modified for new v5_VG_DBStruct         *
 ***********************************************************************/
{    
    int		ii;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    

    /*
     *  Pass on header information.
     */    
    v5_el->hdr = v4_el->hdr;
   

    /*
     *  Convert VG type-specific information and increase header size
     *  (recsz) and version number (version) if necessary.
     */        
    switch ( (int)v4_el->hdr.vg_type ) {
        
	case WBOX_ELM:          /* Watch Box Type */
    	    
	    v5_el->hdr.version = (char)( (int)v4_el->hdr.version + 1 );
	    v5_el->hdr.recsz = ( (sizeof(float) * 2 * v4_el->elem.wbx.info.numpts)
                 		+ sizeof(VG_HdrStruct)
                 		+ sizeof(v5_WatchBoxInfo) );
	    v5_el->elem.wbx.info.numpts   = v4_el->elem.wbx.info.numpts;
	    v5_el->elem.wbx.info.w_style  = v4_el->elem.wbx.info.w_style;
	    v5_el->elem.wbx.info.w_shape  = v4_el->elem.wbx.info.w_shape;
	    v5_el->elem.wbx.info.w_number = v4_el->elem.wbx.info.w_number;
	    v5_el->elem.wbx.info.w_type   = v4_el->elem.wbx.info.w_type;
	    strcpy( v5_el->elem.wbx.info.w_file, v4_el->elem.wbx.info.w_file );

	    for ( ii = 0; ii < v4_el->elem.wbx.info.numpts * 2; ii++ )  
		v5_el->elem.wbx.latlon[ii] = v4_el->elem.wbx.latlon[ii];

	    v5_el->elem.wbx.info.w_istat = v4_el->elem.wbx.info.w_istat;
	    strcpy(v5_el->elem.wbx.info.w_iss_t, v4_el->elem.wbx.info.w_iss_t);
	    strcpy(v5_el->elem.wbx.info.w_exp_t, v4_el->elem.wbx.info.w_exp_t);
	    v5_el->elem.wbx.info.w_severity = v4_el->elem.wbx.info.w_severity;
	    strcpy(v5_el->elem.wbx.info.w_timezone,v4_el->elem.wbx.info.w_timezone);
	    strcpy(v5_el->elem.wbx.info.w_hailsz, v4_el->elem.wbx.info.w_hailsz);
	    strcpy(v5_el->elem.wbx.info.w_windg, v4_el->elem.wbx.info.w_windg);
	    strcpy(v5_el->elem.wbx.info.w_tops, v4_el->elem.wbx.info.w_tops);
	    strcpy(v5_el->elem.wbx.info.w_msmv_d, v4_el->elem.wbx.info.w_msmv_d);
	    strcpy(v5_el->elem.wbx.info.w_msmv_s, v4_el->elem.wbx.info.w_msmv_s);
	    strcpy(v5_el->elem.wbx.info.w_states, v4_el->elem.wbx.info.w_states);
	    strcpy(v5_el->elem.wbx.info.w_adjarea, v4_el->elem.wbx.info.w_adjarea);
	    strcpy(v5_el->elem.wbx.info.w_replw, v4_el->elem.wbx.info.w_replw);
	    strcpy(v5_el->elem.wbx.info.w_fcstr, v4_el->elem.wbx.info.w_fcstr);
	    v5_el->elem.wbx.info.w_issued = v4_el->elem.wbx.info.w_issued;
	    strcpy(v5_el->elem.wbx.info.wsm_iss_t, v4_el->elem.wbx.info.wsm_iss_t);
	    strcpy(v5_el->elem.wbx.info.wsm_exp_t, v4_el->elem.wbx.info.wsm_exp_t);
	    strcpy(v5_el->elem.wbx.info.wsm_ref, v4_el->elem.wbx.info.wsm_ref);
	    strcpy(v5_el->elem.wbx.info.wsm_from, v4_el->elem.wbx.info.wsm_from);
	    strcpy(v5_el->elem.wbx.info.wsm_meso, v4_el->elem.wbx.info.wsm_meso);
	    strcpy(v5_el->elem.wbx.info.wsm_fcstr, v4_el->elem.wbx.info.wsm_fcstr);
	    v5_el->elem.wbx.info.numcnty = v4_el->elem.wbx.info.numcnty;
	    v5_el->elem.wbx.info.cn_flag = v4_el->elem.wbx.info.cn_flag;
	    for ( ii = 0; ii < v4_el->elem.wbx.info.numcnty; ii++ )  {
		v5_el->elem.wbx.info.cn_fips[ii] = v4_el->elem.wbx.info.cn_fips[ii];
		v5_el->elem.wbx.info.cn_ltln[ii] = v4_el->elem.wbx.info.cn_ltln[ii];
		v5_el->elem.wbx.info.cn_ltln[ii+v4_el->elem.wbx.info.numcnty] = 
		       v4_el->elem.wbx.info.cn_ltln[ii+v4_el->elem.wbx.info.numcnty];
            }
	    
	    strcpy(v5_el->elem.wbx.info.w_a0id,
		v4_el->elem.wbx.info.w_a0id);
	    v5_el->elem.wbx.info.w_a0lt = v4_el->elem.wbx.info.w_a0lt;
	    v5_el->elem.wbx.info.w_a0ln = v4_el->elem.wbx.info.w_a0ln;
	    v5_el->elem.wbx.info.w_a0dis = v4_el->elem.wbx.info.w_a0dis;
	    strcpy(v5_el->elem.wbx.info.w_a0dir, v4_el->elem.wbx.info.w_a0dir);
	    strcpy(v5_el->elem.wbx.info.w_a1id, v4_el->elem.wbx.info.w_a1id);
	    v5_el->elem.wbx.info.w_a1lt = v4_el->elem.wbx.info.w_a1lt;
	    v5_el->elem.wbx.info.w_a1ln = v4_el->elem.wbx.info.w_a1ln;
	    v5_el->elem.wbx.info.w_a1dis = v4_el->elem.wbx.info.w_a1dis;
	    strcpy(v5_el->elem.wbx.info.w_a1dir, v4_el->elem.wbx.info.w_a1dir);
	
	    break;
	    
	case SIGAIRM_ELM:
	case SIGCONV_ELM:
	case SIGINTL_ELM:
	case SIGNCON_ELM:
	case SIGOUTL_ELM:	/* Sigmet Type	*/
	
	    v5_el->elem.sig = v4_el->elem.sig;
	    		
	    break;	
     }
         
}

/*=====================================================================*/

void cvg_v5_to_v6 ( v5_VG_DBStruct *v5_el, VG_DBStruct *v6_el, int *iret )
/************************************************************************
 * cvg_v5_to_v6								*
 *									*
 * This function converts a version 5 VG element into version 6 element	*
 *									*
 * cvg_v5_to_v6 (v5_el, v6_el, iret )			                *
 *									*
 * Input parameters:							*
 *	*v5_el		v5_VG_DBStruct	Pointer to version 5 VG element	*
 *									*
 * Output parameters:							*
 *	*v6_el		VG_DBStruct	Pointer to latest VG element	*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/XTRIA        01/03   initial coding				*
 ***********************************************************************/
{    
    int		ii;
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    

    /*
     *  Pass on header information.
     */    
    v6_el->hdr = v5_el->hdr;
   

    /*
     *  Convert VG type-specific information and increase header size
     *  (recsz) and version number (version) if necessary.
     */        
    switch ( (int)v5_el->hdr.vg_type ) {
        
	case WBOX_ELM:          /* Watch Box Type */
    	    
	    v6_el->hdr.version = (char)( (int)v5_el->hdr.version + 1 );
	    v6_el->hdr.recsz = ( (sizeof(float) * 2 * v5_el->elem.wbx.info.numpts)
                 		+ sizeof(VG_HdrStruct)
                 		+ sizeof(WatchBoxInfo) );
	    v6_el->elem.wbx.info.numpts   = v5_el->elem.wbx.info.numpts;
	    v6_el->elem.wbx.info.w_style  = v5_el->elem.wbx.info.w_style;
	    v6_el->elem.wbx.info.w_shape  = v5_el->elem.wbx.info.w_shape;
	    v6_el->elem.wbx.info.w_number = v5_el->elem.wbx.info.w_number;
	    v6_el->elem.wbx.info.w_type   = v5_el->elem.wbx.info.w_type;
	    strcpy( v6_el->elem.wbx.info.w_file, v5_el->elem.wbx.info.w_file );

	    for ( ii = 0; ii < v5_el->elem.wbx.info.numpts * 2; ii++ ) {
		v6_el->elem.wbx.latlon[ii] = v5_el->elem.wbx.latlon[ii];
            }

	    v6_el->elem.wbx.info.w_istat = v5_el->elem.wbx.info.w_istat;
	    strcpy(v6_el->elem.wbx.info.w_iss_t, v5_el->elem.wbx.info.w_iss_t);
	    strcpy(v6_el->elem.wbx.info.w_exp_t, v5_el->elem.wbx.info.w_exp_t);
	    v6_el->elem.wbx.info.w_severity = v5_el->elem.wbx.info.w_severity;
	    strcpy(v6_el->elem.wbx.info.w_timezone,v5_el->elem.wbx.info.w_timezone);
	    strcpy(v6_el->elem.wbx.info.w_hailsz, v5_el->elem.wbx.info.w_hailsz);
	    strcpy(v6_el->elem.wbx.info.w_windg, v5_el->elem.wbx.info.w_windg);
	    strcpy(v6_el->elem.wbx.info.w_tops, v5_el->elem.wbx.info.w_tops);
	    strcpy(v6_el->elem.wbx.info.w_msmv_d, v5_el->elem.wbx.info.w_msmv_d);
	    strcpy(v6_el->elem.wbx.info.w_msmv_s, v5_el->elem.wbx.info.w_msmv_s);
	    strcpy(v6_el->elem.wbx.info.w_states, v5_el->elem.wbx.info.w_states);
	    strcpy(v6_el->elem.wbx.info.w_adjarea, v5_el->elem.wbx.info.w_adjarea);
	    strcpy(v6_el->elem.wbx.info.w_replw, v5_el->elem.wbx.info.w_replw);
	    strcpy(v6_el->elem.wbx.info.w_fcstr, v5_el->elem.wbx.info.w_fcstr);
	    v6_el->elem.wbx.info.w_issued = v5_el->elem.wbx.info.w_issued;
	    strcpy(v6_el->elem.wbx.info.wsm_iss_t, v5_el->elem.wbx.info.wsm_iss_t);
	    strcpy(v6_el->elem.wbx.info.wsm_exp_t, v5_el->elem.wbx.info.wsm_exp_t);
	    strcpy(v6_el->elem.wbx.info.wsm_ref, v5_el->elem.wbx.info.wsm_ref);
	    strcpy(v6_el->elem.wbx.info.wsm_from, v5_el->elem.wbx.info.wsm_from);
	    strcpy(v6_el->elem.wbx.info.wsm_meso, v5_el->elem.wbx.info.wsm_meso);
	    strcpy(v6_el->elem.wbx.info.wsm_fcstr, v5_el->elem.wbx.info.wsm_fcstr);
	    v6_el->elem.wbx.info.numcnty = v5_el->elem.wbx.info.numcnty;
	    v6_el->elem.wbx.info.cn_flag = v5_el->elem.wbx.info.cn_flag;
	    for ( ii = 0; ii < v5_el->elem.wbx.info.numcnty; ii++ )  {
		v6_el->elem.wbx.info.cn_fips[ii] = v5_el->elem.wbx.info.cn_fips[ii];
		v6_el->elem.wbx.info.cn_ltln[ii] = v5_el->elem.wbx.info.cn_ltln[ii];
		v6_el->elem.wbx.info.cn_ltln[ii+v5_el->elem.wbx.info.numcnty] = 
		       v5_el->elem.wbx.info.cn_ltln[ii+v5_el->elem.wbx.info.numcnty];
            }
	    
	    strcpy(v6_el->elem.wbx.info.w_a0id,
		v5_el->elem.wbx.info.w_a0id);
	    v6_el->elem.wbx.info.w_a0lt = v5_el->elem.wbx.info.w_a0lt;
	    v6_el->elem.wbx.info.w_a0ln = v5_el->elem.wbx.info.w_a0ln;
	    v6_el->elem.wbx.info.w_a0dis = v5_el->elem.wbx.info.w_a0dis;
	    strcpy(v6_el->elem.wbx.info.w_a0dir, v5_el->elem.wbx.info.w_a0dir);
	    strcpy(v6_el->elem.wbx.info.w_a1id, v5_el->elem.wbx.info.w_a1id);
	    v6_el->elem.wbx.info.w_a1lt = v5_el->elem.wbx.info.w_a1lt;
	    v6_el->elem.wbx.info.w_a1ln = v5_el->elem.wbx.info.w_a1ln;
	    v6_el->elem.wbx.info.w_a1dis = v5_el->elem.wbx.info.w_a1dis;
	    strcpy(v6_el->elem.wbx.info.w_a1dir, v5_el->elem.wbx.info.w_a1dir);
	
            /*
             * Add marker info to new WatchBoxInfo.
             */
            v6_el->elem.wbx.info.w_mrktyp = 1;
            v6_el->elem.wbx.info.w_mrksiz = 1.0;
            v6_el->elem.wbx.info.w_mrkwid = 1;

	    break;
	    
	case SIGAIRM_ELM:
	case SIGCONV_ELM:
	case SIGINTL_ELM:
	case SIGNCON_ELM:
	case SIGOUTL_ELM:	/* Sigmet Type	*/
	
	    v6_el->elem.sig = v5_el->elem.sig;
	    		
	    break;	
     }
         
}

/*=====================================================================*/

