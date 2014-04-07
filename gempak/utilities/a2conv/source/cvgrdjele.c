#include "cvgcmn.h"
#include "drwids.h"

#define HDR_SZ		( (int)sizeof(VG_HdrStruct) )
#define MAX_PTS_SZ	( (int)sizeof(float) * MAXPTS )

#define	MAX_LINE_SZ     ( HDR_SZ + sizeof(LineType) )
#define	MAX_FRONT_SZ    ( HDR_SZ + (int)sizeof(FrontType) )
#define	MAX_SPLINE_SZ   ( HDR_SZ + sizeof(SpLineType) )

#define MAX_SYM_SZ	( HDR_SZ + (int)sizeof(SymType) )
#define MAX_WIND_SZ     ( HDR_SZ + (int)sizeof(WindType) )
#define MAX_TEXT_SZ    	( HDR_SZ + sizeof(TextType) )
#define MAX_SPTEXT_SZ   ( HDR_SZ + (int)sizeof(SptxType) )
#define MAX_CIRCLE_SZ   ( HDR_SZ + (int)sizeof(CircType) )
 
#define MAX_SIGMET_SZ  	( HDR_SZ + (int)sizeof(SigmetType) )
#define MAX_CCF_SZ	( HDR_SZ + (int)sizeof(CCFType) )
#define MAX_ASH_SZ	( HDR_SZ + (int)sizeof(AshType) )
#define MAX_VOL_SZ	( HDR_SZ + (int)sizeof(VolType) )
#define MAX_TRACK_SZ    ( HDR_SZ + (int)sizeof(TrackType) )
#define MAX_WATCHBOX_SZ ( HDR_SZ + (int)sizeof(WatchBoxType) )
#define MAX_WATCHSM_SZ  ( HDR_SZ + sizeof(WatchSMType) )
#define MAX_LIST_SZ	( HDR_SZ + (int)sizeof(ListType) )
#define MAX_JET_SZ	( HDR_SZ + (int)sizeof(JetType) )



void cvg_rdjele ( VG_DBStruct *el, int el_start, int el_size, 
						FILE *fp, int *iret )
/************************************************************************
 * cvg_rdjele								*
 *									*
 * This function reads a VG element from an open VG file. 		*
 *									*
 *  Note:  It is assumed that the *el contains a loaded header.		*
 *									*
 * cvg_rdjele ( el, el_start, el_size, fp, iret )			*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	element structure		*
 *	el_start	int		File offset to start of element	*
 *	el_size		int		size of element to read in Bytes*
 *	*fp		FILE		Handle to open file		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  3 = VG element too large	*
 *					 -3 = seek to a bad location	*
 *					 -8 = no VG file is open	*
 *					-24 = no VG header loaded	*
 *					-45 = invalid VG version	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/09	Copied from cvg_rdele			*
 * S. Jacobs/NCEP	12/09	Replaced call to cvg_rdoldele with	*
 * 				error return code			*
 * S. Jacobs/NCEP	12/09	Removed call to cvg_eladj		*
 ***********************************************************************/
{
    int 	  *pnpts, ier, nbin, nbinfo, itrunc, max_version;
    int 	  info_pos, lon_pos, ierr;
    int		  one = 1;
    int		  tmp;
    int		  max_sz, info_sz;
    unsigned char *pinfo, *el_lon;
/*---------------------------------------------------------------------*/

    *iret = 0;
    info_pos = el_start + HDR_SZ;
    
    /*
     *  Determine the latest version # for VG elements.
     */
    max_version = 0; /* Latest version # for most VG element types */
    
    if ( el->hdr.vg_class == CLASS_WATCHES )  max_version = CUR_WBX_VER;
    if ( ( el->hdr.vg_class == CLASS_SIGMETS ) &&
         el->hdr.vg_type != SIGCCF_ELM &&
	 el->hdr.vg_type != VOLC_ELM &&
	 el->hdr.vg_type != ASHCLD_ELM )      max_version = CUR_SIG_VER;

    
    /*
     *  Read old versions VG elements (only WATCHES & SIGMETS have old
     *  versions currently). 
     */
    if ( (int)el->hdr.version < max_version ) { 
	//cvg_rdoldele( el, el_start, el->hdr.recsz, fp, iret ); 
	*iret = -45;
	return;
    }
    
    else { 
        /*
         * Read current version of VG elements.  
         */
	pnpts  = NULL;
	itrunc = G_FALSE;
        pinfo  = (void *) &(el->elem);

	nbinfo = el_size - HDR_SZ;

	/*
	 *  Lines and fronts may have more than MAXPTS in their latlon array
	 *  if they were created through the vg driver.  If they do, they will
	 *  be truncated to MAXPTS, and an iret of +3 returned.
	 *
	 *  All other types will be checked against their maximum size, and
	 *  an error reported if they exceed this.
	 */
	switch ( el->hdr.vg_class ) {
	
	    case CLASS_LINES:

	        if (el->hdr.vg_type == LINE_ELM) {
		    max_sz  = MAX_LINE_SZ;
		    pnpts   = &(el->elem.lin.info.numpts);
		    info_sz = sizeof(LineInfo);
		    el_lon  = (void *)el->elem.lin.latlon;
		}
		else {
		    max_sz  = MAX_SPLINE_SZ;
		    pnpts   = &(el->elem.spl.info.numpts);
		    info_sz = sizeof(SpLineInfo);
		    el_lon  = (void *)el->elem.spl.latlon;
		}

		if (el_size > max_sz) {
		    itrunc  = G_TRUE;
		    el_lon  += MAX_PTS_SZ;	
  		    nbinfo  =  info_sz + MAX_PTS_SZ; 
		    el->hdr.recsz = max_sz;
		}

		break;

	    case CLASS_FRONTS:

		if (el_size > MAX_FRONT_SZ) {
		    itrunc = G_TRUE;
		    pnpts  = &(el->elem.frt.info.numpts);
		    el_lon = (void *)el->elem.frt.latlon; 
		    el_lon += MAX_PTS_SZ;
		    nbinfo = MAX_FRONT_SZ - HDR_SZ;
		    el->hdr.recsz = MAX_FRONT_SZ;
		}

  		pnpts  = &(el->elem.frt.info.numpts);
		break;

	
	    case CLASS_SYMBOLS:
	        if (el->hdr.recsz > MAX_SYM_SZ) {
		    *iret = -11;
		}
		break;

	    case CLASS_WINDS:
	        if (el->hdr.recsz > MAX_WIND_SZ) {
		    *iret = -11;
		}
		break;

	    case CLASS_TEXT:
		if (el->hdr.recsz > MAX_SPTEXT_SZ) {
		    *iret = -11;
		}
		break;
	 
	    case CLASS_CIRCLE:
		if (el->hdr.recsz > MAX_CIRCLE_SZ) {
		    *iret = -11;
		}
		break;
 
            case CLASS_SIGMETS:
		if ( ( ( el->hdr.vg_type != SIGCCF_ELM &&
			 el->hdr.vg_type != ASHCLD_ELM &&
			 el->hdr.vg_type != VOLC_ELM ) &&
		       el->hdr.recsz > MAX_SIGMET_SZ )  ||
        ( el->hdr.vg_type == SIGCCF_ELM && el->hdr.recsz > MAX_CCF_SZ ) ||
	( el->hdr.vg_type == ASHCLD_ELM && el->hdr.recsz > MAX_ASH_SZ ) ||
	( el->hdr.vg_type ==   VOLC_ELM && el->hdr.recsz > MAX_VOL_SZ ) ) {
		    *iret = -11;
		}
		break;
 
            case CLASS_TRACKS:
		if (el->hdr.recsz > MAX_TRACK_SZ) {
		    *iret = -11;
		}
		break;

            case CLASS_WATCHES:
		if (el->hdr.recsz > MAX_WATCHBOX_SZ) {
		    *iret = -11;
		}
		break;

            case CLASS_LIST:
		if (el->hdr.recsz > MAX_LIST_SZ) {
		    *iret = -11;
		}
		break;

            case CLASS_MET:
		if (el->hdr.vg_type == JET_ELM &&
		    el->hdr.recsz > MAX_JET_SZ) {
		    *iret = -11;
		}
		
		break;
	    default:
	        break;

        }   /* end of switch */

	if ( *iret < 0 ) {  
	    return;
   	}

	/*
	 *  Read the element data.  This read will not include the lon
	 *  array if the element has > MAXPTS.  The lons will be read using
	 *  the "if ( itrunc )" condition statements below.
	 */
	if ( el->hdr.vg_type == GFA_ELM ) {
	    cvg_rdgfa ( fp, (long)info_pos, el, &ier );
	}
	else if ( el->hdr.vg_type == TCA_ELM ) {
	    cvg_rdtca ( fp, (long)info_pos, el, &ier );
	}
	else if ( el->hdr.vg_type == TCERR_ELM ) {
	    cvg_rdtce ( fp, (long)info_pos, el, &ier );
	}
	else if ( el->hdr.vg_type == TCTRK_ELM ) {
	    cvg_rdtct ( fp, (long)info_pos, el, &ier );
	}
	else if ( el->hdr.vg_type == TCBKL_ELM ) {
	    cvg_rdtcb ( fp, (long)info_pos, el, &ier );
	}
	else {	
	    cfl_seek( fp, (long)info_pos, 0, &ier );
	    cfl_read( fp, nbinfo, (void *)( pinfo ), &nbin, &ier );
        }

	/*
	 *  Read the lons for truncated elements.
	 */
  	if ( itrunc ) {

	    /*
	     *  Set element characteristics to reasonable values, considering
	     *  that the line is truncated.  Set iret = 3 to signal truncation.
	     */
	    *iret   = 3;
	    *pnpts  = MAXPTS;
	    el->hdr.closed = 0;
	    el->hdr.filled = 0;

	    lon_pos  = info_pos + info_sz + 
    			( (el_size - HDR_SZ - info_sz) / 2 );

	    cfl_seek ( fp, (long)lon_pos, 0, &ier );
	    cfl_read ( fp, MAX_PTS_SZ, (void *)(el_lon), &nbin, &ier); 
	}
  

	if ( MTMACH == MTULTX ||
	     MTMACH == MTALPH ||
	     MTMACH == MTLNUX ) {
	    if ( itrunc ) {
  	        mv_swp4 (&one, pnpts, &tmp );
	        *pnpts = tmp;
	    }

	    cvg_swap( SWPINF, G_TRUE, *el, el, &ierr );
	}

    }

    /*
     * If the object is in the set of placed data, adjust the coordinates
     */
 /*   cvg_eladj(el_start, el, &ierr);  */
}
