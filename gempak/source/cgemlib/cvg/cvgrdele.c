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
#define MAX_SGWX_SZ     ( HDR_SZ + (int)sizeof(SGWXType) )
#define MAX_ASH_SZ	( HDR_SZ + (int)sizeof(AshType) )
#define MAX_VOL_SZ	( HDR_SZ + (int)sizeof(VolType) )
#define MAX_TRACK_SZ    ( HDR_SZ + (int)sizeof(TrackType) )
#define MAX_WATCHBOX_SZ ( HDR_SZ + (int)sizeof(WatchBoxType) )
#define MAX_WATCHSM_SZ  ( HDR_SZ + sizeof(WatchSMType) )
#define MAX_LIST_SZ	( HDR_SZ + (int)sizeof(ListType) )
#define MAX_JET_SZ	( HDR_SZ + (int)sizeof(JetType) )



void cvg_rdele ( VG_DBStruct *el, int el_start, int el_size, 
						FILE *fp, int *iret )
/************************************************************************
 * cvg_rdele								*
 *									*
 * This function reads a VG element from an open VG file. 		*
 *									*
 *  Note:  It is assumed that the *el contains a loaded header.		*
 *									*
 * cvg_rdele ( el, el_start, el_size, fp, iret )			*
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
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	10/96	Created					*
 * D. Keiser/GSC	 1/97	Clean up and rename			*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * C. Lin/EAI	        12/97	text element alignment remapping	*
 * G. Krueger/EAI	 1/98	Truncate too long elements.		*
 * G. Krueger/EAI	 4/98	Truncate too long elements to MAXPTS-1.	*
 * S. Jacobs/NCEP	 6/98	Added check for small front pips	*
 * A. Hardy/GSC         12/98   Added CLASS_CIRCLE                      *
 * E. Safford/GSC	04/99	fix no return value compiler warning	*
 * D.W.Plummer/NCEP     12/99   add call to cvg_rdwbx                   *
 * M. Li/GSC		12/99	Added cvg_swap and mv_swp4		*
 * F. J. Yen/NCEP	08/00	Added call to cvg_rdsig			*
 * R. Curtis/EAI	10/00	Added CCF check to Sigmet class check   *
 * J. Wu/GSC		11/00	Removed cvd_rdsym, cvd_rdwnd, cvg_rdcir *
 * E. Safford/GSC	12/00	rename params, clean up duplicate reads *
 * J. Wu/GSC		02/01	Replaced cvg_rdwbx & cvg_rdsig with new *
 *                              cvg_rdoldele() and added version control*
 * J. Wu/GSC		02/01	Modified 'unused2' to 'version' in VG   *
 * S. Jacobs/NCEP	 2/01	Added machine type MTLNUX		*
 * D.W.Plummer/NCEP	06/03	chgs for ASHCLD_ELM and VOLC_ELM	*
 * J. Wu/GSC		09/03	add LIST_ELM & JET_ELM			*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * J. Wu/GSC		01/04	add cvg_rdgfa for new GFA_ELM		*
 * B. Yin/SAIC		02/04	added cvg_rdtca for TCA_ELM             *
 * B. Yin/SAIC		05/04	redefined MAX_TCA_SZ	                *
 * B. Yin/SAIC		08/04	removed MAX_TCA_SZ for water and islands*
 * J. Wu/SAIC		10/04	remove MAX_GFA_SZ			*
 * S. Danz/AWC          03/06   Add check for placement information     *
 * m.gamazaychikov/SAIC	04/07	add cvg_rdtce, cvg_rdtct and cvg_rdtcb	*
 * L. Hinson/AWC        01/12   Add SGWX_ELM                            *
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
	cvg_rdoldele( el, el_start, el->hdr.recsz, fp, iret );    
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
		} else if (el->hdr.vg_type == SGWX_ELM &&
                    el->hdr.recsz > MAX_SGWX_SZ) {
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
    cvg_eladj(el_start, el, &ierr);
}
