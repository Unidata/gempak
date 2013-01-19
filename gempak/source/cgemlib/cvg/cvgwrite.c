#include "cvgcmn.h"
#include "pgprm.h"

void cvg_write ( VG_DBStruct *el, int start, int numbytes, 
		FILE *fp, Boolean inc_place, int *iret )
/************************************************************************
 * cvg_write								*
 *									*
 * This function writes an element record to an opened VG file.		*
 *									*
 * cvg_write  ( el, start, numbytes, fp, inc_place, iret )		*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct 	Pointer toVG record structure	*
 *	start		int		Offset to start VG record	*
 *	numbytes	int		Number of bytes to be written	*
 *	*fp		FILE		file pointer to VG file		*
 *	inc_place	Boolean		flag to include VG in placement *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -3 = seek to a bad location	*
 *					-17 = error writing to VG file	*
 *					-30 = invalid color(s)   	*
 *					-47 = no proper file specified	*
 *									*
 **									*
 * Log:									*
 * J. Wu/GSC		02/01	Created based on cvg_writelm()		*
 * S. Jacobs/NCEP	 2/01	Added machine type MTLNUX		*
 * E. Safford/GSC	07/01	add frees for all error returns		*
 * J. Wu/SAIC		10/02	use memcpy() to speed up performance	*
 * T. Piper/SAIC	12/02	Only use tmp_el when necessary		*
 * T. Piper/SAIC	12/02	Added -2 option for start		*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * J. Wu/SAIC		01/04	add cvg_writgfa for GFA_ELM		*
 * B. Yin/SAIC		02/04	added cvg_writtca for TCA_ELM		*
 * B. Yin/SAIC		05/04	copy entire el to tmp_el      	 	*
 * B. Yin/SAIC		07/04	Fixed a bug when grouping TCA  	 	*
 * S. Danz/AWC		07/06	Add ability to send objects to placement*
 * E. Safford/SAIC	06/07	pass lower level cvg error code upward	*
 ***********************************************************************/
{
    int		ier;
    long        elpos;
    VG_DBStruct	*tmp_el;
    Boolean     autopl_enabled, cflError = False;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *  Verify the file pointer.
     */         
    if ( fp == NULL )
    {
        *iret = -47;
	return;
    }

    /*
     *  Check the proper color settings.
     */
    if ( el->hdr.maj_col < 0 || el->hdr.maj_col > 32 || 
	 el->hdr.maj_col < 0 || el->hdr.min_col > 32 )  {
	*iret = -30;	
	return; 
    } 

    /* 
     *  Seek to the specified location.
     */
    if ( start == -2 ) { 
	ier = 0;
    }
    else if ( start == -1 ) { 
        cfl_seek( fp, 0, SEEK_END, &ier ); 
    }
    else {
       cfl_seek( fp, (long)start, SEEK_SET, &ier );
    }

    if ( ier != 0 )  {
        *iret = -3;
	return; 
    }

    /*
     * If the caller is asking to include placement, see if its even on
     * if not, or if we haven't initialized placement, don't bother
     */
    if (inc_place) {
        ctb_pfbool( "ENABLE_AUTOPLACE", &autopl_enabled, &ier );
        if (autopl_enabled && cvg_metadata) {
            elpos = ftell(fp);
            cvg_el2place(elpos, el, iret);
        }
    }

    /*
     *  Swap bit order of VG elements if necessary.
     *  Use tmp_el to avoid actually changing data in el when swapping.
     */
    if ( MTMACH == MTULTX ||
    	 MTMACH == MTALPH ||
	 MTMACH == MTLNUX ) {
        tmp_el = (VG_DBStruct *) malloc ( sizeof(VG_DBStruct) );
        memcpy ( tmp_el, el, sizeof(VG_DBStruct) );

        
	if ( numbytes == sizeof( VG_HdrStruct ) ) {
 	    cvg_swap( SWPHDR, G_FALSE, *tmp_el, tmp_el, &ier );  
	} 
 	else {
	    cvg_swap( SWPALL, G_FALSE, *tmp_el, tmp_el, &ier );  
	} 

	if ( el->hdr.vg_type == GFA_ELM ) {
	    cvg_writgfa ( fp, tmp_el, &ier );
	}
	else if (   ( el->hdr.vg_type == TCA_ELM ) 
		 && ( numbytes != sizeof( VG_HdrStruct ) ) ) {
	    cvg_writtca ( fp, tmp_el, &ier );
	}
	else {
	    cfl_writ( fp, numbytes, (unsigned char *)tmp_el, &ier);
	    if( ier < 0 ) {
		cflError = True;
            }	
	}

	free(tmp_el);
    }	
    else {
	if ( el->hdr.vg_type == GFA_ELM ) {
	    cvg_writgfa ( fp, el, &ier );
	}
	else if (   ( el->hdr.vg_type == TCA_ELM )
                 && ( numbytes != sizeof( VG_HdrStruct ) ) ) {
	    cvg_writtca ( fp, el, &ier );
	}
	else {
	    cfl_writ( fp, numbytes, (unsigned char *)el, &ier);
  	    if( ier < 0 ) {
		cflError = True;
            }	 
        }
    }

    if( ier < 0 ) {
        if( !cflError ) {
	    *iret = ier;
	}
	else {
	    *iret = -17;
	}
    }

}
