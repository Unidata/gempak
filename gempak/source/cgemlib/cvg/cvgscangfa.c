#include "cvgcmn.h"
#include "pgprm.h"
#include "drwids.h"

void cvg_scangfa ( char *fname, int layer, int subtype, int areatype,
		   char *tag, VG_DBStruct *el, int *selected, int *iret )
/************************************************************************
 * cvg_scangfa								*
 *									*
 * This function scans a vector graphics file looking for a GFA element	*
 * that matches the layer, subtype, area type (hazard) and tag input.	*
 * The first matching record is returned in "el". Also,			*
 * the element should match one of the active filters.			*
 *									*
 * cvg_scan ( fname, layer, subtype, areatype, tag, el, selected, iret )*
 *									*
 * Input parameters:							*
 *	*fname		char		Name of file to scan from	*
 *	layer		int		layer to test against		*
 *	subtype		int		subtype to test against		*
 *	areatype	int		area type to test against	*
 * 	tag		char		gfa tag to test against		*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 * 	*selected	int		Offset to selected element	*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					 -6 = element not found		*
 *					-13 = error reading VG header	*
 *					-14 = error reading VG element	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/04	initial coding (modified from cvg_scan)	*
 * J. Wu/SAIC		10/04	use cvg_getFld to access GFA attributes	*
 * E. Safford/SAIC	07/05	replace seqnum param with tag		*
 * J. Wu/SAIC		06/06	call cvg_matchfilter 			*
 * M. Li/SAIC           03/07   Updated cvg_matchfilter                 *
 ***********************************************************************/
{
    int		ii, ier, el_layer, location;
    float	llx,lly,urx,ury;
    long	size;
    char	newfil[133], reqfil[133], cclass, ctype, value[32];
    FILE	*fp;
    filter_t    el_filter, timeMatched;
    Boolean     filter_match, matchAny = False;
/*---------------------------------------------------------------------*/

    *iret = 0;
    *selected = -1;
    
    if ( !fname ) {
	strcpy ( reqfil, work_file );
    }
    else { 
	strcpy ( reqfil, fname );
    }

    /* 
     *  Inquire the size of the VG file and open the file for update.
     */
    cfl_inqr ( reqfil, NULL, &size, newfil, &ier );
    fp = (FILE *) cfl_uopn ( newfil, &ier );
    if ( (ier != 0) || (fp == NULL) ) {
	*iret = -1;
	return;
    }

    /* 
     *  Loop through all elements to find a match.
     */    
    for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++ ) {

	crg_goffset ( ii, &location, &ier );
	if ( location < 0 ) {
	    continue;
	}

	crg_get ( ii, &el_layer, el_filter, &llx, &lly, &urx, &ury, &ier );
	if ( ier >= 0 ) {
	    crg_gtyp ( ii, &cclass, &ctype, &ier );
	}	
	
	if ( ier < 0 ) {
	    continue;
	}
	
	
	/*
	 *  Match layer, class, vg type first.
	 */
	if ( (el_layer == layer) && (cclass == CLASS_MET) &&
	     (ctype == GFA_ELM) ) {

	    
	    /*
	     *  Then check if the element matches one of the filters.
	     */
            cvg_matchfilter ( el_filter, matchAny, &filter_match, timeMatched, &ier );
	    
	    if ( !filter_match ) {
		continue;
	    }
            
	    
	    /*
             *  Read the element.
             */
            cvg_rdrecnoc ( newfil, fp, location, el, iret );	    
	    if ( *iret != 0 ) {
	        continue;
	    }
	    
	    
	    /*
             *  Match the subtype, area type, and tag.
             */
	    cvg_getFld ( el, TAG_GFA_SUBTYPE, value, &ier );
	    if ( subtype == atoi(value) ) {
	        cvg_getFld ( el, TAG_GFA_AREATYPE, value, &ier );
	        
  		if ( areatype == atoi(value) ) {
  	            cvg_getFld ( el, TAG_GFA_TAG, value, &ier ); 

		    if( strcmp( tag, value ) == 0 ) {	            
		        *selected = location;
		        break;
  		    } 
		}
	    }
	}	    
    }	


    /*
     *  Check if there is an matching record found.
     */
    if ( *selected < 0 ) {
        *iret = -6;
    }
    
    /*
     *  Close the VG file.
     */
    cfl_clos(fp, &ier);
    if ( ier != 0 )  *iret = -2;

}
