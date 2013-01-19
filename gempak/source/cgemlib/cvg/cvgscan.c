#include "cvgcmn.h"
#include "pgprm.h"
#include "drwids.h"

/*
 *  Private functions
 */
static void cvg_scangfabox (FILE *fp, int layer, float fx, float fy, 
		int start, long size, float *mindist, int *sel_elm, int *iret );


/*
 *  Public functions
 */
void cvg_scan ( char *fname, int layer, char class, float fx, float fy, 
		int start, VG_DBStruct *el, int *selected, int *nearest, 
								int *iret )
/************************************************************************
 * cvg_scan								*
 *									*
 * This function scans a vector graphics file looking for elements that	*
 * match the layer/vg_class input.  The first closest matching record 	*
 * is returned in "el".							*
 *									*
 * cvg_scan ( fname, layer, class, fx, fy, start, el, selected, 	*
 *            nearest, iret)						*
 *									*
 * Input parameters:							*
 *	*fname		char		Name of file to scan from	*
 *	layer		int		layer to test against		*
 *	class		char		vg_class to test against	*
 *	fx		float		X coordinate to test		*
 *	fy		float		Y coordinate to test		*
 *	start		int		Offset to start of VG record	*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 * 	*selected	int		Offset to selected element	*
 *	*nearest	int		Nearest point on elelment	*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					 -6 = element not found		*
 *					-13 = error reading VG header	*
 *					-14 = error reading VG element	*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	10/96		Created				*
 * D. Keiser/GSC	 1/97		Clean up			*
 * E. Wehner/EAi	2/97	cgr_distance -> cgr_dist		*
 * E. Wehner/Eai	5/97	Ability to scan by class		*
 * E. Wehner/Eai	5/97	dont process points if elem. unknown	*
 * E. Wehner/EAi	8/97	Remove range record from grInfo		*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * E. Wehner/EAi	9/97	Killed grinfo record			*
 * F. J. Yen/NCEP	11/97   Removed gtrans definition.  Replaced	*
 *				default directory " " with NULL when	*
 *				invoking cfl_inqr.			*
 * G. Krueger/EAI	 1/98	Ignore non-fatal read warnings.		*
 * F. Yen/NCEP           1/98   Updated calls for crg library cleanup   *
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Law/GSC		05/98	Cleaned up.  Added call crg_gtyp	*
 * T. Piper/GSC		10/98	Prolog update				*
 * S. Law/GSC		03/99	Changed cgr_dist to cgr_segdist		*
 * J. Wu/SAIC		01/02	add layer param				*
 * W.D.Plummer/NCEP	12/02	chg call sequence of cgr_segdist	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * J. Wu/SAIC		01/04	repeat first point of a closed element	*
 *                              to the end for correct ele. selection	*
 * J. Wu/SAIC		07/04	add display filter			*
 * B. Yin/SAIC		07/04	Added code to free TCA memory		*
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC		10/04	free GFA block memory			*
 * J. Wu/SAIC		06/06	call cvg_matchfilter 			*
 * M. Li/SAIC           03/07   Updated cvg_matchfilter                 *
 * M. Li/SAIC           04/07   Initialize matchAny			* 
 * J. Wu/SAIC		07/07	read second range records first		*
 ***********************************************************************/
{
    int		location, second, clsbox;
    float	llx,lly,urx,ury, boxdst;
    int		ii, closest_offset, np, ier, level, el_layer;
    int		ier1, flag;
    long	size;
    int		hot_pt;	/* hot point -- which segment was the closest */
    float	closest_distance, distance;
    float	dx[MAXPTS], dy[MAXPTS], nx, ny;
    char	grp[4], newfil[133];
    char	reqfil[133], cclass, ctype, tclass;
    FILE	*fp;
    filter_t	el_filter, timeMatched;
    Boolean	filter_match, matchAny = False;
/*---------------------------------------------------------------------*/
    *iret = 0;
    closest_distance = 99999.0F;
    closest_offset = -1;
    strcpy(grp, "CVG");
    level = 2;
    if ( !fname) {
	strcpy(reqfil, work_file);
    }
    else { 
	strcpy(reqfil, fname);
    }

    /* 
     *  Inquire the size of the VG file and open the file for update.
     */
    cfl_inqr(reqfil, NULL, &size, newfil, &ier);
    fp = (FILE *) cfl_uopn(newfil, &ier);
    if (( ier != 0) || (fp == NULL)) {
	*iret = -1;
	return;
    }
    
    
    /* 
     *  Look at range records from GFA text boxs first.
     */
    if ( class == CLASS_ANY || class == CLASS_MET ) {
        cvg_scangfabox ( fp, layer, fx, fy, start, size, 
    			 &boxdst, &clsbox, &ier );
    
        if ( ier < 0 ) {        
            return;
        }    
    
        if ( clsbox >= 0 ) {
            *nearest = 0;
            closest_distance = boxdst;
            closest_offset = clsbox;
        }
    }
    

    /* 
     *  Look at the VG header to see if any elements are in range.
     */
    cclass = class;
    
    for (ii=0; ii< MAX_EDITABLE_ELEMS; ii++) {

	if ( crg_isauxrec( ii, &ier ) ) {
	    continue;	
	}
	
	crg_goffset(ii, &location, &ier);
	if (location < 0) {
	    continue;
	}

	crg_get(ii, &el_layer, el_filter, &llx, &lly, &urx, &ury, &ier);
	if (ier >= 0 && class != CLASS_ANY) {
	    crg_gtyp (ii, &cclass, &ctype, &ier);
	}	
	
	if ( ier < 0 ) {
	    continue;
	}
	
	crg_gtyp ( ii, &tclass, &ctype, &ier );
	if ( ctype == GFA_ELM && clsbox >= 0 ) {
	    continue;    
	}
	
	 
	if ( (el_layer == layer) && (class == cclass) &&
	    (llx <= fx) && (fx <= urx) &&
	    (lly <= fy) && (fy <= ury)) {
	    
	    /*
	     *  Check if the element matches the given filter.
	     */
            cvg_matchfilter ( el_filter, matchAny, &filter_match, timeMatched, &ier );
	    
	    if ( !filter_match ) {
		continue;
	    }
	    
	    /*
	     *  Read the VG header.
	     */
	    cvg_rdhdr (newfil, fp, location, (int)size,
		       (void *)el, &flag, &ier);
	    if (ier != 0) {
		er_lmsg (&level, grp, &ier, newfil, &ier1,
			 strlen(grp), strlen(newfil));
		*iret = -13;
		return;
	    }

	    /*
	     *  Read the VG element.
	     */
	    cvg_rdele(el, location, el->hdr.recsz, fp, &ier);
	    if (ier < 0) {
		er_lmsg (&level, grp, &ier, newfil, &ier1,
			 strlen(grp), strlen(newfil));
		*iret = -14;
		return;
	    }

	    /*
	     * Convert the element vertices to D coords.
	     */
	    cvg_todev( el, &np, dx, dy, &ier);

	    /*
	     * For closed elements, repeat the first point to the end. 
	     * Otherwise, click near the last segment may not result
	     * in a correct selection.   
	     */
	    if ( el->hdr.closed && np < MAXPTS ) {
	        dx[np]=dx[0];
	        dy[np]=dy[0];
	        np++;	    
            }
	    
	    /*
	     *  Check distance to the line segment.
	     */
	    if (ier == 0) {
		cgr_segdist(&np, dx, dy, &fx, &fy, &distance, &hot_pt, 
			    &second, &nx, &ny, &ier);
		if (distance <= closest_distance) {
		    *nearest = hot_pt;
		    closest_distance = distance;
		    closest_offset = location;
		}
	    }

	    /* 
	     * Free TCA break point/GFA block memory
	     */
            if ( el->hdr.vg_type == TCA_ELM ) {
               cvg_freeBkpts ( el );
            }
            else if ( el->hdr.vg_type == GFA_ELM ) {
               cvg_freeElPtr ( el );
            }
	    	    	
	}	/* endif in range and correct class */      
    }	/* end for loop for each slot in range record. */
    

    /*
     *	    Save the file position of the element that was selected.
     */
    if (closest_offset >= 0) {
	*selected = closest_offset;

	/*
	 *	    Read the VG header.
	 */
	cvg_rdhdr(newfil, fp, closest_offset, (int)size, (void *)el,
		  &flag, &ier);
	if (ier != 0) {
	    er_lmsg (&level, grp, &ier, newfil, &ier1,
		     strlen(grp), strlen(newfil) );
	    *iret = -13;
	    return;
	}

	/*
	 *	    Read the VG element.
	 */
	cvg_rdele(el, closest_offset, el->hdr.recsz, fp, &ier);
	if ( ier < 0 ) {
	    er_lmsg (&level, grp, &ier, newfil, &ier1,
		     strlen(grp), strlen(newfil) );
	    *iret = -14;
	    return;
	}		    
    }
    else {
	*iret = -6;
    }

    cfl_clos(fp, &ier);
    if ( ier != 0 ) *iret = -2;
}

/*=====================================================================*/

static void cvg_scangfabox ( FILE *fp, int layer, float fx, float fy, 
		int start, long size, float *mindist, int *sel_elm, 
		int *iret )
/************************************************************************
 * cvg_scangfabox							*
 *									*
 * This function scans a VG file looking on a given layer for a GFA 	*
 * element whose text box is the closest to the input click point.	*
 *									*
 * static cvg_scangfabox ( fp, layer, fx, fy, start, size, mindist,	*
 *            		   sel_elm, iret )				*
 *									*
 * Input parameters:							*
 *	*fp		FILE		file pointer to scan from	*
 *	layer		int		layer to test against		*
 *	fx		float		X coordinate to test		*
 *	fy		float		Y coordinate to test		*
 *	start		int		Offset to start of VG record	*
 *	size		long		Size of the file		*
 *									*
 * Output parameters:							*
 * 	*mindist	int		Distance between box & click pt	*
 * 	*sel_elm	int		Offset to selected element	*
 *	*iret		int		Return code			*
 *					-13 = error reading VG header	*
 *					-14 = error reading VG element	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC           07/07   initial coding				* 
 ***********************************************************************/
{
    int		location, flag, ii, np, ier, el_layer;
    
    float	llx, lly, urx, ury, distance;
    float	lat[1], lon[1], dx[1], dy[1];
    
    char	cclass, ctype, value[ 32 ];
    
    filter_t	el_filter, timeMatched;
    
    Boolean	filter_match, matchAny = False;
    VG_DBStruct	tmp_el;
/*---------------------------------------------------------------------*/

    *iret = 0;
    *mindist = 99999.0F;
    *sel_elm = -1;


    /*
     *  Only look for range records for GFA text boxes.
     */    
    for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++ ) {

	/*
	 *  Verify VG type, range record type, and location.
	 */	
	crg_gtyp ( ii, &cclass, &ctype, &ier );	
	if ( ier < 0 || ctype != GFA_ELM ) {
	    continue;	    
	}	
	
	if ( !crg_isauxrec( ii, &ier ) ) {
	    continue;	
	}

	crg_goffset ( ii, &location, &ier );
	if ( location < 0 ) {
	    continue;
	}
	
	
	/*
	 *  Match layer, filter and check if the click point fall within
	 *  the GFA text box.  Then find the distance between the click
	 *  point and the center of the text box.
	 */	
	crg_get ( ii, &el_layer, el_filter, &llx, &lly, &urx, &ury, &ier );	
	if ( ier < 0 ) {
	    continue;
	}
	
	 
	if ( (el_layer == layer) && (llx <= fx) && (fx <= urx) &&
	     (lly <= fy) && (fy <= ury) ) {
	    
            cvg_matchfilter ( el_filter, matchAny, &filter_match, timeMatched, &ier );	    
	    if ( !filter_match ) {
		continue;
	    }
	    	    
	    
	    /*
	      *   Read the GFA element.
	      */
 	    cvg_rdhdr ( "", fp, location, (int)size, &tmp_el, &flag, &ier);
	    if ( ier != 0 ) {
		*iret = -13;
		return;
	    }
	    
	    cvg_rdele ( &tmp_el, location, tmp_el.hdr.recsz, fp, &ier );
	    if ( ier < 0) {
		*iret = -14;
		return;
	    }
	    
	    
            /*
	      *   Find the distance.
	      */
	    np = 1;
            
	    cvg_getFld ( &tmp_el, TAG_GFA_LAT, value, &ier );
            lat[ 0 ] = atof ( value );
        	
	    cvg_getFld ( &tmp_el, TAG_GFA_LON, value, &ier );
	    lon[ 0 ] = atof ( value );
		    
            gtrans ( sys_M, sys_D, &np, lat, lon, dx, dy, &ier, 
	             strlen(sys_M), strlen(sys_D) );
 	    
	    if ( ier == 0 ) {
	        
	        distance = (float) G_DIST ( dx[0], dy[0], fx, fy );
		
		if ( distance <= *mindist ) {
		    *mindist = distance;
		    *sel_elm = location;
		}
	    
            }
	    	    	    
            
	    /*
	      *   Free memory.
	      */
            cvg_freeElPtr ( &tmp_el );
	
	}     
    }	    
}

/*=====================================================================*/

