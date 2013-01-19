#include "cvgcmn.h"
#include "pgprm.h"
#include "drwids.h"


void cvg_fscan ( char *fname, int layer, char class, char sel_grpd, 
			        int fpts, float fx[], float fy[], 
		 		int *num_sel, int offsets[], int *iret )
/************************************************************************
 * cvg_fscan								*
 *									*
 * This function scans a vector graphics file looking for all elements  *
 * that match the vg_class/layer input and lie within the figure 	*
 * described by coordinate point arrays fx and fy.			*
 *									*
 * The figure described by fx and fy MUST BE CLOSED.			*
 *									*
 * cvg_fscan ( fname, layer, class, sel_grpd, fpts, fx, fy,		*
 *	       num_sel, offsets, iret )					*
 *									*
 * Input parameters:							*
 *	*fname		char		Name of file to scan from	*
 *	layer		int		layer to test against		*
 *	class		char		vg_class to test against	*
 *	sel_grpd	char		search for grpd or non-grpd elms*
 *	fpts		int		number of coordinate pts	*
 *	fx []		float		X coordinates to test		*
 *	fy []		float		Y coordinates to test		*
 *									*
 * Output parameters:							*
 *	*num_sel	int		number of offsets		*
 * 	offsets []	int		Offsets to selected elements	*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					 -6 = element not found		*
 *					-13 = error reading VG header	*
 *					-14 = error reading VG element	*
 **									*
 * Log:									*
 * E. Safford/GSC	11/98	initial coding, copied cvg_scan		*
 * E. Safford/GSC	11/98	add dsply_grpd param           		*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * J. Wu/SAIC         	01/02   add layer param   			*
 * R. Tian/SAIC		07/02	removed dsply_grpd and related codes	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * J. Wu/SAIC		07/04	add filter param to crg_get()		*
 * B. Yin/SAIC          08/04   added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC		10/04	free GFA block pointers			*
 ***********************************************************************/
{
int		location, ii, jj, np, ier, level, sel;
long		size;
int		el_layer, flag, in_out[MAXPTS], intersect, grp_num; 
float		llx, lly, urx, ury, dx[MAXPTS], dy[MAXPTS], xp[5], yp[5];
char		grp[4], newfil[133], grpd_ok, grp_typ;
char		reqfil[133], cclass, ctype;
VG_DBStruct	el;
FILE		*fp;
filter_t	filter;
/*---------------------------------------------------------------------*/

    *iret = 0;
    sel = 0;
    strcpy(grp, "CVG");
    level = 2;

    if ( !fname) {
	strcpy(reqfil, work_file);
    }
    else { 
	strcpy(reqfil, fname);
    }

    /* 
     *  Inquire the size of the VG file and open the file for read.
     */
    cfl_inqr(reqfil, NULL, &size, newfil, &ier);
    fp = (FILE *) cfl_ropn(newfil, NULL, &ier);
    if (( ier != 0) || (fp == NULL)) {
	*iret = -1;
	return;
    }

    /* 
     *  Look at the VG header to see which elements are in range.
     */
    cclass = class;
    for (ii=0; ii< MAX_EDITABLE_ELEMS; ii++) {
	crg_goffset(ii, &location, &ier);
	if (location < 0) {
	    continue;
	}

	/*
	 *  check range record for layer, location and grouped status 
	 */
	crg_get(ii, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);
  	if (ier < 0 || (el_layer != layer) ) { 
	    continue;
	}
	else {
	    /* 
	     *  Compare required group status and element group status
	     *  If in agreement, grpd_ok is set to true.
	     */
	    crg_gtyp (ii, &cclass, &ctype, &ier);
	    if (sel_grpd) {
		crg_ggrp (ii, &grp_typ, &grp_num, iret);
		grpd_ok = (grp_typ)? 1 : 0;
	    }
	    else {
		grpd_ok = 1;
	    }
  	}

	/*
	 *  Proceed to process element if class matches or is irrelevent
	 *  and group assigment matches requested grouped status.
	 */
	if ((cclass == class || class == CLASS_ANY) && grpd_ok) {

	    xp[0] = xp[3] = xp[4] = llx;	
	    xp[1] = xp[2] = urx;
	    yp[0] = yp[1] = yp[4] = lly;	
	    yp[2] = yp[3] = ury;

  	    np = 5;
  	    cgr_polyint (sys_D, &np, xp, yp, sys_D, &fpts, fx, fy, 
	    		  &intersect, &ier); 
	    if (ier < 0)
	        continue;

	    /*
	     * if element range record intersects search area retrieve
	     * element and determine if any vertex points are actually 
	     * within the search area.
	     */
	    if (intersect) { 
	        /*
	         *  Read the VG header and element.
	         */
	        cvg_rdhdr (newfil, fp, location, (int)size, &el, &flag, &ier);
	        if (ier != 0) {
	   	    er_lmsg (&level, grp, &ier, newfil, &ier,
			 strlen(grp), strlen(newfil));
	    	    *iret = -13;
		    return;
	        }

		/*
		 *  If the element is in a group and sel_grpd is true, then
		 *  include all elments of the group within offsets list
		 */
	        cvg_rdele(&el, location, el.hdr.recsz, fp, &ier);
	        if (ier < 0) {
		    er_lmsg (&level, grp, &ier, newfil, &ier,
		    strlen(grp), strlen(newfil));
		    *iret = -14;
		    return;
	        }

	        cvg_todev( &el, &np, dx, dy, &ier);
	        cgr_inpoly(sys_D, &np, dx, dy, sys_D, &fpts, fx, fy, in_out, &ier);

                /*
                 * Free TCA break point/GFA block memory
                 */
                if ( el.hdr.vg_type == TCA_ELM ) {
                   cvg_freeBkpts ( &el );
                }
                else if ( el.hdr.vg_type == GFA_ELM ) {
                    cvg_freeElPtr ( &el );
	        }
		
		
		/*
		 *  Load the location into the array of offsets that fall 
		 *  within the search area.
		 */
	        for (jj=0; jj<np; jj++) {
		    if (in_out[jj]) {
		        offsets[sel] = location;	
		        sel++;
		        break;
		    }
 	        }
	    }	/* endif intersect */
	}  /* endif correct class */
    }   /* end for loop for each slot in range record. */

    *num_sel = sel;
    
    cfl_clos(fp, &ier);
    if ( ier != 0 ) *iret = -2;
}
