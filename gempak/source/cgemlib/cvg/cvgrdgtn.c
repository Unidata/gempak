#include "cvgcmn.h"

void cvg_rdgtn ( char *fname, FILE *fp, long *size, int istoff,
                 char gptyp, char vgclss, char vgnum, int sbtyp, 
		 int *ieloff, int *gpnum, int *iret )
/************************************************************************
 * cvg_rdgtn								*
 *									*
 * This function scans the element headers in a VG file for group type, *
 * vg_class and vg_type matching the input values.  When a match is     *
 * found, the group number is returned. Scanning starts at ISTOFF and   *
 * the position of the element after the matched element is returned.   *
 *									*
 * cvg_rdgtn (fname, fp, size, istoff, gptyp, vgclss, vgnum, sbtyp,     *
 *            ieloff, gpnum, iret)					* 
 *									*
 * Input parameters:							*
 *	*fname		char		File name			*
 *	*fp		FILE 		File pointer			*
 *	*size		long		Size of VG file			*
 *	istoff		int		Starting offset number		*
 *      gptyp		char		Input group type		*
 *	vgclss		char		Group type class to compare	*
 *	vgnum 		char		Group type num. to compar	*
 *	sbtyp		int		Group subtype to compare	*
 *									*
 * Output parameters:							*
 * 	*ieloff		int		Offset num. after found element *
 *	*gpnum		int		Group type number		*
 *	*iret		int		Return code			*
 *					-14 = error reading VG element	*
 *									*
 **									*
 * Log:									*
 * A. Hardy/SAIC	11/01	created					*
 * A. Hardy/SAIC	 1/02	initialized gpnum			*
 * A. Hardy/SAIC	 2/02	changed iret -> ier for cvg_subtyp	*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * J. Wu/SAIC		10/04	free GFA blocks & TCA pointers		*
 ***********************************************************************/
{
    int		found;
    int		ier, level;
    int		ier1, flag, curpos, subtyp;
    char	grp[4];
    VG_DBStruct el;
/*---------------------------------------------------------------------*/
    *iret  = 0;
    *gpnum = 0;
    level  = 4;
    found  = False;
    strcpy(grp, "CVG");

   /*
    *  Read the VG header.
    */
    curpos = istoff;
    while (!found && ( (long)curpos <= *size ) ) {
        cvg_rdhdr (fname, fp, curpos, (int)*size, &el, &flag, &ier);
        if ( ier != 0 ) {
	    *iret = ier;
	    return;
        }

       /*
        *  Read the VG element.
        */
        cvg_rdele(&el, curpos, el.hdr.recsz, fp, &ier);
        if (ier < 0) {
            er_lmsg (&level, grp, &ier, fname, &ier1, strlen(grp), 
	               strlen(fname));
	    *iret = -14;
	    return;
        }

       /*
        * Check the group type, VG number and class of the element.
	*/

        if ( ( el.hdr.grptyp   == gptyp ) && 
	     ( el.hdr.vg_type  == vgnum ) && 
	     ( el.hdr.vg_class == vgclss ) ) {
              
           /*
	    * Check the subtype of the element.
	    */
	    
	    if ( sbtyp > 0 ) {
	        cvg_subtyp( &el, &subtyp, &ier );
		if ( sbtyp == subtyp ) {
                    found  = True;
	            *gpnum = el.hdr.grpnum;
		}
	    }
	    else {
                found  = True;
	        *gpnum = el.hdr.grpnum;
	    }
	}
        curpos += el.hdr.recsz;
	if ( found )  {
	    *ieloff =  curpos;
	}
	else {
	    *ieloff = IMISSD;
	}
    
	/*
          * Free TCA/GFA memory
          */
        if ( el.hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( &el );
        }
	else if ( el.hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( &el );
	}    
    }
}
