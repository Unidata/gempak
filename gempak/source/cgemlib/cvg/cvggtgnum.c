#include "cvgcmn.h"

void cvg_gtgnum ( char *fname, FILE *fp, char grptyp, long size,
                  int *grpnum, int *iret )
/************************************************************************
 * cvg_gtgnum								*
 *									*
 * This function returns the next group number above the current 	*
 * group number for the requested group type.				*
 *									*
 * cvg_gtgnum ( fname, fp, grptyp, size, grpnum, iret )			*
 *									*
 * Input parameters:							*
 *	*fname		char            VG file name			*
 *	*fp		FILE		Pointer to open file 		*
 *	grptyp		char		Group type to return		*
 *      size		long		Size of VF file			*
 *									*
 * Input/Output parameters:						*
 *	*grpnum		int		Number of group set to new #	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Hardy/SAIC	11/01	Modification from cvq_nxtgnm		*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int 	ier, flag;
    long	maxbytes;
    VG_DBStruct	el;
    char        grp[4];
    int		fpos;
    int		nextgnum;
    int		quit;
/*---------------------------------------------------------------------*/
    *iret    = 0;
    fpos     = 0;
    quit     = 0;
    maxbytes = size;
    nextgnum = *grpnum;
    strcpy(grp, "CVG");

    while ( ((long)fpos < maxbytes) && (!quit)) {
        
       /*
	* Read the VG header.
	*/

	cvg_rdhdr(fname, fp, fpos, (int)maxbytes, &el, &flag, &ier);

       /* 
	* If this element is in the same type of group, check to
	* see if the group number is above that which has already
	* been processed.
	*/

	if ( ( el.hdr.recsz > 0 ) && ( el.hdr.recsz < 65536 ) ) {
	    if ( el.hdr.grptyp == grptyp ) {

	       /*
	        * For the element to be of interest, it has to
	        * be above the currently desired group number.
	        */
	        if ( el.hdr.grpnum > *grpnum ) {

	           /*
		    * if the currently used group number is equal to the 
		    * "next" group number, then we haven't processed any
		    * higher group numbers than the one passed in.  OR
		    * if the current elements group number is GREATER 
	 	    * THAN the group number we processed last time in 
		    * the loop. In both of these cases, store the group 
		    * number.
 		    */

		    if ( ( *grpnum == nextgnum) || 
		         ( el.hdr.grpnum > nextgnum ) ) {
		        nextgnum = el.hdr.grpnum;
		    }
	        }
	    }

	    fpos += el.hdr.recsz;

	}
	else {
 	    quit = 1;
	}

    }

    *grpnum = nextgnum;

    return;

}
