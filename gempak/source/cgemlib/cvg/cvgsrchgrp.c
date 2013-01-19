#include "cvgcmn.h"

void cvg_srchgrp ( char *vfname, FILE *fp, long *size, char gptyp,
                   int gpnum, int maxoff, int members[], int *numingrp,
                   int *iret )
/************************************************************************
 * cvg_srchgrp								*
 *									*
 * This function scans a vector graphics file and returns elements that	*
 * are members of a particular group.  The elements are returned as 	*
 * offsets into the file.						*
 *									*
 * cvg_srchgrp ( vfname, fp, size, gptyp, gpnum, maxoff, members, 	*
 *		 numingrp, iret )					*
 *									*
 * Input parameters:							*
 *	*vfname		char	VG filename				*
 *      *fp	        FILE    Pointer to file				*
 *      *size		long    Size of VG file				*
 *	gptyp		char	Group type				*
 *	gpnum		int	Group number				*
 *	maxoff		int	Maximum number of offsets per group	*
 *									*
 * Output parameters:							*
 *	members[]	int	Member elements				*	
 *	*numingrp	int	Number in group				*
 *	*iret		int	Return code				*
 *				-13 = error reading VG header		*
 *									*
 **									*
 * Log:									*
 * A. Hardy/SAIC	11/01   Created					*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{

    int		quit, flag, level, curpos, ier, ier1;
    char	grp[4];
    VG_DBStruct el;

/*---------------------------------------------------------------------*/
    *iret       = 0;
    quit        = 0;
    level       = 4;
    curpos      = 0;
    (*numingrp) = 0;
    strcpy(grp, "CVG");

   /* 
    * Read the VG head and do while more data to process for this 
    * particular group; 
    */

    while (!quit) {
	cvg_rdhdr(vfname, fp, curpos, (int)*size, &el, &flag, &ier);
	if ( ier != 0 ) {
	    er_lmsg ( &level, grp, &ier, vfname, &ier1,
			   strlen(grp), strlen(vfname) );
	    *iret = -13;
	    return;
	}

       /* 
	* examine the group type and group number to see if it matches
	*/

	if ( (gptyp == el.hdr.grptyp) && ( gpnum == el.hdr.grpnum ) ) {
	   /* 
	    * if the group matches, add this to the members array, and 
	    * keep looking for member elements 
	    */
	    members[(*numingrp)] = curpos;
	    (*numingrp)++;
 	    if ((*numingrp) > maxoff) {
		quit = G_TRUE;
	    }
	}
	curpos += el.hdr.recsz;

	if ((flag) || ((long)curpos >= *size) ) {
	    quit = G_TRUE;
	}
    }
}
