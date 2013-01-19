#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"

void cvq_scangrp ( char *vfname, char gptyp, int gpnum, int maxoff,
                   int members[], int *numingrp, int *iret )
/************************************************************************
 * cvq_scangrp								*
 *									*
 * This function scans a vector graphics file and returns elements that	*
 * are members of a particular group.  The elements are returned as 	*
 * offsets into the file.						*
 *									*
 * cvq_scangrp ( vfname, gptyp, gpnum, maxoff, members, numingrp, iret )*
 *									*
 * Input parameters:							*
 *	*vfname		char	VG filename				*
 *	gptyp		char	Group type				*
 *	gpnum		int	Group number				*
 *	maxoff		int	Maximum number of offsets per group	*
 *									*
 * Output parameters:							*
 *	members[]	int	Member elements				*	
 *	*numingrp	int	Number in group				*
 *	*iret		int	Return code				*
 *				 -1 = error opening VG file		*
 *				 -2 = error closing VG file		*
 *				 -6 = Group number/type not set		*
 *				-13 = error reading VG header		*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/97	Created					*
 * E. Wehner/Eai	 7/97	Added file close			*
 * F. J. Yen/NCEP	10/97	Added return code for error		*
 *				in closing VG file and correct		*
 *				return code description			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * T. Piper/GSC		10/98	Prolog update				*
 * A. Hardy/SAIC	11/01   Add maxoff to call sequence; cvgsrchgrp *
 ***********************************************************************/
{

    int		ier;
    long	size;
    char	newfil[133], grp[4];
    FILE	*fp;
/*---------------------------------------------------------------------*/
    *iret = 0;
    strcpy(grp, "CVQ");

    /* 
     * Check to see that the group type and group numbers are set.  
     * If they are not, return.

    if ((gptyp == 0) || (gpnum == 0))
    {
        *iret = -6;
        return;
    }

     */

   /* 
    *  Inquire the size of the VG file and open the file for update.
    */

    cfl_inqr(vfname, " ", &size, newfil, &ier);
    fp = (FILE *) cfl_ropn(vfname, NULL,&ier);
    if ( ( ier != 0 ) || ( fp == NULL ) ) 
    {
	*iret = -1;
	return;
    }

    /* 
     * Scan VG file for elements of the same group type and group number.
     */

     cvg_srchgrp (vfname, fp, &size, gptyp, gpnum, maxoff, members, 
                  numingrp, &ier );

     cfl_clos(fp, &ier);
     if ( ier != 0 )
         *iret = -2;

}
