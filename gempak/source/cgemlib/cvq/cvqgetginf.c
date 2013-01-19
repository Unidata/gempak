#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"

void cvq_getginf ( char *fname, int fpos, char *grptyp, 
						int *grpnum, int *iret )
/************************************************************************
 * cvq_getginf								*
 *									*
 * This function returns the group type & number for the element 	*
 * at the requested file position.					*
 *									*
 * cvq_getginf  ( fname, fpos, grptyp, grpnum, iret )			*
 *									*
 * Input parameters:							*
 *	*fname		char		Name of file to load		*
 *	fpos		int		Position in file 		*
 *									*
 * Output parameters:							*
 *	*grptyp		char		Group type for the element	*
 *	*grpnum		int		Group number for the element	*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					 -5 = VG file is empty		*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/97	Created					*
 * F. J. Yen/NCEP	10/97	Add closing of VG file.			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 ***********************************************************************/
{
    int 	ier, flag;
    long	maxbytes;
    char 	newfil[256];
    VG_DBStruct	el;
    FILE	*fp;
    char        grp[4];
/*---------------------------------------------------------------------*/

    strcpy(grp, "CVG");
    *iret = 0;
    maxbytes = 0;

/*  Check to see if there is a file of the specified name, 
 *  open it if it is there.
 */    
    cfl_inqr(fname, NULL, &maxbytes, newfil, &ier);

/*
 *  If empty file just return, otherwise, attempt to open and
 *  open it if it is there.
 */

    if (maxbytes == 0)
    {
	*iret = -5;
        return;
    }
    else
    {
	fp = (FILE *) cfl_ropn(newfil, NULL, &ier);
	if ( ( ier != 0 ) || ( fp == NULL ) )
        {
	    *iret = -1;
            return;
        }
    }

/*
 *  	Read the VG header.
 */
    cvg_rdhdr(newfil, fp, fpos, (int)maxbytes, &el, &flag, &ier);


/*
 *    If the number of bytes is valid, continue.
 */
    if ((el.hdr.recsz >0) && (el.hdr.recsz < 65536)  ) 
    {
        (*grptyp) = el.hdr.grptyp;
	(*grpnum) = el.hdr.grpnum;
	
    }
    else
    {
	*grptyp = 0;
    }

    cfl_clos(fp, &ier);
    if ( ier != 0 )
         *iret = -2;

    return;

}
