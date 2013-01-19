#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"

void cvq_higp ( char *fname, int *grpnum, int *iret )
/************************************************************************
 * cvq_higp								*
 *									*
 * This function returns the highest group number in use in the 	*
 * specified VGF file.							*
 *									*
 * cvq_higp ( fname, grpnum, iret )					*
 *									*
 * Input parameters:							*
 *	*fname		char		Name of file to load		*
 *									*
 * Input/Output parameters:						*
 *	*grpnum		int		Number of group set to new #	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					 -5 = VG file is empty		*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97	Created					*
 * F. J. Yen/NCEP       10/97   Add return code of -2 for error in	*
 *                              closing VG file.  Correct description	*
 *				of return code for -1.			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * T. Piper/GSC		10/98	Prolog update				*
 ***********************************************************************/
{
    int 	ier, flag;
    long	maxbytes;
    char 	newfil[256];
    VG_DBStruct	el;
    FILE	*fp;
    char        grp[4];
    int		fpos;
    int		highest;
    int		quit;
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

    highest = 0;
    fpos = 0;
    quit = 0;
    while ( ((long)fpos < maxbytes) && (!quit))
    {

	/*
	 *  	Read the VG header.
	 */
	cvg_rdhdr(newfil, fp, fpos, (int)maxbytes, &el, &flag, &ier);

	/* 
	 * if this element is in the same type of group, check to
	 * see if the group number is above that which has already
	 * been processed...
	 */
	if ((el.hdr.recsz >0) && (el.hdr.recsz < 65536)  ) 
	{

	    /*
	     * for the element to be of interest, it has to
	     * be above the current highest group number
	     */
	    if (el.hdr.grpnum > highest)
	    {

		highest = el.hdr.grpnum;
	    }
	    fpos += el.hdr.recsz;
	
	}
	else
	{
 	    quit = 1;
	}

    }

    (*grpnum) = highest;

    cfl_clos( fp, &ier);
    if ( ier != 0 )
	 *iret = -2;
    return;

}
