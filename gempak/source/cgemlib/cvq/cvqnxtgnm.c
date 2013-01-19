#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"

void cvq_nxtgnm ( char *fname, char grptyp, int *grpnum, int *iret )
/************************************************************************
 * cvq_nxtgnm								*
 *									*
 * This function returns the next group number above the current 	*
 * group number for the requested group type.				*
 *									*
 * cvq_nxtgnm ( fname, grptyp, grpnum, iret )				*
 *									*
 * Input parameters:							*
 *	*fname		char		Name of file to load		*
 *	grptyp		char		Group type to return		*
 *									*
 * Input/Output parameters:						*
 *	*grpnum		int		Number of group set to new #	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = Error opening VG file	*
 *					 -2 = Error closing VG file	*
 *					 -5 = VG file is empty		*
 *					 -6 = Group type/number not set *
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/97	Created					*
 * E. Wehner/EAi	 7/97	Added file close			*
 * F.J. Yen/NCEP	10/97	Correct return codes (different errors	*
 *				had same return code of -1; changed one	*
 *				to -6).  Add return code for error in	*
 *				closing VG file.			*
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
    int		nextgnum;
    int		quit;
/*---------------------------------------------------------------------*/

    strcpy(grp, "CVG");
    *iret = 0;
    maxbytes = 0;

    if ((grptyp <= 0) || (*grpnum < 0) )
    {
        *iret = -6;
        return;
    }

    


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

    nextgnum = (*grpnum);
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
	  if (el.hdr.grptyp == grptyp)
	  {

	    /*
	     * for the element to be of interest, it has to
	     * be above the currently desired group number
	     */
	    if (el.hdr.grpnum > (*grpnum))
	    {

		/*
		 * if the currently used group number is equal to the 
		 * "next" group number, then we haven't processed any
		 * higher group numbers than the one passed in.  OR
		 * if the current elements group number is below the 
	 	 * group number we processed last time in the loop.
		 * In both of these cases, store the group number.
 		 */
		if ( ((*grpnum) == nextgnum) || (el.hdr.grpnum < nextgnum) )
		{
		    nextgnum = el.hdr.grpnum;
		}
	    }
	  }
	  fpos += el.hdr.recsz;
	
	}
	else
	{
 	    quit = 1;
	}

    }

    (*grpnum) = nextgnum;

    cfl_clos( fp, &ier);
    if ( ier != 0 )
         *iret = -2;

    return;

}
