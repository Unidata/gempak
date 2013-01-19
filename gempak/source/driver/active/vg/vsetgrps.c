#include "vgcmn.h"

void vsetgrps ( int *iret )
/************************************************************************
 * vsetgrps								*
 *									*
 * This subroutine gets the next group number for all group types.      *
 *									*
 * vsetgrps ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * D. Kidwell/NCEP	 6/02						*
 * D. Kidwell/NCEP	 6/02	Removed arg jgroup; added kgindx init   *
 ***********************************************************************/
{
	int	ier, ierq;
	int	ii;
	int	grpnum;
	char	igtyp, path[256];
	long	size;
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 * 	Check whether file exists.
 */
  
	cfl_inqr ( curfil, NULL, &size, path, &ierq );

/*
 *	Get the next group number.
 */

	for ( ii = 1; ii < MAX_GROUP_TYPE; ii++ ) {
	    if ( ierq == 0 ) {
		grpnum = 0;
	        igtyp  = (char) ii;
	        cvg_gtgnum ( curfil, flun, igtyp, size, &grpnum, &ier );
	        kgrpns[ii] = grpnum;
	    }
	    else {
	        kgrpns[ii] = 0;
	    }
	    kgindx[ii] = 0;
	}
 
}
