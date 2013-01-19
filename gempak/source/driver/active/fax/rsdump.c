#include "faxcmn.h"

void rsdump ( int *iret )
/************************************************************************
 * rsdump								*
 *									*
 * This function writes the contents of a raster image to a file.	*
 *									*
 * rsdump ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/96	Created					*
 * S. Jacobs/NCEP	 7/97	Cleaned up header and global variables	*
 * S. Jacobs/NCEP	 7/97	Removed unnecessary include files	*
 ***********************************************************************/
{

    FILE	*fp;
    int		ier;

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    fp = cfl_wopn ( filnam, &ier );
    if  ( ( fp == NULL ) || ( ier < 0 ) )  {
         *iret = G_NOWROPN;
         return;
    }

    cfl_writ ( fp, msize, rasimg, &ier );

    cfl_clos ( fp, &ier );

}
