#include "cvgcmn.h"

void cvg_crvgf ( char *fname, int *iret )
/************************************************************************
 * cvg_crvgf								*
 *									*
 * This function creates a new vector graphics file. 			*
 *									*
 * cvg_crvgf ( fname, iret )						*
 *									*
 * Input parameters:							*
 *	*fname		char		Name of file to create		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 **									*
 * Log:									*
 * E. Wehner/EAi	10/96	Created					*
 * D. Keiser/GSC	 1/97	Clean up				*
 * E. Wehner/EAi	 6/97	Add a header to all VGFs		*
 * E. Wehner/EAi	 9/97	Handle missing file name		*
 * T. Piper/GSC		10/98	Prolog update				*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int		ier;
    FILE	*fp;
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 *  Create and open the file for write access.
 */
    if (!fname)
    {
	*iret = -47;
	return;
    }
        
    fp = (FILE *) cfl_wopn( fname, &ier );
    if ( ( ier != 0 ) || ( fp == NULL ) ) {
	*iret = -1;
	return;
    }

   

/*
 *  Close the file.
 */
    cfl_clos( fp, &ier );
    if ( ier != 0 )
	*iret = -2;

    cvg_svfhed(fname, &ier);

}



