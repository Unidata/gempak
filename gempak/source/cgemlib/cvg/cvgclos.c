#include "cvgcmn.h"

void cvg_clos ( FILE *fp, int *iret )
/************************************************************************
 * cvg_clos								*
 *									*
 * This function closes a VG file.					*
 *									*
 * cvg_clos ( fp, iret )						*
 *									*
 * Input parameters:							*
 *	*fp		FILE		File pointer			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -2 = error closing VG file	*
 *					 -8 = file is not open		*
 **									*
 * Log:									*
 * D. Keiser/GSC	 1/97	Copied from UTF_CLOS			*
 * A. Hardy/GSC		 1/01	Changed fp to equal NULL		*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    int			ier;

/*---------------------------------------------------------------------*/
    *iret = 0;

/*
**  Close the VG file.
*/
    if ( fp == NULL )
	*iret = -8;
    else {
	cfl_clos(fp, &ier);
	if ( ier != 0 )
	    *iret = -2;
    }
}
