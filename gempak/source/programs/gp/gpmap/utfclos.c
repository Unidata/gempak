#include "geminc.h"
#include "gemprm.h"

void utf_clos ( FILE *fp, int *iret )
/************************************************************************
 * utf_clos								*
 *									*
 * This function closes a UTF file.					*
 *									*
 * utf_clos ( fp, iret )						*
 *									*
 * Input parameters:							*
 *	*fp		FILE		File pointer			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					-11 = file is not open		*
 *					-12 = error closing UTF file	*
 **									*
 * Log:									*
 * D. Keiser/GSC	12/96						*
 ***********************************************************************/
{
    int			ier;

/*---------------------------------------------------------------------*/
    *iret = 0;

/*
**  Close the UTF file.
*/
    if ( fp == 0 )
	*iret = -11;
    else {
	cfl_clos(fp, &ier);
	if ( ier != 0 )
	    *iret = -12;
    }
}
