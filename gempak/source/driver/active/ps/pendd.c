#include "pscmn.h"

void pendd ( int *ieop, int *iret )
/************************************************************************
 * PENDD								*
 *									*
 * This subroutine closes the PostScript output file.			*
 *									*
 * PENDD  ( IEOP, IRET )						*
 *									*
 * Input parameters:							*
 *	ieop		int*		End plotting flag		*
 *					  0 = retain subprocess		*
 *					  1 = stop subprocess		*
 *									*
 * Output parameters:							*
 *	iret		int*		Return code			*
 **									*
 * Log:									*
 * A. Chang/EAI		 2/94						*
 ***********************************************************************/
{

/*--------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Close file at end of plot.
 */
	if ( *ieop == 1 )  pclosp ( iret );

}
