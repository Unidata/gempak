#include "nccmn.h"

void mscolr ( int *icolr, int *iret )
/************************************************************************
 * mscolr								*
 * 									*
 * This subroutine sets the color in the metafile.			*
 * 									*
 * mscolr  ( icolr, iret )						*
 * 									*
 * Input parameters:							*
 *	*icolr		int		Color number			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Chang/EAI		03/94						*
 * A. Chang/EAI		05/94		Added code to check icolcur	*
 * S. Jacobs/NMC	 6/94		General clean up		*
 * L. Williams/EAI	 7/94		Reformat header			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	lcolor_req = *icolr;
	fcolor_req = *icolr;
}
