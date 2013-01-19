#include "nccmn.h"

void mslwid ( int *ilwid, int *iret )
/************************************************************************
 * mslwid								*
 * 									*
 * This subroutine sets the hardware line width for the metafile.	*
 *									*
 * mslwid  ( ilwid, iret )						*
 *									*
 * Input parameters:							*
 * 	*ilwid		int		Line width			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Chang/EAI	 	 4/94						*
 * A. Chang/EAI	 	 5/94		Check for line width change	*
 * S. Jacobs/NMC	 6/94		General clean up		*
 * L. Williams/EAI	 7/94		Reformat header			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	lwidth_req = *ilwid;
}
