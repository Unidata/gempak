#include "vgcmn.h"

void vsgtgn ( int *igtyp, int *ignum, int *iret )
/************************************************************************
 * vsgtgn								*
 *									*
 * This subroutine sets the group type and group number for the current *
 * element.                                                             *
 *									*
 * vsgtgn ( igtyp, ignum, iret )					*
 *									*
 * Input parameters:							*
 *	*igtyp		int		Group type			*
 *	*ignum		int		Group number			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * D. Kidwell/NCEP	 6/02						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the current group type.
 */
	kgtyp = (char) *igtyp;

/*
 *	Set the current group number.
 */
	kgnum = kgrpns[*igtyp] + *ignum;

}
