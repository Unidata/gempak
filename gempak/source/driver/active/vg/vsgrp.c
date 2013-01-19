#include "vgcmn.h"

void vsgrp ( int *igroup, int *iret )
/************************************************************************
 * vsgrp								*
 *									*
 * This subroutine starts a new drawing element group.			*
 *									*
 * vsgrp ( igroup, iret )						*
 *									*
 * Input parameters:							*
 *	*igroup		int		Group type			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 7/97						*
 * D. Kidwell/NCEP	 6/02	Added call to vsetgrps                  *
 * D. Kidwell/NCEP	 6/02	Replaced vsetgrps call with kgindx array*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the current group type.
 */
	kgtyp = (char) *igroup;

/*
 *	Update the current group number.
 */
	kgindx[*igroup]++;
	kgnum = kgrpns[*igroup] + kgindx[*igroup];

}
