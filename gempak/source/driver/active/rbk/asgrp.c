#include	"ardcmn.h"

void asgrp ( int *igroup, int *iret )
/************************************************************************
 * asgrp								*
 *									*
 * This subroutine starts a new drawing element group.			*
 *									*
 * asgrp ( igroup, iret )						*
 *									*
 * Input parameters:							*
 *	*igroup		int		Group type			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 8/99						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the current group type.
 */
	kgtyp = *igroup;

}
