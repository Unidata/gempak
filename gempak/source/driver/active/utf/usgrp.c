#include	"utfcmn.h"

void usgrp ( int *igroup, int *iret )
/************************************************************************
 * usgrp								*
 *									*
 * This subroutine starts a new drawing element group.			*
 *									*
 * usgrp ( igroup, iret )						*
 *									*
 * Input parameters:							*
 *	*igroup		int		Group type			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/99						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the current group type.
 */
	kgtyp = *igroup;

}
