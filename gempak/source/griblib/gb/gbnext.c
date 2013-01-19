#include "gbcmn.h"

void gb_next ( int *ivers, int *iret )
/************************************************************************
 * gb_next								*
 *									*
 * This function will be the interface for GB_SCAN, and is used to	*
 * find the next GRIB message.						*
 *									*
 * gb_next ( ivers, iret )						*
 *									*
 * Output parameters:							*
 *	*ivers		int		GRIB version number		*
 *	*iret		int		Return code			*
 *					-16 = error on next msg		*
 **									*
 * Log:									*
 * J. Chou/EAI		 7/93						*
 * S. Jacobs/EAI	 7/93	Clean up				*
 * S. Jacobs/EAI	11/93	Added return of version number		*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * D.W.Plummer/NCEP	 3/96	Change for cfl_ call sequence		*
 * D.W.Plummer/NCEP	 6/96	Reset length variables to zero		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	*iret = 0;

	/*
	 *  (Re)set section length variable
	 */

	pdslength = 0;
	gdslength = 0;
	bdslength = 0;
	bmslength = 0;

	/*
	 *  Scan the files for the next message.
	 */

	gb_scan ( gbfile.fptr, infile.fptr, ivers, iret );

}
