#include "pscmn.h"

void pslwid ( int *ilwid, int *iret )
/************************************************************************
 * PSLWID								*
 * 									*
 * This subroutine sets the hardware line width.			*
 *									*
 * PSLWID  ( ilwid, IRET )						*
 *									*
 * Input parameters:							*
 * 	*ilwid		INT	 	Line width 			*
 *									*
 * Output parameters:							*
 *	*IRET		INT		Return code			*
 **									*
 * Log:									*
 * A. Chang/EAI		 2/94						*
 * S. Jacobs/NCEP	 7/97	Changed width 1 to be scaled		*
 ***********************************************************************/
{

	float	wid;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Set line width in user space.  Note that this assumes the printer
 *	has 300 dots / inch.  We have set up our plotting space to be
 *	2304 dots / inch. ( 2304 / 300 = 7.68 )
 */
	if  ( *ilwid <= 1 )  {
	    wid = 1.;
	}
	else if   ( *ilwid >= 150 )  {
	    wid = 150.;
	}
	else  {
	    wid = *ilwid;
	}

	irwdth = G_NINT ( wid * 7.68 );
}
