#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_gfill ( int *ifill, int *iret )
/************************************************************************
 * cds_gfill								*
 *									*
 * This function gets the current fill flag.				*
 *									*
 * cds_gfill ( ifill, iret )						*
 *									*
 * Input parameters:							*
 *	*ifill		int	Fill flag (0-FALSE,1-TRUE)		*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	10/97	Created					*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * F. J. Yen/NCEP	 5/98	Renamed from cds_getfil. Retyped ifill.	*
 * D.W.Plummer/NCEP      6/98   Change ifill back to int 		*
 * T. Piper/GSC		10/98	Correct prolog for ifill change to int  *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;
    
    *ifill = cdsFill;
    
}
