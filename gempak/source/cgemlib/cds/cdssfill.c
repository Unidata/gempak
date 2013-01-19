#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_sfill ( int *ifill, int *iret )
/************************************************************************
 * cds_sfill								*
 *									*
 * This function sets the fill flag.					*
 *									*
 * cds_sfill ( ifill, iret )						*
 *									*
 * Input parameters:							*
 *	*ifill		int		Fill flag (0-FALSE,1-TRUE)	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	10/97	Created					*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include  *
 * F. J. Yen/NCEP	 5/98	Renamed from cds_setfil			*
 * S. Jacobs/NCEP	11/99	Changed ifill to a pointer		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;
    
    cdsFill = *ifill;
    
}
