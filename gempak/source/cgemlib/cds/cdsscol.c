#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_scol ( int icol, int *iret )
/************************************************************************
 * cds_scol								*
 *									*
 * This function sets the color flag.					*
 *									*
 * cds_scol ( icol, iret )						*
 *									*
 * Input parameters:							*
 *	icol		int		Color				*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/97	Created					*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include  *
 * F. J. Yen/NCEP	 5/98	Renamed from cds_setcol; correct desc.	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;
    
    cdsColor = icol;
    
}
