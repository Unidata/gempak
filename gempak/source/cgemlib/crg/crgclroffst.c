#include "crgcmn.h"


void crg_clroffst ( int joffset, int *iret )
/************************************************************************
 * crg_clroffst								*
 *									*
 * This function clears the range record for the element located at	*
 * file position joffset.						*
 *									*
 * crg_clroffst ( joffset, iret )					*
 *									*
 * Input parameters:							*
 *	joffset		int		File position of the element	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -1 = element not found		*
 **									*
 * Log:									*
 * E. Wehner/EAi	 8/97	Created					*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * F.J.Yen/NCEP		12/97	Renamed from crg_posclr.  Cleaned up.	*
 ***********************************************************************/
{
    int		ier, elnum;
/*---------------------------------------------------------------------*/

    *iret = 0;

    crg_getinx (joffset, &elnum, &ier);

    if (ier < 0)
    {
       *iret = -1;
       return;
    }

    crg_clear( elnum, &ier);

}
