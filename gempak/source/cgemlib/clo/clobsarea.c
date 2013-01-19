#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

void clo_bsarea ( float *latll, float *lonll, float *latur, float *lonur, 
								int *iret )
/************************************************************************
 * clo_bsarea								*
 *									*
 * This function sets geographical limits for bounds access.		*
 *									*
 * clo_bsarea ( latll, lonll, latur, lonur, iret )			*
 *									*
 * Input parameters:							*
 *	*latll		float	Latitude  - lower left			*
 *	*lonll		float	Longitude - lower left			*
 *	*latur		float	Latitude  - upper right			*
 *	*lonur		float	Longitude - upper right			*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 4/01	Created					*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = 0;

    latllBnd = *latll;
    lonllBnd = *lonll;
    laturBnd = *latur;
    lonurBnd = *lonur;

}
