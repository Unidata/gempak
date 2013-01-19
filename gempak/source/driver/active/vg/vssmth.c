#include	"vgcmn.h"

void vssmth ( int *ismtyp, float *dens, int *ietype, float *tensn, int *iret )
/************************************************************************
 * vssmth								*
 *									*
 * This subroutine sets the line smoothing attributes.			*
 *									*
 * vssmth ( ismtyp, dens, ietype, tensn, iret )				*
 *									*
 * Input parameters:							*
 *	*ismtyp		int		Smoothing type			*
 *					  0 = none			*
 *					  1 = splines			*
 *	*dens		float		Density of intermediate points	*
 *	*ietype		int		End point type			*
 *	*tensn		float		Line tension			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 2/98						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	ksmtyp = *ismtyp;
	rdens  = *dens ;
	ketype = *ietype;
	rtensn = *tensn;

}
