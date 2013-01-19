#include "vgcmn.h"

void vsfrnt ( int *mfcod, float *pipsz, int *mpipst, int *mpipdr, int *iret )
/************************************************************************
 * vsfrnt								*
 *									*
 * This subroutine sets the front attributes.				*
 *									*
 * vsfrnt ( mfcod, pipsz, mpipst, mpipdr, iret )			*
 *									*
 * Input parameters:							*
 *	*mfcod		int		Front code			*
 *	*pipsz		float		Size of a pip on a front	*
 *	*mpipst		int		Size multiplier for a stroke	*
 *	*mpipdr		int		Direction multiplier		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * S. Jacobs/NCEP	 6/98	Changed pip size to type float		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	kfcod  = *mfcod;
	kpipsz = G_NINT ( *pipsz * 100.0F );
	kpipst = *mpipst;
	kpipdr = *mpipdr;

}
