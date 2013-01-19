#include "gifcmn.h"

void wscolb ( int *index, int *iret )
/************************************************************************
 * wscolb								*
 *									*
 * This function assigns LUT element to current color for the gif	*
 * image.								*
 *									*
 * wscolb ( index,iret )						*
 *									*
 * Input parameter:							*
 *  *index		int	color index				*
 *									*
 * Output parameter:							*
 *  *iret		int	return code				*
 *									*
 **									*
 * Log:									*
 * J. Nielsen-G/TAMU	12/96						*
 * T. Lee/GSC		 7/00	Renamed from gdr_applyLUT 		*
 ***********************************************************************/
{
	*iret = G_NORMAL;
	/*
	 * Make sure that the index is within acceptable bounds
	 */
	if ( ( *index < 0 ) || ( *index >= MXCOL ) ) return;

	/*
	 * Set color index.
	 */
	CurrentColorIndex = LUT [*index];
}
