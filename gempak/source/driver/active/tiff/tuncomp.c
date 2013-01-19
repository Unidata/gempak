#include "tiffcmn.h"

void tuncomp ( int *kpos, int *iret )
/************************************************************************
 * tuncomp								*
 *									*
 * This function sets the output array to the uncompressed data values.	*
 * However, since this output only requires 16 colors, the pixel values	*
 * are represented by half-bytes (4 bits).				*
 *									*
 * tuncomp ( kpos, iret )						*
 *									*
 * Input/Output parameters:						*
 *	*kpos		int		Byte location before/after	*
 *					   setting the output data	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 9/00						*
 ***********************************************************************/
{

	int	i, jpos;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	jpos = *kpos;

	for ( i = 0; i < kbit*klin; i+=2 )  {

	    group4[jpos] = rasimg[i] * 16 + rasimg[i+1];

	    jpos++;

	}

	*kpos = jpos;

}
