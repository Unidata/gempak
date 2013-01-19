#include "faxcmn.h"

void rsndsix ( int jpos, unsigned char ch, int *iret )
/************************************************************************
 * rsndsix								*
 *									*
 * This routine stores a 6-bit entity that is left justified in an	*
 * 8-bit byte into an output array. This routine calculates where 	*
 * the entity exists in the array based on which 6-bit entity this	*
 * is.									*
 *									*
 * void rsndsix  ( jpos, ch, iret )					*
 *									*
 * Input parameters:							*
 *	jpos		int		Position of 6-bit byte		*
 *	ch		unsigned char	6-bit left justified byte	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * E. Wehner/EAi	12/96	Corrected byte masking			*
 * E. Wehner/Eai	 3/97	Init. byte on plane			*
 * S. Jacobs/NCEP	 7/97	Removed stdio.h				*
 * S. Jacobs/NCEP	 7/97	Renamed to rsndsix to match file name	*
 * S. Jacobs/NCEP	 7/97	Updated header, comments and var names	*
 ***********************************************************************/
{

	int	total_bits, byte_start, byte_stop, bit_start;

/*---------------------------------------------------------------------*/

	*iret  = G_NORMAL;

/*
 *	Compute the total number of bits in the array thus far.
 */
	total_bits = jpos * 6;

/*
 *	Calculate where in relation to 8-bit bytes this 6-bit entity
 *	will be stored. This number could be split over two bytes,
 *	or it could occur completely in a single byte.
 */
	byte_start = (  total_bits    / 8 ) + NHEAD;
	byte_stop  = ( (total_bits+5) / 8 ) + NHEAD;

	bit_start =  total_bits    % 8;

/* 
 *	Pack the requested 6 bits into the plane. Split the 6 bits
 *	up properly between the one or two destination bytes.
 */
	switch ( bit_start )  {

	    case 0:
	    case 1:
		sixbit[byte_start] = '\0';
		sixbit[byte_start] = ch & 0xfc;
		break;

	    case 2:
	    case 3:
		sixbit[byte_start] |= ( ( ch >> 2 ) & 0x3f );
		break;

	    case 4:
	    case 5:
		sixbit[byte_start] |= ( ch >> 4 ) & 0x0f;
		sixbit[byte_stop]  |= ( ch << 4 ) & 0xc0;
		break;

	    case 6:
	    case 7:
		sixbit[byte_start] |= ( ch >> 6 ) & 0x03;
		sixbit[byte_stop]  |= ( ch << 2 ) & 0xf0;
		break;

	    default: 
/*
 *		Something is wrong, reject the 6-bit encoding.
 */
		break;
	}

}
