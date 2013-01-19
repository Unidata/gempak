#include "faxcmn.h"

void rrunle ( int *ipos, int lenrun, int nvalue, int *iret )
/************************************************************************
 * rrunle								*
 *									*
 * This function will encode a run of consecutive ones or zeroes into	*
 * a packed format. Each six bit entity is composed of a 2 bit flag and	*
 * 4 bits of data.							*
 *									*
 * rrunle ( ipos, lenrun, nvalue, iret )				*
 *									*
 * Input parameters:							*
 *	*ipos		int		Position of 6-bit byte		*
 *	lenrun		int 		Length of the run in bits	*
 *	nvalue		int		Value of the run (0 or 1 )	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * E. Wehner/EAi	11/96	Modified data type to unsigned 		*
 * S. Jacobs/NCEP	 7/97	Added faxcmn.h				*
 * S. Jacobs/NCEP	 7/97	Updated header, comments and var names	*
 ***********************************************************************/
{

	int		length;
	unsigned char	ch;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	The 6-bit encoding stores the number of half-bytes of 
 *	consecutive 0's or 1's, therefore divide the number of bits
 *	by 4 to get the number of half-bytes.
 */
	length = lenrun / 4;

/*
 *	The 6-bit coding for consecutive half-bytes has the following
 *	format:
 *		FFTTTT
 *	where FF is the value 0 or 1 and TTTT is the number of
 *	consecutive half-bytes. Since this would allow for only up
 *	to 15 half-bytes, the encoding allows for multipliers for
 *	subsequent groups of the same FF value. This is encoded as
 *	follows:
 *		1 * TTTT(1) + 16 * TTTT(2) + 256 * TTTT(3)
 *
 *	Encode the number of half-bytes according to the scheme above.
 */
	ch = ( nvalue << 6 ) | ( ( length & 0x0f ) << 2 );
	rsndsix ( *ipos, ch, iret );
	(*ipos)++;

	if  ( length >= 16 )  {
	    ch =  ( nvalue << 6 ) | ( ( ( length & 0xf0 ) >> 4 ) << 2 );
	    rsndsix ( *ipos, ch, iret );
	    (*ipos)++;
	}

	if  ( length >= 256 )  {
	    ch = ( nvalue << 6 ) | ( ( length >> 8 ) << 2 );
	    rsndsix ( *ipos, ch, iret );
	    (*ipos)++;
	}

}
