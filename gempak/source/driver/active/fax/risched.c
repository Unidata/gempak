#include "faxcmn.h"

void risched ( int *ioff, int *iret )
/************************************************************************
 * risched								*
 *  									*
 * This routine adds a 16 byte ISCHED record to a 6-bit fax product.	*
 *									*
 * The ISCHED components of the 6-bit fax product is a 16 byte record	*
 * with the following fields:						*
 *									*
 *	byte 1-2:	Subset number					*
 *	byte 3-4:	Offset to 1st scanline (currently 0)		*
 *	byte 5-6:	Reserved					*
 *	byte 7-8:	Number of scan lines in the section		*
 *	byte 9:		Flag bits (currently 0x80 )			*
 *	byte 10:	Indent						*
 *	byte 11:	X size convert to bytes				*
 *	byte 12:	Offset of next inset (currently 0)		*
 *	byte 13-14:	Start scanline of next inset (currently 0)	*
 *	byte 15-16:	Inset number of following inset (currently 0)	*
 *	byte 17-20:	Padding (0's)					*
 *  									*
 * risched ( ioff, iret )						*
 *									*
 * Input parameters							*
 *	*ioff		int		Offset of the ISCHED record	*
 *									*
 * Output parameters							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * E. Wehner/Eai	 3/97	Changed from xsize/ysize to scanlines	*
 * S. Jacobs/NCEP	 7/97	Cleaned up global vars; Removed stdio.h	*
 * S. Jacobs/NCEP	 7/97	Updated header, comments and var names	*
 * S. Jacobs/NCEP	 7/97	Added indent value from product table	*
 * S. Jacobs/NCEP	 8/97	Added reserved value from prod table	*
 * S. Jacobs/NCEP	 5/98	Changed to allow for multiple subsets	*
 ***********************************************************************/
{

	int	isubset, i;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

	for ( i = 0; i < nsub; i++ )  {

/*
 *	    Subset number (byte 1-2)
 */
	    isubset = atoi(subset[i]);
	    sixbit[(*ioff)++] = ( isubset >> 8 ) & 0xff;
	    sixbit[(*ioff)++] = isubset & 0xff;

/*
 *	    Offset to first scanline (byte 3-4)
 */
	    sixbit[(*ioff)++] = 0x00;
	    sixbit[(*ioff)++] = 0x00;
	
/*
 *	    Reserved (byte 5-6)
 */
	    sixbit[(*ioff)++] = ( krsv[i] >> 8 ) & 0xff;
	    sixbit[(*ioff)++] = krsv[i] & 0xff;
	
/*
 *	    Number of scanlines (byte 7-8)
 */ 
	    sixbit[(*ioff)++] = ( klin[i] >> 8 ) & 0xff;
	    sixbit[(*ioff)++] = klin[i] & 0xff;

/*
 *	    Flag (byte 9)
 */
	    sixbit[(*ioff)++] = 0x80;

/*
 *	    Indent (byte 10)
 */
	    sixbit[(*ioff)++] = kind[i] / 8;

/*
 *	    Xsize in bytes (byte 11)
 */
	    sixbit[(*ioff)++] = kbit[i] / 8;

/*
 *	    Offset of the next inset (byte 12)
 */
	    sixbit[(*ioff)++] = 0x00;

/*
 *	    Start scanline of next inset (byte 13-14)
 */
	    sixbit[(*ioff)++] = 0x00;
	    sixbit[(*ioff)++] = 0x00;

/*
 *	    Inset number of following inset (byte 15-16)
 */
	    sixbit[(*ioff)++] = 0x00;
	    sixbit[(*ioff)++] = 0x00;  

	}

}
