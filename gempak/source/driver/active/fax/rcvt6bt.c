#include "faxcmn.h"

void rcvt6bt ( int *nbyte, int *iret)
/************************************************************************
 * rcvt6bt								*
 *									*
 * This routine converts a raster image to NMC 6-bit fax format. The	*
 * scan lines are run-length encoded into groups of conecutive ones	*
 * and zeros, where possible.						*
 *									*
 * rcvt6bt  ( nbyte, iret )						*
 *									*
 * Output parameters:							*
 *	*nbyte		int		Number of bytes in 6-bit image	*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * E. Wehner/Eai	12/96	Change data type of character to usign	*
 * E. Wehner/EAi	 3/97 	Change x/ysize to numscans.		*
 * S. Jacobs/NCEP	 7/97	Updated global vars; Removed stdio.h	*
 * S. Jacobs/NCEP	 7/97	Reorganized code; Removed uneeded vars	*
 * S. Jacobs/NCEP	 5/98	Changed to allow for multiple subsets	*
 ***********************************************************************/
{

	unsigned char	ch, half_byte;
	int		k, icnt, ncon, mode, jbit, n6bit;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Construct the header of the 6-bit fax format file.
 *	The 6-bit file header starts with 3 bytes with value 0xff.
 */
	sixbit[0] = 0xff;
	sixbit[1] = 0xff;
	sixbit[2] = 0xff;

	rmapid ( iret );

/*
 *	Loop over the original raster image checking one half-byte at
 *	a time.
 */
	n6bit = 0;
	mode  = NO_MODE;
	jbit  = 0;
	ncon  = 0;
	icnt  = 0;

	while ( icnt < msize )  {

	    for ( k = 0; k < 2; k++ )  {

		jbit += 4;

/*
 *		Isolate the high and low order parts of the byte
 *		into half-bytes.
 */
		if  ( k == 0 )  {
		    half_byte = ( rasimg[icnt] >> 4 ) & 0x0f;
		}
		else  {
		    half_byte = rasimg[icnt] & 0x0f;
		}
 
		switch ( half_byte )  {

/*
 *		    The half-byte has all 0's.
 */
		    case 0x00:

			switch ( mode )  {

			    case ZEROS_MODE:
/*
 *				Increase the number of 0's.
 */
				ncon += 4;
				break;

			    case ONES_MODE:
/*
 *				Switching mode, write out the last
 *				series. Start the count of 0's.
 */
				rrunle ( &n6bit, ncon, mode, iret );
				ncon = 4;
				break;

			    case NO_MODE:
			    default:
/*
 *				Start the count of 0's.
 */
				ncon = 4;
				break;
			}     
			mode = ZEROS_MODE;
			break;

/*
 *		    The half-byte has all 1's.
 */
		    case 0x0f:

			switch ( mode )  {

			    case ZEROS_MODE:
/*
 *				Switching mode, write out the last
 *				series. Start the count of 1's.
 */
				rrunle ( &n6bit, ncon, mode, iret );
				ncon = 4;
				break;

			    case ONES_MODE:
/*
 *				Increase the number of 1's.
 */
				ncon += 4;
				break;

			    case NO_MODE:
			    default:
/*
 *				Start the count of 1's.
 */
				ncon = 4;
				break;
			    }     
			    mode = ONES_MODE;
			    break;

/*
 *		    The half-byte is variant.
 */
		    default:

			switch  ( mode )  {

			    case ZEROS_MODE:
/*
 *				Switching mode, write out the last
 *				series.
 */
				rrunle ( &n6bit, ncon, mode, iret );
				break;

			    case ONES_MODE:
/*
 *				Switching mode, write out the last
 *				series.
 */
				rrunle ( &n6bit, ncon, mode, iret );
				break;

			    case NO_MODE:
			    default:
			    	break;
			}   
			ncon = 0;  
			mode = NO_MODE;

/*
 *			This is a half-byte with alternating 0's
 *			and 1's. Write the half byte out using the
 *			format "10TTTT" (0x80 plus the half-byte).
 */
			ch = 0x80 | ( half_byte << 2 );
			rsndsix ( n6bit, ch, iret );
			n6bit++;
			break;

		}
 
/*
 *		If this is the end of the scan line, write the current
 *		buffer of 0's or 1's, then add the end of scan flag.
 */
		if  ( jbit >= kbit[0] )  {
		    switch ( mode )  {
		        case ZEROS_MODE:
/*
 *			    Switching mode, write out the last series.
 */
			    rrunle ( &n6bit, ncon, mode, iret );
			    ncon = 0;
			    break;

			case ONES_MODE:
/*
 *			    Switching mode, write out the last series.
 */
			    rrunle ( &n6bit, ncon, mode, iret );
			    ncon = 0;
			    break;

		        case NO_MODE:
		        default:
/*
 *			    Do nothing.
 */
			    break;
		    }   

/*
 *		    Add the end of scan flag.
 */
		    ch = 0xc0;
		    rsndsix ( n6bit, ch, iret );
		    n6bit++;

		    jbit = 0;
		    mode = NO_MODE;
		    ncon = 0;

		}

	    }

/*
 *	    Increment the bit counter for the loop.
 */
	    icnt++;

	}

/*
 *	Add the end of map flag.
 */ 
	ch = 0xcc;
	rsndsix ( n6bit, ch, iret );
	n6bit++;

/*
 *	Pad the graphics image to a total of 1440 bytes.
 */
	*nbyte = ( n6bit * (float)(6.0/8.0) ) + NHEAD;
	rpadrec ( nbyte, 1440, iret );

/*
 *	Add the Fax Schedule Controls Information to the output file.
 *	The beginning of this section is flagged by 3 bytes with values
 *	0xff, 0xff and 0xfd.
 */
	sixbit[(*nbyte)++] = 0xff;
	sixbit[(*nbyte)++] = 0xff;
	sixbit[(*nbyte)++] = 0xfd;

	(*nbyte) += 13;

/*
 *	Add the ISCHED record to the 6-bit file. And pad it as
 *	necessary.
 */
	risched ( nbyte, iret );

	rpadrec ( nbyte, 1440, iret );

/*
 *	Add the end of file flag. This is a set of 3 bytes with values
 *	0xff, 0xff and 0xfc.
 */
	sixbit[(*nbyte)++] = 0xff;
	sixbit[(*nbyte)++] = 0xff;
	sixbit[(*nbyte)++] = 0xfc;

/*
 *	Pad rest of the output file to an even 1440-byte boundary.
 */
	rpadrec ( nbyte, 1440, iret );

}
