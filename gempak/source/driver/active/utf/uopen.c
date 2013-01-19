#include "utfcmn.h"

#define LENHDR   	12

void uopen ( int *iret )
/************************************************************************
 * uopen								*
 *									*
 * This subroutine opens a file and loads the header for a              *
 * Universal Transmission Format (UTF) file. 				*
 *									*
 * uopen ( iret )			               			*
 *                                                                      *
 * Output parameters:							*
 *	*iret 		int 		Return Code			*
 **									*
 * Log:									*
 * E. Safford/GSC	11/96	Initial Coding				*
 * S. Jacobs/NCEP	 8/97	Clean up header	and comments		*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 * S. Jacobs/NCEP	 9/97	Changed the time stamp for the product	*
 * S. Jacobs/NCEP	 2/98	Changed the minutes in the time stamp	*
 ***********************************************************************/
{

	unsigned char   hdr[LENHDR];
	int		itime, ierr;  

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

 /*     
  *	If no file name is specified, return with an error.
  */

	if ( strlen ( filnam ) == (size_t)0 ) {
	    *iret = G_NOUTFL;
	    return;
	}

/*
 *	If the open fails, return immediately.
 */
	flun = cfl_wopn ( filnam, &ierr );

	if  ( ierr != 0 ) {
	    *iret  = G_NOUTFL;
	    opnfil = G_FALSE;
	    return;
	}

/*
 *	Mark file as opened.
 */
	opnfil  = G_TRUE;

/*
 *	The header is 12 bytes long and the storage scheme is as follows:
 *
 *	  hdr[0]         --> header marker (0xc1) 
 *	  hdr[1]         --> projection indicator value 
 *	  hdr[2], hdr[3] --> geography scale value 
 *	  hdr[4], hdr[5] --> imax (max width) value
 *	  hdr[6], hdr[7] --> jmax (max height) value
 *	  hdr[8]	 --> all 5 bits of the day starting at the high 
 *		  	     order and then 3 of the 4 bits of the month
 *	  hdr[9]	 --> the remaining 1 bit of the month (at high
 *			     order) all 7 bits of the year
 *	  hdr[10]	 --> the first 8 bits of the time in hundreds of
 *			     hours
 *	  hdr[11]	 --> the remaining 4 bits of the time (at high 
 *			     order) all 4 bits of the pdc value
 *			     (presently 0) 
 */

	hdr[0] = 0xc1;
	hdr[1] = kmap & BMASK;

	hdr[2] = (kgscl >> 8) & BMASK;
	hdr[3] = kgscl & BMASK;

	hdr[4] = (kxsize >> 8) & BMASK;
	hdr[5] = kxsize & BMASK;

	hdr[6] = (kysize >> 8) & BMASK;
	hdr[7] = kysize & BMASK;

	hdr[8] = ( (kdd << 3) & 0xf8 ) | ( (kmm >> 1) & 0x07 );
	hdr[9] = ( ( (kmm & 0x01) << 7 ) & 0x80 ) | (kyy & 0x7f);

	itime = khh * 100 + knn;
	hdr[10] = (itime >> 4) & BMASK;
	hdr[11] = (itime << 4) & 0xf0;

/*
 *	Write the header array to the buffer.
 */
	uwrbuf ( hdr, LENHDR, iret );

}
