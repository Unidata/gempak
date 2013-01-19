#include	"utfcmn.h"

#define LENHDR	8

void uegrp ( int *iret )
/************************************************************************
 * uegrp								*
 *									*
 * This subroutine ends a drawing element group.			*
 *									*
 * uegrp ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/99						*
 ***********************************************************************/
{

	int		i, m, ier, num, iarr[3], ipri;
	unsigned char	txthdr[LENHDR], tchar[MXAFOS];

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	If the group type is not 11, return.
 */
	if  ( kgtyp != 11 )  {
	    kgtyp = 0;
	    nchr  = 0;
	    mlen[0] = 0;
	    mlen[1] = 0;
	    mlen[2] = 0;
	    mrchr[0][0] = CHNULL;
	    mrchr[1][0] = CHNULL;
	    mrchr[2][0] = CHNULL;
	    return;
	}

/*
 *	Set the header information.
 *	  txthdr[0] -->  The mode.  The mode will be 0xc8 for
 *			   offset text.
 *	  txthdr[1] -->  The i,j coords flag in bit 0 (high order),
 *			   the char block mode in bit 1,
 *			   the reverse block mode in bit 2,
 *			   the zoom threshold in bits 3 and 4,
 *			   the zoom factor in bits 5-7
 *			 Bit 1 is turned on.
 *	  txthdr[2] -->  The char size multiplier in bits 0-1, and
 *			   the first 6 bits of the icoord value
 *	  txthdr[3] -->  The remaining 8 bits of the icoord value.
 *	  txthdr[4] -->  The first 8 bits of the jcoord value.
 *	  txthdr[5] -->  The last 8 bits of the jcoord value.
 *	  txthdr[6] -->  The I offset, in this case always 0xf0.
 *	  txthdr[7] -->  The J offset, in this case always 0x06.
 */

	txthdr[0] = 0xc8;

/*
 *	Use the station priority to set the zoom threshold.
 */
	cst_numb ( mrchr[3], &ipri, &ier );
	txthdr[1] = 0x40 | ( ( (ipri-90) << 3 ) & 0x18 );

	txthdr[2] = ( (nfntsz << 6) & 0xc0 ) | ( (mxx >> 8) & 0x3f );
	txthdr[3] = mxx & BMASK;
	txthdr[4] = ( myy >> 8 ) & BMASK;
	txthdr[5] = myy & BMASK;
	txthdr[6] = 0xf0;
	txthdr[7] = 0x06;

/*
 *	Set the UTF text string to the three strings for the Medium
 *	range product.
 *
 *	First, add 3 backspaces.
 */
	m = 0;
	tchar[m] = 0x08; m++;
	tchar[m] = 0x08; m++;
	tchar[m] = 0x08; m++;

/*
 *	Add the anomaly string.
 */
	for ( i = 0; i < mlen[1]; i++, m++ )  {
	    tchar[m] = mrchr[1][i];
	}
	tchar[m] = 0x0d; m++;

/*
 *	Get the anomaly values. Check the POP value to determine
 *	which marker to write the file.
 */
	cst_ilst ( mrchr[1], '/', 0, 3, iarr, &num, &ier );

	tchar[m] = 0x12; m++;
	if  ( iarr[2] > 0 )  {
	    /* Filled circle */
	    tchar[m] = 0x03; m++;
	}
	else {
	    /* Open circle */
	    tchar[m] = 0x0e; m++;
	}
	tchar[m] = 0x11; m++;

/*
 *	Add the station ID.
 */
	for ( i = 0; i < mlen[0]; i++, m++ )  {
	    tchar[m] = mrchr[0][i];
	}
	tchar[m] = 0x0d; m++;

/*
 *	Add 3 backspaces.
 */
	tchar[m] = 0x08; m++;
	tchar[m] = 0x08; m++;
	tchar[m] = 0x08; m++;

/*
 *	Add the temperature and POP values.
 */
	for ( i = 0; i < mlen[2]; i++, m++ )  {
	    tchar[m] = mrchr[2][i];
	}

	tchar[m] = CHNULL;

/*
 *	Write the header and the string to the buffer.
 */
	uwrbuf ( txthdr, LENHDR, iret );
	uwrbuf ( tchar, m, iret );

/*
 *	Reset the group type to 0.
 *	Reset the number of strings to 0.
 *	Reset the array of strings.
 */
	kgtyp = 0;
	nchr  = 0;
	mlen[0] = 0;
	mlen[1] = 0;
	mlen[2] = 0;
	mrchr[0][0] = CHNULL;
	mrchr[1][0] = CHNULL;
	mrchr[2][0] = CHNULL;

}
