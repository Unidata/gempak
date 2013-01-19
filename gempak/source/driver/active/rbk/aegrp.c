#include "ardcmn.h"

#define LENHDR		11
#define ONEFOUR		 6

void aegrp ( int *iret )
/************************************************************************
 * aegrp								*
 *									*
 * This subroutine ends a drawing element group.			*
 *									*
 * aegrp ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 8/99						*
 * S. Jacobs/NCEP	 3/01	Fixed size and use of LENHDR		*
 ***********************************************************************/
{

	int		i, m, ier, num, iarr[3], ipri;
	unsigned char	txthdr[LENHDR], atthdr[ONEFOUR], tchar[5000];

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
 *	Setting the plot parameters block: mode 1 submode 4.
 *	  atthdr[0] -->  0x40 is the field flag
 *	  atthdr[1] -->  0x03 is the length of the block
 *	  atthdr[2] -->  0x01 is the mode of the block
 *	  atthdr[3] -->  0x04 is the submode of the block
 *	  atthdr[4] -->  Zoom Disable flag (bit 7 from high order bit;
 *					    always 0)
 *			 Zoom Threshold (bits 6-0; from priority number)
 *	  atthdr[5] -->  Zoom Factor
 */
	cst_numb ( mrchr[3], &ipri, &ier );

	atthdr[0] = 0x40;
	atthdr[1] = 0x03;
	atthdr[2] = 0x01;
	atthdr[3] = 0x04;

	switch ( ipri ) {

	    case 93:
		atthdr[4] = 0x10;
		break;

	    case 92:
		atthdr[4] = 0x09;
		break;

	    case 91:
		atthdr[4] = 0x04;
		break;

	    case 90:
	    default:
		atthdr[4] = 0x00;
		break;
	}

	atthdr[5] = 0x00;

/*
 *	Write the plot parameter information to the buffer.
 */
	awrbuf ( atthdr, ONEFOUR, iret );

/*
 *	Set the text string to the three strings for the Medium
 *	range product.
 *
 *	First, add 1 up space and 3 backspaces.
 */
	m = 0;
	tchar[m] = 0x0b; m++;
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

	tchar[m] = 0x09; m++;

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

/*
 *	If the length is even, add a blank to the end.
 */
	if  ( ! (m % 2) )  {
	    tchar[m] = ' '; m++;
	}

/*
 *	Terminate the string with a NULL.
 */
	tchar[m] = CHNULL;

/*
 *	Assign values to the text header in the following format:
 *	  txthdr[0]  -->  0x40 is the field flag
 *	  txthdr[1]  -->  the length of the block.
 *	  txthdr[2]  -->  0x05 is the mode of the block.
 *	  txthdr[3]  -->  0x01 is the submode of the block.
 *	  txthdr[4]  -->  The first 8 bits of the icoord (ix) value
 *	  txthdr[5]  -->  The last 8 bits of the icoord value.
 *	  txthdr[6]  -->  The first 8 bits of the jcoord (iy) value
 *	  txthdr[7]  -->  The last 8 bits of the jcoord value.
 *	  txthdr[8]  -->  The x offset
 *	  txthdr[9]  -->  The y offset
 *	  txthdr[10] -->  The block/reverse block mode/char. size
 */
	txthdr[0] = 0x40;
	txthdr[1] = ( ( LENHDR + m ) / 2 ) & BMASK;
	txthdr[2] = 0x05;
	txthdr[3] = 0x01;
	txthdr[4] = ( mxx >> 8 ) & BMASK;
	txthdr[5] = mxx & BMASK;
	txthdr[6] = ( myy >> 8 ) & BMASK;
	txthdr[7] = myy & BMASK;
	txthdr[8] = 0x00;
	txthdr[9] = 0x00;
	txthdr[10] = 0x80 & BMASK;

/*
 *	Write the header and the string to the buffer.
 */
	awrbuf ( txthdr, LENHDR, iret );
	awrbuf ( tchar, m, iret );

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
