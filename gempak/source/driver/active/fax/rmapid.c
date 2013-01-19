#include "faxcmn.h"

void rmapid ( int *iret )
/************************************************************************
 * rmapid								*
 *  									*
 * This routine adds a map identification string to a 6-bit fax format	*
 * file. The string starts with an "F", followed by the product subset	*
 * number, then a title and finally a date/time stamp. The characters	*
 * are encoded using the Extended CDC Display code.			*
 *									*
 * void rmapid ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * S. Jacobs/NCEP	 7/97	Cleaned up global vars; Added faxcmn.h	*
 * S. Jacobs/NCEP	 7/97	Added subset and time stamp to title	*
 * S. Jacobs/NCEP	 5/98	Changed to allow for multiple subsets	*
 * A. Hardy/GSC		12/00   Added nc G_MIN string length check 	*
 * B. Yin/SAIC		03/04	Changed cfdate to css_date		*
 ***********************************************************************/
{

	int	isize, i, ier, itype, iyy, imm, idd, ihh, inn, iss, nc;
	int 	julian;
	char	hdr[49], tmpl[21], date[21], dattim[21], tmzn[4];

/*---------------------------------------------------------------------*/

	*iret = 0;

/*
 *	Construct the date/time (GMT) string.
 */
	strcpy ( tmpl, "HHNNZ DDMMM" );
	itype = 1;
	css_date ( &itype, &iyy, &imm, &idd, &ihh, &inn, &iss, &julian, tmzn, &ier );
	sprintf ( dattim, "%02d%02d%02d/%02d%02d",
		  iyy, imm, idd, ihh, inn );
	cfl_mnam ( dattim, tmpl, date, &ier );

/*
 *	Construct the header string.
 */
	cst_lstr ( descr[0], &nc, &ier );
	nc = G_MIN ( 30, nc );
	strcpy  ( hdr, "F" );
	strcat  ( hdr, subset[0] );
	strcat  ( hdr, " " );
	strncat ( hdr, descr[0], nc );
	strcat  ( hdr, " " );
	strcat  ( hdr, date );

/*
 *	Set the size of the header string.
 */
	isize = G_MIN ( (NHEAD-3), (int)strlen(hdr) );

/*
 *	Convert each character to CDC and embed in the file header.
 *	The identification string starts at the fourth byte of the 
 *	6-bit image.
 */
	for ( i = 0; i < isize; i++ )  {
	   sixbit[i+3] = rcvt2cdc ( hdr[i] );
	}

/*
 *	Load the remainder of the line with blanks.
 */
	for ( i = isize; i < NHEAD-3; i++ )  {
	    sixbit[i+3] = rcvt2cdc ( ' ' );
	}

}
