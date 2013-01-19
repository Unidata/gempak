#include "gbcmn.h"

void gb_gbdh ( int *length, int *bdflag, int *scale, float *ref, 
						int *nbits, int *iret )
/************************************************************************
 * gb_gbdh								*
 *									*
 * This function gets the BDS header information.			*
 *									*
 * gb_gbdh ( length, bdflag, scale, ref, nbits, iret )			*
 *									*
 * Output parameters:							*
 *	*length		int		Length of BDS			*
 *	*bdflag		int		Flag from Table 11		*
 *	*scale		int		Binary scale factor		*
 *	*ref		float		Reference value			*
 *	*nbits		int		Number of bits for packing	*
 *	*iret		int		Return code			*
 *					-16 = error on next msg		*
 **									*
 * Log:									*
 * K. Brill/EMC		12/95						*
 * S. Jacobs/NCEP	 1/96	Changed DA_READ to CFL_READ		*
 * D.W.Plummer/NCEP	 2/96	Cleanup GBDIAGs and comments		*
 * D.W.Plummer/NCEP	 3/96	Changed cfl_ call sequence		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 ***********************************************************************/
{
int		offset, ret, nbytes;
int		flag, first_byte, isign, ia, ib, indx;
unsigned char	*buffer;

/*---------------------------------------------------------------------*/

	*iret = 0;

	if ( GBDIAG_BDS == TRUE )
	    printf ( "READ BINARY DATA HEADER\n" );

	/*
	 *	Read in the first 14 bytes of the BDS.
	 */
	offset = cursor + ISLENGTH + pdslength + gdslength + bmslength;
	/*
	 *      Allocate space for the buffer.
	 */
        buffer = (unsigned char *)
                  malloc ( 14 * sizeof(unsigned char) );

	cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	cfl_read ( gbfile.fptr, 14, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -16;
	    return;
	}
	/*
	 *	Get the length of the section.
	 *	bytes 1-3
	 */
	indx = 0;
	*length = gb_btoi ( buffer, indx, 3, FALSE );

	/*
	 *	Get the flag information which determines which
	 *	type of grid is in the message.
	 *	byte 4
	 */
	indx = 3;
	flag = gb_btoi ( buffer, indx, 1, FALSE );
	*bdflag = flag;

	/*
	 *	Set the scale factor (E).
	 *	bytes 5-6
	 */
	indx = 4;
	*scale = gb_btoi ( buffer, indx, 2, TRUE );

	/*
	 *	Set the reference value. The reference value is a floating
	 *	point number constructed from various parts of the next
	 *	four bytes.
	 *	bytes 7-10
	 */
	indx = 6;
	first_byte = gb_btoi ( buffer, indx, 1, FALSE );
	isign = first_byte >> 7;
	ia = first_byte & 127;
	indx = 7;
	ib = gb_btoi ( buffer, indx, 3, FALSE );

	*ref = (float) ( pow(-1.0,(double)isign) *
			 pow(2.0,-24.0) * (double) ib *
			 pow(16.0,(double)ia-64.0) );

	if ( ( ( flag & 32 ) >> 5 ) == 1 )
	    *ref = (float) ( (int) *ref );

	/*
	 *	Set the number of bits used for the packing.
	 *	byte 11
	 */
	indx = 10;
	*nbits = gb_btoi ( buffer, indx, 1, FALSE );

        free ( buffer );
}
