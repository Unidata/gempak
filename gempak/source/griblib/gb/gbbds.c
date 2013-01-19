#include "gbcmn.h"

void gb_bds ( unsigned char *ptarray, int kxky, int *xgrid )
/************************************************************************
 * gb_bds								*
 *									*
 * This function decodes section 4 (Binary Data Section) of a GRIB	*
 * message.								*
 *									*
 * gb_bds ( ptarray, kxky, xgrid )					*
 *									*
 * Input parameters:							*
 *	*ptarray	unsigned char	Data buffer			*
 *	kxky		int		Number of grid points		*
 *									*
 * Output parameters:							*
 *	*xgrid		int		Packed gridded data		*
 **									*
 * Log:									*
 * J. Chou/EAI		02/93						*
 * J. Chou/EAI 		07/93						*
 * S. Jacobs/EAI	11/93	Clean up; Added GBDIAG prints		*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * D.W.Plummer/NCEP      2/96   Cleanup GBDIAG prints & comments	*
 * S. Jacobs/NCEP	 8/98	Changed xgrid from long int to int	*
 * T. Piper/GSC		11/98	Updated prolog				*
 ***********************************************************************/
{
	int	length, flag, kind, first_byte;
	int	isign, ia, ib;
	int	indx;
	float	ref;
	size_t	jj;

/*---------------------------------------------------------------------*/

	/*
	 * BYTES 1-3
	 * Get the length of the section.
	 */

	indx = 0; 
	bds.length = gb_btoi(ptarray, indx, 3, FALSE); 
	length = bds.length;

	/*
	 * BYTE 4
	 * Get the flag information which determines which
	 * type of grid is in the message.
	 */

	indx = 3; 
	bds.flag = gb_btoi(ptarray, indx, 1, FALSE);
	flag = bds.flag; 

	if ( ( ( flag & 128 ) >> 7 ) == 0 ) {
	    if ( ( ( flag & 64 ) >> 6 ) == 0 )
		kind = 1;
	    else {
		if ( ( ( flag & 4 ) >> 2 ) == 1 )
		    kind = 3;
		else
		    kind = 2;
	    }
        }
        else {
	    if ( ( ( flag & 64 ) >> 6 ) == 0 )
		kind = 4;
	    else
		kind = 5;
        }

	/*
	 * BYTES 5-6
	 * Set the scale factor (E).
	 */

	indx = 4; 
	bds.binary_scale = gb_btoi(ptarray, indx, 2, TRUE ); 

	/*
	 * BYTES 7-10
	 * Set the reference value. The reference value is a floating
	 * point number constructed from various parts of the next
	 * four bytes.
	 */

	indx = 6; 
	first_byte = gb_btoi(ptarray, indx, 1, FALSE); 
	isign = first_byte >> 7;
	ia = first_byte & 127;
	indx = 7; 
	ib = gb_btoi(ptarray, indx, 3, FALSE); 

	ref = (float) ( pow(-1.0,(double)isign) *
			 pow(2.0,-24.0) * (double) ib *
			 pow(16.0,(double)ia-64.0) );

	if ( ( ( flag & 32 ) >> 5 ) == 1 ) 
	    ref = (float) ( (int) ref );

	bds.ref_value = ref;

	/*
	 * BYTE 11
	 * Set the number of bits used for the packing.
	 */

	indx = 10; 
	bds.num_bits = gb_btoi(ptarray, indx, 1, FALSE); 
	indx = 11; 

	if ( bds.num_bits == 0 )
	    kind = 0;

	/*
	 * Choose the packing type based on the flags from above.
	 */

	switch ( kind ) {
	    case 0:
		    for ( jj = 0; jj < kxky*2/sizeof(int)+1; jj++ ) {
			xgrid[jj] = 0;
		    }
		    bds.num_bits = 2;
		    break;
	    case 1:
		    gb_gspk(ptarray, indx, length, xgrid);
		    break;
	    case 2:
		    gb_gsec(ptarray, indx, length, xgrid);
		    break;
	    case 3:
		    gb_gmtx(ptarray, indx, length, xgrid);
		    break;
	    case 4:
		    gb_sspk(ptarray, indx, length, xgrid);
		    break;
	    case 5:
		    gb_scpk(ptarray, indx, length, xgrid);
		    break;
	    default:
		    break;
	}

	if ( GBDIAG_BDS == TRUE ) {
	    printf ( " BDS bytes  1 -  3 (bds.length)       = %d\n", bds.length );
	    printf ( " BDS byte        4 (bds.flag)         = %d\n", bds.flag );
	    printf ( " BDS bytes  5 -  6 (bds.binary_scale) = %d\n", bds.binary_scale );
	    printf ( " BDS bytes  7 - 10 (bds.ref_value)    = %f\n", bds.ref_value );
	    printf ( " BDS byte       11 (bds.num_bits)     = %d\n", bds.num_bits );
	}

	return;

}
