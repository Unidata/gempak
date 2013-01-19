#include "gbcmn.h"

void gb_read ( int *mxgrib, int *ipack, int *iret )
/************************************************************************
 * gb_read                                                              *
 *                                                                      *
 * This function reads an entire grib message.				*
 *                                                                      *
 * gb_read ( mxgrib, ipack, iret )                              	*
 *                                                                      *
 * Input parameters:							*
 *	*mxgrib		int		Maximum grib message allowed	*
 * Output parameters:                                                   *
 *      *ipack          int             Entire GRIB message             *
 *      *iret           int             Return code                     *
 *                                       -19 = error reading GRIB file  *
 *					   1 = grid too large		*
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	10/02	Created					*
 * T. Piper/SAIC	04/03	Added mxgrib parameter and check	*
 * T. Piper/SAIC	05/03	Fixed calculation of totlen		*
 ***********************************************************************/
{
int		ier, nbytes, totlen;
long		offset;

/*---------------------------------------------------------------------*/

	*iret = 0;
	offset = (long)cursor;
	totlen = cursor1 - cursor;
	if ( totlen <= *mxgrib ) {
 
	    cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ier );
            cfl_read ( gbfile.fptr, totlen, (unsigned char *)ipack, 
							&nbytes, &ier );
	    if ( ier != 0 ) {
                *iret = -19;
            }
	}
	else {
	    *iret = 1;
	}
}
