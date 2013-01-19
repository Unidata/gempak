#include "gbcmn.h"

void gb_ggds ( float *gdsarr, int *irltv, int *nrmflg, int *iret )
/************************************************************************
 * gb_ggds								*
 *									*
 * This function gets the GRIB information from GB_GDS and passes	*
 * that information back to a function as independent variables.	*
 *									*
 * gb_ggds ( gdsarr, irltv, nrmflg, iret )				*
 *									*
 * Output parameters:							*
 *	*gdsarr 	float		GDS information array		*
 *					   (GRIB projection number,	*
 *					   number of columns of data,	*
 *					   number of rows of data,	*
 *					   lat/lon of corners)		*
 *	*irltv		int		Resolution flag			*
 *	*nrmflg		int		Flag for "normal" grid		*
 *					   (i.e., the grid is NOT	*
 *					   rotated or stretched and	*
 *					   the scanning mode is OK)	*
 *	*iret		int		Return code			*
 *					-19 = error on message		*
 **									*
 * Log:									*
 * J. Chou/EAI		 7/93						*
 * S. Jacobs/EAI	11/93	Changed GDS info array to FLOAT		*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * L. Sager		 8/95	Fix memory allocation error		*
 * S. Jacobs/NCEP	 1/96	Changed DA_READ to CFL_READ		*
 * D.W.Plummer/NCEP	 3/96	Changed cfl_ call sequence		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 ***********************************************************************/
{
	int	offset, ret, temp, idrct, jdrct, consec, nbytes, indx;
	unsigned char	*buffer;

/*---------------------------------------------------------------------*/
	*iret = 0;

	offset = cursor + ISLENGTH + pdslength;
/*
 *	Allocate space for the buffer.
 */
        buffer = (unsigned char *)
                  malloc ( 3       * sizeof(unsigned char) );
	cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	cfl_read ( gbfile.fptr, 3, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -19;
	    return;
	}

	indx = 0;
	temp = gb_btoi ( buffer, indx, 3, FALSE );
        free ( buffer );
/*
 *      Allocate space for the buffer.
 */
        buffer = (unsigned char *)
                  malloc ( temp    * sizeof(unsigned char) );
	cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	cfl_read ( gbfile.fptr, temp, buffer, &nbytes, &ret );

	if ( ret != 0 ) {
	    *iret = -19;
	    return;
	}

	gb_gds ( buffer );

	gdsarr[0] = (float) gds.grid_proj;
	gdsarr[1] = (float) gds.kx;
	gdsarr[2] = (float) gds.ky;
	gdsarr[3] = gds.latll;
	gdsarr[4] = gds.lonll;
	gdsarr[5] = gds.latur;
	gdsarr[6] = gds.lonur;
	gdsarr[7] = gds.angle1;
	gdsarr[8] = gds.angle2;
	gdsarr[9] = gds.angle3;

	*irltv = ( gds.flag1 >> 3 ) & 1;

	idrct  = ( gds.scan_mode >> 7 ) & 1;
	jdrct  = ( gds.scan_mode >> 6 ) & 1;
	consec = ( gds.scan_mode >> 5 ) & 1;

	if ( idrct == 0 && jdrct == 1 && consec == 0 ) {
	    *nrmflg = TRUE;
	}
	else {
	    *nrmflg = FALSE;
	}

	gdslength = temp;

	free ( buffer );
}
