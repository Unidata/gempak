#include "gbcmn.h"

void gb_ges ( int *iret )
/************************************************************************
 * gb_ges								*
 *									*
 * This function gets the GRIB information from GB_ENDS and passes	*
 * that information back to a function as independent variables.	*
 *									*
 * gb_ges ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					-19 = error on message		*
 **									*
 * Log:									*
 * J. Chou/EAI		 7/93						*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * L. Sager		 8/95	Fix memory allocation error		*
 * S. Jacobs/NCEP	 1/96	Changed DA_READ to CFL_READ		*
 * D.W.Plummer/NCEP	 3/96	Changed cfl_ call sequence		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 ***********************************************************************/
{
	int		offset, ret, nbytes;
	unsigned char	*buffer;

/*---------------------------------------------------------------------*/
	*iret = 0;

	offset = cursor + ISLENGTH + pdslength + gdslength +
			  bmslength + bdslength;
	buffer = (unsigned char *)
                 malloc ( 4       * sizeof(unsigned char) );
	cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	cfl_read ( gbfile.fptr, 4, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -19;
	    return;
	}

	gb_ends ( buffer );

	free ( buffer );
}
