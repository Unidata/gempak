#include "gbcmn.h"

void gb_gids ( FILE *ifdes, int *iret )
/************************************************************************
 * gb_gids                                                              *
 *                                                                      *
 * This function gets the GRIB information from GB_IDS and passes	*
 * that information back to a function as independent variables.	*
 *                                                                      *
 * gb_gids ( ifdes, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ifdes		FILE		GRIB file number		*
 *                                                                      *
 * Output parameters:        						*
 *	*iret		int		Return code			*
 *                                          -19 = Error on message      *
 **                                                                     *
 * Log:                                                                 *
 * J. Chou/EAI           7/93                                           *
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * S. Jacobs/NMC	 3/95		Changed read from 4 to 8 chars	*
 * L. Sager/NMC		 8/95		Fix memory allocation error   	*
 * S. Jacobs/NCEP	 1/96		Changed DA_READ to CFL_READ	*
 * D.W.Plummer/NCEP	 3/96		Changed cfl_read call sequence	*
 * T. Piper/GSC		11/98		Updated prolog			*
 ***********************************************************************/
{ 
	int		offset, ret, nbytes;
	unsigned char	*buffer;

/*---------------------------------------------------------------------*/
	*iret = 0;

	offset = cursor;
/* 
 *      Allocate space for the buffer.
 */
        buffer = (unsigned char *)
                  malloc ( 8 * sizeof(unsigned char) );
	cfl_seek ( ifdes, offset, SEEK_SET, &ret );
	cfl_read ( ifdes, 8, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -19;
	    return;
	}

	gb_ids ( buffer );

	free ( buffer );
}	
