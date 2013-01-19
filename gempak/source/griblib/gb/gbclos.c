#include "gbcmn.h"

void gb_clos ( int *iret )
/************************************************************************
 * gb_clos								*
 *									*
 * This function will close a GRIB file and its INDEX file.             *
 *                                                                      *
 * gb_clos ( iret )                             		        *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret		int		Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * J. Chou/EAI           7/93                                           *
 * S. Jacobs/EAI         7/93           Clean up                        *
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * S. Jacobs/NCEP	 1/96		Changed DA_CLOS to CFL_CLOS	*
 * T. Piper/GSC		11/98		Updated prolog			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = 0;

/*
**	Close the INDEX file, if it was opened.
*/

        if ( infile.fptr != NULL )
            cfl_clos ( infile.fptr, iret );

/*
**	Close the GRIB file.
*/
        cfl_clos ( gbfile.fptr, iret );

/*
**	Check the status of closing the file.
*/
	if( *iret != 0 )
	    *iret = -15;

}

