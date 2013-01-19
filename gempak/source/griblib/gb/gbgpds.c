#include "gbcmn.h"

void gb_gpds ( int *itime, int *iaccm, int *ilevel, int *ivcord, 
		int *iparm, int *igrdnm, int *icodtbl, int *icenter, 
		int *isgds, int *isbms, int *ispdse, char *cpds, 
		int *iclen, int *iret )
/************************************************************************
 * gb_gpds								*
 *									*
 * This function gets the GRIB information from GB_PDS and passes	*
 * that information back to a function as independent variables.	*
 *									*
 * gb_gpds ( itime, iaccm, ilevel, ivcord, iparm, igrdnm,		*
 *	     icodtbl, icenter, isgds, isbms, ispdse, cpds, iclen, iret )*
 *									*
 * Output parameters:							*
 *	*itime  	int		Date, time, forecast array	*
 *	*iaccm		int		Accumulation time		*
 *	*ilevel 	int		Level(s)			*
 *	*ivcord		int		Vertical coord number		*
 *	*iparm		int		Parameter number		*
 *	*igrdnm		int		GRIB grid proj number		*
 *	*icodtbl	int		Parameter code table version	*
 *	*icenter	int		Originating center number	*
 *	*isgds		int		GDS present flag		*
 *	*isbms		int		BMS present flag		*
 *	*ispdse		int		PDS present flag		*
 *	*cpds		char		PDS extension			*
 *	*iclen		int		Length of PDS extension		*
 *	*iret		int		Return code			*
 *					-19 = error on message		*
 **									*
 * Log:									*
 * J. Chou/EAI		 7/93						*
 * S. Jacobs/EAI	11/93	Fixed coord decoding			*
 * S. Jacobs/EAI	12/93	Added center number			*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * L. Sager/NMC		 8/95	Fix memory allocation error		*
 * S. Jacobs/NCEP	 1/96	Changed DA_READ to CFL_READ		*
 * D.W.Plummer/NCEP	 2/96	Fix level processing,			*
 *				Add pds extension processing		*
 * D.W.Plummer/NCEP	 3/96	Change cfl_ call sequence		*
 * K. Brill/EMC		 9/96	Use combined level for 119		*
 * D.W.Plummer/NCEP	10/96	Added more vertical levels		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * D.W.Plummer/NCEP     11/97   Added cases 10,11,12 for pds.time_unit  *
 * S. Jacobs/NCEP	 4/99	Added special case for RFC precip data	*
 * D.W.Plummer/NCEP	10/99	Bug fix for pds.time_range == 1		*
 * S. Jacobs/NCEP	 2/00	Fixed year for date/time/fcst array	*
 * Chiz/Unidata		 3/00	Created gb_ftim and gb_vlev subroutines	*
 * M. Li/GSC		 5/00	Cleaned up				*
 ***********************************************************************/
{
	int		offset, ret, nbytes, ier;
	unsigned char	*buffer;

/*---------------------------------------------------------------------*/
	*iret = 0;

	offset = cursor;

	/*
	 * Allocate space for the Indicator Section buffer.
	 */

	buffer = (unsigned char *)
                  malloc ( 8 * sizeof(unsigned char) );
	cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	cfl_read ( gbfile.fptr, 8, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -19;
	    return;
	}

	gb_ids ( buffer );

	offset = cursor + ISLENGTH;

	cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	cfl_read ( gbfile.fptr, 3, buffer, &nbytes, &ret );
	if ( ret != 0 ) {
	    *iret = -19;
	    return;
	}

	pds.length = gb_btoi(buffer, 0, 3, FALSE);

        free ( buffer );

/*
 *      Allocate space for the Parameter Definition Section buffer.
 */
        buffer = (unsigned char *)
                  malloc ( pds.length * sizeof(unsigned char) );
	cfl_seek ( gbfile.fptr, offset, SEEK_SET, &ret );
	cfl_read ( gbfile.fptr, pds.length, buffer, &nbytes, &ret );
        if ( ret != 0 ) {
	    *iret = -19;
	    return;
	}

	gb_pds ( buffer );

	*isgds   = pds.isgds;
	*isbms   = pds.isbms;
	*iparm   = pds.parameter;
	*ivcord  = pds.vcoord;
	*icodtbl = pds.version;
	*icenter = pds.center;
	*igrdnm  = pds.grid_id;

	*ispdse = pds.pdse;
	strcpy ( cpds, pds.extension );
	*iclen = strlen( cpds );

	gb_ftim(itime, iaccm, &ier);

	gb_vlev(ilevel, &ier);

	pdslength = pds.length;
	free ( buffer );
}
