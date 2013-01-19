#include "na.h"

void na_smry ( FILE **fps, const int *nfps, const int *nread,
               const int *nwrit, const char *infil, const char *outfil,
	       int *iret )
/************************************************************************
 * na_smry								*
 *									*
 * This routine will print out a short summary of the number of grids   *
 * processed.                                                           *
 *									*
 * na_smry ( fps, nfps, nread, nwrit, infil, outfil, iret)       	*
 *									*
 * Input parameters:							*
 *	**fps		FILE		FILE pointers for output	*
 *	*nfps		const int	Number of outputs		*
 *	*nread		const int	Num of msgs read from GRIB file *
 *	*nwrit		const int	Num of fields written to GEMPAK *
 *	*infil		const char	Input GRIB file.                *
 *	*outfil		const char	Output Gempak grid file.        *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Gilbert/NCEP	11/04						*
 * R. Tian/SAIC		 8/06		Recoded from Fortran		*
 ************************************************************************/
{
    int ii;
/*----------------------------------------------------------------------*/
    *iret = 0;

    for ( ii = 0; ii < *nfps; ii++ ) {
    	fprintf ( fps[ii], "\n\n     %5d GRIB messages were read or scanned from the GRIB file:\n     %72.72s\n\n", *nread, infil );

	if ( strcmp ( outfil, "list" ) != 0 &&
	     strcmp ( outfil, "LIST" ) != 0 ) {
	    fprintf ( fps[ii], "     %5d grids were written to the GEMPAK file:\n     %72.72s\n\n", *nwrit, outfil );
	}
    }
 
    return;
}
