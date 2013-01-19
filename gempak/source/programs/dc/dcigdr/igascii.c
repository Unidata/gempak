#include "geminc.h"
#include "gemprm.h"
#include "bridge.h"

void ig_ascii ( char *filnam, int irptdt[5], float *lat, float *lon,
			float *sgwh, int *iret )
/************************************************************************
 * ig_ascii								*
 *									*
 * This subroutine writes the ASCII output to the ASCII	output stream.	*
 *									*
 * void ig_ascii ( filnam, orbn, lat, lon, irptdt, sgwh, iret )		*
 *									*
 * Input parameters:							*
 *	*filnam		char	Output filnam				*
 *	irptdt[5]	int	Date/Time				*
 *	*lat		float	Latitude				*
 *	*lon		float	Longitude				*
 *	*sgwh		float	Significant Wave Height			*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code:				*
 *				  0 = normal return			*
 **									*
 * Log:									*
 * T. Piper/SAIC	03/06	Created.				*
 * S. Chiswell/Unidata	10/07	Updated for dynamic file names		*
 ***********************************************************************/
{
	static FILE	*fptr=NULL;
	static char	*lastfile=NULL;
/*---------------------------------------------------------------------*/
	*iret = 0;

	if ( ( lastfile == NULL ) || ( strcmp(lastfile, filnam) != 0 ) ) {
	    if ( lastfile != NULL ) free(lastfile);
	    lastfile = (char *)malloc(strlen(filnam)+1);
            cst_ncpy(lastfile, filnam, strlen(filnam), iret);
	    if ( fptr != NULL ) {
		cfl_clos ( fptr, iret );
		fptr = NULL;
	    }
	}

	if ( fptr == NULL ) {
	    fptr = cfl_wopn( filnam, iret );
	    if ( *iret != 0 )  return;
	}

	fprintf(fptr, "%i %i %i %i %i %.6f %.6f %.6f\n",
			irptdt[0], irptdt[1], irptdt[2],
			irptdt[3], irptdt[4], *lat, *lon, *sgwh);
}
