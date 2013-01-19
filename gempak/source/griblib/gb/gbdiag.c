#include "gbcmn.h"

int	GBDIAG_IDS;
int	GBDIAG_PDS;
int	GBDIAG_GDS;
int	GBDIAG_BMS;
int	GBDIAG_BDS;
int	GBDIAG_END;

void gb_diag ( char *gbdiag, int *iret )
/************************************************************************
 * gb_diag								*
 *									*
 * This function sets the GRIB gbdiag flags.				*
 *									*
 * gb_diag ( gbdiag, iret )						*
 *									*
 * Input parameters:							*
 *	*gbdiag		char		GBDIAG string			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/96	New					*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 ***********************************************************************/
{

char	*cd, diag[73];

/*---------------------------------------------------------------------*/
	*iret = 0;

	gbdiag[71] = '\0';
	strcpy( diag, gbdiag );

	GBDIAG_IDS = FALSE;
	GBDIAG_PDS = FALSE;
	GBDIAG_GDS = FALSE;
	GBDIAG_BMS = FALSE;
	GBDIAG_BDS = FALSE;
	GBDIAG_END = FALSE;

	cd = strtok( diag, ";" );
	while ( cd != NULL ) {
		if ( strncmp( cd, "ALL", 3 ) == 0 ) {
			GBDIAG_IDS = TRUE;
			GBDIAG_PDS = TRUE;
			GBDIAG_GDS = TRUE;
			GBDIAG_BMS = TRUE;
			GBDIAG_BDS = TRUE;
			GBDIAG_END = TRUE;
			break;
		}
		else if ( strncmp( cd, "IDS", 3 ) == 0 )
			GBDIAG_IDS = TRUE;
		else if ( strncmp( cd, "PDS", 3 ) == 0 )
			GBDIAG_PDS = TRUE;
		else if ( strncmp( cd, "GDS", 3 ) == 0 )
			GBDIAG_GDS = TRUE;
		else if ( strncmp( cd, "BMS", 3 ) == 0 )
			GBDIAG_BMS = TRUE;
		else if ( strncmp( cd, "BDS", 3 ) == 0 )
			GBDIAG_BDS = TRUE;
		else if ( strncmp( cd, "END", 3 ) == 0 )
			GBDIAG_END = TRUE;
		cd = strtok( NULL, ";" );
	}

}
