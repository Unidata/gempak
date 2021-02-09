#include "fortran_wrappers.h"

void cti_ccnt ( const char *dattim, char *cent, int *iret )
/************************************************************************
 * TI_CCNT								*
 *									*
 * This subroutine gets the first 2 digits of a 4-digit year based on   *
 * the standard GEMPAK time.  Any 2-digit year less than or equal to 40 *
 * will be assumed to be in the 21st century; years greater than 40 will*
 * be assumed to be in the 20th century.                                *
 *									*
 * TI_CCNT  ( DATTIM, CENT, IRET )					*
 *									*
 * Input parameters:							*
 *	DATTIM		CHAR*		GEMPAK time			*
 *									*
 * Output parameters:							*
 *	CENT    	CHAR*  		First 2 digits of year          *
 *	IRET		INTEGER		Return code			*
 *					  0 = normal return		*
 *					 -1 = invalid time		*
 *					 -7 = invalid year		*
 **									*
 * Log:									*
 * R. Tian/SAIC		 4/06	C wrapper of TI_CCNT			*
 * B. Hebbard/NCEP	 3/18   Moved century break from 2020 to 2040   *
 ************************************************************************/
{
    char tmpcen[3];
/*----------------------------------------------------------------------*/
    /*
     * CALL TI_CCNT.
     */
    ti_ccnt ( (char *)dattim, tmpcen, iret, strlen(dattim), sizeof(tmpcen) );

    /*
     * Convert Fortran string to C string.
     */
    tmpcen[2] = '\0';
    strcpy ( cent, tmpcen );

    return;
}
