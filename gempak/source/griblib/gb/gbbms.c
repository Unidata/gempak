#include "gbcmn.h"

void gb_bms ( unsigned char *ptarray )
/************************************************************************
 * gb_bms								*
 *									*
 * This functions decodes section 3 (Bit Map Section) of a GRIB		*
 * message.								*
 *									*
 * gb_bms ( ptarray )							*
 *									*
 * Input parameters:							*
 *	*ptarray	unsigned char	Data buffer			*
 **									*
 * Log:									*
 * J. Chou/EAI		02/93						*
 * S. Jacobs/EAI        11/93           Clean up; Added GBDIAG prints   *
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * D.W.Plummer/NCEP	 2/96		Cleanup GBDIAGs and comments	*
 * S. Jacobs/NCEP	12/00		Added prototypes		*
 ***********************************************************************/
{
int	indx;

/*---------------------------------------------------------------------*/

	/*
	 * BYTES 1-3
	 * Get the length of the section.
	 */
	indx = 0;
	bms.length = gb_btoi(ptarray, indx, 3, FALSE);

	/*
	 * BYTE 4
	 * Get the number of unused bits at end of the section.
	 */
	indx = 3;
	bms.unused_bits = gb_btoi(ptarray, indx, 1, FALSE);

	/*
	 * BYTES 5-6
	 * If the value is zero, then a bit map follows. Otherwise
	 * use a predefined bitmap provided by the center.
	 */
	indx = 4;
	bms.table = gb_btoi(ptarray, indx, 2, FALSE);

	if ( GBDIAG_BMS == TRUE )  {
	    printf( " BMS bytes  1 -  3 (bms.length)      = %d\n",
		bms.length );
	    printf( " BMS byte        4 (bms.unused_bits) = %d\n",
		bms.unused_bits );
	    printf( " BMS bytes  5 -  6 (bms.table)       = %d\n",
		bms.table );
	}

}
