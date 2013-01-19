#include "gbcmn.h"

void gb_ids ( unsigned char *ptarray )
/************************************************************************
 * gb_ids								*
 *									*
 * This function decodes section 0 (Indicator Section) of a GRIB	*
 * message.								*
 *									*
 * gb_ids ( ptarray )							*
 *									*
 * Input parameters:							*
 *	*ptarray	unsigned char	Data buffer			*
 **									*
 * Log:									*
 * J. Chou/EAI		02/93						*
 * S. Jacobs/EAI        11/93	Clean up; Added GBDIAG prints   	*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * D.W.Plummer/NCEP	 2/96	Cleanup GBDIAGs and comments		*
 * D.W.Plummer/NCEP	 3/99	Correct printf statement		*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 ***********************************************************************/
{
int	indx;

/*---------------------------------------------------------------------*/

	/*
	 * BYTES 1-4
	 * "GRIB"
	 */

	/*
	 * BYTES 5-7
	 * Get the length of the entire message.
	 * (ptarray starts with byte 5)
	 */
	indx = 4;
	ids.msg_length = gb_btoi(ptarray, indx, 3, FALSE);

	/*
	 * BYTE 8
	 * Get the GRIB edition number.
	 */
	indx = 7;
	ids.edition = gb_btoi(ptarray, indx, 1, FALSE);

	if ( GBDIAG_IDS == TRUE )  {
	    printf(" IDS bytes 0 -  4 (GRIB msg id)    = GRIB\n" );
	    printf(" IDS bytes 5 -  7 (ids.msg_length) = %d\n",
		ids.msg_length );
	    printf(" IDS byte       8 (ids.edition)    = %d\n",
		ids.edition );
	}

}
