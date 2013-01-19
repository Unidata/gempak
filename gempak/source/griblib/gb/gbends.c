#include "gbcmn.h"

void gb_ends ( unsigned char *ptarray )
/************************************************************************
 * gb_ends								*
 *									*
 * This function decodes section 5 (End Section) of a GRIB message.	*
 * The end section should be equal to "7777".				*
 *									*
 * gb_ends ( ptarray )							*
 *									*
 * Input parameters:							*
 *	*ptarray	unsigned char	Data buffer			*
 **									*
 * Log:									*
 * J. Chou/EAI		02/93						*
 * S. Jacobs/EAI        11/93	Clean up; Added GBDIAG prints		*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * D.W.Plummer/NCEP	 2/96	Cleanup GBDIAGs and comments		*
 * T. Piper/GSC		11/98	Updated prolog				*
 * S. Jacobs/NCEP	 4/99	Added a NULL to the end of end_string	*
 ***********************************************************************/
{
	int    i;

/*---------------------------------------------------------------------*/

	for ( i = 0; i < 4; i++ )
		es.end_string[i] = ptarray[i]; 

	es.end_string[4] = CHNULL;

	if ( GBDIAG_END == TRUE )
	    printf ( " END bytes  1 -  4 = %s\n", es.end_string );
}
