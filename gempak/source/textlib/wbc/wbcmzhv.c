#include "geminc.h"
#include "gemprm.h"

#define  NUM_MZ   16 

static char	*_mzones[] = { "AM", "AN", "GM", "PH", "PK",
                               "PM", "PS", "PZ", "LH", "LO",
			       "LM", "LE", "LS", "CW", "LC",
			       "SL" };

void wbc_mzhv ( char *states, Boolean *hvmz, int *iret )
/************************************************************************
 * wbc_mzhv								*
 *                                                                      *
 * This function checks the state string for any marine zones. 		*
 *                                                                      *
 * wbc_mzhv ( states, hvmz, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*states		char		String of state ids		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*hvmz		char		States id string; no Mar. Zones *
 *					  true = have >= 1 mar. zones   *
 *					 false = no mar. zones found    *
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP          5/03						*
 * A. Hardy/NCEP          9/04	Added SL -St. Lawrence Seaway		*
 ***********************************************************************/
{
    int     ii;
    char    *ptr;
/*-------------------------------------------------------------------*/
    *iret = 0;
    *hvmz  = False;

   /*
    * Check to see if marine zones are listed in the state string.
    */

    ii = 0;

    while ( ( ii <  NUM_MZ ) && ( !*hvmz )) {
	ptr = (char *) strstr ( states, _mzones[ii]);
	if ( ptr != (char *)NULL )  {
	    *hvmz = True;
	}
	ii++;
    }

}
