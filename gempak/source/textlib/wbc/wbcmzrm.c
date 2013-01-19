#include "geminc.h"
#include "gemprm.h"

#define  NUM_ZNS   10 

static char	*_mzones[] = { "AM", "AN", "GM", "PH", "PK",
                               "PM", "PS", "PZ", "LC", "SL" };

void wbc_mzrm ( char *states, char *ststr, int *len1, int *iret )
/************************************************************************
 * wbc_mzrm								*
 *                                                                      *
 * This function removes the specific marine zones listed above from	*
 * the state id string. 						*
 *                                                                      *
 * wbc_mzrm ( states, len1, len2, stzstr, sttstr, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*states		char		String of state ids		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*ststr		char		States id string; no Mar. Zones *
 *	*len1		int		Length of ststr			*
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP          5/03						*
 * A. Hardy/NCEP          9/04		Added "SL"			*
 * T. Piper/SAIC	02/05	Removed unused variable 'stnam'		*
 ***********************************************************************/
{
    int     ii, ier, ilen, ipos;
/*-------------------------------------------------------------------*/
    *iret = 0;
    ier   = 0;
    ststr[0] = '\0';

   /*
    * Remove unwanted marine zones listed above from state strings.
    */

    ii = 0;

    while ( ( ii <  NUM_ZNS ) ) {
	cst_rmst ( states, _mzones[ii], &ipos, states, &ier);
	if ( ipos > 0 ) {
            cst_rxbl ( states, states, &ilen, &ier);
	}
	ii++;
    }

    cst_lstr ( states, &ilen, &ier );
    cst_ncpy ( ststr, states, ilen, &ier );
}
