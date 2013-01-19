#include "geminc.h"
#include "gemprm.h"

void wbc_dsts ( char *states, int *len1, int *irmzn, char *stzstr, 
                char *sttstr, int *iret )
/************************************************************************
 * wbc_dsts								*
 *                                                                      *
 * This function creates the formatted state zones string and formatted *
 * state names string.							*
 *                                                                      *
 * wbc_dsts ( states, len1,  irmzn, stzstr, sttstr, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*states		char		String of state ids		*
 *	*len1		int		Max length of stzstr		*
 *	irmzn		int		Flag for removing marine zone id*
 *                                         0 - do not remove		*
 *                                         1 - remove			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*stzstr		char		States zone string 		*
 *	*sttstr		char		State names string 		*
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP          5/03						*
 * A. Hardy/NCEP          3/04	Added wbc_mzrm,chg stzon,stnam array sz *
 * A. Hardy/NCEP          3/05	Added flag for removing mz from string  *
 ***********************************************************************/
{
    int     ier, ier1, len, lens;
    char    stzon[500], *arrch, name[22], stnam[500], hold[50];
/*-------------------------------------------------------------------*/
    *iret = 0;
    ier   = 0;
    ier1 = 0;
    stzon[0] = '\0';
    stnam[0] = '\0';
    stzstr[0] = '\0';
    sttstr[0] = '\0';

    cst_lstr ( states, &lens, &ier);
    cst_ncpy ( stnam, states, lens, &ier );

   /*
    * Check if the specific marine zone ids should be removed.
    */

    if ( *irmzn == 1 ) {
        wbc_mzrm ( stnam, states, &len, &ier );
    }

   /*
    * Create state zones string.
    */

    arrch = strtok( states, " " );
    while ( arrch != NULL ) {
        if ( strlen (stzon) > (size_t)0 ) strcat ( stzon, "-" );
        strcat ( stzon, arrch );
        strcat ( stzon, "Z000" );
        arrch = strtok( NULL," " );
    }
    len = G_MIN ( *len1, (int)strlen (stzon) );
    cst_ncpy ( stzstr, stzon, len, &ier);

   /*
    * Create full state names string.
    */

    hold[0] = '\0';
    stzon[0] = '\0';
    arrch = strtok( stnam, " " );

    while ( ( arrch != NULL ) && ( ier1 == 0 ) ) {
        tb_idst  ( arrch, name, &ier, strlen(arrch), sizeof(name) ); 
	sprintf ( hold, "\n         %-20s", name);
	cst_ncat ( sttstr, hold, &len, &ier  );
	cst_ncpy ( states, arrch, strlen(arrch), &ier1 );
	arrch = strtok( NULL," " );
    }
    if ( ier1 != 0 ) {
	*iret = ier;
    }
}
