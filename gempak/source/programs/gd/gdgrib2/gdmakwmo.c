#include "gdgrib2.h"
#include "ctbcmn.h"


void gdmakewmo ( GDG2_input *input, GDG2_gemgrid *gemgrid, char *chdr, 
                 int *iret )
/************************************************************************
 * gdmakewmo                                                            *
 *                                                                      *
 * This routine generates a WMO Header from user input parameter WMOHDR *
 * and gempak grid date/time info, if necessary.                        *
 *                                                                      *
 *  Usage:                                                              *
 *      gdmakewmo( input, gemgrid, chdr, iret )                         *
 *                                                                      *
 *  Input Arguments:                                                    *
 *      *input            GDG2_input       input structure for GDGRIB2  *
 *      *gemgrid          GDG2_gemgrid     grid and Gempak info         *
 *                                                                      *
 *  Output Arguments:                                                   *
 *      *chdr              char            WMO Header                   *
 *      *iret              int             Error return code.           *
 *                                           0 = Normal                 *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP	 8/05	Orig					*
 * S. Gilbert/NCEP	 8/05	Changed string manipulations		*
 * S. Jacobs/NCEP	 2/11	Replaced strncpy with cst_ncpy		*
 ***********************************************************************/
{
    int    ret, num, j, ier2;
    int    numparts = 3;
    char   cparts[numparts][LLMXLN], *plist[numparts], *cdef="\0";


    /*
     *  Separate WMO Header parts from user input
     */
    for ( j=0; j<numparts; j++) plist[j] = cparts[j];
    cst_clst( input->wmohdr, '/', cdef, numparts, LLMXLN, plist, &num, &ret);

    /*
     *  Set first six characters of WMO Header
     */
    if ( strlen( plist[0] ) == (size_t)6 ) {
       cst_ncpy( chdr, plist[0], 6, &ret );
    }
    else {
       *iret = -9;
       strcpy( chdr, cdef );
       return;
    }

    /*
     *  Insert space after first six characters of WMO Header
     */
    strcat( chdr, " " );

    /*
     *  Set originating center identifier.
     */
    if ( strlen( plist[1] ) == (size_t)4 ) {
       strcat( chdr, plist[1] );
    }
    else {
       strcat( chdr, "KWBC" );
    }

    /*
     *  Insert space after originating center identifier.
     */
    strcat( chdr, " " );

    /*
     *  Set Date/Time group
     */
    if ( strlen( plist[2] ) == (size_t)6 ) {
       strcat( chdr, plist[2] );
    }
    else {
       cst_ncpy ( chdr+12, gemgrid->ctime[0]+4, 2, &ier2 );
       cst_ncpy ( chdr+14, gemgrid->ctime[0]+7, 4, &ier2 );
    }

    /*
     *  Add CR-CR-LF
     */
    chdr[18] = CHCR;
    chdr[19] = CHCR;
    chdr[20] = CHLF;
    chdr[21] = CHNULL;

    return;

}
