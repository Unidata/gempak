#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_mtyp ( char filnms[][LLPATH], int numfil, int *iret )
/************************************************************************
 * shp_mtyp                                                             *
 *                                                                      *
 * This function detects the map type from the input file names.	*
 *                                                                      *
 * shp_mtyp ( filnms, numfil, iret )                           		*
 *                                                                      *
 * Input parameters:                                                    *
 *      filnms[][LLPATH]	char		Input file names	*
 *	numfil			int		Number of files		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          6/04   	Initial coding                  *
 * R. Tian/SAIC          2/05   	Detect using file names         *
 * R. Tian/SAIC          4/05   	Added ice edge map type		*
 * R. Tian/SAIC          4/05   	Added RFC basin map type	*
 * R. Tian/SAIC          6/05   	Added time zone and us state	*
 * R. Tian/SAIC         10/05   	Added Mexico states		*
 * m.gamazaychikov/SAIC 06/08   	Added CN prov, Offshore Marine  *
 *                                      zones, and undef map types	*
 * S. Jacobs/NCEP	 4/10		Added Predictive Service Areas	*
 * X. Guo/CWS            4/12           Changed NPSA naming convention  *
 ***********************************************************************/
{
    int nf;
/*---------------------------------------------------------------------*/
    *iret = 0;
    maptyp = 0;

    for ( nf = 0; nf < numfil; nf++ ) {
        if ( strncmp ( filnms[nf], "c_", 2 ) == 0 ) {
            /*
             * AWIPS Counties.
             */
	    maptyp |= MAP_CNTY;
	} else if ( strncmp ( filnms[nf], "mz", 2 ) == 0 ) {
            /*
             * Coastal and Offshore Marine Zones.
             */
	    maptyp |= MAP_MARI;
	} else if ( strncmp ( filnms[nf], "z_", 2 ) == 0 ) {
            /*
             * Public Zone Boundaries.
             */
	    maptyp |= MAP_ZONE;
        } else if ( strncmp ( filnms[nf], "fz", 2 ) == 0 ) {
            /*
             * Fire Weather Zone Boundaries.
             */
            maptyp |= MAP_FIRE;
	} else if ( strncmp ( filnms[nf], "rf", 2 ) == 0 ) {
            /*
             * River Forecast Center Regions.
             */
            maptyp |= MAP_RFCR;
        } else if ( strncmp ( filnms[nf], "w_", 2 ) == 0 ) {
            /*
             * County Warning Area Boundaries.
             */
            maptyp |= MAP_CWAB;
        } else if ( strncmp ( filnms[nf], "nic", 3 ) == 0 ) {
            /*
             * Ice Edge.
             */
            maptyp |= MAP_IEDG;
        } else if ( strncmp ( filnms[nf], "ba", 2 ) == 0 ) {
            /*
             * River Forecast Center Basin.
             */
            maptyp |= MAP_RFCB;
        } else if ( strncmp ( filnms[nf], "s_", 2 ) == 0 ) {
            /*
             * US State and Territories.
             */
            maptyp |= MAP_USST;
        } else if ( strncmp ( filnms[nf], "timezn", 6 ) == 0 ) {
            /*
             * Time Zone.
             */
            maptyp |= MAP_TMZN;
        } else if ( strncmp ( filnms[nf], "states", 6 ) == 0 ) {
            /*
             * Mexico States.
             */
            maptyp |= MAP_MXST;
        } else if ( strncmp ( filnms[nf], "province", 8 ) == 0 ) {
            /*
             * Canadian Proovinces.
             */
            maptyp |= MAP_CNPR;
        } else if ( strncmp ( filnms[nf], "oz", 2 ) == 0 ) {
            /*
             * Offshore Marine Zones.
             */
            maptyp |= MAP_OFSH;
        } else if ( strncmp ( filnms[nf], "National_PSA", 12 ) == 0 ) {
            /*
             * Predictive Service Areas
             */
            maptyp |= MAP_NPSA;
        } else {
            /*
             * Undefine Map type.
             */
            maptyp |= MAP_UNDF;
        }
    }
}
