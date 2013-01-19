#include "geminc.h"
#include "gemprm.h"

void grc_wnmc ( FILE **fps, const int *nfps, const int *title,
                const int *num, const char *gdattm1, const char *gdattm2,
		const int *level1, const int *level2, const int *ivcord,
		const char *parm, const int *nmcprm, const int *nmcvco,
		const int *nmcgdn, int *iret )
/************************************************************************
 * grc_wnmc								*
 *									*
 * This subroutine writes NMC and GEMPAK grid identifier information	*
 * to the specified logical unit using a standard format.  TITLE is 	*
 * set to indicate that the title line:					*
 *									*
 * MESG#  NMCGRD# PRM# VCD#  GEMPAK_TIME  LEVEL1  LEVEL2  VCRD   PARM	*
 *									*
 * is to be written first.  The first three parameters written are	*
 * NMC grid identifiers (grid number, parm number, vertical coord.	*
 * number).								*
 *									*
 * grc_wnmc ( fps, nfps, title, num, gdattm1, gdattm2, level1, level2,	*
 *            ivcord, parm, nmcprm, nmcvco, nmcgdn, iret )		*
 *									*
 * Input parameters:							*
 *	*fps		const FILE	FILE pointers for write		*
 *	*nfps		const int	Number of file for write	*
 *	*title		const int	Flag to write title		*
 *	*num		const int	GRIB message number		*
 *	*gdattm1	const char	GEMPAK time			*
 *	*gdattm2	const char	GEMPAK time			*
 *	*level1		const int	Vertical levels			*
 *	*level2		const int	Vertical levels			*
 *	*ivcord		const int	Vertical coordinate		*
 *	*parm		const char	Parameter name			*
 *	*nmcprm		const int	NMC parameter number		*
 *	*nmcvco		const int	NMC vertical coord. number	*
 *	*nmcgdn		const int	NMC grid number			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * R. Tian/SAIC		 8/06		From GR_WNMC			*
 ************************************************************************/
{
    char vcord[5], dt[19], p[13], lev[7];
    int ii, ier;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Translate vertical coordinate.
     */
    clv_ccrd ( *ivcord, vcord, &ier );

    /*
     * Move character strings into variables with correct length.
     */
    strncpy ( dt, gdattm1, 18 );
    dt[18] = '\0';
    strncpy ( p, parm, 12 );
    p[12] = '\0';

    /*
     * Make level 2 into a string.  Write nothing if level2 = -1.
     */
    if ( *level2 == -1 ) {
	lev[0] = '\0';
    } else {
	cst_inch ( *level2, lev, &ier );
    }

    /*
     * Do prints for all unit numbers.
     */
    for ( ii = 0; ii < *nfps; ii++ ) {
	/*
	 * Write title if requested.
	 */
	if ( *title == G_TRUE ) {
	    fprintf ( fps[ii], "\n MESG# NMCGRD# PRM#   VCD#    GEMPAK_TIME"
	        "       LEVL1 LEVL2  VCRD PARM\n" );
	}

	/*
	 * Write the grid identifier.
	 */
	fprintf ( fps[ii], "%5d  %4d   %4d   %4d    %18.18s%6d %-6.6s %4.4s %-12.12s\n",
	    *num, *nmcgdn, *nmcprm, *nmcvco, dt, *level1, lev, vcord, p );
    }

    return;
}
