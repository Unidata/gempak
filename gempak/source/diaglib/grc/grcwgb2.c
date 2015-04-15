#include "geminc.h"
#include "gemprm.h"

void grc_wgb2 ( FILE **fps, const int *nfps, const int *title,
                const int *num, const char *gdattm1, const char *gdattm2,
		const int *level1, const int *level2, const int *ivcord,
		const char *parm, const int *idis, const int *icat,
		const int *iidn, const int *ipdn, const int *ivco,
		const int *igdn, int *iret )
/************************************************************************
 * grc_wgb2								*
 *									*
 * This subroutine writes GRIB2 and GEMPAK grid identifier information	*
 * to the specified logical unit using a standard format.  TITLE is 	*
 * set to indicate that the title line:					*
 *									*
 *MESG# GDT# DIS# CAT# ID # PDT# VCD# GEMPAK_TIME LEVEL1 LEVEL2 VCRD PARM*
 *									*
 * is to be written first.  The first three parameters written are	*
 * grid identifiers (grid number, discipline number, category number,	* 
 * product identification template number, and vertical coord. number.	*
 *									*
 * grc_wgb2 ( fps, nfps, title, num, gdattm, level, ivcord, parm,       *
 *            idis, icat, iidn, ipdn, ivco, igdn, iret )		*
 *									*
 * Input parameters:							*
 *	**fps		FILE		FILE pointers for write		*
 *	*nfps		const int	Number of files for write	*
 *	*title		const int	Flag to write title		*
 *	*num		const int	GRIB message number		*
 *	*gdattm1	const char	GEMPAK time			*
 *	*gdattm2	const char	GEMPAK time			*
 *	*level1		const int	Vertical levels			*
 *	*level2		const int	Vertical levels			*
 *	*ivcord		const int	Vertical coordinate		*
 *	*parm		const char	Parameter name			*
 *	*idis		const int	GRIB2 discipline number		*
 *	*icat		const int	GRIB2 category number		*
 *	*iidn		const int	GRIB2 id number			*
 *	*ipdn		const int	GRIB2 product definition number	*
 *	*ivco		const int	GRIB2 vertical coord. number	*
 *	*igdn		const int	GRIB2 grid definition template #*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * m.gamazaychikov/SAIC	 5/03						*
 * T. Piper/SAIC        05/03   Fixed title format statement and made   *
 *                              integer variables actually integers!    *
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 * S. Jacobs/NCEP	 6/14	Fixed size of date/time displayed	*
 ************************************************************************/
{
    char vcord[5], dt[21], pm[13], lev[7];
    int ii, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Translate vertical coordinate.
     */
    clv_ccrd  ( *ivcord, vcord, &ier );

    /*
     * Move character strings into variables with correct length.
     */
    strncpy ( dt, gdattm1, 20 );
    dt[20] = '\0';
    strncpy ( pm, parm, 12 );
    pm[12] = '\0';

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
	    fprintf ( fps[ii], "\n MESG# GDT# DIS# CAT# ID#  PDT# VCD#"    
	                       "    GEMPAK_TIME    LEVL1 LEVL2  VCRD PARM\n" );
	}

	/*
	 * Write the grid identifier.
	 */
	fprintf ( fps[ii], "%5d  %3d  %3d  %3d  %3d  %3d  %3d   %20.20s%6d %-6.6s %-4.4s %-12.12s\n", *num, *igdn, *idis, *icat, *iidn, *ipdn, *ivco, dt, *level1, lev, vcord, pm );
    }

    return;
}
