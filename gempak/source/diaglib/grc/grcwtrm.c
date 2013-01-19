#include "geminc.h"
#include "gemprm.h"

void grc_wtrm ( FILE *fp, const int *title, const int *ignum,
                const char *gdattm1, const char *gdattm2,
		const int *level1, const int *level2, const int *ivcord,
                const char *parm, int *iret )
/************************************************************************
 * grc_wtrm								*
 *									*
 * This subroutine writes a grid identifier to the specified logical	*
 * unit using a standard format.  TITLE is set to indicate that the 	*
 * title line:								*
 *									*
 *    NUM    TIME1     TIME2     LEVEL1    LEVEL2   VCORD     PARM	*
 *									*
 * is to be written first.  If IGNUM is not positive, the grid 		*
 * number will not be written and will not be included in the title.	*
 *									*
 * grc_wtrm ( fp, title, ignum, gdattm1, gdattm2, level1, level2,	*
 *            ivcord, parm, iret )					*
 *									*
 * Input parameters:							*
 *	*fp		FILE		File pointer for write		*
 *	*title		const int	Flag to write title		*
 *	*ignum		const int	Grid number			*
 *	*gdattm1	const char	GEMPAK time			*
 *	*gdattm2	const char	GEMPAK time			*
 *	*level1		const int	Vertical levels			*
 *	*level2		const int	Vertical levels			*
 *	*ivcord		const int	Vertical coordinate		*
 *	*parm		const char	Parameter name			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 7/87						*
 * M. desJardins/GSFC	 6/88	GEMPAK4					*
 * M. desJardins/GSFC	 4/89	Added call to LV_CCRD			*
 * K. Brill/NMC		01/92	Add different WRITE format for VAX	*
 * M. desJardins/NMC	 3/92	Make line 79 characters to stop wrap	*
 * M. Linda/GSC		 9/97	Corrected right border of prologue	*
 * R. Tian/SAIC		 9/6	Recoded from Fortran			*
 ************************************************************************/
{
    char vcord[5], dt1[19], dt2[17], p[13], lev[6];
    int ier;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Translate vertical coordinate.
     */
    clv_ccrd ( *ivcord, vcord, &ier );

    /*
     * Move character strings into variables with correct length.
     */
    strncpy ( dt1, gdattm1, 18 );
    dt1[18] = '\0';
    strncpy ( dt2, gdattm2, 16 );
    dt2[16] = '\0';
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
     * Write title if requested, checking whether grid number is to
     * be written.
     */
    if ( *title == G_TRUE && *ignum > 0 ) {
	fprintf ( fp, "  NUM       TIME1             TIME2"
	              "           LEVL1 LEVL2  VCORD PARM\n" );
    } else if ( *title == G_TRUE ) {
	fprintf ( fp, "    TIME1             TIME2         "
	              "LEVL1 LEVL2   VCORD PARM\n" );
    }

    /*
     * Write the grid identifier.
     */
    if ( *ignum > 0 ) {
	fprintf ( fp, "%5d     %18.18s%16.16s  %6d  %-5.5s  %4.4s %-12.12s\n",
	          *ignum, dt1, dt2, *level1, lev, vcord, parm );
    } else {
	fprintf ( fp, "%-18.18s %16.16s%6d  %-5.5s   %4.4s %-12.12s\n",
	          dt1, dt2, *level1, lev, vcord, parm );
    }

    return;
}
