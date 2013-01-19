#include "gddiag.h"

#define DONAM	1
#define DOHAT	2
#define DOAAT	3
#define DOPCT	4

void gdgwrt ( const float *grid, const char *time1, const char *time2,
              const int *level1, const int *level2, const int *ivcord,
	      const char *parm, const char *grdnam, const char *gpack,
	      const int *ihzrmp, const int *idrct, int *iret )
/************************************************************************
 * gdgwrt								*
 *									*
 * This subrutine writes a warning message if a grid is already in	*
 * the file.  The grid identifier is written and the user is prompted	*
 * to accept the grid, change the parameter name or exit.		*
 *									*
 * gdgwrt ( grid, time1, time2, level1, level2, ivcord, parm, grdnam,	*
 *          gpack, ihzrmp, idrct, iret )				*
 *									*
 * Input parameters:							*
 *	*grid		const float	Grid of data			*
 *	*time1		const char	Grid date/time			*
 *	*time2		const char	Grid date/time			*
 *	*level1		const int	Grid levels			*
 *	*level2		const int	Grid levels			*
 *	*ivcord		const int	Grid vertical coordinate	*
 *	*parm		const char	Grid parameter name		*
 *	*grdnam		const char	User specified grid name	*
 *	*gpack		const char	Grid packing information	*
 *	*ihzrmp		const int	Horizontal remapping		*
 *	*idrct		const int	Directional flag		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  2 = user entered EXIT		*
 *					  0 = normal return		*
 *					 -7 = error writing grid	*
 **									*
 * Log:									*
 * M. Goodman/RDS	10/85						*
 * M. desJardins/GSFC	 7/87	Rewritten				*
 * M. desJardins/GSFC	 8/88	GEMPAK 4				*
 * S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
 * K. Brill/NMC		02/91   Added in-line parameters		*
 * K. Brill/NMC		07/92	Change gname to upper case (J. Whistler)*
 * K. Brill/NMC		07/92	Added % in-line parameter		*
 * K. Brill/EMC		11/97	Fixed in-line parm: i->ii in DO ii loop	*
 * M. Li/SAIC		04/04	Added ihzrmp, and idrct			*
 * R. Tian/SAIC		 1/05	Modified for time/file management	*
 * T. Lee/SAIC		12/05	Initialized ighdr			*
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 ************************************************************************/
{
    char gname[13], gtime[21], glevl[13], gcord[13], answer[13], vparm[13],
         timout1[21], timout2[21], gdnbuf[LLMXLN+1], *pchr;
    int respnd, exist, tltflg, pagflg, newln, rplc;
    int ighdr[LLGDHD], levout1, levout2, ivcout;
    int dowhat, inam, ihat, iaat, ipct, len, zero, ii, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    tltflg = G_TRUE;
    pagflg = G_FALSE;
    newln  = G_FALSE;
    rplc   = G_TRUE;
    zero = 0;
    inam = 0;
    ihat = 0;
    iaat = 0;
    ipct = 0;

    /*
     * Get name of grid to use along with in-line parameters.
     */
    if ( strlen(grdnam) > 0 ) {
        dowhat = DONAM;
        cst_lcuc ( (char *)grdnam, gdnbuf, &ier );
        for ( pchr = gdnbuf; *pchr != '\0'; pchr++ ) {
	    if ( *pchr == '^' ) dowhat = DOHAT;
	    if ( *pchr == '@' ) dowhat = DOAAT;
	    if ( *pchr == '%' ) dowhat = DOPCT;

	    switch ( dowhat ) {
	        /*
	         * Retrieve grid name.
	         */
	        case DONAM:
		    gname[inam++] = *pchr;
		break;

		/*
		 * Retrieve in-line time.
		 */
		case DOHAT:
		    if ( *pchr == '^' ) continue;
		    gtime[ihat++] = *pchr;
		break;

		/*
		 * Retrieve in-line level.
		 */
		case DOAAT:
		    if ( *pchr == '@' ) continue;
		    glevl[iaat++] = *pchr;
		break;

		/*
		 * Retrieve in-line vertical coordinate.
		 */
		case DOPCT:
		    if ( *pchr == '%' ) continue;
		    gcord[ipct++] = *pchr;
		break;
	    }
	}
	gname[inam] = '\0';
	gtime[ihat] = '\0';
	glevl[iaat] = '\0';
	gcord[ipct] = '\0';
    }
    if ( inam == 0 ) strcpy ( gname, parm );
    if ( ihat > 0 ) {
        strcpy ( timout1, gtime );
	timout2[0] = '\0';
    } else {
        strcpy ( timout1, time1 );
        strcpy ( timout2, time2 );

    }
    if ( iaat > 0 ) {
	cst_numb ( glevl, &levout1, &ier );
        levout2 = -1;
    } else {
        levout1 = *level1;
        levout2 = *level2;
    }
    if ( ipct > 0 ) {
        clv_cord ( gcord, vparm, &ivcout, &ier );
    } else {
        ivcout = *ivcord;
    }

    /*
     * Check if the grid already exists in the grid file.
     * Write message that grid will be replaced.
     */
    cst_lcuc ( gname, gname, &ier );
    dgc_qgrd  ( timout1, timout2, &levout1, &levout2, &ivcout, gname,
        &exist, &ier );
    if ( ier == 0 && exist == G_TRUE ) {
	printf ( "  This grid is already in file.  It will be replaced.\n" );
    }

    /*
     * Write the grid identifiers.
     */
    printf ( "\n" );
    grc_wtrm  ( stdout, &tltflg, &zero, timout1, timout2, &levout1,
        &levout2, &ivcout, gname, &ier );

    /*
     * Allow user to enter a new parameter name.
     */
    ip_resp ( &respnd, &ier );
    if ( respnd == G_TRUE ) {
	tm_str ( "Enter a new grid parameter name, <cr> to accept",
	    &pagflg, &newln, answer, iret,
	    strlen("Enter a new grid parameter name, <cr> to accept"), 13 );
	st_null ( answer, answer, &len, &ier, 13, 13 );

	/*
	 * Return if the user typed EXIT.
	 */
	if ( *iret == 2 ) return;

	/* 
	 * Check if user entered new name.
	 */
	if ( *iret == 0 ) strcpy ( gname, answer );
    }

    /*
     * Write out the grid and check for errors.
     */
    for ( ii = 0; ii < LLGDHD; ii++ ) ighdr[ii] = 0;
    if ( *ihzrmp != IMISSD ) ighdr[0] = *ihzrmp;
    if ( *idrct  != IMISSD ) ighdr[1] = *idrct;

    dgc_nwdt ( grid, timout1, timout2, &levout1, &levout2, &ivcout, gname,
        ighdr, gpack, &rplc, iret );
    if ( *iret != 0 ) {
	er_wmsg ( "DG", iret, " ", &ier, strlen("DG"), strlen(" ") );
	*iret = -7;
    }

    return;
}
