#include "geminc.h"
#include "gemprm.h"

void ctb_dhrsitos ( int *ionoff, int *hrsbfr, int *mnsbfr, int *hraftr,
		    int *mnaftr, int *mstrct, char *binhrs, int *iret )
/************************************************************************
 * ctb_dhrsitos								*
 *									*
 * This function converts the "BIN HOURS" into a character string.	*
 *									*
 * ctb_dhrsitos ( ionoff, hrsbfr, mnsbfr, hraftr, mnaftr, mstrct,	*
 *		  binhrs, iret )					*
 *									*
 * Input parameters:							*
 *	*ionoff		int		ON/OFF flag for time binning	*
 *	*hrsbfr		int		hours before current time	*
 *	*mnsbfr		int		minutes before current time	*
 *	*hraftr		int		hours after current time	*
 *	*mnaftr		int		minutes after current time	*
 *	*mstrct		int		ON/OFF flag for most recent only*
 *									*
 * Output parameters:							*
 *	*binhrs		char		bin hours, minutes, and flags	*
 *	*iret		int		Return code			*
 *				  	0 - Normal			*
 **									*
 * Log:									*
 * T. Lee/SAIC		 9/04	initial coding				*
 * m.gamazaychikov/SAIC	12/04	added ionoff				*
 * A. Hardy/NCEp	02/05	changed hrsbfr to '< 0'			*
 * F. J. Yen/NCEP	04/08	Added bin minutes and most recent flag	*
 ***********************************************************************/
{
    char	tmp[4], hronoff[4], cmstrct[4];
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;
    binhrs [0] = '\0';

    /* 
     *  Check input.
     */

    if  ( *ionoff ==1 ) {
	strcpy ( hronoff, "ON" );
    }
    else {
	strcpy ( hronoff, "OFF" );
    }

    if  ( *hrsbfr < 0 ) {
	*hrsbfr = 0;
    }

    if  ( *mnsbfr < 0 ) {
	*mnsbfr = 0;
    }

    if  ( *hraftr < 0 ) {
	*hraftr = 0;
    }

    if  ( *mnaftr < 0 ) {
	*mnaftr = 0;
    }

    if  ( *mstrct ==1 ) {
	strcpy ( cmstrct, "ON" );
    }
    else {
	strcpy ( cmstrct, "OFF" );
    }

    /* 
     *  Convert integer to character and create a data string.
     */
    strcpy  ( binhrs, hronoff);
    strcat  ( binhrs, "/" );
    sprintf ( tmp, "%d", *hrsbfr );
    strcat  ( binhrs, tmp );
    strcat  ( binhrs, ":" );
    sprintf ( tmp, "%02d", *mnsbfr );
    strcat  ( binhrs, tmp );
    strcat  ( binhrs, "/" );
    sprintf ( tmp, "%d", *hraftr );
    strcat  ( binhrs, tmp );
    strcat  ( binhrs, ":" );
    sprintf ( tmp, "%02d", *mnaftr );
    strcat  ( binhrs, tmp );
    strcat  ( binhrs, "/" );
    strcat  ( binhrs, cmstrct );

}

