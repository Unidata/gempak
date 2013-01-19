#include "geminc.h"
#include "gemprm.h"

void ctb_dhrsstoi ( char *binhrs, int *ionoff, int *hrsbfr, int *mnsbfr,
		    int *hraftr, int *mnaftr, int *mstrct, int *iret )
/************************************************************************
 * ctb_dhrsstoi								*
 *									*
 * This function converts a character string of "BIN HOURS" to integers.*
 *									*
 * ctb_dhrsstoi ( binhrs, ionoff, hrsbfr, mnsbfr, hraftr, mnaftr,	*
 *		  mstrct, iret )					*
 *									*
 * Input parameters:							*
 *	*binhrs		char		bin hours			*
 *									*
 * Output parameters:							*
 *	*ionoff		int		time binning on/off flag	*
 *	*hrsbfr		int		hours before current time	*
 *	*mnsbfr		int		minutes before current time	*
 *	*hraftr		int		hours after current time	*
 *	*mnaftr		int		minutes after current time	*
 *	*mstrct		int		most recent only flag		*
 *	*iret		int		Return code			*
 *				  	0 - Normal			*
 **									*
 * Log:									*
 * T. Lee/SAIC		 9/04	initial coding				*
 * m.gamazaychikov/SAIC	12/04	added ionoff flag handling		*
 * A. Hardy/NCEp	02/05	changed hrsbfr to '< 0'			*
 * F. J. Yen/NCEP	04/08	added mnsbfr, mnaftr, and mstrct (CSC)	*
 ***********************************************************************/
{
    char	  *cp, hronoff[4], mronoff[4]; 
    int           nocc, ier, itype, num, itime[2];
/*---------------------------------------------------------------------*/
    
    *iret = G_NORMAL;

    /*
     * Check bin hours validity
     */

    if ( strcmp (binhrs, "NONE") == 0 || binhrs[0] == '\0') {
       *ionoff = *hrsbfr = *hraftr = *mnsbfr = *mnaftr = *mstrct = 0;
       return;
    }
    cst_alnm (binhrs[0], &itype, &ier); 
    if ( itype == 1 ) {
        /* 
         *  Convert the character string to integer.
         */
        cst_nocc ( binhrs, '/', 1, 0, &nocc, &ier );
        cst_ncpy ( hronoff, binhrs, nocc, &ier );
        cp = strtok ( binhrs+nocc, "/" );
    }
    else {
        cp = strtok ( binhrs, "/" );
        strcpy ( hronoff, "OFF" );
    }

    cst_ilst ( cp, ':', 0, 2, itime, &num, &ier );
    *hrsbfr = itime [0];
    *mnsbfr = itime [1];
    cp = strtok ( NULL, "/" );
    cst_ilst ( cp, ':', 0, 2, itime, &num, &ier );
    *hraftr = itime [0];
    *mnaftr = itime [1];

    cp = strtok ( NULL, "/" );
    if ( cp == NULL ) {
	strcpy ( mronoff, "OFF" );
    }
    else {
        cst_alnm (cp[0], &itype, &ier); 
        if ( itype != 1 ) {
	    strcpy ( mronoff, "OFF" );
	}
	else {
	    strcpy ( mronoff, cp );
	}
    }

    /* 
     *  Check data validity.
     */

    cst_lcuc ( hronoff, hronoff, &ier );
    if ( strcmp ( hronoff, "ON" ) == 0 ) {
        *ionoff = 1;
    }
    else {
        *ionoff = 0;
    }

    if  ( *hrsbfr < 0 ) {
	*hrsbfr = abs (*hrsbfr);
    }

    if  ( *mnsbfr < 0 ) {
	*mnsbfr = 0;
    }
    else if ( *mnsbfr > 59 ) {
	*mnsbfr = 59;
    }

    if  ( *hraftr < 0 ) {
	*hraftr = 0;
    }

    if  ( *mnaftr < 0 ) {
	*mnaftr = 0;
    }
    else if ( *mnaftr > 59 ) {
	*mnaftr = 59;
    }

    /* 
     *  Check data validity.
     */
    cst_lcuc ( mronoff, mronoff, &ier );
    if ( strcmp ( mronoff, "ON" ) == 0 ) {
        *mstrct = 1;
    }
    else {
        *mstrct = 0;
    }
}
