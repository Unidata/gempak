#include "gdgrib.h"

void gdgwmo ( const char *wmohdr, const int *ncntr, const char *cdd,
              const char *chhmm, unsigned char *chdr, int *iret )
/************************************************************************
 * gdgwmo								*
 *									*
 * This subrutine creates a 21-byte WMO header for a GRIB message.	*
 *									*
 * The user input for the WMO header is entered as:			*
 *									*
 *	            WMO_ID / Origin_ID / DDHHMM				*
 *									*
 * where WMO_ID is the first six bytes of the header, Origin_ID is the	*
 * originating center 4-letter identifier, and DDHHMM is the reference	*
 * two-digit day, hour, and minute.					*
 *									*
 * The first six bytes of the header must be provided by the user.	*
 *									*
 * gdgwmo ( wmohdr, ncntr, cdd, chhmm, chdr, iret )			*
 *									*
 * Input parameters:							*
 *	*wmohdr		const char	User input for the WMO header	*
 *	*ncntr		const int	Center ID from PDS byte 5	*
 *	*cdd		const char	2-digit day of month		*
 *	*chhmm		const char	4-digit hour minute		*
 *									*
 * Output parameters:							*
 *	*chdr		unsigned char	Output WMO header		*
 *	*iret		int		Return code			*
 *					 +6 = center # inconsistent	*
 *					  0 = normal return		*
 *					 -9 = 1st 6 chars required	*
 *					-10 = supplied hdr is too long	*
 **									*
 * Log:									*
 * K. Brill/HPC		08/99						*
 * K. Brill/HPC		 2/00	WMOHDR entered as 3 parts		*
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 ************************************************************************/
{
    char grps[3][9], *cdp[3], cdum[33];
    int ii, num, lnt, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    chdr[0] = '\0';
    memset ( cdum, 0, sizeof(cdum) );

    if ( strlen(wmohdr) == 0 ) return;
    for ( ii = 0; ii < 3; ii++ ) cdp[ii] = grps[ii];
    cst_clst ( (char *)wmohdr, '/', "", 3, 8, cdp, &num, &ier );
    lnt = strlen ( grps[0] );
    if ( lnt == 6 ) {
        memcpy ( chdr, grps[0], 6 );
	chdr[6] = ' ';
    } else if ( lnt > 6 ) {
	*iret = -10;
	return;
    } else {
	*iret = -9;
	return;
    }
    lnt = strlen ( grps[1] );
    if ( lnt == 4 ) {
        memcpy ( cdum, chdr, 7 );
	memcpy ( &cdum[7], grps[1], 4 );
	cdum[11] = ' ';
	if ( *ncntr == 7 && strncmp ( grps[1], "KWBC", 4 ) != 0 ) *iret = 6;
    } else {
	memcpy ( cdum, chdr, 7 );
	memcpy ( &cdum[7], "KWBC ", 5 );
	if ( *ncntr != 7 ) *iret = 6;
    }
    memcpy ( chdr, cdum, 12 );
    lnt = strlen ( grps[2] );
    if ( lnt == 6 ) {
        memcpy ( cdum, chdr, 12 );
	memcpy ( &cdum[12], grps[2], 6 );
    } else {
        memcpy ( cdum, chdr, 12 );
	memcpy ( &cdum[12], cdd, 2 );
	memcpy ( &cdum[14], chhmm, 4 );
    }
    memcpy ( chdr, cdum, 18 );
    chdr[18] = CHCR;
    chdr[19] = CHCR;
    chdr[20] = CHLF;
    chdr[21] = '\0';

    return;
}
