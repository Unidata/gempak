#include "gdgrib.h"

void pds_vldt ( const char *flgdtm1, const char *flgdtm2,
                const char *lasttm, int *idt, unsigned char *b13_25,
		int *i13_25, int *iret )
/************************************************************************
 * pds_vldt								*
 *									*
 * This subroutine uses the full GEMPAK time (YYYYMMDD/HHHHFhhhmm) for	*
 * grids to set PDS bytes 13 through 25.				*
 *									*
 * Note that the input date must have a 4-digit year.			*
 *									*
 * pds_vldt  ( flgdtm, lasttm, idt, b13_25, i13_25, iret )		*
 *									*
 * Input parameters:							*
 *	*flgdtm1	const char	GEMPAK grid times		*
 *	*flgdtm2	const char	GEMPAK grid times		*
 *	*lasttm		const char	Last grid time			*
 *	*idt		int		Time interval from parm (hours) *
 *									*
 * Output parameters:							*
 *	*b13_25		unsigned char	PDS byte values 13--25		*
 *	*i13_25		int		Integer value of bytes 13--25	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-89 = cannot handle dual times	*
 *					-90 = 4-digit year required	*
 *					-91 = forecast must be in hours *
 *					-97 = fhr not dvsble by 3, 6, 12*
 *					-98 = idt not dvsble by 3, 6, 12*
 **									*
 * Log:									*
 * K. Brill/HPC		 7/99						*
 * K. Brill/HPC		01/06	Allow time units 3, 6, 12 (PDS byte 18) *
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 ************************************************************************/
{
    char ctype, chm[7], hlddtm[21];
    int intdtm[3], icy, iyr, imh, idy, ihr, imn, mhr, i18, ifh, jfh, ier,
        ii;
/*----------------------------------------------------------------------*/
    *iret = 0;
    for ( ii = 0; ii < 13; ii++ ) {
	i13_25[ii] = 255;
	b13_25[ii] = (unsigned char)(255);
    }

    if ( strlen ( flgdtm2 ) > 0 ) {
	*iret = -89;
	return;
    }
    if ( strchr ( flgdtm1, '/' ) != &flgdtm1[8] ) {
	*iret = -90;
	return;
    }

    /*
     * Compared GFULTM with TG_FULL, and the only difference is that GFULTM
     * deals with 4-digit year. Make the caller get rid of the first two
     * digits of the year and call TG_FULL instead.
     */
    ctg_full ( &flgdtm1[2], lasttm, lasttm, hlddtm, &ier );
    tg_ctoi ( hlddtm, intdtm, &ier, strlen(hlddtm) );
    ctg_cftm ( intdtm[2], &ctype, chm, &ier );
    if ( strlen ( chm ) > 3 ) {
	*iret = -91;
	return;
    }

    /*
     * Split the GEMPAK time into integers.
     */
    strncpy ( hlddtm, flgdtm1, 2 );
    hlddtm[2] = '\0';
    cst_numb ( hlddtm, &icy, &ier );
    iyr = intdtm[0] / 10000;
    if ( iyr == 0 ) {
	iyr = 100;
    } else {
	icy += 1;
    }
    imh = ( intdtm[0] / 100 ) % 100;
    idy = intdtm[0] % 100;
    ihr = intdtm[1] / 100;
    imn = intdtm[1] % 100;
    cst_numb ( chm, &ifh, &ier );
    mhr = 0;
    i18 = 9;
    jfh = ifh;
    while ( jfh > 255 && i18 < 12 ) {
	i18 += 1;
	mhr += 3;
	if ( ifh % mhr == 0 ) {
	    jfh = ifh / mhr;
	}
    }
    if ( jfh > 255 ) {
	*iret = -97;
	return;
    }
    ifh = jfh;
    if ( mhr != 0 ) {
	if ( *idt % mhr != 0 ) {
	    *iret = -98;
	    return;
	}
	*idt /= mhr;
    } else {
	i18 = 1;
    }

    /*
     * Start assigning output integers.
     */
    i13_25[0] = iyr;
    i13_25[1] = imh;
    i13_25[2] = idy;
    i13_25[3] = ihr;
    i13_25[4] = imn;
    i13_25[5] = i18;
    if ( *idt <= 0 ) {
	i13_25[6] = ifh;
	i13_25[7] = 0;
	i13_25[8] = 0;
    } else {
	i13_25[6] = ifh - (*idt);
	i13_25[7] = ifh;
	i13_25[8] = 4;
    }
    i13_25[9] = 0;
    i13_25[10] = 0;
    i13_25[11] = 0;
    i13_25[12] = icy;
    for ( ii = 0; ii < 13; ii++ ) {
	b13_25[ii] = (unsigned char)( i13_25[ii] );
    }

    return;
}
