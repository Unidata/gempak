#include "gdgrib.h"

#define DONAM   1
#define DOHAT   2
#define DOAAT   3
#define DOPCT   4

void gdgpds ( const char *pdsval, const char *vercen, const float *rnvblk,
              const char *gparm, const int *ivcord, const int *level1,
	      const int *level2, const int *havbms, const int *ip10sc,
	      const char *lasttm, const char *gdttm1, const char *gdttm2,
	      const char *gbtbls, const int *igpds, int *nbpds,
              unsigned char *cpds, char *cdd, char *chhmm, int *iret )
/************************************************************************
 * gdgpds								*
 *									*
 * This subroutine bridges the gap between the user input and PDS_MAKE.	*
 * The PDS byte array is returned.					*
 * 									*
 * gdgpds  ( pdsval, vercen, rnvblk, gparm, ivcord, level1, level2,	*
 *           havbms, ip10sc, lasttm, gdttm1, gdttm2, gbtbls, igpds,	*
 *           nbpds, cpds, cdd, chhmm, iret )				*
 *									*
 * Input parameters:							*
 *	*pdsval		const char	User input override PDS numbers	*
 *	*vercen		const char	User input for octets 4,5,6,26  *
 *	*rnvblk		const float	Grid navigation block		*
 *	*gparm		const char	Grid parameter from DG_GRID	*
 *	*ivcord		connst int	Grid vert coord # from DG_GRID	*
 *	*level1		const int	Grid level from DG_GRID		*
 *	*level2		const int	Grid level from DG_GRID		*
 *	*havbms		const int	Flag for existence of BMS	*
 *	*ip10sc		const int	Power of 10 scale factor	*
 *	*lasttm		const char	Last grid time			*
 *	*gdttm1		const char	Grid time from DG_GRID		*
 *	*gdttm2		const char	Grid time from DG_GRID		*
 *	*gbtbls		const char	User input for GRIB tables	*
 *	*igpds		const int	Grid navigation # from CPYFIL	*
 *									*
 * Input and output parameter:						*
 *	*nbpds		int		Input:  Max length for PDS	*
 *					Output: Actual length of PDS	*
 * Output parameters:							*
 *	*cpds		unsigned char	PDS array			*
 *	*cdd		char		2-digit day of month		*
 *	*chhmm		char		Hour minute of data		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * K. Brill/HPC		08/99						*
 * K. Brill/HPC		 2/00	Add IGPDS				*
 * K. Brill/HPC		 3/00	Avoid character assignment to itself	*
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 ************************************************************************/
{
    char *pchr, gvcord[5], gdnbuf[LLMXLN], flgdtm1[21], flgdtm2[21],
         otime[21], oparm[13], ovcrd[13], olevl[17], c2d[3], cdum[3], cyr[3];
    int dowhat, inam, ihat, iaat, ipct, iyr, num, noptv, idoc, idngp,
        idosc, ipos[4], ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    inam = 0;
    ihat = 0;
    iaat = 0;
    ipct = 0;
    oparm[0] = '\0';
    otime[0] = '\0';
    olevl[0] = '\0';
    ovcrd[0] = '\0';

    /*
     * Decode the override grid identifier parameters:
     */
    if ( strlen(pdsval) > 0 ) {
        dowhat = DONAM;
	cst_lcuc ( (char *)pdsval, gdnbuf, &ier );
        for ( pchr = gdnbuf; *pchr != '\0'; pchr++ ) {
            if ( *pchr == '^' ) dowhat = DOHAT;
            if ( *pchr == '@' ) dowhat = DOAAT;
            if ( *pchr == '%' ) dowhat = DOPCT;

            switch ( dowhat ) {
                /*
                 * Retrieve grid name.
                 */
                case DONAM:
                    oparm[inam++] = *pchr;
                break;

                /*
                 * Retrieve in-line time.
                 */
                case DOHAT:
                    if ( *pchr == '^' ) continue;
                    otime[ihat++] = *pchr;
                break;

                /*
                 * Retrieve in-line level.
                 */
                case DOAAT:
                    if ( *pchr == '@' ) continue;
                    olevl[iaat++] = *pchr;
                break;

                /*
                 * Retrieve in-line vertical coordinate.
                 */
                case DOPCT:
                    if ( *pchr == '%' ) continue;
                    ovcrd[ipct++] = *pchr;
                break;
            }
        }
        oparm[inam] = '\0';
        otime[ihat] = '\0';
        olevl[iaat] = '\0';
        ovcrd[ipct] = '\0';
    }

    /*
     * Check for override on the date/time stamp.
     */
    if ( ihat > 0 ) {
        strcpy ( flgdtm1, otime );
	flgdtm2[0] = '\0';
    } else {
	/*
	 * Add century to YYMMDD.
	 */
	if ( strchr ( gdttm1, '/' ) - gdttm1 < 8 ) {
	    strncpy ( cyr, gdttm1, 2 );
	    cyr[2] = '\0';
	    cst_numb ( cyr, &iyr, &ier );
	    if ( iyr < 70 ) {
	        strcpy ( flgdtm1, "20" );
		strcat ( flgdtm1, gdttm1 );
	    } else {
	        strcpy ( flgdtm1, "19" );
		strcat ( flgdtm1, gdttm1 );
	    }
	} else {
            strcpy ( flgdtm1, gdttm1 );
	}
	flgdtm2[0] = '\0';
    }

    /*
     * Next, set the OCTET 4, 5, 6, and 26 values.
     */
    cst_ilst ( (char *)vercen, '/', 0, 4, ipos, &num, &ier );
    if ( ipos[0] != 0 ) {
	noptv = ipos[0];
    } else {
	noptv = 2;
    }
    if ( ipos[1] != 0 ) {
	idoc = ipos[1];
    } else {
	idoc = 7;
    }
    if  ( ipos[2] != 0 ) {
	idngp = ipos[2];
    } else {
	idngp = 0;
    }
    if ( ipos[3] != 0 ) {
	idosc = ipos[3];
    } else {
	idosc = 5;
    }

    /*
     * Translate vertical coordinate number as character string.
     */
    if ( *ivcord == 0 ) {
	strcpy ( gvcord, "NONE" );
    } else if ( *ivcord == 1 ) {
	strcpy ( gvcord, "PRES" );
    } else if ( *ivcord == 2 ) {
	strcpy ( gvcord, "THTA" );
    } else if ( *ivcord == 3 ) {
	strcpy ( gvcord, "HGHT" );
    } else if ( *ivcord == 4 ) {
	strcpy ( gvcord, "SGMA" );
    } else if ( *ivcord == 5 ) {
	strcpy ( gvcord, "DPTH" );
    } else {
	strcpy ( gvcord, "NULL" );
    }

    /*
     * Build the PDS now.
     */
    pds_make( &noptv, &idoc, &idngp, &idosc, rnvblk, gparm, oparm, gvcord,
        ovcrd, level1, level2, olevl, havbms, ip10sc, lasttm, flgdtm1, flgdtm2,
	gbtbls, igpds, nbpds, cpds, ipos, iret );

    /*
     * Convert day, hour, and minute in IPOS to characters.
     */
    cst_inch ( ipos[0], c2d, &ier );
    if ( ipos[0] < 10 ) {
        cdum[0] = '0';
        cdum[1] = c2d[0];
        cdum[2] = '\0';
    } else {
	strcpy ( cdum, c2d );
    }
    strcpy ( cdd, cdum );
    cst_inch ( ipos[1], c2d, &ier );
    if ( ipos[1] < 10 ) {
        cdum[0] = '0';
        cdum[1] = c2d[0];
        cdum[2] = '\0';
    } else {
	strcpy ( cdum, c2d );
    }
    strncpy ( chhmm, cdum, 2 );
    cst_inch ( ipos[2], c2d, &ier );
    if ( ipos[2] < 10 ) {
        cdum[0] = '0';
        cdum[1] = c2d[0];
        cdum[2] = '\0';
    } else {
	strcpy ( cdum, c2d );
    }
    strncpy ( &chhmm[2], cdum, 2 );
    chhmm[4] = '\0';

    return;
}
