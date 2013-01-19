#include "gdgrib.h"

void pds_make ( const int *noptv, const int *idoc, const int *idngp,
                const int *idosc, const float *rnvblk, const char *gparm,
		const char *oparm, const char *gvcrd, const char *ovcrd,
		const int *level1, const int *level2, const char *olevl,
                const int *havbms, const int *ipsc10, const char *lasttm,
		const char *flgdtm1, const char *flgdtm2, const char *gbtbls,
                const int *igpds, int *nbyts, unsigned char *cpds,
		int *idhm, int *iret )
/************************************************************************
 * pds_make								*
 *									*
 * This subroutine combines user input, grid header information, and	*
 * information from other section builders to make the full GRIB PDS.	*
 *									*
 * If an overide parameter is blank, then the corresponding GEMPAK	*
 * parameter is used to look up the GRIB number in the appropriate	*
 * GRIB table.  OPARM and OVCRD must translate into a numeric value.	*
 * OLEVL must represent a single numeric value or two values separated	*
 * by a colon.								*
 *									*
 * pds_make ( noptv, idoc, idngp, idosc, rnvblk, gparm, oparm, gvcrd,	*
 *            ovcrd, level1, level2, olevl, havbms, ipsc10, lasttm,	*
 *            flgdtm1, flgdtm2, gbtbls, igpds, nbyts, cpds, idhm,	*
 *            iret )							*
 *									*
 * Input parameters:							*
 *	*noptv		const int	Number of parm table version	*
 *	*idoc		const int	Identification of Center	*
 *	*idngp		const int	ID number of generating process *
 *	*idosc		const int	ID number of sub center		*
 *	*rnvblk    	const float	Grid navigation block		*
 *	*gparm		const char	GEMPAK parameter name string	*
 *	*oparm		const char	User overide GRIB parameter #	*
 *	*gvcrd		const char	GEMPAK VCORD name string	*
 *	*ovcrd		const char	User overide GRIB ver cord #	*
 *	*level1		int		GEMPAK vert level values	*
 *	*level2		int		GEMPAK vert level values	*
 *	*olevl 		const char	User overide of level values	*
 *	*havbms		const int	Flag for existence of a BMS	*
 *	*ipsc10		const int	Power of 10 scale factor	*
 *	*lasttm		const char	Last grid time			*
 *	*flgdtm1	const char	Complete single GEMPAK time	*
 *	*flgdtm2	const char	Complete single GEMPAK time	*
 *	*gbtbls		const char	User input for GRIB table files *
 *	*igpds		const int	Grid navigation # from CPYFIL	*
 *									*
 * Input and output parameter:						*
 *	*nbyts		int		Input:  maximum # of bytes for	*
 *						the PDS			*
 *					Output:  actual # of bytes in	*
 *						 the PDS		*
 *									*
 * Output parameters:							*
 *	*cpds		unsigned char	GRIB PDS octets			*
 *	*idhm		int		Day, hour, minute for WMO hdr	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-92 = PDS allocation too small	*
 *					-93 = IPSC10 too large		*
 *					-95 = CPYFIL grid # mismatch	*
 *					-96 = CPYFIL # not valid for PDS*
 **									*
 * Log:									*
 * K. Brill/HPC		 8/99						*
 * K. Brill/HPC		 2/00	Added IGPDS; return if its > 255;	*
 *				Allow override names for OPARM & OVCRD  *
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 ************************************************************************/
{
    char tbls[4][LLMXLN], *cdp[4], cvrnm[5];
    int ibyt7, ibyt8, ibyt9, ibyt10, ibyt11, ibyt12, ivscal;
    int lvls[2], i13_25[13], ibyts[2], num, navsz, idt, ii, jj, ibase,
        nb, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    if ( *igpds > 255 || *igpds <= 0 ) {
	*iret = -96;
	return;
    }

    /*
     * Get the GRIB table file names from gbtbls.
     */
    cst_inch ( *noptv, cvrnm, &ier );
    for ( ii = 0; ii < 4; ii++ ) cdp[ii] = tbls[ii];
    cst_clst ( (char *)gbtbls, ';', "?", 4, LLMXLN, cdp, &num, iret );
    if ( tbls[0][0] == '?' ) {
        sprintf ( tbls[0], "%s%s%s", "wmogrib", cvrnm, ".tbl" );
    }
    if ( tbls[1][0] == '?' ) {
	sprintf ( tbls[1], "%s%s%s", "ncepgrib", cvrnm, ".tbl" );
    }
    if ( tbls[2][0] == '?' ) {
	strcpy ( tbls[2], "vcrdgrib1.tbl" );
    }
    if ( tbls[3][0] == '?' ) { 
	tbls[3][0] = '\0';
    }
		    
    /*
     * PDS length = 28.
     */
    if ( *nbyts < 28 ) {
	*iret = -92;
	return;
    } else {
	*nbyts = 28;
    }
    cpds[0] = (unsigned char) (0);
    cpds[1] = (unsigned char) (0);
    cpds[2] = (unsigned char) (28);

    /*
     * Set the fixed identifiers.
     */
    cpds[3] = (unsigned char) (*noptv);
    cpds[4] = (unsigned char) (*idoc);
    cpds[5] = (unsigned char) (*idngp);

    /*
     * Set the grid number.
     */
    navsz = 16;
    pds_byt7  ( rnvblk, &navsz, &cpds[6], &ibyt7, iret );
    if ( *iret != 0 ) {
	/*
	 * Write a warning.  GDS will be made anyway.
	 */
	er_wmsg ( "GDGRIB", iret, " ", &ier, 6, 1 );
	*iret = 0;
    }

    /*
     * Set the GDS (always present) and BMS flags.
     */
    ibyt8 = 128;
    if ( *havbms == G_TRUE ) ibyt8 += 64;
    cpds[7] = (unsigned char) (ibyt8);

    /*
     * Check for override parameter number.
     */
    idt = 0;
    if ( strlen(oparm) > 0 ) {
	cst_numb ( (char *)oparm, &ibyt9, &ier );
	if ( ier == 0 && ibyt9 > 0 && ibyt9 < 255 ) {
	    cpds[8] = (unsigned char) ( ibyt9 );
	} else {
	    pds_byt9 ( oparm, tbls[0], tbls[1], &cpds[8], &ibyt9, &idt, iret );
	    if ( *iret != 0 ) {
		printf (" User parm input is invalid--- trying GRIB table.\n");
	        pds_byt9 ( gparm, tbls[0], tbls[1], &cpds[8], &ibyt9,
		    &idt, iret );
	    }
	}
    } else {
	pds_byt9 ( gparm, tbls[0], tbls[1], &cpds[8], &ibyt9, &idt, iret );
    }
    if ( *iret != 0 ) return;

    /*
     * Check for override vertical coordinate number.
     */
    if ( strlen(ovcrd) > 0 ) {
	cst_numb ( (char *)ovcrd, &ibyt10, &ier );
	if ( ier == 0 && ibyt10 > 0 && ibyt10 < 255 ) {
	    cpds[9] = (unsigned char) ( ibyt10 );
	    ivscal = 0;
	} else {
	    pds_by10 ( ovcrd, tbls[2], "", &cpds[9], &ibyt10, &ivscal, iret );
	    if ( *iret != 0 ) {
		printf ( " User vertical coordinate input is invalid---"
                         " trying GRIB table.\n" );
		pds_by10 ( gvcrd, tbls[2], "", &cpds[9], &ibyt10, &ivscal,
		    iret );
	    }
	}
    } else {
	pds_by10 ( gvcrd, tbls[2], "", &cpds[9], &ibyt10, &ivscal, iret );
    }
    if ( *iret != 0 ) return;

    /*
     * Check for override for vertical coordinate values.
     * Fill bytes 11-12.  Adjust byte 10 for layers.
     */
    if ( strlen(olevl) > 0 ) {
	cst_ilst ( (char *)olevl, ':', -1, 2, lvls, &num, &ier );
	if ( ier != 0 ) {
	    lvls[0] = *level1;
	    lvls[1] = *level2;
	}
    } else {
	lvls[0] = *level1;
	lvls[1] = *level2;
    }
    pds_by11 ( &lvls[0], &lvls[1], &ivscal, &cpds[9], &ibyt10,
  	&cpds[10], &ibyt11, &cpds[11], &ibyt12, iret );
    if ( *iret != 0 ) return;

    /*
     * Process time information to fill bytes 13-25.
     */
    pds_vldt ( flgdtm1, flgdtm2, lasttm, &idt, &cpds[12], i13_25, iret );
    if ( *iret != 0 ) return;
    idhm[0] = i13_25[2];
    idhm[1] = i13_25[3];
    idhm[2] = i13_25[4];

    /*
     * Set the subcenter number.
     */
    cpds[25] = (unsigned char) ( *idosc );

    /*
     * Store the decimal scale factor.
     */
    nb = 2;
    ibase = 256;
    gdigit ( ipsc10, &ibase, &nb, ibyts, &ier );
    if ( ier != 0 ) {
	*iret = -93;
	return;
    }
    if ( *ipsc10 < 0 ) ibyts[1] += 128;
    jj = 26;
    for ( ii = 1; ii >= 0; ii-- ) {
	cpds[jj++] = (unsigned char) ( ibyts[ii] );
    }

    return;
}
