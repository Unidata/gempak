#include "gdgrib.h"

void pds_by10 ( const char *cvcrd, const char *wmotb, const char *nceptb,
                unsigned char *byte10, int *ibyt10, int *iscale, int *iret )
/************************************************************************
 * pds_by10								*
 *									*
 * This subroutine uses the GEMPAK GRIB vertical coordinate lookup	*
 * tables to determine the value of PDS octet 10.			*
 *									*
 * pds_by10 ( cvcrd, wmotb, nceptb, byte10, ibyt10, iscale, iret )	*
 *									*
 * Input parameters:							*
 *	*cvcrd		const char	GEMPAK VCORD name string	*
 *	*wmotb		const char	WMO GRIB VCORD LUT file name	*
 *	*nceptb		const char	NCEP GRIB VCORD LUT file name	*
 *									*
 * Output parameters:							*
 *	*byte10		unsigned char	Byte with GRIB VCORD # stored	*
 *	*ibyt10		int		Integer value of byte 10	*
 *	*iscale		int		Power of 10 scaling in GEMPAK	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-85 = VCORD not found		*
 *					-86 = VCORD # is invalid	*
 **									*
 * Log:									*
 * K. Brill/HPC		 7/99						*
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 ************************************************************************/
{
    char filnam[2][LLMXLN], record[LLMXLN], prmnam[17], chkprm[17];
    int found;
    int ifile, iprm, iscl, ier;
    FILE *fp;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *byte10 = (unsigned char)( 255 );
    *ibyt10 = 255;
    *iscale = 0;

    /*
     * Check the tables.
     */
    cst_lcuc ( (char *)cvcrd, chkprm, &ier );
    strcpy ( filnam[0], wmotb );
    strcpy ( filnam[1], nceptb );
    found = G_FALSE;
    ifile = 0;
    while ( ifile < 2 && found == G_FALSE ) {
        fp = cfl_tbop ( filnam[ifile++], "grid", &ier );
	if ( ier != 0 ) continue;

	while ( ! feof(fp) && found == G_FALSE ) {
	    cfl_trln ( fp, sizeof(record), record, &ier );
	    if ( ier != 0 ) continue;

	    sscanf ( record, "%d", &iprm );
	    sscanf ( &record[59], "%s %d", prmnam, &iscl );

	    if ( strcmp ( prmnam, chkprm ) == 0 ) {
	        found = G_TRUE;
		*ibyt10 = iprm;
		*iscale = iscl;
	    }
	}
	cfl_clos ( fp, &ier );
    }
    if ( found == G_FALSE ) {
	*iret = -85;
    } else if ( *ibyt10 < 255 && *ibyt10 > 0 ) {
	*byte10 = (unsigned char)( *ibyt10 );
    } else {
	*iret = -86;
    }

    return;
}
