#include "gdgrib.h"

void pds_byt9 ( const char *parm, const char *wmotb, const char *nceptb,
                unsigned char *byte9, int *ibyt9, int *idt, int *iret )
/************************************************************************
 * pds_byt9								*
 *									*
 * This subroutine uses the GEMPAK GRIB parameter lookup tables to	*
 * determine the value of PDS octet 9.					*
 *									*
 * pds_byt9 ( parm, wmotb, nceptb, byte9, ibyt9, idt, iret )		*
 *									*
 * Input parameters:							*
 *	*parm		const char	GEMPAK parameter name string	*
 *	*wmotb		const char	WMO GRIB parm LUT file name	*
 *	*nceptb		const char	NCEP GRIB parm LUT file name	*
 *									*
 * Output parameters:							*
 *	*byte9		unsigned char	Byte with GRIB parm # stored	*
 *	*ibyt9		int		Integer value of byte 9		*
 *	*idt		int		Value of any imbedded integer	*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-83 = parm not found		*
 *					-84 = parm # not valid in GRIB	*
 *					-94 = parm name is too long	*
 **									*
 * Log:									*
 * K. Brill/HPC		 8/99						*
 * K. Brill/HPC		 3/00	Avoid character assignment to itself	*
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 ************************************************************************/
{
     char filnam[2][LLMXLN], record[LLMXLN], prmnam[17], chkprm[17],
          cinprm[17], cnum[17], cdum;
     int found;
     int ifile, iprm, nnums, kchr, ityp, ic, ier;
     FILE *fp;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *idt = -9999;
    *byte9 = (unsigned char)( 255 );
    *ibyt9 = 255;

    if ( strlen(parm) > 16 ) {
	*iret = -94;
	return;
    }
    cst_lcuc ( (char *)parm, cinprm, &ier );
    strcpy ( filnam[0], wmotb );
    strcpy ( filnam[1], nceptb );
    found = G_FALSE;
    ifile = 0;
    while ( ifile < 2 && found == G_FALSE ) {
	fp = cfl_tbop ( filnam[ifile++], "grid", &ier );
	if ( ier != 0 ) continue;

	while ( !feof(fp) && found == G_FALSE ) {
	    cfl_trln ( fp, sizeof(record), record, &ier );
	    if ( ier != 0 ) continue;

	    sscanf ( record, "%d", &iprm );
	    sscanf ( &record[59], "%s", prmnam );

	    memset ( cnum, 0, sizeof(cnum) );
	    strcpy ( chkprm, cinprm );
	    if ( strcmp ( prmnam, chkprm ) == 0 ) {
	        found = G_TRUE;
		*ibyt9 = iprm;
	    } else if ( strstr ( chkprm, prmnam ) == chkprm ) {
	        strcpy ( cnum, &chkprm[strlen(prmnam)] );
		cst_numb ( cnum, idt, &ier );
		if ( ier == 0 ) {
		    found = G_TRUE;
		    *ibyt9 = iprm;
		}
	    } else {
	        if ( strchr ( prmnam, '-' ) ) {
		    nnums = 0;
		    kchr = 0;
		    for ( ic = 0; ic < (int)strlen(chkprm); ic++ ) {
		        cst_alnm ( chkprm[ic], &ityp, &ier );
			if ( ityp == 2 ) {
			    cnum[nnums++] = chkprm[ic]; 
			    if ( nnums < 3 ) {
			        chkprm[kchr++] = '-';
			    }
			} else {
			    cdum = chkprm[ic];
			    chkprm[kchr++] = cdum;
			}
		    }
		    if ( nnums > 0 ) {
		        if ( strncmp ( chkprm, prmnam, strlen(prmnam) ) == 0 ) {
			    found = G_TRUE;
			    cst_numb ( cnum, idt, &ier );
			    *ibyt9 = iprm;
			}
		    }
		}
	    }
	}
	cfl_clos ( fp, &ier );
    }

    if ( found == G_FALSE ) {
	*iret = -83;
    } else if ( *ibyt9 < 255 && *ibyt9 > 0 ) {
	*byte9 = (unsigned char)( *ibyt9 );
    } else {
	*iret = -84;
    }

    return;
}
