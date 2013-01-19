#include "gdgrib.h"

void pds_byt7 ( const float *rnvblk, const int *nnv, unsigned char *byte7,
                int *ibyt7, int *iret )
/************************************************************************
 * PDS_BYT7								*
 *									*
 * This subroutine uses the GEMPAK grid navigation table file to find	*
 * the grid number that matches the navigation in rnvblk.  If none is	*
 * found, byte7=255.							*
 *									*
 * PDS_BYT7  ( RNVBLK, NNV, BYTE7, IBYT7, IRET )			*
 *									*
 * Input parameters:							*
 *	RNVBLK (NNV)	REAL		GEMPAK grid navigation block	*
 *	NNV		INTEGER		Length of navigation block	*
 *									*
 * Output parameters:							*
 *	BYTE7		CHAR*1		Byte with grid # value stored	*
 *	IBYT7		INTEGER		Integer value of byte 7		*
 *	IRET		INTEGER		Return code			*
 *					  0 = normal return		*
 *					 +1 = cannot open grdnav.tbl	*
 *					 +2 = error reading grdnav.tbl	*
 *					 +3 = error decoding grdnav.tbl *
 *					 +4 = grid not in grdnav.tbl	*
 **									*
 * Log:									*
 * K. Brill/HPC		 7/99						*
 * K. Brill/HPC		11/99	Return +1-4 warning messages		*
 * R. Tian/SAIC		10/06	Recoded from Fortran			*
 ************************************************************************/
{
    char buffer[LLMXLN], namgd[5], prjgd[4];
    float chknav[LLNNAV], ang1, ang2, ang3, rlat1, rlon1, rlat2, rlon2,
          deln;
    int found, angflg;
    int navsz, numgd, nxgd, nygd, junk, ier;
    FILE *fp;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *byte7 = (unsigned char)( 255 );
    *ibyt7 = 255;

    fp = cfl_tbop ( "grdnav.tbl", "grid", &ier );
    if ( ier != 0 ) {
	*iret = +1;
	return;
    }
    if ( G_DIFF ( rnvblk[0], 2.0 ) ) {
        angflg = G_TRUE;
    } else {
        angflg = G_FALSE;
    }

    found = G_FALSE;
    navsz = LLNNAV;
    while ( ! feof(fp) && found == G_FALSE ) {
        cfl_trln ( fp, sizeof(buffer), buffer, &ier );
	if ( ier != 0 ) continue;

	sscanf ( buffer, "%s %d %s %f %f %f %f %f %f %f %d %d %f %d",
	    namgd, &numgd, prjgd, &ang1, &ang2, &ang3,
	    &rlat1, &rlon1, &rlat2, &rlon2, &nxgd, &nygd, &deln, &junk );
	grc_mnav ( prjgd, &nxgd, &nygd, &rlat1, &rlon1, &rlat2, &rlon2,
	    &ang1, &ang2, &ang3, &angflg, chknav, &ier );
	grc_cnav ( rnvblk, chknav, &navsz, &found, &ier );
    }

    if ( found == G_TRUE ) {
	if ( numgd < 256 && numgd > 0 ) {
	    *byte7 = (unsigned char)( numgd );
	    *ibyt7 = numgd;
	}
    } else {
	*iret = +4;
    }

    cfl_clos ( fp, &ier );

    return;
}
