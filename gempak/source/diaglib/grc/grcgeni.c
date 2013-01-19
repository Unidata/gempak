#include "geminc.h"
#include "gemprm.h"

void grc_geni ( const char *gdfile, FILE **fps, const int *nfps,
                int *iret )
/************************************************************************
 * grc_geni								*
 *									*
 * This subroutine prints general grid information given a grid		*
 * filename.  The grid file is opened and closed within this function.	*
 *									*
 * grc_geni ( gdfile, fps, nfps, iret )					*
 *									*
 * Input parameters:							*
 *	*gdfile		const char	Grid file name			*
 *	**fps		FILE		File pointer for write		*
 *	*nfps		const int	Number of files for write	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * R. Tian/SAIC		 8/06	From GD_GENI				*
 ************************************************************************/
{
    char proj[21], firstm[21], lasttm[21];
    float deltan, deltax, deltay, gbnds[4], dbnds[4], ebnds[4],
	anl[LLNANL], rnav[LLNNAV];
    int iextnd[4], anlsz, navsz, igdfln, maxgrd, numgrd, kx, ky, in, nc,
	ii, ier;
    int wrtflg;
/*----------------------------------------------------------------------*/
    *iret = 0;

    wrtflg = G_FALSE;
    anlsz = LLNANL;
    navsz = LLNNAV;
    gd_open ( (char *)gdfile, &wrtflg, &anlsz, &navsz, &igdfln, anl, rnav,
        &maxgrd, &ier, strlen(gdfile) );
    cgd_ngrd ( igdfln, &numgrd, firstm, lasttm, &ier );

    /*
     * Loop through each output unit.
     */
    for ( ii = 0; ii < *nfps; ii++ ) {
	/*
	 * Write output grid file name.
	 */
	fprintf ( fps[ii], "\n GRID FILE: %s\n", gdfile  );

	/*
	 * Write grid navigation block.
	 */
	kx = (int)rnav[4];
	ky = (int)rnav[5];
	in = (int)rnav[0];
	cst_itos ( (int *)&rnav[1], 1, &nc, proj, &ier );
	fprintf ( fps[ii], "\n GRID NAVIGATION: \n" );
	if ( ( in == 1 ) || ( in == 2 ) || ( in == 3 ) ) {
	    fprintf ( fps[ii], "     PROJECTION:          %s\n", proj );
	    if ( in == 2 ) {
	    	fprintf ( fps[ii], "     ANGLES:            %8.1f%8.1f%8.1f\n",
		    rnav[10], rnav[11], rnav[12] );
	    }
	    fprintf ( fps[ii], "     GRID SIZE:         %4d%4d\n", kx, ky );
	    fprintf ( fps[ii], "     LL CORNER:         %10.2f%10.2f\n",
	        rnav[6], rnav[7] );
	    fprintf ( fps[ii], "     UR CORNER:         %10.2f%10.2f\n",
	        rnav[8], rnav[9] );
	} else {
	    fprintf ( fps[ii], "      UNKNOWN GRID NAVIGATION \n" );
	}

	/*
	 * Get grid analysis variables.
	 */
	grc_rban ( anl, &deltan, &deltax, &deltay, gbnds,
	           ebnds, dbnds, iextnd, &ier );

	/*
	 * Write grid analysis block.
	 */
	fprintf ( fps[ii], "\n GRID ANALYSIS BLOCK: \n" );
	if ( ier == 0 ) {
	    fprintf ( fps[ii], "     ANALYSIS TYPE:        BARNES\n" );
	    fprintf ( fps[ii], "     DELTAN:           %9.3f\n", deltan );
	    fprintf ( fps[ii], "     DELTAX:           %9.3f\n", deltax );
	    fprintf ( fps[ii], "     DELTAY:           %9.3f\n", deltay );
	    fprintf ( fps[ii], "     GRID AREA:         %8.2f%8.2f%8.2f%8.2f\n",
	        gbnds[0], gbnds[1], gbnds[2], gbnds[3] );
	    fprintf ( fps[ii], "     EXTEND AREA:       %8.2f%8.2f%8.2f%8.2f\n",
	        ebnds[0], ebnds[1], ebnds[2], ebnds[3] );
	    fprintf ( fps[ii], "     DATA AREA:         %8.2f%8.2f%8.2f%8.2f\n",
	        dbnds[0], dbnds[1], dbnds[2], dbnds[3] );
	} else {
	    fprintf ( fps[ii], "      UNKNOWN ANALYSIS TYPE \n" );
	}

	/*
	 * Write out the number of grids.
	 */
	fprintf ( fps[ii], "\n Number of grids in file: %5d\n", numgrd );
	fprintf ( fps[ii], "\n Maximum number of grids in file: %6d\n", maxgrd );

	/*
	 * Write out first and last times.
	 */
	fprintf ( fps[ii], "\n First time in file: %s\n", firstm );
	fprintf ( fps[ii], " Last  time in file: %s\n\n", lasttm ); 
    }

    gd_clos ( &igdfln, &ier );

    return;
}
