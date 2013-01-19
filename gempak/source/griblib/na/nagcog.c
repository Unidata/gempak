#include "na.h"

void na_gcog ( const char *gdoutf, const char *proj, const char *grdarea,
               const char *kxky, const char *cpyfil, const int *fill,
	       const char *garea, const float *gdsarr, int *maxg, 
	       int *igdfln, float *rnvful, int *igrdnm, char *cprj,
	       int *ikx, int *iky, int *jx, int *jy, int *ksubx,
	       int *ksuby, int *subset, int *iret )
/************************************************************************
 * na_gcog								*
 *									*
 * This routine will create a new GEMPAK grid file, or open an 		*
 * existing file.							*
 *									*
 * na_gcog ( gdoutf, proj, grdarea, kxky, cpyfil, fill, garea, gdsarr,	*
 *	     maxg, igdfln, rnvful, igrdnm, cprj, ikx, iky, jx, jy,	*
 *	     ksubx, ksuby, subset, iret )				*
 *									*
 * Input parameters:							*
 *	*gdoutf		const char	GEMPAK output grid file		*
 *	*proj		const char	Projection of grid to create	*
 *	*grdarea	const char	Grid area of grid to create	*
 *	*kxky		const char	Number of points for grid	*
 *	*cpyfil		const char	File or table number to copy	*
 *					   for navigation info		*
 *	*fill		const int	Fill flag			*
 *	*garea		const char	Grid subset area		*
 *	*gdsarr		const float	GDS projection information	*
 *									*
 * Input/output parameters:						*
 *	*maxg		int		Max number of grids for file	*
 *									*
 * Output parameters:							*
 *	*igdfln		int		Output grid file number		*
 *	*rnvful		float		Navigation block for full grid	*
 *	*igrdnm		int		Navigation number from table	*
 *	*cprj		char		Projection of the grid		*
 *	*ikx		int		Number of points in X-direction	*
 *	*iky		int		Number of points in Y-direction	*
 *	*jx		int		Number of X pts on filled grid	*
 *	*jy		int		Number of Y pts on filled grid	*
 *	*ksubx (2)	int		Subset range in X-direction	*
 *	*ksuby (2)	int		Subset range in Y-direction	*
 *	*subset		int		Subset flag			*
 *	*iret		int		Return code			*
 *						-10 GDOUTF = ' '	*
 *						-11 Error opening file	*
 *						-12 Error creating file	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/EAI	 7/93						*
 * S. Jacobs/NMC	 2/94	Added file name return to FL_INQR	*
 * K. Brill/NMC		 4/95	Added grid subsetting			*
 * S. Jacobs/NMC	 5/95	Added subset flag			*
 * K. Brill/NMC		 5/96	Added fill and check for grid too big	*
 * D.W.Plummer/NCEP	 3/00	Rename NAGSUB to GR_SUBA		*
 * T. Piper/SAIC	 1/02	Initialized anlblk			*
 * K. Brill/HPC		11/02	Check igszch against LLMXTG		*
 * R. Tian/SAIC		 4/05	Changed GD_OPFN to GD_OPEN		*
 * R. Tian/SAIC		 7/06	Recoded from Fortran			*
 * S. Gilbert/NCEP	10/06	Added er_wmsg calls                     *
 ************************************************************************/
{
    char cpytmp[73], name[5], newfil[133];
    float anlblk[LLNANL], rnvtst[LLNNAV], anltst[LLNANL], altln[4],
          rnvblk[LLNNAV];
    long flen;
    int exist, gsflag;
    int navsz, anlsz, iflno, ihd, mgrd, ii, ier, ier1;
/*----------------------------------------------------------------------*/
    *iret  = 0;
    anlsz = LLNANL;
    navsz = LLNNAV;

    /*
     * Initialize anlblk.
     */
    for ( ii = 0; ii < LLNANL; ii++ ) {
        anlblk[ii] = 0.;
    }

    /*
     * If the output file is blank, return.
     */
    if ( gdoutf[0] == '\0' ) {
	*iret = -10;
	return;
    }

    /*
     * Check for the existence of the output file. If it exists, open
     * it, if not, create it.
     */
    cfl_inqr ( (char *)gdoutf, NULL, &flen, newfil, &ier );
    if ( ier == 0 ) {
	printf ( "Opening the GEMPAK grid file\n" );
	exist = G_TRUE;
	gd_open ( (char *)gdoutf, &exist, &anlsz, &navsz, igdfln, anlblk,
	          rnvblk, maxg, &ier, strlen(gdoutf) );
	if ( ier != 0 ) {
	    *iret = -11;
	    return;
	}

	/*
	 * Check the navigation information against the GRIB info.
	 */
	cst_lcuc ( (char *)cpyfil, cpytmp, &ier );
	if ( cpyfil[0] == '\0' ) {
	    /*
	     * CASE 1: Get navigation block from the user input.
	     */
	    na_gnav ( proj, kxky, grdarea, cprj, ikx, iky, altln, rnvful,
	              &ier );
	    if ( ier != 0 ) {
                er_wmsg ( "NA", &ier, " ", &ier1, 2, 1 );
		*iret = -11;
		return;
	    }
	} else if ( cpyfil[0] == '#' ) {
	    /*
	     * CASE 2: Get navigation block from the grid
	     *         navigation table input.
	     */
	    na_gtbl ( cpyfil, name, cprj, ikx, iky, altln, rnvful, anltst,
	              &ier );
	    if ( ier != 0 ) {
                er_wmsg ( "NA", &ier, " ", &ier1, 2, 1 );
		*iret = -11;
		return;
	    }
	    cst_numb ( (char *)&cpyfil[1], igrdnm, &ier );
	} else if ( strcmp ( cpytmp, "GDS" ) == 0 ) {
	    /*
	     * CASE 3: Get navigation block from the GDS information.
	     */
	    if ( *fill == G_TRUE && G_DIFF ( gdsarr[1] + gdsarr[2], 0.0F ) ) {
		*iret = -21;
		return;
	    }
	    na_ggds ( gdsarr, cprj, ikx, iky, altln, rnvful, &ier );
	    if ( ier != 0 ) {
                er_wmsg ( "NA", &ier, " ", &ier1, 2, 1 );
		*iret = -11;
		return;
	    }
	}

	for ( ii = 0; ii < LLNNAV; ii++ ) {
	    rnvtst[ii] = rnvful[ii];
	}
	grc_suba ( garea, fill, rnvtst, altln, ksubx, ksuby, subset, iret );
	if ( *iret != 0 ) return;
	grc_cnav ( rnvblk, rnvtst, &navsz, &gsflag, &ier );
	if ( gsflag == G_FALSE ) {
	    *iret = -11;
	    return;
	}
    } else {
	/*
	 * Create a new file by one of the methods below.
	 */
	printf ( "Creating the GEMPAK grid file...\n" );
	ihd = 10;
	cst_lcuc ( (char *)cpyfil, cpytmp, &ier );
	if ( cpyfil[0] == '\0' ) {
	    /*
	     * CASE 1: Build new navigation block from the user input.
	     */
	    na_gnav ( proj, kxky, grdarea, cprj, ikx, iky, altln, rnvful,
	              &ier );
	    if ( ier != 0 ) {
                er_wmsg ( "NA", &ier, " ", &ier1, 2, 1 );
	    	*iret = -12;
	    	return;
	    }
	    if ( ier == 0 ) na_ganl ( "4/0;0;0;0", rnvful, anlblk, &ier );
	} else if ( cpyfil[0] == '#' ) {
	    /*
	     * CASE 2: Build new navigation and analysis blocks from
	     *         the grid navigation table input.
	     */
	    na_gtbl ( cpyfil, name, cprj, ikx, iky, altln, rnvful, anlblk,
	              &ier );
	    if ( ier != 0 ) {
                er_wmsg ( "NA", &ier, " ", &ier1, 2, 1 );
		*iret = -12;
		return;
	    }
	    cst_numb ( (char *)&cpyfil[1], igrdnm, &ier );
	} else if ( strcmp ( cpytmp, "GDS" ) == 0 ) {
	    /*
	     * CASE 3: Build new navigation block from the GDS
	     *	       information.
	     */
	    if ( *fill == G_TRUE && G_DIFF ( gdsarr[1] + gdsarr[2], 0.0F ) ) {
		*iret = -21;
		return;
	    }
	    na_ggds ( gdsarr, cprj, ikx, iky, altln, rnvful, &ier );
	    if ( ier != 0 ) {
                er_wmsg ( "NA", &ier, " ", &ier1, 2, 1 );
		*iret = -12;
		return;
	    }
	    if ( ier == 0 ) na_ganl ( "4/0;0;0;0", rnvful, anlblk, &ier );
	} else {
	    /*
	     * CASE 4: Get navigation and analysis blocks from the
	     *	       old file.
	     */
	    exist = G_FALSE;
	    gd_open ( (char *)cpyfil, &exist, &anlsz, &navsz, &iflno, anlblk,
	              rnvful, &mgrd, &ier, strlen(cpyfil) );
	    if ( ier != 0 ) {
		*iret = -12;
		return;
	    }
	    grc_rnav ( rnvful, cprj, ikx, iky, &ier );
	    altln[0] = rnvful[6];
	    altln[1] = rnvful[7];
	    altln[2] = rnvful[8];
	    altln[3] = rnvful[9];
	}

	/*
	 * Create the file.
	 */
	for ( ii = 0; ii < LLNNAV; ii++ ) {
	    rnvblk[ii] = rnvful[ii];
	}
	grc_suba ( garea, fill, rnvblk, altln, ksubx, ksuby, subset, iret );
	if ( *iret != 0 ) return;
	gd_cref ( gdoutf, &navsz, rnvblk, &anlsz, anlblk, &ihd, maxg,
	          igdfln, &ier, strlen(gdoutf) );
	if ( ier != 0 ) {
	    *iret = -12;
	    return;
	}
    }

    /*
     * If necessary, get dimensions of staggered grid.
     */
    if ( *fill == G_TRUE ) {
	grc_rnav ( rnvful, name, jx, jy, &ier );
	*jx = ( *jx + 1 ) / 2;
    }
    printf ( "GEMPAK grid file is ready...\n" );

    return;
}
