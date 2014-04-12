#include "dg.h"

void dg_prcp ( const char *time1, const char *time2, const int *level1,
               const int *level2, const int *ivcord, const char *parm,
	       int *num, int *iret )
/************************************************************************
 * dg_prcp								*
 *									*
 * This subroutine computes precipitation from variables found in a 	*
 * grid file.  The subroutine first tries to compute the output grid	*
 * from the P, S, or C values found in the grid.  If not successful,	*
 * it will convert from inches to millimeters or vice versa.  Finally,	*
 * it will try to compute precipitation from a rate.			*
 *									*
 * This subroutine will also accumulate precipitation by type in a	*
 * similar way, with the initial letter denoting the type according to  *
 * the following:							*
 *									*
 *     Initial Letter      Precipitation Type				*
 *          W                  Snowfall					*
 *          I                  Ice pellets				*
 *          Z                  Freezing rain                            *
 *          A                  Rain  					*
 *          H                  Hail 					*
 *          G                  Graupel					*
 *          N                  Snow melt				*
 *          R                  Storm surface runoff			*
 *          L                  Total liquid equivalent precipitation    *
 *									*
 * The last letter of the parameter name follows the accumulation	*
 * interval and denotes inches or millimeters by I or M, respectively.  *
 * Conversion from inches to millimeters or vice versa is supported	*
 * for these variables as well.						*
 *									*
 * dg_prcp ( time1, time2, level1, level2, ivcord, parm, num, iret )    *
 *									*
 * Input parameters:							*
 *      *time1          const char      Date/time                       *
 *      *time2          const char      Date/time                       *
 *      *level1         const int       Level                           *
 *      *level2         const int       Level                           *
 *      *ivcord         const int       Vertical coordinate             *
 *      *parm           const char      Parameter name                  *
 *									*
 * Input and output parameters:						*
 *      *num            int             Location of grid                *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -7 = grid cannot be found	*
 **									*
 * Log:									*
 * K. Brill/NMC		 8/91						*
 * K. Brill/NMC		 2/92	Correct factor for snow depth conversion*
 * K. Brill/NMC		 3/92	Call ST_NUMB				*
 * K. Brill/NMC		 2/93	Check for alterne ways to get precip	*
 * M. desJardins/NMC	 7/93	DG_UHDR --> DG_UPSG			*
 * M. desJardins/NMC	 3/94	Check for precipitation as a rate	*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC          2/06   Recoded from Fortran                    *
 * K. Brill/WPC          4/13   Document accumulation for precip types	*
 ************************************************************************/
{
    char pp[14], lstchr;
    int lenstr, nhrs, ier;
/*----------------------------------------------------------------------*/
    *iret = -7;

    /*
     * Read in the precipitation and convert between P, S, and C, or
     * read in a precipitation type accumulation (W,I,Z,A,H,G,N,R,L).
     */
    dg_prdr ( time1, time2, level1, level2, ivcord, parm, num, iret );
    if ( *iret == 0 ) return;

    /*
     * Get precipitation in either millimeters or inches.
     */
    cst_lstr ( (char *)parm, &lenstr, &ier );
    lstchr = parm[lenstr-1];
    strcpy ( pp, parm );
    if ( lstchr == 'M' ) {
	pp[lenstr-1] = 'I';
	dg_prdr ( time1, time2, level1, level2, ivcord, pp, num, iret );
	if ( *iret == 0 ) {
	    pd_inmm ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		      _dggrid.dgg[(*num)-1].grid, &ier );
	}
    } else if ( lstchr == 'I' ) {
	pp[lenstr-1] = 'M';
	dg_prdr ( time1, time2, level1, level2, ivcord, pp, num, iret );
	if ( *iret == 0 ) {
	    pd_mmin ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
		      _dggrid.dgg[(*num)-1].grid, &ier );
	}
    }

    /*
     * Check to see if data is available as a rate.
     */
    if ( *iret != 0 ) {
	strcpy ( pp, "PR" );
	strncat ( pp, &parm[1], lenstr-2 );
	pp[lenstr] = '\0';
	dg_grdr ( time1, time2, level1, level2, ivcord, pp, num, iret );
	if ( *iret == 0 ) {
	    strncpy ( pp, &parm[1], lenstr - 2 );
	    pp[lenstr-2] = '\0';
	    cst_numb ( pp, &nhrs, &ier );
	    if ( ier != 0 ) return;
	    pd_prcr ( _dggrid.dgg[(*num)-1].grid, &nhrs, &_dgfile.kxyd,
		      _dggrid.dgg[(*num)-1].grid, &ier );
	    if ( lstchr == 'I' ) {
		pd_mmin ( _dggrid.dgg[(*num)-1].grid, &_dgfile.kxyd,
			  _dggrid.dgg[(*num)-1].grid, &ier );
	    }
	}
    }

    return;
}
