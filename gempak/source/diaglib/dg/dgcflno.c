#include "dg.h"

void dgc_flno ( const char *gfunc, int *igdfln, int *iret )
/************************************************************************
 * dgc_flno								*
 *									*
 * This subroutine returns the grid file number corresponding to the	*
 * first grid file referenced in GFUNC.  This number can be used to 	*
 * call GD_ subroutines to find the levels in a grid file.		*
 *									*
 * dgc_flno ( gfunc, igdfln, iret )					*
 *									*
 * Input parameters:							*
 *	*gfunc		const char	Input for GFUNC			*
 *									*
 * Output parameters:							*
 *	*igdfln		int		Grid file number		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-32 = invalid file number	*
 **									*
 * Log:									*
 * K. Brill/GSC		11/89						*
 * M. desJardins/NMC	 3/92	Check for file not open; MGFILE->MMFILE	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    char *plus, fnum[2];
    int jf;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check for a + indicating that a file other than the first file
     * is to be used.
     */
    plus = strchr ( gfunc, '+' );

    /*
     * Get the correct file number.  
     */
    if ( ! plus ) { 
	*igdfln = _dgfile.idflnm[0];
    } else {
        plus++;
	if ( ( *plus == 'f' ) || ( *plus == 'F' ) ) plus++;
	fnum[0] = *plus;
	fnum[1] = '\0';
	cst_numb ( fnum, &jf, iret );
	if ( ( *iret != 0 ) || ( jf < 1 ) || ( jf > MMFILE ) ) {
	    *igdfln = 0;
	} else {
	    *igdfln = _dgfile.idflnm[jf-1];
	}
    }

    /*
     * If the file number is invalid, return an error.
     */
    if ( *igdfln <= 0 ) *iret = -32;

    return;
}
