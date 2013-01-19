#include "gdgrib.h"

void gds_mak ( const int *navchg, const float *rnvblk, const int *nnv,
               int *nbytes, unsigned char *cgds, int *iret )
/************************************************************************
 * gds_mak								*
 *									*
 * This subroutine uses the GEMPAK grid navigation block to decide	*
 * which GDS maker to call.						*
 *									*
 * gds_mak ( navchg, rnvblk, nnv, nbytes, cgds, iret )			*
 *									*
 * Input parameters:							*
 *	*navchg		const int	Flag for navigation change	*
 *	*rnvblk		const float	GEMPAK grid navigation block	*
 *	*nnv		const int	Size of the navigation block	*
 *									*
 * Input and output parameter:						*
 *	*nbytes		int		Input: # of bytes available in	*
 *					       CGDS			*
 *					Output: # of bytes filled in	*
 *					       CGDS			*
 *									*
 * Output parameters:							*
 *	*cgds		unsigned char	GRIB GDS section		*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-75 = map proj not supported	*
 **									*
 * Log:									*
 * K. Brill/HPC		 8/99						*
 * K. Brill/HPC		 3/00	Added NAVCHG flag			*
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 ************************************************************************/
{
    int nc, ier;
    char proj[4];
/*----------------------------------------------------------------------*/
    *iret = 0 ;
    cst_itos ( (int *)&rnvblk[1], 1, &nc, proj, &ier );
    cst_rmbl ( proj, proj, &nc, &ier );

    /*
     * Select appropriate GDS maker routine.
     */
    if ( strcmp ( proj, "CED" ) == 0 ) {
	gds_ced ( rnvblk, nnv, nbytes, cgds, iret );
    } else if ( strcmp ( proj, "MER" ) == 0 ) {
	gds_mer ( rnvblk, nnv, nbytes, cgds, iret );
    } else if ( strcmp ( proj, "STR" ) == 0 ) {
	gds_str ( navchg, rnvblk, nnv, nbytes, cgds, iret );
    } else if ( strcmp ( proj, "SCC" ) == 0 ) {
	gds_lcc ( navchg, rnvblk, nnv, nbytes, cgds, iret );
    } else if ( strcmp ( proj, "LCC" ) == 0 ) {
	gds_lcc ( navchg, rnvblk, nnv, nbytes, cgds, iret );
    } else {
	*iret = -75;
    }

    return;
}
