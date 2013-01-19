#include "df.h"

void df_corl ( const int *num, int *iret )
/************************************************************************
 * df_corl								*
 *									*
 * This subroutine computes the Coriolis acceleration at each grid	*
 * point.  The following equation is used:				*
 *									*
 *     CORL = 2. * OMEGA * SIN ( LATR )					*
 * 									*
 * This computation has no operand.					*
 *									*
 * df_corl ( num, iret )						*
 *									*
 * Input parameters:							*
 *	*num      	const int	Grid number			*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-10 = Internal grid list is full*
 *					-12 = ... must be a scalar	*
 *					-16 = Map proj. ... is invalid	*
 *					-20 = Stack is full		*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * W. Skillman/GSFC	 5/88	Added new stack functions		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC		10/89   Subsetting				*
 * K. Brill/NMC		11/90	Pass in the grid number			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				by using internal grids for lat/lon	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		10/05	Recoded from Fortran			*
 ************************************************************************/
{
    int nval, idglat, kx, ky, ksub1, ksub2, fidx, cidx;
    float *gklat, *gnum;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Check if navigation parameters have been computed.
     */
    dg_ltln ( iret );
    if ( *iret != 0 ) return;

    /*
     * Query DGCMN.CMN idglat.
     */
    nval = 1;
    dg_iget ( "IDGLAT", &nval, &idglat, iret );

    /*
     * Grid number to grid.
     */
    dg_getg ( &idglat, &gklat, &kx, &ky, &ksub1, &ksub2, iret );
    dg_getg ( num, &gnum, &kx, &ky, &ksub1, &ksub2, iret );

    /*
     * Fill the grid with coriolis parameter.
     */
    for ( fidx = ksub1; fidx <= ksub2; fidx++ ) {
	cidx = fidx - 1;
        gnum[cidx] = 2. * OMEGA * sin ( gklat[cidx] );
    }

    return;
}
