#include "df.h"

void df_mass ( int *iret )
/************************************************************************
 * df_mass								*
 *									*
 * This subroutine computes MASS, the mass per unit volume in a layer:	*
 *									*
 *     MASS ( PRES ) = 100 * LDF (PRES) / ( GRAVTY * (level1 - level2) )*
 *									*
 *                     Where: the 100 converts millibars to Pascals	*
 *                            level1, level2 are also converted to	*
 *                              Pascals when VCORD=PRES			*
 *									*
 * The volume is expressed in units of					*
 * meters * meters * (units of vert coord).				*
 *									*
 * df_mass ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * M. desJardins/GSFC	 7/88	Rewritten so layer can be found		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC		10/89   Subsetting				*
 * M. desJardins/NMC	 7/93	Changed update scheme			*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    char time1[21], time2[21], gparm[13], gfunc[13];
    int level1, level2, ndp, ivcord, kx, ky, ksub1, ksub2, i, im1, zero,
        ier;
    float dellev, cnst, *gndp, dg;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Pressure is on the stack.  Compute the level difference.
     * Note that this function is called by DG_GETS.  Therefore,
     * it cannot call subroutines which call DG_GETS.
     */
    df_ldf ( iret );
    if ( *iret != 0 )  return;

    /*
     * Get information about the pressure difference grid.
     */
    dg_tops ( gfunc, &ndp, time1, time2, &level1, &level2,
              &ivcord, gparm, iret );
    if ( *iret != 0 )  return;

    /*
     * Compute the layer difference of the vertical coordinate
     */
    dellev = (float)( level1 - level2 );

    /*
     * Convert millibars to pascals when vert. coord. is PRES.
     */
    if ( ivcord == 1 )  dellev *= 100.;

    /*
     * Compute constant for multiplication.
     */
    cnst = -1. / GRAVTY;

    /*
     * Compute the mass grid.
     */
    dg_getg ( &ndp, &gndp, &kx, &ky, &ksub1, &ksub2, iret );

    for ( i = ksub1; i <= ksub2; i++ ) {
        im1 = i - 1;
	if ( ERMISS ( gndp[im1] ) ) {
	    gndp[im1] = RMISSD;
	} else {
	    /*
	     * Convert pressure from millibars to pascals, then 
	     * compute average mass.
  	     */
	    dg = gndp[im1] * 100.;
	    gndp[im1] = G_ABS ( cnst * dg / dellev );
	}
    }

    /*
     * Make a name MASS and update header; the stack is current.
     */
    dg_upsg ( time1, time2, &level1, &level2, &ivcord, &zero, "MASS",
              &ndp, iret );
    dg_esub ( &ndp, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
