#include "df.h"

void df_xval ( const int *num, int *iret )
/************************************************************************
 * df_xval								*
 *									*
 * This subroutine returns the value of the x coordinate at each	*
 * grid point; only grids in graph coordinates may be used.		*
 *									*
 * df_xval ( num, iret )						*
 *									*
 * Input parameters:							*
 *	*num		const int	Grid number			*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					-10 = Internal grid list is full*
 *					-12 = ... must be a scalar	*
 *					-16 = Map proj. ... is invalid	*
 *					-20 = Stack is full		*
 **									*
 * Log:									*
 * I. Graffman/RDS	 2/87	DG_XVAL					*
 * M. desJardins/GSFC	 5/88	Renamed and fixed documentation		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/NMC         11/90   Pass the grid number in			*
 * J. Whistler/SSAI	 7/91	Initialize k to 0			*
 * K. Brill/NMC		 1/92	Replace GERROR with ER_WMSG		*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Brill/HPC		 4/02	Use internal grids as scratch grids	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    char cprj[5];
    int nrx, nry, nxy, kxyd, nval, kxd, kyd, ksub1, ksub2, i, j, indx, ier, zero;
    float *gnrx, *gnry, *gnxy, *gnum, snav[LLNNAV];
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Check for graph coordinate system.
     */
    nval = 1;
    dg_fget ( "SNAV", &nval, snav, iret );
    if ( !G_DIFFT(snav[0], 3.0F, GDIFFD) ) {
	*iret = -16;
	dg_cget ( "CPRJ", cprj, &ier );
	dg_cset ( "ERRST", cprj, &ier );
	return;
    }

    /*
     * Allocate scratch grids.
     */
    dg_nxts ( &nrx, iret );
    if ( *iret != 0 ) return;
    dg_nxts ( &nry, iret );
    if ( *iret != 0 ) return;
    dg_nxts ( &nxy, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &nrx, &gnrx, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nry, &gnry, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &nxy, &gnxy, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Load scratch array with grid values.
     */
    indx = 0;
    for ( j = 1; j <= kyd; j++ ) {
	for ( i = 1; i <= kxd; i++ ) {
	    gnrx[indx] = (float)i;
	    gnry[indx] = (float)j;
	    indx++;
	}
    }

    /*
     * Transform the points.
     */	
    dg_getg ( num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );
    kxyd = kxd * kyd;
    gtrans ( sys_G, sys_M, &kxyd, gnrx, gnry, gnum, gnxy, iret,
        strlen(sys_G), strlen(sys_M) );
    if ( *iret != 0 ) {
	*iret = -16;
	dg_cget ( "CPRJ", cprj, &ier );
	dg_cset ( "ERRST", cprj, &ier );
	return;
    }
    dg_esub ( num, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
