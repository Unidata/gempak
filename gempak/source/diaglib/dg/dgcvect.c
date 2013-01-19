#include "dg.h"

void dgc_vect ( const char *gdattm, const char *glevel, const char *gvcord,
                const char *gvect, char *pfunc, float *ugrid, float *vgrid,
	        int *igx, int *igy, char *time1, char *time2, int *level1,
	        int *level2, int *ivcord, char *parmu, char *parmv,
	        int *iret )
/************************************************************************
 * dgc_vect								*
 *									*
 * This subroutine computes a grid diagnostic vector quantity.  The 	*
 * u and v components returned in UGRID and VGRID are in north 		*
 * relative coordinates.  GDATTM, GLEVEL, GVCORD and GVECT should have	*
 * the values entered by the user.					*
 *									*
 * dgc_vect ( gdattm, glevel, gvcord, gvect, pfunc, ugrid, vgrid, igx,	*
 *            igy, time1, time2, level1, level2, ivcord, parmu, parmv,	*
 *            iret )							*
 *									*
 * Input parameters:							*
 *	*gdattm		const char	Input date/time			*
 *	*glevel		const char	Input level			*
 *	*gvcord		const char	Input vertical coordinate	*
 *	*gvect		const char	Diagnostic function		*
 *									*
 * Output parameters:							*
 *	*pfunc		char		Diagnostic error string		*
 *	*ugrid		float		Output u component grid		*
 *	*vgrid		float		Output v component grid		*
 *	*igx		int		Number of points in x dir	*
 *	*igy		int		Number of points in y dir	*
 *	*time1		char		Output date/time		*
 *	*time2		char		Output date/time		*
 *	*level1		int		Output level 			*
 *	*level2		int		Output level 			*
 *	*ivcord		int		Output vertical coordinate	*
 *	*parmu		char		Parameter name for u component 	*
 *	*parmv		char		Parameter name for v component 	*
 *	*iret		int		Return code			*
 *				  	  3 = user typed EXIT		*
 *					  0 = normal return		*
 *					 -3 = GFUNC is blank		*
 *					 -5 = output grid not a vector	*
 *					 -6 = wrong number of operands	*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					 -9 = incorrect operands	*
 *					-10 = internal grid list is full*
 *					-11 = operand must be a vector	*
 *					-12 = operand must be a scalar	*
 *					-13 = operand not in grid file	*
 *					-14 = DG_INIT not initialized	*
 *					-15 = polar grid center invalid	*
 *					-16 = map proj is invalid	*
 *					-17 = LEVEL must be a layer	*
 *					-18 = TIME must be a range	*
 *					-19 = invalid operator		*
 *					-20 = stack is full		*
 *					-21 = stack is empty		*
 *					-22 = TIME is invalid		*
 *					-23 = LEVEL is invalid		*
 *					-24 = IVCORD is invalid		*
 *					-26 = layer of layers invalid	*
 *					-27 = time range layer invalid	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 4/86	Cleaned up errors; add GR_FIND		*
 * M. desJardins/GSFC	 5/88	Documentation				*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/GSFC	 4/89	Added grid rel and north rel subs	*
 * S. Schotz/GSC	 6/90	Removed respnd flag			*
 * K. Brill/NMC		 8/90   Do not rotate if grid relative rqstd	*
 * K. Brill/NMC          9/90   Do not rotate if CIRC is requested	*
 * R. Tian/SAIC		 3/06	Recoded from Fortran			*
 ************************************************************************/
{
    char gvcchk[33];
    int nc, ier;
/*----------------------------------------------------------------------*/
    /*
     * Get the grid relative components first.
     */
    dgc_vecr ( gdattm, glevel, gvcord, gvect, pfunc, ugrid, vgrid, igx, igy,
        time1, time2, level1, level2, ivcord, parmu, parmv, iret );

    /*
     * If the vector has been calculated, compute the north relative
     * components unless it is supposed to be returned as grid relative.
     */
    if ( *iret == 0 ) {
	strncpy ( gvcchk, gvect, 32 );
	gvcchk[32] = '\0';
	cst_ldsp ( gvcchk, gvcchk, &nc, &ier );
	cst_lcuc ( gvcchk, gvcchk, &ier );
	if ( ! strstr ( gvcchk, "CIRC" ) ) {
	    dg_nrel ( ugrid, vgrid, ugrid, vgrid, iret );
	}
    }

    return;
}
