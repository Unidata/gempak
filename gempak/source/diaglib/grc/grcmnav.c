#include "geminc.h"
#include "gemprm.h"

void grc_mnav  ( const char *proj, const int *kx, const int *ky, 
		const float *rlat1, const float *rlon1, 
		const float *rlat2, const float *rlon2, 
		const float *angl1, const float *angl2, 
		const float *angl3, const int *angflg, float *rnvblk, 
		int *iret )
/************************************************************************
 * grc_mnav								*
 *									*
 * This subroutine makes a navigation block for a grid file.  The 	*
 * projection may be any simple, full for graph projection.  If 	*
 * ANGFLG is set, the projection must be a full map projection.  	*
 * Otherwise, a simple map projection will be defined.			*
 *									*
 * grc_mnav  ( proj, kx, ky, rlat1, rlon1, rlat2, rlon2, angl1, 	*
 *            angl2, angl3, angflg, rnvblk, iret )			*
 *									*
 * Input parameters:							*
 *	proj		char		Projection name			*
 *	kx		int		Number of x grid points		*
 *	ky		int		Number of y grid points 	*
 *	rlat1		float		Lower left latitude/x		*
 *	rlon1		float		Lower left longitude/y		*
 *	rlat2		float		Upper right latitude/x		*
 *	rlon2		float		Upper right longitude/y		*
 *	angl1		float		Projection angle 1		*
 *	angl2		float		Projection angle 2		*
 *	angl3		float		Projection angle 3		*
 *	angflg		int		Full projection flag		*
 *									*
 * Output parameters:							*
 *	rnvblk [llnnav)	float		Navigation block		*
 *	iret		int		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 8/88	Rewrote ||GEMPAK 4			*
 * K. Brill/NMC		01/92	Initialize the navigation block to zero *
 * K. Brill/NMC		02/92	Use LLNNAV				*
 * D.W.Plummer/NCEP      1/05   Translated from FORTRAN                 *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
	gr_mnav ( (char *)proj, (int *)kx, (int *)ky, (float *)rlat1,
	    (float *)rlon1, (float *)rlat2, (float *)rlon2,
	    (float *)angl1, (float *)angl2, (float *)angl3,
	    (int *)angflg, rnvblk, iret, strlen(proj) );

	return;
}
