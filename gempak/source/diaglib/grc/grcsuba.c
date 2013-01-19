#include "geminc.h"
#include "gemprm.h"

void grc_suba ( const char *garea, const int *fill, float *rnvblk,
                float *altln, int *ksubx, int *ksuby, int *subset,
		int *iret )
/************************************************************************
 * GR_SUBA								*
 *									*
 * This subroutine determines the navigation of a grid subset.  The	*
 * input navigation block is used to set a navigation which is then	*
 * subset using the input in GAREA.  GAREA is either @ followed by	*
 * actual grid index coordinates of the lower-left and upper right	*
 * points of the subset rectangle, or it is the lat/lon coordinates	*
 * of the two corner points.  In either case, the four numbers are	*
 * separated by semicolons.  In the latter case, the nearest grid	*
 * points are used.							*
 *									*
 * Once the subset is determined the navigation block is modified	*
 * accordingly.  The subset coordinates relative to the larger grid	*
 * are set and returned in KSUBX and KSUBY.  The lat/lon coordinates	*
 * of the corners are returned in ALTLN.				*
 *									*
 * If the grid is an eta staggered grid, then the area is adjusted so   *
 * that the corner points are h points.					*
 *									*
 * GR_SUBA ( GAREA, FILL, RNVBLK, ALTLN, KSUBX, KSUBY, SUBSET, IRET )	*
 *									*
 * Input parameters:							*
 *	GAREA		CHAR*		Input for subset area		*
 *									*
 * Input and output parameters:						*
 *	FILL		LOGICAL		Flag for filled staggered grid  *
 *	RNVBLK (*)	REAL		Grid navigation block		*
 *									*
 * Output parameters:							*
 *	ALTLN (4)	REAL		Lat/lon bounds of subset	*
 *	KSUBX (2)	INTEGER		Start/stop x index of subset	*
 *	KSUBY (2)	INTEGER		Start/stop y index of subset	*
 *	SUBSET		LOGICAL		Subset flag			*
 *	IRET		INTEGER		Return code			*
 *					  0 = Normal			*
 *					 -4 = Invalid navigation info	*
 *					-20 = Invalid input for GAREA	*
 *									*
 **									*
 * Log:									*
 * K. Brill/NMC		 4/95						*
 * S. Jacobs/NMC	 5/95	Added call to LC_GARE to parse GAREA	*
 * L. Sager/NMC		 8/95	Added NAGSS2 to permit grid wrapping	*
 * K. Brill/EMC		 4/96	Added fill to start on h point		*
 * G. Krueger/EAI	 6/96	Add default projection			*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 * D.W.Plummer/NCEP	 3/00	Re-name from NAGSUB to GR_SUBA and	*
 * 				Re-name from NAGSS2 to GR_SUB2		*
 * R. Tian/SAIC         07/06   Translated from Fortran                 *
 ************************************************************************/
{
/*----------------------------------------------------------------------*/

    gr_suba ( garea, fill, rnvblk, altln, ksubx, ksuby, subset, iret,
               strlen(garea) );

    return;
}
