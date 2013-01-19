#include "geminc.h"
#include "gemprm.h"

void gdtxrvf ( char *time, char *gvcord, char *gvecx, int *levt, int *levb, 
              int *lavflg, float *rgx, float *rgy, int *inttyp, int *lvert, 
              float *gridu, float *gridv, float *uu, float *vv, int *iret, 
              int timesize, int gvcordsize, int gvecxsize );

void gdtxrv ( char *time, char *gvcord, char *gvecx, int *levt, int *levb, 
              int *lavflg, float *rgx, float *rgy, int *inttyp, int *lvert, 
              float *uu, float *vv, int *iret, int timesize, int gvcordsize,
              int gvecxsize );

void gdtxrv ( char *time, char *gvcord, char *gvecx, int *levt, int *levb, 
              int *lavflg, float *rgx, float *rgy, int *inttyp, int *lvert, 
              float *uu, float *vv, int *iret, int timesize, int gvcordsize,
              int gvecxsize )
/************************************************************************
 * GDTXRV								*
 *									*
 * This function is a wrapper for subroutine GDTXRVF.  It's sole        *
 * purpose is to dynamically allocate two work arrays, and then pass    *
 * them and all of this function's arguments on to GDTXRVF.             *
 * The work arrays are freed from memory as soon as GDTXRVF returns.    *
 *									*
 * GDTXRV ( TIME, GVCORD, GVECX, LEVT, LEVB, LAVFLG, RGX, RGY, INTTYP,	*
 *	    LVERT, UU, VV, IRET)					*
 *									*
 * Input parameters:							*
 *	TIME(2)		  CHAR*		Time to read			*
 *	GVCORD		  CHAR*		User vertical coord		*
 *	GVECX		  CHAR*		User function			*
 *	LEVT		  INTEGER	Top level (or level if only 1)	*
 *	LEVB		  INTEGER	Bottom level			*
 *	LAVFLG		  LOGICAL	Flag for using layer average	*
 *	RGX(1)		  REAL		Grid point x			*
 *	RGY(1)		  REAL		Grid point y			*
 *	INTTYP		  INTEGER	Interpolation type		*
 *	LVERT		  LOGICAL	Vector type non-relative	*
 *									*
 * Output parameters:							*
 *	UU(1)		  REAL		Output data u (missing ok)	*
 *	VV(1)		  REAL		Output data v (missing ok)	*
 *	IRET		  INTEGER	Return code			*
 *					  non zero if a problem		*
 **									*
 * Log:									*
 * S.Gilbert/NCEP	 8/07	                       			*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    int   kx, ky, kxy, ier;
    float     *gridu, *gridv;
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 *  Query grid size
 */
    dg_kxky ( &kx, &ky, &ier );

/*
 *  Allocate work arrays
 */
    kxy = kx * ky;
    G_MALLOC ( gridu, float, kxy, "gdtxrv - gridu");
    G_MALLOC ( gridv, float, kxy, "gdtxrv - gridu");

    gdtxrvf ( time, gvcord, gvecx, levt, levb, 
              lavflg, rgx, rgy, inttyp, lvert, 
              gridu, gridv, uu, vv, iret, 
              timesize, gvcordsize, gvecxsize );

/*
 *    Free work array
 */
    G_FREE ( gridu, float );
    G_FREE ( gridv, float );

}
