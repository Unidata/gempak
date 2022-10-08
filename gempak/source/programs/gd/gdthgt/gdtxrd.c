#include "geminc.h"
#include "gemprm.h"

void gdtxrdf ( char *time, char *gvcord, char *gfunc, int *levt, int *levb, 
              int *lavflg, float *rgx, float *rgy, int *inttyp, 
              float *grid, float *yy, 
              int *iret, size_t timesize, size_t gvcordsize, size_t gfuncsize );

void gdtxrd ( char *time, char *gvcord, char *gfunc, int *levt, int *levb, 
              int *lavflg, float *rgx, float *rgy, int *inttyp, float *yy, 
              int *iret, size_t timesize, size_t gvcordsize, size_t gfuncsize );

void gdtxrd ( char *time, char *gvcord, char *gfunc, int *levt, int *levb, 
              int *lavflg, float *rgx, float *rgy, int *inttyp, float *yy, 
              int *iret, size_t timesize, size_t gvcordsize, size_t gfuncsize )
/************************************************************************
 * gdtxrd								*
 *									*
 * This function is a wrapper for subroutine GDTXRDF.  It's sole        *
 * purpose is to dynamically allocate a work array, and then pass       *
 * it and all of this function's arguments on to GDTXRDF.               *
 * The work array is freed from memory as soon as GDTXRDF returns.      *
 *									*
 * gdtxrd ( time, gvcord, gfunc, levt, levb, lavflg, rgx, rgy, inttyp,	*
 *	    yy, iret)							*
 *									*
 * Input parameters:							*
 *	TIME(2)		  CHAR*		Time to read			*
 *	GVCORD		  CHAR*		User vertical coord		*
 *	GFUNC		  CHAR*		User function			*
 *	LEVT		  INTEGER	Top level (or level if only 1)	*
 *	LEVB		  INTEGER	Bottom level			*
 *	LAVFLG		  LOGICAL	Flag for using layer average	*
 *	RGX(1)		  REAL		Grid point x			*
 *	RGY(1)		  REAL		Grid point y			*
 *	INTTYP		  INTEGER	Interpolation type		*
 *									*
 * Output parameters:							*
 *	YY(1)		  REAL		Output data (missing ok)	*
 *	IRET		  INTEGER	Return code			*
 *					  non zero if a problem		*
 **									*
 * Log:									*
 * S.Gilbert/NCEP	 8/07	                       			*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 * B. Hebbard/NCEP	08/21	Changed strlen in protos int->size_t    *
 ***********************************************************************/
{
    int   kx, ky, kxy, ier;
    float *grid;
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 *  Query grid size
 */
    dg_kxky ( &kx, &ky, &ier );

/*
 *  Allocate work array
 */
    kxy = kx * ky;
    G_MALLOC ( grid, float, kxy, "gdtxrd - grid");

    gdtxrdf ( time, gvcord, gfunc, levt, levb, 
              lavflg, rgx, rgy, inttyp, grid, yy, 
              iret, timesize, gvcordsize, gfuncsize );

/*
 *  Free work array
 */
    G_FREE ( grid, float );

}
