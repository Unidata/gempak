#include "geminc.h"
#include "gemprm.h"

void gdxdtaf  ( int *iflno, char *gdatim, char *gvcord, float *ystrt,
               float *ystop, char *gfunc, char *time, int *ivcord,
               float *rgx, float *rgy, int *nhxs, float *grid, 
               float *rlvl, float *xgrd,
               int *nvxs, char *parm, float *ybeg, float *yend, int *iret,
               size_t gdatimsize, size_t gvcordsize, size_t gfuncsize, size_t timesize, 
               size_t parmsize );

void gdxdta  ( int *iflno, char *gdatim, char *gvcord, float *ystrt,
               float *ystop, char *gfunc, char *time, int *ivcord,
               float *rgx, float *rgy, int *nhxs, float *rlvl, float *xgrd,
               int *nvxs, char *parm, float *ybeg, float *yend, int *iret,
               size_t gdatimsize, size_t gvcordsize, size_t gfuncsize, size_t timesize, 
               size_t parmsize );

void gdxdta  ( int *iflno, char *gdatim, char *gvcord, float *ystrt,
               float *ystop, char *gfunc, char *time, int *ivcord,
               float *rgx, float *rgy, int *nhxs, float *rlvl, float *xgrd,
               int *nvxs, char *parm, float *ybeg, float *yend, int *iret,
               size_t gdatimsize, size_t gvcordsize, size_t gfuncsize, size_t timesize, 
               size_t parmsize )
/************************************************************************
 * gdxdta								*
 *									*
 * This function is a wrapper for subroutine GDXDTAF.  It's sole        *
 * purpose is to dynamically allocate a work array, and then pass       *
 * it and all of this function's arguments on to GDXDTAF.               *
 * The work array is freed from memory as soon as GDXDTAF returns.      *
 *									*
 * gdxdta  ( iflno, gdatim, gvcord, ystrt, ystop, gfunc, 		*
 *           time, ivcord, rgx, rgy, nhxs, rlvl, xgrd, 			*
 *           nvxs, parm, ybeg, yend, iret )				*
 *									*
 * Input parameters:							*
 *      IFLNO             INTEGER       Grid file number                *
 *	GDATIM		  CHAR*		User input date/time		*
 *	GVCORD		  CHAR*		User input vert coord		*
 *      YSTRT             REAL          Bottom vert coord value		*
 *      YSTOP             REAL          Top vert coord value		*
 *	GFUNC	 	  CHAR*		User input function		*
 *	TIME  (2)	  CHAR*		Time to search for levels	*
 *	IVCORD		  INTEGER	Vertical coordinate for search	*
 *	RGX  (NHXS)	  REAL		X grid coordinates		*
 *	RGY  (NHXS)	  REAL		Y grid coordinates		*
 *	NHXS		  INTEGER	Number of xsect pts in horiz.	*
 *									*
 * Output parameters:							*
 *      RLVL (NVXS)	  REAL		Vertical levels in grid		*	
 *	XGRD (NHXS, NVXS) REAL		Array of cross section values	*
 *      NVXS              INTEGER       Number of xsect pts in vert.	*
 *	PARM		  CHAR*		Parameter name			*
 *	YBEG		  REAL		Beginning y value of grid	*
 *      YEND		  REAL          Ending y value of grid		*
 *	IRET		  INTEGER	Return code			*
 *					  7 = GFUNC not specified	*
 *					  0 = normal return		*
 *					 -6 = GVCORD is invalid		*
 *					-12 = no levels found		*
 *					-13 = @level not allowed	*
 *					-14 = %vcord not allowed	*
 *				        -19 = GFUNC is not valid	*
 **									*
 * Log:									*
 * S. Gilbert/NCEP       8/07   					*
 * S. Jacobs/NCEP	02/08	Set grid size as LLMXGD			*
 * B. Hebbard/NCEP	08/21	Changed strlen in protos int->size_t    *
 ***********************************************************************/
{
    int   kx, ky, kxy, ier;
    float *grid;
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 *  Allocate work array
 */
    kxy = LLMXGD;
    G_MALLOC ( grid, float, kxy, "gdxdta - grid" );

    gdxdtaf  ( iflno, gdatim, gvcord, ystrt, ystop, gfunc, time,
		ivcord, rgx, rgy, nhxs, grid, rlvl, xgrd, nvxs,
		parm, ybeg, yend, iret, gdatimsize, gvcordsize,
		gfuncsize, timesize, parmsize );

/*
 *  Free work array
 */
    G_FREE ( grid, float );

}
