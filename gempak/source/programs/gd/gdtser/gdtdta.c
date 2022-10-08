#include "geminc.h"
#include "gemprm.h"

void gdtdtaf ( char *glevel, char *gvcord, char *gfunc,
               char *gpoint, float * grid, int *npts, char *timfnd, float *rgx,
               float *rgy,    float *rlat,   float *rlon,  float *x, float *y,
               char *parm,   int *level,  int *ivcord, int *iret,
               size_t glevelsize, size_t gvcordsize, 
	       size_t gfuncsize,  size_t gpointsize,
               size_t timfndsize, size_t parmsize );

void gdtdta  ( char *glevel, char *gvcord, char *gfunc,
               char *gpoint, int *npts,   char *timfnd, float *rgx,
               float *rgy,    float *rlat,   float *rlon,  float *x, float *y,
               char *parm,   int *level,  int *ivcord, int *iret,
               size_t glevelsize, size_t gvcordsize,
	       size_t gfuncsize,  size_t gpointsize,
               size_t timfndsize, size_t parmsize );

void gdtdta  ( char *glevel, char *gvcord, char *gfunc,
               char *gpoint, int *npts,   char *timfnd, float *rgx,
               float *rgy,    float *rlat,   float *rlon,  float *x, float *y,
               char *parm,   int *level,  int *ivcord, int *iret,
               size_t glevelsize, size_t gvcordsize, 
	       size_t gfuncsize,  size_t gpointsize,
               size_t timfndsize, size_t parmsize )
/************************************************************************
 * gdtdta								*
 *									*
 * This function is a wrapper for subroutine GDTDTAF.  It's sole        *
 * purpose is to dynamically allocate a work array, and then pass       *
 * it and all of this function's arguments on to GDTDTAF.               *
 * The work array is freed from memory as soon as GDTDTAF returns.      *
 *									*
 * gdtdta  ( glevel, gvcord, gfunc, gpoint, npts, timfnd,               *
 *           rgx,    rgy,    rlat,  rlon,   x,      y,    parm,         *
 *           level,  ivcord, iret )           				*
 *									*
 * Input parameters:							*
 *	GLEVEL		CHAR*		User input level		*
 *	GVCORD		CHAR*		User input vert coord		*
 *	GFUNC		CHAR*		User input function		*
 *	GPOINT		CHAR*		User input point to plot	*
 *									*
 * Output parameters:							*
 *	NPTS		INTEGER		Number of points		*
 *      TIMFND (NPTS)   CHAR*           Time values                     *
 *	RGX		REAL		X grid coordinate		*
 *	RGY		REAL		Y grid coordinate		*
 *	RLAT		REAL		Latitude			*
 *	RLON		REAL		Longitude			*
 *	X      (NPTS)	REAL		X coordinates			*
 *	Y      (NPTS)	REAL		Y coordinates			*
 *	PARM		CHAR*		Parameter name			*
 *	LEVEL  (2)	INTEGER		Level for search		*
 *	IVCORD		INTEGER		Vertical coordinate for search	*
 *	IRET		INTEGER		Return code			*
 *					 +1 = points missing for ... 	*
 *					  0 = normal return		*
 *					 -4 = invalid grid point	*
 *					 -9 = no valid points for ...	*
 **									*
 * Log:									*
 * S. Gilbert/NCEP	 8/07						*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 * B. Hebbard/NCEP	08/21	Changed strlen in protos int->size_t    *
 ***********************************************************************/
{
    int   kxy, ier;
    int   iacss=1, navsz;
    float *grid, rnvblk[LLNNAV];
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 *  Query grid size
 */
    gd_gnav( &iacss, rnvblk, &navsz, &ier );

/*
 *  Allocate work array
 */
    kxy = (int)rnvblk[4] * (int)rnvblk[5];
    G_MALLOC ( grid, float, kxy, "gdtdta - grid");

    gdtdtaf ( glevel, gvcord, gfunc,
              gpoint,  grid, npts, timfnd, rgx,
              rgy,    rlat,   rlon,  x, y,
              parm,   level,  ivcord, iret,
              glevelsize, gvcordsize, gfuncsize, gpointsize,
              timfndsize, parmsize );

/*
 *  Free work array
 */
    G_FREE ( grid, float );

}
