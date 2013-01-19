#include "geminc.h"
#include "gemprm.h"

void gdpdtaf ( int *iflno, char *gdatim, char *gvcord, char *gfunc, 
               char *gpoint, char *time, int *ivcord, float *ystrt,
               float *ystop, float *grid, float *rgx, float *rgy, float *rlat,
               float *rlon, int *npts, float *x, float *y, char *parm, 
               int *iret, int gdatimsize, int gvcordsize, int gfuncsize,
               int gpointsize, int timesize, int parmsize );

void gdpdta  ( int *iflno, char *gdatim, char *gvcord, char *gfunc, 
               char *gpoint, char *time, int *ivcord, float *ystrt,
               float *ystop, float *rgx, float *rgy, float *rlat,
               float *rlon, int *npts, float *x, float *y, char *parm, 
               int *iret, int gdatimsize, int gvcordsize, int gfuncsize,
               int gpointsize, int timesize, int parmsize );

void gdpdta  ( int *iflno, char *gdatim, char *gvcord, char *gfunc, 
               char *gpoint, char *time, int *ivcord, float *ystrt,
               float *ystop, float *rgx, float *rgy, float *rlat,
               float *rlon, int *npts, float *x, float *y, char *parm, 
               int *iret, int gdatimsize, int gvcordsize, int gfuncsize,
               int gpointsize, int timesize, int parmsize )
/************************************************************************
 * gdpdta								*
 *									*
 * This function is a wrapper for subroutine GDPDTAF.  It's sole        *
 * purpose is to dynamically allocate a work array, and then pass 	*
 * it and all of this function's arguments on to GDPDTAF.		*
 * The work array is freed from memory as soon as GDPDTAF returns.      *
 *									*
 * GDPDTA  ( IFLNO, GDATIM, GVCORD, GFUNC, GPOINT, TIME, IVCORD,	*
 *           YSTRT, YSTOP, RGX, RGY, RLAT, RLON, NPTS, X, Y, PARM, 	*
 *	     IRET )							*
 *									*
 * Input parameters:							*
 *      IFLNO           INTEGER         Grid file number                *
 *	GDATIM		CHAR*		User input date/time		*
 *	GVCORD		CHAR*		User input vert coord		*
 *	GFUNC		CHAR*		User input function		*
 *	GPOINT		CHAR*		User input point to plot	*
 *	TIME  (2)	CHAR*		Time to search for levels	*
 *	IVCORD		INTEGER		Vertical coordinate for search	*
 *      YSTRT	 	REAL            Starting vert coord value	*
 *      YSTOP           REAL            Stopping vert coord value       *
 *									*
 * Output parameters:							*
 *	RGX		REAL		X grid coordinate		*
 *	RGY		REAL		Y grid coordinate		*
 *	RLAT		REAL		Latitude			*
 *	RLON		REAL		Longitude			*
 *	NPTS		INTEGER		Number of points		*
 *	X    (NPTS)	REAL		X coordinates			*
 *	Y    (NPTS)	REAL		Y coordinates			*
 *	PARM		CHAR*		Parameter name			*
 *	IRET		INTEGER		Return code			*
 *					  0 = normal return		*
 *					 -4 = invalid grid point	*
 *					 -9 = no valid points		*
 *					-10 = no levels at this time	*
 **									*
 * Log:									*
 * S. Gilbert/NCEP	08/07						*
 * S. Jacobs/NCEP	02/08	Set grid size as LLMXGD			*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{

     int   kx, ky, kxy, ier;
     float     *grid;
/*---------------------------------------------------------------------*/
     *iret = 0;

/*
 *    Allocate work array
 */
     kxy = LLMXGD;
     G_MALLOC ( grid, float, kxy, "gdpdta - grid");

     gdpdtaf ( iflno, gdatim, gvcord, gfunc, 
               gpoint, time, ivcord, ystrt,
               ystop, grid, rgx, rgy, rlat,
               rlon, npts, x, y, parm, 
               iret, gdatimsize, gvcordsize, gfuncsize,
               gpointsize, timesize, parmsize );

/*
 *    Free work array
 */
    G_FREE ( grid, float );
}
