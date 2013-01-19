#include "geminc.h"
#include "gemprm.h"

void gdpduvf ( int *iflno, char *gdatim, char *gvcord, char *gvect, 
               char *gpoint, char *time, int *ivcord, float *ystrt, 
               float *ystop, float *gridu, float *gridv, float *rgx, 
               float *rgy, float *rlat, float *rlon,
               int *npts, float *u, float *v, float *y, char *parmu, 
               char *parmv, int *iret, int gdatimsize, int gvcordsize, 
               int gvectsize, int gpointsize, int timesize, int parmusize, 
               int parmvsize );

void gdpduv  ( int *iflno, char *gdatim, char *gvcord, char *gvect, 
               char *gpoint, char *time, int *ivcord, float *ystrt, 
               float *ystop, float *rgx, float *rgy, float *rlat, float *rlon,
               int *npts, float *u, float *v, float *y, char *parmu, 
               char *parmv, int *iret, int gdatimsize, int gvcordsize, 
               int gvectsize, int gpointsize, int timesize, int parmusize, 
               int parmvsize );

void gdpduv  ( int *iflno, char *gdatim, char *gvcord, char *gvect, 
               char *gpoint, char *time, int *ivcord, float *ystrt, 
               float *ystop, float *rgx, float *rgy, float *rlat, float *rlon,
               int *npts, float *u, float *v, float *y, char *parmu, 
               char *parmv, int *iret, int gdatimsize, int gvcordsize, 
               int gvectsize, int gpointsize, int timesize, int parmusize, 
               int parmvsize )
/************************************************************************
 * gdpduv								*
 *									*
 * This function is a wrapper for subroutine GDPDUVF.  It's sole        *
 * purpose is to dynamically allocate two work arrays, and then pass    *
 * them and all of this function's arguments on to GDPDUVF.             *
 * The work arrays are freed from memory as soon as GDPDUVF returns.    *
 *									*
 * GDPDUV  ( IFLNO, GDATIM, GVCORD, GVECT, GPOINT, TIME, IVCORD,	*
 *           YSTRT, YSTOP, RGX, RGY, RLAT, RLON, NPTS, U, V, Y, PARMU,	*
 *	     PARMV, IRET )						*
 *									*
 * Input parameters:							*
 *	IFLNO		INTEGER		Grid file number		*
 *	GDATIM		CHAR*		User input date/time		*
 *	GVCORD		CHAR*		User input vert coord		*
 *	GVECT		CHAR*		User input function		*
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
 *	U    (NPTS)	REAL		u components 			*
 *      V    (NPTS)     REAL 		v components			*
 *	Y    (NPTS)	REAL		Y coordinates			*
 *	PARMU		CHAR*		Parameter name for u comp	*
 *      PARMV           CHAR*           Parameter name for v comp	*
 *	IRET		INTEGER		Return code			*
 *					  0 = normal return		*
 *					 -4 = invalid grid point	*
 *					 -9 = no valid points		*
 *					-10 = no levels at this time	*
 **									*
 * Log:									*
 * S. Gilbert/NCEP	08/07   					*
 * S. Jacobs/NCEP	02/08	Set grid size as LLMXGD			*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    int   kx, ky, kxy, ier;
    float *gridu, *gridv;
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 *  Allocate work arrays
 */
    kxy = LLMXGD;
    G_MALLOC ( gridu, float, kxy, "gdpduv - gridu");
    G_MALLOC ( gridv, float, kxy, "gdpduv - gridv");

    gdpduvf ( iflno, gdatim, gvcord, gvect, 
               gpoint, time, ivcord, ystrt, 
               ystop, gridu, gridv, rgx, rgy, rlat, rlon,
               npts, u, v, y, parmu, 
               parmv, iret, gdatimsize, gvcordsize, 
               gvectsize, gpointsize, timesize, parmusize, 
               parmvsize );

/*
 *  Free work array
 */
    G_FREE ( gridu, float );
    G_FREE ( gridv, float );

}
