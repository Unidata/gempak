#include "geminc.h"
#include "gemprm.h"

void gcfillf ( int *kx, int *ky, float *grid, int *ioffx, int *ioffy, 
               int *iskip, int *nlvl, float *clvl, int *icolr, int *linlbl, 
               int *lintyp, float *zgrid, int *iret );

void gcfill  ( int *kx, int *ky, float *grid, int *ioffx, int *ioffy, 
               int *iskip, int *nlvl, float *clvl, int *icolr, int *linlbl, 
               int *lintyp, int *iret )
/************************************************************************
 * gcfill								*
 * 									*
 * This function is a wrapper for subroutine GCFILLF.  It's sole        *
 * purpose is to dynamically allocate a work array, and then pass  	*
 * it and all of this function's arguments on to GCFILLF.               *
 * The work arrays are freed from memory as soon as GCFILLF returns.    *
 * 									*
 * void gcfill  ( int *kx, int *ky, float *grid, int *ioffx, int *ioffy,*
 *                int *iskip, int *nlvl, float *clvl, int *icolr,       *
 *                int *linlbl, int *lintyp, int *iret )			*
 *									*
 * Input parameters:							*
 *									*
 *	KX		INTEGER		Number of x grid points 	*
 *	KY		INTEGER		Number of y grid points		*
 *	GRID (KX,KY)	REAL		Grid data array			*
 *	IOFFX		INTEGER		X offset to first point		*
 *	IOFFY		INTEGER		Y offset to first point		*
 *	ISKIP		INTEGER		Skip factor in original grid	*
 *	NLVL		INTEGER		Number of contour levels	*
 *	CLVL   (NLVL)	REAL		Contour level values		*
 *	ICOLR  (NLVL+1)	INTEGER		Contour color numbers		*
 *	LINLBL (NLVL+1)	INTEGER		Contour label types		*
 *	LINTYP (NLVL+1)	INTEGER		Contour fill types		*
 *									*
 * Output parameters:							*
 *									*
 * 	IRET		INTEGER		Return code			*
 **									*
 * Log:									*
 * S. Gilbert/NCEP      03/07   C wrapper for gcfillf                   *
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    int   kxy;
    float *zgrid;
    int   ier, iperr;
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 *  Allocate work arrays
 */
    kxy = (*kx) * (*ky);

    G_MALLOC ( zgrid, float, kxy, "gcfill - zgrid"); 
    if ( zgrid == NULL ) {
         iperr = -3;
         er_wmsg ( "GU", &iperr, " ", &ier, strlen("GU"), strlen(" ") );
         return;
    }

    gcfillf  ( kx, ky, grid, ioffx, ioffy, iskip, nlvl, 
               clvl, icolr, linlbl, lintyp, zgrid, iret );

/*
 *  Free work arrays
 */
    G_FREE ( zgrid, float );

}
