#include "geminc.h"
#include "gemprm.h"

void gcboxxf ( const int *kx, const int *ky, const float *grid, 
               const int *ioffx, const int *ioffy, 
               const int *iskip, const int *nlvl, const float *clvl, 
               const int *icolr, const int *lintyp, 
               const int *linwid, const int *linlbl, float *zgrid, int *iret );

void gcboxx  ( const int *kx, const int *ky, const float *grid, 
               const int *ioffx, const int *ioffy, 
               const int *iskip, const int *nlvl, const float *clvl, 
               const int *icolr, const int *lintyp, 
               const int *linwid, const int *linlbl, int *iret )
/************************************************************************
 * gcboxx								*
 * 									*
 * This function is a wrapper for subroutine GCBOXXF.  It's sole        *
 * purpose is to dynamically allocate a work array, and then pass  	*
 * it and all of this function's arguments on to GCBOXXF.               *
 * The work arrays are freed from memory as soon as GCBOXXF returns.    *
 * 									*
 * void gcboxx  ( int *kx, int *ky, float *grid, int *ioffx, int *ioffy,*
 *                int *iskip, int *nlvl, float *clvl, int *icolr,       *
 *                int *lintyp,                                          *
 *                int *linwid, int *linlbl, int *iret )                 *
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
 *	LINTYP (NLVL+1)	INTEGER		Contour line types		*
 *	LINWID (NLVL+1)	INTEGER		Contour line widths		*
 *	LINLBL (NLVL+1)	INTEGER		Contour label types		*
 *									*
 * Output parameters:							*
 *									*
 * 	IRET		INTEGER		Return code			*
 **									*
 * Log:									*
 * S. Gilbert/NCEP      03/07   C wrapper for gcboxxf                   *
 * T. Piper/SAIC	03/08	Replace cmm functions with Macros	*
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

    G_MALLOC ( zgrid, float, kxy, "gcboxx - zgrid" ); 
    if ( zgrid == NULL ) {
	iperr = -3;
	er_wmsg ( "GU", &iperr, " ", &ier, strlen("GU"), strlen(" ") );
	return;
    }

    gcboxxf  ( kx, ky, grid, ioffx, ioffy, iskip, nlvl, 
	       clvl, icolr, lintyp, linwid, linlbl, 
	       zgrid, iret );

/*
 *  Free work arrays
 */
    G_FREE ( zgrid, float );

}
