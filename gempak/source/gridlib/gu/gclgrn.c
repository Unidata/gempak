#include "geminc.h"
#include "gemprm.h"

void gclgrnf ( int *kx, int *ky, float *grid, int *ioffx, int *ioffy, 
               int *iskip, int *nlvl, float *clvl, char *clbl, 
               int *icolr, int *lintyp, int *linwid, int *linlbl, 
     	       int *scflag, float *zgrid, int *jhline, int*jvline,
               int *iret, size_t strsize );

void gclgrn  ( int *kx, int *ky, float *grid, int *ioffx, int *ioffy, 
               int *iskip, int *nlvl, float *clvl, char *clbl, 
               int *icolr, int *lintyp, int *linwid, int *linlbl, 
     	       int *scflag, int *iret, size_t strsize )
/************************************************************************
 * gclgrn								*
 * 									*
 * This function is a wrapper for subroutine GCLGRNF.  It's sole 	*
 * purpose is to dynamically allocate three work arrays, and then pass 	*
 * them and all of this function's arguments on to GCLGRNF.             *
 * The work arrays are freed from memory as soon as GCLGRNF returns.    *
 * 									*
 * gclgrn  ( kx, ky, grid, ioffx, ioffy, iskip, nlvl, clvl, clbl, 	*
 *           icolr, lintyp, linwid, linlbl, scflag, iret )		*
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
 *	CLBL   (NLVL)	CHAR*		Contour labels			*
 *	ICOLR  (NLVL)	INTEGER		Contour color numbers		*
 *	LINTYP (NLVL)	INTEGER		Contour line types		*
 *	LINWID (NLVL)	INTEGER 	Contour line widths		*
 *	LINLBL (NLVL)	INTEGER		Contour label types		*
 *	SCFLAG		LOGICAL		Small contour suppress flag	*
 *	STRSIZE		INTEGER		Fortran character len for CLBL  *
 *									*
 * Output parameters:							*
 *									*
 * 	IRET		INTEGER		Return code			*
 **									*
 * Log:									*
 * S. Gilbert/NCEP	 7/07						*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    int   kxy, *jhline, *jvline;
    float *zgrid;
    int   ier, iperr; 
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 *  Allocate work arrays
 */
    kxy = (*kx) * (*ky);

    G_MALLOC ( zgrid, float, kxy, "gclgrn - zgrid"); 
    G_MALLOC ( jhline, int,  kxy, "gclgrn - jhline");
    G_MALLOC ( jvline, int,  kxy, "gclgrn - jvline");
    if ( zgrid == NULL || jhline == NULL || jvline == NULL ) {
        iperr = -3;
        er_wmsg ( "GU", &iperr, " ", &ier, strlen("GU"), strlen(" ") );
        return;
    }

    gclgrnf ( kx, ky, grid, ioffx, ioffy, iskip, nlvl,
              clvl, clbl, icolr, lintyp, linwid, linlbl,
              scflag, zgrid, jhline, jvline, iret, strsize );

/*
 *  Free work arrays
 */
    G_FREE ( zgrid, float );
    G_FREE ( jhline, int );
    G_FREE ( jvline, int );

}
