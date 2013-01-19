#include "geminc.h"
#include "gemprm.h"

void gstrmlf(const int *kx, const int *ky, float *u, float *v, const int *iminx,
              const int *jminy, const int *imaxx, const int *jmaxy,
     	      int *misflg, float *filtst, float *filtar, float *ststop,
     	      float *dispc, float *displ, float *ux, float*vy, 
              int *flag1, int*flag2, float*coslat, float *gelon, int *iret );

void gstrml (const int *kx, const int *ky, float *u, float *v, const int *iminx,
               const int *jminy, const int *imaxx, const int *jmaxy,
     	       int *misflg, float *filtst, float *filtar, float *ststop,
     	       float *dispc, float *displ, int *iret )
/************************************************************************
 * gstrml								*
 * 									*
 * This function is a wrapper for subroutine GSTRMLF.  It's sole        *
 * purpose is to dynamically allocate six work arrays, and then pass    *
 * them and all of this function's arguments on to GSTRMLF.             *
 * The work arrays are freed from memory as soon as GSTRMLF returns.    *
 *									*
 * void gstrml  ( int *kx, int *ky, float *u, float *v, int *iminx, 	*
 *              int *jminy, int *imaxx, int *jmaxy,			*
 *    	       int *misflg, float *filtst, float *filtar, float *ststop,*
 *    	       float *dispc, float *displ, int *iret )			*
 * 									*
 * Input parameters:							*
 * 	KX		INTEGER		Number of x grid points 	*
 * 	KY		INTEGER		Number of y grid points 	*
 * 	U (KX,KY)	REAL		U - component grid		*
 * 	V (KX,KY)	REAL		V - component grid		*
 *	IMINX		INTEGER		First x point of subgrid	*
 * 	JMINY		INTEGER		First y point of subgrid	*
 *	IMAXX		INTEGER		Last  x point of subgrid	*
 *	JMAXY		INTEGER		Last  y point of subgrid	*
 * 	MISFLG 		LOGICAL		Interpolate missing data flag	*
 *      FILTST          REAL            Filter to thin strmlines        *
 *      FILTAR          REAL            Filter to thin strmline arrows  *
 *      STSTOP          REAL            Controls stopping of strmline   *
 *                                              near another strmline   *
 *      DISPC           REAL            Controls stopping of strmline   *
 *                                              when wind speed is small*
 *      DISPL           REAL            Controls pre-scaling of vectos  *
 * 									*
 * Output parameters:							*
 * 	IRET		INTEGER		Return code			*
 **									*
 * Log:									*
 * S. Gilbert/NCEP	 3/07	Wrapper to GSTRMLF			*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
     int       kxy;
     int      *flag1, *flag2;
     float    *ux, *vy, *coslat, *gelon;
     int      ier, iperr;
/*---------------------------------------------------------------------*/
/*
 *  Allocate work arrays
 */
    kxy = (*kx) * (*ky);

    G_MALLOC ( ux, float, kxy, "gstrml - ux");
    G_MALLOC ( vy, float, kxy, "gstrml - vy");
    G_MALLOC ( flag1, int, kxy, "gstrml - flag1");
    G_MALLOC ( flag2, int, kxy, "gstrml - flag2");
    G_MALLOC ( coslat, float, kxy, "gstrml - coslat");
    G_MALLOC ( gelon, float, kxy, "gstrml - gelon");
 
    if ( ux == NULL || vy == NULL || flag1 == NULL || 
	 flag2 == NULL || coslat == NULL || gelon == NULL ) {
         iperr = -3;
         er_wmsg ( "GU", &iperr, " ", &ier, strlen("GU"), strlen(" ") );
         return;
    }

    gstrmlf  ( kx, ky, u, v, iminx, jminy, imaxx, jmaxy,
               misflg, filtst, filtar, ststop,
               dispc, displ, ux, vy, flag1, flag2,
               coslat, gelon, iret );

/*
 *  Free work arrays
 */
    G_FREE ( ux, float );
    G_FREE ( vy, float );
    G_FREE ( flag1, int );
    G_FREE ( flag2, int );
    G_FREE ( coslat, float );
    G_FREE ( gelon, float );

}
