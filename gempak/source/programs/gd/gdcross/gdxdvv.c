#include "geminc.h"
#include "gemprm.h"

void gdxdvvf ( int *iflno, char *gdatim, char *gvcord, float *ystrt,
               float *ystop, char *gvecx, char *time, int *ivcord, 
               float *rgx, float *rgy, int *nhxs, float *gridu, float *gridv, 
               float *rlvl, float *ugrd, 
               float *vgrd, float *ponth, int *nvxs, char *parmu, char *parmv, 
               int *lvert, int *lscal, int *iret, size_t gdatimsize, 
               size_t gvcordsize, size_t gvecxsize, size_t timesize, size_t parmusize, 
               size_t parmvsize );

void gdxdvv  ( int *iflno, char *gdatim, char *gvcord, float *ystrt,
               float *ystop, char *gvecx, char *time, int *ivcord, 
               float *rgx, float *rgy, int *nhxs, float *rlvl, float *ugrd, 
               float *vgrd, float *ponth, int *nvxs, char *parmu, char *parmv, 
               int *lvert, int *lscal, int *iret, size_t gdatimsize, 
               size_t gvcordsize, size_t gvecxsize, size_t timesize, size_t parmusize, 
               size_t parmvsize );

void gdxdvv  ( int *iflno, char *gdatim, char *gvcord, float *ystrt,
               float *ystop, char *gvecx, char *time, int *ivcord, 
               float *rgx, float *rgy, int *nhxs, float *rlvl, float *ugrd, 
               float *vgrd, float *ponth, int *nvxs, char *parmu, char *parmv, 
               int *lvert, int *lscal, int *iret, size_t gdatimsize, 
               size_t gvcordsize, size_t gvecxsize, size_t timesize, size_t parmusize, 
               size_t parmvsize )
/************************************************************************
 * gdxdvv								*
 *									*
 * This function is a wrapper for subroutine GDXDVVF.  It's sole        *
 * purpose is to dynamically allocate two work arrays, and then pass    *
 * them and all of this function's arguments on to GDXDVVF.             *
 * The work arrays are freed from memory as soon as GDXDVVF returns.    *
 *									*
 * gdxdvv  ( iflno, gdatim, gvcord, ystrt, ystop, gvecx, time, ivcord,	*
 *           rgx, rgy, nhxs, rlvl, ugrd, vgrd, ponth, nvxs,		*
 *	     parmu, parmv, lvert, lscal, iret )				*
 *									*
 * Input parameters:							*
 *      IFLNO             INTEGER       Grid file number                *
 *	GDATIM		  CHAR*		User input date/time		*
 *	GVCORD		  CHAR*		User input vert coord		*
 *      YSTRT             REAL          Bottom vert coord value		*
 *      YSTOP             REAL          Top vert coord value		*
 *	GVECX	 	  CHAR*		User input function		*
 *	TIME  (2)	  CHAR*		Time to search for levels	*
 *	IVCORD		  INTEGER	Vertical coordinate for search	*
 *	RGX  (NHXS)	  REAL		X grid coordinates		*
 *	RGY  (NHXS)	  REAL		Y grid coordinates		*
 *	NHXS		  INTEGER	Number of xsect pts in horiz.	*
 *									*
 * Output parameters:							*
 *      RLVL (NVXS)	  REAL		Vertical levels in grid		*
 *	UGRD (NHXS, NVXS) REAL		Array of xsect u components	*
 *	VGRD (NHXS, NVXS) REAL		Array of xsect v components	*
 *	PONTH(NHXS, NVXS) REAL		Array of p on theta		*
 *      NVXS              INTEGER       Number of xsect pts in vert.	*
 *	PARMU		  CHAR*		Parameter name of u grid	*
 * 	PARMV		  CHAR*		Parameter name of v grid	*
 *	LVERT		  LOGICAL	Flag for vertical circ comp	*
 *	LSCAL		  LOGICAL	Flag for scaled vert circ comp	*
 *	IRET		  INTEGER	Return code			*
 *					  7 = GVECT not specified	*
 *					  0 = normal return		*
 *					 -6 = invalid vertical coord.	*
 *					-12 = no levels found		*
 *					-13 = @level not allowed	*
 *					-14 = %vcord not allowed	*
 *					-18 = GVECX not valid		*
 **									*
 * Log:									*
 * S. Gilbert/NCEP       8/07   					*
 * S. Jacobs/NCEP	02/08	Set grid size as LLMXGD			*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 * B. Hebbard/NCEP	08/21	Changed strlen in protos int->size_t    *
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
    G_MALLOC ( gridu, float, kxy, "gdxdvv - gridu" );
    G_MALLOC ( gridv, float, kxy, "gdxdvv - gridv" );

    gdxdvvf ( iflno, gdatim, gvcord, ystrt, ystop, gvecx, time,
		ivcord, rgx, rgy, nhxs, gridu, gridv, rlvl, ugrd,
		vgrd, ponth, nvxs, parmu, parmv, lvert, lscal,
		iret, gdatimsize, gvcordsize, gvecxsize, timesize,
		parmusize, parmvsize );

/*
 *    Free work array
 */
    G_FREE ( gridu, float );
    G_FREE ( gridv, float );

}
