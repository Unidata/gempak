#include "geminc.h"
#include "gemprm.h"

void gdedatf ( int *iproc, int *lun, int *iflno, int *ipktyp, 
               int *nbits, int *kx, int *ky, float *grid, int *iret );

void gdedat (  int *iproc, int *lun, int *iflno, int *ipktyp,
               int *nbits, int *kx, int *ky, int *iret );

void gdedat (  int *iproc, int *lun, int *iflno, int *ipktyp,
               int *nbits, int *kx, int *ky, int *iret )
/************************************************************************
 * gdedat                                                               *
 *                                                                      *
 * This function is a wrapper for subroutine GDEDATF.  It's sole        *
 * purpose is to dynamically allocate a work array, and then pass       *
 * it and all of this function's arguments on to GDEDATF.               *
 * The work array is freed from memory as soon as GDEDATF returns.      *
 *                                                                      *
 *  gdedat ( iproc, lun, iflno, ipktyp, nbits, kx, ky, iret )           * 
 *                                                                      *
 * Input parameters:                                                    *
 *      IPROC           INTEGER         Process flag                    *
 *      LUN             INTEGER         Logical unit number             *
 *      IFLNO           INTEGER         File access number              *
 *      IPKTYP          INTEGER         GEMPAK packing type             *
 *      NBITS           INTEGER         Number of bits                  *
 *      KX              INTEGER         Number of points in x dir       *
 *      KY              INTEGER         Number of points in y dir       *
 *                                                                      *
 * Output parameters:                                                   *
 *      IRET            INTEGER         Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		11/07                                           *
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    int   kxy, ier;
    float *grid;
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 *  Allocate work array
 */
    kxy = (*kx) * (*ky);
    G_MALLOC ( grid, float, kxy, "gdedat - grid" );

    gdedatf ( iproc, lun, iflno, ipktyp, nbits, kx, ky, grid, iret ); 

/*
 *  Free work array
 */
    G_FREE ( grid, float );
}
