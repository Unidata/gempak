#include "geminc.h"
#include "gemprm.h"

void gdstaaf ( int *kx, int *ky,
               char *glevel, char *gvcord, char *gfunc, char *grdnam,
               float *grid, float *sums, float *sumsq,
               float *gmax, float *gmin, float *cnt,
               int *iret,
               size_t glevelsize, size_t gvcordsize, size_t gfuncsize, size_t grdnamsize );

void gdstaa  ( int *kx, int *ky,
               char *glevel, char *gvcord, char *gfunc, char *grdnam,
               int *iret,
               size_t glevelsize, size_t gvcordsize, size_t gfuncsize, size_t grdnamsize );

void gdstaa  ( int *kx, int *ky,
               char *glevel, char *gvcord, char *gfunc, char *grdnam,
               int *iret,
               size_t glevelsize, size_t gvcordsize, size_t gfuncsize, size_t grdnamsize ) 

/************************************************************************
 * gdstaa  (GDSTAT allocation)                                         *
 *                                                                      *
 * This function is a wrapper for work subroutine GDSTAAF.  Its sole    *
 * purpose is to dynamically allocate work arrays, and then pass        *
 * them and all of this function's arguments on to GDSTAAF.             *
 * The work arrays are freed from memory as soon as GDSTAAF returns.    *
 *                                                                      *
 *   CALL GDSTAA ( KX, KY, GLEVEL, GVCORD, GFUNC, GRDNAM, IRET ) *
 *                                                                      *
 * Input parameters:                                                    *
 *      KX              INTEGER         Number of points in x dir       *
 *      KY              INTEGER         Number of points in y dir       *
 *      GLEVEL          CHAR*           Grid level parameter setting    *
 *      GVCORD          CHAR*           Grid vertical coordinate        *
 *      GFUNC           CHAR*           Scalar grid                     *
 *      GRDNAM          CHAR*           Grid parameter name             *
 *                                                                      *
 * Output parameters:                                                   *
 *      IRET            INTEGER         Return code                     *
 *                                                                      *
 * String length parameters (implicit in Fortran, explicit in C):       *
 *      GLEVELSIZE      INTEGER         Length of GLEVEL string         *
 *      GVCORDSIZE      INTEGER         Length of GVCORD string         *
 *      GFUNCSIZE       INTEGER         Length of GFUNC string          *
 *      GRDNAMSIZE      INTEGER         Length of GRDNAM string         *
 *                                                                      *
 * Working storage (allocated):                                         *
 *      GRID            REAL(*)         Grid values                     *
 *      SUMS            REAL(*)         Sums of grid values             *
 *      SUMSQ           REAL(*)         Sums of squares                 *
 *      GMAX            REAL(*)         Maxima                          *
 *      GMIN            REAL(*)         Minima                          *
 *      CNT             REAL(*)         Counts of valid values          *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Hebbard/SAIC	03/08   Initial					*
 * B. Hebbard/NCEP	08/21	Changed strlen in protos int->size_t    *
 ***********************************************************************/
{

     int    kxy;
     float  *grid, *sums, *sumsq, *gmax, *gmin, *cnt;
     *iret = 0;

/*
 *    Allocate work arrays
 */
     kxy = (*kx) * (*ky);
     G_MALLOC ( grid,  float,  kxy, "gdstat - grid"  );
     G_MALLOC ( sums,  float,  kxy, "gdstat - sums"  );
     G_MALLOC ( sumsq, float,  kxy, "gdstat - sumsq" );
     G_MALLOC ( gmax,  float,  kxy, "gdstat - gmax"  );
     G_MALLOC ( gmin,  float,  kxy, "gdstat - gmin"  );
     G_MALLOC ( cnt,   float,  kxy, "gdstat - cnt"   );

/*
 *    Call the routine we're wrapping
 */
     gdstaaf ( kx, ky, glevel, gvcord, gfunc, grdnam,
               grid, sums, sumsq, gmax, gmin, cnt,
               iret,
               glevelsize, gvcordsize, gfuncsize, grdnamsize ); 

/*
 *    Free work arrays
 */
     G_FREE ( grid,  float );
     G_FREE ( sums,  float );
     G_FREE ( sumsq, float );
     G_FREE ( gmax,  float );
     G_FREE ( gmin,  float );
     G_FREE ( cnt,   float );
}

