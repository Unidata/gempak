#include "geminc.h"
#include "gemprm.h"

void gdzdatf ( int *iproc, int *ipos, char *timfnd, char *gdfile, 
               char *glevel, char *gvcord, char *gfunc, char *scale, 
               char *sffile, char *sfp, float *grid, int *isffln, 
               int *iret, int timfndsize, int gdfilesize, 
               int glevelsize, int gvcordsize, int gfuncsize,
               int scalesize, int sffilesize, int sfpsize );

void gdzdat ( int *iproc, int *ipos, char *timfnd, char *gdfile, 
               char *glevel, char *gvcord, char *gfunc, char *scale, 
               char *sffile, char *sfp, int *isffln, int *iret,
               int timfndsize, int gdfilesize,  
               int glevelsize, int gvcordsize, int gfuncsize, 
               int scalesize, int sffilesize, int sfpsize  );

void gdzdat ( int *iproc, int *ipos, char *timfnd, char *gdfile, 
               char *glevel, char *gvcord, char *gfunc, char *scale,
               char *sffile, char *sfp, int *isffln, int *iret, 
               int timfndsize, int gdfilesize,
               int glevelsize, int gvcordsize, int gfuncsize,
               int scalesize, int sffilesize, int sfpsize )
/************************************************************************
 * gdzdat                                                               *
 *                                                                      *
 * This function is a wrapper for subroutine GDZDATF.  It's sole        *
 * purpose is to dynamically allocate a work array, and then pass       *
 * it and all of this function's arguments on to GDZDATF.               *
 * The work array is freed from memory as soon as GDZDATF returns.      *
 *                                                                      *
 *  gdzdat ( iproc, ipos, timfnd, gdfile, glevel, gvcord, gfunc, scale, *      
 *           sffile, sfp, isffln, iret )                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      IPROC           INTEGER         Process flag                    *
 *      IPOS            INTEGER         Position of string in list      *
 *      TIMFND          CHAR*           Range of grid times             *
 *      GDFILE          CHAR*           Grid file                       *
 *      GLEVEL          CHAR*           Grid level                      *
 *      GVCORD          CHAR*           Vertical coordinate             *
 *      GFUNC           CHAR*           Grid function                   *
 *      SCALE           CHAR*           Scaling factor                  *
 *      SFFILE          CHAR*           Surface file                    *
 *      SFP             CHAR*           Surface parameter               *
 *      ISFFLN          INTEGER         Surface file number             *
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
    int   kx, ky, kxy, ier;
    float *grid;
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 *  Query grid size
 */
    dg_kxky ( &kx, &ky, &ier );

/*
 *  Allocate work array
 */
    kxy = kx * ky;
    G_MALLOC ( grid, float, kxy, "gdzdat - grid" );

    gdzdatf ( iproc, ipos, timfnd, gdfile, glevel, gvcord, gfunc, 
              scale, sffile, sfp, grid, isffln, iret,
              timfndsize, gdfilesize, glevelsize, gvcordsize, gfuncsize,
              scalesize, sffilesize, sfpsize ); 

/*
 *  Free work array
 */
    G_FREE ( grid, float );
}
