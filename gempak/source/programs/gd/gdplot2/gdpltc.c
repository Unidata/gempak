#include        "geminc.h"
#include        "gemprm.h"

void gdpltc     ( int *iframe, int *ibang, int *exist, int *pltmap,
                  int *ix1, int *iy1, int *ix2, int *iy2, int *idrpfl, 
                  int *lindef, char *time, char *parm, int *level, 
                  int *scavld, int *vctvld, int *ivcord, int *iscale, 
                  char *gdpfun, int *iret, size_t time_size, size_t parm_size,
                  size_t gdpfun_size );
void gdpltcf    ( int *iframe, int *ibang, int *kxy, int *exist, int *pltmap,
                  int *ix1, int *iy1, int *ix2, int *iy2, int *idrpfl, 
                  int *lindef, float *grid, float *gridu, float *gridv,
                  float *subgrd, float *sped, float *drct, char *time, 
                  char *parm, int *level, 
                  int *scavld, int *vctvld, int *ivcord, int *iscale, 
                  char *gdpfun, int *iret, size_t time_size, size_t parm_size,
                  size_t gdpfun_size );

void gdpltc     ( int *iframe, int *ibang, int *exist, int *pltmap,
                  int *ix1, int *iy1, int *ix2, int *iy2, int *idrpfl, 
                  int *lindef, char *time, char *parm, int *level, 
                  int *scavld, int *vctvld, int *ivcord, int *iscale, 
                  char *gdpfun, int *iret, size_t time_size, size_t parm_size,
                  size_t gdpfun_size )
/************************************************************************
 * gdpltc								*
 *									*
 * This function is specifically used a bridge between fortran          *
 * subroutines GDPLTB and GDPLTCF.  It's sole purpose is to dynamically *
 * allocate six work arrays, and then pass them and all of this         *
 * function's arguments on to GDPLTCF.  				*
 * The work arrays are freed from memory as soon as GDPLTCF returns.	*
 *									*
 * gdpltc  ( iframe, ibang, exist, pltmap, ix1, iy1, ix2, iy2,          *
 *           idrpfl, lindef, time, parm, level, scavld, vctvld,         *
 *           ivcord, iscale, gdpfun, iret )				*
 *									*
 * Input parameters:							*
 *	IFRAME		INTEGER		Frame number			*
 *	IBANG		INTEGER		Bang number for parameters	*
 *	EXIST		LOGICAL		Do SAT or RAD image file exist? *
 *	PLTMAP		LOGICAL		Flag to plot maps for each bang *
 *	IX1		INTEGER		Lower left map I bound		*
 *	IY1		INTEGER		Lower left map J bound		*
 *	IX2		INTEGER		Upper right map I bound		*
 *	IY2		INTEGER		Upper right map J bound		*
 *	IDRPFL		INTEGER		Flag to drop the image		*
 *									*
 * Input/Output parameters:						*
 *	LINDEF		INTEGER		line number used for title	*
 *									*
 * Output parameters:							*
 *      TIME            CHARACTER*	Output date/time		*
 *      PARM            CHARACTER*	Output parameter name		*
 *      LEVEL           INTEGER		Output level			*
 *	SCAVLD		LOGICAL		Flag for scaler field		*
 *	VCTVLD		LOGICAL		Flag for vector field		*
 *      IVCORD		INTEGER		Output vertical coordinate	*
 *      ISCALE		INTEGER		Scaling factor			*
 *      GDPFUN          CHARACTER*	Diagnostic function		*
 *	IRET		INTEGER		Return code			*
 *					  0 = normal return		*
 **									*
 * Log:									*
 * S.Gilbert/NCEP	07/07		New				*
 * S.Gilbert/NCEP	07/07		Added dg_kxky query		*
 * T. Piper/SAIC	03/08	Replaced cmm functions with Macros	*
 ***********************************************************************/
{
    int   ier, iperr;
    float *grid, *gridu, *gridv, *subgrd, *sped, *drct;
    int   kx, ky, kxy, maxkxky;
/*---------------------------------------------------------------------*/
/*
 *  Get grid dimensions
 */
    dg_kxky ( &kx, &ky, &ier );
    kxy = kx * ky;

/*
 *  Allocate work arrays
 */
    G_MALLOC ( grid,   float, kxy, "gdpltc - grid" );
    G_MALLOC ( gridu,  float, kxy, "gdpltc - gridu" );
    G_MALLOC ( gridv,  float, kxy, "gdpltc - gridv" );
    G_MALLOC ( subgrd, float, kxy, "gdpltc - subgrd" );
    G_MALLOC ( sped,   float, kxy, "gdpltc - sped" );
    G_MALLOC ( drct,   float, kxy, "gdpltc - drct" );
 
    if ( grid == NULL || gridu == NULL || gridv == NULL ||
	 subgrd == NULL || sped == NULL || drct == NULL ) {
          iperr = -73;
          er_wmsg ( "DG", &iperr, " ", &ier, strlen("DG"), strlen(" ") );
          return;
    }

/*
 *  Pass arguments on to GDPLTCF
 */
    maxkxky = (kx > ky ) ? kx: ky;
    gdpltcf ( iframe, ibang, &maxkxky, exist, pltmap,
               ix1, iy1, ix2, iy2, idrpfl, lindef,
               grid, gridu, gridv, subgrd, sped, drct,
               time, parm, level, scavld, vctvld,
               ivcord, iscale, gdpfun, iret, time_size, parm_size,
               gdpfun_size );

/*
 *  Free work arrays
 */
    G_FREE ( grid, float );
    G_FREE ( gridu, float );
    G_FREE ( gridv, float );
    G_FREE ( subgrd, float );
    G_FREE ( sped, float );
    G_FREE ( drct, float );

}
