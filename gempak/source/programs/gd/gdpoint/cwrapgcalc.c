#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define cwrap_gcalc cwrap_gcalc_
#define cwrap_gcalcf cwrap_gcalcf_
#endif

void cwrap_gcalcf ( char *timfnd, char *glevel, char *gvcord, char *gfunc,
                float *grid, int *kx, int *ky, char *scale, int *gintp, float *rgx, float *rgy, float *yy,
                int *iret, int size_timfnd, int size_glevel,
                int size_gvcord, int size_gfunc, int size_scale );

void cwrap_gcalc ( char *timfnd, char *glevel, char *gvcord,
		char *gfunc, char *scale, int *gintp, float *rgx, float *rgy,
		float *yy, int *iret, 
		int size_timfnd, int size_glevel,
                int size_gvcord, int size_gfunc, int size_scale);

void cwrap_gcalc ( char *timfnd, char *glevel, char *gvcord,
		char *gfunc, char *scale, int *gintp, float *rgx, float *rgy,
		float *yy, int *iret, 
		int size_timfnd, int size_glevel,
                int size_gvcord, int size_gfunc, int size_scale)
{
int kx, ky, kxy, ier;
float *grid;

    *iret = 0;

    dg_kxky ( &kx, &ky, &ier );
/*
 *    Allocate work array
 */
    kxy = kx * ky;
    grid   = (float *)cmm_malloc1d ( kxy, sizeof(float), &ier );

    cwrap_gcalcf ( timfnd, glevel, gvcord, gfunc, grid, &kx, &ky,
                   scale, gintp, rgx, rgy, yy, iret, size_timfnd, size_glevel,
                   size_gvcord, size_gfunc, size_scale );

/*
 *    Free work array
 */
    cmm_free1d ( (void *)grid, &ier );


}

