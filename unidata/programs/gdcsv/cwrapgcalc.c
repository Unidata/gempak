#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define cwrap_gcalc cwrap_gcalc_
#define cwrap_gcalcf cwrap_gcalcf_
#endif

void cwrap_gcalcf ( char *timfnd, char *glevel, char *gvcord, char *gfunc,
		float *grid, int *ksubx, int *ksuby, char *scale, int *nlun, int *luns, 
		char *devs, int *iret, int size_timfnd, int size_glevel,
                int size_gvcord, int size_gfunc, int size_scale, int size_devs);

void cwrap_gcalc ( char *timfnd, char *glevel, char *gvcord, 
		char *gfunc, int *ksubx, int *ksuby, char *scale, int *nlun, 
		int *luns, char *devs, int *iret, 
		int size_timfnd, int size_glevel, 
		int size_gvcord, int size_gfunc, int size_scale, int size_devs);

void cwrap_gcalc ( char *timfnd, char *glevel, char *gvcord, 
		char *gfunc, int *ksubx, int *ksuby, char *scale, int *nlun, 
		int *luns, char *devs, int *iret, 
		int size_timfnd, int size_glevel, 
		int size_gvcord, int size_gfunc, int size_scale, int size_devs)
{
int kx, ky, kxy, ier;
float *grid;

    *iret = 0;

    dg_kxky ( &kx, &ky, &ier );
    if ( ksubx[0] == IMISSD ) {
	ksubx[0] = 1;
	ksubx[1] = kx;
	ksuby[0] = 1;
	ksuby[1] = ky;
    }
/*
 *    Allocate work array
 */
    kxy = kx * ky;
    grid   = (float *)cmm_malloc1d ( kxy, sizeof(float), &ier );

    cwrap_gcalcf ( timfnd, glevel, gvcord, gfunc, grid, ksubx, ksuby, 
		   scale, nlun, luns, devs, iret, size_timfnd, size_glevel,
                   size_gvcord, size_gfunc, size_scale, size_devs);

/*
 *    Free work array
 */
    cmm_free1d ( (void *)grid, &ier );

}
