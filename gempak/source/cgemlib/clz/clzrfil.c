#include <geminc.h>
#include <gemprm.h>

#ifdef UNDERSCORE
#define clz_rfil	clz_rfil_
#endif

void clz_read ( FILE *fp, int nblocks, long zbytes, int imgsz, unsigned char *data,
                int *ibin, int *ibout, int *iret );

void	clz_rfil ( char *filnam, char *defdir, int *nblocks, int *ioff, int *obytes,
			unsigned char *data, int *ibin, int *ibout, int *iret)
{
int ier;
long flen, nzbytes;
FILE 	*fp;
char newfil[256];

cfl_inqr ( filnam, defdir, &flen, newfil, iret );
if (*iret != 0) return;

fp = cfl_ropn ( filnam, defdir, iret );
if ( *iret != 0 ) return;

cfl_seek ( fp, (long)( *ioff), SEEK_SET, &ier );

nzbytes = flen - (long)(*ioff);
clz_read( fp, *nblocks, nzbytes, *obytes, data, ibin, ibout, iret );

cfl_clos ( fp, &ier );

}
