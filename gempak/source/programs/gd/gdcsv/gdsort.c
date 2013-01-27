#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define gdpout	gdpout_
#endif

typedef struct rvals {
	int i;
	float val;
	} rvals;

int _cmpfunc( );

void gdpout ( int *nval, float hgrd[], int *nsiz, int *iret )
{
int i;
*iret = 0;

qsort ( hgrd, (size_t)(*nsiz), sizeof(float), _cmpfunc );
for ( i = 0; i < *nval && i < *nsiz; i++ )
   printf("look %d %f\n",i,hgrd[*nsiz - 1 - i]);
return;
}

int _cmpfunc( float *val1, float *val2)
{

if ( *val1 < *val2 )
   return ( -1 );
else if ( *val1 > *val2 )
   return ( 1 );
else
   return ( 0 );

}
