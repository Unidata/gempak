#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define dgconav	dgconav_
#endif

void	dgconav ( float *rnvblk, int *iret )
{
	dg_onav ( (const float *)rnvblk, iret );
}
