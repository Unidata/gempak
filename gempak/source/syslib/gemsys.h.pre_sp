/*  
    This include file contains constants used in the GEMPAK SYSLIB C
    subroutines, and the system include files.

									*/

#include "geminc.h"
#include "gemprm.h"

#define  NCHECK		3
				/* Number of times to check for subprocess */
#define  GEM_DEBUG	0
				/* Debug level				*/
#define  GEMPAK_MQ_G	12340000
				/* Prefix for APPL-GPLT queue key	*/
#define  GEMPAK_MQ_D	45670000
				/* Prefix for GPLT-DEVICE queue key	*/
#define  GEMPAK_MQ_T	78900000
				/* PREFIX for APPL-GPLT temporary queue	*/

#ifdef GLOBAL

   FILE *fdes[2];

#else

   extern FILE *fdes[2];

#endif

/*
 *  SYSLIB prototypes
 */

void ccheck (	int    *mproc,
        	int    *mbchan,
        	int    *iwact,
        	int    *iret );

void cendmq (	int    *mbchan,
        	int    *iret );

void cgetmq (	int    mproc,
        	int    idebug,
        	int    *jsatty,
        	int    *key,
        	int    *mbchan,
        	int    *iret );

void ciproc (	int     *mproc,
        	int     *mbchan,
        	int     *iret );

void crecv (	int   *itype,
        	int   *iwait,
        	int   *ichan,
        	int   idata [128],
        	int   *iret );

void csend (	int     *itype,
        	int     *ichan,
        	int     idata [128],
        	int     *nwords,
        	int     *iret );

void csproc (	int    *mproc,
        	char   *image,
        	int    *mbchan,
        	int    *iret );

void grecv (	int	*itype,
		int	*wait,
		int	*ichan,
		int	*idata,
		int	*iret );

void gsend (	int	*itype, 
		int 	*ichan, 
		int 	*idata, 
		int 	*nwords, 
		int 	*iret );

