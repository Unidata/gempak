/*  
    This include file contains constants used in the GEMPAK SYSLIB C
    subroutines, and the system include files.

									*/

#include "geminc.h"
#include "gemprm.h"
#include "sys/ipc.h"
#include "sys/sem.h"
#include "time.h"

#define  NCHECK		6
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
 *  SYSLIB unions
 */
union gemsemun {
  int val;              /* used for SETVAL only */
  struct semid_ds *buf; /* for IPC_STAT and IPC_SET */
  ushort *array;        /* used for GETALL and SETALL */
};

/*
 *  SYSLIB prototypes
 */

int cnsleep (  const struct timespec *intvl,
               struct timespec *rtn);

void csgrab (  int    *mbchan,
               int    *grab,
               int    *semid,
               int    *iret );

void csfree (  int    *mbchan,
               int    *free,
               int    *semid,
               int    *iret );

void ccheck (	int    *mproc,
        	int    *mbchan,
        	int    *iwact,
        	int    *iret );

void cendmq (	int    *mbchan,
        	int    *iret );

void cgetmq (	int    mproc,
        	int    idebug,
        	int    init,
        	int    *jsatty,
        	int    *key,
        	int    *mbchan,
        	int    *smchan,
        	int    *iret );

void ciproc (	int     *mproc,
        	int     *mbchan,
        	int     *iret );

void cigetq (  int    *mproc,
               int    *requeue,
               key_t  *key,
               int    *mbchan,
               int    *smchan,
               int    *iret );

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

