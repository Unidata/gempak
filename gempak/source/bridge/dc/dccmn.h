/************************************************************************
 * dccmn.h								*
 *									*
 * This file contains header files and global variables for use in the	*
 * decoder bridge routines.						*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/95						*
 * S. Jacobs/NCEP	 4/96	Added AFOS bulletin identifiers		*
 * S. Jacobs/NCEP	 6/96	Added ipid, nbull; Replaced ldfd->fplog	*
 * S. Jacobs/NCEP	 7/96	Replaced fplog->dcdlog			*
 * A. Hardy/GSC          1/01   Added dc, dcb prototypes		*
 * S. Jacobs/NCEP	 2/01	Removed all references to ulog		*
 * m.gamazaychikov/SAIC 07/05  	Added iwndht				*
 * H. Zeng/SAIC		08/05	Added second station table		*
 * L. Hinson/AWC        06/08   Add circflg                             *
 ***********************************************************************/

#include "geminc.h"	/* Include all necessary C header files */
#include "gemprm.h"	/* Include the GEMPAK parameters */
#include "bridge.h"	/* Include Bridge specific parameters */

/*---------------------------------------------------------------------*/

/* Structure definitions */

/* Buffer pointers */
typedef struct {
    char *sbuf;		/* Starting address of the buffer */
    char *ebuf;		/* Ending address of the buffer */
    char *bhead;	/* Bulletin head -- pointer to ^A */
    char *btail;	/* Bulletin tail -- pointer to ^C */
    char *nxtfeed;	/* Pointer to the start of next data feed */
    char *nxtpeek;	/* Pointer to the next byte to be peeked */
} dcb_t;

/* States for finding a bulletin */
typedef enum {
    START,
    NOT_IN_BULLETIN,
    SOH_,
    SOH_CR_,
    SOH_CR_CR_,
    IN_BULLETIN,
    CR_,
    CR_CR_,
    CR_CR_NL_,
    Z_,
    Z_C_,
    Z_C_Z_,
    N_,
    N_N_,
    N_N_N_
} fstate_t;

/*---------------------------------------------------------------------*/

/* Global variables */

#ifdef DCCMN_GLOBAL

	char		dcdlog[DCMXLN];
				/* Decoder log file name */

	char		cprgnm[DCMXLN];
				/* Program name for log messages */

	int		ipid;
				/* Process ID for log messages */

	unsigned	nbull;
				/* Number of bulletins read
				   and processed */

	int		ivrblv, itmout, nhours, irltim, txtflg, circflg,
			iadstn, maxtim, iwndht;
				/* Logging level;
				   Timeout value in seconds;
				   Number of hours to decode
				      prior to the "current" time;
				   Real-time flag;
				   Flag for not saving text data;
				   Number of additional stations;
				   Maximum number of times;
                                   Cutoff height for dropsonde winds */

	char		curtim[DCMXLN], prmfil[DCMXLN], stntbl[DCMXLN],
			stntb2[DCMXLN];
				/* Parameter packing table;
				   Station table;
				   Second Station table */

#else

	extern char	dcdlog[DCMXLN];

	extern char	cprgnm[DCMXLN];

	extern int	ipid;

	extern unsigned	nbull;

	extern int	ivrblv, itmout, nhours, irltim, txtflg,
			circflg, iadstn, maxtim, iwndht;

	extern char	curtim[DCMXLN], prmfil[DCMXLN], stntbl[DCMXLN],
			stntb2[DCMXLN];

#endif

/*
 *  dcb prototypes
 */
int 	dcb_getb ( 	char	*bulletin );

int 	dcb_init ( 	void );

int 	dcb_isempt ( 	void );

int 	dcb_isfull ( 	void );

unsigned char dcb_peekc(unsigned char	*ch );

size_t 	dcb_put ( 	int	fd,
			int	tmout);	

void 	dcb_sbhd ( 	int	offset );

void 	dcb_sbtl ( 	int	offset );


/*
 *  dc prototypes
 */

#include "proto_bridge.h"


void 	dc_dlog  ( 	char	*messag,
			int	*lenm,
			int	*iret );

void 	dc_gbul ( 	char	*bulletin,
			int	*lenb,
			int	*ifdtyp,
			int	*iret );

void 	dc_shnd ( 	int	sig );

void 	dc_sgnl ( void );

void	dc_wbuf (	int	*iret );

void 	dc_wclg  ( 	int	loglev,
			char	*errgrp,
			int	numerr,
			char	*errstr,
			int	*iret );

void	dc_wlog ( 	int	*loglev, 
			char	*errgrp, 
			int	*numerr, 
			char	*errstr, 
			int     *iret,
			... );

