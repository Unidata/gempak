/************************************************************************
 * proto_sigavgf.h                                                      *
 *                                                                      *
 * This include file contains function prototypes for the SIGAVGF files.*
 *                                                                      *
 **                                                                     *
 * A. Hardy/GSC          3/02                                           *
 * M. Li/SAIC		 7/04	Removed grpch from sigajet		*
 * M. Li/SAIC		01/05	Added sigamcld, add chlvl, remove jtime	*
 * M. Li/SAIC		10/05	Added flvl to sigajet			*
 ***********************************************************************/


#ifndef PROTO_SIGAVGF
#define PROTO_SIGAVGF

void sigacld ( 	char 	*fhour, 
		int 	numcld, 
		cloud_t *ptrc, 
		int 	itime[],
                char 	grpch,
		int 	*iret );

void sigafrt ( 	char 	*fhour, 
		int 	numfrt, 
		front_t *ptrf, 
		int 	itime[],
                char 	grpch,
		char	*chlvl,
		int 	*iret );


void sigajet ( 	char 	*fhour, 
		int 	numjet, 
		jets_t 	*ptrj, 
		int 	itime[],
		char	*chlvl,
		char	*flvl,
		int 	*iret );

void sigamcld ( char    *fhour,
                int     nummcld,
                mcloud_t *ptrm,
                int     itime[],
                char    grpch,
                int     *iret );

void sigatrp ( 	char 	*fhour, 
		int 	rnum, 
		trop_t 	*ptrr, 
		int 	hnum,
		trophi_t *ptrh, 
		int 	lnum, 
		troplo_t *ptrl, 
		int 	itime[],
		char	*chlvl,
		int 	*iret );

void sigatur ( 	char 	*fhour, 
		int 	numtur, 
		turb_t 	*ptrb, 
		int 	itime[],
                char 	grpch,
		char	*chlvl,
		int 	*iret );

void sigavts ( 	char 	*fhour, 
		int 	numstm, 
		storm_t *ptrs, 
		int 	numvlr, 
		volrad_t *ptrv, 
		int 	itime[], 
                char 	grpch,
		char	*chlvl,
		int	*iret );

#endif /* PROTO_SIGAVGF */
