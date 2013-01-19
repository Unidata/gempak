
/************************************************************************
 * proto_sigbufr.h                                                      *
 *                                                                      *
 * This include file contains function prototypes for all the c files   *
 * in the SIGBUFR library.                                              *
 *									*
 **                                                                     *
 * D. Kidwell/NCEP	 1/02          					*
 * M. Li/SAIC		12/04	Added new function sigbmcld,  and new	*
 *				parameters chlvl and ifpoutm		*
 ***********************************************************************/


#ifndef PROTO_SIGBUFR
#define PROTO_SIGBUFR


void sigbcld     ( FILE	*ifpout, 
		   int 	*iret );

void sigbeof     ( int 	n,
		   int 	*iret );

void sigbeom     ( int 	*n,
		   int 	*iret );

void sigberror   ( const char *str, 
		   int 	*iret );

void sigbfronts  ( FILE *ifpout, 
		   int 	type, 
		   char	*chlvl,
		   int 	*iret );

void sigbjets    ( FILE *ifpout, 
		   char	*chlvl,
		   int 	*iret );

void sigbmcld	 ( FILE *ifpoutm,
                   int  *iret );

void sigbradtn   ( FILE *ifpout, 
		   FILE	*ifpoutm,
		   int 	*iret );

void sigbstorm   ( FILE *ifpout, 
		   FILE	*ifpoutm,
		   int 	*iret );

void sigbtrop    ( FILE *ifpout, 
		   char	*chlvl,
		   int 	*iret );

void sigbturb    ( FILE *ifpout, 
		   char	*chlvl,
		   int 	*iret );

void sigbvolcano ( FILE *ifpout, 
		   FILE	*ifpoutm,
		   int  *iret );

#endif /* PROTO_SIGBUFR */
