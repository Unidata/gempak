#include "geminc.h"
#include "gemprm.h"
#include "ctbcmn.h"

#define GFATEMP_TBL 		"gfa.tbl"
#define MAX_STRLEN		512

#define TOP			( 0 )
#define BOTTOM			( 1 )


/************************************************************************
 * ctbgfa.c                                                             *
 *                                                                      *
 * This module contains the subroutines to read in and retrieve GFA     *
 * gui information.				    		        *
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *   public functions:                                                  *
 *	ctb_gfard	-	read in the GFA gui info from the table	*
 *	ctb_gfagfhr	-	get forecast hour choice list		*
 *      ctb_gfaghrAndUTC -      get forecast hour & UTC time choice list*
 *	ctb_gfaghaz 	-	get hazard choice list			*
 *	ctb_gfagndesc	-	get the number of descriptor for hazard *
 *	ctb_gfagdesc	-	get descriptor and the number of choice	*
 *	ctb_gfagdc	-	get a choice string			*
 *	ctb_gfagid	-	get hazard id  				*
 *	ctb_gfagdesk	-	get desk list				*
 *	ctb_gfagtag	-	get tag list				*
 *	ctb_gfagcat	-	get hazard category			*
 *	ctb_gfagtemp	-	get airmet file name template		*
 *	ctb_gfaCmpSeverity  -   compare severity of two descriptions	*
 *      ctb_gfaCombineIFRTypes- combine 2 IFR type strings into one     *
 *      ctb_gfaCombineMtnObscTypes- combine 2 Mtn Obsc type strings to 1*
 *      ctb_gfaWorstFL	- 	get the worst flight and freezing level *
 *      ctb_gfaWorstIssue   - 	get the worst issue status		*
 *      ctb_gfagfhrAndUTC -     get forecast hour/valid time choice list*
 *                                                                      *
 *   private functions:                                                 *
 *	ctb_gfaIsHazard   -	check if a hazard is already in gfaHaz	*
 *	ctb_gfaIsDesc	  -	check if a desc is already in gfaHaz	*
 *	ctb_gfaAddHazard  -	add a hazard into gfaHaz		*
 *	ctb_gfaAddDesc	  -	add a descriptor into gfaHaz		*
 *	ctb_gfaAddChoice  -	add a choice into gfaHaz		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           10/04   Created                                *
 * B. Yin/SAIC           11/04   Added ctb_gfagcat                      *
 * B. Yin/SAIC           11/04   Changed gfa_temp.tbl to gfa.tbl        *
 * B. Yin/SAIC           12/04   Added airmet file name template        *
 * J. Wu/SAIC		 10/05	 Added GFA table initialization flag    *
 * B. Yin/SAIC           07/06   removed ctb_gfaGetWorstIssTyp          *
 * L. Hinson/AWC         09/09   Add ctb_gfaCombineMtnObscTypes         *
 * L. Hinson/AWC         09/09   Add ctb_gfagfhrAndUTC                  *
 ***********************************************************************/

   /*
    * Private functions
    */
   static int  ctb_gfaIsHazard ( char * str, int *index );
   static int  ctb_gfaIsDesc ( char * str, int indexHaz, int *index );
   static void ctb_gfaAddHazard ( char *hazStr );
   static void ctb_gfaAddDesc ( char *descStr, int hazIndex );
   static void ctb_gfaAddChoice ( char *choice, int hazIndex, int descIndex );

   /*
    * Global variables
    */
   static int		nFcstHr;	/* number of forecast hour choices */
   static char 		**fcstHr;	/* forecast hour choices	   */
   static int		nTags;		/* number of tags		   */
   static char 		**tags;		/* tags				   */
   static int		nDesks;		/* number of desks		   */
   static char 		**desks;	/* desks			   */
   static int           nIssOpts;       /* Number of Issue Options         */
   static char          **issOpts;      /* issue options                   */
   static GFA_Haz_t	gfaHazard;	/* GFA gui information		   */
   static char		fileTemp[ 128 ];/* AIRMET file name template	   */

   static int		_gfaInit = G_FALSE;     /* gfa.tbl loaded or not  */
   
/*======================================================================*/


void ctb_gfard ( int *iret )
/************************************************************************
 * ctb_gfard                                                            *
 *                                                                      *
 * This routine reads in GFA gui information from table gfa_temp.tbl.	*
 *                                                                      *
 * void ctb_gfard ( iret )	                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 *	None                                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                      =  0: normal                    *
 *                                      = -1: no table/data             *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 * B. Yin/SAIC          11/04   Added hazard category                   *
 * B. Yin/SAIC          12/04   Added airmet file name template         *
 * L. Hinson/AWC        09/05   Added issue status processing           *      
 * J. Wu/SAIC		10/05	Added initialization flag _gfaInit	*
 * E. Safford/SAIC	01/06	Treat any "NULL_STRING" as a "" entry	*
 * B. Yin/SAIC          06/06   Added desks and changed tag items       *
 ***********************************************************************/
{
    int 	ii, bufsize, len;
    int 	row = 0, ier = 0, indexHaz = 0, indexDesc = 0;
    char	oneLine [ MAX_STRLEN ], *field1, *field2, *field3;
    size_t	jj, lineLenth;

    FILE        *tblFile = NULL;
/*----------------------------------------------------------------------*/

    /*
     * Check if the table has been loaded. 
     */
    if ( _gfaInit ) {
        return;	
    }
    
    /*
     * Open the gfa gui table file
     */
    if ( ( tblFile = cfl_tbop ( GFATEMP_TBL, "pgen", &ier ) ) != NULL ) {

       cfl_tbnr ( tblFile, &row, &ier );

    }

    if ( !tblFile || row == 0 || ier != 0 ) {

       *iret = -1;
       return;

    }

    /*
     * Put 'New' as the first item of tags.
     */
    nTags = 1;
    G_MALLOC ( tags, char*, nTags, "tag NEW" );
    len = 4;
    G_MALLOC ( tags[ 0 ], char, len, "tag NEW" );
    strcpy ( tags[ 0 ], "NEW" );

    /*
     * Read in hazard choices, forecast hour choices and tags
     */
    bufsize = sizeof ( oneLine );

    for ( ii = 0; ii < row; ii++ ) {

        cfl_trln ( tblFile, bufsize, oneLine, &ier );

        if ( strncasecmp ( oneLine, "hazard", 6 ) == 0 ) {

	   /*
	    * Skip blank space
	    */
	   jj = 6;
	   while ( (( oneLine[ jj ] == ' ' ) || ( oneLine[ jj ] == '\t' ))
	 	   && ( jj < strlen ( oneLine ) ) ) {

		 jj++;
	   }

	   if ( strlen ( oneLine ) > (size_t)6 ) {

	      ctb_gfaAddHazard ( &oneLine[ jj ] );
	   }
	   else {
	      
	      continue;
	   }

	}
	else if ( strncasecmp ( oneLine, "fcsthr", 6 ) == 0 ) {

	   /*
	    * Skip blank space
	    */
	   jj = 6;
	   while ( (( oneLine[ jj ] == ' ' ) || ( oneLine[ jj ] == '\t' ))
		   && ( jj < strlen ( oneLine ) ) ) {

		 jj++;
	   }

	   if ( strlen ( oneLine ) <= (size_t)6 ) {
	    
	      continue;
	   }

	   /*
	    * Allocate memory for fcsthr
  	    */
	   if ( nFcstHr == 0 ) {
	       G_MALLOC ( fcstHr, char*, nFcstHr + 1, "forecast hours" );
	   }
	   else {
	       G_REALLOC ( fcstHr, char*, nFcstHr + 1, "forecast hours" );
	   }

	   G_MALLOC ( fcstHr[ nFcstHr ], char, 
		      (int)strlen ( &oneLine [ jj ] ) + 1, "forecast hours" );

	   strcpy ( fcstHr[ nFcstHr ], &oneLine[ jj ] );
	   nFcstHr++;

	}
	else if ( strncasecmp ( oneLine, "tag", 3 ) == 0 ) {

	   /*
	    * Skip blank space
	    */
	   jj = 3;
	   while ( (( oneLine[ jj ] == ' ' ) || ( oneLine[ jj ] == '\t' ))
		   && ( jj < strlen ( oneLine ) ) ) {

		 jj++;
	   }

	   if ( strlen ( oneLine ) <= (size_t)3 ||
	        strcasecmp ( &oneLine[ jj ], "new" ) == 0 ) {

	      /*
	       *  Skip 'new' because it's been added at the begining.
	       */
              strcpy ( tags[ 0 ], &oneLine[ jj ] );
	      continue;
	   }

	   /*
	    * Allocate memory for tags 
  	    */
	   if ( nTags == 0 ) {
	       G_MALLOC ( tags, char*, nTags + 1, "tags" );
	   }
	   else {
	       G_REALLOC ( tags, char*, nTags + 1, "tags" );
	   }

	   G_MALLOC ( tags[ nTags ], char, 
		      (int)strlen ( &oneLine [ jj ] ) + 1, "tags" );

	   strcpy ( tags[ nTags ], &oneLine[ jj ] );
	   nTags++;

	}
	else if ( strncasecmp ( oneLine, "desk", 4 ) == 0 ) {

	   /*
	    * Skip blank space
	    */
	   jj = 4;
	   while ( (( oneLine[ jj ] == ' ' ) || ( oneLine[ jj ] == '\t' ))
		   && ( jj < strlen ( oneLine ) ) ) {

		 jj++;
	   }

	   if ( strlen ( oneLine ) <= (size_t)4 ) {

	      continue;
	   }

	   /*
	    * Allocate memory for desks 
  	    */
	   if ( nDesks == 0 ) {
	       G_MALLOC ( desks, char*, nDesks + 1, "desks" );
	   }
	   else {
	       G_REALLOC ( desks, char*, nDesks + 1, "desks" );
	   }

	   G_MALLOC ( desks[ nDesks ], char, 
		      (int)strlen ( &oneLine [ jj ] ) + 1, "desks" );

	   strcpy ( desks[ nDesks ], &oneLine[ jj ] );
	   nDesks++;

	}
        else if (strncasecmp ( oneLine, "issue", 5 ) == 0 ) {
           jj = 5;
           while ( (( oneLine[ jj ] == ' ' ) || ( oneLine[ jj ] == '\t' ))
		   && ( jj < strlen ( oneLine ) ) ) {

		 jj++;
	   }

	   if ( strlen ( oneLine ) <= (size_t)5 ) {
	    
	      continue;
	   }
           if ( nIssOpts == 0 ) {
             G_MALLOC ( issOpts, char*, nIssOpts + 1, "issOpts" );
           }
           else {
	       G_REALLOC ( issOpts, char*, nIssOpts + 1, "issOpts" );
	   }

	   G_MALLOC ( issOpts[ nIssOpts ], char, 
		      (int)strlen ( &oneLine [ jj ] ) + 1, "issOpts" );
           strcpy ( issOpts [ nIssOpts ], &oneLine[ jj ] );
           nIssOpts++;
        }
	else if ( strncasecmp ( oneLine, "airmet_file", 11 ) == 0 ) {
	   /*
  	    * Skip blank space
   	    */
    	   jj = 11;
   	   while ( (( oneLine[ jj ] == ' ' ) || ( oneLine[ jj ] == '\t' ))
   		   && ( jj < strlen ( oneLine ) ) ) {

		 jj++;
 	   }

	   if ( strlen ( &oneLine[ jj ] ) > (size_t)0 ) {
	 
	      strcpy ( fileTemp, &oneLine[ jj ] );
	   }
	}

    }

    cfl_seek( tblFile, 0, SEEK_SET, &ier );

    /*
     * Read in hazard descriptors and choices
     */
    for ( ii = 0; ii < row; ii++ ) {

        cfl_trln( tblFile, bufsize, oneLine, &ier );

	lineLenth = strlen ( oneLine );

	/*
	 * Get the first field
	 */
	jj = 0;
	while ( oneLine[ jj ] != ' ' && oneLine[ jj ] != '\t' && ++jj < lineLenth );
	oneLine[ jj ] = '\0';
	field1 = oneLine;

	if ( !ctb_gfaIsHazard ( field1, &indexHaz ) ) continue;

	/*
	 * Get the second field
	 */
	jj++;
	while ( (oneLine[ jj ] == ' ' || oneLine[ jj ] == '\t' ) && ++jj < lineLenth );
        
   	if ( jj >= lineLenth ) continue;
         
	field2 = &oneLine[ jj ];
	while ( oneLine[ jj ] != ' ' && oneLine[ jj ] != '\t' && ++jj < lineLenth );
	oneLine[ jj ] = '\0';

	/*
	 * Get the third field
	 */
 	jj++;
	while ( (oneLine[ jj ] == ' ' || oneLine[ jj ] == '\t' ) && ++jj < lineLenth );
	if ( jj >= lineLenth ) continue;

	field3 = &oneLine[ jj ];

	if( strcmp( field3, "NULL_STRING" ) == 0 )  strcpy( field3, "" );

	/*
	 * Add the descriptor and the choice
	 */
	if ( ( gfaHazard.haz[ indexHaz ].ndesc == 0 ) ||
	     !ctb_gfaIsDesc ( field2, indexHaz, &indexDesc ) ) {

	   ctb_gfaAddDesc  ( field2, indexHaz );
	   ctb_gfaAddChoice ( field3, indexHaz, gfaHazard.haz[ indexHaz ].ndesc - 1 );
	}

	else {
           ctb_gfaAddChoice ( field3, indexHaz, indexDesc );
	}

    }

    cfl_clos( tblFile, &ier );

    *iret = 0;

    /*
     * Set the initialization flag TRUE. 
     */
    _gfaInit = G_TRUE;

}

/*======================================================================*/

void ctb_gfagfhr ( char *sep, int *nfhr, char *fhrlist, int *iret )
/************************************************************************
 * ctb_gfagfhr		                                                *
 *                                                                      *
 * This routine gets the list of forecast hours and the number of 	*
 * forecast hours.							*
 *                                                                      *
 * void ctb_gfagfhr ( sep, nfhr, fhrlist, iret )			* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*sep		char		list separator	 	        *
 *                                                                      *
 * Output parameters:                                                   *
 *	*nfhr		int		number of forecast hours        *
 *	*fhrlist	char		list of forecast hours 	        *
 *	*iret		int		return code                     *
 *					=  0: normal return		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 * J. Wu/SAIC		10/05	Load GFA table if necessary             *
 ***********************************************************************/
{
    int ii, ier;
/*----------------------------------------------------------------------*/

    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    *nfhr = nFcstHr;
    fhrlist[ 0 ] = '\0';

    for ( ii = 0; ii < nFcstHr; ii++ ) {		

	sprintf ( fhrlist, "%s%s%s", fhrlist, fcstHr[ ii ],
		  ( ii == nFcstHr - 1 )? "" : sep );
                  
    }

    *iret = 0;
}

/*======================================================================*/

void ctb_gfagfhrAndUTC ( char *sep, char *cycle, int *nfhr, char *fhrlist,
                         int *iret )
/*************************************************************************
  ctb_gfagfhrAndUTC
  
  This routine gets the list of forecast hours and the number of forecast
  hours.  It also computes the valid forecast time from the given cycle.
  
  ctb_gfagfhrAndUTC (sep, cycle, nfhr, fhrlist, iret)
  
  Input parameters:
      *sep      char    list separator
      *cycle    char    current cycle Time
      
  Output paramters:
      *nfhr     int     number of forecast hours
      *fhrlist  char    list of forecast hours
      *iret     int     return code
                        = 0: normal return
                        
  Log:
  L. Hinson    09/09  Created
*************************************************************************/
{
    int ii, ier;
    char fcstHrP[10], fcstUTCHr[4];
/*----------------------------------------------------------------------*/

    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    *nfhr = nFcstHr;
    fhrlist[ 0 ] = '\0';

    for ( ii = 0; ii < nFcstHr; ii++ ) {		
        if (strcasecmp(fcstHr[ii],"OTHER") != 0 && strstr(fcstHr[ii],"-")==NULL) {
          sprintf(fcstUTCHr,"%02dZ",(atoi(fcstHr[ii])+atoi(cycle)) % 24);
          sprintf(fcstHrP,"%2s",fcstHr[ ii ]);
          sprintf ( fhrlist, "%s%s  %s%s", fhrlist, fcstHrP, fcstUTCHr,
		  ( ii == nFcstHr - 1 )? "" : sep );

        } else {          
          sprintf ( fhrlist, "%s%s%s", fhrlist, fcstHr[ii],
		  ( ii == nFcstHr - 1 )? "" : sep );
        }
    }

    *iret = 0;
}

/*======================================================================*/

void ctb_gfagtag ( char *sep, int *ntag, char *taglist, int *iret )
/************************************************************************
 * ctb_gfagtag		                                                *
 *                                                                      *
 * This routine gets the list of tagss and the number of tags.		*
 *                                                                      *
 * void ctb_gfagtag ( sep, ntag, taglist, iret )			* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*sep		char		list separator	 	        *
 *                                                                      *
 * Output parameters:                                                   *
 *	*ntag		int		number of tags		        *
 *	*taglist	char		list of tags	 	        *
 *	*iret		int		return code                     *
 *					=  0: normal return		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 * J. Wu/SAIC		10/05	Load GFA table if necessary             *
 ***********************************************************************/
{
    int ii, ier;
/*----------------------------------------------------------------------*/

    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    *ntag = nTags;
    taglist[ 0 ] = '\0';

    for ( ii = 0; ii < nTags; ii++ ) {		

	sprintf ( taglist, "%s%s%s", taglist, tags[ ii ],
		  ( ii == nTags - 1 )? "" : sep );

    }

    *iret = 0;
}

/*======================================================================*/

void ctb_gfagdesk ( char *sep, int *ndesk, char *desklist, int *iret )
/************************************************************************
 * ctb_gfagrgn		                                                *
 *                                                                      *
 * This routine gets the list of desks and the number of desks.		*
 *                                                                      *
 * void ctb_gfagrgn ( sep, ndesk, desklist, iret )			* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*sep		char		list separator	 	        *
 *                                                                      *
 * Output parameters:                                                   *
 *	*ndesk		int		number of desks	        	*
 *	*desklist	char		list of desks	 	        *
 *	*iret		int		return code                     *
 *					=  0: normal return		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		05/06	Created 	                        *
 ***********************************************************************/
{
    int ii, ier;
/*----------------------------------------------------------------------*/

    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    *ndesk = nDesks;
    desklist[ 0 ] = '\0';

    for ( ii = 0; ii < nDesks; ii++ ) {		

	sprintf ( desklist, "%s%s%s", desklist, desks[ ii ],
		  ( ii == nDesks - 1 )? "" : sep );

    }

    *iret = 0;
}

void ctb_gfagiss (char *sep, int *nissOpt, char *issOptlist, int *iret )
/************************************************************************
 * ctb_gfagiss		                                                *
 *                                                                      *
 * This routine gets the list of issue status options and the number    *
 *                                                                      *
 * void ctb_gfagtag ( sep, nissOpt, issOptlist, iret )			* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*sep		char		list separator	 	        *
 *                                                                      *
 * Output parameters:                                                   *
 *	*nissOpt	int		number of issue 	        *
 *	*issOptlist	char		list of issue options	        *
 *	*iret		int		return code                     *
 *					=  0: normal return		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Hinson/AWC	11/05	Created 	                        *
 ***********************************************************************/
{
    int ii, ier;
/*----------------------------------------------------------------------*/

    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    *iret 	    = 0;
    *nissOpt 	    = nIssOpts;
    issOptlist[ 0 ] = '\0';

    for ( ii = 0; ii < nIssOpts; ii++ ) {
        sprintf ( issOptlist, "%s%s%s",issOptlist, issOpts [ ii ],
                  ( ii == nIssOpts - 1 )? "" : sep );
    }
  
}  
  
/*======================================================================*/

void ctb_gfaghaz ( char *sep, int *nhaz, char *hazlist, int *iret )
/************************************************************************
 * ctb_gfaghaz		                                                *
 *                                                                      *
 * This routine gets the list of hazards and the number of hazards in   *
 * the GFA gui information structure.					*
 *                                                                      *
 * void ctb_gfaghaz ( sep, nhaz, hazlist, iret )			* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*sep		char		list separator	 	        *
 *                                                                      *
 * Output parameters:                                                   *
 *	*nhaz		int		number of hazards 	        *
 *	*hazlist	char		list of hazards 	        *
 *	*iret		int		return code                     *
 *					=  0: normal return		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 * J. Wu/SAIC		10/05	Load GFA table if necessary             *
 ***********************************************************************/
{
    int ii, ier;
/*----------------------------------------------------------------------*/
    
    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    *nhaz = gfaHazard.nhaz;
    hazlist[ 0 ] = '\0';

    for ( ii = 0; ii < *nhaz; ii++ ) {

	sprintf ( hazlist, "%s%s%s", hazlist, gfaHazard.haz[ ii ].hazard,
		  ( ii == *nhaz - 1 )? "" : sep );
    }

    *iret = 0;   

}

/*======================================================================*/

void ctb_gfagndesc ( char *haz, int *ndesc, int *iret )
/************************************************************************
 * ctb_gfagndesc	                                                *
 *                                                                      *
 * This routine gets the number of descriptors in a hazard from the GFA *
 * gui information structure.						*
 *                                                                      *
 * void ctb_gfagndesc ( haz, ndesc, iret )				* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*haz		char		hazard string		        *
 *                                                                      *
 * Output parameters:                                                   *
 *	*ndesc		int		number of descriptors 	        *
 *	*iret		int		return code                     *
 *					=  0: normal return		*
 *					= -1: couldn't find the hazard	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 * J. Wu/SAIC		10/05	Load GFA table if necessary             *
 ***********************************************************************/
{
    int ii, ier;
/*----------------------------------------------------------------------*/

    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    for ( ii = 0; ii < gfaHazard.nhaz; ii++ ) {

	if ( strcasecmp ( haz, gfaHazard.haz[ ii ].hazard ) == 0 ) {

	   *ndesc = gfaHazard.haz[ ii ].ndesc;
	   *iret = 0;
	   return;

	}
    }

    *ndesc = 0;
    *iret  = -1;
}
/*======================================================================*/

void ctb_gfagcat ( char *haz, char *cat, int *iret )
/************************************************************************
 * ctb_gfagcat		                                                *
 *                                                                      *
 * This routine gets the category (Serria/Tango/Zulu) of a hazard.	*
 *                                                                      *
 * void ctb_gfagcat ( haz, cat, iret )					* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*haz		char		hazard string		        *
 *                                                                      *
 * Output parameters:                                                   *
 *	*cat		int		hazard category 	        *
 *	*iret		int		return code                     *
 *					=  0: normal return		*
 *					= -1: couldn't find the hazard	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		11/04	Created 	                        *
 * J. Wu/SAIC		10/05	Load GFA table if necessary             *
 ***********************************************************************/
{
    int ii, ier;
/*----------------------------------------------------------------------*/

    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    for ( ii = 0; ii < gfaHazard.nhaz; ii++ ) {

	if ( strcasecmp ( haz, gfaHazard.haz[ ii ].hazard ) == 0 ) {

	   strcpy ( cat, gfaHazard.haz[ ii ].cat );
	   *iret = 0;
	   return;

	}
    }

    gfaHazard.haz[ ii ].cat[ 0 ] = '\0';
    *iret  = -1;
}

/*======================================================================*/


void ctb_gfagdesc ( char *haz, int *which, char *type, char *desc, 
		    int *nchoices, int *iret )
{
/************************************************************************
 * ctb_gfagdesc	                                                        *
 *                                                                      *
 * This routine gets a descriptor string from the GFA gui information 	*
 * structure.								*
 *                                                                      *
 * void ctb_gfagdesc ( haz, which, type, desc, nchoices, iret )   	* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*haz		char		hazard string		        *
 *	*which		int		which descriptor (start from 0) *
 *                                                                      *
 * Output parameters:                                                   *
 *	*type		char		descriptor type 	        *
 *	*desc		char		descriptor string 	        *
 *	*nchoies	int		number of choices 	        *
 *	*iret		int		return code                     *
 *					=  0: normal return		*
 *					= -1: couldn't find the desc	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 * J. Wu/SAIC		10/05	Load GFA table if necessary             *
 ***********************************************************************/
    int ii, ier;
/*----------------------------------------------------------------------*/

    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    for ( ii = 0; ii < gfaHazard.nhaz; ii++ ) {

	if ( strcasecmp ( haz, gfaHazard.haz[ ii ].hazard ) == 0 ) {

	   if ( *which < gfaHazard.haz[ ii ].ndesc && *which >= 0 ) {

	      strcpy ( desc, gfaHazard.haz[ ii ].desc[ *which ].label );
	      strcpy ( type, gfaHazard.haz[ ii ].desc[ *which ].type );
	      *nchoices =  gfaHazard.haz[ ii ].desc[ *which ].nchoices;

	      *iret = 0;
	      return;
	   }
	}
    }

    *iret  = -1;
}

/*======================================================================*/


void ctb_gfagdc ( char *haz, int *which, int *choice, char *str, int *iret )
/************************************************************************
 * ctb_gfagdc	                                                        *
 *                                                                      *
 * This routine gets a choice string from the GFA gui information 	*
 * structure.								*
 *                                                                      *
 * void ctb_gfagdc ( haz, which, choice, str, iret )            	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*haz		char		hazard string		        *
 *	*which		int		which descriptor (start from 0) *
 *	*choice		int		which choice (start from 0)	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*str		char		choice string 	                *
 *	*iret		int		return code                     *
 *					=  0: normal return		*
 *					= -1: couldn't find the choice	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 * J. Wu/SAIC		10/05	Load GFA table if necessary             *
 * B. Yin/SAIC		03/06	Handle popup type for hazard IFR        *
 ***********************************************************************/
{
    int ii, ier;
/*----------------------------------------------------------------------*/

    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    for ( ii = 0; ii < gfaHazard.nhaz; ii++ ) {

	if ( strcasecmp ( haz, gfaHazard.haz[ ii ].hazard ) == 0 ) {

	   if ( *which < gfaHazard.haz[ ii ].ndesc && *which >= 0 &&
		*choice < gfaHazard.haz[ ii ].desc[ *which ].nchoices &&
		*choice >= 0 ) {

	      if ( ( strcasecmp ( gfaHazard.haz[ ii ].desc[ *which ].type,
			     "PULLDOWN" ) == 0 ) || 
		   ( strcasecmp ( gfaHazard.haz[ ii ].desc[ *which ].type,
                             "POPUP" ) == 0 ) ) {

	         strcpy ( str, gfaHazard.haz[ ii ].desc[ *which ].choice[ *choice ] );
	      }

	      *iret = 0;
	      return;

	   }
	}
    }

    *iret  = -1;
}

/*======================================================================*/


static int ctb_gfaIsHazard ( char *testStr, int *index )
/************************************************************************
 * ctb_gfaIsHazard	                                                *
 *                                                                      *
 * This routine check if a text string is alread in the hazard		*
 * field of the GFA gui information structure.				*
 *                                                                      *
 * void ctb_gfaIsHazard ( testStr, index )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*testStr	char	 hazard string	       			*
 *                                                                      *
 * Output parameters:                                                   *
 *	index		int	which haz, if the string is in gfaHaz   *
 *                                                                      *
 * Return parameters:                                                   *
 *			int	0: the string is not in the gfaHaz	*
 *				1: the string is in the gfaHaz		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 ***********************************************************************/
{
    int ii;
/*----------------------------------------------------------------------*/

    for ( ii = 0; ii < gfaHazard.nhaz; ii++ ) {

	if ( strcasecmp ( testStr, gfaHazard.haz[ ii ].hazard ) == 0 ) {

	   *index = ii;
	   return 1;
	}
    }

    return 0;
}

/*======================================================================*/


static int ctb_gfaIsDesc ( char *testStr, int indexHaz, int *index )
/************************************************************************
 * ctb_gfaIsDesc	                                                *
 *                                                                      *
 * This routine check if a text string is alread in the descriptor      *
 * field of the GFA gui information structure.				*
 *                                                                      *
 * void ctb_gfaIsDesc ( testStr, indexHaz, index )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*testStr	char	 descriptor string	       		*
 *	indexHaz	int	 which hazard	       			*
 *                                                                      *
 * Output parameters:                                                   *
 *	index		int	which desc, if the string is in gfaHaz  *
 *                                                                      *
 * Return parameters:                                                   *
 *			int	0: the string is not in the gfaHaz	*
 *				1: the string is in the gfaHaz		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 ***********************************************************************/
{
    int ii;
/*----------------------------------------------------------------------*/

    for ( ii = 0; ii < gfaHazard.haz[ indexHaz ].ndesc; ii++ ) {

	if ( strcasecmp ( testStr, 
	          gfaHazard.haz[ indexHaz ].desc[ ii ].label ) == 0 ) {

	   *index = ii;
	   return 1;
	}
    }

    return 0;
}

/*======================================================================*/


static void ctb_gfaAddHazard ( char *hazStr )
/************************************************************************
 * ctb_gfaAddHazard                                                     *
 *                                                                      *
 * This routine adds a hazard into the GFA gui information structure.	*
 *                                                                      *
 * void ctb_gfaAddHazard ( hazStr )			             	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*hazStr		char		hazard string	            	*
 *                                                                      *
 * Output parameters:                                                   *
 *	None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 * B. Yin/SAIC		10/04	Added hazard category		        *
 * B. Yin/SAIC          06/06   Added hazard id and the string format   *
 ***********************************************************************/
{
   char hazType[ 21 ];
/*---------------------------------------------------------------------*/

   /*
    * Allocate memory for hazard structure
    */
   if ( gfaHazard.nhaz == 0 ) {

      G_MALLOC ( gfaHazard.haz, Hazard_t, gfaHazard.nhaz + 1, "hazard" );

   }
   else {

      G_REALLOC ( gfaHazard.haz, Hazard_t, gfaHazard.nhaz + 1, "hazard" );
   }

   /*
    *  Get hazard type, hazard category, and hazard id.
    */
   sscanf( hazStr, "%s%s%s", hazType,
                   gfaHazard.haz[ gfaHazard.nhaz ].cat,
                   gfaHazard.haz[ gfaHazard.nhaz ].id );

   /*
    *  Write hazard
    */
   G_MALLOC ( gfaHazard.haz[ gfaHazard.nhaz ].hazard, char,
              (int)strlen ( hazType ) + 1, "hazard" );

   strcpy ( gfaHazard.haz[ gfaHazard.nhaz ].hazard, hazType );

   gfaHazard.haz[ gfaHazard.nhaz ].ndesc = 0;
   gfaHazard.nhaz++;

}

/*======================================================================*/

static void ctb_gfaAddDesc ( char *descStr, int hazIndex )
/************************************************************************
 * ctb_gfaAddDesc                                                       *
 *                                                                      *
 * This routine adds a descriptor into the GFA gui information 		*
 * structure.								*
 *                                                                      *
 * void ctb_gfaAddDesc ( descStr, hazIndex )		             	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*descStr	char		descriptor string               *
 *	hazIndex	int		which hazard                    *
 *                                                                      *
 * Output parameters:                                                   *
 *	None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 * B. Yin/SAIC          11/04   Added hazard category                   *
 ***********************************************************************/
{
   int numDesc;
/*----------------------------------------------------------------------*/

   numDesc = gfaHazard.haz[ hazIndex ].ndesc;

   /*
    * Allocate memory for hazard descriptor structure
    */
   if ( numDesc == 0 ) {

      G_MALLOC ( gfaHazard.haz[ hazIndex ].desc, Haz_Desc_t, 
		 numDesc + 1, "hazard descriptor" ); 
   }
   else {

      G_REALLOC ( gfaHazard.haz[ hazIndex ].desc, Haz_Desc_t, 
		  numDesc + 1, "hazard descriptor" );
   }

   G_MALLOC ( gfaHazard.haz[ hazIndex ].desc[ numDesc ].label,
	      char,  (int)strlen( descStr ) + 1, "hazard descriptor" );	

   strcpy ( gfaHazard.haz[ hazIndex ].desc[ numDesc ].label, descStr );

   gfaHazard.haz[ hazIndex ].desc[ numDesc ].nchoices = 0;
   gfaHazard.haz[ hazIndex ].ndesc++;
}

/*======================================================================*/


static void ctb_gfaAddChoice ( char *choice, int hazIndex, int descIndex )
/************************************************************************
 * ctb_gfaAddChoice                                                     *
 *                                                                      *
 * This routine adds a choice in the GFA gui information structure.	*
 *                                                                      *
 * void ctb_gfaAddChoice ( choice, hazIndex, descIndex )             	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*choice		char		choice string                   *
 *	hazIndex	int		which hazard                    *
 *	descIndex	int		which descriptor                *
 *                                                                      *
 * Output parameters:                                                   *
 *	None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		10/04	Created 	                        *
 * B. Yin/SAIC          11/04   Added hazard category                   *
 * B. Yin/SAIC          03/06   Handle checkbox and popup               *
 ***********************************************************************/
{
   int 	numChoices, typeLen = 10;
   char	*columns;
/*----------------------------------------------------------------------*/

   numChoices = gfaHazard.haz[ hazIndex ].desc[ descIndex ].nchoices;

   /*
    * Allocate memory for choice array
    */
   if ( numChoices == 0 ) {

      /*
       * Check the type: pull-down or user iput
       */
      if ( strncasecmp ( choice, "userinput", 9 ) == 0 ) {
	 
 	 G_MALLOC ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].type,
                    char, typeLen, "hazard type" );

	 strcpy ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].type,
		  "USERINPUT" );

	 gfaHazard.haz[ hazIndex ].desc[ descIndex ].choice = NULL;
	
	 if ( strlen ( choice ) > (size_t)10 ) {

	    columns = strchr ( choice, ':' );
  	    gfaHazard.haz[ hazIndex ].desc[ descIndex ].nchoices =
		atoi ( &columns [ 1 ] );
	 }
	 else {

	    gfaHazard.haz[ hazIndex ].desc[ descIndex ].nchoices = 16;

	 }

	 return;

      }
      else if ( strncasecmp ( choice, "checkbox", 8 ) == 0 ) {

 	 G_MALLOC ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].type,
                    char, typeLen, "hazard type" );

	 strcpy ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].type,
		  "CHECKBOX" );

         G_MALLOC ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].choice, 
		    char*, typeLen - 9, "hazard choices" ); 

      }
      else if ( strncasecmp ( choice, "popup", 5 ) == 0 ) {

 	 G_MALLOC ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].type,
                    char, typeLen, "hazard type" );

	 strcpy ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].type,
		  "POPUP" );

         G_MALLOC ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].choice, 
		    char*, typeLen - 9, "hazard choices" ); 

      }
      else {

 	 G_MALLOC ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].type,
                    char, typeLen, "hazard type" );

	 strcpy ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].type,
		  "PULLDOWN" );

         G_MALLOC ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].choice, 
		    char*, typeLen - 9, "hazard choices" ); 
      }
   }
   else {

      G_REALLOC ( gfaHazard.haz[ hazIndex ].desc[ descIndex ].choice, 
		  char*, numChoices + 1, "hazard choices" );
   }

   if ( strncasecmp ( choice, "checkbox", 8 ) == 0 ) {
    
      choice = &choice[ 9 ];

   }
   else if ( strncasecmp ( choice, "popup", 5 ) == 0 ) {
	    
      choice = &choice[ 6 ];

   }

   G_MALLOC ( gfaHazard.haz[ hazIndex ].desc [ descIndex ].choice [ numChoices ],
	      char,  (int)strlen( choice ) + 1, "hazard choices" );	

   strcpy ( gfaHazard.haz[ hazIndex ].desc [ descIndex ].choice [ numChoices ], choice );

   gfaHazard.haz[ hazIndex ].desc[ descIndex ].nchoices++;

}

/*======================================================================*/

void ctb_gfagtemp ( char *fileName, int *iret )
/************************************************************************
 * ctb_gfagtemp		                                                *
 *                                                                      *
 * This routine gets the airmet file name template		 	*
 *                                                                      *
 * void ctb_gfagtemp ( fileName, iret )					* 
 *                                                                      *
 * Input parameters:                                                    *
 * 	None                                                            *
 *                                                                      *
 * Output parameters:                                                   *
 *	*fileName	char		airmet file name template       *
 *	*iret		int		return code                     *
 *					=  0: normal return		*
 *					= -1: no template		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		12/04	Created 	                        *
 * J. Wu/SAIC		10/05	Load GFA table if necessary             *
 ***********************************************************************/
{
    int ier;
/*----------------------------------------------------------------------*/

    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    if ( strlen ( fileTemp ) > (size_t)0 ) {
       
       strcpy ( fileName, fileTemp );

       *iret = 0;
    }

    else {
 
      fileName[ 0 ] = '\0';
      *iret = -1;
    }
}

/*======================================================================*/
void ctb_gfaCmpSeverity ( char *hazard, char *descriptor, char *value1,
			  char *value2, char *sevValue, int *iret )
/************************************************************************
 * ctb_gfaCmpSeverity							*
 *									*
 * This function compares the severity of two GFA hazard/descriptor	*
 * combinations and returns the most severe.  This relative severity    *
 * ranking is simply based on the order of occurance within the gfa.tbl *
 * so if they are not in descending order of severity, this function    *
 * will not return correct results.					*
 * 									*
 * Note that in the case of an error, value1 will be returned as the    *
 * highest value.  Also note that a value of "No Qualifier" will be     *
 * treated as the lowest severity.  A positive iret code indicates that *
 * one of the values was "No Qualifier".				*
 *									*
 * void ctb_gfaCmpSeverity ( hazard, descriptor, value1,                *
 *				value2, sevValue )			*
 *									*
 * Input parameters:							*
 *	*hazard		int	Area type of the elements 		*
 *	*descriptor	char	descriptor name (ie. Severity)		*
 *	*value1		char	First description value 		*
 *	*value2		char	Second description value 		*
 *									*
 * Output parameters:							*
 *	*sevValue	char	The more severe descriptor value	*
 *      *iret		int	return code				*
 *				   0 = normal				*
 *				  -1 = can't find hazard in gfa.tbl	* 
 *				  -2 = can't find descriptor in gfa.tbl *
 *				   2 = value2 is "No Qualifier"		*
 *				   1 = value1 is "No Qualifier"         *
 **									*
 * Log:									*
 * J. Wu/SAIC		10/05	move from pggfaw_cmpSeverity()		*
 ***********************************************************************/
{
    int		ii, ndesc, choice, nchoices, ier;
    char	type[ 50 ], desc[ 50 ];
    char	choiceStr[ 50 ];

    int		sev1 = 0, sev2 = 0;
    Boolean	foundDescriptor = False;
    Boolean	foundVal1 = False, foundVal2 = False;
/*---------------------------------------------------------------------*/

    if ( !_gfaInit ) {
        ctb_gfard ( &ier );	
    }

    *iret = 0;

    /*
     *  This is the default.  If anything goes wrong, (like we can't
     *  find the descriptor or table), value1 will be returned as the 
     *  sevValue. 
     */
    strcpy( sevValue, value1 );


    /*
     *  Check if either value is "No Qualifier".  This will appear in
     *  the choice list for the descriptor, but is to be treated as the 
     *  lowest priority.  Trick is -- it usually appears at the end of the 
     *  list which would give it the highest value if we didn't specificly 
     *  screen it out here.
     */
    if( strcmp( value2, "No Qualifier") == 0 ) {
        *iret = 2;
	return;
    }

    if( strcmp( value1, "No Qualifier") == 0 ) {
	*iret = 1;
	strcpy( sevValue, value2 );
	return; 
    }


    ctb_gfagndesc( hazard, &ndesc, &ier );
    if( ier < 0 ) {
	*iret = -1;
	return;
    }

    for( ii=0; ii< ndesc && !foundDescriptor; ii++ ) {
        ctb_gfagdesc( hazard, &ii, type, desc, &nchoices, &ier );

        if( strcmp( desc, descriptor )== 0 ) {
            foundDescriptor = True;

	    for( choice=0; choice < nchoices; choice++ ) {
	        ctb_gfagdc( hazard, &ii, &choice, choiceStr, &ier );  

                if( ier >= 0 ) {
		    if( strcmp( choiceStr, value1 )== 0 ) {
		       sev1 = choice;
		       foundVal1 = True;
                    }
		    if( strcmp( choiceStr, value2 )== 0 ) {
		       sev2 = choice;
		       foundVal2 = True;
		    }
		}
            }
        }
    }

    if( foundVal1 && foundVal2 ) {
        if( sev2 > sev1 ) {
	    strcpy( sevValue, value2 );
        }
    }
    else if( !foundDescriptor ) {
        *iret = -2;
    }

}

/*=====================================================================*/
	     
void ctb_gfaCombineIFRTypes( const char *inType0, const char *inType1, 
			char *outType, int *iret )

/************************************************************************
 * ctb_gfaCombineIFRTypes                                               *
 *                                                                      *
 * This routine takes two GFA IFR Type strings and puts them together 	*
 * in a single string in the order specified in the gfa.tbl file.  It   *
 * does this by stepping through the complete list of IFR Types and     *
 * checking to see if each type appears in either of the input types.   * 
 * If a particular type is found in either input string then it is      *
 * written out to the IFRType.						*
 *                                                                      *
 * Of the two inputs, inType0 and inType1, one may be a null string or  *
 * of zero length, but both cannot be null/zero length.			*
 *									*
 * void ctb_gfaCombineIFRTypes ( inType0, inType1, outType , iret ) 	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	*inType0	const char	first IFR type			*
 * 	*inType1	const char	second IFR type			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*outType	char		combination IFR type            *
 *	*iret		int		return code                     *
 *					=  0: normal return		*
 *					= -7: NULL or 0 length inputs   *
 *                                      = -9: gfa.tbl file not yet read *	
 *				        = -10:  no IFR in gfa.tbl	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	04/06	Initial coding                  	*
 * E. Safford/SAIC	07/06	correct CIG/VIS format, revise strlen 	*
 *				  handling for inputs			*
 ***********************************************************************/
{
    int		ii, jj, idx = -1, nLoaded, num= 32;
    int		len0, len1;
    static int	ntypes = 0;
    static char **IFRTypes = (char **)NULL;
    char	*ptr;

    Boolean	firstVisType;
/*----------------------------------------------------------------------*/

    *iret = 0;
    outType[ 0 ] = '\0';

    if( !inType0 ) {
        len0 = 0;
    } else {
	len0 = strlen( inType0 );
    }

    if( !inType1 ) {
        len1 = 0;
    } else {
	len1 = strlen( inType1 );
    }

    /*
     *  One of the inputs can be 0 length, but return an error if both are.
     */
    if( len0 == 0 && len1 == 0 ) {	
        *iret = -7;
	return;
    }


    /*
     *  If ntypes is 0, load the valid IFR types into the IFRTypes array.
     */
    if( ntypes == 0 ) {

        /*
         *  Verify the gfaHazard structure has been loaded; quit if it isn't.
         */
        if ( ! _gfaInit ) {
	    *iret = -9;
	    return;
        }
  
	/*
	 *  Find the index number for IFR.
	 */ 
        for( ii=0; ii< gfaHazard.nhaz; ii++ ) {
	    if( strcasecmp( gfaHazard.haz[ii].hazard, "IFR" ) == 0 ) {
	        idx = ii;
	        break;
	    }
        }

        /*
         *  An idx of < 0 should be impossible to reach but, just to be safe, 
	 *  we're going to check for it anyway, and exit if we have a bad value.
         */
        if( idx < 0 ) {
	    *iret = -10;
	    return;
        }        

        /*
         *  Get a count of the number of IFR types.  Count both the labels and
	 *  the non-null string choices. 
         */
        for( ii=0; ii< gfaHazard.haz[idx].ndesc; ii++ ) {
            ntypes++;	    
        
	    for( jj=0; jj < gfaHazard.haz[idx].desc[ ii ].nchoices; jj++ ) {
	        if( strlen( gfaHazard.haz[ idx ].desc[ ii ].choice[ jj ] ) > 0 ) {
	            ntypes++;
                } 
	    }
        }

        /*
	 *  Now allocate space and load the actual values into IFRTypes.
	 */
	nLoaded = 0;
        G_MALLOC( IFRTypes, char *, ntypes, "ctb_gfaCombineIFRTypes: IFRTypes" );

        for( ii=0; ii< gfaHazard.haz[idx].ndesc; ii++ ) {
        
            G_MALLOC( IFRTypes[ nLoaded ], char, num, "ctb_gfaCombineIFRTypes: IFRTypes" );
            strcpy( IFRTypes[ nLoaded ], gfaHazard.haz[idx].desc[ii].label );
	    nLoaded++;

	    for( jj=0; jj < gfaHazard.haz[idx].desc[ ii ].nchoices; jj++ ) {
	        if( strlen( gfaHazard.haz[ idx ].desc[ ii ].choice[ jj ] ) > 0 ) {
                    G_MALLOC( IFRTypes[ nLoaded ], char, num, "ctb_gfaCombineIFRTypes: IFRTypes" );
                    strcpy( IFRTypes[ nLoaded ], gfaHazard.haz[idx].desc[ii].choice[jj] );
	            nLoaded++;
                } 
	    }
        }

	/*
	 *  Remove all underscores and replace with blanks.
	 */
	for( ii=0; ii< nLoaded; ii++ ) {
	    for( jj=0; (unsigned int)jj < (strlen( IFRTypes[ii]) ); jj++ ) {
	        if( IFRTypes[ii][jj] == '_' ) {
		    IFRTypes[ii][jj] = ' ';
		}
	    }
	}

    }  /* if types == 0 */

    
    /*
     *  Loop though the list of IFRTypes.  If an individual type is found in
     *  either of the input strings, then write it to the output string.
     */
    firstVisType = True;
    for( ii=0; ii<ntypes; ii++ ) {

        ptr = NULL;
	if( len0 > 0 ) {
        ptr = strstr( inType0, IFRTypes[ii] );
        }

        if( ptr == NULL && len1 > 0 ) {
            ptr = strstr( inType1, IFRTypes[ ii ] );
        }

        if( ptr != NULL ) {
	    if( strlen( outType ) <= 0 ) {
		strcpy( outType, IFRTypes[ ii ] );
	    }
	    else {

		if( ii == 1 ) {
                strcat( outType, "/" );
		}
		else if( ii > 1 && firstVisType ) {
                    strcat( outType, " " );
		    firstVisType = False;
		}
		else  {
                    strcat( outType, "/" );
	        }

		strcat( outType, IFRTypes[ ii ] );
	    }
        } 
    }

}

/*======================================================================*/

void ctb_gfagid ( char haz[], char hazId[], int *iret )
/************************************************************************
 * ctb_gfagid                                                           *
 *                                                                      *
 * This routine gets the identifier of a hazard.                        *
 *                                                                      *
 * void ctb_gfagid ( haz, hazId, iret )                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *      haz             char[]          hazard type                     *
 *                                                                      *
 * Output parameters:                                                   *
 *      hazId           char[]          hazard id                       *
 *      *iret           int             return code                     *
 *                                      =  0: normal return             *
 *                                      = -1: no id found               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          06/06   Created                                 *
 ***********************************************************************/
{
    int ii, ier;
/*----------------------------------------------------------------------*/

    if ( !_gfaInit ) {

        ctb_gfard ( &ier );

    }

    for ( ii = 0; ii < gfaHazard.nhaz; ii++ ) {

        if ( strcasecmp ( haz, gfaHazard.haz[ ii ].hazard ) == 0 ) {

              strcpy ( hazId, gfaHazard.haz[ ii ].id );

              *iret = 0;
              return;

        }
    }

    *iret  = -1;

}

/*======================================================================*/

void ctb_gfaWorstIssue ( int nEl, char **elIssue,
                         char *worstIssue, int *iret )
/************************************************************************
 * ctb_gfaWorstIssue	                                       		*
 *                                                                      *
 * This function gets the worst issue type from a set of issue types.	*
 *									*
 * void ctb_gfaWorstIssue ( nEl, elIssue, worstIssue, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	nEL		int		number of elements in the array	*
 *	**elIssue	char		input GFA issue array		*
 *									*
 * Output parameters:                                                   *
 *	*worstIssue	char		worst issue status		*
 *	*iret		int		return code			*
 *					0 - normal			*
 *									*
 * Return parameters:                                             	*
 *    			None			       		 	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		09/06	Modified from pgsmear_getWorstIssue()	*
 * B. Yin/SAIC		11/06	Return if no input element.		*
 ***********************************************************************/
{
    int         ii;
                                                                          
    Boolean     allNew, someNew, allCan, someCan;
/*---------------------------------------------------------------------*/
    
    *iret = 0;
                                                                            
    worstIssue[ 0 ] = '\0';

    if ( !(nEl > 0 ) ) return;
                                                                         
    /* 
     *  If any of the input GFA is COR, return COR.
     */
    for ( ii = 0; ii < nEl; ii++ ) {
                                                                        
        if ( strcasecmp ( elIssue[ ii ], "COR" ) == 0 ) {
                                                                           
           strcpy ( worstIssue, "COR" );
           return;
                                                                        
        }
                                                                          
    }
                                                                           
    /* 
     *  If any of the input GFA is AMD, return AMD.
     */
    for ( ii = 0; ii < nEl; ii++ ) {
                                                                            
        if ( strcasecmp ( elIssue[ ii ], "AMD" ) == 0 ) {
                                                                             
           strcpy ( worstIssue, "AMD" );
           return;
                                                                          
        }
                                                                      
    }
                                                                        
    /*
     *  If all input GFAs are NEW, return NEW.
     *  If some of them are NEW, return AMD.
     */
    allNew  = True;
    someNew = False;
                                                                         
    for ( ii = 0; ii < nEl; ii++ ) {
                                                                          
	allNew = allNew && ( strcasecmp ( elIssue[ ii ], "NEW" ) == 0 );

	if (  strcasecmp ( elIssue[ ii ], "NEW" ) == 0 ) someNew = True;

    }

    someNew = someNew && !allNew;
                                                                        
    if ( allNew ) {
                                                                          
        strcpy ( worstIssue, "NEW" );
        return;
                                                                        
    }
    else if ( someNew )  {
                                                                        
        strcpy ( worstIssue, "AMD" );
        return;
                                                                           
    }
                                                                       
    /*
     *  If all input GFAs are CAN, return CAN.
     *  If some of them are CAN, return AMD.
     */
    allCan  = True;
    someCan = False;
                                                                         
    for ( ii = 0; ii < nEl; ii++ ) {

	allCan = allCan && ( strcasecmp ( elIssue[ ii ], "CAN" ) == 0 );

	if (  strcasecmp ( elIssue[ ii ], "CAN" ) == 0 ) someCan = True;
                                                                           
    }

    someCan = someCan && !allCan;
                                                                            
    if ( allCan ) {
                                                                     
        strcpy ( worstIssue, "CAN" );
        return;
                                                                       
    }
    else if ( someCan )  {
                                                                             
        strcpy ( worstIssue, "AMD" );
        return;
                                                                         
    }
                                                                      
    strcpy ( worstIssue, "NRML" );
                                                            
}

/*=====================================================================*/

void ctb_gfaWorstFL ( int topBottom, const char *level0,
		      const char *fzlBottom0, const char *fzlTop0,
		      const char *level1, const char *fzlBottom1,
		      const char *fzlTop1, char *worstFL, 
		      char *worstFzlBottom, char *worstFzlTop,
		      int *iret )
/************************************************************************
 * ctb_gfaWorstFL	                                          	*
 *                                                                      *
 * This function gets the worst flight level of two GFAs.		*
 *									*
 * void ctb_gfaWorstFL ( topBottom, level0, fzlBottom0,fzlTop0, 	*
 *		level1, fzlBottom1, fzlTop1, worstFL, worstFzlBottom, 	*
 *		worstFzlTop, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	topBottom	int		top level or bottom level	*
 *	*level0		const char	first flight level		*
 *	*fzlBottom0	const char	first FZL bottom		*
 *	*fzlTop0	const char	first FZL top			*
 *	*level1		const char	second flight level		*
 *	*fzlBottom1	const char	second FZL bottom		*
 *	*fzlTop1	const char	second FZL top			*
 *									*
 * Output parameters:                                                   *
 *	*worstFL	char		worst flight level		*
 *	*worstFzlBottom char		worst FZL bottom		*
 *	*worstFzlTop	char		worst FZL top			*
 *	*iret		int		return code			*
 *					 0:	normal			*
 *					-1:	both levels are bad	*
 *									*
 * Return parameters:                                             	*
 *    			None			       		 	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		09/06	Moved from pgsmear_getWorstFL()		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    *iret 		= 0;
    worstFL[ 0 ] 	= '\0';
    worstFzlBottom[ 0 ] = '\0';
    worstFzlTop[ 0 ] 	= '\0';

    /*
     *  If one input is bad and the other is good, set the worst to the
     *  good one and return. If both are bad, return -1.
     */
    if ( ( ( level0 == NULL ) || ( strlen ( level0 ) < (size_t)3 ) ) &&
         ( ( level1 != NULL ) && ( strlen ( level1 ) >=(size_t)3 ) ) ) {
   
       strcpy ( worstFL, level1 );
       return;

    }
    else if ( ( ( level1 == NULL ) || ( strlen ( level1 ) < (size_t)3 ) ) &&
              ( ( level0 != NULL ) && ( strlen ( level0 ) >=(size_t)3 ) ) ) {
		   
       strcpy ( worstFL, level0 );
       return;

    }
    else if ( ( ( level0 == NULL ) || ( strlen ( level0 ) < (size_t)3 ) ) &&
              ( ( level1 == NULL ) || ( strlen ( level1 ) < (size_t)3 ) ) ) {
		   
       *iret = -1;
       return;

    }

    /*
     *  If both levels are good, get the worst one.
     */
    if ( topBottom == TOP ) {

	if ( cvg_getFlghtLvl( level0 ) > cvg_getFlghtLvl( level1 ) ) {
	
	   strcpy ( worstFL, level0 );
	}
	else {

	   strcpy ( worstFL, level1 );
	}
    }
    else if ( topBottom == BOTTOM ) {

	if ( ( strcasecmp ( level0, "SFC" ) == 0 ) ||
	     ( strcasecmp ( level1, "SFC" ) == 0 ) ) {

	   strcpy ( worstFL, "SFC" );

	}
	else if ( ( strcasecmp ( level0, "FZL" ) == 0 ) &&
	          ( strcasecmp ( level1, "FZL" ) == 0 ) ) {
	  
	   strcpy ( worstFL, "FZL" );
	 
	   strcpy ( worstFzlTop, ( cvg_getFlghtLvl( fzlTop0 ) < cvg_getFlghtLvl( fzlTop1 ) ) ?  
	   		fzlTop1 : fzlTop0 );

	   strcpy ( worstFzlBottom, ( cvg_getFlghtLvl( fzlBottom0 ) < cvg_getFlghtLvl( fzlBottom1 ) ) ?  
	   		fzlBottom0 : fzlBottom1 );

	}
	else if ( ( strcasecmp ( level0, "FZL" ) == 0 ) &&
	          ( strcasecmp ( level1, "FZL" ) != 0 ) ) {
	   
	   if ( cvg_getFlghtLvl( level1 ) < cvg_getFlghtLvl( fzlBottom0 ) ) {

	      strcpy ( worstFL, level1 );

	   }
	   else {
	 
	      strcpy ( worstFL, "FZL" );
	      strcpy ( worstFzlBottom, fzlBottom0 );
	      strcpy ( worstFzlTop, fzlTop0 );

	   }

	}
	else if ( ( strcasecmp ( level0, "FZL" ) != 0 ) &&
	          ( strcasecmp ( level1, "FZL" ) == 0 ) ) {

	   if ( cvg_getFlghtLvl( level0 ) < cvg_getFlghtLvl( fzlBottom1 ) ) {

	      strcpy ( worstFL, level0 );

	   }
	   else {
	 
	      strcpy ( worstFL, "FZL" );
	      strcpy ( worstFzlBottom, fzlBottom1 );
	      strcpy ( worstFzlTop, fzlTop1 );

	   }

	}
	else if ( cvg_getFlghtLvl( level0 ) < cvg_getFlghtLvl( level1 ) ) {
			
	   strcpy ( worstFL, level0 );

	}
	else if ( cvg_getFlghtLvl( level0 ) >= cvg_getFlghtLvl( level1 ) ) {
			
	   strcpy ( worstFL, level1 );

	}
    }
}

/*======================================================================*/


void ctb_gfapCombineIFRTypes( const char *inType0, const char *inType1,
                        char *outType, int *iret )

/************************************************************************
 * ctb_gfapCombineIFRTypes                                              *
 *                                                                      *
 * This routine takes two GFA' IFR Type strings and puts them together  *
 * in a single string in the order specified in the gfa.tbl file.  It   *
 * does this by stepping through the complete list of IFR Types and     *
 * checking to see if each type appears in either of the input types.   *
 * If a particular type is found in either input string then it is      *
 * written out to the IFRType.                                          *
 *                                                                      *
 * Of the two inputs, inType0 and inType1, one may be a null string or  *
 * of zero length, but both cannot be null/zero length.                 *
 *                                                                      *
 * void ctb_gfapCombineIFRTypes ( inType0, inType1, outType , iret )    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *inType0        const char      first IFR type                  *
 *      *inType1        const char      second IFR type                 *
 *                                                                      *
 * Output parameters:                                                   *
 *      *outType        char            combination IFR type            *
 *      *iret           int             return code                     *
 *                                      =  0: normal return             *
 *                                      = -7: NULL or 0 length inputs   *
 *                                      = -9: gfa.tbl file not yet read *
 *                                      = -10:  no IFR in gfa.tbl       *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC      06/08   Modified from ctb_gfaCombineIFRTypes        *
 ***********************************************************************/
{
    int         ii, jj, idx = -1, nLoaded, num= 32;
    int         len0, len1;
    static int  ntypes = 0;
    static char **IFRTypes = (char **)NULL;
    char        *ptr;

    Boolean     firstVisType;
/*----------------------------------------------------------------------*/

    *iret = 0;
    outType[ 0 ] = '\0';

    if( !inType0 ) {
        len0 = 0;
    } else {
        len0 = strlen( inType0 );
    }

    if( !inType1 ) {
        len1 = 0;
    } else {
        len1 = strlen( inType1 );
    }

    /*
     *  One of the inputs can be 0 length, but return an error if both are.
     */
    if( len0 == 0 && len1 == 0 ) {
        *iret = -7;
        return;
    }


    /*
     *  If ntypes is 0, load the valid IFR types into the IFRTypes array.
     */
    if( ntypes == 0 ) {

        /*
         *  Verify the gfaHazard structure has been loaded; quit if it isn't.
         */
        if ( ! _gfaInit ) {
            *iret = -9;
            return;
        }

        /*
         *  Find the index number for C&V.
         */
        for( ii=0; ii< gfaHazard.nhaz; ii++ ) {
            if( strcasecmp( gfaHazard.haz[ii].hazard, "C&V" ) == 0 ) {
                idx = ii;
                break;
            }
        }

        /*
         *  An idx of < 0 should be impossible to reach but, just to be safe,
         *  we're going to check for it anyway, and exit if we have a bad value.
         */
        if( idx < 0 ) {
            *iret = -10;
            return;
        }

        /*
         *  Get a count of the number of IFR WX types. 
         */
        for( ii=0; ii< gfaHazard.haz[idx].ndesc; ii++ ) {

            if ( strcasecmp( gfaHazard.haz[idx].desc[ ii ].type, "POPUP" ) == 0 ) {

               for( jj=0; jj < gfaHazard.haz[idx].desc[ ii ].nchoices; jj++ ) {
                   if( strlen( gfaHazard.haz[ idx ].desc[ ii ].choice[ jj ] ) > 0 ) {
                       ntypes++;
                   }
               }
            }
        }

	/*
	 *  Add 4 CIG/VIS labels.
	 */
        ntypes += 4;

        /*
         *  Now allocate space and load the actual values into IFRTypes.
         */
        nLoaded = 0;
        G_MALLOC( IFRTypes, char *, ntypes, "ctb_gfaCombineIFRTypes: IFRTypes" );

        G_MALLOC( IFRTypes[ nLoaded ], char, num, "ctb_gfaCombineIFRTypes: IFRTypes" );
        strcpy( IFRTypes[ nLoaded ], "CIG BLW 010" );
        nLoaded++;

        G_MALLOC( IFRTypes[ nLoaded ], char, num, "ctb_gfaCombineIFRTypes: IFRTypes" );
        strcpy( IFRTypes[ nLoaded ], "CIG 010-030" );
        nLoaded++;

        G_MALLOC( IFRTypes[ nLoaded ], char, num, "ctb_gfaCombineIFRTypes: IFRTypes" );
        strcpy( IFRTypes[ nLoaded ], "VIS BLW 3SM" );
        nLoaded++;

        G_MALLOC( IFRTypes[ nLoaded ], char, num, "ctb_gfaCombineIFRTypes: IFRTypes" );
        strcpy( IFRTypes[ nLoaded ], "VIS 3-5SM" );
        nLoaded++;

        for( ii=0; ii< gfaHazard.haz[idx].ndesc; ii++ ) {

          if ( strcasecmp( gfaHazard.haz[idx].desc[ ii ].type, "POPUP" ) == 0 ) {

            for( jj=0; jj < gfaHazard.haz[idx].desc[ ii ].nchoices; jj++ ) {
                if( strlen( gfaHazard.haz[ idx ].desc[ ii ].choice[ jj ] ) > 0 ) {
                    G_MALLOC( IFRTypes[ nLoaded ], char, num, "ctb_gfaCombineIFRTypes: IFRTypes" );
                    strcpy( IFRTypes[ nLoaded ], gfaHazard.haz[idx].desc[ii].choice[jj] );
                    nLoaded++;
                }
            }
          }
        }

        /*
         *  Remove all underscores and replace with blanks.
         */
        for( ii=0; ii< nLoaded; ii++ ) {
            for( jj=0; (unsigned int)jj < (strlen( IFRTypes[ii]) ); jj++ ) {
                if( IFRTypes[ii][jj] == '_' ) {
                    IFRTypes[ii][jj] = ' ';
                }
            }
        }

    }  /* if types == 0 */


    /*
     *  Loop though the list of IFRTypes.  If an individual type is found in
     *  either of the input strings, then write it to the output string.
     */
    firstVisType = True;
    for( ii=0; ii<ntypes; ii++ ) {

        ptr = NULL;
        if( len0 > 0 ) {
            ptr = strstr( inType0, IFRTypes[ii] );
        }

        if( ptr == NULL && len1 > 0 ) {
            ptr = strstr( inType1, IFRTypes[ ii ] );
        }

        if( ptr != NULL ) {

            if( strlen( outType ) <= 0 ) {

                strcpy( outType, IFRTypes[ ii ] );
            }
            else {

                if( ii == 2 || ii == 3 ) {

		  /*
		   * Add / before VIS types.
		   */
                  strcat( outType, "/" );

                }
                else if( ii > 1 && firstVisType ) {

                    strcat( outType, " " );
                    firstVisType = False;

                }
                else  {

                    strcat( outType, "/" );
                }

                strcat( outType, IFRTypes[ ii ] );
            }
        }
    }

}

/*===========================================================================*/
void ctb_gfapCombineMtnObscTypes( const char *inType0, const char *inType1,
                           char *outType, int *iret )
/******************************************************************************
 This routine takes two GFA' MT_OBSC Type strings and puts them together.
 Its functionality is based from the functionality of ctb_gfapCombineIFRTypes.
 
 void ctb_gfapCombineMtnObscTypes ( inType0, inType1, outType, iret )
 
 Input parameters:
   *inType0        const char      first MT_OBSC type
   *inType1        const char      second MT_OBSC type
 Output parameters:
   *outType        char            combination MT_OBSC type            
   *iret           int             return code                     
                                       =  0: normal return             
                                       = -7: NULL or 0 length inputs   
                                       = -9: gfa.tbl file not yet read 
                                       = -10:  no MT_OBSC in gfa.tbl
 Log:
 L. Hinson/AWC    09/09  Modelled after ctb_gfapCombineIFRTypes
*******************************************************************************/ 
{
    int         ii, jj, idx = -1, nLoaded, num = 32;
    int         len0, len1;
    static int  ntypes = 0;
    static char **MtnObscTypes = (char **)NULL;
    char        *ptr;
    *iret = 0;
    outType[ 0 ] = '\0';
    if ( !inType0 ) {
      len0 = 0;
    } else {
      len0 = strlen( inType0);
    }
    
    if( !inType1 ) {
        len1 = 0;
    } else {
        len1 = strlen( inType1 );
    }
    /*
     *  One of the inputs can be 0 length, but return an error if both are.
     */
    if( len0 == 0 && len1 == 0 ) {
        *iret = -7;
        return;
    }    
    /*
     *  If ntypes is 0, load the valid IFR types into the IFRTypes array.
     */
    if( ntypes == 0 ) {
       /*
         *  Verify the gfaHazard structure has been loaded; quit if it isn't.
         */
        if ( ! _gfaInit ) {
            *iret = -9;
            return;
        }
        /*
         *  Find the index number for C&V.
         */
        for( ii=0; ii< gfaHazard.nhaz; ii++ ) {
            if( strcasecmp( gfaHazard.haz[ii].hazard, "MT_OBSC" ) == 0 ) {
                idx = ii;
                break;
            }
        }
        /*
         *  An idx of < 0 should be impossible to reach but, just to be safe,
         *  we're going to check for it anyway, and exit if we have a bad value.
         */
        if( idx < 0 ) {
            *iret = -10;
            return;
        }
        /*
         *  Get a count of the number of potential MTN OBSC types. 
         */
        for( ii=0; ii< gfaHazard.haz[idx].ndesc; ii++ ) {

            if ( strcasecmp( gfaHazard.haz[idx].desc[ ii ].type, "POPUP" ) == 0 ) {

               for( jj=0; jj < gfaHazard.haz[idx].desc[ ii ].nchoices; jj++ ) {
                   if( strlen( gfaHazard.haz[ idx ].desc[ ii ].choice[ jj ] ) > 0 ) {
                       ntypes++;
                   }
               }
            }
        }
        /*
         *  Now allocate space and load the actual values into MtnObscTypes.
         */
        nLoaded = 0;
        G_MALLOC( MtnObscTypes, char *, ntypes, "ctb_gfaCombineMtnObscTypes: MtnObscTypes" );
        for( ii=0; ii< gfaHazard.haz[idx].ndesc; ii++ ) {

          if ( strcasecmp( gfaHazard.haz[idx].desc[ ii ].type, "POPUP" ) == 0 ) {

            for( jj=0; jj < gfaHazard.haz[idx].desc[ ii ].nchoices; jj++ ) {
                if( strlen( gfaHazard.haz[ idx ].desc[ ii ].choice[ jj ] ) > 0 ) {
                    G_MALLOC( MtnObscTypes[ nLoaded ], char, num, "ctb_gfaCombineMtnObscTypes: MtnObscTypes" );
                    strcpy( MtnObscTypes[ nLoaded ], gfaHazard.haz[idx].desc[ii].choice[jj] );
                    nLoaded++;
                }
            }
          }
        }
        /*
         *  Remove all underscores and replace with blanks.
         */
        for( ii=0; ii< nLoaded; ii++ ) {
            for( jj=0; (unsigned int)jj < (strlen( MtnObscTypes[ii]) ); jj++ ) {
                if( MtnObscTypes[ii][jj] == '_' ) {
                    MtnObscTypes[ii][jj] = ' ';
                }
            }
        }
   
     } /* if types == 0 */
     
     /*
      * Loop through the list of MtnObscTypes.
      */
    for( ii=0; ii<ntypes; ii++ ) {

        ptr = NULL;
        if( len0 > 0 ) {
            ptr = strstr( inType0, MtnObscTypes[ii] );
        }

        if( ptr == NULL && len1 > 0 ) {
            ptr = strstr( inType1, MtnObscTypes[ ii ] );
        }

        if( ptr != NULL ) {

            if( strlen( outType ) <= 0 ) {
                sprintf( outType, "MTNS OBSC BY %s", MtnObscTypes[ ii ] );
            }
            else {
                strcat( outType, "/" );
                strcat( outType, MtnObscTypes[ ii ] );
            }
        }
    }

}

/*======================================================================*/

G_Boolean ctb_gfaHasCV ( void )
/************************************************************************
 * ctb_gfaHasCV		                                                *
 *                                                                      *
 * This routine checks if C&V is in gfa.tbl			 	*
 *                                                                      *
 * G_Boolean ctb_gfagtemp ( void )					* 
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * 	None                                                            *
 *                                                                      *
 * Return Value:                    	                                *
 *	True	if C&V is in gfa.tbl     				*
 *	False	if C&V is not in gfa.tbl     				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		06/08	Created 	                        *
 ***********************************************************************/
{
    int ii;
/*----------------------------------------------------------------------*/


    for( ii=0; ii< gfaHazard.nhaz; ii++ ) {

       if( strcasecmp( gfaHazard.haz[ii].hazard, "C&V" ) == 0 ) {

	   return True;
       }
    }

    return False;

}

