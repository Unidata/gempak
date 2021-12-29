#include "afcmn.h"


/************************************************************************
 * affmtxml.c                                             		*
 *                                                                      *
 * This module contains all the subroutines used to format the xml	*
 * string, which is used to produce the airmet bulletin.		*
 *                                                                      *
 * CONTENTS:                                                            *
 *   public( to the library) functions:                                 *
 *      af_fmt2xml		- generate information string           *
 *				  in the fromline string		*
 *                                                                      *
 *   private functions:							*
 *      af_catStr		- add string to the end of first string	*
 *      af_addTags		- form an XML string with the given tag	*
 *	af_overrideIssueTm	- determine if input issue time should  *
 *				  be used or overridden			*
 *      af_getMinBase		- determine the minimum FL base		*
 *      af_findExternalFzlvls	- Find top/base for a FA area that is   *
 * 				  not intersected by any FZLVL contours	*
 *    af_getFirstLastIssueTimes - Get first and last issue times	*
 ***********************************************************************/


/*
 *  Private functions
 */
static void af_catStr		( char **str1, char *str2 );

static void af_addTags		( VG_DBStruct *el, char *xmlTag, char *gfaTag, 
			 	 char **outStr );

static void af_getMinBase 	( const char *base1, const char *base2, 
			          char *min, int *iret );

static void af_findExternalFzlvls ( const GFA_Elem_Format *fmt_in, int nfmt,
                               float xCentroid, float yCentroid,
                               char *top, char *base, int *iret );

static Boolean af_getFzlRange ( VG_DBStruct *el, char area[], 
				 int *top, int *bottom );

static void af_getFirstLastIssueTimes ( int *firstIssueTm, int *lastIssueTm, 
					char *firstCycle, char *lastCycle,
					int *iret );

/* 
 *  Global variables
 */
static long 	_capacity = ONEBLOCK;



/*
 *  Public functions
 */

void af_fmt2xml ( int nareas, char areas[][ 8 ], int ntypes, 
			char *types[3], char *day, char *cycle, int nin, 
                        GFA_Elem_Format *fmt_in, 
                        const float *xCentroid, const float *yCentroid,
                        char *string[NUM_FA_AREAS][NUM_TYPES],
                        int *iret )
/************************************************************************
 * af_fmt2xml                                                     	*
 *                                                                      *
 * This routine takes a GFA format structure array as input and	 	*
 * generates an information string (xml format), which can be used to 	*
 *  create bulletin.							*
 *                                                                      *
 *  Note: "string" is allocated in this routine and must be freed by	*
 *	  the caller.                                                   *
 *                                                                      *
 * void af_fmt2xml (area, type, day, cycle, nin, fmt_in, 		*
 *			   xCentroid, yCentroid, string, iret)		*
 *                                                                      *
 * Input parameters:                                                    *
 *	nAreas		int		number of FA areas		*
 *	areas[][8]	char		array of area strings		*
 *	nTypes		int		number of hazard types 		*
 *	types[][8]	char		array of hazard type strings	*
 *      *day            char            day                             *
 *      *cycle		char		cycle				*
 *      *nin		int		number of input elements	*
 *      *fmt_in		GFA_Elem_Format  array of GFA format structure	*
 *	*xCentroid	float		x array of FA area centroids	*
 *	*yCentroid	float		y array of FA area centroids	*
 *                                                                      *
 * Output parameters:                                                   *
 *      **string	char		'<tag>value</tag>' string	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 *					-1: no issue time		*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          10/04   Created                         	*
 * B. Yin/SAIC          11/04   Format only smear GFAs          	*
 *                               Keep only states in FA_area file	*
 * E. Safford/SAIC	02/05	add multiple ReferToSigmet 		*
 * E. Safford/SAIC	03/05	put Type value (ICE) in DUE TO element	*
 * E. Safford/SAIC	04/05	"FL" only if >= 180, change BTWN to BTN *
 * E. Safford/SAIC	04/05	use al_cleanFromLine to rm dup points	*
 * B. Yin/SAIC		06/05	change SMEAR to GFA_USER_SMEAR and	*
 *				GFA_SYSTEM_SMEAR			*
 * E. Safford/SAIC	07/05	rm update number from string 		*
 * J. Wu/SAIC           07/05   change input to GFA format structure    *
 * B. Yin/SAIC		07/05	call ctb_airmetGetIssueTm to get issueTm*
 * E. Safford/SAIC	07/05	add G. Lakes & coastal waters to stList	*
 * L. Hinson/AWC        09/05   Added day argument to be used instead   *
 *                                of localtime                          *
 * H. Zeng/SAIC	       	09/05	added overrideIssueTm			*
 * H. Zeng/SAIC	       	09/05	checked the str length of stList	*
 * H. Zeng/SAIC	       	09/05	added lat/lon info into xml str 	*
 * B. Yin/SAIC	       	10/05	use separator dash for LLWS     	*
 * J. Wu/SAIC          	10/05   skip the deleted format structures	*
 * B. Yin/SAIC		10/05	check which states are affected by a	*
 *				water only airmet			*
 * B. Yin/SAIC		11/05	reset capacity for new string		*
 * J. Wu/SAIC          	11/05   process multiple freezing levels  	*
 * E. Safford/SAIC	12/05	fix schema reference, add outlooks	*
 * E. Safford/SAIC	12/05	M_FZLVL uses separator dash       	*
 * E. Safford/SAIC	12/05	allow NEW to override issue time  	*
 * E. Safford/SAIC	12/05	use af_fmtStateList()                   *
 * E. Safford/SAIC	12/05	use af_overrideIssueTm()		*
 * E. Safford/SAIC	01/06	param change for af_fmtStateList	*
 * B. Yin/SAIC		01/06	add LEVEL fro FZLVL			*
 * E. Safford/SAIC	02/06	use level in calculating freezingRange	*
 * B. Yin/SAIC		02/06	rename TURB-HI/LO to TURB		*
 * E. Safford/SAIC	02/06	do not process LLWS outlooks, allow     *
 *				 From line of > 5 pts for closed FZLVL  *
 * E. Safford/SAIC	02/06	don't process FZLVL or M_FZLVL outlooks *
 * E. Safford/SAIC	02/06	fix sgi compiler warning w/ VLAs	*
 * E. Safford/SAIC	03/06	FZLVL use fromType -1 avoid pt reorder  * 
 * B. Yin/SAIC		03/06	handle no FZLVL in FA areas condition	*
 * J. Wu/SAIC           03/06   append condition wording to XML string  *
 * E. Safford/SAIC	04/06   correct condition wording location, rm  *
 *				  conditions element from xml		*
 * E. Safford/SAIC	04/06	don't create xml if state list is empty *
 * E. Safford/SAIC	05/06	add fzlBase/Top to xml stream           *
 * E. Safford/SAIC      05/06   fix retrieval of "DUE TO"		*
 * E. Safford/SAIC      06/06   fix point order on closed FZLVL		*
 * E. Safford/SAIC	07/06   fix FZLVL format issues    		*
 * B. Yin/SAIC          07/06   change subtype (haz and category types) *
 * D.W.Plummer/NCEP	07/06	Remove closed check for FZLVL formatting*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * J. Wu/SAIC		10/06	move af_cleanFromLine & af_getNextVor	*
 *				to clo lib for new point reduction alg.	*
 * B. Yin/SAIC          01/07   retrieve range info from FZLVL elem.    *
 * B. Yin/SAIC          02/07   Fixed the issue time day boundary bug.  *
 * D.W.Plummer/NCEP	03/07	Pass along <reduceFlgPts>		*
 * D.W.Plummer/NCEP	04/07	Correct bug to pass along <reduceFlgPts>*
 * B. Yin/SAIC          05/07   Fixed the until time month boundary bug.*
 * B. Yin/SAIC		07/07	Added the tag number into output strings*
 * S. Jacobs/NCEP	10/10	Added check for closed FZLVL lines	*
 ***********************************************************************/
{
    int         ii, jj, mm, icycle, nHaz, ier, iarea, itype;
    int		topInt = -1, strSize, closed;
    int		npts, issueTm1, issueTm2;
    int         gfaSubType = GFA_INIT, fromType, areaIndex;
    int		rangeTop, rangeBottom;
    char	hazList[ STD_STRLEN ], cat[ 32 ];
    char	timeStr[ 16 ], otlkStr[16], tmpStr[ STD_STRLEN ];
    char	top[ 16 ], bottom[ 16 ], fmLineStr[ STD_STRLEN * 2 ];
    char	cleanFmLineStr[ STD_STRLEN * 2 ];
    char	bottomMin[4] = "", newBottom[4] = "";
    char	level[ 16 ], adjLevel[ 16 ];
    
    char	newStateList[ STD_STRLEN ] = "";
    char	airmetXsd[ FILE_FULLSZ ], hazardType[ STD_STRLEN ] = "";
    char	fzlTop[ STD_STRLEN ], fzlBase[ STD_STRLEN ];
    char	firstCycle[ 32 ], lastCycle[ 32 ], airmetTag[ 32 ];
    char	cval[ 32 ];

    long 	flen;

    time_t	tt;
    struct tm	*localTm;
    
    Boolean	overrideIssueTm;
    Boolean	useFromLine = True;
    Boolean	isSmear = False;
    Boolean	foundM_FZLVL[ NUM_FA_AREAS ];
    Boolean	foundFZLVL[ NUM_FA_AREAS ];
    Boolean	findRange, addAirmetTag;
    
    VG_DBStruct *el;
/*---------------------------------------------------------------------*/

    /*
     *  locate the airmet.xsd schema file either locally or in the 
     *  default location.  This must be a full path specification
     *  containing no aliases.
     */
    cfl_tinq( "airmet.xsd", XSD, &flen, airmetXsd, &ier );

    /*
     * Initialize addAirmetTag
     */
    addAirmetTag = True;
    ctb_pfstr ( "ADD_AIRMET_TAG", cval, &ier );
    if ( ier >= 0 ) {
        if( strcasecmp( cval, "TRUE" ) != 0 ) {
            addAirmetTag = False;
        }
    }

    for ( iarea = 0; iarea < nareas; iarea++ ) { 

        foundM_FZLVL[ iarea ] = False;
        foundFZLVL[ iarea ] = False;

	bottomMin[ 0 ] = '\0';
        topInt = 0;


        for ( itype = 0; itype < ntypes; itype++ ) { 

	    overrideIssueTm = False;
            overrideIssueTm = af_overrideIssueTm( areas[iarea], types[itype],
	    			nin, fmt_in, &ier );

            strSize = ONEBLOCK;
	    _capacity = ONEBLOCK;
            G_MALLOC ( string[iarea][itype], char, strSize, "af_format string" );

            sprintf( string[iarea][itype], "%s", XML_HDR );

            af_catStr ( &string[iarea][itype], "<gfaInfo " );

	    /*
	     *  Add the schema file reference
	     */
            af_catStr ( &string[iarea][itype], SCM_FILE );
	    af_catStr ( &string[iarea][itype], "\"file:");
	    af_catStr ( &string[iarea][itype], airmetXsd);

            af_catStr ( &string[iarea][itype], "\"><hdr><faArea>" );
            af_catStr ( &string[iarea][itype], areas[iarea] );
            af_catStr ( &string[iarea][itype], "</faArea>" );

            /* 
             * Get current local time and issue time
             */
            tt = time ( NULL );
            localTm = localtime ( &tt );

	    af_getIssueTm ( cycle, day, overrideIssueTm, timeStr, &ier );

            af_catStr ( &string[iarea][itype], "<issueTime>" );
            af_catStr ( &string[iarea][itype], timeStr );
            af_catStr ( &string[iarea][itype], "</issueTime>" );

            /*
             * Get until time
             */
            icycle = atoi ( cycle );

            if ( ( icycle < 0 ) || ( icycle > 23 ) ) {
               sprintf ( timeStr, "xxxxxx" );
               sprintf ( otlkStr, "xxxxxx" );
            }
            else {

	       /*
                *  For the first day of each month, if current time is between 0 and 
		*  3 (first cycle) and an AIRMET or OUTLOOK is updated for the last 
		*  cycle(the last day of previous month), the month used to calculate
		*  the 'until time' should be the previous month.   
		*/
               af_getFirstLastIssueTimes ( &issueTm1, &issueTm2, firstCycle, 
	       				   lastCycle, &ier );
                                                                                 
               if ( ( localTm->tm_mday == 1 ) && ( localTm->tm_hour < atoi(firstCycle) )
                    && ( atoi( day ) >= 28 ) && ( icycle == atoi(lastCycle) ) ) {
                                                                                                                 
                  if ( localTm->tm_mon == 0 ) {
                                                                                                                 
                     localTm->tm_year--;
                     localTm->tm_mon = 11;
                                                                                                                 
                  }
                  else {
                                                                                                                 
                    localTm->tm_mon--;
                                                                                                                 
                  }
                                                                                                                 
               }

               localTm->tm_mday = atoi(day);
               localTm->tm_hour = icycle;
               localTm->tm_min  = 0;
               tt = mktime ( localTm ) + 6*60*60;
               localTm = localtime ( &tt );
               sprintf ( timeStr, "%02d%02d%02d", localTm->tm_mday,
           	 localTm->tm_hour, localTm->tm_min );

	       /* 
	        *  calculate the end of the outlook
		*/
	       tt = mktime( localTm ) + 6*60*60;
	       localTm = localtime( &tt );
	       sprintf( otlkStr, "%02d%02d%02d", localTm->tm_mday,  
           	 localTm->tm_hour, localTm->tm_min );
            }

            af_catStr ( &string[iarea][itype], "<untilTime>" );
            af_catStr ( &string[iarea][itype], timeStr );
            af_catStr ( &string[iarea][itype], "</untilTime>" );

            af_catStr ( &string[iarea][itype], "<outlookEndTime>" );
            af_catStr ( &string[iarea][itype], otlkStr );
            af_catStr ( &string[iarea][itype], "</outlookEndTime></hdr>" );

            /*
             *  Loop over all GFA format structures to find matching structures
             *  (FA area and airmat type) and extract information needed.
             *
             */
            for ( ii = 0; ii < nin; ii++ ) {

	        el = &fmt_in[ ii ].el;

                /*
	          *  Skip the element if it is marked as deleted
                  */
		if ( fmt_in[ii].delete ) {
		    continue;
	        }

                
		/*
	          *  Skip the element if it is not the same area type as input type
                  */
		if ( strcasecmp( areas[iarea], fmt_in[ii].area ) != 0 ) {
	            continue;
	        }
        
	
                /*
	         *  Skip the element if it is not the same hazard type as input type
		 */
	        ctb_gfaghaz ( ";", &nHaz, hazList, &ier );

	        if ( nHaz == 0 ) {
	           ctb_gfard ( &ier );
	        }
	
	        cvg_getFld ( el, TAG_GFA_AREATYPE, hazList, &ier );
	        ctb_gfagcat ( hazList, cat, &ier );

                if ( strcasecmp( types[itype], cat ) != 0 ) { 
	            continue;
	        }	
	
                /*
	          *  Extract information from matching elements.
                  */	 	 	
	        if ( (int)el->hdr.vg_type == GFA_ELM &&
    	             !el->hdr.delete )  {

		    /*
		     *  set the isSmear flag (False indicates outlook)
		     */
		    isSmear = True;
	            cvg_getFld ( el, TAG_GFA_SUBTYPE, tmpStr, &ier );
                    gfaSubType = atoi( tmpStr ) - atoi( tmpStr ) / 10 * 10;

	            if ( ( gfaSubType == GFA_USER_OUTLOOK ) ||
	                 ( gfaSubType == GFA_SYSTEM_OUTLOOK ) ) {
			isSmear = False;
		    }


                   /*
		    *  LLWS can never produce an outlook.  Skip formatting the element
		    *  if that's the combination that's found.
		    *
		    *  Similarly a FZLVL or M_FZLVL outlook has no meaning --
		    *  there is no outlook section of the freezing level 
		    *  paragraph in the Zulu airmet.
		    */
                    if( !isSmear && strcasecmp( hazList, "LLWS" ) == 0 ) continue;
		    if( !isSmear && strcasecmp( hazList, "FZLVL" ) == 0 ) continue;
		    if( !isSmear && strcasecmp( hazList, "M_FZLVL" ) == 0 ) continue;

                    /*
	             *  Check for M_FZLVL and FZLVL
                     */
	            if ( !foundM_FZLVL[ iarea ] && 
		    	               strcasecmp( hazList, "M_FZLVL" ) == 0 ) { 
	                foundM_FZLVL[ iarea ] = True;
	            }
		    else if( !foundFZLVL[ iarea ] && 
		    	               strcasecmp( hazList, "FZLVL" ) == 0 ) {
		        foundFZLVL[ iarea ] = True;
		    }

		    /*
		     *  Get the hazard type
		     */
 		    cvg_getFld( el, TAG_GFA_AREATYPE, hazardType, &ier );

                    /*
                     *  Get the formatted State list.  If the state list is
                     *  empty then skip this hazard altogether.
                     */
                    af_fmtStateList( hazardType, el->elem.gfa.info.npts,
                                 el->elem.gfa.latlon,
                                 &el->elem.gfa.latlon[ el->elem.gfa.info.npts ],
                                 areas[iarea], fmt_in[ ii ].adjarea,
                                 newStateList, &ier );

		    if( ier < 0 || strlen( newStateList ) <= 0 ) {
                        continue;
                    }


                    /*
                     *  Start xml string
                     */
		    if( isSmear ) {
                        af_catStr ( &string[iarea][itype], "<smear>" );
		    } 
		    else {
                        af_catStr ( &string[iarea][itype], "<outlook>" );
                    }

		    /*
		     *  Add hazard type to xml string  
		     */
                    cvg_getFld( el, TAG_GFA_AREATYPE, hazardType, &ier );
		    
		    af_TURBRenameHILO ( hazardType );

		    af_catStr( &string[iarea][itype], "<hazard>" ); 
		    af_catStr( &string[iarea][itype], hazardType ); 
		    af_catStr( &string[iarea][itype], "</hazard>" ); 

		    /*
	             *  Add State list to xml string 
		     */
                    af_fmtStateList( hazardType, el->elem.gfa.info.npts,
	     		         el->elem.gfa.latlon,
			         &el->elem.gfa.latlon[ el->elem.gfa.info.npts ],
	                         areas[iarea], fmt_in[ ii ].adjarea, 
			         newStateList, &ier );
           
		    if( ier >= 0 ) {
	            af_catStr ( &string[iarea][itype], "<stateList>" );
	                af_catStr ( &string[iarea][itype], newStateList );
                        af_catStr ( &string[iarea][itype], "</stateList>" );
		    }
  

		    /*
                     * Lat/Lon & Close Flag info
                     */
		    af_catStr ( &string[iarea][itype], "<closeFlg>" );
  		    sprintf( tmpStr, "%d", (int)(el->hdr.closed) ); 
		    af_catStr ( &string[iarea][itype], tmpStr );
		    af_catStr ( &string[iarea][itype], "</closeFlg>" );

		    af_catStr ( &string[iarea][itype], "<nLatLonPts>" );
		    sprintf( tmpStr, "%d", el->elem.gfa.info.npts );
		    af_catStr ( &string[iarea][itype], tmpStr );
		    af_catStr ( &string[iarea][itype], "</nLatLonPts>" );

		    af_catStr ( &string[iarea][itype], "<latPts>" );

		    npts = el->elem.gfa.info.npts;
		    tmpStr[0] = '\0';

		    for ( jj = 0; jj < npts; jj++ ) {

		      sprintf( &(tmpStr[strlen(tmpStr)]), "%-8.4f ", 
			       el->elem.gfa.latlon[jj]);
                    }
		    tmpStr[strlen(tmpStr)-1] = '\0';
                    
		    af_catStr ( &string[iarea][itype], tmpStr );
		    af_catStr ( &string[iarea][itype], "</latPts>" );

		    af_catStr ( &string[iarea][itype], "<lonPts>" );

		    npts = el->elem.gfa.info.npts;
		    tmpStr[0] = '\0';

		    for ( jj = npts; jj < 2*npts; jj++ ) {

		      sprintf( &(tmpStr[strlen(tmpStr)]), "%-9.4f ", 
			       el->elem.gfa.latlon[jj]);
                    }
		    tmpStr[strlen(tmpStr)-1] = '\0';

		    af_catStr ( &string[iarea][itype], tmpStr );
		    af_catStr ( &string[iarea][itype], "</lonPts>" );

                    /*
 		     * Also send along the reduceFlg flag for diagnostic plotting
 		     */
                    af_catStr ( &string[iarea][itype], "<reduceFlgPts>" );

		    if ( fmt_in[ ii ].reduceFlg != (int *)NULL )  {


                        npts = el->elem.gfa.info.npts;
                        tmpStr[0] = '\0';

                        for ( jj = 0; jj < npts; jj++ ) {
			    sprintf( &(tmpStr[strlen(tmpStr)]), "%d ", fmt_in[ ii ].reduceFlg[jj] );
                        }
                        tmpStr[strlen(tmpStr)-1] = '\0';

                        af_catStr ( &string[iarea][itype], tmpStr );

		    }

                    af_catStr ( &string[iarea][itype], "</reduceFlgPts>" );


	            /*
               	     *  From line 
		     *    Use a From line for most smears. LLWS, M_FZLVL
		     *    closed FZLVLs and all outlooks use a BOUNDED BY line.
		     *    
		     *    For open FZLVL only, do no point reordering when 
		     *    generating the From line.
	             */
		    useFromLine = True;
                    fromType = SIGTYP_AREA; 

		    if( isSmear ) {
    		        cvg_getFld ( el, TAG_GFA_AREATYPE, tmpStr, &ier );
		        if ( strcasecmp ( tmpStr, "LLWS" ) == 0 ||
			     strcasecmp ( tmpStr, "M_FZLVL") == 0 ||
			     strcasecmp ( tmpStr, "FZLVL") == 0 ) {
		           useFromLine = False;
		        }

			closed = fmt_in[ii].openLine ? 0 : 1;
			if( strcasecmp( tmpStr, "FZLVL" ) == 0 && !closed ) {
  			    fromType = NO_POINT_REORDER; 
			}
		    }
		    else {		/* outlook, not a smear */
			useFromLine = False;
		    }

	            fmLineStr[ 0 ] = '\0';
	            if ( el->elem.gfa.info.npts > 0 ) {
			if ( useFromLine ) {
	                   clo_from ( GFA_ELM, fromType, el->elem.gfa.info.npts,
	      	                   4, el->elem.gfa.latlon,
			           &el->elem.gfa.latlon[ el->elem.gfa.info.npts ], 
			           sizeof ( fmLineStr ), fmLineStr, &ier );
	                   clo_cleanFmLine( fmLineStr, FROM_LINE, cleanFmLineStr, &ier );
			}
			else {
	                   clo_from ( GFA_ELM, fromType, el->elem.gfa.info.npts,
	      	                   5, el->elem.gfa.latlon,
			           &el->elem.gfa.latlon[ el->elem.gfa.info.npts ], 
			           sizeof ( fmLineStr ), fmLineStr, &ier );
	                   clo_cleanFmLine( fmLineStr, BOUNDED_BY_LINE, cleanFmLineStr, &ier );
			}


                        af_catStr ( &string[iarea][itype], "<fromLine>" );
                        af_catStr ( &string[iarea][itype], cleanFmLineStr );
                        af_catStr ( &string[iarea][itype], " </fromLine>" );
	            }

	            /*
	             *  Frequency, Severity
	             */
	            af_addTags ( el, "<Frequency>", "<Frequency>", &string[iarea][itype] );
	            af_addTags ( el, "<Severity>", "<Severity>", &string[iarea][itype]);

	            /* 
	             *  Top/Bottom 
	             */
	            cvg_getFld ( el, TAG_GFA_TOP, top, &ier );

		    if ( ier == 0 ) {
	                sprintf ( tmpStr, "<Top>%s</Top>", top );
	                af_catStr ( &string[iarea][itype], tmpStr );
	            
		        if ( strcasecmp( hazList, "M_FZLVL" ) == 0 ) { 
	                    topInt = G_MAX ( atoi ( top ), topInt );
	                }
	            }

	            cvg_getFld ( el, TAG_GFA_BOTTOM, bottom, &ier );
	            if ( ier == 0 ) {
	                sprintf ( tmpStr, "<Base>%s</Base>", bottom );
	                af_catStr ( &string[iarea][itype], tmpStr );
		        
			if ( strcasecmp( hazList, "M_FZLVL" ) == 0 ) { 
			    if( strlen( bottomMin ) > (size_t) 0 ) {

                                af_getMinBase( bottomMin, bottom, newBottom, &ier );
				if( ier == 0 ) {
				    strcpy( bottomMin, newBottom );
	                        }        
	                    }
	                    else {
				strcpy( bottomMin, bottom );
			    }
	                }
	            }

		    /*
		     *  Add the freezing level base and top if they exist.
		     */                   
		    cvg_getFld( el, TAG_GFA_FZL_TOP, fzlTop, &ier );
		    if( ier == 0 ) {
			sprintf( tmpStr, "<FzlTop>%s</FzlTop>", fzlTop );
			af_catStr( &string[iarea][itype], tmpStr );
		    }

		    cvg_getFld( el, TAG_GFA_FZL_BOTTOM, fzlBase, &ier );
		    if( ier == 0 ) {
			sprintf( tmpStr, "<FzlBase>%s</FzlBase>", fzlBase );
			af_catStr( &string[iarea][itype], tmpStr );
		    }


		    /*
		     *  Level
		     *
		     *  Note that determining an FA area's top and bottom 
		     *  freezing levels using the level from a FZLVL hazard
		     *  means that the actual freezing level may range from 
		     *  040 feet below and 040 feet above the FZLVL.  To 
		     *  take that into account we add/subtract 040 from the
		     *  level when determining the bottomMin and topInt.
		     */ 
	            cvg_getFld ( el, "Level", level, &ier );
	            if ( ier == 0 ) {
                        
	                sprintf ( tmpStr, "<Level>%s</Level>", level );
	                af_catStr ( &string[iarea][itype], tmpStr );

			if( strcasecmp( hazList, "FZLVL" ) == 0 ) {

			    if ( af_getFzlRange( el, areas[ iarea], &rangeTop, &rangeBottom ) ) {
 					
				topInt = rangeTop;
				sprintf( bottomMin, "%03i", rangeBottom );

			    }
 			    else {
			    	if( strcmp( level, "SFC") != 0 && 
						atoi( level ) >= FOUR_THOUSAND ) {
			        	sprintf( adjLevel, "%03i", 
				    			atoi( level ) - FOUR_THOUSAND );
			    	}
			    	else {
			        	strcpy( adjLevel, level );
			    	}


			    	if( strlen( bottomMin ) > (size_t) 0 ) {

                                	af_getMinBase( bottomMin, adjLevel, 
							newBottom, &ier );
					if( ier == 0 ) {
				    		strcpy( bottomMin, newBottom );
	            			}        
	            	    	}
	                    	else {
					strcpy( bottomMin, adjLevel );
			    	}
			

			    	if( strcasecmp( level, "SFC" ) != 0 ) {
			        	topInt = G_MAX ( 
				    		(atoi( level )+ FOUR_THOUSAND), topInt ); 
			    	}
			    	else {
			        	topInt = G_MAX( FOUR_THOUSAND, topInt ); 
			    	}
			    }
			}
		    }

	            /* 
	             *  Some GFEs have a "DUE TO" field.  But Ice uses a Type field.  
	             *  If DUE_TO returns nothing then try Type, and load that into the
	             *  DUE_TO xml element.
	             */
	            cvg_getFld ( el, "DUE TO", tmpStr, &ier );

	            if ( ier < 0 ) {
	                cvg_getFld ( el, "Type", tmpStr, &ier );
                    }  

	            if ( ier == 0 ) {
	               af_catStr ( &string[iarea][itype], "<DUE_TO>" );
	               af_catStr ( &string[iarea][itype], tmpStr );
	               af_catStr ( &string[iarea][itype], "</DUE_TO>" );
	            }
	      

	            af_addTags ( el, "<Status>", TAG_GFA_STATUS, &string[iarea][itype] );

	            /*
	             *  Condition wording based on snapshots
	             */
		    if( strcasecmp( hazList, "FZLVL" ) != 0  && 
		             fmt_in[ii].wording != (char *)NULL ) {
		        af_catStr ( &string[iarea][itype], fmt_in[ii].wording );
                    }

	            /*  
	             *  Airmet Tag
	             */
	            if ( addAirmetTag &&
		         ( strcasecmp( hazardType, "FZLVL" ) != 0 &&
		           strcasecmp( hazardType, "M_FZLVL" ) != 0 ) ) {

		       cvg_getFld( el, TAG_GFA_TAG, tmpStr, &ier );
                       cvg_getFld( el, TAG_GFA_AREATYPE, hazardType, &ier );
		
		       /*
  			* Add prefix H or L for TURB-HI/TURB-LO
                        */
	     	       if ( strcasecmp( hazardType, "TURB-LO" ) == 0 ) {
		   
		          sprintf( airmetTag, "L%s", tmpStr );

		       } 
		       else if ( strcasecmp( hazardType, "TURB-HI" ) == 0 ) {

		          sprintf( airmetTag, "H%s", tmpStr );

		       } 
		       else {

			  strcpy( airmetTag, tmpStr );

		       }

		       /*
 			* Put tag number in the output string
			*/
      		       sprintf ( tmpStr, "<airmetTag>%s</airmetTag>", airmetTag );
       		       af_catStr ( &string[iarea][itype], tmpStr );

	            }

		    if( isSmear ) {
                        af_catStr ( &string[iarea][itype], "</smear>" );
		    }
		    else {
                        af_catStr ( &string[iarea][itype], "</outlook>" );
		    }

		 } /* End of "if" check for non-deleted GFA elements */
	     
	     }  /* End of loop over all elements */
	     
	     
	     /*
	      *  Freezing range for hazard M_FZLVL
	      */
	     if ( foundM_FZLVL[ iarea ] || foundFZLVL[ iarea ] ) {
                 af_catStr ( &string[iarea][itype], "<freezingRange>" );
	         
		 if ( topInt > 0 ) {
		     sprintf ( tmpStr, "<Top>%03i</Top>", topInt );
	             af_catStr ( &string[iarea][itype], tmpStr );                 
		 }
           
		 if ( strlen( bottomMin ) > (size_t) 0 ) {
		     sprintf( tmpStr, "<Base>%s</Base>", bottomMin );
	             af_catStr ( &string[iarea][itype], tmpStr );
	         }

		 af_catStr ( &string[iarea][itype], "</freezingRange>" );  	         
             }

             else {
                                                                                     
                 top[ 0 ]    = '\0';
                 bottom[ 0 ] = '\0';

		 findRange = False;

    		 for ( mm = 0; mm < nin; mm++ ) {

        		if ( !fmt_in[ mm ].delete ) {
                                                                                  
			    if ( af_getFzlRange( &(fmt_in[ mm ].el), areas[ iarea], 
						 &rangeTop, &rangeBottom ) ) {
 					
				sprintf( top, "%03i", rangeTop );
				sprintf( bottom, "%03i", rangeBottom );
				findRange = True;
				break;

			    }
			}
		 }

		 if ( !findRange ) {

                 	areaIndex = 0;

		 	/*
		  	*  Find the FA area index.
		  	*/
                 	for ( mm = 0; mm < NUM_FA_AREAS; mm++ ) {

                     		if ( strcasecmp ( _FA_Area[ mm ], areas[iarea] ) == 0 ) {

                        	areaIndex = mm;
                        	break;

                     		}
                  	}
                                                                                
                 	af_findExternalFzlvls ( fmt_in, nin, xCentroid[ areaIndex ],
                        	        yCentroid[ areaIndex ], top, bottom, &ier );
                                                                                  
		 }

                 if ( findRange || ier == 0 ) {
                                                                                   
                    		af_catStr ( &string[iarea][itype], "<freezingRange>" );
                                                                                     
                    		sprintf ( tmpStr, "<Top>%s</Top>", top );
                    		af_catStr ( &string[iarea][itype], tmpStr );
                                                                                      
                    		sprintf ( tmpStr, "<Base>%s</Base>", bottom );
                    		af_catStr ( &string[iarea][itype], tmpStr );
                                                                                   
                    		af_catStr ( &string[iarea][itype], "</freezingRange>" );

                 }

             }

             af_catStr ( &string[iarea][itype], "</gfaInfo>" );
             *iret = 0;

	} /* type */
    } /* area */
}

/*=====================================================================*/

static void af_catStr ( char **str1, char *str2 ) 
/************************************************************************
 * af_catStr                                                            *
 *                                                                      *
 * This routine adds str2 at the end of str1 and realloc memory for str1*
 * if str1 is not large enough to add str2.				* 
 *                                                                      *
 *                                                                      *
 * static void af_catStr ( str1, str2 )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      **str1		char		string				*
 *      *str2		char		string to add to str2		*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          11/04   	Created                         *
 * B. Yin/SAIC          12/04   	Fixed the capacity bug          *
 * B. Yin/SAIC		11/05		changed capacity to global	*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 ***********************************************************************/
{
    
    if ( (long)(strlen( *str1 ) + strlen( str2 )) >= _capacity ) {

       G_REALLOC ( *str1, char, _capacity + ONEBLOCK, "af_format" );

       _capacity += ONEBLOCK;

    }	
    
    strcat ( *str1, str2 );

}

/*=====================================================================*/

static void af_addTags ( VG_DBStruct *el, char *xmlTag, char *gfaTag, 
			 char **outStr ) 
/************************************************************************
 * af_addTags                                                           *
 *                                                                      *
 * This routine adds the begining tag, the contents and the ending tag  *
 * into an XML string.    						*
 *                                                                      *
 * static void af_addTags ( el, xmlTag, gfaTag, outStr )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	gfa element			*
 *      *xmlTag		char		xml tag 			*
 *      *gfaTag		char		gfa tag 			*
 *                                                                      *
 * Output parameters:                                                   *
 *      **outstr	char		output string			*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          11/04   Created	                         	*
 * L. Hinson/AWC	10/05	add check for non-defined severity	*
 * E. Safford/SAIC	11/05	force MOD severity only for TURB & ICE	*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 ***********************************************************************/
{
    int		ier;
    char	tmpStr[ STD_STRLEN ], hazard[ STD_STRLEN ];
/*---------------------------------------------------------------------*/

    af_catStr ( outStr, xmlTag );

    cvg_getFld ( el, gfaTag, tmpStr, &ier );

    if ( ier == 0 ) {
       af_catStr ( outStr, tmpStr );
    }
    else {
        /* 
	 * Exception Handling for Non-Defined Severity 
	 *   limited to TURB and ICE hazards.
	 */
	cvg_getFld( el, TAG_GFA_AREATYPE, hazard, &ier );

        if (( strcmp( gfaTag, "<Severity>" ) == 0 ) && 
	    ( strcmp( hazard, "TURB") == 0 ||
	      strcmp( hazard, "ICE" ) == 0 )) {
            af_catStr( outStr, "MOD" );
        }
    }

    sprintf ( tmpStr, "%c%c%s", xmlTag[ 0 ], '/', &xmlTag[ 1 ] );

    af_catStr ( outStr, tmpStr );

}


/*=====================================================================*/

Boolean af_overrideIssueTm( const char *area, const char *airmetType, 
				   int nin, const GFA_Elem_Format *fmt_in, 
   				   int *iret )
/************************************************************************
 * af_overrideIssueTm                                                   *
 *                                                                      *
 * This routine determines if the standard issue time should be     	*
 * overridden.  An issue time is overridden if any of the hazards       *
 * (smears) are of an issuance type that is CAN, COR, NEW, or AMD.      *
 * A return of True indicates the override should take place.		*
 *									*
 * static Boolean *af_overrideIssueTm ( area, airmetType, nin, fmt_in, 	*
 *					*iret )				*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *area		const char  hazard area (BOS, MIA, CHI, etc)	*
 *      *airmetType	const char  type of airmet (SIERRA, TANGO, ZULU)*
 *	nin		int	    number of formatted elements	*
 *      *fmt_in		const Gfa_Elem_Format  list of formatted elems  *
 *                                                    			*
 * Output parameters:                                             	*
 *      *iret           int     Return code                     	*
 *                                       0: normal return               *
 *					-1: error reading from fmt_in	*
 * Return:								*
 *	Boolean		True if issue time is to be overridden		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/05	initial coding                        	*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * B. Yin		07/09	handle IFR->C&G issues			*
 * L. Hinson            11/09   handle IFR Amendment Time Issue         *
 ***********************************************************************/
{
    int		ii, ier;
    char	hazList[ STD_STRLEN ], cat[ 32 ];
    char        orighazard[ STD_STRLEN ];
    char	stat_val[32];
    Boolean	override = False;
/*--------------------------------------------------------------------*/

    *iret = 0;

    for( ii = 0; ii < nin && !override; ii++ ) {

        /*
	 *  Ignore if the element is marked as deleted
         */
	if ( fmt_in[ii].delete ) {
	    continue;
	}

	/*
	 *  Ignore if the element's area doesn't match
	 */
	if( strcasecmp( area, fmt_in[ii].area ) != 0 ) {
	    continue;
	}

	/*
	 *  Ignore if the element's type doesn't match
	 */
	cvg_getFld ( &(fmt_in[ii].el), TAG_GFA_AREATYPE, hazList, &ier );
	if( ier < 0 ) {
	    *iret = -1;
	    break;
	}
        
        strcpy(orighazard, hazList);
	if( strcasecmp( hazList, "IFR") == 0) {
	  strcpy( hazList, "C&V" );
	}

	ctb_gfagcat ( hazList, cat, &ier );
        
        if (ier !=0 && strcasecmp(orighazard,"IFR")==0) {
          ctb_gfagcat( "IFR", cat, &ier);
        }
        

        if ( strcasecmp( airmetType, cat ) != 0 ) { 
	    continue;
	}	


        cvg_getFld ( &(fmt_in[ii].el), TAG_GFA_STATUS, stat_val, &ier );
	if( ier < 0 ) {
	    *iret = -1;
	    break;
	}
	  
        if ( strcmp ( stat_val, "CAN" ) == 0 ||
	     strcmp ( stat_val, "COR" ) == 0 ||
	     strcmp ( stat_val, "NEW" ) == 0 ||
	     strcmp ( stat_val, "AMD" ) == 0    ) {

	    override = True;
        } 
    }

    return( override );
}

/*=====================================================================*/

static void af_getMinBase ( const char *base1, const char *base2, 
			      char *min, int *iret )
/************************************************************************
 * af_getMinBase                                                        *
 *                                                                      *
 * This routine determines the minimum base FL value.  This is not	*
 * as simple as getting the minimum value, because "SFC" and "FZL" are  *
 * valid base flight levels.  Both base values are assumed to be 3 char *
 * long strings or NULL values.  Anything longer will be returned as an *
 * error.                                                               *
 *									*
 * Note that it is assumed that adequate space (4 chars) has been       *
 * allocated for min by the calling routine.				*
 *                                                                      *
 * static void *af_getMinBase ( base1, base2, min, iret )		*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *base1		char	first base flight level         	*
 *      *base2		char	second base flight level         	*
 *                                                    			*
 * Output parameters:                                             	*
 *	*min		char	minimum base value			*
 *      *iret           int     Return code                     	*
 *                                       0: normal return               *
 *					-1:  base string too long	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/05	initial coding                        	*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 ***********************************************************************/
{
    int		ibase1, ibase2, minBase;
/*--------------------------------------------------------------------*/

    *iret  = 0;
    min[0] = '\0';

    if( strlen( base1 ) > (size_t) 3 || strlen( base2 ) > (size_t)3 ) {
	*iret = -1;
	return;
    }

    if( ( strcmp( base1, "SFC" ) == 0 ) || 
        ( strcmp( base2, "SFC" ) == 0 ) ) {
        strcpy( min, "SFC" );
    }
    else if( ( strcmp( base1, "FZL" ) == 0 ) || 
             ( strcmp( base2, "FZL" ) == 0 ) ) {
        strcpy( min, "FZL" );
    }
    else { 
	ibase1 = atoi( base1 );
	ibase2 = atoi( base2 );

	minBase = G_MIN( ibase1, ibase2 );

	if( minBase == 0 ) {
	    strcpy( min, "SFC" );
	}
	else {
	    sprintf( min, "%03d", minBase );	
	}
    }
  
}

/*=====================================================================*/

                                                                               
static void af_findExternalFzlvls ( const GFA_Elem_Format *fmt_in, int nfmt,
                               float xCentroid, float yCentroid,
                               char *top, char *base, int *iret )
/************************************************************************
 * af_findExternalFzlvls                                                *
 *                                                                      *
 * This routine returns the top and base levels for a FA area that is   *
 * not intersected by any FZLVL contours. It sorts all external fzlvls	*
 * by distance from the area centroid, then uses the level from that	*
 * contour.  If the contour is to the left of the area then the answer  *
 * is base = contour level, top = base + 040.  If the contour is to the *
 * right of the area then the answer top = contour level, base = top -  *
 * 040.									*
 *                                                                      *
 * static void af_findExternalFzlvls ( fmt_in, nfmt, xCentroid,         *
 *                      yCentroid, top, base, iret )                    *
 *                                                                      *
 * Input parameters:                                                    *
 *      *fmt_in         GFA_Elem_Format formatted gfa element array     *
 *      nfmt            int             number of elements in fmt_in    *
 *      xCentroid       float           lat of the FA area centroid     *
 *      yCentroid       float           lon of the FA area centroid     *
 *                                                                      *
 * Output parameters:                                                   *
 *      *top            char            top FZLVL level in the FA area  *
 *      *base           char            base FZLVL level in the FA area *
 *      *iret           int             return code: 0 normal           *
 * 						     1 nothing found    *
 *                                                                      *
 * Return parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           3/06   Created                                 *
 * E. Safford/SAIC	07/06	Use nearest fzlvl, not largest left/    *
 *				  smallest right (which was error prone)*
 * E. Safford/SAIC	08/06	moved from afcreate.c			*
 * D.W.Plummer/NCEP     12/06   Added tol to cgr_qrol calling sequence  *
 * E. Safford/SAIC      12/06   correct position use w/ cgr_qrol chng   *
 * E. Safford/SAIC      01/07   corrected one more bug in cgr_qrol usage*
 ***********************************************************************/
{
    int         ii, npts, one, nFzl, closed, ier;
    int         jj, tmp, nearestVert;
    int		*fmtIdx, position, frzlvl, idx; 
    float       tol=0.0F;

    float       *xnormal, *ynormal, xCtrNorm, yCtrNorm, *distance, ftmp;
    char        hazard[ 32 ], tmpStr[ 32 ];
/*---------------------------------------------------------------------*/

    *iret    = 0;
    one      = 1;
    nFzl     = 0;
    xnormal  = NULL;
    ynormal  = NULL;
    distance = NULL;
    fmtIdx   = NULL;

    /*
     *  Convert centroid to normalized coordinates.
     */
    gtrans ( sys_N, sys_M, &one, &xCentroid, &yCentroid,
             &xCtrNorm, &yCtrNorm, &ier, strlen(sys_M), strlen(sys_N) );

                                                                                    
    for ( ii = 0; ii < nfmt; ii++ ) {
                                                                                  
        if ( !fmt_in[ ii ].delete ) {
                                                                                    
           cvg_getFld ( &(fmt_in[ ii ].el), TAG_GFA_AREATYPE, hazard, &ier );
                                                                                
           if ( strncasecmp ( hazard, "FZLVL", 5 ) == 0 ) {
                                                                                
              nFzl++;
                                                                              
              npts = fmt_in[ ii ].el.elem.gfa.info.npts;
                                                                                 
              G_MALLOC ( xnormal, float, npts, "AF_EXTERNALFZLVLS xnormal" );
              G_MALLOC ( ynormal, float, npts, "AF_EXTERNALFZLVLS ynormal" );

              /*
               *  Convert formatted fzlvl contour to normalized coordinates
               */
              gtrans ( sys_M, sys_N, &npts, (float*)fmt_in[ ii ].el.elem.gfa.latlon,
                       (float*)&(fmt_in[ ii ].el.elem.gfa.latlon[npts]), xnormal,
                       ynormal, &ier, strlen(sys_M), strlen(sys_N) );
                                                                                   
              closed = fmt_in[ ii ].openLine ? 0 : 1;
                                                                                
              G_REALLOC ( fmtIdx, int, nFzl, "AF_EXTERNALFZLVLS fmtIdx" );
              G_REALLOC ( distance, float, nFzl, "AF_EXTERNALFZLVLS distance" );

              fmtIdx[ nFzl -1 ] = ii;

	      /*
	       *  Determine the distance from the area centroid to the line.
	       */
  	      cgr_dist( npts, xnormal, ynormal, xCentroid, yCentroid, 
	      		&distance[ nFzl -1 ], &nearestVert, &ier ); 
	      
  
              G_FREE ( xnormal, float );
              G_FREE ( ynormal, float );
                                                                                 
           }
        }
    }

    if( nFzl == 0 ) {
	*iret = 1;
	return;
    }


    /*
     *  Sort the distance array.
     */
    for ( ii = 0; ii < nFzl - 1; ii++ ) {
        for ( jj = 0; jj < nFzl - 1; jj++ ) {

            if ( distance[ jj ] > distance[ jj + 1 ] ) {
                                                                               
                ftmp = distance[ jj ];
                distance[ jj ] = distance[ jj + 1 ];
                distance[ jj + 1 ] = ftmp;

                tmp = fmtIdx[ jj ];
                fmtIdx[ jj ] = fmtIdx[ jj + 1 ];
                fmtIdx[ jj + 1 ]= tmp;
            }
        }
    }


    /*
     *  Determine if the closest contour is left or right of the 
     *  area centroid and get the freezing level from it.
     */
    idx = fmtIdx[0];

    npts = fmt_in[ idx ].el.elem.gfa.info.npts;

    G_MALLOC ( xnormal, float, npts, "AF_EXTERNALFZLVLS xnormal" );
    G_MALLOC ( ynormal, float, npts, "AF_EXTERNALFZLVLS ynormal" );
                                                                                 
    /*
     *  Convert formatted fzlvl contour to normalized coordinates
     */
    gtrans ( sys_M, sys_N, &npts, (float*)fmt_in[ idx ].el.elem.gfa.latlon,
                (float*)&(fmt_in[ idx ].el.elem.gfa.latlon[npts]), xnormal,
                ynormal, &ier, strlen(sys_M), strlen(sys_N) );
                                                                                   
    closed = fmt_in[ idx ].openLine ? 0 : 1;


    cgr_qrol ( &npts, xnormal, ynormal, &closed, &xCentroid,
                      &yCentroid, &tol, &position, &ier );

    G_FREE ( xnormal, float );
    G_FREE ( ynormal, float );


    /*
     *  Get the freezing level value and use it to calculate the 
     *  top and bottom.
     */
    tmpStr[ 0 ] = '\0';

    cvg_getFld ( &fmt_in[ idx ].el, "Level", tmpStr, &ier );
                                                                                  
    if ( strcasecmp ( tmpStr, "sfc" ) == 0 ) {
        frzlvl = 0;
    }
    else {
        frzlvl = atoi ( tmpStr );
    }

    top[ 0 ] = '\0';
    base[ 0 ] = '\0';

    if( position == 1 ) {		/* fzlvl is right of area centroid */
        sprintf ( base, "%03i", frzlvl );
        sprintf ( top,  "%03i", frzlvl + 40 );
    }
    else {				/* fzlvl is left of area centroid */
        strcpy ( top, "SFC" );
        strcpy ( base, "SFC" );

        if ( frzlvl > 0 ) {
           sprintf ( top, "%03i", frzlvl );

           if ( frzlvl - 40 > 0 ) {
              sprintf ( base, "%03i", frzlvl - 40 );
           }
        }
    }


    G_FREE ( distance, float );
    G_FREE ( fmtIdx,   int );

    return;
                                                                                   
}

/*=====================================================================*/

static Boolean af_getFzlRange ( VG_DBStruct *el, char area[], 
				int *top, int *bottom )
/************************************************************************
 * af_getFzlRange                                                       *
 *                                                                      *
 * This routine retrieves top and bottom freezing levels for a specified*
 * FA area from the input element.					*
 *                                                                      *
 * static Boolean af_getFzlRange ( el, area, top, bottom ) 		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	gfa element			*
 *      area[] 		char		FA area name			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *top		int		top freezing level		*
 *      *bottom		int		bottom freezing level		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          01/07   Created	                         	*
 * B. Yin/CWS		01/11	check negative freezing levels		*
 *				and set them to 0			*
 ***********************************************************************/
{
    int 	ier;
    char	ranges[ STD_STRLEN ], *aRange, *delim, haz[ STD_STRLEN ];
    Boolean	ret;
/*---------------------------------------------------------------------*/

    cvg_getFld ( el, TAG_GFA_AREATYPE, haz, &ier );
 
    if ( ier != 0 || strcasecmp( haz, "FZLVL" ) != 0 ) return False;

    /*
     *  Get the range string from the input element.
     */
    cvg_getFld ( el, TAG_GFA_FZLRANGE, ranges, &ier );

    ret = True;

    /*
     *  Get the top and bottom for the specified FA area.
     */
    if ( ier == 0 && ( aRange = strstr( ranges, area ) ) != NULL ) {

       *top = *bottom = 0;
        
       if ( ( delim = strchr( aRange, ';' ) ) != NULL ) {
	
          *top = atoi( delim + 1 );
	  if ( *top < 0 ) *top = 0;

       }

       if ( ( delim = strchr( delim + 1, ';' ) ) != NULL ) {
	
          *bottom = atoi( delim + 1 );
	  if ( *bottom < 0 ) *bottom = 0;

       }

       if ( *top == 0 && *bottom == 0 ) {

          ret = False;

       }

    }
    else {

       ret = False;

   }

   return ret;

}

/*=====================================================================*/

static void af_getFirstLastIssueTimes( int *firstIssueTm, int *lastIssueTm, 
				       char *firstCycle, char *lastCycle,
				       int *iret )
/************************************************************************
 * af_getFirstLastIssueTimes                                            *
 *                                                                      *
 * This routine gets the first item and the last item of the issue time *
 * array and converts them into minutes. It also returns the first and	*
 * the last cycles							*
 *                                                                      *
 * static void af_getFirstLastIssueTimes ( firstIssueTm, lastIssueTm, 	*
 *					   iret ) 			*
 *                                                                      *
 * Input parameters:                                                    *
 *	None.                                          			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*firstIssueTm	int		first issue time in minutes	*
 *	*lastIssueTm	int		last issue time in minutes	*
 *	*firstCycle	char		first cycle			*
 *	*lastCycle	char		last cycle			*
 *	*iret	        int		return code 0: normal		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          02/07   Created	                         	*
 * B. Yin/SAIC          05/07   Added first/last cycles to output	*
 * S. Guan/NCEP         07/20   Added tzone as an input of TI_DST	*  
 ***********************************************************************/
{

    int         ii, tmType, ier, tmArray[ 5 ], isDst, nCycles, tmpint;
    int		nSortedCycle;
    char        **cycles, issueTm[ 32 ];
    char        dattim[ 20 ], tzone[2];
/*---------------------------------------------------------------------*/
                                                                                      
    *iret = 0;

    /*
     *  Read the cycle times from the airmet table
     */
    tmType = 1;
    css_gtim ( &tmType, dattim, &ier );
    ti_ctoi ( dattim, tmArray, &ier, strlen ( dattim ) );
    /* Central Time zone assumed for this code specific to AWC. */
    tzone[0] = 'C';
    tzone[1] = '\0';
    ti_dst ( tmArray, tzone, &isDst, &ier );

    ctb_airmetGetCycleTms( (isDst != 0), &nCycles, &cycles, &ier );

    /*
     * Sort the cycle array and get the first and last items.
     */
    if ( ier >= 0 ) {

       cst_sort ( 1, &nCycles, cycles, &nSortedCycle, cycles, &ier );

       ctb_airmetGetIssueTm( cycles[ 0 ], issueTm, &ier );
       cst_numb( issueTm, &tmpint, &ier );
       if ( ier < 0 ) tmpint = 0;
       *firstIssueTm = tmpint / 100 * 60 + tmpint % 100;	      

       ctb_airmetGetIssueTm( cycles[ nSortedCycle - 1 ], issueTm, &ier );
       cst_numb( issueTm, &tmpint, &ier );
       if ( ier < 0 ) tmpint = 0;
       *lastIssueTm = tmpint / 100 * 60 + tmpint % 100;	      

       strcpy( firstCycle, cycles[ 0 ] );
       strcpy( lastCycle, cycles[ nSortedCycle - 1 ] );

    }
    else {

       *firstIssueTm = *lastIssueTm = 0;
       *iret = -1;

    }

    for ( ii = 0; ii < nCycles; ii++ ) {

     	G_FREE( cycles[ ii ], char );

    }

    G_FREE( cycles, char * );

}

/*=====================================================================*/

void af_getIssueTm ( char* cycle, char* day, Boolean overrideIssueTm, 
		     char* issueTm, int *iret )
/************************************************************************
 * af_getIssueTm		                                        *
 *                                                                      *
 * This routine calculates the issue time for a GFA bulletin.		*
 *                                                                      *
 * void af_getIssueTm ( cycle, day, overrideIssueTm, issueTm, iret ) 	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*cycle		char		issue cycle                	*
 *	*day		char		issue day                	*
 *	overrideIssueTm	Boolean		flag to override issue time   	*
 *	                                          			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*issueTm	char		issue time DDHHMM		*
 *	*iret	        int		return code 0: normal		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          03/07   Created	                         	*
 * B. Yin/SAIC          04/07   Fixed the day boundary bug.      	*
 * B. Yin/SAIC          05/07   Change af_getFirstLastIssueTm calling   *
 *				sequence				*
 * E. Safford		08/07	initialize time variables		*
 * B. Yin/SAIC		12/07	make sure issueTm string has 6 char	*
 ***********************************************************************/
{

    int	ier, ier2, jj;
    int	loc_tarry[5], iss_tarry[5], issueHhmm;
    int	localTmInMin = 0, issueTmInMin = 0, nmin, one_min;

    char 	timeStr[ 32 ], firstCycle[ 32 ], lastCycle[ 32 ];
    time_t	tt;
    struct tm	*localTm;

    static int		firstIssueTm = 0 , lastIssueTm = 0;
    static Boolean	initIssueTm = False;
/*---------------------------------------------------------------------*/

    /* 
     * Get current local time and issue time
     */
    tt = time ( NULL );
    localTm = localtime ( &tt );

    ctb_airmetGetIssueTm( cycle, timeStr, &ier );
	    
    /*
     * Convert local&issue time into integer arrays.
     */
    loc_tarry[0] = localTm->tm_year + 1900;
    loc_tarry[1] = localTm->tm_mon  + 1;
    loc_tarry[2] = localTm->tm_mday;
    loc_tarry[3] = localTm->tm_hour;
    loc_tarry[4] = localTm->tm_min;

    issueHhmm = 0;
    sscanf ( timeStr, "%d", &issueHhmm );

    iss_tarry[0] = localTm->tm_year + 1900;
    iss_tarry[1] = localTm->tm_mon  + 1;
    iss_tarry[2] = localTm->tm_mday;
    iss_tarry[3] = issueHhmm / 100;
    iss_tarry[4] = issueHhmm % 100;
    
    /*
     * Construct timeStr string below.
     */
    if ( ier == 0 ) {

      /*
       * Compare the local time to the issue time if
       * overrideIssueTm flag is TRUE.
       */
      if ( overrideIssueTm == TRUE ) {

	if ( !initIssueTm ) {

	   af_getFirstLastIssueTimes( &firstIssueTm, &lastIssueTm, 
	   			      firstCycle, lastCycle, &ier );

	   if ( ier < 0 ) firstIssueTm = lastIssueTm = 0;

	   initIssueTm = True;

	}

	localTmInMin = localTm->tm_hour * 60 + localTm->tm_min;
	issueTmInMin = iss_tarry[ 3 ] * 60 + iss_tarry[ 4 ];

	ti_mdif (loc_tarry, iss_tarry, &nmin, &ier2);

        if ( ( issueTmInMin == lastIssueTm && localTmInMin <= firstIssueTm )
		     || nmin > 0 ) {


            /*
             * set issue time = local time
             */
	    for (jj=0;jj<5;jj++)  iss_tarry[jj] = loc_tarry[jj];

		  sprintf ( timeStr, "%02d%02d", iss_tarry[3],
			   iss_tarry[4]);
        }
	else {

	    /*
             * set issue time = issue time + 1 min
             */
	    one_min = 1;
	    ti_addm (iss_tarry, &one_min, iss_tarry, &ier2);

	    sprintf ( timeStr, "%02d%02d", iss_tarry[3],
			   iss_tarry[4]);
	}

      }

      if ( issueTmInMin == lastIssueTm && localTmInMin <= firstIssueTm ) {

	 /*
	  *  Use the local day if the issue time crosses the day boundary.
	  */
         sprintf ( issueTm, "%02d%s", localTm->tm_mday, timeStr );
	  
      }
      else {

	   /*
	    *  write out the issue time as 6 char string.
	    */
           sprintf ( issueTm, "%02d%s", atoi(day), timeStr );
      
      }

      *iret = 0;

    }
    else {

      sprintf ( issueTm, "%02dxxxx", localTm->tm_mday );
      *iret = -1;

    }

}
