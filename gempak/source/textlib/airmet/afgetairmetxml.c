
#include "afcmn.h"

/************************************************************************
 * afgetairmetxml.c                                       		*
 *                                                                      *
 * This module contains all the subroutines used to extract the airmet	*
 * information from each F/BB/A (From, Bounded By, Along) vg element    *
 * and encode it in an xml document which can be transformed (via xslt) *
 * to an airmet text bulletin.						*
 *                                                                      *
 * CONTENTS:                                                            *
 *   public( to the library) functions:                                 *
 *      af_getAirmetXml		- extract the F/BB/A element info and   *
 *				  put it in an xml document.		*
 *                                                                      *
 *   private functions:							*
 *	af_getXmlDocHdr		- build the xml document header		*
 *	af_getElemXml		- verify elem is correct type/area and  *
 *				  load its info into an xml document	*
 *	af_elm2Xml		- translate info from elem to xml	* 
 *      af_getFzlvlRngXml	- find the airmet Fzlvl info and add	*
 *				  it to an xml document			* 
 *	af_getFzlvlRng		- get fzlvl range info from VG element	*
 *				   range field				*
 *	af_inferFzlvlRange  	- get fzlvl range info from all FZLVL 	*
 *				   elems within FA Area			* 
 *	af_getExternalFzlvlRng	- get fzlvl range info from FZLVL elems	*
 *				   not in the FA Area			*
 *	af_orderStateList	- re-order state list as necessary      *
 *	af_catStr		- add string to the end of first string *
 *      af_getIssueTimes 	- get the first & last available issue  *
 *				   times to calculate the next cycle	*
 *	af_elmOk2Use		- verify a given VG element matches 	*
 *				   area and bulletin type		*
 *	af_matchHaz2Bul		- verify input hazard fits the bulletin *
 *	af_matchAreas		- verify area string is in FA Area list	*
 *	af_addTags		- add specified GFA element to XML doc	*
 *	af_validateVgFile	- validate an input vg file name	*
 * 	af_getAdjArea		- return the adjacent area 		*
 *	af_getMinBase		- determine the minimum base level	*
 *      af_getFLBaseTop         - get the base/top range for a FL       *
 ***********************************************************************/


/*
 *  Private functions
 */
static void af_getXmlDocHdr ( char *area, char *day, char *cycle, 
			char *issueTm, char **outputXml, int *iret );

static void af_getElemXml( char *vgfile, FILE *vgFilePtr, long fileLen, char *area, 
			   char *bulType, char **outputXml, Boolean *overrideTm,
			   int *iret );

static void af_elm2Xml ( char *area, VG_DBStruct *el, 
			 char **outputXml, int *iret ) ;

static void af_getFzlvlRngXml( char *vgfile, FILE *vgFilePtr, long fileLen,
				 char *area, char **outputXml, int *iret );

static void af_getFzlvlRng( char *vgfile, FILE *vgFilePtr, long fileLen,
			 char *area, char *topStr, char *bottomStr, int *iret );

static void af_inferFzlvlRange( char *vgfile, FILE *vgFilePtr, long fileLen,
			 char *area, char *topStr, char *bottomStr, int *iret );

static void af_getExternalFzlvlRng( char *vgfile, FILE *vgFilePtr, 
			long fileLen, char *area, char *topStr, 
			char *bottomStr, int *iret );

static void af_orderStateList ( char *area, char *origList, char *orderedList,
				int *iret );

static void af_catStr ( char **str1, char *str2 ) ;

static void af_getIssueTimes( int *firstIssueTm, int *lastIssueTm, 
				       char *firstCycle, char *lastCycle,
				       int *iret );

static Boolean af_elmOk2Use( VG_DBStruct *el, char *area, char *bulType );

static Boolean af_matchHaz2Bul( int hazard, char *bulType );

static Boolean af_matchAreas( char *elAreas, char *bulArea );

static void af_addTags ( VG_DBStruct *el, char *xmlTag, char *gfaTag, 
			 char **outStr );

static void af_validateVgFile( char *vgfile, FILE **vgFilePtr, char *fullFileName,
			       long *fileLen, int *iret );

static void af_getAdjArea( char *area, char *adjArea, int *iret );

static void af_getMinBase ( const char *base1, const char *base2, 
			      char *min, int *iret );

static void af_getFLBaseTop( char *level, char *base, char *top, int *iret );



#define	ERROR_ZERO_LENGTH_VG_FILE	( -5 )
#define ERROR_CANT_OPEN_VG_FILE		( -6 )
#define ERROR_READING_VG_ELEMENT	( -7 )


/* 
 *  Global variables
 */
static long 	_capacity = ONEBLOCK;

int		_Sierra_Hazards[ NUM_SIERRA_HAZARDS ] = 
			{ GFA_HAZARD_IFR, 	GFA_HAZARD_MT_OBSC };

int		_Tango_Hazards[ NUM_TANGO_HAZARDS ] = 
			{ GFA_HAZARD_TURB, 	GFA_HAZARD_TURB_HI, 
			  GFA_HAZARD_TURB_LO,	GFA_HAZARD_SFC_WND,
                          GFA_HAZARD_LLWS };

int		_Zulu_Hazards[ NUM_ZULU_HAZARDS ] = 
			{ GFA_HAZARD_ICE,	GFA_HAZARD_FZLVL_SFC,
			  GFA_HAZARD_FZLVL,	GFA_HAZARD_M_FZLVL };


/*
 *  Public functions
 */

void af_getAirmetXml ( char *vgFile, int nAreas, char areas[][ 8 ], int nTypes, 
			char *types[3], char *day, char *cycle, char *issueTm,
                        char *outputXml[6][3],
                        int *iret )
/************************************************************************
 * af_getAirmetXml                                                     	*
 *                                                                      *
 * This routine generates an xml formatted document containing the 	*
 * airmet text report for the specified areas and airmet types.       	*
 *                                                                      *
 *  Note: "outputXml" is allocated in this routine and must be freed by	*
 *	  the caller.                                                   *
 *                                                                      *
 * void af_getAirmetXml (vgfile, nAreas, areas, nTypes, types, day,     *
 *			cycle, issueTm, outputXml, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*vgFile		char	Vg file name and path 			*
 *	nAreas		int	number of areas to be formatted		*
 *	areas[][8]	char	array of area strings			*
 *	nTypes		int	number of hazard types to be formatted	*
 *	types[3]  	char	array of hazard type strings		*
 *      *day            char    forecast day                            *
 *      *cycle		char	forecast cycle				*
 *	*issueTm	char	forecast issue time			*
 *                                                                      *
 * Output parameters:                                                   *
 *      **outputXml	char	array of xml documents, each being the  *
 *				 content for an airmet bulletin		*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 *					-1: nAreas <= 0   		*
 *					-2: missing an area name	*
 *					-3: nTypes <= 0			*
 *					-4: missing a type name		*
 *				        -5: zero length vg file 	*
 *					-6: unable to open vg file	*	
 *					-7: error reading vg element	*
 *					 1: can't find airmet.xsd	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	05/07	initial coding             		*
 * B. Yin/SAIC		12/07	override issue time if necessary	*
 * B. Yin/SAIC		12/07	set return code to 2 if there are 	*
 *				different freezing level ranges		*
 * B. Yin/SAIC		03/08	set override time flag for each FA area	*
 * B. Yin/SAIC		01/09	close the workfile			*
 * B. Hebbard/NCEP      05/18   Jira NAWIPS-32 Redmine 14150:  Double   *
 *                              allocated size of outputXml array pera  *
 *                              AWC request, to relieve crash issues.   *
 ***********************************************************************/
{
    int		ii, iarea, itype, ier;
    long	fileLen = 0;
    char	timeStr[ 32 ], *timeTag, fullVgName[ FILE_FULLSZ ];
    Boolean	overrideTm = False;

    FILE	*vgFilePtr;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    af_initBnds( &ier );

    /*
     *  Sanity check on input areas
     */
    if ( nAreas <= 0 ) {
	*iret = -1;
    }
    else {
        for( ii=0; ii<nAreas; ii++ ) {
            if( strlen( areas[ii] ) <= 0 ) {
		*iret = -2;
		break;
	    }
	}
    }

    if( *iret < 0 ) return;

    /*
     *  Sanity check on input types
     */
    if ( nTypes <= 0 ) {
	*iret = -3;
    }
    else {
        for( ii=0; ii<nTypes; ii++ ) {
            if( strlen( types[ii] ) <= 0 ) {
		*iret = -4;
		break;
	    }
	}
    }

    if( *iret < 0 ) return;

    af_validateVgFile( vgFile, &vgFilePtr, fullVgName, &fileLen, &ier );
    if( ier < 0 ) {
        *iret = ier;
	return;
    }  

    for( iarea = 0; iarea < nAreas; iarea++ ) {
        for ( itype = 0; itype < nTypes; itype++ ) { 

  	    af_getXmlDocHdr( areas[ iarea ], day, cycle, issueTm,
	    		&outputXml[iarea][itype], &ier);

    	    overrideTm = False;
            af_getElemXml( vgFile, vgFilePtr, fileLen, areas[ iarea ], types[ itype ],
	    			&outputXml[iarea][itype], &overrideTm, &ier );

	    /*
	     * Override the issue time if necessary
	     */
	    if ( overrideTm )  {

               af_getIssueTm ( cycle, day, overrideTm, timeStr, &ier ); 

	       timeTag = NULL;
	       timeTag = strstr ( outputXml[ iarea ] [ itype ], "<issueTime>" );

	       if ( timeTag && ( ier >= 0 ) ) {
		  strncpy( timeTag + strlen("<issueTime>"), timeStr, 6 );
	       }

	    }

	    if( strcmp( types[ itype ], "ZULU" ) == 0 ) {
                af_getFzlvlRngXml( vgFile, vgFilePtr, fileLen, 
	    			areas[ iarea ], &outputXml[iarea][itype], &ier );
		*iret = ( ier == 2 ) ? 2 : *iret;
            }

            strcat ( outputXml[iarea][itype], "</gfaInfo>" );
	} 

    } 

    cfl_clos( vgFilePtr, &ier );
    af_freeBnds( &ier );

}

/*=====================================================================*/

static void af_getXmlDocHdr ( char *area, char *day, char *cycle, 
			char *issueTm, char **outputXml, int *iret )
/************************************************************************
 * af_getXmlDocHdr                                                      *
 *                                                                      *
 * This routine builds the xml document header for a given airmet       *
 * bulletin.  Calling routine must free memory allocated for outputXml. *
 *                                                                      *
 * static void af_getXmlDocHdr ( airmetXsd, area, day, cycle, issueTm,  *
 *			outputXml, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *area		char	FA Area  				*
 *      *day		char	day of issue  				*
 *	*cycle		char	airmet cycle				*
 *	*issueTm	char	issue time				*
 *									*
 * Output parameters:							*
 *	**outputXml	char	output xml string			*
 *	*iret		int	return code				*
 *					 0 = Normal			*
 *					 1 = unable to find airmet.xsd  *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	05/07	initial coding        			*
 ***********************************************************************/
{
    int		strSize = ONEBLOCK, ier, icycle;
    int		issueTm1, issueTm2;
    long	flen;

    char	airmetXsd[ FILE_FULLSZ ];
    char	timeStr[ 32 ], otlkStr[ 32 ];
    char	firstCycle[ 32 ], lastCycle[ 32 ];


    time_t      tt;
    struct tm   *localTm;
    Boolean	overrideIssueTm = False; 

/*---------------------------------------------------------------------*/


    *iret = 0;

    /*
     *  locate the airmet.xsd schema file either locally or in the 
     *  default location.  This must be a full path specification
     *  containing no aliases.  Failure to find this file is non-fatal.
     *  
     *  I think it's non-fatal.  Test when missing.
     */
    cfl_tinq( "airmet.xsd", XSD, &flen, airmetXsd, &ier );
    if( ier < 0 ) {
	*iret = 1;
    }

    _capacity = ONEBLOCK;
    G_MALLOC ( *outputXml, char, strSize * 4, "af_getXmlDocHdr outputXml" );

    sprintf( *outputXml, "%s", XML_HDR );
    af_catStr ( outputXml, "<gfaInfo " );
   
    /*
     *  Add the schema file reference
     */
    af_catStr( outputXml, SCM_FILE );
    af_catStr( outputXml, "\"file:");
    af_catStr( outputXml, airmetXsd);

    af_catStr( outputXml, "\"><hdr><faArea>" );
    af_catStr( outputXml, area );
    af_catStr( outputXml, "</faArea>" );

    /*
     *  Add the issue time
     */
    af_getIssueTm ( cycle, day, overrideIssueTm, timeStr, &ier ); 
    af_catStr( outputXml, "<issueTime>" );
    af_catStr( outputXml, timeStr );
    af_catStr( outputXml, "</issueTime>" );


    /*
     *  Calculate the untilTime and outlook expiration time
     */
    tt = time ( NULL );
    localTm = localtime ( &tt );

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
        af_getIssueTimes ( &issueTm1, &issueTm2, firstCycle, lastCycle, &ier );
       
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

    af_catStr ( outputXml, "<untilTime>" );
    af_catStr ( outputXml, timeStr );
    af_catStr ( outputXml, "</untilTime>" );
  
    af_catStr ( outputXml, "<outlookEndTime>" );
    af_catStr ( outputXml, otlkStr );
    af_catStr ( outputXml, "</outlookEndTime>" );


    /*
     *  End the <hdr> xml element
     */
    af_catStr( outputXml, "</hdr>" );

}

/*=====================================================================*/

static Boolean af_elmOk2Use( VG_DBStruct *el, char *area, char *bulType )
/************************************************************************
 * af_elmOk2Use                                                         *
 *                                                                      *
 * This routine determines if the vg element is an F/BB/A element and   *
 * if it matches the input FA area and airmet bulletin type.  If it	* 
 * matches then True is returned.					*
 *                                                                      *
 *                                                                      *
 * static Boolean af_elmOk2Use ( *el, *area, *bulType )         	* 
 *                                                                      *
 * Input parameters:                                                    *
 *      *el	 VG_DBStruct	VG element                		*
 *      *area		char	FA area					*
 *	*bulType	char	text bulletin type (Sierra, Tango, Zulu)*
 *									*
 * Output parameters:							*
 *		none							*
 * Return:								*
 *		Boolean		True if el is F/BB/A type GFA elm and   *
 *				 matches the area and bulType		*
 **                                                                     *
 * Log									*
 * E. Safford/SAIC	07/07	initial coding       			*
 ***********************************************************************/
{
    char	str[ 32 ], areaStr[ 32 ];
    int		subType = -1, hazard = -1, ier;
    Boolean	ok2Use  = G_FALSE;
/*---------------------------------------------------------------------*/

    cvg_getFld( el, TAG_GFA_SUBTYPE, str, &ier );
    if( ier >= 0 ) {
        subType = atoi( str ) % 10;
        hazard  = atoi( str ) / 10;

	if( subType == GFA_FBBA_AIRMET || subType == GFA_FBBA_OUTLOOK ) {

            cvg_getFld( el, TAG_GFA_AREAS, areaStr, &ier );
	    if( ier >= 0 ) {

	        ok2Use = af_matchHaz2Bul( hazard, bulType ) && 
	    	         af_matchAreas( areaStr, area );
            }
	}
    }
   
    return( ok2Use );

}

/*=====================================================================*/

static Boolean af_matchHaz2Bul( int hazard, char *bulType )
/************************************************************************
 * af_matchHaz2Bul                                                      *
 *                                                                      *
 * This routine determines if the hazard type is a valid type for the   *
 * input buletin type.  If the match is good then True is returned.	* 
 *                                                                      *
 *                                                                      *
 * static Boolean af_matchHaz2Bul ( hazard, *bulType )         		* 
 *                                                                      *
 * Input parameters:                                                    *
 *      hazard		int	hazard type				*
 *	*bulType	char	text bulletin type (Sierra, Tango, Zulu)*
 *									*
 * Output parameters:							*
 *		none							*
 * Return:								*
 *		Boolean		True if hazard is valid for bulType     *
 **                                                                     *
 * Log									*
 * E. Safford/SAIC	07/07	initial coding       			*
 ***********************************************************************/
{
    int		ii;
    Boolean	match = G_FALSE;
/*---------------------------------------------------------------------*/

    if( strcmp( bulType, "SIERRA" ) == 0 ) {

        for( ii = 0; ii < NUM_SIERRA_HAZARDS; ii++ ) {
            if( hazard == _Sierra_Hazards[ ii ] ) {
		match = G_TRUE;
		break;
	    }
        }
    }
    else if( strcmp( bulType, "TANGO" ) == 0 ) {

        for( ii = 0; ii < NUM_TANGO_HAZARDS; ii++ ) {
            if( hazard == _Tango_Hazards[ ii ] ) {
		match = G_TRUE;
		break;
	    }
        }
    }
    else if( strcmp( bulType, "ZULU" ) == 0 ) {

        for( ii = 0; ii < NUM_ZULU_HAZARDS; ii++ ) {
            if( hazard == _Zulu_Hazards[ ii ] ) {
		match = G_TRUE;
		break;
	    }
	}
    }

    return( match );
}

/*=====================================================================*/

static Boolean af_matchAreas( char *elAreas, char *bulArea )
/************************************************************************
 * af_matchAreas                                                        *
 *                                                                      *
 * This routine determines if the element's FA areas includes the  	*
 * requested FA area for the bulletin.  If the bulArea is included in   *
 * the elAreas then True is returned.					* 
 *                                                                      *
 * static Boolean af_matchAreas ( *elAreas, *bulArea )         		* 
 *                                                                      *
 * Input parameters:                                                    *
 *      *elAreas	char	element's areas				*
 *	*bulArea	char	text bulletin's FA area			*
 *									*
 * Output parameters:							*
 *		none							*
 * Return:								*
 *		Boolean		True if bulArea is found in elAreas     *
 **                                                                     *
 * Log									*
 * E. Safford/SAIC	07/07	initial coding       			*
 ***********************************************************************/
{
    Boolean	match = G_FALSE;
/*---------------------------------------------------------------------*/

    if( strstr( elAreas, bulArea ) ) {
	match = G_TRUE;
    }

    return( match );
}

/*=====================================================================*/
static void af_getElemXml( char *vgfile, FILE *vgFilePtr, long fileLen, char *area, 
			   char *bulType, char **outputXml, Boolean *overrideTm, 
			   int *iret )
/************************************************************************
 * af_getElemXml                                                        *
 *                                                                      *
 * This routine pulls the information from each qualifying gfa element  *
 * in the vgfile and puts it into the outputXml as an xml element.	* 
 *                                                                      *
 *                                                                      *
 * static void af_getElemXml ( *vgfile, *area, **outputXml, *iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *vgfile		char	VG filename (full path)               	*
 *	*vgFilePtr	FILE	VG file pointer				*
 *	fileLen		long	VG file length				*
 *      *area		char	FA area					*
 *	*bulType	char	text bulletin type (Sierra, Tango, Zulu)*
 *									*
 * Output parameters:							*
 *	**outputXml	char	xml string to cat the output to		*
 * 	*overrideTm	Boolean	flag to override the issue time		*
 *	*iret		int	return code 				*
 *					 0 = Normal			*
 *					-2 = error reading vg elm hdr	*
 *					-7 = error reading vg element	*
 **                                                                     *
 * Log									*
 * E. Safford/SAIC	07/07	initial coding       			*
 * B. Yin/SAIC		12/07	get the override issue time flag	*
 * J. Wu/SAIC		01/08	pull info from 2nd FA area if it exists	*
 ***********************************************************************/
{
    int		curpos = 0, flag, recsize, ier;
    char	timeFlag[ 32 ], areastr[ 16 ], *cptr;
    Boolean	more   = G_TRUE;

    VG_DBStruct	el;
/*---------------------------------------------------------------------*/


    while( more && ( (long)curpos < fileLen ) ) {

       cvg_rdhdr( vgfile, vgFilePtr, curpos, (int)fileLen, &el, &flag, &ier );
       if( ier < 0 ) {
           *iret = -2;
	   more = G_FALSE;
       }
       else {
	   recsize = el.hdr.recsz;

           if( el.hdr.delete == 0 ) {
               cvg_rdele( &el, curpos, recsize, vgFilePtr, &ier );
	       if( ier < 0 ) {
	           *iret = ERROR_READING_VG_ELEMENT;
		   more = G_FALSE;
	       }

	       if( more && el.hdr.vg_type == GFA_ELM ) {

	           /*
		    *  Check if this is an F/BB/A element, and verify a match 
		    *  on requested FA Area and hazard with respect to 
		    *  bulletin type.  
		    */
		   if( af_elmOk2Use( &el, area, bulType ) ) {

		       /*
		        *  Extract the info from the element and dump it into
			*  the outputXml.
			*
			*  Note: if the first area matches "area", extract for that area.
			*        if it does not match, check if the second area matches "area". 
			*        This check is for FBBAs that cross over 2 FA areas.      
			*/
                       cvg_getFld( &el, TAG_GFA_OVERRIDE_TM, timeFlag, &ier );
		       *overrideTm = ( atoi( timeFlag ) == 0 ) ? False : True;
                       
		       cvg_getFld( &el, TAG_GFA_AREAS, areastr, &ier );
                       cptr = strtok( areastr, "-" );            
		       
		       if ( strstr( cptr, area ) ) {
		           af_elm2Xml( area, &el, outputXml, &ier );		       
		       }
		       else {
		           cptr = strtok ( NULL, "-" );	
                           if ( cptr ) {
		               af_elm2Xml( cptr, &el, outputXml, &ier );	                       		       
		           }
		       }
		   } 
	       }

               cvg_freeElPtr( &el );
	   }

	   curpos += recsize;
       }
    }

}

/*=====================================================================*/

static void af_elm2Xml ( char *area, VG_DBStruct *el, char **outputXml, 
			 int *iret ) 
/************************************************************************
 * af_elm2Xml                                                           *
 *                                                                      *
 * This routine builds the xml from the input F/BB/A element and adds it*
 * to the outputXml document.              				* 
 *									*
 *									*
 * NOTE:  The order in which the xml elements are written to the        *
 * outputXml string must match the order of the elements specified in   *
 * the airmet.xsd document's smearType definition.  If the order does   *
 * not match then the outpuXml document will be invalid and may not     *
 * transform correctly or at all someday (if the xml lib gets updated).	*
 * The best bet is to keep the order the same.  To make sure the 	*
 * outputXml document is correct, dump it out to a file and then use	*
 * oxygen or some other editor to validate it against the airmet.xsd	*
 * schema.								*
 *                                                                      *
 *									*
 * static void af_elm2Xml ( *area, *el, **outputXml, *iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*area		char		FA Area				*
 *      *el		VG_DBStruct	F/BB/A element			*
 *									*
 * Input/Output parameters:						*
 *      **outputXml	char		output xml document		*
 *									*
 * Output parameters:							*
 *      *iret		int		return code          		*
 *					   0 = Normal			*
 *					  -1 = unable to get subType	*
 *					  -2 = not FBBA element		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	07/07	initial coding     			*
 * J. Wu/SAIC		10/07	prefix "H" for TURB-HI & "L" for TURB-LO*
 * B. Yin/SAIC          03/08   dynamically allocate memory for lat/lon *
 * S. Jacobs/NCEP	10/10	Added check for closed FZLVL lines	*
 ***********************************************************************/
{
    char        tmpStr[ 256 ], hazardType[ 32 ], stateList[ 256 ];
    char        orderedStateList[ 256 ], airmetTag[ 32 ], cval[ 32 ];
    char        fmLineStr[ 256 ], cleanFmLineStr[ 256 ];
    char        *latLonStr;
    int         subType, npts, ii, ier;
    int         fromType;

    Boolean     isAirmet    = G_FALSE;
    Boolean     useFromLine = G_TRUE;
    Boolean     addAirmetTag;
/*---------------------------------------------------------------------*/
    *iret = 0;

    cvg_getFld( el, TAG_GFA_SUBTYPE, tmpStr, &ier );
    if( ier >= 0 ) {
        subType = atoi( tmpStr ) % 10;
    }
    else {
        *iret = -1;
        return;
    }

    if( subType != GFA_FBBA_AIRMET && subType != GFA_FBBA_OUTLOOK ) {
        *iret = -2;
        return;
    }


    if( subType == GFA_FBBA_AIRMET ) {
        isAirmet = G_TRUE;
    }


    /*
     *  Get element's hazard type
     */
    cvg_getFld( el, TAG_GFA_AREATYPE, hazardType, &ier );
    af_TURBRenameHILO ( hazardType );

    /*
     *  LLWS, FZLVL, and M_FZLVL cannot have outlooks.  Ignore these
     *  combinations if found for some reason.
     */
    if( !isAirmet && strcasecmp( hazardType, "LLWS" ) == 0 ) return;
    if( !isAirmet && strcasecmp( hazardType, "FZLVL" ) == 0 ) return;
    if( !isAirmet && strcasecmp( hazardType, "M_FZLVL" ) == 0 ) return;


    /*
     *  Start xml string
     */
    if( isAirmet ) {
        af_catStr ( outputXml, "<smear>" );
    }
    else {
        af_catStr ( outputXml, "<outlook>" );
    }

    af_catStr( outputXml, "<hazard>" );
    af_catStr( outputXml, hazardType );
    af_catStr( outputXml, "</hazard>" );


    /*
     *  State List -- this needs to be re-ordered for the FA Area.
     */
    cvg_getFld( el, TAG_GFA_STATESLIST, stateList, &ier );
    af_orderStateList( area, stateList, orderedStateList, &ier );
    if( ier >= 0 ) {
        af_catStr ( outputXml, "<stateList>" );
        af_catStr ( outputXml, orderedStateList );
        af_catStr ( outputXml, "</stateList>" );
    }


    /*
     * Add Lat/Lon & Close Flag info
     */
    af_catStr ( outputXml, "<closeFlg>" );
    sprintf( tmpStr, "%d", (int)(el->hdr.closed) );
    af_catStr ( outputXml, tmpStr );
    af_catStr ( outputXml, "</closeFlg>" );

    af_catStr ( outputXml, "<nLatLonPts>" );
    sprintf( tmpStr, "%d", el->elem.gfa.info.npts );
    af_catStr ( outputXml, tmpStr );
    af_catStr ( outputXml, "</nLatLonPts>" );

    af_catStr ( outputXml, "<latPts>" );

    npts = el->elem.gfa.info.npts;
    G_MALLOC ( latLonStr, char, npts*10, "af_elm2xml latLonStr" );
    latLonStr[0] = '\0';

    for ( ii = 0; ii < npts; ii++ ) {
        sprintf( &(latLonStr[strlen(latLonStr)]), "%-8.4f ", el->elem.gfa.latlon[ii]);
    }
    latLonStr[strlen(latLonStr)-1] = '\0';

    af_catStr ( outputXml, latLonStr );
    G_FREE( latLonStr, char );
    af_catStr ( outputXml, "</latPts>" );

    af_catStr ( outputXml, "<lonPts>" );

    npts = el->elem.gfa.info.npts;
    G_MALLOC ( latLonStr, char, npts*11, "af_elm2xml latLonStr" );
    latLonStr[0] = '\0';

    for ( ii = npts; ii < 2*npts; ii++ ) {

         sprintf( &(latLonStr[strlen(latLonStr)]), "%-9.4f ", el->elem.gfa.latlon[ii]);
    }
    latLonStr[strlen(latLonStr)-1] = '\0';

    af_catStr ( outputXml, latLonStr );
    G_FREE( latLonStr, char );
    af_catStr ( outputXml, "</lonPts>" );


    /*
     *  NOTE TO SELF:
     *  
     *  The FROM line should be saved in the element when it is created/edited,
     *  non?  We shouldn't be re-calculating it here.
     */


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

    if( isAirmet ) {
        cvg_getFld ( el, TAG_GFA_AREATYPE, tmpStr, &ier );
	if ( strcasecmp ( tmpStr, "LLWS" ) == 0 ||
	     strcasecmp ( tmpStr, "M_FZLVL") == 0 ||
	     strcasecmp ( tmpStr, "FZLVL") == 0 ) {
	    useFromLine = False;
	}

	if( strcasecmp( tmpStr, "FZLVL" ) == 0 && !(el->hdr.closed) ) {
	    fromType = NO_POINT_REORDER; 
	}
    }
    else {	  		/* outlook, not a smear */
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


        af_catStr ( outputXml, "<fromLine>" );
        af_catStr ( outputXml, cleanFmLineStr );
        af_catStr ( outputXml, " </fromLine>" );
    }


    /*
     *  Frequency, Severity
     */
    af_addTags ( el, "<Frequency>", "<Frequency>", outputXml );
    af_addTags ( el, "<Severity>", "<Severity>", outputXml );

    /*
     *  Add any top/base Flight Levels 
     */
    af_addTags ( el, "<Top>", TAG_GFA_TOP, outputXml );
    af_addTags ( el, "<Base>", TAG_GFA_BOTTOM, outputXml );

    /*
     *  Add the freezing level base and top and FZLVL Level value.
     */                   
    af_addTags ( el, "<FzlTop>", TAG_GFA_FZL_TOP, outputXml );
    af_addTags ( el, "<FzlBase>", TAG_GFA_FZL_BOTTOM, outputXml );
    af_addTags ( el, "<Level>", "Level", outputXml );


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
        af_catStr ( outputXml, "<DUE_TO>" );
        af_catStr ( outputXml, tmpStr );
        af_catStr ( outputXml, "</DUE_TO>" );
    }

    af_addTags ( el, "<Status>", TAG_GFA_STATUS, outputXml );

    /*
     *  Conditions wording
     */
    if( isAirmet ) {
        af_addTags ( el, "<fromCondsDvlpg>", TAG_GFA_CONDSBEGIN, outputXml );
        af_addTags ( el, "<fromCondsEndg>", TAG_GFA_CONDSEND, outputXml );
    }
    else {
	af_catStr( outputXml, "<genOlk> TRUE </genOlk>" );
        af_addTags ( el, "<otlkCondsDvlpg>", TAG_GFA_CONDSBEGIN, outputXml );
        af_addTags ( el, "<otlkCondsEndg>", TAG_GFA_CONDSEND, outputXml );
    }

    /*
     *  Add airmet tag.
     */
    addAirmetTag = True;
    ctb_pfstr ( "ADD_AIRMET_TAG", cval, &ier );
    if ( ier >= 0 ) {
        if( strcasecmp( cval, "TRUE" ) != 0 ) {
            addAirmetTag = False;
        }
    }
    
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
       	af_catStr ( outputXml, tmpStr );

    }
    
    if( isAirmet ) {
        af_catStr( outputXml, "</smear>" ); 
    }
    else {         
        af_catStr( outputXml, "</outlook>" ); 
    }

}

/*=====================================================================*/

static void af_getFzlvlRngXml( char *vgfile, FILE *vgFilePtr, long fileLen,
				 char *area, char **outputXml, int *iret )
/************************************************************************
 * af_getFzlvlRngXml                                                    *
 *                                                                      *
 * This routine builds the <freezingRange> element in the outputXml     *
 * document.  The <freezingRange> has two elements:  <Top> and <Base>.	* 
 * The top/base values are aquired in 1 of 3 ways:			*
 *  	1.  taken from any FZLVL element (if it was created using       *
 *          gdfrzl and therefore has a freezingRange field)		*
 *      2.  by extrapolating the base/top information those FZLVL       *
 *          contours and M_FZLVL hazards that intersect the FA Area,  	*
 *      3.  by finding the nearest FZLVL contour outside of the FA Area.*
 *                                                                      *
 * static void af_getFzlvlRngXml ( vgfile, vgFilePtr, fileLen, area,	*
 *				outputXml, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*vgfile		char	vg file name and path			*
 *	*vgFilePtr	FILE	pointer to vg file			*
 *	fileLen		long	length of vg file			*
 *      *area		char	FA area					*
 *									*
 * Input/Output parameters:                                             *
 *	**outputXml	char	the output xml document        		*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 *				 0:	normal				*
 *				-1:	no VG file			*
 *				 2:	different ranges are found	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	08/07	initial coding     			*
 * B. Yin/SAIC		12/07	set return code to 2 if there are 	*
 *				different freezing level ranges		*
 ***********************************************************************/
{
    int			ier;
    char		topStr[ 12 ], bottomStr[ 12 ];
/*---------------------------------------------------------------------*/

    *iret = 0;

    if( !vgFilePtr || fileLen <= 0 ) {
        *iret = -1;
	return;
    }

    /*
     *  Plan A:  extract the FZLVL range (base and top values) from any 
     *  FZLVL element (gdfrzl loads the levels for each FA Area into 
     *  each FZLVL element.
     *
     *  Plan B:  infere the FZLVL range by examining all FZLVLs and 
     *  M_FZLVL areas that intersect the specified FA Area.
     * 
     *  Plan C:  get the FZLVL range by finding the nearest FZLVL not in
     *  the specified FA Area and extrapolate the fzlvl information from that.
     */
    af_getFzlvlRng( vgfile, vgFilePtr, fileLen, area, topStr, bottomStr, &ier );


    if( ier != 0 && ier != 2 ) {

        af_inferFzlvlRange( vgfile, vgFilePtr, fileLen, area, 
    				topStr, bottomStr, &ier );

	if( ier != 0 || strlen( topStr ) <= 0 ) {

            af_getExternalFzlvlRng( vgfile, vgFilePtr, fileLen, area, 
    				topStr, bottomStr, &ier );

        }		
    }
    else if ( ier == 2 ) { 

        *iret = 2;

    }

    af_catStr( outputXml, "<freezingRange>" );

    af_catStr( outputXml, "<Top>" );
    af_catStr( outputXml, topStr );
    af_catStr( outputXml, "</Top>" );

    af_catStr( outputXml, "<Base>" );
    af_catStr( outputXml, bottomStr );
    af_catStr( outputXml, "</Base>" );

    af_catStr( outputXml, "</freezingRange>" );


}

/*=====================================================================*/

static void af_getFzlvlRng( char *vgfile, FILE *vgFilePtr, long fileLen,
			 char *area, char *topStr, char *bottomStr, int *iret )
/************************************************************************
 * af_getFzlvlRng                                                 	*
 *                                                                      *
 * Search the input vgFilePtr and try to find the freezing range field  *
 * within any FZLVL element.  If the FZLVL contours were created using  *
 * gdfrzl then they will have the grid-derived freezing level top and   *
 * bottom for each FA Area.  Return the top and bottom for the          *
 * requested area. 							*
 *                                                                      *
 * static void af_getFzlvlRng ( vgfile, vgFilePtr, fileLen, area,	*
 *				topStr, bottomStr, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*vgfile		char	vg file name and path			*
 *	*vgFilePtr	FILE	pointer to vg file			*
 *	fileLen		long	length of vg file			*
 *      *area		char	FA area					*
 *									*
 * Output parameters:							*
 *	*topStr		char	top level				*
 *	*bottomStr	char	bottom level				*
 *	*iret		int	return code				*
 *					 0 = Normal			*
 *					-1 = error rewinding file	*
 *					 1 = no top/bottom values found *
 *					 2 = found two or more ranges	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	08/07	initial coding       			*
 * B. Yin/SAIC		11/07 	fix top/bottom gibberish 		* 
 * B. Yin/SAIC		12/07	check different freezing level ranges	*
 * B. Yin/SAIC		03/08	account for multi-freezing level ranges	*
 * B. Yin/SAIC		04/08	return 1 if range not found		*
 * B. Yin/CWS		01/11	check negative freezing levels 		*
 *				and set them to 0			*
 ***********************************************************************/
{
    int                 ier, curPos;
    char                rangeStr[ STD_STRLEN ], tmpRange[ STD_STRLEN ];
    char                *aRange, *delim, *top, *bottom, frctHr[32];
    char                hazard[32], areaStr[64], mbase[32];
    char                maxTop[16], minBase[16], mtop[32], tmp[32];

    Boolean             found;
    VG_DBStruct         el;
/*---------------------------------------------------------------------*/

    *iret          = 0;
    topStr[ 0 ]    = '\0';
    bottomStr[ 0 ] = '\0';

    maxTop[ 0 ]    = '\0';
    minBase[ 0 ]   = '\0';

    /*
     *  Rewind vgFilePtr to the beginning of the file.
     */
    cfl_seek( vgFilePtr, 0, SEEK_SET, &ier );
    if (ier < 0 ) {
        *iret = -1;
        return;
    }

    curPos = 0;
    ier    = 0;
    found  = G_FALSE;

    rangeStr[ 0 ] = '\0';

    while( curPos < fileLen ) {

        cvg_rdrecnoc( vgfile, vgFilePtr, curPos, &el, &ier );
        if( ier < 0 ) break;

        if( el.hdr.recsz > 0 ) {
            curPos += el.hdr.recsz;

            if ( !el.hdr.delete ) {

               cvg_getFld( &el, TAG_GFA_AREATYPE, hazard, &ier );

               if ( strcasecmp ( hazard, "M_FZLVL" ) == 0 ) {

                  /*
                   *  Retrieve top/bottom from M_FZLVL.
                   */
                  cvg_getFld( &el, TAG_GFA_AREAS, areaStr, &ier );

                  if ( strstr( areaStr, area ) ) {

                     cvg_getFld( &el, TAG_GFA_TOP, mtop, &ier );
                     cvg_getFld( &el, TAG_GFA_BOTTOM, mbase, &ier );

                     /*
                      *  Determine the minBase, maxTop values and save
                      */
                     if ( strlen( minBase ) <= 0 ) {

                          strcpy( minBase, mbase );

                     }
                     else {

                          af_getMinBase( mbase, minBase, tmp, &ier );
                          if( ier >= 0 ) strcpy( minBase, tmp );

                    }

                    if ( strlen( maxTop ) <= 0 ) {

                       strcpy( maxTop, mtop );
                    }
                    else {

                       if ( atoi( mtop ) > atoi( maxTop ) ) {

                          strcpy( maxTop, mtop );

                       }
                    }
                  }
               }
               else {

                  /*
                   *  Retrieve range info only from smears/outlooks.
                   */
                  cvg_getFld( &el, TAG_GFA_FCSTHR, frctHr, &ier );

                  if ( strchr( frctHr, '-' ) != NULL ) {

                     tmpRange[ 0 ] = '\0';
                     cvg_getFld( &el, TAG_GFA_FZLRANGE, tmpRange, &ier );

                     if ( ier >= 0 && strlen( tmpRange ) > 0 && !el.hdr.delete ) {

                        if ( !found ) {

                           found = G_TRUE;
                           strcpy( rangeStr, tmpRange );

                        }
                        else if ( strcmp( rangeStr, tmpRange ) != 0 ) {


                           /*
                            *  Set return code to 2 if there are different ranges.
                            */
                           *iret = 2;

                        }
                     }
                  }
               }
            }

            cvg_freeElPtr( &el );
        }
    }


    if( found ) {
        aRange = NULL;
        aRange = strstr( rangeStr, area );

        if( aRange ) {
           if( ( delim = strchr( aRange, ';' ) ) != NULL ) {
              top = delim + 1;
              sprintf( topStr, "%03i", (atoi( top )<0)?0:atoi(top) );
              topStr[3] = '\0';
           }
           if( ( delim = strchr( delim+1, ';') ) != NULL ) {
              bottom = delim+1;
              sprintf( bottomStr, "%03i", (atoi( bottom )<0)? 0:atoi( bottom ) );
              bottomStr[3] = '\0';
           }
        }

	/*
	 * account for multi-frezing levels.
	 */
        if ( strlen( minBase ) > 0 ) {

           if ( strlen( bottomStr ) > 0 ) {

              af_getMinBase( minBase, bottomStr, tmp, &ier );
              if( ier >= 0 ) strcpy( bottomStr, tmp );

           }
           else {

              strcpy ( bottomStr, minBase );

           }
        }

        if ( strlen( maxTop ) > 0 ) {

           if ( strlen( topStr ) > 0 ) {

              if ( atoi( maxTop ) > atoi( topStr ) ) {

                 strcpy( topStr, maxTop );

              }
           }
           else {

              strcpy( topStr, maxTop );

           }
        }

        if( strlen( topStr ) <= 0 || strlen( bottomStr ) <= 0 ) {
            *iret = 1;
        }
    }
    else {
	
	*iret = 1;

    }

}

/*=====================================================================*/

static void af_inferFzlvlRange( char *vgfile, FILE *vgFilePtr, long fileLen,
			 char *area, char *topStr, char *bottomStr, int *iret )
/************************************************************************
 * af_inferFzlvlRange                                                   *
 *                                                                      *
 * Search the input vgFilePtr and find all the GFA FZLVL elements that  *
 * match the specified area input.  Return the top and bottom freezing  *
 * level values by adding 4000 feet to the top and subtracting 4000     *
 * the bottom (assuming it isn't surface).                              *
 *                                                                      *
 * static void af_inferFzlvlRange ( vgfile, vgFilePtr, fileLen, area,	*
 *				topStr, bottomStr, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*vgfile		char	vg file name and path			*
 *	*vgFilePtr	FILE	pointer to vg file			*
 *	fileLen		long	length of vg file			*
 *      *area		char	FA area					*
 *									*
 * Output parameters:							*
 *	*topStr		char	top level				*
 *	*bottomStr	char	bottom level				*
 *	*iret		int	return code				*
 *					 0 = Normal			*
 *					-1 = error rewinding file	*
 *					 1 = no top/bottom values found *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	08/07	initial coding       			*
 * B. Yin/SAIC		04/08	change SFC_FZLVL to FZLVL_SFC		*
 ***********************************************************************/
{
    int			ier, curPos;
    char		hazard[ STD_STRLEN ],  tmp[ STD_STRLEN ];
    char		areaStr[ STD_STRLEN ], levelStr[ STD_STRLEN ];
    char		minBase[ STD_STRLEN ], maxTop[ STD_STRLEN ];
    char		top[ STD_STRLEN ],     base[ STD_STRLEN ];

    VG_DBStruct		el;
/*---------------------------------------------------------------------*/

    *iret          = 1;
    topStr[ 0 ]    = '\0';
    bottomStr[ 0 ] = '\0';

    maxTop[ 0 ]    = '\0';
    minBase[ 0 ]   = '\0';

    /*
     *  Rewind vgFilePtr to the beginning of the file.
     */
    cfl_seek( vgFilePtr, 0, SEEK_SET, &ier );
    if (ier < 0 ) {
	*iret = -1;
	return;
    }

    curPos = 0;
    ier    = 0;

    while( curPos < fileLen ) {

        cvg_rdrecnoc( vgfile, vgFilePtr, curPos, &el, &ier );
        if( ier < 0 ) break;

	if( el.hdr.recsz > 0 ) {
            curPos += el.hdr.recsz;


            if ( (int)el.hdr.vg_type != GFA_ELM || el.hdr.delete ) {
                cvg_freeElPtr( &el );
                continue;
	    }

	       
  	    cvg_getFld( &el, TAG_GFA_AREATYPE, hazard, &ier );

            if ( strcasecmp ( hazard, "FZLVL" ) != 0 &&
	         strcasecmp ( hazard, "FZLVL_SFC" ) != 0 &&
		 strcasecmp ( hazard, "M_FZLVL" ) != 0 ) {
                cvg_freeElPtr( &el );
                continue;
	    }

		 
            cvg_getFld( &el, TAG_GFA_AREAS, areaStr, &ier );
	    if( !strstr( areaStr, area ) ) {
                cvg_freeElPtr( &el );
                continue;
	    }


	    /*
	     *  Get the Base and Top values
	     */
	    if( strcasecmp( hazard, "FZLVL" ) == 0 ||
	        strcasecmp( hazard, "FZLVL_SFC" ) == 0 ) {
			
                cvg_getFld( &el, "Level", levelStr, &ier );
                af_getFLBaseTop( levelStr, base, top, &ier );

	    }
	    else if ( strcasecmp( hazard, "M_FZLVL" ) == 0 ) {
                cvg_getFld( &el, TAG_GFA_TOP, top, &ier ); 
                cvg_getFld( &el, TAG_GFA_BOTTOM, base, &ier ); 
            }


	    /*
	     *  Determine the minBase, maxTop values and save
	     */
	    if( strlen( minBase ) <= 0 ) {
		strcpy( minBase, base );
	    }
	    else {
		af_getMinBase( base, minBase, tmp, &ier );
		if( ier >= 0 ) strcpy( minBase, tmp );
	    }


	    if( strlen( maxTop ) <= 0 ) {
		strcpy( maxTop, top );
	    }
	    else {
		if( atoi( top ) > atoi( maxTop ) ) {
		    strcpy( maxTop, top );
		}
	    }

            cvg_freeElPtr( &el );
	}

    }

    strcpy( topStr, maxTop );
    strcpy( bottomStr, minBase );

    if( strlen( bottomStr ) > 0 && strlen( topStr ) > 0 ) *iret = 0; 

}

/*=====================================================================*/

static void af_orderStateList ( char *area, char *origList, 
				char *orderedList, int *iret ) 
/************************************************************************
 * af_orderStateList                                                    *
 *                                                                      *
 * This routine puts the input list of states in the correct order for  *
 * the specified FA Area.  The order will always be those states in the	* 
 * FA area first (in their correct order), then those states in the     *
 * adjacent FA Area, if any.  Two letter abreviations for the Great     *
 * Lakes (LH, LS, LM, LE, LO) are valid "states" for these purposes. 	*
 *									*
 * The phrase "CSTL WTRS" or "AND CSTL WTRS" is also valid in the state *
 * list, but must always be placed at the end of the re-ordered state   *
 * list.  Note that "AND" means the airmet/outlook covers both land and *
 * water, and that the absence of "AND" indicates the airmet/outlook is *
 * _only_ over water.							*
 *                                                                      *
 * NOTE:  The MT_OBSC hazard has a limited list of states in which it   *
 * can occur.  It is presumed that either the user has not included     *
 * those states (if the GFA F/BB/A element was drawn manually) or that  *
 * the F/BB/A format process has already weeded the non-MT_OBSC states  *
 * out.									*
 *   									*
 * static void af_orderStateList ( area, origList, orderedList )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *area		char	FA Area					*
 *      *origList	char	potentially un-ordered list of states	*
 *									*
 * Output parameters:                                                   *
 *	*orderedList	char	ordered state list			*
 *	*ier		int	return code				*
 *				   0 = Normal				*
 *				  -1 = Error processing area states	*
 *				  -2 = Error processing adjacent area 	*
 *				  -3 = invalid input area 		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	08/06	initial coding        			*
 * B. Yin/SAIC		03/08 	Remove the extra space before CSTL WTRS	*
 ***********************************************************************/
{
    char	ptype[] = "AIRMET",      adjArea[ STD_STRLEN ];
    char	finalList[ STD_STRLEN ], adjStList[ STD_STRLEN ];

    char	localOrig[ STD_STRLEN ];
    char	*and = NULL, *cstl = NULL, *wtrs = NULL;

    int		ii, ier;

    Boolean	containsAND  = G_FALSE, containsCSTL = G_FALSE;
/*---------------------------------------------------------------------*/

    strcpy( localOrig, origList );

    cstl = strstr( localOrig, "CSTL" );
    wtrs = strstr( localOrig, "WTRS" );
    and  = strstr( localOrig, "AND" );

    /* 
     *  Remove "AND" "CSTL" and "WTRS" from localOrig by substituting
     *  spaces.  The clo_fastates() routine (used below) recognizes the
     *  phrase "CSTL WTRS" as valid for those FA area which have coastal
     *  waters.  The problem is we always want this phrase at the end of
     *  the states list, regardless of whether its found in the area or the
     *  adjacent area.  So we'll set some flags if it's found and add the
     *  phrase back in once we have the states in the right order. 
     */
    if( cstl ) {
	containsCSTL = G_TRUE;
	for( ii=0; ii<4; ii++ ) {
	    cstl[ ii ] = ' ';
	}
    }

    if( wtrs ) {
	for( ii=0; ii<4; ii++ ) {
	    wtrs[ ii ] = ' ';
	}
    }

    if( and  ) {
	containsAND  = G_TRUE;
	for( ii=0; ii<3; ii++ ) {
	    and[ ii ] = ' ';
	}
    }


    /*
     *  Initialize iret and copy origList to orderedList to provide a 
     *  default value in case error prevents processing state list.
     */
    *iret = 0;
    strcpy( orderedList, origList );


    af_getAdjArea( area, adjArea, &ier );
    if( ier < 0 ) {
	*iret = -3;
        return;
    }

    clo_fastates( area, localOrig, ' ', ptype, finalList, &ier );
    if( ier < 0 ) {
	*iret = -1;
	return;
    }

    clo_fastates( adjArea, localOrig, ' ', ptype, adjStList, &ier );
    if( ier >= 0 ) {
	if ( strlen( adjStList ) > 0 ) {
	   strcat( finalList, " " );
	}
	strcat( finalList, adjStList );
    }
    else {
	*iret = -2;
	return;
    }

    strcpy( orderedList, finalList );

    if( containsAND && containsCSTL ) {
        strcat( orderedList, " AND CSTL WTRS" );
    }
    else if( containsCSTL ) {
	strcat( orderedList, " CSTL WTRS" );
    }

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
/*
Boolean af_overrideIssueTm( const char *area, const char *airmetType, 
				   int nin, const GFA_Elem_Format *fmt_in, 
   				   int *iret )
*/
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
 ***********************************************************************/
/* {
    int		ii, ier;
    char	hazList[ STD_STRLEN ], cat[ 32 ];
    char	stat_val[32];
    Boolean	override = False;
*/
/*--------------------------------------------------------------------*/
/*
    *iret = 0;

    for( ii = 0; ii < nin && !override; ii++ ) {
*/
        /*
	 *  Ignore if the element is marked as deleted
	if ( fmt_in[ii].delete ) {
	    continue;
	}
         */

	/*
	 *  Ignore if the element's area doesn't match
	if( strcasecmp( area, fmt_in[ii].area ) != 0 ) {
	    continue;
	}
	 */

	/*
	 *  Ignore if the element's type doesn't match
	cvg_getFld ( &(fmt_in[ii].el), TAG_GFA_AREATYPE, hazList, &ier );
	if( ier < 0 ) {
	    *iret = -1;
	    break;
	}
	ctb_gfagcat ( hazList, cat, &ier );

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
	 */

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
static void af_getFLBaseTop( char *level, char *base, char *top, int *iret )
/************************************************************************
 * af_getFLBaseTop                                                      *
 *                                                                      *
 * This routine takes a given Flight level (FL) and returns 4,000 feet	*
 * below and above the FL as the base and top values.  These are 	*
 * the theoretical base and top values for a given FA area that has a   *
 * FZLVL contour of the given FL in it.  				*
 *									*
 * Note:  If the input level value is SFC or 000 the base remains SFC.  *
 * IF the FL is "FZL" (which is a valid Flight level) cannot be 	*
 * evaluated and no base or top value is returned.              	*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *level		char	flight level value              	*
 *                                                    			*
 * Output parameters:                                             	*
 *	*base		char	flight level - 4,000 ft			*
 *	*top		char	flight level + 4,000 ft			*
 *      *iret           int     Return code                     	*
 *                                       0: normal return               *
 *					-1:  level string empty   	*
 *					-2:  FL is "FZL"          	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	08/07	initial coding                        	*
 ***********************************************************************/
{
    int		ilevel, ibase = IMISSD, itop = IMISSD;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    base[ 0 ] = '\0';
    top[ 0 ]  = '\0';


    if( !level || ( strlen( level ) <= 0 ) ) {
	*iret = -1;
    }
    else if( strncasecmp( level, "FZL", 3 ) == 0 ) {
	*iret = -2;
    }

    if( *iret != G_NORMAL ) return;


    if( strncasecmp( level, "SFC", 3 ) == 0 ) {
	ibase = 0;
	itop  = 40;
    }
    else {
	ilevel = atoi( level );
	if( ilevel == 0 ) {
	    ibase = 0;
	    itop  = 40;
	}
	else {
	    ibase  = ilevel - 40;
	    itop   = ilevel + 40;
	}
    }

    if( ibase == 0 ) {
	strcpy( base, "SFC" );
    }
    else if (ibase > 0 ) {
	sprintf( base, "%03d", ibase );
    }

    if( itop > 0 ) {
        sprintf( top,  "%03d", itop );	
    }

}

/*=====================================================================*/

static void af_getExternalFzlvlRng( char *vgfile, FILE *vgFilePtr, 
			long fileLen, char *area, char *topStr, 
			char *bottomStr, int *iret )
/************************************************************************
 * af_getExternalFzlvlRng                                               *
 *                                                                      *
 * This routine returns the top and base levels for a FA area that is   *
 * not intersected by any FZLVL contours. It sorts all external fzlvls	*
 * by distance from the area centroid, then uses the level from that	*
 * contour.  If the contour is to the left of the area then the answer  *
 * is base = contour level, top = base + 040.  If the contour is to the *
 * right of the area then the answer top = contour level, base = top -  *
 * 040.									*
 *                                                                      *
 * static void af_getExternalFzlvlRng ( vgfile, vgFilePtr, filelen,     *
 *			area, topStr, bottomStr, *iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*vgfile		char	vg file name and path			*
 *	*vgFilePtr	FILE	pointer to vg file			*
 *	fileLen		long	length of vg file			*
 *      *area		char	FA area					*
 *									*
 * Output parameters:							*
 *	*topStr		char	top level				*
 *	*bottomStr	char	bottom level				*
 *	*iret		int	return code				*
 *					 0 = Normal			*
 *					-1 = error rewinding file	*
 *					-2 = unable to match area	*
 *					 1 = no top/bottom values found *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	08/07	initial coding       			*
 * B. Yin/SAIC		04/08	change SFC_FZLVL to FZLVL_SFC		*
 ***********************************************************************/
{
    int			areaIdx = -1, ii, nearestVert, closed;
    int			ier, curPos, nearest = -1, npts, position;
    int			frzlvl;

    float		nearestDist = 9999.0, distance, tol = 0.0F;

    char		hazard[ STD_STRLEN ], levelStr[ STD_STRLEN ];
    char		contourStr[ STD_STRLEN ];

    static float	xAreaCentroid[ NUM_FA_AREAS ];
    static float	yAreaCentroid[ NUM_FA_AREAS ];
    static Boolean	loadCentroid = G_FALSE;

    float		*xnormal, *ynormal;
    VG_DBStruct		el;
/*---------------------------------------------------------------------*/

    *iret    = 0;
    topStr[ 0 ]    = '\0';
    bottomStr[ 0 ] = '\0';

    /*
     *  Load the area centroid array.  These will be normalized coords.
     */
    if ( ! loadCentroid ) {
        af_loadCentroids ( xAreaCentroid, yAreaCentroid, &ier );
        loadCentroid = G_TRUE;
    }

    /*
     *  Find the FA Area index
     */
    for( ii=0; ii< NUM_FA_AREAS; ii++ ) {
        if( strcasecmp( _FA_Area[ii], area ) == 0 ) {
            areaIdx = ii;
	    break;
	}
    }

    if( areaIdx < 0 ) {
	*iret = -2;
	return;
    }


    /*
     *  Rewind vgFilePtr to the beginning of the file.
     */
    cfl_seek( vgFilePtr, 0, SEEK_SET, &ier );
    if (ier < 0 ) {
	*iret = -1;
	return;
    }

    curPos = 0;
    ier    = 0;

    /* 
     *  Find the FZLVL contour nearest to the FA Area centroid.
     */
    while( curPos < fileLen ) {

        cvg_rdrecnoc( vgfile, vgFilePtr, curPos, &el, &ier );
        if( ier < 0 ) break;

	if( el.hdr.recsz > 0 ) {
            curPos += el.hdr.recsz;

            if ( (int)el.hdr.vg_type != GFA_ELM || el.hdr.delete ) {
                cvg_freeElPtr( &el );
                continue;
	    }

  	    cvg_getFld( &el, TAG_GFA_AREATYPE, hazard, &ier );

            if ( strcasecmp ( hazard, "FZLVL" ) != 0 &&
	         strcasecmp ( hazard, "FZLVL_SFC" ) != 0 ) {
                cvg_freeElPtr( &el );
                continue;
	    }

	    npts = el.elem.gfa.info.npts;

	    G_MALLOC ( xnormal, float, npts, "af_getExternalFzlvlRng xnormal" );
	    G_MALLOC ( ynormal, float, npts, "af_getExternalFzlvlRng ynormal" );

	    gtrans( sys_M, sys_N, &npts, (float *)&el.elem.gfa.latlon,
	    	    (float *)&el.elem.gfa.latlon[npts], xnormal, ynormal,
		    &ier, strlen( sys_M ), strlen( sys_N ) );


	    cgr_dist( npts, xnormal, ynormal, xAreaCentroid[ areaIdx ],
	    	      yAreaCentroid[ areaIdx ], &distance, &nearestVert, &ier );
            
            if( nearest < 0 || distance < nearestDist ) {
	        nearest = curPos - el.hdr.recsz;
		nearestDist = distance;

  	        cvg_getFld( &el, "Level", levelStr, &ier );

                closed = 0;
	        cvg_getFld( &el, "Contour", contourStr, &ier );
	        if( strncasecmp( contourStr, "Closed", 6 ) == 0 ) {
                    closed = 1;
                }

		cgr_qrol( &npts, xnormal, ynormal, &closed, 
			  &xAreaCentroid[ areaIdx ], &yAreaCentroid[ areaIdx ],
			  &tol, &position, &ier );

            }


            G_FREE( xnormal, float );
	    G_FREE( ynormal, float ); 

            cvg_freeElPtr( &el );
	}
    }

    if( nearest > 0 ) {

	if( strcasecmp( levelStr, "SFC" ) == 0 ) {
	    frzlvl = 0;
	}
	else {
	    frzlvl = atoi( levelStr );
        }

	if( position == 1 ) {		/* fzlvl is right of area centroid */
	    sprintf( bottomStr, "%03i", frzlvl );
	    sprintf( topStr,    "%03i", frzlvl + 40 );
	}
	else {				/* fzlvl is left of area centroid */
	    strcpy( bottomStr, "SFC" );
	    strcpy( topStr, "SFC" );

	    if( frzlvl > 0 ) {
		sprintf( topStr, "%03i", frzlvl );

		if( frzlvl - 40 > 0 ) {
		    sprintf( bottomStr, "%03i", frzlvl - 40 );
		}
            }
	}
    }

}

/*=====================================================================*/

static void af_getIssueTimes( int *firstIssueTm, int *lastIssueTm, 
				       char *firstCycle, char *lastCycle,
				       int *iret )
/************************************************************************
 * af_getIssueTimes                                                     *
 *                                                                      *
 * This routine gets the first item and the last item of the issue time *
 * array and converts them into minutes. It also returns the first and	*
 * the last cycles							*
 *                                                                      *
 * static void af_getIssueTimes ( firstIssueTm, lastIssueTm, iret ) 	*
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
 * S. Guan/NCEP	        07/20   Added tzone as an input of TI_DST       *  
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

static void af_validateVgFile( char *vgfile, FILE **vgFilePtr, char *fullFileName,
				long *fileLen, int *iret )
/************************************************************************
 * af_validateVgFile	                                                *
 *                                                                      *
 * This routine validates the input vgfile.  If the file is valid a     *
 * file pointer to the open file is returned as well as the expanded    *
 * file name and file length.						*
 *                                                                      *
 * static void af_validateVgFile ( vgfile, vgFilePtr, fullFileName,     *
 *					fileLen, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*vgFile		char	Vg file name and path 			*
 *                                                                      *
 * Output parameters:                                                   *
 *	**vgFilePtr	FILE	file pointer (NULL if any problems)	*
 *	*fullFileName	char	expanded file name			*
 *	*fileLen	long	length of file				*
 *	*iret	        int		return code          		*
 *					  0 = Normal			*
 *					 -5 = Zero length vg file	*
 *					 -6 = Unable to open vg file	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC      08/07   Initial coding                         	*
 ***********************************************************************/
{
    int		ier;
/*---------------------------------------------------------------------*/

    *iret     		= 0;
    *vgFilePtr 		= NULL;
    *fileLen  		= 0;
    fullFileName[ 0 ] 	= '\0';


    /*
     *  Get the vgFile's length.
     */
    cfl_inqr( vgfile, NULL, fileLen, fullFileName, &ier );
    if( ier < 0 ) {
	*iret = ERROR_CANT_OPEN_VG_FILE;
        return;
    }
    if( *fileLen <= 0 ) {
	*iret = ERROR_ZERO_LENGTH_VG_FILE;
 	return;	
    }


    /*
     *  Open the vg file.
     */
    *vgFilePtr = cfl_ropn( fullFileName, NULL, &ier );    
    if( ier < 0 ) {
	*iret = ERROR_CANT_OPEN_VG_FILE;
    }

}
/*=====================================================================*/

static void af_getAdjArea( char *area, char *adjArea, int *iret )
/************************************************************************
 * af_getAdjArea	                                                *
 *                                                                      *
 * This routine takes the input FA area and determines the adjacent     *
 * FA Area (the other area within the area's FA Region).  Note that     *
 * both the area and adjArea will be expressed using the standard 3     *
 * character upper case abreviation for the area name.			*
 *                                                                      *
 * static void af_getAdjArea ( area, adjArea, iret)       		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*area		char	FA Area               			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*adjArea	char	adjacent FA Area name			*
 *	*iret	        int		return code          		*
 *					  0 = Normal			*
 *					 -1 = couldn't match area 	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC      08/07   Initial coding                         	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    *iret = 0;
    adjArea[ 0 ] = '\0';


    if( strcmp( area, "SFO" ) == 0 ) {
	strcpy( adjArea, "SLC" );
    }
    else if( strcmp( area, "SLC" ) == 0 ) {
	strcpy( adjArea, "SFO" );
    }
    else if( strcmp( area, "CHI" ) == 0 ) {
	strcpy( adjArea, "DFW" );
    }
    else if( strcmp( area, "DFW" ) == 0 ) {
	strcpy( adjArea, "CHI" );
    }
    else if( strcmp( area, "BOS" ) == 0 ) {
	strcpy( adjArea, "MIA" );
    }
    else if( strcmp( area, "MIA" ) == 0 ) {
	strcpy( adjArea, "BOS" );
    }


    if( strlen( adjArea ) <= 0 ) {
        *iret = -1;
    }
}

/*=====================================================================*/
