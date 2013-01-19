#include "xmlcmn.h"

#define	BLOCK_SIZE	( 1024 )
#define PGEN		"pgen"


typedef char FieldStr[ BLOCK_SIZE ];


static	FieldStr	*_field;

static	int		_nfields = 0;
static  int		_fieldIdx = -1;
static  int		_nParentFound = False;

static  char		*_parentElPath;
static	char		_curElName[ BLOCK_SIZE ];
static  char		_buffer[ BLOCK_SIZE ];
static  char		*_delim;

static	char		**_outputArray;
static  int		*_outputLen;

static  Boolean		_matchedParent = False;
static  Boolean		_warning = False, _error = False, _fatalError = False;



static void xml_setDelimiter( const char *delim );
static void xml_startDocument( void *userData );
static void xml_endDocument( void *userData ); 
static void xml_characters( void *userData, const xmlChar *ch, int len);
static void xml_startElement( void *ctx, const xmlChar *name,
			    const xmlChar **atts ) ;
static void xml_endElement( void *ctx, const xmlChar *name ); 
static void xml_warning( void *userData, const char *msg, ... );
static void xml_error( void *userData, const char *msg, ... );
static void xml_fatalError( void *userData, const char *msg, ... );
static void xml_initParser( xmlSAXHandler *mySax, int *iret );


/*=====================================================================*/

static void xml_setDelimiter( const char *delim ) 
/************************************************************************
 * xml_setDelimiter 							*
 *									*
 * This function sets the _delim string for use in formatting the       * 
 * output.  If the *delim parameter is NULL then a single blank (" ")   *
 * is set as the delimiter value.                                       *
 * 									*
 * Input parameters:                     	                        *
 *	*delim		const char	delimiter string (may be NULL) 	*	
 *                                                                      *
 * Log:                                                                 *
 * E. Safford/SAIC	03/06   initial coding                          *
 ***********************************************************************/
{
    int		delimLen = 2;
/*---------------------------------------------------------------------*/

    if( delim ) {
	delimLen = strlen( delim );
    }

    G_MALLOC( _delim, char, delimLen, "xml_readTable:_delim" ); 
 
    if( delim != NULL ) { 
	strcpy( _delim, delim );
    } else {
	strcpy( _delim, " " );
    } 
}

/*=====================================================================*/

static void xml_setParentPath( const char *parentPath, int *iret ) 
/************************************************************************
 * xml_setParentPath 							*
 *									*
 * This function sets the _parentElPath string for use in determining   *
 * when the parser gets to the desired parent element.                  *
 * 									*
 * Input parameters:                     	                        *
 *	*parentPath	const char	element path from document root	*	
 *					  to parent element		*
 *                                                                      *
 * Log:                                                                 *
 * E. Safford/SAIC	03/06   initial coding                          *
 ***********************************************************************/
{
    int		len;
/*---------------------------------------------------------------------*/
    *iret = 0;
    if( parentPath == NULL ) {
	*iret = -4;
	return;
    }

    len = strlen( parentPath ) + 1;
    G_MALLOC( _parentElPath, char, len, "xml_readTable:_parentElPath" );

    strcpy( _parentElPath, parentPath );
}

/*=====================================================================*/

/* ARGSUSED */
static void xml_startDocument( void *userData ) { 
/************************************************************************
 * xml_startDocument							*
 *									*
 * This function is called when the parser finds the beginning of an  	* 
 * xml document.  This routine intializes some variables that are used  *
 * to process the document.                                             *
 * 									*
 * Input parameters:                     	                        *
 *	*userData	void		pointer to user data           	*	
 *                                                                      *
 * Log:                                                                 *
 * E. Safford/SAIC	03/06   initial coding                          *
 ***********************************************************************/

    _matchedParent = False;
    _fieldIdx      = -1;
    _nParentFound  = 0;

    _curElName[0]  = '\0';
    _buffer[0]     = '\0';

}

/*=====================================================================*/

static void xml_endDocument( void *userData ) {
}

/*=====================================================================*/

/* ARGSUSED */
static void xml_characters( void          *userData,
			    const xmlChar *ch,
			    int           len) 
/************************************************************************
 * xml_characters							*
 *									*
 * This function is called when the parser finds character data within	* 
 * an element.  The characters are handed to this routine in variable   *
 * lengths, and there is no guarantee that an individual element's data *
 * will arrive in one piece.  Therefore the ch string is written into   *
 * the _buffer.  							* 
 * 									*
 * Input parameters:                     	                        *
 *	*userData	void		pointer to user data (not used)	*	
 *      *ch		const xmlChar	element character data		*
 *	len		int       	length of ch string		*
 *                                                                      *
 * Log:                                                                 *
 * E. Safford/SAIC	03/06   initial coding                          *
 ***********************************************************************/
{
    if( _matchedParent && _fieldIdx >= 0 ) {

        if( strlen( _buffer ) == ( size_t )0 ) {
            strncpy( _buffer, (char *)ch, len );
        }
        else {
	    strncat( _buffer, (char *)ch, len );
        }
    }
}

/*=====================================================================*/

/* ARGSUSED */
static void xml_startElement( void          *ctx,
			      const xmlChar *name,
			      const xmlChar **atts ) 
/************************************************************************
 * xml_startElement							*
 *									*
 * This function is called when the parser finds the start of an	* 
 * element.  The element name is concatenated to the _curElName.  If    *
 * the parent element was previously matched then this routine looks    * 
 * for a match on one of the desired fields (elements).  If the parent  *
 * element was not matched to begin with then this looks for a match on *
 * it.  								*
 * 									*
 * Input parameters:                     	                        *
 *	*ctx		void		pointer to current context   	*	
 *      *name		const char	element name			*
 *	**atts		const char	attribute name(s)		*
 *                                                                      *
 * Log:                                                                 *
 * E. Safford/SAIC	03/06   initial coding                          *
 ***********************************************************************/
{
    int	ii;
    char	*newName;
/*---------------------------------------------------------------------*/

    G_MALLOC( newName, char, xmlStrlen( name ) + 1, "xml_startElement" );
    sprintf( newName, "%s", name );

    strcat( _curElName, "/" );
    strcat( _curElName, newName );


    /*
     *  If the last element matched the parent then check this 
     *  element to see if it's one of the fields we're looking 
     *  for.
     *
     *  Otherwise check for a match on the parent.
     */
    if( _matchedParent ) {

	_fieldIdx = -1;

	for( ii=0; ii<_nfields; ii++ ) {
	    if( strcmp( _field[ii], newName ) == 0 ) {
	        _fieldIdx = ii;
		break;
	    }
	}

	/*
	 *  initialize the buffer if we matched a field.
	 */
	if( _fieldIdx >= 0 ) {
            for( ii=0; ii<BLOCK_SIZE; ii++ ) {
	        _buffer[ ii ] = '\0';
            }
	}
    }
    else {

        if( strcmp( _curElName, _parentElPath ) == 0 ) {
	    _matchedParent = True;
	}
    }

    G_FREE( newName, char );
}

/*=====================================================================*/

static void xml_endElement( void *ctx, const xmlChar *name ) {
/************************************************************************
 * xml_endElement							*
 *									*
 * This function handles the event triggered when the SAX parser finds  *
 * the end of an element.  						*
 *									*
 * It looks to see if the _matchedParent flag   *
 * is True.  This indicates that the
 * 									*
 * Input/Output parameters:                                             *
 *	*mySax		xmlSAXHandler	pointer to SAX handler struct   *	
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = normal                    *
 *					  1 = nfields > number of fields*
 *					 -1 = no table fields specified *
 * Log:                                                                 *
 * E. Safford/SAIC	03/06   initial coding                          *
 ***********************************************************************/

 int	curSz = BLOCK_SIZE;
 char	*ptr = NULL;
/*---------------------------------------------------------------------*/


    /*
     *  If we've just finished reading one of the elements that we
     *  want, add it to the output.
     */
    if( _matchedParent && ( _fieldIdx >= 0 ) ) {

	/*
	 *  If the buffer won't fit into the allocated space, realloc the
	 *  output array field.
	 */
  	while( strlen( _outputArray[ _fieldIdx ] ) + strlen( _buffer ) > 
		(size_t)_outputLen[ _fieldIdx ] ) { 

	    _outputLen[ _fieldIdx ] = _outputLen[ _fieldIdx ] + BLOCK_SIZE;
	    curSz = _outputLen[ _fieldIdx ];

            _outputArray[ _fieldIdx ] = ( char * )realloc( _outputArray[ _fieldIdx ], 
	    		curSz * sizeof( char * ) );

        }
  

	if( strlen( _outputArray[ _fieldIdx ] ) <= ( size_t )0 ) {
	    strcpy( _outputArray[ _fieldIdx ], _buffer );
	}
	else if( strlen( _outputArray[ _fieldIdx ] ) + strlen( _buffer ) < 
			(size_t)_outputLen[ _fieldIdx ] ) {
	    strcat( _outputArray[ _fieldIdx ], _delim );
	    strcat( _outputArray[ _fieldIdx ], _buffer );
	}

	_fieldIdx = -1;
	_buffer[0] = '\0';
    }


    /*
     *  Update _curElName by removing the element just ended.
     */
    ptr = strrchr( _curElName, '/' );
    if( ptr != NULL ) {
        *ptr = '\0';
    }


    /*
     *  Update _matchedParent to see if we still do.
     */
    if( _matchedParent ) {

	if( strcmp( _curElName, _parentElPath ) != 0 ) {
	    _matchedParent = False;
	    _nParentFound++;
	}

    }
}


/*=====================================================================*/

/* ARGSUSED */
static void xml_warning( void *userData, const char *msg, ... ) 
/************************************************************************
 * xml_warning      							*
 *									*
 * This function is called when the parser raises a warning condition.  * 
 * The first msg will be dumped to stdout.				*
 * 									*
 * Input parameters:                     	                        *
 *	*userData	void		pointer to user data           	*	
 *	*msg, ...	const char	variable argument list of msgs  *
 *                                                                      *
 * Log:                                                                 *
 * E. Safford/SAIC	03/06   initial coding                          *
 ***********************************************************************/
{ 
    _warning = True;
    printf("  xml_readTable warning:  %s\n", msg );
}

/*=====================================================================*/

/* ARGSUSED */
static void xml_error( void *userData, const char *msg, ... ) 
/************************************************************************
 * xml_error        							*
 *									*
 * This function is called when the parser encounters a non-fatal error *
 * condition.  The first msg will be dumped to stdout.			* 
 * 									*
 * Input parameters:                     	                        *
 *	*userData	void		pointer to user data           	*	
 *	*msg, ...	const char	variable argument list of msgs  *
 *                                                                      *
 * Log:                                                                 *
 * E. Safford/SAIC	03/06   initial coding                          *
 ***********************************************************************/
{ 
    _error = True;
    printf("  xml_readTable error:  %s\n", msg );
}

/*=====================================================================*/

/* ARGSUSED */
static void xml_fatalError( void *userData, const char *msg, ... ) 
/************************************************************************
 * xml_fatalError      							*
 *									*
 * This function is called when the parser encounters a fatal error 	*
 * condition.  The first msg will be dumped to stdout.			* 
 * 									*
 * Input parameters:                     	                        *
 *	*userData	void		pointer to user data           	*	
 *	*msg, ...	const char	variable argument list of msgs  *
 *                                                                      *
 * Log:                                                                 *
 * E. Safford/SAIC	03/06   initial coding                          *
 ***********************************************************************/
{
    _fatalError = True;
    printf("  xml_readTable fatal error:  %s\n", msg );
}


/*=====================================================================*/

static void xml_initParser( xmlSAXHandler *mySax, int *iret ) 

/************************************************************************
 * xml_initParser							*
 *									*
 * This function initializes the fields of the mySax structure to point *
 * at the correct functions in this module.  These functions are fired  *
 * by the SAX parser when it encounters the specified condition/event.	* 
 * 									*
 * Input/Output parameters:                                             *
 *	*mySax		xmlSAXHandler	pointer to SAX handler struct   *	
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = normal                    *
 *					  1 = nfields > number of fields*
 *					 -1 = no table fields specified *
 * Log:                                                                 *
 * E. Safford/SAIC	03/06   initial coding                          *
 ***********************************************************************/
{
    *iret = 0;

    mySax->internalSubset		= NULL;
    mySax->isStandalone			= NULL;
    mySax->hasInternalSubset		= NULL;
    mySax->hasExternalSubset		= NULL;
    mySax->resolveEntity		= NULL;
    mySax->getEntity			= NULL;
    mySax->entityDecl			= NULL;
    mySax->notationDecl			= NULL;
    mySax->attributeDecl		= NULL;
    mySax->elementDecl			= NULL;
    mySax->unparsedEntityDecl		= NULL;
    mySax->setDocumentLocator		= NULL;
    mySax->startDocument		= xml_startDocument;
    mySax->endDocument			= xml_endDocument;
    mySax->startElement			= xml_startElement;
    mySax->endElement			= xml_endElement;
    mySax->reference			= NULL;
    mySax->characters			= xml_characters;
    mySax->ignorableWhitespace		= NULL;
    mySax->processingInstruction	= NULL;
    mySax->comment			= NULL;
    mySax->warning			= xml_warning;
    mySax->error			= xml_error;
    mySax->fatalError			= xml_fatalError;
    mySax->getParameterEntity		= NULL;
    mySax->cdataBlock			= NULL;
    mySax->externalSubset		= NULL;
    mySax->initialized			= 0;
}


/*=====================================================================*/


void xml_readTable( const char 	*xmlTableFile,
		    const char  *xmlTableDir,
		    const char  *parentElPath,
		    int		nfields,
		    const char	*delim,
		    char	***output,
       		    int		*iret,
		    char	*fields,... )
/************************************************************************
 * xml_readTable                                                        *
 *                                                                      *
 * This function reads any table in xml format, returning the specifed  *
 * fields in the output array.  The xml document must be well-formed.   *
 * Memory allocated for the output array must be freed by the calling   *
 * routine.								*
 *									*
 * See ~nawdev/doc/Prog_guide/xml/cgemlib_xml.sxw for an example of how *
 * to use this routine.							*
 * 									*
 * Input parameters:                                                    *
 *	*xmlTableFile	const char	xml table file name             *
 *	*xmlTableDir    const char	directory under $GEMTBL in which*
 *					  the table is located		*
 *	*parentElPath	const char      full parent element spec from   *
 *					  the root element down to the  *
 *					  parent of the field(s).	*	
 *	nfields		int		number of fields (elements) to  *
 *					  be returned			*
 *	*delim		const char	delimiter to use between field  *
 *					  values.  If NULL " " is used. *
 *                                                                      *
 * Output parameters:                                                   *
 *	***output	char		2 dim array of field values.    *
 *					  Each specified field will be  *
 *					  packed in to a single string  *
 *					  with delim as the delimiter.  *
 *      *iret           int             Return code                     *
 *					  3 = warning from parser       *
 *					  2 = non-fatal parse error     *
 *					  1 = nfields > number of fields*
 *                                        0 = normal                    *
 *					 -1 = unable to open xmlTable   *
 *					 -2 = no table fields specified *
 *					 -3 = fatal parse error         *
 *					 -4 = no parent path specified  *
 * Variable parameter list:						*
 *	*fields, ...	char		NULL terminated variable	*
 *					  variable argument list of     *
 *					  fields (elements) sought.  The*
 *					  number of field strings should*
 *					  match the nfields parameter.  *
 *									*
 * Log:                                                                 *
 * E. Safford/SAIC	03/06   initial coding                          *
 ***********************************************************************/
{
    int			ier, ii, ctr;
    int			size;
    const char		*nextArg;
    char		xmlFile[ FILE_FULLSZ ], testFile[ FILE_FULLSZ ];
    long		flen = 0;

    va_list		argp;
    xmlSAXHandler	mySax;
/*---------------------------------------------------------------------*/

    *iret   = 0;

    /*
     *  Check xmlTableFile existance/location.
     */
    strcpy( testFile, xmlTableFile );
    cfl_tinq( testFile, PGEN, &flen, xmlFile, &ier );
    if( ier != 0 ) {
        *iret = -1;
	return;
    }


    /*
     *  Load the delimiter and the parent element path.
     */
    xml_setDelimiter( delim );
    xml_setParentPath( parentElPath, &ier );
    if( ier != 0 ) {
	*iret = -4;
	return;
    }


    /*
     *  Set up target table fields
     */
    if( nfields <= 0 || fields == NULL ) {
        *iret = -2;
	return;
    }

    _nfields = nfields;
    G_MALLOC( _field, FieldStr, _nfields, "xml_readTable" );


    /*
     *  Read the variable length argument list of fields.
     */
    nextArg = fields;
    va_start( argp, fields );
    ctr = 0;

    while( nextArg != NULL && ctr < _nfields ) { 
	strcpy( _field[ ctr ], nextArg );
        nextArg = va_arg( argp, char *); 
	ctr++;
    }

    va_end( argp );

    if( ctr < nfields ) {
        *iret = 1;
    }


    /*
     *  Allocate space for the _outputArray.
     */
    size = sizeof( char ** );
    G_MALLOC( _outputArray, char*, size, "xml_readtable:_outputArray" );

    G_MALLOC( _outputLen, int, _nfields, "xml_readTable:_outputLen" );

    for( ii=0; ii<_nfields; ii++ ) {
	size = BLOCK_SIZE;
	G_MALLOC( _outputArray[ ii ], char, size, "xml_readtable" );
	strcpy( _outputArray[ ii ], "" );
        _outputLen[ ii ] = BLOCK_SIZE;
    }


    /* 
     * Initialize parser and parse the table. 
     */
    xml_initParser( &mySax, &ier );

    ier = 0;
    xmlSAXParseFile( &mySax, xmlFile, ier ); 

    if( _fatalError ) {
	*iret = -3;
    } else {

	/*
	 *  report any non-fatal parser problems.
	 */
        if( _error ) {
	    *iret = 2;
	} else if( _warning ) {
	    *iret = 3;
	}


	/*
	 *  Point *output to the _outputArray data.
	 */
        *output = _outputArray;
    }
    
    G_FREE( _delim, char );
    G_FREE( _parentElPath, char );
    G_FREE( _field, FieldStr );
    G_FREE( _outputLen, int );

}

