#include "xmlcmn.h"

#define OVERAGE  ( 50 )



void xml_makeXpathExpr(	const char	*elementName,
			int		elementNumber,
			const char	*childName,
			int		childNumber,
			const char	*childValue,
			xmlChar		**expr,
       			int		*iret )
/************************************************************************
 * xml_makeXpathExpr                                                    *
 *                                                                      *
 * This function constructs an XPath expression from the input          *
 * parameters.                						*
 *									*
 * Note that this routine will begin constructing the XPath expression  *
 * by using a wildcard to always select the root element of the xml     *
 * document.  This is "[slash]*[slash]"						*
 * 									*
 * The elementName parameter may be NULL.  If elementName is NULL then  *
 * elementNumber and matchValue will not be used, even if they have     *
 * valid values.							*
 *									*
 * If elementName is not NULL, then elementNumber may have a valid      *
 * value.  Valid values for elementNumber are any positive integer; a   *
 * value of 0 means NULL or no value. 					* 
 * 									*
 * The childName parameter may be NULL.  If childName is not NULL then  *
 * either childNumber or childValue may have a valid value.  If both    *
 * have valid values then childNumber only will be used -- both cannot  *
 * be used in the same XPath expression.  Valid values for childNumber  *
 * are any positive integer; a value of 0 means NULL or no value.  Valid*
 * values for childValue is any non-NULL string.  If both childNumber   *
 * and childValue have values then childNumber will be used and a       *
 * warning iret value is set.						*
 *									*
 * Finally, the childValue and elementNumber cannot both have values.   *
 * This would result in an invalid XPath expression.  If both childValue*
 * and elementNumber have values then childValue is ignored and a       *
 * warning iret value is set.						*
 *									* 
 * The calling routine is responsible for freeing memory allocated for  *
 * the expr parameter.							*
 *                                                                      *
 * int xml_makeXpathExpr( elementName, elementNumber, childName, 	*
 *			  childNumber, childValue, expr, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*elementName	const char	element name(s) or NULL    	* 
 *	elementNumber	int       	optional element number or 0    *
 *	*childName 	const char	child element name or NULL      * 
 *	childNumber 	int       	optional child element num or 0 *
 *	*childValue	const char	optional child match value or   *
 *					  NULL				*
 *                                                                      *
 * Output parameters:                                                   *
 *      **expr,		xmlChar		resulting XPath expression      *
 *					  NOTE: caller must free this   *
 *      *iret           int             Return code                     *
 *                                        0 = normal                    *
 *                                       -1 = unable to allocate memory *
 *					  2 = both childNumber and      *
 *					       childValue have values,  *
 *                                             using only childNumber   *
 *					  3 = both childValue and       *
 *					       elementNumber have values*
 *                                             using only elementNumber *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	12/05	initial coding (adapted from tutorial   *
 *				 by Aleksey Sanin, Copyright 2004, used *
 *                               under GNU Free Documentation License)  *
 * E. Safford/SAIC	04/06	correct error codes with xml.err	*
 ***********************************************************************/
{
    char	elNum[ 128 ] = "", childNum[ 128 ] = "";
    int		size = 0;
 /*--------------------------------------------------------------------*/

    if( elementName ) 
        size += strlen( elementName );
    if( childName )
        size += strlen( childName );
    if( childValue )
        size += strlen( childValue );
    size += OVERAGE;


    *iret = 0;

    *expr = (xmlChar *)malloc( size * sizeof( xmlChar ) );
    if( !*expr ) {
        *iret = -1;
	return;
    }

    /*
     *  Start the XPath search at the root element of the document.
     *  The "(slash)*(slash)" finds the root element (there is only 1 
     *  in any xml doc).
     */
    strcpy( (char *)*expr, "/*/" );

    /*
     *  The elementName is then appended.  Include either the elementNumber
     *  or matchValue if valid.  If both are valid use only the elementNumber.
     *  If elementName is NULL then skip to childName.
     */
    if( elementName ) {
        strcat( (char *)*expr, elementName );
	if( elementNumber > 0 ) {
	    sprintf( elNum, "[%d]", elementNumber );
	    strcat( (char *)*expr, elNum );
	}
    }

    if( childName ) {

	if( childNumber > 0 ) {
	    sprintf( childNum, "[%d]", childNumber );
	    strcat( (char *)*expr, "/" );
	    strcat( (char *)*expr, childName );
	    strcat( (char *)*expr, childNum );
	}
	else if( childValue && elementNumber <= 0 ) {
	    strcat( (char *)*expr, "[" );
	    strcat( (char *)*expr, childName );
	    strcat( (char *)*expr, "='" );
	    strcat( (char *)*expr, childValue );
	    strcat( (char *)*expr, "']" );
	}
	else {
	    strcat( (char *)*expr, "/" );
	    strcat( (char *)*expr, childName );
        }
    }


    if( childValue && elementNumber > 0 ) {
        *iret = 3;
    }
    else if( childNumber > 0 && childValue ) {
	*iret = 2;
    }

}

/*========================================================================*/


void xml_executeXpathExpr(const char 		*xmlBuf, 
	                  int			bufSize,  
		     	  const xmlChar 	*xpathExpr,
			  xmlDocPtr		*doc,		/* output*/
			  xmlXPathObjectPtr	*xpathObj,  	/* output*/
			  int			*iret )

/************************************************************************
 * xml_executeXpathExpr                                                 *
 *                                                                      *
 * This function executes an XPath expression and returns the resulting *
 * xpathObj and document.     						*
 *									*
 * The document must be returned to the calling routine because the     *
 * xpathObj points to a node within that document.  The calling         *
 * routine must free both the doc and xpathObj pointers.		*
 *									*
 * void xml_executeXpathExpr( xmlBuf, bufSize, xpathExpr,               *
 *				doc, xpathObj, iret )	 		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*xmlBuf		const char	xml document                    * 
 *	elementNumber	int       	length of xmlBuf                *
 *	*xpathExpr 	const char	XPath expression                * 
 *                                                                      *
 * Output parameters:                                                   *
 *	*doc		xmlDocPtr	pointer to parsed document	*
 *      *xpathObj,	xmlXpathObjectPtr object returned by XPath      *
 *      *iret           int             Return code                     *
 *                                        0 = normal                    *
 *                                      -13 = unable to parse xmlBuf    *
 *					-14 = unable to create context  *
 *					-15 = error evaluating xpathExpr*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	01/05	initial coding				* 
 * E. Safford/SAIC	04/06	correct error codes with xml.err	*
 ***********************************************************************/
{

    xmlXPathContextPtr	xpathCtx; 

/*---------------------------------------------------------------------*/

    *iret     = 0;
    *xpathObj = NULL;
    *doc      = NULL;

    /* 
     * Load XML document 
     */
    *doc = xmlParseMemory( xmlBuf, bufSize );
    if (*doc == NULL) {
        *iret = -13;
	return;
    }


    /* 
     * Create xpath evaluation context 
     */
    xpathCtx = xmlXPathNewContext(*doc);
    if( xpathCtx == NULL ) {
      	*iret = -14;
	return;
    }

    /* 
     * Evaluate xpath expression 
     */
    *xpathObj = xmlXPathEvalExpression(xpathExpr, xpathCtx);

    if(*xpathObj == NULL) {
      	*iret = -15;
    }

    /*
     *  Free the context
     */
    xmlXPathFreeContext( xpathCtx );

}

