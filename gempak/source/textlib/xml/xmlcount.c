#include "xmlcmn.h"



int xml_count( 	const char	*xmlBuf,
       		const char	*elementName, 
		int		elementNumber,
		const char	*childName,
       		const char	*childValue, 
       		int		*iret )
/************************************************************************
 * xml_count                                                            *
 *                                                                      *
 * This function returns the number of specified elements that are      *
 * contained in an xml file.  						*
 * 									*
 * The input element is actually part of an XPath expression.  This     *
 * expression by default assumes the searched for element is a child    *
 * of the root node of the xml document (/root/elementName).  The root  *
 * node should not be specified in elementName, however.  This routine  *
 * will prepend "[slash]*[slash]" to the beginning of the element name, *
 * thus always matching the root.					* 
 *                                                                      *
 * See additional documentation and usage examples of this and other    *
 * cgemlib/xml library routines at:					*
 *									*
 *	$NAWIPS/doc/Prog_guid/xml/cgemlibxml.sxw.			*
 *									*
 * Valid Inputs:							*
 *                                                                      *
 * int xml_count( xmlBuf, elementName, elementNumber, childName,        *
 *                childValue, iret ) 					*
 *                                                                      *
 * Input parameters:                                                    *
 *      *xmlBuf		const char	buffer containing xml document  *
 *	*elementName	const char	element name to be counted	* 
 *	elementNumber	int		number of element or 0 for none *
 *	*childName	const char	optional child element name     *
 *	*childValue 	const char	optional element value or NULL  *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0 = normal                    *
 *					  1 = non-fatal xpath error     *
 *					       see Valid Inputs above   *
 *                                       -1 = bad xmlBuf pointer (NULL) *
 *                                       -2 = bufSize <= 0              *
 *					 -3 = fatal error creating      *
 *					       XPath expression		*
 *                                       -4 = unable to allocate memory *
 *					 -5 = xpath context error	*
 *					 -6 = error parsing xmlBuf	*
 *                                       -7 = xpath context error       *
 *                                       -8 = xpath expression error    *
 *                                       -9 = node list is empty        *
 * Return:								*
 *			int		number of instances of the 	*
 *					  specified element in xmlBuf  	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	11/05	initial coding (adapted from tutorial   *
 *				 by Aleksey Sanin, Copyright 2004, used *
 *                               under GNU Free Documentation License)  *
 ***********************************************************************/
{
    int 		count      = 0, ier = 0, bufSize = 0;

    xmlChar		*xpathExpr = NULL;
    xmlDocPtr 		doc        = NULL; 
    xmlXPathObjectPtr 	xpathObj   = NULL; 
    xmlNodeSetPtr	nodes      = NULL;
 /*--------------------------------------------------------------------*/

    *iret   = 0;

    /*
     *  Sanity check on input buffer
     */ 
    if( xmlBuf == NULL ) {
	*iret = -1;
	return( count );
    }

    bufSize = strlen( xmlBuf );
    if( bufSize <= 0 ) {
	*iret = -2;
	return( count );
    }


    /* 
     * Init libxml 
     */
    xmlInitParser();

    /*
     * Construct the XPath expression
     */
    xml_makeXpathExpr( elementName, elementNumber, childName, 0, childValue,
    			&xpathExpr, &ier );
    if( ier < 0 ) {
	*iret = -3;
	G_FREE( xpathExpr, xmlChar );
	return( count );
    }
    else if( ier > 0 ) {
	*iret = 1;
    }


    xml_executeXpathExpr( xmlBuf, bufSize, xpathExpr, &doc, &xpathObj, &ier );

    if( ier < 0 ) {
        *iret = -8;
    } 
    else {
        /* 
         * Process results 
         */
        nodes = xpathObj->nodesetval;
        count = nodes->nodeNr;
    }


    /* 
     * Shutdown libxml and cleanup
     */
    G_FREE( xpathExpr, xmlChar ); 
    xmlXPathFreeObject(xpathObj); 
    xmlFreeDoc(doc);   

    xmlCleanupParser();


    return( count );
}

