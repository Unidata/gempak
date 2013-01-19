#include "xmlcmn.h"


void xml_value(	const char	*xmlBuf,
       		const char	*elementName, 
       		int 		elementNumber, 
		const char	*childName,
		int		childNumber,
		char		**value,
       		int		*iret )
/************************************************************************
 * xml_value                                                            *
 *                                                                      *
 * This function returns the value of the specified element that is     *
 * contained in an xml file.   						*
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
 *                                                                      *
 * int xml_value( xmlBuf, elementName, elementNumber, childName,        *
 *			childNumber, value, iret ) 			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *xmlBuf		const char	buffer containing xml document  *
 *	*elementName	const char	element name to be counted	* 
 *	elementNumber 	int       	element number or 0         	*
 *      *childName   	const char	name of child element or NULL   *
 *	childNumber 	int       	child number or 0          	*
 *                                                                      *
 * Output parameters:                                                   *
 *	**value		char		value of element (calling 	*
 *					  routine must free memory)	*
 *      *iret           int             Return code                     *
 *					  1 = non-fatal xpath error     *
 *					       see Valid Inputs above   *
 *                                        0 = normal                    *
 *                                       -1 = bad xmlBuf pointer (NULL) *
 *                                       -2 = bufSize <= 0              *
 *					 -3 = fatal error creating      *
 *					       XPath expression		*
 *					-15 = error executing xpath expr*
 *                                       -9 = node list is empty        *
 *                                      -16 = xpath expr returned more  *
 *						than one node		*
 *					 -9 = selected node is empty	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	11/05	initial coding (adapted from tutorial   *
 *				 by Aleksey Sanin, Copyright 2004, used *
 *                               under GNU Free Documentation License)  *
 * E. Safford/SAIC	04/06	correct error codes with xml.err	*
 ***********************************************************************/
{
    int 		ier, bufSize = 0, size = 0;
    xmlChar		*xpathExpr = NULL;
    xmlXPathObjectPtr	xpathObj;
    xmlNodeSetPtr	nodes;
    xmlNodePtr 		curNode;
    xmlDocPtr 		doc; 

 /*--------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  sanity check on parameters
     */ 
    if( xmlBuf == NULL ) {
	*iret = -1;
	return;
    }
    
    bufSize = strlen( xmlBuf );

    if( bufSize <= 0 ) {
	*iret = -2;
	return;
    }


    /* 
     * Init libxml 
     */
    xmlInitParser();


    /*
     * Make the xpath expression
     */
    xml_makeXpathExpr( elementName, elementNumber, childName, 
    		       childNumber, NULL, &xpathExpr, &ier );

    if( ier < 0 ) {
	*iret = -3;
	G_FREE( xpathExpr, xmlChar );
	return;
    }
    else if( ier > 0 ) {
	*iret = 1;
    }


    xml_executeXpathExpr( xmlBuf, bufSize, xpathExpr, &doc, &xpathObj, &ier );
    if( ier != 0 ) {
        *iret = -15;
    }
    else {
        nodes = xpathObj->nodesetval;
        if( nodes ) {
            size = nodes->nodeNr;
        }


        /*
         *  Size indicates the number of nodes returned from the XPath 
	 *  expression.  This should be 1 and only 1; both 0 and > 1 are 
	 *  errors.  0 means the XPath expression didn't match anything, 
	 *  and > 1 means it matched too many things and we can't identify
	 *  a unique value to return.
         */
        if( size <= 0 ) {
	    *iret = -9;
        }

        if( size > 1 ) {
	    *iret = -16;
        }
    }


    if( *iret >= 0 ) {

        /*
         *  Return the content of the node.  This must be an XML_TEXT_NODE and 
	 *  it must have non-NULL content.  Any other condition is an error.
         */
        curNode = nodes->nodeTab[0];

        if( curNode->type == XML_ELEMENT_NODE ) {
	    curNode= curNode->children;
        }

	if( ( curNode->type == XML_TEXT_NODE ) && curNode->content ) {
	    *value = malloc( (strlen( (char *)(curNode->content) ) + 1) * 
	    			sizeof( char ));
	    strcpy( *value, (char *)curNode->content );
        }
        else {
	    *iret = -9;
        }
    }

    /* 
     * Shutdown libxml and clean up
     */
    xmlCleanupParser();
    G_FREE( xpathExpr, xmlChar );

    xmlXPathFreeObject( xpathObj );
    xmlFreeDoc( doc ); 
    
    return;
}
