#include "xmlcmn.h"
#include "libxml/xmlwriter.h"

#define MY_ENCODING "ISO-8859-1"


void xml_getSubDoc( const char	  *xmlBuf,
       		    const char	  *elementName, 
		    int		  elementNumber,
		    const char	  *childName,
       		    unsigned char **xmlOut, 
       		    int		  *iret )
/************************************************************************
 * xml_getSubDoc                                                        *
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
 *									*
 * NOTE:  calling routine must free xmlOut				*
 *                                                                      *
 *									*
 * int xml_getSubDoc( xmlBuf, elementName, elementNumber, childName, 	*
 *				xmlout, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *      *xmlBuf		const char	buffer containing xml document  *
 *	*elementName	const char	element name to be counted	* 
 *	elementNumber	int		number of element		*
 *	*childName 	const char	optional child element or NULL  *
 *                                                                      *
 * Output parameters:                                                   *
 *	**xmlOut 	unsigned char	resulting xml document		*
 *					  caller must free this		*
 *      *iret           int             Return code                     *
 *                                        0 = normal                    *
 *                                       -1 = bad xmlBuf pointer (NULL) *
 *                                       -2 = bufSize <= 0              *
 *                                       -4 = unable to allocate memory *
 *					 -5 = xpath context error	*
 *					 -6 = error parsing xmlBuf	*
 *                                       -7 = xpath context error       *
 *                                       -8 = xpath expression error    *
 *                                       -9 = node list is empty        *
 *					-10 = elementName param is NULL *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	01/06	initial coding                          *
 * E. Safford/SAIC	04/06	correct error codes with xml.err	*
 ***********************************************************************/
{
    int 		bufSize = 0, newSize = 0, ier;

    xmlChar		*xpathExpr = NULL;
    xmlDocPtr           doc        = NULL;
    xmlXPathObjectPtr   xpathObj   = NULL;

    xmlNodeSetPtr	nodes      = NULL;
    xmlNodePtr		newRoot    = NULL;
    xmlChar		*newBuf    = NULL; 
 /*--------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  sanity check on parameters
     */ 
    if( !xmlBuf ) {
	*iret = -1;
    }
  
    bufSize = strlen( xmlBuf );
    if( bufSize <= 0 ) {
	*iret = -2;
    }
    else if( !elementName  ) {
	*iret = -10;
    }
  
    if( *iret != 0 ) {
	return;
    }


    /* 
     * Init libxml 
     */
    xmlInitParser();


    xml_makeXpathExpr( elementName, elementNumber, childName, 
    			0, NULL, &xpathExpr, &ier ); 
    if( ier < 0 ) {
        *iret = -8;
    }

    xml_executeXpathExpr( xmlBuf, bufSize, xpathExpr, &doc, &xpathObj, &ier );
    if( xpathObj == NULL || ier < 0 ) {
	*iret = -8;
        xmlFreeDoc(doc); 
        return;
    }

   
    nodes   = xpathObj->nodesetval;
    newRoot = *(nodes->nodeTab);

    xmlDocSetRootElement( doc, newRoot );
    xmlDocDumpMemory( doc, &newBuf, &newSize );  

    if( newSize > 0 ) {
	*xmlOut = (unsigned char *) malloc( (newSize + 1) * sizeof( unsigned char ) );
  	strcpy( (char *)*xmlOut, (char *)newBuf ); 
    }

    /* 
     * Shutdown libxml 
     */
    G_FREE( xpathExpr, xmlChar );
    G_FREE( newBuf, xmlChar );
    xmlXPathFreeObject( xpathObj );
    xmlFreeDoc(doc); 
    xmlCleanupParser();
   

    return;
}
