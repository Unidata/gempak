#include "geminc.h"
#include "gemprm.h"

#include <libxml/xmlmemory.h> 
#include <libxml/debugXML.h> 
#include <libxml/HTMLtree.h>
#include <libxml/xmlIO.h>
#include <libxml/xinclude.h>
#include <libxml/catalog.h>
#include <libxslt/xslt.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>


#define	XSLT	"xslt"

int xml_transform( const char    *xmlBuf,
		   int	          bufSize,
		   char          *xsltFile,
		   unsigned char **outDoc,
		   int	         *iret )
/************************************************************************
 * xml_transfom                                                         *
 *                                                                      *
 * This function transforms an xml document (contained in the xmlBuf)   *
 * using an xslt document and applying the xslt transformation.         *
 *                                                                      *
 *                                                                      *
 * void xml_transform( xmlBuf, bufSize, xsltFile, outDoc, iret )        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *xmlBuf		const char	buffer containing the xml doc   *
 *	bufSize		int		size of the *xmlBuf		* 
 *      *xsltFile   	char		path & name of xslt document    *
 *                                                                      *
 * Output parameters:                                                   *
 *	**outDoc	unsigned char	resulting output string		*
 *      *iret           int             Return code                     *
 *                                        0 = normal                    *
 *					 -1 = bad xmlBuf pointer	*
 *                                       -2 = bufSize <= 0              *
 *                                      -11 = error opening xslt file   *
 *                                      -12 = error in xslt file        *
 * Return:								*
 *			int		size of the outDoc string	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	10/05	initial coding (adapted from tutorial   *
 *				 by John Fleck, Copyright 2001, used    *
 *                               under GNU Free Documentation License)  *
 * E. Safford/SAIC	12/05	correct error code for bad file open(-3)*
 * E. Safford/SAIC	01/06	avoid crash if bad xslt doc             *
 * E. Safford/SAIC	03/06	use cfl_tinq for file access       	*
 * E. Safford/SAIC	04/06	correct error codes with xml.err	*
 ***********************************************************************/

{
   xmlDocPtr    doc, res = NULL;
   char		xsltDoc[ FILE_FULLSZ ];
   int          ier, outDocLen;
   const char   *params[ 17 ];
   long		flen;
   xsltStylesheetPtr cur = NULL;
 /*--------------------------------------------------------------------*/

   *iret           = 0;
   params[ 0 ]     = NULL;

   if( bufSize <= 0 ) {
      *iret = -2;
      return( 0 );
   }

   if( xmlBuf == NULL ) {
      *iret = -1;
      return( 0 );
   }


   /* 
    *  Make sure the xsltFile can be found.
    */
   cfl_tinq( xsltFile, XSLT, &flen, xsltDoc, &ier );
   if( ier != 0 ) {
      *iret = -11;
      return( 0 );
   }

   /*
    *  Set some initial values for the xml parser
    */
   xmlSubstituteEntitiesDefault( 1 );
   xmlLoadExtDtdDefaultValue = 1;

   /*
    *  Parse the xml and xslt docs, perform the transformation and save the
    *  results to the outDoc string.
    */
   cur = xsltParseStylesheetFile( (const xmlChar *)xsltDoc );
   doc = xmlParseMemory( xmlBuf, bufSize );


   res = xsltApplyStylesheet( cur, doc, params );

   if( res ) {
      xsltSaveResultToString( (xmlChar **)outDoc, &outDocLen, res, cur );
   }
   else {
      *iret = -12;
      return( 0 );
   }

   /*
    * Free up the allocated space and reset the xslt & xml libs
    */
   xsltFreeStylesheet( cur );
   xmlFreeDoc( res );
   xmlFreeDoc( doc );

   xsltCleanupGlobals();
   xmlCleanupParser();


   return( outDocLen );

}

