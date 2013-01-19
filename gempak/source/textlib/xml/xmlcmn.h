/************************************************************************
 * xmlcmn.h								*
 *									*
 * This file contains definitions used by the CGEMLIB xml functions.	*
 *									*
 **									*
 * S. Jacobs/NCEP	11/98						*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"


#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>


void   xml_makeXpathExpr( const char		*elementName,
		          int			elementNumber,
		          const char		*childName,
		          int			childNumber,
		          const char		*childValue,
		          xmlChar		**expr,
		          int			*iret );

void xml_executeXpathExpr(const char		*xmlBuf, 
		     	  int			bufSize,
		     	  const xmlChar		*xpathExpr,
			  xmlDocPtr		*doc,
			  xmlXPathObjectPtr	*xpathObj,
			  int			*iret );
