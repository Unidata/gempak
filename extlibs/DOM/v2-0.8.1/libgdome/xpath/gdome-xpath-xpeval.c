/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xpath-xpeval.c
 *
 * CopyRight (C) 2002 T.J. Mather <tjmather@tjmather.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include "gdome.h"
#include "gdome-xpath.h"
#include "gdome-xpath-xpeval.h"
#include "gdome-xpath-xpnsresolv.h"
#include "gdome-xpath-xpresult.h"
#include "gdome-xml-node.h"

Gdome_xpath_XPathEvaluator *gdome_xpath_XPathEvaluator = NULL;

const GdomeXPathEvaluatorVtab gdome_xpath_xpeval_vtab =
{
	gdome_xpath_xpeval_ref,
	gdome_xpath_xpeval_unref,
	gdome_xpath_xpeval_createNSResolver,
	gdome_xpath_xpeval_createResult,
  gdome_xpath_xpeval_evaluate
};

/**
 * gdome_xpath_xpeval_mkref:
 *
 * Constructor of the XPathEvaluator interface.
 * Returns: the reference to the XPathEvaluator structure.
 */
GdomeXPathEvaluator *
gdome_xpath_xpeval_mkref (void)
{
  if (gdome_xpath_XPathEvaluator == NULL) {
    gdome_xpath_XPathEvaluator = g_new (Gdome_xpath_XPathEvaluator, 1);
    gdome_xpath_XPathEvaluator->refcnt = 1;
    gdome_xpath_XPathEvaluator->vtab = &gdome_xpath_xpeval_vtab;
  } else
    gdome_xpath_XPathEvaluator->refcnt++;

  return (GdomeXPathEvaluator *)gdome_xpath_XPathEvaluator;
}

/**
 * gdome_xpath_xpeval_ref:
 * @self:  XPathEvaluator Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the XPathEvaluator structure.
 */
void
gdome_xpath_xpeval_ref (GdomeXPathEvaluator *self, GdomeException *exc)
{
	Gdome_xpath_XPathEvaluator *priv = (Gdome_xpath_XPathEvaluator *)self;

	g_return_if_fail (self != NULL);
	g_return_if_fail (exc != NULL);

	g_assert(self == (GdomeXPathEvaluator *)gdome_xpath_XPathEvaluator);

	priv->refcnt++;
}

/**
 * gdome_xpath_xpeval_unref:
 * @self:  XPathEvaluator Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the XPathEvaluator. Free the structure
 * if XPathEvaluator will have zero reference.
 */
void
gdome_xpath_xpeval_unref (GdomeXPathEvaluator *self, GdomeException *exc)
{
	Gdome_xpath_XPathEvaluator *priv = (Gdome_xpath_XPathEvaluator *)self;

	g_return_if_fail (self != NULL);
	g_return_if_fail (exc != NULL);

	g_assert(self == (GdomeXPathEvaluator *)gdome_xpath_XPathEvaluator);

	g_assert(priv->refcnt > 0);
	priv->refcnt--;

	if (priv->refcnt == 0) {
		g_free (self);
		gdome_xpath_XPathEvaluator = NULL;
	}
}

/**
 * gdome_xpath_xpeval_createNSResolver:
 * @self: GdomeXPathEvaluator Object ref
 * @nodeResolver: The node to be used as a context for namespace resolution.
 * @exc:  Exception Object ref
 *
 * Adapts any DOM node to resolve namespaces so that an XPath expression
 * can be easily evaluated relative to the context of the node where it
 * appeared within the document.
 * Returns: GdomeXPathNSResolver object.
 */
GdomeXPathNSResolver *
gdome_xpath_xpeval_createNSResolver (GdomeXPathEvaluator *self, GdomeNode *nodeResolver, GdomeException *exc)
{
	return (GdomeXPathNSResolver *)gdome_xpath_xpnsresolv_mkref(nodeResolver);
}

/**
 * gdome_xpath_xpeval_createResult:
 * @self:  GdomeXPathEvaluator Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an GdomeXPathResult object which may be passed as a parameter to the
 *          evaluation methods of this GdomeXPathEvaluator so that a new one is not
 *          created on each call to an evaluation method.
 */
GdomeXPathResult *
gdome_xpath_xpeval_createResult (GdomeXPathEvaluator *self, GdomeException *exc)
{
	return gdome_xpath_xpresult_mkref(NULL, NULL);
}

/**
 * gdome_xpath_xpeval_evaluate:
 * @self:  GdomeXPathEvaluator Object ref
 * @expression:  The XPath expression string to be parsed and evaluated.
 * @contextNode:  Context node for the evaluation of this XPath expression.
 * @resolver:  The resolver permits translation of prefixes within the XPath
 *            expression into appropriate namespace URIs.
 * @type:  If specified, result will be coerced to return the specified type.
 * @result:  Specifies a GdomeXPathResult object to be re-used.
 * @exc:  Exception Object ref
 *
 * Evaluates an XPath expression string
 * Returns: a result of the specified type if possible. 
 */
GdomeXPathResult *
gdome_xpath_xpeval_evaluate (GdomeXPathEvaluator *self, GdomeDOMString *expression, GdomeNode *contextNode, GdomeXPathNSResolver *resolver, unsigned int type, GdomeXPathResult *result, GdomeException *exc)
{
  xmlXPathObjectPtr res = NULL;
  xmlXPathContextPtr ctxt;
  xmlNodePtr refNode;
  xmlChar *path;
  Gdome_xml_Node *cn = (Gdome_xml_Node *)contextNode;
	Gdome_xpath_XPathNSResolver *resolv = (Gdome_xpath_XPathNSResolver *)resolver;

  g_return_val_if_fail (expression != NULL, NULL);
  refNode = cn->n;

  ctxt = xmlXPathNewContext( refNode->doc );
  ctxt->node = refNode;

	if (resolv != NULL) {
		ctxt->namespaces = xmlGetNsList(resolv->n->doc, resolv->n);
		ctxt->nsNr = 0;
		if (ctxt->namespaces != NULL) {
			while (ctxt->namespaces[ctxt->nsNr] != NULL)
				ctxt->nsNr++;
		}
	}

  path = expression->str;
	res = xmlXPathEval( path, ctxt );

	/* XML::LibXML should also have this patch... */
	if (ctxt->namespaces != NULL)
		xmlFree(ctxt->namespaces);

	xmlXPathFreeContext(ctxt);

  return gdome_xpath_xpresult_mkref(contextNode, res);
}
