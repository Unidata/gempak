/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xpath.c
 *
 * CopyRight (C) 2002 T.J. Mather <tjmather@tjmather.com>
 *
 * This file is generated automatically.  To make changes, edit
 * test/apigen/xpath.xml
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

#include <libxml/tree.h>
#include <libxml/xpath.h>
#include "gdome.h"
#include "gdome-xml-node.h"
#include "gdome-xpath.h"
#include "gdome-xpath-xpeval.h"
#include "gdome-xpath-xpresult.h"
#include "gdome-xpath-xpnsresolv.h"


/******************************************************************************
          GdomeXPathEvaluator interface API
 ******************************************************************************/
/**
 * gdome_xpeval_mkref:
 *
 * Constructor of the XPathEvaluator interface.
 * Returns: the reference to the XPathEvaluator structure.
 */
GdomeXPathEvaluator *
gdome_xpeval_mkref (void)
{
	return gdome_xpath_xpeval_mkref ();
}
/**
 * gdome_xpeval_ref:
 * @self:  XPathEvaluator Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the XPathEvaluator structure.
 */
void
gdome_xpeval_ref (GdomeXPathEvaluator *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xpath_XPathEvaluator *)self)->vtab->ref (self, exc);
}
/**
 * gdome_xpeval_unref:
 * @self:  XPathEvaluator Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the XPathEvaluator. Free the structure
 * if XPathEvaluator will have zero reference.
 */
void
gdome_xpeval_unref (GdomeXPathEvaluator *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xpath_XPathEvaluator *)self)->vtab->unref (self, exc);
}
/**
 * gdome_xpeval_createNSResolver:
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
gdome_xpeval_createNSResolver (GdomeXPathEvaluator *self, GdomeNode *nodeResolver, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xpath_XPathEvaluator *)self)->vtab->createNSResolver (self, nodeResolver, exc);
}
/**
 * gdome_xpeval_createResult:
 * @self:  GdomeXPathEvaluator Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an GdomeXPathResult object which may be passed as a parameter to the
 *          evaluation methods of this GdomeXPathEvaluator so that a new one is not
 *          created on each call to an evaluation method.
 */
GdomeXPathResult *
gdome_xpeval_createResult (GdomeXPathEvaluator *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xpath_XPathEvaluator *)self)->vtab->createResult (self, exc);
}
/**
 * gdome_xpeval_evaluate:
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
gdome_xpeval_evaluate (GdomeXPathEvaluator *self, GdomeDOMString *expression, GdomeNode *contextNode, GdomeXPathNSResolver *resolver, unsigned int type, GdomeXPathResult *result, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xpath_XPathEvaluator *)self)->vtab->evaluate (self, expression, contextNode, resolver, type, result, exc);
}

/******************************************************************************
          GdomeXPathNSResolver interface API
 ******************************************************************************/
/**
 * gdome_xpnsresolv_ref:
 * @self:  XPathNSResolver Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the XPathNSResolver structure.
 */
void
gdome_xpnsresolv_ref (GdomeXPathNSResolver *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xpath_XPathNSResolver *)self)->vtab->ref (self, exc);
}
/**
 * gdome_xpnsresolv_unref:
 * @self:  XPathNSResolver Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the XPathNSResolver. Free the structure
 * if XPathNSResolver will have zero reference.
 */
void
gdome_xpnsresolv_unref (GdomeXPathNSResolver *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xpath_XPathNSResolver *)self)->vtab->unref (self, exc);
}
/**
 * gdome_xpnsresolv_lookupNamespaceURI:
 * @self:  GdomeXPathNSResolver Object ref
 * @prefix:  The prefix of the namespace to look for
 * @exc:  Exception Object ref
 *
 * Look up the namespace URI associated to the given namespace prefix. The
 * XPath evaluator must never call this with a null or empty argument, because
 * the result of doing this is undefined.
 * Returns: namespace URI
 */
GdomeDOMString *
gdome_xpnsresolv_lookupNamespaceURI (GdomeXPathNSResolver *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xpath_XPathNSResolver *)self)->vtab->lookupNamespaceURI (self, prefix, exc);
}

/******************************************************************************
          GdomeXPathResult interface API
 ******************************************************************************/
/**
 * gdome_xpresult_ref:
 * @self:  XPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the XPathResult structure.
 */
void
gdome_xpresult_ref (GdomeXPathResult *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xpath_XPathResult *)self)->vtab->ref (self, exc);
}
/**
 * gdome_xpresult_unref:
 * @self:  XPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the XPathResult. Free the structure
 * if XPathResult will have zero reference.
 */
void
gdome_xpresult_unref (GdomeXPathResult *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xpath_XPathResult *)self)->vtab->unref (self, exc);
}
/**
 * gdome_xpresult_resultType:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a code representing the type of this result, as defined by the type
 * constants.
 */
unsigned short
gdome_xpresult_resultType (GdomeXPathResult *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xpath_XPathResult *)self)->vtab->resultType (self, exc);
}
/**
 * gdome_xpresult_booleanValue:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this boolean result.
 */
GdomeBoolean
gdome_xpresult_booleanValue (GdomeXPathResult *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xpath_XPathResult *)self)->vtab->booleanValue (self, exc);
}
/**
 * gdome_xpresult_numberValue:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this number result.
 */
double
gdome_xpresult_numberValue (GdomeXPathResult *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xpath_XPathResult *)self)->vtab->numberValue (self, exc);
}
/**
 * gdome_xpresult_stringValue:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this GdomeDOMString result.
 */
GdomeDOMString *
gdome_xpresult_stringValue (GdomeXPathResult *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xpath_XPathResult *)self)->vtab->stringValue (self, exc);
}
/**
 * gdome_xpresult_singleNodeValue:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Returns: The value of this single node result, which may be %NULL. This result is not
 *          guaranteed to be the first node in document order where the expression
 *          evaluates to multiple nodes.
 */
GdomeNode *
gdome_xpresult_singleNodeValue (GdomeXPathResult *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xpath_XPathResult *)self)->vtab->singleNodeValue (self, exc);
}
/**
 * gdome_xpresult_iterateNext:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Iterates and returns the next node from the node set or %NULL if there
 * are no more nodes. 
 * Returns: the next node.
 */
GdomeNode *
gdome_xpresult_iterateNext (GdomeXPathResult *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xpath_XPathResult *)self)->vtab->iterateNext (self, exc);
}
