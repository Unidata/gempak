/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xpath-xpresult.c
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
#include <libxml/xpath.h>
#include "gdome.h"
#include "gdome-xpath.h"
#include "gdome-xpath-xpresult.h"
#include "gdome-xml-node.h"

const GdomeXPathResultVtab gdome_xpath_xpresult_vtab =
	{
		gdome_xpath_xpresult_ref,
		gdome_xpath_xpresult_unref,
		gdome_xpath_xpresult_resultType,
		gdome_xpath_xpresult_booleanValue,
		gdome_xpath_xpresult_numberValue,
		gdome_xpath_xpresult_stringValue,
		gdome_xpath_xpresult_singleNodeValue,
		gdome_xpath_xpresult_iterateNext
	};

GdomeXPathResult *
gdome_xpath_xpresult_mkref (GdomeNode *gnode, xmlXPathObjectPtr res)
{
  Gdome_xpath_XPathResult *result = NULL;
	GdomeException exc;

  result = g_new (Gdome_xpath_XPathResult, 1);
  result->res = res;
  result->refcnt = 1;
	result->gnode = gnode;
	result->pos = 0;
  result->vtab = &gdome_xpath_xpresult_vtab;

	gdome_xml_n_ref (gnode, &exc);

  return (GdomeXPathResult *)result;
}

/**
 * gdome_xpath_xpresult_ref:
 * @self:  XPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the XPathResult structure.
 */
void
gdome_xpath_xpresult_ref (GdomeXPathResult *self, GdomeException *exc)
{
	Gdome_xpath_XPathResult *priv = (Gdome_xpath_XPathResult *)self;

	g_return_if_fail (self != NULL);
	g_return_if_fail (exc != NULL);

	priv->refcnt++;
}

/**
 * gdome_xpath_xpresult_unref:
 * @self:  XPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the XPathResult. Free the structure
 * if XPathResult will have zero reference.
 */
void
gdome_xpath_xpresult_unref (GdomeXPathResult *self, GdomeException *exc)
{
	Gdome_xpath_XPathResult *priv = (Gdome_xpath_XPathResult *)self;

	g_return_if_fail (self != NULL);
	g_return_if_fail (exc != NULL);

	g_assert(priv->refcnt > 0);
	priv->refcnt--;

	if (priv->refcnt == 0) {
		gdome_xml_n_unref (priv->gnode, exc);
		/*		xmlXPathFreeNodeSet(priv->res->nodesetval);
					priv->res->nodesetval = 0;*/
		xmlXPathFreeObject(priv->res);
		g_free (self);
	}
}

/**
 * gdome_xpath_xpresult_resultType:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a code representing the type of this result, as defined by the type
 * constants.
 */
unsigned short
gdome_xpath_xpresult_resultType (GdomeXPathResult *self, GdomeException *exc)
{
	Gdome_xpath_XPathResult *result = (Gdome_xpath_XPathResult *) self;

	if (result->res == NULL) {
		return GDOME_ANY_TYPE;
	} else {
		switch (result->res->type) {
		  case XPATH_NODESET:
	  		return GDOME_ORDERED_NODE_ITERATOR_TYPE;
    	case XPATH_BOOLEAN:
  			return GDOME_BOOLEAN_TYPE;
    	case XPATH_NUMBER:
  			return GDOME_NUMBER_TYPE;
      case XPATH_STRING:
  			return GDOME_STRING_TYPE;
  	  default:
  			return GDOME_ANY_TYPE;
		}
	}
}

/**
 * gdome_xpath_xpresult_booleanValue:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this boolean result.
 */
GdomeBoolean
gdome_xpath_xpresult_booleanValue (GdomeXPathResult *self, GdomeException *exc)
{
	Gdome_xpath_XPathResult *result = (Gdome_xpath_XPathResult *) self;
  if (result->res->type == XPATH_BOOLEAN) {
		return result->res->boolval;
	} else {
		*exc = GDOME_TYPE_ERR;
		return 0;
	}
}

/**
 * gdome_xpath_xpresult_numberValue:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this number result.
 */
double
gdome_xpath_xpresult_numberValue (GdomeXPathResult *self, GdomeException *exc)
{
	Gdome_xpath_XPathResult *result = (Gdome_xpath_XPathResult *) self;
  if (result->res->type == XPATH_NUMBER) {
		return result->res->floatval;
	} else {
		*exc = GDOME_TYPE_ERR;
		return 0;
	}
}

/**
 * gdome_xpath_xpresult_stringValue:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this GdomeDOMString result.
 */
GdomeDOMString *
gdome_xpath_xpresult_stringValue (GdomeXPathResult *self, GdomeException *exc)
{
	Gdome_xpath_XPathResult *result = (Gdome_xpath_XPathResult *) self;
  if (result->res->type == XPATH_STRING) {
		return gdome_str_mkref_dup (result->res->stringval);
	} else {
		*exc = GDOME_TYPE_ERR;
		return NULL;
	}
}

/**
 * gdome_xpath_xpresult_singleNodeValue:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Returns: The value of this single node result, which may be %NULL. This result is not
 *          guaranteed to be the first node in document order where the expression
 *          evaluates to multiple nodes.
 */
GdomeNode *
gdome_xpath_xpresult_singleNodeValue (GdomeXPathResult *self, GdomeException *exc)
{
	xmlNodeSetPtr nodelist = NULL;
	Gdome_xpath_XPathResult *result = (Gdome_xpath_XPathResult *) self;
  if (result->res->type == XPATH_NODESET) {
		nodelist = result->res->nodesetval;
		if ( nodelist && nodelist->nodeNr > 0 ) {
			return (GdomeNode *)gdome_xml_n_mkref (nodelist->nodeTab[0]);
		} else {
			return NULL;
		}
	} else {
		*exc = GDOME_TYPE_ERR;
		return NULL;
	}
}

/**
 * gdome_xpath_xpresult_iterateNext:
 * @self:  GdomeXPathResult Object ref
 * @exc:  Exception Object ref
 *
 * Iterates and returns the next node from the node set or %NULL if there
 * are no more nodes. 
 * Returns: the next node.
 */
GdomeNode *
gdome_xpath_xpresult_iterateNext (GdomeXPathResult *self, GdomeException *exc)
{
	Gdome_xpath_XPathResult *result = (Gdome_xpath_XPathResult *) self;
  xmlNodeSetPtr nsp = result->res->nodesetval;
  int pos = result->pos;
  xmlNodePtr tnode;

	if (nsp == NULL)
		return NULL;

  if (pos >= nsp->nodeNr)
		return NULL;

  tnode = nsp->nodeTab[pos];
  result->pos++;

	return gdome_xml_n_mkref (tnode);
}
