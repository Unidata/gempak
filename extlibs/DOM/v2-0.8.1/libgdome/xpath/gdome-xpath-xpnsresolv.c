/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xpath-xpnsresolv.c
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
#include "gdome-xml-node.h"
#include "gdome-xpath.h"
#include "gdome-xpath-xpnsresolv.h"

const GdomeXPathNSResolverVtab gdome_xpath_xpnsresolv_vtab =
  {
		gdome_xpath_xpnsresolv_ref,
		gdome_xpath_xpnsresolv_unref,
    gdome_xpath_xpnsresolv_lookupNamespaceURI
  };

GdomeXPathNSResolver *
gdome_xpath_xpnsresolv_mkref( GdomeNode *nodeResolver )
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)nodeResolver;
  Gdome_xpath_XPathNSResolver *resolv = g_new (Gdome_xpath_XPathNSResolver, 1);
	GdomeException exc;

  resolv->vtab = &gdome_xpath_xpnsresolv_vtab;
  resolv->n = priv->n;
	resolv->refcnt = 1;
	resolv->gnode = nodeResolver;
	gdome_xml_n_ref ( nodeResolver, &exc);
  return (GdomeXPathNSResolver *)resolv;
}

/**
 * gdome_xpath_xpnsresolv_ref:
 * @self:  XPathNSResolver Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the XPathNSResolver structure.
 */
void
gdome_xpath_xpnsresolv_ref (GdomeXPathNSResolver *self, GdomeException *exc)
{
	Gdome_xpath_XPathNSResolver *priv = (Gdome_xpath_XPathNSResolver *)self;

	g_return_if_fail (self != NULL);
	g_return_if_fail (exc != NULL);

	priv->refcnt++;
}

/**
 * gdome_xpath_xpnsresolv_unref:
 * @self:  XPathNSResolver Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the XPathNSResolver. Free the structure
 * if XPathNSResolver will have zero reference.
 */
void
gdome_xpath_xpnsresolv_unref (GdomeXPathNSResolver *self, GdomeException *exc)
{
	Gdome_xpath_XPathNSResolver *priv = (Gdome_xpath_XPathNSResolver *)self;

	g_return_if_fail (self != NULL);
	g_return_if_fail (exc != NULL);

	g_assert(priv->refcnt > 0);
	priv->refcnt--;

	if (priv->refcnt == 0) {
		gdome_xml_n_unref (priv->gnode, exc);
		g_free (self);
	}
}

/**
 * gdome_xpath_xpnsresolv_lookupNamespaceURI:
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
gdome_xpath_xpnsresolv_lookupNamespaceURI( GdomeXPathNSResolver *self, GdomeDOMString *prefix, GdomeException *exc)
{
  xmlNsPtr ns = NULL;
  Gdome_xpath_XPathNSResolver *priv = (Gdome_xpath_XPathNSResolver *)self;

  ns = xmlSearchNs(priv->n->doc, priv->n, prefix->str);
  if (ns != NULL) {
    return gdome_str_mkref_own ((gchar *)ns->href);
  } else {
    return NULL;
  }
}
