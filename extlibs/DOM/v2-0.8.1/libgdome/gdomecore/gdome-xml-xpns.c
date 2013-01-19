/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-xpns.c
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
#include "gdome.h"
#include <libxml/tree.h>
#include "gdome-xml-node.h"
#include "gdome.h"
#include "gdome-xml-xpns.h"
#include "gdome-xml-util.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-treegc.h"

const GdomeXPathNamespaceVtab gdome_xml_xpns_vtab =
{
	{
    gdome_xml_n_ref,
    gdome_xml_n_unref,
    gdome_xml_n_query_interface,
    gdome_xml_n_nodeName,
    gdome_xml_n_nodeValue,
    gdome_xml_n_set_nodeValue,
    gdome_xml_n_nodeType,
    gdome_xml_n_parentNode,
    gdome_xml_n_childNodes,
    gdome_xml_n_firstChild,
    gdome_xml_n_lastChild,
    gdome_xml_n_previousSibling,
    gdome_xml_n_nextSibling,
    gdome_xml_n_attributes,
    gdome_xml_n_ownerDocument,
    gdome_xml_n_insertBefore,
    gdome_xml_n_replaceChild,
    gdome_xml_n_removeChild,
    gdome_xml_n_appendChild,
    gdome_xml_n_hasChildNodes,
    gdome_xml_n_cloneNode,
    gdome_xml_n_normalize,
    gdome_xml_n_isSupported,
    gdome_xml_n_namespaceURI,
    gdome_xml_n_prefix,
    gdome_xml_n_set_prefix,
    gdome_xml_n_localName,
    gdome_xml_n_hasAttributes,
    gdome_xml_n_addEventListener,
    gdome_xml_n_removeEventListener,
    gdome_xml_n_dispatchEvent,
    gdome_xml_n_subTreeDispatchEvent
	},
	gdome_xml_xpns_ownerElement
};

GdomeXPathNamespace *
gdome_xml_xpns_mkref (xmlNs *ns){
	Gdome_xml_XPathNamespace *result = NULL;
	xmlNodePtr n;

	if (ns == NULL)
		return NULL;
	if (ns->type == XML_NAMESPACE_DECL) {
		n = xmlNewNode(ns, "");
		n->type = XML_NAMESPACE_DECL;
		result = g_new (Gdome_xml_XPathNamespace, 1);
    result->refcnt = 1;
		result->vtab = &gdome_xml_xpns_vtab;
		result->super.user_data = NULL;
		result->n = n;
		result->accessType = GDOME_READONLY_NODE;
	} else {
		g_warning ("gdome_xml_xpns_mkref: invalid node type");
    result = NULL;
  }

	return (GdomeXPathNamespace *)result;
}

/**
 * gdome_xml_xpns_unref:
 * @self:  XPathNamespace Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference
 */
void
gdome_xml_xpns_unref (GdomeNode *self, GdomeException *exc) {
	Gdome_xml_XPathNamespace *priv = (Gdome_xml_XPathNamespace *)self;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_NS (priv));
	g_return_if_fail (exc != NULL);

	if (priv->refcnt > 0)
		priv->refcnt--;

	if (priv->refcnt == 0) {
    g_free (self);
  }
}

/**
 * gdome_xml_xpns_ownerElement:
 * @self:  XPathNamespace Object ref
 * @exc:  Exception Object ref
 *
 * NOT IMPLEMENTED
 * Returns: %NULL
 */
GdomeElement *
gdome_xml_xpns_ownerElement (GdomeXPathNamespace *self, GdomeException *exc) {
  /* FIXME libxml's xpath function will have to return node information
     with namespaces ? */
	return NULL;
}
