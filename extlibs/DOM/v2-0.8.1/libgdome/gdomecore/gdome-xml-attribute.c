/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-attribute.c
 *
 * Copyright (C) 1999 Raph Levien <raph@acm.org>
 * Copyright (C) 2000 Mathieu Lacage <mathieu@gnu.org>
 * Copyright (C) 2001 Paolo Casarini <paolo@casarini.org>
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

#include <string.h>
#include "gdome.h"
#include "gdome-events.h"
#include "gdome-treegc.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include "gdome-xml-util.h"
#include "gdome-xml-str.h"
#include "gdome-xml-node.h"
#include "gdome-evt-event.h"
#include "gdome-evt-mevent.h"
#include "gdome-xml-attribute.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-xml-document.h"

const GdomeAttrVtab gdome_xml_a_vtab = {
	{
		gdome_xml_n_ref,
		gdome_xml_n_unref,
		gdome_xml_a_query_interface,
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
  gdome_xml_a_name,
  gdome_xml_a_ownerElement,
	gdome_xml_a_specified,
	gdome_xml_a_value,
	gdome_xml_a_set_value
};

/**
 * gdome_xml_a_query_interface:
 * @self:  Attr Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_a_query_interface (GdomeNode *self, const char *interface, GdomeException *exc)
{
  Gdome_xml_Attr *priv = (Gdome_xml_Attr *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_A (priv), NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "Node") ||
      !strcmp (interface, "Attr") ||
      !strcmp (interface, "EventTarget")) {
    priv->refcnt++;
    return self;
  }
  else
    return NULL;
}

/**
 * gdome_xml_a_mkref_ns
 * @n:  The libxml namespace of which a Gdome reference is wanted
 *
 * Constructor of Attribute wrapper to namespace declaration
 * Returns: the reference to the Gdome Attribute node.
 */
GdomeNode *
gdome_xml_a_mkref_ns (xmlNode *node, xmlNs *ns) {
	Gdome_xml_Attr *result = NULL;
	xmlAttr *a;

	if (ns == NULL)
		return NULL;

	if (ns->type == XML_NAMESPACE_DECL) {
		a = gdome_xmlNewNsDecl(node, ns->prefix, ns->href);
		result = g_new (Gdome_xml_Attr, 1);
		result->refcnt = 1;
		result->vtab = &gdome_xml_a_vtab;
		result->super.user_data = NULL;
		result->n = a;
		result->accessType = GDOME_READONLY_NODE;
		result->ll = NULL;

		gdome_treegc_addNode ((GdomeNode *)result);

		a->_private = result;

	} else {
		g_warning ("gdome_xml_a_mkref_ns: invalid node type");
		result = NULL;
	}

	return (GdomeNode *)result;
}

/**
 * gdome_xml_a_unref_ns:
 * @self:  Attr Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of an namespace declaration attribute.
 * Free the Node structure if the Node will have zero reference
 */
void
gdome_xml_a_unref_ns (GdomeNode *self, GdomeException *exc) {
	Gdome_xml_Attr *priv = (Gdome_xml_Attr *)self;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_A (priv));
	g_return_if_fail (exc != NULL);

	if (priv->refcnt > 0)
		priv->refcnt--;

	if (priv->refcnt == 0) {
		gdome_treegc_delNode (self);
		g_free (self);
	}
}

/**
 * gdome_xml_a_name:
 * @self:  Attr Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the qualified name of this attribute.
 */
GdomeDOMString *
gdome_xml_a_name (GdomeAttr *self, GdomeException *exc) {
  Gdome_xml_Attr *priv = (Gdome_xml_Attr *)self;
  GdomeDOMString *ret = NULL;
 
  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_A (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

	if(gdome_xmlGetNs((xmlNode *)priv->n) != NULL)
		ret = gdome_xml_str_mkref_own (g_strdup_printf("%s:%s",
																									 (gchar *) gdome_xmlGetNsPrefix((xmlNode *)priv->n),
																									 (gchar *) gdome_xmlGetName((xmlNode *)priv->n)));
	else
		ret = gdome_xml_str_mkref_dup((gchar *)gdome_xmlGetName ((xmlNode *)priv->n));

  return ret;
}

/**
 * gdome_xml_a_ownerElement:
 * @self:  Attr Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the Element node this attribute is attached to or %NULL if this
 * attribute is not in use.
 */
GdomeElement *
gdome_xml_a_ownerElement (GdomeAttr *self, GdomeException *exc) {
  Gdome_xml_Attr *priv = (Gdome_xml_Attr *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_A (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  return (GdomeElement *)gdome_xml_n_mkref (gdome_xmlGetParent ((xmlNode *)priv->n));
}

/**
 * gdome_xml_a_specified:
 * @self:  Attr Object ref
 * @exc:  Exception Object ref
 *
 * NOT IMPLEMENTED
 * Returns: %TRUE
 */
GdomeBoolean
gdome_xml_a_specified (GdomeAttr *self, GdomeException *exc)
{
  /* FIXME: probably flesh out when dealing with validation. */

  return TRUE;
}

/**
 * gdome_xml_a_value:
 * @self:  Attr Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this attribute.
 */
GdomeDOMString *
gdome_xml_a_value (GdomeAttr *self, GdomeException *exc) {
  Gdome_xml_Attr *priv = (Gdome_xml_Attr *)self;
  xmlChar *value = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_A (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  value = xmlNodeGetContent ((xmlNode *)priv->n);

  if (value != NULL)
    return gdome_xml_str_mkref_own (value);
  else
    return gdome_xml_str_mkref_dup("");
}

/**
 * gdome_xml_a_set_value:
 * @self:  Attr Object ref
 * @value:  The new value to set
 * @exc:  Exception Object ref
 *
 * Sets the value of this attribute.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_xml_a_set_value (GdomeAttr *self, GdomeDOMString *value, GdomeException *exc) {
  Gdome_xml_Attr *priv = (Gdome_xml_Attr *)self;
  GdomeMutationEvent *mev;
	GdomeDOMString *prevValue, *attrName;
  GdomeElement *parent;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_A (priv));
  g_return_if_fail (value != NULL);
  g_return_if_fail (exc != NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

  prevValue = gdome_xml_a_value (self, exc);
  gdome_xmlSetAttrValue (priv->n, (xmlChar *)value->str);

  /* Fire of events if the Attr is owned by an someone */
  parent = gdome_xml_a_ownerElement (self, exc);
  if (parent != NULL) {
		if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
			/* Fire DOMAttrModified */
			mev = gdome_evt_mevnt_mkref ();
			attrName = gdome_xml_a_name (self, exc);
			gdome_xml_str_ref (value);
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, (GdomeNode *)self,
																							 prevValue, value, attrName, GDOME_MODIFICATION, exc);
			gdome_xml_n_dispatchEvent ((GdomeNode *)parent, (GdomeEvent *)mev, exc);
			gdome_xml_str_unref (value);
			gdome_xml_str_unref (attrName);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}

		if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
			/* Fire DOMSubtreeModified */
			mev = gdome_evt_mevnt_mkref ();
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, NULL,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent ((GdomeNode *)parent, (GdomeEvent *)mev, exc);
			gdome_xml_n_unref ((GdomeNode *)parent, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}
  }

  if (prevValue != NULL)
    gdome_xml_str_unref (prevValue);
}
