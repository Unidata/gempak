/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-element.c
 *
 * Copyright (C) 1999 Raph Levien <raph@acm.org>
 * Copyright (C) 2000 Mathieu Lacage <mathieu@gnu.org>
 * Copyright (C) 2001 Paolo Casrini <paolo@casarini.org>
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
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include <libxml/hash.h>
#include "gdome-xml-util.h"
#include "gdome-xml-str.h"
#include "gdome-xml-node.h"
#include "gdome-xml-nodel.h"
#include "gdome-evt-event.h"
#include "gdome-evt-mevent.h"
#include "gdome-xml-element.h"
#include "gdome-xml-attribute.h"
#include "gdome-xml-nnodem.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-xml-document.h"


/* vtable for the Element */
const GdomeElementVtab gdome_xml_el_vtab = {
  {
    gdome_xml_n_ref,
    gdome_xml_n_unref,
    gdome_xml_el_query_interface,
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
    gdome_xml_el_attributes,
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
  gdome_xml_el_tagName,
  gdome_xml_el_getAttribute,
  gdome_xml_el_setAttribute,
  gdome_xml_el_removeAttribute,
  gdome_xml_el_getAttributeNode,
  gdome_xml_el_setAttributeNode,
  gdome_xml_el_removeAttributeNode,
  gdome_xml_el_getElementsByTagName,
  gdome_xml_el_getAttributeNS,
  gdome_xml_el_setAttributeNS,
  gdome_xml_el_removeAttributeNS,
  gdome_xml_el_getAttributeNodeNS,
  gdome_xml_el_setAttributeNodeNS,
  gdome_xml_el_getElementsByTagNameNS,
	gdome_xml_el_hasAttribute,
	gdome_xml_el_hasAttributeNs
};

/**
 * gdome_xml_el_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_el_query_interface (GdomeNode *self, const char *interface, GdomeException *exc)
{
	Gdome_xml_Element *priv = (Gdome_xml_Element *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "Node") ||
      !strcmp (interface, "Element") ||
      !strcmp (interface, "EventTarget")) {
    priv->refcnt++;
    return self;
  }
  else
    return NULL;
}

/**
 * gdome_xml_el_tagName:
 * @self:  Element Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the qualified name of the element.
 */
GdomeDOMString *
gdome_xml_el_tagName (GdomeElement *self, GdomeException *exc) {
	Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
  GdomeDOMString *ret = NULL;
 
  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

	if(gdome_xmlGetNs(priv->n) != NULL && gdome_xmlGetNsPrefix(priv->n) != NULL )
		ret = gdome_xml_str_mkref_own (g_strdup_printf("%s:%s",
																									 (gchar *) gdome_xmlGetNsPrefix(priv->n),
																									 (gchar *) gdome_xmlGetName(priv->n)));
	else
		ret = gdome_xml_str_mkref_dup((gchar *)gdome_xmlGetName (priv->n));

	return ret;
}

/**
 * gdome_xml_el_attributes:
 * @self:  Element Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap of the element's attributes.
 */
GdomeNamedNodeMap *
gdome_xml_el_attributes (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

  g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  return gdome_xml_nnm_mkref(gdome_xml_doc_mkref (gdome_xmlGetOwner (priv->n)),
                             (GdomeElement *)self, gdome_xmlGetAttrList(priv->n), gdome_xmlGetNsDeclList(priv->n),
                             priv->accessType, GDOME_ATTRIBUTE_NODE);
}

/**
 * gdome_xml_el_getAttribute:
 * @self:  Element Objects ref
 * @name:  The name of attribute of which the value is wanted
 * @exc:  Exception Object ref
 *
 * Retrieves an attribute value by name.
 * Returns: the Attr value as a string, or the empty string if that attribute
 * does not have a specified or default value.
 */
GdomeDOMString *
gdome_xml_el_getAttribute (GdomeElement *self, GdomeDOMString *name, GdomeException *exc)
{
  Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
  xmlChar *value = NULL;
	xmlNs *ns = NULL;
	gchar **strs;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (name != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

	value = xmlGetProp (priv->n, name->str);

	if (value != NULL)
		return gdome_xml_str_mkref_own (value);
	else {
		strs = g_strsplit((gchar *)name->str, ":", 0);
		if (xmlStrEqual(strs[0], "xmlns"))
			ns = gdome_xmlGetNsDecl(priv->n, strs[1]);
		g_strfreev(strs);
		if (ns != NULL)
			return gdome_xml_str_mkref_dup ((gchar *)ns->href);
		else
			return gdome_xml_str_mkref_dup ("");
	}
}

/**
 * gdome_xml_el_setAttribute:
 * @self:  Element Objects ref
 * @name:  The name of the attribute to create or alter
 * @value: Value to set in string form
 * @exc:  Exception Object ref
 *
 * Adds a new attribute. If an attribute with that name is already present in
 * the element, its value is changed to be that of the value parameter.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
 */
void
gdome_xml_el_setAttribute (GdomeElement *self, GdomeDOMString *name, GdomeDOMString *value, GdomeException *exc)
{
  Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
  GdomeMutationEvent *mev;
	GdomeDOMString *prevValue;
  GdomeNode *attr;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EL (priv));
  g_return_if_fail (name != NULL);
  g_return_if_fail (value != NULL);
  g_return_if_fail (exc != NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

	if (!(strcmp(name->str, "xmlns"))) {
		gdome_xmlLinkNsDecl (priv->n,
												 NULL,
												 (xmlChar *)value->str);
		return;
	}

  prevValue = gdome_xml_str_mkref_own (xmlGetProp (priv->n, name->str));
  attr = gdome_xml_n_mkref ((xmlNode *)xmlSetProp (priv->n, name->str, value->str));

  if (attr != NULL) {
		if (gdome_xml_n_eventEnabledByCode((GdomeNode *)self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
			/* Fire DOMAttrModified */
			mev = gdome_evt_mevnt_mkref ();
			gdome_xml_str_ref (name);
			gdome_xml_str_ref (value);
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, attr,
																							 prevValue, value, name, GDOME_MODIFICATION, exc);
			gdome_xml_n_dispatchEvent ((GdomeNode *)self, (GdomeEvent *)mev, exc);
			gdome_xml_str_unref (value);
			gdome_xml_str_unref (name);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}
		gdome_xml_n_unref (attr, exc);

		if (gdome_xml_n_eventEnabledByCode((GdomeNode *)self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
			/* Fire DOMSubtreeModified */
			mev = gdome_evt_mevnt_mkref ();
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, NULL,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent ((GdomeNode *)self, (GdomeEvent *)mev, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}
	}

	if (prevValue != NULL)
		gdome_xml_str_unref (prevValue);
}


/**
 * gdome_xml_el_removeAttribute:
 * @self:  Element Objects ref
 * @name:  The name of the attribute to remove
 * @exc:  Exception Object ref
 *
 * Removes an attribute by name. If the removed attribute is known to have a
 * default value, an attribute immediately appears containing the default
 * value as well as the corresponding namespace URI, local name, and prefix
 * when applicable.
 * To remove an attribute by local name and namespace URI, use the
 * gdome_el_removeAttributeNS() function.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
 */
void
gdome_xml_el_removeAttribute (GdomeElement *self, GdomeDOMString *name, GdomeException *exc)
{
  Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
  GdomeNamedNodeMap *nnm = NULL;
	GdomeNode *attr = NULL;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EL (priv));
  g_return_if_fail (name != NULL);
  g_return_if_fail (exc != NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

  nnm =  gdome_xml_nnm_mkref((GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)gdome_xmlGetOwner (priv->n)),
                             self, gdome_xmlGetAttrList(priv->n), NULL,
                             priv->accessType, GDOME_ATTRIBUTE_NODE);
	attr = gdome_xml_nnm_removeNamedItem(nnm, name, exc);
  gdome_xml_nnm_unref(nnm, exc);

  if (attr != NULL)
    gdome_xml_n_unref(attr, exc);
}

/**
 * gdome_xml_el_getAttributeNode:
 * @self:  Element Objects ref
 * @name:  The name of the attribute to retreive
 * @exc:  Exception Object ref
 *
 * Retrieves an attribute node by name.
 * To retrieve an attribute node by qualified name and namespace URI, use the
 * gdome_el_getAttributeNodeNS() method.
 * Returns: the Attr node with the specified @name or %NULL if there is no such
 * attribute.
 */
GdomeAttr *
gdome_xml_el_getAttributeNode (GdomeElement *self, GdomeDOMString *name, GdomeException *exc)
{
	Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
	GdomeNamedNodeMap *nnm;
  GdomeAttr *ret;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (name != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  nnm = gdome_xml_nnm_mkref((GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)gdome_xmlGetOwner (priv->n)),
                            self, gdome_xmlGetAttrList(priv->n), gdome_xmlGetNsDeclList(priv->n),
                            priv->accessType, GDOME_ATTRIBUTE_NODE);
	ret = (GdomeAttr *)gdome_xml_nnm_getNamedItem(nnm, name, exc);
	gdome_xml_nnm_unref(nnm, exc);

  return ret;
}

/**
 * gdome_xml_el_setAttributeNode:
 * @self:  Element Object ref
 * @newAttr:  The Attr node to add to the attribute list
 * @exc:  Exception Object ref
 *
 * Adds a new attribute node. If an attribute with that name is already present
 * in the element, it is replaced by the new one.
 * To add a new attribute node with a qualified name and namespace URI, use the
 * gdome_el_setAttributeNodeNS() method.
 *
 * %GDOME_WRONG_DOCUMENT_ERR: Raised if @newAttr was created from a different
 * document than the one that created the element.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
 * %GDOME_INUSE_ATTRIBUTE_ERR: Raised if @newAttr is already an attribute of
 * another Element object. The DOM user must explicitly clone Attr nodes to
 * re-use them in other elements.
 * Returns: if the @newAttr attribute replaces an existing attribute, the
 * replaced Attr node is returned, otherwise %NULL is returned.
 */
GdomeAttr *
gdome_xml_el_setAttributeNode (GdomeElement *self, GdomeAttr *newAttr, GdomeException *exc)
{
  Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
	GdomeNamedNodeMap *nnm;
	GdomeAttr *ret;

  g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (newAttr != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_A (newAttr), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return NULL;
  }

  nnm =  gdome_xml_nnm_mkref((GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)gdome_xmlGetOwner (priv->n)),
                             self, gdome_xmlGetAttrList(priv->n), NULL, priv->accessType, GDOME_ATTRIBUTE_NODE);
	ret = (GdomeAttr *)gdome_xml_nnm_setNamedItem(nnm, (GdomeNode *)newAttr, exc);
  gdome_xml_nnm_unref(nnm, exc);

	return ret;
}

/**
 * gdome_xml_el_removeAttributeNode:
 * @self:  Element Objects ref
 * @oldAttr:  The Attr node to remove from the attribute list
 * @exc:  Exception Object ref
 *
 * Removes the specified attribute node. If the removed Attr has a default
 * value it is immediately replaced. The replacing attribute has the same
 * namespace URI and local name, as well as the original prefix, when
 * applicable.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
 * %GDOME_NOT_FOUND_ERR: Raised if @oldAttr is not an attribute of the element.
 * Returns: the Attr node that was removed.
 */
GdomeAttr *
gdome_xml_el_removeAttributeNode (GdomeElement *self, GdomeAttr *oldAttr, GdomeException *exc)
{
  Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
  GdomeDOMString *localName, *namespaceURI;
  GdomeNamedNodeMap *nnm = NULL;
  GdomeAttr *ret = NULL;
  xmlAttr *attr = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (oldAttr != NULL, NULL);
	g_return_val_if_fail(GDOME_XML_IS_A (oldAttr), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return NULL;
  }

  attr = (xmlAttr *)((Gdome_xml_Attr *)oldAttr)->n;
  nnm = gdome_xml_nnm_mkref((GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)gdome_xmlGetOwner (priv->n)),
                            self, gdome_xmlGetAttrList(priv->n), NULL, priv->accessType, GDOME_ATTRIBUTE_NODE);

  if (attr->ns == NULL) {
    localName = gdome_xml_str_mkref(gdome_xmlGetName((xmlNode *)attr));
		ret = (GdomeAttr *)gdome_xml_nnm_removeNamedItem(nnm, localName, exc);
    gdome_xml_str_unref(localName);
  } else {
    localName = gdome_xml_str_mkref(gdome_xmlGetName((xmlNode *)attr));
    namespaceURI = gdome_xml_str_mkref(gdome_xmlGetNsURI((xmlNode *)attr));
    ret = (GdomeAttr *)gdome_xml_nnm_removeNamedItemNS(nnm, namespaceURI, localName, exc);
    gdome_xml_str_unref(localName);
    gdome_xml_str_unref(namespaceURI);
  }
	gdome_nnm_unref (nnm, exc);

  return ret;
}

/**
 * gdome_xml_el_getElementsByTagName:
 * @self:  Element Objects ref
 * @name:  The name of the tag to match on. The special value * matches all
 *         tags.
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList of all descendant elements with a given tag name, in the
 * order in which they are encountered in a preorder traversal of this Element
 * tree.
 */
GdomeNodeList *
gdome_xml_el_getElementsByTagName (GdomeElement *self, GdomeDOMString *name, GdomeException *exc)
{
  Gdome_xml_Element *priv = (Gdome_xml_Element *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (name != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  return gdome_xml_nl_mkref ((GdomeNode*)self, name, NULL, GDOME_READWRITE_NODE);
}

/**
 * gdome_xml_el_getElementsByTagNameNS:
 * @self:  Element Objects ref
 * @namespaceURI:  The namespace URI of the elements to match on. The special
 *              value * matches all namespaces.
 * @localName:  The local name of the elements to match on. The special value
 *              * matches all local names.
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList of all the descendant elements with a given local name
 * and namespace URI in the order in which they are encountered in a preorder
 * traversal of this Element tree.
 */
GdomeNodeList *
gdome_xml_el_getElementsByTagNameNS (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
  Gdome_xml_Element *priv = (Gdome_xml_Element *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (namespaceURI != NULL, NULL);
  g_return_val_if_fail (localName != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  return gdome_xml_nl_mkref ((GdomeNode*)self, localName, namespaceURI, GDOME_READWRITE_NODE);
}

/**
 * gdome_xml_el_getAttributeNS:
 * @self:  Element Objects ref
 * @namespaceURI:  The namespaceURI of attribute to retrieve
 * @localName:  The localName of the attribute to retrieve
 * @exc:  Exception Object ref
 *
 * Retrieves an attribute value by local name and namespace URI.
 * Returns: the Attr value as a string, or the empty string if that attribute
 * does not have a specified or default value.
 */
GdomeDOMString *
gdome_xml_el_getAttributeNS (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
  Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
  xmlChar *value = NULL;
	xmlNs *ns = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (namespaceURI != NULL, NULL);
  g_return_val_if_fail (localName != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

	value = xmlGetNsProp (priv->n, localName->str, namespaceURI->str);

	if (value != NULL)
		return gdome_xml_str_mkref_own (value);
	else if (xmlStrEqual(namespaceURI->str, GDOME_XMLNS_NAMESPACE)) {
		ns = gdome_xmlGetNsDecl(priv->n, localName->str);
		if (ns != NULL)
			return gdome_xml_str_mkref_dup ((gchar *)ns->href);
		else
			return gdome_xml_str_mkref_dup ("");
	} else {
		return gdome_xml_str_mkref_dup ("");
	}
}

/**
 * gdome_xml_el_setAttributeNS:
 * @self:  Element Objects ref
 * @namespaceURI:  The namespace URI of attribute to create or alter
 * @qualifiedName:  The qualifiedName of the attribute to create or alter
 * @value: Value to set in string form
 * @exc:  Exception Object ref
 *
 * Adds a new attribute. If an attribute with the same @namespaceURI and
 * localName is already present in the element, its value is changed to be
 * that of the value parameter.
 *
 * %GDOME_NAMESPACE_ERR: Raised if the @qualifiedName is malformed, if the
 * @qualifiedName has a prefix and the @namespaceURI is %NULL, if the
 * @qualifiedName has a prefix that is "xml" and the @namespaceURI is different
 * from "http://www.w3.org/XML/1998/namespace", or if the @qualifiedName is
 * "xmlns" and the namespaceURI is different from "http://www.w3.org/2000/xmlns/".
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
 */
void
gdome_xml_el_setAttributeNS (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeDOMString *value, GdomeException *exc)
{
  Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
  gchar **strs;
  gchar *prefix = NULL;
  gchar *localName = NULL;
	xmlNs *ns = NULL;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EL (priv));
  g_return_if_fail (namespaceURI != NULL);
  g_return_if_fail (qualifiedName != NULL);
  g_return_if_fail (value != NULL);
  g_return_if_fail (exc != NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

	/* test malformed qualifiedName */
	strs = g_strsplit((gchar *)qualifiedName->str, ":", 0);
  if (strs[0] && strs[1] && !strs[2]) {              /* there's only one ':' */
    if (namespaceURI == NULL) {
      *exc = GDOME_NAMESPACE_ERR;
    } else {
      prefix = g_strdup(strs[0]);
      localName = g_strdup(strs[1]);
      if ((!strcmp (prefix, "xml") && strcmp(namespaceURI->str, "http://www.w3.org/XML/1998/namespace"))) {
        *exc = GDOME_NAMESPACE_ERR;
      }
    }
  } else if (!strs[1] && namespaceURI != NULL) {     /* there's no ':' */
    prefix = g_strdup("");
    localName = g_strdup(strs[0]);
    if (strcmp (localName, "xmlns"))
      *exc = GDOME_NAMESPACE_ERR;
  } else if (strs[0] && strs[1] && strs[2]) {        /* there are more than one ':' */
    *exc = GDOME_NAMESPACE_ERR;
	}
	g_strfreev(strs);

	if (!*exc) {
		GdomeMutationEvent *mev;
		GdomeDOMString *prevValue = NULL;
		GdomeNode *attr = NULL;

    if (localName != NULL) {
			if (!(strcmp(prefix, "xmlns"))) {
				/* BUG 1001 need to set attr and prevValue so events are fired */
				gdome_xmlLinkNsDecl (priv->n,
														 (xmlChar *)localName,
														 (xmlChar *)value->str);
			} else {
				xmlDoc * doc = gdome_xmlGetOwner(priv->n);
				ns = xmlSearchNsByHref (doc, priv->n, (xmlChar *)namespaceURI->str);
				if(ns == NULL || !xmlStrEqual((xmlChar *)prefix, ns->prefix)) {
					ns = gdome_xmlNewNs (doc, namespaceURI->str, prefix);
				}
				prevValue = gdome_xml_str_mkref_own (xmlGetNsProp (priv->n,
																													 (xmlChar *)localName,
																													 (xmlChar *)namespaceURI->str));
				attr = gdome_xml_n_mkref ((xmlNode *)xmlSetNsProp(priv->n, ns,
																													(xmlChar *)localName,
																													(xmlChar *)value->str));
			}
			g_free(prefix);
			g_free(localName);
		}
		else {
		 	prevValue = gdome_xml_str_mkref_own (xmlGetProp (priv->n,
		 																									 (xmlChar *)qualifiedName->str));
		 	attr = gdome_xml_n_mkref ((xmlNode *)xmlSetProp (priv->n,
		 																									 (xmlChar *)qualifiedName->str,
		 																									 (xmlChar *)value->str));
		}

		if (attr != NULL) {
			if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
				/* Fire DOMAttrModified */
				mev = gdome_evt_mevnt_mkref ();
				gdome_xml_str_ref (qualifiedName);
				gdome_xml_str_ref (value);
				gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																								 TRUE, FALSE, attr,
																								 prevValue, value, qualifiedName, GDOME_MODIFICATION, exc);
				gdome_xml_n_dispatchEvent ((GdomeNode *)self, (GdomeEvent *)mev, exc);
				gdome_xml_str_unref (value);
				gdome_xml_str_unref (qualifiedName);
				gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
			}
			gdome_xml_n_unref (attr, exc);

			if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
				/* Fire DOMSubtreeModified */
				mev = gdome_evt_mevnt_mkref ();
				gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																								 TRUE, FALSE, NULL,
																								 NULL, NULL, NULL, 0, exc);
				gdome_xml_n_dispatchEvent ((GdomeNode *)self, (GdomeEvent *)mev, exc);
				gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
			}
		}

		if (prevValue != NULL)
			gdome_xml_str_unref (prevValue);
	}
	else {
		if (localName != NULL) {
			g_free (prefix);
			g_free (localName);
		}

		return;
  }
}

/**
 * gdome_xml_el_removeAttributeNS:
 * @self:  Element Objects ref
 * @namespaceURI:  The namespaceURI of attribute to remove
 * @localName:  The localName of the attribute to remove
 * @exc:  Exception Object ref
 *
 * Removes an attribute by local name and namespace URI. If the removed
 * attribute has a default value it is immediately replaced. The replacing
 * attribute has the same namespace URI and local name, as well as the
 * original prefix.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
 */
void
gdome_xml_el_removeAttributeNS (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
  Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
	GdomeNamedNodeMap *nnm = NULL;
	GdomeNode *attr = NULL;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EL (priv));
  g_return_if_fail (namespaceURI != NULL);
  g_return_if_fail (localName != NULL);
  g_return_if_fail (exc != NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

  nnm =  gdome_xml_nnm_mkref((GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)gdome_xmlGetOwner (priv->n)),
                             self, gdome_xmlGetAttrList(priv->n), NULL, priv->accessType, GDOME_ATTRIBUTE_NODE);
	attr = (GdomeNode *)gdome_xml_nnm_removeNamedItemNS(nnm, namespaceURI, localName, exc);
  gdome_xml_nnm_unref(nnm, exc);

	if (attr != NULL)
    gdome_xml_n_unref(attr, exc);
}

/**
 * gdome_xml_el_getAttributeNodeNS:
 * @self:  Element Objects ref
 * @namespaceURI:  The namespace URI of the attribute to retrieve.
 * @localName:  The local name of the attribute to retrieve.
 * @exc:  Exception Object ref
 *
 * Retrieves an Attr node by local name and namespace URI.
 * Returns: the Attr node with the specified attribute local name and
 * namespace URI or %NULL if there is no such attribute.
 */
GdomeAttr *
gdome_xml_el_getAttributeNodeNS (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
	Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
	GdomeNamedNodeMap *nnm;
  GdomeAttr *ret;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (namespaceURI != NULL, NULL);
  g_return_val_if_fail (localName != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  nnm = gdome_xml_nnm_mkref((GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)gdome_xmlGetOwner (priv->n)),
                            self, gdome_xmlGetAttrList(priv->n), gdome_xmlGetNsDeclList(priv->n),
                            priv->accessType, GDOME_ATTRIBUTE_NODE);
	ret = (GdomeAttr *)gdome_xml_nnm_getNamedItemNS(nnm, namespaceURI, localName, exc);
	gdome_xml_nnm_unref(nnm, exc);

  return ret;
}

/**
 * gdome_xml_el_setAttributeNodeNS:
 * @self:  Element Object ref
 * @newAttr:  The Attr node to add to the attribute list
 * @exc:  Exception Object ref
 *
 * Adds a new attribute. If an attribute with that local name and that
 * namespace URI is already present in the element, it is replaced by the new
 * one.
 *
 * %GDOME_WRONG_DOCUMENT_ERR: Raised if @newAttr was created from a different
 * document than the one that created the element.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
 * %GDOME_INUSE_ATTRIBUTE_ERR: Raised if @newAttr is already an attribute of
 * another Element object. The DOM user must explicitly clone Attr nodes to
 * re-use them in other elements.
 * Returns: if the @newAttr attribute replaces an existing attribute with the
 * same local name and namespace URI, the replaced Attr node is returned,
 * otherwise %NULL is returned.
 */
GdomeAttr *
gdome_xml_el_setAttributeNodeNS (GdomeElement *self, GdomeAttr *newAttr, GdomeException *exc)
{
	Gdome_xml_Element *priv = (Gdome_xml_Element *)self;
	GdomeNamedNodeMap *nnm;
	GdomeAttr *ret;

  g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail(GDOME_XML_IS_EL (priv), NULL);
  g_return_val_if_fail (newAttr != NULL, NULL);
	g_return_val_if_fail(GDOME_XML_IS_A (newAttr), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return NULL;
  }

  nnm =  gdome_xml_nnm_mkref((GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)gdome_xmlGetOwner (priv->n)),
                             self, gdome_xmlGetAttrList(priv->n), NULL, priv->accessType, GDOME_ATTRIBUTE_NODE);
	ret = (GdomeAttr *)gdome_xml_nnm_setNamedItemNS(nnm, (GdomeNode *)newAttr, exc);
	gdome_xml_nnm_unref(nnm, exc);

	return ret;
}

/**
 * gdome_xml_el_hasAttribute:
 * @self:  Element Objects ref
 * @name:  The name of the attribute to look for.
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE when an attribute with a given name is specified on this
 * element or has a default value, %FALSE otherwise.
 */
GdomeBoolean
gdome_xml_el_hasAttribute (GdomeElement *self, GdomeDOMString *name, GdomeException *exc)
{
  Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	xmlChar *val = NULL;
	xmlNs *ns = NULL;
	gchar **strs = NULL;

  g_return_val_if_fail (priv != NULL, 0);
  g_return_val_if_fail (GDOME_XML_IS_EL (priv), 0);
  g_return_val_if_fail (name != NULL, 0);
  g_return_val_if_fail (exc != NULL, 0);

  val = xmlGetProp (priv->n, name->str);

  if (val != NULL) {
    xmlFree(val);
		return 1;
	} else {
		strs = g_strsplit((gchar *)name->str, ":", 0);
		if (xmlStrEqual(strs[0], "xmlns"))
			ns = gdome_xmlGetNsDecl(priv->n, strs[1]);
		g_strfreev(strs);
		if (ns != NULL)
			return 1;
		else
			return 0;
	}
}

/**
 * gdome_xml_el_hasAttributeNS:
 * @self:  Element Objects ref
 * @namespaceURI:  The namespaceURI of the attribute to look for.
 * @localName:  The localName of the attribute to look for.
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE when an attribute with a given local name and namespace URI
 * is specified on this element or has a default value, %FALSE otherwise.
 */
GdomeBoolean         
gdome_xml_el_hasAttributeNs (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
  Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	xmlChar *val;
	xmlNs *ns;

  g_return_val_if_fail (priv != NULL, 0);
  g_return_val_if_fail (GDOME_XML_IS_EL (priv), 0);
  g_return_val_if_fail (namespaceURI != NULL, 0);
  g_return_val_if_fail (localName != NULL, 0);
  g_return_val_if_fail (exc != NULL, 0);

	val = xmlGetNsProp (priv->n, localName->str, namespaceURI->str);

	if (val != NULL) {
    xmlFree(val);
		return 1;
	} else if (xmlStrEqual(namespaceURI->str, GDOME_XMLNS_NAMESPACE)) {
		ns = gdome_xmlGetNsDecl(priv->n, localName->str);
		if (ns != NULL)
			return 1;
		else
			return 0;
	} else {
		return 0;
	}
}

