/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-node.c
 *
 * Copyright (C) 1999 Raph Levien <raph@acm.org>
 * Copyright (C) 2000 Mathieu Lacage <mathieu@gnu.org>
 * Copyright (C) 2000 Anders Carlsson <andersca@gnu.org>
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <sys/time.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "gdome.h"
#include "gdome-events.h"
#include "gdome-refdebug.h"
#include "gdome-treegc.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include <libxml/hash.h>
#include "gdome-xml-util.h"
#include "gdome-xml-xmlmemory.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-xml-xmldtdutil.h"
#include "gdome-xml-str.h"
#include "gdome-evt-event.h"
#include "gdome-evt-eventl.h"
#include "gdome-evt-mevent.h"
#include "gdome-evt-propagation.h"
#include "gdome-xml-nodel.h"
#include "gdome-xml-node.h"
#include "gdome-xml-document.h"
#include "gdome-xml-element.h"
#include "gdome-xml-attribute.h"
#include "gdome-xml-cdata.h"
#include "gdome-xml-entityref.h"
#include "gdome-xml-text.h"
#include "gdome-xml-cdatas.h"
#include "gdome-xml-comment.h"
#include "gdome-xml-entity.h"
#include "gdome-xml-pi.h"
#include "gdome-xml-notation.h"
#include "gdome-xml-documentt.h"
#include "gdome-xml-documentf.h"
#include "gdome-xml-domimpl.h"
#include "gdome-xpath.h"
#include "gdome-xml-xpns.h"

/* indexed by node type 
	 first one is NULL because there 
   is no node type number 0 
   cf: spec p25 (DOM 1). interface Node.
*/
const void * gdome_xml_vtabs[] = {
	NULL, 
	&gdome_xml_el_vtab,
	&gdome_xml_a_vtab,
	&gdome_xml_t_vtab,
	&gdome_xml_cds_vtab,
	&gdome_xml_er_vtab,
	&gdome_xml_ent_vtab,
	&gdome_xml_pi_vtab,
	&gdome_xml_c_vtab,
	&gdome_xml_doc_vtab, 
	&gdome_xml_dt_vtab,
	&gdome_xml_df_vtab,
	&gdome_xml_not_vtab,
	&gdome_xml_doc_vtab,
  &gdome_xml_dt_vtab,
  NULL,
  NULL,
  &gdome_xml_ent_vtab
};

/***************
 * Virtual Table
 **************/
const GdomeNodeVtab gdome_xml_n_vtab = 
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
	gdome_xml_n_subTreeDispatchEvent,
  gdome_xml_n_canAppend
};


/* function to get the xmlNode from a GdomeNode */
xmlNode *
gdome_xml_n_get_xmlNode (GdomeNode *node)
{
	Gdome_xml_Node *xmlnode;

	xmlnode = (Gdome_xml_Node *)node;

	return xmlnode->n;
}

/***********************************************************
 ***** Constructor and Reference Count Implementation ******
 ***********************************************************/

/**
 * gdome_xml_n_mkref:
 * @n:  The libxml node of which a Gdome reference is wanted
 *
 * Constructor of the Node structure.
 * Returns: the reference to the Gdome node wanted.
 */
GdomeNode *
gdome_xml_n_mkref (xmlNode *n) {
	Gdome_xml_Node *result = NULL;
	xmlElementType type;

	if (n == NULL)
		return NULL;

	type = n->type;
	if (n->_private && type != XML_NAMESPACE_DECL) {
		result = (Gdome_xml_Node *)n->_private;
		result->refcnt++;
	} else {
		switch (type) {
		case XML_ELEMENT_NODE:
		case XML_TEXT_NODE:
		case XML_CDATA_SECTION_NODE:
		case XML_ENTITY_REF_NODE:
		case XML_PI_NODE:
		case XML_COMMENT_NODE:
		case XML_NOTATION_NODE:
		case XML_DOCUMENT_FRAG_NODE:
		case XML_ATTRIBUTE_NODE:
		case XML_ENTITY_DECL: /*libxml use this instead of XML_ENTITY_NODE*/
			result = g_new (Gdome_xml_Node, 1);

			result->refcnt = 1;
      result->vtab = (GdomeNodeVtab *)gdome_xml_vtabs[n->type];
      result->super.user_data = NULL;
      result->n = n;
      if (type == XML_NOTATION_NODE || type == XML_ENTITY_NODE || type == XML_ENTITY_DECL)
        result->accessType = GDOME_READONLY_NODE;
      else
        result->accessType = GDOME_READWRITE_NODE;
      result->ll = NULL;

			n->_private = result;

#ifdef DEBUG_REFCNT
      gdome_refdbg_addRef ((void *)result, GDOME_REFDBG_NODE);
#endif
      gdome_treegc_addNode ((GdomeNode *)result);

      break;
    case XML_DOCUMENT_TYPE_NODE:
    case XML_DTD_NODE:
      return (GdomeNode *)gdome_xml_dt_mkref ((xmlDtd *)n);
		case XML_HTML_DOCUMENT_NODE:
		case XML_DOCUMENT_NODE:
      return (GdomeNode *)gdome_xml_doc_mkref ((xmlDoc *)n);
		case XML_NAMESPACE_DECL:
			return (GdomeNode *)gdome_xml_xpns_mkref ((xmlNs *)n);
		default:
			g_warning ("gdome_xml_n_mkref: invalid node type");
			break;
		}
	}
	return (GdomeNode *)result;
}

/**
 * gdome_xml_n_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_xml_n_ref (GdomeNode *self, GdomeException *exc) {
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_N (priv));
  g_return_if_fail (exc != NULL);

	priv->refcnt++;
}

/**
 * gdome_xml_n_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_xml_n_unref (GdomeNode *self, GdomeException *exc) {
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
  xmlElementType type;

  g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_N (priv));
  g_return_if_fail (exc != NULL);

  type = gdome_xmlGetType (priv->n);
  switch (type) {
  case XML_DOCUMENT_TYPE_NODE:
  case XML_DTD_NODE:
    gdome_xml_dt_unref (self, exc);
    return;
  case XML_DOCUMENT_NODE:
    gdome_xml_doc_unref (self, exc);
    return;
  default:
    if (priv->refcnt > 0)
      priv->refcnt--;

    if (priv->refcnt == 0 && priv->ll == NULL) {
      xmlNode *n = (xmlNode *)priv->n;

			gdome_treegc_delNode (self);

      n->_private = NULL;
			if (type != XML_NOTATION_NODE &&
					type != XML_ENTITY_DECL &&
					n->parent == NULL)
				gdome_xmlFreeSubtree (n);
			else {
				if (type == XML_ATTRIBUTE_NODE)
					if (gdome_xmlIsNsDecl ((xmlAttr *)n))
						gdome_xmlFreeSubtree (n);
			}

#ifdef DEBUG_REFCNT
      gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
      g_free (self);
    }
  }
}

/**
 * gdome_xml_n_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_n_query_interface (GdomeNode *self, const char *interface, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "Node") ||
      !strcmp (interface, "EventTarget")) {
    priv->refcnt++;
    return self;
  }
  else
    return NULL;
}

/**************************************
 ***** Attributes implementation ******
 **************************************/

/**
 * gdome_xml_n_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_xml_n_attributes (GdomeNode *self, GdomeException *exc)
{
	g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (self), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	if (GDOME_XML_IS_EL (self))
		return gdome_el_attributes((GdomeElement *)self, exc);

	return NULL;
}

/**
 * gdome_xml_n_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_xml_n_childNodes (GdomeNode *self, GdomeException *exc)
{
	g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (self), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

  if (GDOME_XML_IS_NS(self))
		return NULL;

  return gdome_xml_nl_mkref ((GdomeNode*)self, NULL, NULL, GDOME_READONLY_NODE);
}

/**
 * gdome_xml_n_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_xml_n_firstChild (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

  switch (gdome_xmlGetType (priv->n)) {
  case XML_DOCUMENT_TYPE_NODE:
  case XML_DTD_NODE:
  case XML_PI_NODE:
  case XML_COMMENT_NODE:
  case XML_TEXT_NODE:
  case XML_CDATA_SECTION_NODE:
  case XML_NOTATION_NODE:
  case XML_NAMESPACE_DECL:
    return NULL;
  case XML_DOCUMENT_NODE:
  case XML_DOCUMENT_FRAG_NODE:
  case XML_ENTITY_REF_NODE:
  case XML_ELEMENT_NODE:
  case XML_ATTRIBUTE_NODE:
  case XML_ENTITY_NODE:
  case XML_ENTITY_DECL:
    return gdome_xml_n_mkref (gdome_xmlGetFirstChild (priv->n));
  default:
    g_warning ("gdome_xml_n_firstChild: invalid node type");
    return NULL;
  }
}

/**
 * gdome_xml_n_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_xml_n_lastChild (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

  switch (gdome_xmlGetType (priv->n)) {
  case XML_DOCUMENT_TYPE_NODE:
  case XML_DTD_NODE:
  case XML_PI_NODE:
  case XML_COMMENT_NODE:
  case XML_TEXT_NODE:
  case XML_CDATA_SECTION_NODE:
  case XML_NOTATION_NODE:
  case XML_NAMESPACE_DECL:
    return NULL;
  case XML_DOCUMENT_NODE:
  case XML_DOCUMENT_FRAG_NODE:
  case XML_ENTITY_REF_NODE:
  case XML_ELEMENT_NODE:
  case XML_ATTRIBUTE_NODE:
  case XML_ENTITY_NODE:
  case XML_ENTITY_DECL:
    return gdome_xml_n_mkref (gdome_xmlGetLastChild (priv->n));
  default:
    g_warning ("gdome_xml_n_lastChild: invalid node type");
    return NULL;
  }
}

/**
 * gdome_xml_n_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_xml_n_localName (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	if((gdome_xmlGetType(priv->n) == XML_ELEMENT_NODE ||
			gdome_xmlGetType(priv->n) == XML_ATTRIBUTE_NODE) &&
		 gdome_xmlGetNs(priv->n) != NULL)
		return gdome_xml_str_mkref_dup ((gchar *)gdome_xmlGetName (priv->n));
	else
		return NULL;
}

/**
 * gdome_xml_n_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_xml_n_namespaceURI (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	if((gdome_xmlGetType(priv->n) == XML_ELEMENT_NODE ||
			gdome_xmlGetType(priv->n) == XML_ATTRIBUTE_NODE ||
			gdome_xmlGetType(priv->n) == XML_NAMESPACE_DECL) &&
		 gdome_xmlGetNs(priv->n) != NULL)
		return gdome_xml_str_mkref_dup ((gchar *)gdome_xmlGetNsURI (priv->n));
	else
		return NULL;
}

/**
 * gdome_xml_n_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_xml_n_nextSibling (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	if(GDOME_XML_IS_A (priv) || GDOME_XML_IS_NS (priv))
		return NULL;
	else
		return gdome_xml_n_mkref (gdome_xmlGetNext (priv->n));
}

/**
 * gdome_xml_n_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_xml_n_nodeName (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	GdomeDOMString *ret = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	switch (gdome_xmlGetType (priv->n)) {
	case XML_ATTRIBUTE_NODE:
		ret = gdome_a_name((GdomeAttr *)self, exc);
		break;
	case XML_ELEMENT_NODE:
		ret = gdome_el_tagName((GdomeElement *)self, exc);
		break;
  case XML_DOCUMENT_TYPE_NODE:
  case XML_DTD_NODE:
    ret = gdome_dt_name((GdomeDocumentType *)self, exc);
    break;
  case XML_PI_NODE:
    ret = gdome_pi_target((GdomeProcessingInstruction *)self, exc);
		break;
  case XML_ENTITY_NODE:
  case XML_ENTITY_DECL: /*libxml use this instead of XML_ENTITY_NODE*/
	case XML_ENTITY_REF_NODE:
	case XML_NOTATION_NODE:
		ret = gdome_xml_str_mkref_dup((gchar *)gdome_xmlGetName (priv->n));
		break;
	case XML_CDATA_SECTION_NODE:
		ret = gdome_xml_str_mkref_dup("#cdata-section");
		break;
	case XML_COMMENT_NODE:
		ret = gdome_xml_str_mkref_dup("#comment");
		break;
	case XML_DOCUMENT_NODE:
		ret = gdome_xml_str_mkref_dup("#document");
		break;
	case XML_DOCUMENT_FRAG_NODE:
		ret = gdome_xml_str_mkref_dup("#document-fragment");
		break;
	case XML_TEXT_NODE:
		ret = gdome_xml_str_mkref_dup("#text");
		break;
  case XML_NAMESPACE_DECL:
    ret = gdome_xml_str_mkref_dup(gdome_xmlGetNsPrefix(priv->n));
		break;
	default:
		g_warning ("gdome_n_name: wrong Node Type.");
	}

	return ret;
}

/**
 * gdome_xml_n_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_xml_n_nodeType (GdomeNode *self, GdomeException *exc)
{
  Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
  xmlElementType type;

	g_return_val_if_fail (priv != NULL, 0);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), 0);
	g_return_val_if_fail (exc != NULL, 0);

  type = gdome_xmlGetType(priv->n);
  if (type == XML_DTD_NODE)
    return XML_DOCUMENT_TYPE_NODE;
  else if (type == XML_ENTITY_DECL)
    return XML_ENTITY_NODE;
	else if (type == XML_NAMESPACE_DECL)
		return GDOME_XPATH_NAMESPACE_NODE;
  else
    return type;
}

/**
 * gdome_xml_n_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_xml_n_nodeValue (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	GdomeDOMString *ret = NULL;

	g_return_val_if_fail (priv != NULL, 0);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (exc != NULL, 0);

	switch (gdome_xmlGetType (priv->n)) {
	case XML_ATTRIBUTE_NODE:
		ret = gdome_a_value ((GdomeAttr *)self, exc);
		break;
  case XML_PI_NODE:
    ret = gdome_pi_data ((GdomeProcessingInstruction *)self, exc);
    break;
	case XML_CDATA_SECTION_NODE:
	case XML_COMMENT_NODE:
	case XML_TEXT_NODE:
		ret = gdome_xml_str_mkref_own (xmlNodeGetContent (priv->n));
		break;
	case XML_ELEMENT_NODE:
	case XML_ENTITY_NODE:
	case XML_ENTITY_REF_NODE:
	case XML_DOCUMENT_NODE:
	case XML_DOCUMENT_TYPE_NODE:
	case XML_DOCUMENT_FRAG_NODE:
  case XML_NOTATION_NODE:
  case XML_DTD_NODE:
  case XML_ENTITY_DECL:
	case XML_NAMESPACE_DECL:
		break;
	default:
		g_warning ("could not recognize the Node Type.");
	}

	return ret;
}

/**
 * gdome_xml_n_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_xml_n_ownerDocument (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

  if (gdome_xmlGetType (priv->n) == XML_DOCUMENT_NODE)
    return NULL;
  else
    return (GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)gdome_xmlGetOwner (priv->n));
}

/**
 * gdome_xml_n_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_xml_n_parentNode (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	if(GDOME_XML_IS_TREE_N (priv))
		return gdome_xml_n_mkref (gdome_xmlGetParent (priv->n));
	else
		return NULL;
}

/**
 * gdome_xml_n_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_xml_n_prefix (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_val_if_fail (priv != NULL, 0);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), 0);
	g_return_val_if_fail (exc != NULL, 0);

	if((gdome_xmlGetType(priv->n) == XML_ELEMENT_NODE ||
			gdome_xmlGetType(priv->n) == XML_ATTRIBUTE_NODE ||
			gdome_xmlGetType(priv->n) == XML_NAMESPACE_DECL) &&
		 gdome_xmlGetNs(priv->n) != NULL)
		return gdome_xml_str_mkref_dup ((gchar *)gdome_xmlGetNsPrefix (priv->n));
	else
		return NULL;
}

/**
 * gdome_xml_n_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_xml_n_previousSibling (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	if(GDOME_XML_IS_A (priv) || GDOME_XML_IS_NS (priv))
		return NULL;
	else
		return gdome_xml_n_mkref (gdome_xmlGetPrev (priv->n));
}

/**
 * gdome_xml_n_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_xml_n_set_nodeValue (GdomeNode *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_N (priv));
	g_return_if_fail (nodeValue != NULL);
  g_return_if_fail (exc != NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

	switch (gdome_xmlGetType(priv->n)) {
	case XML_ATTRIBUTE_NODE:
		gdome_xml_a_set_value ((GdomeAttr *)self, nodeValue, exc);
		break;
  case XML_PI_NODE:
    gdome_xml_pi_set_data ((GdomeProcessingInstruction *)self, nodeValue, exc);
    break;
	case XML_CDATA_SECTION_NODE:
	case XML_COMMENT_NODE:
  case XML_TEXT_NODE:
    gdome_xml_cd_set_data ((GdomeCharacterData *)self, nodeValue, exc);
		break;
	case XML_ELEMENT_NODE:
	case XML_ENTITY_NODE:
	case XML_ENTITY_REF_NODE:
	case XML_DOCUMENT_NODE:
	case XML_DOCUMENT_TYPE_NODE:
	case XML_DOCUMENT_FRAG_NODE:
  case XML_NOTATION_NODE:
  case XML_DTD_NODE:
  case XML_NAMESPACE_DECL:
		break;
	default:
		g_warning ("could not recognize the Node Type.");
  }
}

/**
 * gdome_xml_n_set_prefix:
 * @self:  Node Object ref
 * @prefix:  The new prefix for this node.
 * @exc:  Exception Object ref
 *
 * Sets a new nemaspace prefix for this node.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 * %GDOME_NAMESPACE_ERR: Raised if the specified @prefix is malformed per the
 * Namespaces in XML specification, if the namespaceURI of this node is %NULL,
 * if the specified prefix is "xml" and the namespaceURI of this node is
 * different from "http://www.w3.org/XML/1998/namespace", if this node is an
 * attribute and the specified prefix is "xmlns" and the namespaceURI of this
 * node is different from "http://www.w3.org/2000/xmlns/", or if this node is
 * an attribute and the qualifiedName of this node is "xmlns".
 */
void
gdome_xml_n_set_prefix (GdomeNode *self, GdomeDOMString *prefix, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	xmlNs *ns;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_N (priv));
	g_return_if_fail (prefix != NULL);
	g_return_if_fail (exc != NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

  if((gdome_xmlGetType(priv->n) == XML_ELEMENT_NODE ||
			gdome_xmlGetType(priv->n) == XML_ATTRIBUTE_NODE) &&
		 gdome_xmlGetNs(priv->n) != NULL &&
     !xmlStrEqual(gdome_xmlGetNsPrefix (priv->n), (xmlChar *)prefix->str)) {

		/* tests for NAMESPACE_ERR */
    gchar *nsURI = (gchar *)gdome_xmlGetNsURI (priv->n);
		if (nsURI == NULL ||
				(!strcmp (prefix->str, "xml") &&
				 strcmp (nsURI, "http://www.w3.org/XML/1998/namespace")) ||
				(gdome_xmlGetType(priv->n) == XML_ATTRIBUTE_NODE &&
				 !strcmp (prefix->str, "xmlns") &&
				 strcmp (nsURI, GDOME_XMLNS_NAMESPACE)) ||
				(gdome_xmlGetType(priv->n) == XML_ATTRIBUTE_NODE &&
				 !strcmp (gdome_xmlGetName(priv->n), "xmlns"))) {
			*exc = GDOME_NAMESPACE_ERR;
			return;
		}
		ns = gdome_xmlNewNs (priv->n->doc, gdome_xmlGetNsURI (priv->n), (xmlChar *)prefix->str);
		gdome_xmlSetNs(priv->n, ns);
	}
}

/**************************************
 ******* Methods implementation *******
 **************************************/

/**
 * gdome_xml_n_appendChild:
 * @self:  Node Object ref
 * @newChild:  The node to add
 * @exc:  Exception Object ref
 *
 * Adds the node @newChild to the end of the list of children of this node.
 * If the @newChild is already in the tree, it is first removed. If it is a
 * DocumentFragment node, the entire contents of the document fragment are
 * moved into the child list of this node
 *
 * %GDOME_HIERARCHY_REQUEST_ERR: Raised if this node is of a type that does not
 * allow children of the type of the @newChild node, or if the node to append is
 * one of this node's ancestors or this node itself.
 * %GDOME_WRONG_DOCUMENT_ERR: Raised if @newChild was created from a different
 * document than the one that created this node.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 * Returns: the node added.
 */
GdomeNode *
gdome_xml_n_appendChild (GdomeNode *self, GdomeNode *newChild, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	Gdome_xml_Node *new_priv = (Gdome_xml_Node *)newChild;
	GdomeMutationEvent *mev;
	xmlNode *ret;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (new_priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (new_priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	if (!gdome_xml_n_canAppend (self, newChild, exc)) {
		*exc = GDOME_HIERARCHY_REQUEST_ERR;
		return NULL;
	}

	if (gdome_xmlGetOwner(new_priv->n) != gdome_xmlGetOwner(priv->n)) {
		*exc = GDOME_WRONG_DOCUMENT_ERR;
		return NULL;
	}

	if (GDOME_ISREADONLY (priv)) {
		*exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
		return NULL;
	}

	/* If the newChild is already in the tree, it is first removed. */
	if (gdome_xmlGetParent(new_priv->n) != NULL)
		gdome_xmlUnlinkChild(gdome_xmlGetParent(new_priv->n), new_priv->n);

	ret = gdome_xmlAppendChild(priv->n, new_priv->n);

	/* Delete children list for DocumentFragment Node */
	if (gdome_xmlGetType (new_priv->n) == XML_DOCUMENT_FRAG_NODE) {
		gdome_xmlSetFirstChild(new_priv->n, NULL);
		gdome_xmlSetLastChild(new_priv->n, NULL);
	}

	if (gdome_xml_n_eventEnabledByCode (self, DOM_NODE_INSERTED_EVENT_TYPE)) {
		/* Fire DOMNodeInserted */
		mev = gdome_evt_mevnt_mkref ();
		gdome_xml_n_ref (self, exc);
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_NODE_INSERTED_EVENT_TYPE,
																						 TRUE, FALSE, self,
																						 NULL, NULL, NULL, 0, exc);
		gdome_xml_n_dispatchEvent (newChild, (GdomeEvent *)mev, exc);
		gdome_xml_n_unref (self, exc);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}

	if (gdome_xml_n_eventEnabledByCode (self, DOM_NODE_INSERTED_INTO_DOCUMENT_EVENT_TYPE)) {
		/* Fire DOMNodeInsertedIntoDocument */
		mev = gdome_evt_mevnt_mkref ();
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_NODE_INSERTED_INTO_DOCUMENT_EVENT_TYPE,
																						 FALSE, FALSE, NULL,
																						 NULL, NULL, NULL, 0, exc);
		gdome_xml_n_dispatchEvent (newChild, (GdomeEvent *)mev, exc);
		gdome_xml_n_subTreeDispatchEvent (newChild, (GdomeEvent *)mev, exc);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}
	
	if (gdome_xml_n_eventEnabledByCode (self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMSubtreeModified */
		mev = gdome_evt_mevnt_mkref ();
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																						 TRUE, FALSE, NULL,
																						 NULL, NULL, NULL, 0, exc);
		gdome_xml_n_dispatchEvent (self, (GdomeEvent *)mev, exc);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}
	
	return gdome_xml_n_mkref (ret);
}

/**
 * gdome_xml_n_cloneNode:
 * @self:  Node Object ref
 * @deep:  If true, recursively clone the subtree under the specified node;
 *         if false, clone only the node itself (and its attributes, if it is
 *         an Element).
 * @exc:  Exception Object ref
 *
 * Makes a duplicate of this node, i.e., serves as a generic copy
 * constructor for nodes. The duplicate node has no parent; (parentNode is
 * %NULL).
 * %GDOME_DOCUMENT_TYPE_NODE, %GDOME_NOTATION_NODE and %GDOME_ENTITY_NODE
 * nodes are not supported.
 *
 * %GDOME_NOT_SUPPORTED_ERR: Raised if the type of node being cloned is not
 * supported.
 * Returns: the duplicate node.
 */
GdomeNode *
gdome_xml_n_cloneNode (GdomeNode *self, GdomeBoolean deep, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
  xmlNode *n = NULL;
  xmlElementType type;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

  type = gdome_xmlGetType (priv->n);
  if (type == XML_ENTITY_NODE ||
      type == XML_ENTITY_DECL ||
			type == XML_NOTATION_NODE ||
			type == XML_DOCUMENT_TYPE_NODE ||
			type == XML_DTD_NODE ||
			type == XML_NAMESPACE_DECL )
		*exc = GDOME_NOT_SUPPORTED_ERR;
	else if (type == XML_DOCUMENT_NODE)
		n = (xmlNode *)xmlCopyDoc ((xmlDoc *)priv->n, deep);
	else if (type == XML_ATTRIBUTE_NODE) {
		n = (xmlNode *)xmlCopyProp (NULL, (xmlAttr *)priv->n);
		if (gdome_xmlGetOwner (n) == NULL)
			gdome_xmlSetOwner (n , gdome_xmlGetOwner (priv->n));
	}
	else
		n = xmlDocCopyNode (priv->n, gdome_xmlGetOwner(priv->n), deep);

	if (*exc)
		return NULL;
  else
		return gdome_xml_n_mkref (n);
}

/**
 * gdome_xml_n_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_xml_n_hasAttributes (GdomeNode *self, GdomeException *exc) {
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_val_if_fail (priv != NULL, 0);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), 0);
	g_return_val_if_fail (exc != NULL, 0);

	if (GDOME_XML_IS_EL (priv))
		return (gdome_xmlGetAttrList (priv->n) != NULL);
	else
    return FALSE;
}

/**
 * gdome_xml_n_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_xml_n_hasChildNodes (GdomeNode *self, GdomeException *exc) {
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;

	g_return_val_if_fail (priv != NULL, 0);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), 0);
  g_return_val_if_fail (exc != NULL, 0);

  switch (gdome_xmlGetType (priv->n)) {
  case XML_DOCUMENT_TYPE_NODE:
  case XML_DTD_NODE:
  case XML_PI_NODE:
  case XML_COMMENT_NODE:
  case XML_TEXT_NODE:
  case XML_CDATA_SECTION_NODE:
  case XML_NOTATION_NODE:
	case XML_NAMESPACE_DECL:
    return FALSE;
  case XML_DOCUMENT_NODE:
  case XML_DOCUMENT_FRAG_NODE:
  case XML_ENTITY_REF_NODE:
  case XML_ELEMENT_NODE:
  case XML_ATTRIBUTE_NODE:
  case XML_ENTITY_NODE:
  case XML_ENTITY_DECL:
		return (gdome_xmlGetFirstChild (priv->n) != NULL);
  default:
    g_warning ("gdome_xml_n_hasChildNodes: invalid node type");
    return FALSE;
  }
}

/**
 * gdome_xml_n_insertBefore:
 * @self:  Node Object ref
 * @newChild:  The new node to put in the child list
 * @refChild:  the reference node, i.e., the node before which the new node
 *             must be inserted.
 * @exc:  Exception Object ref
 *
 * Inserts the node @newChild before the existing child node @refChild. If
 * @refChild is %NULL, insert @newChild at the end of the list of children.
 * If @newChild is a DocumentFragment node, all of its children are inserted,
 * in the same order, before @refChild. If the @newChild is already in the
 * tree, it is first removed.
 *
 * %GDOME_HIERARCHY_REQUEST_ERR: Raised if this node is of a type that does not
 * allow children of the type of the @newChild node, or if the node to insert is
 * one of this node's ancestors or this node itself.
 * %GDOME_WRONG_DOCUMENT_ERR: Raised if @newChild was created from a different
 * document than the one that created this node.
 * %GDOME_NOT_FOUND_ERR: Raised if @refChild is not a child of this node.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 * Returns: the node being inserted.
 */
GdomeNode *
gdome_xml_n_insertBefore (GdomeNode *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	Gdome_xml_Node *new_priv = (Gdome_xml_Node *)newChild;
	Gdome_xml_Node *ref_priv = (Gdome_xml_Node *)refChild;
	GdomeMutationEvent *mev;
	xmlNode *ret = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (new_priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (new_priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	if (!gdome_xml_n_canAppend (self, newChild, exc)) {
		*exc = GDOME_HIERARCHY_REQUEST_ERR;
		return NULL;
	}

	if (gdome_xmlGetOwner(new_priv->n) != gdome_xmlGetOwner(priv->n)) {
		*exc = GDOME_WRONG_DOCUMENT_ERR;
		return NULL;
	}

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return NULL;
  }

  /* If refChild is null, insert newChild at the end of the list of children.*/
	if(ref_priv == NULL)
		return gdome_xml_n_appendChild (self, newChild, exc);
	else
		g_return_val_if_fail (GDOME_XML_IS_N (ref_priv), NULL);

	/* If the newChild is already in the tree, it is first removed. */
  if (gdome_xmlGetParent(new_priv->n) != NULL) {
		if (gdome_xml_n_eventEnabledByCode (self, DOM_NODE_REMOVED_EVENT_TYPE)) {
			/* Fire DOMNodeRemoved */
			mev = gdome_evt_mevnt_mkref ();
			gdome_xml_n_ref (self, exc);
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_NODE_REMOVED_EVENT_TYPE,
																							 TRUE, FALSE, self,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent (newChild, (GdomeEvent *)mev, exc);
			gdome_xml_n_unref (self, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}

		if (gdome_xml_n_eventEnabledByCode (self, DOM_NODE_REMOVED_FROM_DOCUMENT_EVENT_TYPE)) {
			/* Fire DOMNodeRemovedFromDocument */
			mev = gdome_evt_mevnt_mkref ();
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_NODE_REMOVED_FROM_DOCUMENT_EVENT_TYPE,
																							 FALSE, FALSE, NULL,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent (newChild, (GdomeEvent *)mev, exc);
			gdome_xml_n_subTreeDispatchEvent (newChild, (GdomeEvent *)mev, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}

    gdome_xmlUnlinkChild(gdome_xmlGetParent(new_priv->n), new_priv->n);
  }

	if ((ret = gdome_xmlInsertBeforeChild(priv->n, new_priv->n, ref_priv->n)) == NULL) {
		*exc = GDOME_NOT_FOUND_ERR;
		return NULL;
  }

  /* Unlink children list for DocumentFragment Node */
  if (gdome_xmlGetType (new_priv->n) == XML_DOCUMENT_FRAG_NODE) {
    gdome_xmlSetFirstChild(new_priv->n, NULL);
    gdome_xmlSetLastChild(new_priv->n, NULL);
  }

	if (gdome_xml_n_eventEnabledByCode (self, DOM_NODE_INSERTED_EVENT_TYPE)) {
		/* Fire DOMNodeInserted */
		mev = gdome_evt_mevnt_mkref ();
		gdome_xml_n_ref (self, exc);
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_NODE_INSERTED_EVENT_TYPE,
																						 TRUE, FALSE, self,
																						 NULL, NULL, NULL, 0, exc);
		gdome_xml_n_dispatchEvent (newChild, (GdomeEvent *)mev, exc);
		gdome_xml_n_unref (self, exc);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}

	if (gdome_xml_n_eventEnabledByCode (self, DOM_NODE_INSERTED_INTO_DOCUMENT_EVENT_TYPE)) {
		/* Fire DOMNodeInsertedIntoDocument */
		mev = gdome_evt_mevnt_mkref ();
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_NODE_INSERTED_INTO_DOCUMENT_EVENT_TYPE,
																						 FALSE, FALSE, NULL,
																						 NULL, NULL, NULL, 0, exc);
		gdome_xml_n_dispatchEvent (newChild, (GdomeEvent *)mev, exc);
		gdome_xml_n_subTreeDispatchEvent (newChild, (GdomeEvent *)mev, exc);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}
	
	if (gdome_xml_n_eventEnabledByCode (self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMSubtreeModified */
		mev = gdome_evt_mevnt_mkref ();
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																						 TRUE, FALSE, NULL,
																						 NULL, NULL, NULL, 0, exc);
		gdome_xml_n_dispatchEvent (self, (GdomeEvent *)mev, exc);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}

	return gdome_xml_n_mkref (ret);
}

/**
 * gdome_xml_n_isSupported:
 * @self:  Node Object ref
 * @feature:  The name of the feature to test.
 * @version:  This is the version number of the feature to test. In Level 2,
 *            version 1, this is the string "2.0". If the version is not
 *            specified, supporting any version of the feature will cause the
 *            method to return %TRUE.
 * @exc:  Exception Object ref
 *
 * Tests whether the DOM implementation implements a specific feature and that
 * feature is supported by this node.
 * Returns: %TRUE if the specified feature is supported on this node, %FALSE
 * otherwise.
 */
GdomeBoolean
gdome_xml_n_isSupported (GdomeNode *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	GdomeBoolean ret;
  GdomeDOMImplementation *domimpl;

	g_return_val_if_fail (priv != NULL, FALSE);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), FALSE);
	g_return_val_if_fail (feature != NULL, FALSE);
	g_return_val_if_fail (exc != NULL, FALSE);

	if (version == NULL)
		return TRUE;

	domimpl = gdome_xml_di_mkref();
	ret = gdome_xml_di_hasFeature (domimpl, feature, version, exc);
	gdome_xml_di_unref (domimpl, exc);

  return ret;
}

/**
 * gdome_xml_n_normalize:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Puts all Text nodes in the full depth of the sub-tree underneath this Node,
 * including attribute nodes, into a "normal" form where only structure (e.g.,
 * elements, comments, processing instructions, CDATA sections, and entity
 * references) separates Text nodes, i.e., there are neither adjacent Text
 * nodes nor empty Text nodes.
 */
void
gdome_xml_n_normalize (GdomeNode *self, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	GdomeNode *child = NULL, *new_child = NULL;
	GdomeNode *next = NULL, *new_next = NULL;
	GdomeDOMString *str = NULL;
	GdomeNode *result = NULL;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_N (priv));
	g_return_if_fail (exc != NULL);

	/* this is a generic implementation, not optimized for speed */
	child = gdome_n_firstChild (self, exc);
	while (child != NULL) {
		unsigned short type;

		type = gdome_n_nodeType (child, exc);
		if (type == GDOME_TEXT_NODE) {
			next = gdome_n_nextSibling (child, exc);
			while (next != NULL) {
				if (gdome_n_nodeType (next, exc) != GDOME_TEXT_NODE) {
					gdome_n_unref (next, exc);
					break;
				}
				str = gdome_cd_data ((GdomeCharacterData *)next, exc);
				gdome_cd_appendData ((GdomeCharacterData *)child, str, exc);
				gdome_str_unref (str);
				new_next = gdome_n_nextSibling (next, exc);
				result = gdome_n_removeChild (self, next, exc);
				gdome_n_unref (result, exc);
				gdome_n_unref (next, exc);
				next = new_next;
			}
		} else if (type == GDOME_ELEMENT_NODE) {
			GdomeNamedNodeMap *nnm;
			gulong i, n;

			gdome_n_normalize (child, exc);
			nnm = gdome_n_attributes (child, exc);
			n = gdome_nnm_length (nnm, exc);
			for (i = 0; i < n; i++) {
				GdomeNode *attr;

				attr = gdome_nnm_item (nnm, i, exc);
				gdome_n_normalize (attr, exc);
				gdome_n_unref (attr, exc);
			}
			gdome_nnm_unref (nnm, exc);
		}
		new_child = gdome_n_nextSibling (child, exc);
		gdome_n_unref (child, exc);
		child = new_child;
	}
}

/**
 * gdome_xml_n_removeChild:
 * @self:  Node Object ref
 * @oldChild:  The node being removed
 * @exc:  Exception Object ref
 *
 * Removes the child node indicated by @oldChild from the list of children, and
 * returns it.
 *
 * %GDOME_NOT_FOUND_ERR: Raised if @oldChild is not a child of this node.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 * Returns: the node removed.
 */
GdomeNode *
gdome_xml_n_removeChild (GdomeNode *self, GdomeNode *oldChild, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	Gdome_xml_Node *old_priv = (Gdome_xml_Node *)oldChild;
	GdomeMutationEvent *mev;
	xmlNode *ret;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), NULL);
	g_return_val_if_fail (old_priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_TREE_N (old_priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return NULL;
  }

	if (gdome_xml_n_eventEnabledByCode (self, DOM_NODE_REMOVED_EVENT_TYPE)) {
		/* Fire DOMNodeRemoved */
		mev = gdome_evt_mevnt_mkref ();
		gdome_xml_n_ref (self, exc);
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_NODE_REMOVED_EVENT_TYPE,
																						 TRUE, FALSE, self,
																						 NULL, NULL, NULL, 0, exc);
		gdome_xml_n_dispatchEvent (oldChild, (GdomeEvent *)mev, exc);
		gdome_xml_n_unref (self, exc);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}

	if (gdome_xml_n_eventEnabledByCode (self, DOM_NODE_REMOVED_FROM_DOCUMENT_EVENT_TYPE)) {
		/* Fire DOMNodeRemovedFromDocument */
		mev = gdome_evt_mevnt_mkref ();
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_NODE_REMOVED_FROM_DOCUMENT_EVENT_TYPE,
																						 FALSE, FALSE, NULL,
																						 NULL, NULL, NULL, 0, exc);
		gdome_xml_n_dispatchEvent (oldChild, (GdomeEvent *)mev, exc);
		gdome_xml_n_subTreeDispatchEvent (oldChild, (GdomeEvent *)mev, exc);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}

	if (gdome_xml_n_eventEnabledByCode (self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMSubtreeModified */
		mev = gdome_evt_mevnt_mkref ();
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																						 TRUE, FALSE, NULL,
																						 NULL, NULL, NULL, 0, exc);
		gdome_xml_n_dispatchEvent (self, (GdomeEvent *)mev, exc);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}

  if ((ret = gdome_xmlUnlinkChild(priv->n, old_priv->n)) == NULL) {
		*exc = GDOME_NOT_FOUND_ERR;
		return NULL;
  }

  return gdome_xml_n_mkref (ret);
}

/**
 * gdome_xml_n_replaceChild:
 * @self:  Node Object ref
 * @newChild:  The new node to put in the child list
 * @oldChild:  The node being replaced in the list
 * @exc:  Exception Object ref
 *
 * Replaces the child node @oldChild with @newChild in the list of children,
 * and returns the @oldChild node. If @newChild is a DocumentFragment object,
 * @oldChild is replaced by all of the DocumentFragment children, which are
 * inserted in the same order. If the @newChild is already in the tree, it is
 * first removed.
 *
 * %GDOME_HIERARCHY_REQUEST_ERR: Raised if this node is of a type that does
 * not allow children of the type of the @newChild node, or if the node to put
 * in is one of this node's ancestors or this node itself.
 * %GDOME_WRONG_DOCUMENT_ERR: Raised if @newChild was created from a different
 * document than the one that created this node.
 * %GDOME_NOT_FOUND_ERR: Raised if @oldChild is not a child of this node.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 * Returns: the node replaced.
 */
GdomeNode *
gdome_xml_n_replaceChild (GdomeNode *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	GdomeNode *ret = NULL;
	GdomeNode *refChild = NULL;

	g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (self), NULL);
	g_return_val_if_fail (newChild != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (newChild), NULL);
	g_return_val_if_fail (oldChild != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (oldChild), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

  if (GDOME_ISREADONLY (self)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return NULL;
  }

  refChild = gdome_n_nextSibling (oldChild, exc);
	if (gdome_n_removeChild (self, oldChild, exc)) {
		if (gdome_n_insertBefore (self, newChild, refChild, exc)) {
			gdome_n_unref (newChild, exc);
      ret = oldChild;
		} else {
			gdome_n_unref (oldChild, exc);
			ret = NULL;
		}
	}

	if (refChild)
		gdome_n_unref (refChild, exc);

  return ret;
}

/**
 * gdome_xml_n_addEventListener:
 * @self:  Node Object ref
 * @type:  the event type for which the user is registering
 * @listener:  the listener parameter takes an interface implemented by the
 *             user which contains the methods to be called when the event occurs.
 * @useCapture:  if %TRUE, useCapture indicates that the user wishes to initiate
 *               capture. After initiating capture, all events of the specified
 *               type will be dispatched to the registered EventListener before
 *               being dispatched to any EventTargets beneath them in the tree.
 *               Events which are bubbling upward through the tree will not
 *               trigger an EventListener designated to use capture.
 * @exc:  Exception Object ref
 *
 * This method allows the registration of event listeners on the event target.
 * If an EventListener is added to an EventTarget while it is processing an
 * event, it will not be triggered by the current actions but may be triggered
 * during a later stage of event flow, such as the bubbling phase.
 * If multiple identical EventListeners are registered on the same EventTarget
 * with the same parameters the duplicate instances are discarded. They do not
 * cause the EventListener to be called twice and since they are discarded
 * they do not need to be removed with the removeEventListener method.
 */
void
gdome_xml_n_addEventListener (GdomeNode *self, GdomeDOMString *type,
                              GdomeEventListener *listener, GdomeBoolean useCapture,
                              GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	Gdome_xml_ListenerList *ll = NULL;
	Gdome_xml_ListenerList *temp;
  guint32 codeType;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_N (priv));
	g_return_if_fail (listener != NULL);
	g_return_if_fail (type != NULL);
	g_return_if_fail (exc != NULL);

	/* duplicate instances are discarted */
  codeType = gdome_evt_evnt_codeOfName (type->str);
	temp = priv->ll;
	while (temp) {
		if (temp->listener == listener &&
				temp->type == codeType &&
				temp->useCapture == useCapture) {
			return;
		}
		temp = temp->next;
	}

	/* create a new ListenerList entry*/
	ll = g_new (Gdome_xml_ListenerList, 1);
	ll->type = codeType;
	gdome_evt_evntl_ref (listener, exc);
	ll->listener = listener;
	ll->useCapture = useCapture;

	/* add it at begin of the ListenerList of the target */
	ll->next = priv->ll;
	priv->ll = ll;
}

/**
 * gdome_xml_n_removeEventListener:
 * @self:  Node Object ref
 * @type:  Specifies the event type of the EventListener being removed.
 * @listener:  The EventListener parameter indicates the EventListener to be
 *             removed.
 * @useCapture:  Specifies whether the EventListener being removed was
 *               registered as a capturing listener or not. If a listener was
 *               registered twice, one with capture and one without, each must
 *               be removed separately. Removal of a capturing listener does
 *               not affect a non-capturing version of the same listener, and
 *               vice versa.
 * @exc:  Exception Object ref
 *
 * This method allows the removal of event listeners from the event target. If
 * an EventListener is removed from an EventTarget while it is processing an
 * event, it will not be triggered by the current actions. EventListeners can
 * never be invoked after being removed. Calling removeEventListener with
 * arguments which do not identify any currently registered EventListener on
 * the EventTarget has no effect.
 */
void
gdome_xml_n_removeEventListener (GdomeNode *self, GdomeDOMString *type, 
																 GdomeEventListener *listener, GdomeBoolean useCapture, 
																 GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
  Gdome_xml_ListenerList *temp, *prev = NULL;
  guint32 codeType;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_N (priv));
	g_return_if_fail (listener != NULL);
	g_return_if_fail (type != NULL);
	g_return_if_fail (exc != NULL);

  codeType = gdome_evt_evnt_codeOfName (type->str);
  temp = priv->ll;
	while (temp) {
    if (useCapture == temp->useCapture &&
        temp->listener == listener &&
        temp->type == codeType) {
        if (prev)
          prev->next = temp->next;
        else {
          g_assert (priv->ll == temp);
          priv->ll = priv->ll->next;
        }
        temp->next = NULL;
        gdome_evt_evntl_unref (listener, exc);
        g_free (temp);
        break;
    }
    prev = temp;
		temp = temp->next;
	}
}


/**
 * gdome_xml_n_dispatchEvent:
 * @self:  Node Object ref
 * @evt:  Specifies the event type, behavior, and contextual information to be
 *        used in processing the event.
 * @exc:  Exception Object ref
 *
 * This method allows the dispatch of events into the implementations event
 * model. Events dispatched in this manner will have the same capturing and
 * bubbling behavior as events dispatched directly by the implementation. The
 * target of the event is the EventTarget on which dispatchEvent is called.
 *
 * GDOME_UNSPECIFIED_EVENT_TYPE_ERR: Raised if the Event's type was not
 * specified by initializing the event before dispatchEvent was called.
 * Specification of the Event's type as %NULL or an empty string will also
 * trigger this exception.
 * Returns: if preventDefault was called the value is %FALSE, else the value
 *          is %TRUE.
 */
GdomeBoolean
gdome_xml_n_dispatchEvent (GdomeNode *self, GdomeEvent *evt, GdomeException *exc)
{
  Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
  Gdome_evt_Event *ev_priv = (Gdome_evt_Event *)evt;

	g_return_val_if_fail (priv != NULL, FALSE);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), FALSE);
	g_return_val_if_fail (evt != NULL, FALSE);
  g_return_val_if_fail (exc != NULL, FALSE);

  if (ev_priv->type == 0) {
    *exc = GDOME_EVENT_EXCEPTION + GDOME_UNSPECIFIED_EVENT_TYPE_ERR;
    return FALSE;
  }

  gdome_xml_n_ref (self, exc);
	ev_priv->target = self;
  gdome_evt_fireEvent (evt);
  gdome_xml_n_unref (self, exc);
	
	return !ev_priv->default_prevented;
}

/**
 * gdome_xml_n_subTreeDispatchEvent:
 * @self:  Node Object ref
 * @evt:  Specifies the event type, behavior, and contextual information to be
 *        used in processing the event.
 * @exc:  Exception Object ref
 *
 * This method allows the dispatch of events into the implementations event
 * model. Events dispatched in this manner will have the same capturing and
 * bubbling behavior as events dispatched directly by the implementation. The
 * target of the event is any nodes in the subtree of the EventTarget on which
 * dispatchEvent is called.
 *
 * GDOME_UNSPECIFIED_EVENT_TYPE_ERR: Raised if the Event's type was not
 * specified by initializing the event before dispatchEvent was called.
 * Specification of the Event's type as %NULL or an empty string will also
 * trigger this exception.
 */
void
gdome_xml_n_subTreeDispatchEvent (GdomeNode *self, GdomeEvent *evt, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
  GdomeNode *child, *temp;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_N (priv));
  g_return_if_fail (evt != NULL);
	g_return_if_fail (exc != NULL);

	child = gdome_xml_n_firstChild (self, exc);

	while (child) {
		gdome_xml_n_dispatchEvent (child, evt, exc);
		gdome_xml_n_subTreeDispatchEvent (child, evt, exc);
		temp = gdome_xml_n_nextSibling (child, exc);
		gdome_xml_n_unref (child, exc);
		child = temp;
	}
}

/**
 * gdome_xml_n_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_xml_n_canAppend (GdomeNode *self, GdomeNode *newChild, GdomeException *exc)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	Gdome_xml_Node *new_priv = (Gdome_xml_Node *)newChild;
	xmlElementType new_type;
  GdomeBoolean ret = TRUE;

	g_return_val_if_fail (priv != NULL, FALSE);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), FALSE);
	g_return_val_if_fail (new_priv != NULL, FALSE);
	g_return_val_if_fail (GDOME_XML_IS_N (new_priv), FALSE);
	g_return_val_if_fail (exc != NULL, FALSE);

  /* Check if self can have a child of newChild type */
  new_type = gdome_xmlGetType (new_priv->n);
  if (new_type == XML_DOCUMENT_FRAG_NODE)
		return TRUE;
	switch (gdome_xmlGetType (priv->n)) {
	case XML_DOCUMENT_TYPE_NODE:
  case XML_DTD_NODE:
	case XML_PI_NODE:
	case XML_COMMENT_NODE:
  case XML_TEXT_NODE:
  case XML_CDATA_SECTION_NODE:
  case XML_NOTATION_NODE:
		ret = FALSE;
    break;
	case XML_DOCUMENT_NODE:
    /* Element (max of one), ProcessingInstruction, Comment, DocumentType (max of one) */
		if ((new_type != XML_ELEMENT_NODE &&
				 new_type != XML_PI_NODE &&
				 new_type != XML_COMMENT_NODE &&
				 new_type != XML_DTD_NODE &&
				 new_type != XML_DOCUMENT_TYPE_NODE) ||
				(new_type == XML_ELEMENT_NODE &&
				 xmlDocGetRootElement ((xmlDoc *)priv->n) != NULL) ||
				((new_type == XML_DTD_NODE ||
					new_type == XML_DOCUMENT_TYPE_NODE) &&
				 ((xmlDoc *)priv->n)->intSubset != NULL))
			ret = FALSE;
    break;
  case XML_DOCUMENT_FRAG_NODE:
  case XML_ENTITY_REF_NODE:
  case XML_ELEMENT_NODE:
  case XML_ENTITY_NODE:
	case XML_ENTITY_DECL:
		/* Element, ProcessingInstruction, Comment, Text, CDATASection, EntityReference	*/
		if (new_type != XML_ELEMENT_NODE &&
				new_type != XML_PI_NODE &&
				new_type != XML_COMMENT_NODE &&
				new_type != XML_TEXT_NODE &&
				new_type != XML_CDATA_SECTION_NODE &&
				new_type != XML_ENTITY_REF_NODE)
			ret = FALSE;
    break;
	case XML_ATTRIBUTE_NODE:
    /* Text, EntityReference */
		if (new_type != XML_TEXT_NODE &&
				new_type != XML_ENTITY_REF_NODE)
			ret = FALSE;
    break;
  default:
    g_warning ("gdome_xml_n_canAppend: invalid node type");
		ret = FALSE;
    break;
	}

  /* Check if newChild is an ancestor of self */
	if (ret) {
		xmlNode *p = priv->n;
		while (p != NULL) {
			if (p == new_priv->n) {
				ret = FALSE;
				break;
			}
			p = gdome_xmlGetParent (p);
		}
	}

  return ret;
}

/**
 * gdome_xml_n_eventEnabledByCode:
 * @self:  Node Objects ref
 * @code:  The event type to check
 *
 * Returns: TRUE if the specified event type is enabled
 */
GdomeBoolean
gdome_xml_n_eventEnabledByCode (GdomeNode *self, guint32 code)
{
	Gdome_xml_Node *priv = (Gdome_xml_Node *)self;
	GdomeException exc = 0;
	GdomeDocument *doc;
	GdomeBoolean res;

	g_return_val_if_fail (priv != NULL, FALSE);
	g_return_val_if_fail (GDOME_XML_IS_N (priv), FALSE);

  doc = gdome_xml_doc_mkref (gdome_xmlGetOwnerOrSelf (priv->n));
	g_assert(doc != NULL);

	res = gdome_xml_doc_eventEnabledByCode (doc, code);

	gdome_doc_unref(doc, &exc);
	g_assert(exc == 0);

	return res;
}
