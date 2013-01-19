/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-nnm.c
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
#include <glib.h>
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
#include "gdome-xml-attribute.h"
#include "gdome-xml-document.h"
#include "gdome-xml-element.h"
#include "gdome-xml-str.h"
#include "gdome-evt-event.h"
#include "gdome-evt-mevent.h"
#include "gdome-xml-nnodem.h"
#include "gdome-xml-xmlutil.h"

typedef struct _nodeIterator nodeIterator;
struct _nodeIterator {
	gulong cur;
	gulong index;
	xmlNode *node;
};

const GdomeNamedNodeMapVtab gdome_xml_nnm_vtab = {
	gdome_xml_nnm_ref,
	gdome_xml_nnm_unref,
	gdome_xml_nnm_query_interface,
	gdome_xml_nnm_getNamedItem,
	gdome_xml_nnm_setNamedItem,
	gdome_xml_nnm_removeNamedItem,
	gdome_xml_nnm_item,
	gdome_xml_nnm_length,
	gdome_xml_nnm_getNamedItemNS,
	gdome_xml_nnm_setNamedItemNS,
	gdome_xml_nnm_removeNamedItemNS
};

void
itemHashScanner (void *payload, void *data, xmlChar *name) {
  nodeIterator *priv = (nodeIterator *)data;

	if(priv->cur < priv->index)
    priv->cur++;
  else if(priv->node == NULL)
    priv->node = (xmlNode *)payload;
}

/**
 * gdome_xml_nnm_mkref:
 * @doc:  the Document owner ref
 * @elem:  the Element owner ref, can be %NULL
 * @data:  the pointer to the structure to export as NamedNodeMap
 * @data2:  extra data, used for getAttributes
 * @readonly:  %TRUE if the GdomeNamedNodeMap is readonly
 * @type:  the type of the @data structure passed
 *
 * Construct the NamedNodeMap Object.
 * Returns: a pointer to the GdomeNamedNodeMap Object.
 */
GdomeNamedNodeMap *
gdome_xml_nnm_mkref (GdomeDocument *doc, GdomeElement *elem, void *data, void *data2, GdomeAccessType accessType, GdomeNodeType type) {
  Gdome_xml_NamedNodeMap *result = NULL;
  GdomeException exc;

	if (doc == NULL)
    return NULL;

	result = g_new (Gdome_xml_NamedNodeMap, 1);
  memset(result, 0, sizeof(Gdome_xml_NamedNodeMap));
#ifdef DEBUG_REFCNT
  gdome_refdbg_addRef ((void *)result, GDOME_REFDBG_NAMEDNODEMAP);
#endif
	result->refcnt = 1;
	result->vtab = &gdome_xml_nnm_vtab;

  result->doc = doc;
  if (elem != NULL)
    gdome_xml_n_ref ((GdomeNode *)elem, &exc);
  result->elem = elem;
	if(type == GDOME_ENTITY_NODE || type == GDOME_NOTATION_NODE)
		result->data.ht = (xmlHashTable *)data;
	else if(type == GDOME_ATTRIBUTE_NODE) {
		result->data.lst = (xmlNode *)data;
		result->ns = (xmlNs *)data2;
	} else {
		g_free(result);
		return NULL;
	}

	result->accessType = accessType;
	result->type = type;

	return (GdomeNamedNodeMap *) result;
}

/**
 * gdome_xml_nnm_ref:
 * @self:  NodeList Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified NamedNodeMap.
 */
void
gdome_xml_nnm_ref (GdomeNamedNodeMap *self, GdomeException *exc) {
	Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;
	
  g_return_if_fail (self != NULL);
  g_return_if_fail (exc != NULL);

	priv->refcnt++;
}

/**
 * gdome_xml_nnm_unref:
 * @self:  NodeList Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified NamedNodeMap. Free the
 * NamedNodeMap structure if the specified NamedNodeMap will have zero
 * reference.
 */
void
gdome_xml_nnm_unref (GdomeNamedNodeMap *self, GdomeException *exc) {
	Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;

  g_return_if_fail (self != NULL);
  g_return_if_fail (exc != NULL);

	priv->refcnt--;

  if (priv->refcnt == 0) {
    gdome_xml_n_unref ((GdomeNode *)priv->doc, exc);
    if (priv->elem != NULL)
      gdome_xml_n_unref ((GdomeNode *)priv->elem, exc);

#ifdef DEBUG_REFCNT
    gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NAMEDNODEMAP);
#endif
    g_free (self);
  }
}

/**
 * gdome_xml_nnm_query_interface:
 * @self:  NamedNodeMap Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_nnm_query_interface (GdomeNamedNodeMap *self, const char *interface, GdomeException *exc)
{
	Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "NamedNodeMap")) {
    priv->refcnt++;
    return self;
  }
  else
    return NULL;
}

/**
 * gdome_xml_nnm_length:
 * @self:  NamedNodeMap Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the number of nodes in this map. The range of valid child node
 * indices is 0 to length-1 inclusive.
 */
gulong
gdome_xml_nnm_length (GdomeNamedNodeMap *self, GdomeException *exc) {
	Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;
	xmlHashTable *ht;
	xmlNode *n;
	xmlNs *ns;
	long result = 0L;

  g_return_val_if_fail (priv != NULL, 0L);
  g_return_val_if_fail (exc != NULL, 0L);

  /* HashTable */
	if (priv->type == GDOME_ENTITY_NODE || priv->type == GDOME_NOTATION_NODE) {
		ht = priv->data.ht;

    if((result = xmlHashSize(ht)) == -1)
      return 0L;
	}
  /* List */
	else if (priv->type == GDOME_ATTRIBUTE_NODE) {
		for(n  = priv->data.lst; n  != NULL; n  = n->next,  result++);
		for(ns = priv->ns;       ns != NULL; ns = ns->next, result++);
	}
	
	return result;
}

/**
 * gdome_xml_nnm_getNamedItem:
 * @self:  NamedNodeMap Objects ref
 * @name:  The nodeName of the node to retrieve.
 * @exc:  Exception Object ref
 *
 * Retrieves a node specified by name.
 * Returns: a Node (of any type) with the specified nodeName, or %NULL if it
 * does not identify any node in this map.
 */
GdomeNode *
gdome_xml_nnm_getNamedItem (GdomeNamedNodeMap *self, GdomeDOMString *name, GdomeException *exc) {
	Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;
	xmlHashTable *ht = NULL;
	xmlNode *n = NULL;
	xmlNs *ns = NULL;
	gchar **strs = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (name != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  /* HashTable */
	if (priv->type == GDOME_ENTITY_NODE || priv->type == GDOME_NOTATION_NODE) {
		ht = priv->data.ht;

    n = (xmlNode*) xmlHashLookup (ht, (xmlChar *) name->str);
	}
  /* List */
  else if (priv->type == GDOME_ATTRIBUTE_NODE) {
		strs = g_strsplit((gchar *)name->str, ":", 0);
		if (xmlStrEqual(strs[0], "xmlns")) {
			n = ((Gdome_xml_Element *)priv->elem)->n;
			ns = gdome_xmlGetNsDecl(n,
															(xmlChar *)strs[1]);
			g_strfreev(strs);
			if (ns != NULL) {
				return (GdomeNode *) gdome_xml_a_mkref_ns (n, ns);
			} else {
				return NULL;
			}
		}
		g_strfreev(strs);
    n = (xmlNode *)gdome_xmlGetAttr(((Gdome_xml_Element *)priv->elem)->n,
                                    (xmlChar *)name->str);
	}
	if(n != NULL)
		return (GdomeNode *) gdome_xml_n_mkref (n);
	else
		return NULL;
}

/**
 * gdome_xml_nnm_getNamedItemNS:
 * @self:  NamedNodeMap Objects ref
 * @namespaceURI:  The namespace URI of the node to retrieve.
 * @localName:  The local name of the node to retrieve.
 * @exc:  Exception Object ref
 *
 * Retrieves a node specified by local name and namespace URI.
 * Returns: a Node (of any type) with the specified local name and namespace
 * URI, or %NULL if they do not identify any node in this map.
 */
GdomeNode *
gdome_xml_nnm_getNamedItemNS (GdomeNamedNodeMap *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc) {
	Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;
	xmlHashTable *ht = NULL;
	xmlNode *n = NULL;
	xmlNs *ns = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (localName != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

	if (namespaceURI == NULL)
		return gdome_xml_nnm_getNamedItem (self, localName, exc);

  /* HashTable */
	if (priv->type == GDOME_ENTITY_NODE || priv->type == GDOME_NOTATION_NODE) {
		ht = priv->data.ht;

    n = (xmlNode*) xmlHashLookup2 (ht, (xmlChar *) localName->str,
                                   (xmlChar *) namespaceURI->str);
	}
  /* List */
	else if (priv->type == GDOME_ATTRIBUTE_NODE) {
		if (xmlStrEqual(namespaceURI->str, GDOME_XMLNS_NAMESPACE)) {
			n = ((Gdome_xml_Element *)priv->elem)->n;
			ns = gdome_xmlGetNsDecl(n,
															(xmlChar *)localName->str);
			if (ns != NULL)
				return (GdomeNode *) gdome_xml_a_mkref_ns (n, ns);
			else
				return NULL;
		} else {
			n = (xmlNode *)gdome_xmlGetNsAttr(((Gdome_xml_Element *)priv->elem)->n,
																				(xmlChar *)namespaceURI->str,
																				(xmlChar *)localName->str);
		}
	}

	if(n != NULL)
		return (GdomeNode *) gdome_xml_n_mkref (n);
	else
		return NULL;
}

/**
 * gdome_xml_nnm_item:
 * @self:  NamedNodeMap Objects ref
 * @index:  Index into this map.
 * @exc:  Exception Object ref
 *
 * Returns: the indexth item in the map. If index is greater than or equal to
 * the number of nodes in this map, this returns %NULL.
 */
GdomeNode *
gdome_xml_nnm_item (GdomeNamedNodeMap *self, gulong index, GdomeException *exc) {
	Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;
	xmlNode *n = NULL;
	xmlNs *ns = NULL;
	xmlHashTable *ht;
	nodeIterator *iter;
  gulong cur;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  /* HashTable */
	if (priv->type == GDOME_ENTITY_NODE || priv->type == GDOME_NOTATION_NODE) {
		ht = priv->data.ht;
		iter = g_new(nodeIterator, 1);

    iter->cur = 0L;
		iter->index = index;
    iter->node = NULL;
		xmlHashScan(ht, itemHashScanner, iter);
		n = iter->node;

    g_free(iter);
	}
  /* List */
	else if (priv->type == GDOME_ATTRIBUTE_NODE) {
    for(n = priv->data.lst, cur = 0;
        n != NULL && cur < index;
        cur++,	n = n->next);
		if (n == NULL) {
			for (ns = priv->ns;
					 ns != NULL && cur < index;
					 cur++, ns = ns->next);
			return gdome_xml_a_mkref_ns (((Gdome_xml_Element *)priv->elem)->n, ns);
		}
	}

	return gdome_xml_n_mkref (n);
}

/**
 * gdome_xml_nnm_removeNamedItem:
 * @self:  NamedNodeMap Objects ref
 * @name:  The nodeName of the node to remove.
 * @exc:  Exception Object ref
 *
 * Removes a node specified by name. When this map contains the attributes
 * attached to an element, if the removed attribute is known to have a default
 * value, an attribute immediately appears containing the default value as
 * well as the corresponding namespace URI, local name, and prefix when applicable.
 *
 * %GDOME_NOT_FOUND_ERR: Raised if there is no node named @name in this map.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this map is readonly.
 * Returns: the node removed from this map if a node with such a name exists.
 */
GdomeNode *
gdome_xml_nnm_removeNamedItem (GdomeNamedNodeMap *self, GdomeDOMString *name, GdomeException *exc) {
  Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;
  GdomeNode *ret = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (name != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (priv->accessType == GDOME_READONLY_NODE) {
		*exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
		return NULL;
	}

  /* Hash Table */
	if (priv->type == GDOME_ENTITY_NODE || priv->type == GDOME_NOTATION_NODE) {
    xmlHashTable *ht = priv->data.ht;

    if((ret = gdome_xml_nnm_getNamedItem(self, name, exc)) != NULL) {
      xmlHashRemoveEntry(ht, name->str, NULL);
			return ret;
    }
	}
  /* List */
  else if (priv->type == GDOME_ATTRIBUTE_NODE) {
    GdomeNode *parent = gdome_xml_n_mkref (((Gdome_xml_Element *)priv->elem)->n);
    Gdome_xml_Node *parent_priv = (Gdome_xml_Node *)parent;
    xmlChar *defaultvalue = NULL;
    GdomeNode *defaultattr = NULL;
    GdomeMutationEvent *mev;
    GdomeDOMString *attrName;
		gchar **strs = NULL;

		strs = g_strsplit((gchar *)name->str, ":", 0);

		if (xmlStrEqual(strs[0], "xmlns")) {
			xmlNs *ns = gdome_xmlUnlinkNsDecl(parent_priv->n, strs[1]);
			/* ns is no longer in libxml tree, need to add it back so that it will
				 get freed when libxml doc tree is freed */
			gdome_xmlSetOldNs(parent_priv->n->doc, ns);
			ret = gdome_xml_a_mkref_ns (parent_priv->n, ns);
		} else 
			ret = gdome_xml_n_mkref ((xmlNode *)gdome_xmlUnlinkAttr(parent_priv->n,
																															name->str));

    if(ret != NULL) {
			if (gdome_xml_nnm_eventEnabledByCode (self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
				/* Fire DOMAttrModified */
				mev = gdome_evt_mevnt_mkref ();
				attrName = gdome_xml_n_nodeName (ret, exc);
				gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																								 TRUE, FALSE, ret,
																								 NULL, NULL, attrName, GDOME_REMOVAL, exc);
				gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
				gdome_xml_str_unref (attrName);
				gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
			}

      /* resync structure */
			priv->data.lst = (xmlNode *)gdome_xmlGetAttrList(parent_priv->n);

      /* check for a default value and eventually insert it */
      defaultvalue = xmlGetProp(parent_priv->n, name->str);
			if(defaultvalue != NULL) {
        defaultattr = gdome_xml_n_mkref ((xmlNode *)xmlNewProp(parent_priv->n,
                                                               name->str,
                                                               defaultvalue));
        xmlFree(defaultvalue);

				if (gdome_xml_nnm_eventEnabledByCode (self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
					/* Fire DOMAttrModified */
					if (defaultattr != NULL) {
						mev = gdome_evt_mevnt_mkref ();
						attrName = gdome_xml_n_nodeName (defaultattr, exc);
						gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																										 TRUE, FALSE, defaultattr,
																										 NULL, NULL, attrName, GDOME_ADDITION, exc);
						gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
						gdome_xml_str_unref (attrName);
						gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
						gdome_xml_n_unref (defaultattr, exc);
					}
				}
      }

			if (gdome_xml_nnm_eventEnabledByCode (self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
				/* Fire DOMSubtreeModified */
				mev = gdome_evt_mevnt_mkref ();
				gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																								 TRUE, FALSE, NULL,
																								 NULL, NULL, NULL, 0, exc);
				gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
				gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
			}

      /* Check for ID removal */
      if (xmlIsID(parent_priv->n->doc, parent_priv->n,
                  (xmlAttr *)((Gdome_xml_Node *)ret)->n))
        xmlRemoveID(parent_priv->n->doc, (xmlAttr *)((Gdome_xml_Node *)ret)->n);
    }
    gdome_xml_n_unref (parent, exc);
		g_strfreev(strs);
    return ret;
	}

	*exc = GDOME_NOT_FOUND_ERR;
	return NULL;
}

/**
 * gdome_xml_nnm_removeNamedItemNS:
 * @self:  NamedNodeMap Objects ref
 * @namespaceURI:  The namespace URI of the node to remove.
 * @localName:  The local name of the node to remove.
 * @exc:  Exception Object ref
 *
 * Removes a node specified by local name and namespace URI. When this map
 * contains the attributes attached to an element, if the removed attribute
 * is known to have a default value, an attribute immediately appears
 * containing the default value as well as the corresponding namespace URI,
 * local name, and prefix when applicable.
 *
 * %GDOME_NOT_FOUND_ERR: Raised if there is no node named @name in this map.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this map is readonly.
 * Returns: the node removed from this map if a node with such a local name and
 * namespace URI exists.
 */
GdomeNode *
gdome_xml_nnm_removeNamedItemNS (GdomeNamedNodeMap *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc) {
	Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;
  GdomeNode *ret = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (localName != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

	if (namespaceURI == NULL)
		return gdome_xml_nnm_removeNamedItem (self, localName, exc);

	if (priv->accessType == GDOME_READONLY_NODE) {
		*exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
		return NULL;
	}

  /* Hash Table */
	if (priv->type == GDOME_ENTITY_NODE || priv->type == GDOME_NOTATION_NODE) {
    xmlHashTable *ht = priv->data.ht;

    if((ret = gdome_xml_nnm_getNamedItemNS(self, namespaceURI, localName, exc)) != NULL) {
      xmlHashRemoveEntry2(ht, namespaceURI->str, localName->str, NULL);
      return ret;
    }
	}
  /* List */
	else if (priv->type == GDOME_ATTRIBUTE_NODE) {
    GdomeNode *parent = gdome_xml_n_mkref (((Gdome_xml_Element *)priv->elem)->n);
    Gdome_xml_Node *parent_priv = (Gdome_xml_Node *)parent;
    xmlChar *defaultvalue = NULL;
    GdomeNode *defaultattr = NULL;
    GdomeMutationEvent *mev;
    GdomeDOMString *attrName;

		if (xmlStrEqual(namespaceURI->str, GDOME_XMLNS_NAMESPACE))
			ret = gdome_xml_a_mkref_ns (parent_priv->n,
																	(xmlNs *)gdome_xmlUnlinkNsDecl(parent_priv->n,
																																 localName->str));
		else
			ret = gdome_xml_n_mkref ((xmlNode *)gdome_xmlUnlinkNsAttr(parent_priv->n,
																																namespaceURI->str,
																																localName->str));
    if(ret != NULL) {
			if (gdome_xml_nnm_eventEnabledByCode (self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
				/* Fire DOMAttrModified */
				mev = gdome_evt_mevnt_mkref ();
				attrName = gdome_xml_n_nodeName (ret, exc);
				gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																								 TRUE, FALSE, ret,
																								 NULL, NULL, attrName, GDOME_REMOVAL, exc);
				gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
				gdome_xml_str_unref (attrName);
				gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
			}

      /* resync structure */
			priv->data.lst = (xmlNode *)gdome_xmlGetAttrList(parent_priv->n);

      /* check for a default value and eventually insert it */
      defaultvalue = xmlGetNsProp(parent_priv->n, localName->str, namespaceURI->str);
			if(defaultvalue != NULL) {
        defaultattr = gdome_xml_n_mkref ((xmlNode *)xmlNewNsProp(parent_priv->n,
                                                                 gdome_xmlGetNs (((Gdome_xml_Node *)ret)->n),
                                                                 localName->str,
                                                                 defaultvalue));
        xmlFree(defaultvalue);

				if (gdome_xml_nnm_eventEnabledByCode (self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
					/* Fire DOMAttrModified */
					if (defaultattr != NULL) {
						mev = gdome_evt_mevnt_mkref ();
						attrName = gdome_xml_n_nodeName (defaultattr, exc);
						gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																										 TRUE, FALSE, defaultattr,
																										 NULL, NULL, attrName, GDOME_ADDITION, exc);
						gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
						gdome_xml_str_unref (attrName);
						gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
						gdome_xml_n_unref (defaultattr, exc);
					}
				}
			}

			if (gdome_xml_nnm_eventEnabledByCode (self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
				/* Fire DOMSubtreeModified */
				mev = gdome_evt_mevnt_mkref ();
				gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																								 TRUE, FALSE, NULL,
																								 NULL, NULL, NULL, 0, exc);
				gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
				gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
			}

      /* Check for ID removal */
      if (xmlIsID(parent_priv->n->doc, parent_priv->n,
                  (xmlAttr *)((Gdome_xml_Node*)ret)->n))
        xmlRemoveID(parent_priv->n->doc, (xmlAttr *)((Gdome_xml_Node *)ret)->n);

    }
    gdome_xml_n_unref (parent, exc);
    return ret;
	}

	*exc = GDOME_NOT_FOUND_ERR;
	return NULL;
}

/**
 * gdome_xml_nnm_setNamedItem:
 * @self:  NamedNodeMap Objects ref
 * @arg:  a node to store in this map.
 * @exc:  Exception Object ref
 *
 * Adds a node using its nodeName attribute. If a node with that name is
 * already present in this map, it is replaced by the new one.
 *
 * %GDOME_WRONG_DOCUMENT_ERR: Raised if @arg was created from a different
 * document than the one that created this map.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this map is readonly.
 * %GDOME_INUSE_ATTRIBUTE_ERR: Raised if arg is an Attr that is already an
 * attribute of another Element object. The DOM user must explicitly clone
 * Attr nodes to re-use them in other elements.
 * Returns: if the new Node replaces an existing node, the replaced Node is
 * returned, otherwise %NULL is returned.
 * %GDOME_HIERARCHY_REQUEST_ERR: Raised if an attempt is made to add a node
 * doesn't belong in this NamedNodeMap. Examples would include trying to
 * insert something other than an Attr node into an Element's map of
 * attributes, or a non-Entity node into the DocumentType's map of Entities.
 */
GdomeNode *
gdome_xml_nnm_setNamedItem (GdomeNamedNodeMap *self, GdomeNode *arg, GdomeException *exc) {
	Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;
  xmlNode *newn = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (arg != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (arg), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

	if (((Gdome_xml_Document *)priv->doc)->n != ((Gdome_xml_Node*)arg)->n->doc) {
		*exc = GDOME_WRONG_DOCUMENT_ERR;
    return NULL;
	}
	if (priv->accessType == GDOME_READONLY_NODE) {
		*exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
		return NULL;
  }
  if (priv->type != gdome_xml_n_nodeType (arg, exc)) {
    *exc = GDOME_HIERARCHY_REQUEST_ERR;
    return NULL;
  }

	newn = ((Gdome_xml_Node*)arg)->n;

  /* Hash Table */
	if(priv->type == GDOME_ENTITY_NODE || priv->type == GDOME_NOTATION_NODE) {
		xmlHashTable *ht = priv->data.ht;
    xmlNode *ret = NULL;

		if((ret = xmlHashLookup(ht, gdome_xmlGetName(newn))) != NULL) {
			xmlHashUpdateEntry(ht, gdome_xmlGetName(newn), newn, NULL);
      return (GdomeNode *) gdome_xml_n_mkref(ret);
		} else {
			xmlHashAddEntry(ht, gdome_xmlGetName(newn), newn);
			return NULL;
		}
	}
  /* List */
	else if (priv->type == GDOME_ATTRIBUTE_NODE) {
    GdomeNode *parent = gdome_xml_n_mkref (((Gdome_xml_Element *)priv->elem)->n);
    Gdome_xml_Node *parent_priv = (Gdome_xml_Node *)parent;
    GdomeNode *ret = NULL;
    GdomeMutationEvent *mev;
    GdomeDOMString *attrName;

    if(newn->prev || newn->next ||
       (gdome_xmlGetParent(newn) && gdome_xmlGetParent(newn) != parent_priv->n)) {
      *exc = GDOME_INUSE_ATTRIBUTE_ERR;
      gdome_xml_n_unref (parent, exc);
			return NULL;
		}

    ret = gdome_xml_n_mkref ((xmlNode *)gdome_xmlUnlinkAttr(parent_priv->n,
                                                            gdome_xmlGetName(newn)));
    if (ret != NULL) {
			if (gdome_xml_nnm_eventEnabledByCode (self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
				/* Fire DOMAttrModified */
				mev = gdome_evt_mevnt_mkref ();
				attrName = gdome_xml_n_nodeName (ret, exc);
				gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																								 TRUE, FALSE, ret,
																								 NULL, NULL, attrName, GDOME_REMOVAL, exc);
				gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
				gdome_xml_str_unref (attrName);
				gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
			}
    }

    gdome_xmlLinkAttr(parent_priv->n, (xmlAttr *)newn);

		if (gdome_xml_nnm_eventEnabledByCode (self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
			/* Fire DOMAttrModified */
			mev = gdome_evt_mevnt_mkref ();
			gdome_xml_n_ref (arg, exc);
			attrName = gdome_xml_n_nodeName (arg, exc);
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, arg,
																							 NULL, NULL, attrName, GDOME_ADDITION, exc);
			gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
			gdome_xml_str_unref (attrName);
			gdome_xml_n_unref (arg, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}

    /* resync structure */
    priv->data.lst = (xmlNode *)gdome_xmlGetAttrList(parent_priv->n);

		if (gdome_xml_nnm_eventEnabledByCode (self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
			/* Fire DOMSubtreeModified */
			mev = gdome_evt_mevnt_mkref ();
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, NULL,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}

    gdome_xml_n_unref (parent, exc);
    return ret;
  }
  return NULL;
}

/**
 * gdome_xml_nnm_setNamedItemNS:
 * @self:  NamedNodeMap Objects ref
 * @arg:  a node to store in this map. The node will later be accessible
 *        using the value of its namespaceURI and localName attributes.
 * @exc:  Exception Object ref
 *
 * Adds a node using its namespaceURI and localName. If a node with that
 * namespace URI and that local name is already present in this map, it is
 * replaced by the new one
 *
 * %GDOME_WRONG_DOCUMENT_ERR: Raised if @arg was created from a different
 * document than the one that created this map.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this map is readonly.
 * %GDOME_INUSE_ATTRIBUTE_ERR: Raised if @arg is an Attr that is already an
 * attribute of another Element object. The DOM user must explicitly clone
 * Attr nodes to re-use them in other elements.
 * Returns: If the new Node replaces an existing node the replaced Node is
 * returned, otherwise null is returned.
 * %GDOME_HIERARCHY_REQUEST_ERR: Raised if an attempt is made to add a node
 * doesn't belong in this NamedNodeMap. Examples would include trying to
 * insert something other than an Attr node into an Element's map of
 * attributes, or a non-Entity node into the DocumentType's map of Entities.
 */
GdomeNode *
gdome_xml_nnm_setNamedItemNS (GdomeNamedNodeMap *self, GdomeNode *arg, GdomeException *exc) {
	Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;
	xmlNode *newn = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (arg != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_N (arg), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (((Gdome_xml_Document *)priv->doc)->n != ((Gdome_xml_Node*)arg)->n->doc) {
		*exc = GDOME_WRONG_DOCUMENT_ERR;
    return NULL;
	}
	if (priv->accessType == GDOME_READONLY_NODE) {
		*exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
		return NULL;
	}
  if (priv->type != gdome_xml_n_nodeType (arg, exc)) {
    *exc = GDOME_HIERARCHY_REQUEST_ERR;
    return NULL;
  }

	newn = ((Gdome_xml_Node*)arg)->n;

  /* Hash Table */
	if(priv->type == GDOME_ENTITY_NODE || priv->type == GDOME_NOTATION_NODE) {
		xmlHashTable *ht = priv->data.ht;
    xmlNode *ret = NULL;

		if((ret = xmlHashLookup2(ht, gdome_xmlGetName(newn), gdome_xmlGetNsURI(newn))) != NULL) {
			xmlHashUpdateEntry2(ht, gdome_xmlGetName(newn), gdome_xmlGetNsURI(newn), newn, NULL);
      return (GdomeNode *) gdome_xml_n_mkref(ret);
		} else {
			xmlHashAddEntry2(ht, gdome_xmlGetName(newn), gdome_xmlGetNsURI(newn), newn);
			return NULL;
		}

	}
  /* List */
	else if (priv->type == GDOME_ATTRIBUTE_NODE) {
    GdomeNode *parent = gdome_xml_n_mkref (((Gdome_xml_Element *)priv->elem)->n);
    Gdome_xml_Node *parent_priv = (Gdome_xml_Node *)parent;
    GdomeNode *ret = NULL;
    GdomeMutationEvent *mev;
    GdomeDOMString *attrName;

    if(newn->prev || newn->next ||
       (gdome_xmlGetParent(newn) && gdome_xmlGetParent(newn) != parent_priv->n)) {
      *exc = GDOME_INUSE_ATTRIBUTE_ERR;
      gdome_xml_n_unref (parent, exc);
			return NULL;
		}

    ret = gdome_xml_n_mkref ((xmlNode *)gdome_xmlUnlinkNsAttr(parent_priv->n,
                                                              gdome_xmlGetNsURI(newn),
                                                              gdome_xmlGetName(newn)));
    if (ret != NULL) {
			if (gdome_xml_nnm_eventEnabledByCode (self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
				/* Fire DOMAttrModified */
				mev = gdome_evt_mevnt_mkref ();
				attrName = gdome_xml_n_nodeName (ret, exc);
				gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																								 TRUE, FALSE, ret,
																								 NULL, NULL, attrName, GDOME_REMOVAL, exc);
				gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
				gdome_xml_str_unref (attrName);
				gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
			}
    }

    gdome_xmlLinkAttr(parent_priv->n, (xmlAttr *)newn);

		if (gdome_xml_nnm_eventEnabledByCode (self, DOM_ATTR_MODIFIED_EVENT_TYPE)) {
			/* Fire DOMAttrModified */
			mev = gdome_evt_mevnt_mkref ();
			gdome_xml_n_ref (arg, exc);
			attrName = gdome_xml_n_nodeName (arg, exc);
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_ATTR_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, arg,
																							 NULL, NULL, attrName, GDOME_ADDITION, exc);
			gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
			gdome_xml_str_unref (attrName);
			gdome_xml_n_unref (arg, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}

    /* resync structure */
    priv->data.lst = (xmlNode *)gdome_xmlGetAttrList(parent_priv->n);

		if (gdome_xml_nnm_eventEnabledByCode (self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
			/* Fire DOMSubtreeModified */
			mev = gdome_evt_mevnt_mkref ();
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, NULL,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}

    gdome_xml_n_unref (parent, exc);
    return ret;
  }
  return NULL;
}

GdomeBoolean
gdome_xml_nnm_eventEnabledByCode (GdomeNamedNodeMap *self, guint32 code)
{
	Gdome_xml_NamedNodeMap *priv = (Gdome_xml_NamedNodeMap *)self;

	g_return_val_if_fail (priv != NULL, FALSE);

	return gdome_xml_doc_eventEnabledByCode (priv->doc, code);
}
