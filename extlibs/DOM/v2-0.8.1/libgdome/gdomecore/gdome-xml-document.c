/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-document.c
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

#include <stdlib.h>
#include <string.h>
#include "gdome.h"
#include "gdome-events.h"
#include "gdome-treegc.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include <libxml/hash.h>
#include "gdome-xml-domimpl.h"
#include "gdome-xml-util.h"
#include "gdome-xml-str.h"
#include "gdome-xml-node.h"
#include "gdome-xml-nodel.h"
#include "gdome-xml-documentt.h"
#include "gdome-evt-event.h"
#include "gdome-evt-mevent.h"
#include "gdome-xml-document.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-xml-element.h"


typedef struct _idsIterator idsIterator;
struct _idsIterator {
	xmlChar *elementId;
	xmlNode *element;
};

/* ****************************************************************/
/* ****************  The Document Virtual Table  **************** */
/* ****************************************************************/

const GdomeDocumentVtab gdome_xml_doc_vtab = 
{
	{
		gdome_xml_n_ref,
		gdome_xml_doc_unref,
		gdome_xml_doc_query_interface,
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
	gdome_xml_doc_doctype,
	gdome_xml_doc_implementation,
	gdome_xml_doc_documentElement,
	gdome_xml_doc_createElement,
	gdome_xml_doc_createDocumentFragment,
	gdome_xml_doc_createTextNode,
	gdome_xml_doc_createComment,
	gdome_xml_doc_createCDATASection,
	gdome_xml_doc_createProcessingInstruction,
	gdome_xml_doc_createAttribute,
	gdome_xml_doc_createEntityReference,
	gdome_xml_doc_getElementsByTagName,
	gdome_xml_doc_importNode,
	gdome_xml_doc_createElementNS,
	gdome_xml_doc_createAttributeNS,
	gdome_xml_doc_getElementsByTagNameNS,
	gdome_xml_doc_getElementById,
	gdome_xml_doc_createEvent
};

/**
 * gdome_xml_doc_mkref:
 * @n:  The libxml document node of which a Gdome reference is wanted
 *
 * Constructor of the Document structure.
 * Returns: the reference to the Gdome Document wanted.
 */
GdomeDocument *
gdome_xml_doc_mkref (xmlDoc *n) {
	Gdome_xml_Document *result = NULL;

	if (n == NULL)
		return NULL;
	if (n->_private) {
		result = (Gdome_xml_Document *)n->_private;

		/* workaround since doc_unref decreases livenodes if refcnt==0*/
		if (result->refcnt == 0)
			result->livenodes++;

		result->refcnt++;
	} else if (n->type == XML_DOCUMENT_NODE ||
						 n->type == XML_HTML_DOCUMENT_NODE ) {
		result = g_new (Gdome_xml_Document, 1);
		result->refcnt = 1;
		result->vtab = &gdome_xml_doc_vtab;
		result->super.user_data = NULL;
		result->n = n;
		result->accessType = GDOME_READWRITE_NODE;
		result->ll = NULL;
		result->livenodes = 0;
		/* By default all kind of events can be fired */
		result->event_mask = DOM_ALL_EVENT_TYPES;
		/* result->event_mask = 0; */
		n->_private = result;

#ifdef DEBUG_REFCNT
		gdome_refdbg_addRef ((void *)result, GDOME_REFDBG_NODE);
#endif
		gdome_treegc_addNode ((GdomeNode *)result);
	} else {
		g_warning ("gdome_xml_doc_mkref: invalid node type");
		result = NULL;
	}

	return (GdomeDocument *)result;
}

/**
 * gdome_xml_doc_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Document Node. Free the Node
 * structure if the Node will have zero reference.
 */
void
gdome_xml_doc_unref (GdomeNode *self, GdomeException *exc) {
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_DOC (priv));
	g_return_if_fail (exc != NULL);

	if (priv->refcnt > 0)
		priv->refcnt--;

	if (priv->refcnt == 0 && priv->ll == NULL)
		gdome_treegc_delNode (self);
}

/***************************
 * Implementation of the
 * Document Interface
 ***************************/

/**
 * gdome_xml_doc_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_doc_query_interface (GdomeNode *self, const char *interface, GdomeException *exc)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (interface != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	if (!strcmp (interface, "Node") ||
			!strcmp (interface, "Document") ||
			!strcmp (interface, "DocumentEvent") ||
			!strcmp (interface, "EventTarget")) {
		priv->refcnt++;
		return self;
	}
	else
		return NULL;
}

/**
 * gdome_xml_doc_doctype:
 * @self:  Document Object ref
 * @exc:  Exception Object ref
 *
 * Returns: The Document Type Declaration associated with this document. The
 * DOM Level 2 does not support editing the Document Type Declaration. docType
 * cannot be altered in any way, including through the use of methods inherited
 * from the Node interface, such as insertNode or removeNode.
 */
GdomeDocumentType *
gdome_xml_doc_doctype (GdomeDocument *self, GdomeException *exc)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	xmlDoc *doc = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	doc = priv->n;
	if (doc->intSubset != NULL)
		return (GdomeDocumentType *) gdome_xml_dt_mkref (doc->intSubset);
	else
		return NULL;
}

/**
 * gdome_xml_doc_implementation:
 * @self:  Document Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the DOMImplementation object that handles this document.
 */
GdomeDOMImplementation *
gdome_xml_doc_implementation (GdomeDocument *self, GdomeException *exc)
{
	g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (self), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	/* Always return the global DOMImplementation we have */		
	return  gdome_xml_di_mkref ();
}

/**
 * gdome_xml_doc_documentElement:
 * @self:  Document Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the root Element of the Document.
 */
GdomeElement *
gdome_xml_doc_documentElement (GdomeDocument *self, GdomeException *exc)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	xmlNode *result;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	result = xmlDocGetRootElement (priv->n);

	return (GdomeElement *)gdome_xml_n_mkref (result);
}

/**
 * gdome_xml_doc_createElement:
 * @self:  Document Object ref
 * @tagName:  The name of the element type to instantiate.
 * @exc:  Exception Object ref
 *
 * Creates an element of the type specified. Note that the instance returned
 * implements the Element interface, so attributes can be specified directly on
 * the returned object.
 * To create an element with a qualified name and namespace URI, use the
 * gdome_doc_createElementNS() function.
 * Returns: a new Element object with the nodeName attribute set to tagName,
 * and localName, prefix, and namespaceURI set to %NULL.
 */
GdomeElement *
gdome_xml_doc_createElement (GdomeDocument *self, GdomeDOMString *tagName, GdomeException *exc)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	xmlNode *result = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (tagName != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	result = xmlNewDocNode (priv->n, NULL, tagName->str, NULL);

	return (GdomeElement *)gdome_xml_n_mkref (result);
}

/**
 * gdome_xml_doc_createElementNS:
 * @self:  Document Object ref
 * @namespaceURI:  The namespace URI of the element to create
 * @qualifiedName:  The qualified name of the element to create
 * @exc:  Exception Object ref
 *
 * Creates an element of the given qualified name and namespace URI.
 *
 * %GDOME_NAMESPACE_ERR: Raised if the @qualifiedName is malformed, if the
 * @qualifiedName has a prefix and the @namespaceURI is %NULL, or if the
 * @qualifiedName has a prefix that is "xml" and the @namespaceURI is different
 * from "http://www.w3.org/XML/1998/namespace".
 * Returns: a new Element object with the following attributes:
 * %Node.nodeName = @qualifiedName,
 * %Node.namespaceURI = @namespaceURI,
 * %Node.prefix = prefix, extracted from @qualifiedName,
 * %Node.localName = localName, extracted from @qualifiedName,
 * %Element.tagName = @qualifiedName.
 */
GdomeElement *
gdome_xml_doc_createElementNS (GdomeDocument *self, GdomeDOMString *namespaceURI,
                               GdomeDOMString *qualifiedName, GdomeException *exc) {
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	gchar **strs;
	gchar *prefix = NULL;
	gchar *localName = NULL;
	xmlNs *ns = NULL;
	xmlNode *result = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (qualifiedName != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	/* test malformed qualifiedName */
	strs = g_strsplit((gchar *)qualifiedName->str, ":", 0);
	if (strs[0] && strs[1] && !strs[2]) {            /* there's one ':' */
		if (namespaceURI == NULL) {
			*exc = GDOME_NAMESPACE_ERR;
		} else {
			prefix = g_strdup(strs[0]);
			localName = g_strdup(strs[1]);
			if ((!strcmp (prefix, "xml") && strcmp(namespaceURI->str, "http://www.w3.org/XML/1998/namespace"))) {
				*exc = GDOME_NAMESPACE_ERR;
			}
		}
	} else if (!strs[1] && namespaceURI != NULL) {   /* there's no ':' */
		localName = g_strdup(strs[0]);
	} else if (strs[0] && strs[1] && strs[2]) {      /* there are more than one ':' */
		*exc = GDOME_NAMESPACE_ERR;
	}
	g_strfreev(strs);

	if (!*exc) {
		if (localName != NULL) {
			ns = gdome_xmlNewNs (priv->n, namespaceURI->str, prefix);
			result = xmlNewDocNode (priv->n, ns, (xmlChar *)localName, NULL);
			if (prefix != NULL)
				g_free (prefix);
			g_free (localName);
		} else
			result = xmlNewDocNode (priv->n, NULL, (xmlChar *)qualifiedName->str, NULL);

		return (GdomeElement *)gdome_xml_n_mkref (result);
	} else {
		if (localName != NULL) {
			g_free (prefix);
			g_free (localName);
		}

		return NULL;
	}
}

/**
 * gdome_xml_doc_createDocumentFragment:
 * @self:  Document Object ref
 * @exc:  Exception Object ref
 *
 * Creates an empty DocumentFragment object. 
 * Returns: the new DocumentFragment object.
 */
GdomeDocumentFragment *
gdome_xml_doc_createDocumentFragment (GdomeDocument *self, GdomeException *exc)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	xmlNode *result = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	result = xmlNewDocFragment (priv->n);

	return (GdomeDocumentFragment *)gdome_xml_n_mkref (result);
}

/**
 * gdome_xml_doc_createTextNode:
 * @self:  Document Object ref
 * @data:  The data for the node
 * @exc:  Exception Object ref
 *
 * Creates a Text node given the specified string.
 * Returns: The new TextNode object.
 */
GdomeText *
gdome_xml_doc_createTextNode (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	xmlNode *result = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (data != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	result = xmlNewDocText (priv->n, data->str);

	return (GdomeText *)gdome_xml_n_mkref (result);
}

/**
 * gdome_xml_doc_createComment:
 * @self:  Document Object ref
 * @data:  The data for the comment contents
 * @exc:  Exception Object ref
 *
 * Creates a Comment node whose value is the specified string.
 * Returns: the new Comment object.
 */
GdomeComment *
gdome_xml_doc_createComment (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	xmlNode *result = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (data != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	result = xmlNewComment (data->str);
	gdome_xmlSetOwner (result, priv->n);

	return (GdomeComment *)gdome_xml_n_mkref (result);
}

/**
 * gdome_xml_doc_createCDATASection:
 * @self:  Document Object ref
 * @data:  The data for the CDATASection contents
 * @exc:  Exception Object ref
 *
 * Creates a CDATASection node whose value is the specified string.
 * Returns: the new CDATASection object.
 */
GdomeCDATASection *
gdome_xml_doc_createCDATASection (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	xmlNode *result = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (data != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	result = xmlNewCDataBlock (priv->n, data->str, strlen (data->str));
	gdome_xmlSetOwner (result, priv->n);

	return (GdomeCDATASection *)gdome_xml_n_mkref (result);
}

/**
 * gdome_xml_doc_createProcessingInstruction:
 * @self:  Document Object ref
 * @target:  The target part of the processing instruction
 * @data:  The data for the node
 * @exc:  Exception Object ref
 *
 * Creates a ProcessingInstruction node given the specified name and data
 * strings.
 * Returns: The new ProcessingInstruction object.
 */
GdomeProcessingInstruction *
gdome_xml_doc_createProcessingInstruction (GdomeDocument *self, GdomeDOMString *target, GdomeDOMString *data, GdomeException *exc)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	xmlNode *result;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (target != NULL, NULL);
	g_return_val_if_fail (data != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	result = xmlNewPI (target->str, data->str);
	gdome_xmlSetOwner (result, priv->n);

	return (GdomeProcessingInstruction *)gdome_xml_n_mkref (result);
}

/**
 * gdome_xml_doc_createAttribute:
 * @self:  Document Object ref
 * @name:  The name of the attribute
 * @exc:  Exception Object ref
 *
 * Creates an Attr of the given @name. Note that the Attr instance can then be
 * set on an Element using the setAttributeNode method. To create an attribute
 * with a qualified name and namespace URI, use the
 * gdome_doc_createAttributeNS() method.
 * Returns: a new Attr object with the nodeName attribute set to @name, and
 * localName, prefix, and namespaceURI set to %NULL. The value of the attribute
 * is the empty string.
 */
GdomeAttr *
gdome_xml_doc_createAttribute (GdomeDocument *self, GdomeDOMString *name, GdomeException *exc)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	xmlAttr *result = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (name != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	result = xmlNewDocProp (priv->n, name->str, NULL);

	return (GdomeAttr *)gdome_xml_n_mkref ((xmlNode *)result);
}

/**
 * gdome_xml_doc_createAttributeNS:
 * @self:  Document Object ref
 * @namespaceURI:  The namespace URI of the attribute to create
 * @qualifiedName:  The qualified name of the attribute to create
 * @exc:  Exception Object ref
 *
 * Creates an attribute of the given qualified name and namespace URI.
 *
 * %GDOME_NAMESPACE_ERR: Raised if the @qualifiedName is malformed, if the
 * @qualifiedName has a prefix and the @namespaceURI is %NUKK, if the
 * @qualifiedName has a prefix that is "xml" and the @namespaceURI is different
 * from "http://www.w3.org/XML/1998/namespace", or if the @qualifiedName is
 * "xmlns" and the @namespaceURI is different from "http://www.w3.org/2000/xmlns/".
 * Returns: a new Attr object with the following attributes:
 * %Node.nodeName = @qualifiedName,
 * %Node.namespaceURI = @namespaceURI,
 * %Node.prefix = prefix, extracted from @qualifiedName,
 * %Node.localName = localName, extracted from @qualifiedName,
 * %Attr.name = @qualifiedName,
 * %Node.nodeValue = the empty string.
 */
GdomeAttr *
gdome_xml_doc_createAttributeNS (GdomeDocument *self, GdomeDOMString *namespaceURI,
                                 GdomeDOMString *qualifiedName, GdomeException *exc) {
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	gchar **strs;
	gchar *prefix = NULL;
	gchar *localName = NULL;
	xmlNs *ns = NULL;
	xmlAttr *result = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (qualifiedName != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

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
		if (localName != NULL) {
			result = xmlNewDocProp (priv->n, (xmlChar *)localName, NULL);
			ns = gdome_xmlNewNs (priv->n, namespaceURI->str, prefix);
			xmlSetNs((xmlNode *)result, ns);
			g_free (prefix);
			g_free (localName);
		} else
			result = xmlNewDocProp (priv->n, (xmlChar *)qualifiedName->str, NULL);

		return (GdomeAttr *)gdome_xml_n_mkref ((xmlNode *)result);
	} else {
		if (localName != NULL) {
			g_free (prefix);
			g_free (localName);
		}

		return NULL;
	}
}

/**
 * gdome_xml_doc_createEntityReference:
 * @self:  Document Object ref
 * @name:  The name of the entity to reference
 * @exc:  Exception Object ref
 *
 * Creates an EntityReference object. In addition, if the referenced entity is
 * known, the child list of the EntityReference node is made the same as that
 * of the corresponding Entity node.
 * Returns: the new EntityReference object.
 */
GdomeEntityReference *
gdome_xml_doc_createEntityReference (GdomeDocument *self, GdomeDOMString *name, GdomeException *exc)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	xmlNode *result;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (name != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	result = xmlNewReference (priv->n, name->str);

	return (GdomeEntityReference *)gdome_xml_n_mkref (result);
}

/* Hash Scanner function for gdome_xml_doc_getElementById */
void
idsHashScanner (void *payload, void *data, xmlChar *name) {
	idsIterator *priv = (idsIterator *)data;

	if (priv->element == NULL && xmlStrEqual (name, priv->elementId))
		priv->element = (xmlNode *)gdome_xmlGetParent ((xmlNode *)((xmlID *)payload)->attr);
}

/**
 * gdome_xml_doc_getElementById:
 * @self:  Document Object ref
 * @elementId:  The unique id value for an element
 * @exc:  Exception Object ref
 *
 * Returns: the Element whose %ID is given by @elementId. If no such element
 * exists, returns %NULL. Behavior is not defined if more than one element has
 * this %ID.
 */
GdomeElement *
gdome_xml_doc_getElementById (GdomeDocument *self, GdomeDOMString *elementId, GdomeException *exc) {
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	xmlHashTable *ids = NULL;
	idsIterator iter;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (elementId != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	ids = (xmlHashTable *)priv->n->ids;

	iter.elementId = (xmlChar *)elementId->str;
	iter.element = NULL;
	xmlHashScan(ids, idsHashScanner, &iter);

	return (GdomeElement *)gdome_xml_n_mkref (iter.element);
}

/**
 * gdome_xml_doc_getElementsByTagName:
 * @self:  Document Object ref
 * @tagName:  The name of the tag to match on. The special value * matches
 *            all tags.
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList of all the elements with a given tag name in the order
 * in which they are encountered in a preorder traversal of the Document tree.
 */
GdomeNodeList *
gdome_xml_doc_getElementsByTagName (GdomeDocument *self, GdomeDOMString *tagname, GdomeException *exc) {
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (tagname != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	return gdome_xml_nl_mkref ((GdomeNode*)self, tagname, NULL, GDOME_READWRITE_NODE);
}

/**
 * gdome_xml_doc_getElementsByTagNameNS:
 * @self:  Document Objects ref
 * @namespaceURI:  The namespace URI of the elements to match on. The special
 *                 value * matches all namespaces.
 * @localName:  The local name of the elements to match on. The special value
 *              * matches all local names.
 * @exc:  Document Object ref
 *
 * Returns: a NodeList of all the descendant elements with a given local name
 * and namespace URI in the order in which they are encountered in a preorder
 * traversal of this Document tree.
 */
GdomeNodeList *
gdome_xml_doc_getElementsByTagNameNS (GdomeDocument *self, GdomeDOMString *namespaceURI,
                                      GdomeDOMString *localName, GdomeException *exc) {
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (namespaceURI != NULL, NULL);
	g_return_val_if_fail (localName != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	return gdome_xml_nl_mkref ((GdomeNode*)self, localName, namespaceURI, GDOME_READWRITE_NODE);
}

/**
 * gdome_xml_doc_importNode:
 * @self:  Document Objects ref
 * @importedNode:  The node to import.
 * @deep:  If %TRUE, recursively import the subtree under the specified node;
 *         if %FALSE, import only the node itself. This has no effect on Attr,
 *         EntityReference, and Notation nodes.
 * @exc:  Exception Object ref
 *
 *
 * Imports a node from another document to this document. The returned node has
 * no parent; (parentNode is %NULL). The source node is not altered or removed
 * from the original document; this method creates a new copy of the source
 * node. %GDOME_DOCUMENT_NODE, %GDOME_DOCUMENT_TYPE_NODE, %GDOME_NOTATION_NODE
 * and %GDOME_ENTITY_NODE nodes are not supported.
 *
 * %GDOME_NOT_SUPPORTED_ERR: Raised if the type of node being imported is not
 * supported.
 * Returns: the imported node that belongs to this Document.
 */
GdomeNode *
gdome_xml_doc_importNode (GdomeDocument *self, GdomeNode *importedNode, GdomeBoolean deep, GdomeException *exc) {
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;
	Gdome_xml_Node *priv_node = (Gdome_xml_Node *)importedNode;
	xmlNode *ret = NULL;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (importedNode != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	switch (gdome_xml_n_nodeType (importedNode, exc)) {
	case XML_ATTRIBUTE_NODE:
		g_assert (gdome_xmlGetOwner ((xmlNode *)priv->n) == priv->n);
		ret = (xmlNode *)xmlCopyProp ((xmlNode *)priv->n, (xmlAttr *)priv_node->n);
		gdome_xmlSetParent (ret, NULL);
		break;
	case XML_DOCUMENT_FRAG_NODE:
	case XML_ELEMENT_NODE:
	case XML_ENTITY_REF_NODE:
	case XML_PI_NODE:
	case XML_TEXT_NODE:
	case XML_CDATA_SECTION_NODE:
	case XML_COMMENT_NODE:
		ret = xmlCopyNode (priv_node->n, deep);
		xmlSetTreeDoc (ret, priv->n);
		break;
	default:
		*exc = GDOME_NOT_SUPPORTED_ERR;
	}

	return gdome_xml_n_mkref (ret);
}

/**
 * gdome_xml_doc_createEvent:
 * @self:  Document Objects ref
 * @eventType:  The @eventType parameter specifies the type of Event interface
 *              to be created.
 * @exc:  Exception Object ref
 *
 * If the Event interface specified is supported by the implementation this
 * method will return a new Event of the interface type requested. If the Event
 * is to be dispatched via the dispatchEvent method the appropriate event init
 * method must be called after creation in order to initialize the Event's
 * values. As an example, a user wishing to synthesize some kind of
 * MutationEvents would call createEvent with the parameter "MutationEvents".
 * The gdome_mevnt_initMuatationEvent() method could then be called on the
 * newly created MutationEvent to set the specific type of MutationEvent to be
 * dispatched and set its context information.
 *
 * GDOME_NOT_SUPPORTED_ERR: Raised if the implementation does not support the
 * type of Event interface requested
 * Returns: the newly created Event.
 */
GdomeEvent *
gdome_xml_doc_createEvent (GdomeDocument *self, GdomeDOMString *eventType, GdomeException *exc) {
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), NULL);
	g_return_val_if_fail (eventType != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	if (!strcmp (eventType->str, "MutationEvents"))
		return (GdomeEvent *)gdome_evt_mevnt_mkref ();
	else if (!strcmp (eventType->str, "Events"))
		return gdome_evt_evnt_mkref ();
	else {
		*exc = GDOME_NOT_SUPPORTED_ERR;
		return NULL;
	}
}

void
gdome_xml_doc_enableEventByName (GdomeDocument *self, const char *name)
{
	gdome_xml_doc_enableEventByCode (self, gdome_evt_evnt_codeOfName (name));
}

void
gdome_xml_doc_enableEventByCode (GdomeDocument *self, guint32 code)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_DOC (priv));

	priv->event_mask |= code;
}

void
gdome_xml_doc_disableEventByName (GdomeDocument *self, const char *name)
{
	gdome_xml_doc_disableEventByCode (self, gdome_evt_evnt_codeOfName (name));
}

void
gdome_xml_doc_disableEventByCode (GdomeDocument *self, guint32 code)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_DOC (priv));

	priv->event_mask &= ~code;
}

/**
 * gdome_xml_doc_event_is_enabled:
 * @self:  Document Object ref
 * @mask:  The event type to check
 *
 * Returns: TRUE if the specified event type is enabled
 */
GdomeBoolean
gdome_xml_doc_eventEnabledByName (GdomeDocument *self, const char *name)
{
	return gdome_xml_doc_eventEnabledByCode (self, gdome_evt_evnt_codeOfName (name));
}

GdomeBoolean
gdome_xml_doc_eventEnabledByCode (GdomeDocument *self, guint32 code)
{
	Gdome_xml_Document *priv = (Gdome_xml_Document *)self;

	g_return_val_if_fail (priv != NULL, FALSE);
	g_return_val_if_fail (GDOME_XML_IS_DOC (priv), FALSE);

	return (priv->event_mask & code) == code;
}
