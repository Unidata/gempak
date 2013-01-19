/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-documentt.c
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gdome.h"
#include "gdome-refdebug.h"
#include "gdome-treegc.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include <libxml/hash.h>
#include <libxml/xmlIO.h>
#include "gdome-xml-util.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-xml-xmldtdutil.h"
#include "gdome-xml-str.h"
#include "gdome-xml-node.h"
#include "gdome-xml-documentt.h"
#include "gdome-xml-nnodem.h"

const GdomeDocumentTypeVtab gdome_xml_dt_vtab = {
  {
		gdome_xml_n_ref,
		gdome_xml_dt_unref,
		gdome_xml_dt_query_interface,
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
	gdome_xml_dt_name,
	gdome_xml_dt_entities,
	gdome_xml_dt_notations,
	gdome_xml_dt_publicId,
	gdome_xml_dt_systemId,
	gdome_xml_dt_internalSubset
};

/**
 * gdome_xml_dt_mkref:
 * @n:  The libxml dtd node of which a Gdome reference is wanted
 *
 * Constructor of the DocumentType structure.
 * Returns: the reference to the Gdome DocumentType wanted.
 */
GdomeDocumentType *
gdome_xml_dt_mkref (xmlDtd *n) {
	Gdome_xml_DocumentType *result = NULL;

	if (n == NULL)
		return NULL;
	if (n->_private) {
		result = (Gdome_xml_DocumentType *)n->_private;
		result->refcnt++;
	} else if (n->type == XML_DOCUMENT_TYPE_NODE || n->type == XML_DTD_NODE) {
		result = g_new (Gdome_xml_DocumentType, 1);
		result->refcnt = 1;
		result->vtab = &gdome_xml_dt_vtab;
		result->super.user_data = NULL;
		result->n = n;
		result->accessType = GDOME_READONLY_NODE;
		result->ll = NULL;
		n->_private = result;

		/* Notations init */
		result->notations = gdome_xmlNotationsHashCreate (n->doc);

		/* Entities init */
		result->entities = gdome_xmlEntitiesHashCreate (n->doc);

#ifdef DEBUG_REFCNT
		gdome_refdbg_addRef ((void *)result, GDOME_REFDBG_NODE);
#endif
		if(n->doc)
			gdome_treegc_addNode ((GdomeNode *)result);

	} else {
		g_warning ("gdome_xml_dt_mkref: invalid node type");
		result = NULL;
	}

	return (GdomeDocumentType *)result;
}

/**
 * gdome_xml_dt_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_xml_dt_unref (GdomeNode *self, GdomeException *exc) {
	Gdome_xml_DocumentType *priv = (Gdome_xml_DocumentType *)self;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_DT (priv));
	g_return_if_fail (exc != NULL);

	if (priv->refcnt > 0)
		priv->refcnt--;

	if (priv->refcnt == 0 && priv->ll == NULL) {
		xmlNode *n = (xmlNode *)priv->n;

		n->_private = NULL;

		/* Free Notations */
		if (priv->notations != NULL)
			gdome_xmlNotationsHashFree (priv->notations);

		/* Free Entities */
		if (priv->entities != NULL)
			gdome_xmlEntitiesHashFree (priv->entities);

		if(n->doc)
			gdome_treegc_delNode (self);
		else /* The node is not linked to a document */
			xmlFreeDtd ((xmlDtd *)n);
#ifdef DEBUG_REFCNT
		gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
		g_free (self);
	}
}

/**
 * gdome_xml_dt_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_dt_query_interface (GdomeNode *self, const char *interface, GdomeException *exc)
{
	Gdome_xml_DocumentType *priv = (Gdome_xml_DocumentType *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DT (priv), NULL);
	g_return_val_if_fail (interface != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	if (!strcmp (interface, "Node") ||
			!strcmp (interface, "DocumentType") ||
			!strcmp (interface, "EventTarget")) {
		priv->refcnt++;
		return self;
	}
	else
		return NULL;
}

/**
 * gdome_xml_dt_name:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of DTD; i.e., the name immediately following the %DOCTYPE
 * keyword.
 */
GdomeDOMString *
gdome_xml_dt_name (GdomeDocumentType *self, GdomeException *exc)
{
	Gdome_xml_DocumentType *priv = (Gdome_xml_DocumentType *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DT (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	return gdome_xml_str_mkref_dup((gchar *)gdome_xmlGetName ((xmlNode *)priv->n));
}

/**
 * gdome_xml_dt_entities:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the general entities, both external and
 * internal, declared in the DTD. Parameter entities are not contained.
 * Duplicates are discarded.
 */
GdomeNamedNodeMap *
gdome_xml_dt_entities (GdomeDocumentType *self, GdomeException *exc)
{
	Gdome_xml_DocumentType *priv = (Gdome_xml_DocumentType *)self;
	xmlDtd *dt;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DT (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	dt = priv->n;
	return gdome_xml_nnm_mkref ((GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)gdome_xmlGetOwner ((xmlNode *)dt)),
															NULL, priv->entities, NULL, GDOME_READONLY_NODE, GDOME_ENTITY_NODE);
}

/**
 * gdome_xml_dt_notations:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the notations declared in the DTD.
 * Duplicates are discarded. Every node in this map also implements the
 * Notation interface.
 */
GdomeNamedNodeMap *
gdome_xml_dt_notations (GdomeDocumentType *self, GdomeException *exc)
{
	Gdome_xml_DocumentType *priv = (Gdome_xml_DocumentType *)self;
	xmlDtd *dt;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DT (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	dt = priv->n;
	return gdome_xml_nnm_mkref ((GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)gdome_xmlGetOwner ((xmlNode *)dt)),
															NULL, priv->notations, NULL, GDOME_READONLY_NODE, GDOME_NOTATION_NODE);
}

/**
 * gdome_xml_dt_publicId:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the public identifier of the external subset.
 */
GdomeDOMString *
gdome_xml_dt_publicId (GdomeDocumentType *self, GdomeException *exc)
{
	Gdome_xml_DocumentType *priv = (Gdome_xml_DocumentType *)self;
	xmlDtd *dt;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DT (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	dt = priv->n;
	return gdome_xml_str_mkref_dup ((gchar *)dt->ExternalID);
}

/**
 * gdome_xml_dt_systemId:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the public identifier of the external subset.
 */
GdomeDOMString *
gdome_xml_dt_systemId (GdomeDocumentType *self, GdomeException *exc)
{
	Gdome_xml_DocumentType *priv = (Gdome_xml_DocumentType *)self;
	xmlDtd *dt;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DT (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	dt = priv->n;
	return gdome_xml_str_mkref_dup ((gchar *)dt->SystemID);
}

/**
 * gdome_xml_dt_internalSubset:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: The internal subset as a string, or %NULL if there is none or on
 * error.
 */
GdomeDOMString *
gdome_xml_dt_internalSubset (GdomeDocumentType *self, GdomeException *exc)
{
	Gdome_xml_DocumentType *priv = (Gdome_xml_DocumentType *)self;
	xmlOutputBuffer *out_buff = NULL;
	gchar *ret;
	xmlDtd *is;

	g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_DT (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	g_assert (priv->n->doc != NULL);
	g_assert (priv->n->doc->intSubset != NULL);
	is = priv->n->doc->intSubset;

	if ((out_buff = xmlAllocOutputBuffer(NULL)) == NULL ) {
		return NULL;
	}
	xmlNodeDumpOutput (out_buff, NULL, (xmlNode *)is, 0, 0, NULL);
	xmlOutputBufferFlush(out_buff);
	ret = g_strndup (out_buff->buffer->content, out_buff->buffer->use);
	(void)xmlOutputBufferClose(out_buff);

	return gdome_xml_str_mkref_own (ret);
}
