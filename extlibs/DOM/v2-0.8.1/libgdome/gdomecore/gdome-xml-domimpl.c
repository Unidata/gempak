/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-domimpl.c
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

#include <string.h>
#include <glib.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/parserInternals.h>
#include <libxml/hash.h>
#include <libxml/entities.h>
#include "gdome.h"
#include "gdome-treegc.h"
#include "gdome-xml-node.h"
#include "gdome-xml-document.h"
#include "gdome-xml-documentt.h"
#include "gdome-xml-util.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-xml-domimpl.h"

Gdome_xml_DOMImplementation *gdome_xml_DOMImplementation = NULL;

/* vtable for the DOMImplementation */
const GdomeDOMImplementationVtab gdome_xml_di_vtab = {
	gdome_xml_di_ref,
	gdome_xml_di_unref,
	gdome_xml_di_query_interface,
	gdome_xml_di_hasFeature,
	gdome_xml_di_createDocumentType,
	gdome_xml_di_createDocument,
	gdome_xml_di_freeDoc,
	gdome_xml_di_createDocFromURI,
	gdome_xml_di_createDocFromMemory,
	gdome_xml_di_createDocFromURIWithEntitiesTable,
	gdome_xml_di_createDocFromMemoryWithEntitiesTable,
	gdome_xml_di_saveDocToFile,
  gdome_xml_di_saveDocToMemory,
  gdome_xml_di_saveDocToFileEnc,
  gdome_xml_di_saveDocToMemoryEnc,
	gdome_xml_di_enableEvent,
	gdome_xml_di_disableEvent,
	gdome_xml_di_eventIsEnabled
};

/**
 * gdome_xml_di_mkref:
 *
 * Constructor of the DOMImplementation interface.
 * Returns: the reference to the DOMImplementation structure.
 */
GdomeDOMImplementation *
gdome_xml_di_mkref (void) {
	if (gdome_xml_DOMImplementation == NULL) {
		gdome_xml_DOMImplementation = g_new (Gdome_xml_DOMImplementation, 1);
		memset(gdome_xml_DOMImplementation, 0, sizeof(Gdome_xml_DOMImplementation));
		gdome_xml_DOMImplementation->refcnt = 1;
		gdome_xml_DOMImplementation->vtab = &gdome_xml_di_vtab;
	} else
		gdome_xml_DOMImplementation->refcnt++;

	return (GdomeDOMImplementation *)gdome_xml_DOMImplementation;
}

/**
 * gdome_xml_di_ref:
 * @self:  DOMImplementation Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the DOMImplementation structure.
 */
void
gdome_xml_di_ref (GdomeDOMImplementation *self,
                  GdomeException *exc) {
	Gdome_xml_DOMImplementation *priv = (Gdome_xml_DOMImplementation *)self;

	g_return_if_fail (self != NULL);
	g_return_if_fail (exc != NULL);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	priv->refcnt++;
}

/**
 * gdome_xml_di_unref:
 * @self:  DOMImplementation Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the DOMImplementation. Free the structure
 * if DOMImplementation will have zero reference.
 */
void
gdome_xml_di_unref (GdomeDOMImplementation *self,
                    GdomeException *exc) {
	Gdome_xml_DOMImplementation *priv = (Gdome_xml_DOMImplementation *)self;
	
	g_return_if_fail (self != NULL);
	g_return_if_fail (exc != NULL);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	g_assert (priv->refcnt > 0);
	priv->refcnt--;

	if (priv->refcnt == 0) {
		g_free (self);
		gdome_xml_DOMImplementation = NULL;
	}
}

/**
 * gdome_xml_di_query_interface:
 * @self:  DOMImplementation Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_di_query_interface (GdomeDOMImplementation *self, const char *interface, GdomeException *exc)
{
	Gdome_xml_DOMImplementation *priv = (Gdome_xml_DOMImplementation *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (interface != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	if (!strcmp (interface, "DOMImplementation")) {
		priv->refcnt++;
		return self;
	}
	else
		return NULL;
}

/**
 * gdome_xml_di_hasFeature:
 * @self:  The DOMImplementation Object ref
 * @feature:  The name of the feature to test (case-insensitive).
 * @version:  This is the version number of the feature to test. In Level 2,
 *            version 1, this is the string "2.0". If the version is not
 *            specified, supporting any version of the feature will cause the
 *            method to return %TRUE.
 * @exc:  Exception Object ref
 *
 * Tests whether the DOM implementation implements a specific feature.
 * Returns: %TRUE if the feature is supported, %FALSE otherwise.
 */
GdomeBoolean 
gdome_xml_di_hasFeature (GdomeDOMImplementation *self,
                         GdomeDOMString *feature,
                         GdomeDOMString *version,
                         GdomeException *exc)
{
	g_return_val_if_fail (self != NULL, FALSE);
	g_return_val_if_fail (feature != NULL, FALSE);
	g_return_val_if_fail (exc != NULL, FALSE);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	if (version == NULL)
		return TRUE;

	if (strcmp (version->str, "1.0") && strcmp (version->str,"2.0"))
		return FALSE;

	if (!g_strcasecmp(feature->str, "Core") ||
			!g_strcasecmp(feature->str, "XML") ||
			!g_strcasecmp(feature->str, "Events") ||
			!g_strcasecmp(feature->str, "MutationEVents"))
		return TRUE;

	return FALSE;
}

/**
 * gdome_xml_di_createDocumentType:
 * @self:  The DOMImplementation Object ref
 * @qualifiedName:  The qualified name of the document type to be created
 * @publicID:  The external subset public identifier
 * @systemID:  The external subset system identifier
 * @exc:  The Exception Object ref
 *
 * Creates an empty DocumentType node. Entity declarations and notations are
 * not made available. Entity reference expansions and default attribute
 * additions do not occur. It is expected that a future version of the DOM will
 * provide a way for populating a DocumentType.
 *
 * %GDOME_NAMESPACE_ERR: Raised if the qualifiedName is malformed.
 * Returns: A new DocumentType node.
 */
GdomeDocumentType *
gdome_xml_di_createDocumentType (GdomeDOMImplementation *self,
																 GdomeDOMString *qualifiedName,
																 GdomeDOMString *publicId,
																 GdomeDOMString *systemId,
																 GdomeException *exc) {
	gchar **strs, *localName = NULL;
	xmlDtd *n;
	xmlChar *pch1 = NULL, *pch2 = NULL;

	g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (qualifiedName != NULL, NULL);
	if (publicId != NULL)
		pch1 = publicId->str;
	if (systemId != NULL)
		pch2 = systemId->str;
	g_return_val_if_fail (exc != NULL, NULL);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	/* test malformed qualifiedName */
	strs = g_strsplit((gchar *)qualifiedName->str, ":", 0);
	if (strs[0] && strs[1] && strs[2]) {
		*exc = GDOME_NAMESPACE_ERR;
	} else if (strs[0] && strs[1] && !strs[2]) {
		localName = g_strdup(strs[1]);
	}
	g_strfreev(strs);

	if (!*exc) {
		/* create the new DTD node */
		if (localName != NULL) {
			n = xmlCreateIntSubset (NULL, (xmlChar *)localName, pch1, pch2);
			g_free(localName);
		} else
			n = xmlCreateIntSubset (NULL, (xmlChar *)qualifiedName->str, pch1, pch2);

		/* FOR THE FUTURE: What about prefix?
		 DOM2 specs about DocumentType interface says: "...the effect of namespaces
		 and the various XML schema efforts on DTD representation are not clearly
		 understood as of this writing." */

		return (GdomeDocumentType *)gdome_xml_dt_mkref (n);
	} else {
		if (localName != NULL)
			g_free(localName);
		return NULL;
	}
}

/**
 * gdome_xml_di_createDocument:
 * @self:  DOMImplementation Object ref
 * @namespaceURI:  The namespace URI of the document element to create
 * @qualifiedName:  The qualified name of the document element to be created
 * @doctype:  The type of document to be created or NULL
 * @exc:  Exception Object ref
 *
 * Creates an XML Document object of the specified type with its document
 * element.
 *
 * %GDOME_NAMESPACE_ERR: Raised if the @qualifiedName is malformed, if the
 * @qualifiedName has a prefix and the @namespaceURI is %NULL, or if the
 * @qualifiedName has a prefix that is "xml" and the @namespaceURI is different
 * from "http://www.w3.org/XML/1998/namespace"
 * %GDOME_WRONG_DOCUMENT_ERR: Raised if @doctype has already been used with a
 * different document.
 * Returns: a new Document node.
 */
GdomeDocument *
gdome_xml_di_createDocument (GdomeDOMImplementation *self,
														 GdomeDOMString *namespaceURI,
														 GdomeDOMString *qualifiedName,
														 GdomeDocumentType *doctype,
														 GdomeException *exc) {
	GdomeDocument *ret = NULL;
	GdomeElement *docElem;
	GdomeNode *result = NULL;

	g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (qualifiedName != NULL, NULL);
	if (doctype != NULL) {
		g_return_val_if_fail (GDOME_XML_IS_DT(doctype), NULL);
		if (gdome_xmlGetOwner((xmlNode *)((Gdome_xml_DocumentType *)doctype)->n) != NULL) {
			*exc = GDOME_WRONG_DOCUMENT_ERR;
			return NULL;
		}
	}
	g_return_val_if_fail (exc != NULL, NULL);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	ret = (GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)xmlNewDoc ("1.0"));
  g_assert (ret != NULL);

	/* Add the doctype */
	if (doctype != NULL) {
		xmlDtd *n = xmlCreateIntSubset (((Gdome_xml_Document *)ret)->n,
																		((Gdome_xml_DocumentType *)doctype)->n->name,
																		((Gdome_xml_DocumentType *)doctype)->n->ExternalID,
																		((Gdome_xml_DocumentType *)doctype)->n->SystemID);
		xmlFreeDtd (((Gdome_xml_DocumentType *)doctype)->n);
		((Gdome_xml_DocumentType *)doctype)->n = n;
		gdome_treegc_addNode ((GdomeNode *)doctype);
	}

	docElem = gdome_xml_doc_createElementNS (ret, namespaceURI, qualifiedName, exc);
	if (!*exc) {
		result = gdome_xml_n_appendChild((GdomeNode *)ret, (GdomeNode *)docElem, exc);
    g_assert (result != NULL);
		gdome_xml_n_unref (result, exc);
		gdome_xml_n_unref ((GdomeNode *)docElem, exc);

    return ret;
	} else {
		gdome_xml_doc_unref ((GdomeNode *)ret, exc);
		return NULL;
	}
}

/**
 * gdome_xml_di_freeDoc:
 * @self:  DOMImplementation Object ref
 * @doc:  The Document Object ref to be freed
 * @exc:  Exception Object ref
 *
 * DEPRECATED!
 * Free all objects and structures associated to the Document specified.
 */
void
gdome_xml_di_freeDoc (GdomeDOMImplementation *self,
											GdomeDocument *doc,
											GdomeException *exc) {
	Gdome_xml_Document *priv_doc = (Gdome_xml_Document *)doc;

	g_return_if_fail (self != NULL);
	g_return_if_fail (doc != NULL);
	g_return_if_fail (exc != NULL);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	xmlFreeDoc (priv_doc->n);
}

/**
 * gdome_xml_di_createDocFromURI:
 * @self:  DOMImplementation Object ref
 * @uri:  the uri of XML file to load
 * @mode:  specifies if the document is to be parsed, validated or recovered
 *         and if the entity substitution has to be done.
 * @exc:  Exception Object ref
 *
 * Loads a XML Document from a URI and build a DOM tree. If @mode is
 * %GDOME_LOAD_PARSING the document must be well formed, if @mode is
 * %GDOME_LOAD_VALIDATING the document must valid and if @mode is
 * %GDOME_LOAD_RECOVERING a tree is built anyway.
 * If the %GDOME_LOAD_SUBSTITUTE_ENTITIES flag is added, entity substitution is
 * performed (es. @mode = %GDOME_LOAD_PARSING | %GDOME_LOAD_SUBSTITUTE_ENTITIES
 * ).
 * If the %GDOME_LOAD_COMPLETE_ATTR flag is set, the tree is populated
 * with default attributes from the DTD.
 * Returns: The Document Object of the created tree or %NULL if an error occur.
 */
GdomeDocument *
gdome_xml_di_createDocFromURI (GdomeDOMImplementation *self,
															 const char *uri,
															 unsigned int  mode,
															 GdomeException *exc) {
	int oldvalue =  xmlDoValidityCheckingDefaultValue;
	GdomeDocument *ret = NULL;
	int prevSubstValue;

	g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (uri != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	if (mode & GDOME_LOAD_SUBSTITUTE_ENTITIES)
		prevSubstValue = xmlSubstituteEntitiesDefault (1);
	else
		prevSubstValue = xmlSubstituteEntitiesDefault (0);

	if (mode & GDOME_LOAD_COMPLETE_ATTRS)
		xmlLoadExtDtdDefaultValue |= XML_COMPLETE_ATTRS;

	switch (mode & (GDOME_LOAD_PARSING |
									GDOME_LOAD_VALIDATING |
									GDOME_LOAD_RECOVERING)) {
	case GDOME_LOAD_PARSING:
		xmlDoValidityCheckingDefaultValue = 0;
		ret = (GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)xmlParseFile (uri));
		break;
	case GDOME_LOAD_VALIDATING:
		xmlDoValidityCheckingDefaultValue = 1;
		ret = (GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)xmlParseFile (uri));
		break;
	case GDOME_LOAD_RECOVERING:
		xmlDoValidityCheckingDefaultValue = 0;
		ret = (GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)xmlRecoverFile (uri));
		break;
	}

	xmlSubstituteEntitiesDefault (prevSubstValue);
	xmlDoValidityCheckingDefaultValue = oldvalue;
	return ret;
}

/**
 * gdome_xml_di_createDocFromMemory:
 * @self:  DOMImplementation Object ref
 * @buffer:  a pointer to char array that holds the XML document
 * @mode:  specifies if the document is to be parsed, validated or recovered
 *         and if the entity substitution has to be done.
 * @exc:  Exception Object ref
 *
 * Loads a XML Document from a memory buffer and build a DOM tree. If @mode is
 * %GDOME_LOAD_PARSING the document must be well formed, if @mode is
 * %GDOME_LOAD_VALIDATING the document must valid and if @mode is
 * %GDOME_LOAD_RECOVERING a tree is built anyway.
 * If the %GDOME_LOAD_SUBSTITUTE_ENTITIES flag is added, entity substitution is
 * performed (es. @mode = %GDOME_LOAD_PARSING | %GDOME_LOAD_SUBSTITUTE_ENTITIES
 * ).
 * If the %GDOME_LOAD_COMPLETE_ATTR flag is set, the tree is populated
 * with default attributes from the DTD.
 * Returns: The Document Object of the created tree or %NULL if an error occur.
 */
GdomeDocument *
gdome_xml_di_createDocFromMemory (GdomeDOMImplementation *self,
																	char *buffer,
																	unsigned int mode,
																	GdomeException *exc) {
	int oldvalue =  xmlDoValidityCheckingDefaultValue;
	GdomeDocument *ret = NULL;
	int size;
	int prevSubstValue;

	g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (buffer != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	size = strlen (buffer);

	if (mode & GDOME_LOAD_SUBSTITUTE_ENTITIES)
		prevSubstValue = xmlSubstituteEntitiesDefault (1);
	else
		prevSubstValue = xmlSubstituteEntitiesDefault (0);

	if (mode & GDOME_LOAD_COMPLETE_ATTRS)
		xmlLoadExtDtdDefaultValue |= XML_COMPLETE_ATTRS;

	switch (mode & (GDOME_LOAD_PARSING |
									GDOME_LOAD_VALIDATING |
									GDOME_LOAD_RECOVERING)) {
	case GDOME_LOAD_PARSING:
		xmlDoValidityCheckingDefaultValue = 0;
		ret = (GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)xmlParseMemory (buffer, size));
		break;
	case GDOME_LOAD_VALIDATING:
		xmlDoValidityCheckingDefaultValue = 1;
		ret = (GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)xmlParseMemory (buffer, size));
		break;
	case GDOME_LOAD_RECOVERING:
		xmlDoValidityCheckingDefaultValue = 0;
		ret = (GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)xmlRecoverMemory (buffer, size));
		break;
	}

	xmlSubstituteEntitiesDefault (prevSubstValue);
	xmlDoValidityCheckingDefaultValue = oldvalue;
	return ret;
}

/* Helper variables and function for WithEntityTable APIs*/
static getEntitySAXFunc default_entity_resolver;
static xmlHashTable *pfwet_entities;

static xmlEntity *
pfwet_entity_resolver(void* ctxt, const xmlChar *name) {
	xmlEntity *entity = NULL;

	if (default_entity_resolver != NULL)
		entity = default_entity_resolver(ctxt, name);
	if (entity == NULL)
		entity = (xmlEntity *)xmlHashLookup(pfwet_entities, name);
	if (entity == NULL)
		fprintf (stderr, "Entity %s not found!\n", name);

	return entity;
}

/**
 * gdome_xml_di_createDocFromURIWithEntitiesTable:
 * @self:  DOMImplementation Object ref
 * @uri:  the uri of XML file to load
 * @entityTable:  array of #GdomeEntitiesTableEntry that define name, value,
 *                publicID ans systemID of entities used but not declared
 *                in DTD. The array must be NULL terminated.
 * @mode:  specifies if the document is to be parsed, validated or recovered
 *         and if the entity substitution has to be done.
 * @exc:  Exception Object ref
 *
 * Loads a XML Document using also entity declaration specified in @entityTable
 * from a URI and build a DOM tree. If @mode is %GDOME_LOAD_PARSING the
 * document must be well formed, if @mode is %GDOME_LOAD_VALIDATING the
 * document must valid and if @mode is %GDOME_LOAD_RECOVERING a tree is built
 * anyway.
 * If the %GDOME_LOAD_SUBSTITUTE_ENTITIES flag is added, entity substitution is
 * performed (es. @mode = %GDOME_LOAD_PARSING | %GDOME_LOAD_SUBSTITUTE_ENTITIES
 * ).
 * If the %GDOME_LOAD_COMPLETE_ATTR flag is set, the tree is populated
 * with default attributes from the DTD.
 * Returns: The Document Object of the created tree or %NULL if an error occur.
 */
GdomeDocument *
gdome_xml_di_createDocFromURIWithEntitiesTable (GdomeDOMImplementation *self,
                                                const char *uri,
                                                const GdomeEntitiesTableEntry entityTable[],
                                                unsigned int mode,
                                                GdomeException *exc) {
	int oldvalue =  xmlDoValidityCheckingDefaultValue;
	xmlParserCtxt *ctxt;
	xmlDoc *doc;
	int i, recovery = 0;
	int prevSubstValue;

	g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (uri != NULL, NULL);
	g_return_val_if_fail (entityTable != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	if (mode & GDOME_LOAD_SUBSTITUTE_ENTITIES)
		prevSubstValue = xmlSubstituteEntitiesDefault (1);
	else
		prevSubstValue = xmlSubstituteEntitiesDefault (0);

	if (mode & GDOME_LOAD_COMPLETE_ATTRS)
		xmlLoadExtDtdDefaultValue |= XML_COMPLETE_ATTRS;

	switch (mode & (GDOME_LOAD_PARSING |
									GDOME_LOAD_VALIDATING |
									GDOME_LOAD_RECOVERING)) {
	case GDOME_LOAD_PARSING:
		xmlDoValidityCheckingDefaultValue = 0;
		recovery = 0;
		break;
	case GDOME_LOAD_VALIDATING:
		xmlDoValidityCheckingDefaultValue = 1;
		recovery = 0;
		break;
	case GDOME_LOAD_RECOVERING:
		xmlDoValidityCheckingDefaultValue = 0;
		recovery = 1;
		break;
	}

	/* Create the Parser Context and Set the sax getEntity function */
	ctxt = xmlCreateFileParserCtxt(uri);
	if (ctxt == NULL)
		return NULL;
	g_assert (ctxt->sax != NULL);
  default_entity_resolver = ctxt->sax->getEntity;
	ctxt->sax->getEntity = pfwet_entity_resolver;

	/* Create the hash Table of entities from entityTable */
	if ((doc = xmlNewDoc ("1.0")) == NULL)
		return NULL;
	if (xmlCreateIntSubset(doc, NULL, NULL, NULL) == NULL)
		return NULL;
	for (i=0; entityTable[i].name != NULL; i++) {
		xmlAddDocEntity (doc, entityTable[i].name, XML_INTERNAL_GENERAL_ENTITY,
										 entityTable[i].publicID, entityTable[i].systemID, entityTable[i].value);
	}
	pfwet_entities = doc->intSubset->entities;

	/* Parse the XML document */
	xmlParseDocument(ctxt);
	xmlFreeDoc (doc);
	if ((ctxt->wellFormed) || recovery)
		doc = ctxt->myDoc;
	else {
		doc = NULL;
		xmlFreeDoc(ctxt->myDoc);
		ctxt->myDoc = NULL;
	}
	xmlFreeParserCtxt(ctxt);

	xmlSubstituteEntitiesDefault (prevSubstValue);
	xmlDoValidityCheckingDefaultValue = oldvalue;
	return (GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)doc);
}

/**
 * gdome_xml_di_createDocFromMemoryWithEntitiesTable:
 * @self:  DOMImplementation Object ref
 * @buffer:  a pointer to char array that holds the XML document
 * @entityTable:  array of #GdomeEntitiesTableEntry that define name, value,
 *                publicID ans systemID of entities used but not declared
 *                in DTD. The array must be NULL terminated.
 * @mode:  specifies if the document is to be parsed, validated or recovered
 *         and if the entity substitution has to be done.
 * @exc:  Exception Object ref
 *
 * Loads a XML Document using also entity declaration specified in @entityTable
 * from a memory buffer and build a DOM tree. If @mode is %GDOME_LOAD_PARSING
 * the document must be well formed, if @mode is %GDOME_LOAD_VALIDATING the
 * document must valid and if @mode is %GDOME_LOAD_RECOVERING a tree is built
 * anyway.
 * If the %GDOME_LOAD_SUBSTITUTE_ENTITIES flag is added, entity substitution is
 * performed (es. @mode = %GDOME_LOAD_PARSING | %GDOME_LOAD_SUBSTITUTE_ENTITIES
 * ).
 * If the %GDOME_LOAD_COMPLETE_ATTR flag is set, the tree is populated
 * with default attributes from the DTD.
 * Returns: The Document Object of the created tree or %NULL if an error occur.
 */
GdomeDocument *
gdome_xml_di_createDocFromMemoryWithEntitiesTable (GdomeDOMImplementation *self,
																									 char *buffer,
																									 const GdomeEntitiesTableEntry entityTable[],
																									 unsigned int mode,
																									 GdomeException *exc) {
	int oldvalue =  xmlDoValidityCheckingDefaultValue;
	xmlParserCtxt *ctxt;
	xmlDoc *doc;
	int i, recovery = 0, size;
	int prevSubstValue;

	g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (buffer != NULL, NULL);
	g_return_val_if_fail (entityTable != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	size = strlen (buffer);

	if (mode & GDOME_LOAD_SUBSTITUTE_ENTITIES)
		prevSubstValue = xmlSubstituteEntitiesDefault (1);
	else
		prevSubstValue = xmlSubstituteEntitiesDefault (0);

	if (mode & GDOME_LOAD_COMPLETE_ATTRS)
		xmlLoadExtDtdDefaultValue |= XML_COMPLETE_ATTRS;

	switch (mode & (GDOME_LOAD_PARSING |
									GDOME_LOAD_VALIDATING |
									GDOME_LOAD_RECOVERING)) {
	case GDOME_LOAD_PARSING:
		xmlDoValidityCheckingDefaultValue = 0;
		recovery = 0;
		break;
	case GDOME_LOAD_VALIDATING:
		xmlDoValidityCheckingDefaultValue = 1;
		recovery = 0;
		break;
	case GDOME_LOAD_RECOVERING:
		xmlDoValidityCheckingDefaultValue = 0;
		recovery = 1;
		break;
	}

	/* Create the Parser Context and Set the sax getEntity function */
	ctxt = xmlCreateMemoryParserCtxt(buffer, size);
	if (ctxt == NULL)
		return NULL;
	g_assert (ctxt->sax != NULL);
  default_entity_resolver = ctxt->sax->getEntity;
	ctxt->sax->getEntity = pfwet_entity_resolver;

	/* Create the hash Table of entities from entityTable */
	if ((doc = xmlNewDoc ("1.0")) == NULL)
		return NULL;
	if (xmlCreateIntSubset(doc, NULL, NULL, NULL) == NULL)
		return NULL;
	for (i=0; entityTable[i].name != NULL; i++) {
		xmlAddDocEntity (doc, entityTable[i].name, XML_INTERNAL_GENERAL_ENTITY,
										 entityTable[i].publicID, entityTable[i].systemID, entityTable[i].value);
	}
	pfwet_entities = doc->intSubset->entities;

	/* Parse the XML document */
	xmlParseDocument(ctxt);
	xmlFreeDoc (doc);
	if ((ctxt->wellFormed) || recovery)
		doc = ctxt->myDoc;
	else {
		doc = NULL;
		xmlFreeDoc(ctxt->myDoc);
		ctxt->myDoc = NULL;
	}
	xmlFreeParserCtxt(ctxt);

	xmlSubstituteEntitiesDefault (prevSubstValue);
	xmlDoValidityCheckingDefaultValue = oldvalue;
	return (GdomeDocument *)gdome_xml_n_mkref ((xmlNode *)doc);
}

/**
 * gdome_xml_di_saveDocToFile:
 * @self:  DOMImplementation Object ref
 * @doc:  the Document of which the tree is wanted to be saved
 * @filename:  the XML filename to save
 * @mode:  the indentation mode wanted
 * @exc:  Exception Object ref
 *
 * Save the DOM tree of the Document specified to file.
 * Returns: %FALSE in case of failure, %TRUE otherwise.
 */
GdomeBoolean
gdome_xml_di_saveDocToFile (GdomeDOMImplementation *self, GdomeDocument *doc,
                            const char *filename, GdomeSavingCode mode,
                            GdomeException *exc)
{
	Gdome_xml_Document *priv_doc = (Gdome_xml_Document *)doc;

	g_return_val_if_fail (self != NULL, FALSE);
	g_return_val_if_fail (doc != NULL, FALSE);
	g_return_val_if_fail (filename != NULL, FALSE);
	g_return_val_if_fail (exc != NULL, FALSE);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	return (xmlSaveFormatFile (filename, priv_doc->n, mode) != -1);
}

/**
 * gdome_xml_di_saveDocToFileEnc:
 * @self:  DOMImplementation Object ref
 * @doc:  the Document of which the tree is wanted to be saved
 * @filename:  the XML filename to save
 * @encoding:  character encoding to use when generating XML text
 * @mode:  the indentation mode wanted
 * @exc:  Exception Object ref
 *
 * Save the DOM tree of the Document specified to file with the specified
 * Character encoding.
 * Returns: %FALSE in case of failure, %TRUE otherwise.
 */
GdomeBoolean
gdome_xml_di_saveDocToFileEnc (GdomeDOMImplementation *self, GdomeDocument *doc,
                               const char *filename, const char *encoding,
                               GdomeSavingCode mode, GdomeException *exc)
{
	Gdome_xml_Document *priv_doc = (Gdome_xml_Document *)doc;

	g_return_val_if_fail (self != NULL, FALSE);
	g_return_val_if_fail (doc != NULL, FALSE);
	g_return_val_if_fail (filename != NULL, FALSE);
	g_return_val_if_fail (exc != NULL, FALSE);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	return (xmlSaveFormatFileEnc (filename, priv_doc->n, encoding, mode) != -1);
}

/**
 * gdome_xml_di_saveDocToMemory:
 * @self:  DOMImplementation Object ref
 * @doc:  the Document of which the tree is wanted to be saved
 * @mem:  the memory pointer where the saved document is returned
 * @mode:  the indentation mode wanted
 * @exc:  Exception Object ref
 *
 * Save the DOM tree of the Document specified to a new allocated memory and
 * return it in the @mem pointer.
 * Returns: %FALSE in case of failure, %TRUE otherwise.
 */
GdomeBoolean
gdome_xml_di_saveDocToMemory (GdomeDOMImplementation *self, GdomeDocument *doc,
															char **mem, GdomeSavingCode mode,
															GdomeException *exc)
{
	Gdome_xml_Document *priv_doc = (Gdome_xml_Document *)doc;
	int size = 0;

	g_return_val_if_fail (self != NULL, FALSE);
	g_return_val_if_fail (doc != NULL, FALSE);
	g_return_val_if_fail (mem != NULL, FALSE);
	g_return_val_if_fail (exc != NULL, FALSE);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	xmlDocDumpFormatMemory(priv_doc->n, (xmlChar **)mem, &size, mode);
	if ((mem == NULL) && (size > 0))
		return FALSE;
	else
		return TRUE;
}

/**
 * gdome_xml_di_saveDocToMemoryEnc:
 * @self:  DOMImplementation Object ref
 * @doc:  the Document of which the tree is wanted to be saved
 * @mem:  the memory pointer where the saved document is returned
 * @encoding:  character encoding to use when generating XML text
 * @mode:  the indentation mode wanted
 * @exc:  Exception Object ref
 *
 * Save the DOM tree of the Document specified using the specified character
 * encoding standard to a new allocated memory and return it in the @mem
 * pointer.
 * Returns: %FALSE in case of failure, %TRUE otherwise.
 */
GdomeBoolean
gdome_xml_di_saveDocToMemoryEnc (GdomeDOMImplementation *self, GdomeDocument *doc,
                                 char **mem, const char *encoding,
                                 GdomeSavingCode mode, GdomeException *exc)
{
	Gdome_xml_Document *priv_doc = (Gdome_xml_Document *)doc;
	int size = 0;

	g_return_val_if_fail (self != NULL, FALSE);
	g_return_val_if_fail (doc != NULL, FALSE);
	g_return_val_if_fail (mem != NULL, FALSE);
	g_return_val_if_fail (encoding != NULL, FALSE);
	g_return_val_if_fail (exc != NULL, FALSE);
	g_assert(self == (GdomeDOMImplementation *)gdome_xml_DOMImplementation);

	xmlDocDumpFormatMemoryEnc(priv_doc->n, (xmlChar **)mem, &size, encoding, mode);
	if ((mem == NULL) && (size > 0))
		return FALSE;
	else
		return TRUE;
}

/**
 * gdome_xml_di_enableEvent:
 * @self:  DOMImplementation Object ref
 * @doc:  the Document on which we want to enable events
 * @name:  the name of the event type to enable
 * @exc:  Exception Object ref
 *
 * Enables the propagation of the event type specified.
 */
void
gdome_xml_di_enableEvent (GdomeDOMImplementation *self, GdomeDocument *doc,
													const char *name,
													GdomeException *exc)
{
	g_return_if_fail (self != NULL);
	g_return_if_fail (doc != NULL);
	g_return_if_fail (name != NULL);

	gdome_xml_doc_enableEventByName(doc, name);
}

/**
 * gdome_xml_di_disableEvent:
 * @self:  DOMImplementation Object ref
 * @doc:  the Document on which we want to disable events
 * @name:  the name of the event type to disable
 * @exc:  Exception Object ref
 *
 * Disables the propagation of the event type specified.
 */
void
gdome_xml_di_disableEvent (GdomeDOMImplementation *self, GdomeDocument *doc,
													 const char *name,
													 GdomeException *exc)
{
	g_return_if_fail (self != NULL);
	g_return_if_fail (doc != NULL);
	g_return_if_fail (name != NULL);

	gdome_xml_doc_disableEventByName(doc, name);
}

/**
 * gdome_xml_di_eventIsEnabled:
 * @self:  DOMImplementation Object ref
 * @doc:  the Document on which we want to make the check
 * @name:  the name of the event type to check
 * @exc:  Exception Object ref
 *
 * Checks if the specified event type is enabled.
 * Returns: %TRUE if the event type specified is enabled, %FALSE otherwise.
 */
GdomeBoolean
gdome_xml_di_eventIsEnabled (GdomeDOMImplementation *self, GdomeDocument *doc,
														 const char *name,
														 GdomeException *exc)
{
	g_return_val_if_fail (self != NULL, FALSE);
	g_return_val_if_fail (doc != NULL, FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	return gdome_xml_doc_eventEnabledByName(doc, name);
}
