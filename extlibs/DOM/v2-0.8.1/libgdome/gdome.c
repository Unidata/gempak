/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome.c
 *
 * Copyright (C) 1999 Raph Levien <raph@acm.org>
 * Copyright (C) 2000 Mathieu Lacage <mathieu@gnu.org>
 * CopyRight (C) 2001 Paolo Casarini <paolo@casarini.org>
 *
 * This file is generated automatically.  To make changes, edit
 * test/apigen/core.xml
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

#include <libxml/parser.h>
#include <libxml/hash.h>
#include "gdome.h"
#include "gdome-xml-str.h"
#include "gdome-xml-domimpl.h"
#include "gdome-xml-nnodem.h"
#include "gdome-xml-nodel.h"
#include "gdome-xml-domimpl.h"
#include "gdome-xml-node.h"
#include "gdome-xml-cdata.h"
#include "gdome-xml-text.h"
#include "gdome-xml-cdatas.h"
#include "gdome-xml-comment.h"
#include "gdome-xml-document.h"
#include "gdome-xml-documentf.h"
#include "gdome-xml-documentt.h"
#include "gdome-xml-attribute.h"
#include "gdome-xml-element.h"
#include "gdome-xml-entity.h"
#include "gdome-xml-entityref.h"
#include "gdome-xml-pi.h"
#include "gdome-xml-xmldtdutil.h"
#include "gdome-xml-notation.h"
#include "gdome-xml-xpns.h"


/******************************************************************************
          GdomeDOMString interface API
 ******************************************************************************/
/**
 * gdome_str_mkref:
 * @str:  a %NULL terminated string
 *
 * Creates a #GdomeDOMString from a statically allocated string.
 * Returns: the new DOMString object.
 */
GdomeDOMString *
gdome_str_mkref (const gchar *str)
{
	return gdome_xml_str_mkref (str);
}
/**
 * gdome_str_mkref_own:
 * @str:  a %NULL terminated string
 *
 * Creates a #GdomeDOMString from a dynamically allocated gchar buffer.
 * Returns: the new DOMString object.
 */
GdomeDOMString *
gdome_str_mkref_own (gchar *str)
{
	return gdome_xml_str_mkref_own (str);
}
/**
 * gdome_str_mkref_xml:
 * @str:  a %NULL terminated string
 *
 * Creates a #GdomeDOMString from a xmlChar buffer already allocated by libxml.
 * DEPRECATED
 * Returns: the new DOMString object.
 */
GdomeDOMString *
gdome_str_mkref_xml (xmlChar *str)
{
	return gdome_xml_str_mkref_xml (str);
}
/**
 * gdome_str_mkref_dup:
 * @str:  a %NULL terminated string
 *
 * Creates a #GdomeDOMString from a static or dynamically allocated gchar
 * buffer, but a copy of the initializing string is done before construction.
 * Returns: the new DOMString object.
 */
GdomeDOMString *
gdome_str_mkref_dup (const gchar *str)
{
	return gdome_xml_str_mkref_dup (str);
}
/**
 * gdome_str_ref:
 * @self:  DOMString Object ref
 *
 * Increase the reference count of the specified #GdomeDOMString.
 */
void
gdome_str_ref (GdomeDOMString *self)
{
	gdome_xml_str_ref (self);
}
/**
 * gdome_str_unref:
 * @self:  DOMString Object ref
 *
 * Decrease the reference count of the specified #GdomeDOMString. Free the
 * structure if the object will have zero reference.
 */
void
gdome_str_unref (GdomeDOMString *self)
{
	gdome_xml_str_unref (self);
}
/**
 * gdome_str_equal:
 * @self:  DOMString Object ref
 * @str:  DOMString to be compared
 *
 * Returns: %TRUE if the strings are equal, %FALSE otherwise.
 */
GdomeBoolean
gdome_str_equal (GdomeDOMString *self, GdomeDOMString *str)
{
	return gdome_xml_str_equal (self, str);
}
/**
 * gdome_str_equalIgnoreCase:
 * @self:  DOMString Object ref
 * @str:  DOMString to be compared
 *
 * Returns: %TRUE if the strings are equal ignoring case, %FALSE otherwise.
 */
GdomeBoolean
gdome_str_equalIgnoreCase (GdomeDOMString *self, GdomeDOMString *str)
{
	return gdome_xml_str_equalIgnoreCase (self, str);
}
/**
 * gdome_str_charAt:
 * @self:  DOMString Object ref
 * @index:  the index of the character wanted
 *
 * Returns: the @index -th character in the specified string
 */
gchar
gdome_str_charAt (GdomeDOMString *self, int index)
{
	return gdome_xml_str_charAt (self, index);
}
/**
 * gdome_str_concat:
 * @self:  DOMString Object ref
 * @str:  DOMString to be appended
 *
 * Returns: a new #GdomeDOMString that is the concatenation of this string
 * with @str. If @str is %NULL a new reference to this string is returned.
 */
GdomeDOMString *
gdome_str_concat (GdomeDOMString *self, GdomeDOMString *str)
{
	return gdome_xml_str_concat (self, str);
}
/**
 * gdome_str_isEmpty:
 * @self:  DOMString Object ref
 *
 * Returns: %TRUE if this string is empty, %FALSE otherwise.
 */
GdomeBoolean
gdome_str_isEmpty (GdomeDOMString *self)
{
	return gdome_xml_str_isEmpty (self);
}
/**
 * gdome_str_endsWith:
 * @self:  DOMString Object ref
 * @suffix:  DOMString to check
 *
 * Returns: %TRUE if this string ends with @str.
 */
GdomeBoolean
gdome_str_endsWith (GdomeDOMString *self, GdomeDOMString *suffix)
{
	return gdome_xml_str_endsWith (self, suffix);
}
/**
 * gdome_str_startsWith:
 * @self:  DOMString Object ref
 * @prefix:  DOMString to check
 *
 * Returns: %TRUE if this string starts with @str.
 */
GdomeBoolean
gdome_str_startsWith (GdomeDOMString *self, GdomeDOMString *prefix)
{
	return gdome_xml_str_startsWith (self, prefix);
}
/**
 * gdome_str_length:
 * @self:  DOMString Object ref
 *
 * Returns: the length of this string.
 */
int
gdome_str_length (GdomeDOMString *self)
{
	return gdome_xml_str_length (self);
}

/******************************************************************************
          GdomeDOMImplementation interface API
 ******************************************************************************/
/**
 * gdome_di_mkref:
 *
 * Constructor of the DOMImplementation interface.
 * Returns: the reference to the DOMImplementation structure.
 */
GdomeDOMImplementation *
gdome_di_mkref (void)
{
	return gdome_xml_di_mkref ();
}
/**
 * gdome_di_ref:
 * @self:  DOMImplementation Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the DOMImplementation structure.
 */
void
gdome_di_ref (GdomeDOMImplementation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DOMImplementation *)self)->vtab->ref (self, exc);
}
/**
 * gdome_di_unref:
 * @self:  DOMImplementation Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the DOMImplementation. Free the structure
 * if DOMImplementation will have zero reference.
 */
void
gdome_di_unref (GdomeDOMImplementation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DOMImplementation *)self)->vtab->unref (self, exc);
}
/**
 * gdome_di_query_interface:
 * @self:  DOMImplementation Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_di_query_interface (GdomeDOMImplementation *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->query_interface (self, interface, exc);
}
/**
 * gdome_di_createDocument:
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
gdome_di_createDocument (GdomeDOMImplementation *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeDocumentType *doctype, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->createDocument (self, namespaceURI, qualifiedName, doctype, exc);
}
/**
 * gdome_di_createDocumentType:
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
gdome_di_createDocumentType (GdomeDOMImplementation *self, GdomeDOMString *qualifiedName, GdomeDOMString *publicId, GdomeDOMString *systemId, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->createDocumentType (self, qualifiedName, publicId, systemId, exc);
}
/**
 * gdome_di_hasFeature:
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
gdome_di_hasFeature (GdomeDOMImplementation *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->hasFeature (self, feature, version, exc);
}
/**
 * gdome_di_freeDoc:
 * @self:  DOMImplementation Object ref
 * @doc:  The Document Object ref to be freed
 * @exc:  Exception Object ref
 *
 * DEPRECATED!
 * Free all objects and structures associated to the Document specified.
 */
void
gdome_di_freeDoc (GdomeDOMImplementation *self, GdomeDocument *doc, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DOMImplementation *)self)->vtab->freeDoc (self, doc, exc);
}
/**
 * gdome_di_createDocFromURI:
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
gdome_di_createDocFromURI (GdomeDOMImplementation *self, const char *uri, unsigned int mode, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->createDocFromURI (self, uri, mode, exc);
}
/**
 * gdome_di_createDocFromMemory:
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
gdome_di_createDocFromMemory (GdomeDOMImplementation *self, char *buffer, unsigned int mode, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->createDocFromMemory (self, buffer, mode, exc);
}
/**
 * gdome_di_createDocFromURIWithEntitiesTable:
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
gdome_di_createDocFromURIWithEntitiesTable (GdomeDOMImplementation *self, const char *uri, const GdomeEntitiesTableEntry entityTable[], unsigned int mode, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->createDocFromURIWithEntitiesTable (self, uri, entityTable, mode, exc);
}
/**
 * gdome_di_createDocFromMemoryWithEntitiesTable:
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
gdome_di_createDocFromMemoryWithEntitiesTable (GdomeDOMImplementation *self, char *buffer, const GdomeEntitiesTableEntry entityTable[], unsigned int mode, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->createDocFromMemoryWithEntitiesTable (self, buffer, entityTable, mode, exc);
}
/**
 * gdome_di_saveDocToFile:
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
gdome_di_saveDocToFile (GdomeDOMImplementation *self, GdomeDocument *doc, const char *filename, GdomeSavingCode mode, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->saveDocToFile (self, doc, filename, mode, exc);
}
/**
 * gdome_di_saveDocToFileEnc:
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
gdome_di_saveDocToFileEnc (GdomeDOMImplementation *self, GdomeDocument *doc, const char *filename, const char *encoding, GdomeSavingCode mode, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->saveDocToFileEnc (self, doc, filename, encoding, mode, exc);
}
/**
 * gdome_di_saveDocToMemory:
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
gdome_di_saveDocToMemory (GdomeDOMImplementation *self, GdomeDocument *doc, char **mem, GdomeSavingCode mode, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->saveDocToMemory (self, doc, mem, mode, exc);
}
/**
 * gdome_di_saveDocToMemoryEnc:
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
gdome_di_saveDocToMemoryEnc (GdomeDOMImplementation *self, GdomeDocument *doc, char **mem, const char *encoding, GdomeSavingCode mode, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->saveDocToMemoryEnc (self, doc, mem, encoding, mode, exc);
}
/**
 * gdome_di_enableEvent:
 * @self:  DOMImplementation Object ref
 * @doc:  the Document on which we want to enable events
 * @name:  the name of the event type to enable
 * @exc:  Exception Object ref
 *
 * Enables the propagation of the event type specified.
 */
void
gdome_di_enableEvent (GdomeDOMImplementation *self, GdomeDocument *doc, const char *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DOMImplementation *)self)->vtab->enableEvent (self, doc, name, exc);
}
/**
 * gdome_di_disableEvent:
 * @self:  DOMImplementation Object ref
 * @doc:  the Document on which we want to disable events
 * @name:  the name of the event type to disable
 * @exc:  Exception Object ref
 *
 * Disables the propagation of the event type specified.
 */
void
gdome_di_disableEvent (GdomeDOMImplementation *self, GdomeDocument *doc, const char *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DOMImplementation *)self)->vtab->disableEvent (self, doc, name, exc);
}
/**
 * gdome_di_eventIsEnabled:
 * @self:  DOMImplementation Object ref
 * @doc:  the Document on which we want to make the check
 * @name:  the name of the event type to check
 * @exc:  Exception Object ref
 *
 * Checks if the specified event type is enabled.
 * Returns: %TRUE if the event type specified is enabled, %FALSE otherwise.
 */
GdomeBoolean
gdome_di_eventIsEnabled (GdomeDOMImplementation *self, GdomeDocument *doc, const char *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DOMImplementation *)self)->vtab->eventIsEnabled (self, doc, name, exc);
}

/******************************************************************************
          GdomeNode interface API
 ******************************************************************************/
/**
 * gdome_n_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_n_attributes (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->attributes (self, exc);
}
/**
 * gdome_n_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_n_childNodes (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->childNodes (self, exc);
}
/**
 * gdome_n_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_n_firstChild (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->firstChild (self, exc);
}
/**
 * gdome_n_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_n_lastChild (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->lastChild (self, exc);
}
/**
 * gdome_n_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_n_localName (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->localName (self, exc);
}
/**
 * gdome_n_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_n_namespaceURI (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->namespaceURI (self, exc);
}
/**
 * gdome_n_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_n_nextSibling (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->nextSibling (self, exc);
}
/**
 * gdome_n_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_n_nodeName (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->nodeName (self, exc);
}
/**
 * gdome_n_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_n_nodeType (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->nodeType (self, exc);
}
/**
 * gdome_n_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_n_nodeValue (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->nodeValue (self, exc);
}
/**
 * gdome_n_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_n_set_nodeValue (GdomeNode *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Node *)self)->vtab->set_nodeValue (self, nodeValue, exc);
}
/**
 * gdome_n_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_n_ownerDocument (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->ownerDocument (self, exc);
}
/**
 * gdome_n_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_n_parentNode (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->parentNode (self, exc);
}
/**
 * gdome_n_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_n_prefix (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->prefix (self, exc);
}
/**
 * gdome_n_set_prefix:
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
gdome_n_set_prefix (GdomeNode *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Node *)self)->vtab->set_prefix (self, prefix, exc);
}
/**
 * gdome_n_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_n_previousSibling (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->previousSibling (self, exc);
}
/**
 * gdome_n_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_n_ref (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Node *)self)->vtab->ref (self, exc);
}
/**
 * gdome_n_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_n_unref (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_Node *)self)->vtab->unref (self, exc);
}
/**
 * gdome_n_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_n_query_interface (GdomeNode *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->query_interface (self, interface, exc);
}
/**
 * gdome_n_appendChild:
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
gdome_n_appendChild (GdomeNode *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->appendChild (self, newChild, exc);
}
/**
 * gdome_n_cloneNode:
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
gdome_n_cloneNode (GdomeNode *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->cloneNode (self, deep, exc);
}
/**
 * gdome_n_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_n_hasAttributes (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->hasAttributes (self, exc);
}
/**
 * gdome_n_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_n_hasChildNodes (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->hasChildNodes (self, exc);
}
/**
 * gdome_n_insertBefore:
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
gdome_n_insertBefore (GdomeNode *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->insertBefore (self, newChild, refChild, exc);
}
/**
 * gdome_n_isSupported:
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
gdome_n_isSupported (GdomeNode *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->isSupported (self, feature, version, exc);
}
/**
 * gdome_n_normalize:
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
gdome_n_normalize (GdomeNode *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Node *)self)->vtab->normalize (self, exc);
}
/**
 * gdome_n_removeChild:
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
gdome_n_removeChild (GdomeNode *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->removeChild (self, oldChild, exc);
}
/**
 * gdome_n_replaceChild:
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
gdome_n_replaceChild (GdomeNode *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->replaceChild (self, newChild, oldChild, exc);
}
/**
 * gdome_n_addEventListener:
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
gdome_n_addEventListener (GdomeNode *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Node *)self)->vtab->addEventListener (self, type, listener, useCapture, exc);
}
/**
 * gdome_n_removeEventListener:
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
gdome_n_removeEventListener (GdomeNode *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Node *)self)->vtab->removeEventListener (self, type, listener, useCapture, exc);
}
/**
 * gdome_n_dispatchEvent:
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
gdome_n_dispatchEvent (GdomeNode *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->dispatchEvent (self, evt, exc);
}
/**
 * gdome_n_subTreeDispatchEvent:
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
gdome_n_subTreeDispatchEvent (GdomeNode *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Node *)self)->vtab->subTreeDispatchEvent (self, evt, exc);
}
/**
 * gdome_n_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_n_canAppend (GdomeNode *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Node *)self)->vtab->canAppend (self, newChild, exc);
}

/******************************************************************************
          GdomeDocumentFragment interface API
 ******************************************************************************/
/**
 * gdome_df_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_df_attributes (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_df_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_df_childNodes (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_df_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_df_firstChild (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_df_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_df_lastChild (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_df_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_df_localName (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_df_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_df_namespaceURI (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_df_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_df_nextSibling (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_df_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_df_nodeName (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_df_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_df_nodeType (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_df_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_df_nodeValue (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_df_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_df_set_nodeValue (GdomeDocumentFragment *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentFragment *)self)->vtab->super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_df_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_df_ownerDocument (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_df_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_df_parentNode (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_df_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_df_prefix (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_df_set_prefix:
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
gdome_df_set_prefix (GdomeDocumentFragment *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentFragment *)self)->vtab->super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_df_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_df_previousSibling (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_df_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_df_ref (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentFragment *)self)->vtab->super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_df_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_df_unref (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_DocumentFragment *)self)->vtab->super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_df_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_df_query_interface (GdomeDocumentFragment *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_df_appendChild:
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
gdome_df_appendChild (GdomeDocumentFragment *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_df_cloneNode:
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
gdome_df_cloneNode (GdomeDocumentFragment *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_df_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_df_hasAttributes (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_df_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_df_hasChildNodes (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_df_insertBefore:
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
gdome_df_insertBefore (GdomeDocumentFragment *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_df_isSupported:
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
gdome_df_isSupported (GdomeDocumentFragment *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_df_normalize:
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
gdome_df_normalize (GdomeDocumentFragment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentFragment *)self)->vtab->super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_df_removeChild:
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
gdome_df_removeChild (GdomeDocumentFragment *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_df_replaceChild:
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
gdome_df_replaceChild (GdomeDocumentFragment *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_df_addEventListener:
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
gdome_df_addEventListener (GdomeDocumentFragment *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentFragment *)self)->vtab->super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_df_removeEventListener:
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
gdome_df_removeEventListener (GdomeDocumentFragment *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentFragment *)self)->vtab->super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_df_dispatchEvent:
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
gdome_df_dispatchEvent (GdomeDocumentFragment *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_df_subTreeDispatchEvent:
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
gdome_df_subTreeDispatchEvent (GdomeDocumentFragment *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentFragment *)self)->vtab->super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_df_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_df_canAppend (GdomeDocumentFragment *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentFragment *)self)->vtab->super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeDocument interface API
 ******************************************************************************/
/**
 * gdome_doc_doctype:
 * @self:  Document Object ref
 * @exc:  Exception Object ref
 *
 * Returns: The Document Type Declaration associated with this document. The
 * DOM Level 2 does not support editing the Document Type Declaration. docType
 * cannot be altered in any way, including through the use of methods inherited
 * from the Node interface, such as insertNode or removeNode.
 */
GdomeDocumentType *
gdome_doc_doctype (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->doctype (self, exc);
}
/**
 * gdome_doc_documentElement:
 * @self:  Document Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the root Element of the Document.
 */
GdomeElement *
gdome_doc_documentElement (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->documentElement (self, exc);
}
/**
 * gdome_doc_implementation:
 * @self:  Document Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the DOMImplementation object that handles this document.
 */
GdomeDOMImplementation *
gdome_doc_implementation (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->implementation (self, exc);
}
/**
 * gdome_doc_createAttribute:
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
gdome_doc_createAttribute (GdomeDocument *self, GdomeDOMString *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->createAttribute (self, name, exc);
}
/**
 * gdome_doc_createAttributeNS:
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
gdome_doc_createAttributeNS (GdomeDocument *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->createAttributeNS (self, namespaceURI, qualifiedName, exc);
}
/**
 * gdome_doc_createCDATASection:
 * @self:  Document Object ref
 * @data:  The data for the CDATASection contents
 * @exc:  Exception Object ref
 *
 * Creates a CDATASection node whose value is the specified string.
 * Returns: the new CDATASection object.
 */
GdomeCDATASection *
gdome_doc_createCDATASection (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->createCDATASection (self, data, exc);
}
/**
 * gdome_doc_createComment:
 * @self:  Document Object ref
 * @data:  The data for the comment contents
 * @exc:  Exception Object ref
 *
 * Creates a Comment node whose value is the specified string.
 * Returns: the new Comment object.
 */
GdomeComment *
gdome_doc_createComment (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->createComment (self, data, exc);
}
/**
 * gdome_doc_createDocumentFragment:
 * @self:  Document Object ref
 * @exc:  Exception Object ref
 *
 * Creates an empty DocumentFragment object. 
 * Returns: the new DocumentFragment object.
 */
GdomeDocumentFragment *
gdome_doc_createDocumentFragment (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->createDocumentFragment (self, exc);
}
/**
 * gdome_doc_createElement:
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
gdome_doc_createElement (GdomeDocument *self, GdomeDOMString *tagName, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->createElement (self, tagName, exc);
}
/**
 * gdome_doc_createElementNS:
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
gdome_doc_createElementNS (GdomeDocument *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->createElementNS (self, namespaceURI, qualifiedName, exc);
}
/**
 * gdome_doc_createEntityReference:
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
gdome_doc_createEntityReference (GdomeDocument *self, GdomeDOMString *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->createEntityReference (self, name, exc);
}
/**
 * gdome_doc_createProcessingInstruction:
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
gdome_doc_createProcessingInstruction (GdomeDocument *self, GdomeDOMString *target, GdomeDOMString *data, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->createProcessingInstruction (self, target, data, exc);
}
/**
 * gdome_doc_createTextNode:
 * @self:  Document Object ref
 * @data:  The data for the node
 * @exc:  Exception Object ref
 *
 * Creates a Text node given the specified string.
 * Returns: The new TextNode object.
 */
GdomeText *
gdome_doc_createTextNode (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->createTextNode (self, data, exc);
}
/**
 * gdome_doc_getElementById:
 * @self:  Document Object ref
 * @elementId:  The unique id value for an element
 * @exc:  Exception Object ref
 *
 * Returns: the Element whose %ID is given by @elementId. If no such element
 * exists, returns %NULL. Behavior is not defined if more than one element has
 * this %ID.
 */
GdomeElement *
gdome_doc_getElementById (GdomeDocument *self, GdomeDOMString *elementId, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->getElementById (self, elementId, exc);
}
/**
 * gdome_doc_getElementsByTagName:
 * @self:  Document Object ref
 * @tagName:  The name of the tag to match on. The special value * matches
 *            all tags.
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList of all the elements with a given tag name in the order
 * in which they are encountered in a preorder traversal of the Document tree.
 */
GdomeNodeList *
gdome_doc_getElementsByTagName (GdomeDocument *self, GdomeDOMString *tagname, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->getElementsByTagName (self, tagname, exc);
}
/**
 * gdome_doc_getElementsByTagNameNS:
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
gdome_doc_getElementsByTagNameNS (GdomeDocument *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->getElementsByTagNameNS (self, namespaceURI, localName, exc);
}
/**
 * gdome_doc_importNode:
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
gdome_doc_importNode (GdomeDocument *self, GdomeNode *importedNode, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->importNode (self, importedNode, deep, exc);
}
/**
 * gdome_doc_createEvent:
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
gdome_doc_createEvent (GdomeDocument *self, GdomeDOMString *eventType, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Document *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->createEvent (self, eventType, exc);
}
/**
 * gdome_doc_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_doc_attributes (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_doc_childNodes (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_doc_firstChild (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_doc_lastChild (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_doc_localName (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_doc_namespaceURI (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_doc_nextSibling (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_doc_nodeName (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_doc_nodeType (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_doc_nodeValue (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_doc_set_nodeValue (GdomeDocument *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Document *)self)->vtab->super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_doc_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_doc_ownerDocument (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_doc_parentNode (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_doc_prefix (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_set_prefix:
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
gdome_doc_set_prefix (GdomeDocument *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Document *)self)->vtab->super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_doc_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_doc_previousSibling (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_doc_ref (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Document *)self)->vtab->super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_doc_unref (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_Document *)self)->vtab->super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_doc_query_interface (GdomeDocument *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_doc_appendChild:
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
gdome_doc_appendChild (GdomeDocument *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_doc_cloneNode:
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
gdome_doc_cloneNode (GdomeDocument *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_doc_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_doc_hasAttributes (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_doc_hasChildNodes (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_insertBefore:
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
gdome_doc_insertBefore (GdomeDocument *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_doc_isSupported:
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
gdome_doc_isSupported (GdomeDocument *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_doc_normalize:
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
gdome_doc_normalize (GdomeDocument *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Document *)self)->vtab->super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_doc_removeChild:
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
gdome_doc_removeChild (GdomeDocument *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_doc_replaceChild:
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
gdome_doc_replaceChild (GdomeDocument *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_doc_addEventListener:
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
gdome_doc_addEventListener (GdomeDocument *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Document *)self)->vtab->super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_doc_removeEventListener:
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
gdome_doc_removeEventListener (GdomeDocument *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Document *)self)->vtab->super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_doc_dispatchEvent:
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
gdome_doc_dispatchEvent (GdomeDocument *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_doc_subTreeDispatchEvent:
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
gdome_doc_subTreeDispatchEvent (GdomeDocument *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Document *)self)->vtab->super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_doc_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_doc_canAppend (GdomeDocument *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Document *)self)->vtab->super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeCharacterData interface API
 ******************************************************************************/
/**
 * gdome_cd_data:
 * @self:  The CharacterData Object ref
 * @exc:  The Exception Object ref
 *
 * Returns: the character data of the node that implements this interface.
 */
GdomeDOMString *
gdome_cd_data (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->data (self, exc);
}
/**
 * gdome_cd_set_data:
 * @self:  The CharacterData Object ref
 * @data:  The data value for this node
 * @exc:  The Exception Object ref
 *
 * Sets the data value of this node.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cd_set_data (GdomeCharacterData *self, GdomeDOMString *data, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->set_data (self, data, exc);
}
/**
 * gdome_cd_length:
 * @self:  The CharacterData Object ref
 * @exc:  The GdomeException Object ref
 *
 * Returns: The number of 16-bit units that are available. This may have the
 * value zero, i.e., CharacterData nodes may be empty.
 */
gulong
gdome_cd_length (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0L;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0L;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->length (self, exc);
}
/**
 * gdome_cd_appendData:
 * @self:  The CharacterData Object ref
 * @arg:  The string to append
 * @exc:  The Exception Object ref
 *
 * Append the string to the end of the character data of the node. Upon
 * success, data provides access to the concatenation of data and the
 * string specified.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cd_appendData (GdomeCharacterData *self, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->appendData (self, arg, exc);
}
/**
 * gdome_cd_deleteData:
 * @self:  The CharacterData Object ref
 * @offset:  The offset from which to start removing
 * @count:  The number of 16-bit units to delete.
 * @exc:  The Exception Object ref
 *
 * Remove a range of 16-bit units from the node. If the sum of @offset and
 * @count exceeds length then all 16-bit units from @offset to the end of the
 * data are deleted. Upon success, data and length reflect the change.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cd_deleteData (GdomeCharacterData *self, gulong offset, gulong count, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->deleteData (self, offset, count, exc);
}
/**
 * gdome_cd_insertData:
 * @self:  The CharacterData Object ref
 * @offset:  The character offset at which to insert
 * @arg:  The string to insert
 * @exc:  The Exception Object ref
 *
 * Insert a string at the specified 16-bit unit offset.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cd_insertData (GdomeCharacterData *self, gulong offset, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->insertData (self, offset, arg, exc);
}
/**
 * gdome_cd_replaceData:
 * @self:  The CharacterData Object ref
 * @offset:  The offset from which to start replacing
 * @count:  The number of 16-bit units to replace.
 * @arg:  The String with which the range must be replaced
 * @exc:  The GdomeException Object ref
 *
 * Replace the characters starting at the specified 16-bit unit @offset with
 * the specified string. If the sum of @offset and @count exceeds length, then
 * all 16-bit units to the end of the data are replaced;
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cd_replaceData (GdomeCharacterData *self, gulong offset, gulong count, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->replaceData (self, offset, count, arg, exc);
}
/**
 * gdome_cd_substringData:
 * @self:  The CharacterData Object ref
 * @offset:  Start offset of substring to extract
 * @count:  The number of 16-bit units to extract
 * @exc:  The Exception Object ref
 *
 * Extracts a range of data from the node.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * Returns: The specified substring. If the sum of @offset and @count exceeds
 * the length, then all 16-bit units to the end of the data are returned.
 */
GdomeDOMString *
gdome_cd_substringData (GdomeCharacterData *self, gulong offset, gulong count, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->substringData (self, offset, count, exc);
}
/**
 * gdome_cd_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_cd_attributes (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_cd_childNodes (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_cd_firstChild (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_cd_lastChild (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_cd_localName (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_cd_namespaceURI (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_cd_nextSibling (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_cd_nodeName (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_cd_nodeType (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_cd_nodeValue (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cd_set_nodeValue (GdomeCharacterData *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_cd_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_cd_ownerDocument (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_cd_parentNode (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_cd_prefix (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_set_prefix:
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
gdome_cd_set_prefix (GdomeCharacterData *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_cd_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_cd_previousSibling (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_cd_ref (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_cd_unref (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_CharacterData *)self)->vtab->super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_cd_query_interface (GdomeCharacterData *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_cd_appendChild:
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
gdome_cd_appendChild (GdomeCharacterData *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_cd_cloneNode:
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
gdome_cd_cloneNode (GdomeCharacterData *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_cd_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_cd_hasAttributes (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_cd_hasChildNodes (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_insertBefore:
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
gdome_cd_insertBefore (GdomeCharacterData *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_cd_isSupported:
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
gdome_cd_isSupported (GdomeCharacterData *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_cd_normalize:
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
gdome_cd_normalize (GdomeCharacterData *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_cd_removeChild:
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
gdome_cd_removeChild (GdomeCharacterData *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_cd_replaceChild:
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
gdome_cd_replaceChild (GdomeCharacterData *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_cd_addEventListener:
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
gdome_cd_addEventListener (GdomeCharacterData *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_cd_removeEventListener:
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
gdome_cd_removeEventListener (GdomeCharacterData *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_cd_dispatchEvent:
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
gdome_cd_dispatchEvent (GdomeCharacterData *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_cd_subTreeDispatchEvent:
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
gdome_cd_subTreeDispatchEvent (GdomeCharacterData *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CharacterData *)self)->vtab->super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_cd_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_cd_canAppend (GdomeCharacterData *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_CharacterData *)self)->vtab->super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeText interface API
 ******************************************************************************/
/**
 * gdome_t_splitText:
 * @self:  The Text Object ref
 * @offset: The 16-bit unit offset at which to split, starting from 0
 * @exc:  The GdomeException Object ref
 *
 * Breaks this node into two nodes at the specified @offset, keeping both in
 * the tree as siblings. After being split, this node will contain all the
 * content up to the @offset point. A new node of the same type, which contains
 * all the content at and after the @offset point, is returned. If the original
 * node had a parent node, the new node is inserted as the next sibling of the
 * original node. When the @offset is equal to the length of this node, the new
 * node has no data.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
 * Returns: the new node, of the same type as this node.
 */
GdomeText *
gdome_t_splitText (GdomeText *self, gulong offset, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Text *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->splitText (self, offset, exc);
}
/**
 * gdome_t_data:
 * @self:  The CharacterData Object ref
 * @exc:  The Exception Object ref
 *
 * Returns: the character data of the node that implements this interface.
 */
GdomeDOMString *
gdome_t_data (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.data ((GdomeCharacterData *)self, exc);
}
/**
 * gdome_t_set_data:
 * @self:  The CharacterData Object ref
 * @data:  The data value for this node
 * @exc:  The Exception Object ref
 *
 * Sets the data value of this node.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_t_set_data (GdomeText *self, GdomeDOMString *data, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.set_data ((GdomeCharacterData *)self, data, exc);
}
/**
 * gdome_t_length:
 * @self:  The CharacterData Object ref
 * @exc:  The GdomeException Object ref
 *
 * Returns: The number of 16-bit units that are available. This may have the
 * value zero, i.e., CharacterData nodes may be empty.
 */
gulong
gdome_t_length (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0L;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0L;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.length ((GdomeCharacterData *)self, exc);
}
/**
 * gdome_t_appendData:
 * @self:  The CharacterData Object ref
 * @arg:  The string to append
 * @exc:  The Exception Object ref
 *
 * Append the string to the end of the character data of the node. Upon
 * success, data provides access to the concatenation of data and the
 * string specified.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_t_appendData (GdomeText *self, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.appendData ((GdomeCharacterData *)self, arg, exc);
}
/**
 * gdome_t_deleteData:
 * @self:  The CharacterData Object ref
 * @offset:  The offset from which to start removing
 * @count:  The number of 16-bit units to delete.
 * @exc:  The Exception Object ref
 *
 * Remove a range of 16-bit units from the node. If the sum of @offset and
 * @count exceeds length then all 16-bit units from @offset to the end of the
 * data are deleted. Upon success, data and length reflect the change.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_t_deleteData (GdomeText *self, gulong offset, gulong count, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.deleteData ((GdomeCharacterData *)self, offset, count, exc);
}
/**
 * gdome_t_insertData:
 * @self:  The CharacterData Object ref
 * @offset:  The character offset at which to insert
 * @arg:  The string to insert
 * @exc:  The Exception Object ref
 *
 * Insert a string at the specified 16-bit unit offset.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_t_insertData (GdomeText *self, gulong offset, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.insertData ((GdomeCharacterData *)self, offset, arg, exc);
}
/**
 * gdome_t_replaceData:
 * @self:  The CharacterData Object ref
 * @offset:  The offset from which to start replacing
 * @count:  The number of 16-bit units to replace.
 * @arg:  The String with which the range must be replaced
 * @exc:  The GdomeException Object ref
 *
 * Replace the characters starting at the specified 16-bit unit @offset with
 * the specified string. If the sum of @offset and @count exceeds length, then
 * all 16-bit units to the end of the data are replaced;
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_t_replaceData (GdomeText *self, gulong offset, gulong count, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.replaceData ((GdomeCharacterData *)self, offset, count, arg, exc);
}
/**
 * gdome_t_substringData:
 * @self:  The CharacterData Object ref
 * @offset:  Start offset of substring to extract
 * @count:  The number of 16-bit units to extract
 * @exc:  The Exception Object ref
 *
 * Extracts a range of data from the node.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * Returns: The specified substring. If the sum of @offset and @count exceeds
 * the length, then all 16-bit units to the end of the data are returned.
 */
GdomeDOMString *
gdome_t_substringData (GdomeText *self, gulong offset, gulong count, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.substringData ((GdomeCharacterData *)self, offset, count, exc);
}
/**
 * gdome_t_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_t_attributes (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_t_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_t_childNodes (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_t_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_t_firstChild (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_t_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_t_lastChild (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_t_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_t_localName (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_t_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_t_namespaceURI (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_t_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_t_nextSibling (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_t_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_t_nodeName (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_t_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_t_nodeType (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_t_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_t_nodeValue (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_t_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_t_set_nodeValue (GdomeText *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_t_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_t_ownerDocument (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_t_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_t_parentNode (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_t_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_t_prefix (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_t_set_prefix:
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
gdome_t_set_prefix (GdomeText *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_t_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_t_previousSibling (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_t_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_t_ref (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_t_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_t_unref (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_Text *)self)->vtab->super.super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_t_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_t_query_interface (GdomeText *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_t_appendChild:
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
gdome_t_appendChild (GdomeText *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_t_cloneNode:
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
gdome_t_cloneNode (GdomeText *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_t_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_t_hasAttributes (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_t_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_t_hasChildNodes (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_t_insertBefore:
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
gdome_t_insertBefore (GdomeText *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_t_isSupported:
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
gdome_t_isSupported (GdomeText *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_t_normalize:
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
gdome_t_normalize (GdomeText *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_t_removeChild:
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
gdome_t_removeChild (GdomeText *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_t_replaceChild:
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
gdome_t_replaceChild (GdomeText *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_t_addEventListener:
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
gdome_t_addEventListener (GdomeText *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_t_removeEventListener:
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
gdome_t_removeEventListener (GdomeText *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_t_dispatchEvent:
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
gdome_t_dispatchEvent (GdomeText *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_t_subTreeDispatchEvent:
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
gdome_t_subTreeDispatchEvent (GdomeText *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Text *)self)->vtab->super.super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_t_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_t_canAppend (GdomeText *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Text *)self)->vtab->super.super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeCDATASection interface API
 ******************************************************************************/
/**
 * gdome_cds_splitText:
 * @self:  The Text Object ref
 * @offset: The 16-bit unit offset at which to split, starting from 0
 * @exc:  The GdomeException Object ref
 *
 * Breaks this node into two nodes at the specified @offset, keeping both in
 * the tree as siblings. After being split, this node will contain all the
 * content up to the @offset point. A new node of the same type, which contains
 * all the content at and after the @offset point, is returned. If the original
 * node had a parent node, the new node is inserted as the next sibling of the
 * original node. When the @offset is equal to the length of this node, the new
 * node has no data.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
 * Returns: the new node, of the same type as this node.
 */
GdomeText *
gdome_cds_splitText (GdomeCDATASection *self, gulong offset, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Text *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.splitText ((GdomeText *)self, offset, exc);
}
/**
 * gdome_cds_data:
 * @self:  The CharacterData Object ref
 * @exc:  The Exception Object ref
 *
 * Returns: the character data of the node that implements this interface.
 */
GdomeDOMString *
gdome_cds_data (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.data ((GdomeCharacterData *)self, exc);
}
/**
 * gdome_cds_set_data:
 * @self:  The CharacterData Object ref
 * @data:  The data value for this node
 * @exc:  The Exception Object ref
 *
 * Sets the data value of this node.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cds_set_data (GdomeCDATASection *self, GdomeDOMString *data, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.set_data ((GdomeCharacterData *)self, data, exc);
}
/**
 * gdome_cds_length:
 * @self:  The CharacterData Object ref
 * @exc:  The GdomeException Object ref
 *
 * Returns: The number of 16-bit units that are available. This may have the
 * value zero, i.e., CharacterData nodes may be empty.
 */
gulong
gdome_cds_length (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0L;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0L;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.length ((GdomeCharacterData *)self, exc);
}
/**
 * gdome_cds_appendData:
 * @self:  The CharacterData Object ref
 * @arg:  The string to append
 * @exc:  The Exception Object ref
 *
 * Append the string to the end of the character data of the node. Upon
 * success, data provides access to the concatenation of data and the
 * string specified.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cds_appendData (GdomeCDATASection *self, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.appendData ((GdomeCharacterData *)self, arg, exc);
}
/**
 * gdome_cds_deleteData:
 * @self:  The CharacterData Object ref
 * @offset:  The offset from which to start removing
 * @count:  The number of 16-bit units to delete.
 * @exc:  The Exception Object ref
 *
 * Remove a range of 16-bit units from the node. If the sum of @offset and
 * @count exceeds length then all 16-bit units from @offset to the end of the
 * data are deleted. Upon success, data and length reflect the change.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cds_deleteData (GdomeCDATASection *self, gulong offset, gulong count, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.deleteData ((GdomeCharacterData *)self, offset, count, exc);
}
/**
 * gdome_cds_insertData:
 * @self:  The CharacterData Object ref
 * @offset:  The character offset at which to insert
 * @arg:  The string to insert
 * @exc:  The Exception Object ref
 *
 * Insert a string at the specified 16-bit unit offset.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cds_insertData (GdomeCDATASection *self, gulong offset, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.insertData ((GdomeCharacterData *)self, offset, arg, exc);
}
/**
 * gdome_cds_replaceData:
 * @self:  The CharacterData Object ref
 * @offset:  The offset from which to start replacing
 * @count:  The number of 16-bit units to replace.
 * @arg:  The String with which the range must be replaced
 * @exc:  The GdomeException Object ref
 *
 * Replace the characters starting at the specified 16-bit unit @offset with
 * the specified string. If the sum of @offset and @count exceeds length, then
 * all 16-bit units to the end of the data are replaced;
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cds_replaceData (GdomeCDATASection *self, gulong offset, gulong count, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.replaceData ((GdomeCharacterData *)self, offset, count, arg, exc);
}
/**
 * gdome_cds_substringData:
 * @self:  The CharacterData Object ref
 * @offset:  Start offset of substring to extract
 * @count:  The number of 16-bit units to extract
 * @exc:  The Exception Object ref
 *
 * Extracts a range of data from the node.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * Returns: The specified substring. If the sum of @offset and @count exceeds
 * the length, then all 16-bit units to the end of the data are returned.
 */
GdomeDOMString *
gdome_cds_substringData (GdomeCDATASection *self, gulong offset, gulong count, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.substringData ((GdomeCharacterData *)self, offset, count, exc);
}
/**
 * gdome_cds_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_cds_attributes (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_cds_childNodes (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_cds_firstChild (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_cds_lastChild (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_cds_localName (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_cds_namespaceURI (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_cds_nextSibling (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_cds_nodeName (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_cds_nodeType (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_cds_nodeValue (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_cds_set_nodeValue (GdomeCDATASection *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_cds_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_cds_ownerDocument (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_cds_parentNode (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_cds_prefix (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_set_prefix:
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
gdome_cds_set_prefix (GdomeCDATASection *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_cds_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_cds_previousSibling (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_cds_ref (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_cds_unref (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_CDATASection *)self)->vtab->super.super.super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_cds_query_interface (GdomeCDATASection *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_cds_appendChild:
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
gdome_cds_appendChild (GdomeCDATASection *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_cds_cloneNode:
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
gdome_cds_cloneNode (GdomeCDATASection *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_cds_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_cds_hasAttributes (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_cds_hasChildNodes (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_insertBefore:
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
gdome_cds_insertBefore (GdomeCDATASection *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_cds_isSupported:
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
gdome_cds_isSupported (GdomeCDATASection *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_cds_normalize:
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
gdome_cds_normalize (GdomeCDATASection *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_cds_removeChild:
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
gdome_cds_removeChild (GdomeCDATASection *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_cds_replaceChild:
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
gdome_cds_replaceChild (GdomeCDATASection *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_cds_addEventListener:
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
gdome_cds_addEventListener (GdomeCDATASection *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_cds_removeEventListener:
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
gdome_cds_removeEventListener (GdomeCDATASection *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_cds_dispatchEvent:
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
gdome_cds_dispatchEvent (GdomeCDATASection *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_cds_subTreeDispatchEvent:
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
gdome_cds_subTreeDispatchEvent (GdomeCDATASection *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_CDATASection *)self)->vtab->super.super.super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_cds_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_cds_canAppend (GdomeCDATASection *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_CDATASection *)self)->vtab->super.super.super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeComment interface API
 ******************************************************************************/
/**
 * gdome_c_data:
 * @self:  The CharacterData Object ref
 * @exc:  The Exception Object ref
 *
 * Returns: the character data of the node that implements this interface.
 */
GdomeDOMString *
gdome_c_data (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.data ((GdomeCharacterData *)self, exc);
}
/**
 * gdome_c_set_data:
 * @self:  The CharacterData Object ref
 * @data:  The data value for this node
 * @exc:  The Exception Object ref
 *
 * Sets the data value of this node.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_c_set_data (GdomeComment *self, GdomeDOMString *data, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.set_data ((GdomeCharacterData *)self, data, exc);
}
/**
 * gdome_c_length:
 * @self:  The CharacterData Object ref
 * @exc:  The GdomeException Object ref
 *
 * Returns: The number of 16-bit units that are available. This may have the
 * value zero, i.e., CharacterData nodes may be empty.
 */
gulong
gdome_c_length (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0L;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0L;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.length ((GdomeCharacterData *)self, exc);
}
/**
 * gdome_c_appendData:
 * @self:  The CharacterData Object ref
 * @arg:  The string to append
 * @exc:  The Exception Object ref
 *
 * Append the string to the end of the character data of the node. Upon
 * success, data provides access to the concatenation of data and the
 * string specified.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_c_appendData (GdomeComment *self, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.appendData ((GdomeCharacterData *)self, arg, exc);
}
/**
 * gdome_c_deleteData:
 * @self:  The CharacterData Object ref
 * @offset:  The offset from which to start removing
 * @count:  The number of 16-bit units to delete.
 * @exc:  The Exception Object ref
 *
 * Remove a range of 16-bit units from the node. If the sum of @offset and
 * @count exceeds length then all 16-bit units from @offset to the end of the
 * data are deleted. Upon success, data and length reflect the change.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_c_deleteData (GdomeComment *self, gulong offset, gulong count, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.deleteData ((GdomeCharacterData *)self, offset, count, exc);
}
/**
 * gdome_c_insertData:
 * @self:  The CharacterData Object ref
 * @offset:  The character offset at which to insert
 * @arg:  The string to insert
 * @exc:  The Exception Object ref
 *
 * Insert a string at the specified 16-bit unit offset.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_c_insertData (GdomeComment *self, gulong offset, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.insertData ((GdomeCharacterData *)self, offset, arg, exc);
}
/**
 * gdome_c_replaceData:
 * @self:  The CharacterData Object ref
 * @offset:  The offset from which to start replacing
 * @count:  The number of 16-bit units to replace.
 * @arg:  The String with which the range must be replaced
 * @exc:  The GdomeException Object ref
 *
 * Replace the characters starting at the specified 16-bit unit @offset with
 * the specified string. If the sum of @offset and @count exceeds length, then
 * all 16-bit units to the end of the data are replaced;
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_c_replaceData (GdomeComment *self, gulong offset, gulong count, GdomeDOMString *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.replaceData ((GdomeCharacterData *)self, offset, count, arg, exc);
}
/**
 * gdome_c_substringData:
 * @self:  The CharacterData Object ref
 * @offset:  Start offset of substring to extract
 * @count:  The number of 16-bit units to extract
 * @exc:  The Exception Object ref
 *
 * Extracts a range of data from the node.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * Returns: The specified substring. If the sum of @offset and @count exceeds
 * the length, then all 16-bit units to the end of the data are returned.
 */
GdomeDOMString *
gdome_c_substringData (GdomeComment *self, gulong offset, gulong count, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_CharacterData *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.substringData ((GdomeCharacterData *)self, offset, count, exc);
}
/**
 * gdome_c_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_c_attributes (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_c_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_c_childNodes (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_c_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_c_firstChild (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_c_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_c_lastChild (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_c_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_c_localName (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_c_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_c_namespaceURI (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_c_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_c_nextSibling (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_c_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_c_nodeName (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_c_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_c_nodeType (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_c_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_c_nodeValue (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_c_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_c_set_nodeValue (GdomeComment *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_c_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_c_ownerDocument (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_c_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_c_parentNode (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_c_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_c_prefix (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_c_set_prefix:
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
gdome_c_set_prefix (GdomeComment *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_c_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_c_previousSibling (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_c_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_c_ref (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_c_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_c_unref (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_Comment *)self)->vtab->super.super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_c_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_c_query_interface (GdomeComment *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_c_appendChild:
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
gdome_c_appendChild (GdomeComment *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_c_cloneNode:
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
gdome_c_cloneNode (GdomeComment *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_c_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_c_hasAttributes (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_c_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_c_hasChildNodes (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_c_insertBefore:
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
gdome_c_insertBefore (GdomeComment *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_c_isSupported:
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
gdome_c_isSupported (GdomeComment *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_c_normalize:
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
gdome_c_normalize (GdomeComment *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_c_removeChild:
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
gdome_c_removeChild (GdomeComment *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_c_replaceChild:
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
gdome_c_replaceChild (GdomeComment *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_c_addEventListener:
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
gdome_c_addEventListener (GdomeComment *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_c_removeEventListener:
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
gdome_c_removeEventListener (GdomeComment *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_c_dispatchEvent:
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
gdome_c_dispatchEvent (GdomeComment *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_c_subTreeDispatchEvent:
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
gdome_c_subTreeDispatchEvent (GdomeComment *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Comment *)self)->vtab->super.super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_c_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_c_canAppend (GdomeComment *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Comment *)self)->vtab->super.super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeAttr interface API
 ******************************************************************************/
/**
 * gdome_a_name:
 * @self:  Attr Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the qualified name of this attribute.
 */
GdomeDOMString *
gdome_a_name (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Attr *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->name (self, exc);
}
/**
 * gdome_a_ownerElement:
 * @self:  Attr Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the Element node this attribute is attached to or %NULL if this
 * attribute is not in use.
 */
GdomeElement *
gdome_a_ownerElement (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Attr *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->ownerElement (self, exc);
}
/**
 * gdome_a_specified:
 * @self:  Attr Object ref
 * @exc:  Exception Object ref
 *
 * NOT IMPLEMENTED
 * Returns: %TRUE
 */
GdomeBoolean
gdome_a_specified (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Attr *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->specified (self, exc);
}
/**
 * gdome_a_value:
 * @self:  Attr Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this attribute.
 */
GdomeDOMString *
gdome_a_value (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Attr *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->value (self, exc);
}
/**
 * gdome_a_set_value:
 * @self:  Attr Object ref
 * @value:  The new value to set
 * @exc:  Exception Object ref
 *
 * Sets the value of this attribute.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_a_set_value (GdomeAttr *self, GdomeDOMString *value, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Attr *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Attr *)self)->vtab->set_value (self, value, exc);
}
/**
 * gdome_a_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_a_attributes (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_a_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_a_childNodes (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_a_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_a_firstChild (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_a_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_a_lastChild (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_a_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_a_localName (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_a_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_a_namespaceURI (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_a_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_a_nextSibling (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_a_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_a_nodeName (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_a_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_a_nodeType (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_a_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_a_nodeValue (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_a_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_a_set_nodeValue (GdomeAttr *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Attr *)self)->vtab->super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_a_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_a_ownerDocument (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_a_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_a_parentNode (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_a_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_a_prefix (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_a_set_prefix:
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
gdome_a_set_prefix (GdomeAttr *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Attr *)self)->vtab->super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_a_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_a_previousSibling (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_a_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_a_ref (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Attr *)self)->vtab->super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_a_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_a_unref (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_Attr *)self)->vtab->super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_a_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_a_query_interface (GdomeAttr *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_a_appendChild:
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
gdome_a_appendChild (GdomeAttr *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_a_cloneNode:
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
gdome_a_cloneNode (GdomeAttr *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_a_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_a_hasAttributes (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_a_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_a_hasChildNodes (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_a_insertBefore:
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
gdome_a_insertBefore (GdomeAttr *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_a_isSupported:
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
gdome_a_isSupported (GdomeAttr *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_a_normalize:
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
gdome_a_normalize (GdomeAttr *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Attr *)self)->vtab->super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_a_removeChild:
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
gdome_a_removeChild (GdomeAttr *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_a_replaceChild:
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
gdome_a_replaceChild (GdomeAttr *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_a_addEventListener:
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
gdome_a_addEventListener (GdomeAttr *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Attr *)self)->vtab->super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_a_removeEventListener:
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
gdome_a_removeEventListener (GdomeAttr *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Attr *)self)->vtab->super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_a_dispatchEvent:
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
gdome_a_dispatchEvent (GdomeAttr *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_a_subTreeDispatchEvent:
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
gdome_a_subTreeDispatchEvent (GdomeAttr *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Attr *)self)->vtab->super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_a_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_a_canAppend (GdomeAttr *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Attr *)self)->vtab->super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeElement interface API
 ******************************************************************************/
/**
 * gdome_el_tagName:
 * @self:  Element Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the qualified name of the element.
 */
GdomeDOMString *
gdome_el_tagName (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->tagName (self, exc);
}
/**
 * gdome_el_getAttribute:
 * @self:  Element Objects ref
 * @name:  The name of attribute of which the value is wanted
 * @exc:  Exception Object ref
 *
 * Retrieves an attribute value by name.
 * Returns: the Attr value as a string, or the empty string if that attribute
 * does not have a specified or default value.
 */
GdomeDOMString *
gdome_el_getAttribute (GdomeElement *self, GdomeDOMString *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->getAttribute (self, name, exc);
}
/**
 * gdome_el_getAttributeNS:
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
gdome_el_getAttributeNS (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->getAttributeNS (self, namespaceURI, localName, exc);
}
/**
 * gdome_el_getAttributeNode:
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
gdome_el_getAttributeNode (GdomeElement *self, GdomeDOMString *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->getAttributeNode (self, name, exc);
}
/**
 * gdome_el_getAttributeNodeNS:
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
gdome_el_getAttributeNodeNS (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->getAttributeNodeNS (self, namespaceURI, localName, exc);
}
/**
 * gdome_el_getElementsByTagName:
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
gdome_el_getElementsByTagName (GdomeElement *self, GdomeDOMString *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->getElementsByTagName (self, name, exc);
}
/**
 * gdome_el_getElementsByTagNameNS:
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
gdome_el_getElementsByTagNameNS (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->getElementsByTagNameNS (self, namespaceURI, localName, exc);
}
/**
 * gdome_el_hasAttribute:
 * @self:  Element Objects ref
 * @name:  The name of the attribute to look for.
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE when an attribute with a given name is specified on this
 * element or has a default value, %FALSE otherwise.
 */
GdomeBoolean
gdome_el_hasAttribute (GdomeElement *self, GdomeDOMString *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->hasAttribute (self, name, exc);
}
/**
 * gdome_el_hasAttributeNS:
 * @self:  Element Objects ref
 * @namespaceURI:  The namespaceURI of the attribute to look for.
 * @localName:  The localName of the attribute to look for.
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE when an attribute with a given local name and namespace URI
 * is specified on this element or has a default value, %FALSE otherwise.
 */
GdomeBoolean
gdome_el_hasAttributeNS (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->hasAttributeNS (self, namespaceURI, localName, exc);
}
/**
 * gdome_el_removeAttribute:
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
gdome_el_removeAttribute (GdomeElement *self, GdomeDOMString *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Element *)self)->vtab->removeAttribute (self, name, exc);
}
/**
 * gdome_el_removeAttributeNS:
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
gdome_el_removeAttributeNS (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Element *)self)->vtab->removeAttributeNS (self, namespaceURI, localName, exc);
}
/**
 * gdome_el_removeAttributeNode:
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
gdome_el_removeAttributeNode (GdomeElement *self, GdomeAttr *oldAttr, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->removeAttributeNode (self, oldAttr, exc);
}
/**
 * gdome_el_setAttribute:
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
gdome_el_setAttribute (GdomeElement *self, GdomeDOMString *name, GdomeDOMString *value, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Element *)self)->vtab->setAttribute (self, name, value, exc);
}
/**
 * gdome_el_setAttributeNS:
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
gdome_el_setAttributeNS (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeDOMString *value, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Element *)self)->vtab->setAttributeNS (self, namespaceURI, qualifiedName, value, exc);
}
/**
 * gdome_el_setAttributeNode:
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
gdome_el_setAttributeNode (GdomeElement *self, GdomeAttr *newAttr, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->setAttributeNode (self, newAttr, exc);
}
/**
 * gdome_el_setAttributeNodeNS:
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
gdome_el_setAttributeNodeNS (GdomeElement *self, GdomeAttr *newAttr, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Element *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->setAttributeNodeNS (self, newAttr, exc);
}
/**
 * gdome_el_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_el_attributes (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_el_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_el_childNodes (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_el_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_el_firstChild (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_el_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_el_lastChild (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_el_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_el_localName (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_el_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_el_namespaceURI (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_el_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_el_nextSibling (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_el_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_el_nodeName (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_el_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_el_nodeType (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_el_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_el_nodeValue (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_el_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_el_set_nodeValue (GdomeElement *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Element *)self)->vtab->super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_el_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_el_ownerDocument (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_el_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_el_parentNode (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_el_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_el_prefix (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_el_set_prefix:
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
gdome_el_set_prefix (GdomeElement *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Element *)self)->vtab->super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_el_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_el_previousSibling (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_el_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_el_ref (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Element *)self)->vtab->super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_el_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_el_unref (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_Element *)self)->vtab->super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_el_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_el_query_interface (GdomeElement *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_el_appendChild:
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
gdome_el_appendChild (GdomeElement *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_el_cloneNode:
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
gdome_el_cloneNode (GdomeElement *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_el_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_el_hasAttributes (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_el_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_el_hasChildNodes (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_el_insertBefore:
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
gdome_el_insertBefore (GdomeElement *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_el_isSupported:
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
gdome_el_isSupported (GdomeElement *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_el_normalize:
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
gdome_el_normalize (GdomeElement *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Element *)self)->vtab->super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_el_removeChild:
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
gdome_el_removeChild (GdomeElement *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_el_replaceChild:
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
gdome_el_replaceChild (GdomeElement *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_el_addEventListener:
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
gdome_el_addEventListener (GdomeElement *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Element *)self)->vtab->super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_el_removeEventListener:
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
gdome_el_removeEventListener (GdomeElement *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Element *)self)->vtab->super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_el_dispatchEvent:
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
gdome_el_dispatchEvent (GdomeElement *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_el_subTreeDispatchEvent:
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
gdome_el_subTreeDispatchEvent (GdomeElement *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Element *)self)->vtab->super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_el_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_el_canAppend (GdomeElement *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Element *)self)->vtab->super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeDocumentType interface API
 ******************************************************************************/
/**
 * gdome_dt_entities:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the general entities, both external and
 * internal, declared in the DTD. Parameter entities are not contained.
 * Duplicates are discarded.
 */
GdomeNamedNodeMap *
gdome_dt_entities (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_DocumentType *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->entities (self, exc);
}
/**
 * gdome_dt_internalSubset:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: The internal subset as a string, or %NULL if there is none or on
 * error.
 */
GdomeDOMString *
gdome_dt_internalSubset (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_DocumentType *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->internalSubset (self, exc);
}
/**
 * gdome_dt_name:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of DTD; i.e., the name immediately following the %DOCTYPE
 * keyword.
 */
GdomeDOMString *
gdome_dt_name (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_DocumentType *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->name (self, exc);
}
/**
 * gdome_dt_notations:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the notations declared in the DTD.
 * Duplicates are discarded. Every node in this map also implements the
 * Notation interface.
 */
GdomeNamedNodeMap *
gdome_dt_notations (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_DocumentType *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->notations (self, exc);
}
/**
 * gdome_dt_publicId:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the public identifier of the external subset.
 */
GdomeDOMString *
gdome_dt_publicId (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_DocumentType *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->publicId (self, exc);
}
/**
 * gdome_dt_systemId:
 * @self:  DocumentType Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the public identifier of the external subset.
 */
GdomeDOMString *
gdome_dt_systemId (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_DocumentType *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->systemId (self, exc);
}
/**
 * gdome_dt_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_dt_attributes (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_dt_childNodes (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_dt_firstChild (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_dt_lastChild (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_dt_localName (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_dt_namespaceURI (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_dt_nextSibling (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_dt_nodeName (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_dt_nodeType (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_dt_nodeValue (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_dt_set_nodeValue (GdomeDocumentType *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentType *)self)->vtab->super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_dt_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_dt_ownerDocument (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_dt_parentNode (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_dt_prefix (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_set_prefix:
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
gdome_dt_set_prefix (GdomeDocumentType *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentType *)self)->vtab->super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_dt_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_dt_previousSibling (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_dt_ref (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentType *)self)->vtab->super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_dt_unref (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_DocumentType *)self)->vtab->super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_dt_query_interface (GdomeDocumentType *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_dt_appendChild:
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
gdome_dt_appendChild (GdomeDocumentType *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_dt_cloneNode:
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
gdome_dt_cloneNode (GdomeDocumentType *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_dt_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_dt_hasAttributes (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_dt_hasChildNodes (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_insertBefore:
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
gdome_dt_insertBefore (GdomeDocumentType *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_dt_isSupported:
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
gdome_dt_isSupported (GdomeDocumentType *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_dt_normalize:
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
gdome_dt_normalize (GdomeDocumentType *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentType *)self)->vtab->super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_dt_removeChild:
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
gdome_dt_removeChild (GdomeDocumentType *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_dt_replaceChild:
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
gdome_dt_replaceChild (GdomeDocumentType *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_dt_addEventListener:
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
gdome_dt_addEventListener (GdomeDocumentType *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentType *)self)->vtab->super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_dt_removeEventListener:
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
gdome_dt_removeEventListener (GdomeDocumentType *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentType *)self)->vtab->super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_dt_dispatchEvent:
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
gdome_dt_dispatchEvent (GdomeDocumentType *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_dt_subTreeDispatchEvent:
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
gdome_dt_subTreeDispatchEvent (GdomeDocumentType *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_DocumentType *)self)->vtab->super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_dt_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_dt_canAppend (GdomeDocumentType *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_DocumentType *)self)->vtab->super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeNotation interface API
 ******************************************************************************/
/**
 * gdome_not_publicId:
 * @self:  Notations Object ref
 * @exc:   Exception Object ref
 *
 * Returns: the public identifier of this notation. If the public identifier
 * was not specified, this is NULL.
 */
GdomeDOMString *
gdome_not_publicId (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Notation *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->publicId (self, exc);
}
/**
 * gdome_not_systemId:
 * @self:  Notations Object ref
 * @exc:   Exception Object ref
 *
 * Returns: the system identifier of this notation. If the system identifier
 * was not specified, this is NULL.
 */
GdomeDOMString *
gdome_not_systemId (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Notation *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->systemId (self, exc);
}
/**
 * gdome_not_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_not_attributes (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_not_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_not_childNodes (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_not_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_not_firstChild (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_not_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_not_lastChild (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_not_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_not_localName (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_not_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_not_namespaceURI (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_not_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_not_nextSibling (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_not_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_not_nodeName (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_not_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_not_nodeType (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_not_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_not_nodeValue (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_not_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_not_set_nodeValue (GdomeNotation *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Notation *)self)->vtab->super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_not_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_not_ownerDocument (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_not_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_not_parentNode (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_not_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_not_prefix (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_not_set_prefix:
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
gdome_not_set_prefix (GdomeNotation *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Notation *)self)->vtab->super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_not_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_not_previousSibling (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_not_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_not_ref (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Notation *)self)->vtab->super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_not_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_not_unref (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_Notation *)self)->vtab->super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_not_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_not_query_interface (GdomeNotation *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_not_appendChild:
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
gdome_not_appendChild (GdomeNotation *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_not_cloneNode:
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
gdome_not_cloneNode (GdomeNotation *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_not_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_not_hasAttributes (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_not_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_not_hasChildNodes (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_not_insertBefore:
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
gdome_not_insertBefore (GdomeNotation *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_not_isSupported:
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
gdome_not_isSupported (GdomeNotation *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_not_normalize:
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
gdome_not_normalize (GdomeNotation *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Notation *)self)->vtab->super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_not_removeChild:
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
gdome_not_removeChild (GdomeNotation *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_not_replaceChild:
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
gdome_not_replaceChild (GdomeNotation *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_not_addEventListener:
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
gdome_not_addEventListener (GdomeNotation *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Notation *)self)->vtab->super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_not_removeEventListener:
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
gdome_not_removeEventListener (GdomeNotation *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Notation *)self)->vtab->super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_not_dispatchEvent:
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
gdome_not_dispatchEvent (GdomeNotation *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_not_subTreeDispatchEvent:
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
gdome_not_subTreeDispatchEvent (GdomeNotation *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Notation *)self)->vtab->super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_not_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_not_canAppend (GdomeNotation *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Notation *)self)->vtab->super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeEntity interface API
 ******************************************************************************/
/**
 * gdome_ent_publicId:
 * @self:  The Entity Object ref
 * @exc:  The Exception Object ref
 *
 * Returns: the public identifier associated with the entity, if specified.
 * If the public identifier was not specified, this is %NULL.
 */
GdomeDOMString *
gdome_ent_publicId (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Entity *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->publicId (self, exc);
}
/**
 * gdome_ent_systemId:
 * @self:  The Entity Object ref
 * @exc:  The Exception Object ref
 *
 * Returns: the system identifier associated with the entity, if specified.
 * If the system identifier was not specified, this is %NULL.
 */
GdomeDOMString *
gdome_ent_systemId (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Entity *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->systemId (self, exc);
}
/**
 * gdome_ent_notationName:
 * @self:  The Entity Object ref
 * @exc:  The Exception Object ref
 *
 * Returns: for unparsed entities, the name of the notation for the entity.
 * For parsed entities, this is %NULL.
 */
GdomeDOMString *
gdome_ent_notationName (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Entity *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->notationName (self, exc);
}
/**
 * gdome_ent_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_ent_attributes (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_ent_childNodes (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_ent_firstChild (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_ent_lastChild (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_ent_localName (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_ent_namespaceURI (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_ent_nextSibling (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_ent_nodeName (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_ent_nodeType (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_ent_nodeValue (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_ent_set_nodeValue (GdomeEntity *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Entity *)self)->vtab->super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_ent_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_ent_ownerDocument (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_ent_parentNode (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_ent_prefix (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_set_prefix:
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
gdome_ent_set_prefix (GdomeEntity *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Entity *)self)->vtab->super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_ent_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_ent_previousSibling (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_ent_ref (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Entity *)self)->vtab->super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_ent_unref (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_Entity *)self)->vtab->super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_ent_query_interface (GdomeEntity *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_ent_appendChild:
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
gdome_ent_appendChild (GdomeEntity *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_ent_cloneNode:
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
gdome_ent_cloneNode (GdomeEntity *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_ent_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_ent_hasAttributes (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_ent_hasChildNodes (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_insertBefore:
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
gdome_ent_insertBefore (GdomeEntity *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_ent_isSupported:
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
gdome_ent_isSupported (GdomeEntity *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_ent_normalize:
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
gdome_ent_normalize (GdomeEntity *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Entity *)self)->vtab->super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_ent_removeChild:
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
gdome_ent_removeChild (GdomeEntity *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_ent_replaceChild:
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
gdome_ent_replaceChild (GdomeEntity *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_ent_addEventListener:
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
gdome_ent_addEventListener (GdomeEntity *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Entity *)self)->vtab->super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_ent_removeEventListener:
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
gdome_ent_removeEventListener (GdomeEntity *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Entity *)self)->vtab->super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_ent_dispatchEvent:
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
gdome_ent_dispatchEvent (GdomeEntity *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_ent_subTreeDispatchEvent:
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
gdome_ent_subTreeDispatchEvent (GdomeEntity *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_Entity *)self)->vtab->super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_ent_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_ent_canAppend (GdomeEntity *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_Entity *)self)->vtab->super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeEntityReference interface API
 ******************************************************************************/
/**
 * gdome_er_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_er_attributes (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_er_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_er_childNodes (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_er_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_er_firstChild (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_er_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_er_lastChild (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_er_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_er_localName (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_er_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_er_namespaceURI (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_er_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_er_nextSibling (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_er_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_er_nodeName (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_er_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_er_nodeType (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_er_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_er_nodeValue (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_er_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_er_set_nodeValue (GdomeEntityReference *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_EntityReference *)self)->vtab->super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_er_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_er_ownerDocument (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_er_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_er_parentNode (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_er_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_er_prefix (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_er_set_prefix:
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
gdome_er_set_prefix (GdomeEntityReference *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_EntityReference *)self)->vtab->super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_er_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_er_previousSibling (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_er_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_er_ref (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_EntityReference *)self)->vtab->super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_er_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_er_unref (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_EntityReference *)self)->vtab->super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_er_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_er_query_interface (GdomeEntityReference *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_er_appendChild:
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
gdome_er_appendChild (GdomeEntityReference *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_er_cloneNode:
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
gdome_er_cloneNode (GdomeEntityReference *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_er_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_er_hasAttributes (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_er_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_er_hasChildNodes (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_er_insertBefore:
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
gdome_er_insertBefore (GdomeEntityReference *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_er_isSupported:
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
gdome_er_isSupported (GdomeEntityReference *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_er_normalize:
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
gdome_er_normalize (GdomeEntityReference *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_EntityReference *)self)->vtab->super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_er_removeChild:
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
gdome_er_removeChild (GdomeEntityReference *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_er_replaceChild:
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
gdome_er_replaceChild (GdomeEntityReference *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_er_addEventListener:
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
gdome_er_addEventListener (GdomeEntityReference *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_EntityReference *)self)->vtab->super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_er_removeEventListener:
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
gdome_er_removeEventListener (GdomeEntityReference *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_EntityReference *)self)->vtab->super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_er_dispatchEvent:
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
gdome_er_dispatchEvent (GdomeEntityReference *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_er_subTreeDispatchEvent:
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
gdome_er_subTreeDispatchEvent (GdomeEntityReference *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_EntityReference *)self)->vtab->super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_er_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_er_canAppend (GdomeEntityReference *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_EntityReference *)self)->vtab->super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeProcessingInstruction interface API
 ******************************************************************************/
/**
 * gdome_pi_data:
 * @self:  ProcessingInstruction Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the content of this processing instruction. This is from the first
 * non white space character after the target to the character immediately
 * preceding the %?>.
 */
GdomeDOMString *
gdome_pi_data (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_EntityReference *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->data (self, exc);
}
/**
 * gdome_pi_set_data:
 * @self:  ProcessingInstruction Object ref
 * @data:  The string of data to be set
 * @exc:  Exception Object ref
 *
 * Set the content of this processing instruction.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_pi_set_data (GdomeProcessingInstruction *self, GdomeDOMString *data, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_EntityReference *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_ProcessingInstruction *)self)->vtab->set_data (self, data, exc);
}
/**
 * gdome_pi_target:
 * @self:  ProcessingInstruction Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the target of this processing instruction. XML defines this as being
 * the first token following the markup that begins the processing instruction.
 */
GdomeDOMString *
gdome_pi_target (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_EntityReference *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->target (self, exc);
}
/**
 * gdome_pi_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_pi_attributes (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_pi_childNodes (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_pi_firstChild (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_pi_lastChild (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_pi_localName (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_pi_namespaceURI (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_pi_nextSibling (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_pi_nodeName (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_pi_nodeType (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_pi_nodeValue (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_pi_set_nodeValue (GdomeProcessingInstruction *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_ProcessingInstruction *)self)->vtab->super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_pi_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_pi_ownerDocument (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_pi_parentNode (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_pi_prefix (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_set_prefix:
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
gdome_pi_set_prefix (GdomeProcessingInstruction *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_ProcessingInstruction *)self)->vtab->super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_pi_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_pi_previousSibling (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_pi_ref (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_ProcessingInstruction *)self)->vtab->super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_pi_unref (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_ProcessingInstruction *)self)->vtab->super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_pi_query_interface (GdomeProcessingInstruction *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_pi_appendChild:
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
gdome_pi_appendChild (GdomeProcessingInstruction *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_pi_cloneNode:
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
gdome_pi_cloneNode (GdomeProcessingInstruction *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_pi_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_pi_hasAttributes (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_pi_hasChildNodes (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_insertBefore:
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
gdome_pi_insertBefore (GdomeProcessingInstruction *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_pi_isSupported:
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
gdome_pi_isSupported (GdomeProcessingInstruction *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_pi_normalize:
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
gdome_pi_normalize (GdomeProcessingInstruction *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_ProcessingInstruction *)self)->vtab->super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_pi_removeChild:
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
gdome_pi_removeChild (GdomeProcessingInstruction *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_pi_replaceChild:
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
gdome_pi_replaceChild (GdomeProcessingInstruction *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_pi_addEventListener:
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
gdome_pi_addEventListener (GdomeProcessingInstruction *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_ProcessingInstruction *)self)->vtab->super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_pi_removeEventListener:
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
gdome_pi_removeEventListener (GdomeProcessingInstruction *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_ProcessingInstruction *)self)->vtab->super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_pi_dispatchEvent:
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
gdome_pi_dispatchEvent (GdomeProcessingInstruction *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_pi_subTreeDispatchEvent:
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
gdome_pi_subTreeDispatchEvent (GdomeProcessingInstruction *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_ProcessingInstruction *)self)->vtab->super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_pi_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_pi_canAppend (GdomeProcessingInstruction *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_ProcessingInstruction *)self)->vtab->super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeXPathNamespace interface API
 ******************************************************************************/
/**
 * gdome_xpns_ownerElement:
 * @self:  XPathNamespace Object ref
 * @exc:  Exception Object ref
 *
 * NOT IMPLEMENTED
 * Returns: %NULL
 */
GdomeElement *
gdome_xpns_ownerElement (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_EntityReference *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->ownerElement (self, exc);
}
/**
 * gdome_xpns_attributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NamedNodeMap containing the attributes of this node (if it is an
 * Element) or %NULL otherwise.
 */
GdomeNamedNodeMap *
gdome_xpns_attributes (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.attributes ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_childNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a NodeList that contains all children of this node. If there are no
 * children, this is a NodeList containing no nodes.  Note that attributes are
 * not considered to be children of element nodes - use gdome_n_attributes to
 * obtain the attributes.
 */
GdomeNodeList *
gdome_xpns_childNodes (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.childNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_firstChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 * 
 * Returns: the first child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_xpns_firstChild (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.firstChild ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_lastChild:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the last child of this node. If there is no such node,
 * this returns %NULL.
 */
GdomeNode *
gdome_xpns_lastChild (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.lastChild ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_localName:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the local part of the qualified name of this node.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_xpns_localName (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.localName ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_namespaceURI:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace URI of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_xpns_namespaceURI (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.namespaceURI ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_nextSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately following this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_xpns_nextSibling (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.nextSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_nodeName:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of this node, depending on its type.
 */
GdomeDOMString *
gdome_xpns_nodeName (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.nodeName ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_nodeType:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: an integer bigger than %0 representing the type of the underlying
 * object or %0 on error.
 */
unsigned short
gdome_xpns_nodeType (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.nodeType ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_nodeValue:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the value of this node, depending on its type.
 */
GdomeDOMString *
gdome_xpns_nodeValue (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.nodeValue ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_set_nodeValue:
 * @self:  Node Object ref
 * @nodeValue:  The new value for this node.
 * @exc:  Exception Object ref
 *
 * Sets The value of this node, depending on its type.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_xpns_set_nodeValue (GdomeXPathNamespace *self, GdomeDOMString *nodeValue, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_XPathNamespace *)self)->vtab->super.set_nodeValue ((GdomeNode *)self, nodeValue, exc);
}
/**
 * gdome_xpns_ownerDocument:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the documment object associated with this node. This is also the
 * Document object used to create new nodes. When this node is a Document or a
 * DocumentType which is not used with any Document yet, this is %NULL.
 */
GdomeDocument *
gdome_xpns_ownerDocument (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.ownerDocument ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_parentNode:
 * @self: Node Object ref
 * @exc: Exception Object ref
 *
 * Returns: the parent of this node. All nodes, except Attr,
 * Document, DocumentFragment, Entity, and Notation may have a parent. However,
 * if a node has just been created and not yet added to the tree, or if it has
 * been removed from the tree, this is %NULL.
 */
GdomeNode *
gdome_xpns_parentNode (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.parentNode ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_prefix:
 * @self:  Node Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the namespace prefix of this node, or %NULL if it is unspecified.
 * For nodes of any type other than %GDOME_ELEMENT_NODE and
 * %GDOME_ATTRIBUTE_NODE and nodes created with a DOM Level 1 method, this is
 * always %NULL.
 */
GdomeDOMString *
gdome_xpns_prefix (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.prefix ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_set_prefix:
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
gdome_xpns_set_prefix (GdomeXPathNamespace *self, GdomeDOMString *prefix, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_XPathNamespace *)self)->vtab->super.set_prefix ((GdomeNode *)self, prefix, exc);
}
/**
 * gdome_xpns_previousSibling:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the node immediately preceding this node. If there is no such
 * node, this returns %NULL.
 */
GdomeNode *
gdome_xpns_previousSibling (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.previousSibling ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_ref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Node.
 */
void
gdome_xpns_ref (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_XPathNamespace *)self)->vtab->super.ref ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_unref:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Node. Free the Node structure
 * if the Node will have zero reference.
 */
void
gdome_xpns_unref (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)self)->n == NULL) {
		if (((Gdome_xml_Node *)self)->refcnt > 0)
			((Gdome_xml_Node *)self)->refcnt--;

		if (((Gdome_xml_Node *)self)->refcnt == 0 && ((Gdome_xml_Node *)self)->ll == NULL) {
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODE);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_XPathNamespace *)self)->vtab->super.unref ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xpns_query_interface (GdomeXPathNamespace *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.query_interface ((GdomeNode *)self, interface, exc);
}
/**
 * gdome_xpns_appendChild:
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
gdome_xpns_appendChild (GdomeXPathNamespace *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.appendChild ((GdomeNode *)self, newChild, exc);
}
/**
 * gdome_xpns_cloneNode:
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
gdome_xpns_cloneNode (GdomeXPathNamespace *self, GdomeBoolean deep, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.cloneNode ((GdomeNode *)self, deep, exc);
}
/**
 * gdome_xpns_hasAttributes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any attributes, %FALSE otherwise.
 */
GdomeBoolean
gdome_xpns_hasAttributes (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.hasAttributes ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_hasChildNodes:
 * @self:  Node Object ref
 * @exc:  Exception Object ref
 *
 * Returns: %TRUE if this node has any children, %FALSE otherwise.
 */
GdomeBoolean
gdome_xpns_hasChildNodes (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.hasChildNodes ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_insertBefore:
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
gdome_xpns_insertBefore (GdomeXPathNamespace *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.insertBefore ((GdomeNode *)self, newChild, refChild, exc);
}
/**
 * gdome_xpns_isSupported:
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
gdome_xpns_isSupported (GdomeXPathNamespace *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.isSupported ((GdomeNode *)self, feature, version, exc);
}
/**
 * gdome_xpns_normalize:
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
gdome_xpns_normalize (GdomeXPathNamespace *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_XPathNamespace *)self)->vtab->super.normalize ((GdomeNode *)self, exc);
}
/**
 * gdome_xpns_removeChild:
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
gdome_xpns_removeChild (GdomeXPathNamespace *self, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.removeChild ((GdomeNode *)self, oldChild, exc);
}
/**
 * gdome_xpns_replaceChild:
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
gdome_xpns_replaceChild (GdomeXPathNamespace *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.replaceChild ((GdomeNode *)self, newChild, oldChild, exc);
}
/**
 * gdome_xpns_addEventListener:
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
gdome_xpns_addEventListener (GdomeXPathNamespace *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_XPathNamespace *)self)->vtab->super.addEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_xpns_removeEventListener:
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
gdome_xpns_removeEventListener (GdomeXPathNamespace *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_XPathNamespace *)self)->vtab->super.removeEventListener ((GdomeNode *)self, type, listener, useCapture, exc);
}
/**
 * gdome_xpns_dispatchEvent:
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
gdome_xpns_dispatchEvent (GdomeXPathNamespace *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.dispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_xpns_subTreeDispatchEvent:
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
gdome_xpns_subTreeDispatchEvent (GdomeXPathNamespace *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_XPathNamespace *)self)->vtab->super.subTreeDispatchEvent ((GdomeNode *)self, evt, exc);
}
/**
 * gdome_xpns_canAppend:
 * @self:  Node Object ref
 * @newChild:  The Node Object ref of the node to test
 * @exc:  Exception Object ref
 *
 * Tests if a @newChild can be added in the child list of this node.
 * Returns: %TRUE if @newChild can be added, %FALSE otherwise.
 */
GdomeBoolean
gdome_xpns_canAppend (GdomeXPathNamespace *self, GdomeNode *newChild, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	if (((Gdome_xml_Node *)self)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_xml_XPathNamespace *)self)->vtab->super.canAppend ((GdomeNode *)self, newChild, exc);
}

/******************************************************************************
          GdomeNodeList interface API
 ******************************************************************************/
/**
 * gdome_nl_length:
 * @self:  NodeList Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the number of nodes in the list. The range of valid child node
 * indices is 0 to length-1 inclusive.
 */
gulong
gdome_nl_length (GdomeNodeList *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0L;
	}
	if (((Gdome_xml_Node *)((Gdome_xml_NodeList *)self)->root)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0L;
	}
	*exc = 0;
	return ((Gdome_xml_NodeList *)self)->vtab->length (self, exc);
}
/**
 * gdome_nl_ref:
 * @self:  NodeList Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified NodeList.
 */
void
gdome_nl_ref (GdomeNodeList *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_Node *)((Gdome_xml_NodeList *)self)->root)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_NodeList *)self)->vtab->ref (self, exc);
}
/**
 * gdome_nl_unref:
 * @self:  NodeList Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified NodeList. Free the NodeList
 * structure if the specified NodeList will have zero reference.
 */
void
gdome_nl_unref (GdomeNodeList *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_Node *)((Gdome_xml_NodeList *)self)->root)->n == NULL) {
		if (--((Gdome_xml_NodeList *)self)->refcnt == 0) {
			gdome_n_unref (((Gdome_xml_NodeList *)self)->root, exc);
			if(((Gdome_xml_NodeList *)self)->tagName != NULL)
				gdome_str_unref(((Gdome_xml_NodeList *)self)->tagName);
			if(((Gdome_xml_NodeList *)self)->tagURI != NULL)
				gdome_str_unref(((Gdome_xml_NodeList *)self)->tagURI);
#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODELIST);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_NodeList *)self)->vtab->unref (self, exc);
}
/**
 * gdome_nl_query_interface:
 * @self:  NodeList Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_nl_query_interface (GdomeNodeList *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)((Gdome_xml_NodeList *)self)->root)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_NodeList *)self)->vtab->query_interface (self, interface, exc);
}
/**
 * gdome_nl_item:
 * @self:  NodeList Object ref
 * @index:  Index into the collection
 * @exc:  Exception Object ref
 *
 * Returns: the indexth item in the collection. If @index is greater than or
 * equal to the number of nodes in the list, this returns %NULL.
 */
GdomeNode *
gdome_nl_item (GdomeNodeList *self, gulong index, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_Node *)((Gdome_xml_NodeList *)self)->root)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_NodeList *)self)->vtab->item (self, index, exc);
}

/******************************************************************************
          GdomeNamedNodeMap interface API
 ******************************************************************************/
/**
 * gdome_nnm_length:
 * @self:  NamedNodeMap Objects ref
 * @exc:  Exception Object ref
 *
 * Returns: the number of nodes in this map. The range of valid child node
 * indices is 0 to length-1 inclusive.
 */
gulong
gdome_nnm_length (GdomeNamedNodeMap *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0L;
	}
	if (((Gdome_xml_NamedNodeMap *)self)->type == GDOME_ATTRIBUTE_NODE && ((Gdome_xml_Node *)((Gdome_xml_NamedNodeMap *)self)->elem)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return 0L;
	}
	*exc = 0;
	return ((Gdome_xml_NamedNodeMap *)self)->vtab->length (self, exc);
}
/**
 * gdome_nnm_ref:
 * @self:  NodeList Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified NamedNodeMap.
 */
void
gdome_nnm_ref (GdomeNamedNodeMap *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	if (((Gdome_xml_NamedNodeMap *)self)->type == GDOME_ATTRIBUTE_NODE && ((Gdome_xml_Node *)((Gdome_xml_NamedNodeMap *)self)->elem)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return;
	}
	*exc = 0;
	((Gdome_xml_NamedNodeMap *)self)->vtab->ref (self, exc);
}
/**
 * gdome_nnm_unref:
 * @self:  NodeList Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified NamedNodeMap. Free the
 * NamedNodeMap structure if the specified NamedNodeMap will have zero
 * reference.
 */
void
gdome_nnm_unref (GdomeNamedNodeMap *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	if (((Gdome_xml_NamedNodeMap *)self)->type == GDOME_ATTRIBUTE_NODE && ((Gdome_xml_Node *)((Gdome_xml_NamedNodeMap *)self)->elem)->n == NULL) {
		if (--((Gdome_xml_NamedNodeMap *)self)->refcnt == 0) {
			gdome_doc_unref (((Gdome_xml_NamedNodeMap *)self)->doc, exc);
			gdome_el_unref (((Gdome_xml_NamedNodeMap *)self)->elem, exc);

#ifdef DEBUG_REFCNT
			gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NAMEDNODEMAP);
#endif
			g_free (self);
		}
		return;
	}
	((Gdome_xml_NamedNodeMap *)self)->vtab->unref (self, exc);
}
/**
 * gdome_nnm_query_interface:
 * @self:  NamedNodeMap Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_nnm_query_interface (GdomeNamedNodeMap *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_NamedNodeMap *)self)->type == GDOME_ATTRIBUTE_NODE && ((Gdome_xml_Node *)((Gdome_xml_NamedNodeMap *)self)->elem)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_NamedNodeMap *)self)->vtab->query_interface (self, interface, exc);
}
/**
 * gdome_nnm_getNamedItem:
 * @self:  NamedNodeMap Objects ref
 * @name:  The nodeName of the node to retrieve.
 * @exc:  Exception Object ref
 *
 * Retrieves a node specified by name.
 * Returns: a Node (of any type) with the specified nodeName, or %NULL if it
 * does not identify any node in this map.
 */
GdomeNode *
gdome_nnm_getNamedItem (GdomeNamedNodeMap *self, GdomeDOMString *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_NamedNodeMap *)self)->type == GDOME_ATTRIBUTE_NODE && ((Gdome_xml_Node *)((Gdome_xml_NamedNodeMap *)self)->elem)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_NamedNodeMap *)self)->vtab->getNamedItem (self, name, exc);
}
/**
 * gdome_nnm_getNamedItemNS:
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
gdome_nnm_getNamedItemNS (GdomeNamedNodeMap *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_NamedNodeMap *)self)->type == GDOME_ATTRIBUTE_NODE && ((Gdome_xml_Node *)((Gdome_xml_NamedNodeMap *)self)->elem)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_NamedNodeMap *)self)->vtab->getNamedItemNS (self, namespaceURI, localName, exc);
}
/**
 * gdome_nnm_item:
 * @self:  NamedNodeMap Objects ref
 * @index:  Index into this map.
 * @exc:  Exception Object ref
 *
 * Returns: the indexth item in the map. If index is greater than or equal to
 * the number of nodes in this map, this returns %NULL.
 */
GdomeNode *
gdome_nnm_item (GdomeNamedNodeMap *self, gulong index, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_NamedNodeMap *)self)->type == GDOME_ATTRIBUTE_NODE && ((Gdome_xml_Node *)((Gdome_xml_NamedNodeMap *)self)->elem)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_NamedNodeMap *)self)->vtab->item (self, index, exc);
}
/**
 * gdome_nnm_removeNamedItem:
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
gdome_nnm_removeNamedItem (GdomeNamedNodeMap *self, GdomeDOMString *name, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_NamedNodeMap *)self)->type == GDOME_ATTRIBUTE_NODE && ((Gdome_xml_Node *)((Gdome_xml_NamedNodeMap *)self)->elem)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_NamedNodeMap *)self)->vtab->removeNamedItem (self, name, exc);
}
/**
 * gdome_nnm_removeNamedItemNS:
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
gdome_nnm_removeNamedItemNS (GdomeNamedNodeMap *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_NamedNodeMap *)self)->type == GDOME_ATTRIBUTE_NODE && ((Gdome_xml_Node *)((Gdome_xml_NamedNodeMap *)self)->elem)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_NamedNodeMap *)self)->vtab->removeNamedItemNS (self, namespaceURI, localName, exc);
}
/**
 * gdome_nnm_setNamedItem:
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
gdome_nnm_setNamedItem (GdomeNamedNodeMap *self, GdomeNode *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_NamedNodeMap *)self)->type == GDOME_ATTRIBUTE_NODE && ((Gdome_xml_Node *)((Gdome_xml_NamedNodeMap *)self)->elem)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_NamedNodeMap *)self)->vtab->setNamedItem (self, arg, exc);
}
/**
 * gdome_nnm_setNamedItemNS:
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
gdome_nnm_setNamedItemNS (GdomeNamedNodeMap *self, GdomeNode *arg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	if (((Gdome_xml_NamedNodeMap *)self)->type == GDOME_ATTRIBUTE_NODE && ((Gdome_xml_Node *)((Gdome_xml_NamedNodeMap *)self)->elem)->n == NULL) {
		*exc = GDOME_INVALID_STATE_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_xml_NamedNodeMap *)self)->vtab->setNamedItemNS (self, arg, exc);
}
