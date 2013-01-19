/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome.h
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

/* ----- gdome.h ----- */
#ifndef GDOME_H
#define GDOME_H

#include <glib.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* error types for our exception handling system. */
typedef enum {
  GDOME_NOEXCEPTION_ERR = 0,
	GDOME_INDEX_SIZE_ERR = 1,
	GDOME_DOMSTRING_SIZE_ERR = 2,
	GDOME_HIERARCHY_REQUEST_ERR = 3,
	GDOME_WRONG_DOCUMENT_ERR = 4,
	GDOME_INVALID_CHARACTER_ERR = 5,
	GDOME_NO_DATA_ALLOWED_ERR = 6,
	GDOME_NO_MODIFICATION_ALLOWED_ERR = 7,
	GDOME_NOT_FOUND_ERR = 8,
	GDOME_NOT_SUPPORTED_ERR = 9,
	GDOME_INUSE_ATTRIBUTE_ERR = 10,
	GDOME_INVALID_STATE_ERR = 11,
	GDOME_SYNTAX_ERR = 12,
	GDOME_INVALID_MODIFICATION_ERR = 13,
	GDOME_NAMESPACE_ERR = 14,
	GDOME_INVALID_ACCESS_ERR = 15,
	GDOME_NULL_POINTER_ERR = 100
} GdomeExceptionCode;

typedef enum {
	GDOME_CORE_EXCEPTION = 0 << 16,
	GDOME_EVENT_EXCEPTION = 1 << 16,
	GDOME_XPATH_EXCEPTION = 2 << 16
} GdomeExceptionType;

typedef enum {
	GDOME_EXCEPTION_TYPE_MASK = 0x00FF0000,
	GDOME_EXCEPTION_CODE_MASK = 0x0000FFFF
} GdomeExceptionMasks;

/* Exception Handling Macros */
#define GDOME_EXCEPTION_TYPE(exc) ((exc) & GDOME_EXCEPTION_TYPE_MASK)
#define GDOME_EXCEPTION_CODE(exc) ((exc) & GDOME_EXCEPTION_CODE_MASK)

/* access type for node handles */
typedef enum {
	GDOME_READONLY_NODE = 0,
	GDOME_READWRITE_NODE = 1
} GdomeAccessType;

typedef enum {
	GDOME_LOAD_PARSING = 0,
	GDOME_LOAD_VALIDATING = 1,
	GDOME_LOAD_RECOVERING = 2,
	GDOME_LOAD_SUBSTITUTE_ENTITIES = 4,
	GDOME_LOAD_COMPLETE_ATTRS = 8
} GdomeLoadingCode;

typedef enum {
	GDOME_SAVE_STANDARD = 0,
	GDOME_SAVE_LIBXML_INDENT = 1
} GdomeSavingCode;

typedef struct _GdomeDOMString GdomeDOMString;
typedef int GdomeBoolean;
typedef guint32 GdomeException;
typedef long long GdomeDOMTimeStamp;

struct _GdomeDOMString {
	gchar *str;
	gint refcnt;
	void (*unref) (GdomeDOMString *self);
};

typedef struct _GdomeNode GdomeNode;
typedef GdomeNode GdomeEventTarget;
typedef struct _GdomeAttr GdomeAttr;
typedef struct _GdomeCharacterData GdomeCharacterData;
typedef struct _GdomeText GdomeText;
typedef struct _GdomeCDATASection GdomeCDATASection;
typedef struct _GdomeComment GdomeComment;
typedef struct _GdomeDOMImplementation GdomeDOMImplementation;
typedef struct _GdomeDocument GdomeDocument;
typedef struct _GdomeDocumentFragment GdomeDocumentFragment;
typedef struct _GdomeDocumentType GdomeDocumentType;
typedef struct _GdomeElement GdomeElement;
typedef struct _GdomeEntity GdomeEntity;
typedef struct _GdomeEntityReference GdomeEntityReference;
typedef struct _GdomeNamedNodeMap GdomeNamedNodeMap;
typedef struct _GdomeNodeList GdomeNodeList;
typedef struct _GdomeNotation GdomeNotation;
typedef struct _GdomeProcessingInstruction GdomeProcessingInstruction;

/* Events interfaces */
typedef struct _GdomeEvent GdomeEvent;
typedef struct _GdomeEventListener GdomeEventListener;
typedef struct _GdomeMutationEvent GdomeMutationEvent;
typedef struct _GnomeNode GnomeEventTarget;
typedef struct _GnomeDocument GnomeDocumentEvent;

/* XPath interfaces */
typedef struct _GdomeXPathEvaluator GdomeXPathEvaluator;
typedef struct _GdomeXPathNamespace GdomeXPathNamespace;
typedef struct _GdomeXPathNSResolver GdomeXPathNSResolver;
typedef struct _GdomeXPathResult GdomeXPathResult;

/* Traversal interfaces */
typedef struct _GdomeNodeFilter GdomeNodeFilter;
typedef struct _GdomeNodeIterator GdomeNodeIterator;
typedef struct _GdomeTreeWalker GdomeTreeWalker;

typedef struct _GdomeNodeVtab GdomeNodeVtab;
typedef struct _GdomeAttrVtab GdomeAttrVtab;
typedef struct _GdomeCharacterDataVtab GdomeCharacterDataVtab;
typedef struct _GdomeTextVtab GdomeTextVtab;
typedef struct _GdomeCDATASectionVtab GdomeCDATASectionVtab;
typedef struct _GdomeCommentVtab GdomeCommentVtab;
typedef struct _GdomeDOMImplementationVtab GdomeDOMImplementationVtab;
typedef struct _GdomeDocumentVtab GdomeDocumentVtab;
typedef struct _GdomeDocumentFragmentVtab GdomeDocumentFragmentVtab;
typedef struct _GdomeDocumentTypeVtab GdomeDocumentTypeVtab;
typedef struct _GdomeElementVtab GdomeElementVtab;
typedef struct _GdomeEntityVtab GdomeEntityVtab;
typedef struct _GdomeEntityReferenceVtab GdomeEntityReferenceVtab;
typedef struct _GdomeNamedNodeMapVtab GdomeNamedNodeMapVtab;
typedef struct _GdomeNodeListVtab GdomeNodeListVtab;
typedef struct _GdomeNotationVtab GdomeNotationVtab;
typedef struct _GdomeProcessingInstructionVtab GdomeProcessingInstructionVtab;
typedef struct _GdomeXPathNamespaceVtab GdomeXPathNamespaceVtab;

typedef enum {
	GDOME_ELEMENT_NODE = 1,
	GDOME_ATTRIBUTE_NODE = 2,
	GDOME_TEXT_NODE = 3,
	GDOME_CDATA_SECTION_NODE = 4,
	GDOME_ENTITY_REFERENCE_NODE = 5,
	GDOME_ENTITY_NODE = 6,
	GDOME_PROCESSING_INSTRUCTION_NODE = 7,
	GDOME_COMMENT_NODE = 8,
	GDOME_DOCUMENT_NODE = 9,
	GDOME_DOCUMENT_TYPE_NODE = 10,
	GDOME_DOCUMENT_FRAGMENT_NODE = 11,
	GDOME_NOTATION_NODE = 12,
	GDOME_XPATH_NAMESPACE_NODE = 13
} GdomeNodeType;

typedef struct _GdomeEntitiesTableEntry GdomeEntitiesTableEntry;
struct _GdomeEntitiesTableEntry {
	gchar *name;
	gchar *value;
	gchar *publicID;
	gchar *systemID;
};

struct _GdomeNode {
	gpointer user_data;
};

struct _GdomeAttr {
	gpointer user_data;
};

struct _GdomeCharacterData {
	gpointer user_data;
};

struct _GdomeText {
	gpointer user_data;
};

struct _GdomeCDATASection {
	gpointer user_data;
};

struct _GdomeComment {
	gpointer user_data;
};

struct _GdomeDOMImplementation {
	gpointer user_data;
};

struct _GdomeDocument {
	gpointer user_data;
};

struct _GdomeDocumentFragment {
	gpointer user_data;
};

struct _GdomeDocumentType {
	gpointer user_data;
};

struct _GdomeElement {
	gpointer user_data;
};

struct _GdomeEntity {
	gpointer user_data;
};

struct _GdomeEntityReference {
	gpointer user_data;
};

struct _GdomeNamedNodeMap {
	gpointer user_data;
};

struct _GdomeNodeList {
	gpointer user_data;
};

struct _GdomeNotation {
	gpointer user_data;
};

struct _GdomeProcessingInstruction {
	gpointer user_data;
};

struct _GdomeXPathNamespace {
	gpointer user_data;
};

/* Public API of Gdome */

/*-------------------------- DOMString -------------------------- */
GdomeDOMString     *gdome_str_mkref           (const gchar *str);
GdomeDOMString     *gdome_str_mkref_own       (gchar *str);
GdomeDOMString     *gdome_str_mkref_dup       (const gchar *str);
void                gdome_str_ref             (GdomeDOMString *self);
void                gdome_str_unref           (GdomeDOMString *self);
GdomeBoolean        gdome_str_equal           (GdomeDOMString *self,
																							 GdomeDOMString *str);
GdomeBoolean        gdome_str_equalIgnoreCase (GdomeDOMString *self,
																							 GdomeDOMString *str);
gchar               gdome_str_charAt          (GdomeDOMString *self,
																							 int index);
GdomeDOMString     *gdome_str_concat          (GdomeDOMString *self,
																							 GdomeDOMString *str);
GdomeBoolean        gdome_str_endsWith        (GdomeDOMString *self,
																							 GdomeDOMString *suffix);
GdomeBoolean        gdome_str_isEmpty         (GdomeDOMString *self);
int                 gdome_str_length          (GdomeDOMString *self);
GdomeBoolean        gdome_str_startsWith      (GdomeDOMString *self,
                                               GdomeDOMString *prefix);


/* ---------------------------  Node  --------------------------- */
void                gdome_n_ref                  (GdomeNode *self,
																									GdomeException *exc);
void                gdome_n_unref                (GdomeNode *self,
																									GdomeException *exc);
gpointer            gdome_n_query_interface      (GdomeNode *self,
																									const char *interface,
																									GdomeException *exc);
GdomeDOMString    * gdome_n_nodeName             (GdomeNode *self,
																									GdomeException *exc);
GdomeDOMString    * gdome_n_nodeValue            (GdomeNode *self,
																									GdomeException *exc);
void                gdome_n_set_nodeValue        (GdomeNode *self,
																									GdomeDOMString *nodeValue,
																									GdomeException *exc);
unsigned short      gdome_n_nodeType             (GdomeNode *self,
																									GdomeException *exc);
GdomeNode         * gdome_n_parentNode           (GdomeNode *self,
																									GdomeException *exc);
GdomeNodeList     * gdome_n_childNodes           (GdomeNode *self,
																									GdomeException *exc);
GdomeNode         * gdome_n_firstChild           (GdomeNode *self,
																									GdomeException *exc);
GdomeNode         * gdome_n_lastChild            (GdomeNode *self,
																									GdomeException *exc);
GdomeNode         * gdome_n_previousSibling      (GdomeNode *self,
																									GdomeException *exc);
GdomeNode         * gdome_n_nextSibling          (GdomeNode *self,
																									GdomeException *exc);
GdomeNamedNodeMap * gdome_n_attributes           (GdomeNode *self,
																									GdomeException *exc);
GdomeDocument     * gdome_n_ownerDocument        (GdomeNode *self,
																									GdomeException *exc);
GdomeNode         * gdome_n_insertBefore         (GdomeNode *self,
																									GdomeNode *newChild,
																									GdomeNode *refChild,
																									GdomeException *exc);
GdomeNode         * gdome_n_replaceChild         (GdomeNode *self,
																									GdomeNode *newChild,
																									GdomeNode *oldChild,
																									GdomeException *exc);
GdomeNode         * gdome_n_removeChild          (GdomeNode *self,
																									GdomeNode *oldChild,
																									GdomeException *exc);
GdomeNode         * gdome_n_appendChild          (GdomeNode *self,
																									GdomeNode *newChild,
																									GdomeException *exc);
GdomeBoolean        gdome_n_hasChildNodes        (GdomeNode *self,
																									GdomeException *exc);
GdomeNode         * gdome_n_cloneNode            (GdomeNode *self,
																									GdomeBoolean deep,
																									GdomeException *exc);
void                gdome_n_normalize            (GdomeNode *self,
																									GdomeException *exc);
GdomeBoolean        gdome_n_isSupported          (GdomeNode *self,
																									GdomeDOMString *feature,
																									GdomeDOMString *version,
																									GdomeException *exc);
GdomeDOMString    * gdome_n_namespaceURI         (GdomeNode *self,
																									GdomeException *exc);
GdomeDOMString    * gdome_n_prefix               (GdomeNode *self,
																									GdomeException *exc);
void                gdome_n_set_prefix           (GdomeNode *self,
																									GdomeDOMString *prefix,
																									GdomeException *exc);
GdomeDOMString    * gdome_n_localName            (GdomeNode *self,
																									GdomeException *exc);
GdomeBoolean        gdome_n_hasAttributes        (GdomeNode *self,
																									GdomeException *exc);
void                gdome_n_addEventListener     (GdomeNode *self,
																									GdomeDOMString *type,
																									GdomeEventListener *listener,
																									GdomeBoolean useCapture,
																									GdomeException *exc);
void                gdome_n_removeEventListener  (GdomeNode *self,
																									GdomeDOMString *type,
																									GdomeEventListener *listener,
																									GdomeBoolean useCapture,
																									GdomeException *exc);
GdomeBoolean        gdome_n_dispatchEvent        (GdomeNode *self,
																									GdomeEvent *evt,
																									GdomeException *exc);
void                gdome_n_subTreeDispatchEvent (GdomeNode *self,
																									GdomeEvent *evt,
																									GdomeException *exc);
GdomeBoolean        gdome_n_canAppend            (GdomeNode *self,
																									GdomeNode *newChild,
																									GdomeException *exc);


/* ---------------------------  Element  --------------------------- */
GdomeDOMString     * gdome_el_tagName               (GdomeElement *self, 
																										 GdomeException *exc);
GdomeDOMString     * gdome_el_getAttribute          (GdomeElement *self,
																										 GdomeDOMString *name,
																										 GdomeException *exc);
void                 gdome_el_setAttribute          (GdomeElement *self,
																										 GdomeDOMString *name,
																										 GdomeDOMString *value,
																										 GdomeException *exc);
void                 gdome_el_removeAttribute       (GdomeElement *self,
																										 GdomeDOMString *name,
																										 GdomeException *exc);
GdomeAttr          * gdome_el_getAttributeNode      (GdomeElement *self,
																										 GdomeDOMString *name,
																										 GdomeException *exc);
GdomeAttr          * gdome_el_setAttributeNode      (GdomeElement *self,
																										 GdomeAttr *newAttr,
																										 GdomeException *exc);
GdomeAttr          * gdome_el_removeAttributeNode   (GdomeElement *self,
																										 GdomeAttr *oldAttr,
																										 GdomeException *exc);
GdomeNodeList      * gdome_el_getElementsByTagName  (GdomeElement *self,
																										 GdomeDOMString *name,
																										 GdomeException *exc);

/* DOM2 interfaces */

GdomeDOMString     * gdome_el_getAttributeNS        (GdomeElement *self, 
																										 GdomeDOMString *namespaceURI,
																										 GdomeDOMString *localName,
																										 GdomeException *exc);
void                 gdome_el_setAttributeNS        (GdomeElement *self,
																										 GdomeDOMString *namespaceURI,
																										 GdomeDOMString *qualifiedName,
																										 GdomeDOMString *value,
																										 GdomeException *exc);
void                 gdome_el_removeAttributeNS     (GdomeElement *self,
																										 GdomeDOMString *namespaceURI,
																										 GdomeDOMString *localName,
																										 GdomeException *exc);
GdomeAttr          * gdome_el_getAttributeNodeNS    (GdomeElement *self,
																										 GdomeDOMString *namespaceURI,
																										 GdomeDOMString *localName,
																										 GdomeException *exc);
GdomeAttr          * gdome_el_setAttributeNodeNS    (GdomeElement *self,
																										 GdomeAttr *newAttr,
																										 GdomeException *exc);
GdomeNodeList      * gdome_el_getElementsByTagNameNS (GdomeElement *self,
																											GdomeDOMString *namespaceURI,
																											GdomeDOMString *localName,
																											GdomeException *exc);
GdomeBoolean         gdome_el_hasAttribute           (GdomeElement *self,
																											GdomeDOMString *name,
																											GdomeException *exc);
GdomeBoolean         gdome_el_hasAttributeNS         (GdomeElement *self,
																											GdomeDOMString *namespaceURI,
																											GdomeDOMString *localName,
																											GdomeException *exc);


/* Node inherited interfaces. */

void                 gdome_el_ref                    (GdomeElement *self,
																											GdomeException *exc);
void                 gdome_el_unref                  (GdomeElement *self,
																											GdomeException *exc);
gpointer             gdome_el_query_interface        (GdomeElement *self,
																											const char *interface,
																											GdomeException *exc);
GdomeDOMString     * gdome_el_nodeName               (GdomeElement *self,
																											GdomeException *exc);
GdomeDOMString     * gdome_el_nodeValue              (GdomeElement *self,
																											GdomeException *exc);
void                 gdome_el_set_nodeValue          (GdomeElement *self,
																											GdomeDOMString *nodeValue,
																											GdomeException *exc);
unsigned short       gdome_el_nodeType               (GdomeElement *self,
																											GdomeException *exc);
GdomeNode          * gdome_el_parentNode             (GdomeElement *self,
																											GdomeException *exc);
GdomeNodeList      * gdome_el_childNodes             (GdomeElement *self,
																											GdomeException *exc);
GdomeNode          * gdome_el_firstChild             (GdomeElement *self,
																											GdomeException *exc);
GdomeNode          * gdome_el_lastChild              (GdomeElement *self,
																											GdomeException *exc);
GdomeNode          * gdome_el_previousSibling        (GdomeElement *self,
																											GdomeException *exc);
GdomeNode          * gdome_el_nextSibling            (GdomeElement *self,
																											GdomeException *exc);
GdomeNamedNodeMap  * gdome_el_attributes             (GdomeElement *self,
																											GdomeException *exc);
GdomeDocument      * gdome_el_ownerDocument          (GdomeElement *self,
																											GdomeException *exc);
GdomeNode          * gdome_el_insertBefore           (GdomeElement *self,
																											GdomeNode *newChild,
																											GdomeNode *refChild,
																											GdomeException *exc);
GdomeNode          * gdome_el_replaceChild           (GdomeElement *self,
																											GdomeNode *newChild,
																											GdomeNode *oldChild,
																											GdomeException *exc);
GdomeNode          * gdome_el_removeChild            (GdomeElement *self,
																											GdomeNode *oldChild,
																											GdomeException *exc);
GdomeNode          * gdome_el_appendChild            (GdomeElement *self,
																											GdomeNode *newChild,
																											GdomeException *exc);
GdomeBoolean         gdome_el_hasChildNodes          (GdomeElement *self,
																											GdomeException *exc);
GdomeNode          * gdome_el_cloneNode              (GdomeElement *self,
																											GdomeBoolean deep,
																											GdomeException *exc);
void                 gdome_el_normalize              (GdomeElement *self,
																											GdomeException *exc);
GdomeBoolean         gdome_el_isSupported            (GdomeElement *self,
																											GdomeDOMString *feature,
																											GdomeDOMString *version,
																											GdomeException *exc);
GdomeDOMString     * gdome_el_namespaceURI           (GdomeElement *self,
																											GdomeException *exc);
GdomeDOMString     * gdome_el_prefix                 (GdomeElement *self,
																											GdomeException *exc);
void                 gdome_el_set_prefix             (GdomeElement *self,
																											GdomeDOMString *prefix,
																											GdomeException *exc);
GdomeDOMString     * gdome_el_localName              (GdomeElement *self,
																											GdomeException *exc);
GdomeBoolean        gdome_el_hasAttributes           (GdomeElement *self,
																											GdomeException *exc);
void                 gdome_el_addEventListener       (GdomeElement *self,
																											GdomeDOMString *type,
																											GdomeEventListener *listener,
																											GdomeBoolean useCapture,
																											GdomeException *exc);
void                 gdome_el_removeEventListener    (GdomeElement *self,
																											GdomeDOMString *type,
																											GdomeEventListener *listener,
																											GdomeBoolean useCapture,
																											GdomeException *exc);
GdomeBoolean         gdome_el_dispatchEvent          (GdomeElement *self,
																											GdomeEvent *evt,
																											GdomeException *exc);
void                 gdome_el_subTreeDispatchEvent   (GdomeElement *self,
																											GdomeEvent *evt,
																											GdomeException *exc);
GdomeBoolean         gdome_el_canAppend              (GdomeElement *self,
																											GdomeNode *newChild,
																											GdomeException *exc);



/* ---------------------------  Attr  --------------------------- */
void                  gdome_a_ref                  (GdomeAttr *self,
																										GdomeException *exc);
void                  gdome_a_unref                (GdomeAttr *self,
																										GdomeException *exc);
gpointer              gdome_a_query_interface      (GdomeAttr *self,
																										const char *interface,
																										GdomeException *exc);
GdomeDOMString      * gdome_a_name                 (GdomeAttr *self,
																										GdomeException *exc);
GdomeElement        * gdome_a_ownerElement         (GdomeAttr *self,
																										GdomeException *exc);
GdomeBoolean          gdome_a_specified            (GdomeAttr *self,
																										GdomeException *exc);
GdomeDOMString      * gdome_a_value                (GdomeAttr *self,
																										GdomeException *exc);
void                  gdome_a_set_value            (GdomeAttr *self,
																										GdomeDOMString *value,
																										GdomeException *exc);
GdomeDOMString      * gdome_a_nodeName             (GdomeAttr *self,
																										GdomeException *exc);
GdomeDOMString      * gdome_a_nodeValue            (GdomeAttr *self,
																										GdomeException *exc);
void                  gdome_a_set_nodeValue        (GdomeAttr *self,
																										GdomeDOMString *nodeValue,
																										GdomeException *exc);
unsigned short        gdome_a_nodeType             (GdomeAttr *self,
																										GdomeException *exc);
GdomeNode           * gdome_a_parentNode           (GdomeAttr *self,
																										GdomeException *exc);
GdomeNodeList       * gdome_a_childNodes           (GdomeAttr *self,
																										GdomeException *exc);
GdomeNode           * gdome_a_firstChild           (GdomeAttr *self,
																										GdomeException *exc);
GdomeNode           * gdome_a_lastChild            (GdomeAttr *self,
																										GdomeException *exc);
GdomeNode           * gdome_a_previousSibling      (GdomeAttr *self,
																										GdomeException *exc);
GdomeNode           * gdome_a_nextSibling          (GdomeAttr *self,
																										GdomeException *exc);
GdomeNamedNodeMap   * gdome_a_attributes           (GdomeAttr *self,
																										GdomeException *exc);
GdomeDocument       * gdome_a_ownerDocument        (GdomeAttr *self,
																										GdomeException *exc);
GdomeNode           * gdome_a_insertBefore         (GdomeAttr *self,
																										GdomeNode *newChild,
																										GdomeNode *refChild,
																										GdomeException *exc);
GdomeNode           * gdome_a_replaceChild         (GdomeAttr *self,
																										GdomeNode *newChild,
																										GdomeNode *oldChild,
																										GdomeException *exc);
GdomeNode           * gdome_a_removeChild          (GdomeAttr *self,
																										GdomeNode *oldChild,
																										GdomeException *exc);
GdomeNode           * gdome_a_appendChild          (GdomeAttr *self,
																										GdomeNode *newChild,
																										GdomeException *exc);
GdomeBoolean          gdome_a_hasChildNodes        (GdomeAttr *self,
																										GdomeException *exc);
GdomeNode           * gdome_a_cloneNode            (GdomeAttr *self,
																										GdomeBoolean deep,
																										GdomeException *exc);
void                  gdome_a_normalize            (GdomeAttr *self,
																										GdomeException *exc);
GdomeBoolean          gdome_a_isSupported          (GdomeAttr *self,
																										GdomeDOMString *feature,
																										GdomeDOMString *version,
																										GdomeException *exc);
GdomeDOMString      * gdome_a_namespaceURI         (GdomeAttr *self,
																										GdomeException *exc);
GdomeDOMString      * gdome_a_prefix               (GdomeAttr *self,
																										GdomeException *exc);
void                  gdome_a_set_prefix           (GdomeAttr *self,
																										GdomeDOMString *prefix,
																										GdomeException *exc);
GdomeDOMString      * gdome_a_localName            (GdomeAttr *self,
																										GdomeException *exc);
GdomeBoolean          gdome_a_hasAttributes        (GdomeAttr *self,
																										GdomeException *exc);
void                  gdome_a_addEventListener     (GdomeAttr *self,
																										GdomeDOMString *type,
																										GdomeEventListener *listener,
																										GdomeBoolean useCapture,
																										GdomeException *exc);
void                  gdome_a_removeEventListener  (GdomeAttr *self,
																										GdomeDOMString *type,
																										GdomeEventListener *listener,
																										GdomeBoolean useCapture,
																										GdomeException *exc);
GdomeBoolean          gdome_a_dispatchEvent        (GdomeAttr *self,
																										GdomeEvent *evt,
																										GdomeException *exc);
void                  gdome_a_subTreeDispatchEvent (GdomeAttr *self,
																										GdomeEvent *evt,
																										GdomeException *exc);
GdomeBoolean          gdome_a_canAppend            (GdomeAttr *self,
																										GdomeNode *newChild,
																										GdomeException *exc);


GdomeDOMString *gdome_cd_data (GdomeCharacterData *self, GdomeException *exc);
void gdome_cd_set_data (GdomeCharacterData *self, GdomeDOMString *data, GdomeException *exc);
gulong gdome_cd_length (GdomeCharacterData *self, GdomeException *exc);
GdomeDOMString *gdome_cd_substringData (GdomeCharacterData *self, gulong offset, gulong count, GdomeException *exc);
void gdome_cd_appendData (GdomeCharacterData *self, GdomeDOMString *arg, GdomeException *exc);
void gdome_cd_insertData (GdomeCharacterData *self, gulong offset, GdomeDOMString *arg, GdomeException *exc);
void gdome_cd_deleteData (GdomeCharacterData *self, gulong offset, gulong count, GdomeException *exc);
void gdome_cd_replaceData (GdomeCharacterData *self, gulong offset, gulong count, GdomeDOMString *arg, GdomeException *exc);
void gdome_cd_ref (GdomeCharacterData *self, GdomeException *exc);
void gdome_cd_unref (GdomeCharacterData *self, GdomeException *exc);
gpointer gdome_cd_query_interface (GdomeCharacterData *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_cd_nodeName (GdomeCharacterData *self, GdomeException *exc);
GdomeDOMString *gdome_cd_nodeValue (GdomeCharacterData *self, GdomeException *exc);
void gdome_cd_set_nodeValue (GdomeCharacterData *self, GdomeDOMString *nodeValue, GdomeException *exc);
unsigned short gdome_cd_nodeType (GdomeCharacterData *self, GdomeException *exc);
GdomeNode *gdome_cd_parentNode (GdomeCharacterData *self, GdomeException *exc);
GdomeNodeList *gdome_cd_childNodes (GdomeCharacterData *self, GdomeException *exc);
GdomeNode *gdome_cd_firstChild (GdomeCharacterData *self, GdomeException *exc);
GdomeNode *gdome_cd_lastChild (GdomeCharacterData *self, GdomeException *exc);
GdomeNode *gdome_cd_previousSibling (GdomeCharacterData *self, GdomeException *exc);
GdomeNode *gdome_cd_nextSibling (GdomeCharacterData *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_cd_attributes (GdomeCharacterData *self, GdomeException *exc);
GdomeDocument *gdome_cd_ownerDocument (GdomeCharacterData *self, GdomeException *exc);
GdomeNode *gdome_cd_insertBefore (GdomeCharacterData *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc);
GdomeNode *gdome_cd_replaceChild (GdomeCharacterData *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_cd_removeChild (GdomeCharacterData *self, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_cd_appendChild (GdomeCharacterData *self, GdomeNode *newChild, GdomeException *exc);
GdomeBoolean gdome_cd_hasChildNodes (GdomeCharacterData *self, GdomeException *exc);
GdomeNode *gdome_cd_cloneNode (GdomeCharacterData *self, GdomeBoolean deep, GdomeException *exc);
void gdome_cd_normalize (GdomeCharacterData *self, GdomeException *exc);
GdomeBoolean gdome_cd_isSupported (GdomeCharacterData *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDOMString *gdome_cd_namespaceURI (GdomeCharacterData *self, GdomeException *exc);
GdomeDOMString *gdome_cd_prefix (GdomeCharacterData *self, GdomeException *exc);
void gdome_cd_set_prefix (GdomeCharacterData *self, GdomeDOMString *prefix, GdomeException *exc);
GdomeDOMString *gdome_cd_localName (GdomeCharacterData *self, GdomeException *exc);
GdomeBoolean gdome_cd_hasAttributes (GdomeCharacterData *self, GdomeException *exc);
void gdome_cd_addEventListener (GdomeCharacterData *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
void gdome_cd_removeEventListener (GdomeCharacterData *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
GdomeBoolean gdome_cd_dispatchEvent (GdomeCharacterData *self, GdomeEvent *evt, GdomeException *exc);
void gdome_cd_subTreeDispatchEvent (GdomeCharacterData *self, GdomeEvent *evt, GdomeException *exc);
GdomeBoolean gdome_cd_canAppend (GdomeCharacterData *self, GdomeNode *newChild, GdomeException *exc);



GdomeText *gdome_t_splitText (GdomeText *self, gulong offset, GdomeException *exc);
GdomeDOMString *gdome_t_data (GdomeText *self, GdomeException *exc);
void gdome_t_set_data (GdomeText *self, GdomeDOMString *data, GdomeException *exc);
gulong gdome_t_length (GdomeText *self, GdomeException *exc);
GdomeDOMString *gdome_t_substringData (GdomeText *self, gulong offset, gulong count, GdomeException *exc);
void gdome_t_appendData (GdomeText *self, GdomeDOMString *arg, GdomeException *exc);
void gdome_t_insertData (GdomeText *self, gulong offset, GdomeDOMString *arg, GdomeException *exc);
void gdome_t_deleteData (GdomeText *self, gulong offset, gulong count, GdomeException *exc);
void gdome_t_replaceData (GdomeText *self, gulong offset, gulong count, GdomeDOMString *arg, GdomeException *exc);
void gdome_t_ref (GdomeText *self, GdomeException *exc);
void gdome_t_unref (GdomeText *self, GdomeException *exc);
gpointer gdome_t_query_interface (GdomeText *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_t_nodeName (GdomeText *self, GdomeException *exc);
GdomeDOMString *gdome_t_nodeValue (GdomeText *self, GdomeException *exc);
void gdome_t_set_nodeValue (GdomeText *self, GdomeDOMString *nodeValue, GdomeException *exc);
unsigned short gdome_t_nodeType (GdomeText *self, GdomeException *exc);
GdomeNode *gdome_t_parentNode (GdomeText *self, GdomeException *exc);
GdomeNodeList *gdome_t_childNodes (GdomeText *self, GdomeException *exc);
GdomeNode *gdome_t_firstChild (GdomeText *self, GdomeException *exc);
GdomeNode *gdome_t_lastChild (GdomeText *self, GdomeException *exc);
GdomeNode *gdome_t_previousSibling (GdomeText *self, GdomeException *exc);
GdomeNode *gdome_t_nextSibling (GdomeText *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_t_attributes (GdomeText *self, GdomeException *exc);
GdomeDocument *gdome_t_ownerDocument (GdomeText *self, GdomeException *exc);
GdomeNode *gdome_t_insertBefore (GdomeText *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc);
GdomeNode *gdome_t_replaceChild (GdomeText *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_t_removeChild (GdomeText *self, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_t_appendChild (GdomeText *self, GdomeNode *newChild, GdomeException *exc);
GdomeBoolean gdome_t_hasChildNodes (GdomeText *self, GdomeException *exc);
GdomeNode *gdome_t_cloneNode (GdomeText *self, GdomeBoolean deep, GdomeException *exc);
void gdome_t_normalize (GdomeText *self, GdomeException *exc);
GdomeBoolean gdome_t_isSupported (GdomeText *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDOMString *gdome_t_namespaceURI (GdomeText *self, GdomeException *exc);
GdomeDOMString *gdome_t_prefix (GdomeText *self, GdomeException *exc);
void gdome_t_set_prefix (GdomeText *self, GdomeDOMString *prefix, GdomeException *exc);
GdomeDOMString *gdome_t_localName (GdomeText *self, GdomeException *exc);
GdomeBoolean gdome_t_hasAttributes (GdomeText *self, GdomeException *exc);
void gdome_t_addEventListener (GdomeText *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
void gdome_t_removeEventListener (GdomeText *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
GdomeBoolean gdome_t_dispatchEvent (GdomeText *self, GdomeEvent *evt, GdomeException *exc);
void gdome_t_subTreeDispatchEvent (GdomeText *self, GdomeEvent *evt, GdomeException *exc);
GdomeBoolean gdome_t_canAppend (GdomeText *self, GdomeNode *newChild, GdomeException *exc);



GdomeText *gdome_cds_splitText (GdomeCDATASection *self, gulong offset, GdomeException *exc);
GdomeDOMString *gdome_cds_data (GdomeCDATASection *self, GdomeException *exc);
void gdome_cds_set_data (GdomeCDATASection *self, GdomeDOMString *data, GdomeException *exc);
gulong gdome_cds_length (GdomeCDATASection *self, GdomeException *exc);
GdomeDOMString *gdome_cds_substringData (GdomeCDATASection *self, gulong offset, gulong count, GdomeException *exc);
void gdome_cds_appendData (GdomeCDATASection *self, GdomeDOMString *arg, GdomeException *exc);
void gdome_cds_insertData (GdomeCDATASection *self, gulong offset, GdomeDOMString *arg, GdomeException *exc);
void gdome_cds_deleteData (GdomeCDATASection *self, gulong offset, gulong count, GdomeException *exc);
void gdome_cds_replaceData (GdomeCDATASection *self, gulong offset, gulong count, GdomeDOMString *arg, GdomeException *exc);
void gdome_cds_ref (GdomeCDATASection *self, GdomeException *exc);
void gdome_cds_unref (GdomeCDATASection *self, GdomeException *exc);
gpointer gdome_cds_query_interface (GdomeCDATASection *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_cds_nodeName (GdomeCDATASection *self, GdomeException *exc);
GdomeDOMString *gdome_cds_nodeValue (GdomeCDATASection *self, GdomeException *exc);
void gdome_cds_set_nodeValue (GdomeCDATASection *self, GdomeDOMString *nodeValue, GdomeException *exc);
unsigned short gdome_cds_nodeType (GdomeCDATASection *self, GdomeException *exc);
GdomeNode *gdome_cds_parentNode (GdomeCDATASection *self, GdomeException *exc);
GdomeNodeList *gdome_cds_childNodes (GdomeCDATASection *self, GdomeException *exc);
GdomeNode *gdome_cds_firstChild (GdomeCDATASection *self, GdomeException *exc);
GdomeNode *gdome_cds_lastChild (GdomeCDATASection *self, GdomeException *exc);
GdomeNode *gdome_cds_previousSibling (GdomeCDATASection *self, GdomeException *exc);
GdomeNode *gdome_cds_nextSibling (GdomeCDATASection *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_cds_attributes (GdomeCDATASection *self, GdomeException *exc);
GdomeDocument *gdome_cds_ownerDocument (GdomeCDATASection *self, GdomeException *exc);
GdomeNode *gdome_cds_insertBefore (GdomeCDATASection *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc);
GdomeNode *gdome_cds_replaceChild (GdomeCDATASection *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_cds_removeChild (GdomeCDATASection *self, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_cds_appendChild (GdomeCDATASection *self, GdomeNode *newChild, GdomeException *exc);
GdomeBoolean gdome_cds_hasChildNodes (GdomeCDATASection *self, GdomeException *exc);
GdomeNode *gdome_cds_cloneNode (GdomeCDATASection *self, GdomeBoolean deep, GdomeException *exc);
void gdome_cds_normalize (GdomeCDATASection *self, GdomeException *exc);
GdomeBoolean gdome_cds_isSupported (GdomeCDATASection *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDOMString *gdome_cds_namespaceURI (GdomeCDATASection *self, GdomeException *exc);
GdomeDOMString *gdome_cds_prefix (GdomeCDATASection *self, GdomeException *exc);
void gdome_cds_set_prefix (GdomeCDATASection *self, GdomeDOMString *prefix, GdomeException *exc);
GdomeDOMString *gdome_cds_localName (GdomeCDATASection *self, GdomeException *exc);
GdomeBoolean gdome_cds_hasAttributes (GdomeCDATASection *self, GdomeException *exc);
void gdome_cds_addEventListener (GdomeCDATASection *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
void gdome_cds_removeEventListener (GdomeCDATASection *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
GdomeBoolean gdome_cds_dispatchEvent (GdomeCDATASection *self, GdomeEvent *evt, GdomeException *exc);
void gdome_cds_subTreeDispatchEvent (GdomeCDATASection *self, GdomeEvent *evt, GdomeException *exc);
GdomeBoolean gdome_cds_canAppend (GdomeCDATASection *self, GdomeNode *newChild, GdomeException *exc);



GdomeDOMString *gdome_c_data (GdomeComment *self, GdomeException *exc);
void gdome_c_set_data (GdomeComment *self, GdomeDOMString *data, GdomeException *exc);
gulong gdome_c_length (GdomeComment *self, GdomeException *exc);
GdomeDOMString *gdome_c_substringData (GdomeComment *self, gulong offset, gulong count, GdomeException *exc);
void gdome_c_appendData (GdomeComment *self, GdomeDOMString *arg, GdomeException *exc);
void gdome_c_insertData (GdomeComment *self, gulong offset, GdomeDOMString *arg, GdomeException *exc);
void gdome_c_deleteData (GdomeComment *self, gulong offset, gulong count, GdomeException *exc);
void gdome_c_replaceData (GdomeComment *self, gulong offset, gulong count, GdomeDOMString *arg, GdomeException *exc);
void gdome_c_ref (GdomeComment *self, GdomeException *exc);
void gdome_c_unref (GdomeComment *self, GdomeException *exc);
gpointer gdome_c_query_interface (GdomeComment *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_c_nodeName (GdomeComment *self, GdomeException *exc);
GdomeDOMString *gdome_c_nodeValue (GdomeComment *self, GdomeException *exc);
void gdome_c_set_nodeValue (GdomeComment *self, GdomeDOMString *nodeValue, GdomeException *exc);
unsigned short gdome_c_nodeType (GdomeComment *self, GdomeException *exc);
GdomeNode *gdome_c_parentNode (GdomeComment *self, GdomeException *exc);
GdomeNodeList *gdome_c_childNodes (GdomeComment *self, GdomeException *exc);
GdomeNode *gdome_c_firstChild (GdomeComment *self, GdomeException *exc);
GdomeNode *gdome_c_lastChild (GdomeComment *self, GdomeException *exc);
GdomeNode *gdome_c_previousSibling (GdomeComment *self, GdomeException *exc);
GdomeNode *gdome_c_nextSibling (GdomeComment *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_c_attributes (GdomeComment *self, GdomeException *exc);
GdomeDocument *gdome_c_ownerDocument (GdomeComment *self, GdomeException *exc);
GdomeNode *gdome_c_insertBefore (GdomeComment *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc);
GdomeNode *gdome_c_replaceChild (GdomeComment *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_c_removeChild (GdomeComment *self, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_c_appendChild (GdomeComment *self, GdomeNode *newChild, GdomeException *exc);
GdomeBoolean gdome_c_hasChildNodes (GdomeComment *self, GdomeException *exc);
GdomeNode *gdome_c_cloneNode (GdomeComment *self, GdomeBoolean deep, GdomeException *exc);
void gdome_c_normalize (GdomeComment *self, GdomeException *exc);
GdomeBoolean gdome_c_isSupported (GdomeComment *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDOMString *gdome_c_namespaceURI (GdomeComment *self, GdomeException *exc);
GdomeDOMString *gdome_c_prefix (GdomeComment *self, GdomeException *exc);
void gdome_c_set_prefix (GdomeComment *self, GdomeDOMString *prefix, GdomeException *exc);
GdomeDOMString *gdome_c_localName (GdomeComment *self, GdomeException *exc);
GdomeBoolean gdome_c_hasAttributes (GdomeComment *self, GdomeException *exc);
void gdome_c_addEventListener (GdomeComment *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
void gdome_c_removeEventListener (GdomeComment *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
GdomeBoolean gdome_c_dispatchEvent (GdomeComment *self, GdomeEvent *evt, GdomeException *exc);
void gdome_c_subTreeDispatchEvent (GdomeComment *self, GdomeEvent *evt, GdomeException *exc);
GdomeBoolean gdome_c_canAppend (GdomeComment *self, GdomeNode *newChild, GdomeException *exc);



GdomeDOMImplementation * gdome_di_mkref (void);
void gdome_di_ref (GdomeDOMImplementation *self, GdomeException *exc);
void gdome_di_unref (GdomeDOMImplementation *self, GdomeException *exc);
gpointer gdome_di_query_interface (GdomeDOMImplementation *self, const char *interface, GdomeException *exc);
GdomeBoolean gdome_di_hasFeature (GdomeDOMImplementation *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDocumentType *gdome_di_createDocumentType (GdomeDOMImplementation *self, GdomeDOMString *qualifiedName, GdomeDOMString *publicId, GdomeDOMString *systemId, GdomeException *exc);
GdomeDocument *gdome_di_createDocument (GdomeDOMImplementation *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeDocumentType *doctype, GdomeException *exc);
void gdome_di_freeDoc (GdomeDOMImplementation *self, GdomeDocument *doc, GdomeException *exc);
GdomeDocument *gdome_di_createDocFromURI (GdomeDOMImplementation *self, const char *uri, unsigned int mode, GdomeException *exc);
GdomeDocument *gdome_di_createDocFromMemory (GdomeDOMImplementation *self, char *buffer, unsigned int mode, GdomeException *exc);
GdomeDocument *gdome_di_createDocFromURIWithEntitiesTable (GdomeDOMImplementation *self, const char *uri, const GdomeEntitiesTableEntry entityTable[], unsigned int mode, GdomeException *exc);
GdomeDocument *gdome_di_createDocFromMemoryWithEntitiesTable (GdomeDOMImplementation *self, char *buffer, const GdomeEntitiesTableEntry entityTable[], unsigned int mode, GdomeException *exc);
GdomeBoolean gdome_di_saveDocToFile (GdomeDOMImplementation *self, GdomeDocument *doc, const char *filename, GdomeSavingCode mode, GdomeException *exc);
GdomeBoolean gdome_di_saveDocToFileEnc (GdomeDOMImplementation *self, GdomeDocument *doc, const char *filename, const char *encoding, GdomeSavingCode mode, GdomeException *exc);
GdomeBoolean gdome_di_saveDocToMemory (GdomeDOMImplementation *self, GdomeDocument *doc, char **mem, GdomeSavingCode mode, GdomeException *exc);
	GdomeBoolean gdome_di_saveDocToMemoryEnc (GdomeDOMImplementation *self, GdomeDocument *doc, char **mem, const char *encoding, GdomeSavingCode mode, GdomeException *exc);
void gdome_di_enableEvent (GdomeDOMImplementation *self, GdomeDocument *doc, const char *name, GdomeException *exc);
void gdome_di_disableEvent (GdomeDOMImplementation *self, GdomeDocument *doc, const char *name, GdomeException *exc);
GdomeBoolean gdome_di_eventIsEnabled (GdomeDOMImplementation *self, GdomeDocument *doc, const char *name, GdomeException *exc);

GdomeDocumentType *gdome_doc_doctype (GdomeDocument *self, GdomeException *exc);
GdomeDOMImplementation *gdome_doc_implementation (GdomeDocument *self, GdomeException *exc);
GdomeElement *gdome_doc_documentElement (GdomeDocument *self, GdomeException *exc);
GdomeElement *gdome_doc_createElement (GdomeDocument *self, GdomeDOMString *tagName, GdomeException *exc);
GdomeDocumentFragment *gdome_doc_createDocumentFragment (GdomeDocument *self, GdomeException *exc);
GdomeText *gdome_doc_createTextNode (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc);
GdomeComment *gdome_doc_createComment (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc);
GdomeCDATASection *gdome_doc_createCDATASection (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc);
GdomeProcessingInstruction *gdome_doc_createProcessingInstruction (GdomeDocument *self, GdomeDOMString *target, GdomeDOMString *data, GdomeException *exc);
GdomeAttr *gdome_doc_createAttribute (GdomeDocument *self, GdomeDOMString *name, GdomeException *exc);
GdomeEntityReference *gdome_doc_createEntityReference (GdomeDocument *self, GdomeDOMString *name, GdomeException *exc);
GdomeNodeList *gdome_doc_getElementsByTagName (GdomeDocument *self, GdomeDOMString *tagname, GdomeException *exc);
GdomeNode *gdome_doc_importNode (GdomeDocument *self, GdomeNode *importedNode, GdomeBoolean deep, GdomeException *exc);
GdomeElement *gdome_doc_createElementNS (GdomeDocument *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeException *exc);
GdomeAttr *gdome_doc_createAttributeNS (GdomeDocument *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeException *exc);
GdomeNodeList *gdome_doc_getElementsByTagNameNS (GdomeDocument *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc);
GdomeElement *gdome_doc_getElementById (GdomeDocument *self, GdomeDOMString *elementId, GdomeException *exc);
GdomeEvent *gdome_doc_createEvent (GdomeDocument *self, GdomeDOMString *eventType, GdomeException *exc);
void gdome_doc_ref (GdomeDocument *self, GdomeException *exc);
void gdome_doc_unref (GdomeDocument *self, GdomeException *exc);
gpointer gdome_doc_query_interface (GdomeDocument *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_doc_nodeName (GdomeDocument *self, GdomeException *exc);
GdomeDOMString *gdome_doc_nodeValue (GdomeDocument *self, GdomeException *exc);
void gdome_doc_set_nodeValue (GdomeDocument *self, GdomeDOMString *nodeValue, GdomeException *exc);
unsigned short gdome_doc_nodeType (GdomeDocument *self, GdomeException *exc);
GdomeNode *gdome_doc_parentNode (GdomeDocument *self, GdomeException *exc);
GdomeNodeList *gdome_doc_childNodes (GdomeDocument *self, GdomeException *exc);
GdomeNode *gdome_doc_firstChild (GdomeDocument *self, GdomeException *exc);
GdomeNode *gdome_doc_lastChild (GdomeDocument *self, GdomeException *exc);
GdomeNode *gdome_doc_previousSibling (GdomeDocument *self, GdomeException *exc);
GdomeNode *gdome_doc_nextSibling (GdomeDocument *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_doc_attributes (GdomeDocument *self, GdomeException *exc);
GdomeDocument *gdome_doc_ownerDocument (GdomeDocument *self, GdomeException *exc);
GdomeNode *gdome_doc_insertBefore (GdomeDocument *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc);
GdomeNode *gdome_doc_replaceChild (GdomeDocument *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_doc_removeChild (GdomeDocument *self, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_doc_appendChild (GdomeDocument *self, GdomeNode *newChild, GdomeException *exc);
GdomeBoolean gdome_doc_hasChildNodes (GdomeDocument *self, GdomeException *exc);
GdomeNode *gdome_doc_cloneNode (GdomeDocument *self, GdomeBoolean deep, GdomeException *exc);
void gdome_doc_normalize (GdomeDocument *self, GdomeException *exc);
GdomeBoolean gdome_doc_isSupported (GdomeDocument *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDOMString *gdome_doc_namespaceURI (GdomeDocument *self, GdomeException *exc);
GdomeDOMString *gdome_doc_prefix (GdomeDocument *self, GdomeException *exc);
void gdome_doc_set_prefix (GdomeDocument *self, GdomeDOMString *prefix, GdomeException *exc);
GdomeDOMString *gdome_doc_localName (GdomeDocument *self, GdomeException *exc);
GdomeBoolean gdome_doc_hasAttributes (GdomeDocument *self, GdomeException *exc);
void gdome_doc_addEventListener (GdomeDocument *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
void gdome_doc_removeEventListener (GdomeDocument *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
GdomeBoolean gdome_doc_dispatchEvent (GdomeDocument *self, GdomeEvent *evt, GdomeException *exc);
void gdome_doc_subTreeDispatchEvent (GdomeDocument *self, GdomeEvent *evt, GdomeException *exc);
GdomeBoolean gdome_doc_canAppend (GdomeDocument *self, GdomeNode *newChild, GdomeException *exc);



void gdome_df_ref (GdomeDocumentFragment *self, GdomeException *exc);
void gdome_df_unref (GdomeDocumentFragment *self, GdomeException *exc);
gpointer gdome_df_query_interface (GdomeDocumentFragment *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_df_nodeName (GdomeDocumentFragment *self, GdomeException *exc);
GdomeDOMString *gdome_df_nodeValue (GdomeDocumentFragment *self, GdomeException *exc);
void gdome_df_set_nodeValue (GdomeDocumentFragment *self, GdomeDOMString *nodeValue, GdomeException *exc);
unsigned short gdome_df_nodeType (GdomeDocumentFragment *self, GdomeException *exc);
GdomeNode *gdome_df_parentNode (GdomeDocumentFragment *self, GdomeException *exc);
GdomeNodeList *gdome_df_childNodes (GdomeDocumentFragment *self, GdomeException *exc);
GdomeNode *gdome_df_firstChild (GdomeDocumentFragment *self, GdomeException *exc);
GdomeNode *gdome_df_lastChild (GdomeDocumentFragment *self, GdomeException *exc);
GdomeNode *gdome_df_previousSibling (GdomeDocumentFragment *self, GdomeException *exc);
GdomeNode *gdome_df_nextSibling (GdomeDocumentFragment *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_df_attributes (GdomeDocumentFragment *self, GdomeException *exc);
GdomeDocument *gdome_df_ownerDocument (GdomeDocumentFragment *self, GdomeException *exc);
GdomeNode *gdome_df_insertBefore (GdomeDocumentFragment *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc);
GdomeNode *gdome_df_replaceChild (GdomeDocumentFragment *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_df_removeChild (GdomeDocumentFragment *self, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_df_appendChild (GdomeDocumentFragment *self, GdomeNode *newChild, GdomeException *exc);
GdomeBoolean gdome_df_hasChildNodes (GdomeDocumentFragment *self, GdomeException *exc);
GdomeNode *gdome_df_cloneNode (GdomeDocumentFragment *self, GdomeBoolean deep, GdomeException *exc);
void gdome_df_normalize (GdomeDocumentFragment *self, GdomeException *exc);
GdomeBoolean gdome_df_isSupported (GdomeDocumentFragment *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDOMString *gdome_df_namespaceURI (GdomeDocumentFragment *self, GdomeException *exc);
GdomeDOMString *gdome_df_prefix (GdomeDocumentFragment *self, GdomeException *exc);
void gdome_df_set_prefix (GdomeDocumentFragment *self, GdomeDOMString *prefix, GdomeException *exc);
GdomeDOMString *gdome_df_localName (GdomeDocumentFragment *self, GdomeException *exc);
GdomeBoolean gdome_df_hasAttributes (GdomeDocumentFragment *self, GdomeException *exc);
void gdome_df_addEventListener (GdomeDocumentFragment *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
void gdome_df_removeEventListener (GdomeDocumentFragment *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
GdomeBoolean gdome_df_dispatchEvent (GdomeDocumentFragment *self, GdomeEvent *evt, GdomeException *exc);
void gdome_df_subTreeDispatchEvent (GdomeDocumentFragment *self, GdomeEvent *evt, GdomeException *exc);
GdomeBoolean gdome_df_canAppend (GdomeDocumentFragment *self, GdomeNode *newChild, GdomeException *exc);



GdomeDOMString *gdome_dt_name (GdomeDocumentType *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_dt_entities (GdomeDocumentType *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_dt_notations (GdomeDocumentType *self, GdomeException *exc);
GdomeDOMString *gdome_dt_publicId (GdomeDocumentType *self, GdomeException *exc);
GdomeDOMString *gdome_dt_systemId (GdomeDocumentType *self, GdomeException *exc);
GdomeDOMString *gdome_dt_internalSubset (GdomeDocumentType *self, GdomeException *exc);
void gdome_dt_ref (GdomeDocumentType *self, GdomeException *exc);
void gdome_dt_unref (GdomeDocumentType *self, GdomeException *exc);
gpointer gdome_dt_query_interface (GdomeDocumentType *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_dt_nodeName (GdomeDocumentType *self, GdomeException *exc);
GdomeDOMString *gdome_dt_nodeValue (GdomeDocumentType *self, GdomeException *exc);
void gdome_dt_set_nodeValue (GdomeDocumentType *self, GdomeDOMString *nodeValue, GdomeException *exc);
unsigned short gdome_dt_nodeType (GdomeDocumentType *self, GdomeException *exc);
GdomeNode *gdome_dt_parentNode (GdomeDocumentType *self, GdomeException *exc);
GdomeNodeList *gdome_dt_childNodes (GdomeDocumentType *self, GdomeException *exc);
GdomeNode *gdome_dt_firstChild (GdomeDocumentType *self, GdomeException *exc);
GdomeNode *gdome_dt_lastChild (GdomeDocumentType *self, GdomeException *exc);
GdomeNode *gdome_dt_previousSibling (GdomeDocumentType *self, GdomeException *exc);
GdomeNode *gdome_dt_nextSibling (GdomeDocumentType *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_dt_attributes (GdomeDocumentType *self, GdomeException *exc);
GdomeDocument *gdome_dt_ownerDocument (GdomeDocumentType *self, GdomeException *exc);
GdomeNode *gdome_dt_insertBefore (GdomeDocumentType *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc);
GdomeNode *gdome_dt_replaceChild (GdomeDocumentType *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_dt_removeChild (GdomeDocumentType *self, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_dt_appendChild (GdomeDocumentType *self, GdomeNode *newChild, GdomeException *exc);
GdomeBoolean gdome_dt_hasChildNodes (GdomeDocumentType *self, GdomeException *exc);
GdomeNode *gdome_dt_cloneNode (GdomeDocumentType *self, GdomeBoolean deep, GdomeException *exc);
void gdome_dt_normalize (GdomeDocumentType *self, GdomeException *exc);
GdomeBoolean gdome_dt_isSupported (GdomeDocumentType *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDOMString *gdome_dt_namespaceURI (GdomeDocumentType *self, GdomeException *exc);
GdomeDOMString *gdome_dt_prefix (GdomeDocumentType *self, GdomeException *exc);
void gdome_dt_set_prefix (GdomeDocumentType *self, GdomeDOMString *prefix, GdomeException *exc);
GdomeDOMString *gdome_dt_localName (GdomeDocumentType *self, GdomeException *exc);
GdomeBoolean gdome_dt_hasAttributes (GdomeDocumentType *self, GdomeException *exc);
void gdome_dt_addEventListener (GdomeDocumentType *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
void gdome_dt_removeEventListener (GdomeDocumentType *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
GdomeBoolean gdome_dt_dispatchEvent (GdomeDocumentType *self, GdomeEvent *evt, GdomeException *exc);
void gdome_dt_subTreeDispatchEvent (GdomeDocumentType *self, GdomeEvent *evt, GdomeException *exc);
GdomeBoolean gdome_dt_canAppend (GdomeDocumentType *self, GdomeNode *newChild, GdomeException *exc);






GdomeDOMString *gdome_ent_publicId (GdomeEntity *self, GdomeException *exc);
GdomeDOMString *gdome_ent_systemId (GdomeEntity *self, GdomeException *exc);
GdomeDOMString *gdome_ent_notationName (GdomeEntity *self, GdomeException *exc);
void gdome_ent_ref (GdomeEntity *self, GdomeException *exc);
void gdome_ent_unref (GdomeEntity *self, GdomeException *exc);
gpointer gdome_ent_query_interface (GdomeEntity *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_ent_nodeName (GdomeEntity *self, GdomeException *exc);
GdomeDOMString *gdome_ent_nodeValue (GdomeEntity *self, GdomeException *exc);
void gdome_ent_set_nodeValue (GdomeEntity *self, GdomeDOMString *nodeValue, GdomeException *exc);
unsigned short gdome_ent_nodeType (GdomeEntity *self, GdomeException *exc);
GdomeNode *gdome_ent_parentNode (GdomeEntity *self, GdomeException *exc);
GdomeNodeList *gdome_ent_childNodes (GdomeEntity *self, GdomeException *exc);
GdomeNode *gdome_ent_firstChild (GdomeEntity *self, GdomeException *exc);
GdomeNode *gdome_ent_lastChild (GdomeEntity *self, GdomeException *exc);
GdomeNode *gdome_ent_previousSibling (GdomeEntity *self, GdomeException *exc);
GdomeNode *gdome_ent_nextSibling (GdomeEntity *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_ent_attributes (GdomeEntity *self, GdomeException *exc);
GdomeDocument *gdome_ent_ownerDocument (GdomeEntity *self, GdomeException *exc);
GdomeNode *gdome_ent_insertBefore (GdomeEntity *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc);
GdomeNode *gdome_ent_replaceChild (GdomeEntity *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_ent_removeChild (GdomeEntity *self, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_ent_appendChild (GdomeEntity *self, GdomeNode *newChild, GdomeException *exc);
GdomeBoolean gdome_ent_hasChildNodes (GdomeEntity *self, GdomeException *exc);
GdomeNode *gdome_ent_cloneNode (GdomeEntity *self, GdomeBoolean deep, GdomeException *exc);
void gdome_ent_normalize (GdomeEntity *self, GdomeException *exc);
GdomeBoolean gdome_ent_isSupported (GdomeEntity *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDOMString *gdome_ent_namespaceURI (GdomeEntity *self, GdomeException *exc);
GdomeDOMString *gdome_ent_prefix (GdomeEntity *self, GdomeException *exc);
void gdome_ent_set_prefix (GdomeEntity *self, GdomeDOMString *prefix, GdomeException *exc);
GdomeDOMString *gdome_ent_localName (GdomeEntity *self, GdomeException *exc);
GdomeBoolean gdome_ent_hasAttributes (GdomeEntity *self, GdomeException *exc);
void gdome_ent_addEventListener (GdomeEntity *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
void gdome_ent_removeEventListener (GdomeEntity *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
GdomeBoolean gdome_ent_dispatchEvent (GdomeEntity *self, GdomeEvent *evt, GdomeException *exc);
void gdome_ent_subTreeDispatchEvent (GdomeEntity *self, GdomeEvent *evt, GdomeException *exc);
GdomeBoolean gdome_ent_canAppend (GdomeEntity *self, GdomeNode *newChild, GdomeException *exc);



void gdome_er_ref (GdomeEntityReference *self, GdomeException *exc);
void gdome_er_unref (GdomeEntityReference *self, GdomeException *exc);
gpointer gdome_er_query_interface (GdomeEntityReference *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_er_nodeName (GdomeEntityReference *self, GdomeException *exc);
GdomeDOMString *gdome_er_nodeValue (GdomeEntityReference *self, GdomeException *exc);
void gdome_er_set_nodeValue (GdomeEntityReference *self, GdomeDOMString *nodeValue, GdomeException *exc);
unsigned short gdome_er_nodeType (GdomeEntityReference *self, GdomeException *exc);
GdomeNode *gdome_er_parentNode (GdomeEntityReference *self, GdomeException *exc);
GdomeNodeList *gdome_er_childNodes (GdomeEntityReference *self, GdomeException *exc);
GdomeNode *gdome_er_firstChild (GdomeEntityReference *self, GdomeException *exc);
GdomeNode *gdome_er_lastChild (GdomeEntityReference *self, GdomeException *exc);
GdomeNode *gdome_er_previousSibling (GdomeEntityReference *self, GdomeException *exc);
GdomeNode *gdome_er_nextSibling (GdomeEntityReference *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_er_attributes (GdomeEntityReference *self, GdomeException *exc);
GdomeDocument *gdome_er_ownerDocument (GdomeEntityReference *self, GdomeException *exc);
GdomeNode *gdome_er_insertBefore (GdomeEntityReference *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc);
GdomeNode *gdome_er_replaceChild (GdomeEntityReference *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_er_removeChild (GdomeEntityReference *self, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_er_appendChild (GdomeEntityReference *self, GdomeNode *newChild, GdomeException *exc);
GdomeBoolean gdome_er_hasChildNodes (GdomeEntityReference *self, GdomeException *exc);
GdomeNode *gdome_er_cloneNode (GdomeEntityReference *self, GdomeBoolean deep, GdomeException *exc);
void gdome_er_normalize (GdomeEntityReference *self, GdomeException *exc);
GdomeBoolean gdome_er_isSupported (GdomeEntityReference *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDOMString *gdome_er_namespaceURI (GdomeEntityReference *self, GdomeException *exc);
GdomeDOMString *gdome_er_prefix (GdomeEntityReference *self, GdomeException *exc);
void gdome_er_set_prefix (GdomeEntityReference *self, GdomeDOMString *prefix, GdomeException *exc);
GdomeDOMString *gdome_er_localName (GdomeEntityReference *self, GdomeException *exc);
GdomeBoolean gdome_er_hasAttributes (GdomeEntityReference *self, GdomeException *exc);
void gdome_er_addEventListener (GdomeEntityReference *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
void gdome_er_removeEventListener (GdomeEntityReference *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
GdomeBoolean gdome_er_dispatchEvent (GdomeEntityReference *self, GdomeEvent *evt, GdomeException *exc);
void gdome_er_subTreeDispatchEvent (GdomeEntityReference *self, GdomeEvent *evt, GdomeException *exc);
GdomeBoolean gdome_er_canAppend (GdomeEntityReference *self, GdomeNode *newChild, GdomeException *exc);



void gdome_nnm_ref (GdomeNamedNodeMap *self, GdomeException *exc);
void gdome_nnm_unref (GdomeNamedNodeMap *self, GdomeException *exc);
gpointer gdome_nnm_query_interface (GdomeNamedNodeMap *self, const char *interface, GdomeException *exc);
GdomeNode *gdome_nnm_getNamedItem (GdomeNamedNodeMap *self, GdomeDOMString *name, GdomeException *exc);
GdomeNode *gdome_nnm_setNamedItem (GdomeNamedNodeMap *self, GdomeNode *arg, GdomeException *exc);
GdomeNode *gdome_nnm_removeNamedItem (GdomeNamedNodeMap *self, GdomeDOMString *name, GdomeException *exc);
GdomeNode *gdome_nnm_item (GdomeNamedNodeMap *self, gulong index, GdomeException *exc);
gulong gdome_nnm_length (GdomeNamedNodeMap *self, GdomeException *exc);
GdomeNode *gdome_nnm_getNamedItemNS (GdomeNamedNodeMap *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc);
GdomeNode *gdome_nnm_setNamedItemNS (GdomeNamedNodeMap *self, GdomeNode *arg, GdomeException *exc);
GdomeNode *gdome_nnm_removeNamedItemNS (GdomeNamedNodeMap *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc);



void gdome_nl_ref (GdomeNodeList *self, GdomeException *exc);
void gdome_nl_unref (GdomeNodeList *self, GdomeException *exc);
gpointer gdome_nl_query_interface (GdomeNodeList *self, const char *interface, GdomeException *exc);
GdomeNode *gdome_nl_item (GdomeNodeList *self, gulong index, GdomeException *exc);
gulong gdome_nl_length (GdomeNodeList *self, GdomeException *exc);



GdomeDOMString *gdome_not_publicId (GdomeNotation *self, GdomeException *exc);
GdomeDOMString *gdome_not_systemId (GdomeNotation *self, GdomeException *exc);
void gdome_not_ref (GdomeNotation *self, GdomeException *exc);
void gdome_not_unref (GdomeNotation *self, GdomeException *exc);
gpointer gdome_not_query_interface (GdomeNotation *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_not_nodeName (GdomeNotation *self, GdomeException *exc);
GdomeDOMString *gdome_not_nodeValue (GdomeNotation *self, GdomeException *exc);
void gdome_not_set_nodeValue (GdomeNotation *self, GdomeDOMString *nodeValue, GdomeException *exc);
unsigned short gdome_not_nodeType (GdomeNotation *self, GdomeException *exc);
GdomeNode *gdome_not_parentNode (GdomeNotation *self, GdomeException *exc);
GdomeNodeList *gdome_not_childNodes (GdomeNotation *self, GdomeException *exc);
GdomeNode *gdome_not_firstChild (GdomeNotation *self, GdomeException *exc);
GdomeNode *gdome_not_lastChild (GdomeNotation *self, GdomeException *exc);
GdomeNode *gdome_not_previousSibling (GdomeNotation *self, GdomeException *exc);
GdomeNode *gdome_not_nextSibling (GdomeNotation *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_not_attributes (GdomeNotation *self, GdomeException *exc);
GdomeDocument *gdome_not_ownerDocument (GdomeNotation *self, GdomeException *exc);
GdomeNode *gdome_not_insertBefore (GdomeNotation *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc);
GdomeNode *gdome_not_replaceChild (GdomeNotation *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_not_removeChild (GdomeNotation *self, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_not_appendChild (GdomeNotation *self, GdomeNode *newChild, GdomeException *exc);
GdomeBoolean gdome_not_hasChildNodes (GdomeNotation *self, GdomeException *exc);
GdomeNode *gdome_not_cloneNode (GdomeNotation *self, GdomeBoolean deep, GdomeException *exc);
void gdome_not_normalize (GdomeNotation *self, GdomeException *exc);
GdomeBoolean gdome_not_isSupported (GdomeNotation *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDOMString *gdome_not_namespaceURI (GdomeNotation *self, GdomeException *exc);
GdomeDOMString *gdome_not_prefix (GdomeNotation *self, GdomeException *exc);
void gdome_not_set_prefix (GdomeNotation *self, GdomeDOMString *prefix, GdomeException *exc);
GdomeDOMString *gdome_not_localName (GdomeNotation *self, GdomeException *exc);
GdomeBoolean gdome_not_hasAttributes (GdomeNotation *self, GdomeException *exc);
void gdome_not_addEventListener (GdomeNotation *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
void gdome_not_removeEventListener (GdomeNotation *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
GdomeBoolean gdome_not_dispatchEvent (GdomeNotation *self, GdomeEvent *evt, GdomeException *exc);
void gdome_not_subTreeDispatchEvent (GdomeNotation *self, GdomeEvent *evt, GdomeException *exc);
GdomeBoolean gdome_not_canAppend (GdomeNotation *self, GdomeNode *newChild, GdomeException *exc);



GdomeDOMString *gdome_pi_target (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeDOMString *gdome_pi_data (GdomeProcessingInstruction *self, GdomeException *exc);
void gdome_pi_set_data (GdomeProcessingInstruction *self, GdomeDOMString *data, GdomeException *exc);
void gdome_pi_ref (GdomeProcessingInstruction *self, GdomeException *exc);
void gdome_pi_unref (GdomeProcessingInstruction *self, GdomeException *exc);
gpointer gdome_pi_query_interface (GdomeProcessingInstruction *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_pi_nodeName (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeDOMString *gdome_pi_nodeValue (GdomeProcessingInstruction *self, GdomeException *exc);
void gdome_pi_set_nodeValue (GdomeProcessingInstruction *self, GdomeDOMString *nodeValue, GdomeException *exc);
unsigned short gdome_pi_nodeType (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeNode *gdome_pi_parentNode (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeNodeList *gdome_pi_childNodes (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeNode *gdome_pi_firstChild (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeNode *gdome_pi_lastChild (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeNode *gdome_pi_previousSibling (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeNode *gdome_pi_nextSibling (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeNamedNodeMap *gdome_pi_attributes (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeDocument *gdome_pi_ownerDocument (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeNode *gdome_pi_insertBefore (GdomeProcessingInstruction *self, GdomeNode *newChild, GdomeNode *refChild, GdomeException *exc);
GdomeNode *gdome_pi_replaceChild (GdomeProcessingInstruction *self, GdomeNode *newChild, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_pi_removeChild (GdomeProcessingInstruction *self, GdomeNode *oldChild, GdomeException *exc);
GdomeNode *gdome_pi_appendChild (GdomeProcessingInstruction *self, GdomeNode *newChild, GdomeException *exc);
GdomeBoolean gdome_pi_hasChildNodes (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeNode *gdome_pi_cloneNode (GdomeProcessingInstruction *self, GdomeBoolean deep, GdomeException *exc);
void gdome_pi_normalize (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeBoolean gdome_pi_isSupported (GdomeProcessingInstruction *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
GdomeDOMString *gdome_pi_namespaceURI (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeDOMString *gdome_pi_prefix (GdomeProcessingInstruction *self, GdomeException *exc);
void gdome_pi_set_prefix (GdomeProcessingInstruction *self, GdomeDOMString *prefix, GdomeException *exc);
GdomeDOMString *gdome_pi_localName (GdomeProcessingInstruction *self, GdomeException *exc);
GdomeBoolean gdome_pi_hasAttributes (GdomeProcessingInstruction *self, GdomeException *exc);
void gdome_pi_addEventListener (GdomeProcessingInstruction *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
void gdome_pi_removeEventListener (GdomeProcessingInstruction *self, GdomeDOMString *type, GdomeEventListener *listener, GdomeBoolean useCapture, GdomeException *exc);
GdomeBoolean gdome_pi_dispatchEvent (GdomeProcessingInstruction *self, GdomeEvent *evt, GdomeException *exc);
void gdome_pi_subTreeDispatchEvent (GdomeProcessingInstruction *self, GdomeEvent *evt, GdomeException *exc);
GdomeBoolean gdome_pi_canAppend (GdomeProcessingInstruction *self, GdomeNode *newChild, GdomeException *exc);
GdomeElement * gdome_xpns_ownerElement (GdomeXPathNamespace *self, GdomeException *exc);
GdomeDocument     * gdome_xpns_ownerDocument        (GdomeXPathNamespace *self,
																										 GdomeException *exc);
GdomeDOMString    * gdome_xpns_prefix               (GdomeXPathNamespace *self,
																										 GdomeException *exc);
GdomeDOMString    * gdome_xpns_nodeName             (GdomeXPathNamespace *self,
																										 GdomeException *exc);
unsigned short      gdome_xpns_nodeType             (GdomeXPathNamespace *self,
																										 GdomeException *exc);
GdomeDOMString    * gdome_xpns_namespaceURI         (GdomeXPathNamespace *self,
																										 GdomeException *exc);


/* Casting Macro */
#include <gdome-util.h>
/**
 * GDOME_A:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeAttr one.
 */
#define GDOME_A(node) gdome_cast_a((GdomeNode *)node)
/**
 * GDOME_CD:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeCharacterData one.
 */
#define GDOME_CD(node) gdome_cast_cd((GdomeNode *)node)
/**
 * GDOME_CDS:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeCDATASection one.
 */
#define GDOME_CDS(node) gdome_cast_cds((GdomeNode *)node)
/**
 * GDOME_C:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeComment one.
 */
#define GDOME_C(node) gdome_cast_c((GdomeNode *)node)
/**
 * GDOME_DOC:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeDocument one.
 */
#define GDOME_DOC(node) gdome_cast_doc((GdomeNode *)node)
/**
 * GDOME_DF:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeDocumentFragment one.
 */
#define GDOME_DF(node) gdome_cast_df((GdomeNode *)node)
/**
 * GDOME_DT:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeDocumentType one.
 */
#define GDOME_DT(node) gdome_cast_dt((GdomeNode *)node)
/**
 * GDOME_EL:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeElement one.
 */
#define GDOME_EL(node) gdome_cast_el((GdomeNode *)node)
/**
 * GDOME_ENT:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeEntity one.
 */
#define GDOME_ENT(node) gdome_cast_ent((GdomeNode *)node)
/**
 * GDOME_ER:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeEntityReference one.
 */
#define GDOME_ER(node) gdome_cast_er((GdomeNode *)node)
/**
 * GDOME_N:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeNode one.
 */
#define GDOME_N(node) gdome_cast_n((GdomeNode *)node)
/**
 * GDOME_NOT:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeNotation one.
 */
#define GDOME_NOT(node) gdome_cast_not((GdomeNode *)node)
/**
 * GDOME_PI:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeProcessingInstruction one.
 */
#define GDOME_PI(node) gdome_cast_pi((GdomeNode *)node)
/**
 * GDOME_T:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeText one.
 */
#define GDOME_T(node) gdome_cast_t((GdomeNode *)node)
/**
 * GDOME_EVNTT:
 * @node:  Node Object ref
 *
 * Cast a ganeral GdomeNode reference to a GdomeEventTarget one.
 */
#define GDOME_EVNTT(node) gdome_cast_evntt((GdomeNode *)node)

/**
 * GDOME_XMLNS_NAMESPACE:
 *
 * This is the namespace for the xmlns: prefix for namespace declaration
 * attributes.
 */
#define GDOME_XMLNS_NAMESPACE \
    (const xmlChar *) "http://www.w3.org/2000/xmlns/"

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* GDOME_H */
