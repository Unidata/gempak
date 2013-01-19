/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-document.h
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

#ifndef GDOME_DOC_FILE
#define GDOME_DOC_FILE

struct _GdomeDocumentVtab {
  GdomeNodeVtab super;
  GdomeDocumentType *(*doctype) (GdomeDocument *self, GdomeException *exc);
  GdomeDOMImplementation *(*implementation) (GdomeDocument *self, GdomeException *exc);
  GdomeElement *(*documentElement) (GdomeDocument *self, GdomeException *exc);
  GdomeElement *(*createElement) (GdomeDocument *self, GdomeDOMString *tagName, GdomeException *exc);
  GdomeDocumentFragment *(*createDocumentFragment) (GdomeDocument *self, GdomeException *exc);
  GdomeText *(*createTextNode) (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc);
  GdomeComment *(*createComment) (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc);
  GdomeCDATASection *(*createCDATASection) (GdomeDocument *self, GdomeDOMString *data, GdomeException *exc);
  GdomeProcessingInstruction *(*createProcessingInstruction) (GdomeDocument *self, GdomeDOMString *target, GdomeDOMString *data, GdomeException *exc);
  GdomeAttr *(*createAttribute) (GdomeDocument *self, GdomeDOMString *name, GdomeException *exc);
  GdomeEntityReference *(*createEntityReference) (GdomeDocument *self, GdomeDOMString *name, GdomeException *exc);
  GdomeNodeList *(*getElementsByTagName) (GdomeDocument *self, GdomeDOMString *tagname, GdomeException *exc);
  GdomeNode *(*importNode) (GdomeDocument *self, GdomeNode *importedNode, GdomeBoolean deep, GdomeException *exc);
  GdomeElement *(*createElementNS) (GdomeDocument *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeException *exc);
  GdomeAttr *(*createAttributeNS) (GdomeDocument *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeException *exc);
  GdomeNodeList *(*getElementsByTagNameNS) (GdomeDocument *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc);
  GdomeElement *(*getElementById) (GdomeDocument *self, GdomeDOMString *elementId, GdomeException *exc);
  GdomeEvent *(*createEvent) (GdomeDocument *self, GdomeDOMString *eventType, GdomeException *exc);
};

typedef struct _Gdome_xml_Document Gdome_xml_Document;
struct _Gdome_xml_Document {
	GdomeDocument super;
	const GdomeDocumentVtab *vtab;
	int refcnt;
	xmlDoc *n;
  GdomeAccessType accessType;
  Gdome_xml_ListenerList *ll;
  int livenodes;
	unsigned event_mask;
};

void                      gdome_xml_doc_enableEventByName    (GdomeDocument *self,
																															const char *name);
void                      gdome_xml_doc_enableEventByCode    (GdomeDocument *self,
																															unsigned int code);
void                      gdome_xml_doc_disableEventByName   (GdomeDocument *self,
																															const char *name);
void                      gdome_xml_doc_disableEventByCode   (GdomeDocument *self,
																															unsigned int code);
GdomeBoolean              gdome_xml_doc_eventEnabledByName   (GdomeDocument *self,
																															const char *name);
GdomeBoolean              gdome_xml_doc_eventEnabledByCode   (GdomeDocument *self,
																															unsigned int code);

GdomeDocument           * gdome_xml_doc_mkref                  (xmlDoc *n);
void                      gdome_xml_doc_unref                  (GdomeNode *self,
                                                                GdomeException *exc);
gpointer                  gdome_xml_doc_query_interface        (GdomeNode *self,
                                                                const char *interface,
                                                                GdomeException *exc);
GdomeDocumentType       * gdome_xml_doc_doctype                (GdomeDocument *self,
																																GdomeException *exc);
GdomeDOMImplementation  * gdome_xml_doc_implementation         (GdomeDocument *self,
																																GdomeException *exc);
GdomeElement            * gdome_xml_doc_documentElement        (GdomeDocument *self,
																																GdomeException *exc);
GdomeElement            * gdome_xml_doc_createElement          (GdomeDocument *self,
																																GdomeDOMString *tagName, 
																																GdomeException *exc);
GdomeElement            * gdome_xml_doc_createElementNS        (GdomeDocument *self,
																																GdomeDOMString *namespaceURI,
																																GdomeDOMString *qualifiedName,
																																GdomeException *exc);
GdomeDocumentFragment   * gdome_xml_doc_createDocumentFragment (GdomeDocument *self,
																																GdomeException *exc);
GdomeText               * gdome_xml_doc_createTextNode         (GdomeDocument *self, 
																																GdomeDOMString *data, 
																																GdomeException *exc);
GdomeComment            * gdome_xml_doc_createComment          (GdomeDocument *self, 
																																GdomeDOMString *data, 
																																GdomeException *exc);
GdomeCDATASection       * gdome_xml_doc_createCDATASection     (GdomeDocument *self, 
																																GdomeDOMString *data, 
																																GdomeException *exc);
GdomeProcessingInstruction   * gdome_xml_doc_createProcessingInstruction     (GdomeDocument *self, 
																																							GdomeDOMString *target, 
																																							GdomeDOMString *data, 
																																							GdomeException *exc);
GdomeAttr               * gdome_xml_doc_createAttribute        (GdomeDocument *self, 
																																GdomeDOMString *name, 
																																GdomeException *exc);
GdomeAttr               * gdome_xml_doc_createAttributeNS      (GdomeDocument *self,
																																GdomeDOMString *namespaceURI,
																																GdomeDOMString *qualifiedName,
																																GdomeException *exc);
GdomeEntityReference    * gdome_xml_doc_createEntityReference  (GdomeDocument *self,
																																GdomeDOMString *name,
																																GdomeException *exc);
GdomeElement            * gdome_xml_doc_getElementById         (GdomeDocument *self,
																																GdomeDOMString *elementId,
																																GdomeException *exc);
GdomeNodeList           * gdome_xml_doc_getElementsByTagName   (GdomeDocument *self,
																																GdomeDOMString *tagname, 
																																GdomeException *exc);
GdomeNodeList           * gdome_xml_doc_getElementsByTagNameNS (GdomeDocument *self,
																																GdomeDOMString *namespaceURI,
																																GdomeDOMString *localName,
																																GdomeException *exc);
GdomeNode               * gdome_xml_doc_importNode             (GdomeDocument *self,
                                                                GdomeNode *importedNode,
                                                                GdomeBoolean deep,
                                                                GdomeException *exc);
GdomeEvent *              gdome_xml_doc_createEvent            (GdomeDocument *self,
                                                                GdomeDOMString *eventType,
                                                                GdomeException *exc);


extern const GdomeDocumentVtab gdome_xml_doc_vtab;

#endif /* GDOME_DOC_FILE */












