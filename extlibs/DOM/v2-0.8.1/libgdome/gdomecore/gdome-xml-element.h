/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-element.h
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


/* ----------------------------------------------------------------- */
/* ---------------------------  Element  --------------------------- */
/* ----------------------------------------------------------------- */


#ifndef GDOME_ELEMENT_FILE
#define GDOME_ELEMENT_FILE

struct _GdomeElementVtab {
	GdomeNodeVtab super;
	GdomeDOMString *(*tagName) (GdomeElement *self, GdomeException *exc);
  GdomeDOMString *(*getAttribute) (GdomeElement *self, GdomeDOMString *name, GdomeException *exc);
  void (*setAttribute) (GdomeElement *self, GdomeDOMString *name, GdomeDOMString *value, GdomeException *exc);
  void (*removeAttribute) (GdomeElement *self, GdomeDOMString *name, GdomeException *exc);
  GdomeAttr *(*getAttributeNode) (GdomeElement *self, GdomeDOMString *name, GdomeException *exc);
  GdomeAttr *(*setAttributeNode) (GdomeElement *self, GdomeAttr *newAttr, GdomeException *exc);
  GdomeAttr *(*removeAttributeNode) (GdomeElement *self, GdomeAttr *oldAttr, GdomeException *exc);
  GdomeNodeList *(*getElementsByTagName) (GdomeElement *self, GdomeDOMString *name, GdomeException *exc);
  GdomeDOMString *(*getAttributeNS) (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc);
  void (*setAttributeNS) (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeDOMString *value, GdomeException *exc);
  void (*removeAttributeNS) (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc);
  GdomeAttr *(*getAttributeNodeNS) (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc);
	GdomeAttr *(*setAttributeNodeNS) (GdomeElement *self, GdomeAttr *newAttr, GdomeException *exc);
	GdomeNodeList *(*getElementsByTagNameNS) (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc);
  GdomeBoolean (*hasAttribute) (GdomeElement *self, GdomeDOMString *name, GdomeException *exc);
  GdomeBoolean (*hasAttributeNS) (GdomeElement *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc);
};

typedef struct _Gdome_xml_Element Gdome_xml_Element;
struct _Gdome_xml_Element {
  GdomeElement super;
	const GdomeElementVtab *vtab;
  int refcnt;
  xmlNode *n;
  GdomeAccessType accessType;
  Gdome_xml_ListenerList *ll;
};




/* forward declarations */
gpointer             gdome_xml_el_query_interface        (GdomeNode *self,
                                                          const char *interface,
                                                          GdomeException *exc);
GdomeDOMString     * gdome_xml_el_tagName                (GdomeElement *self, 
																													GdomeException *exc);
GdomeDOMString     * gdome_xml_el_getAttribute           (GdomeElement *self, 
																													GdomeDOMString *name, 
																													GdomeException *exc);
void                 gdome_xml_el_setAttribute           (GdomeElement *self, 
																													GdomeDOMString *name, 
																													GdomeDOMString *value, 
																													GdomeException *exc);
void                 gdome_xml_el_removeAttribute        (GdomeElement *self, 
																													GdomeDOMString *name, 
																													GdomeException *exc);
GdomeAttr          * gdome_xml_el_getAttributeNode       (GdomeElement *self, 
																													GdomeDOMString *name, 
																													GdomeException *exc);
GdomeAttr          * gdome_xml_el_setAttributeNode       (GdomeElement *self, 
																													GdomeAttr *newAttr, 
																													GdomeException *exc);
GdomeAttr          * gdome_xml_el_removeAttributeNode    (GdomeElement *self, 
																													GdomeAttr *oldAttr, 
																													GdomeException *exc);
GdomeNodeList      * gdome_xml_el_getElementsByTagName   (GdomeElement *self, 
																													GdomeDOMString *name, 
																													GdomeException *exc);
GdomeDOMString     * gdome_xml_el_getAttributeNS         (GdomeElement *self, 
																													GdomeDOMString *namespaceURI, 
																													GdomeDOMString *localName, 
																													GdomeException *exc);
void                 gdome_xml_el_setAttributeNS         (GdomeElement *self, 
																													GdomeDOMString *namespaceURI, 
																													GdomeDOMString *localName, 
																													GdomeDOMString *value, 
																													GdomeException *exc);
void                 gdome_xml_el_removeAttributeNS      (GdomeElement *self, 
																													GdomeDOMString *namespaceURI, 
																													GdomeDOMString *localName, 
																													GdomeException *exc);
GdomeAttr          * gdome_xml_el_getAttributeNodeNS     (GdomeElement *self, 
																													GdomeDOMString *namespaceURI, 
																													GdomeDOMString *localName, 
																													GdomeException *exc);
GdomeAttr          * gdome_xml_el_setAttributeNodeNS     (GdomeElement *self, 
																													GdomeAttr *newAttr, 
																													GdomeException *exc);
GdomeNodeList      * gdome_xml_el_getElementsByTagNameNS (GdomeElement *self, 
																													GdomeDOMString *namespaceURI, 
																													GdomeDOMString *localName, 
																													GdomeException *exc);
GdomeNamedNodeMap  * gdome_xml_el_attributes             (GdomeNode *self, 
																													GdomeException *exc);
GdomeBoolean         gdome_xml_el_hasAttribute           (GdomeElement *self, 
																													GdomeDOMString *name, 
																													GdomeException *exc);
GdomeBoolean         gdome_xml_el_hasAttributeNs         (GdomeElement *self, 
																													GdomeDOMString *namespaceURI, 
																													GdomeDOMString *localName, 
																													GdomeException *exc);


extern const GdomeElementVtab gdome_xml_el_vtab;

#endif /* GDOME_ELEMENT_FILE */
