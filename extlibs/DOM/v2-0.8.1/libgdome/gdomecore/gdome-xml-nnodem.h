/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-nnm.h
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

#ifndef GDOME_XML_NNODEM_H
#define GDOME_XML_NNODEM_H

struct _GdomeNamedNodeMapVtab {
	void (*ref) (GdomeNamedNodeMap *self, GdomeException *exc);
	void (*unref) (GdomeNamedNodeMap *self, GdomeException *exc);
	gpointer (*query_interface) (GdomeNamedNodeMap *self, const char *interface, GdomeException *exc);
	GdomeNode *(*getNamedItem) (GdomeNamedNodeMap *self, GdomeDOMString *name, GdomeException *exc);
	GdomeNode *(*setNamedItem) (GdomeNamedNodeMap *self, GdomeNode *arg, GdomeException *exc);
	GdomeNode *(*removeNamedItem) (GdomeNamedNodeMap *self, GdomeDOMString *name, GdomeException *exc);
	GdomeNode *(*item) (GdomeNamedNodeMap *self, gulong index, GdomeException *exc);
	gulong (*length) (GdomeNamedNodeMap *self, GdomeException *exc);
	GdomeNode *(*getNamedItemNS) (GdomeNamedNodeMap *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc);
	GdomeNode *(*setNamedItemNS) (GdomeNamedNodeMap *self, GdomeNode *arg, GdomeException *exc);
	GdomeNode *(*removeNamedItemNS) (GdomeNamedNodeMap *self, GdomeDOMString *namespaceURI, GdomeDOMString *localName, GdomeException *exc);
};

typedef struct _Gdome_xml_NamedNodeMap Gdome_xml_NamedNodeMap;
struct _Gdome_xml_NamedNodeMap {
  GdomeNamedNodeMap super;
	const GdomeNamedNodeMapVtab *vtab;
  int refcnt;

  union {
    xmlHashTable *ht;
    xmlNode *lst;
  } data;
  xmlNs *ns;
  GdomeDocument *doc;
  GdomeElement *elem;
	GdomeAccessType accessType;
  GdomeNodeType type;
};

extern const GdomeNamedNodeMapVtab gdome_xml_nnm_vtab;

GdomeNamedNodeMap *gdome_xml_nnm_mkref         (GdomeDocument *doc,
                                                GdomeElement *elem,
                                                void *data,
                                                void *data2,
                                                GdomeAccessType accessType,
                                                GdomeNodeType type);
void          gdome_xml_nnm_ref                (GdomeNamedNodeMap *self,
                                                GdomeException *exc);
void          gdome_xml_nnm_unref              (GdomeNamedNodeMap *self,
                                                GdomeException *exc);
gpointer      gdome_xml_nnm_query_interface    (GdomeNamedNodeMap *self,
                                                const char *interface,
                                                GdomeException *exc);
gulong        gdome_xml_nnm_length             (GdomeNamedNodeMap *self,
                                                GdomeException *exc);
GdomeNode *   gdome_xml_nnm_getNamedItem       (GdomeNamedNodeMap *self,
                                                GdomeDOMString *name,
                                                GdomeException *exc);
GdomeNode *   gdome_xml_nnm_getNamedItemNS     (GdomeNamedNodeMap *self,
                                                GdomeDOMString *namespaceURI,
                                                GdomeDOMString *localName,
                                                GdomeException *exc);
GdomeNode *   gdome_xml_nnm_item               (GdomeNamedNodeMap *self,
                                                gulong index,
                                                GdomeException *exc);
GdomeNode *   gdome_xml_nnm_removeNamedItem    (GdomeNamedNodeMap *self,
                                                GdomeDOMString *name,
                                                GdomeException *exc);
GdomeNode *   gdome_xml_nnm_removeNamedItemNS  (GdomeNamedNodeMap *self,
                                                GdomeDOMString *namespaceURI,
                                                GdomeDOMString *localName,
                                                GdomeException *exc);
GdomeNode *   gdome_xml_nnm_setNamedItem       (GdomeNamedNodeMap *self,
                                                GdomeNode *arg,
                                                GdomeException *exc);
GdomeNode *   gdome_xml_nnm_setNamedItemNS     (GdomeNamedNodeMap *self,
                                                GdomeNode *arg,
                                                GdomeException *exc);
GdomeBoolean  gdome_xml_nnm_eventEnabledByCode (GdomeNamedNodeMap *self,
																								guint32 code);
#endif /* GDOME_XML_NNODEM_H */
