/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-nodel.h
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


#ifndef GDOME_FILE_NODEL
#define GDOME_FILE_NODEL

struct _GdomeNodeListVtab {
  void (*ref) (GdomeNodeList *self, GdomeException *exc);
  void (*unref) (GdomeNodeList *self, GdomeException *exc);
  gpointer (*query_interface) (GdomeNodeList *self, const char *interface, GdomeException *exc);
  GdomeNode *(*item) (GdomeNodeList *self, gulong index, GdomeException *exc);
  gulong (*length) (GdomeNodeList *self, GdomeException *exc);
};

typedef struct _Gdome_xml_NodeList Gdome_xml_NodeList;
struct _Gdome_xml_NodeList {
  GdomeNodeList super;
	const GdomeNodeListVtab *vtab;
	int refcnt;
  GdomeNode *root;
  GdomeDOMString *tagName;
  GdomeDOMString *tagURI;
  GdomeAccessType accessType;
};

GdomeNodeList * gdome_xml_nl_mkref            (GdomeNode *root,
                                               GdomeDOMString *tagName,
                                               GdomeDOMString *tagURI,
                                               GdomeAccessType accessType);
void            gdome_xml_nl_ref              (GdomeNodeList *self, 
																							 GdomeException *exc);
void            gdome_xml_nl_unref            (GdomeNodeList *self, 
																							 GdomeException *exc);
gpointer        gdome_xml_nl_query_interface  (GdomeNodeList *self,
																							 const char *interface, 
																							 GdomeException *exc);
GdomeNode     * gdome_xml_nl_item             (GdomeNodeList *self, 
																							 gulong index,
																							 GdomeException *exc);
gulong          gdome_xml_nl_length           (GdomeNodeList *self,
																							 GdomeException *exc);

extern const GdomeNodeListVtab gdome_xml_nl_vtab;

#endif /* GDOME_FILE_NODEL */







