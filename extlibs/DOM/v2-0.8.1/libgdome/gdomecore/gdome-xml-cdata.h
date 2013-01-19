/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-cdata.h
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


/* ----------------------------------------------------------------------- */
/* ---------------------------  CharacterData  --------------------------- */
/* ---- This implements the CharacterData interface of the DOM1 spec  ---- */
/* ----------------------------------------------------------------------- */


#ifndef GDOME_CDATA_FILE
#define GDOME_CDATA_FILE

struct _GdomeCharacterDataVtab {
  GdomeNodeVtab super;
  GdomeDOMString *(*data) (GdomeCharacterData *self, GdomeException *exc);
  void (*set_data) (GdomeCharacterData *self, GdomeDOMString *data, GdomeException *exc);
  gulong (*length) (GdomeCharacterData *self, GdomeException *exc);
  GdomeDOMString *(*substringData) (GdomeCharacterData *self, gulong offset, gulong count, GdomeException *exc);
  void (*appendData) (GdomeCharacterData *self, GdomeDOMString *arg, GdomeException *exc);
  void (*insertData) (GdomeCharacterData *self, gulong offset, GdomeDOMString *arg, GdomeException *exc);
  void (*deleteData) (GdomeCharacterData *self, gulong offset, gulong count, GdomeException *exc);
  void (*replaceData) (GdomeCharacterData *self, gulong offset, gulong count, GdomeDOMString *arg, GdomeException *exc);
};

typedef struct _Gdome_xml_CharacterData Gdome_xml_CharacterData;
struct _Gdome_xml_CharacterData {
  GdomeCharacterData super;
	const GdomeCharacterDataVtab *vtab;
  int refcnt;
	xmlNode *n;
  GdomeAccessType accessType;
  Gdome_xml_ListenerList *ll;
};


extern const GdomeCharacterDataVtab gdome_xml_cd_vtab;



gpointer           gdome_xml_cd_query_interface (GdomeNode *self,
                                                 const char *interface,
                                                 GdomeException *exc);
GdomeDOMString   * gdome_xml_cd_data        (GdomeCharacterData *self, 
																						 GdomeException *exc);
void               gdome_xml_cd_set_data    (GdomeCharacterData *self, 
																						 GdomeDOMString *data, 
																						 GdomeException *exc);
gulong             gdome_xml_cd_length      (GdomeCharacterData *self,
																						 GdomeException *exc);
GdomeDOMString   * gdome_xml_cd_substringData (GdomeCharacterData *self, 
																							 gulong offset,
																							 gulong count,
																							 GdomeException *exc);
void               gdome_xml_cd_appendData  (GdomeCharacterData *self, 
																						 GdomeDOMString *arg, 
																						 GdomeException *exc);
void               gdome_xml_cd_insertData  (GdomeCharacterData *self, 
																						 gulong offset,
																						 GdomeDOMString *arg, 
																						 GdomeException *exc);
void               gdome_xml_cd_deleteData  (GdomeCharacterData *self, 
																						 gulong offset,
																						 gulong count,
																						 GdomeException *exc);
void               gdome_xml_cd_replaceData (GdomeCharacterData *self, 
																						 gulong offset,
																						 gulong count,
																						 GdomeDOMString *arg, 
																						 GdomeException *exc);

#endif /* GDOME_CDATA_FILE */
