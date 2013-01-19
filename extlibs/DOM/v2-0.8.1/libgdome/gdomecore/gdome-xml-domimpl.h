/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-domimpl.h
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

/* ----------------------------------------------------------------- */
/* ----------------------  DOMImplementation  ---------------------- */
/* ----------------------------------------------------------------- */

#ifndef GDOME_DOMIMPL_FILE
#define GDOME_DOMIMPL_FILE

struct _GdomeDOMImplementationVtab {
  void (*ref) (GdomeDOMImplementation *self, GdomeException *exc);
  void (*unref) (GdomeDOMImplementation *self, GdomeException *exc);
  gpointer (*query_interface) (GdomeDOMImplementation *self, const char *interface, GdomeException *exc);
  GdomeBoolean (*hasFeature) (GdomeDOMImplementation *self, GdomeDOMString *feature, GdomeDOMString *version, GdomeException *exc);
  GdomeDocumentType *(*createDocumentType) (GdomeDOMImplementation *self, GdomeDOMString *qualifiedName, GdomeDOMString *publicId, GdomeDOMString *systemId, GdomeException *exc);
  GdomeDocument *(*createDocument) (GdomeDOMImplementation *self, GdomeDOMString *namespaceURI, GdomeDOMString *qualifiedName, GdomeDocumentType *doctype, GdomeException *exc);
  void (*freeDoc) (GdomeDOMImplementation *self, GdomeDocument *doc, GdomeException *exc);
  GdomeDocument *(*createDocFromURI) (GdomeDOMImplementation *self, const char *uri, unsigned int  mode, GdomeException *exc);
  GdomeDocument *(*createDocFromMemory) (GdomeDOMImplementation *self, char *buffer, unsigned int mode, GdomeException *exc);
  GdomeDocument *(*createDocFromURIWithEntitiesTable) (GdomeDOMImplementation *self, const char *uri, const GdomeEntitiesTableEntry entityTable[], unsigned int mode, GdomeException *exc);
  GdomeDocument *(*createDocFromMemoryWithEntitiesTable) (GdomeDOMImplementation *self, char *buffer, const GdomeEntitiesTableEntry entityTable[], unsigned int mode, GdomeException *exc);
  GdomeBoolean (*saveDocToFile) (GdomeDOMImplementation *self, GdomeDocument *doc, const char *filename, GdomeSavingCode mode, GdomeException *exc);
  GdomeBoolean (*saveDocToMemory) (GdomeDOMImplementation *self, GdomeDocument *doc, char **mem, GdomeSavingCode mode, GdomeException *exc);
	GdomeBoolean (*saveDocToFileEnc) (GdomeDOMImplementation *self, GdomeDocument *doc, const char *filename, const char *encoding, GdomeSavingCode mode, GdomeException *exc);
  GdomeBoolean (*saveDocToMemoryEnc) (GdomeDOMImplementation *self, GdomeDocument *doc, char **mem, const char *encoding, GdomeSavingCode mode, GdomeException *exc);
	void (*enableEvent) (GdomeDOMImplementation *self, GdomeDocument *doc, const char *name, GdomeException *exc);
	void (*disableEvent) (GdomeDOMImplementation *self, GdomeDocument* doc, const char *name, GdomeException *exc);
	GdomeBoolean (*eventIsEnabled) (GdomeDOMImplementation *self, GdomeDocument* doc, const char *name, GdomeException *exc);
};

typedef struct _Gdome_xml_DOMImplementation Gdome_xml_DOMImplementation;
struct _Gdome_xml_DOMImplementation {
	GdomeDOMImplementation super;
	const GdomeDOMImplementationVtab *vtab;
	int refcnt;
};

GdomeDOMImplementation * gdome_xml_di_mkref (void);

GdomeBoolean   gdome_xml_di_hasFeature (GdomeDOMImplementation *self,
                                        GdomeDOMString *feature,
                                        GdomeDOMString *version,
                                        GdomeException *exc);

GdomeDocumentType *gdome_xml_di_createDocumentType (GdomeDOMImplementation *self,
                                                    GdomeDOMString *qualifiedName,
                                                    GdomeDOMString *publicId,
                                                    GdomeDOMString *systemId,
                                                    GdomeException *exc);

GdomeDocument *gdome_xml_di_createDocument (GdomeDOMImplementation *self,
                                            GdomeDOMString *namespaceURI,
                                            GdomeDOMString *qualifiedName,
                                            GdomeDocumentType *doctype,
                                            GdomeException *exc);

void           gdome_xml_di_ref (GdomeDOMImplementation *self,
                                 GdomeException *exc);

void           gdome_xml_di_unref (GdomeDOMImplementation *self,
                                   GdomeException *exc);
gpointer       gdome_xml_di_query_interface (GdomeDOMImplementation *self,
                                             const char *interface, GdomeException *exc);

void            gdome_xml_di_freeDoc   (GdomeDOMImplementation *self,
                                        GdomeDocument *doc,
                                        GdomeException *exc);
GdomeDocument * gdome_xml_di_createDocFromURI (GdomeDOMImplementation *self,
                                               const char *uri,
                                               unsigned int mode,
                                               GdomeException *exc);
GdomeDocument * gdome_xml_di_createDocFromMemory (GdomeDOMImplementation *self,
                                                  char *buffer,
                                                  unsigned int mode,
                                                  GdomeException *exc);
GdomeDocument * gdome_xml_di_createDocFromURIWithEntitiesTable (GdomeDOMImplementation *self,
                                                                const char *uri,
                                                                const GdomeEntitiesTableEntry entityTable[],
                                                                unsigned int mode,
                                                                GdomeException *exc);
GdomeDocument * gdome_xml_di_createDocFromMemoryWithEntitiesTable (GdomeDOMImplementation *self,
                                                                   char *buffer,
                                                                   const GdomeEntitiesTableEntry entityTable[],
                                                                   unsigned int mode,
                                                                   GdomeException *exc);
GdomeBoolean gdome_xml_di_saveDocToFile (GdomeDOMImplementation *self,
                                         GdomeDocument *doc,
                                         const char *filename,
                                         GdomeSavingCode mode,
                                         GdomeException *exc);
GdomeBoolean gdome_xml_di_saveDocToFileEnc (GdomeDOMImplementation *self,
                                            GdomeDocument *doc,
																						const char *filename,
																						const char *encoding,
																						GdomeSavingCode mode,
																						GdomeException *exc);
GdomeBoolean gdome_xml_di_saveDocToMemory (GdomeDOMImplementation *self,
                                           GdomeDocument *doc,
                                           char **mem,
                                           GdomeSavingCode mode,
                                           GdomeException *exc);
GdomeBoolean gdome_xml_di_saveDocToMemoryEnc (GdomeDOMImplementation *self,
                                              GdomeDocument *doc,
                                              char **mem,
                                              const char *encoding,
                                              GdomeSavingCode mode,
                                              GdomeException *exc);
void gdome_xml_di_enableEvent (GdomeDOMImplementation *self,
															 GdomeDocument *doc,
															 const char *name,
															 GdomeException *exc);
void gdome_xml_di_disableEvent (GdomeDOMImplementation *self,
																GdomeDocument *doc,
																const char *name,
																GdomeException *exc);
GdomeBoolean gdome_xml_di_eventIsEnabled (GdomeDOMImplementation *self,
																					GdomeDocument *doc,
																					const char *name,
																					GdomeException *exc);

extern const GdomeDOMImplementationVtab gdome_xml_di_vtab;



#endif /* GDOME_DOMIMPL_FILE */


