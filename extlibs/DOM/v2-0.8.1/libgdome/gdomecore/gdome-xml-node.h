/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-node.h
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


/* -------------------------------------------------------------- */
/* ---------------------------  Node  --------------------------- */
/* -------------------------------------------------------------- */



#ifndef GDOME_NODE_FILE
#define GDOME_NODE_FILE

struct _GdomeNodeVtab {
	void                (*ref)                  (GdomeNode *self, GdomeException *exc);
	void                (*unref)                (GdomeNode *self, GdomeException *exc);
	gpointer            (*query_interface)      (GdomeNode *self, const char *interface,
                                               GdomeException *exc);
	GdomeDOMString *    (*nodeName)             (GdomeNode *self, GdomeException *exc);
	GdomeDOMString *    (*nodeValue)            (GdomeNode *self, GdomeException *exc);
	void                (*set_nodeValue)        (GdomeNode *self, GdomeDOMString *nodeValue,
                                               GdomeException *exc);
  unsigned short      (*nodeType)             (GdomeNode *self, GdomeException *exc);
	GdomeNode *         (*parentNode)           (GdomeNode *self, GdomeException *exc);
	GdomeNodeList *     (*childNodes)           (GdomeNode *self, GdomeException *exc);
	GdomeNode *         (*firstChild)           (GdomeNode *self, GdomeException *exc);
	GdomeNode *         (*lastChild)            (GdomeNode *self, GdomeException *exc);
	GdomeNode *         (*previousSibling)      (GdomeNode *self, GdomeException *exc);
	GdomeNode *         (*nextSibling)          (GdomeNode *self, GdomeException *exc);
	GdomeNamedNodeMap * (*attributes)           (GdomeNode *self, GdomeException *exc);
	GdomeDocument *     (*ownerDocument)        (GdomeNode *self, GdomeException *exc);
	GdomeNode *         (*insertBefore)         (GdomeNode *self, GdomeNode *newChild,
                                               GdomeNode *refChild, GdomeException *exc);
	GdomeNode *         (*replaceChild)         (GdomeNode *self, GdomeNode *newChild,
                                               GdomeNode *oldChild, GdomeException *exc);
	GdomeNode *         (*removeChild)          (GdomeNode *self, GdomeNode *oldChild,
                                               GdomeException *exc);
	GdomeNode *         (*appendChild)          (GdomeNode *self, GdomeNode *newChild,
                                               GdomeException *exc);
	GdomeBoolean        (*hasChildNodes)        (GdomeNode *self, GdomeException *exc);
	GdomeNode *         (*cloneNode)            (GdomeNode *self, GdomeBoolean deep,
                                               GdomeException *exc);
	void                (*normalize)            (GdomeNode *self, GdomeException *exc);
	GdomeBoolean        (*isSupported)          (GdomeNode *self, GdomeDOMString *feature,
                                               GdomeDOMString *version, GdomeException *exc);
  GdomeDOMString *    (*namespaceURI)         (GdomeNode *self, GdomeException *exc);
	GdomeDOMString *    (*prefix)               (GdomeNode *self, GdomeException *exc);
	void                (*set_prefix)           (GdomeNode *self, GdomeDOMString *prefix,
                                               GdomeException *exc);
	GdomeDOMString *    (*localName)            (GdomeNode *self, GdomeException *exc);
	GdomeBoolean        (*hasAttributes)        (GdomeNode *self, GdomeException *exc);
	void                (*addEventListener)     (GdomeNode *self, GdomeDOMString *type,
                                               GdomeEventListener *listener,
                                               GdomeBoolean useCapture,
                                               GdomeException *exc);
	void                (*removeEventListener)  (GdomeNode *self, GdomeDOMString *type,
                                               GdomeEventListener *listener,
                                               GdomeBoolean useCapture, GdomeException *exc);
	GdomeBoolean        (*dispatchEvent)        (GdomeNode *self, GdomeEvent *evt,
                                               GdomeException *exc);
  void                (*subTreeDispatchEvent) (GdomeNode *self, GdomeEvent *evt,
                                               GdomeException *exc);
  GdomeBoolean        (*canAppend)            (GdomeNode *self,
                                               GdomeNode *newChild,
                                               GdomeException *exc);
};

/* structure used for the Node events */
typedef struct _Gdome_xml_ListenerList Gdome_xml_ListenerList;
struct _Gdome_xml_ListenerList {
	Gdome_xml_ListenerList *next;
	guint32 type;
	GdomeEventListener *listener;
  GdomeBoolean useCapture;
};


typedef struct _Gdome_xml_Node Gdome_xml_Node;
struct _Gdome_xml_Node {
	GdomeNode super;
	const GdomeNodeVtab *vtab;
	int refcnt;
  xmlNode *n;
  GdomeAccessType accessType;
	Gdome_xml_ListenerList *ll;
};

xmlNode           * gdome_xml_n_get_xmlNode      (GdomeNode *node);
GdomeNode         * gdome_xml_n_mkref            (xmlNode *n);
void                gdome_xml_n_ref              (GdomeNode *self,
																									GdomeException *exc);
void                gdome_xml_n_unref            (GdomeNode *self, 
																									GdomeException *exc);
gpointer            gdome_xml_n_query_interface  (GdomeNode *self,
																									const char *interface, 
																									GdomeException *exc);
GdomeDOMString    * gdome_xml_n_nodeName         (GdomeNode *self, 
																									GdomeException *exc);
GdomeDOMString    * gdome_xml_n_nodeValue        (GdomeNode *self, 
																									GdomeException *exc);
void                gdome_xml_n_set_nodeValue    (GdomeNode *self, 
																									GdomeDOMString *nodeValue, 
																									GdomeException *exc);
unsigned short      gdome_xml_n_nodeType         (GdomeNode *self, 
																									GdomeException *exc);
GdomeNode         * gdome_xml_n_parentNode       (GdomeNode *self, 
																									GdomeException *exc);
GdomeNodeList     * gdome_xml_n_childNodes       (GdomeNode *self, 
																									GdomeException *exc);
GdomeNode         * gdome_xml_n_firstChild       (GdomeNode *self, 
																									GdomeException *exc);
GdomeNode         * gdome_xml_n_lastChild        (GdomeNode *self, 
																									GdomeException *exc);
GdomeNode         * gdome_xml_n_previousSibling  (GdomeNode *self, 
																									GdomeException *exc);
GdomeNode         * gdome_xml_n_nextSibling      (GdomeNode *self, 
																									GdomeException *exc);
GdomeNamedNodeMap * gdome_xml_n_attributes       (GdomeNode *self, 
																									GdomeException *exc);
GdomeDocument     * gdome_xml_n_ownerDocument    (GdomeNode *self, 
																									GdomeException *exc);
GdomeNode         * gdome_xml_n_insertBefore     (GdomeNode *self, 
																									GdomeNode *newChild, 
																									GdomeNode *refChild, 
																									GdomeException *exc);
GdomeNode         * gdome_xml_n_replaceChild     (GdomeNode *self, 
																									GdomeNode *newChild, 
																									GdomeNode *oldChild, 
																									GdomeException *exc);
GdomeNode         * gdome_xml_n_removeChild      (GdomeNode *self, 
																									GdomeNode *oldChild, 
																									GdomeException *exc);
GdomeNode         * gdome_xml_n_appendChild      (GdomeNode *self, 
																									GdomeNode *newChild, 
																									GdomeException *exc);
GdomeBoolean        gdome_xml_n_hasChildNodes    (GdomeNode *self, 
																									GdomeException *exc);
GdomeNode         * gdome_xml_n_cloneNode        (GdomeNode *self, 
																									GdomeBoolean deep, 
																									GdomeException *exc);
void                gdome_xml_n_normalize        (GdomeNode *self, 
																									GdomeException *exc);
GdomeBoolean        gdome_xml_n_isSupported      (GdomeNode *self,
																									GdomeDOMString *feature, 
																									GdomeDOMString *version, 
																									GdomeException *exc);
GdomeDOMString    * gdome_xml_n_namespaceURI     (GdomeNode *self, 
																									GdomeException *exc);
GdomeDOMString    * gdome_xml_n_prefix           (GdomeNode *self, 
																									GdomeException *exc);
void                gdome_xml_n_set_prefix       (GdomeNode *self, 
																									GdomeDOMString *prefix, 
																									GdomeException *exc);
GdomeDOMString    * gdome_xml_n_localName        (GdomeNode *self, 
																									GdomeException *exc);
GdomeBoolean        gdome_xml_n_hasAttributes    (GdomeNode *self,
																									GdomeException *exc);
void                gdome_xml_n_addEventListener (GdomeNode *self, 
																									GdomeDOMString *type, 
																									GdomeEventListener *listener, 
																									GdomeBoolean useCapture, 
																									GdomeException *exc);
void                gdome_xml_n_removeEventListener (GdomeNode *self, 
																										 GdomeDOMString *type, 
																										 GdomeEventListener *listener, 
																										 GdomeBoolean useCapture, 
																										 GdomeException *exc);
GdomeBoolean        gdome_xml_n_dispatchEvent    (GdomeNode *self, 
																									GdomeEvent *evt, 
																									GdomeException *exc);
GdomeNode         * gdome_xml_n_appendChild      (GdomeNode *self, 
																									GdomeNode *newChild, 
																									GdomeException *exc);
void                gdome_xml_n_subTreeDispatchEvent (GdomeNode *self,
                                                      GdomeEvent *evt,
                                                      GdomeException *exc);
GdomeBoolean        gdome_xml_n_canAppend        (GdomeNode *self,
                                                  GdomeNode *newChild,
                                                  GdomeException *exc);
GdomeBoolean        gdome_xml_n_eventEnabledByCode (GdomeNode *self,
																										guint32 code);

extern const GdomeNodeVtab gdome_xml_n_vtab;

#endif /* GDOME_NODE_FILE */
