/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-evt-event.h
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


/* --------------------------------------------------------------- */
/* ---------------------------  Event  --------------------------- */
/* --------------------------------------------------------------- */


#ifndef GDOME_EVENT_FILE
#define GDOME_EVENT_FILE

#include <libxml/tree.h>

struct _GdomeEventVtab {
  void (*ref) (GdomeEvent *self, GdomeException *exc);
  void (*unref) (GdomeEvent *self, GdomeException *exc);
  gpointer (*query_interface) (GdomeEvent *self, const char *interface, GdomeException *exc);
  GdomeBoolean (*bubbles) (GdomeEvent *self, GdomeException *exc);
  GdomeBoolean (*cancelable) (GdomeEvent *self, GdomeException *exc);
  GdomeEventTarget *(*currentTarget) (GdomeEvent *self, GdomeException *exc);
  unsigned short (*eventPhase) (GdomeEvent *self, GdomeException *exc);
  GdomeEventTarget *(*target) (GdomeEvent *self, GdomeException *exc);
  GdomeDOMTimeStamp (*timeStamp) (GdomeEvent *self, GdomeException *exc);
  GdomeDOMString *(*type) (GdomeEvent *self, GdomeException *exc);
  void (*initEvent) (GdomeEvent *self, GdomeDOMString *eventTypeArg, GdomeBoolean canBubbleArg, GdomeBoolean cancelableArg, GdomeException *exc);
  void (*preventDefault) (GdomeEvent *self, GdomeException *exc);
  void (*stopPropagation) (GdomeEvent *self, GdomeException *exc);
};

typedef struct _Gdome_evt_Event Gdome_evt_Event;

struct _Gdome_evt_Event {
  GdomeEvent super;
  const GdomeEventVtab *vtab;
  GdomeEventType etype;
  int refcnt;

  GdomeBoolean bubbles;
	GdomeBoolean cancelable;	
	xmlNode *currentTarget;
	unsigned short eventPhase;
	GdomeNode *target;
	GdomeDOMTimeStamp timestamp;
	guint32 type;
	GdomeBoolean propagation_stopped;
	GdomeBoolean default_prevented;
};

GdomeEvent       * gdome_evt_evnt_mkref            (void);
void               gdome_evt_evnt_ref              (GdomeEvent *self,
                                                    GdomeException *exc);
void               gdome_evt_evnt_unref            (GdomeEvent *self,
                                                    GdomeException *exc);
gpointer           gdome_evt_evnt_query_interface  (GdomeEvent *self,
                                                    const char *interface,
                                                    GdomeException *exc);
GdomeDOMString   * gdome_evt_evnt_type             (GdomeEvent *self,
                                                    GdomeException *exc);
GdomeEventTarget * gdome_evt_evnt_target           (GdomeEvent *self,
                                                    GdomeException *exc);
GdomeEventTarget * gdome_evt_evnt_currentTarget    (GdomeEvent *self,
                                                    GdomeException *exc);
unsigned short     gdome_evt_evnt_eventPhase       (GdomeEvent *self,
                                                    GdomeException *exc);
GdomeBoolean       gdome_evt_evnt_bubbles          (GdomeEvent *self,
                                                    GdomeException *exc);
GdomeBoolean       gdome_evt_evnt_cancelable       (GdomeEvent *self,
                                                    GdomeException *exc);
GdomeDOMTimeStamp  gdome_evt_evnt_timeStamp        (GdomeEvent *self,
                                                    GdomeException *exc);
void               gdome_evt_evnt_stopPropagation  (GdomeEvent *self,
                                                    GdomeException *exc);
void               gdome_evt_evnt_preventDefault   (GdomeEvent *self,
                                                    GdomeException *exc);
void               gdome_evt_evnt_initEvent        (GdomeEvent *self,
                                                    GdomeDOMString *eventTypeArg,
                                                    GdomeBoolean canBubbleArg,
                                                    GdomeBoolean cancelableArg,
                                                    GdomeException *exc);

/* The following are constants corresponding to the
 * event types that Gdome2 can handle. These constants
 * are used internally only and should not be exposed to the
 * public API
 */
enum _Gdome_xml_EventCodes {
	DOM_SUBTREE_MODIFIED_EVENT_TYPE = 0x01,
	DOM_NODE_INSERTED_EVENT_TYPE = 0x02,
	DOM_NODE_REMOVED_EVENT_TYPE = 0x04,
	DOM_NODE_REMOVED_FROM_DOCUMENT_EVENT_TYPE = 0x08,
	DOM_NODE_INSERTED_INTO_DOCUMENT_EVENT_TYPE = 0x10,
	DOM_ATTR_MODIFIED_EVENT_TYPE = 0x20,
	DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE = 0x40,

	DOM_ALL_EVENT_TYPES = (-1)
};

/* Some auxiliary functions */
guint32            gdome_evt_evnt_codeOfName       (const char *name);
const char*        gdome_evt_evnt_nameOfCode       (guint32 code);
void               gdome_evt_evnt_initEventByCode  (GdomeEvent *self,
                                                    guint32 eventTypeArg,
                                                    GdomeBoolean canBubbleArg,
                                                    GdomeBoolean cancelableArg,
                                                    GdomeException *exc);


extern const GdomeEventVtab gdome_evt_evnt_vtab;

#endif /* GDOME_EVENT_FILE */
