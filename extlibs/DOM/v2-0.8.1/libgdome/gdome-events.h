/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gdome-events.h
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

#ifndef GDOME_EVENTS_H
#define GDOME_EVENTS_H

#include <gdome.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef enum {
	GDOME_UNSPECIFIED_EVENT_TYPE_ERR = 0
} GdomeEventExceptionCode;

typedef struct _GdomeEventVtab GdomeEventVtab;
typedef struct _GdomeEventListenerVtab GdomeEventListenerVtab;
typedef struct _GdomeMutationEventVtab GdomeMutationEventVtab;

typedef enum {
	GDOME_BASE_EVENT = 1,
	GDOME_MUTATION_EVENT = 2,
} GdomeEventType;

typedef enum {
	GDOME_CAPTURING_PHASE = 1,
	GDOME_AT_TARGET = 2,
	GDOME_BUBBLING_PHASE = 3
} GdomePhaseType;

typedef enum {
	GDOME_ADDITION = 1,
	GDOME_MODIFICATION = 2,
	GDOME_REMOVAL = 3
} GdomeAttrChangeType;

struct _GdomeEvent {
	gpointer user_data;
};

struct _GdomeEventListener {
	gpointer user_data;
};

struct _GdomeMutationEvent {
	gpointer user_data;
};

GdomeEvent *gdome_evnt_mkref (void);
void gdome_evnt_ref (GdomeEvent *self, GdomeException *exc);
void gdome_evnt_unref (GdomeEvent *self, GdomeException *exc);
gpointer gdome_evnt_query_interface (GdomeEvent *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_evnt_type (GdomeEvent *self, GdomeException *exc);
GdomeEventTarget *gdome_evnt_target (GdomeEvent *self, GdomeException *exc);
GdomeEventTarget *gdome_evnt_currentTarget (GdomeEvent *self, GdomeException *exc);
unsigned short gdome_evnt_eventPhase (GdomeEvent *self, GdomeException *exc);
GdomeBoolean gdome_evnt_bubbles (GdomeEvent *self, GdomeException *exc);
GdomeBoolean gdome_evnt_cancelable (GdomeEvent *self, GdomeException *exc);
GdomeDOMTimeStamp gdome_evnt_timeStamp (GdomeEvent *self, GdomeException *exc);
void gdome_evnt_stopPropagation (GdomeEvent *self, GdomeException *exc);
void gdome_evnt_preventDefault (GdomeEvent *self, GdomeException *exc);
void gdome_evnt_initEvent (GdomeEvent *self, GdomeDOMString *eventTypeArg, GdomeBoolean canBubbleArg, GdomeBoolean cancelableArg, GdomeException *exc);



GdomeEventListener *gdome_evntl_aux_mkref (void (*callback) (GdomeEventListener *self,
                                                             GdomeEvent *event,
                                                             GdomeException *exc),
                                           gpointer priv,
					   void (priv_callback) (GdomeEventListener* self));
GdomeEventListener *gdome_evntl_mkref (void (*callback) (GdomeEventListener *self,
                                                         GdomeEvent *event,
                                                         GdomeException *exc),
                                       gpointer priv);
void gdome_evntl_ref (GdomeEventListener *self, GdomeException *exc);
void gdome_evntl_unref (GdomeEventListener *self, GdomeException *exc);
gpointer gdome_evntl_query_interface (GdomeEventListener *self, const char *interface, GdomeException *exc);
void gdome_evntl_handleEvent (GdomeEventListener *self, GdomeEvent *evt, GdomeException *exc);
gpointer gdome_evntl_get_priv (GdomeEventListener *self);

GdomeMutationEvent *gdome_mevnt_mkref (void);
GdomeNode *gdome_mevnt_relatedNode (GdomeMutationEvent *self, GdomeException *exc);
GdomeDOMString *gdome_mevnt_prevValue (GdomeMutationEvent *self, GdomeException *exc);
GdomeDOMString *gdome_mevnt_newValue (GdomeMutationEvent *self, GdomeException *exc);
GdomeDOMString *gdome_mevnt_attrName (GdomeMutationEvent *self, GdomeException *exc);
gushort gdome_mevnt_attrChange (GdomeMutationEvent *self, GdomeException *exc);
void gdome_mevnt_initMutationEvent (GdomeMutationEvent *self, GdomeDOMString *typeArg, GdomeBoolean canBubbleArg, GdomeBoolean cancelableArg, GdomeNode *relatedNodeArg, GdomeDOMString *prevValueArg, GdomeDOMString *newValueArg, GdomeDOMString *attrNameArg, gushort attrChangeArg, GdomeException *exc);
void gdome_mevnt_ref (GdomeMutationEvent *self, GdomeException *exc);
void gdome_mevnt_unref (GdomeMutationEvent *self, GdomeException *exc);
gpointer gdome_mevnt_query_interface (GdomeMutationEvent *self, const char *interface, GdomeException *exc);
GdomeDOMString *gdome_mevnt_type (GdomeMutationEvent *self, GdomeException *exc);
GdomeNode *gdome_mevnt_target (GdomeMutationEvent *self, GdomeException *exc);
GdomeNode *gdome_mevnt_currentTarget (GdomeMutationEvent *self, GdomeException *exc);
unsigned short gdome_mevnt_eventPhase (GdomeMutationEvent *self, GdomeException *exc);
GdomeBoolean gdome_mevnt_bubbles (GdomeMutationEvent *self, GdomeException *exc);
GdomeBoolean gdome_mevnt_cancelable (GdomeMutationEvent *self, GdomeException *exc);
GdomeDOMTimeStamp gdome_mevnt_timeStamp (GdomeMutationEvent *self, GdomeException *exc);
void gdome_mevnt_stopPropagation (GdomeMutationEvent *self, GdomeException *exc);
void gdome_mevnt_preventDefault (GdomeMutationEvent *self, GdomeException *exc);
void gdome_mevnt_initEvent (GdomeMutationEvent *self, GdomeDOMString *eventTypeArg, GdomeBoolean canBubbleArg, GdomeBoolean cancelableArg, GdomeException *exc);

/* Casting Macro */
#include <gdome-util.h>
/**
 * GDOME_EVNT:
 * @evt:  Event Object ref
 *
 * Cast a ganeral GdomeEvent reference to a GdomeEvent one.
 */

#define GDOME_EVNT(evt) gdome_cast_evnt((GdomeEvent *)evt)
/**
 * GDOME_MEVNT:
 * @evt:  Event Object ref
 *
 * Cast a ganeral GdomeEvent reference to a GdomeMutationEvent one.
 */

#define GDOME_MEVNT(evt) gdome_cast_mevnt((GdomeEvent *)evt)

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* GDOME_EVENTS_H */
