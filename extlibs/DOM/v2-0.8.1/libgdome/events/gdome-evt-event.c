/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-event.c
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <string.h>
#include "gdome.h"
#include "gdome-events.h"
#include "gdome-refdebug.h"
#include "gdome-evt-util.h"
#include "gdome-xml-str.h"
#include "gdome-xml-node.h"
#include "gdome-evt-event.h"

static struct {
	const char* name;
	guint32 code;
} eventNameTypeMap[] = {
	{ "DOMSubtreeModified", DOM_SUBTREE_MODIFIED_EVENT_TYPE },
	{ "DOMNodeInserted", DOM_NODE_INSERTED_EVENT_TYPE },
	{ "DOMNodeInsertedIntoDocument", DOM_NODE_INSERTED_INTO_DOCUMENT_EVENT_TYPE },
	{ "DOMNodeRemoved", DOM_NODE_REMOVED_EVENT_TYPE },
	{ "DOMNodeRemovedFromDocument", DOM_NODE_REMOVED_FROM_DOCUMENT_EVENT_TYPE },
	{ "DOMAttrModified", DOM_ATTR_MODIFIED_EVENT_TYPE },
	{ "DOMCharacterDataModified", DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE },

	/* some wildcards */
	{ "", 0 },
	{ "*", DOM_ALL_EVENT_TYPES },

	{ NULL, 0 }
};

const GdomeEventVtab gdome_evt_evnt_vtab = {
  gdome_evt_evnt_ref,
  gdome_evt_evnt_unref,
  gdome_evt_evnt_query_interface,
  gdome_evt_evnt_bubbles,
  gdome_evt_evnt_cancelable,
  gdome_evt_evnt_currentTarget,
  gdome_evt_evnt_eventPhase,
  gdome_evt_evnt_target,
  gdome_evt_evnt_timeStamp,
  gdome_evt_evnt_type,
  gdome_evt_evnt_initEvent,
  gdome_evt_evnt_preventDefault,
  gdome_evt_evnt_stopPropagation,
};


/**
 * gdome_evt_evnt_mkref:
 *
 * Returns: a new allocated #GdomeEvent.
 */
GdomeEvent *
gdome_evt_evnt_mkref (void) {
  Gdome_evt_Event *result = NULL;

  result = g_new (Gdome_evt_Event, 1);
#ifdef DEBUG_REFCNT
  gdome_refdbg_addRef ((void *)result, GDOME_REFDBG_EVENT);
#endif
  memset(result, 0, sizeof(Gdome_evt_Event));
  result->vtab = &gdome_evt_evnt_vtab;
  result->etype = GDOME_BASE_EVENT;
  result->refcnt = 1;
  result->super.user_data = NULL;

  return (GdomeEvent *)result;
}

/**
 * gdome_evt_evnt_ref:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Event.
 */
void
gdome_evt_evnt_ref (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EVNT (priv));
  g_return_if_fail (exc != NULL);

  priv->refcnt++;
}

/**
 * gdome_evt_evnt_unref:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Event. Free the Event
 * structure if the Event will have zero reference.
 */
void
gdome_evt_evnt_unref (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EVNT (priv));
  g_return_if_fail (exc != NULL);

  if (--priv->refcnt == 0) {
#ifdef DEBUG_REFCNT
    gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_EVENT);
#endif
    g_free (self);
  }
}

/**
 * gdome_evt_evnt_query_interface:
 * @self:  Event Object ref
 * @interface:  name of the Interface needed.
 * @exc:  Exception Object ref
 *
 * Returns: a reference to the object that implements the interface needed or
 * %NULL if this object doesn't implement the interface specified.
 */
gpointer
gdome_evt_evnt_query_interface (GdomeEvent *self, const char *interface, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EVNT (priv), NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "Event")) {
		priv->refcnt++;
		return self;
	}
  else
    return NULL;
}

/**
 * gdome_evt_evnt_bubbles:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: If the event can bubble the value is %TRUE, else the value is
 * %FALSE.
 */
GdomeBoolean
gdome_evt_evnt_bubbles (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_val_if_fail (priv != NULL, FALSE);
  g_return_val_if_fail (GDOME_XML_IS_EVNT (priv), FALSE);
  g_return_val_if_fail (exc != NULL, FALSE);

  return priv->bubbles;
}

/**
 * gdome_evt_evnt_cancelable:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: If the default action can be prevented the value is %TRUE, else
 * the value is %FALSE.
 */
GdomeBoolean
gdome_evt_evnt_cancelable (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_val_if_fail (priv != NULL, FALSE);
  g_return_val_if_fail (GDOME_XML_IS_EVNT (priv), FALSE);
  g_return_val_if_fail (exc != NULL, FALSE);

  return priv->cancelable;
}

/**
 * gdome_evt_evnt_currentTarget:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the EventTarget whose EventListeners are currently being
 * processed. This is particularly useful during capturing and bubbling.
 */
GdomeEventTarget *
gdome_evt_evnt_currentTarget (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EVNT (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  return (GdomeEventTarget *)gdome_xml_n_mkref (priv->currentTarget);
}

/**
 * gdome_evt_evnt_eventPhase:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the phase of event flow is currently being evaluated.
 */
unsigned short
gdome_evt_evnt_eventPhase (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_val_if_fail (priv != NULL, FALSE);
  g_return_val_if_fail (GDOME_XML_IS_EVNT (priv), FALSE);
  g_return_val_if_fail (exc != NULL, FALSE);

  return priv->eventPhase;
}

/**
 * gdome_evt_evnt_target:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the EventTarget to which the event was originally dispatched.
 */
GdomeEventTarget *
gdome_evt_evnt_target (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;
  GdomeNode *result = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EVNT (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  result = priv->target;
  gdome_xml_n_ref (result, exc);
  return (GdomeEventTarget *)result;
}

/**
 * gdome_evt_evnt_timeStamp:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the time (in milliseconds relative to the epoch) at which the
 * event was created. Due to the fact that some systems may not provide this
 * information the value of timeStamp may be not available for all events.
 * When not available, a value of 0 will be returned. Examples of epoch time
 * are the time of the system start or 0:0:0 UTC 1st January 1970.
 */
GdomeDOMTimeStamp
gdome_evt_evnt_timeStamp (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

	g_return_val_if_fail (priv != NULL, 0);
  g_return_val_if_fail (GDOME_XML_IS_EVNT (priv), 0);
  g_return_val_if_fail (exc != NULL, 0);

	return priv->timestamp;
}

/**
 * gdome_evt_evnt_type:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: The name of the event (case-insensitive). The name must be an XML
 * name.
 */
GdomeDOMString *
gdome_evt_evnt_type (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EVNT (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  return gdome_xml_str_mkref_dup (gdome_evt_evnt_nameOfCode (priv->type));
}

/**
 * gdome_evt_evnt_initEventByCode
 * 
 * see the documentation for gdome_evt_evnt_initEvent
 */
void
gdome_evt_evnt_initEventByCode (GdomeEvent *self, guint32 eventTypeArg, GdomeBoolean canBubbleArg, GdomeBoolean cancelableArg, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EVNT (priv));
  g_return_if_fail (exc != NULL);

  /* todo: If you call this on an internally generated event, there
     will be memory problems. Maybe we just need a flag for whether it
     was internally generated or created through DocumentEvent. Maybe
     another flag to check the "before dispatch" phase. */
  priv->type = eventTypeArg;
  priv->bubbles = canBubbleArg;
  priv->cancelable = cancelableArg;
}

/**
 * gdome_evt_evnt_initEvent:
 * @self:  Event Object ref
 * @eventTypeArg:  Specifies the event type. This type may be any event type
 *                 currently defined in this specification or a new event type.
 *                 The string must be an XML name.
 *                 Any new event type must not begin with any upper, lower, or
 *                 mixed case version of the string "DOM".
 *                 This prefix is reserved for future DOM event sets. It is
 *                 also strongly recommended that third parties adding their
 *                 own events use their own prefix to avoid confusion and
 *                 lessen the probability of conflicts with other new events.
 * @canBubbleArg:  Specifies whether or not the event can bubble.
 * @cancelableArg:  Specifies whether or not the event's default action can be
 *                  prevented.
 * @exc:  Exception Object ref
 *
 * The initEvent method is used to initialize the value of an Event created
 * through the gdome_doc_createEvent() API. This method may only be called
 * before the Event has been dispatched via the dispatchEvent method, though
 * it may be called multiple times during that phase if necessary. If called
 * multiple times the final invocation takes precedence. If called from a
 * subclass of Event interface only the values specified in the initEvent
 * method are modified, all other attributes are left unchanged.
 */
void
gdome_evt_evnt_initEvent (GdomeEvent *self, GdomeDOMString *eventTypeArg, GdomeBoolean canBubbleArg, GdomeBoolean cancelableArg, GdomeException *exc)
{
  g_return_if_fail (eventTypeArg != NULL);
	gdome_evt_evnt_initEventByCode (self, gdome_evt_evnt_codeOfName (eventTypeArg->str), canBubbleArg, cancelableArg, exc);
}

/**
 * gdome_evt_evnt_preventDefault:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * If an event is cancelable, the preventDefault method is used to signify
 * that the event is to be canceled, meaning any default action normally
 * taken by the implementation as a result of the event will not occur. If,
 * during any stage of event flow, the preventDefault method is called the
 * event is canceled. Any default action associated with the event will not
 * occur. Calling this method for a non-cancelable event has no effect. Once
 * preventDefault has been called it will remain in effect throughout the
 * remainder of the event's propagation. This method may be used during any
 * stage of event flow.
 */
void
gdome_evt_evnt_preventDefault (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EVNT (priv));
  g_return_if_fail (exc != NULL);

  if (priv->cancelable)
    priv->default_prevented = TRUE;
}

/**
 * gdome_evt_evnt_stopPropagation:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * The stopPropagation method is used prevent further propagation of an event
 * during event flow. If this method is called by any EventListener the event
 * will cease propagating through the tree. The event will complete dispatch
 * to all listeners on the current EventTarget before event flow stops. This
 * method may be used during any stage of event flow.
 */
void
gdome_evt_evnt_stopPropagation (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EVNT (priv));
  g_return_if_fail (exc != NULL);

  priv->propagation_stopped = TRUE;
}

/**
 * gdome_evt_evnt_codeOfName:
 * @name: name of the event
 *
 * Returns the internal code corresponding to the event whose name is @name
 */
guint32
gdome_evt_evnt_codeOfName (const char *name)
{
	unsigned i;

	for (i = 0; eventNameTypeMap[i].name != NULL && strcmp(eventNameTypeMap[i].name, name); i++) ;

	if (eventNameTypeMap[i].name != NULL)
		return eventNameTypeMap[i].code;
	else
		return 0;
}

/**
 * gdome_evt_evnt_nameOfCode:
 * @code: code of the event
 *
 * Returns the symbolic name corresponding to the event whose code is @code
 */
const char*
gdome_evt_evnt_nameOfCode (unsigned int code)
{
	unsigned i;

	for (i = 0; eventNameTypeMap[i].name != NULL && eventNameTypeMap[i].code != code; i++) ;

	if (eventNameTypeMap[i].name != NULL)
		return eventNameTypeMap[i].name;
	else
		return NULL;
}
