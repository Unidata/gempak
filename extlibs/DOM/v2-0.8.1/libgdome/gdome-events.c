/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-events.c
 *
 * Copyright (C) 1999 Raph Levien <raph@acm.org>
 * Copyright (C) 2000 Mathieu Lacage <mathieu@gnu.org>
 * CopyRight (C) 2001 Paolo Casarini <paolo@casarini.org>
 *
 * This file is generated automatically.  To make changes, edit
 * test/apigen/events.xml
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

#include "gdome-events.h"
#include "gdome-evt-eventl.h"
#include "gdome-evt-event.h"
#include "gdome-evt-mevent.h"


/******************************************************************************
          GdomeEventListener interface API
 ******************************************************************************/
/**
 * gdome_evntl_aux_mkref:
 * @callback:  the callback function that is to be called when
 *             gdome_evntl_handleEvent() is called.
 * @priv:  private data
 * @priv_callback: the callback function to be called just before
 *                 the event listener is destroyed, so that the
 *                 client can do something sensible with the
 *                 priv field (like unreffing it)
 *
 * Returns: a new allocated #GdomeEventListener.
 */
GdomeEventListener *
gdome_evntl_aux_mkref (void (*callback) (GdomeEventListener *self, GdomeEvent *event, GdomeException *exc), gpointer priv, void (*priv_callback) (GdomeEventListener* self))
{
	return gdome_evt_evntl_aux_mkref (callback, priv, priv_callback);
}
/**
 * gdome_evntl_mkref:
 * @callback:  the callback function that is to be called when
 *             gdome_evntl_handleEvent() is called.
 * @priv:  private data
 *
 * Returns: a new allocated #GdomeEventListener.
 */
GdomeEventListener *
gdome_evntl_mkref (void (*callback) (GdomeEventListener *self, GdomeEvent *event, GdomeException *exc), gpointer priv)
{
	return gdome_evt_evntl_mkref (callback, priv);
}
/**
 * gdome_evntl_ref:
 * @self:  EventListener Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified EventListener.
 */
void
gdome_evntl_ref (GdomeEventListener *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_EventListener *)self)->vtab->ref (self, exc);
}
/**
 * gdome_evntl_unref:
 * @self:  EventListener Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified EventListener. Free the
 * EventListener structure if the EventListener will have zero reference.
 */
void
gdome_evntl_unref (GdomeEventListener *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_EventListener *)self)->vtab->unref (self, exc);
}
/**
 * gdome_evntl_query_interface:
 * @self:  EventListener Object ref
 * @interface:  name of the Interface needed.
 * @exc:  Exception Object ref
 *
 * Returns: a reference to the object that implements the interface needed or
 * %NULL if this object doesn't implement the interface specified.
 */
gpointer
gdome_evntl_query_interface (GdomeEventListener *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_EventListener *)self)->vtab->query_interface (self, interface, exc);
}
/**
 * gdome_evntl_handleEvent:
 * @self:  EventListener Object ref
 * @evt:  the Event Object ref that contains contextual information about the
 *        event.
 * @exc:  Exception Object ref
 *
 * This method is called whenever an event occurs of the type for which the
 * EventListener interface was registered.
 */
void
gdome_evntl_handleEvent (GdomeEventListener *self, GdomeEvent *evt, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_EventListener *)self)->vtab->handleEvent (self, evt, exc);
}
/**
 * gdome_evntl_get_priv:
 * @self:  EventListener Object Ref
 *
 * DEPRECATED
 * Returns: the priv data field associated with this object
 */
gpointer
gdome_evntl_get_priv (GdomeEventListener *self)
{
	return gdome_evt_evntl_get_priv (self);
}

/******************************************************************************
          GdomeEvent interface API
 ******************************************************************************/
/**
 * gdome_evnt_bubbles:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: If the event can bubble the value is %TRUE, else the value is
 * %FALSE.
 */
GdomeBoolean
gdome_evnt_bubbles (GdomeEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_evt_Event *)self)->vtab->bubbles (self, exc);
}
/**
 * gdome_evnt_cancelable:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: If the default action can be prevented the value is %TRUE, else
 * the value is %FALSE.
 */
GdomeBoolean
gdome_evnt_cancelable (GdomeEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_evt_Event *)self)->vtab->cancelable (self, exc);
}
/**
 * gdome_evnt_currentTarget:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the EventTarget whose EventListeners are currently being
 * processed. This is particularly useful during capturing and bubbling.
 */
GdomeEventTarget *
gdome_evnt_currentTarget (GdomeEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_Event *)self)->vtab->currentTarget (self, exc);
}
/**
 * gdome_evnt_eventPhase:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the phase of event flow is currently being evaluated.
 */
unsigned short
gdome_evnt_eventPhase (GdomeEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_evt_Event *)self)->vtab->eventPhase (self, exc);
}
/**
 * gdome_evnt_target:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the EventTarget to which the event was originally dispatched.
 */
GdomeEventTarget *
gdome_evnt_target (GdomeEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_Event *)self)->vtab->target (self, exc);
}
/**
 * gdome_evnt_timeStamp:
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
gdome_evnt_timeStamp (GdomeEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0L;
	}
	*exc = 0;
	return ((Gdome_evt_Event *)self)->vtab->timeStamp (self, exc);
}
/**
 * gdome_evnt_type:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: The name of the event (case-insensitive). The name must be an XML
 * name.
 */
GdomeDOMString *
gdome_evnt_type (GdomeEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_Event *)self)->vtab->type (self, exc);
}
/**
 * gdome_evnt_mkref:
 *
 * Returns: a new allocated #GdomeEvent.
 */
GdomeEvent *
gdome_evnt_mkref (void)
{
	return gdome_evt_evnt_mkref ();
}
/**
 * gdome_evnt_ref:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Event.
 */
void
gdome_evnt_ref (GdomeEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_Event *)self)->vtab->ref (self, exc);
}
/**
 * gdome_evnt_unref:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Event. Free the Event
 * structure if the Event will have zero reference.
 */
void
gdome_evnt_unref (GdomeEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_Event *)self)->vtab->unref (self, exc);
}
/**
 * gdome_evnt_query_interface:
 * @self:  Event Object ref
 * @interface:  name of the Interface needed.
 * @exc:  Exception Object ref
 *
 * Returns: a reference to the object that implements the interface needed or
 * %NULL if this object doesn't implement the interface specified.
 */
gpointer
gdome_evnt_query_interface (GdomeEvent *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_Event *)self)->vtab->query_interface (self, interface, exc);
}
/**
 * gdome_evnt_initEvent:
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
gdome_evnt_initEvent (GdomeEvent *self, GdomeDOMString *eventTypeArg, GdomeBoolean canBubbleArg, GdomeBoolean cancelableArg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_Event *)self)->vtab->initEvent (self, eventTypeArg, canBubbleArg, cancelableArg, exc);
}
/**
 * gdome_evnt_preventDefault:
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
gdome_evnt_preventDefault (GdomeEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_Event *)self)->vtab->preventDefault (self, exc);
}
/**
 * gdome_evnt_stopPropagation:
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
gdome_evnt_stopPropagation (GdomeEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_Event *)self)->vtab->stopPropagation (self, exc);
}

/******************************************************************************
          GdomeMutationEvent interface API
 ******************************************************************************/
/**
 * gdome_mevnt_attrChange:
 * @self:  MutationEvent Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the type of change which triggered the DOMAttrModified event.
 * The values can be %MODIFICATION, %ADDITION, or %REMOVAL. If an error
 * occur, %0 is returned.
 */
gushort
gdome_mevnt_attrChange (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->attrChange (self, exc);
}
/**
 * gdome_mevnt_attrName:
 * @self:  MutationEvent Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of the changed Attr node in a DOMAttrModified event.
 */
GdomeDOMString *
gdome_mevnt_attrName (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->attrName (self, exc);
}
/**
 * gdome_mevnt_newValue:
 * @self:  MutationEvent Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the new value of the Attr node in DOMAttrModified events,
 * and of the CharacterData node in DOMCharDataModified events.
 */
GdomeDOMString *
gdome_mevnt_newValue (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->newValue (self, exc);
}
/**
 * gdome_mevnt_prevValue:
 * @self:  MutationEvent Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the previous value of the Attr node in DOMAttrModified events,
 * and of the CharacterData node in DOMCharDataModified events.
 */
GdomeDOMString *
gdome_mevnt_prevValue (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->prevValue (self, exc);
}
/**
 * gdome_mevnt_relatedNode:
 * @self:  MutationEvent Object ref
 * @exc:  Exception Object ref
 *
 * Returns: a secondary node related to a mutation event. For example, if a
 * mutation event is dispatched to a node indicating that its parent has
 * changed, the relatedNode is the changed parent. If an event is instead
 * dispatched to a subtree indicating a node was changed within it, the
 * relatedNode is the changed node. In the case of the DOMAttrModified event
 * it indicates the Attr node which was modified, added, or removed.
 */
GdomeNode *
gdome_mevnt_relatedNode (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->relatedNode (self, exc);
}
/**
 * gdome_mevnt_mkref:
 *
 * Returns: a new allocated #GdomeMutationEvent.
 */
GdomeMutationEvent *
gdome_mevnt_mkref (void)
{
	return gdome_evt_mevnt_mkref ();
}
/**
 * gdome_mevnt_initMutationEvent:
 * @self:  MutationEvent Object ref
 * @typeArg:  Specifies the event type.
 * @canBubbleArg:  Specifies whether or not the event can bubble.
 * @cancelableArg:  Specifies whether or not the event's default action
 *                  can be prevented.
 * @relatedNodeArg:  Specifies the Event's related Node. This value may
 *                   be %NULL.
 * @prevValueArg:  Specifies the Event's prevValue attribute. This value
 *                 may be %NULL.
 * @newValueArg:  Specifies the Event's newValue attribute. This value
 *                may be %NULL.
 * @attrNameArg:  Specifies the Event's attrName attribute. This value
 *                may be null.
 * @attrChangeArg:  Specifies the Event's attrChange attribute
 * @exc:  Exception Object ref
 *
 * The initMutationEvent method is used to initialize the value of a
 * #GdomeMutationEvent created through the DocumentEvent interface. This
 * method may only be called before the MutationEvent has been dispatched
 * via the dispatchEvent method, though it may be called multiple times
 * during that phase if necessary. If called multiple times, the final
 * invocation takes precedence.
 */
void
gdome_mevnt_initMutationEvent (GdomeMutationEvent *self, GdomeDOMString *typeArg, GdomeBoolean canBubbleArg, GdomeBoolean cancelableArg, GdomeNode *relatedNodeArg, GdomeDOMString *prevValueArg, GdomeDOMString *newValueArg, GdomeDOMString *attrNameArg, gushort attrChangeArg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_MutationEvent *)self)->vtab->initMutationEvent (self, typeArg, canBubbleArg, cancelableArg, relatedNodeArg, prevValueArg, newValueArg, attrNameArg, attrChangeArg, exc);
}
/**
 * gdome_mevnt_bubbles:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: If the event can bubble the value is %TRUE, else the value is
 * %FALSE.
 */
GdomeBoolean
gdome_mevnt_bubbles (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->super.bubbles ((GdomeEvent *)self, exc);
}
/**
 * gdome_mevnt_cancelable:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: If the default action can be prevented the value is %TRUE, else
 * the value is %FALSE.
 */
GdomeBoolean
gdome_mevnt_cancelable (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return FALSE;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->super.cancelable ((GdomeEvent *)self, exc);
}
/**
 * gdome_mevnt_currentTarget:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the EventTarget whose EventListeners are currently being
 * processed. This is particularly useful during capturing and bubbling.
 */
GdomeEventTarget *
gdome_mevnt_currentTarget (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->super.currentTarget ((GdomeEvent *)self, exc);
}
/**
 * gdome_mevnt_eventPhase:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the phase of event flow is currently being evaluated.
 */
unsigned short
gdome_mevnt_eventPhase (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->super.eventPhase ((GdomeEvent *)self, exc);
}
/**
 * gdome_mevnt_target:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the EventTarget to which the event was originally dispatched.
 */
GdomeEventTarget *
gdome_mevnt_target (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->super.target ((GdomeEvent *)self, exc);
}
/**
 * gdome_mevnt_timeStamp:
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
gdome_mevnt_timeStamp (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return 0L;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->super.timeStamp ((GdomeEvent *)self, exc);
}
/**
 * gdome_mevnt_type:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Returns: The name of the event (case-insensitive). The name must be an XML
 * name.
 */
GdomeDOMString *
gdome_mevnt_type (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->super.type ((GdomeEvent *)self, exc);
}
/**
 * gdome_mevnt_ref:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified Event.
 */
void
gdome_mevnt_ref (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_MutationEvent *)self)->vtab->super.ref ((GdomeEvent *)self, exc);
}
/**
 * gdome_mevnt_unref:
 * @self:  Event Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified Event. Free the Event
 * structure if the Event will have zero reference.
 */
void
gdome_mevnt_unref (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_MutationEvent *)self)->vtab->super.unref ((GdomeEvent *)self, exc);
}
/**
 * gdome_mevnt_query_interface:
 * @self:  Event Object ref
 * @interface:  name of the Interface needed.
 * @exc:  Exception Object ref
 *
 * Returns: a reference to the object that implements the interface needed or
 * %NULL if this object doesn't implement the interface specified.
 */
gpointer
gdome_mevnt_query_interface (GdomeMutationEvent *self, const char *interface, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return NULL;
	}
	*exc = 0;
	return ((Gdome_evt_MutationEvent *)self)->vtab->super.query_interface ((GdomeEvent *)self, interface, exc);
}
/**
 * gdome_mevnt_initEvent:
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
gdome_mevnt_initEvent (GdomeMutationEvent *self, GdomeDOMString *eventTypeArg, GdomeBoolean canBubbleArg, GdomeBoolean cancelableArg, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_MutationEvent *)self)->vtab->super.initEvent ((GdomeEvent *)self, eventTypeArg, canBubbleArg, cancelableArg, exc);
}
/**
 * gdome_mevnt_preventDefault:
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
gdome_mevnt_preventDefault (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_MutationEvent *)self)->vtab->super.preventDefault ((GdomeEvent *)self, exc);
}
/**
 * gdome_mevnt_stopPropagation:
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
gdome_mevnt_stopPropagation (GdomeMutationEvent *self, GdomeException *exc)
{
	if (self == NULL) {
		*exc = GDOME_NULL_POINTER_ERR;
		return;
	}
	*exc = 0;
	((Gdome_evt_MutationEvent *)self)->vtab->super.stopPropagation ((GdomeEvent *)self, exc);
}
