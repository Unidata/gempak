/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-mevent.c
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
#include "gdome-xml-node.h"
#include "gdome-xml-str.h"
#include "gdome-evt-event.h"
#include "gdome-evt-eventl.h"
#include "gdome-evt-mevent.h"

const GdomeMutationEventVtab gdome_evt_mevnt_vtab = 
{
  {
    gdome_evt_evnt_ref,
    gdome_evt_mevnt_unref,
    gdome_evt_mevnt_query_interface,
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
  },
  gdome_evt_mevnt_attrChange,
  gdome_evt_mevnt_attrName,
  gdome_evt_mevnt_newValue,
  gdome_evt_mevnt_prevValue,
  gdome_evt_mevnt_relatedNode,
  gdome_evt_mevnt_initMutationEvent
};

/**
 * gdome_evt_mevnt_mkref:
 *
 * Returns: a new allocated #GdomeMutationEvent.
 */
GdomeMutationEvent *
gdome_evt_mevnt_mkref (void) {
  Gdome_evt_MutationEvent *result = NULL;

  result = g_new (Gdome_evt_MutationEvent, 1);
#ifdef DEBUG_REFCNT
  gdome_refdbg_addRef ((void *)result, GDOME_REFDBG_EVENT);
#endif
  memset(result, 0, sizeof(Gdome_evt_MutationEvent));
  result->vtab = &gdome_evt_mevnt_vtab;
  result->etype = GDOME_MUTATION_EVENT;
  result->refcnt = 1;
  result->super.user_data = NULL;

  return (GdomeMutationEvent *)result;
}

/**
 * gdome_evt_mevnt_unref:
 * @self:  MutationEvent Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified MutationEvent. Free the
 * MutationEvent structure if the MutationEvent will have zero reference.
 */
void
gdome_evt_mevnt_unref (GdomeEvent *self, GdomeException *exc)
{
  Gdome_evt_MutationEvent *priv = (Gdome_evt_MutationEvent *)self;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EVNT (priv));
  g_return_if_fail (exc != NULL);

  if (--priv->refcnt == 0) {
    if (priv->attrName != NULL)
      gdome_xml_str_unref (priv->attrName);
    if (priv->newValue != NULL)
      gdome_xml_str_unref (priv->newValue);
    if (priv->prevValue != NULL)
			gdome_xml_str_unref (priv->prevValue);
		if (priv->relatedNode != NULL)
			gdome_xml_n_unref (priv->relatedNode, exc);
#ifdef DEBUG_REFCNT
    gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_EVENT);
#endif
    g_free (self);
  }
}

/**
 * gdome_evt_mevnt_query_interface:
 * @self:  Event Object ref
 * @interface:  name of the Interface needed.
 * @exc:  Exception Object ref
 *
 * Returns: a reference to the object that implements the interface needed or
 * %NULL if this object doesn't implement the interface specified.
 */
void *
gdome_evt_mevnt_query_interface (GdomeEvent *self, const char *interface, GdomeException *exc)
{
  Gdome_evt_Event *priv = (Gdome_evt_Event *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EVNT (priv), NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "Event") ||
      !strcmp (interface, "MutationEvent")) {
		priv->refcnt++;
		return self;
	}
  else
    return NULL;
}


/**
 * gdome_evt_mevnt_attrChange:
 * @self:  MutationEvent Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the type of change which triggered the DOMAttrModified event.
 * The values can be %MODIFICATION, %ADDITION, or %REMOVAL. If an error
 * occur, %0 is returned.
 */
gushort
gdome_evt_mevnt_attrChange (GdomeMutationEvent *self, GdomeException *exc)
{
	Gdome_evt_MutationEvent *priv = (Gdome_evt_MutationEvent *)self;

  g_return_val_if_fail (self != NULL, 0);
	g_return_val_if_fail (exc != NULL, 0);

  return priv->attrChange;
}

/**
 * gdome_evt_mevnt_attrName:
 * @self:  MutationEvent Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the name of the changed Attr node in a DOMAttrModified event.
 */
GdomeDOMString *
gdome_evt_mevnt_attrName (GdomeMutationEvent *self, GdomeException *exc)
{
  Gdome_evt_MutationEvent *priv = (Gdome_evt_MutationEvent *)self;

  g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);
	if (priv->attrName != NULL) gdome_str_ref(priv->attrName);

  return priv->attrName;
}

/**
 * gdome_evt_mevnt_newValue:
 * @self:  MutationEvent Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the new value of the Attr node in DOMAttrModified events,
 * and of the CharacterData node in DOMCharDataModified events.
 */
GdomeDOMString *
gdome_evt_mevnt_newValue (GdomeMutationEvent *self, GdomeException *exc)
{
  Gdome_evt_MutationEvent *priv = (Gdome_evt_MutationEvent *)self;

  g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);
	if (priv->newValue != NULL) gdome_str_ref(priv->newValue);

  return priv->newValue;
}

/**
 * gdome_evt_mevnt_prevValue:
 * @self:  MutationEvent Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the previous value of the Attr node in DOMAttrModified events,
 * and of the CharacterData node in DOMCharDataModified events.
 */
GdomeDOMString *
gdome_evt_mevnt_prevValue (GdomeMutationEvent *self, GdomeException *exc)
{
  Gdome_evt_MutationEvent *priv = (Gdome_evt_MutationEvent *)self;

  g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);
	if (priv->prevValue != NULL) gdome_str_ref(priv->prevValue);

  return priv->prevValue;
}

/**
 * gdome_evt_mevnt_relatedNode:
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
gdome_evt_mevnt_relatedNode (GdomeMutationEvent *self, GdomeException *exc)
{
  Gdome_evt_MutationEvent *priv = (Gdome_evt_MutationEvent *)self;

  g_return_val_if_fail (self != NULL, NULL);
	g_return_val_if_fail (exc != NULL, NULL);
	if (priv->relatedNode != NULL) gdome_n_ref(priv->relatedNode, exc);

  return priv->relatedNode;
}

/**
 * gdome_evt_mevnt_initMutationEventByCode:
 *
 * see documentation for gdome_evt_mevnt_initMutationEvent
 */
void
gdome_evt_mevnt_initMutationEventByCode (GdomeMutationEvent *self,
																				 guint32 typeArg,
																				 GdomeBoolean canBubbleArg,
																				 GdomeBoolean cancelableArg,
																				 GdomeNode *relatedNodeArg,
																				 GdomeDOMString *prevValueArg,
																				 GdomeDOMString *newValueArg,
																				 GdomeDOMString *attrNameArg,
																				 gushort attrChangeArg,
																				 GdomeException *exc)
{
	Gdome_evt_MutationEvent *priv = (Gdome_evt_MutationEvent *)self;

	g_return_if_fail (self != NULL);
  g_return_if_fail (exc != NULL);

	gdome_evt_evnt_initEventByCode ((GdomeEvent *)self, typeArg, canBubbleArg, cancelableArg, exc);
  priv->attrChange = attrChangeArg;
  if (attrNameArg != NULL) {
    gdome_xml_str_ref (attrNameArg);
    priv->attrName = attrNameArg;
  }
  if (newValueArg != NULL) {
    gdome_xml_str_ref (newValueArg);
    priv->newValue = newValueArg;
  }
  if (prevValueArg != NULL) {
    gdome_xml_str_ref (prevValueArg);
    priv->prevValue = prevValueArg;
  }
  if (relatedNodeArg != NULL) {
    gdome_xml_n_ref (relatedNodeArg, exc);
    priv->relatedNode = relatedNodeArg;
  }
}

/**
 * gdome_evt_mevnt_initMutationEvent:
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
gdome_evt_mevnt_initMutationEvent (GdomeMutationEvent *self,
                                   GdomeDOMString *typeArg,
                                   GdomeBoolean canBubbleArg,
                                   GdomeBoolean cancelableArg,
                                   GdomeNode *relatedNodeArg,
                                   GdomeDOMString *prevValueArg,
                                   GdomeDOMString *newValueArg,
                                   GdomeDOMString *attrNameArg,
                                   gushort attrChangeArg,
                                   GdomeException *exc)
{
	g_return_if_fail (typeArg != NULL);
	gdome_evt_mevnt_initMutationEventByCode (self,
																					 gdome_evt_evnt_codeOfName (typeArg->str),
																					 canBubbleArg,
																					 cancelableArg,
																					 relatedNodeArg,
																					 prevValueArg,
																					 newValueArg,
																					 attrNameArg,
																					 attrChangeArg,
																					 exc);
}
