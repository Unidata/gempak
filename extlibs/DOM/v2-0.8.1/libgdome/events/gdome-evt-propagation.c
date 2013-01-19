/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-evt-propagation.c
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

/********************************************************************
 **  Helper functions for the Implementation of Event Propagation  **
 ********************************************************************/

#include <glib.h>
#include <string.h>
#include <libxml/tree.h>
#include "gdome.h"
#include "gdome-events.h"
#include "gdome-xml-node.h"
#include "gdome-evt-event.h"
#include "gdome-evt-eventl.h"
#include "gdome-evt-propagation.h"

typedef struct _Gdome_evt_PropagationEl Gdome_evt_PropagationEl;
struct _Gdome_evt_PropagationEl {
	GdomeEventListener *listener;
	GdomeNode *target;
	Gdome_evt_PropagationEl *next;
};

static void
add_timestamp (GdomeEvent *event)
{
	Gdome_evt_Event *priv = (Gdome_evt_Event *)event;
  GTimeVal tv;

	g_get_current_time (&tv);

	if (priv->timestamp == 0)
		priv->timestamp = (tv.tv_sec * (long long)1000) + tv.tv_usec / 1000;
}

void
gdome_evt_invokeListeners (Gdome_evt_PropagationEl *list,
													 GdomeEvent *event)
{
	Gdome_evt_PropagationEl *el;
	Gdome_evt_Event *ev_priv = (Gdome_evt_Event *)event;
  GdomeException exc = 0;

	el = list;
	while (el) {
		if (!ev_priv->propagation_stopped) {
			ev_priv->currentTarget = ((Gdome_xml_Node *)el->target)->n;
			gdome_evntl_handleEvent (el->listener, event, &exc);
		}

		gdome_n_unref (el->target, &exc);
		gdome_evntl_unref (el->listener, &exc);

    el = el->next;
	}
}

void
gdome_evt_fireEvent (GdomeEvent *event)
{
	GMemChunk *mem_chunk;
	Gdome_evt_PropagationEl *el;
	Gdome_evt_PropagationEl *captureList = NULL;
  Gdome_evt_PropagationEl *attargetList = NULL;
	Gdome_evt_PropagationEl *bubbleList = NULL;
	Gdome_evt_PropagationEl *bubbleList_lastEl = NULL;

	Gdome_evt_Event *ev_priv = (Gdome_evt_Event *)event;
	Gdome_xml_Node *priv = (Gdome_xml_Node *)ev_priv->target;
	Gdome_xml_ListenerList *ll;
	GdomeException exc = 0;

	xmlNode *p;

	mem_chunk = g_mem_chunk_create (Gdome_evt_PropagationEl, 128, G_ALLOC_ONLY);

	add_timestamp (event);

	/* Builds capture and bubble propagation lists */
	p = priv->n->parent;
	while (p != NULL) {

    /* if this node has at least one listener */
		if (p->_private != NULL && ((Gdome_xml_Node *)p->_private)->ll != NULL) {
			Gdome_xml_Node *node = p->_private;

      /* scans listeners list */
			ll = node->ll;
      while (ll) {
        /* if the current listener is for this event */
				if (ev_priv->type == ll->type) {
					/* builds a new propagation el */
					el = g_chunk_new (Gdome_evt_PropagationEl, mem_chunk);
          gdome_evntl_ref (ll->listener, &exc);
					el->listener = ll->listener;
					gdome_n_ref ((GdomeNode *)node, &exc);
					el->target = (GdomeNode *)node;
          el->next = NULL;

          /* prepends it in the capture list */
					if (ll->useCapture) {
						if (captureList != NULL) {
							el->next = captureList;
						}
						captureList = el;
					}
          /* appends it in the bubble list if the event makes bubbles */
					else if (ev_priv->bubbles) {
						if (bubbleList == NULL) {
              bubbleList = el;
						} else {
              g_assert (bubbleList_lastEl != NULL);
							bubbleList_lastEl->next = el;
						}
            bubbleList_lastEl = el;
					}
				}
        ll = ll->next;
			}
		}
    p = p->parent;
	}

	/* Builds attarget propagation list */
	ll = priv->ll;
	while (ll) {
		/* if the current listener is for this event */
		if (!ll->useCapture && ev_priv->type == ll->type) {
			/* builds a new propagation el */
			el = g_chunk_new (Gdome_evt_PropagationEl, mem_chunk);
			gdome_evntl_ref (ll->listener, &exc);
			el->listener = ll->listener;
			gdome_n_ref ((GdomeNode *)priv, &exc);
			el->target = (GdomeNode *)priv;
			el->next = NULL;

			/* prepends it in the attarget list */
			if (attargetList != NULL) {
				el->next = attargetList;
			}
			attargetList = el;
		}
    ll = ll->next;
	}

  /* Flow control flags init */
	ev_priv->default_prevented = FALSE;
	ev_priv->propagation_stopped = FALSE;

	/* CAPTURING PHASE */
	if (captureList != NULL) {
		ev_priv->eventPhase = GDOME_CAPTURING_PHASE;
		gdome_evt_invokeListeners (captureList, event);
	}

	/* AT_TARGET */
	if (attargetList != NULL) {
		ev_priv->eventPhase = GDOME_AT_TARGET;
		gdome_evt_invokeListeners (attargetList, event);
	}

	/* BUBBLING PHASE */
	if (bubbleList != NULL) {
		ev_priv->eventPhase = GDOME_BUBBLING_PHASE;
		gdome_evt_invokeListeners (bubbleList, event);
	}

  g_mem_chunk_destroy (mem_chunk);
}
