/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-eventl.c
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
#include <stdlib.h>
#include <string.h>
#include "gdome.h"
#include "gdome-events.h"
#include "gdome-refdebug.h"
#include "gdome-evt-util.h"
#include "gdome-evt-eventl.h"
#include "gdome-evt-event.h"


const GdomeEventListenerVtab gdome_evt_evntl_vtab =
{
  gdome_evt_evntl_ref,
  gdome_evt_evntl_unref,
  gdome_evt_evntl_query_interface,
  gdome_evt_evntl_handleEvent
};

/**
 * gdome_evt_evntl_aux_mkref:
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
gdome_evt_evntl_aux_mkref (void (*callback) (GdomeEventListener *self,
																				 GdomeEvent *event, 
																				 GdomeException *exc), 
											 gpointer priv,
											 void (*priv_callback)(GdomeEventListener* self))
{
  Gdome_evt_EventListener *result = NULL;

  result = g_new (Gdome_evt_EventListener, 1);
#ifdef DEBUG_REFCNT
  gdome_refdbg_addRef ((void *)result, GDOME_REFDBG_EVENTLISTENER);
#endif
  result->refcnt = 1;
  result->vtab = (GdomeEventListenerVtab *) &gdome_evt_evntl_vtab;
  result->super.user_data = NULL;

  result->callback = callback;
  result->priv = priv;
	result->priv_callback = priv_callback;

  return (GdomeEventListener *)result;
}


/**
 * gdome_evt_evntl_mkref:
 * @callback:  the callback function that is to be called when
 *             gdome_evntl_handleEvent() is called.
 * @priv:  private data
 *
 * Returns: a new allocated #GdomeEventListener.
 */
GdomeEventListener *
gdome_evt_evntl_mkref (void (*callback) (GdomeEventListener *self,
																				 GdomeEvent *event, 
																				 GdomeException *exc), 
											 gpointer priv)
{
  return gdome_evt_evntl_aux_mkref(callback, priv, NULL);
}


/* 
	 an extension to the DOM spec. You can attach abitrary data
	 to the listener and get it back through this function
	 when the listener is called.
	 This is used specifically by Gill.
	 It is baaaad. I need to find another way to do this fast.
*/
/**
 * gdome_evt_evntl_get_priv:
 * @self:  EventListener Object Ref
 *
 * DEPRECATED
 * Returns: the priv data field associated with this object
 */
gpointer
gdome_evt_evntl_get_priv (GdomeEventListener *self)
{
  Gdome_evt_EventListener *priv = (Gdome_evt_EventListener *) self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EVNTL (priv), NULL);

  return priv->priv;
}

/**
 * gdome_evt_evntl_ref:
 * @self:  EventListener Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified EventListener.
 */
void
gdome_evt_evntl_ref (GdomeEventListener *self, GdomeException *exc)
{
  Gdome_evt_EventListener *priv = (Gdome_evt_EventListener *)self;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EVNTL (priv));
  g_return_if_fail (exc != NULL);

  priv->refcnt++;
}

/**
 * gdome_evt_evntl_unref:
 * @self:  EventListener Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified EventListener. Free the
 * EventListener structure if the EventListener will have zero reference.
 */
void
gdome_evt_evntl_unref (GdomeEventListener *self, GdomeException *exc)
{
  Gdome_evt_EventListener *priv = (Gdome_evt_EventListener *)self;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EVNTL (priv));
  g_return_if_fail (exc != NULL);

  if (--priv->refcnt == 0) {
		if (priv->priv_callback != NULL) priv->priv_callback(self);
#ifdef DEBUG_REFCNT
    gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_EVENTLISTENER);
#endif
		g_free (self);
  }

}

/**
 * gdome_evt_evntl_query_interface:
 * @self:  EventListener Object ref
 * @interface:  name of the Interface needed.
 * @exc:  Exception Object ref
 *
 * Returns: a reference to the object that implements the interface needed or
 * %NULL if this object doesn't implement the interface specified.
 */
gpointer
gdome_evt_evntl_query_interface (GdomeEventListener *self, const char *interface, GdomeException *exc)
{
  Gdome_evt_EventListener *priv = (Gdome_evt_EventListener *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_EVNTL (priv), NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "EventListener")) {
		priv->refcnt++;

		return self;
	}
  else
    return NULL;
}

/**
 * gdome_evt_evntl_handleEvent:
 * @self:  EventListener Object ref
 * @evt:  the Event Object ref that contains contextual information about the
 *        event.
 * @exc:  Exception Object ref
 *
 * This method is called whenever an event occurs of the type for which the
 * EventListener interface was registered.
 */
void
gdome_evt_evntl_handleEvent (GdomeEventListener *self, GdomeEvent *evt, GdomeException *exc)
{
  Gdome_evt_EventListener *priv = (Gdome_evt_EventListener *) self;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_EVNTL (priv));
  g_return_if_fail (evt != NULL);
  g_return_if_fail (GDOME_XML_IS_EVNT (evt));
  g_return_if_fail (exc != NULL);

  priv->callback (self, evt, exc);
}
