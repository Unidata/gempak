/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-evt-eventl.h
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
/* ---------------------------  EventListener  --------------------------- */
/* ----------------------------------------------------------------------- */

#ifndef GDOME_FILE_EVENTL
#define GDOME_FILE_EVENTL

#include "gdome-events.h"

struct _GdomeEventListenerVtab {
  void (*ref) (GdomeEventListener *self, GdomeException *exc);
  void (*unref) (GdomeEventListener *self, GdomeException *exc);
  gpointer (*query_interface) (GdomeEventListener *self, const char *interface, GdomeException *exc);
  void (*handleEvent) (GdomeEventListener *self, GdomeEvent *evt, GdomeException *exc);
};

typedef struct _Gdome_evt_EventListener Gdome_evt_EventListener;

struct _Gdome_evt_EventListener {
  GdomeEventListener super;
  const GdomeEventListenerVtab *vtab;
  int refcnt;
	void (*callback) (GdomeEventListener *self, 
                    GdomeEvent *event,
                    GdomeException *exc);
	gpointer priv;
	void (*priv_callback) (GdomeEventListener* self);
};

GdomeEventListener *
gdome_evt_evntl_aux_mkref (void (*callback) (GdomeEventListener *self,
                                             GdomeEvent *event,
                                             GdomeException *exc),
                           gpointer priv,
													 void (*priv_callback) (GdomeEventListener* self));
GdomeEventListener *
gdome_evt_evntl_mkref (void (*callback) (GdomeEventListener *self,
                                         GdomeEvent *event,
                                         GdomeException *exc),
                       gpointer priv);

void      gdome_evt_evntl_ref               (GdomeEventListener *self,
																						 GdomeException *exc);
void      gdome_evt_evntl_unref             (GdomeEventListener *self,
																						 GdomeException *exc);
gpointer  gdome_evt_evntl_query_interface   (GdomeEventListener *self,
																						 const char *interface, 
																						 GdomeException *exc);
void      gdome_evt_evntl_handleEvent       (GdomeEventListener *self,
																						 GdomeEvent *evt,
																						 GdomeException *exc);
gpointer  gdome_evt_evntl_get_priv          (GdomeEventListener *self);


extern const GdomeEventListenerVtab gdome_evt_evntl_vtab;

#endif /* GDOME_FILE_EVENTL */

