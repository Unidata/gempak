/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-evt-mevent.h
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
/* ---------------------------  MutationEvent  --------------------------- */
/* ----------------------------------------------------------------------- */

#ifndef GDOME_MEVENT_FILE
#define GDOME_MEVENT_FILE

struct _GdomeMutationEventVtab {
  GdomeEventVtab super;
  gushort (*attrChange) (GdomeMutationEvent *self, GdomeException *exc);
  GdomeDOMString *(*attrName) (GdomeMutationEvent *self, GdomeException *exc);
  GdomeDOMString *(*newValue) (GdomeMutationEvent *self, GdomeException *exc);
  GdomeDOMString *(*prevValue) (GdomeMutationEvent *self, GdomeException *exc);
  GdomeNode *(*relatedNode) (GdomeMutationEvent *self, GdomeException *exc);
  void (*initMutationEvent) (GdomeMutationEvent *self, GdomeDOMString *typeArg, GdomeBoolean canBubbleArg, GdomeBoolean cancelableArg, GdomeNode *relatedNodeArg, GdomeDOMString *prevValueArg, GdomeDOMString *newValueArg, GdomeDOMString *attrNameArg, gushort attrChangeArg, GdomeException *exc);
};

typedef struct _Gdome_evt_MutationEvent Gdome_evt_MutationEvent;

struct _Gdome_evt_MutationEvent {
  GdomeMutationEvent super;
  const GdomeMutationEventVtab *vtab;
  GdomeEventType etype;
  int refcnt;

	GdomeBoolean bubbles;
	GdomeBoolean cancelable;	
	xmlNode *currentTarget;
	unsigned short eventPhase;
	GdomeNode *target;
	GdomeDOMTimeStamp timestamp;
	guint32 *type;
	GdomeBoolean propagation_stopped;
  GdomeBoolean default_prevented;

  unsigned short attrChange;
	GdomeDOMString *attrName;
	GdomeDOMString *newValue;
	GdomeDOMString *prevValue;
	GdomeNode *relatedNode;
};


GdomeMutationEvent * gdome_evt_mevnt_mkref           (void);
void                 gdome_evt_mevnt_unref           (GdomeEvent *self,
                                                      GdomeException *exc);
gpointer             gdome_evt_mevnt_query_interface (GdomeEvent *self,
                                                      const char *interface,
                                                      GdomeException *exc);
GdomeNode          * gdome_evt_mevnt_target          (GdomeEvent *self,
                                                      GdomeException *exc);
GdomeNode          * gdome_evt_mevnt_relatedNode     (GdomeMutationEvent *self,
                                                      GdomeException *exc);
GdomeDOMString     * gdome_evt_mevnt_prevValue       (GdomeMutationEvent *self,
                                                      GdomeException *exc);
GdomeDOMString     * gdome_evt_mevnt_newValue        (GdomeMutationEvent *self,
                                                      GdomeException *exc);
GdomeDOMString     * gdome_evt_mevnt_attrName        (GdomeMutationEvent *self,
                                                      GdomeException *exc);
gushort              gdome_evt_mevnt_attrChange      (GdomeMutationEvent *self,
                                                      GdomeException *exc);
void                 gdome_evt_mevnt_initMutationEvent   (GdomeMutationEvent *self,
                                                          GdomeDOMString *typeArg,
                                                          GdomeBoolean canBubbleArg,
                                                          GdomeBoolean cancelableArg,
                                                          GdomeNode *relatedNodeArg,
                                                          GdomeDOMString *prevValueArg,
                                                          GdomeDOMString *newValueArg,
                                                          GdomeDOMString *attrNameArg,
                                                          gushort attrChangeArg,
                                                          GdomeException *exc);

/* auxiliary functions */

void                 gdome_evt_mevnt_initMutationEventByCode   (GdomeMutationEvent *self,
																																guint32 typeArg,
																																GdomeBoolean canBubbleArg,
																																GdomeBoolean cancelableArg,
																																GdomeNode *relatedNodeArg,
																																GdomeDOMString *prevValueArg,
																																GdomeDOMString *newValueArg,
																																GdomeDOMString *attrNameArg,
																																gushort attrChangeArg,
																																GdomeException *exc);

extern const GdomeMutationEventVtab gdome_evt_mevnt_vtab;

#endif /* GDOME_MEVENT_FILE */
