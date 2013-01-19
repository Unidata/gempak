/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-refdebug.c
 *
 * CopyRight (C) 2001 Paolo Casarini <paolo@casarini.org>
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

#include <glib.h>
#include <libxml/parser.h>
#include <libxml/hash.h>
#include "gdome.h"
#include "gdome-events.h"
#include "gdome-private-list.h"
#include "gdome-xml-node.h"
#include "gdome-xml-nodel.h"
#include "gdome-xml-nnodem.h"
#include "gdome-evt-event.h"
#include "gdome-evt-eventl.h"
#include "gdome-refdebug.h"

#define NUMOFLISTS 6
GdomePrivateList *refDebug[NUMOFLISTS];

void
gdome_refdbg_addRef (void *ref, GdomeRefType type)
{
  refDebug[type] = gdome_private_list_prepend (refDebug[type], ref);
}

gint
gdome_refdbg_numRef (void *ref, GdomeRefType type)
{
  GdomePrivateList *l;
  void * res = NULL;

  for (l = refDebug[type]; l != NULL && res == NULL; l = l->next)
    if (l->data == ref)
      res = l->data;

  if (res)
    switch (type) {
    case GDOME_REFDBG_STR: return ((GdomeDOMString *)res)->refcnt;
    case GDOME_REFDBG_NODE: return ((Gdome_xml_Node *)res)->refcnt;
    case GDOME_REFDBG_NODELIST: return ((Gdome_xml_NodeList *)res)->refcnt;
    case GDOME_REFDBG_NAMEDNODEMAP: return ((Gdome_xml_NamedNodeMap *)res)->refcnt;
    case GDOME_REFDBG_EVENT: return ((Gdome_evt_Event *)res)->refcnt;
    case GDOME_REFDBG_EVENTLISTENER: return ((Gdome_evt_EventListener *)res)->refcnt;
    default: g_warning ("RefDebug, numRef: BAD TYPE\n");
    }

  return -1;
}

void gdome_refdbg_delRef (void *ref, GdomeRefType type)
{
  refDebug[type] = gdome_private_list_remove(refDebug[type], ref);
}
