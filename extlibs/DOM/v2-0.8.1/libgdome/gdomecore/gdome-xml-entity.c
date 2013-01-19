/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-entity.c
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

#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include "gdome.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include <libxml/entities.h>
#include "gdome-xml-util.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-xml-str.h"
#include "gdome-xml-node.h"
#include "gdome-xml-entity.h"

const GdomeEntityVtab gdome_xml_ent_vtab = {
  {
    gdome_xml_n_ref,
    gdome_xml_n_unref,
    gdome_xml_ent_query_interface,
    gdome_xml_n_nodeName,
    gdome_xml_n_nodeValue,
    gdome_xml_n_set_nodeValue,
    gdome_xml_n_nodeType,
    gdome_xml_n_parentNode,
    gdome_xml_n_childNodes,
    gdome_xml_n_firstChild,
    gdome_xml_n_lastChild,
    gdome_xml_n_previousSibling,
    gdome_xml_n_nextSibling,
    gdome_xml_n_attributes,
    gdome_xml_n_ownerDocument,
    gdome_xml_n_insertBefore,
    gdome_xml_n_replaceChild,
    gdome_xml_n_removeChild,
    gdome_xml_n_appendChild,
    gdome_xml_n_hasChildNodes,
    gdome_xml_n_cloneNode,
    gdome_xml_n_normalize,
    gdome_xml_n_isSupported,
    gdome_xml_n_namespaceURI,
    gdome_xml_n_prefix,
    gdome_xml_n_set_prefix,
    gdome_xml_n_localName,
    gdome_xml_n_hasAttributes,
    gdome_xml_n_addEventListener,
    gdome_xml_n_removeEventListener,
    gdome_xml_n_dispatchEvent,
    gdome_xml_n_subTreeDispatchEvent
  },
  gdome_xml_ent_publicId,
  gdome_xml_ent_systemId,
  gdome_xml_ent_notationName
};


/**
 * gdome_xml_ent_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_ent_query_interface (GdomeNode *self, const char *interface, GdomeException *exc)
{
	Gdome_xml_Entity *priv = (Gdome_xml_Entity *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_ENT (priv), NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "Node") ||
      !strcmp (interface, "Entity") ||
      !strcmp (interface, "EventTarget")) {
    priv->refcnt++;
    return self;
  }
  else
    return NULL;
}

/**
 * gdome_xml_ent_publicId:
 * @self:  The Entity Object ref
 * @exc:  The Exception Object ref
 *
 * Returns: the public identifier associated with the entity, if specified.
 * If the public identifier was not specified, this is %NULL.
 */
GdomeDOMString *
gdome_xml_ent_publicId (GdomeEntity *self, GdomeException *exc)
{
  Gdome_xml_Entity *priv = (Gdome_xml_Entity *)self;
  xmlEntity *ent = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_ENT (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  ent = priv->n;
  if (ent->etype != XML_EXTERNAL_GENERAL_UNPARSED_ENTITY)
    return NULL;
  else
    return gdome_xml_str_mkref_dup((gchar *)ent->ExternalID);
}

/**
 * gdome_xml_ent_systemId:
 * @self:  The Entity Object ref
 * @exc:  The Exception Object ref
 *
 * Returns: the system identifier associated with the entity, if specified.
 * If the system identifier was not specified, this is %NULL.
 */
GdomeDOMString *
gdome_xml_ent_systemId (GdomeEntity *self, GdomeException *exc)
{
  Gdome_xml_Entity *priv = (Gdome_xml_Entity *)self;
  xmlEntity *ent = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_ENT (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  ent = priv->n;
  if (ent->etype != XML_EXTERNAL_GENERAL_UNPARSED_ENTITY)
    return NULL;
  else
    return gdome_xml_str_mkref_dup((gchar *)ent->SystemID);
}

/**
 * gdome_xml_ent_notationName:
 * @self:  The Entity Object ref
 * @exc:  The Exception Object ref
 *
 * Returns: for unparsed entities, the name of the notation for the entity.
 * For parsed entities, this is %NULL.
 */
GdomeDOMString *
gdome_xml_ent_notationName (GdomeEntity *self, GdomeException *exc)
{
  Gdome_xml_Entity *priv = (Gdome_xml_Entity *)self;
  xmlEntity *ent = NULL;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_ENT (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  ent = priv->n;
  if (ent->etype != XML_EXTERNAL_GENERAL_UNPARSED_ENTITY)
    return NULL;
  else
    return gdome_xml_str_mkref_dup((gchar *)gdome_xmlGetContent((xmlNode *)ent));
}
