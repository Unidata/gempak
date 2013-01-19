/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-cdatas.c
 *
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

#include <string.h>
#include "gdome.h"
#include "gdome-xml-util.h"
#include "gdome-xml-node.h"
#include "gdome-xml-cdata.h"
#include "gdome-xml-text.h"
#include "gdome-xml-cdatas.h"


const GdomeCDATASectionVtab gdome_xml_cds_vtab = {
  {
    {
      {
        gdome_xml_n_ref,
        gdome_xml_n_unref,
        gdome_xml_cds_query_interface,
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
      gdome_xml_cd_data,
      gdome_xml_cd_set_data,
      gdome_xml_cd_length,
      gdome_xml_cd_substringData,
      gdome_xml_cd_appendData,
      gdome_xml_cd_insertData,
      gdome_xml_cd_deleteData,
      gdome_xml_cd_replaceData
    },
    gdome_xml_t_splitText
  },
};

/**
 * gdome_xml_cds_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_cds_query_interface (GdomeNode *self, const char *interface, GdomeException *exc)
{
  Gdome_xml_CDATASection *priv = (Gdome_xml_CDATASection *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_CDS (priv), NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "Node") ||
      !strcmp (interface, "CharacterData") ||
      !strcmp (interface, "Text") ||
      !strcmp (interface, "CDATASection") ||
      !strcmp (interface, "EventTarget")) {
    priv->refcnt++;
    return self;
  }
  else
    return NULL;
}
