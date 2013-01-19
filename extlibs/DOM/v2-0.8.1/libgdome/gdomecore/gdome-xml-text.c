/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-text.c
 *
 * Copyright (C) 1999 Raph Levien <raph@acm.org>
 * Copyright (C) 2000 Mathieu Lacage <mathieu@gnu.org>
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

/* -------------------------------------------------------------- */
/* ---------------------------  Text  --------------------------- */
/* -------------------------------------------------------------- */


#include <string.h>
#include "gdome.h"
#include "gdome-events.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include "gdome-xml-util.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-xml-str.h"
#include "gdome-xml-node.h"
#include "gdome-xml-cdata.h"
#include "gdome-xml-document.h"
#include "gdome-evt-event.h"
#include "gdome-evt-mevent.h"
#include "gdome-xml-text.h"


const GdomeTextVtab gdome_xml_t_vtab =
{
	{
  	{
			gdome_xml_n_ref,
			gdome_xml_n_unref,
			gdome_xml_t_query_interface,
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
};

/**
 * gdome_xml_t_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_t_query_interface (GdomeNode *self, const char *interface, GdomeException *exc)
{
	Gdome_xml_Text *priv = (Gdome_xml_Text *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_T (priv), NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "Node") ||
      !strcmp (interface, "CharacterData") ||
      !strcmp (interface, "Text") ||
      !strcmp (interface, "EventTarget")) {
    priv->refcnt++;
    return self;
  }
  else
    return NULL;
}

/**
 * gdome_xml_t_splitText:
 * @self:  The Text Object ref
 * @offset: The 16-bit unit offset at which to split, starting from 0
 * @exc:  The GdomeException Object ref
 *
 * Breaks this node into two nodes at the specified @offset, keeping both in
 * the tree as siblings. After being split, this node will contain all the
 * content up to the @offset point. A new node of the same type, which contains
 * all the content at and after the @offset point, is returned. If the original
 * node had a parent node, the new node is inserted as the next sibling of the
 * original node. When the @offset is equal to the length of this node, the new
 * node has no data.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
 * Returns: the new node, of the same type as this node.
 */
GdomeText *
gdome_xml_t_splitText (GdomeText *self, gulong offset, GdomeException *exc)
{
	Gdome_xml_Text *priv = (Gdome_xml_Text *)self;
	Gdome_xml_Text *new_priv = NULL;
  GdomeText *ret = NULL;
  GdomeDocument *owner;
	GdomeDOMString *new_str = NULL;
	gulong len;
	xmlNode *parent;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_T (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return NULL;
  }

  len = gdome_xml_cd_length ((GdomeCharacterData *)self, exc);
	if (len < offset) {
		*exc = GDOME_INDEX_SIZE_ERR;
		return NULL;
	}

  /* create the new text node */
  owner = gdome_xml_n_ownerDocument ((GdomeNode *)self, exc);
  if (len == offset)
    new_str = gdome_xml_str_mkref ("");
	else
		new_str = gdome_xml_cd_substringData ((GdomeCharacterData *)self, offset, len - offset, exc);
  ret = gdome_xml_doc_createTextNode (owner, new_str, exc);
  gdome_str_unref (new_str);
  gdome_doc_unref (owner, exc);
	new_priv = (Gdome_xml_Text *)ret;

	/* delete splitted data */
  gdome_xml_cd_deleteData ((GdomeCharacterData *)self, offset, len - offset, exc);

	/* insert the new node in the right position */
	parent = gdome_xmlGetParent (priv->n);
  if (parent != NULL) {
    GdomeMutationEvent *mev;
    GdomeNode *gparent;

		gdome_xmlSetParent (new_priv->n, parent);
		if (gdome_xmlGetLastChild (parent) == priv->n)
			gdome_xmlSetLastChild (parent, new_priv->n);
    gparent = gdome_xml_n_mkref (parent);

		if (gdome_xml_n_eventEnabledByCode ((GdomeNode *) self, DOM_NODE_INSERTED_EVENT_TYPE)) {
			/* Fire DOMNodeInserted */
			mev = gdome_evt_mevnt_mkref ();
			gdome_xml_n_ref ((GdomeNode *)ret, exc);
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_NODE_INSERTED_EVENT_TYPE,
																							 TRUE, FALSE, gparent,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent ((GdomeNode *)ret, (GdomeEvent *)mev, exc);
			gdome_xml_n_unref ((GdomeNode *)ret, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}

		if (gdome_xml_n_eventEnabledByCode ((GdomeNode *) self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
			/* Fire DOMSubtreeModified */
			mev = gdome_evt_mevnt_mkref ();
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, NULL,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent (gparent, (GdomeEvent *)mev, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
			gdome_xml_n_unref (gparent, exc);
		}
  }
	gdome_xmlSetNext (new_priv->n, gdome_xmlGetNext (priv->n));
	gdome_xmlSetPrev (new_priv->n, priv->n);
	gdome_xmlSetPrev (gdome_xmlGetNext (priv->n), new_priv->n);
	gdome_xmlSetNext (priv->n, new_priv->n);

	return ret;
}
