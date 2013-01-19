/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-treegc.c
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
#include "gdome.h"
#include "gdome-events.h"
#include "gdome-refdebug.h"
#include <libxml/tree.h>
#include "gdome-xml-util.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-xml-node.h"
#include "gdome-xml-document.h"

/**
 * gdome_treegc_addNode:
 * @node:  the wrapper to register
 *
 * Increase the livenodes field
 */
void
gdome_treegc_addNode (GdomeNode *node)
{
  Gdome_xml_Node *priv = (Gdome_xml_Node *)node;
  Gdome_xml_Document *ownerDoc;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_N(priv));

	ownerDoc =  gdome_xmlGetOwner (priv->n)->_private;
	g_assert (ownerDoc != NULL);
	ownerDoc->livenodes++;
}

/**
 * gdome_treegc_delNode:
 * @node:  the wrapper to un-register
 *
 * Decrease the livenodes field. If the field reaches zero
 * (that is, there are no more live wrappers) free the whole document tree
 */
void
gdome_treegc_delNode (GdomeNode *node)
{
  Gdome_xml_Node *priv = (Gdome_xml_Node *)node;
  Gdome_xml_Document *ownerDoc;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_N(priv));

	ownerDoc =  gdome_xmlGetOwner (priv->n)->_private;
	g_assert (ownerDoc != NULL);
	ownerDoc->livenodes--;

	if (ownerDoc->livenodes == 0) {
		ownerDoc->n->_private = NULL;

#ifdef DEBUG_REFCNT
		gdome_refdbg_delRef ((void *)ownerDoc, GDOME_REFDBG_NODE);
#endif

		xmlFreeDoc (ownerDoc->n);
		g_free (ownerDoc);
	}
}

/**
 * gdome_treegc_invalidateNode:
 * @node:  the wrapper to invalidate
 *
 * Set the n field of the wrapper to NULL and
 * decrease the livenode field of the document
 */
void
gdome_treegc_invalidateNode (GdomeNode *node)
{
  Gdome_xml_Node *priv = (Gdome_xml_Node *)node;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (priv->n != NULL);

	gdome_treegc_delNode (node);
	priv->n = NULL;
}

/**
 * gdome_treegc_livenodes:
 * @node:  a node in the Document main tree
 *
 * Returns the number of live nodes in the Document main tree.
 */
int
gdome_treegc_livenodes (GdomeNode *node)
{
  Gdome_xml_Node *priv = (Gdome_xml_Node *)node;
  Gdome_xml_Document *ownerDoc;

  g_return_val_if_fail (priv != NULL, -1);
  g_return_val_if_fail (GDOME_XML_IS_N(priv), -1);

  ownerDoc =  gdome_xmlGetOwner (priv->n)->_private;
	g_assert (ownerDoc != NULL);

  return ownerDoc->livenodes;
}
