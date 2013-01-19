/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-nodel.c
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdlib.h>
#include <string.h>
#include "gdome.h"
#include "gdome-refdebug.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include "gdome-xml-util.h"
#include "gdome-xml-node.h"
#include "gdome-xml-nodel.h"
#include "gdome-xml-str.h"
#include "gdome-xml-xmlutil.h"

const GdomeNodeListVtab gdome_xml_nl_vtab = {
  gdome_xml_nl_ref,
  gdome_xml_nl_unref,
  gdome_xml_nl_query_interface,
  gdome_xml_nl_item,
  gdome_xml_nl_length
};

/**
 * gdome_xml_nl_mkref:
 * @root:  Node Object root of the subtree wanted
 * @tagName:  name of tags wanted, may be "*" for all nodes or NULL
 * @tagURI:  namespaceURI of the tags wanted, may be "*" for any namespace or NULL
 *
 * Returns: a NodeList Objects ref that point to the subtree specified.
 */
GdomeNodeList *
gdome_xml_nl_mkref (GdomeNode *root, GdomeDOMString *tagName, GdomeDOMString *tagURI, GdomeAccessType accessType) {
  Gdome_xml_NodeList *result = NULL;
  GdomeException exc;

	if(root == NULL || (tagURI != NULL && tagName == NULL))
    return NULL;

  result = g_new (Gdome_xml_NodeList, 1);
  memset(result, 0, sizeof(Gdome_xml_NodeList));
#ifdef DEBUG_REFCNT
  gdome_refdbg_addRef ((void *)result, GDOME_REFDBG_NODELIST);
#endif

  result->refcnt = 1;
  gdome_xml_n_ref (root, &exc);
	result->root = root;
  if(tagName != NULL)
    result->tagName = gdome_xml_str_mkref_dup(tagName->str);
  if(tagURI != NULL)
    result->tagURI = gdome_xml_str_mkref_dup(tagURI->str);
  result->accessType = accessType;
	result->vtab = &gdome_xml_nl_vtab;

	return (GdomeNodeList *)result;
}

/**
 * gdome_xml_nl_ref:
 * @self:  NodeList Object ref
 * @exc:  Exception Object ref
 *
 * Increase the reference count of the specified NodeList.
 */
void
gdome_xml_nl_ref (GdomeNodeList *self, GdomeException *exc)
{
  Gdome_xml_NodeList *priv = (Gdome_xml_NodeList *)self;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (exc != NULL);

  priv->refcnt++;
}

/**
 * gdome_xml_nl_unref:
 * @self:  NodeList Object ref
 * @exc:  Exception Object ref
 *
 * Decrease the reference count of the specified NodeList. Free the NodeList
 * structure if the specified NodeList will have zero reference.
 */
void
gdome_xml_nl_unref (GdomeNodeList *self, GdomeException *exc)
{
  Gdome_xml_NodeList *priv = (Gdome_xml_NodeList *)self;
	
  g_return_if_fail (priv != NULL);
  g_return_if_fail (exc != NULL);

  if (--priv->refcnt == 0) {
    gdome_xml_n_unref (priv->root, exc);
    if(priv->tagName != NULL)
      gdome_xml_str_unref(priv->tagName);
    if(priv->tagURI != NULL)
      gdome_xml_str_unref(priv->tagURI);
#ifdef DEBUG_REFCNT
    gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_NODELIST);
#endif
		g_free (self);
	}
}

/**
 * gdome_xml_nl_query_interface:
 * @self:  NodeList Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_nl_query_interface (GdomeNodeList *self, const char *interface, GdomeException *exc)
{
	Gdome_xml_NodeList *priv = (Gdome_xml_NodeList *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "NodeList")) {
    priv->refcnt++;
    return self;
  }
  else
    return NULL;
}

/**
 * gdome_xml_nl_item:
 * @self:  NodeList Object ref
 * @index:  Index into the collection
 * @exc:  Exception Object ref
 *
 * Returns: the indexth item in the collection. If @index is greater than or
 * equal to the number of nodes in the list, this returns %NULL.
 */
GdomeNode *
gdome_xml_nl_item (GdomeNodeList *self, gulong index, GdomeException *exc)
{
  Gdome_xml_NodeList *priv = (Gdome_xml_NodeList *)self;
	xmlNode *root = NULL;
	xmlNode *ret = NULL;
  gulong cur = 0;
	
  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (GDOME_XML_IS_LEAF_N (priv->root))
    return NULL;

  root = ((Gdome_xml_Node*)priv->root)->n;
  if (priv->tagName != NULL && priv->tagURI != NULL)
    ret = gdome_xmlNamedPreorderTraversal(root, (xmlChar *)priv->tagURI->str, (xmlChar *)priv->tagName->str, &cur, index+1);
  else if (priv->tagName != NULL && priv->tagURI == NULL)
    ret = gdome_xmlNamedPreorderTraversal(root, NULL, (xmlChar *)priv->tagName->str, &cur, index+1);
  else
    ret = gdome_xmlGetChildrenItem(root, index);

	return gdome_xml_n_mkref (ret);
}

/**
 * gdome_xml_nl_length:
 * @self:  NodeList Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the number of nodes in the list. The range of valid child node
 * indices is 0 to length-1 inclusive.
 */
gulong
gdome_xml_nl_length (GdomeNodeList *self, GdomeException *exc)
{
  Gdome_xml_NodeList *priv = (Gdome_xml_NodeList *)self;
  xmlNode *root = NULL;
  gulong ret = 0L;

  g_return_val_if_fail (priv != NULL, 0L);
  g_return_val_if_fail (exc != NULL, 0L);

  if (GDOME_XML_IS_LEAF_N (priv->root))
    return 0L;

  root = ((Gdome_xml_Node*)priv->root)->n;
  if (priv->tagName != NULL && priv->tagURI != NULL)
    gdome_xmlNamedCntTreeElements(root,  (xmlChar *)priv->tagURI->str, (xmlChar *)priv->tagName->str, &ret);
  else if (priv->tagName != NULL && priv->tagURI == NULL)
    gdome_xmlNamedCntTreeElements(root, NULL, (xmlChar *)priv->tagName->str, &ret);
  else
    ret = gdome_xmlGetChildrensLength(root);

  return ret;
}
