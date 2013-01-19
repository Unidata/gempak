/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-xmlmemory.c
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

/* NOTE

 This file contains some functions that work directly on the gnome-xml
 tree structure. So if something change we'll have to work only on this
 module. Many of functions implemented here are a copy of functions in
 libxml2 tree.c file. Differences should be only that in gdome_ ones we
 set Gdome_xml_Node->n to NULL before calling xmlFree function.

*/

#include "gdome.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include "gdome-xml-node.h"
#include "gdome-treegc.h"

static const xmlChar xmlStringText[] = { 't', 'e', 'x', 't', 0 };
static const xmlChar xmlStringTextNoenc[] =
  { 't', 'e', 'x', 't', 'n', 'o', 'e', 'n', 'c', 0 };
static const xmlChar xmlStringComment[] = { 'c', 'o', 'm', 'm', 'e', 'n', 't', 0 };

void gdome_xmlFreeProp (xmlAttrPtr cur);
void gdome_xmlFreePropList (xmlAttrPtr cur);
void gdome_xmlFreeNode (xmlNodePtr cur);
void gdome_xmlFreeNodeList (xmlNodePtr cur);

/**
 * gdome_xmlFreeSubtree:
 * @cur:  the root node of the subtree to free
 *
 * Free a node, this is a recursive behaviour, all the children are freed too.
 * This function also set to NULL the n field in the Gdome_xml_Node structure.
 */
void
gdome_xmlFreeSubtree (xmlNode *root) {
  g_return_if_fail (root != NULL);

  if (root->type == XML_ATTRIBUTE_NODE)
		gdome_xmlFreeProp ((xmlAttrPtr)root);
	else
		gdome_xmlFreeNode ((xmlNodePtr)root);
}

/**
 * gdome_xmlFreeProp:
 * @cur:  an attribute
 *
 * Free one attribute, all the content is freed too
 */
void
gdome_xmlFreeProp(xmlAttrPtr cur) {
  if (cur == NULL)
    return;

  /* Check for ID removal -> leading to invalid references ! */
  if ((cur->parent != NULL) && (cur->parent->doc != NULL) &&
      ((cur->parent->doc->intSubset != NULL) ||
       (cur->parent->doc->extSubset != NULL))) {
    if (xmlIsID(cur->parent->doc, cur->parent, cur))
      xmlRemoveID(cur->parent->doc, cur);
  }
  if (cur->name != NULL) xmlFree((char *) cur->name);
  if (cur->children != NULL) gdome_xmlFreeNodeList(cur->children);
  if (cur->_private != NULL) gdome_treegc_invalidateNode(cur->_private);
  xmlFree(cur);
}

/**
 * gdome_xmlFreePropList:
 * @cur:  the first property in the list
 *
 * Free a property and all its siblings, all the children are freed too.
 */
void
gdome_xmlFreePropList(xmlAttrPtr cur) {
  xmlAttrPtr next;
  if (cur == NULL)
    return;

  while (cur != NULL) {
    next = cur->next;
    gdome_xmlFreeProp(cur);
    cur = next;
  }
}

/**
 * gdome_xmlFreeNodeList:
 * @cur:  the first node in the list
 *
 * Free a node and all its siblings, this is a recursive behaviour, all
 * the children are freed too.
 */
void
gdome_xmlFreeNodeList(xmlNodePtr cur) {
  xmlNodePtr next;
  if (cur == NULL)
    return;

  while (cur != NULL) {
    next = cur->next;
    /* unroll to speed up freeing the document */
    if (cur->type != XML_DTD_NODE) {
      if ((cur->children != NULL) &&
          (cur->type != XML_ENTITY_REF_NODE))
        gdome_xmlFreeNodeList(cur->children);
      if (cur->properties != NULL)
        gdome_xmlFreePropList(cur->properties);
      if ((cur->type != XML_ELEMENT_NODE) &&
          (cur->type != XML_XINCLUDE_START) &&
          (cur->type != XML_XINCLUDE_END) &&
          (cur->type != XML_ENTITY_REF_NODE)) {
        if (cur->content != NULL) xmlFree(cur->content);
      }
      if (((cur->type == XML_ELEMENT_NODE) ||
           (cur->type == XML_XINCLUDE_START) ||
           (cur->type == XML_XINCLUDE_END)) &&
          (cur->nsDef != NULL))
        xmlFreeNsList(cur->nsDef);

      /*
       * When a node is a text node or a comment, it uses a global static
       * variable for the name of the node.
       *
       * The xmlStrEqual comparisons need to be done when (happened with
       * XML::libXML and XML::libXSLT) the library is included twice
       * statically in the binary and a tree allocated by one occurent
       * of the lib gets freed by the other occurence, in this case
       * the string addresses compare are not sufficient.
       */
      if ((cur->name != NULL) &&
          (cur->name != xmlStringText) &&
          (cur->name != xmlStringTextNoenc) &&
          (cur->name != xmlStringComment)) {
        if (cur->type == XML_TEXT_NODE) {
          if ((!xmlStrEqual(cur->name, xmlStringText)) &&
              (!xmlStrEqual(cur->name, xmlStringTextNoenc)))
            xmlFree((char *) cur->name);
        } else if (cur->type == XML_COMMENT_NODE) {
          if (!xmlStrEqual(cur->name, xmlStringComment))
            xmlFree((char *) cur->name);
        } else
          xmlFree((char *) cur->name);
      }
      /* TODO : derecursivate this function */
      if (cur->_private != NULL) gdome_treegc_invalidateNode(cur->_private);
      xmlFree(cur);
    }
    cur = next;
  }
}

/**
 * gdome_xmlFreeNode:
 * @cur:  the node
 *
 * Free a node, this is a recursive behaviour, all the children are freed too.
 * This doesn't unlink the child from the list, use xmlUnlinkNode() first.
 */
void
gdome_xmlFreeNode(xmlNodePtr cur) {
  if (cur == NULL)
    return;

  /* use xmlFreeDtd for DTD nodes */
  if (cur->type == XML_DTD_NODE)
    return;
  if ((cur->children != NULL) &&
      (cur->type != XML_ENTITY_REF_NODE))
    gdome_xmlFreeNodeList(cur->children);
  if (cur->properties != NULL)
    gdome_xmlFreePropList(cur->properties);
  if ((cur->type != XML_ELEMENT_NODE) &&
      (cur->content != NULL) &&
      (cur->type != XML_ENTITY_REF_NODE) &&
      (cur->type != XML_XINCLUDE_END) &&
      (cur->type != XML_XINCLUDE_START)) {
    xmlFree(cur->content);
  }

  /*
   * When a node is a text node or a comment, it uses a global static
   * variable for the name of the node.
   *
   * The xmlStrEqual comparisons need to be done when (happened with
   * XML::libXML and XML::libXSLT) the library is included twice statically
   * in the binary and a tree allocated by one occurent of the lib gets
   * freed by the other occurence, in this case the string addresses compare
   * are not sufficient.
   */
  if ((cur->name != NULL) &&
      (cur->name != xmlStringText) &&
      (cur->name != xmlStringTextNoenc) &&
      (cur->name != xmlStringComment)) {
    if (cur->type == XML_TEXT_NODE) {
      if ((!xmlStrEqual(cur->name, xmlStringText)) &&
          (!xmlStrEqual(cur->name, xmlStringTextNoenc)))
        xmlFree((char *) cur->name);
    } else if (cur->type == XML_COMMENT_NODE) {
      if (!xmlStrEqual(cur->name, xmlStringComment))
        xmlFree((char *) cur->name);
    } else
      xmlFree((char *) cur->name);
  }

  if (cur->nsDef != NULL) xmlFreeNsList(cur->nsDef);
  if (cur->_private != NULL) gdome_treegc_invalidateNode(cur->_private);
  xmlFree(cur);
}

