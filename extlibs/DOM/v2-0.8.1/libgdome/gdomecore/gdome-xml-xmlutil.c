/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-xmlutil.c
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
 module.

*/

#include <glib.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include <string.h>
#include "gdome.h"
#include "gdome-xml-xmlutil.h"

#ifndef INLINE
/**
 * gdome_xmlGetPrevChild:
 * @node:  the node of which the previous sibling is wanted
 *
 * Returns: the previous child of the node specified.
 */
xmlNode *
gdome_xmlGetPrev(xmlNode *node) {
	if (node == NULL)
		return NULL;

	return node->prev;
}

/**
 * gdome_xmlSetPrev:
 * @node:  the node of which the previous sibling is to be set
 * @prev:  the node to set as previous
 *
 * Sets <code>prev</code> as the previous sibling of <code>node</code>
 */
void
gdome_xmlSetPrev(xmlNode *node, xmlNode *prev) {
	if (node == NULL)
		return;

	node->prev = prev;
}

/**
 * gdome_xmlGetNextChild:
 * @node:  the node of which the next child is wanted
 *
 * Returns: the next child of the node specified.
 */
xmlNode *
gdome_xmlGetNext(xmlNode *node) {
	if (node == NULL)
		return NULL;

	return node->next;
}

/**
 * gdome_xmlSetNext:
 * @node:  the node of which the next sibling is to be set
 * @prev:  the node to set as next
 *
 * Sets <code>next</code> as the next sibling of <code>node</code>
 */
void
gdome_xmlSetNext(xmlNode *node, xmlNode *next) {
	if (node == NULL)
		return;

	node->next= next;
}

/**
 * gdome_xmlGetFirstChild:
 * @node:  the node of which the first child is wanted
 *
 * Returns: the first child of the node specified.
 */
xmlNode *
gdome_xmlGetFirstChild(xmlNode *node) {
	if (node == NULL)
		return NULL;

	return node->children;
}

/**
 * gdome_xmlSetLastChild:
 * @node:  the node of which the last child is to be set
 * @last:  the node to set as last child
 *
 * Sets @last as the last child of @node.
 */
void
gdome_xmlSetFirstChild (xmlNode *node, xmlNode *first) {
  if (node == NULL)
    return;

  node->children = first;
}

/**
 * gdome_xmlGetLastChild:
 * @node:  the node of which the last child is wanted
 *
 * Returns: the last child of the node specified.
 */
xmlNode *
gdome_xmlGetLastChild(xmlNode *node) {
	if (node == NULL)
		return NULL;

	return node->last;
}

/**
 * gdome_xmlSetLastChild:
 * @node:  the node of which the last child is to be set
 * @last:  the node to set as last child
 *
 * Sets @last as the last child of @node.
 */
void gdome_xmlSetLastChild (xmlNode *node, xmlNode *last) {
  if (node == NULL)
    return;

  node->last = last;
}

/**
 * gdome_xmlGetAttrList:
 * @elem:  the element node of which the attribute list is wanted
 *
 * Returns: the first attribute of the list.
 */
xmlAttr *
gdome_xmlGetAttrList(xmlNode *elem) {
	if (elem == NULL)
		return NULL;

	return elem->properties;
}

/**
 * gdome_xmlGetNsDeclList:
 * @elem:  the element node of which the namespace decl list is wanted
 *
 * Returns: the first namespace of the list.
 */
xmlNs *
gdome_xmlGetNsDeclList(xmlNode *elem) {
	if (elem == NULL)
		return NULL;

	return elem->nsDef;
}

/**
 * gdome_xmlSetAttrList:
 * @elem:  the element of node which the attribute list is to be set
 * @attr:  the attribute that has to start the list
 *
 * Set the attribute list of the element specified to <code>attr</code>
 */
void
gdome_xmlSetAttrList(xmlNode *elem, xmlAttr *attr) {
	if(elem == NULL)
		return;

	elem->properties = attr;
}
#endif /* INLINE */

/**
 * gdome_xmlSetNsDeclList:
 * @elem:  the element of node which the attribute list is to be set
 * @attr:  the namespace declarationattribute that has to start the list
 *
 * Set the namespace definition list of the element specified to
 * <code>attr</code>
 */
void
gdome_xmlSetNsDeclList(xmlNode *elem, xmlChar *prefix, xmlChar *href) {
	xmlNs * ns;

	if(elem == NULL)
		return;

	ns = xmlNewNs(elem, href, prefix);

	elem->nsDef = ns;
}

#ifndef INLINE
/**
 * gdome_xmlGetOwner:
 * @node:  the node of which the owner document is wanted
 *
 * Returns: the document that owns the node specified.
 */
xmlDoc *
gdome_xmlGetOwner(xmlNode *node) {
	if (node == NULL)
		return NULL;

	return node->doc;
}
#endif /* INLINE */

/**
 * gdome_xmlGetOwnerOrSelf:
 * @node: the node of which the owner document is wanted
 *
 * Returns: the document that owns the node specified,
 *          or the node itself if it turns out to be the document node
 */
xmlDoc *
gdome_xmlGetOwnerOrSelf(xmlNode *node) {
	if (node == NULL)
		return NULL;

	if (gdome_xmlGetType(node) == XML_DOCUMENT_NODE)
		return (xmlDoc *) node;
	else
		return gdome_xmlGetOwner(node);
}

#ifndef INLINE
/**
 * gdome_xmlSetOwner:
 * @node:  the node of which the owner document is to be set
 *
 * Set the owner of @node.
 */
void
gdome_xmlSetOwner(xmlNode *node, xmlDoc *owner) {
	if (node == NULL)
		return;

	node->doc = owner;
}

/**
 * gdome_xmlGetParent:
 * @node:  the node of which the parent node is wanted
 *
 * Returns: the parent of the node specified.
 */
xmlNode *
gdome_xmlGetParent(xmlNode *node) {
	if (node == NULL)
		return NULL;

	return node->parent;
}

/**
 * gdome_xmlSetParent:
 * @node:  the node of which the parent is to be set
 * @parent:  the parent to set
 *
 * Set the parent of @node.
 */
void
gdome_xmlSetParent(xmlNode *node, xmlNode *parent) {
	if (node == NULL)
		return;

	node->parent = parent;
}
#endif /* INLINE */

/**
 * gdome_xmlGetNsPrefix:
 * @node:  the node of which the prefix is wanted
 *
 * Returns: the prefix of the node specified or %NULL if the node has no
 * prefix.
 */
const xmlChar *
gdome_xmlGetNsPrefix(xmlNode *node) {
	if (node == NULL || node->ns == NULL)
		return NULL;

	return node->ns->prefix;
}

/**
 * gdome_xmlGetNsURI:
 * @node:  the node of which the namespaceURI is wanted
 *
 * Returns: the namespaceURI of the node specified or %NULL if the node
 * has no namespace.
 */
const xmlChar *
gdome_xmlGetNsURI(xmlNode *node) {
	if (node == NULL || node->ns == NULL)
		return NULL;

	return node->ns->href;
}

#ifndef INLINE
/**
 * gdome_xmlGetNs:
 * @node:  the node of which the namespace is wanted
 *
 * Returns: the namespace of the node specified or %NULL if the node
 * has no namespace.
 */
xmlNs *
gdome_xmlGetNs(xmlNode *node) {
	if (node == NULL)
		return NULL;

	return node->ns;
}

/**
 * gdome_xmlSetNs:
 * @node:  the node of which the namespace is to be set
 * @ns:  the namespace to set
 *
 * Set @ns as namespace for @node
 */
void
gdome_xmlSetNs (xmlNode *node, xmlNs *ns) {
	if (node == NULL || ns == NULL)
    return;

  node->ns = ns;
}

/**
 * gdome_xmlGetType:
 * @node:  the node of which the type is wanted
 *
 * Returns: the type of the node specified.
 */
xmlElementType
gdome_xmlGetType(xmlNode *node) {
	if (node == NULL)
		return 0;

	return node->type;
}

/**
 * gdome_xmlGetContent:
 * @node:  the node of which the content is wanted
 *
 * Returns: the content of the node specified.
 */
xmlChar *
gdome_xmlGetContent(xmlNode *node) {
	if (node == NULL)
		return NULL;

	return node->content;
}

/**
 * gdome_xmlGetName:
 * @node:  the node of which the name is wanted
 *
 * Returns: the name of the node specified.
 */
const xmlChar *
gdome_xmlGetName(xmlNode *node) {
	if (node == NULL)
		return NULL;

	return node->name;
}
#endif /* INLINE */

/**
 * gdome_xmlGetAttr:
 * @elem:  the element node where find the attribute to unlink
 * @name:  the name of attribute to search
 *
 * Get the attribute specified by @name if exists.
 * Returns: the attribute if found, otherwise %NULL
 */
xmlAttr *
gdome_xmlGetAttr(xmlNode *elem, const xmlChar* name) {
  xmlAttr *cur = NULL;
	if(elem == NULL || name == NULL)
		return NULL;

  for(cur = elem->properties;
      cur && !xmlStrEqual(name, gdome_xmlGetName((xmlNode *)cur));
      cur = cur->next);

  return cur;
}

/**
 * gdome_xmlGetNsAttr:
 * @elem:  the element node where find the attribute to unlink
 * @namespaceURI:  the namespace URI of attribute to search
 * @localName:  the local name of the attribute to search
 *
 * Get the attribute specified by @namespaceURI and its
 * @localName, if exists.
 * Returns: the attribute if found, %NULL otherwise.
 */
xmlAttr *
gdome_xmlGetNsAttr(xmlNode *elem, const xmlChar* namespaceURI, const xmlChar* localName) {
  xmlAttr *cur = NULL;

  if(elem == NULL || namespaceURI == NULL || localName == NULL)
		return NULL;

  for(cur = elem->properties;
			cur != NULL &&
			(!xmlStrEqual(localName, gdome_xmlGetName((xmlNode *)cur)) ||
			 ((cur->ns != NULL && !xmlStrEqual(namespaceURI, gdome_xmlGetNsURI((xmlNode *)cur))) ||
				cur->ns == NULL));
			cur = cur->next);

  return cur;
}


/**
 * gdome_xmlSetAttrValue:
 * @attr:  the attribute which the value is to be set
 * @value:  the value to set
 *
 * Set a new value to an Attribute node.
 */
void
gdome_xmlSetAttrValue(xmlAttr *attr, xmlChar *value) {
  if(attr == NULL)
    return;

  if (attr->children != NULL)
    xmlFreeNodeList(attr->children);
  attr->children = NULL;
  attr->last = NULL;

  if (value != NULL) {
    xmlChar *buffer;
    xmlNode *tmp;

    buffer = xmlEncodeEntitiesReentrant(attr->doc, value);
    attr->children = xmlStringGetNodeList(attr->doc, buffer);
    attr->last = NULL;
    tmp = attr->children;
    for(tmp = attr->children; tmp != NULL; tmp = tmp->next) {
      tmp->parent = (xmlNode *)attr;
      tmp->doc = attr->doc;
      if (tmp->next == NULL)
        attr->last = tmp;
    }
    xmlFree (buffer);
  }

  return;
}

/**
 * gdome_xmlUnlinkChild:
 * @parent:  the node that owns the node to unlink
 * @child:  the node to unlink
 *
 * Unlink the node specified, if exists
 * Returns: the unlinked node if found, otherwise %NULL
 */
xmlNode *
gdome_xmlUnlinkChild(xmlNode *parent, xmlNode* child) {

  if(parent == NULL || child == NULL)
		return NULL;

  if(gdome_xmlGetParent(child) != parent)
		return NULL;

	if (child->prev)
		child->prev->next = child->next;
	else
		parent->children = child->next;
	if (child->next)
		child->next->prev = child->prev;
	else
    parent->last = child->prev;
	child->prev = child->next = NULL;
	child->parent = NULL;

	return child;
}

/**
 * gdome_xmlAppendChild:
 * @parent:  the node that where append the child
 * @child:  the node to append
 *
 * Append @child at the end of the @parent children
 * list. If @child is a %XML_DOCUMENT_FRAG_NODE, all its childrens
 * are appended at the end of the @parent children list.
 * Returns: the appended node.
 */
xmlNode *
gdome_xmlAppendChild(xmlNode *parent, xmlNode* child) {
  xmlNode *iter;

  if (parent == NULL || child == NULL)
		return NULL;

  if (child->type == XML_DOCUMENT_FRAG_NODE) {
		if (child->children != NULL && child->last != NULL) {
      for (iter = child->children; iter != NULL; iter = iter->next)
        iter->parent = parent;

      if (parent->last) {
        parent->last->next = child->children;
        child->children->prev = parent->last;
      } else {
        parent->children = child->children;
        child->children->prev = NULL;
      }
      parent->last = child->last;
      child->last->next = NULL;
    }
  } else {
    if (parent->last) {
      parent->last->next = child;
      child->prev = parent->last;
    } else {
      parent->children = child;
      child->prev = NULL;
    }
    parent->last = child;
    child->next = NULL;
    child->parent = parent;
  }

	return child;
}

/**
 * gdome_xmlInsertBeforeChild:
 * @parent:  the node where insert the child
 * @newchild:  the node to add
 * @refchild:  the node before which the new node must be inserted.
 *
 * Inserts @newchild before @refchild in the
 * @parent list of childrens. If @newchild is a
 * %XML_DOCUMENT_FRAG_NODE, all its childrens are inserted before
 * @refchild in the @parent list of childrens.
 * Returns: the inserted node or %NULL if @refchild doesn't belong to
 * @parent.
 */
xmlNode *
gdome_xmlInsertBeforeChild(xmlNode *parent, xmlNode* newchild, xmlNode *refchild) {
  xmlNode *iter;

  if (parent == NULL || newchild == NULL || refchild == NULL)
		return NULL;

  if  (gdome_xmlGetParent(refchild) != parent)
		return NULL;

  if (newchild->type == XML_DOCUMENT_FRAG_NODE) {
		if (newchild->children != NULL && newchild->last != NULL) {
      for (iter = newchild->children; iter != NULL; iter = iter->next)
        iter->parent = parent;

			if (refchild->prev) {
				refchild->prev->next = newchild->children;
				newchild->children->prev = refchild->prev;
			} else {
				parent->children = newchild->children;
				newchild->children->prev = NULL;
			}
			refchild->prev = newchild->last;
      newchild->last->next = refchild;
		}
	} else {
		if (refchild->prev) {
			refchild->prev->next = newchild;
			newchild->prev = refchild->prev;
		} else {
			parent->children = newchild;
			newchild->prev = NULL;
		}
		refchild->prev = newchild;
		newchild->next = refchild;
		newchild->parent = parent;
	}

	return newchild;
}

/**
 * gdome_xmlUnlinkAttr:
 * @elem:  the element node where find the attribute to unlink
 * @name:  the name of attribute to search
 *
 * Unlink the attribute specified by @name, if an attribute with
 * such name exists in @elem.
 * Returns: the attribute unlinked if found, otherwise %NULL.
 */
xmlAttr *
gdome_xmlUnlinkAttr(xmlNode *elem, const xmlChar* name) {
  xmlAttr *cur = NULL;

  if(elem == NULL || name == NULL)
		return NULL;

  cur = gdome_xmlGetAttr(elem, name);

	if(cur) {
		if (cur->prev)
			cur->prev->next = cur->next;
		else
			elem->properties = cur->next;
		if (cur->next)
      cur->next->prev = cur->prev;
    cur->prev = cur->next = NULL;
    cur->parent = NULL;
  }

	return cur;
}

/**
 * gdome_xmlUnlinkNsAttr:
 * @elem:  the element node where find the attribute to unlink
 * @namespaceURI:  the namespace URI of attribute to search
 * @localName:  the local name of the attribute to search
 *
 * Unlink the attribute specified by @namespaceURI and its
 * @localName, if an attribute with such a local name and
 * namespace URI exists in @elem.
 * Returns: the attribute unlinked if found, %NULL otherwise.
 */
xmlAttr *
gdome_xmlUnlinkNsAttr(xmlNode *elem, const xmlChar* namespaceURI, const xmlChar* localName) {
  xmlAttr *cur = NULL;

  if(elem == NULL || namespaceURI == NULL || localName == NULL)
		return NULL;

	cur = gdome_xmlGetNsAttr(elem, namespaceURI, localName);

  if(cur) {
    if (cur->prev)
			cur->prev->next = cur->next;
		else
			elem->properties = cur->next;
    if (cur->next)
      cur->next->prev = cur->prev;
    cur->prev = cur->next = NULL;
    cur->parent = NULL;
    return cur;
	}

  return NULL;
}

/**
 * gdome_xmlUnlinkNsDecl:
 * @elem:  the element node where find the attribute to unlink
 * @localName:  the name of attribute to search
 *
 * Unlink the attribute specified by @name, if an attribute with
 * such name exists in @elem.
 * Returns: the attribute unlinked if found, otherwise %NULL.
 */
xmlNs *
gdome_xmlUnlinkNsDecl(xmlNode *elem, const xmlChar* localName) {
  xmlNs *cur = NULL;
	xmlNs *prev = NULL;

  if(elem == NULL)
		return NULL;

	if (localName == NULL || xmlStrEqual(localName, "")) {
		cur = elem->nsDef;
		while (cur != NULL) {
			if (cur->prefix == NULL) {
				if (prev)
					prev->next = cur->next;
				else
					elem->nsDef = cur->next;
				cur->next = NULL;
				return cur;
			}
			prev = cur;
			cur = cur->next;
		}
	} else {
		cur = elem->nsDef;
		while (cur != NULL) {
			if (cur->prefix != NULL && xmlStrEqual(localName, cur->prefix)) {
				if (prev)
					prev->next = cur->next;
				else
					elem->nsDef = cur->next;
				cur->next = NULL;
				return cur;
			}
			prev = cur;
			cur = cur->next;
		}
	}

	return NULL;
}

/**
 * gdome_xmlLinkAttr:
 * @elem:  the element node where add the attribute
 * @attr:  the attribute to add
 *
 * Link @attribute to the @elem.
 * Returns: -1 if fails, 0 otherwise.
 */
int
gdome_xmlLinkAttr(xmlNode *elem, xmlAttr *attr) {
  xmlAttr *cur = NULL;

  if (elem == NULL || attr == NULL)
    return -1;

  gdome_xmlSetParent((xmlNode *)attr, (xmlNode *)elem);
  gdome_xmlSetOwner((xmlNode *)attr, gdome_xmlGetOwner(elem));

  if(gdome_xmlGetAttrList(elem) == NULL) {
    gdome_xmlSetAttrList(elem, attr);
    return 0;
  } else {
    for(cur = gdome_xmlGetAttrList(elem); cur->next != NULL; cur = cur->next);

    cur->next = attr;
    attr->prev = cur;
    return 0;
  }
}

/**
 * gdome_xmlLinkNsDecl:
 * @elem:  the element node where add the namespace declaration
 * @attr:  the namespace declaration attribute to add
 *
 * Link @attr to the @elem.
 * Returns: -1 if fails, 0 otherwise.
 */
int
gdome_xmlLinkNsDecl(xmlNode *elem, xmlChar *prefix, xmlChar *href) {
  xmlNs *cur = NULL;
	xmlNs * ns;

  if (elem == NULL)
    return -1;

  if(gdome_xmlGetNsDeclList(elem) == NULL) {
    gdome_xmlSetNsDeclList(elem, prefix, href);
    return 0;
  } else {
    for(cur = gdome_xmlGetNsDeclList(elem); cur->next != NULL; cur = cur->next);
		ns = xmlNewNs(elem, href, prefix);
    cur->next = ns;
    return 0;
  }
}

/**
 * gdome_xmlGetChildrenItem:
 * @parent:  The node of which the children this is to be scan
 * @index: The index of the children wanted
 *
 * Returns: the index-th children of the parent specified.
 */
xmlNode *
gdome_xmlGetChildrenItem (xmlNode *parent, gulong index) {
  xmlNode *ret = NULL;
  gulong cur;

	for(ret = parent->children, cur = 0;
			ret != NULL && cur < index;
			cur++,	ret = ret->next);

	return ret;
}

/**
 * gdome_xmlNamedPreorderTraversal:
 * @root:  The root node of the subtree to scan
 * @tagURI:  the namespaceURI of the nodes to select or NULL if no namespace
 *          check is wanted
 * @tagName:  the localName of the nodes to select
 * @cur:  The current node counter (must be 0L)
 * @index: The index of node wanted
 *
 * Returns: the index-th node of the subtree with the localName and nameSpace
 * specified.
 */
xmlNode *
gdome_xmlNamedPreorderTraversal (xmlNode *root, xmlChar *tagURI, xmlChar *tagName, gulong *cur, gulong index) {
  xmlNode *iter, *ret = NULL;

  iter = root->children;

  if (*cur == index)
    return root;
  else if (iter == NULL)
    return NULL;

  while(iter != NULL && *cur<index) {
    if(iter->type == XML_ELEMENT_NODE &&
       (xmlStrEqual(gdome_xmlGetName(iter), tagName) ||
        xmlStrEqual(tagName, "*"))) {
      if(tagURI != NULL &&
         (xmlStrEqual(gdome_xmlGetNsURI(iter), tagURI) ||
          xmlStrEqual(tagURI, "*")))
        (*cur)++;
      else if(tagURI == NULL)
        (*cur)++;
    }
    ret = gdome_xmlNamedPreorderTraversal(iter, tagURI, tagName, cur, index);
    if(ret == NULL)
      iter = iter->next;
  }

  return ret;
}

/**
 * gdome_xmlGetChildrensLength:
 * @parent:  The node of which the length of the children list is wanted
 *
 * Returns: the length of the children list of the node specified.
 */
gulong
gdome_xmlGetChildrensLength (xmlNode *parent) {
  xmlNode *iter = NULL;
  gulong ret;

	for(iter = parent->children, ret = 0;
			iter != NULL;
			ret++,	iter = iter->next);

	return ret;
}

/**
 * gdome_xmlNamedCntTreeElementes:
 * @root:  The root node of the subtree to scan
 * @tagURI:  the namespaceURI of the nodes to select or NULL if no namespace
 *          check is wanted
 * @tagName:  the localName of the nodes to select
 * @cur:  The current node counter (must be %0L)
 *
 * Sets in @cur the length of thr subtree specified.
 */
void
gdome_xmlNamedCntTreeElements (xmlNode *root, xmlChar *tagURI, xmlChar *tagName, gulong *cur) {
  xmlNode *iter;

  iter = root->children;

  if (iter == NULL)
    return;

  while(iter != NULL) {
    if(iter->type == XML_ELEMENT_NODE &&
       (xmlStrEqual(gdome_xmlGetName(iter), tagName) ||
        xmlStrEqual(tagName, "*"))) {
      if(tagURI != NULL &&
         (xmlStrEqual(gdome_xmlGetNsURI(iter), tagURI) ||
          xmlStrEqual(tagURI, "*")))
        (*cur)++;
      else if(tagURI == NULL)
        (*cur)++;
    }
    gdome_xmlNamedCntTreeElements (iter, tagURI, tagName, cur);
    iter = iter->next;
  }

  return;
}

/*
 * gdome_xmlGetNsDecl:
 * @elem:  Element on which to look for namespace declaration attribute
 * @name:  namespace declaration prefix
 *
 * Similar to xmlGetProp, but gets the namespace declaration attributes.
 * Note that expects name to not include 'xmlns' string
 */
xmlNs *
gdome_xmlGetNsDecl (xmlNode *node, xmlChar *localName) {
	xmlNsPtr cur;
	xmlNs *ret = NULL;
	if (node == NULL)
		return NULL;

	if (localName == NULL || xmlStrEqual(localName, "")) {
		cur = node->nsDef;
		while (cur != NULL) {
			if (cur->prefix == NULL) {
				ret = cur;
				break;
			}
			cur = cur->next;
		}
	} else {
		cur = node->nsDef;
		while (cur != NULL) {
			if (cur->prefix != NULL && xmlStrEqual(localName, cur->prefix)) {
				ret = cur;
				break;
			}
			cur = cur->next;
		}
	}
	return ret;
}

/*
 * gdome_xmlNewNsDecl
 * @node:  The holding node
 * @prefix:  namespace declaration prefix
 * @value: namespace URI
 *
 * Similar to xmlNewProp, but does not attach attribute to owner element's
 * children.
 */
xmlAttrPtr
gdome_xmlNewNsDecl(xmlNodePtr node, const xmlChar *prefix, const xmlChar *value) {
	xmlAttrPtr cur;
	xmlDocPtr doc = NULL;

	/*
	 * Allocate a new property and fill the fields.
	 */
	cur = (xmlAttrPtr) xmlMalloc(sizeof(xmlAttr));
	if (cur == NULL) {
		xmlGenericError(xmlGenericErrorContext,
										"xmlNewProp : malloc failed\n");
		return(NULL);
	}
	memset(cur, 0, sizeof(xmlAttr));
	cur->type = XML_ATTRIBUTE_NODE;

	cur->parent = node; 
	if (node != NULL) {
		doc = node->doc;
		cur->doc = doc;
	}
	if (prefix == NULL) {
		cur->name = g_strdup("xmlns");
	} else {
		cur->name = g_strdup(prefix);
		cur->ns = gdome_xmlNewNs (doc, GDOME_XMLNS_NAMESPACE, "xmlns");
	}
	if (value != NULL) {
		xmlChar *buffer;
		xmlNodePtr tmp;

		buffer = xmlEncodeEntitiesReentrant(doc, value);
		cur->children = xmlStringGetNodeList(doc, buffer);
		cur->last = NULL;
		tmp = cur->children;
		while (tmp != NULL) {
	    tmp->parent = (xmlNodePtr) cur;
	    tmp->doc = doc;
	    if (tmp->next == NULL)
				cur->last = tmp;
	    tmp = tmp->next;
		}
		xmlFree(buffer);
	}	

	return(cur);
}

/*
 * gdome_xmlIsNsDecl:
 * @a:  Attribute node
 * 
 * Returns %TRUE if attribute is namespace declaration node, %FALSE otherwise.
 */
GdomeBoolean
gdome_xmlIsNsDecl (xmlAttr *a) {
	if (xmlStrEqual(a->name, "xmlns"))
		return 1;
	if (a->ns != NULL)
		if (xmlStrEqual(a->ns->href, GDOME_XMLNS_NAMESPACE))
			return 1;
	return 0;
}

/*
 * gdome_xmlGetNsDeclByAttr:
 * @a:  Attribute Node
 *
 * Get pointer to namespace node corresponding to namespace declaration
 * attribute.  Used by getUserData and setUserData.
 */
xmlNs *
gdome_xmlGetNsDeclByAttr (xmlAttr *a) {
	if (a->ns == NULL && xmlStrEqual(a->name, "xmlns"))
		return gdome_xmlGetNsDecl(a->parent, NULL);
	else if (a->ns != NULL && xmlStrEqual(a->ns->href, GDOME_XMLNS_NAMESPACE))
		return gdome_xmlGetNsDecl(a->parent, (xmlChar *)a->name);
	else
		return NULL;
}

/*
 * gdome_xmlSetOldNs:
 * @doc:  Document node
 * @ns:  Namespace node
 *
 * Adds namespace node to libxml's oldNs structure, so that it will
 * get freed when the document is freed.
 */
void
gdome_xmlSetOldNs (xmlDoc *doc, xmlNs *ns) {
/*
 xmlNs *cur;
*/
	if (doc == NULL)
		return;

	if (doc->oldNs == NULL) {
		/*
		 * Allocate a new Namespace and fill the fields.
		 */
		doc->oldNs = (xmlNsPtr) xmlMalloc(sizeof(xmlNs));
		if (doc->oldNs == NULL) {
			xmlGenericError(xmlGenericErrorContext,
											"xmlSearchNsByHref : malloc failed\n");
			return;
		}
		memset(doc->oldNs, 0, sizeof(xmlNs));
		doc->oldNs->type = XML_LOCAL_NAMESPACE;
		
		doc->oldNs->href = xmlStrdup(XML_XML_NAMESPACE); 
		doc->oldNs->prefix = xmlStrdup((const xmlChar *)"xml"); 
	}

	ns->next = doc->oldNs;
	doc->oldNs = ns;
/*
	cur = doc->oldNs;
	while (cur->next != NULL) {
		cur = cur->next;
	}
	cur->next = ns;
*/
}

xmlNs*
gdome_xmlNewNs (xmlDoc *doc, const xmlChar *href, const xmlChar *prefix)
{
	xmlNs *cur;

	g_return_val_if_fail (doc != NULL, NULL);
	g_return_val_if_fail (href != NULL, NULL);

	cur = doc->oldNs;
	while (cur != NULL &&
				 ((cur->prefix == NULL && prefix != NULL) ||
					(cur->prefix != NULL && prefix == NULL) ||
					!xmlStrEqual (cur->prefix, prefix)) &&
				 !xmlStrEqual (cur->href, href))
		cur = cur->next;

	if (cur == NULL) {
		cur = xmlNewNs (NULL, href, prefix);
		cur->next = doc->oldNs;
		doc->oldNs = cur;
	}

	return cur;
}
