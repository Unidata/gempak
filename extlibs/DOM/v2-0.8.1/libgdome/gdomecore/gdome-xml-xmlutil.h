/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-xmlutil.h
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

#ifndef GDOME_XML_XMLUTIL_H
#define GDOME_XML_XMLUTIL_H

#define INLINE0

#ifdef INLINE
#define gdome_xmlGetPrev(node) ((node)->prev)
#define gdome_xmlSetPrev(node,p) ((node)->prev = (p))
#define gdome_xmlGetNext(node) ((node)->next)
#define gdome_xmlSetNext(node,p) ((node)->next = (p))
#define gdome_xmlGetFirstChild(node) ((node)->children)
#define gdome_xmlSetFirstChild(node,p) ((node)->children = (p))
#define gdome_xmlGetLastChild(node) ((node)->last)
#define gdome_xmlSetLastChild(node,p) ((node)->last = (p))
#define gdome_xmlGetAttrList(elem) ((elem)->properties)
#define gdome_xmlSetAttrList(elem,p) ((elem)->properties = (p))
#define gdome_xmlGetNsDeclList(elem) ((elem)->nsDef)
#define gdome_xmlGetOwner(node) ((node)->doc)
#define gdome_xmlSetOwner(node,p) ((node)->doc = (p))
#define gdome_xmlGetParent(node) ((node)->parent)
#define gdome_xmlSetParent(node,p) ((node)->parent = (p))
#define gdome_xmlGetNs(node) ((node)->ns)
#define gdome_xmlSetNs(node,p) ((node)->ns = (p))
#define gdome_xmlGetType(node) ((node)->type)
#define gdome_xmlGetName(node) ((node)->name)
#define gdome_xmlGetContent(node) ((node)->content)
#else
xmlNode *       gdome_xmlGetPrev                (xmlNode *node);
void            gdome_xmlSetPrev                (xmlNode *node,
                                                 xmlNode *prev);
xmlNode *       gdome_xmlGetNext                (xmlNode *node);
void            gdome_xmlSetNext                (xmlNode *node,
                                                 xmlNode *next);
xmlNode *	      gdome_xmlGetFirstChild          (xmlNode *node);
void            gdome_xmlSetFirstChild          (xmlNode *node,
                                                 xmlNode *first);
xmlNode *       gdome_xmlGetLastChild           (xmlNode *node);
void            gdome_xmlSetLastChild           (xmlNode *node,
                                                 xmlNode *last);
xmlAttr *       gdome_xmlGetAttrList            (xmlNode *elem);
xmlNs *         gdome_xmlGetNsDeclList          (xmlNode *elem);
void            gdome_xmlSetAttrList            (xmlNode *elem,
                                                 xmlAttr *attr);
xmlDoc *        gdome_xmlGetOwner               (xmlNode *node);
void            gdome_xmlSetOwner               (xmlNode *node,
                                                 xmlDoc *owner);
xmlNode *       gdome_xmlGetParent              (xmlNode *node);
void            gdome_xmlSetParent              (xmlNode *node,
                                                 xmlNode *parent);
xmlNs *         gdome_xmlGetNs                  (xmlNode *node);
void            gdome_xmlSetNs                  (xmlNode *node, xmlNs *ns);
xmlElementType  gdome_xmlGetType                (xmlNode *node);
const xmlChar * gdome_xmlGetName                (xmlNode *node);
xmlChar *       gdome_xmlGetContent             (xmlNode *node);
#endif /* INLINE */

void            gdome_xmlSetNsDeclList          (xmlNode *elem,
                                                 xmlChar *prefix,
																								 xmlChar *href);
xmlDoc *        gdome_xmlGetOwnerOrSelf         (xmlNode *node);
const xmlChar * gdome_xmlGetNsPrefix            (xmlNode *node);
const xmlChar * gdome_xmlGetNsURI               (xmlNode *node);
xmlAttr *       gdome_xmlGetAttr                (xmlNode *elem,
                                                 const xmlChar* name);
xmlAttr *       gdome_xmlGetNsAttr              (xmlNode *elem,
                                                 const xmlChar* namespaceURI,
                                                 const xmlChar* localName);
void            gdome_xmlSetAttrValue           (xmlAttr *attr,
                                                 xmlChar *value);
xmlNode *       gdome_xmlAppendChild            (xmlNode *parent,
                                                 xmlNode *child);
xmlNode *       gdome_xmlInsertBeforeChild      (xmlNode *parent,
                                                 xmlNode* newchild,
                                                 xmlNode *refchild);
xmlNode *       gdome_xmlUnlinkChild            (xmlNode *parent,
                                                 xmlNode *child);
xmlAttr *       gdome_xmlUnlinkAttr             (xmlNode *elem,
                                                 const xmlChar* name);
xmlAttr *       gdome_xmlUnlinkNsAttr           (xmlNode *elem,
                                                 const xmlChar* namespaceURI,
                                                 const xmlChar* localName);
xmlNs *         gdome_xmlUnlinkNsDecl           (xmlNode *elem,
																								 const xmlChar* localName);
int             gdome_xmlLinkAttr               (xmlNode *elem,
                                                 xmlAttr *attr);
int             gdome_xmlLinkNsDecl             (xmlNode *elem,
																								 xmlChar *prefix,
																								 xmlChar *href);
xmlNode *       gdome_xmlGetChildrenItem        (xmlNode *parent,
                                                 gulong index);
xmlNode *       gdome_xmlNamedPreorderTraversal (xmlNode *root,
                                                 xmlChar *tagURI,
                                                 xmlChar *tagName,
                                                 gulong *cur,
                                                 gulong index);
gulong          gdome_xmlGetChildrensLength     (xmlNode *parent);
void            gdome_xmlNamedCntTreeElements   (xmlNode *root,
                                                 xmlChar *tagURI,
                                                 xmlChar *tagName,
                                                 gulong *cur);
xmlNs *         gdome_xmlGetNsDecl              (xmlNode *elem,
																								 xmlChar *localName);
xmlAttrPtr      gdome_xmlNewNsDecl              (xmlNodePtr node,
																								 const xmlChar *name,
																								 const xmlChar *value);
GdomeBoolean    gdome_xmlIsNsDecl               (xmlAttr *a);
xmlNs *         gdome_xmlGetNsDeclByAttr        (xmlAttr *a);
void            gdome_xmlSetOldNs               (xmlDoc *doc, xmlNs *ns);
xmlNs *         gdome_xmlNewNs                  (xmlDoc *doc, const xmlChar *href, const xmlChar *prefix);

#endif /* GDOME_XML_XMLUTIL_H */
