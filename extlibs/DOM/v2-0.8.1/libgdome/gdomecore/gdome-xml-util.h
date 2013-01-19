/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-util.h
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

#ifndef GDOME_XMLUTIL_FILE
#define GDOME_XMLUTIL_FILE

#include <libxml/tree.h>
#include "gdome.h"
#include "gdome-xml-node.h"

#define GDOME_XML_IS_N(node) (\
((Gdome_xml_Node *) node)->n->type == XML_ELEMENT_NODE       ||  \
((Gdome_xml_Node *) node)->n->type == XML_TEXT_NODE          ||  \
((Gdome_xml_Node *) node)->n->type == XML_CDATA_SECTION_NODE ||  \
((Gdome_xml_Node *) node)->n->type == XML_ENTITY_REF_NODE    ||  \
((Gdome_xml_Node *) node)->n->type == XML_ENTITY_NODE        ||  \
((Gdome_xml_Node *) node)->n->type == XML_PI_NODE            ||  \
((Gdome_xml_Node *) node)->n->type == XML_COMMENT_NODE       ||  \
((Gdome_xml_Node *) node)->n->type == XML_ATTRIBUTE_NODE     ||  \
((Gdome_xml_Node *) node)->n->type == XML_NOTATION_NODE      ||  \
((Gdome_xml_Node *) node)->n->type == XML_DOCUMENT_TYPE_NODE ||  \
((Gdome_xml_Node *) node)->n->type == XML_DOCUMENT_FRAG_NODE ||  \
((Gdome_xml_Node *) node)->n->type == XML_DTD_NODE           ||  \
((Gdome_xml_Node *) node)->n->type == XML_DOCUMENT_NODE      ||  \
((Gdome_xml_Node *) node)->n->type == XML_ENTITY_DECL        ||  \
((Gdome_xml_Node *) node)->n->type == XML_HTML_DOCUMENT_NODE ||  \
((Gdome_xml_Node *) node)->n->type == XML_NAMESPACE_DECL )

#define GDOME_XML_IS_TREE_N(node) (\
((Gdome_xml_Node *) node)->n->type == XML_ELEMENT_NODE       ||  \
((Gdome_xml_Node *) node)->n->type == XML_TEXT_NODE          ||  \
((Gdome_xml_Node *) node)->n->type == XML_CDATA_SECTION_NODE ||  \
((Gdome_xml_Node *) node)->n->type == XML_ENTITY_REF_NODE    ||  \
((Gdome_xml_Node *) node)->n->type == XML_PI_NODE            ||  \
((Gdome_xml_Node *) node)->n->type == XML_COMMENT_NODE       ||  \
((Gdome_xml_Node *) node)->n->type == XML_DOCUMENT_TYPE_NODE ||  \
((Gdome_xml_Node *) node)->n->type == XML_DTD_NODE )

#define GDOME_XML_IS_LEAF_N(node) (\
((Gdome_xml_Node *) node)->n->type == XML_NOTATION_NODE      ||  \
((Gdome_xml_Node *) node)->n->type == XML_TEXT_NODE          ||  \
((Gdome_xml_Node *) node)->n->type == XML_CDATA_SECTION_NODE ||  \
((Gdome_xml_Node *) node)->n->type == XML_PI_NODE            ||  \
((Gdome_xml_Node *) node)->n->type == XML_COMMENT_NODE       ||  \
((Gdome_xml_Node *) node)->n->type == XML_DOCUMENT_TYPE_NODE ||  \
((Gdome_xml_Node *) node)->n->type == XML_DTD_NODE )

#define GDOME_XML_IS_DOC(node) (\
((Gdome_xml_Node *) node)->n->type == XML_DOCUMENT_NODE      ||  \
((Gdome_xml_Node *) node)->n->type == XML_HTML_DOCUMENT_NODE )

#define GDOME_XML_IS_CD(cd) (\
((Gdome_xml_Node *) cd)->n->type == XML_CDATA_SECTION_NODE ||  \
((Gdome_xml_Node *) cd)->n->type == XML_TEXT_NODE          ||  \
((Gdome_xml_Node *) cd)->n->type == XML_COMMENT_NODE )

#define GDOME_XML_IS_CDS(cds) (\
((Gdome_xml_Node *) cds)->n->type == XML_CDATA_SECTION_NODE )

#define GDOME_XML_IS_C(c) (\
((Gdome_xml_Node *) c)->n->type == XML_COMMENT_NODE )

#define GDOME_XML_IS_T(t) (\
((Gdome_xml_Node *) t)->n->type == XML_CDATA_SECTION_NODE ||  \
((Gdome_xml_Node *) t)->n->type == XML_TEXT_NODE )

#define GDOME_XML_IS_EL(el) (\
((Gdome_xml_Node *) el)->n->type == XML_ELEMENT_NODE )

#define GDOME_XML_IS_NOT(not) (\
((Gdome_xml_Node *) not)->n->type == XML_NOTATION_NODE )

#define GDOME_XML_IS_A(a) (\
((Gdome_xml_Node *) a)->n->type == XML_ATTRIBUTE_NODE )

#define GDOME_XML_IS_DF(df) (\
((Gdome_xml_Node *) df)->n->type == XML_DOCUMENT_FRAG_NODE )

#define GDOME_XML_IS_NL(cd) 1

#define GDOME_XML_IS_ENT(ent) (\
((Gdome_xml_Node *) ent)->n->type == XML_ENTITY_NODE        ||  \
((Gdome_xml_Node *) ent)->n->type == XML_ENTITY_DECL )

#define GDOME_XML_IS_ER(er) (\
((Gdome_xml_Node *) er)->n->type == XML_ENTITY_REF_NODE )

#define GDOME_XML_IS_PI(pi) (\
((Gdome_xml_Node *) pi)->n->type == XML_PI_NODE )

#define GDOME_XML_IS_DT(dt) (\
((Gdome_xml_Node *) dt)->n->type == XML_DTD_NODE ||  \
((Gdome_xml_Node *) dt)->n->type == XML_DOCUMENT_TYPE_NODE )

#define GDOME_XML_IS_NS(ns) (\
((Gdome_xml_Node *) ns)->n->type == XML_NAMESPACE_DECL )

/* Utility Macro */
#define GDOME_ISREADONLY(node) ((Gdome_xml_Node *)node)->accessType == GDOME_READONLY_NODE

#endif /* GDOME_XMLUTIL_FILE */
