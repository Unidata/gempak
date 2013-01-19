/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-xmldtdutil.h
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

#ifndef GDOME_XML_XMLNOTATIONS_H
#define GDOME_XML_XMLNOTATIONS_H

typedef struct _gdome_xmlNotation gdome_xmlNotation;
struct _gdome_xmlNotation {
	void               *_private;
	xmlElementType          type; /* XML_NOTATION_NODE */
	const xmlChar          *name;	/* Attribute name */
	struct _xmlNode    *children;	/* NULL */
	struct _xmlNode        *last;	/* NULL */
	struct _xmlDtd       *parent;	/* NULL */
	struct _xmlNode        *next;	/* NULL */
	struct _xmlNode        *prev;	/* NULL */
	struct _xmlDoc          *doc; /* the containing document */

	const xmlChar      *PublicID;	/* External identifier for PUBLIC */
	const xmlChar      *SystemID;	/* URI for a SYSTEM or PUBLIC Entity */

	struct _xmlNotation    *orig;	/* the corresponding xmlNotation* */
};

xmlHashTable *gdome_xmlNotationsHashCreate (xmlDoc *doc);
void          gdome_xmlNotationsHashFree (xmlHashTable *ht);
xmlHashTable *gdome_xmlEntitiesHashCreate (xmlDoc *doc);
void          gdome_xmlEntitiesHashFree (xmlHashTable *ht);

#endif /* GDOME_XML_XMLNOTATIONS_H */

