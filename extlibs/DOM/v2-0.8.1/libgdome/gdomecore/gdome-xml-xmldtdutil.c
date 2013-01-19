/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-xmldtdutil.c
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
 tree structure. We use it to make a right implementation of DocumentType
 interface at gdome2 layer: gnome-xml make difference between internal and
 external entities and notations.
 Theese functions are to joining notations and entities in a single hash holded
 by the Gdome_xml_DocumentType struct.
 With this functions we also create a wrapper on gnome-xml xmlNotation structs
 trasforming them in gdome_xmlNotation structs.
*/

#include <string.h>
#include "gdome.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include <libxml/xmlerror.h>
#include <libxml/hash.h>
#include "gdome-xml-xmldtdutil.h"


typedef struct _hashIterator hashIterator;
struct _hashIterator {
	xmlDoc *doc;
	xmlHashTable *ht;
};

/* Hash Scanner function for gdome_xmlNotationsHashCreate*/
void
notationsHashScanner (void *payload, void *data, xmlChar *name) {
	hashIterator *priv = (hashIterator *)data;
	xmlNotation *orig_not = (xmlNotation *)payload;
  gdome_xmlNotation *new_not = NULL;

  new_not = (gdome_xmlNotation *) xmlMalloc(sizeof(gdome_xmlNotation));
	if (new_not == NULL) {
		xmlGenericError(xmlGenericErrorContext,
										"gdome_createGdomeNotationsHash : malloc failed\n");
		return;
	}
	memset(new_not, 0 , sizeof(gdome_xmlNotation));

  /* Initialize gdome_xmlNotation struct */
	new_not->type = XML_NOTATION_NODE;
  new_not->name = orig_not->name;
	new_not->doc = priv->doc;
	new_not->PublicID = orig_not->PublicID;
	new_not->SystemID = orig_not->SystemID;
  new_not->orig = orig_not;

  /* add the new struct to the new hash */
  xmlHashAddEntry (priv->ht, new_not->name, new_not);
}

/* Hash Scanner function for gdome_xmlEntitiesHashCreate*/
void
entitiesHashScanner (void *payload, void *data, xmlChar *name) {
	hashIterator *priv = (hashIterator *)data;
	xmlEntity *orig_not = (xmlEntity *)payload;

  /* add the struct to the new hash */
  xmlHashAddEntry (priv->ht, name, orig_not);
}

/* Clean up a gdome_xmlNotation record */
void notationDeallocator(gdome_xmlNotation *not) {
	if (not == NULL) return;

  /*
	if (not->name != NULL)
		xmlFree((char *) not->name);
	if (not->PublicID != NULL)
		xmlFree((char *) not->PublicID);
	if (not->SystemID != NULL)
		xmlFree((char *) not->SystemID);
  xmlFree(not);
  */
  g_free (not);
}

/**
 * gdome_xmlNotationsHashCreate:
 * @doc:  Document that own notations
 *
 * Create a gdome notations hash joining external and internal notations hash
 * of the Document specified.
 * Returns: a gdome notations hash.
 */
xmlHashTable *
gdome_xmlNotationsHashCreate (xmlDoc *doc) {
	xmlHashTable *ret = NULL;
  hashIterator iter;

	if (doc == NULL)
		return NULL;

	if ((doc->extSubset != NULL && doc->extSubset->notations != NULL) ||
			(doc->intSubset != NULL && doc->intSubset->notations != NULL)) {
		ret = xmlHashCreate(0);
		iter.doc = doc;
		iter.ht = ret;
	}

	if (doc->extSubset != NULL && doc->extSubset->notations != NULL)
		xmlHashScan(doc->extSubset->notations, notationsHashScanner, &iter);
	if (doc->intSubset != NULL && doc->intSubset->notations != NULL)
		xmlHashScan(doc->intSubset->notations, notationsHashScanner, &iter);

	return ret;
}

/**
 * gdome_xmlNotationsHashFree:
 * @ht:  HashTable to free
 *
 * Free the Notations HashTable specified.
 */
void
gdome_xmlNotationsHashFree (xmlHashTable *ht) {
  xmlHashFree (ht, (xmlHashDeallocator) notationDeallocator);
}

/**
 * gdome_xmlEntitiesHashCreate:
 * @doc:  Document that own notations
 *
 * Create a gdome entities hash joining external and internal entities hash
 * of the Document specified.
 * Returns: a gdome entities hash.
 */
xmlHashTable *
gdome_xmlEntitiesHashCreate (xmlDoc *doc) {
	xmlHashTable *ret = NULL;
  hashIterator iter;

	if (doc == NULL)
		return NULL;

	if ((doc->extSubset != NULL && doc->extSubset->entities != NULL) ||
			(doc->intSubset != NULL && doc->intSubset->entities != NULL)) {
		ret = xmlHashCreate(0);
		iter.doc = doc;
		iter.ht = ret;
	}

	if (doc->extSubset != NULL && doc->extSubset->entities != NULL)
		xmlHashScan(doc->extSubset->entities, entitiesHashScanner, &iter);
	if (doc->intSubset != NULL && doc->intSubset->entities != NULL)
		xmlHashScan(doc->intSubset->entities, entitiesHashScanner, &iter);

	return ret;
}

/**
 * gdome_xmlEntitiesHashFree:
 * @ht:  HashTable to free
 *
 * Free the Entities HashTable specified.
 */
void
gdome_xmlEntitiesHashFree (xmlHashTable *ht) {
  xmlHashFree (ht, NULL);
}
