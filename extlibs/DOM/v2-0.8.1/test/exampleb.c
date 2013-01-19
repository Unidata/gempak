/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* exampleb.c
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

#include <stdio.h>
#include <gdome.h>

int main (int argc, char **argv) {
	GdomeDOMImplementation *domimpl;
	GdomeDocument *doc;
	GdomeElement *root, *el;
	GdomeNodeList *childs;
	GdomeException exc;
	GdomeDOMString *name, *value;
	unsigned long i, nchilds;

	/* First I get a DOMImplementation reference */
	domimpl = gdome_di_mkref ();

	/* I load a new document from the file name "exampleb.xml */
	doc = gdome_di_createDocFromURI(domimpl, LOCALDIR"/exampleb.xml", GDOME_LOAD_PARSING, &exc);
	if (doc == NULL) {
		fprintf (stderr, "DOMImplementation.createDocFromURI: failed\n\tException #%d\n", exc);
		return 1;
	}

	/* I get reference to the root element of the document */
	root = gdome_doc_documentElement (doc, &exc);
	if (root == NULL) {
		fprintf (stderr, "Document.documentElement: NULL\n\tException #%d\n", exc);
		return 1;
	}

	/* I get the reference to the childrens NodeList of the root element */
	childs = gdome_el_childNodes (root, &exc);
	if (childs == NULL) {
		fprintf (stderr, "Element.childNodes: NULL\n\tException #%d\n", exc);
		return 1;
	}

	/* I add the attribute CHECKED="yes" to all element childs of the root element */
	name = gdome_str_mkref ("CHECKED");
	value = gdome_str_mkref ("yes");
	nchilds = gdome_nl_length (childs, &exc);
	for (i = 0; i < nchilds; i++) {
		el = (GdomeElement *)gdome_nl_item (childs, i, &exc);
		if (el == NULL) {
			fprintf (stderr, "NodeList.item(%d): NULL\n\tException #%d\n", (int)i, exc);
			return 1;
		}
		if (gdome_el_nodeType (el, &exc) == GDOME_ELEMENT_NODE) {
			gdome_el_setAttribute (el, name, value, &exc);
			if (exc) {
				fprintf (stderr, "Element.setAttribute: failed\n\tException #%d\n", exc);
				return 1;
			}
		}
		gdome_el_unref (el, &exc);
  }
  gdome_nl_unref (childs, &exc);
  gdome_el_unref (root, &exc);

	/* I save the modified document to a file named "exampleb_out.xml */
	if (!gdome_di_saveDocToFile (domimpl, doc, LOCALDIR"/exampleb_out.xml", GDOME_SAVE_STANDARD, &exc)) {
		fprintf (stderr, "DOMImplementation.saveDocToFile: failed\n\tException #%d\n", exc);
		return 1;
	}

	/* I free the document structure and the DOMImplementation */
	gdome_doc_unref (doc, &exc);
	gdome_di_unref (domimpl, &exc);

	return 0;
}
