/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* examplea.c
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
	GdomeText *txtnode;
	GdomeException exc;
	GdomeNode *result;
	GdomeDOMString *name, *value;

	/* First I get a DOMImplementation reference */
  domimpl = gdome_di_mkref ();

	/* I create a new document with TEST as root element */
	name = gdome_str_mkref ("TEST");
	doc = gdome_di_createDocument(domimpl, NULL, name, NULL, &exc);
	if (doc == NULL) {
		fprintf (stderr, "DOMImplementation.createDocument: failed\n\tException #%d\n", exc);
		return 1;
	}
	gdome_str_unref (name);

	/* I get reference to the root element of the document */
	root = gdome_doc_documentElement (doc, &exc);
	if (root == NULL) {
		fprintf (stderr, "Document.documentElement: NULL\n\tException #%d\n", exc);
		return 1;
	}

	/* I set an attribute to the root element */
	name = gdome_str_mkref ("PACKAGE");
	value = gdome_str_mkref ("gdome2");
	gdome_el_setAttribute (root, name, value, &exc);
	if (exc) {
		fprintf (stderr, "Element.setAttribute: failed\n\tException #%d\n", exc);
		return 1;
	}
	gdome_str_unref (name);
	gdome_str_unref (value);

	/* I create a new element called RELEASE */
	name = gdome_str_mkref ("RELEASE");
	el = gdome_doc_createElement (doc, name, &exc);
	if (el == NULL) {
		fprintf (stderr, "Document.createElement: NULL\n\tException #%d\n", exc);
		return 1;
	}
	gdome_str_unref (name);

	/* I create a Text Node*/
	value = gdome_str_mkref ("0.6.x");
	txtnode = gdome_doc_createTextNode (doc, value, &exc);
	if (txtnode == NULL) {
		fprintf (stderr, "Document.createTextNode: NULL\n\tException #%d\n", exc);
		return 1;
	}
	gdome_str_unref (value);

	/* I append the Text Node created to the childs list of the RELEASE element */
	result = gdome_el_appendChild (el, (GdomeNode *)txtnode, &exc);
	if (result != (GdomeNode *)txtnode) {
		fprintf (stderr, "Element.appendChild: failed\n\tException #%d\n", exc);
		return 1;
	}
	gdome_t_unref(txtnode, &exc);
	gdome_n_unref(result, &exc);

	/* I append the RELEASE element to the childs list of the root element */
	result = gdome_el_appendChild (root, (GdomeNode *)el, &exc);
	if (result != (GdomeNode *)el) {
		fprintf (stderr, "Element.appendChild: failed\n\tException #%d\n", exc);
		return 1;
	}
	gdome_el_unref(el, &exc);
	gdome_el_unref(root, &exc);
	gdome_n_unref(result, &exc);

	/* I save the  created document to a file named "examplea.xml */
	if (!gdome_di_saveDocToFile (domimpl, doc, LOCALDIR"/examplea.xml", GDOME_SAVE_STANDARD, &exc)) {
		fprintf (stderr, "DOMImplementation.saveDocToFile: failed\n\tException #%d\n", exc);
		return 1;
	}

	/* I free the document structure and the DOMImplementation */
	gdome_doc_unref (doc, &exc);
	gdome_di_unref (domimpl, &exc);

	return 0;
}

