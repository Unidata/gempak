/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* examplec.c
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

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <gdome.h>

int isSpaceStr (GdomeDOMString *domstr) {
	gchar *str = domstr->str;

	for (; *str; str++)
		if (!isspace(*str))
			return 0;
	return 1;
}

void cleanSubTree (GdomeNode *node) {
	GdomeNode *child = NULL, *dchild = NULL;
	GdomeNodeList *nl = NULL;
	GdomeException exc;
	GdomeDOMString *str;
	long i, nllength;

	nl = gdome_n_childNodes (node, &exc);
	if ((nllength = gdome_nl_length (nl, &exc)) == 0) {
		if (nl != NULL)
			gdome_nl_unref (nl, &exc);
		return;
	}

	for (i = nllength-1; i >= 0; i--) {
		child = gdome_nl_item (nl, i, &exc);
		str = NULL;

		if (gdome_n_nodeType (child, &exc) == GDOME_COMMENT_NODE ||
				(gdome_n_nodeType (child, &exc) == GDOME_TEXT_NODE &&
				 isSpaceStr ((str = gdome_t_data ((GdomeText *)child, &exc))))) {
			dchild = gdome_n_removeChild (node, child, &exc);

			if (str != NULL)
				gdome_str_unref (str);
			if (dchild != NULL)
				gdome_n_unref (dchild, &exc);
		}
		else if (gdome_n_hasChildNodes (child, &exc))
			cleanSubTree (child);

		if (child != NULL)
			gdome_n_unref (child, &exc);
	}

	if (nl != NULL)
    gdome_nl_unref (nl, &exc);
}

int main (int argc, char **argv) {
	GdomeDOMImplementation *domimpl;
	GdomeDocument *doc;
	GdomeElement *rootel;
	GdomeException exc;

	/* First I get a DOMImplementation reference */
	domimpl = gdome_di_mkref ();

	/* I load a new document from the file name "exampleb.xml */
	doc = gdome_di_createDocFromURI(domimpl, LOCALDIR"/examplec.xml", GDOME_LOAD_PARSING, &exc);
	if (doc == NULL) {
		fprintf (stderr, "DOMImplementation.createDocFromURI: failed\n\tException #%d\n", exc);
		return 1;
	}

	/* I get reference to the root element of the document */
	rootel = gdome_doc_documentElement (doc, &exc);
	if (rootel == NULL) {
		fprintf (stderr, "Document.documentElement: NULL\n\tException #%d\n", exc);
		return 1;
	}

  cleanSubTree ((GdomeNode *)rootel);
  gdome_el_unref (rootel, &exc);

	if (!gdome_di_saveDocToFile (domimpl, doc, LOCALDIR"/examplec_out.xml", GDOME_SAVE_STANDARD, &exc)) {
		fprintf (stderr, "DOMImplementation.saveDocToFile: failed\n\tException #%d\n", exc);
		return 1;
	}

	/* I free the document structure and the DOMImplementation */
	gdome_doc_unref (doc, &exc);
	gdome_di_unref (domimpl, &exc);

	return 0;
}
