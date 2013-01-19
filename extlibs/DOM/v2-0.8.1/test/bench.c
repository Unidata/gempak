/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* bench.c
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

void printElHead (GdomeElement *el) {
	GdomeDOMString *str;
	GdomeException exc;

	str = gdome_el_tagName (el, &exc);
	printf("<%s", str->str);
	gdome_str_unref (str);

	if (gdome_el_hasAttributes (el, &exc)) {
		unsigned int i,len;
		GdomeNamedNodeMap *attrs;
		GdomeAttr *attr;

		attrs = gdome_el_attributes (el, &exc);

		len = gdome_nnm_length (attrs, &exc);
		for (i = 0; i < len; i++) {
			attr = (GdomeAttr *)gdome_nnm_item (attrs, i, &exc);
			str = gdome_a_name (attr, &exc);
			printf (" %s=", str->str);
			gdome_str_unref (str);
			str = gdome_a_value (attr, &exc);
			printf ("\"%s\"", str->str);
			gdome_str_unref (str);
			gdome_a_unref (attr, &exc);
		}

		gdome_nnm_unref (attrs, &exc);
	}

	printf (">");
}

void printElFoot (GdomeElement *el) {
	GdomeDOMString *str;
	GdomeException exc;

	str = gdome_el_tagName (el, &exc);
	printf("</%s>", str->str);
	gdome_str_unref (str);
}

void traversal (GdomeElement *el) {
	GdomeNode *child, *temp;
	unsigned short type;
	GdomeDOMString *str;
	GdomeException exc;

	child = gdome_el_firstChild (el, &exc);

	while(child) {
		type = gdome_n_nodeType (child, &exc);
		switch (type) {
		case GDOME_ELEMENT_NODE:
			printElHead ((GdomeElement *)child);
			traversal ((GdomeElement *)child);
			printElFoot ((GdomeElement *)child);
			break;
		case GDOME_TEXT_NODE:
			str = gdome_t_data ((GdomeText *)child, &exc);
			printf (str->str);
			gdome_str_unref (str);
			break;
		}
		temp = gdome_n_nextSibling (child, &exc);
		gdome_n_unref (child, &exc);
		child = temp;
	}
}

int main (int argc, char **argv) {
	GdomeDOMImplementation *domimpl;
	GdomeDocument *domdoc;
	GdomeException exc;

	if (argc != 2) {
		fprintf(stderr, "Usage: %s <XML file>\n", argv[0]);
		return -1;
	}

	domimpl = gdome_di_mkref ();
	domdoc = gdome_di_createDocFromURI (domimpl, argv[1], GDOME_LOAD_PARSING, &exc);
	if (domdoc == NULL) {
		fprintf(stderr, "DOMImplementation.createDocFromURI NULL\n");
		return -1;
	}

	traversal ((GdomeElement *)domdoc);

	gdome_di_freeDoc (domimpl, domdoc, &exc);
	gdome_di_unref (domimpl, &exc);

	return 0;
}
