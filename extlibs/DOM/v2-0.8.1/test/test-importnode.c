/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-importnode.c
 *
 * Copyright (C) 2002 T.J. Mather <tjmather@tjmather.com>
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

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gdome.h>

char buffer1[] = "<?xml version=\"1.0\"?>\n"
                 "<!DOCTYPE root [\n"
                 "<!ENTITY lt \"&#38;#60;\">\n"
                 "]>\n"
                 "<root>lt</root>";

char buffer2[] = "<?xml version='1.0'?>\n"
                 "<root></root>";

void dot() {
  printf(".");
  fflush(stdout);
}

int
test_importnode (GdomeDocument *domdoc1, GdomeDocument *domdoc2) {
	GdomeElement *el1, *el2;
	GdomeText *txt1, *txt2, *txt2a;
	int exc = 0;

  el1 = gdome_doc_documentElement (domdoc1, &exc);
  if (el1 == NULL) {
    fprintf (stderr, "Document.documentElement: NULL\n");
    return 0;
  }
	dot();
  el2 = gdome_doc_documentElement (domdoc2, &exc);
  if (el2 == NULL) {
    fprintf (stderr, "Document.documentElement: NULL\n");
    return 0;
  }
	dot();
	txt1 = (GdomeText *)gdome_el_firstChild (el1, &exc);
	if (txt1 == NULL) {
		fprintf (stderr, "Element.firstChild: NULL\n");
		return 0;
	}
	dot();

	txt2 = (GdomeText *)gdome_doc_importNode(domdoc2, (GdomeNode *)txt1, 1, &exc);
	if (txt2 == NULL) {
		fprintf (stderr, "Document.importNode: NULL\n");
		return 0;
	}

	dot();
	txt2a = (GdomeText *) gdome_el_appendChild(el2, (GdomeNode *)txt2, &exc);

	gdome_el_unref(el2, &exc);
	gdome_el_unref(el1, &exc);
	gdome_t_unref(txt1, &exc);
	gdome_t_unref(txt2, &exc);
	gdome_t_unref(txt2a, &exc);
	dot();
	return 1;
}

int
main (int argc, char **argv)
{
  GdomeDOMImplementation *domImpl = NULL;
  GdomeDocument *domdoc1 = NULL;
  GdomeDocument *domdoc2 = NULL;
  GdomeException exc = 0;
  gboolean ret;

  domImpl = gdome_di_mkref();

  domdoc1 = gdome_di_createDocFromMemory (domImpl, buffer1, GDOME_LOAD_PARSING, &exc);
  if (domdoc1 == NULL) {
    fprintf (stderr, "DOMImplementation.createDocFromMemory: failed\n");
    return -1;
  }

  domdoc2 = gdome_di_createDocFromMemory (domImpl, buffer2, GDOME_LOAD_PARSING, &exc);
  if (domdoc2 == NULL) {
    fprintf (stderr, "DOMImplementation.createDocFromMemory: failed\n");
    return -1;
  }

	ret = test_importnode(domdoc1,domdoc2);

  gdome_doc_unref (domdoc1, &exc);
  gdome_doc_unref (domdoc2, &exc);

  if(ret)
    return 0;
  else
    return -1;
}
