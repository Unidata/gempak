/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-loadsave.c
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
#include <stdlib.h>
#include <gdome.h>

char buffer1[] = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
                 "<!DOCTYPE TEST [\n"
                 "  <!ELEMENT TEST ANY>\n"
                 "  <!ELEMENT OTHER ANY>\n"
                 "  <!ELEMENT NODE ANY>\n"
                 "  <!ATTLIST NODE\n"
                 "    ITEM ID #REQUIRED\n"
                 "  >\n"
                 "]>\n"
                 "<TEST>\n"
                 "  <OTHER>\n"
                 "    <OTHER>\n"
                 "      <OTHER>\n"
                 "        <NODE ITEM=\"A\">zero</NODE>\n"
                 "        <NODE ITEM=\"B\">one</NODE>\n"
                 "      </OTHER>\n"
                 "      <NODE ITEM=\"C\">two</NODE>\n"
                 "    </OTHER>\n"
                 "  </OTHER>\n"
                 "  <NODE ITEM=\"D\">three\n"
                 "    <OTHER>\n"
                 "      <OTHER></OTHER>\n"
                 "    </OTHER>\n"
                 "    <NODE ITEM=\"E\">four</NODE>\n"
                 "    <NODE ITEM=\"F\">five</NODE>\n"
                 "  </NODE>\n"
                 "  <OTHER>\n"
                 "    <OTHER>\n"
                 "      <OTHER>\n"
                 "        <NODE ITEM=\"G\">six</NODE>\n"
                 "        <NODE ITEM=\"I\">seven</NODE>\n"
                 "      </OTHER>\n"
                 "      <NODE ITEM=\"L\">eight</NODE>\n"
                 "    </OTHER>\n"
                 "  </OTHER>\n"
                 "</TEST>\n";

char buffer2[] = "<?xml version=\"1.0\"?>\n"
                 "<!DOCTYPE TEST [\n"
                 "  <!ELEMENT TEST ANY>\n"
                 "  <!ENTITY foo \"bar\">\n"
                 "]>\n"
                 "<TEST>\n"
                 "  &Uogon;\n"
                 "  &foo;\n"
                 "  &UpArrowDownArrow;\n"
                 "</TEST>\n";

const GdomeEntitiesTableEntry entityTable[] = {
{ "UnderBar", "\xcc\xb2\x00", NULL, NULL },
{ "UnderBrace", "\xef\xb8\xb8\x00", NULL, NULL },
{ "UnderBracket", "\xe2\x8e\xb5\x00", NULL, NULL },
{ "UnderParenthesis", "\xef\xb8\xb6\x00", NULL, NULL },
{ "Union", "\xe2\x8b\x83\x00", NULL, NULL },
{ "UnionPlus", "\xe2\x8a\x8e\x00", NULL, NULL },
{ "uogon", "\xc5\xb3\x00", NULL, NULL },
{ "Uogon", "\xc5\xb2\x00", NULL, NULL },
{ "uopf", "\xf0\x9d\x95\xa6\x00", NULL, NULL },
{ "Uopf", "\xf0\x9d\x95\x8c\x00", NULL, NULL },
{ "uparrow", "\xe2\x86\x91\x00", NULL, NULL },
{ "Uparrow", "\xe2\x87\x91\x00", NULL, NULL },
{ "UpArrow", "\xe2\x86\x91\x00", NULL, NULL },
{ "UpArrowBar", "\xe2\xa4\x92\x00", NULL, NULL },
{ "UpArrowDownArrow", "\xe2\x87\x85\x00", NULL, NULL },
{ NULL, NULL, NULL, NULL}};

void
dot() {
	printf(".");
	fflush(stdout);
}

int
main (int argc, char **argv)
{
	GdomeDOMImplementation *domImpl = NULL;
	GdomeDocument *domdoc = NULL;
  GdomeException exc = 0;
  char *mem;

	domImpl = gdome_di_mkref();

  domdoc = gdome_di_createDocFromURI (domImpl, LOCALDIR"/test-document2.xml", GDOME_LOAD_PARSING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOMImplementation.createDocFromURI: parsing failed\n   test-document2.xml not found\n");
      return -1;
  }
  gdome_doc_unref(domdoc, &exc);
  dot();

  domdoc = gdome_di_createDocFromURI (domImpl, LOCALDIR"/test-document2.xml", GDOME_LOAD_PARSING | GDOME_LOAD_SUBSTITUTE_ENTITIES, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOMImplementation.createDocFromURI: parsing with substitution failed\n   test-document2.xml not found\n");
      return -1;
  }
  gdome_doc_unref(domdoc, &exc);
  dot();

  domdoc = gdome_di_createDocFromURI (domImpl, LOCALDIR"/test-loadsave1.xml", GDOME_LOAD_VALIDATING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOMImplementation.createDocFromURI: validating failed\n   test-loadsave1.xml not found\n");
      return -1;
  }
  gdome_doc_unref(domdoc, &exc);
  dot();

  domdoc = gdome_di_createDocFromMemory (domImpl, buffer1, GDOME_LOAD_PARSING, &exc);
  if (domdoc == NULL) {
		fprintf (stderr, "DOMImplementation.createDocFromMemory: parsing failed\n");
		return -1;
  }
	if (!gdome_di_saveDocToMemory (domImpl, domdoc, &mem, 0, &exc)) {
		fprintf (stderr, "DOMImplementation.saveDocToMemory: failed\n");
		return -1;
	}
  free (mem);
	if (!gdome_di_saveDocToMemoryEnc (domImpl, domdoc, &mem, "ISO-8859-1", 0, &exc)) {
		fprintf (stderr, "DOMImplementation.saveDocToMemory: failed\n");
		return -1;
  }
  free (mem);
  gdome_doc_unref(domdoc, &exc);
  dot();

  domdoc = gdome_di_createDocFromMemory (domImpl, buffer1, GDOME_LOAD_VALIDATING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOMImplementation.createDocFromMemory: validating failed\n");
      return -1;
  }
  gdome_doc_unref(domdoc, &exc);
  dot();

  domdoc = gdome_di_createDocFromURIWithEntitiesTable (domImpl, LOCALDIR"/test-loadsave2.xml", entityTable, GDOME_LOAD_PARSING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOMImplementation.createDocFromURIWithEntityTable: parsing failed\n   test-loadsave2.xml not found\n");
      return -1;
  }
  gdome_doc_unref(domdoc, &exc);
  dot();

  domdoc = gdome_di_createDocFromURIWithEntitiesTable (domImpl, LOCALDIR"/test-loadsave2.xml", entityTable, GDOME_LOAD_PARSING | GDOME_LOAD_SUBSTITUTE_ENTITIES, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOMImplementation.createDocFromURIWithEntityTable: parsing with substitution failed\n   test-loadsave2.xml not found\n");
      return -1;
  }
  gdome_doc_unref(domdoc, &exc);
  dot();

  domdoc = gdome_di_createDocFromURIWithEntitiesTable (domImpl, LOCALDIR"/test-loadsave1.xml", entityTable,  GDOME_LOAD_VALIDATING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOMImplementation.createDocFromURIWithEntityTable: validating failed\n   test-loadsave2.xml not found\n");
      return -1;
  }
  gdome_doc_unref(domdoc, &exc);
  dot();

  domdoc = gdome_di_createDocFromMemoryWithEntitiesTable (domImpl, buffer2, entityTable,  GDOME_LOAD_PARSING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOMImplementation.createDocFromMemoryWithEntityTable: parsing failed\n");
      return -1;
  }
  gdome_doc_unref(domdoc, &exc);
  dot();

  domdoc = gdome_di_createDocFromMemoryWithEntitiesTable (domImpl, buffer2, entityTable, GDOME_LOAD_VALIDATING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOMImplementation.createDocFromMemoryWithEntityTable: validating failed\n");
      return -1;
  }
  gdome_doc_unref(domdoc, &exc);
  dot();

  printf("test_loadsave\n");

  return 0;
}
