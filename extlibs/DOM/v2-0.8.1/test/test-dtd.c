/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-dtd.c
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
#include <string.h>
#include <gdome.h>

void
dot() {
	printf(".");
	fflush(stdout);
}

gboolean
test_dtd (GdomeDocument *doc)
{
	GdomeDocumentType *dt;
  GdomeNamedNodeMap *ents, *nots;
  GdomeNodeList *nl;
  GdomeNode *node;
  GdomeDOMString *str;
	GdomeException exc;
	GdomeEntity *ent;
  GdomeNotation *not;
	gboolean ret = TRUE;

	dt = gdome_doc_doctype (doc, &exc);

  /* Test DocumentType.childNodes */
  nl = gdome_dt_childNodes (dt, &exc);
	if (nl == NULL)
		fprintf (stderr, "\nDocumentType.childNodes, NULL"), ret = FALSE;
  if (gdome_nl_length (nl, &exc) != 0L)
		fprintf (stderr, "\nDocumentType.childNodes, not empty"), ret = FALSE;
  if ((node = gdome_nl_item (nl, 0L, &exc)) != NULL)
		fprintf (stderr, "\nDocumentType.childNodes, not empty"), ret = FALSE;
  gdome_nl_unref (nl, &exc);
  dot();

	/* Test DocumentType.name */
	str = gdome_dt_name (dt, &exc);
	if (str == NULL)
		fprintf (stderr, "\nDocumentType.name, NULL"), ret = FALSE;
  else if (ret && strcmp (str->str, "TEST-DTD"))
		fprintf (stderr, "\nDocumentType.name, wrong"), ret = FALSE;
  if (ret) gdome_str_unref (str);
  dot();

  /* Test DocumentType.publicId */
	str = gdome_dt_publicId (dt, &exc);
	if (str != NULL) {
		fprintf (stderr, "\nDocumentType.publicId, wrong"), ret = FALSE;
		gdome_str_unref (str);
	}
  dot();

	/* Test DocumentType.systemId */
	str = gdome_dt_systemId (dt, &exc);
	if (str == NULL)
		fprintf (stderr, "\nDocumentType.systemId, NULL"), ret = FALSE;
  else if (ret && strcmp (str->str, "test-dtd.dtd"))
		fprintf (stderr, "\nDocumentType.systemId, wrong"), ret = FALSE;
  if (ret) gdome_str_unref (str);
	dot();

	/* Test DocumentType.internalSubset */
	str = gdome_dt_internalSubset (dt, &exc);
	if (str == NULL)
		fprintf (stderr, "\nDocumentType.internalSubset, NULL"), ret = FALSE;
  else if (ret && strcmp (str->str, "<!DOCTYPE TEST-DTD SYSTEM \"test-dtd.dtd\">"))
    fprintf (stderr, "\nDocumentType.internalSubset, wrong"), ret = FALSE;
  if (ret) gdome_str_unref (str);
	dot();

  /* Test DocumentType.entities */
	ents = gdome_dt_entities (dt, &exc);
  if (ents == NULL)
		fprintf (stderr, "\nDocumentType.entities, NULL"), ret = FALSE;
  else if (ret && gdome_nnm_length (ents, &exc) != 5)
		fprintf (stderr, "\nDocumentType.entities, wrong length"), ret = FALSE;
  dot();

	/* Test DocumentType.notations */
	nots = gdome_dt_notations (dt, &exc);
  if (ents == NULL)
		fprintf (stderr, "\nDocumentType.notations, NULL"), ret = FALSE;
  else if (ret && gdome_nnm_length (ents, &exc) != 5)
		fprintf (stderr, "\nDocumentType.notations, wrong length"), ret = FALSE;
  dot();

	printf ("test_dtd\n");

  /* Test Parsed Entity */
  str = gdome_str_mkref ("FOO1");
	ent = (GdomeEntity *)gdome_nnm_getNamedItem (ents, str, &exc);
  gdome_str_unref (str);
	if (ent == NULL)
		fprintf (stderr, "\nNamedNodeMap.getNamedItem(\"FOO1\"), NULL"), ret = FALSE;
  str = gdome_ent_nodeName (ent, &exc);
  if (ret && strcmp (str->str, "FOO1"))
		fprintf (stderr, "\nEntity.nodeName(), (parsed) FOO1 wrong nodeName"), ret = FALSE;
  gdome_str_unref (str);
  if (ret && gdome_ent_publicId (ent, &exc) != NULL)
		fprintf (stderr, "\nEntity.publicId(), (parsed) FOO1 NOT NULL"), ret = FALSE;
  if (ret && gdome_ent_systemId (ent, &exc) != NULL)
		fprintf (stderr, "\nEntity.systemId(), (parsed) FOO1 NOT NULL"), ret = FALSE;
  if (ret && gdome_ent_notationName (ent, &exc) != NULL)
		fprintf (stderr, "\nEntity.notationName(), (parsed) FOO1 NOT NULL"), ret = FALSE;
	if (ret)
		gdome_ent_unref (ent, &exc);
  dot();


  /* Test Unparsed Entity */
  str = gdome_str_mkref ("FOO2");
	ent = (GdomeEntity *)gdome_nnm_getNamedItem (ents, str, &exc);
  gdome_str_unref (str);
	if (ent == NULL)
		fprintf (stderr, "\nNamedNodeMap.getNamedItem(\"FOO2\"), NULL"), ret = FALSE;
  str = gdome_ent_nodeName (ent, &exc);
  if (ret && strcmp (str->str, "FOO2"))
		fprintf (stderr, "\nEntity.nodeName(), (unparsed) FOO2 wrong"), ret = FALSE;
  gdome_str_unref (str);
  if (ret && gdome_ent_publicId (ent, &exc) != NULL)
		fprintf (stderr, "\nEntity.publicId(), (unparsed) NOT NULL"), ret = FALSE;
  str = gdome_ent_systemId (ent, &exc);
  if (ret && strcmp (str->str, "file.type2"))
		fprintf (stderr, "\nEntity.systemId(), (unparsed) FOO2 wrong"), ret = FALSE;
	gdome_str_unref (str);
  str =gdome_ent_notationName (ent, &exc);
  if (ret && strcmp (str->str, "type2"))
		fprintf (stderr, "\nEntity.notationName(), (unparsed) FOO2 wrong"), ret = FALSE;
  gdome_str_unref (str);
	if (ret)
		gdome_ent_unref (ent, &exc);
  dot();

	printf ("test_entity\n");

  /* Test Notation */
  str = gdome_str_mkref ("type1");
  not = (GdomeNotation *)gdome_nnm_getNamedItem (nots, str, &exc);
  gdome_str_unref (str);
	if (not == NULL)
		fprintf (stderr, "\nNamedNodeMap.getNamedItem(\"type1\"), NULL"), ret = FALSE;
  str = gdome_not_nodeName (not, &exc);
  if (ret && strcmp (str->str, "type1"))
		fprintf (stderr, "\nNotation.nodeName(), wrong nodeName"), ret = FALSE;
  gdome_str_unref (str);
	if (ret && gdome_not_publicId (not, &exc) != NULL)
		fprintf (stderr, "\nNotation.publicId(), NOT NULL"), ret = FALSE;
  str = gdome_not_systemId (not, &exc);
  if (ret && strcmp (str->str, "program1"))
		fprintf (stderr, "\nNotation.systemId(), wrong"), ret = FALSE;
  gdome_str_unref (str);
  /* Test Notation.childNodes */
  nl = gdome_not_childNodes (not, &exc);
	if (nl == NULL)
		fprintf (stderr, "\nNotation.childNodes, NULL"), ret = FALSE;
  if (gdome_nl_length (nl, &exc) != 0L)
		fprintf (stderr, "\nNotation.childNodes, not empty"), ret = FALSE;
  if ((node = gdome_nl_item (nl, 0L, &exc)) != NULL)
		fprintf (stderr, "\nNotation.childNodes, not empty"), ret = FALSE;
  gdome_nl_unref (nl, &exc);
	if (ret)
		gdome_not_unref (not, &exc);
	dot();

  printf ("test_notation\n");

  gdome_nnm_unref (ents, &exc);
  gdome_nnm_unref (nots, &exc);
  gdome_dt_unref (dt, &exc);

  return ret;
}

int
main (int argc, char **argv)
{
	GdomeDOMImplementation *domImpl = NULL;
	GdomeDocument *domdoc = NULL;
	GdomeException exc = 0;
	gboolean ret;

  domImpl = gdome_di_mkref();

	domdoc = gdome_di_createDocFromURI (domImpl, LOCALDIR"/test-dtd.xml", GDOME_LOAD_VALIDATING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOImplementation.createDocFromURI: failed\n   test-dtd.xml not found\n");
      return -1;
  }
  ret = test_dtd(domdoc);
  gdome_doc_unref(domdoc, &exc);

  if(ret)
		return 0;
	else
		return -1;
}
