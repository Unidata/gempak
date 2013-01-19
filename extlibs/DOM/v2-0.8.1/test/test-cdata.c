/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-cdata.c
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
test_cdata1(GdomeDocument *doc) {
  GdomeText *txt, *txt1;
  GdomeNode *node;
	GdomeException exc;
  GdomeDOMString *str;
  GdomeNodeList *nl;
  gboolean ret = TRUE;

  str = gdome_str_mkref ("Initial String");
  txt = gdome_doc_createTextNode (doc, str, &exc);
  gdome_str_unref (str);

  /* Test Text.childNodes */
  nl = gdome_t_childNodes (txt, &exc);
	if (nl == NULL)
		fprintf (stderr, "\nText.childNodes, NULL"), ret = FALSE;
  if (gdome_nl_length (nl, &exc) != 0L)
		fprintf (stderr, "\nText.childNodes, not empty"), ret = FALSE;
  if ((node = gdome_nl_item (nl, 0L, &exc)) != NULL)
		fprintf (stderr, "\nText.childNodes, not empty"), ret = FALSE;
  gdome_nl_unref (nl, &exc);
  dot();

  /* Test Text.length */
	if (gdome_t_length (txt, &exc) != 14)
		fprintf (stderr, "\nText.length, wrong"), ret = FALSE;
  dot();

  /* Test Text.data */
	str = gdome_t_data (txt, &exc);
  if (strcmp(str->str, "Initial String"))
		fprintf (stderr, "\nText.data, wrong"), ret = FALSE;
  gdome_str_unref (str);
  dot();

	/* Test Text.set_data */
  str = gdome_str_mkref ("0123456789");
  gdome_t_set_data (txt, str, &exc);
  gdome_str_unref (str);
  str = gdome_t_data (txt, &exc);
	if (strcmp(str->str, "0123456789"))
		fprintf (stderr, "\nText.set_data, wrong"), ret = FALSE;
  gdome_str_unref (str);
  dot();

	/* Test Text.substringData() */
	str = gdome_t_substringData (txt, 3L, 3L, &exc);
  if (strcmp (str->str, "345"))
		fprintf (stderr, "\nText.substringData(3,3), wrong"), ret = FALSE;
  gdome_str_unref (str);
	str = gdome_t_substringData (txt, 4L, 10L, &exc);
  if (strcmp (str->str, "456789"))
		fprintf (stderr, "\nText.substringData(4,10), wrong"), ret = FALSE;
  gdome_str_unref(str);
	dot();

  /* Test Text.appendData() */
	str = gdome_str_mkref ("ABCDEF");
  gdome_t_appendData (txt, str, &exc);
  gdome_str_unref (str);
  str = gdome_t_data (txt, &exc);
	if (strcmp(str->str, "0123456789ABCDEF"))
		fprintf (stderr, "\nText.appendData, wrong"), ret = FALSE;
  gdome_str_unref (str);
	dot();

  /* Test Text.insertData() */
	str = gdome_str_mkref ("X");
  gdome_t_insertData (txt, 3L, str, &exc);
  gdome_str_unref (str);
  str = gdome_t_data (txt, &exc);
	if (strcmp(str->str, "012X3456789ABCDEF"))
		fprintf (stderr, "\nText.insertData, wrong"), ret = FALSE;
  gdome_str_unref (str);
	dot();

  /* Test Text.deleteData() */
  gdome_t_deleteData (txt, 3L, 1L, &exc);
  str = gdome_t_data (txt, &exc);
	if (strcmp(str->str, "0123456789ABCDEF"))
    fprintf (stderr, "\nText.deleteData(3,1), wrong"), ret = FALSE;
  gdome_str_unref (str);
  gdome_t_deleteData (txt, 10L, 8L, &exc);
  str = gdome_t_data (txt, &exc);
	if (strcmp(str->str, "0123456789"))
    fprintf (stderr, "\nText.deleteData(10,8), wrong"), ret = FALSE;
  gdome_str_unref (str);
	dot();

  /* Test Text.replaceData() */
	str = gdome_str_mkref ("ABC");
  gdome_t_replaceData (txt, 0L, 3L, str, &exc);
  gdome_str_unref (str);
  str = gdome_t_data (txt, &exc);
	if (strcmp(str->str, "ABC3456789"))
		fprintf (stderr, "\nText.replaceData(0,3,\"ABC\"), wrong"), ret = FALSE;
	gdome_str_unref (str);
	str = gdome_str_mkref ("XXXXXX");
  gdome_t_replaceData (txt, 4L, 3L, str, &exc);
  gdome_str_unref (str);
  str = gdome_t_data (txt, &exc);
	if (strcmp(str->str, "ABC3XXXXXX789"))
		fprintf (stderr, "\nText.replaceData(4,3,\"XXXXXX\"), wrong"), ret = FALSE;
	gdome_str_unref(str);
	str = gdome_str_mkref ("XABCDEF");
  gdome_t_replaceData (txt, 12L, 1L, str, &exc);
  gdome_str_unref (str);
  str = gdome_t_data (txt, &exc);
	if (strcmp(str->str, "ABC3XXXXXX78XABCDEF"))
		fprintf (stderr, "\nText.replaceData(12,1,\"XABCDEF\"), wrong"), ret = FALSE;
	gdome_str_unref (str);
	dot();

	printf ("test_cdata1\n");

	/* Test Text.splitText */
  txt1 = gdome_t_splitText (txt, 10L, &exc);
  str = gdome_t_data (txt, &exc);
	if (strcmp(str->str, "ABC3XXXXXX"))
    fprintf (stderr, "\nText.splitText(10), wrong"), ret = FALSE;
  gdome_str_unref (str);
  str = gdome_t_data (txt1, &exc);
	if (strcmp(str->str, "78XABCDEF"))
    fprintf (stderr, "\nText1.splitText(10), wrong"), ret = FALSE;
  gdome_str_unref (str);
  node = gdome_t_nextSibling (txt, &exc);
  if (node != (GdomeNode *)txt1)
    fprintf (stderr, "\nText.splitText(10), wrong next"), ret = FALSE;
  gdome_n_unref (node, &exc);
  node = gdome_t_previousSibling (txt1, &exc);
  if (node != (GdomeNode *)txt)
		fprintf (stderr, "\nText1.splitText(10), wrong previous"), ret = FALSE;
  gdome_n_unref (node, &exc);
  dot();

	printf ("test_text1\n");

  gdome_t_unref (txt, &exc);
  gdome_t_unref (txt1, &exc);
	return ret;
}

int
main (int argc, char **argv)
{
	GdomeDOMImplementation *domImpl = NULL;
	GdomeDocument *domdoc = NULL;
	GdomeDOMString *str = NULL;
	GdomeException exc = 0;
	gboolean ret;

	domImpl = gdome_di_mkref();
	str = gdome_str_mkref ("TEST");
  domdoc = gdome_di_createDocument (domImpl, NULL, str, NULL, &exc);
  gdome_str_unref (str);

	ret = test_cdata1(domdoc);
  gdome_doc_unref(domdoc, &exc);

	if(ret)
		return 0;
	else
		return -1;
}
