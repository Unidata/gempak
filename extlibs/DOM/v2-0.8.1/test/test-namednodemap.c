/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-namednodemap.c
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

#define LBUFFER 256

#include <glib.h>
#include <stdio.h>
#include <gdome.h>

void dot() {
  printf(".");
  fflush(stdout);
}

gboolean
test_nnm (GdomeNamedNodeMap *nnm, GdomeElement *el) {
  GdomeException exc = 0;
  GdomeDOMString *str;
  GdomeNode *gnode;
	GdomeNode *a,*b,*c;

  gchar buffer[LBUFFER];
  long len=0;
  gint i;
  gboolean ret = TRUE;

  /* test inizial length */
  len = gdome_nnm_length (nnm, &exc);
	if(len != 27)
    fprintf(stderr,"\nNameNodeMap.length, wrong"), ret = FALSE;
  dot();

  /* test the content -readonly-*/
	/* test NamedNodeMap.getNamedItem */
  len = 27;
  for (i=3; i<=len && ret; i++) {
    g_snprintf(buffer,LBUFFER,"FOO%d",i-2);
    str = gdome_str_mkref(buffer);
    gnode = gdome_nnm_getNamedItem(nnm, str, &exc);
    if(gnode == NULL)
      fprintf(stderr,"\nNamedNodeMap.getNamedItem(\"%s\"), NULL",buffer), ret = FALSE;
    gdome_str_unref (str);
    gdome_n_unref (gnode, &exc);
  }
  dot();

  /* test NamedNodeMap.item */
  for (i=0; i<=len && ret; i++) {
    gnode = gdome_nnm_item(nnm, (unsigned long) i, &exc);
    if(gnode == NULL && i<len)
      fprintf(stderr,"\nNamedNodeMap.item(\"%d\"), NULL",i), ret = FALSE;
    else if(gnode != NULL && i>=len)
      fprintf(stderr,"\nNamedNodeMap.item(\"%d\"), NOT NULL",i), ret = FALSE;
    else
      gdome_n_unref (gnode, &exc);
  }
  dot();

  /* test NamedNodeMap.removeNamedItem - remove the odd-named element */
  for (i=3; i<=len && ret; i+=2) {
    g_snprintf(buffer,LBUFFER,"FOO%d",i-2);
    str = gdome_str_mkref(buffer);
    gnode = gdome_nnm_removeNamedItem(nnm, str, &exc);

    if(gnode == NULL)
      fprintf(stderr,"\nNamedNodeMap.removeNamedItem(\"%s\"), NULL",buffer), ret = FALSE;
    else
      gdome_n_unref (gnode, &exc);

    /* test if it has been really removed */
    gnode = gdome_nnm_getNamedItem(nnm, str, &exc);
    if(gnode != NULL)
      fprintf(stderr,"\nNamedNodeMap.getNamedItem(\"%s\"), NOT NULL",buffer), ret = FALSE;
    gdome_str_unref (str);
  }
  dot();

  /* test the post-remove length */
  len = gdome_nnm_length (nnm, &exc);
  if(len != 14)
    fprintf(stderr,"\nNameNodeMap.length, %lu wrong", len);
  dot();

  /* test if not odd-named are present*/
  len = 25;
  for (i=4; i<=len && ret; i+=2) {
    g_snprintf(buffer,LBUFFER,"FOO%d",i-2);
    str = gdome_str_mkref(buffer);
    gnode = gdome_nnm_getNamedItem(nnm, str, &exc);
    if(gnode == NULL)
      fprintf(stderr,"\nNamedNodeMap.getNamedItem(\"%s\"), NULL",buffer), ret = FALSE;
    else
      gdome_n_unref (gnode, &exc);
    gdome_str_unref (str);
  }
  dot();

  /* test to remove not present element */
  str = gdome_str_mkref("FOOBAR");
  gnode = gdome_nnm_removeNamedItem(nnm, str, &exc);
  if (gnode != NULL)
    fprintf(stderr,"\nNamedNodeMap.removeNamedItem(\"FOOBAR\"), NOT NULL"), ret = FALSE;
	gdome_str_unref (str);
  dot();

  /* test setNamedItem */
  str = gdome_str_mkref("FOO6");
  a = gdome_nnm_removeNamedItem(nnm, str, &exc);
	gnode = gdome_nnm_getNamedItem(nnm, str, &exc);
	if(gnode != NULL)
		fprintf(stderr,"\nNamedNodeMap.getNamedItem(\"%s\"), NOT NULL",str->str), ret = FALSE;
	gdome_str_unref (str);
  str = gdome_str_mkref("FOO8");
  b = gdome_nnm_removeNamedItem(nnm, str, &exc);
	gnode = gdome_nnm_getNamedItem(nnm, str, &exc);
	if(gnode != NULL)
		fprintf(stderr,"\nNamedNodeMap.getNamedItem(\"%s\"), NOT NULL",str->str), ret = FALSE;
	gdome_str_unref (str);
  str = gdome_str_mkref("FOO10");
	c = gdome_nnm_removeNamedItem(nnm, str, &exc);
	gnode = gdome_nnm_getNamedItem(nnm, str, &exc);
	if(gnode != NULL)
		fprintf(stderr,"\nNamedNodeMap.getNamedItem(\"%s\"), NOT NULL",str->str), ret = FALSE;
	gdome_str_unref (str);

  str = gdome_str_mkref("FOO6");
  gnode = gdome_nnm_setNamedItem(nnm, a, &exc);
	if(gnode != NULL)
		fprintf(stderr,"\nNamedNodeMap.setNamedItem(\"%s\"), NOT NULL",str->str), ret = FALSE;
	gnode = gdome_nnm_getNamedItem(nnm, str, &exc);
	if(gnode == NULL)
		fprintf(stderr,"\nNamedNodeMap.getNamedItem(\"%s\"), NULL",str->str), ret = FALSE;
  gdome_str_unref (str);
  gdome_n_unref (gnode, &exc);
  gdome_n_unref (a, &exc);
  str = gdome_str_mkref("FOO8");
  gnode = gdome_nnm_setNamedItem(nnm, b, &exc);
	if(gnode != NULL)
		fprintf(stderr,"\nNamedNodeMap.setNamedItem(\"%s\"), NOT NULL",str->str), ret = FALSE;
	gnode = gdome_nnm_getNamedItem(nnm, str, &exc);
	if(gnode == NULL)
		fprintf(stderr,"\nNamedNodeMap.getNamedItem(\"%s\"), NULL",str->str), ret = FALSE;
	gdome_str_unref (str);
  gdome_n_unref (gnode, &exc);
  gdome_n_unref (b, &exc);
  str = gdome_str_mkref("FOO10");
  gnode = gdome_nnm_setNamedItem(nnm, c, &exc);
	if(gnode != NULL)
		fprintf(stderr,"\nNamedNodeMap.setNamedItem(\"%s\"), NOT NULL",str->str), ret = FALSE;
	gnode = gdome_nnm_getNamedItem(nnm, str, &exc);
	if(gnode == NULL)
		fprintf(stderr,"\nNamedNodeMap.getNamedItem(\"%s\"), NULL",str->str), ret = FALSE;
	gdome_str_unref (str);
  gdome_n_unref (gnode, &exc);
  gdome_n_unref (c, &exc);
  dot();

	printf("test_namednodemap\n");

  return ret;
}

int
main (int argc, char **argv)
{
	GdomeDOMImplementation *domImpl = NULL;
  GdomeDocument *domdoc = NULL;
  GdomeElement *el = NULL;
  GdomeNamedNodeMap *nnm = NULL;
	GdomeException exc = 0;
	gboolean ret;

	domImpl = gdome_di_mkref();
  domdoc = gdome_di_createDocFromURI (domImpl, LOCALDIR"/test-namednodemap.xml", GDOME_LOAD_PARSING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOImplementation.createDocFromUri: failed\n   test-namednodemap.xml not found\n");
      return -1;
  }
  el = gdome_doc_documentElement (domdoc, &exc);
  if (el == NULL) {
      fprintf (stderr, "Document.documentElement: NULL\n");
      return -1;
  }
  nnm = gdome_el_attributes (el, &exc);
  if (el == NULL) {
      fprintf (stderr, "ELement.attributes: NULL\n");
      return -1;
  }

  ret = test_nnm(nnm, el);

  gdome_nnm_unref (nnm, &exc);
  gdome_el_unref (el, &exc);
  gdome_doc_unref (domdoc, &exc);


	if(ret)
		return 0;
	else
		return -1;
}

