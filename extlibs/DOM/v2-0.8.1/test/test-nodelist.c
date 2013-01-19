/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-nodelist.c
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

void
dot() {
  printf(".");
  fflush(stdout);
}

gboolean
test_nl(GdomeElement *el) {
  GdomeNodeList *nl;

	GdomeDOMString *name;
	GdomeDOMString *URI;
	GdomeException exc;
	GdomeNode *node, *childs[7], *tmp;
	unsigned long lun, i;
  gboolean ret = TRUE;

	/* test NodeList.length */
	name = gdome_str_mkref("NODE");
	nl = gdome_el_getElementsByTagName(el, name, &exc);
	lun = gdome_nl_length(nl, &exc);
	if(lun != 19)
		fprintf(stderr,"\nNodeList.length(NULL,\"%s\"), wrong",name->str), ret = FALSE;
  gdome_str_unref (name);
	gdome_nl_unref(nl, &exc);
	name = gdome_str_mkref("*");
	nl = gdome_el_getElementsByTagName(el, name, &exc);
  lun = gdome_nl_length(nl, &exc);
	if(lun != 35)
    fprintf(stderr,"\nNodeList.length(NULL,\"%s\"), wrong",name->str), ret = FALSE;
  gdome_str_unref (name);
	gdome_nl_unref(nl, &exc);
	name = gdome_str_mkref("OTHER");
	nl = gdome_el_getElementsByTagName(el, name, &exc);
  lun = gdome_nl_length(nl, &exc);
	if(lun != 16)
    fprintf(stderr,"\nNodeList.length(NULL,\"%s\"), wrong",name->str), ret = FALSE;
  gdome_str_unref (name);
	gdome_nl_unref(nl, &exc);
	name = gdome_str_mkref("NODE");
	URI = gdome_str_mkref("urn:cips.ciak.uk");
	nl = gdome_el_getElementsByTagNameNS(el, URI, name, &exc);
  lun = gdome_nl_length(nl, &exc);
	if(lun != 9)
    fprintf(stderr,"\nNodeList.length(\"%s\",\"%s\"), wrong",URI->str,name->str), ret = FALSE;
  gdome_str_unref (name);
  gdome_str_unref (URI);
	gdome_nl_unref(nl, &exc);
	name = gdome_str_mkref("*");
	URI = gdome_str_mkref("urn:cips.ciak.uk");
	nl = gdome_el_getElementsByTagNameNS(el, URI, name, &exc);
  lun = gdome_nl_length(nl, &exc);
	if(lun != 9)
    fprintf(stderr,"\nNodeList.length(\"%s\",\"%s\"), wrong",URI->str,name->str), ret = FALSE;
  gdome_str_unref (name);
  gdome_str_unref (URI);
	gdome_nl_unref(nl, &exc);
	name = gdome_str_mkref("NODE");
	URI = gdome_str_mkref("*");
	nl = gdome_el_getElementsByTagNameNS(el, URI, name, &exc);
  lun = gdome_nl_length(nl, &exc);
	if(lun != 19)
    fprintf(stderr,"\nNodeList.length(\"%s\",\"%s\"), wrong",URI->str,name->str), ret = FALSE;
  gdome_str_unref (URI);
  gdome_str_unref (name);
  gdome_nl_unref(nl, &exc);
  dot();

	/* test Nodelist::item */
	name = gdome_str_mkref("NODE");
	nl = gdome_el_getElementsByTagName(el, name, &exc);
	for(i=0; i<19 && ret;i++) {
		node = gdome_nl_item(nl, i, &exc);
    if(node == NULL)
			fprintf(stderr,"\nNodeList.item(%lu), NULL",i), ret = FALSE;
    else
			gdome_n_unref(node,&exc);
	}
	gdome_nl_unref(nl, &exc);
  gdome_str_unref (name);
	name = gdome_str_mkref("NODE");
	URI = gdome_str_mkref("urn:cips.ciak.uk");
	nl = gdome_el_getElementsByTagNameNS(el, URI, name, &exc);
	for(i=0; i<9 && ret;i++) {
		node = gdome_nl_item(nl, i, &exc);
    if(node == NULL)
			fprintf(stderr,"\nNodeList.item(%lu) NS, NULL",i), ret = FALSE;
    else
			gdome_n_unref(node,&exc);
	}
	gdome_nl_unref(nl, &exc);
  gdome_str_unref (name);
  gdome_str_unref (URI);
	dot();

	/* test Nodelist in GdomeNode.childNodes*/
	name = gdome_str_mkref("NODE");
	nl = gdome_el_getElementsByTagName(el, name, &exc);
	node = gdome_nl_item(nl, 9L, &exc);
	gdome_nl_unref(nl, &exc);
  gdome_str_unref (name);
	nl = gdome_n_childNodes(node, &exc);
	lun = gdome_nl_length(nl, &exc);
	if (lun != 7)
		fprintf(stderr,"\nNodeList.length() ChildNodes, wrong"), ret = FALSE;
	for(i=0; i<lun && ret ;i++) {
		childs[i] = gdome_nl_item(nl, i, &exc);
		if (childs[i] == NULL)
      fprintf(stderr,"\nNodeList.item(%lu) ChildNodes, NULL", i), ret = FALSE;
  }
  dot();

  /* Test Nodelist Liveness */
	for (i=0; i<lun ; i++) {
		tmp = gdome_n_removeChild (node, childs[i], &exc);
		gdome_n_unref (tmp, &exc);
	}
  if (gdome_nl_length (nl, &exc) != 0)
		fprintf(stderr,"\nNodeList NOT LIVE"), ret = FALSE;
	for (i=0; i<7 && ret; i++) {
		tmp = gdome_n_appendChild (node, childs[i], &exc);
		gdome_n_unref (tmp, &exc);
    gdome_n_unref (childs[i], &exc);
		if (gdome_nl_length (nl, &exc) != i+1)
			fprintf(stderr,"\nNodeList NOT LIVE"), ret = FALSE;
  }
  gdome_nl_unref (nl, &exc);
  gdome_n_unref (node, &exc);
  dot();

	printf("test_nodelist\n");

  return ret;
}

int
main (int argc, char **argv)
{
	GdomeDOMImplementation *domImpl = NULL;
  GdomeDocument *domdoc = NULL;
  GdomeElement *el = NULL;
	GdomeException exc = 0;
	gboolean ret;

	domImpl = gdome_di_mkref();
  domdoc = gdome_di_createDocFromURI(domImpl, LOCALDIR"/test-nodelist.xml", GDOME_LOAD_PARSING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOImplementation.createDocFromURI: failed\n   test-nodelist.xml not found\n");
      return -1;
  }
  el = gdome_doc_documentElement (domdoc, &exc);
  if (el == NULL) {
      fprintf (stderr, "Document.documentElement: failed\n");
      return -1;
  }
  ret = test_nl(el);
  gdome_el_unref (el, &exc);
  gdome_doc_unref(domdoc, &exc);


	if(ret)
		return 0;
	else
		return -1;
}
