/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-treegc.c
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
#include <string.h>
#include <gdome.h>
#include <gdome-treegc.h>

void
dot() {
	printf(".");
	fflush(stdout);
}

/**
 * test_treegc1
 * @domImpl  The DOM implementation
 *
 * Returns TRUE if gdome_n_removeChild works fine with w.r.t. treegc.
 */
gboolean
test_treegc1(GdomeDOMImplementation *domImpl)
{
  GdomeDocument *doc;
	GdomeNodeList *nl = NULL;
  GdomeNode *node1 = NULL, *node2 = NULL;
  GdomeAttr *attr = NULL;
  GdomeText *text = NULL;
  GdomeDOMString *name = NULL;
  GdomeException exc;
  gboolean ret = TRUE;

  doc = gdome_di_createDocFromURI(domImpl, LOCALDIR"/test-treegc.xml", GDOME_LOAD_PARSING, &exc);
	if (doc == NULL)
    fprintf (stderr, "DOImplementation.createDocFromURI: failed\n   test-treegc.xml not found\n"), ret = FALSE;
  else {
    dot();

    name = gdome_str_mkref ("TOTEST");
    nl = gdome_doc_getElementsByTagName (doc, name, &exc);
    gdome_str_unref (name);
		node1 = gdome_nl_item (nl, 0L, &exc);
    gdome_nl_unref (nl, &exc);
    if (gdome_treegc_livenodes (node1) != 2)
      fprintf (stderr, "\nLivenodes check 1: Failed"), ret = FALSE;
    else {
      dot();

      node2 = gdome_n_firstChild (node1, &exc);
      if (gdome_treegc_livenodes (node2) != 3)
        fprintf (stderr, "\nLivenodes check 2: Failed"), ret = FALSE;
      else {
        dot();

        name = gdome_str_mkref ("ATTR");
        attr = gdome_el_getAttributeNode ((GdomeElement *)node2, name, &exc);
        gdome_str_unref (name);
        if (gdome_treegc_livenodes ((GdomeNode *)attr) != 4)
          fprintf (stderr, "\nLivenodes check 3: Failed"), ret = FALSE;
        else {
          dot();

          text = (GdomeText *)gdome_a_firstChild (attr, &exc);
          if (gdome_treegc_livenodes ((GdomeNode *)text) != 5)
            fprintf (stderr, "\nLivenodes check 4: Failed"), ret = FALSE;
          else {
            dot();

            node2 = gdome_n_removeChild (node1, node2, &exc);
            if (gdome_treegc_livenodes ((GdomeNode *)doc) != 5)
              fprintf (stderr, "\nLivenodes check 5: Failed"), ret = FALSE;
            else {
              dot();

              gdome_n_unref (node1, &exc);
              if (gdome_treegc_livenodes ((GdomeNode *)doc) != 4)
                fprintf (stderr, "\nLivenodes check 6: Failed"), ret = FALSE;
              else {
                dot();

                gdome_n_unref (node2, &exc);
                if (gdome_treegc_livenodes ((GdomeNode *)doc) != 4)
									fprintf (stderr, "\nLivenodes check 7: Failed"), ret = FALSE;
								else {
                  dot();

									gdome_n_unref (node2, &exc);
                  if (gdome_treegc_livenodes ((GdomeNode *)doc) != 1)
										fprintf (stderr, "\nLivenodes check 8: Failed"), ret = FALSE;
									else {
										dot();

										gdome_a_unref (attr, &exc);
										if (gdome_treegc_livenodes ((GdomeNode *)doc) != 1)
											fprintf (stderr, "\nLivenodes check 9: Failed"), ret = FALSE;
										else {
											dot();

											gdome_t_unref (text, &exc);
											if (gdome_treegc_livenodes ((GdomeNode *)doc) != 1)
												fprintf (stderr, "\nLivenodes check 10: Failed"), ret = FALSE;
											else {
												dot();

												gdome_doc_unref (doc, &exc);
											}
										}
									}
								}
              }
            }
          }
        }
      }
    }
  }

  printf ("test_treegc1\n");

  return ret;
}

/**
 * test_treegc2
 * @domImpl  The DOM implementation
 *
 * Returns TRUE if gdome_n_cloneNode and gdome_n_appendChild works fine with
 * w.r.t. treegc.
 */
gboolean
test_treegc2(GdomeDOMImplementation *domImpl)
{
  GdomeDocument *doc;
	GdomeNodeList *nl = NULL;
  GdomeNode *node1 = NULL, *node2 = NULL, *node3 = NULL, *node4 = NULL;
  GdomeAttr *attr = NULL;
  GdomeText *text = NULL;
  GdomeDOMString *name = NULL;
  GdomeException exc;
  gboolean ret = TRUE;

  doc = gdome_di_createDocFromURI(domImpl, LOCALDIR"/test-treegc.xml", GDOME_LOAD_PARSING, &exc);
	if (doc == NULL)
    fprintf (stderr, "DOImplementation.createDocFromURI: failed\n   test-treegc.xml not found\n"), ret = FALSE;
  else {
    dot();

    name = gdome_str_mkref ("TOTEST");
    nl = gdome_doc_getElementsByTagName (doc, name, &exc);
    gdome_str_unref (name);
		node1 = gdome_nl_item (nl, 0L, &exc);
    gdome_nl_unref (nl, &exc);
    if (gdome_treegc_livenodes (node1) != 2)
      fprintf (stderr, "\nLivenodes check 1: Failed"), ret = FALSE;
    else {
      dot();

      node2 = gdome_n_firstChild (node1, &exc);
      if (gdome_treegc_livenodes (node2) != 3)
        fprintf (stderr, "\nLivenodes check 2: Failed"), ret = FALSE;
      else {
        dot();

        name = gdome_str_mkref ("ATTR");
        attr = gdome_el_getAttributeNode ((GdomeElement *)node2, name, &exc);
        gdome_str_unref (name);
        if (gdome_treegc_livenodes ((GdomeNode *)attr) != 4)
          fprintf (stderr, "\nLivenodes check 3: Failed"), ret = FALSE;
        else {
          dot();

          text = (GdomeText *)gdome_a_firstChild (attr, &exc);
          if (gdome_treegc_livenodes ((GdomeNode *)text) != 5)
            fprintf (stderr, "\nLivenodes check 4: Failed"), ret = FALSE;
          else {
            dot();

            node3 = gdome_n_cloneNode (node2, TRUE, &exc);
            if (gdome_treegc_livenodes ((GdomeNode *)doc) != 6)
              fprintf (stderr, "\nLivenodes check 5: Failed"), ret = FALSE;
            else {
              dot();

              node3 = gdome_n_appendChild (node1, node3, &exc);
              node4 = gdome_n_cloneNode (node2, TRUE, &exc);
              if (gdome_treegc_livenodes ((GdomeNode *)doc) != 7)
                fprintf (stderr, "\nLivenodes check 6: Failed"), ret = FALSE;
              else {
                dot();

                node4 = gdome_n_insertBefore (node1, node4, node3, &exc);
                if (gdome_treegc_livenodes ((GdomeNode *)doc) != 7)
                  fprintf (stderr, "\nLivenodes check 7: Failed"), ret = FALSE;
                else {
                  dot();

                  gdome_n_unref (node4, &exc);
                  gdome_n_unref (node4, &exc);
                  gdome_n_unref (node3, &exc);
                  gdome_n_unref (node3, &exc);
                  gdome_n_unref (node2, &exc);
                  gdome_a_unref (attr, &exc);
                  gdome_t_unref (text, &exc);
                  gdome_n_unref (node1, &exc);
                  if (gdome_treegc_livenodes ((GdomeNode *)doc) != 1)
                    fprintf (stderr, "\nLivenodes check 8 %d: Failed",gdome_treegc_livenodes ((GdomeNode *)doc)), ret = FALSE;
                  else {
                    dot();

                    gdome_doc_unref (doc, &exc);
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  printf ("test_treegc2\n");

  return ret;
}

int
main (int argc, char **argv)
{
	GdomeDOMImplementation *domImpl = NULL;
  GdomeDocument *doc = NULL;
  GdomeDocumentType *dt = NULL;
  GdomeDOMString *dtname, *pID, *sID, *name;
  GdomeElement *root;
	GdomeException exc = 0;
  gboolean ret = FALSE;

  domImpl = gdome_di_mkref();

  /* Test for DocumentType Node not owned by a Document */
  dtname = gdome_str_mkref ("TEST");
  pID = gdome_str_mkref ("http://www.w3c.org/test.dtd");
  sID = gdome_str_mkref ("test.dtd");
  dt = gdome_di_createDocumentType (domImpl, dtname, pID, sID, &exc);
  gdome_str_unref (dtname);
  gdome_str_unref (pID);
  gdome_str_unref (sID);
  gdome_dt_unref (dt, &exc);

  /* Test for simple Document creation */
  name = gdome_str_mkref ("TEST");
	doc = gdome_di_createDocument(domImpl, NULL, name, NULL, &exc);
  gdome_str_unref (name);
	root = gdome_doc_documentElement (doc, &exc);
	gdome_doc_unref (doc, &exc);
  gdome_el_unref (root, &exc);

  /* Test for Document creation with a DocumentType Node */
  dtname = gdome_str_mkref ("TEST");
  pID = gdome_str_mkref ("http://www.w3c.org/test.dtd");
  sID = gdome_str_mkref ("test.dtd");
  dt = gdome_di_createDocumentType (domImpl, dtname, pID, sID, &exc);
  gdome_str_unref (dtname);
  gdome_str_unref (pID);
  gdome_str_unref (sID);
  name = gdome_str_mkref ("TEST");
	doc = gdome_di_createDocument(domImpl, NULL, name, NULL, &exc);
  gdome_str_unref (name);
	root = gdome_doc_documentElement (doc, &exc);
	gdome_doc_unref (doc, &exc);
  gdome_el_unref (root, &exc);
  gdome_dt_unref (dt, &exc);

  ret = test_treegc1(domImpl);
  ret = test_treegc2(domImpl);

	if(ret)
		return 0;
	else
		return -1;
}
