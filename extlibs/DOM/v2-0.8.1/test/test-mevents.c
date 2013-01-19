/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-mevents.c
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

#include <glib.h>
#include <stdio.h>
#include <string.h>
#include <gdome.h>
#include <gdome-events.h>

char prop[128];

void
signprop (const char *str)
{
  strcat (prop, str);
}

void
resetprop ()
{
  prop[0] = '\0';
}


void callback (GdomeEventListener *self,
               GdomeEvent *event,
               GdomeException *exc)
{
	GdomeNode *ct = gdome_mevnt_currentTarget ((GdomeMutationEvent *)event, exc);
  GdomeDOMString *attrName, *id;

  if (ct != NULL && gdome_n_nodeType (ct, exc) == GDOME_ELEMENT_NODE) {
    attrName = gdome_str_mkref ("ITEM");
    id = gdome_el_getAttribute ((GdomeElement *)ct, attrName, exc);
    gdome_str_unref (attrName);
    signprop (id->str);
    gdome_str_unref (id);
  }
  else
    signprop (" ");
  gdome_n_unref (ct, exc);
}

void
dot() {
	printf(".");
	fflush(stdout);
}

gboolean
test_mevents (GdomeDocument *doc)
{
  GdomeException exc;
  GdomeElement *els[6], *el1, *el2;
  GdomeEventListener *evl;
	GdomeDOMString *id, *evntType, *str, *attrValue;
  GdomeNode *result;
  GdomeAttr *attr;
  GdomeText *txt;
	gchar buff[2];
  gboolean ret = TRUE;
  int i;
  buff[1] = '\0';

  evl = gdome_evntl_mkref (callback, NULL);

  /* Get els refs */
  for (i=0; i<6 && ret; i++) {
    buff[0]='0'+i;
    id = gdome_str_mkref_dup (buff);
    els[i] = gdome_doc_getElementById (doc, id, &exc);
		if (els[i] == NULL)
			fprintf (stderr, "\nDocument.getElementById(\"%s\"), NULL", buff), ret = FALSE;
    gdome_str_unref (id);
	}
  dot();

  /* Setting capture listeners */
  evntType = gdome_str_mkref("DOMNodeInserted");
  for (i=0; i<6; i++)
    gdome_el_addEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMNodeInsertedIntoDocument");
  for (i=0; i<6; i++)
    gdome_el_addEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMNodeRemoved");
  for (i=0; i<6; i++)
    gdome_el_addEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMNodeRemovedFromDocument");
  for (i=0; i<6; i++)
    gdome_el_addEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMSubtreeModified");
  for (i=0; i<6; i++)
    gdome_el_addEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMAttrModified");
  for (i=0; i<6; i++)
    gdome_el_addEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMCharacterDataModified");
  for (i=0; i<6; i++)
    gdome_el_addEventListener (els[i], evntType, evl, TRUE, &exc);
	gdome_str_unref (evntType);
  dot();

  /* DOMCharacterDataModified Test 1 */
  txt = (GdomeText *)gdome_el_firstChild (els[3], &exc);
  str = gdome_str_mkref ("Changed Test String");
	gdome_t_set_data (txt, str, &exc);
	gdome_str_unref (str);
  gdome_t_unref (txt, &exc);
	if (strcmp (prop, "0123012"))
    fprintf (stderr, "\n DOMCharacterDataModified Test 1 , failed"), ret = FALSE;
	resetprop();
	dot();

	/* DOMNodeInserted Test 1 */
  str = gdome_str_mkref ("WOW");
	txt = gdome_doc_createTextNode (doc, str, &exc);
	result = gdome_el_appendChild (els[2], (GdomeNode *)txt, &exc);
  gdome_n_unref (result, &exc);
  gdome_t_unref (txt, &exc);
	gdome_str_unref (str);
	if (strcmp (prop, "01201201"))
		fprintf (stderr, "\n DOMNodeInserted Test 1 , failed"), ret = FALSE;
	resetprop();
	dot();

	/* DOMNodeInserted Test 2 */
  str = gdome_str_mkref ("WOW");
	txt = gdome_doc_createTextNode (doc, str, &exc);
	gdome_str_unref (str);
  str = gdome_str_mkref ("NEWEL");
  el1 = gdome_doc_createElement (doc, str, &exc);
	gdome_str_unref (str);
	result = gdome_el_appendChild (el1, (GdomeNode *)txt, &exc);
  gdome_n_unref (result, &exc);
  gdome_t_unref (txt, &exc);
	result = gdome_el_appendChild (els[1], (GdomeNode *)el1, &exc);
  gdome_n_unref (result, &exc);
  gdome_el_unref (el1, &exc);
	if (strcmp (prop, "0101010"))
		fprintf (stderr, "\n DOMNodeInserted Test 2 , failed"), ret = FALSE;
	resetprop();
	dot();

	/* DOMNodeInserted Test 3 */
  str = gdome_str_mkref ("WOW");
	txt = gdome_doc_createTextNode (doc, str, &exc);
	gdome_str_unref (str);
  str = gdome_str_mkref ("NEWEL");
  el1 = gdome_doc_createElement (doc, str, &exc);
  el2 = gdome_doc_createElement (doc, str, &exc);
	gdome_str_unref (str);
	result = gdome_el_appendChild (el2, (GdomeNode *)txt, &exc);
  gdome_n_unref (result, &exc);
  gdome_t_unref (txt, &exc);
	str = gdome_str_mkref ("ITEM");
	attrValue = gdome_str_mkref ("a");
	gdome_el_setAttribute (el1, str, attrValue, &exc);
  gdome_str_unref (attrValue);
	attrValue = gdome_str_mkref ("b");
	gdome_el_setAttribute (el2, str, attrValue, &exc);
	gdome_str_unref (attrValue);
	gdome_str_unref (str);
	result = gdome_el_appendChild (el1, (GdomeNode *)el2, &exc);
  gdome_n_unref (result, &exc);
  evntType = gdome_str_mkref("DOMNodeInserted");
	gdome_el_addEventListener (el1, evntType, evl, TRUE, &exc);
	gdome_el_addEventListener (el2, evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMNodeInsertedIntoDocument");
	gdome_el_addEventListener (el1, evntType, evl, TRUE, &exc);
	gdome_el_addEventListener (el2, evntType, evl, TRUE, &exc);
	gdome_str_unref (evntType);
	result = gdome_el_appendChild (els[3], (GdomeNode *)el1, &exc);
	gdome_n_unref (result, &exc);
  evntType = gdome_str_mkref("DOMNodeInserted");
	gdome_el_removeEventListener (el1, evntType, evl, TRUE, &exc);
	gdome_el_removeEventListener (el2, evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMNodeInsertedIntoDocument");
	gdome_el_removeEventListener (el1, evntType, evl, TRUE, &exc);
	gdome_el_removeEventListener (el2, evntType, evl, TRUE, &exc);
	gdome_str_unref (evntType);
  gdome_el_unref (el2, &exc);
	gdome_el_unref (el1, &exc);
	if (strcmp (prop, "012301230123a0123ab012"))
		fprintf (stderr, "\n DOMNodeInserted Test 3 , failed"), ret = FALSE;
	resetprop();
	dot();

	/* DOMNodeRemoved Test 1 */
	txt = (GdomeText *)gdome_el_lastChild (els[2], &exc);
	result = gdome_el_removeChild (els[2], (GdomeNode *)txt, &exc);
  gdome_n_unref (result, &exc);
  gdome_t_unref (txt, &exc);
	if (strcmp (prop, "01201201"))
		fprintf (stderr, "\n DOMNodeRemoved Test 1 , failed"), ret = FALSE;
	resetprop();
	dot();

	/* DOMNodeRemoved Test 2 */
  el1 = (GdomeElement *)gdome_el_lastChild (els[1], &exc);
	txt = (GdomeText *)gdome_el_lastChild (el1, &exc);
	result = gdome_el_removeChild (els[1], (GdomeNode *)el1, &exc);
  gdome_n_unref (result, &exc);
  gdome_t_unref (txt, &exc);
  gdome_el_unref (el1, &exc);
	if (strcmp (prop, "0101010"))
		fprintf (stderr, "\n DOMNodeRemoved Test 2 , failed"), ret = FALSE;
	resetprop();
	dot();

	/* DOMNodeRemoved Test 3 */
	el1 = (GdomeElement *)gdome_el_lastChild (els[3], &exc);
  el2 = (GdomeElement *)gdome_el_lastChild (el1, &exc);
  evntType = gdome_str_mkref("DOMNodeRemoved");
	gdome_el_addEventListener (el1, evntType, evl, TRUE, &exc);
	gdome_el_addEventListener (el2, evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMNodeRemovedFromDocument");
	gdome_el_addEventListener (el1, evntType, evl, TRUE, &exc);
	gdome_el_addEventListener (el2, evntType, evl, TRUE, &exc);
	gdome_str_unref (evntType);
	result = gdome_el_removeChild (els[3], (GdomeNode *)el1, &exc);
	gdome_n_unref (result, &exc);
	evntType = gdome_str_mkref("DOMNodeRemoved");
	gdome_el_removeEventListener (el1, evntType, evl, TRUE, &exc);
	gdome_el_removeEventListener (el2, evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMNodeRemovedFromDocument");
	gdome_el_removeEventListener (el1, evntType, evl, TRUE, &exc);
	gdome_el_removeEventListener (el2, evntType, evl, TRUE, &exc);
	gdome_str_unref (evntType);
  gdome_el_unref (el2, &exc);
	gdome_el_unref (el1, &exc);
	if (strcmp (prop, "012301230123a0123ab012"))
		fprintf (stderr, "\n DOMNodeRemoved Test 3 , failed"), ret = FALSE;
	resetprop();
  dot();

  /* DOMAttrModified ADDITION */
  str = gdome_str_mkref ("ATTR");
  attrValue = gdome_str_mkref ("attrValue");
  attr = gdome_doc_createAttribute (doc, str, &exc);
  gdome_a_set_value (attr, attrValue, &exc);
  gdome_str_unref (attrValue);
  gdome_str_unref (str);
  gdome_el_setAttributeNode (els[3], attr, &exc);
  if (strcmp (prop, "012012"))
    fprintf (stderr, "\n DOMAttrModified ADDITION , failed"), ret = FALSE;
  resetprop();
  dot();

  /* DOMAttrModified MODIFICATION */
  attrValue = gdome_str_mkref ("attrNewValue");
  gdome_a_set_value (attr, attrValue, &exc);
  gdome_str_unref (attrValue);
  gdome_a_unref (attr, &exc);
  if (strcmp (prop, "012012"))
    fprintf (stderr, "\n DOMAttrModified MODIFICATION , failed"), ret = FALSE;
  resetprop();
  dot();

  /* DOMAttrModified REMOVAL */
  str = gdome_str_mkref ("attr");
  gdome_el_removeAttribute (els[3], str, &exc);
  gdome_str_unref (str);
  str = gdome_str_mkref ("ATTR");
  gdome_el_removeAttribute (els[3], str, &exc);
  gdome_str_unref (str);
  if (strcmp (prop, "012012"))
    fprintf (stderr, "\n DOMAttrModified REMOVAL , failed"), ret = FALSE;
  resetprop();
  dot();

  /* Remove capture listeners */
  evntType = gdome_str_mkref("DOMNodeInserted");
  for (i=0; i<6; i++)
    gdome_el_removeEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMNodeInsertedIntoDocument");
  for (i=0; i<6; i++)
    gdome_el_removeEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMNodeRemoved");
  for (i=0; i<6; i++)
    gdome_el_removeEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMNodeRemovedFromDocument");
  for (i=0; i<6; i++)
    gdome_el_removeEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMSubtreeModified");
  for (i=0; i<6; i++)
    gdome_el_removeEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMAttrModified");
  for (i=0; i<6; i++)
    gdome_el_removeEventListener (els[i], evntType, evl, TRUE, &exc);
  gdome_str_unref (evntType);
  evntType = gdome_str_mkref("DOMCharacterDataModified");
  for (i=0; i<6; i++)
    gdome_el_removeEventListener (els[i], evntType, evl, TRUE, &exc);
	gdome_str_unref (evntType);
  dot();

  for (i=0; i<6; i++)
    gdome_el_unref (els[i], &exc);

  gdome_evntl_unref (evl, &exc);

  printf ("test_mevents\n");
	return ret;
}

int
main (int argc, char **argv)
{
	GdomeDOMImplementation *domImpl = NULL;
	GdomeDocument *domdoc = NULL;
	GdomeException exc = 0;
	gboolean ret = FALSE;

	domImpl = gdome_di_mkref();
	domdoc = gdome_di_createDocFromURI (domImpl, LOCALDIR"/test-mevents.xml", GDOME_LOAD_PARSING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOImplementation.createDocFromURI: failed\n   test-mevents.xml not found\n");
      return -1;
  }

  ret = test_mevents(domdoc);
  gdome_doc_unref(domdoc, &exc);

	if(ret)
		return 0;
	else
		return -1;
}
