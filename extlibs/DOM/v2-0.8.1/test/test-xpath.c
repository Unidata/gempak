/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-xpath.c
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

#define LBUFFER 256

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gdome.h>
#include <gdome-xpath.h>

void dot() {
  printf(".");
  fflush(stdout);
}

gboolean
test_xpath (GdomeDocument *doc) {
  GdomeException exc = 0;
  GdomeDOMString *str, *temp_str;
  GdomeNode *gnode;
  GdomeXPathEvaluator *eval = NULL;
  GdomeXPathResult *res = NULL;
	GdomeElement *ns_el = NULL;
	GdomeXPathNSResolver *nsresolv = NULL;
	unsigned short result_type;
	double number_value;
	GdomeDOMString *string_value;
	GdomeBoolean boolean_value;

  gboolean ret = TRUE;

  eval = gdome_xpeval_mkref();
  str = gdome_str_mkref("//p");
  res = gdome_xpeval_evaluate(eval, str, (GdomeNode *)doc, NULL, 0, NULL, &exc);

  gnode = gdome_xpresult_singleNodeValue(res, &exc);
  temp_str = gdome_el_tagName((GdomeElement *)gnode, &exc);
  if (strcmp (temp_str->str, "p"))
    fprintf(stderr,"\n1st xpath result tagName, %s != p",temp_str->str), ret = FALSE;
  gdome_n_unref(gnode, &exc);
  gdome_str_unref(str);
	gdome_str_unref(temp_str);
  dot();

  gnode = gdome_xpresult_iterateNext(res, &exc);
  temp_str = gdome_el_tagName((GdomeElement *)gnode, &exc);
  if (strcmp (temp_str->str, "p"))
    fprintf(stderr,"\n1st xpath result tagName, %s != p",temp_str->str), ret = FALSE;
  gdome_n_unref(gnode, &exc);
	gdome_str_unref(temp_str);
  dot();

  gnode = gdome_xpresult_iterateNext(res, &exc);
  temp_str = gdome_el_tagName((GdomeElement *)gnode, &exc);
  if (strcmp (temp_str->str, "p"))
    fprintf(stderr,"\n2nd xpath result tagName, %s != p",temp_str->str), ret = FALSE;
  gdome_n_unref(gnode, &exc);
	gdome_str_unref(temp_str);
  dot();

  gnode = gdome_xpresult_iterateNext(res, &exc);
  if (gnode != NULL)
    fprintf(stderr,"\n3nd xpath result received, none expected"), ret = FALSE;
  gdome_n_unref(gnode, &exc);
  dot();

	/* Test XPath expression that doesn't return anything */
  str = gdome_str_mkref("//q");
  res = gdome_xpeval_evaluate(eval, str, (GdomeNode *)doc, NULL, 0, NULL, &exc);
  gnode = gdome_xpresult_singleNodeValue(res, &exc);
	if (gnode != NULL)
    fprintf(stderr,"\n1st xpath result != NULL"), ret = FALSE;
  gdome_str_unref(str);
  dot();

  gnode = gdome_xpresult_iterateNext(res, &exc);
	if (gnode != NULL)
    fprintf(stderr,"\n1st xpath result != NULL"), ret = FALSE;
  dot();
	gdome_xpresult_unref(res, &exc);

	ns_el = gdome_doc_documentElement(doc, &exc);
	nsresolv = gdome_xpeval_createNSResolver(eval, (GdomeNode *) ns_el, &exc);
 	str = gdome_str_mkref("//foo:bar/*");
  res = gdome_xpeval_evaluate(eval, str, (GdomeNode *)doc, nsresolv, 0, NULL, &exc);

	result_type = gdome_xpresult_resultType(res, &exc);
	if (result_type != GDOME_ORDERED_NODE_ITERATOR_TYPE)
		fprintf(stderr,"\nResult type not node set, got %d",result_type), ret = FALSE;
	dot();

  gnode = gdome_xpresult_iterateNext(res, &exc);
  temp_str = gdome_el_tagName((GdomeElement *)gnode, &exc);
  if (strcmp (temp_str->str, "foo:a1"))
    fprintf(stderr,"\n1st xpath result tagName, %s != foo:a1",temp_str->str), ret = FALSE;
  gdome_n_unref(gnode, &exc);
  gdome_el_unref(ns_el, &exc);
	gdome_str_unref(temp_str);
	gdome_str_unref(str);
	gdome_xpnsresolv_unref(nsresolv, &exc);
  dot();

  gnode = gdome_xpresult_iterateNext(res, &exc);
  temp_str = gdome_el_tagName((GdomeElement *)gnode, &exc);
  if (strcmp (temp_str->str, "foo:a2"))
    fprintf(stderr,"\n2nd xpath result tagName, %s != foo:a2",temp_str->str), ret = FALSE;
  gdome_n_unref(gnode, &exc);
	gdome_str_unref(temp_str);
	gdome_xpresult_unref(res, &exc);
  dot();

	str = gdome_str_mkref("count(//p)");
  res = gdome_xpeval_evaluate(eval, str, (GdomeNode *)doc, NULL, 0, NULL, &exc);
	result_type = gdome_xpresult_resultType(res, &exc);
	if (result_type != GDOME_NUMBER_TYPE)
		fprintf(stderr,"\nResult type not number, got %d",result_type), ret = FALSE;
	dot();

	number_value = gdome_xpresult_numberValue(res, &exc);
	if (number_value != 2)
		fprintf(stderr,"\nResult set count != 1, got %f",number_value), ret = FALSE;
	dot();

	gdome_xpresult_unref(res, &exc);
	gdome_str_unref(str);

	str = gdome_str_mkref("concat('abc','def')");
  res = gdome_xpeval_evaluate(eval, str, (GdomeNode *)doc, NULL, 0, NULL, &exc);
	result_type = gdome_xpresult_resultType(res, &exc);
	if (result_type != GDOME_STRING_TYPE)
		fprintf(stderr,"\nResult type not string, got %d",result_type), ret = FALSE;
	dot();

	string_value = gdome_xpresult_stringValue(res, &exc);
	temp_str = gdome_str_mkref("abcdef");
	if(!gdome_str_equal(string_value, temp_str))
		fprintf(stderr,"\nResult mismatch, %s != %s",string_value->str, temp_str->str), ret = FALSE;
	dot();

	gdome_str_unref(str);
	gdome_str_unref(temp_str);
	gdome_str_unref(string_value);
	gdome_xpresult_unref(res, &exc);

	str = gdome_str_mkref("true()");
  res = gdome_xpeval_evaluate(eval, str, (GdomeNode *)doc, NULL, 0, NULL, &exc);
	result_type = gdome_xpresult_resultType(res, &exc);
	if (result_type != GDOME_BOOLEAN_TYPE)
		fprintf(stderr,"\nResult type not boolean type, got %d",result_type), ret = FALSE;
	dot();

	boolean_value = gdome_xpresult_booleanValue(res, &exc);
	if(boolean_value != 1)
		fprintf(stderr,"\nBoolean value != 1, = %d",boolean_value), ret = FALSE;
	dot();

	gdome_str_unref(str);
	gdome_xpresult_unref(res, &exc);

	/* test GdomeXPathNamespace interface */
	str = gdome_str_mkref("//namespace::*");
	res = gdome_xpeval_evaluate(eval, str, (GdomeNode *)doc, NULL, 0, NULL, &exc);

  gnode = gdome_xpresult_iterateNext(res, &exc);

	/* nodeType */
	result_type = gdome_xpns_nodeType((GdomeXPathNamespace *)gnode, &exc);
	if (result_type != GDOME_XPATH_NAMESPACE_NODE)
		fprintf(stderr, "\nresult_type != namespace node, = %d", result_type), ret = FALSE;

	/* prefix */
	temp_str = gdome_xpns_prefix((GdomeXPathNamespace *)gnode, &exc);
  if (strcmp (temp_str->str, "xml"))
    fprintf(stderr,"\nnamespace prefix, %s != xml",temp_str->str), ret = FALSE;
	gdome_str_unref(temp_str);

	/* nodeName */
	temp_str = gdome_xpns_nodeName((GdomeXPathNamespace *)gnode, &exc);
  if (strcmp (temp_str->str, "xml"))
    fprintf(stderr,"\nnamespace nodeName, %s != xml",temp_str->str), ret = FALSE;
	gdome_str_unref(temp_str);

	/* namespaceURI */
	temp_str = gdome_xpns_namespaceURI((GdomeXPathNamespace *)gnode, &exc);
  if (strcmp (temp_str->str, "http://www.w3.org/XML/1998/namespace"))
    fprintf(stderr,"\nnamespace URI, %s != http://www.w3.org/XML/1998/namespace",temp_str->str), ret = FALSE;
	gdome_str_unref(temp_str);

	gdome_n_unref(gnode, &exc);

  gnode = gdome_xpresult_iterateNext(res, &exc);

	/* nodeType */
	result_type = gdome_xpns_nodeType((GdomeXPathNamespace *)gnode, &exc);
	if (result_type != GDOME_XPATH_NAMESPACE_NODE)
		fprintf(stderr, "\nresult_type != namespace node, = %d", result_type), ret = FALSE;

	/* prefix */
	temp_str = gdome_xpns_prefix((GdomeXPathNamespace *)gnode, &exc);
  if (strcmp (temp_str->str, "a"))
    fprintf(stderr,"\nnamespace prefix, %s != a",temp_str->str), ret = FALSE;
	gdome_str_unref(temp_str);

	/* nodeName */
	temp_str = gdome_xpns_nodeName((GdomeXPathNamespace *)gnode, &exc);
  if (strcmp (temp_str->str, "a"))
    fprintf(stderr,"\nnamespace nodeName, %s != a",temp_str->str), ret = FALSE;
	gdome_str_unref(temp_str);

	/* namespaceURI */
	temp_str = gdome_xpns_namespaceURI((GdomeXPathNamespace *)gnode, &exc);
  if (strcmp (temp_str->str, "http://www.w3.org"))
    fprintf(stderr,"\nnamespace URI, %s != http://www.w3.org",temp_str->str), ret = FALSE;
	gdome_str_unref(temp_str);

	gdome_n_unref(gnode, &exc);

	gdome_str_unref(str);
	gdome_xpresult_unref(res, &exc);
	gdome_xpeval_unref(eval, &exc);
	dot();

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
  domdoc = gdome_di_createDocFromURI (domImpl, LOCALDIR"/test-xpath.xml", GDOME_LOAD_PARSING, &exc);
  if (domdoc == NULL) {
    fprintf (stderr, "DOMImplementation.createDocFromUri: failed\n   test-xpath.xml not found\n");
    return -1;
  }
  el = gdome_doc_documentElement (domdoc, &exc);
  if (el == NULL) {
    fprintf (stderr, "Document.documentElement: NULL\n");
    return -1;
  }

	ret = test_xpath(domdoc);

  gdome_el_unref (el, &exc);
  gdome_doc_unref (domdoc, &exc);

  if(ret)
    return 0;
  else
    return -1;
}
