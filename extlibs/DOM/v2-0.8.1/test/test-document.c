/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-document.c
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
test_document1 (GdomeDocument *doc) {
	GdomeElement *root, *el1, *el2;
	GdomeDocumentFragment *df;
	GdomeText *txt;
	GdomeCDATASection *cds;
	GdomeComment *cmt;
	GdomeProcessingInstruction *pi;
	GdomeAttr *attr1, *attr2;
	GdomeException exc = 0;
	GdomeDOMString *tstr1, *tstr2;
	GdomeDocument *tdoc;
	GdomeNode *ntmp;
	gboolean ret = TRUE;

	/* Test Documet.documentElement */
	root = gdome_doc_documentElement (doc, &exc);
	if (root == NULL)
		fprintf (stderr, "\nDocument.documentElement, NULL"), ret = FALSE;
	else {
		tstr1 = gdome_el_tagName (root, &exc);
		if (strcmp(tstr1->str, "TEST"))
			fprintf (stderr, "\nDocument.documentElement, wrong"), ret = FALSE;
		gdome_str_unref (tstr1);
		/*gdome_el_unref (root);*/
	}
	dot();

	/* Test Document.createElement */
	tstr1 = gdome_str_mkref ("EL1");
	el1 = gdome_doc_createElement (doc, tstr1, &exc);
	gdome_str_unref (tstr1);
	el1 = GDOME_EL(el1);
	if (el1 == NULL)
		fprintf (stderr, "\nDocument.createElement, NULL"), ret = FALSE;
	else if (gdome_el_nodeType (el1, &exc) != GDOME_ELEMENT_NODE)
		fprintf (stderr, "\nDocument.createElement, wrong nodeType"), ret = FALSE;
	else {
    tdoc = gdome_el_ownerDocument (el1, &exc);
		if (tdoc != doc)
			fprintf (stderr, "\nDocument.createElement, wrong ownerDocument"), ret = FALSE;
		else {
			gdome_doc_unref (tdoc, &exc);
      tstr1 = gdome_el_tagName (el1, &exc);
			if (strcmp(tstr1->str, "EL1"))
				fprintf (stderr, "\nDocument.createElement, wrong tagName"), ret = FALSE;
			gdome_str_unref(tstr1);
		}
	}
  dot();

	/* Test Document.createElementNS */
	tstr1 = gdome_str_mkref ("tns:EL2");
	tstr2 = gdome_str_mkref ("urn:test.tst");
	el2 = gdome_doc_createElementNS (doc, tstr2, tstr1, &exc);
	gdome_str_unref (tstr1);
	gdome_str_unref (tstr2);
	if (el2 == NULL)
		fprintf (stderr, "\nDocument.createElementNS, NULL"), ret = FALSE;
	else if (gdome_el_nodeType (el2, &exc) != GDOME_ELEMENT_NODE)
		fprintf (stderr, "\nDocument.createElement, wrong nodeType"), ret = FALSE;
	else {
		tdoc = gdome_el_ownerDocument (el2, &exc);
		if (tdoc != doc)
			fprintf (stderr, "\nDocument.createElementNS, wrong ownerElement"), ret = FALSE;
		else {
			gdome_doc_unref (tdoc, &exc);
			tstr1 = gdome_el_tagName (el2, &exc);
			if (strcmp(tstr1->str, "tns:EL2"))
				fprintf (stderr, "\nDocument.createElement, wrong tagName"), ret = FALSE;
			gdome_str_unref (tstr1);
			tstr1 = gdome_el_localName (el2, &exc);
			if (strcmp(tstr1->str, "EL2"))
				fprintf (stderr, "\nDocument.createElement, wrong localName"), ret = FALSE;
			gdome_str_unref (tstr1);
			tstr1 = gdome_el_namespaceURI (el2, &exc);
			if (strcmp(tstr1->str, "urn:test.tst"))
				fprintf (stderr, "\nDocument.createElement, wrong namespaceURI"), ret = FALSE;
			gdome_str_unref(tstr1);
		}
	}
	dot();

  /* Test Document.createDocumentFragment */
  df = gdome_doc_createDocumentFragment (doc, &exc);
	df = GDOME_DF(df);
	if (df == NULL)
		fprintf (stderr, "\nDocument.createDocumentFragment, NULL"), ret = FALSE;
	else if (gdome_df_nodeType (df, &exc) != GDOME_DOCUMENT_FRAGMENT_NODE)
		fprintf (stderr, "\nDocument.createDocumentFragment, wrong nodeType"), ret = FALSE;
	else {
		tdoc = gdome_df_ownerDocument (df, &exc);
		if (tdoc != doc)
			fprintf (stderr, "\nDocument.createDocumentFragment, wrong ownerDocument"), ret = FALSE;
		else {
			gdome_doc_unref (tdoc, &exc);
			tstr1 = gdome_df_nodeName (df, &exc);
			if (strcmp(tstr1->str, "#document-fragment"))
				fprintf (stderr, "\nDocument.createDocumentFragment, wrong nodeName"), ret = FALSE;
			gdome_str_unref (tstr1);
		}
	}

	gdome_df_unref (df, &exc);
	dot();

  /* Test Document.createTextNode */
	tstr1 = gdome_str_mkref ("<Test>Text Test</Test>");
	txt = gdome_doc_createTextNode (doc, tstr1, &exc);
	gdome_str_unref (tstr1);
/*	txt = GDOME_T(txt);*/
	if (txt == NULL)
		fprintf (stderr, "\nDocument.createTextNode, NULL"), ret = FALSE;
	else if (gdome_t_nodeType (txt, &exc) != GDOME_TEXT_NODE)
		fprintf (stderr, "\nDocument.createTextNode, wrong nodeType"), ret = FALSE;
	else {
		tdoc = gdome_t_ownerDocument (txt, &exc);
		if (tdoc != doc)
			fprintf (stderr, "\nDocument.createTextNode, wrong ownerDocument"), ret = FALSE;
		else {
			gdome_doc_unref (tdoc, &exc);
			tstr1 = gdome_t_nodeName (txt, &exc);
			if (strcmp(tstr1->str, "#text"))
				fprintf (stderr, "\nDocument.createTextNode, wrong nodeName"), ret = FALSE;
			gdome_str_unref (tstr1);
			tstr1 = gdome_t_nodeValue (txt, &exc);
			if (strcmp(tstr1->str, "<Test>Text Test</Test>"))
				fprintf (stderr, "\nDocument.createTextNode, wrong nodeValue"), ret = FALSE;
			gdome_str_unref(tstr1);
		}
	}
	dot();

  /* Test Document.createCDATASection */
	tstr1 = gdome_str_mkref ("<Test>Text Test</Test>");
	cds = gdome_doc_createCDATASection (doc, tstr1, &exc);
	gdome_str_unref (tstr1);
	cds = GDOME_CDS(cds);
	if (cds == NULL)
		fprintf (stderr, "\nDocument.createCDATASection, NULL"), ret = FALSE;
	else if (gdome_cds_nodeType (cds, &exc) != GDOME_CDATA_SECTION_NODE)
		fprintf (stderr, "\nDocument.createCDATASection, wrong nodeType"), ret = FALSE;
	else {
		tdoc = gdome_cds_ownerDocument (cds, &exc);
		if (tdoc != doc)
			fprintf (stderr, "\nDocument.createCDATASection, wrong ownerDocument"), ret = FALSE;
		else {
			gdome_doc_unref (tdoc, &exc);
			tstr1 = gdome_cds_nodeName (cds, &exc);
			if (strcmp(tstr1->str, "#cdata-section"))
				fprintf (stderr, "\nDocument.createCDATASection, wrong nodeName"), ret = FALSE;
			gdome_str_unref (tstr1);
			tstr1 = gdome_cds_nodeValue (cds, &exc);
			if (strcmp(tstr1->str, "<Test>Text Test</Test>"))
				fprintf (stderr, "\nDocument.createCDATASection, wrong nodeValue"), ret = FALSE;
			gdome_str_unref(tstr1);
		}
	}
	dot();

  /* Test Document.createComment */
	tstr1 = gdome_str_mkref ("dududu dadada");
	cmt = gdome_doc_createComment (doc, tstr1, &exc);
	gdome_str_unref (tstr1);
	cmt = GDOME_C(cmt);
	if (cmt == NULL)
		fprintf (stderr, "\nDocument.createComment, NULL"), ret = FALSE;
	else if (gdome_c_nodeType (cmt, &exc) != GDOME_COMMENT_NODE)
		fprintf (stderr, "\nDocument.createComment, wrong nodeType"), ret = FALSE;
	else {
		tdoc = gdome_c_ownerDocument (cmt, &exc);
		if (tdoc != doc)
			fprintf (stderr, "\nDocument.createComment, wrong ownerDocument"), ret = FALSE;
		else {
			gdome_doc_unref (tdoc, &exc);
			tstr1 = gdome_c_nodeName (cmt, &exc);
			if (strcmp(tstr1->str, "#comment"))
				fprintf (stderr, "\nDocument.createComment, wrong nodeName"), ret = FALSE;
			gdome_str_unref (tstr1);
			tstr1 = gdome_c_nodeValue (cmt, &exc);
			if (strcmp(tstr1->str, "dududu dadada"))
				fprintf (stderr, "\nDocument.createComment, wrong nodeValue"), ret = FALSE;
			gdome_str_unref(tstr1);
		}
	}
	dot();

  /* Test Document.createProcessingInstruction */
	tstr1 = gdome_str_mkref ("sqlprocessor");
	tstr2 = gdome_str_mkref ("SELECT * FROM blah");
	pi = gdome_doc_createProcessingInstruction (doc, tstr1, tstr2, &exc);
	gdome_str_unref (tstr1);
	gdome_str_unref (tstr2);
	pi = GDOME_PI(pi);
	if (pi == NULL)
		fprintf (stderr, "\nDocument.createProcessingInstruction, NULL"), ret = FALSE;
	else if (gdome_pi_nodeType (pi, &exc) != GDOME_PROCESSING_INSTRUCTION_NODE)
		fprintf (stderr, "\nDocument.createProcessingInstruction, wrong nodeType"), ret = FALSE;
	else {
		tdoc = gdome_pi_ownerDocument (pi, &exc);
		if (tdoc != doc)
			fprintf (stderr, "\nDocument.createProcessingInstruction, wrong ownerDocument"), ret = FALSE;
		else {
			gdome_doc_unref (tdoc, &exc);
			tstr1 = gdome_pi_nodeName (pi, &exc);
			if (strcmp(tstr1->str, "sqlprocessor"))
				fprintf (stderr, "\nDocument.createProcessingInstruction, wrong nodeName"), ret = FALSE;
			gdome_str_unref (tstr1);
			tstr1 = gdome_pi_nodeValue (pi, &exc);
			if (strcmp(tstr1->str, "SELECT * FROM blah"))
				fprintf (stderr, "\nDocument.createProcessingInstruction, wrong nodeValue"), ret = FALSE;
			gdome_str_unref(tstr1);
		}
	}
	dot();

  /* Test Document.createAttribute */
	tstr1 = gdome_str_mkref ("ATTR1");
	attr1 = gdome_doc_createAttribute (doc, tstr1, &exc);
	gdome_str_unref(tstr1);
	attr1 = GDOME_A(attr1);
	if (attr1 == NULL)
		fprintf (stderr, "\nDocument.createAttribute, NULL"), ret = FALSE;
	else if (gdome_a_nodeType (attr1, &exc) != GDOME_ATTRIBUTE_NODE)
		fprintf (stderr, "\nDocument.createAttribute, wrong nodeType"), ret = FALSE;
	else {
		tdoc = gdome_a_ownerDocument (attr1, &exc);
		if (tdoc != doc)
			fprintf (stderr, "\nDocument.createAttribute, wrong ownerDocument"), ret = FALSE;
		else {
			gdome_doc_unref (tdoc, &exc);
			tstr1 = gdome_a_name (attr1, &exc);
			if (strcmp(tstr1->str, "ATTR1"))
				fprintf (stderr, "\nDocument.createAttribute, wrong name"), ret = FALSE;
			gdome_str_unref (tstr1);
			tstr1 = gdome_a_value (attr1, &exc);
			if (strcmp(tstr1->str, ""))
				fprintf (stderr, "\nDocument.createAttribute, wrong value"), ret = FALSE;
			gdome_str_unref(tstr1);
		}
	}
	dot();

  /* Test Document.createAttributeNS */
	tstr1 = gdome_str_mkref ("tns:ATTR2");
	tstr2 = gdome_str_mkref ("urn:test.tst");
	attr2 = gdome_doc_createAttributeNS (doc, tstr2, tstr1, &exc);
	gdome_str_unref (tstr1);
	gdome_str_unref (tstr2);
	if (attr1 == NULL)
		fprintf (stderr, "\nDocument.createAttributeNS, NULL"), ret = FALSE;
	else if (gdome_a_nodeType (attr2, &exc) != GDOME_ATTRIBUTE_NODE)
		fprintf (stderr, "\nDocument.createAttributeNS, wrong nodeType"), ret = FALSE;
	else {
		tdoc = gdome_a_ownerDocument (attr2, &exc);
		if (tdoc != doc)
			fprintf (stderr, "\nDocument.createAttributeNS, wrong ownerDocument"), ret = FALSE;
		else {
			gdome_doc_unref (tdoc, &exc);
			tstr1 = gdome_a_name (attr2, &exc);
			if (strcmp(tstr1->str, "tns:ATTR2"))
				fprintf (stderr, "\nDocument.createAttributeNS, wrong name"), ret = FALSE;
			gdome_str_unref (tstr1);
			tstr1 = gdome_a_localName (attr2, &exc);
			if (strcmp(tstr1->str, "ATTR2"))
				fprintf (stderr, "\nDocument.createAttributeNS, wrong localName"), ret = FALSE;
			gdome_str_unref (tstr1);
			tstr1 = gdome_a_namespaceURI (attr2, &exc);
			if (strcmp(tstr1->str, "urn:test.tst"))
				fprintf (stderr, "\nDocument.createAttributeNS, wrong namespaceURI"), ret = FALSE;
			gdome_str_unref (tstr1);
			tstr1 = gdome_a_value (attr2, &exc);
			if (strcmp(tstr1->str, ""))
				fprintf (stderr, "\nDocument.createAttributeNS, wrong value"), ret = FALSE;
			gdome_str_unref(tstr1);
		}
	}
	dot();


	/* Now I build a simple tree with the nodes created */
	ntmp = (GdomeNode *)gdome_el_setAttributeNode (root, attr1, &exc);
	gdome_n_unref (ntmp, &exc);
	gdome_a_unref (attr1, &exc);
	ntmp = (GdomeNode *)gdome_el_setAttributeNode (el1, attr2, &exc);
	gdome_n_unref (ntmp, &exc);
  gdome_a_unref (attr2, &exc);
	ntmp = (GdomeNode *)gdome_el_appendChild (root,GDOME_N(el1), &exc);
	gdome_n_unref (ntmp, &exc);
  gdome_el_unref (el1, &exc);
	ntmp = (GdomeNode *)gdome_el_appendChild (root,GDOME_N(pi), &exc);
	gdome_n_unref (ntmp, &exc);
	gdome_pi_unref (pi, &exc);
	ntmp = (GdomeNode *)gdome_el_appendChild (el2,GDOME_N(txt), &exc);
	gdome_n_unref (ntmp, &exc);
	gdome_t_unref (txt, &exc);
	ntmp = (GdomeNode *)gdome_el_appendChild (el2,GDOME_N(cds), &exc);
	gdome_n_unref (ntmp, &exc);
	gdome_cds_unref (cds, &exc);
	ntmp = (GdomeNode *)gdome_el_appendChild (el2,GDOME_N(cmt), &exc);
	gdome_n_unref (ntmp, &exc);
	gdome_c_unref (cmt, &exc);
	ntmp = (GdomeNode *)gdome_el_appendChild (root,GDOME_N(el2), &exc);
	gdome_n_unref (ntmp, &exc);
	gdome_el_unref (el2, &exc);
	gdome_el_unref (root, &exc);
	dot();

	printf ("test_document1\n");
	return ret;
}

gboolean
test_document2 (GdomeDocument *doc) {
	GdomeElement *els[9], *root;
	GdomeNode *tel;
	GdomeNodeList *nl;
	GdomeException exc = 0;
	GdomeDOMString *tstr1;
	gboolean ret = TRUE;
	int i;

	/* Test Document.getElementById*/
	for(i=0; i<9; i++) {
		tstr1 = gdome_str_mkref_own(g_strdup_printf("%d",i));
		els[i] = gdome_doc_getElementById(doc, tstr1, &exc);
		if(els[i] == NULL)
			fprintf (stderr, "\nDocument.getElementById(\"%s\"), NULL",tstr1->str), ret = FALSE;
		gdome_str_unref(tstr1);
	}
	dot();

  /* Test Document.getElementByTagName */
	tstr1 = gdome_str_mkref("NODE");
	nl = gdome_doc_getElementsByTagName(doc, tstr1, &exc);
	if(nl == NULL)
		fprintf (stderr, "\nDocument.getElementByTagName(\"%s\"), NULL",tstr1->str), ret = FALSE;
	if(gdome_nl_length(nl, &exc) != 9)
		fprintf (stderr, "\nDocument.getElementByTagName(\"%s\"), wrong length",tstr1->str), ret = FALSE;
	for(i=0; i<9; i++) {
		tel = gdome_nl_item(nl, (unsigned long)i, &exc);
		if (tel != (GdomeNode *)els[i])
			fprintf (stderr, "\nDocument.getElementByTagName(\"%s\"), wrong %d-th item",tstr1->str,i), ret = FALSE;
		if (tel != NULL)
			gdome_n_unref (tel, &exc);
	}
	gdome_str_unref(tstr1);
	dot();

	/* Is NodeList live? */
	root = gdome_doc_documentElement (doc, &exc);
	tel = gdome_el_removeChild (root, (GdomeNode *)els[3], &exc);
	gdome_el_unref (els[5], &exc);
	gdome_el_unref (els[4], &exc);
	gdome_el_unref (els[3], &exc);
	gdome_n_unref (tel, &exc);
	gdome_el_unref (root, &exc);
	tel = gdome_nl_item(nl, 3L, &exc);
	if(tel != (GdomeNode *)els[6])
		fprintf (stderr, "\nNODELIST IS NOT LIVE!!"), ret = FALSE;
	gdome_n_unref (tel, &exc);
	gdome_nl_unref (nl, &exc);
	dot();

	gdome_el_unref (els[0], &exc);
	gdome_el_unref (els[1], &exc);
	gdome_el_unref (els[2], &exc);
	gdome_el_unref (els[6], &exc);
	gdome_el_unref (els[7], &exc);
	gdome_el_unref (els[8], &exc);

	printf ("test_document2\n");
	return ret;
}

gboolean
test_document3 (GdomeDocument *doc) {
	GdomeElement *els[4], *root;
	GdomeNode *tel;
	GdomeNodeList *nl;
	GdomeException exc = 0;
	GdomeDOMString *tstr1, *tstr2;
	gboolean ret = TRUE;
	int i;

	/* Test Document.getElementById*/
	for(i=1; i<9; i+=2) {
		tstr1 = gdome_str_mkref_own(g_strdup_printf("%d",i));
		els[i/2] = gdome_doc_getElementById(doc, tstr1, &exc);
		if(els[i/2] == NULL)
			fprintf (stderr, "\nDocument.getElementById(\"%s\"), NULL",tstr1->str), ret = FALSE;
		gdome_str_unref(tstr1);
	}
	dot();

  /* Test Document.getElementByTagNameNS */
	tstr1 = gdome_str_mkref("NODE");
	tstr2 = gdome_str_mkref("urn:test.tst");
	nl = gdome_doc_getElementsByTagNameNS(doc, tstr2, tstr1, &exc);
	if(nl == NULL)
		fprintf (stderr, "\nDocument.getElementByTagNameNS(\"%s\",\"%s\"), NULL",tstr2->str,tstr1->str), ret = FALSE;
	if(gdome_nl_length(nl, &exc) != 4)
		fprintf (stderr, "\nDocument.getElementByTagNameNS(\"%s\",\"%s\"), wrong length",tstr2->str,tstr1->str), ret = FALSE;
	for(i=0; i<4; i++) {
		tel = gdome_nl_item(nl, (unsigned long)i, &exc);
		if (tel != (GdomeNode *)els[i])
			fprintf (stderr, "\nDocument.getElementByTagName(\"%s\",\"%s\"), wrong %d-th item",tstr2->str,tstr1->str,i), ret = FALSE;
		if (tel != NULL)
			gdome_n_unref (tel, &exc);
	}
	gdome_str_unref(tstr1);
	gdome_str_unref(tstr2);
	dot();

	/* Is NodeList live? */
	root = gdome_doc_documentElement (doc, &exc);
  tel = gdome_el_removeChild (root, (GdomeNode *)els[1], &exc);
  gdome_el_unref (els[2], &exc);
  gdome_el_unref (els[1], &exc);
	gdome_n_unref (tel, &exc);
	gdome_el_unref (root, &exc);
	tel = gdome_nl_item(nl, 1L, &exc);
	if(tel != (GdomeNode *)els[3])
		fprintf (stderr, "\nNODELIST IS NOT LIVE!!"), ret = FALSE;
	gdome_n_unref (tel, &exc);
	gdome_nl_unref (nl, &exc);
	dot();

  gdome_el_unref (els[0], &exc);
  gdome_el_unref (els[3], &exc);

	printf ("test_document3\n");
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

	ret = test_document1(domdoc);
	gdome_di_saveDocToFile(domImpl, domdoc, LOCALDIR"/test-document1.xml", GDOME_SAVE_STANDARD, &exc);
	gdome_doc_unref(domdoc, &exc);

	domdoc = gdome_di_createDocFromURI(domImpl, LOCALDIR"/test-document2.xml", GDOME_LOAD_PARSING, &exc);
	if (domdoc == NULL) {
		fprintf (stderr, "DOImplementation.createDocFromURI: failed\n   test-document2.xml not found\n");
		return -1;
	}
	ret = test_document2(domdoc);
	gdome_doc_unref (domdoc, &exc);

	domdoc = gdome_di_createDocFromURI(domImpl, LOCALDIR"/test-document3.xml", GDOME_LOAD_PARSING, &exc);
	if (domdoc == NULL) {
		fprintf (stderr, "DOImplementation.createDocFromURI: failed\n   test-document3.xml not found\n");
		return -1;
	}
	ret = test_document3(domdoc);
	gdome_doc_unref (domdoc, &exc);

	if(ret)
		return 0;
	else
		return -1;
}
