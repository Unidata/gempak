/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-node.c
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

void
dot() {
	printf(".");
	fflush(stdout);
}

gboolean
test_node(GdomeNode *el) {
	GdomeNode *node1, *node2, *node3, *node4, *node5;
	GdomeNode *tnode, *cnode, *ttnode;
	GdomeNodeList *nl;
  GdomeNamedNodeMap *nnm;
	GdomeException exc;
  GdomeDOMString *str;
  gboolean ret = TRUE;
  int i;

	nl = gdome_n_childNodes(el, &exc);
  node1 = gdome_nl_item(nl, 1L, &exc);
  node2 = gdome_nl_item(nl, 3L, &exc);
	node3 = gdome_nl_item(nl, 5L, &exc);
	node4 = gdome_nl_item(nl, 7L, &exc);
  node5 = gdome_nl_item(nl, 9L, &exc);

  /*------------------- TEST ATTRIBUTES ------------------*/

	/* Test Node.attributes */
	nnm = gdome_n_attributes(node3, &exc);
	if (nnm == NULL)
		fprintf(stderr,"\nNode.attributes, NULL"), ret = FALSE;
	else if(gdome_nnm_length(nnm, &exc) != 2)
		fprintf(stderr,"\nNode.attributes, wrong length"), ret = FALSE;
	dot();

	/* Test Node.firstChild */
	tnode = gdome_n_firstChild(el, &exc);
  ttnode = gdome_nl_item(nl, 0L, &exc);
  if (tnode != ttnode)
		fprintf(stderr,"\nNode.firstChild, error"), ret = FALSE;
	gdome_n_unref (tnode, &exc);
  gdome_n_unref (ttnode, &exc);
  dot();

	/* Test Node.lastChild */
	tnode = gdome_n_lastChild(el, &exc);
  ttnode = gdome_nl_item(nl, gdome_nl_length(nl, &exc) - 1, &exc);
  if (tnode != ttnode)
		fprintf(stderr,"\nNode.lastChild, error"), ret = FALSE;
	gdome_n_unref (tnode, &exc);
  gdome_n_unref (ttnode, &exc);
	dot();

	/* Test Node.localName */
	str = gdome_n_localName(node2, &exc);
  if(strcmp(str->str,"NODE2"))
		fprintf(stderr,"\nNode.localName, wrong on NODE2"), ret = FALSE;
  gdome_str_unref(str);
	str = gdome_n_localName(node3, &exc);
  if(str != NULL)
		fprintf(stderr,"\nNode.localName, wrong on NODE3"), ret = FALSE;
  dot();

	/* Test Node.namespaceURI */
	str = gdome_n_namespaceURI(node2, &exc);
  if(strcmp(str->str,"urn:test.tst"))
		fprintf(stderr,"\nNode.namespaceURI, wrong on NODE2"), ret = FALSE;
  gdome_str_unref(str);
	str = gdome_n_namespaceURI(node3, &exc);
  if(str != NULL)
		fprintf(stderr,"\nNode.namespaceURI, wrong on NODE3"), ret = FALSE;
	dot();

	/* Test Node.nextSibling */
	tnode = gdome_n_nextSibling(node2, &exc);
  ttnode = gdome_nl_item(nl, 4L, &exc);
  if (tnode != ttnode)
		fprintf(stderr,"\nNode.nextSibling, error"), ret = FALSE;
	gdome_n_unref (tnode, &exc);
  gdome_n_unref (ttnode, &exc);
	dot();

	/* Test Node.nodeName */
	str = gdome_n_nodeName(node1, &exc);
  if(strcmp(str->str,"NODE1"))
		fprintf(stderr,"\nNode.nodeName, wrong on NODE1"), ret = FALSE;
  gdome_str_unref(str);
	str = gdome_n_nodeName(node2, &exc);
	if(strcmp(str->str,"tns:NODE2"))
		fprintf(stderr,"\nNode.nodeName, wrong on NODE2"), ret = FALSE;
	gdome_str_unref(str);
  tnode = gdome_n_firstChild(node1, &exc);
	str = gdome_n_nodeName(tnode, &exc);
	if(strcmp(str->str,"#text"))
		fprintf(stderr,"\nNode.nodeName, wrong on TNODE"), ret = FALSE;
	gdome_str_unref(str);
	gdome_n_unref(tnode, &exc);
  dot();

  /* Test Node.nodeType */
  if (gdome_n_nodeType(node2, &exc) != GDOME_ELEMENT_NODE)
		fprintf(stderr,"\nNode.nextType, error"), ret = FALSE;
	dot();

  /* Test Node.nodeValue */
  tnode = gdome_n_firstChild(node2, &exc);
	str = gdome_n_nodeValue(tnode, &exc);
	if(strcmp(str->str,"This is a node with a namespace"))
		fprintf(stderr,"\nNode.nodeValue, wrong on TNODE"), ret = FALSE;
	gdome_str_unref(str);
	gdome_n_unref(tnode, &exc);
	dot();

	/* Test Node.ownerDocument */
	tnode = (GdomeNode *)gdome_n_ownerDocument(node2, &exc);
  ttnode = (GdomeNode *)gdome_n_ownerDocument(node3, &exc);
	if (tnode != ttnode)
		fprintf(stderr,"\nNode.ownerDocument, error"), ret = FALSE;
	gdome_n_unref (tnode, &exc);
  gdome_n_unref (ttnode, &exc);
	dot();

	/* Test Node.parentNode */
  tnode = (GdomeNode *)gdome_n_parentNode(node2, &exc);
  if (tnode != el)
		fprintf(stderr,"\nNode.parentNode, error"), ret = FALSE;
  gdome_n_unref (tnode, &exc);
	dot();
  
	/* Test Node.prefix */
	str = gdome_n_prefix(node2, &exc);
  if(strcmp(str->str,"tns"))
		fprintf(stderr,"\nNode.prefix, wrong on NODE2"), ret = FALSE;
  gdome_str_unref(str);
	str = gdome_n_prefix(node3, &exc);
  if(str != NULL)
		fprintf(stderr,"\nNode.prefix, wrong on NODE3"), ret = FALSE;
	dot();

	/* Test Node.previousSibling */
	tnode = gdome_n_previousSibling(node2, &exc);
  ttnode = gdome_nl_item(nl, 2L, &exc);
  if (tnode != ttnode)
		fprintf(stderr,"\nNode.previousSibling, error"), ret = FALSE;
	gdome_n_unref (tnode, &exc);
  gdome_n_unref (ttnode, &exc);
	dot();

	/* Test Node::set_nodeValue */
	tnode = gdome_n_firstChild(node2, &exc);
	str = gdome_str_mkref ("xxxxx");
	gdome_n_set_nodeValue(tnode, str, &exc);
  gdome_str_unref(str);
	str = gdome_n_nodeValue(tnode, &exc);
	if(strcmp(str->str,"xxxxx"))
		fprintf(stderr,"\nNode.set_nodeValue, error"), ret = FALSE;
	gdome_str_unref(str);
	gdome_n_unref(tnode, &exc);

	str = gdome_str_mkref ("ATTR1");
	tnode = gdome_nnm_getNamedItem(nnm, str, &exc);
  gdome_str_unref(str);
	str = gdome_str_mkref ("Ciao");
	gdome_n_set_nodeValue(tnode, str, &exc);
  gdome_str_unref(str);
	str = gdome_n_nodeValue(tnode, &exc);
	if(strcmp(str->str,"Ciao"))
		fprintf(stderr,"\nNode.set_nodeValue, error"), ret = FALSE;
	gdome_str_unref(str);
	gdome_n_unref(tnode, &exc);
  gdome_nnm_unref (nnm, &exc);

	dot();

	/* Test Node::set_prefix */
	str = gdome_str_mkref ("xxx");
	gdome_n_set_prefix(node2, str, &exc);
  gdome_str_unref(str);
	str = gdome_n_prefix(node2, &exc);
	if(strcmp(str->str,"xxx"))
		fprintf(stderr,"\nNode.set_prefix, error"), ret = FALSE;
	gdome_str_unref(str);
	dot();

  /*------------------- TEST METHODS ------------------*/

  /* Test Node::hasChildNodes */
  if (!gdome_n_hasChildNodes(node1, &exc))
		fprintf(stderr,"\nNode.hasChildNodes(), error on NODE1"), ret = FALSE;
  if (gdome_n_hasChildNodes(node4, &exc))
		fprintf(stderr,"\nNode.hasChildNodes(), error on NODE4"), ret = FALSE;
  dot();

  /* Test Node::hasAttributes */
  if (!gdome_n_hasAttributes(node3, &exc))
		fprintf(stderr,"\nNode.hasAttributes(), error on NODE3"), ret = FALSE;
  if (gdome_n_hasAttributes(node1, &exc))
		fprintf(stderr,"\nNode.hasAttributes(), error on NODE1"), ret = FALSE;
  dot();

  /* Test Methods... reverse the order of the nodes */
	node1 = gdome_n_appendChild(el, node1, &exc);
  gdome_n_unref (node1, &exc);
	node5 = gdome_n_replaceChild(el, node4, node5, &exc);
  gdome_n_unref (node5, &exc);
	node2 = gdome_n_insertBefore(el, node2, node1, &exc);
  gdome_n_unref (node2, &exc);
	node3 = gdome_n_insertBefore(el, node3, node2, &exc);
  gdome_n_unref (node3, &exc);
  node4 = gdome_n_removeChild(el, node4, &exc);
  gdome_n_unref (node4, &exc);
	node4 = gdome_n_insertBefore(el, node4, node3, &exc);
  gdome_n_unref (node4, &exc);
	node5 = gdome_n_insertBefore(el, node5, node4, &exc);
  gdome_n_unref (node5, &exc);

  /* Add indentation */
	tnode = gdome_nl_item(nl, 0L, &exc);
  cnode = gdome_n_cloneNode(tnode, 0, &exc);
	cnode = gdome_n_insertBefore(el, cnode, node5, &exc);
  gdome_n_unref (cnode, &exc);
  gdome_n_unref (cnode, &exc);
  cnode = gdome_n_cloneNode(tnode, 0, &exc);
  gdome_n_insertBefore(el, cnode, node4, &exc);
  gdome_n_unref (cnode, &exc);
  gdome_n_unref (cnode, &exc);
  cnode = gdome_n_cloneNode(tnode, 0, &exc);
  gdome_n_insertBefore(el, cnode, node3, &exc);
  gdome_n_unref (cnode, &exc);
  gdome_n_unref (cnode, &exc);
  cnode = gdome_n_cloneNode(tnode, 0, &exc);
  gdome_n_insertBefore(el, cnode, node2, &exc);
  gdome_n_unref (cnode, &exc);
  gdome_n_unref (cnode, &exc);
  cnode = gdome_n_cloneNode(tnode, 0, &exc);
  gdome_n_insertBefore(el, cnode, node1, &exc);
  gdome_n_unref (cnode, &exc);
  gdome_n_unref (cnode, &exc);
  str = gdome_str_mkref("\n");
  gdome_n_set_nodeValue(tnode, str, &exc);
  gdome_str_unref (str);
	tnode = gdome_n_appendChild(el, tnode, &exc);
	gdome_n_unref (tnode, &exc);
	gdome_n_unref (tnode, &exc);

	/* delete old indentation */
	/* if nodelist is not live this doesn't work fine */
	for (i=0; i<5; i++) {
		tnode = gdome_nl_item(nl, 0L, &exc);
		gdome_n_removeChild(el, tnode, &exc);
		gdome_n_unref (tnode, &exc);
		gdome_n_unref (tnode, &exc);
	}
  gdome_nl_unref (nl, &exc);
	dot();

	gdome_n_unref (node1, &exc);
  gdome_n_unref (node2, &exc);
  gdome_n_unref (node3, &exc);
  gdome_n_unref (node4, &exc);
  gdome_n_unref (node5, &exc);

	fprintf(stderr,"test_node\n");

  return ret;
}

gboolean
test_df_node(GdomeNode *el) {
	GdomeDocumentFragment *df;
	GdomeDocument *doc;
	GdomeElement *el1, *el2, *el3, *tel, *root;
	GdomeComment *cmt1, *cmt2;
  GdomeText *txt1, *txt2, *txt3, *txt4, *txt5, *txt6;
	GdomeAttr *attr1,*attr2;
  GdomeNodeList *nl;
  GdomeDOMString *str;
  GdomeException exc;
  gboolean ret = TRUE;

	doc = gdome_n_ownerDocument (el, &exc);
	df = gdome_doc_createDocumentFragment (doc, &exc);
  dot();

  /* Creation of comment nodes */
  str = gdome_str_mkref("*** Start of the new nodes ***");
  cmt1 = gdome_doc_createComment(doc, str, &exc);
  gdome_str_unref (str);
  str = gdome_str_mkref("*** Stop of the new nodes ***");
  cmt2 = gdome_doc_createComment(doc, str, &exc);
  gdome_str_unref (str);
  dot();

  /* Creation of attribute nodes */
  str = gdome_str_mkref("DTATTR1");
  attr1 = gdome_doc_createAttribute(doc, str, &exc);
  gdome_str_unref (str);
  str = gdome_str_mkref("dtattr1");
  gdome_a_set_value(attr1, str, &exc);
  gdome_str_unref (str);
  str = gdome_str_mkref("DTATTR2");
  attr2 = gdome_doc_createAttribute(doc, str, &exc);
  gdome_str_unref (str);
  str = gdome_str_mkref("dtattr2");
  gdome_a_set_value(attr2, str, &exc);
  gdome_str_unref (str);
  dot();

  /* Creation of text nodes for indentation */
  str = gdome_str_mkref("\n  ");
  txt1 = gdome_doc_createTextNode(doc, str, &exc);
  txt2 = gdome_doc_createTextNode(doc, str, &exc);
  txt3 = gdome_doc_createTextNode(doc, str, &exc);
  txt4 = gdome_doc_createTextNode(doc, str, &exc);
  txt5 = gdome_doc_createTextNode(doc, str, &exc);
  gdome_str_unref (str);
  str = gdome_str_mkref("\n    ");
  txt6 = gdome_doc_createTextNode(doc, str, &exc);
  gdome_str_unref (str);
  gdome_doc_unref (doc, &exc);
  dot();

  /* Creation of element nodes */
  str = gdome_str_mkref("DTEL1");
  el1 = gdome_doc_createElement(doc, str, &exc);
  gdome_str_unref (str);
  gdome_el_setAttributeNode(el1, attr1, &exc);
  str = gdome_str_mkref("DTEL2");
	el2 = gdome_doc_createElement(doc, str, &exc);
  gdome_str_unref (str);
	gdome_el_setAttributeNode(el2, attr2, &exc);
  gdome_el_appendChild(el1, (GdomeNode *)txt6, &exc);
  gdome_t_unref (txt6, &exc);
  gdome_el_appendChild(el1, (GdomeNode *)el2, &exc);
  gdome_el_unref (el2, &exc);
  gdome_el_appendChild(el1, (GdomeNode *)txt5, &exc);
  gdome_t_unref (txt5, &exc);
  str = gdome_str_mkref("DTEL3");
	el3 = gdome_doc_createElement(doc, str, &exc);
  gdome_str_unref (str);
  dot();

	/* Add theese Nodes to the Document Fragment */
  gdome_df_appendChild(df, (GdomeNode *)cmt1, &exc);
  gdome_c_unref (cmt1, &exc);
  gdome_df_appendChild(df, (GdomeNode *)txt1, &exc);
  gdome_t_unref (txt1, &exc);
  gdome_df_appendChild(df, (GdomeNode *)el1, &exc);
  gdome_el_unref (el1, &exc);
  gdome_df_appendChild(df, (GdomeNode *)txt2, &exc);
  gdome_t_unref (txt2, &exc);
  gdome_df_appendChild(df, (GdomeNode *)el3, &exc);
  gdome_el_unref (el3, &exc);
  gdome_df_appendChild(df, (GdomeNode *)txt3, &exc);
  gdome_t_unref (txt3, &exc);
	gdome_df_appendChild(df, (GdomeNode *)cmt2, &exc);
  gdome_c_unref (cmt2, &exc);
  gdome_df_appendChild(df, (GdomeNode *)txt4, &exc);
  gdome_t_unref (txt4, &exc);
  dot();

  str = gdome_str_mkref("NODE3");
  nl = gdome_doc_getElementsByTagName(doc, str, &exc);
  gdome_str_unref (str);
  tel = (GdomeElement *)gdome_nl_item(nl, 0L, &exc);
  gdome_nl_unref (nl, &exc);
	root = (GdomeElement *)gdome_el_parentNode(tel, &exc);
  if (!gdome_el_insertBefore(root, (GdomeNode *)df, (GdomeNode *)tel, &exc))
    fprintf (stderr, "\nNode.insertBefore (DocumentFragment), failed"), ret = FALSE;
  gdome_df_unref (df, &exc);
  gdome_df_unref (df, &exc);
  gdome_el_unref (tel, &exc);
  gdome_el_unref (root, &exc);
  dot();

  gdome_c_unref (cmt1, &exc);
  gdome_c_unref (cmt2, &exc);
  gdome_a_unref (attr1, &exc);
  gdome_a_unref (attr2, &exc);
  gdome_t_unref (txt1, &exc);
  gdome_t_unref (txt2, &exc);
  gdome_t_unref (txt3, &exc);
  gdome_t_unref (txt4, &exc);
  gdome_t_unref (txt5, &exc);
  gdome_t_unref (txt6, &exc);
  gdome_el_unref (el1, &exc);
  gdome_el_unref (el2, &exc);
  gdome_el_unref (el3, &exc);

	fprintf(stderr,"test_df_node\n");

  return ret;
}

int
main (int argc, char **argv)
{
	GdomeDOMImplementation *domImpl = NULL;
  GdomeDocument *domdoc = NULL;
  GdomeElement *el;
	GdomeException exc = 0;
	gboolean ret;

	domImpl = gdome_di_mkref();
	domdoc = gdome_di_createDocFromURI(domImpl, LOCALDIR"/test-node.xml", GDOME_LOAD_PARSING, &exc);
  if (domdoc == NULL) {
      fprintf (stderr, "DOImplementation.createDocFromURI: failed\n   test-node.xml not found\n");
      return -1;
  }
  el = gdome_doc_documentElement (domdoc, &exc);
	ret = test_node((GdomeNode *)el);
	ret = test_df_node((GdomeNode *)el);
	gdome_di_saveDocToFile(domImpl, domdoc, LOCALDIR"/output.xml", GDOME_SAVE_STANDARD, &exc);
  gdome_el_unref (el, &exc);
  gdome_doc_unref (domdoc, &exc);

	if(ret)
		return 0;
	else
		return -1;
}
