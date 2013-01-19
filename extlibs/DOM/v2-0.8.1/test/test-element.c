/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* test-element.c
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

#include <libxml/tree.h>

void dot() {
	printf(".");
	fflush(stdout);
}

gboolean test_el(GdomeElement *el) {
	GdomeNamedNodeMap *nnm;
	GdomeAttr *attr;
	GdomeAttr *attrNS;
	GdomeAttr *attrDef;
	GdomeAttr *attr_temp;

	GdomeDOMString *attr_name;
	GdomeDOMString *attr_value;
	GdomeDOMString *attr_URI;
	GdomeDOMString *temp_str;
	GdomeDOMString *cmp_str;
	GdomeException exc;
	gboolean ret = TRUE;

  /* test Element::tagName */
	temp_str = gdome_el_tagName(el, &exc);
	cmp_str = gdome_str_mkref("TEST");
	if(!gdome_str_equal(temp_str, cmp_str))
		fprintf(stderr,"\nElement.tagName(), wrong"), ret = FALSE;
	gdome_str_unref (temp_str);
	gdome_str_unref (cmp_str);
	dot();

	/* test Element::attributes */
	nnm = gdome_el_attributes(el, &exc);
	if(nnm == NULL)
		fprintf(stderr,"\nElement.attributes, NULL"), ret = FALSE;
	else
		gdome_nnm_unref(nnm, &exc);
	dot();

  /* test Element::getAttributeNode */
	attr_name = gdome_str_mkref("XXXX");
	attr = gdome_el_getAttributeNode(el, attr_name, &exc);
	if(attr != NULL)
		fprintf(stderr,"\nElement.getAttributeNode(\"%s\"), NOT NULL",attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	attr_name = gdome_str_mkref("FOO1");
	attr = gdome_el_getAttributeNode(el, attr_name, &exc);
	if(attr == NULL)
		fprintf(stderr,"\nElement.getAttributeNode(\"%s\"), NULL",attr_name->str), ret = FALSE;
	else {
		cmp_str = gdome_a_nodeName(attr, &exc);
		if(ret && !gdome_str_equal(cmp_str, attr_name))
			fprintf(stderr,"\nElement.getAttributeNode(\"%s\"), wrong",attr_name->str), ret = FALSE;
	}
	gdome_str_unref (attr_name);
	gdome_str_unref (cmp_str);
	dot();

	/* test Element::getAttributeNodeNS */
	attr_name = gdome_str_mkref("CIPPO2");
	attr_URI = gdome_str_mkref("urn:xxxx.xxxx.xx");
	attrNS = gdome_el_getAttributeNodeNS(el, attr_URI, attr_name, &exc);
	if(attrNS != NULL)
		fprintf(stderr,"\nElement.getAttributeNodeNS(\"%s\",\"%s\"), NOT NULL",attr_URI->str,attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	attr_name = gdome_str_mkref("XXXXX");
	attr_URI = gdome_str_mkref("urn:cips.ciak.uk");
	attrNS = gdome_el_getAttributeNodeNS(el, attr_URI, attr_name, &exc);
	if(attrNS != NULL)
		fprintf(stderr,"\nElement.getAttributeNodeNS(\"%s\",\"%s\"), NOT NULL",attr_URI->str,attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	attr_name = gdome_str_mkref("CIPPO2");
	attr_URI = gdome_str_mkref("urn:cips.ciak.uk");
	attrNS = gdome_el_getAttributeNodeNS(el, attr_URI, attr_name, &exc);
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	if(attrNS == NULL)
		fprintf(stderr,"\nElement.getAttributeNodeNS(\"%s\",\"%s\"), NULL",attr_URI->str,attr_name->str), ret = FALSE;
	else {
		attr_name = gdome_a_nodeName(attrNS,&exc);
		cmp_str = gdome_str_mkref("pippo:CIPPO2");
		if(ret && !gdome_str_equal(attr_name, cmp_str))
			fprintf(stderr,"\nElement.getAttributeNodeNS(\"%s\",\"%s\"), wrong",attr_URI->str,attr_name->str), ret = FALSE;
		gdome_str_unref (attr_name);
		gdome_str_unref (cmp_str);
	}
	dot();

	/* test Element::removeAttributeNode */
	attr_name = gdome_str_mkref("FOO1");
	attr = gdome_el_removeAttributeNode(el, attr, &exc);
	gdome_a_unref (attr, &exc);
	if(attr == NULL)
		fprintf(stderr,"\nElement.removeAttributeNode(attr), NULL"), ret = FALSE;
	else {
		cmp_str = gdome_a_nodeName(attr,&exc);
		if(ret && !gdome_str_equal(cmp_str, attr_name))
			fprintf(stderr,"\nElement.removeAttributeNode(attr), wrong"), ret = FALSE;
		gdome_str_unref (cmp_str);
		if(ret && gdome_el_getAttributeNode(el, attr_name, &exc) != NULL)
			fprintf(stderr,"\nElement.removeAttributeNode(attr), error"), ret = FALSE;
		gdome_str_unref (attr_name);
	}
	attr_name =  gdome_str_mkref("CIPPO2");
	attr_URI = gdome_str_mkref("urn:cips.ciak.uk");
	attrNS = gdome_el_removeAttributeNode(el, attrNS, &exc);
	gdome_a_unref (attrNS, &exc);
	if(attr == NULL)
		fprintf(stderr,"\nElement.removeAttributeNode(attrNS), NULL"), ret = FALSE;
	else {
		temp_str = gdome_a_nodeName(attrNS,&exc);
		cmp_str = gdome_str_mkref("pippo:CIPPO2");
		if(ret && !gdome_str_equal(temp_str, cmp_str))
			fprintf(stderr,"\nElement.removeAttributeNode(attrNS), wrong"), ret = FALSE;
		gdome_str_unref (cmp_str);
		gdome_str_unref (temp_str);
		if(ret && gdome_el_getAttributeNodeNS(el, attr_URI, attr_name, &exc) != NULL)
			fprintf(stderr,"\nElement.removeAttributeNode(attrNS), error"), ret = FALSE;
		gdome_str_unref (attr_URI);
		gdome_str_unref (attr_name);
	}
	attr_name = gdome_str_mkref("FOO2");
	attr_value = gdome_str_mkref("ciccia2");
	attrDef = gdome_el_getAttributeNode(el, attr_name, &exc);
	attrDef = gdome_el_removeAttributeNode(el, attrDef, &exc);
	gdome_a_unref (attrDef, &exc);
	if(attrDef == NULL)
		fprintf(stderr,"\nElement.removeAttributeNode(attrDef), NULL"), ret = FALSE;
	else {
		cmp_str = gdome_a_nodeName(attrDef,&exc);
		if(ret && !gdome_str_equal(cmp_str, attr_name))
			fprintf(stderr,"\nElement.removeAttributeNode(attrDef), wrong");
		else {
			gdome_str_unref (cmp_str);
			gdome_a_unref (attrDef, &exc);
			attrDef = gdome_el_getAttributeNode(el, attr_name, &exc);
			if(ret && attrDef == NULL)
				fprintf(stderr,"\nElement.removeAttributeNode(attrDef), error1"), ret = FALSE;
			else {
				cmp_str = gdome_a_nodeName(attrDef,&exc);
				temp_str = gdome_a_nodeValue(attrDef,&exc);
				if(ret && (!gdome_str_equal(cmp_str, attr_name) ||
									 !gdome_str_equal(temp_str, temp_str)))
					fprintf(stderr,"\nElement.removeAttributeNode(attrDef), error2"), ret = FALSE;
				gdome_str_unref (cmp_str);
				gdome_str_unref (temp_str);
				gdome_str_unref (attr_name);
				gdome_str_unref (attr_value);
				gdome_a_unref (attrDef, &exc);
			}
		}
	}
	dot();

	/* test Element::setAttributeNode */
	attr_name = gdome_str_mkref("FOO1");
	attr_temp = gdome_el_setAttributeNode(el, attr, &exc);
	gdome_a_unref (attr, &exc);
	if(attr_temp != NULL)
		fprintf(stderr,"\nElement.setAttributeNode(attr), NOT NULL"), ret = FALSE;
	else if(ret && (attr = gdome_el_getAttributeNode(el, attr_name, &exc)) == NULL)
		fprintf(stderr,"\nElement.setAttributeNode(attr), error1"), ret = FALSE;
	else {
		cmp_str = gdome_a_nodeName(attr,&exc);
		if(ret && !gdome_str_equal(cmp_str, attr_name))
			fprintf(stderr,"\nElement.setAttributeNode(attr), error2"), ret = FALSE;
		gdome_str_unref (cmp_str);
		gdome_str_unref (attr_name);
		gdome_a_unref (attr, &exc);
	}
	dot();

	/* test Element::setAttributeNodeNS */
	attr_name = gdome_str_mkref("CIPPO2");
	attr_URI = gdome_str_mkref("urn:cips.ciak.uk");
	attr_temp = gdome_el_setAttributeNodeNS(el, attrNS, &exc);
	gdome_a_unref (attrNS, &exc);
	if(attr_temp != NULL)
		fprintf(stderr,"\nElement.setAttributeNodeNS(attrNS), NOT NULL"), ret = FALSE;
	else if(ret && (attrNS = gdome_el_getAttributeNodeNS(el, attr_URI, attr_name, &exc)) == NULL)
		fprintf(stderr,"\nElement::setAttributeNodeNS(attrNS), error1"), ret = FALSE;
	else {
		gdome_str_unref (attr_name);
		gdome_str_unref (attr_URI);
		attr_name = gdome_a_nodeName(attrNS,&exc);
		cmp_str = gdome_str_mkref("pippo:CIPPO2");
		if(ret && !gdome_str_equal(attr_name, cmp_str))
			fprintf(stderr,"\nElement.setAttributeNodeNS(attrNS), error2"), ret = FALSE;
		gdome_str_unref (attr_name);
		gdome_str_unref (cmp_str);
		gdome_a_unref (attr_temp, &exc);
		gdome_a_unref (attrNS, &exc);
	}
	dot();

	/* test Element::getAttribute */
	attr_name = gdome_str_mkref("FOO3");
	temp_str = gdome_el_getAttribute(el, attr_name, &exc);
	if(temp_str == NULL)
		fprintf(stderr,"\nElement.getAttribute(\"%s\"), NULL", attr_name->str), ret = FALSE;
	else {
		cmp_str = gdome_str_mkref("bar3");
		if(ret && !gdome_str_equal(temp_str, cmp_str))
			fprintf(stderr,"\nElement.getAttribute(\"%s\"), wrong", attr_name->str), ret = FALSE;
		gdome_str_unref (cmp_str);
		gdome_str_unref (temp_str);
		gdome_str_unref (attr_name);
	}
	attr_name = gdome_str_mkref("XXXX");
	temp_str = gdome_el_getAttribute(el, attr_name, &exc);
	cmp_str = gdome_str_mkref ("");
	if(!gdome_str_equal (temp_str, cmp_str))
		fprintf(stderr,"\nElement.getAttribute(\"%s\"), NOT empty", attr_name->str), ret = FALSE;
	gdome_str_unref (cmp_str);
	gdome_str_unref (temp_str);
	gdome_str_unref (attr_name);
	dot();

	/* test Element::getAttributeNS */
	attr_name = gdome_str_mkref("CIPPO3");
	attr_URI = gdome_str_mkref("urn:cips.ciak.uk");
	temp_str = gdome_el_getAttributeNS(el, attr_URI, attr_name, &exc);
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	if(temp_str == NULL)
		fprintf(stderr,"\nElement.getAttributeNS(\"%s\",\"%s\"), NULL", attr_URI->str,attr_name->str), ret = FALSE;
	else {
		cmp_str = gdome_str_mkref("lippo3");
		if(ret && !gdome_str_equal(temp_str, cmp_str))
			fprintf(stderr,"\nElement::getAttributeNS(\"%s\",\"%s\"), wrong", attr_URI->str, attr_name->str), ret = FALSE;
		gdome_str_unref (temp_str);
		gdome_str_unref (cmp_str);
	}
	attr_name = gdome_str_mkref("XXXX");
	attr_URI = gdome_str_mkref("urn:cips.ciak.uk");
	temp_str = gdome_el_getAttributeNS(el, attr_URI, attr_name, &exc);
	cmp_str = gdome_str_mkref ("");
	if(!gdome_str_equal (temp_str, cmp_str))
		fprintf(stderr,"\nElement::getAttribute(\"%s\",\"%s\"), NOT empty", attr_URI->str, attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	gdome_str_unref (temp_str);
	attr_name = gdome_str_mkref("CIPPO3");
	attr_URI = gdome_str_mkref("urn:xxxx.xxxx.xx");
	temp_str = gdome_el_getAttributeNS(el, attr_URI, attr_name, &exc);
	if(!gdome_str_equal (temp_str, cmp_str))
		fprintf(stderr,"\nElement::getAttribute(\"%s\",\"%s\"), NOT empty", attr_URI->str, attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	gdome_str_unref (temp_str);
	gdome_str_unref (cmp_str);
	dot();

	/* test Element::setAttribute */
	attr_name = gdome_str_mkref("NEWATTR");
	attr_value = gdome_str_mkref("newvalue");
	gdome_el_setAttribute(el, attr_name, attr_value, &exc);
	temp_str = gdome_el_getAttribute(el, attr_name, &exc);
	if(temp_str == NULL)
		fprintf(stderr,"\nElement::setAttribute(\"%s\",\"%s\"), NULL", attr_name->str, attr_value->str), ret = FALSE;
	else if(ret && !gdome_str_equal(attr_value, temp_str))
		fprintf(stderr,"\nElement::setAttribute(\"%s\",\"%s\"), wrong", attr_name->str, attr_value->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_value);
	gdome_str_unref (temp_str);
	dot();

	/* test Element::setAttributeNS */
	attr_name = gdome_str_mkref("myns:NEWATTR1");
	attr_value = gdome_str_mkref("newvalue1");
	attr_URI = gdome_str_mkref("urn:myns.casarini.org");
	gdome_el_setAttributeNS(el, attr_URI, attr_name, attr_value, &exc);
	gdome_str_unref (attr_name);
	attr_name = gdome_str_mkref("NEWATTR1");
	temp_str = gdome_el_getAttributeNS(el, attr_URI, attr_name, &exc);
	if(temp_str == NULL)
		fprintf(stderr,"\nElement::setAttributeNS(\"%s\",\"%s\",\"%s\"), NULL", attr_URI->str, attr_name->str, attr_value->str), ret = FALSE;
	else if(ret && !gdome_str_equal(attr_value, temp_str))
		fprintf(stderr,"\nElement::setAttributeNS(\"%s\",\"%s\",\"%s\"), wrong", attr_URI->str, attr_name->str, attr_value->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	gdome_str_unref (attr_value);
	gdome_str_unref (temp_str);
	dot();

	/* test Element::removeAttribute */
	attr_name = gdome_str_mkref("FOO4");
	gdome_el_removeAttribute(el, attr_name, &exc);
	temp_str = gdome_el_getAttribute(el, attr_name, &exc);
	cmp_str = gdome_str_mkref ("");
	if(!gdome_str_equal (temp_str, cmp_str))
		fprintf(stderr,"\nElement::removeAttribute(\"%s\"), NOT NULL", attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (temp_str);
	gdome_str_unref (cmp_str);
	dot();

	/* test Element::removeAttributeNS */
	attr_name = gdome_str_mkref("CIPPO4");
	attr_URI = gdome_str_mkref("urn:cips.ciak.uk");
	gdome_el_removeAttributeNS(el, attr_URI, attr_name, &exc);
	temp_str = gdome_el_getAttributeNS(el, attr_URI, attr_name, &exc);
	cmp_str = gdome_str_mkref ("");
	if(!gdome_str_equal (temp_str, cmp_str))
		fprintf(stderr,"\nElement::removeAttributeNS(\"%s\",\"%s\"), failed", attr_URI->str,attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	gdome_str_unref (temp_str);
	gdome_str_unref (cmp_str);
	dot();

	/* test Element::hasAttribute */
	attr_name = gdome_str_mkref("NEWATTR");
	if (!gdome_el_hasAttribute(el, attr_name, &exc))
		fprintf(stderr,"\nElement::hasAttrbute(\"%s\"), wrong", attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	attr_name = gdome_str_mkref("BOBOBO");
	if (gdome_el_hasAttribute(el, attr_name, &exc))
		fprintf(stderr,"\nElement::hasAttribute(\"%s\")2, wrong", attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	dot();

	/* test Element::hasAttributeNS */
	attr_name = gdome_str_mkref("NEWATTR1");
	attr_URI = gdome_str_mkref("urn:myns.casarini.org");
	if (!gdome_el_hasAttributeNS(el, attr_URI, attr_name, &exc))
		fprintf(stderr,"\nElement::hasAttrbuteNS(\"%s\",\"%s\"), wrong", attr_URI->str, attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	attr_name = gdome_str_mkref("BOBOBO");
	attr_URI = gdome_str_mkref("urn:myns.casarini.org");
	if (gdome_el_hasAttributeNS(el, attr_URI, attr_name, &exc))
		fprintf(stderr,"\nElement::hasAttrbuteNS(\"%s\",\"%s\"), wrong", attr_URI->str, attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	attr_name = gdome_str_mkref("NEWATTR1");
	attr_URI = gdome_str_mkref("urn:myns.xxxx.xx");
	if (gdome_el_hasAttributeNS(el, attr_URI, attr_name, &exc))
		fprintf(stderr,"\nElement::hasAttrbuteNS(\"%s\",\"%s\"), wrong", attr_URI->str, attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	dot();

	printf("test_el\n");
	return ret;
}

gboolean test_xmlns(GdomeElement *el) {
	GdomeAttr *attrNS, *attr;

	GdomeDOMString *attr_name;
	GdomeDOMString *attr_value;
	GdomeDOMString *attr_URI;
	GdomeDOMString *temp_str;
	GdomeDOMString *cmp_str = NULL;

	gboolean ret = TRUE;
	GdomeException exc;

	/* test Element::getAttributeNode */	
 	attr_name = gdome_str_mkref("xmlns");
	attr = gdome_el_getAttributeNode(el, attr_name, &exc);
	if(attr == NULL)
		fprintf(stderr,"\nElement.getAttributeNode(\"%s\"), NULL",attr_name->str), ret = FALSE;
	else {
		cmp_str = gdome_a_nodeName(attr, &exc);
		if(ret && !gdome_str_equal(cmp_str, attr_name))
			fprintf(stderr,"\nElement.getAttributeNode(\"%s\"), wrong",attr_name->str), ret = FALSE;
		else {
			gdome_str_unref (cmp_str);
			cmp_str = gdome_a_value(attr, &exc);
			if (cmp_str == NULL)
				fprintf(stderr,"\nAttribute.value(\"%s\"), NULL",attr_name->str), ret = FALSE;
		  else {
				temp_str = gdome_str_mkref ("http://test.org/");
				if(ret && !gdome_str_equal(cmp_str, temp_str))
					fprintf(stderr,"\nAttribute.value(\"%s\"), wrong",cmp_str->str), ret = FALSE;
				gdome_str_unref (temp_str);
			}
		}
	}
	gdome_str_unref (attr_name);
	gdome_str_unref (cmp_str);
	gdome_a_unref (attr, &exc);
	dot();

	/* test Element::hasAttribute */
	attr_name = gdome_str_mkref("xmlns");
	if (!gdome_el_hasAttribute(el, attr_name, &exc))
		fprintf(stderr,"\nElement::hasAttrbute(\"%s\"), wrong", attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	dot();

	/* test Element::getAttributeNS */
	attr_name = gdome_str_mkref("pippo");
	attr_URI = gdome_str_mkref("http://www.w3.org/2000/xmlns/");
	temp_str = gdome_el_getAttributeNS(el, attr_URI, attr_name, &exc);
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	if(temp_str == NULL)
		fprintf(stderr,"\nElement.getAttributeNS(\"%s\",\"%s\"), NULL", attr_URI->str,attr_name->str), ret = FALSE;
	else {
		cmp_str = gdome_str_mkref("urn:cips.ciak.uk");
		if(ret && !gdome_str_equal(temp_str, cmp_str))
			fprintf(stderr,"\nElement::getAttributeNS(\"%s\",\"%s\"), wrong", attr_URI->str, attr_name->str), ret = FALSE;
		gdome_str_unref (temp_str);
		gdome_str_unref (cmp_str);
	}
	dot();

	/* test Element::getAttributeNodeNS */
	attr_name = gdome_str_mkref("CIPPO2");
	attr_URI = gdome_str_mkref("http://www.w3.org/2000/xmlns/");
	attrNS = gdome_el_getAttributeNodeNS(el, attr_URI, attr_name, &exc);
	if(attrNS != NULL)
		fprintf(stderr,"\nElement.getAttributeNodeNS(\"%s\",\"%s\"), NOT NULL",attr_URI->str,attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);

	attr_name = gdome_str_mkref("pippo");
	attr_URI = gdome_str_mkref("http://www.w3.org/2000/xmlns/");
	attrNS = gdome_el_getAttributeNodeNS(el, attr_URI, attr_name, &exc);
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);

	if(attrNS == NULL)
		fprintf(stderr,"\nElement.getAttributeNodeNS(\"%s\",\"%s\"), NULL",attr_URI->str,attr_name->str), ret = FALSE;
	else {
		attr_name = gdome_a_nodeName(attrNS,&exc);
		cmp_str = gdome_str_mkref("xmlns:pippo");
		if(ret && !gdome_str_equal(attr_name, cmp_str))
			fprintf(stderr,"\nElement.getAttributeNodeNS(\"%s\",\"%s\"), wrong",attr_URI->str,attr_name->str), ret = FALSE;
		gdome_str_unref (attr_name);
		gdome_str_unref (cmp_str);
	}
	gdome_a_unref (attrNS, &exc);
	dot();

	/* test Element::setAttributeNS */
	attr_name = gdome_str_mkref("xmlns:myns");
	attr_value = gdome_str_mkref("http://www.myns.com/");
	attr_URI = gdome_str_mkref("http://www.w3.org/2000/xmlns/");
	gdome_el_setAttributeNS(el, attr_URI, attr_name, attr_value, &exc);
	gdome_str_unref (attr_name);
	attr_name = gdome_str_mkref("myns");
	temp_str = gdome_el_getAttributeNS(el, attr_URI, attr_name, &exc);
	if(temp_str == NULL)
		fprintf(stderr,"\nElement::setAttributeNS(\"%s\",\"%s\",\"%s\"), NULL", attr_URI->str, attr_name->str, attr_value->str), ret = FALSE;
	else if(ret && !gdome_str_equal(attr_value, temp_str))
		fprintf(stderr,"\nElement::setAttributeNS(\"%s\",\"%s\",\"%s\"), wrong", attr_URI->str, attr_name->str, attr_value->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_URI);
	gdome_str_unref (attr_value);
	gdome_str_unref (temp_str);
	dot();

	/* test Element::removeAttribute */
	attr_name = gdome_str_mkref("xmlns");
	gdome_el_removeAttribute(el, attr_name, &exc);
	temp_str = gdome_el_getAttribute(el, attr_name, &exc);
	cmp_str = gdome_str_mkref ("");
	if(!gdome_str_equal (temp_str, cmp_str))
		fprintf(stderr,"\nElement::removeAttribute(\"%s\"), NOT NULL", attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (temp_str);
	gdome_str_unref (cmp_str);
	dot();

	/* test Element::hasAttribute */
	attr_name = gdome_str_mkref("xmlns");
	if (gdome_el_hasAttribute(el, attr_name, &exc))
		fprintf(stderr,"\nElement::hasAttrbute(\"%s\"), wrong", attr_name->str), ret = FALSE;
	gdome_str_unref (attr_name);
	dot();

	/* test Element::setAttribute */
	attr_name = gdome_str_mkref("xmlns");
	attr_value = gdome_str_mkref("http://a.com");
	gdome_el_setAttribute(el, attr_name, attr_value, &exc);
	temp_str = gdome_el_getAttribute(el, attr_name, &exc);
	if(!gdome_str_equal (temp_str, attr_value))
		fprintf(stderr,"\nElement::setAttribute(\"%s\"), wrong != %s", attr_name->str, temp_str->str), ret = FALSE;
	gdome_str_unref (attr_name);
	gdome_str_unref (attr_value);
	gdome_str_unref (temp_str);

	/* test Element::getAttributeNode */
	attr_name = gdome_str_mkref("xmlns");
	attr = gdome_el_getAttributeNode(el, attr_name, &exc);
	gdome_str_unref (attr_name);
	if(attr == NULL)
		fprintf(stderr,"\nElement.getAttributeNode(\"%s\"), NULL",attr_name->str), ret = FALSE;
	else {
		attr_name = gdome_a_nodeName(attr,&exc);
		cmp_str = gdome_str_mkref("xmlns");
		if(ret && !gdome_str_equal(attr_name, cmp_str))
			fprintf(stderr,"\nElement.getAttributeNode(\"%s\"), wrong",attr_name->str), ret = FALSE;
		gdome_str_unref (attr_name);
		gdome_str_unref (cmp_str);
	}
	dot();

	gdome_a_unref (attr, &exc);

	printf("test_xmlns\n");
	return ret;
}

/* this function would segfault in gdome2 0.7.0, due to a problem
 * with gdome_nnm_unref decreasing the livenodes since doc->refcnt == 0
 * fixed problem by adding a workaround in increase livenodes in
 * gdome_xml_doc_mkref when doc->refcnt == 0 */
gboolean test_el2(GdomeDocument *domdoc) {
	GdomeElement *el;
	GdomeNodeList *nl;
	unsigned long i, lun;
  gboolean ret = TRUE;
	GdomeNamedNodeMap *attr;
	GdomeNode *childs[7];
	GdomeException exc;

	el = gdome_doc_documentElement (domdoc, &exc);
	gdome_n_unref ((GdomeNode *)domdoc, &exc);
	nl = gdome_n_childNodes((GdomeNode *)el, &exc);
	lun = gdome_nl_length(nl, &exc);
	for(i=0; i<lun && ret ;i++) {
		childs[i] = gdome_nl_item(nl, i, &exc);
		if (childs[i] == NULL)
      fprintf(stderr,"\nNodeList.item(%lu) ChildNodes, NULL", i), ret = FALSE;
	}
	gdome_nl_unref (nl, &exc);
	for(i=0; i<lun && ret ;i++) {
		if (gdome_n_nodeType(childs[i], &exc) == GDOME_ELEMENT_NODE) {
			attr = gdome_n_attributes(childs[i], &exc);
			gdome_nnm_unref (attr, &exc);
		}
	}
	for(i=0; i<lun && ret ;i++) {
		gdome_n_unref (childs[i], &exc);
	}
	gdome_n_unref ((GdomeNode *)el, &exc);
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
	gboolean ret, ret2, ret3;

	domImpl = gdome_di_mkref();
	domdoc = gdome_di_createDocFromURI (domImpl, LOCALDIR"/test-element.xml", GDOME_LOAD_PARSING, &exc);
	if (domdoc == NULL) {
		fprintf (stderr, "DOImplementation.createDocFromURI: failed\n   test-element.xml not found\n");
		return -1;
	}

	el = gdome_doc_documentElement (domdoc, &exc);
	ret = test_el(el);
	ret2 = test_xmlns(el);

	gdome_el_unref (el, &exc);
	gdome_doc_unref (domdoc, &exc);

	domdoc = gdome_di_createDocFromURI (domImpl, LOCALDIR"/test-element.xml", GDOME_LOAD_PARSING, &exc);
	if (domdoc == NULL) {
		fprintf (stderr, "DOImplementation.createDocFromURI: failed\n   test-element.xml not found\n");
		return -1;
	}
	ret3 = test_el2(domdoc);

	if(ret && ret2 && ret3)
		return 0;
	else
		return -1;
}

