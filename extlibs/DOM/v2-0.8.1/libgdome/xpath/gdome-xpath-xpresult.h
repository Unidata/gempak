/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xpath-xpresult.h
 *
 * CopyRight (C) 2002 T.J. Mather <tjmather@tjmather.com>
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

#ifndef GDOME_XPRESULT_FILE
#define GDOME_XPRESULT_FILE

#include <libxml/xpath.h>

struct _GdomeXPathResultVtab {
	void (*ref) (GdomeXPathResult *self, GdomeException *exc);
	void (*unref) (GdomeXPathResult *self, GdomeException *exc);
	unsigned short (*resultType) (GdomeXPathResult *self, GdomeException *exc);
  GdomeBoolean (*booleanValue) (GdomeXPathResult *self, GdomeException *exc);
  double (*numberValue) (GdomeXPathResult *self, GdomeException *exc);
	GdomeDOMString * (*stringValue) (GdomeXPathResult *self, GdomeException *exc);
	GdomeNode * (*singleNodeValue) (GdomeXPathResult *self, GdomeException *exc);
  GdomeNode *(*iterateNext) (GdomeXPathResult *self, GdomeException *exc);
};

typedef struct _Gdome_xpath_XPathResult Gdome_xpath_XPathResult;

struct _Gdome_xpath_XPathResult {
  const GdomeXPathResultVtab *vtab;
  int refcnt;
  xmlXPathObjectPtr res;
	GdomeNode *gnode;
	int pos;
};

void gdome_xpath_xpresult_ref (GdomeXPathResult *self, GdomeException *exc);
void gdome_xpath_xpresult_unref (GdomeXPathResult *self, GdomeException *exc);
GdomeXPathResult * gdome_xpath_xpresult_mkref (GdomeNode *gnode,
																							 xmlXPathObjectPtr res);
unsigned short gdome_xpath_xpresult_resultType (GdomeXPathResult *self,
																											GdomeException *exc);
GdomeBoolean gdome_xpath_xpresult_booleanValue (GdomeXPathResult *self,
																								GdomeException *exc);
double gdome_xpath_xpresult_numberValue (GdomeXPathResult *self,
																				 GdomeException *exc);
GdomeDOMString * gdome_xpath_xpresult_stringValue (GdomeXPathResult *self,
																									 GdomeException *exc);
GdomeNode * gdome_xpath_xpresult_singleNodeValue (GdomeXPathResult *self,
																									GdomeException *exc);
GdomeNode * gdome_xpath_xpresult_iterateNext (GdomeXPathResult *self,
																							GdomeException *exc);

extern const GdomeXPathResultVtab gdome_xpath_xpresult_vtab;

#endif /* GDOME_XPRESULT_FILE */
