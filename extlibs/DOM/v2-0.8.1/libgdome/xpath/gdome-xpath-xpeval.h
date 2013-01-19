/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xpath-xpeval.h
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

#ifndef GDOME_XPEVAL_FILE
#define GDOME_XPEVAL_FILE

struct _GdomeXPathEvaluatorVtab {
	void (*ref) (GdomeXPathEvaluator *self, GdomeException *exc);
	void (*unref) (GdomeXPathEvaluator *self, GdomeException *exc);
	GdomeXPathNSResolver *(*createNSResolver) (GdomeXPathEvaluator *self, GdomeNode *nodeResolver, GdomeException *exc);
	GdomeXPathResult *(*createResult) (GdomeXPathEvaluator *self, GdomeException *exc);
  GdomeXPathResult *(*evaluate) (GdomeXPathEvaluator *self, GdomeDOMString *expression, GdomeNode *contextNode, GdomeXPathNSResolver *resolver, unsigned int type, GdomeXPathResult *result, GdomeException *exc);
};

typedef struct _Gdome_xpath_XPathEvaluator Gdome_xpath_XPathEvaluator;

struct _Gdome_xpath_XPathEvaluator {
  const GdomeXPathEvaluatorVtab *vtab;
  int refcnt;

	GdomeXPathResult *createResult;
	GdomeXPathNSResolver *createNSResolver;
  GdomeXPathResult *evaluate;
};

GdomeXPathEvaluator * gdome_xpath_xpeval_mkref (void);
void gdome_xpath_xpeval_ref (GdomeXPathEvaluator *self, GdomeException *exc);
void gdome_xpath_xpeval_unref (GdomeXPathEvaluator *self, GdomeException *exc);
GdomeXPathNSResolver * gdome_xpath_xpeval_createNSResolver (GdomeXPathEvaluator *self,
																														GdomeNode *nodeResolver,
																														GdomeException *exc);
GdomeXPathResult * gdome_xpath_xpeval_createResult (GdomeXPathEvaluator *self,
																													GdomeException *exc);
GdomeXPathResult * gdome_xpath_xpeval_evaluate (GdomeXPathEvaluator *self,
						GdomeDOMString *expression,
						GdomeNode *contextNode,
						GdomeXPathNSResolver *resolver,
						unsigned int type,
						GdomeXPathResult *result,
						GdomeException *exc);

extern const GdomeXPathEvaluatorVtab gdome_xpath_xpeval_vtab;

#endif /* GDOME_XPEVAL_FILE */
