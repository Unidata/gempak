/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xpath.h
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

#ifndef GDOME_XPATH_H
#define GDOME_XPATH_H

#include <gdome.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* error types for our xpath exception handling system. */
typedef enum {
  GDOME_INVALID_EXPRESSION_ERR = 101,
  GDOME_TYPE_ERR = 102
} GdomeXPathExceptionCode;

/* xpath types */
typedef enum {
	GDOME_ANY_TYPE = 0,
	GDOME_NUMBER_TYPE = 1,
	GDOME_STRING_TYPE = 2,
	GDOME_BOOLEAN_TYPE = 3,
  GDOME_UNORDERED_NODE_ITERATOR_TYPE = 4,
  GDOME_ORDERED_NODE_ITERATOR_TYPE = 5,
  GDOME_UNORDERED_NODE_SNAPSHOT_TYPE = 6,
  GDOME_ORDERED_NODE_SNAPSHOT_TYPE = 7,
  GDOME_ANY_UNORDERED_NODE_TYPE = 8,
  GDOME_FIRST_ORDERED_NODE_TYPE = 9
} GdomeXPathResultType;

typedef struct _GdomeXPathEvaluatorVtab GdomeXPathEvaluatorVtab;
typedef struct _GdomeXPathNSResolverVtab GdomeXPathNSResolverVtab;
typedef struct _GdomeXPathResultVtab GdomeXPathResultVtab;

struct _GdomeXPathEvaluator {
	gpointer user_data;
};

struct _GdomeXPathNSResolver {
	gpointer user_data;
};

struct _GdomeXPathResult {
	gpointer user_data;
};

GdomeXPathEvaluator *gdome_xpeval_mkref (void);
void gdome_xpeval_ref (GdomeXPathEvaluator *self, GdomeException *exc);
void gdome_xpeval_unref (GdomeXPathEvaluator *self, GdomeException *exc);
GdomeXPathNSResolver * gdome_xpeval_createNSResolver (GdomeXPathEvaluator *self, GdomeNode *nodeResolver, GdomeException *exc);
GdomeXPathResult * gdome_xpeval_createResult (GdomeXPathEvaluator *self, GdomeException *exc);
GdomeXPathResult *gdome_xpeval_evaluate (GdomeXPathEvaluator *self, GdomeDOMString *expression, GdomeNode *contextNode, GdomeXPathNSResolver *resolver, unsigned int type, GdomeXPathResult *result, GdomeException *exc);
GdomeXPathNSResolver * gdome_xpnsresolv_mkref( GdomeNode *nodeResolver );
void gdome_xpnsresolv_ref (GdomeXPathNSResolver *self, GdomeException *exc);
void gdome_xpnsresolv_unref (GdomeXPathNSResolver *self, GdomeException *exc);
GdomeDOMString * gdome_xpnsresolv_lookupNamespaceURI( GdomeXPathNSResolver *self, GdomeDOMString *prefix, GdomeException *exc);
void gdome_xpresult_ref (GdomeXPathResult *self, GdomeException *exc);
void gdome_xpresult_unref (GdomeXPathResult *self, GdomeException *exc);
unsigned short gdome_xpresult_resultType (GdomeXPathResult *self, GdomeException *exc);
GdomeBoolean gdome_xpresult_booleanValue (GdomeXPathResult *self, GdomeException *exc);
double gdome_xpresult_numberValue (GdomeXPathResult *self, GdomeException *exc);
GdomeDOMString* gdome_xpresult_stringValue (GdomeXPathResult *self, GdomeException *exc);
GdomeNode * gdome_xpresult_singleNodeValue (GdomeXPathResult *self, GdomeException *exc);
GdomeNode * gdome_xpresult_iterateNext (GdomeXPathResult *self, GdomeException *exc);
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* GDOME_XPATH_H */
