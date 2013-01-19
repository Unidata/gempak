/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xpath-xpnsresolv.h
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

#ifndef GDOME_XPNSRESOLV_FILE
#define GDOME_XPNSRESOLV_FILE

struct _GdomeXPathNSResolverVtab {
	void (*ref) (GdomeXPathNSResolver *self, GdomeException *exc);
	void (*unref) (GdomeXPathNSResolver *self, GdomeException *exc);
  GdomeDOMString *(*lookupNamespaceURI) (GdomeXPathNSResolver *self, GdomeDOMString *prefix, GdomeException *exc);
};

typedef struct _Gdome_xpath_XPathNSResolver Gdome_xpath_XPathNSResolver;

struct _Gdome_xpath_XPathNSResolver {
  const GdomeXPathNSResolverVtab *vtab;
  int refcnt;

	GdomeNode *gnode;
	xmlNodePtr n;
};

GdomeXPathNSResolver * gdome_xpath_xpnsresolv_mkref( GdomeNode *nodeResolver );
void gdome_xpath_xpnsresolv_ref (GdomeXPathNSResolver *self, GdomeException *exc);
void gdome_xpath_xpnsresolv_unref (GdomeXPathNSResolver *self, GdomeException *exc);
GdomeDOMString * gdome_xpath_xpnsresolv_lookupNamespaceURI( GdomeXPathNSResolver *self, GdomeDOMString *prefix, GdomeException *exc);

const GdomeXPathNSResolverVtab gdome_xpath_xpnsresolv_vtab;

#endif /* GDOME_XPNSRESOLV_FILE */
