/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-xpns.h
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

#ifndef GDOME_XPNS_FILE
#define GDOME_XPNS_FILE

struct _GdomeXPathNamespaceVtab {
	GdomeNodeVtab super;
  GdomeElement *(*ownerElement) (GdomeXPathNamespace *self, GdomeException *exc);
};

typedef struct _Gdome_xml_XPathNamespace Gdome_xml_XPathNamespace;

struct _Gdome_xml_XPathNamespace {
	GdomeXPathNamespace super;
  const GdomeXPathNamespaceVtab *vtab;
  int refcnt;
	xmlNode *n;
	GdomeAccessType accessType;
};
GdomeXPathNamespace * gdome_xml_xpns_mkref (xmlNs *n);
GdomeElement * gdome_xml_xpns_ownerElement (GdomeXPathNamespace *self, GdomeException *exc);

extern const GdomeXPathNamespaceVtab gdome_xml_xpns_vtab;

#endif /* GDOME_XPNS_FILE */
