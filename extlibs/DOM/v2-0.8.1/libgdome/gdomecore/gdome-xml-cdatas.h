/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-cdatas.h
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

#ifndef GDOME_CDATA_SECTION_FILE
#define GDOME_CDATA_SECTION_FILE

struct _GdomeCDATASectionVtab {
  GdomeTextVtab super;
};

typedef struct _Gdome_xml_CDATASection Gdome_xml_CDATASection;
struct _Gdome_xml_CDATASection {
  GdomeCDATASection super;
	const GdomeCDATASectionVtab *vtab;
  int refcnt;
  xmlNode *n;
  GdomeAccessType accessType;
  Gdome_xml_ListenerList *ll;
};

gpointer   gdome_xml_cds_query_interface (GdomeNode *self,
                                          const char *interface,
                                          GdomeException *exc);


extern const GdomeCDATASectionVtab gdome_xml_cds_vtab;



#endif /* GDOME_CDATA_SECTION_FILE */
