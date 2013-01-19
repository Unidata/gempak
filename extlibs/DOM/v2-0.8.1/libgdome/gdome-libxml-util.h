/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-libxml-util.h
 *
 * written by Christian Glahn <christian.glahn@uibk.ac.at>
 *  
 * This header is a very limited version of gdomecore/gdome-xml-node.h
 * It gives access to get or set libxml2 datastructures to Gdome.
 * This is extremely helpfull to allow the DOM exchange between
 * different DOM implementations.
 *
 * This fuctions are part of a private interface, and are
 * undocumented and may go away at any time.
 * DO NOT USE them unless you know what you are doing!
 *
 * CopyRight (C) 2002 Paolo Casarini <paolo@casarini.org>
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

#ifndef GDOME_LIBXML_UTIL_H
#define GDOME_LIBXML_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

xmlNode   * gdome_xml_n_get_xmlNode ( GdomeNode * node );
GdomeNode * gdome_xml_n_mkref       ( xmlNode   * n );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
