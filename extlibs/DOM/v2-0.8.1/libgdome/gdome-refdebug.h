/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-refdebug.h
 *
 * CopyRight (C) 2001 Paolo Casarini <paolo@casarini.org>
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

#ifndef GDOME_REFDEBUG_H
#define GDOME_REFDEBUG_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef enum {
  GDOME_REFDBG_STR = 0,
  GDOME_REFDBG_NODE = 1,
  GDOME_REFDBG_NODELIST = 2,
  GDOME_REFDBG_NAMEDNODEMAP = 3,
  GDOME_REFDBG_EVENT = 4,
  GDOME_REFDBG_EVENTLISTENER = 5,
} GdomeRefType;

void gdome_refdbg_addRef (void *ref, GdomeRefType type);
gint gdome_refdbg_numRef (void *ref, GdomeRefType type);
void gdome_refdbg_delRef (void *ref, GdomeRefType type);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* GDOME_REFDEBUG_H */
