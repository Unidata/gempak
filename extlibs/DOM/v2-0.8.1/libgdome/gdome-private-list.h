/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-private-list.h
 *
 * Copyright (C) 1999 Raph Levien <raph@acm.org>
 * Copyright (C) 2000 Mathieu Lacage <mathieu@gnu.org>
 * Copyright (C) 2000 Anders Carlsson <andersca@gnu.org>
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

#ifndef GDOME_PRIVATE_LIST_H
#define GDOME_PRIVATE_LIST_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef struct _GdomePrivateList GdomePrivateList;

struct _GdomePrivateList {
	void *data;
	GdomePrivateList *next;
};

GdomePrivateList *gdome_private_list_append  (GdomePrivateList *list,
																							void *data);
GdomePrivateList *gdome_private_list_prepend (GdomePrivateList *list,
																							void *data);
unsigned int      gdome_private_list_length  (GdomePrivateList *list);
GdomePrivateList *gdome_private_list_remove  (GdomePrivateList *list,
																							void *data);
GdomePrivateList *gdome_private_list_nth     (GdomePrivateList *list,
                                              unsigned int n);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* GDOME_PRIVATE_LIST_H */



