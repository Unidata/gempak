/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-private-list.c
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

#include <stdlib.h>
#include "gdome-private-list.h"

/**
 * gdome_private_list_append:
 * @list:  a pointer to the list to which append a new element
 * @data:  a pointer to the data that we want to add to the list
 *
 * Append a new element containing @data to the list specified.
 * Returns: the list with the new element appended.
 */
GdomePrivateList *
gdome_private_list_append (GdomePrivateList *list, void *data)
{
	GdomePrivateList *new_list;
  
	new_list = malloc (sizeof (GdomePrivateList));
	new_list->data = data;
	new_list->next = NULL;
  
	if (list) {
		while (list->next)
			list = list->next;

		list->next = new_list;

		return list;
	}
	else
		return new_list;
}

/**
 * gdome_private_list_prepend:
 * @list:  a pointer to the list to which add a new element in the first position
 * @data:  a pointer to the data that we want to add to the list
 *
 * Add a new element containing @data at the first position of list
 * specified.
 * Returns the list with the new element added.
 */
GdomePrivateList *
gdome_private_list_prepend (GdomePrivateList *list, void *data)
{
	GdomePrivateList *new_list;

	new_list = malloc (sizeof (GdomePrivateList));
	new_list->data = data;
	new_list->next = list;

	return new_list;
}

/**
 * gdome_private_list_length:
 * @list:  a pointer to a list
 *
 * Returns: the number of elements in the list specified.
 */
unsigned int
gdome_private_list_length (GdomePrivateList *list)
{
	unsigned int length;
	length = 0;

	while (list) {
		length++;
		list = list->next;
	}

	return length;
}

/**
 * gdome_private_list_remove:
 * @list:  a pointer to the list to which remove an element
 * @data:  the pointer to the data that is to be removed from the list
 *
 * Remove the element containing @data from the list.
 * Returns: the resulted list.
 */
GdomePrivateList *
gdome_private_list_remove (GdomePrivateList *list, void *data)
{
	GdomePrivateList *tmp, *prev;

	tmp = list;
	prev = NULL;

	while (tmp) {
		if (tmp->data == data) {
			if (prev)
				prev->next = tmp->next;
			if (list == tmp)
				list = list->next;

			tmp->next = NULL;
			free (tmp);

			break;
		}

		prev = tmp;
		tmp = tmp->next;
	}

	return list;
}

/**
 * gdome_private_list_nth:
 * @list:  a pointer to the list of which the nth element is wanted
 * @n:  the index to the element wanted
 *
 * Returns: a pointer to the nth element of the list specified.
 */
GdomePrivateList *
gdome_private_list_nth (GdomePrivateList *list, unsigned int n)
{
	while ((n-- > 0) && list)
		list = list->next;

	return list;
}
