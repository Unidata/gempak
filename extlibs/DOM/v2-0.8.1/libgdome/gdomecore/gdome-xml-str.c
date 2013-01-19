/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-str.c
 *
 * Copyright (C) 1999 Raph Levien <raph@acm.org>
 * Copyright (C) 2000 Mathieu Lacage <mathieu@gnu.org>
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include "gdome.h"
#include "gdome-refdebug.h"
#include "gdome-xml-str.h"

void
gdome_xml_str_const_unref (GdomeDOMString *self) {
#ifdef DEBUG_REFCNT
      gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_STR);
#endif
	g_free(self);
}

void
gdome_xml_str_unref_own (GdomeDOMString *self) {
	g_free (self->str);
#ifdef DEBUG_REFCNT
      gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_STR);
#endif
	g_free (self);
}

void
gdome_xml_str_unref_xml (GdomeDOMString *self) {
  xmlFree (self->str);
#ifdef DEBUG_REFCNT
      gdome_refdbg_delRef ((void *)self, GDOME_REFDBG_STR);
#endif
  g_free (self);
}

/**
 * gdome_xml_str_mkref:
 * @str:  a %NULL terminated string
 *
 * Creates a #GdomeDOMString from a statically allocated string.
 * Returns: the new DOMString object.
 */
GdomeDOMString *
gdome_xml_str_mkref (const gchar *str) {
	GdomeDOMString *ds;

  if (str == NULL)
    return NULL;

  ds = g_new (GdomeDOMString, 1);
#ifdef DEBUG_REFCNT
      gdome_refdbg_addRef ((void *)ds, GDOME_REFDBG_STR);
#endif
  ds->refcnt = 1;
	ds->unref = gdome_xml_str_const_unref;
	ds->str = (gchar *)str;
	return ds;
}

/**
 * gdome_xml_str_mkref_own:
 * @str:  a %NULL terminated string
 *
 * Creates a #GdomeDOMString from a dynamically allocated gchar buffer.
 * Returns: the new DOMString object.
 */
GdomeDOMString *
gdome_xml_str_mkref_own (gchar *str) {
	GdomeDOMString *ds;

  if (str == NULL)
    return NULL;

  ds = g_new (GdomeDOMString, 1);
#ifdef DEBUG_REFCNT
      gdome_refdbg_addRef ((void *)ds, GDOME_REFDBG_STR);
#endif
	ds->refcnt = 1;
	ds->unref = gdome_xml_str_unref_own;
  ds->str = str;
	return ds;
}

/**
 * gdome_xml_str_mkref_xml:
 * @str:  a %NULL terminated string
 *
 * Creates a #GdomeDOMString from a xmlChar buffer already allocated by libxml.
 * DEPRECATED
 * Returns: the new DOMString object.
 */
GdomeDOMString *
gdome_xml_str_mkref_xml (xmlChar *str) {
  GdomeDOMString *ds;

  if (str == NULL)
    return NULL;

  ds = g_new (GdomeDOMString, 1);
#ifdef DEBUG_REFCNT
      gdome_refdbg_addRef ((void *)ds, GDOME_REFDBG_STR);
#endif
  ds->refcnt = 1;
	ds->unref = gdome_xml_str_unref_xml;
	ds->str = str;
	return ds;
}

/**
 * gdome_xml_str_mkref_dup:
 * @str:  a %NULL terminated string
 *
 * Creates a #GdomeDOMString from a static or dynamically allocated gchar
 * buffer, but a copy of the initializing string is done before construction.
 * Returns: the new DOMString object.
 */
GdomeDOMString *
gdome_xml_str_mkref_dup (const gchar *str) {
	GdomeDOMString *ds;

  if (str == NULL)
    return NULL;

  ds = g_new (GdomeDOMString, 1);
#ifdef DEBUG_REFCNT
      gdome_refdbg_addRef ((void *)ds, GDOME_REFDBG_STR);
#endif
	ds->refcnt = 1;
	ds->unref = gdome_xml_str_unref_own;
  ds->str = g_strdup(str);
	return ds;
}

/**
 * gdome_xml_str_ref:
 * @self:  DOMString Object ref
 *
 * Increase the reference count of the specified #GdomeDOMString.
 */
void
gdome_xml_str_ref (GdomeDOMString *self) {
  g_return_if_fail (self != NULL);

  self->refcnt++;
}

/**
 * gdome_xml_str_unref:
 * @self:  DOMString Object ref
 *
 * Decrease the reference count of the specified #GdomeDOMString. Free the
 * structure if the object will have zero reference.
 */
void
gdome_xml_str_unref (GdomeDOMString *self) {
  g_return_if_fail (self != NULL);

  self->refcnt--;

	if(self->refcnt == 0)
		self->unref (self);
}

/**
 * gdome_xml_str_equal:
 * @self:  DOMString Object ref
 * @str:  DOMString to be compared
 *
 * Returns: %TRUE if the strings are equal, %FALSE otherwise.
 */
GdomeBoolean
gdome_xml_str_equal(GdomeDOMString *self, GdomeDOMString *str) {
  g_return_val_if_fail (self != NULL, FALSE);
  if (str == NULL)
    return FALSE;
	if(xmlStrEqual((xmlChar *)self->str, (xmlChar *)str->str))
		return TRUE;
	else
    return FALSE;
}

/**
 * gdome_xml_str_equalIgnoreCase:
 * @self:  DOMString Object ref
 * @str:  DOMString to be compared
 *
 * Returns: %TRUE if the strings are equal ignoring case, %FALSE otherwise.
 */
GdomeBoolean
gdome_xml_str_equalIgnoreCase(GdomeDOMString *self, GdomeDOMString *str) {
  g_return_val_if_fail (self != NULL, FALSE);
  if (str == NULL)
    return FALSE;
	if(!xmlStrcasecmp((xmlChar *)self->str, (xmlChar *)str->str))
		return TRUE;
	else
    return FALSE;
}

/**
 * gdome_xml_str_charAt:
 * @self:  DOMString Object ref
 * @index:  the index of the character wanted
 *
 * Returns: the @index -th character in the specified string
 */
gchar
gdome_xml_str_charAt (GdomeDOMString *self, int index)
{
	g_return_val_if_fail (self != NULL, '\0');
	if (index < 0 || index >= strlen (self->str)) {
		g_warning ("gdome_str_charAt: out bound error\n");
		return '\0';
	}

	return self->str[index];
}

/**
 * gdome_xml_str_concat:
 * @self:  DOMString Object ref
 * @str:  DOMString to be appended
 *
 * Returns: a new #GdomeDOMString that is the concatenation of this string
 * with @str. If @str is %NULL a new reference to this string is returned.
 */
GdomeDOMString *
gdome_xml_str_concat (GdomeDOMString *self, GdomeDOMString *str)
{
	gchar *ret;

	g_return_val_if_fail (self != NULL, NULL);
	if (str == NULL) {
    self->refcnt++;
		return self;
	}

	ret = g_strconcat (self->str, str->str, NULL);

  return gdome_xml_str_mkref_own (ret);
}

/**
 * gdome_xml_str_endsWith:
 * @self:  DOMString Object ref
 * @suffix:  DOMString to check
 *
 * Returns: %TRUE if this string ends with @str.
 */
GdomeBoolean
gdome_xml_str_endsWith (GdomeDOMString *self, GdomeDOMString *suffix)
{
  int len_self, len_str;

	g_return_val_if_fail (self != NULL, FALSE);
	if (suffix == NULL) {
		return TRUE;
  }
  len_self = strlen (self->str);
  len_str = strlen (suffix->str);

  if (len_str > len_self)
    return FALSE;

  if(xmlStrEqual((xmlChar *)&(self->str[len_self-len_str]), (xmlChar *)suffix->str))
		return TRUE;
	else
    return FALSE;
}

/**
 * gdome_xml_str_isEmpty:
 * @self:  DOMString Object ref
 *
 * Returns: %TRUE if this string is empty, %FALSE otherwise.
 */
GdomeBoolean
gdome_xml_str_isEmpty (GdomeDOMString *self)
{
	if (self != NULL && strlen (self->str) > 0)
		return TRUE;
	return FALSE;
}

/**
 * gdome_xml_str_length:
 * @self:  DOMString Object ref
 *
 * Returns: the length of this string.
 */
int
gdome_xml_str_length (GdomeDOMString *self)
{
  g_return_val_if_fail (self != NULL, 0);

  return strlen (self->str);
}

/**
 * gdome_xml_str_startsWith:
 * @self:  DOMString Object ref
 * @prefix:  DOMString to check
 *
 * Returns: %TRUE if this string starts with @str.
 */
GdomeBoolean
gdome_xml_str_startsWith (GdomeDOMString *self, GdomeDOMString *prefix)
{
  int len_self, len_str;

	g_return_val_if_fail (self != NULL, FALSE);
	if (prefix == NULL) {
		return TRUE;
  }
  len_self = strlen (self->str);
  len_str = strlen (prefix->str);

  if (len_str > len_self)
    return FALSE;

  if(!xmlStrncmp((xmlChar *)self->str, (xmlChar *)prefix->str, len_str))
		return TRUE;
	else
    return FALSE;
}
