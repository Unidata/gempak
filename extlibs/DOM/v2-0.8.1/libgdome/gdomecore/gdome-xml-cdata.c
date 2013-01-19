/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-cdata.c
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

#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include "gdome.h"
#include "gdome-events.h"
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include "gdome-evt-event.h"
#include "gdome-evt-mevent.h"
#include "gdome-xml-util.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-xml-str.h"
#include "gdome-xml-node.h"
#include "gdome-xml-cdata.h"
#include "gdome-xml-document.h"

const GdomeCharacterDataVtab gdome_xml_cd_vtab = {
  {
	  gdome_xml_n_ref,
	  gdome_xml_n_unref,
	  gdome_xml_cd_query_interface,
    gdome_xml_n_nodeName,
    gdome_xml_n_nodeValue,
    gdome_xml_n_set_nodeValue,
    gdome_xml_n_nodeType,
		gdome_xml_n_parentNode,
    gdome_xml_n_childNodes,
    gdome_xml_n_firstChild,
    gdome_xml_n_lastChild,
    gdome_xml_n_previousSibling,
    gdome_xml_n_nextSibling,
    gdome_xml_n_attributes,
    gdome_xml_n_ownerDocument,
    gdome_xml_n_insertBefore,
    gdome_xml_n_replaceChild,
    gdome_xml_n_removeChild,
    gdome_xml_n_appendChild,
    gdome_xml_n_hasChildNodes,
    gdome_xml_n_cloneNode,
    gdome_xml_n_normalize,
    gdome_xml_n_isSupported,
    gdome_xml_n_namespaceURI,
    gdome_xml_n_prefix,
    gdome_xml_n_set_prefix,
    gdome_xml_n_localName,
    gdome_xml_n_hasAttributes,
    gdome_xml_n_addEventListener,
    gdome_xml_n_removeEventListener,
    gdome_xml_n_dispatchEvent,
    gdome_xml_n_subTreeDispatchEvent
  },
  gdome_xml_cd_data,
  gdome_xml_cd_set_data,
  gdome_xml_cd_length,
  gdome_xml_cd_substringData,
  gdome_xml_cd_appendData,
  gdome_xml_cd_insertData,
  gdome_xml_cd_deleteData,
  gdome_xml_cd_replaceData
};

/*
 * Some helper functions to work with UTF-8 string as they were UTF-16 string
 *                                  BEGIN
 */

/**
 * gdome_utf16Length:
 * @utf8str:  The UTF-8 string
 *
 * Returns: the length of the string speciefied in UTF-16 units.
 */
gulong
gdome_utf16Length (xmlChar *utf8str) {
	gulong len = 0L, i;
	char c;

	for (i = 0L; (c = utf8str[i]) != '\0'; i++)
		if ((c & 0xf8) == 0xf0)               /* FIXME: I'm not sure this is */
			len += 2L;                          /*        correct.             */
		else if ((c & 0xc0) != 0x80)
			len++;

	return len;
}

/**
 * gdome_utf16Offeset:
 * @utf8str:  The UTF-8 string
 * @utf16offset:  The UTF-16 offset to find
 * @utf8offset:  The pointer where place the resulted UTF-8 offset
 *
 * Sets the UTF-8 offset of the UTF-16 offset specified in @utf8offset.
 * Returns: TRUE if @utf16offset is found, FALSE if it's out of range.
 */
gboolean
gdome_utf16Offset (xmlChar *utf8str, gulong utf16offset, gulong *utf8offset) {
	gulong len = 0L, i;
	char c;

	for (i = 0L; len < utf16offset && (c = utf8str[i]) != '\0'; i++)
		if ((c & 0xf8) == 0xf0)                 /* FIXME: I'm not sure this is */
			len += 2L;                            /*        correct              */
		else if ((c & 0xc0) != 0x80)
			len++;

	for (;(utf8str[i] & 0xc0) == 0x80; i++);

	*utf8offset = i;

	if (len == utf16offset )
		return TRUE;
	else
		return FALSE;
}

/*                                   END
 * Some helper functions to work with UTF-8 string as they were UTF-16 string
 */

/**
 * gdome_xml_cd_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_cd_query_interface (GdomeNode *self, const char *interface, GdomeException *exc)
{
  Gdome_xml_CharacterData *priv = (Gdome_xml_CharacterData *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_CD (priv), NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "Node") ||
      !strcmp (interface, "CharacterData") ||
      !strcmp (interface, "EventTarget")) {
    priv->refcnt++;
    return self;
  }
  else
    return NULL;
}

/**
 * gdome_xml_cd_data:
 * @self:  The CharacterData Object ref
 * @exc:  The Exception Object ref
 *
 * Returns: the character data of the node that implements this interface.
 */
GdomeDOMString *
gdome_xml_cd_data (GdomeCharacterData *self, GdomeException *exc) {
	Gdome_xml_CharacterData *priv = (Gdome_xml_CharacterData *)self;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_CD (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	return gdome_xml_str_mkref_own (xmlNodeGetContent (priv->n));
}

/**
 * gdome_xml_cd_set_data:
 * @self:  The CharacterData Object ref
 * @data:  The data value for this node
 * @exc:  The Exception Object ref
 *
 * Sets the data value of this node.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_xml_cd_set_data (GdomeCharacterData *self, GdomeDOMString *data, GdomeException *exc)
{
	Gdome_xml_CharacterData *priv = (Gdome_xml_CharacterData *)self;
  GdomeMutationEvent *mev;
	GdomeDOMString *prevValue;
  GdomeNode *parent;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_CD (priv));
	g_return_if_fail (data != NULL);
	g_return_if_fail (exc != NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

  prevValue = gdome_xml_cd_data (self, exc);
	xmlNodeSetContent (priv->n, data->str);

	if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMCharacterDataModified */
		mev = gdome_evt_mevnt_mkref ();
		gdome_str_ref (data);
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE,
																						 TRUE, FALSE, NULL,
																						 prevValue, data, NULL, 0, exc);
		gdome_xml_n_dispatchEvent ((GdomeNode *)self, (GdomeEvent *)mev, exc);
		gdome_xml_str_unref (data);
		gdome_xml_str_unref (prevValue);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}

  if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMSubtreeModified */
		parent = gdome_xml_n_mkref (gdome_xmlGetParent (priv->n));
		if (parent != NULL) {
			mev = gdome_evt_mevnt_mkref ();
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, NULL,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
			gdome_xml_n_unref (parent, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}
	}
}

/**
 * gdome_xml_cd_length:
 * @self:  The CharacterData Object ref
 * @exc:  The GdomeException Object ref
 *
 * Returns: The number of 16-bit units that are available. This may have the
 * value zero, i.e., CharacterData nodes may be empty.
 */
gulong
gdome_xml_cd_length (GdomeCharacterData *self, GdomeException *exc)
{
	Gdome_xml_CharacterData *priv = (Gdome_xml_CharacterData *)self;
	xmlChar *content;
	gulong length;

	g_return_val_if_fail (priv != NULL, 0L);
	g_return_val_if_fail (GDOME_XML_IS_CD (priv), 0L);
	g_return_val_if_fail (exc != NULL, 0L);

	content = xmlNodeGetContent (priv->n);
	length = gdome_utf16Length (content);
	g_free (content);
	return length;
}

/**
 * gdome_xml_cd_substringData:
 * @self:  The CharacterData Object ref
 * @offset:  Start offset of substring to extract
 * @count:  The number of 16-bit units to extract
 * @exc:  The Exception Object ref
 *
 * Extracts a range of data from the node.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * Returns: The specified substring. If the sum of @offset and @count exceeds
 * the length, then all 16-bit units to the end of the data are returned.
 */
GdomeDOMString *
gdome_xml_cd_substringData (GdomeCharacterData *self, gulong offset, gulong count, GdomeException *exc)
{
	Gdome_xml_CharacterData *priv = (Gdome_xml_CharacterData *)self;
	xmlChar *ret = NULL;
	xmlChar *str = NULL;
	gulong start, stop;

	g_return_val_if_fail (priv != NULL, NULL);
	g_return_val_if_fail (GDOME_XML_IS_CD (priv), NULL);
	g_return_val_if_fail (exc != NULL, NULL);

	str = xmlNodeGetContent(priv->n);

	if (!gdome_utf16Offset (str, offset, &start)) {
		xmlFree (str);
		*exc = GDOME_INDEX_SIZE_ERR;
		return NULL;
	}

	gdome_utf16Offset (str, offset+count, &stop);

	ret = (xmlChar *)xmlMalloc (stop - start + 1 * sizeof (xmlChar));
	memcpy (ret, str + start, stop - start);
	ret[stop - start] = '\0';

	xmlFree (str);
	return gdome_xml_str_mkref_own (ret);
}

/**
 * gdome_xml_cd_appendData:
 * @self:  The CharacterData Object ref
 * @arg:  The string to append
 * @exc:  The Exception Object ref
 *
 * Append the string to the end of the character data of the node. Upon
 * success, data provides access to the concatenation of data and the
 * string specified.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_xml_cd_appendData (GdomeCharacterData *self, GdomeDOMString *arg, GdomeException *exc)
{
	Gdome_xml_CharacterData *priv = (Gdome_xml_CharacterData *)self;
  GdomeMutationEvent *mev;
  GdomeDOMString *prevValue, *newValue;
  GdomeNode *parent;
	xmlChar *old_str, *new_str = NULL;
	int len1 = 0, len2 = 0;

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_CD (priv));
	g_return_if_fail (arg != NULL);
	g_return_if_fail (exc != NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

	old_str = xmlNodeGetContent (priv->n);
	len1 = strlen (old_str);
	len2 = strlen (arg->str);
	new_str = xmlMalloc (len1 + len2 + 1 * sizeof (xmlChar));
	memcpy (new_str, old_str, len1);
	memcpy (new_str + len1, arg->str, len2 + 1);

  xmlNodeSetContent (priv->n, new_str);

	if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMCharacterDataModified */
		mev = gdome_evt_mevnt_mkref ();
		prevValue = gdome_xml_str_mkref (old_str);
		newValue = gdome_xml_str_mkref (new_str);
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE,
																						 TRUE, FALSE, NULL,
																						 prevValue, newValue, NULL, 0, exc);
		gdome_xml_n_dispatchEvent ((GdomeNode *)self, (GdomeEvent *)mev, exc);
		gdome_xml_str_unref (newValue);
		gdome_xml_str_unref (prevValue);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}
	g_free (new_str);

  if (old_str != NULL)
    xmlFree (old_str);
	
	if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMSubtreeModified */
		parent = gdome_xml_n_mkref (gdome_xmlGetParent (priv->n));
		if (parent != NULL) {
			mev = gdome_evt_mevnt_mkref ();
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, NULL,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
			gdome_xml_n_unref (parent, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}
	}
}

/**
 * gdome_xml_cd_insertData:
 * @self:  The CharacterData Object ref
 * @offset:  The character offset at which to insert
 * @arg:  The string to insert
 * @exc:  The Exception Object ref
 *
 * Insert a string at the specified 16-bit unit offset.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_xml_cd_insertData (GdomeCharacterData *self, gulong offset, GdomeDOMString *arg, GdomeException *exc)
{
	Gdome_xml_CharacterData *priv = (Gdome_xml_CharacterData *)self;
  GdomeMutationEvent *mev;
  GdomeDOMString *prevValue, *newValue;
  GdomeNode *parent;
	xmlChar *old_str, *new_str = NULL;
	int len1 = 0, len2 = 0;
	gulong start;

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_CD (priv));
	g_return_if_fail (arg != NULL);
	g_return_if_fail (exc != NULL);

	old_str = xmlNodeGetContent (priv->n);
	len1 = strlen (old_str);
	len2 = strlen (arg->str);

	if (!gdome_utf16Offset (old_str, offset, &start)) {
		xmlFree (old_str);
		*exc = GDOME_INDEX_SIZE_ERR;
		return;
	}

	new_str = xmlMalloc (len1 + len2 + 1 * sizeof (xmlChar));
	memcpy (new_str, old_str, start);
	memcpy (new_str + start, arg->str, len2);
  memcpy (new_str + start + len2, old_str + start, len1 - start + 1);

	xmlNodeSetContent (priv->n, new_str);
	
	if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMCharacterDataModified */
		mev = gdome_evt_mevnt_mkref ();
		prevValue = gdome_xml_str_mkref (old_str);
		newValue = gdome_xml_str_mkref (new_str);
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE,
																						 TRUE, FALSE, NULL,
																						 prevValue, newValue, NULL, 0, exc);
		gdome_xml_n_dispatchEvent ((GdomeNode *)self, (GdomeEvent *)mev, exc);
		gdome_xml_str_unref (newValue);
		gdome_xml_str_unref (prevValue);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}
	g_free (new_str);

  if (old_str != NULL)
    xmlFree (old_str);

	if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMSubtreeModified */
		parent = gdome_xml_n_mkref (gdome_xmlGetParent (priv->n));
		if (parent != NULL) {
			mev = gdome_evt_mevnt_mkref ();
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, NULL,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
			gdome_xml_n_unref (parent, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}
	}
}

/**
 * gdome_xml_cd_deleteData:
 * @self:  The CharacterData Object ref
 * @offset:  The offset from which to start removing
 * @count:  The number of 16-bit units to delete.
 * @exc:  The Exception Object ref
 *
 * Remove a range of 16-bit units from the node. If the sum of @offset and
 * @count exceeds length then all 16-bit units from @offset to the end of the
 * data are deleted. Upon success, data and length reflect the change.
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_xml_cd_deleteData (GdomeCharacterData *self, gulong offset, gulong count, GdomeException *exc)
{
	Gdome_xml_CharacterData *priv = (Gdome_xml_CharacterData *)self;
  GdomeMutationEvent *mev;
  GdomeDOMString *prevValue, *newValue;
  GdomeNode *parent;
	xmlChar *old_str, *new_str = NULL;
	gulong start, stop, len1, len2;

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

	g_return_if_fail (priv != NULL);
	g_return_if_fail (GDOME_XML_IS_CD (priv));
	g_return_if_fail (exc != NULL);

	old_str = xmlNodeGetContent (priv->n);
	if (!gdome_utf16Offset (old_str, offset, &start)) {
		xmlFree (old_str);
		*exc = GDOME_INDEX_SIZE_ERR;
		return;
	}

	gdome_utf16Offset (old_str, offset+count, &stop);
  len1 = strlen (old_str);
	len2 = len1 - (stop - start);
	new_str = (xmlChar *)xmlMalloc (len2 + 1 * sizeof (xmlChar));
	memcpy (new_str, old_str, start);
	memcpy (new_str + start, old_str + stop, len1 - stop);
  new_str[len2] = '\0';

	xmlNodeSetContent (priv->n, new_str);

	if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMCharacterDataModified */
		mev = gdome_evt_mevnt_mkref ();
		prevValue = gdome_xml_str_mkref (old_str);
		newValue = gdome_xml_str_mkref (new_str);
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE,
																						 TRUE, FALSE, NULL,
																						 prevValue, newValue, NULL, 0, exc);
		gdome_xml_n_dispatchEvent ((GdomeNode *)self, (GdomeEvent *)mev, exc);
		gdome_xml_str_unref (newValue);
		gdome_xml_str_unref (prevValue);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}
	g_free (new_str);

  if (old_str != NULL)
    xmlFree (old_str);

	if (gdome_xml_n_eventEnabledByCode((GdomeNode *) self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMSubtreeModified */
		parent = gdome_xml_n_mkref (gdome_xmlGetParent (priv->n));
		if (parent != NULL) {
			mev = gdome_evt_mevnt_mkref ();
			gdome_evt_mevnt_initMutationEventByCode (mev, DOM_SUBTREE_MODIFIED_EVENT_TYPE,
																							 TRUE, FALSE, NULL,
																							 NULL, NULL, NULL, 0, exc);
			gdome_xml_n_dispatchEvent (parent, (GdomeEvent *)mev, exc);
			gdome_xml_n_unref (parent, exc);
			gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
		}
	}
}

/**
 * gdome_xml_cd_replaceData:
 * @self:  The CharacterData Object ref
 * @offset:  The offset from which to start replacing
 * @count:  The number of 16-bit units to replace.
 * @arg:  The String with which the range must be replaced
 * @exc:  The GdomeException Object ref
 *
 * Replace the characters starting at the specified 16-bit unit @offset with
 * the specified string. If the sum of @offset and @count exceeds length, then
 * all 16-bit units to the end of the data are replaced;
 *
 * %GDOME_INDEX_SIZE_ERR: Raised if the specified @offset is greater than the
 * number of 16-bit units in data.
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_xml_cd_replaceData (GdomeCharacterData *self, gulong offset, gulong count, GdomeDOMString *arg, GdomeException *exc)
{
	g_return_if_fail (self != NULL);
	g_return_if_fail (GDOME_XML_IS_CD (self));
	g_return_if_fail (arg != NULL);
	g_return_if_fail (exc != NULL);

	gdome_xml_cd_deleteData (self, offset, count, exc);
	if (*exc == 0) {
		gdome_xml_cd_insertData (self, offset, arg, exc);
    if (*exc == GDOME_INDEX_SIZE_ERR) {
      *exc = 0;
      gdome_xml_cd_appendData (self, arg, exc);
    }
	}
}
