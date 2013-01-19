/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-xml-pi.c
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
#include "gdome-xml-util.h"
#include "gdome-xml-str.h"
#include "gdome-xml-node.h"
#include "gdome-xml-xmlutil.h"
#include "gdome-evt-event.h"
#include "gdome-evt-mevent.h"
#include "gdome-evt-propagation.h"
#include "gdome-xml-pi.h"
#include "gdome-xml-document.h"

const GdomeProcessingInstructionVtab gdome_xml_pi_vtab = {
  {
    gdome_xml_n_ref,
    gdome_xml_n_unref,
    gdome_xml_pi_query_interface,
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
  gdome_xml_pi_target,
  gdome_xml_pi_data,
  gdome_xml_pi_set_data
};

/**
 * gdome_xml_pi_query_interface:
 * @self:  Node Object ref
 * @interface:  interface needed
 * @exc:  Exception Object ref
 *
 * Returns: a reference to this object that implements the @interface needed,
 *          or %NULL if the @interface is not supported by this Object.
 */
gpointer
gdome_xml_pi_query_interface (GdomeNode *self, const char *interface, GdomeException *exc)
{
	Gdome_xml_ProcessingInstruction *priv = (Gdome_xml_ProcessingInstruction *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_PI (priv), NULL);
  g_return_val_if_fail (interface != NULL, NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  if (!strcmp (interface, "Node") ||
      !strcmp (interface, "ProcessingInstruction") ||
      !strcmp (interface, "EventTarget")) {
    priv->refcnt++;
    return self;
  }
  else
    return NULL;
}

/**
 * gdome_xml_pi_target:
 * @self:  ProcessingInstruction Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the target of this processing instruction. XML defines this as being
 * the first token following the markup that begins the processing instruction.
 */
GdomeDOMString *
gdome_xml_pi_target (GdomeProcessingInstruction *self, GdomeException *exc)
{
  Gdome_xml_ProcessingInstruction *priv = (Gdome_xml_ProcessingInstruction *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_PI (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

  return gdome_xml_str_mkref_dup((gchar *)gdome_xmlGetName (priv->n));
}

/**
 * gdome_xml_pi_data:
 * @self:  ProcessingInstruction Object ref
 * @exc:  Exception Object ref
 *
 * Returns: the content of this processing instruction. This is from the first
 * non white space character after the target to the character immediately
 * preceding the %?>.
 */
GdomeDOMString *
gdome_xml_pi_data (GdomeProcessingInstruction *self, GdomeException *exc)
{
  Gdome_xml_ProcessingInstruction *priv = (Gdome_xml_ProcessingInstruction *)self;

  g_return_val_if_fail (priv != NULL, NULL);
  g_return_val_if_fail (GDOME_XML_IS_PI (priv), NULL);
  g_return_val_if_fail (exc != NULL, NULL);

 	return gdome_xml_str_mkref_own (xmlNodeGetContent (priv->n));
}

/**
 * gdome_xml_pi_set_data:
 * @self:  ProcessingInstruction Object ref
 * @data:  The string of data to be set
 * @exc:  Exception Object ref
 *
 * Set the content of this processing instruction.
 *
 * %GDOME_NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
 */
void
gdome_xml_pi_set_data (GdomeProcessingInstruction *self, GdomeDOMString *data, GdomeException *exc)
{
	Gdome_xml_ProcessingInstruction *priv = (Gdome_xml_ProcessingInstruction *)self;
  GdomeMutationEvent *mev;
	GdomeDOMString *prevValue;
  GdomeNode *parent;

  g_return_if_fail (priv != NULL);
  g_return_if_fail (GDOME_XML_IS_PI (priv));
  g_return_if_fail (data != NULL);
  g_return_if_fail (exc != NULL);

  if (GDOME_ISREADONLY (priv)) {
    *exc = GDOME_NO_MODIFICATION_ALLOWED_ERR;
    return;
  }

  xmlNodeSetContent (priv->n, (xmlChar *)data->str);

	if (gdome_xml_n_eventEnabledByCode ((GdomeNode *) self, DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE)) {
		/* Fire DOMCharacterDataModified */
		mev = gdome_evt_mevnt_mkref ();
		prevValue = gdome_xml_pi_data (self, exc);
		gdome_str_ref (data);
		gdome_evt_mevnt_initMutationEventByCode (mev, DOM_CHARACTER_DATA_MODIFIED_EVENT_TYPE,
																						 TRUE, FALSE, NULL,
																						 prevValue, data, NULL, 0, exc);
		gdome_xml_n_dispatchEvent ((GdomeNode *)self, (GdomeEvent *)mev, exc);
		gdome_xml_str_unref (data);
		gdome_xml_str_unref (prevValue);
		gdome_evt_mevnt_unref ((GdomeEvent *)mev, exc);
	}

	if (gdome_xml_n_eventEnabledByCode ((GdomeNode *) self, DOM_SUBTREE_MODIFIED_EVENT_TYPE)) {
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
