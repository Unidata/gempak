/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-util.c
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

#include <gdome.h>
#include <libxml/tree.h>
#include "gdome-xml-node.h"
#include "gdome-xml-util.h"
#include "gdome-evt-util.h"
#include "gdome-evt-event.h"

GdomeAttr *
gdome_cast_a (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_A(node))
		return NULL;
  else
		return (GdomeAttr *)node;
}

GdomeCharacterData *
gdome_cast_cd (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_CD(node))
		return NULL;
  else
		return (GdomeCharacterData *)node;
}

GdomeCDATASection *
gdome_cast_cds (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_CDS(node))
		return NULL;
  else
		return (GdomeCDATASection *)node;
}

GdomeComment *
gdome_cast_c (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_C(node))
		return NULL;
  else
		return (GdomeComment *)node;
}

GdomeDocument *
gdome_cast_doc (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_DOC(node))
		return NULL;
  else
		return (GdomeDocument *)node;
}

GdomeDocumentFragment *
gdome_cast_df (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_DF(node))
		return NULL;
  else
		return (GdomeDocumentFragment *)node;
}

GdomeDocumentType *
gdome_cast_dt (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_DT(node))
		return NULL;
  else
		return (GdomeDocumentType *)node;
}

GdomeElement *
gdome_cast_el (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_EL(node))
		return NULL;
  else
		return (GdomeElement *)node;
}

GdomeEntity *
gdome_cast_ent (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_ENT(node))
		return NULL;
  else
		return (GdomeEntity *)node;
}

GdomeEntityReference *
gdome_cast_er (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_ER(node))
		return NULL;
  else
		return (GdomeEntityReference *)node;
}

GdomeNode *
gdome_cast_n (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_N(node))
		return NULL;
  else
		return (GdomeNode *)node;
}

GdomeNotation *
gdome_cast_not (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_NOT(node))
		return NULL;
  else
		return (GdomeNotation *)node;
}

GdomeProcessingInstruction *
gdome_cast_pi (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_PI(node))
		return NULL;
	else
		return (GdomeProcessingInstruction *)node;
}

GdomeText *
gdome_cast_t (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_T(node))
		return NULL;
  else
		return (GdomeText *)node;
}

GdomeEventTarget *
gdome_cast_evntt (GdomeNode *node) {
	if (node == NULL || !GDOME_XML_IS_N(node))
		return NULL;
  else
		return (GdomeEventTarget *)node;
}

GdomeEvent *
gdome_cast_evnt (GdomeEvent *evt) {
	if (evt == NULL || !GDOME_XML_IS_EVNT(evt))
		return NULL;
  else
		return (GdomeEvent *)evt;
}

GdomeMutationEvent *
gdome_cast_mevnt (GdomeEvent *evt) {
	if (evt == NULL || !GDOME_XML_IS_MEVNT(evt))
		return NULL;
  else
		return (GdomeMutationEvent *)evt;
}
