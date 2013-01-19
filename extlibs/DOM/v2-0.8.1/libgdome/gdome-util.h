/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-util.h
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

#ifndef GDOME_UTIL_H
#define GDOME_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

GdomeAttr *                  gdome_cast_a     (GdomeNode *node);
GdomeCharacterData *         gdome_cast_cd    (GdomeNode *node);
GdomeCDATASection *          gdome_cast_cds   (GdomeNode *node);
GdomeComment *               gdome_cast_c     (GdomeNode *node);
GdomeDocument *              gdome_cast_doc   (GdomeNode *node);
GdomeDocumentFragment *      gdome_cast_df    (GdomeNode *node);
GdomeDocumentType *          gdome_cast_dt    (GdomeNode *node);
GdomeElement *               gdome_cast_el    (GdomeNode *node);
GdomeEntity *                gdome_cast_ent   (GdomeNode *node);
GdomeEntityReference *       gdome_cast_er    (GdomeNode *node);
GdomeNode *                  gdome_cast_n     (GdomeNode *node);
GdomeNotation *              gdome_cast_not   (GdomeNode *node);
GdomeProcessingInstruction * gdome_cast_pi    (GdomeNode *node);
GdomeText *                  gdome_cast_t     (GdomeNode *node);
GdomeEventTarget *           gdome_cast_evntt (GdomeNode *node);

GdomeEvent *                 gdome_cast_evnt  (GdomeEvent *evt);
GdomeMutationEvent *         gdome_cast_mevnt (GdomeEvent *evt);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* GDOME_UTIL_H */
