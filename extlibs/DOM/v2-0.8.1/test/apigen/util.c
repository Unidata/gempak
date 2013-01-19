/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* util.c
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

/* Certo, capisci, tipo "Aspettando Godot", la parabola dell'attesa estraniata dal suo contesto sociale, come metafora della condizione dell'uomo di fronte al tempo in quanto altro da lui. */

#include <stdio.h>
#include <ctype.h>
#include <gdome.h>
#include "util.h"

int isSpaceStr (GdomeDOMString *domstr) {
	gchar *str = domstr->str;

	for (; *str; str++)
		if (!isspace(*str))
			return 0;
	return 1;
}

void cleanSubTree (GdomeNode *node) {
	GdomeNode *child = NULL, *dchild = NULL;
	GdomeNodeList *nl = NULL;
	GdomeException exc;
  GdomeDOMString *str;
	long i, nllength;

	nl = gdome_n_childNodes (node, &exc);
	if ((nllength = gdome_nl_length (nl, &exc)) == 0) {
    if (nl != NULL)
			gdome_nl_unref (nl, &exc);
		return;
	}

	for (i = nllength-1; i >= 0; i--) {
		child = gdome_nl_item (nl, i, &exc);
    str = NULL;

		if (gdome_n_nodeType (child, &exc) == GDOME_COMMENT_NODE ||
				(gdome_n_nodeType (child, &exc) == GDOME_TEXT_NODE &&
				 isSpaceStr ((str = gdome_t_data ((GdomeText *)child, &exc))))) {
			dchild = gdome_n_removeChild (node, child, &exc);

			if (str != NULL)
				gdome_str_unref (str);
			if (dchild != NULL)
				gdome_n_unref (dchild, &exc);
		}
		else if (gdome_n_hasChildNodes (child, &exc))
			cleanSubTree (child);

    if (child != NULL)
			gdome_n_unref (child, &exc);
	}

	if (nl != NULL)
    gdome_nl_unref (nl, &exc);
}

GdomeDOMString *getElContent (GdomeElement *el) {
  GdomeNode *child;
  GdomeException exc;

  g_return_val_if_fail (el != NULL, NULL);

  child = gdome_el_firstChild (el, &exc);
	if (el == NULL)
    fprintf (stderr, "CHILDNULL");
  g_return_val_if_fail (child != NULL, NULL);

  if (gdome_n_nodeType (child, &exc) != GDOME_TEXT_NODE &&
      gdome_n_nodeType (child, &exc) != GDOME_CDATA_SECTION_NODE) {
    gdome_n_unref (child, &exc);
    return NULL;
  }

  return gdome_t_data ((GdomeText *)child, &exc);
}
