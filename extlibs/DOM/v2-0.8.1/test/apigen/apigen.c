/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* apigen.c
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

#include <string.h>
#include <stdio.h>
#include <glib.h>
#include <gdome.h>
#include "srcutil.h"
#include "util.h"

#define MAXPARAMS 10
#define MAXEXCEPTION 3

module modInfo;
int writtenAPI = 0;

void
getModuleInfo (GdomeElement *root) {
  GdomeElement *el;
  GdomeNodeList *childs;
  GdomeDOMString *str;
  GdomeException exc;

  childs = gdome_el_childNodes (root, &exc);

  /* SRCDIR */
  el = (GdomeElement *)gdome_nl_item (childs, 0, &exc);
  str = getElContent (el);
  modInfo.srcdir = g_strdup (str->str);
  gdome_str_unref (str);
  gdome_el_unref (el, &exc);

  /* OUTFILE */
  el = (GdomeElement *)gdome_nl_item (childs, 1, &exc);
  str = getElContent (el);
  modInfo.outfile = g_strdup (str->str);
  gdome_str_unref (str);
  gdome_el_unref (el, &exc);

  /* APIPREFIX */
  el = (GdomeElement *)gdome_nl_item (childs, 2, &exc);
  str = getElContent (el);
  modInfo.apiPrefix = g_strdup (str->str);
  gdome_str_unref (str);
  gdome_el_unref (el, &exc);

  /* MODPREFIX */
  el = (GdomeElement *)gdome_nl_item (childs, 3, &exc);
  str = getElContent (el);
  modInfo.modPrefix = g_strdup (str->str);
  gdome_str_unref (str);
  gdome_el_unref (el, &exc);

  /* OUTFILEHEAD */
	el = (GdomeElement *)gdome_nl_item (childs, 4, &exc);
	if (el == NULL)
    fprintf (stderr, "ELNULL");
  str = getElContent (el);
  modInfo.outfilehead = g_strdup (str->str);
  gdome_str_unref (str);
  gdome_el_unref (el, &exc);
}

void createInterfaceAPI (GdomeElement *el, GdomeElement *ancestor, unsigned int degree) {
	GdomeNode *child = NULL;
  GdomeNode *parent = NULL;
  GdomeNode *param = NULL;
  GdomeNode *exception = NULL;
	GdomeNode *ptmp = NULL;
  GdomeElement *implementation = NULL;
	GdomeNodeList *nl = NULL, *nlparams = NULL, *nlexceptions = NULL, *nlimplementation = NULL;
	GdomeException exc;
  GdomeDOMString *nameAttr, *nameMethod, *nameElement;
  GdomeDOMString *name, *srcfile, *iPrefix, *aPrefix = NULL, *content;
  GdomeDOMString *aName = NULL, *type, *readonly, *str_impl = NULL;
  GdomeDOMString *tmp1, *tmp2, *tmp3;
  struct _param params[MAXPARAMS];
  struct _exception exceptions[MAXEXCEPTION];
  unsigned long i, j, nllength, nlplength, nlelength;
  gchar *aPrefixStr = NULL, *aNameStr = NULL;
  gboolean roflag, rawflag;
  int y;

  /* Init const strings */
	nameAttr = gdome_str_mkref ("ATTR");
	nameMethod = gdome_str_mkref ("METHOD");
  nameElement = gdome_str_mkref ("INTERFACE");

	/* Init strings */
  tmp1 = gdome_str_mkref ("NAME");
  name = gdome_el_getAttribute (el, tmp1, &exc);
  gdome_str_unref (tmp1);
  tmp1 = gdome_str_mkref ("PREFIX");
  iPrefix = gdome_el_getAttribute (el, tmp1, &exc);
  gdome_str_unref (tmp1);
	if (ancestor == NULL) {
		printf ("%s:\n", name->str);
    writeTitle(&modInfo, name->str);

    tmp1 = gdome_str_mkref ("SRC");
		srcfile = gdome_el_getAttribute (el, tmp1, &exc);
    gdome_str_unref (tmp1);
  }
  else {
		tmp1 = gdome_str_mkref ("NAME");
		aName = gdome_el_getAttribute (ancestor, tmp1, &exc);
		gdome_str_unref (tmp1);
    aNameStr = aName->str;
		tmp1 = gdome_str_mkref ("SRC");
		srcfile = gdome_el_getAttribute (ancestor, tmp1, &exc);
		gdome_str_unref (tmp1);
    tmp1 = gdome_str_mkref ("PREFIX");
		aPrefix = gdome_el_getAttribute (ancestor, tmp1, &exc);
    gdome_str_unref (tmp1);
    aPrefixStr = aPrefix->str;
  }

  /* Creation of the childrens list*/
  if (ancestor == NULL)
		nl = gdome_el_childNodes (el, &exc);
	else
    nl = gdome_el_childNodes (ancestor, &exc);
	nllength = gdome_nl_length (nl, &exc);

  /* Creation of the local prototypes */
	for (i = 0; i < nllength; i++) {
		child = gdome_nl_item (nl, i, &exc);
    tmp1 = gdome_n_nodeName (child, &exc);

    /* If it's an Attribute */
    if (gdome_str_equal (tmp1, nameAttr)) {

      /* get its type */
			tmp2 = gdome_str_mkref ("TYPE");
			type = gdome_el_getAttribute ((GdomeElement *)child, tmp2, &exc);
      gdome_str_unref (tmp2);

      /* get its name */
			tmp2 = gdome_str_mkref ("NAME");
      content = gdome_el_getAttribute ((GdomeElement *)child, tmp2, &exc);
      gdome_str_unref (tmp2);

      /* set roflag (readonly)*/
      tmp2 = gdome_str_mkref ("READONLY");
      readonly = gdome_el_getAttribute ((GdomeElement *)child, tmp2, &exc);
      gdome_str_unref (tmp2);
      tmp2 = gdome_str_mkref ("YES");
      if (gdome_str_equal (readonly, tmp2))
        roflag = TRUE;
      else
        roflag = FALSE;
      gdome_str_unref (tmp2);

      /* fill info on exceptions */
      tmp2 = gdome_str_mkref ("EXCEPTION");
      nlexceptions = gdome_el_getElementsByTagName ((GdomeElement *)child, tmp2, &exc);
      gdome_str_unref (tmp2);
      nlelength = gdome_nl_length (nlexceptions, &exc);
      for (j=0; j<nlelength; j++) {
        exception = gdome_nl_item (nlexceptions, j, &exc);

        /* get exception type */
        tmp2 = gdome_str_mkref ("TYPE");
        tmp3 = gdome_el_getAttribute ((GdomeElement *)exception, tmp2, &exc);
        gdome_str_unref (tmp2);
        exceptions[j].type = g_strdup(tmp3->str);
        gdome_str_unref (tmp3);

        /* get exception on */
        tmp2 = gdome_str_mkref ("ON");
        tmp3 = gdome_el_getAttribute ((GdomeElement *)exception, tmp2, &exc);
        gdome_str_unref (tmp2);
        exceptions[j].on = g_strdup(tmp3->str);
        gdome_str_unref (tmp3);

        gdome_n_unref (exception, &exc);
      }
      exceptions[j].type = exceptions[j].on = NULL;
      gdome_nl_unref (nlexceptions, &exc);

 			if (!writeAttribute(&modInfo, srcfile->str, iPrefix->str, type->str, content->str, aPrefixStr, aNameStr, name->str, degree, roflag, exceptions))
 				printf ("a: %s %d NO\n", content->str, degree);

      gdome_str_unref (content);
      gdome_str_unref (type);
      gdome_str_unref (readonly);
		}
    /* If it's a Method */
		else if (gdome_str_equal (tmp1, nameMethod)) {
      /* get its type */
			tmp2 = gdome_str_mkref ("TYPE");
			type = gdome_el_getAttribute ((GdomeElement *)child, tmp2, &exc);
      gdome_str_unref (tmp2);

      /* get its name */
			tmp2 = gdome_str_mkref ("NAME");
			content = gdome_el_getAttribute ((GdomeElement *)child, tmp2, &exc);
      gdome_str_unref (tmp2);

			/* set rawflag (RAW)*/
      rawflag = FALSE;
      tmp2 = gdome_str_mkref ("RAW");
			if (gdome_el_hasAttribute ((GdomeElement *)child, tmp2, &exc)) {
				tmp3 = gdome_el_getAttribute ((GdomeElement *)child, tmp2, &exc);
				gdome_str_unref (tmp2);
				tmp2 = gdome_str_mkref ("YES");
				if (gdome_str_equal (tmp3, tmp2))
					rawflag = TRUE;
			}
			gdome_str_unref (tmp2);

      /* fill info on exceptions */
      tmp2 = gdome_str_mkref ("EXCEPTION");
      nlexceptions = gdome_el_getElementsByTagName ((GdomeElement *)child, tmp2, &exc);
      gdome_str_unref (tmp2);
      nlelength = gdome_nl_length (nlexceptions, &exc);
      for (j=0; j<nlelength; j++) {
        exception = gdome_nl_item (nlexceptions, j, &exc);

        /* get exception type */
        tmp2 = gdome_str_mkref ("TYPE");
        tmp3 = gdome_el_getAttribute ((GdomeElement *)exception, tmp2, &exc);
        gdome_str_unref (tmp2);
        exceptions[j].type = g_strdup(tmp3->str);
        gdome_str_unref (tmp3);

        /* get exception on */
        tmp2 = gdome_str_mkref ("ON");
        tmp3 = gdome_el_getAttribute ((GdomeElement *)exception, tmp2, &exc);
        gdome_str_unref (tmp2);
        exceptions[j].on = g_strdup(tmp3->str);
        gdome_str_unref (tmp3);

        gdome_n_unref (exception, &exc);
      }
      exceptions[j].type = exceptions[j].on = NULL;
      gdome_nl_unref (nlexceptions, &exc);

      /* fill info on params */
      tmp2 = gdome_str_mkref ("PARAM");
      nlparams = gdome_el_getElementsByTagName ((GdomeElement *)child, tmp2, &exc);
      gdome_str_unref (tmp2);
      nlplength = gdome_nl_length (nlparams, &exc);
      for (j=0; j<nlplength; j++) {
        param = gdome_nl_item (nlparams, j, &exc);

        /* get param type */
        tmp2 = gdome_str_mkref ("TYPE");
        tmp3 = gdome_el_getAttribute ((GdomeElement *)param, tmp2, &exc);
        gdome_str_unref (tmp2);
        params[j].type = g_strdup(tmp3->str);
        gdome_str_unref (tmp3);

        /* get param name */
        tmp2 = gdome_str_mkref ("NAME");
        tmp3 = gdome_el_getAttribute ((GdomeElement *)param, tmp2, &exc);
        gdome_str_unref (tmp2);
        params[j].name = g_strdup(tmp3->str);
        gdome_str_unref (tmp3);

        gdome_n_unref (param, &exc);
      }
      params[j].type = params[j].name = NULL;
      gdome_nl_unref (nlparams, &exc);

      /* fill implementation data */
      str_impl = NULL;
			tmp2 = gdome_str_mkref ("IMPLEMENTATION");
      nlimplementation = gdome_el_getElementsByTagName ((GdomeElement *)child, tmp2, &exc);
      gdome_str_unref (tmp2);
			if (gdome_nl_length (nlimplementation, &exc) == 1) {
				implementation = (GdomeElement *)gdome_nl_item (nlimplementation, 0L, &exc);
        str_impl = getElContent(implementation);
        gdome_el_unref (implementation, &exc);
      }
      gdome_nl_unref (nlimplementation, &exc);


      if (!rawflag || ancestor == NULL) {
        if (str_impl != NULL) {
          if (!writeMethod(&modInfo, srcfile->str, iPrefix->str, type->str, content->str, aPrefixStr, aNameStr, name->str, degree, params, rawflag, exceptions, str_impl->str))
            printf ("m: %s %d\n", content->str, degree);
          gdome_str_unref(str_impl);
          str_impl = NULL;
        } else {
          if (!writeMethod(&modInfo, srcfile->str, iPrefix->str, type->str, content->str, aPrefixStr, aNameStr, name->str, degree, params, rawflag, exceptions, NULL))
            printf ("m: %s %d\n", content->str, degree);
        }
      }

      for (j=0; params[j].type != NULL; j++) {
        g_free (params[j].type);
        g_free (params[j].name);
      }

      gdome_str_unref (content);
      gdome_str_unref (type);
		}

		gdome_str_unref (tmp1);
    gdome_n_unref (child, &exc);
	}

  /* Creation of the inherited prototypes*/
	if (ancestor == NULL) {
		parent = gdome_el_parentNode (el, &exc);
    tmp1 = gdome_el_tagName ((GdomeElement *)parent, &exc);
		for (y = 1; gdome_str_equal (tmp1, nameElement); y++) {
			gdome_str_unref (tmp1);
			createInterfaceAPI (el, (GdomeElement *)parent, y);

      ptmp = gdome_el_parentNode ((GdomeElement *)parent, &exc);
			gdome_n_unref (parent, &exc);
      gdome_n_ref (ptmp, &exc);
			parent = ptmp;
      gdome_n_unref (ptmp, &exc);
			tmp1 = gdome_el_tagName ((GdomeElement *)parent, &exc);
		}
	}

	gdome_str_unref (nameAttr);
  gdome_str_unref (nameMethod);
  gdome_str_unref (nameElement);
  gdome_str_unref (srcfile);
  gdome_str_unref (iPrefix);
  gdome_str_unref (name);
  if (aPrefix != NULL)
    gdome_str_unref (aPrefix);
	if (nl != NULL)
    gdome_nl_unref (nl, &exc);
}

void interfacesTraversal (GdomeElement *el) {
	GdomeNode *child = NULL;
	GdomeNodeList *nl = NULL;
	GdomeException exc;
  GdomeDOMString *str, *name;
	unsigned long i, nllength;

	nl = gdome_el_childNodes (el, &exc);
	if ((nllength = gdome_nl_length (nl, &exc)) == 0) {
    if (nl != NULL)
			gdome_nl_unref (nl, &exc);
		return;
	}

  name = gdome_str_mkref ("INTERFACE");
	for (i = 0L; i < nllength; i++) {
		child = gdome_nl_item (nl, i, &exc);
		str = gdome_n_nodeName (child, &exc);

		if (gdome_str_equal (str, name)) {
			createInterfaceAPI ((GdomeElement *)child, NULL, 0);
      interfacesTraversal ((GdomeElement *)child);
		}

    gdome_str_unref (str);
		gdome_n_unref (child, &exc);
	}

  gdome_str_unref (name);
	if (nl != NULL)
    gdome_nl_unref (nl, &exc);
}

int main (int argc, char **argv) {
  GdomeDOMImplementation *domimpl;
	GdomeDocument *doc;
	GdomeElement *rootel;
  GdomeException exc;
	FILE *fddest;

	if (argc < 2) {
		fprintf (stderr, "Syntax:\n"
						"\tapigen <infile.xml>\n");
		return 1;
	}

	/* First I get a DOMImplementation reference */
	domimpl = gdome_di_mkref ();

	/* I load a new document from the file name "exampleb.xml */
	doc = gdome_di_createDocFromURI(domimpl, argv[1], GDOME_LOAD_PARSING, &exc);
	if (doc == NULL) {
		fprintf (stderr, "DOMImplementation.parseFile: failed\n\tException #%d\n", exc);
		return 1;
	}

	/* I get reference to the root element of the document */
	rootel = gdome_doc_documentElement (doc, &exc);
	if (rootel == NULL) {
		fprintf (stderr, "Document.documentElement: NULL\n\tException #%d\n", exc);
		return 1;
	}

  cleanSubTree ((GdomeNode *)rootel);
  getModuleInfo (rootel);

	/* I create the outfile with ites head */
  remove (modInfo.outfile);
  if ((fddest = fopen (modInfo.outfile, "a")) == NULL)
    return 1;
	if (!fwrite (modInfo.outfilehead, strlen (modInfo.outfilehead), 1, fddest))
		return 1;
  fclose (fddest);

  /* I create the API */
  interfacesTraversal (rootel);

	/* I free the document structure and the DOMImplementation */
	gdome_di_freeDoc (domimpl, doc, &exc);
	gdome_di_unref (domimpl, &exc);


  printf ("\nWritten API: %d\n", writtenAPI);
	return 0;

}
