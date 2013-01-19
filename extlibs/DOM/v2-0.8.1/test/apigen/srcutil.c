/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* srcutil.c
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

#include <stdio.h>
#include <string.h>
#include <glib.h>
#include "srcutil.h"

#define BUFSIZE 1024
#define STARTSTR "/**\n"
#define ENDSTR " */\n"

extern int writtenAPI;

void
writeException (FILE *fddest, struct _exception *exception, const gchar *type) {
	fputs ("\tif (", fddest);
	fputs (exception->on, fddest);
	fputs (") {\n"
				 "\t\t*exc = GDOME_", fddest);
  fputs (exception->type, fddest);
	fputs (";\n", fddest);

	if (!strcmp (type, "void"))
    fputs ("\t\treturn;\n", fddest);
  else if (!strcmp (type, "GdomeBoolean"))
    fputs ("\t\treturn FALSE;\n", fddest);
  else if (!strcmp (type, "GdomeDOMTimeStamp"))
    fputs ("\t\treturn 0L;\n", fddest);
  else if (strstr (type, "long"))
    fputs ("\t\treturn 0L;\n", fddest);
  else if (strstr (type, "short"))
    fputs ("\t\treturn 0;\n", fddest);
  else if (strstr (type, "double"))
    fputs ("\t\treturn 0;\n", fddest);
  else if (strstr (type, "int32"))
    fputs ("\t\treturn 0;\n", fddest);
  else if (strstr (type, "Gdome") && strstr (type, " *"))
    fputs ("\t\treturn NULL;\n", fddest);
  else if (!strcmp (type, "gpointer"))
    fputs ("\t\treturn NULL;\n", fddest);
  else if (!strcmp (type, "void *"))
    fputs ("\t\treturn NULL;\n", fddest);
  else
    g_warning ("TYPE NOT FOUND");

	fputs ("\t}\n", fddest);
}

void
writeAttributeCall (FILE *fddest, const gchar* name, const gchar* iName, const gchar* modPrefix, const gchar* aName, int degree, gboolean setAttr) {
  gchar *buf, tmp[BUFSIZE];
  int i;

	tmp[0]='\0';

  strcat (tmp, "->");
  if (degree != 0) {
    for (i=0; i<degree; i++)
      strcat (tmp, "super.");
  }

	if (degree == 0) {
		if (setAttr)
			buf = g_strconcat ("\t((Gdome_",modPrefix,"_",iName," *)self)->vtab",tmp,"set_", name, " (self, ", name, ", exc);\n", NULL);
		else
			buf = g_strconcat ("\treturn ((Gdome_",modPrefix,"_",iName," *)self)->vtab",tmp, name, " (self, exc);\n", NULL);
	}
	else {
		if (setAttr)
			buf = g_strconcat ("\t((Gdome_",modPrefix,"_",iName," *)self)->vtab",tmp,"set_", name, " ((Gdome", aName, " *)self, ", name, ", exc);\n", NULL);
		else
			buf = g_strconcat ("\treturn ((Gdome_",modPrefix,"_",iName," *)self)->vtab",tmp, name, " ((Gdome", aName, " *)self, exc);\n", NULL);
	}
  fputs (buf, fddest);
	g_free (buf);

  writtenAPI++;
}

gboolean
writeAttribute (module *modInfo, const gchar *srcfile, const gchar *iPrefix,
                const gchar *type, const gchar *name, const gchar *aPrefix,
								const gchar *aName, const gchar *iName, int degree,
								gboolean readonly, struct _exception exceptions[]) {
  FILE *fdsrc, *fddest;
  gchar *srcFilename;
  gchar *tofind, *tmp;
  char buffer[BUFSIZE];
  int status = 1, i;
  gboolean copy = FALSE;

  srcFilename = g_strconcat (modInfo->srcdir, srcfile, NULL);
  if ((fdsrc = fopen (srcFilename, "r")) == NULL) {
    g_free (srcFilename);
    return FALSE;
  }
  g_free (srcFilename);

  if ((fddest = fopen (modInfo->outfile, "a")) == NULL) {
    return FALSE;
  }

  if (aPrefix == NULL)
    aPrefix = iPrefix;

  if (!readonly)
    status++;

  tofind = g_strconcat (" * ", modInfo->apiPrefix, "_", modInfo->modPrefix, "_", aPrefix, "_", name, ":\n", NULL);
  while (status && fgets (buffer, BUFSIZE, fdsrc)) {
    if (copy) {
      fputs (buffer, fddest);
      if (!strcmp (buffer, ENDSTR)) {
        if (readonly || status == 2)
          tmp = g_strconcat (type, "\n", NULL);
        else
          tmp = g_strdup ("void\n");
        fputs (tmp, fddest);
        g_free (tmp);
        if (readonly || status == 2)
          tmp = g_strconcat (modInfo->apiPrefix, "_", iPrefix, "_", name, " (Gdome", iName, " *self, GdomeException *exc)\n", NULL);
        else
          tmp = g_strconcat (modInfo->apiPrefix, "_", iPrefix, "_set_", name, " (Gdome", iName, " *self, ", type, name, ", GdomeException *exc)\n", NULL);
        fputs (tmp, fddest);
        g_free (tmp);
        fputs ("{\n",fddest);
				for (i=0; exceptions[i].type != NULL; i++)
					if (readonly || status == 2)
						writeException (fddest, &exceptions[i], type);
					else
						writeException (fddest, &exceptions[i], "void");
        fputs ("\t*exc = 0;\n", fddest);
        writeAttributeCall(fddest, name, iName, modInfo->modPrefix, aName, degree, !(readonly || status == 2));
        fputs ("}\n",fddest);

        copy = FALSE;
        status--;

        if (status) {
          g_free (tofind);
          tofind = g_strconcat (" * ", modInfo->apiPrefix, "_", modInfo->modPrefix, "_", aPrefix, "_set_", name, ":\n", NULL);
        }
      }
    }

    if (!strcmp (buffer, tofind)) {
      fputs (STARTSTR, fddest);
      if (readonly || status == 2)
        tmp = g_strconcat (" * ", modInfo->apiPrefix, "_", iPrefix, "_", name, ":\n", NULL);
      else
        tmp = g_strconcat (" * ", modInfo->apiPrefix, "_", iPrefix, "_set_", name, ":\n", NULL);
      fputs (tmp, fddest);
      g_free (tmp);

      copy = TRUE;
    }
  }

  if (status)
    printf ("********* %s: not found!!!", tofind);
  g_free(tofind);


  fclose (fdsrc);
  fclose (fddest);

  return !status;
}

void writeTitle (module *modInfo, const gchar *name) {
  FILE *fddest;
  gchar *tmp;

  if ((fddest = fopen (modInfo->outfile, "a")) == NULL) {
    return;
  }

  fputs ("\n/******************************************************************************\n",fddest);
  tmp = g_strconcat ("          Gdome",name, " interface API\n", NULL);
  fputs (tmp, fddest);
  fputs (" ******************************************************************************/\n",fddest);

  fclose (fddest);
}

void
writeMethodCall (FILE *fddest, const gchar* name, const gchar* iName, const gchar* modPrefix, const gchar* aName, const gchar* type, int degree, struct _param params[]) {
  gchar buf[BUFSIZE];
  int i;

  buf[0]='\0';

  if (!strcmp (type, "void"))
		strcat (buf, "\t");
  else
		strcat (buf, "\treturn ");

	strcat (buf, "((Gdome_");
	strcat (buf, modPrefix);
	strcat (buf, "_");
	strcat (buf, iName);
	strcat (buf, " *)self)->vtab->");

  if (degree != 0) {
    for (i=0; i<degree; i++)
      strcat (buf, "super.");
  }
	strcat (buf, name);
	strcat (buf, " (");
	if (degree != 0) {
    strcat (buf, "(Gdome");
    strcat (buf, aName);
		strcat (buf, " *)");
	}
	strcat (buf, "self, ");
	for (i=0; params[i].type != NULL; i++) {
		strcat (buf, params[i].name);
		strcat (buf, ", ");
	}
  strcat (buf, "exc);\n");
	fputs (buf, fddest);

  writtenAPI++;
}

void
writeRawMethodCall (FILE *fddest, const gchar* name, const gchar* type, struct _param params[]) {
  gchar buf[BUFSIZE];
  int i;

  buf[0]='\0';

  if (!strcmp (type, "void"))
		strcat (buf, "\t");
  else
		strcat (buf, "\treturn ");

  i = strlen (buf);
	memcpy (buf+i, name+3, strlen (name)-5);
  buf[strlen (name)-5+i]='\0';

	strcat (buf, " (");
	for (i=0; params[i].type != NULL; i++) {
		strcat (buf, params[i].name);
		strcat (buf, ", ");
	}
  buf[strlen (buf)-2] = '\0';
  strcat (buf, ");\n");
	fputs (buf, fddest);

  writtenAPI++;
}

gboolean writeMethod (module *modInfo, const gchar *srcfile, const gchar *iPrefix,
                      const gchar *type, const gchar *name, const gchar *aPrefix,
											const gchar *aName, const gchar *iName, int degree,
                      struct _param params[], gboolean raw, struct _exception exceptions[],
                      const gchar *impl) {
  FILE *fdsrc, *fddest;
  gchar *srcFilename;
  gchar *tofind, *tmp, *tmp1;
  char buffer[BUFSIZE];
  int i;
  gboolean copy = FALSE;
  gboolean found = FALSE;

  srcFilename = g_strconcat (modInfo->srcdir, srcfile, NULL);
  if ((fdsrc = fopen (srcFilename, "r")) == NULL) {
    g_free (srcFilename);
    return FALSE;
  }
  g_free (srcFilename);

  if ((fddest = fopen (modInfo->outfile, "a")) == NULL) {
    return FALSE;
  }

  if (aPrefix == NULL)
		aPrefix = iPrefix;

  tofind = g_strconcat (" * ", modInfo->apiPrefix, "_", modInfo->modPrefix, "_", aPrefix, "_", name, ":\n", NULL);
  while (!found && fgets (buffer, BUFSIZE, fdsrc)) {
    if (copy) {
      fputs (buffer, fddest);
      if (!strcmp (buffer, ENDSTR)) {
				tmp = g_strconcat (type, "\n", NULL);
        fputs (tmp, fddest);
        g_free (tmp);
        if (!raw)
          tmp = g_strconcat (modInfo->apiPrefix, "_", iPrefix, "_", name, " (Gdome", iName, " *self, ", NULL);
        else
          tmp = g_strconcat (modInfo->apiPrefix, "_", iPrefix, "_", name, " (", NULL);
				for (i=0; params[i].type != NULL; i++) {
          char *strpos;
					if ((strpos = strstr(params[i].type, "(*)")) != NULL) {
						strpos[2]='\0';
            tmp1 = g_strconcat(tmp, params[i].type, params[i].name, ")", &strpos[3], ", ", NULL);
						strpos[2]=')';
					}
          else if ((strpos = strstr(params[i].type, "[")) != NULL) {
						strpos[0]='\0';
            tmp1 = g_strconcat(tmp, params[i].type, params[i].name, "[", &strpos[1], ", ", NULL);
						strpos[0]='[';
					}
          else if (params[i].type[strlen(params[i].type)-1] == '*')
            tmp1 = g_strconcat(tmp, params[i].type, params[i].name, ", ", NULL);
          else if (!strcmp (params[i].type, "void") && strlen (params[i].name) == 0)
            tmp1 = g_strconcat(tmp, params[i].type, ", ", NULL);
          else
						tmp1 = g_strconcat(tmp, params[i].type, " ", params[i].name, ", ", NULL);
					g_free (tmp);
					tmp = tmp1;
				}
        if (!raw)
          tmp1 = g_strconcat (tmp, "GdomeException *exc)\n", NULL);
        else {
          tmp[strlen (tmp)-2] = '\0';
          tmp1 = g_strconcat (tmp, ")\n", NULL);
        }
        g_free (tmp);
        fputs (tmp1, fddest);
        g_free (tmp1);
        fputs ("{\n",fddest);
        if (!raw) {
					for (i=0; exceptions[i].type != NULL; i++)
            writeException (fddest, &exceptions[i], type);
					fputs ("\t*exc = 0;\n", fddest);
          if (impl != NULL)
            fputs(impl, fddest);
					writeMethodCall(fddest, name, iName, modInfo->modPrefix, aName, type, degree, params);
				}
				else
					writeRawMethodCall(fddest, tofind, type, params);
        fputs ("}\n",fddest);

				copy = FALSE;
				found = TRUE;
      }
    }

    if (!strcmp (buffer, tofind)) {
      fputs (STARTSTR, fddest);
			tmp = g_strconcat (" * ", modInfo->apiPrefix, "_", iPrefix, "_", name, ":\n", NULL);
      fputs (tmp, fddest);
      g_free (tmp);

      copy = TRUE;
    }
  }

  if (!found)
    printf ("********* %s: not found!!!", tofind);
  g_free(tofind);

  fclose (fdsrc);
  fclose (fddest);

  return found;
}

