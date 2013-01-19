/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* srcutil.h
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

#ifndef APIGEN_SRCUTIL
#define APIGEN_SRCUTIL

typedef struct _module module;
struct _module {
  gchar *srcdir;
  gchar *outfile;
  gchar *apiPrefix;
  gchar *modPrefix;
  gchar *outfilehead;
};

typedef struct _param param;
struct _param {
	gchar *type;
	gchar *name;
};

typedef struct _exception exception;
struct _exception {
	gchar *type;
	gchar *on;
};

gboolean     writeAttribute    (module *modInfo,
                                const gchar *srcfile,
                                const gchar *iPrefix,
                                const gchar *type,
                                const gchar *name,
                                const gchar *aPrefix,
                                const gchar *aName,
                                const gchar *iName,
                                int degree,
                                gboolean readonly,
                                struct _exception exceptions[]);
void         writeTitle        (module *modInfo,
                                const gchar *name);
gboolean     writeMethod       (module *modInfo,
                                const gchar *srcfile,
                                const gchar *iPrefix,
                                const gchar *type,
                                const gchar *name,
                                const gchar *aPrefix,
                                const gchar *aName,
                                const gchar *iName,
                                int degree,
                                struct _param params[],
                                gboolean raw,
                                struct _exception exceptions[],
                                const gchar *impl);

#endif /* APIGEN_SRCUTIL */
