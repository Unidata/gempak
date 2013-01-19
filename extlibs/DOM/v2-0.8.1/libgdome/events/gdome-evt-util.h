/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 2; tab-width: 2 -*- */
/* gdome-evt-util.h
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

#ifndef GDOME_EVTUTIL_FILE
#define GDOME_EVTUTIL_FILE

#include "gdome-xml-util.h"
#include "gdome-events.h"

/* EventTarget are Nodes in fact... */
#define GDOME_XML_IS_EVNTT(evntt) GDOME_XML_IS_N(evntt)

#define GDOME_XML_IS_EVNTL(evntl) 1

#define GDOME_XML_IS_EVNT(evnt) \
((Gdome_evt_Event *) evnt)->etype == GDOME_BASE_EVENT || \
((Gdome_evt_Event *) evnt)->etype == GDOME_MUTATION_EVENT

#define GDOME_XML_IS_MEVNT(mevnt) \
((Gdome_evt_Event *) mevnt)->etype == GDOME_MUTATION_EVENT

#endif /* GDOME_EVTUTIL_FILE */
