/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: emalloc.h,v 1.6 1996/01/10 16:21:55 russ Exp $ */

#ifndef _EMALLOC_H
#define _EMALLOC_H
#include <stdlib.h>

#ifdef __cplusplus
extern "C" void* emalloc (size_t); /* exit with message if malloc fails */
extern "C" void* erealloc (void*, size_t); /* exit with message if realloc fails */
extern "C" char* estrdup (char*); /* exit with message if strdup fails */
#elif defined(__STDC__)
extern void* emalloc (size_t); /* exit with message if malloc fails */
extern void* erealloc (void*, size_t); /* exit with message if realloc fails */
extern char* estrdup (char*); /* exit with message if strdup fails */
#else
extern void* emalloc ( /* size_t */ ); /* exit with message if malloc fails */
extern void* erealloc ( /* void*, size_t */ ); /* exit with message if realloc fails */
extern char* estrdup ( /* char* */ ); /* exit with message if strdup fails */
#endif

#endif /* _EMALLOC_H */
