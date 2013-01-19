#include <stdlib.h>
#include <string.h>
#include "ulog.h"
#include "emalloc.h"

/*
 * Check return from malloc() and just exit (with a message) if we're out
 * of memory.
 */
void *
emalloc (size)
    size_t size;
{
    void   *p = (void *) malloc (size);
    if (p == 0) {
	uerror ("malloc: out of memory\n");
	exit (1);
    }
    return p;
}


/*
 * Check return from realloc() and just exit (with a message) if we're out
 * of memory.
 */
void *
erealloc (ptr, size)
    void *ptr;
    size_t size;
{
    void   *p = realloc (ptr, size);
    if (p == 0) {
	uerror ("realloc: out of memory\n");
	exit (1);
    }
    return p;
}


/*
 * Check return from strdup() and just exit (with a message) if we're out
 * of memory.
 */
char *
estrdup (str)
    char *str;
{
    char *s = (char *)emalloc(strlen(str) + 1);
    return strcpy(s, str);
}
