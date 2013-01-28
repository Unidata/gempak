/* iftok.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;

integer iftok_(char *ctok, ftnlen ctok_len)
{
    /* System generated locals */
    integer ret_val;
    icilist ici__1;

    /* Builtin functions */
    integer s_rsli(icilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsli(void);

    ici__1.icierr = 0;
    ici__1.iciend = 0;
    ici__1.icirnum = 1;
    ici__1.icirlen = ctok_len;
    ici__1.iciunit = ctok;
    ici__1.icifmt = 0;
    s_rsli(&ici__1);
    do_lio(&c__3, &c__1, (char *)&ret_val, (ftnlen)sizeof(integer));
    e_rsli();
    return ret_val;
} /* iftok_ */

