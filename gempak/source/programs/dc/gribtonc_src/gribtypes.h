/*
 *   Copyright 1995 University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: gribtypes.h,v 1.5 1995/06/02 18:45:59 russ Exp $ */

/*
 * NMC "GRIB" Edition 1 types
 */

#ifndef _GRIBTYPES_H
#define _GRIBTYPES_H

#define GRIB_ARB	100	/* Arbitrary size for unspecified lengths */
#define G1I_MISSING	255	/* Used for missing or undefined value for
				   1-byte quantities */
#define G2I_MISSING	65535	/* Used for missing or undefined value for
				   2-byte quantities */

typedef unsigned char g1int;		/* A GRIB 1-byte integer */
typedef unsigned char g2int[2];	/* A GRIB 2-byte integer */
typedef unsigned char g3int[3];	/* A GRIB 3-byte integer */
typedef unsigned char g3sint[3];	/* A GRIB signed 3-byte integer */
typedef unsigned char g4flt[4];	/* A GRIB 4-byte float */
typedef unsigned char g2sint[2];	/* A GRIB signed 2-byte integer */

#ifdef __cplusplus
extern "C" int g1i(int);	/* g1int to int */
extern "C" int g2i(g2int);	/* g2int to int */
extern "C" int g2si(g2sint);	/* g2sint to int */
extern "C" int g3i(g3int);	/* g3int to int */
extern "C" int g3si(g3sint);	/* g3sint to int */
extern "C" float g4f(g4flt);	/* g4flt to float */
#elif defined(__STDC__)
extern int g1i(int);		/* g1int to int */
extern int g2i(g2int);		/* g2int to int */
extern int g2si(g2sint);	/* g2sint to int */
extern int g3i(g3int);		/* g3int to int */
extern int g3si(g3sint);	/* g3sint to int */
extern float g4f(g4flt);	/* g4flt to float */
#else
extern int g1i( /* int */ );	/* g1int to int */
extern int g2i( /* g2int */ );  /* g2int to int */
extern int g2si( /* g2sint */ );/* g2sint to int */
extern int g3i( /* g3int */ );  /* g3int to int */
extern int g3si( /* g3sint */ );/* g3sint to int */
extern float g4f( /* g4flt */ );/* g4flt to float */
#endif

#endif /* _GRIBTYPES_H */
