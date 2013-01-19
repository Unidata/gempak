/*
 *   Copyright 1995 University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: grib1.h,v 1.5 1995/06/02 17:11:49 russ Exp $ */

/*
 * NMC "GRIB" Edition 1 Data structures, taken from WMO FM 92-X GRIB
 */

#ifndef _GRIB1_H
#define _GRIB1_H

#ifdef __OBJECTCENTER__
#define const			/* workaround for bug with const parameters */
#endif

#include "gribtypes.h"		/* g1int, g2int, ..., and conversion functions */
#include "gds.h"		/* Grid Description Section (GDS) raw data layout */
#include "get_prod.h"		/* for prod structure */

#define MAX_GRIB_SIZE	100000	/* The biggest we've seen on HRS is 6045 ,
                                   29km eta is 97,000 */
#define HAS_GDS		0x80
#define HAS_BMS		0x40    
#define NONCATALOGED_GRID 255	/* indicates grid is defined by GDS */
#define QUASI_RECT_GRID	256

/*
 * Indicator Section
 */
typedef struct ids {
    char grib[4];		/* 'G', 'R', 'I', 'B' */
    g3int len;			/* length of entire GRIB message */
    g1int edition;		/* GRIB edition number */
} ids;

/*
 * Product Definition Section
 */
typedef struct pds {
    g3int len;			/* length of section in bytes */
    g1int table_version;	/* GRIB tables version number */
    g1int center;		/* id of center */
    g1int model;		/* model id, allocated by center */
    g1int grid;			/* grid id, allocated by center */
    g1int db_flg;		/* whether GDS, BMS included */
    g1int param;		/* parameter id, from GRIB table 2 */
    g1int level_flg;		/* type of level, from GRIB table 3 */
    union {			/* height, pressure, etc of levels (table 3) */
	g2int lev;		/* one level from 2 bytes */
	g1int levs[2];		/* two 1-byte levels */
    } level;
    g1int year;			/* reference time of data */
    g1int month;
    g1int day;
    g1int hour;
    g1int minute;
    g1int tunit;		/* unit of time range, from GRIB table 4 */
    g1int tr[2];		/* periods of time or interval */
    g1int tr_flg;		/* time range indicator, from GRIB table 5 */
    g2int avg;			/* number in average, if any */
    g1int missing;		/* number missing from averages or accums */
    g1int century;		/* 20 for years 1901-2000 */
    g1int subcenter;		/* reserved in GRIB1 standard */
    g2sint scale10;		/* (signed) units decimal scale factor */
    unsigned char reserved1[12];	/* reserved; need not be present */
    unsigned char reserved2[GRIB_ARB]; /* reserved for local center use;
				   need not be present */
} pds;

/*
 * Bit Map Section
 */
typedef struct bms {
    g3int len;			/* Length of section in bytes */
    g1int nbits;		/* Number of unused bits at end of section */
    g2int map_flg;		/* 0 if bit map follows, otherwise catalogued
				   bit map from center */
    unsigned char bits[GRIB_ARB];	/* the bit map */
} bms;


#define BDS_KIND	0x80	/* From GRIB Table 11, kind of data bit.  0
				   for grid-point data, 1 for spherical
				   harmonic coefficients */
#define BDS_PACKING	0x40	/* From GRIB Table 11, kind of packing bit.
				   0 for simple packing, 1 for complex
				   (spherical harmonics) or second-order
				   (grid) packing */
#define BDS_DATATYPE	0x20	/* From GRIB Table 11, data type bit.  0 if
				   floating-point values, 1 if integer
				   values */
#define BDS_MORE_FLAGS	0x10	/* From GRIB Table 11, additional flags bit.
				   0 if no additional flags, 1 if more flag
				   bits for grid-point data second-order
				   packing */
#define BDS_MATRIX	0x40	/* From GRIB Table 11, matrix values.  0 for
				   single datum at each grid point, 1 for
				   matrix of values at each grid point */
#define BDS_SECONDARY	0x20	/* From GRIB Table 11, secondary bit maps.
				   0 if no secondary bit maps, 1 if
				   secondary bit maps present */
#define BDS_WIDTHS	0x10	/* From GRIB Table 11, widths flag.  0 if
				   second-order values have constant width,
				   1 second-order values have different
				   widths */
/*
 * Binary Data Section
 */
typedef struct bds {
    g3int len;			/* Length of section in bytes */
    g1int flg;			/* High 4 bits are flag from GRIB table 11.
				   Low 4 bits are no. of unused bits at end. */
    g2sint scale;		/* (signed) scale factor */
    g4flt ref;			/* Reference value (min of packed values) */
    g1int bits;			/* Number of bits containing each packed val */
    unsigned char data[GRIB_ARB]; /* The packed data */
} bds;

/*
 * The GRIB1 raw struture
 */
typedef struct grib1 {
    char *hdr;			/* Header such as WMO Header, if any */
    ids	*idsp;			/* Indicator section */
    pds	*pdsp;			/* Product definition section */
    gds	*gdsp;			/* Grid description section (optional) */
    bms	*bmsp;			/* Bit map section (optional) */
    bds	*bdsp;			/* Binary data section */
} grib1;

#ifdef __cplusplus
/* Overlay a preallocated raw grib1 on a raw product */
extern "C" int make_grib1(prod*, grib1*);
/* allocate a new grib1 and overlay it on a raw product */
extern "C" grib1* new_grib1(prod*);
extern "C" void free_grib1(grib1*);	/* free a grib1 */
#elif defined(__STDC__)
/* Overlay a preallocated raw grib1 on a raw product */
extern int make_grib1(prod*, grib1*);
/* allocate a new grib1 and overlay it on a raw product */
extern grib1* new_grib1(prod*);
extern void free_grib1(grib1*);	/* free a grib1 */
#else
/* Overlay a preallocated raw grib1 on a raw product */
extern int make_grib1( /* prod*, grib1* */);
/* allocate a new grib1 and overlay it on a raw product */
extern grib1* new_grib1( /* prod* */ );
extern void free_grib1( /* grib1* */ ); /* free a grib1 */
#endif

#endif /* _GRIB1_H */
