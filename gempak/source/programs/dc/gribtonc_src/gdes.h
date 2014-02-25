/*
 *   Copyright 1995 University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: gdes.h,v 1.7 1995/06/15 14:50:44 russ Exp $ */

/*
 * NMC "GRIB" Edition 1 Grid Description Section data structures, in more
 * accessible form, taken from WMO FM 92-X GRIB.
 *
 *  Latitudes and Longitudes are in floating-point degrees rather than
 *  packed-integer millidegrees.
 *  Other quantities are ints rather than 1-, 2-, or 3-byte packed ints.
 */

#ifndef _GDES_H
#define _GDES_H

#include "grib1.h"

#define	GDES_INT_MISSING	-1 /* missing value for ints */
#define GDES_FLOAT_MISSING	-9999. /* missing value for floats */

/*
 * For rotated grids
 */
typedef struct rotated {
    float lat;			/* if rotated, latitude of southern pole */
    float lon;			/* if rotated, longitude of southern pole */
    float angle;		/* if rotated, angle of rotation */
} rotated;

/*
 * For stretched grids
 */
typedef struct stretched {
    float lat;			/* if stretched, lat. of pole of stretching */
    float lon;			/* if stretched, lon. of pole of stretching */
    float factor;		/* if stretched, stretching factor */
} stretched;

/*
 * lat/lon grid (or equidistant cylindrical, or Plate Carree),
 * used when grib1->gds->type is GRID_LL, GRID_RLL, GRID_SLL, or GRID_SRLL
 */
typedef struct gdes_ll {
    int ni;			/* number of points along a parallel */
    int nj;			/* number of points along a meridian */
    float la1;			/* latitude of first grid point */
    float lo1;			/* longitude of first grid point */
    float la2;			/* latitude of last grid point */
    float lo2;			/* longitude of last grid point */
    float di;			/* i direction increment */
    float dj;			/* j direction increment */
    rotated *rot;               /* non-null if rotated, 0 otherwise */
    stretched *strch;           /* non-null if stretched, 0 otherwise */
} gdes_ll;

/*
 * Gaussian lat/lon grid, used when grib1->gds->type is GRID_GAU, GRID_RGAU, 
 * GRID_SGAU, or GRID_SRGAU
 */
typedef struct gdes_gau {
    int ni;			/* number of points along a parallel */
    int nj;			/* number of points along a meridian */
    float la1;			/* latitude of first grid point */
    float lo1;			/* longitude of first grid point */
    float la2;			/* latitude of last grid point */
    float lo2;			/* longitude of last grid point */
    float di;			/* i direction increment */
    int n;			/* # of parallels between a pole and equator */
    rotated *rot;               /* non-null if rotated, 0 otherwise */
    stretched *strch;           /* non-null if stretched, 0 otherwise */
} gdes_gau;

/*
 * Spherical harmonic coefficients, used when grib1->gds->type is GRID_SPH,
 * GRID_RSPH, GRID_SSPH, or GRID_SRSPH
 */
typedef struct
    {
    int j;			/* J pentagonal resolution parameter */
    int k;			/* K pentagonal resolution parameter */
    int m;			/* M pentagonal resolution parameter */
    int type;			/* Representation type (table 9) */
    int mode;			/* Representation mode (table 10) */
    rotated *rot;               /* non-null if rotated, 0 otherwise */
    stretched *strch;           /* non-null if stretched, 0 otherwise */
} gdes_sph;

/*
 * Polar stereographic grid, used when grib1->gds->type is GRID_POLARS or
 * GRID_GNOMON
 */
typedef struct
    {
    int nx;			/* number of points along X-axis */
    int ny;			/* number of points along Y-axis */
    float la1;			/* latitude of first grid point */
    float lo1;			/* longitude of first grid point */
    float lov;			/* orientation of the grid */
    float dx;			/* X-direction grid length */
    float dy;			/* Y-direction grid length */
    int pole;			/* Projection center flag,
				   0 if north pole in projection plane,
				   1 if south pole in projection plane */
} gdes_polars;

/*
 * Mercator grid, used when grib1->gds->type is GRID_MERCAT
 */
typedef struct gdes_mercator {
    int ni;			/* number of points along a parallel */
    int nj;			/* number of points along a meridian */
    float la1;			/* latitude of first grid point */
    float lo1;			/* longitude of first grid point */
    float la2;			/* latitude of last grid point */
    float lo2;			/* longitude of last grid point */
    float latin;                /* latitude at which the Mercator projection
				   cylinder intersects the Earth */
    float di;			/* longitudinal direction grid length,
				   meters */
    float dj;			/* latitudinal direction grid length,
				   meters */
} gdes_mercator;

/*
 * Lambert conformal, secant or tangent, conic or bi-polar (normal or oblique),
 * used when  grib1->gds->type is GRID_LAMBERT, GRID_OLAMBERT, or GRID_ALBERS
 */
typedef struct
    {
    int nx;			/* number of points along X-axis */
    int ny;			/* number of points along Y-axis */
    float la1;			/* latitude of first grid point */
    float lo1;			/* longitude of first grid point */
    float lov;			/* orientation of the grid */
    float dx;			/* X-direction grid length in meters */
    float dy;			/* Y-direction grid length in meters */
    int pole;			/* From projection center flag,
				   0 if north pole in projection plane,
				   1 if south pole in projection plane */
    int centers;		/* From projection center flag,
				   1 if only one projection center is used,
				   2 if projection is bipolar and symmetric */
    float latin1;               /* First latitude from the pole at which the
				   secant cone cuts the sphere */
    float latin2;               /* Second latitude from the pole at which the
				   secant cone cuts the sphere */
    float splat;                /* Latitude of the southern pole */
    float splon;                /* Longitude of the southern pole */
} gdes_lambert;

/*
 * Space view perspective or orthographic,
 * used when  grib1->gds->type is GRID_ALBERS
 */
typedef struct
    {
    int nx;			/* number of points along X-axis (columns) */
    int ny;			/* number of points along Y-axis (rows or
                                   lines) */
    float lap;			/* latitude of sub-satellite point */
    float lop;			/* longitude of sub-satellite point */
    float dx;			/* apparent diameter of earth in grid
                                   lengths, in x direction */
    float dy;			/* apparent diameter of earth in grid
                                   lengths, in y direction */
    float xp;			/* Xp - X-coordinate of sub satellite point */
    float yp;			/* Y-coordinate of sub-satellite point */
    float orient;               /* orientation of the grid; i.e., the angle
                                   in millidegrees between the increasing y
                                   axis and the meridian of the
                                   sub-satellite point in the direction of
                                   increasing latitude */
    float nr;			/* altitude of the camera from the earth's
				   center, measured in units of the earth's
				   (equatorial) radius */
    float xo;			/* X-coordinate of origin of sector image */
    float yo;			/* Y-coordinate of origin of sector image */
} gdes_spacev;

typedef union {
    gdes_ll  		ll;
    gdes_gau  		gau;
    gdes_sph  		sph;
    gdes_polars 		polars;
    gdes_mercator 		mercator;
    gdes_lambert  		lambert;
    gdes_spacev		spacev;
} gengrid;

/*
 * Grid Description, a "cooked" version of the gds structure for easier access
 */
typedef struct gdes {
    int type;			/* representation type, from GRIB table 6 */
    int ncols;			/* number of "columns" */
    int nrows;			/* number of "rows" */
    int npts;			/* just ncols * nrows for rectangular grids,
				   but actual number of points for
				   quasi-regular grids */

    gengrid grid;		/* union of diverse kinds of grids */

    unsigned char res_flags;    /* resolution and component flags (table 7),
				   meaningless for spherical harmonics*/
    unsigned char scan_mode;    /* scanning mode flags (table 8), meaningless
				   for spherical harmonics */
    int nv;			/* number of vertical coordinate parameters */
    float *vc;			/* list of vertical coordinate parameters */
    int quasi;			/* QUASI_RECT for rectangular grid,
				   QUASI_ROWS for grid with varying rows,
				   QUASI_COLS for grid with varying cols
				   */
    int *lc;			/* for quasi-regular grids, list of number
				   of points in each row (or column) */
    int maxlc;			/* for quasi-regular grids, max number of
				   points in a row (or column) */
    int keep;			/* for internal use; this is 1 for statically
				   allocated objects, so they won't get freed */
} gdes;	

#ifdef __cplusplus
extern "C" gdes* make_gdes(grib1*);	/* Make a gdes from raw gds or pds */
extern "C" void free_gdes(gdes*);	/* free a gdes */
extern "C" void print_gdes(gdes*);	/* print a gdes */
extern "C" char* gds_typename(int); /* type of grid projection */
#elif defined(__STDC__)
extern gdes* make_gdes(grib1*);	/* Make a gdes from raw gds or pds */
extern void free_gdes(gdes*); /* free a gdes */
extern void print_gdes(gdes*); /* print a gdes */
extern char* gds_typename(int); /* type of grid projection */
#else
extern gdes* make_gdes( /* grib1* */ );	/* Make a gdes from raw gds or pds */
extern void free_gdes( /* gdes* */ ); /* free a gdes */
extern void print_gdes( /* gdes* */ ); /* print a gdes */
extern char* gds_typename( /* int */ ); /* type of grid projection */
#endif

#endif /* _GDES_H */
