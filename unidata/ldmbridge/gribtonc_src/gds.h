/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: gds.h,v 1.3 1995/06/02 17:11:50 russ Exp $ */

/*
 * NMC "GRIB" Edition 1 Grid Description Section raw data structures, taken
 * from WMO FM 92-X GRIB.
 */

#ifndef _GDS_H
#define _GDS_H
#include "gribtypes.h"

/*
 + GRIB Table 6, Data Representation Types for Grid Description Section
 */
#define GRID_LL		0	/* Latitude/Longitude also called
				   Equidistant Cylindrical or Plate Carree */
#define GRID_MERCAT	1	/* Mercator Projection */
#define GRID_LAMBERT	3	/* Lambert Conformal, secant or tangent,
				   conical or bipolar (normal or oblique)
				   Projection */
#define GRID_GAU	4	/* Gaussian Latitude/Longitude */
#define GRID_POLARS	5	/* Polar Stereographic Projection */
#define	GRID_ALBERS	8	/* Albers equal-area, secant or tangent,
				   conical or bipolar, projection */
#define	GRID_RLL	10	/* Rotated latitude/longitude */
#define GRID_OLAMBERT	13	/* Oblique Lambert conformal, secant or
				   tangent, conical or bipolar, projection */
#define	GRID_RGAU	14	/* Rotated Gaussian latitude/longitude */
#define GRID_SLL	20	/* Stretched latitude/longitude */
#define GRID_SGAU	24	/* Stretched Gaussian latitude/longitude */
#define GRID_SRLL	30	/* Stretched and rotated latitude/longitude */
#define GRID_SRGAU	34	/* Stretched and rotated Gaussian
				   latitude/longitude */
#define GRID_SPH	50	/* Spherical Harmonic Coefficients */
#define GRID_RSPH	60	/* Rotated spherical harmonics */
#define GRID_SSPH	70	/* Stretched spherical harmonics */
#define GRID_SRSPH	80	/* Stretched and rotated spherical harmonics */
#define GRID_SPACEV	90	/* Space view perspective or orthographic */

/* Parameters for these are not defined in WMO or Stackpole GRIB documents */
#define GRID_GNOMON	2	/* Gnomonic Projection */
#define	GRID_UTM	6	/* Universal Transverse Mercator (UTM)
				   projection */
#define	GRID_SIMPOL	7	/* Simple polyconic projection */
#define	GRID_MILLER	9	/* Miller's cylindrical projection */

/* GRIB Table 7 -- Resolution and Component Flags */
#define RESCMP_DIRINC	0x80	/* bit on if directional increments given */
#define RESCMP_EARTH	0x40	/* bit on if earth assumed to be IAU 1965
				   oblate spheroid, off if sperical earth
				   assumed (radius=6367.47 km) */
#define RESCMP_UVRES	0x08	/* bit on if u- and v-components of vector
				   quantities resolved relative to the
				   defined grid in the direction of
				   increasing x and y (or i and j)
				   coordinates respectively, bit off if u-
				   and v-components of vector quantities
				   resolved relative to easterly and
				   northerly directions */

/* Grid Table 8 -- Scanning Mode Flag */
#define SCAN_I_MINUS	0x80	/* bit on if points scan in -i direction,
				   bit off if points scan in +i direction */
#define SCAN_J_PLUS	0x40	/* bit on if points scan in +j direction,
				   bit off if points scan in -j direction */
#define SCAN_J_CONSEC	0x20	/* bit on if adjacent points in j direction
				   are consecutive, bit off if adjacent
				   points in i direction are consecutive */

/*
 * lat/lon grid (or equidistant cylindrical, or Plate Carree),
 * used when grib1->gds->type is GRID_LL
 */
typedef struct
    {
    g2int ni;			/* number of points along a parallel */
    g2int nj;			/* number of points along a meridian */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint la2;			/* latitude of last grid point */
    g3sint lo2;			/* longitude of last grid point */
    g2int di;			/* i direction increment */
    g2int dj;			/* j direction increment */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    unsigned char reserved[4];
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_ll;

/*
 * rotated lat/lon grid (or equidistant cylindrical, or Plate Carree),
 * used when grib1->gds->type is GRID_RLL
 */
typedef struct
    {
    g2int ni;			/* number of points along a parallel */
    g2int nj;			/* number of points along a meridian */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint la2;			/* latitude of last grid point */
    g3sint lo2;			/* longitude of last grid point */
    g2int di;			/* i direction increment */
    g2int dj;			/* j direction increment */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    unsigned char reserved[4];
    g3sint lapole;		/* latitude of southern pole */
    g3sint lopole;		/* longitude of southern pole */
    g4flt angrot;		/* angle of rotation */
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_rll;

/*
 * stretched lat/lon grid (or equidistant cylindrical, or Plate Carree),
 * used when grib1->gds->type is GRID_SLL
 */
typedef struct
    {
    g2int ni;			/* number of points along a parallel */
    g2int nj;			/* number of points along a meridian */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint la2;			/* latitude of last grid point */
    g3sint lo2;			/* longitude of last grid point */
    g2int di;			/* i direction increment */
    g2int dj;			/* j direction increment */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    unsigned char reserved[4];
    g3sint lastr;		/* latitude of pole of stretching */
    g3sint lostr;		/* longitude of pole of stretching */
    g4flt stretch;		/* stretching factor */
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_sll;

/*
 * stretched and rotated lat/lon grid (or equidistant cylindrical, or Plate
 * Carree), * used when grib1->gds->type is GRID_SRLL
 */
typedef struct
    {
    g2int ni;			/* number of points along a parallel */
    g2int nj;			/* number of points along a meridian */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint la2;			/* latitude of last grid point */
    g3sint lo2;			/* longitude of last grid point */
    g2int di;			/* i direction increment */
    g2int dj;			/* j direction increment */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    unsigned char reserved[4];
    g3sint lapole;		/* latitude of southern pole */
    g3sint lopole;		/* longitude of southern pole */
    g4flt angrot;		/* angle of rotation */
    g3sint lastr;		/* latitude of pole of stretching */
    g3sint lostr;		/* longitude of pole of stretching */
    g4flt stretch;		/* stretching factor */
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_srll;

/*
 * Gaussian lat/lon grid, used when grib1->gds->type is GRID_GAU
 */
typedef struct
    {
    g2int ni;			/* number of points along a parallel */
    g2int nj;			/* number of points along a meridian */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint la2;			/* latitude of last grid point */
    g3sint lo2;			/* longitude of last grid point */
    g2int di;			/* i direction increment */
    g2int n;			/* # of parallels between a pole and equator */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    unsigned char reserved[4];
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_gau;

/*
 * Rotated Gaussian lat/lon grid, used when grib1->gds->type is GRID_RGAU
 */
typedef struct
    {
    g2int ni;			/* number of points along a parallel */
    g2int nj;			/* number of points along a meridian */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint la2;			/* latitude of last grid point */
    g3sint lo2;			/* longitude of last grid point */
    g2int di;			/* i direction increment */
    g2int n;			/* # of parallels between a pole and equator */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    unsigned char reserved[4];
    g3sint lapole;		/* latitude of southern pole */
    g3sint lopole;		/* longitude of southern pole */
    g4flt angrot;		/* angle of rotation */
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_rgau;

/*
 * Stretched Gaussian lat/lon grid, used when grib1->gds->type is GRID_SGAU
 */
typedef struct
    {
    g2int ni;			/* number of points along a parallel */
    g2int nj;			/* number of points along a meridian */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint la2;			/* latitude of last grid point */
    g3sint lo2;			/* longitude of last grid point */
    g2int di;			/* i direction increment */
    g2int n;			/* # of parallels between a pole and equator */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    unsigned char reserved[4];
    g3sint lastr;		/* latitude of pole of stretching */
    g3sint lostr;		/* longitude of pole of stretching */
    g4flt stretch;		/* stretching factor */
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_sgau;

/*
 * Stretched and rotated Gaussian lat/lon grid, used when grib1->gds->type
 * is GRID_SRGAU
 */
typedef struct
    {
    g2int ni;			/* number of points along a parallel */
    g2int nj;			/* number of points along a meridian */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint la2;			/* latitude of last grid point */
    g3sint lo2;			/* longitude of last grid point */
    g2int di;			/* i direction increment */
    g2int n;			/* # of parallels between a pole and equator */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    unsigned char reserved[4];
    g3sint lapole;		/* latitude of southern pole */
    g3sint lopole;		/* longitude of southern pole */
    g4flt angrot;		/* angle of rotation */
    g3sint lastr;		/* latitude of pole of stretching */
    g3sint lostr;		/* longitude of pole of stretching */
    g4flt stretch;		/* stretching factor */
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_srgau;

/*
 * Spherical harmonic coefficients, used when grib1->gds->type is GRID_SPH
 */
typedef struct
    {
    g2int j;			/* J pentagonal resolution parameter */
    g2int k;			/* K pentagonal resolution parameter */
    g2int m;			/* M pentagonal resolution parameter */
    g1int type;			/* Representation type (table 9) */
    g1int mode;			/* Representation mode (table 10) */
    unsigned char reserved[18];
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_sph;

/*
 * Rotated spherical harmonic coefficients, used when
 * grib1->gds->type is GRID_RSPH
 */
typedef struct
    {
    g2int j;			/* J pentagonal resolution parameter */
    g2int k;			/* K pentagonal resolution parameter */
    g2int m;			/* M pentagonal resolution parameter */
    g1int type;			/* Representation type (table 9) */
    g1int mode;			/* Representation mode (table 10) */
    unsigned char reserved[18];
    g3sint lapole;		/* latitude of southern pole */
    g3sint lopole;		/* longitude of southern pole */
    g4flt angrot;		/* angle of rotation */
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_rsph;

/*
 * Stretched spherical harmonic coefficients, used when
 * grib1->gds->type is GRID_SSPH
 */
typedef struct
    {
    g2int j;			/* J pentagonal resolution parameter */
    g2int k;			/* K pentagonal resolution parameter */
    g2int m;			/* M pentagonal resolution parameter */
    g1int type;			/* Representation type (table 9) */
    g1int mode;			/* Representation mode (table 10) */
    unsigned char reserved[18];
    g3sint lastr;		/* latitude of pole of stretching */
    g3sint lostr;		/* longitude of pole of stretching */
    g4flt stretch;		/* stretching factor */
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_ssph;

/*
 * Stretched and rotated spherical harmonic coefficients, used when
 * grib1->gds->type is GRID_SRSPH
 */
typedef struct
    {
    g2int j;			/* J pentagonal resolution parameter */
    g2int k;			/* K pentagonal resolution parameter */
    g2int m;			/* M pentagonal resolution parameter */
    g1int type;			/* Representation type (table 9) */
    g1int mode;			/* Representation mode (table 10) */
    unsigned char reserved[18];
    g3sint lapole;		/* latitude of southern pole */
    g3sint lopole;		/* longitude of southern pole */
    g4flt angrot;		/* angle of rotation */
    g3sint lastr;		/* latitude of pole of stretching */
    g3sint lostr;		/* longitude of pole of stretching */
    g4flt stretch;		/* stretching factor */
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_srsph;

/*
 * Polar stereographic grid, used when grib1->gds->type is GRID_POLARS
 */
typedef struct
    {
    g2int nx;			/* number of points along X-axis */
    g2int ny;			/* number of points along Y-axis */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint lov;			/* orientation of the grid */
    g3int dx;			/* X-direction grid length */
    g3int dy;			/* Y-direction grid length */
    g1int pole;			/* Projection center flag */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    unsigned char reserved[4];
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_polars;

/*
 * Gnomonic grid, used when grib1->gds->type is GRID_GNOMON
 */
typedef struct
    {
    g2int nx;			/* number of points along X-axis */
    g2int ny;			/* number of points along Y-axis */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint lov;			/* orientation of the grid */
    g3int dx;			/* X-direction grid length */
    g3int dy;			/* Y-direction grid length */
    g1int pole;		/* Projection center flag */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    unsigned char reserved[4];
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_gnomon;

/*
 * Mercator grid, used when grib1->gds->type is GRID_MERCAT
 */
typedef struct grid_mercator {
    g2int ni;			/* number of points along a parallel */
    g2int nj;			/* number of points along a meridian */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint la2;			/* latitude of last grid point */
    g3sint lo2;			/* longitude of last grid point */
    g3sint latin;		/* latitude at which the Mercator projection
				   cylinder intersects the Earth */
    unsigned char reserved;
    g1int scan_mode;		/* scanning mode flags (table 8) */
    g3int di;			/* longitudinal direction grid length */
    g3int dj;			/* latitudinal direction grid length */
    unsigned char reserved1[8];
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_mercator;

/*
 * Lambert conformal, secant or tangent, conic or bi-polar (normal or oblique),
 * used when  grib1->gds->type is GRID_LAMBERT
 */
typedef struct
    {
    g2int nx;			/* number of points along X-axis */
    g2int ny;			/* number of points along Y-axis */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint lov;			/* orientation of the grid */
    g3int dx;			/* X-direction grid length */
    g3int dy;			/* Y-direction grid length */
    g1int pole;		/* Projection center flag */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    g3sint latin1;		/* First latitude from the pole at which the
				   secant cone cuts the sphere */
    g3sint latin2;		/* Second latitude from the pole at which the
				   secant cone cuts the sphere */
    g3sint splat;		/* Latitude of the southern pole */
    g3sint splon;		/* Longitude of the southern pole */
    unsigned char reserved[2];
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_lambert;

/*
 * Oblique Lambert conformal uses same fields as normal Lambert conformal,
 * used when grib1->gds->type is GRID_OLAMBERT
 */
typedef struct
    {
    g2int nx;			/* number of points along X-axis */
    g2int ny;			/* number of points along Y-axis */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint lov;			/* orientation of the grid */
    g3int dx;			/* X-direction grid length */
    g3int dy;			/* Y-direction grid length */
    g1int pole;		/* Projection center flag */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    g3sint latin1;		/* First latitude from the pole at which the
				   secant cone cuts the sphere */
    g3sint latin2;		/* Second latitude from the pole at which the
				   secant cone cuts the sphere */
    g3sint splat;		/* Latitude of the southern pole */
    g3sint splon;		/* Longitude of the southern pole */
    unsigned char reserved[2];
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_olambert;

/*
 * Albers equal-area, secant or tangent, conic or bi-polar (normal or oblique),
 * used when  grib1->gds->type is GRID_ALBERS
 */
typedef struct
    {
    g2int nx;			/* number of points along X-axis */
    g2int ny;			/* number of points along Y-axis */
    g3sint la1;			/* latitude of first grid point */
    g3sint lo1;			/* longitude of first grid point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3sint lov;			/* orientation of the grid */
    g3int dx;			/* X-direction grid length */
    g3int dy;			/* Y-direction grid length */
    g1int pole;		/* Projection center flag */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    g3sint latin1;		/* First latitude from the pole at which the
				   secant cone cuts the sphere */
    g3sint latin2;		/* Second latitude from the pole at which the
				   secant cone cuts the sphere */
    g3sint splat;		/* Latitude of the southern pole */
    g3sint splon;		/* Longitude of the southern pole */
    unsigned char reserved[2];
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_albers;

/*
 * Space view perspective or orthographic,
 * used when  grib1->gds->type is GRID_SPACEV
 */
typedef struct
    {
    g2int nx;			/* number of points along X-axis (columns) */
    g2int ny;			/* number of points along Y-axis (rows or
				   lines) */
    g3sint lap;			/* latitude of sub-satellite point */
    g3sint lop;			/* longitude of sub-satellite point */
    g1int res_flags;		/* resolution and component flags (table 7) */
    g3int dx;			/* apparent diameter of earth in grid lengths,
				   in x direction */
    g3int dy;			/* apparent diameter of earth in grid lengths,
				   in y direction */
    g2int xp;			/* Xp - X-coordinate of sub satellite point */
    g2int yp;			/* Y-coordinate of sub-satellite point */
    g1int scan_mode;		/* scanning mode flags (table 8) */
    g3int orient;		/* orientation of the grid; i.e., the angle in
				   millidegrees between the increasing y axis
				   and the meridian of the sub-satellite point
				   in the direction of increasing latitude */
    g3int nr;			/* altitude of the camera from the earth's
				   center, measured in units of the earth's
				   (equatorial) radius */
    g2int xo;			/* X-coordinate of origin of sector image */
    g2int yo;			/* Y-coordinate of origin of sector image */
    unsigned char reserved[6];
    union {			/* need not be present */
	g4flt vertc[GRIB_ARB];	/* vertical coordinate parameters */
	g2int npts[2*GRIB_ARB];	/* list of number of points in each row */
    } vn;
} grid_spacev;


/*
 * Grid Description Section, a raw mapping of the GRIB bytes
 */
typedef struct gds {
    g3int len;			/* length of Grid Description Section */
    g1int nv;			/* number of vertical coordinate parameters */
    g1int pv;			/* byte number of the list of vertical coords,
				 * if present, or list of points in each row,
				 * for quasi-regular grids */
    g1int type;			/* representation type, from GRIB table 6 */
    union {
        grid_ll  		ll; /* type GRID_LL */
        grid_rll  		rll; /* type GRID_RLL */
        grid_sll  		sll; /* type GRID_SLL */
        grid_srll  		srll; /* type GRID_SRLL */
        grid_gau  		gau; /* type GRID_GAU */
        grid_rgau  		rgau; /* type GRID_RGAU */
        grid_sgau  		sgau; /* type GRID_SGAU */
        grid_srgau  		srgau; /* type GRID_SRGAU */
        grid_sph  		sph; /* type GRID_SPH */
        grid_rsph  		rsph; /* type GRID_RSPH */
        grid_ssph  		ssph; /* type GRID_SSPH */
        grid_srsph  		srsph; /* type GRID_SRSPH */
        grid_polars 		polars;	/* type GRID_POLARS */
	grid_gnomon		gnomon;	/* type GRID_GNOMON */
        grid_mercator 		mercator; /* type GRID_MERCAT */
        grid_lambert  		lambert; /* type GRID_LAMBERT */
        grid_olambert  		olambert; /* type GRID_OLAMBERT */
	grid_albers		albers;	/* type GRID_ALBERS */
	grid_spacev		spacev;	/* type GRID_SPACEV */
	/* Not well-specified in WMO or Stackpole GRIB documents: */
	/* grid_utm		utm; */	        /* type GRID_UTM */
	/* grid_simpol		simpol; */	/* type GRID_SIMPOL */
	/* grid_miller		miller; */	/* type GRID_MILLER */
    } grid;
} gds;	

#endif /* _GDS_H */
