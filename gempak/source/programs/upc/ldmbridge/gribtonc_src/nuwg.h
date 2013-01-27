/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: nuwg.h,v 1.8 1995/11/17 20:41:32 russ Exp $ */

/*
 * To find which netCDF names correspond with various conventions.
 * Despite the name of this file and functions, the NUWG conventions do not
 * recommend or endorse any particular names for these netCDF components, just
 * that there be an association between the names and their uses in tabular
 * form that makes it easy to change names.  This really should be done at
 * run time, by reading in a table, but here the table is compile-time.
 */

#ifndef NUWG_H_
#define NUWG_H_

enum ncpart {
    DIM_ACCUM,			/* time range indicator dimension for
				   accumulations, e.g. 3-6 hr. precipitation */
    DIM_FHG,			/* fixed-height-above-ground level dimension */
    DIM_LEVEL,			/* level dimension */
    DIM_NAV,			/* navigation dimension */
    DIM_SIGMA,			/* sigma level dimension */
    DIM_HY,			/* hybrid level dimension */
    DIM_FH,			/* fixed height level dimension */
    DIM_BLS,			/* depth below land surface level dimension */
    DIM_ISEN,			/* Isentropic (theta) level level dimension */
    DIM_PDG,			/* specified pressure difference from ground
				   level dimension */
    DIM_FHGH,			/* height level above ground (high precision)
				   level dimension */
    DIM_DBS,			/* depth below sea level dimension */

    DIM_LBLS,			/* dimension for Layer between 2 depths below
				   land surface */
    DIM_LFHG,			/* dimension for layer between 2 height levels
				   above ground */
    DIM_LFHM,			/* dimension for layer between 2 height levels
				   above MSL */
    DIM_LHY,			/* dimension for Layer between 2 hybrid levels
				 */
    DIM_LISEN,			/* dimension for Layer between 2 isentropic
				   (theta) levels */
    DIM_LISH,			/* dimension for layer between 2 isobaric
				   surfaces (high precision) */
    DIM_LISM,			/* dimension for layer between 2 isobaric
				   surfaces (mixed precision) */
    DIM_LISO,			/* dimension for layer between two isobaric
				   levels */
    DIM_LPDG,			/* dimension for layer between levels at
				   specif. pressure diffs from ground */
    DIM_LSH,			/* dimension for layer between 2 sigma levels
				   (high precision) */
    DIM_LS,			/* dimension for layer between 2 sigma levels
				 */
    DIM_NGRIDS,			/* number of subgrids in list of grids stitched
				   together*/
    VAR_REFTIME,		/* reference time variable */
    VAR_VALTIME,		/* valid time variable */
    VAR_GRID_TYPE_CODE,		/* GRIB GDS data representation type */
    VAR_GRID_CENTER,		/* GRIB originating center ID */
    VAR_GRID_NUMBER,		/* GRIB center-catalogued grid number */
    VAR_MODELID,		/* GRIB model Id */
    VAR_RESCOMP,		/* GRIB resolution and component flags */

				/* Navigation variables for diverse grids */
    VAR_DI,
    VAR_DJ,
    VAR_DX,
    VAR_DY,
    VAR_J,
    VAR_K,
    VAR_LA1,
    VAR_LA2,
    VAR_LAP,
    VAR_LATIN,
    VAR_LATIN1,
    VAR_LATIN2,
    VAR_LO1,
    VAR_LO2,
    VAR_LOP,
    VAR_LOV,
    VAR_M,
    VAR_MODE,
    VAR_N,
    VAR_NI,
    VAR_NJ,
    VAR_NR,
    VAR_NX,
    VAR_NY,
    VAR_ORIENTATION,
    VAR_PROJFLAG,
    VAR_ROTANGLE,
    VAR_ROTLAT,
    VAR_ROTLON,
    VAR_SPLAT,
    VAR_SPLON,
    VAR_STRETCHFACTOR,
    VAR_STRETCHLAT,
    VAR_STRETCHLON,
    VAR_TYPE,
    VAR_XO,
    VAR_XP,
    VAR_YO,
    VAR_YP
};

#ifdef __cplusplus
extern "C" char * nuwg_name(enum ncpart);
extern "C" int nuwg_getdim(int ncid, enum ncpart);
extern "C" int nuwg_getvar(int ncid, enum ncpart);
#elif defined(__STDC__)
extern char * nuwg_name(enum ncpart);
extern int nuwg_getdim(int ncid, enum ncpart);
extern int nuwg_getvar(int ncid, enum ncpart);
#else
extern char * nuwg_name( /* enum ncpart */ );
extern int nuwg_getdim( /* int ncid, enum ncpart */ );
extern int nuwg_getvar( /* int ncid, enum ncpart */ );
#endif

#endif /* NUWG_H_ */
