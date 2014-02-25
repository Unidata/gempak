/*
 *	Copyright 1992 University Corporation for Atmospheric Research
 *	   Not for Direct Resale. All copies to include this notice.
 */
/* $Id: levels.c,v 1.12 1995/12/07 15:30:51 russ Exp $ */

#include "ulog.h"
#include "levels.h"

#include <math.h>
#define float_near(x,y)	((float)((y) + 0.1*fabs((x)-(y))) == (float)(y)) /* true if x is "close to" y */

/*
 * Atmospheric level in mb from two 8-bit integers in GRIB product
 */
double
mblev(levels)
     int* levels;
{
    return 256.*levels[0] + levels[1];
}

/*
 * Search in table for specified level.  Returns index
 * of value in table, or -1 if the value was not found.
 */
long
level_index(level, levels, nlevels)
     double level;	/* level sought */
     float* levels;	/* table of levels */
     long nlevels;	/* number of levels in table */
{
    int nn = 0;

    while (nlevels--) {
	float ll;

	ll = *levels++;
	if (float_near(level, ll)) /* true if level "is close to" ll */
	    break;
	nn++;
    }
    return nlevels==-1 ? -1 : nn;
}


/*
 * Search in table for specified layer.  Returns index
 * of value in table, or -1 if the value was not found.
 */
long
layer_index(top, bot, tops, bots, nlayers)
     double top;		/* top of layer sought */
     double bot;		/* bottom of layer sought */
     float* tops;		/* table of layer tops */
     float* bots;		/* table of layer bottoms */
     long nlayers;		/* number of layers in table */
{
    int nn = 0;

    while (nlayers--) {
	if (float_near(top, *tops) && float_near(bot, *bots))
	    break;
	tops++;
	bots++;
	nn++;
    }
    return nlayers==-1 ? -1 : nn;
}


/*
 * Return name for GRIB level, given GRIB level code.
 */
char *
levelname(ii)
     int ii;
{
    switch(ii){
    case LEVEL_SURFACE: 
	return "Surface";
    case LEVEL_CLOUD_BASE: 
	return "Cloud Base";
    case LEVEL_CLOUD_TOP: 
	return "Cloud Top";
    case LEVEL_ISOTHERM: 
	return "0 Isotherm";
    case LEVEL_ADIABAT: 
	return "Adiabatic Condensation";
    case LEVEL_MAX_WIND: 
	return "Maximum Wind";
    case LEVEL_TROP: 
	return "Tropopause";
    case LEVEL_TOP:
	return "Top of Atmosphere";
    case LEVEL_SEABOT:
	return "Sea Bottom";
    case LEVEL_ISOBARIC: 
	return "Isobaric";
    case LEVEL_LISO: 
	return "Layer Between Two Isobaric";
    case LEVEL_MEAN_SEA: 
	return "Mean Sea";
    case LEVEL_FH: 
	return "Fixed Height";
    case LEVEL_LFHM: 
	return "Layer Between Two Heights Above MSL";
    case LEVEL_FHG: 
	return "Fixed Height Above Ground";
    case LEVEL_LFHG: 
	return "Layer Between Two Fixed Heights Above Ground";
    case LEVEL_SIGMA: 
	return "Sigma";
    case LEVEL_LS: 
	return "Layer Between Two Sigma";
    case LEVEL_HY:
	return "Hybrid level";
    case LEVEL_LHY:
	return "Layer between 2 hybrid levels";
    case LEVEL_Bls: 
	return "Below Land Surface";
    case LEVEL_LBls: 
	return "Layer Between Two Depths Below Land Surface";
    case LEVEL_ISEN:
	return "Isentropic (theta) level";
    case LEVEL_LISEN:
	return "Layer between 2 isentropic (theta) levels";
    case LEVEL_PDG:
	return "level at specified pressure difference from ground to level";
    case LEVEL_LPDG:
	return "layer between 2 levels at specified pressure differences from ground to levels";
    case LEVEL_PV:
	return "potential vorticity";
    case LEVEL_LISH: 
	return "Layer Between Two Isobaric Surfaces, High Precision";
    case LEVEL_FHGH:
	return "Height level above ground (high precision)";
    case LEVEL_LSH: 
	return "Layer Between Two Sigma Levels, High Precision";
    case LEVEL_LISM: 
	return "Layer Between Two Isobaric Surfaces, Mixed Precision";
    case LEVEL_DBS: 
	return "Depth Below Sea";
    case LEVEL_ATM:
	return "Entire atmosphere considered as a single layer";
    case LEVEL_OCEAN:
	return "Entire ocean considered as a single layer";
    }
    /* default */
    uerror("unknown level: %d", ii);
    return "reserved or unknown";
}


/*
 * Returns level suffix, used in netCDF names for variables on special
 * levels and in "gribdump -b" level abbreviations.
 */
char *
levelsuffix(lev)
    int lev;
{
				/* Note: If any suffixes are added or
				   changed, they must be added or changed in
				   the function grib_pcode() as well. */
    switch(lev) {
    case LEVEL_SURFACE: return "sfc"; /* surface of the Earth */
    case LEVEL_CLOUD_BASE: return "clbs"; /* cloud base level */
    case LEVEL_CLOUD_TOP: return "cltp"; /* cloud top level */
    case LEVEL_ISOTHERM: return "frzlvl"; /* 0 degree isotherm level */
    case LEVEL_ADIABAT: return "adcn"; /* adiabatic condensation level */
    case LEVEL_MAX_WIND: return "maxwind"; /* maximium wind speed level */
    case LEVEL_TROP: return "trop"; /* at the tropopause */
    case LEVEL_TOP: return "topa"; /* nominal top of atmosphere */
    case LEVEL_SEABOT: return "sbot"; /* sea bottom */
    case LEVEL_ISOBARIC: return ""; /* isobaric level */
    case LEVEL_LISO: return "liso"; /* layer between two isobaric levels */
    case LEVEL_MEAN_SEA: return "msl"; /* mean sea level */
    case LEVEL_FH: return "fh";	/* fixed height level */
    case LEVEL_LFHM: return "lfhm"; /* layer between 2 height levels above MSL */
    case LEVEL_FHG: return "fhg"; /* fixed height above ground */
    case LEVEL_LFHG: return "lfhg"; /* layer between 2 height levels above ground */
    case LEVEL_SIGMA: return "sigma"; /* sigma level */
    case LEVEL_LS: return "ls";	/* layer between 2 sigma levels */
    case LEVEL_HY: return "hybr"; /* Hybrid level */
    case LEVEL_LHY: return "lhyb"; /* Layer between 2 hybrid levels */
    case LEVEL_Bls: return "bls"; /* Depth below land surface */
    case LEVEL_LBls: return "lbls"; /* Layer between 2 depths below land surface */
    case LEVEL_ISEN: return "isen"; /* Isentropic (theta) level */
    case LEVEL_LISEN: return "lisn"; /* Layer between 2 isentropic (theta) levels */
    case LEVEL_PDG: return "pdg"; /* level at specified pressure difference from ground */
    case LEVEL_LPDG: return "lpdg"; /* layer between levels at specif. pressure diffs from ground */
    case LEVEL_PV: return "pv"; /* level of specified potential vorticity */
    case LEVEL_LISH: return "lish"; /* layer between 2 isobaric surfaces (high precision) */
    case LEVEL_FHGH: return "fhgh"; /* height level above ground (high precision) */
    case LEVEL_LSH: return "lsh"; /* layer between 2 sigma levels (high precision) */
    case LEVEL_LISM: return "lism"; /* layer between 2 isobaric surfaces (mixed precision) */
    case LEVEL_DBS: return "dbs"; /* depth below sea level */
    case LEVEL_ATM: return "atm"; /* entire atmosphere considered as a single layer */
    case LEVEL_OCEAN: return "ocn"; /* entire ocean considered as a single layer */
    }
				/* default: */
    uerror("bad level flag: %d", lev);
    return "";
}

/*
 * Returns int for first level (if 2 levels) or level (if only one level)
 */
int
level1(flag, ii)
    int flag;			/* GRIB level flag */
    int *ii;			/* GRIB levels */
{
    switch(flag){
    case LEVEL_SURFACE: 
    case LEVEL_CLOUD_BASE: 
    case LEVEL_CLOUD_TOP: 
    case LEVEL_ISOTHERM: 
    case LEVEL_ADIABAT: 
    case LEVEL_MAX_WIND: 
    case LEVEL_TROP: 
    case LEVEL_TOP:
    case LEVEL_SEABOT:
    case LEVEL_MEAN_SEA: 
    case LEVEL_ATM:
    case LEVEL_OCEAN:
	return 0;
    case LEVEL_ISOBARIC: 
    case LEVEL_FH: 
    case LEVEL_FHG: 
    case LEVEL_SIGMA: 
    case LEVEL_HY:
    case LEVEL_Bls: 
    case LEVEL_ISEN:
    case LEVEL_PDG:
    case LEVEL_FHGH:
    case LEVEL_DBS: 
	return 256*ii[0]+ii[1];	/* 2-octet level */
    case LEVEL_LISO: 
    case LEVEL_LFHM: 
    case LEVEL_LFHG: 
    case LEVEL_LS: 
    case LEVEL_LHY:
    case LEVEL_LBls: 
    case LEVEL_LISEN:
    case LEVEL_LPDG:
    case LEVEL_PV:
    case LEVEL_LISH: 
    case LEVEL_LSH: 
    case LEVEL_LISM: 
	return ii[0];		/* 1-octet level */
    }
    /* default */
    uerror("unknown level: %d", ii);
    return -1;
}


/*
 * Returns int for second level (if 2 levels) or 0 (if only one level)
 */
int
level2(flag, ii)
    int flag;			/* GRIB level flag */
    int *ii;			/* GRIB levels */
{
    switch(flag){
    case LEVEL_SURFACE: 
    case LEVEL_CLOUD_BASE: 
    case LEVEL_CLOUD_TOP: 
    case LEVEL_ISOTHERM: 
    case LEVEL_ADIABAT: 
    case LEVEL_MAX_WIND: 
    case LEVEL_TROP: 
    case LEVEL_TOP:
    case LEVEL_SEABOT:
    case LEVEL_MEAN_SEA: 
    case LEVEL_ATM:
    case LEVEL_OCEAN:
    case LEVEL_ISOBARIC: 
    case LEVEL_FH: 
    case LEVEL_FHG: 
    case LEVEL_SIGMA: 
    case LEVEL_HY:
    case LEVEL_Bls: 
    case LEVEL_ISEN:
    case LEVEL_PDG:
    case LEVEL_PV:
    case LEVEL_FHGH:
    case LEVEL_DBS: 
	return 0;	/* 1-octet level */
    case LEVEL_LISO: 
    case LEVEL_LFHM: 
    case LEVEL_LFHG: 
    case LEVEL_LS: 
    case LEVEL_LHY:
    case LEVEL_LBls: 
    case LEVEL_LISEN:
    case LEVEL_LPDG:
    case LEVEL_LISH: 
    case LEVEL_LSH: 
    case LEVEL_LISM: 
	return ii[1];		/* 2-octet level */
    }
    /* default */
    uerror("unknown level: %d", ii);
    return -1;
}


/*
 * Return GRIB units (as a string) for various kinds of levels.
 */
char *
levelunits(ii)
{
    switch(ii){
    case LEVEL_SURFACE: 
    case LEVEL_CLOUD_BASE: 
    case LEVEL_CLOUD_TOP: 
    case LEVEL_ISOTHERM: 
    case LEVEL_ADIABAT: 
    case LEVEL_MAX_WIND: 
    case LEVEL_TROP: 
    case LEVEL_TOP:
    case LEVEL_SEABOT:
    case LEVEL_MEAN_SEA: 
    case LEVEL_HY:
    case LEVEL_LHY:
    case LEVEL_ATM:
    case LEVEL_OCEAN:
	return "" ;
    case LEVEL_ISOBARIC: 
    case LEVEL_PDG:
    case LEVEL_LPDG:
    case LEVEL_LISH: 
	return "hPa";
    case LEVEL_LISO: 
    case LEVEL_LISM: 
	return "kPa";
    case LEVEL_FH: 
    case LEVEL_FHG: 
    case LEVEL_DBS: 
	return "meters";
    case LEVEL_LFHM: 
    case LEVEL_LFHG: 
	return "hm" ;
    case LEVEL_SIGMA: 
	return ".0001";		/* dimensionless */
    case LEVEL_LS: 
	return ".01";		/* dimensionless */
    case LEVEL_Bls: 
    case LEVEL_LBls: 
    case LEVEL_FHGH:
	return "cm";
    case LEVEL_ISEN:
    case LEVEL_LISEN:
	return "degK";
    case LEVEL_LSH: 
	return ".001";
    case LEVEL_PV:
	return ".000001 K m2/kg/sec";
    }
    /* default */
    return "unknown" ;
}
