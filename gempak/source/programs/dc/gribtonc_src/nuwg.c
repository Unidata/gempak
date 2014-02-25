/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: nuwg.c,v 1.9 1995/11/17 20:41:31 russ Exp $ */


/*
 * Permits some customization of which netCDF names are used in netCDF files
 * for gridded data that follow the NUWG convention.  Although a specific
 * default name is used, this interface allows a user to check other names
 * or aliases for NUWG dimension or variable names.  Ultimately this should
 * be table-driven, with the table parsed to initialize the tests done here,
 * so that recompilation is not necessary to change names.
 */


#include <netcdf.h>
#include "ulog.h"
#include "nuwg.h"


/*
 * Returns name for specified NUWG component, or -1 if there is no
 * such component.
 */
char *
nuwg_name(which)
    enum ncpart which;
{
    switch(which) {
    case DIM_ACCUM:
	return "accum";
    case DIM_FHG:
	return "fhg";
    case DIM_LEVEL:
	return "level";
    case DIM_NAV:
	return "nav";

    case DIM_SIGMA:
	return "sigma";
    case DIM_HY:
	return "hyb";
    case DIM_FH:
	return "fh";
    case DIM_BLS:
	return "bls";
    case DIM_ISEN:
	return "isen";
    case DIM_PDG:
	return "pdg";
    case DIM_FHGH:
	return "fhgh";
    case DIM_DBS:
	return "dbs";

    case DIM_LBLS:
	return "lbls";
    case DIM_LFHG:
	return "lfhg";
    case DIM_LFHM:
	return "lfhm";
    case DIM_LHY:
	return "lhy";
    case DIM_LISEN:
	return "lisen";
    case DIM_LISH:
	return "lish";
    case DIM_LISM:
	return "lism";
    case DIM_LISO:
	return "liso";
    case DIM_LPDG:
	return "lpdg";
    case DIM_LSH:
	return "lsh";
    case DIM_LS:
	return "ls";

    case DIM_NGRIDS:
	return "ngrids";
    case VAR_REFTIME:
	return "reftime";
    case VAR_VALTIME:
	return "valtime";
    case VAR_GRID_TYPE_CODE:
	return "grid_type_code";
    case VAR_GRID_CENTER:
	return "grid_center";
    case VAR_GRID_NUMBER:
	return "grid_number";
    case VAR_RESCOMP:
	return "ResCompFlag";
    case VAR_MODELID:
	return "model_id";
    case VAR_DI:
	return "Di";
    case VAR_DJ:
	return "Dj";
    case VAR_DX:
	return "Dx";
    case VAR_DY:
	return "Dy";
    case VAR_J:
	return "J";
    case VAR_K:
	return "K";
    case VAR_LA1:
	return "La1";
    case VAR_LA2:
	return "La2";
    case VAR_LAP:
	return "Lap";
    case VAR_LATIN:
	return "Latin";
    case VAR_LATIN1:
	return "Latin1";
    case VAR_LATIN2:
	return "Latin2";
    case VAR_LO1:
	return "Lo1";
    case VAR_LO2:
	return "Lo2";
    case VAR_LOP:
	return "Lop";
    case VAR_LOV:
	return "Lov";
    case VAR_M:
	return "M";
    case VAR_MODE:
	return "Mode";
    case VAR_N:
	return "N";
    case VAR_NI:
	return "Ni";
    case VAR_NJ:
	return "Nj";
    case VAR_NR:
	return "Nr";
    case VAR_NX:
	return "Nx";
    case VAR_NY:
	return "Ny";
    case VAR_ORIENTATION:
	return "Orientation";
    case VAR_PROJFLAG:
	return "ProjFlag";
    case VAR_ROTANGLE:
	return "RotAngle";
    case VAR_ROTLAT:
	return "RotLat";
    case VAR_ROTLON:
	return "RotLon";
    case VAR_SPLAT:
	return "SpLat";
    case VAR_SPLON:
	return "SpLon";
    case VAR_STRETCHFACTOR:
	return "StretchFactor";
    case VAR_STRETCHLAT:
	return "StretchLat";
    case VAR_STRETCHLON:
	return "StretchLon";
    case VAR_TYPE:
	return "Type";
    case VAR_XO:
	return "Xo";
    case VAR_XP:
	return "Xp";
    case VAR_YO:
	return "Yo";
    case VAR_YP:
	return "Yp";
    default:
	uerror("nuwg_name() called for bad component: %d", which);
	break;
    }
    /* default */
    return 0;
}


/*
 * Returns dimension ID for specified NUWG dimension, or -1 if there is no
 * such dimension.
 */
int
nuwg_getdim(ncid, which)
    int ncid;
    enum ncpart which;
{
    return ncdimid(ncid, nuwg_name(which));
}


/*
 * Returns variable ID for specified NUWG variable, or -1 if there is no
 * such variable.
 */
int
nuwg_getvar(ncid, which)
    int ncid;
    enum ncpart which;
{
    return ncvarid(ncid, nuwg_name(which));
}
