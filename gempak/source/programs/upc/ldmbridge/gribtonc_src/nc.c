/*
 *	Copyright 1992 University Corporation for Atmospheric Research
 *	   Not for Direct Resale. All copies to include this notice.
 */
/* $Id: nc.c,v 1.28 1995/12/12 23:28:19 russ Exp $ */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <netcdf.h>

#include "ulog.h"
#include "mkdirs_open.h"
#include "nc.h"
#include "nuwg.h"
#include "emalloc.h"
#include "params.h"
#include "units.h"
#include "levels.h"
#include "timeunits.h"
#include "gbds.h"		/* only for FILL_VAL */
#include "gdes.h"

#ifndef FILL_NAME
#define FILL_NAME	"_FillValue"
#endif


typedef struct levels_table {
    int id;			/* dimension id of netCDF dimension */
    float *vals;		/* levels */
    long num;			/* number of levels */
    utUnit *bunitp;		/* level units */
} levels_table;


typedef struct layers_table {
    int id;			/* dimension id of netCDF dimension */
    float *tops;		/* top levels of layers  */
    float *bots;		/* bottom levels of layers  */
    long num;			/* number of layers */
    utUnit *bunitp;		/* top (and bottom) level units */
} layers_table;


typedef struct tris_table {
    int id;			/* dimension id of netCDF dimension */
    float *starts;		/* starts of time ranges */
    float *ends;		/* ends of time ranges */
    long num;			/* number of ranges */
} tris_table;


#ifdef __STDC__
static ncdim* new_dim(int dimid);
static ncvar* new_var(char* ncname, int varid);
static void free_var(ncvar* var);
static void free_dim(ncdim* dim);
static char* parmname(ncfile* nc, int parm, int level);
static int make_ncfile(char* ncname, ncfile* out);
static void free_ncfile(ncfile* np);
static long getrec(ncfile* nc, double reftime, double valtime);
static levels_table* getlevtab(ncfile* nc, ncvar* var);
static layers_table* getlaytab(ncfile* nc, ncvar* var);
static long getlev(product_data* pp, ncfile* nc,
		   ncvar* var);
static long gettri(product_data* pp, ncfile* nc,
		   ncvar* var);
static int make_var(char* ncname, int varid, ncvar* out);
static int var_as_int(ncfile* nc, enum ncpart comp, int* val);
static int var_as_float(ncfile* nc, enum ncpart comp, float* val);
static int var_as_lset(ncfile* nc, enum ncpart comp, lset* list);
static void varerr(ncfile* nc, enum ncpart comp);
static int make_navgrid(ncfile* nc, navinfo* nav);
static int make_navinfo(ncfile* nc, navinfo* nav);
static void free_navinfo(navinfo* np);
static navinfo* new_navinfo(ncfile* nc);
static int gd_fne_err(product_data* pp, ncfile* nc, enum ncpart comp, double pval, double nval);
static int gd_ine_err(product_data* pp, ncfile* nc, enum ncpart comp, int pval, int nval);
static int gd_igt_err(product_data* pp, ncfile* nc, enum ncpart comp, int pval, int nval);
static int check_gd(product_data* pp, ncfile* nc);
static int check_pp(product_data* pp, ncfile* nc);
static int subgrid(ncfile* nc, product_data* pp, long* ix0, long* ix1);
#endif

int ncid;			/* netCDF id of open netCDF output file.
				 * This is at file scope so routine registered
				 * with atexit() can get it to close file. */

static ncdim *
new_dim(dimid)
    int dimid;
{
    char dimname[MAX_NC_NAME];
    long size;
    ncdim *out = (ncdim *)emalloc(sizeof(ncdim));
    
    if (ncdiminq(ncid, dimid, dimname, &size) == -1) {
	return 0;
    }
    out->name = estrdup(dimname);
    out->id = dimid;
				/* We don't cache size because it may change,
				   e.g. adding records, and we don't want to
				   have to keep cached value up to date.  To
				   get size, use ncdiminq(). */

    return out;
}


static int
make_var(ncname, varid, out)
    char *ncname;		/* netCDF pathanme, only used in error msg */
    int varid;			/* variable ID */
    ncvar *out;			/* place to put constructed ncvar */
{
    char varname[MAX_NC_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int id;
    
    if(ncvarinq(ncid, varid, varname, &type, &ndims, dims, (int *)0) == -1) {
	return -1;
    }

    out->id = varid;
    out->name = estrdup(varname);
    out->type = type;
    out->ndims = ndims;
    out->dims = (int *)emalloc(ndims * sizeof(int));
    for (id = 0; id < ndims; id++)
	out->dims[id] = dims[id];

				/* get value of _FillValue attribute, if any,
				   as a float */
    {
	nc_type atttype;
	int attlen;

	if (ncattinq(ncid, varid, FILL_NAME, &atttype, &attlen) == -1) {
	    out->fillval = 0;		/* no fill-value attribute */
	} else {
	    union {
		char c;
		unsigned char u;
		short s;
		long l;
		float f;
		double d;
	    }fillval;
	    
	    out->fillval = (float *) emalloc(sizeof(float));

	    if(ncattget(ncid, varid, FILL_NAME, (void *)&fillval) == -1) {
		uerror("%s: can't get % attribute for variable %s",
		       ncname, FILL_NAME, varname);
		free(out->dims);
		return -1;
	    }
	    switch(atttype) {
	    case NC_CHAR: 
		*out->fillval = fillval.c;
		break;
	    case NC_BYTE:
		*out->fillval = fillval.u;
		break;
	    case NC_SHORT:
		*out->fillval = fillval.s;
		break;
	    case NC_LONG:
		*out->fillval = fillval.l;
		break;
	    case NC_FLOAT:
		*out->fillval = fillval.f;
		break;
	    case NC_DOUBLE:
		*out->fillval = fillval.d;
		break;
	    }
	}
    }

    if (get_units(ncid, varid, &out->bunitp) == -1) {
	uerror("%s: can't get units attribute for variable %s",
	       ncname, varname);
	return -1;
    }

    if (out->bunitp && grib_pcode(varname) != -1) {
	out->uc = uconv(varname, out->bunitp);
    } else {
	out->uc = 0;
    }
    return 0;
}


static void
free_var(var)
    ncvar *var;
{
    if (var) {
	if(var->name)
	    free(var->name);
	if(var->dims)
	    free(var->dims);
	if(var->bunitp)
	    free(var->bunitp);
	if(var->fillval)
	    free(var->fillval);
	free(var);
    }
}


/*
 * Creates a new ncvar structure and fills it in with the information from
 * the open netCDF file whose handle is ncid.  A pointer to the structure is
 * returned, or 0 on failure.
 */
static ncvar *
new_var(ncname, varid)
    char *ncname;
{
    ncvar *out;

    if (varid == -1)		/* handle common failure case with message
				   at higher level */
	return 0;

    out = (ncvar *)emalloc(sizeof(ncvar));
    if (make_var(ncname, varid, out) != 0) {
	free_var(out);
	return 0;
    }

    return out;
}


static void
free_dim(dim)
    ncdim *dim;
{
    if (dim) {
	if(dim->name)
	    free(dim->name);
	free(dim);
    }
}


/*
 * Name of environment variable containing directory in which to search for
 * CDL files if not found relative to directory from which this process is
 * invoked.
 */
#define LDM_ETCDIR	"LDM_ETCDIR"

/*
 * Checks to see if netCDF file with specified name exists.  If not,
 * makes netCDF file from CDL template file.
 * Returns netCDF file ID on success, or -1 on error.
 */
int
cdl_netcdf (cdlname, ncname)
     char *cdlname;	/* CDL file specifying netCDF structure */
     char *ncname;	/* filename of netcdf file to be created */
{
    char cmnd[2*_POSIX_PATH_MAX+20];
    char *cdlfile = cdlname;
    static char *cdldir = 0;
    char envcdl[_POSIX_PATH_MAX];

    ncopts = 0;			/* turn off netCDF error messages from
				   library, means we will have to check all
				   netCDF status returns and interpret */

    if (access(ncname, (R_OK|W_OK)) != 0) { /* no writable ncname exists */
	if (cdlfile == 0) {
	    uerror("%s doesn't exist, and didn't specify a CDL filename",
		   ncname);
	    /* *** Should create a generic netCDF file anyway? *** */
	    return -1;
	}

	/* Try to create any directories in output path that don't exist */
	if (diraccess(ncname,  (R_OK | W_OK), !0) == -1) {
	    serror("can't access directories leading to %s", ncname);
	    return -1;
	}

	/* If CDL file not found, look in environment variable LDM_ETCDIR */
	if (access(cdlname, R_OK) == -1) { /* no CDL file or unreadable */
	    if (cdldir == 0) {	/* executed once, when cdldir first needed */
		char *ldm_etcdir = getenv(LDM_ETCDIR);
		int slen;
		
		if (ldm_etcdir == 0) {
		    uerror("CDL file %s not found & LDM_ETCDIR not in environment",
			   cdlname);
		    return -1;
		}
		slen = strlen(ldm_etcdir);
		cdldir = (char *)emalloc((slen+2) * sizeof(char));
		strcpy(cdldir, ldm_etcdir);
		if (cdldir[slen-1] != '/') { /* append "/" to dir name */
		    strcat(cdldir, "/");
		}
	    }
	    strcat(envcdl,cdldir);
	    strcat(envcdl,cdlname);
	    if (access(envcdl, R_OK) == -1) {
		uerror("can't find CDL file %s, or unreadable", envcdl);
		return -1;
	    }
	    cdlfile = envcdl;
	}
	
	(void) strcpy(cmnd, "ncgen -o ");
	(void) strcat(cmnd, ncname);
	(void) strcat(cmnd , " ");
	(void) strcat(cmnd, cdlfile);
	
	if (system(cmnd) != 0) {
	    uerror("can't run \"%s\"", cmnd);
	    return -1;
	}
    }
    return ncopen(ncname, NC_WRITE);
}


void
setncid(id)
    int id;
{
    ncid = id;
}


int
getncid()
{
    return ncid;
}


/* Close open netCDF file, if any */
void
nccleanup()
{
    ncclose(ncid);
}


/*
 * Returns record number determined by (reftime,valtime) pair.  If
 * (reftime,valtime) is in table, returns corresponding record.  If
 * (reftime,valtime) is not in table, increments record count, enters
 * (reftime, valtime) for new record in table, updates reftime, valtime in
 * netCDF file, and returns record.  Returns -1 on failure.
 */
static long
getrec(nc, reftime, valtime)
    ncfile *nc;
    double reftime;
    double valtime;
{
    static int nrecs = 0;	/* number of records */
#define INIT_SIZE 32		/* initial size of (reftime,valtime) table */
    static int size = INIT_SIZE;
    static double *reftimes = 0;
    static double *valtimes = 0;
    double *dp;
    int i;

    if (reftimes == 0) {	/* one-time initializatin on first call */
	reftimes = (double *) emalloc(INIT_SIZE * sizeof(double));
	valtimes = (double *) emalloc(INIT_SIZE * sizeof(double));
    }

    /* First look in table of existing records */
    for (i=0; i < nrecs; i++) {
	if (reftime == reftimes[i] && valtime == valtimes[i])
	    return i;
    }

    /* Didn't find it, so create a new record */
    if (nrecs+1 == size) {	/* no room for another record, double size of
				   table before adding */
	size *= 2;
	dp = reftimes;
	reftimes = (double *) emalloc(size * sizeof(double));
	for (i = 0; i < nrecs; i++)
	    reftimes[i] = *dp++;
	dp = valtimes;
	valtimes = (double *) emalloc(size * sizeof(double));
	for (i = 0; i < nrecs; i++)
	    valtimes[i] = *dp++;
    }
    reftimes[nrecs] = reftime;
    valtimes[nrecs] = valtime;
				
    {				/* Update reftimes and valtimes in ncfile */
	long ix = nrecs;

	if (ncvarput1(ncid, nc->reftimeid, &ix, (void *)&reftime) == -1 ||
	    ncvarput1(ncid, nc->valtimeid, &ix, (void *)&valtime) == -1) {
	    uerror("%s: failed to add new reftime, valtime", nc->ncname);
	    return -1;
	}
    }
    nrecs++;
    return nrecs-1;
}


/*
 * Creates and returns a pointer to a one-dimensional table of levels
 * from the currently-open netCDF file (handle ncid).  Returns 0 on
 * failure.
 */
static levels_table*
getlevtab(nc, var)
    ncfile *nc;			/* currently open netCDF file */
    ncvar *var;			/* level variable */
{
    int did;
    levdim *lp = nc->levdims;
    
    /* See if appropriate levels table already exists for this file */
    if (var->ndims < 4) {
	uerror("%s: variable %s has too few dimensions for a level",
	       nc->ncname, var->name);
	return 0;
    }
    did = var->dims[1];	/* level dimension */

    while (lp) {
	if(did == lp->levtab->id) {	/* found it */
	    return lp->levtab;
	}
	lp = lp->next;
    }
    /* Not, there, so we must create it */
    lp = (levdim *)emalloc(sizeof(levdim));
    lp->levtab = (levels_table *)emalloc(sizeof(levels_table));
    lp->levtab->id = did;
    lp->next = nc->levdims;

    /* Initialize array of levels */
    {
	levels_table *out = lp->levtab;
	char levname[MAX_NC_NAME];
	int levvarid;		/* variable id of level variable */
	ncvar *lev;		/* level variable */
	long start = 0;

        /* get number of levels */
	if (ncdiminq(ncid, out->id, levname, &out->num) == -1) {
	    uerror("%s: can't get number of %s levels", nc->ncname, var->name);
	    return 0;
	}
	out->vals = (float *) emalloc(out->num * sizeof(float));
	levvarid = ncvarid(ncid, levname);
	if(levvarid == -1) {
	    uerror("%s: No %s coordinate variable for %s level",
		   nc->ncname, levname, var->name);
	    return 0;
	}
	lev = nc->vars[levvarid];
				/* Check consistency of lev variable */
	if (strcmp(lev->name, levname) != 0 ||
	    lev->type != NC_FLOAT ||
	    lev->ndims != 1 ||
	    lev->dims[0] != out->id) {
	    uerror("%s: variable %s must be float %s(%s)", nc->ncname,
		   levname, lev->name, lev->name);
	    return 0;
	}

	if(get_units(ncid, levvarid, &out->bunitp) == -1) {
	    uerror("%s: error getting units attribute for %s",
		   nc->ncname, levname);
	    return 0;
	}

	if(ncvarget(ncid, levvarid, &start, &out->num,
		    (void *)out->vals) ==-1) {
	    uerror("%s: no %s variable for level",
		   nc->ncname, levname);
	    return 0;
	}
    }

    nc->levdims = lp;		/* if all goes well */
    return lp->levtab;
}


/*
 * Creates and returns a pointer to a one-dimensional table of layers
 * from the currently-open netCDF file (handle ncid).  Returns 0 on
 * failure.
 */
static layers_table*
getlaytab(nc, var)
    ncfile *nc;			/* currently open netCDF file */
    ncvar *var;			/* layer variable */
{
    int did;
    laydim *lp = nc->laydims;
    
    /* See if appropriate layers table already exists for this file */
    if (var->ndims < 4) {
	uerror("%s: variable %s has too few dimensions for a layer",
	       nc->ncname, var->name);
	return 0;
    }
    did = var->dims[1];	/* layer dimension */

    while (lp) {
	if(did == lp->laytab->id) {	/* found it */
	    return lp->laytab;
	}
	lp = lp->next;
    }
    /* Not, there, so we must create it */
    lp = (laydim *)emalloc(sizeof(laydim));
    lp->laytab = (layers_table *)emalloc(sizeof(layers_table));
    lp->laytab->id = did;
    lp->next = nc->laydims;

    /* Initialize array of layers */
    {
	layers_table *out = lp->laytab;
	char layname[MAX_NC_NAME];
	char topname[MAX_NC_NAME];
	char botname[MAX_NC_NAME];
	int topvarid;		/* variable id of layer top variable */
	int botvarid;		/* variable id of layer bottom variable */
	ncvar *top;		/* layer top variable */
	ncvar *bot;		/* layer bottom variable */
	long start = 0;

        /* get number of layers */
	if (ncdiminq(ncid, out->id, layname, &out->num) == -1) {
	    uerror("%s: can't get number of %s layers", nc->ncname, var->name);
	    return 0;
	}
	if (strlen(layname)  +  strlen("_top") > (size_t) MAX_NC_NAME) {
	    uerror("%s: name of layer dimension too long (%s)",
		   nc->ncname, layname);
	    return 0;
	}
	out->tops = (float *) emalloc(out->num * sizeof(float));
	strcpy(topname, layname);
	strcat(topname, "_top");
	topvarid = ncvarid(ncid, topname);
	if(topvarid == -1) {
	    uerror("%s: No %s coordinate variable for %s layer top",
		   nc->ncname, layname, var->name);
	    return 0;
	}
	top = nc->vars[topvarid];
				/* Check consistency of top variable */
	if (strcmp(top->name, topname) != 0 ||
	    top->type != NC_FLOAT ||
	    top->ndims != 1 ||
	    top->dims[0] != out->id) {
	    uerror("%s: variable %s must be float %s(%s)", nc->ncname,
		   layname, top->name, top->name);
	    return 0;
	}

	if(get_units(ncid, topvarid, &out->bunitp) == -1) {
	    uerror("%s: error getting units attribute for %s",
		   nc->ncname, topname);
	    return 0;
	}

	if(ncvarget(ncid, topvarid, &start, &out->num,
		    (void *)out->tops) ==-1) {
	    uerror("%s: no %s variable for top of layer",
		   nc->ncname, topname);
	    return 0;
	}
	out->bots = (float *) emalloc(out->num * sizeof(float));
	strcpy(botname, layname);
	strcat(botname, "_bot");
	botvarid = ncvarid(ncid, botname);
	if(botvarid == -1) {
	    uerror("%s: No %s coordinate variable for %s layer bot",
		   nc->ncname, layname, var->name);
	    return 0;
	}
	bot = nc->vars[botvarid];
				/* Check consistency of bot variable */
	if (strcmp(bot->name, botname) != 0 ||
	    bot->type != NC_FLOAT ||
	    bot->ndims != 1 ||
	    bot->dims[0] != out->id) {
	    uerror("%s: variable %s must be float %s(%s)", nc->ncname,
		   layname, bot->name, bot->name);
	    return 0;
	}

	if(ncvarget(ncid, botvarid, &start, &out->num,
		    (void *)out->bots) ==-1) {
	    uerror("%s: no %s variable for bottom of layer",
		   nc->ncname, botname);
	    return 0;
	}
    }

    nc->laydims = lp;		/* if all goes well */
    return lp->laytab;
}


/*
 * Handle levels
 */
static long
levaux(pp, nc, var)
    product_data *pp;	/* decoded GRIB data to be written */
    ncfile *nc;		/* netCDF file to be written */
    ncvar *var;		/* netCDF variable to be written */
{
    int level_flg = pp->level_flg;
    char *varname = var->name;

    /*
	Assumes level dimension is second dimension of a variable, and that
	there is a coordinate variable associated with a level, values of
	which get stored in the level table.
      */

    int levdim;
    double lev;
    long levix;

    levels_table *levtab = getlevtab(nc, var);

    if (levtab == 0) {	/* initialize table of levels */
	return -1;
    }

    lev = level1(pp->level_flg, pp->level);

    /* Must convert to units of level table */
    {
	utUnit bfunit;
	char *funits = levelunits(level_flg);
	double slope=1.;
	double intercept=0.;
	if(utScan(funits, &bfunit) != 0) { /* "from" unit */
	    uerror("Error parsing unit `%s' for level %s", funits, varname);
	    return -1;
	}
	if (levtab->bunitp) {
	    if(utConvert(&bfunit, levtab->bunitp, &slope, &intercept) ==
	       UT_ECONVERT) {
		uerror("units `%s' not conformable with variable %s:units",
		       funits, varname);
		return -1;
	    }
	}
	lev = slope * lev + intercept;
    }
    levix = level_index(lev, levtab->vals, levtab->num);
    if (levix == -1) {
	uerror("%s: In %s, no %f level for %s",
	       pp->header, nc->ncname, lev, varname);
	return -1;
    }
    return levix;
}


/*
 * Handle layers
 */
static long
layaux(pp, nc, var)
    product_data *pp;	/* decoded GRIB data to be written */
    ncfile *nc;		/* netCDF file to be written */
    ncvar *var;		/* netCDF variable to be written */
{
    int layer_flg = pp->level_flg;
    char *varname = var->name;

    /*
	Assumes layer dimension is second dimension of a variable, and that
	there are _top and _bot levels associated with each layer, values of
	which which get stored in the layer table.
      */

    int laydim;
    double top;
    double bot;
    long layix;

    layers_table *laytab = getlaytab(nc, var);

    if (laytab == 0) {
	return -1;
    }

    top = pp->level[0];
    bot = pp->level[1];
    /* Must convert top,bot to units of layer tables */
    {
	utUnit bfunit;
	char *funits = levelunits(layer_flg);
	double slope=1.;
	double intercept=0.;
	if(utScan(funits, &bfunit) != 0) { /* "from" unit */
	    uerror("Error parsing unit `%s' for level %s", funits, varname);
	    return -1;
	}
	if(laytab->bunitp) {
	    if(utConvert(&bfunit, laytab->bunitp, &slope, &intercept) ==
	       UT_ECONVERT) {
		uerror("units `%s' not conformable with variable %s:units",
		       funits, varname);
		return -1;
	    }
	}
	top = slope * top + intercept;
	bot = slope * bot + intercept;
    }
    layix = layer_index(top, bot, laytab->tops, laytab->bots, laytab->num);
    if (layix == -1) {
	uerror("%s: In %s, no (%g,%g) level for %s",
	       pp->header, nc->ncname, top, bot, varname);
	return -1;
    }
    return layix;
}


/*
 * Return netCDF level dimension index appropriate for decoded GRIB
 * product.  Returns -2 if no level dimension appropriate (e.g. for surface
 * variables) or -1 in case of failure.
 */
static long
getlev(pp, nc, var)
    product_data *pp;	/* decoded GRIB data to be written */
    ncfile *nc;		/* netCDF file to be written */
    ncvar *var;		/* netCDF variable to be written */
{
    switch (pp->level_flg) {
    /* Levels */
    case LEVEL_ISOBARIC:
    case LEVEL_FHG:
    case LEVEL_SIGMA:
    case LEVEL_HY:
    case LEVEL_FH:
    case LEVEL_Bls:
    case LEVEL_ISEN:
    case LEVEL_PDG:
    case LEVEL_FHGH:
    case LEVEL_DBS:
	return levaux(pp, nc, var);

    /* Layers */
    case LEVEL_LBls:
    case LEVEL_LFHG:
    case LEVEL_LFHM:
    case LEVEL_LHY:
    case LEVEL_LISEN:
    case LEVEL_LISH:
    case LEVEL_LISM:
    case LEVEL_LISO:
    case LEVEL_LPDG:
    case LEVEL_LS:
    case LEVEL_LSH:
	return layaux(pp, nc, var);

    /* Special levels, just one so no dimension needed */
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
	return -2;
    }
    /* default: */
    return -1;
}


/*
 * Get conventional netcdf variable name, with level indicator appended if
 * appropriate, and see if it exists in the open netCDF file.  If so return
 * name.  If not, return 0.  The name is a pointer to a static string, so
 * should be copied if needed beyond the next call to parmname.
 */
static char*
parmname(nc, parm, level)
    ncfile *nc;			/* netCDF file */
    int parm;			/* parameter code from GRIB product */
    int level;			/* level flag from GRIB product */
{
    char *varname = grib_pname(parm); /* netcdf variable base name */
    int ncopts_save = ncopts;
    char *suffix;
    static char string[MAX_NC_NAME];
    char *name = string;

    if (!varname) {
	uerror("unrecognized GRIB parameter code %d", parm);
	return 0;
    }

    /* Add level modifier to name, if appropriate */
    suffix = "";
    suffix = levelsuffix(level);
    strcpy(name, varname);

    /* The "_sfc", "_msl", and "_liso" suffixes are redundant for some
       parameters, so we explicitly exclude those here */
    if((level != LEVEL_SURFACE || !sfcparam(parm)) &&
       (level != LEVEL_MEAN_SEA || !mslparam(parm)) &&
       (level != LEVEL_LISO || !lisoparam(parm))) {
	if (suffix[0] != '\0') {
	    strcat(name, "_");
	    strcat(name, suffix);
	}
    }
    ncopts = 0;
    if (ncvarid(ncid, name) == -1) {
	uerror("%s: no variable %s in netCDF file", nc->ncname, name);
	name = 0;
    }

    ncopts = ncopts_save;
    return name;
}


/*
 * Stores value of a netCDF variable identified by a NUWG conventional id.
 * In case of failure, returns -1.  The value is converted from
 * whatever type is used for the netCDF variable.
 */
static int
var_as_int(nc, comp, val)
    ncfile *nc;
    enum ncpart comp;
    int *val;			/* where to store the resulting value */

{
    ncvar *var = (ncvar *)emalloc(sizeof(ncvar));
    long start[] = {0};
    long count[] = {1};
    double buf[1];		/* generic data buffer */
    
    if(make_var(nc->ncname, nuwg_getvar(ncid, comp), var) == -1) {
	free_var(var);
	return -1;
    }
    if (ncvarget(ncid, var->id, start, count, (void *)buf) == -1) {
	free_var(var);
	return -1;
    }
    switch (var->type) {	/* return the value as an int, no
				   matter how it is stored */
      case NC_BYTE:
	*val = *(unsigned char *) buf;
	break;
      case NC_CHAR:
	*val = *(char *) buf;
	break;
      case NC_SHORT:
	*val = *(short *) buf;
	break;
      case NC_LONG:
	*val = *(long *) buf;
	break;
      case NC_FLOAT:
	*val = *(float *) buf;
	break;
      case NC_DOUBLE:
	*val = *(double *) buf;
	break;
    }
    free_var(var);
    return 0;
}


/*
 * Stores value of a netCDF variable identified by a NUWG conventional id.
 * In case of failure, returns -1.  The value is converted from
 * whatever type is used for the netCDF variable.
 */
static int
var_as_float(nc, comp, val)
    ncfile *nc;
    enum ncpart comp;
    float *val;
{
    ncvar *var = (ncvar *)emalloc(sizeof(ncvar));
    long start[] = {0};
    long count[] = {1};
    double buf[1];		/* generic data buffer */
    
    if(make_var(nc->ncname, nuwg_getvar(ncid, comp), var) == -1) {
	return -1;
    }
    if (ncvarget(ncid, var->id, start, count, (void *)buf) == -1) {
	free_var(var);
	return -1;
    }
    switch (var->type) {	/* return the value as a float, no
				   matter how it is stored */
      case NC_BYTE:
	*val = *(unsigned char *) buf;
	break;
      case NC_CHAR:
	*val = *(char *) buf;
	break;
      case NC_SHORT:
	*val = *(short *) buf;
	break;
      case NC_LONG:
	*val = *(long *) buf;
	break;
      case NC_FLOAT:
	*val = *(float *) buf;
	break;
      case NC_DOUBLE:
	*val = *(double *) buf;
	break;
    }
    free_var(var);
    return 0;
}


/*
 * Stores values of a netCDF variable (of longs) identified by a NUWG
 * conventional id.  Values are just stored in a list of longs, which can be
 * used as a set in which values are looked up.  In case of failure, returns
 * -1.
 */
static int
var_as_lset(nc, comp, list)
    ncfile *nc;
    enum ncpart comp;
    lset *list;			/* where to store the resulting list */
{
    ncvar *var = (ncvar *)emalloc(sizeof(ncvar));
    static long start[MAX_VAR_DIMS];
    static long count[MAX_VAR_DIMS];
    long prod;
    int i;
    
    if(make_var(nc->ncname, nuwg_getvar(ncid, comp), var) == -1) {
	return -1;
    }
    if (var->type != NC_LONG) {
	uerror("%s: variable %s must be of type long", nc->ncname,
	       nuwg_name(comp));
	free_var(var);
	return -1;
    }
    prod=1;
    for (i=0; i<var->ndims; i++) {
	start[i] = 0;
	if (ncdiminq(ncid, var->dims[i], (char *)0, &count[i]) == -1) {
	    uerror("%s: can't get size of dimension for %s", nc->ncname,
		   nuwg_name(comp));
	    free_var(var);
	    return -1;
	}
	prod *= count[i];
    }
    list->n = prod;
    list->vals = (long *)emalloc(sizeof(long) * prod);
    if (ncvarget(ncid, var->id, start, count, (void *)list->vals) == -1) {
	uerror("%s: can't get values for %s", nc->ncname, nuwg_name(comp));
	free_var(var);
	free(list->vals);
	return -1;
    }
    free_var(var);
    return 0;
}


static void
varerr(nc,comp)
    ncfile *nc;
    enum ncpart comp;
{
    uerror("%s: no variable for %s", nc->ncname, nuwg_name(comp));
}


/*
 *  Returns 0 on success, -1 on failure.
 */
static int
make_navgrid(nc, nav )
    ncfile *nc;			/* netCDF file */
    navinfo *nav;		/* where to put the navinfo */
{
    int *ip;
    float *fp;

    switch(nav->grid_type_code) {
    case GRID_LL:
    case GRID_RLL:
    case GRID_SLL:
    case GRID_SRLL:
    {
	gdes_ll *gg = &nav->grid.ll;
	
	if (var_as_int(nc, VAR_NI, &gg->ni) == -1) {
	    varerr(nc, VAR_NI);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_NJ, &gg->nj) == -1) {
	    varerr(nc, VAR_NJ);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LA1, &gg->la1) == -1) {
	    varerr(nc, VAR_LA1);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LO1, &gg->lo1) == -1) {
	    varerr(nc, VAR_LO1);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LA2, &gg->la2) == -1) {
	    varerr(nc, VAR_LA2);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LO2, &gg->lo2) == -1) {
	    varerr(nc, VAR_LO2);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_DI, &gg->di) == -1) {
	    varerr(nc, VAR_DI);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_DJ, &gg->dj) == -1) {
	    varerr(nc, VAR_DJ);
	    return -1;
	}
	
	if (nav->grid_type_code == GRID_RLL || nav->grid_type_code == GRID_SRLL) {
	    gg->rot = (rotated *)emalloc(sizeof(rotated));
	
	    if (var_as_float(nc, VAR_ROTLAT, &gg->rot->lat) == -1) {
		varerr(nc, VAR_ROTLAT);
		return -1;
	    }
	    if (var_as_float(nc, VAR_ROTLON, &gg->rot->lon) == -1) {
		varerr(nc, VAR_ROTLON);
		return -1;
	    }
	    if (var_as_float(nc, VAR_ROTANGLE, &gg->rot->angle) == -1) {
		varerr(nc, VAR_ROTANGLE);
		return -1;
	    }
	}
	
	if (nav->grid_type_code == GRID_SLL || nav->grid_type_code == GRID_SRLL) {
	    gg->strch = (stretched *)emalloc(sizeof(stretched));
	
	    if (var_as_float(nc, VAR_STRETCHLAT, &gg->strch->lat) == -1) {
		varerr(nc, VAR_STRETCHLAT);
		return -1;
	    }
	    if (var_as_float(nc, VAR_STRETCHLON, &gg->strch->lon) == -1) {
		varerr(nc, VAR_STRETCHLON);
		return -1;
	    }
	    if (var_as_float(nc, VAR_STRETCHFACTOR, &gg->strch->factor) == -1) {
		varerr(nc, VAR_STRETCHFACTOR);
		return -1;
	    }
	}
    }
    break;
    case GRID_GAU:
    case GRID_RGAU:
    case GRID_SGAU:
    case GRID_SRGAU:
    {
	gdes_gau *gg = &nav->grid.gau;

	if (var_as_int(nc, VAR_NI, &gg->ni) == -1) {
	    varerr(nc, VAR_NI);
	    return -1;
	    }
	
	if (var_as_int(nc, VAR_NJ, &gg->nj) == -1) {
	    varerr(nc, VAR_NJ);
	    return -1;
	    }
	
	if (var_as_float(nc, VAR_LA1, &gg->la1) == -1) {
	    varerr(nc, VAR_LA1);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LO1, &gg->lo1) == -1) {
	    varerr(nc, VAR_LO1);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LA2, &gg->la2) == -1) {
	    varerr(nc, VAR_LA2);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LO2, &gg->lo2) == -1) {
	    varerr(nc, VAR_LO2);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_DI, &gg->di) == -1) {
	    varerr(nc, VAR_DI);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_N, &gg->n) == -1) {
	    varerr(nc, VAR_N);
	    return -1;
	}

	if (nav->grid_type_code == GRID_RLL || nav->grid_type_code == GRID_SRLL) {
	    gg->rot = (rotated *)emalloc(sizeof(rotated));
	
	    if (var_as_float(nc, VAR_ROTLAT, &gg->rot->lat) == -1) {
		varerr(nc, VAR_ROTLAT);
		return -1;
	    }
	    if (var_as_float(nc, VAR_ROTLON, &gg->rot->lon) == -1) {
		varerr(nc, VAR_ROTLON);
		return -1;
	    }
	    if (var_as_float(nc, VAR_ROTANGLE, &gg->rot->angle) == -1) {
		varerr(nc, VAR_ROTANGLE);
		return -1;
	    }
	}
	
	if (nav->grid_type_code == GRID_SLL || nav->grid_type_code == GRID_SRLL) {
	    gg->strch = (stretched *)emalloc(sizeof(stretched));
	
	    if (var_as_float(nc, VAR_STRETCHLAT, &gg->strch->lat) == -1) {
		varerr(nc, VAR_STRETCHLAT);
		return -1;
	    }
	    if (var_as_float(nc, VAR_STRETCHLON, &gg->strch->lon) == -1) {
		varerr(nc, VAR_STRETCHLON);
		return -1;
	    }
	    if (var_as_float(nc, VAR_STRETCHFACTOR, &gg->strch->factor) == -1) {
		varerr(nc, VAR_STRETCHFACTOR);
		return -1;
	    }
	}
    }
    break;
    case GRID_SPH:
    case GRID_RSPH:
    case GRID_SSPH:
    case GRID_SRSPH:
    {
	gdes_sph *gg = &nav->grid.sph;

	if (var_as_int(nc, VAR_J, &gg->j) == -1) {
	    varerr(nc, VAR_J);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_K, &gg->k) == -1) {
	    varerr(nc, VAR_K);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_M, &gg->m) == -1) {
	    varerr(nc, VAR_M);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_TYPE, &gg->type) == -1) {
	    varerr(nc, VAR_TYPE);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_MODE, &gg->mode) == -1) {
	    varerr(nc, VAR_MODE);
	    return -1;
	}
	if (nav->grid_type_code == GRID_RLL || nav->grid_type_code == GRID_SRLL) {
	    gg->rot = (rotated *)emalloc(sizeof(rotated));
	
	    if (var_as_float(nc, VAR_ROTLAT, &gg->rot->lat) == -1) {
		varerr(nc, VAR_ROTLAT);
		return -1;
	    }
	    if (var_as_float(nc, VAR_ROTLON, &gg->rot->lon) == -1) {
		varerr(nc, VAR_ROTLON);
		return -1;
	    }
	    if (var_as_float(nc, VAR_ROTANGLE, &gg->rot->angle) == -1) {
		varerr(nc, VAR_ROTANGLE);
		return -1;
	    }
	}
	
	if (nav->grid_type_code == GRID_SLL || nav->grid_type_code == GRID_SRLL) {
	    gg->strch = (stretched *)emalloc(sizeof(stretched));
	
	    if (var_as_float(nc, VAR_STRETCHLAT, &gg->strch->lat) == -1) {
		varerr(nc, VAR_STRETCHLAT);
		return -1;
	    }
	    if (var_as_float(nc, VAR_STRETCHLON, &gg->strch->lon) == -1) {
		varerr(nc, VAR_STRETCHLON);
		return -1;
	    }
	    if (var_as_float(nc, VAR_STRETCHFACTOR, &gg->strch->factor) == -1) {
		varerr(nc, VAR_STRETCHFACTOR);
		return -1;
	    }
	}
    }
    break;
    case GRID_MERCAT:
    {
	gdes_mercator *gg = &nav->grid.mercator;

	if (var_as_int(nc, VAR_NI, &gg->ni) == -1) {
	    varerr(nc, VAR_NI);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_NJ, &gg->nj) == -1) {
	    varerr(nc, VAR_NJ);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LA1, &gg->la1) == -1) {
	    varerr(nc, VAR_LA1);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LO1, &gg->lo1) == -1) {
	    varerr(nc, VAR_LO1);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LA2, &gg->la2) == -1) {
	    varerr(nc, VAR_LA2);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LO2, &gg->lo2) == -1) {
	    varerr(nc, VAR_LO2);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LATIN, &gg->latin) == -1) {
	    varerr(nc, VAR_LATIN);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_DI, &gg->di) == -1) {
	    varerr(nc, VAR_DI);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_DJ, &gg->dj) == -1) {
	    varerr(nc, VAR_DJ);
	    return -1;
	}
	
    }
    break;
    case GRID_GNOMON:		/* fall through */
    case GRID_POLARS:
    {
	gdes_polars *gg = &nav->grid.polars;

	if (var_as_int(nc, VAR_NX, &gg->nx) == -1) {
	    varerr(nc, VAR_NX);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_NY, &gg->ny) == -1) {
	    varerr(nc, VAR_NY);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LA1, &gg->la1) == -1) {
	    varerr(nc, VAR_LA1);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LO1, &gg->lo1) == -1) {
	    varerr(nc, VAR_LO1);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LOV, &gg->lov) == -1) {
	    varerr(nc, VAR_LOV);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_DX, &gg->dx) == -1) {
	    varerr(nc, VAR_DX);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_DY, &gg->dy) == -1) {
	    varerr(nc, VAR_DY);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_PROJFLAG, &gg->pole) == -1) {
	    varerr(nc, VAR_PROJFLAG);
	    return -1;
	}
    }
    break;
    case GRID_LAMBERT:
    {
	gdes_lambert *gg = &nav->grid.lambert;

	if (var_as_int(nc, VAR_NX, &gg->nx) == -1) {
	    varerr(nc, VAR_NX);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_NY, &gg->ny) == -1) {
	    varerr(nc, VAR_NY);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LA1, &gg->la1) == -1) {
	    varerr(nc, VAR_LA1);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LO1, &gg->lo1) == -1) {
	    varerr(nc, VAR_LO1);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LOV, &gg->lov) == -1) {
	    varerr(nc, VAR_LOV);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_DX, &gg->dx) == -1) {
	    varerr(nc, VAR_DX);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_DY, &gg->dy) == -1) {
	    varerr(nc, VAR_DY);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_PROJFLAG, &gg->pole) == -1) {
	    varerr(nc, VAR_PROJFLAG);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LATIN1, &gg->latin1) == -1) {
	    varerr(nc, VAR_LATIN1);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LATIN2, &gg->latin2) == -1) {
	    varerr(nc, VAR_LATIN2);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_SPLAT, &gg->splat) == -1) {
	    varerr(nc, VAR_SPLAT);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_SPLON, &gg->splon) == -1) {
	    varerr(nc, VAR_SPLON);
	    return -1;
	}
	
    }
    break;
    case GRID_SPACEV:
    {
	gdes_spacev *gg = &nav->grid.spacev;

	if (var_as_int(nc, VAR_NX, &gg->nx) == -1) {
	    varerr(nc, VAR_NX);
	    return -1;
	}
	
	if (var_as_int(nc, VAR_NY, &gg->ny) == -1) {
	    varerr(nc, VAR_NY);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LAP, &gg->lap) == -1) {
	    varerr(nc, VAR_LAP);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_LOP, &gg->lop) == -1) {
	    varerr(nc, VAR_LOP);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_DX, &gg->dx) == -1) {
	    varerr(nc, VAR_DX);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_DY, &gg->dy) == -1) {
	    varerr(nc, VAR_DY);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_XP, &gg->xp) == -1) {
	    varerr(nc, VAR_XP);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_YP, &gg->yp) == -1) {
	    varerr(nc, VAR_YP);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_ORIENTATION, &gg->orient) == -1) {
	    varerr(nc, VAR_ORIENTATION);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_NR, &gg->nr) == -1) {
	    varerr(nc, VAR_NR);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_XO, &gg->xo) == -1) {
	    varerr(nc, VAR_XO);
	    return -1;
	}
	
	if (var_as_float(nc, VAR_YO, &gg->yo) == -1) {
	    varerr(nc, VAR_YO);
	    return -1;
	}
	
    }
    break;
    case GRID_ALBERS:
    case GRID_OLAMBERT:
    case GRID_UTM:
    case GRID_SIMPOL:
    case GRID_MILLER:
    default:
	uerror("%s: can't handle %s grids", nc->ncname,
	       gds_typename(nav->grid_type_code));
	return -1;
    }
    return 0;
}


/*
 * Make an in-memory structure for netCDF navigation information and
 * initialize it from specified file.  We cache this information because
 * some of it must be consulted for every decoded product, and it won't
 * change.  Returns 0 on success, -1 on failure.
 */
static int
make_navinfo(nc, nav)
    ncfile *nc;			/* netCDF file */
    navinfo *nav;		/* where to put the navinfo */
{
    char *cp;
    int *ip;
    float *fp;
    
    nav->navid = nuwg_getdim(ncid, DIM_NAV);

    if (var_as_int(nc, VAR_GRID_TYPE_CODE, &nav->grid_type_code) == -1) {
	varerr(nc, VAR_GRID_TYPE_CODE);
	return -1;
    }

    if (var_as_int(nc, VAR_GRID_CENTER, &nav->grid_center) == -1) {
	varerr(nc, VAR_GRID_CENTER);
	return -1;
    }

    /* Multiple grid numbers allowed, they get stitched together */
    if (var_as_lset(nc, VAR_GRID_NUMBER, &nav->grid_numbers) == -1) {
	varerr(nc, VAR_GRID_NUMBER);
	return -1;
    }

    /* GRIB resolution and component flags */
    if (var_as_int(nc, VAR_RESCOMP, &nav->rescomp) == -1) {
	varerr(nc, VAR_RESCOMP);
	return -1;
    }

    if (make_navgrid(nc, nav) == -1) {
	return -1;
    }

    return 0;
}


static void
free_navinfo(np)
    navinfo* np;
{
    if (np) {
	if(np->nav_model)
	    free(np->nav_model);
	free(np);
	if(np->grid_numbers.vals)
	    free(np->grid_numbers.vals);
    }
}


static navinfo *
new_navinfo(nc)
    ncfile *nc;
{
    navinfo *out = (navinfo *)emalloc(sizeof(navinfo));

    if (make_navinfo(nc, out) != 0) {
	free_navinfo(out);
	return 0;
    }

    return out;
}


/*
 * Make an in-memory structure for netCDF information and initialize it from
 * specified file.  We cache this information because some of it must be
 * consulted for every decoded product, and it won't change.
 */
static int
make_ncfile(ncname, out)
    char *ncname;
    ncfile *out;
{
    int ndims;			/* number of dimensions */
    int nvars;			/* number of variables */
    int recid;
    long nrecs;			/* number of records */
    int dimid;
    int varid;
    int *ip;

    out->ncname = estrdup(ncname);
    out->ncid = ncid;

    if (ncinquire(ncid, &ndims, &nvars, (int *)0, &recid) == -1) {
	uerror("%s: ncinquire() failed", ncname);
	return -1;
    }
    out->ndims = ndims;
    out->nvars = nvars;
    out->dims = (ncdim **)emalloc(ndims * sizeof(ncdim *));
    for (dimid = 0; dimid < ndims; dimid++) {
	out->dims[dimid] = new_dim(dimid);
    }
    out->vars = (ncvar **)emalloc(nvars * sizeof(ncvar *));
    for (varid = 0; varid < nvars; varid++) {
	out->vars[varid] = new_var(ncname, varid);
    }

    if (recid == -1) {
	uerror("%s: no record dimension", ncname);
	return -1;
    }
    out->recid = recid;
    out->reftimeid = nuwg_getvar(ncid, VAR_REFTIME);
    out->valtimeid = nuwg_getvar(ncid, VAR_VALTIME);

    if(out->reftimeid == -1) {
	uerror("%s: no reftime variable", ncname);
	return -1;
    }
    if(out->valtimeid == -1) {
	uerror("%s: no valtime variable", ncname);
	return -1;
    }

    /* initialize reftime,valtime,record table */
    if (ncdiminq(ncid, recid, (char *)0, &nrecs) == -1) {
	uerror("%s: can't get number of records", ncname);
	return -1;
    }
    if (nrecs > 0) {
	int nerrs = 0;
	int irec;
	double *reftimes = (double *)emalloc(nrecs*sizeof(double));
	double *valtimes = (double *)emalloc(nrecs*sizeof(double));
	long start[1];
	long count[1];
	start[0] = 0;
	count[0] = nrecs;
	if (ncvarget(ncid, out->reftimeid, start, count, (void *)reftimes)
	    == -1) {
	    uerror("%s: can't get reftimes", out->ncname);
	    nerrs++;
	}
	if (ncvarget(ncid, out->valtimeid, start, count, (void *)valtimes )
	    == -1) {
	    uerror("%s: can't get valtimes", out->ncname);
	    nerrs++;
	}
	if(nerrs == 0) {
	    for (irec = 0; irec < nrecs; irec++) {
		if(getrec(out, reftimes[irec], valtimes[irec]) == -1) {
		    uerror("%s: problem in initializing reftime,valtim",
			   out->ncname);
		    nerrs++;
		}
	    }
	}
	free(reftimes);
	free(valtimes);	    
	if(nerrs)
	    return -1;
    }

    /* Multiple model numbers allowed, e.g. for initialization */
    if (var_as_lset(out, VAR_MODELID, &out->models) == -1) {
	varerr(out, VAR_MODELID);
	return -1;
    }
    
    out->nav = new_navinfo(out);
    if (!out->nav) {		/* get navigation information */
	uerror("%s: can't get navigation information", out->ncname);
	return -1;
    }
    out->levdims = 0;		/* only add layer dimensions as needed */
    out->laydims = 0;		/* only add layer dimensions as needed */
    
    return 0;
}


static void
free_ncfile(np)
    ncfile* np;
{
    if (np) {
	if(np->ncname)
	    free(np->ncname);
	if(np->models.vals)
	    free(np->models.vals);
	free(np);
    }
}


/*
 * Creates a new ncfile structure and fills it in with the information from
 * the open netCDF file whose handle is ncid.  A pointer to the structure is
 * returned, or 0 on failure.
 */
ncfile *
new_ncfile(ncname)
    char *ncname;
{
    ncfile *out = (ncfile *)emalloc(sizeof(ncfile));

    if (make_ncfile(ncname, out) != 0) {
	free_ncfile(out);
	return 0;
    }

    return out;
}


/*
 * Print an error message if two specified floating-point values are not
 * sufficiently close.  Returns 1 if not close, zero otherwise.
 */
static int
gd_fne_err(pp, nc, comp, pval, nval)
    product_data *pp;		/* decoded GRIB product */
    ncfile *nc;			/* netCDF file */
    enum ncpart comp;		/* name-independent id of component */
    float pval;			/* first value, from GRIB product */
    float nval;			/* second value, from netCDF file */
{
#define float_near(x,y)	((float)((y) + 0.1*fabs((x)-(y))) == (float)(y)) /* true if x is "close to" y */
    if (! float_near(pval, nval)) {
	uerror("%s, %s nav. mismatch %s: %g != %g",
	       pp->header, nc->ncname, nuwg_name(comp), pval, nval);
	return 1;
    }
    return 0;    
}

/*
 * Print an error message if two specified floating-point values are not
 * integer close.  Returns 1 if not close, zero otherwise.
 */
static int
gd_fnei_err(pp, nc, comp, pval, nval)
    product_data *pp;		/* decoded GRIB product */
    ncfile *nc;			/* netCDF file */
    enum ncpart comp;		/* name-independent id of component */
    float pval;			/* first value, from GRIB product */
    float nval;			/* second value, from netCDF file */
{
    if ((int) (pval+0.5) != (int) (nval+0.5)) {
	uerror("%s, %s nav. mismatch %s: %d != %d",
	       pp->header, nc->ncname, nuwg_name(comp), (int) (pval+0.5), 
	       (int) (nval+0.5));
	return 1;
    }
    return 0;    
}


/*
 * Print an error message if two specified int values are not
 * equal.  Return 1 if not equal, zero otherwise.
 */
static int
gd_ine_err(pp, nc, comp, pval, nval)
    product_data *pp;		/* decoded GRIB product */
    ncfile *nc;			/* netCDF file */
    enum ncpart comp;		/* name-independent id of component */
    int pval;			/* first value, from GRIB product */
    int nval;			/* second value, from netCDF file */
{
    if (pval != nval) {
	uerror("%s, %s nav. mismatch %s: %d != %d",
	       pp->header, nc->ncname, nuwg_name(comp), pval, nval);
	return 1;
    }
    return 0;    
}


/*
 * Print an error message if first int value is greater than second.
 * Return 1 if not equal, zero otherwise.
 */
static int
gd_igt_err(pp, nc, comp, pval, nval)
    product_data *pp;		/* decoded GRIB product */
    ncfile *nc;			/* netCDF file */
    enum ncpart comp;		/* name-independent id of component */
    int pval;			/* first value, from GRIB product */
    int nval;			/* second value, from netCDF file */
{
    if (pval > nval) {
	uerror("%s, %s nav. mismatch %s: %d > %d",
	       pp->header, nc->ncname, nuwg_name(comp), pval, nval);
	return 1;
    }
    return 0;    
}


/*
 * Check consistency of decoded GRIB product grid description section
 * information with the navigation information in the netCDF file.  Return
 * -1 if inconsistency found, 0 otherwise.
 */
static int
check_gd(pp, nc)
    product_data *pp;		/* decoded GRIB product */
    ncfile *nc;			/* netCDF file */
{
    gengrid *gp = &pp->gd->grid;	/* decoded Grid Description Section */
    navinfo *nav = nc->nav;
    gengrid *gn;		/* Grid Description Section info in netCDF */
    int errs = 0;

    if (gp == 0) {
	uerror("%s: no grid description in product?", pp->header);
	return -1;
    }
    if (nav == 0) {
	uerror("%s: no navigation info in netCDF file?", nc->ncname);
	return -1;
    }
    gn = &nc->nav->grid;	/* GDS info in netCDF file */

    switch(nav->grid_type_code) {
    case GRID_LL:
    case GRID_RLL:
    case GRID_SLL:
    case GRID_SRLL:
    {
	gdes_ll *gng = &gn->ll;
	gdes_ll *gpg = &gp->ll;

	errs += gd_igt_err(pp, nc, VAR_NI, gpg->ni, gng->ni);
	errs += gd_igt_err(pp, nc, VAR_NJ, gpg->nj, gng->nj);
	errs += gd_fne_err(pp, nc, VAR_DI, gpg->di, gng->di);
	errs += gd_fne_err(pp, nc, VAR_DJ, gpg->dj, gng->dj);

	if (nav->grid_type_code == GRID_RLL || nav->grid_type_code == GRID_SRLL) {
	    if (gpg->rot == 0) {
		uerror("%s: rotated grid, but no rotation info?",
		       pp->header);
		return -1;
	    }
	    if (gng->rot == 0) {
		uerror("%s: no rotation info for rotated grid in netCDF",
		       nc->ncname);
		return -1;
	    }
	    errs += gd_fne_err(pp, nc, VAR_ROTLAT, gpg->rot->lat, gng->rot->lat);
	    errs += gd_fne_err(pp, nc, VAR_ROTLON, gpg->rot->lon, gng->rot->lon);
	    errs += gd_fne_err(pp, nc, VAR_ROTANGLE, gpg->rot->angle, gng->rot->angle);
	}
	
	if (nav->grid_type_code == GRID_SLL || nav->grid_type_code == GRID_SRLL) {
	    if (gpg->strch == 0) {
		uerror("%s: stretched grid, but no stretch info?",
		       pp->header);
		return -1;
	    }
	    if (gng->strch == 0) {
		uerror("%s: no stretch info for stretched grid in netCDF",
		       nc->ncname);
		return -1;
	    }
	    errs += gd_fne_err(pp, nc, VAR_STRETCHLAT, gpg->strch->lat, gng->strch->lat);
	    errs += gd_fne_err(pp, nc, VAR_STRETCHLON, gpg->strch->lon, gng->strch->lon);
	    errs += gd_fne_err(pp, nc, VAR_STRETCHFACTOR, gpg->strch->factor, gng->strch->factor);
	}
    }
    break;
    case GRID_GAU:
    case GRID_RGAU:
    case GRID_SGAU:
    case GRID_SRGAU:
    {
	gdes_gau *gng = &gn->gau;
	gdes_gau *gpg = &gp->gau;

	errs += gd_ine_err(pp, nc, VAR_NI, gpg->ni, gng->ni);
	errs += gd_ine_err(pp, nc, VAR_NJ, gpg->nj, gng->nj);
	errs += gd_ine_err(pp, nc, VAR_N, gpg->n, gng->n);
	errs += gd_fne_err(pp, nc, VAR_LA1, gpg->la1, gng->la1);
	errs += gd_fne_err(pp, nc, VAR_LO1, gpg->lo1, gng->lo1);
	errs += gd_fne_err(pp, nc, VAR_LA2, gpg->la2, gng->la2);
	errs += gd_fne_err(pp, nc, VAR_LO2, gpg->lo2, gng->lo2);
	errs += gd_fne_err(pp, nc, VAR_DI, gpg->di, gng->di);

	if (nav->grid_type_code == GRID_RLL || nav->grid_type_code == GRID_SRLL) {
	    if (gpg->rot == 0) {
		uerror("%s: rotated grid, but no rotation info?",
		       pp->header);
		return -1;
	    }
	    if (gng->rot == 0) {
		uerror("%s: no rotation info for rotated grid in netCDF",
		       nc->ncname);
		return -1;
	    }
	    errs += gd_fne_err(pp, nc, VAR_ROTLAT, gpg->rot->lat, gng->rot->lat);
	    errs += gd_fne_err(pp, nc, VAR_ROTLON, gpg->rot->lon, gng->rot->lon);
	    errs += gd_fne_err(pp, nc, VAR_ROTANGLE, gpg->rot->angle, gng->rot->angle);
	}
	
	if (nav->grid_type_code == GRID_SLL || nav->grid_type_code == GRID_SRLL) {
	    if (gpg->strch == 0) {
		uerror("%s: stretched grid, but no stretch info?",
		       pp->header);
		return -1;
	    }
	    if (gng->strch == 0) {
		uerror("%s: no stretch info for stretched grid in netCDF",
		       nc->ncname);
		return -1;
	    }
	    errs += gd_fne_err(pp, nc, VAR_STRETCHLAT, gpg->strch->lat, gng->strch->lat);
	    errs += gd_fne_err(pp, nc, VAR_STRETCHLON, gpg->strch->lon, gng->strch->lon);
	    errs += gd_fne_err(pp, nc, VAR_STRETCHFACTOR, gpg->strch->factor, gng->strch->factor);
	}
    }
    break;
    case GRID_SPH:
    case GRID_RSPH:
    case GRID_SSPH:
    case GRID_SRSPH:
    {
	gdes_sph *gng = &gn->sph;
	gdes_sph *gpg = &gp->sph;

	errs += gd_ine_err(pp, nc, VAR_J, gpg->j, gng->j);
	errs += gd_ine_err(pp, nc, VAR_K, gpg->k, gng->k);
	errs += gd_ine_err(pp, nc, VAR_M, gpg->m, gng->m);
	errs += gd_ine_err(pp, nc, VAR_TYPE, gpg->type, gng->type);
	errs += gd_ine_err(pp, nc, VAR_MODE, gpg->mode, gng->mode);

	if (nav->grid_type_code == GRID_RLL || nav->grid_type_code == GRID_SRLL) {
	    if (gpg->rot == 0) {
		uerror("%s: rotated grid, but no rotation info?",
		       pp->header);
		return -1;
	    }
	    if (gng->rot == 0) {
		uerror("%s: no rotation info for rotated grid in netCDF",
		       nc->ncname);
		return -1;
	    }
	    errs += gd_fne_err(pp, nc, VAR_ROTLAT, gpg->rot->lat, gng->rot->lat);
	    errs += gd_fne_err(pp, nc, VAR_ROTLON, gpg->rot->lon, gng->rot->lon);
	    errs += gd_fne_err(pp, nc, VAR_ROTANGLE, gpg->rot->angle, gng->rot->angle);
	}
	
	if (nav->grid_type_code == GRID_SLL || nav->grid_type_code == GRID_SRLL) {
	    if (gpg->strch == 0) {
		uerror("%s: stretched grid, but no stretch info?",
		       pp->header);
		return -1;
	    }
	    if (gng->strch == 0) {
		uerror("%s: no stretch info for stretched grid in netCDF",
		       nc->ncname);
		return -1;
	    }
	    errs += gd_fne_err(pp, nc, VAR_STRETCHLAT, gpg->strch->lat, gng->strch->lat);
	    errs += gd_fne_err(pp, nc, VAR_STRETCHLON, gpg->strch->lon, gng->strch->lon);
	    errs += gd_fne_err(pp, nc, VAR_STRETCHFACTOR, gpg->strch->factor, gng->strch->factor);
	}
    }
    break;
    case GRID_MERCAT:
    {
	gdes_mercator *gng = &gn->mercator;
	gdes_mercator *gpg = &gp->mercator;

	errs += gd_ine_err(pp, nc, VAR_NI, gpg->ni, gng->ni);
	errs += gd_ine_err(pp, nc, VAR_NJ, gpg->nj, gng->nj);
	errs += gd_fne_err(pp, nc, VAR_LA1, gpg->la1, gng->la1);
	errs += gd_fne_err(pp, nc, VAR_LO1, gpg->lo1, gng->lo1);
	errs += gd_fne_err(pp, nc, VAR_LA2, gpg->la2, gng->la2);
	errs += gd_fne_err(pp, nc, VAR_LO2, gpg->lo2, gng->lo2);
	errs += gd_fne_err(pp, nc, VAR_LATIN, gpg->latin, gng->latin);
	errs += gd_fne_err(pp, nc, VAR_DI, gpg->di, gng->di);
	errs += gd_fne_err(pp, nc, VAR_DJ, gpg->dj, gng->dj);
    }
    break;
    case GRID_GNOMON:		/* fall through */
    case GRID_POLARS:
    {
	gdes_polars *gng = &gn->polars;
	gdes_polars *gpg = &gp->polars;

	errs += gd_ine_err(pp, nc, VAR_NX, gpg->nx, gng->nx);
	errs += gd_ine_err(pp, nc, VAR_NY, gpg->ny, gng->ny);
	errs += gd_fne_err(pp, nc, VAR_LA1, gpg->la1, gng->la1);
	errs += gd_fne_err(pp, nc, VAR_LO1, gpg->lo1, gng->lo1);
	errs += gd_fne_err(pp, nc, VAR_LOV, gpg->lov, gng->lov);
	errs += gd_fnei_err(pp, nc, VAR_DX, gpg->dx, gng->dx);
	errs += gd_fnei_err(pp, nc, VAR_DY, gpg->dy, gng->dy);
	errs += gd_ine_err(pp, nc, VAR_PROJFLAG, gpg->pole, gng->pole);
    }
    break;
    case GRID_LAMBERT:
    {
	gdes_lambert *gng = &gn->lambert;
	gdes_lambert *gpg = &gp->lambert;

	errs += gd_ine_err(pp, nc, VAR_NX, gpg->nx, gng->nx);
	errs += gd_ine_err(pp, nc, VAR_NY, gpg->ny, gng->ny);
	errs += gd_fne_err(pp, nc, VAR_LA1, gpg->la1, gng->la1);
	errs += gd_fne_err(pp, nc, VAR_LO1, gpg->lo1, gng->lo1);
	errs += gd_fne_err(pp, nc, VAR_LOV, gpg->lov, gng->lov);
	errs += gd_fnei_err(pp, nc, VAR_DX, gpg->dx, gng->dx);
	errs += gd_fnei_err(pp, nc, VAR_DY, gpg->dy, gng->dy);
	errs += gd_ine_err(pp, nc, VAR_PROJFLAG, gpg->pole, gng->pole);
	errs += gd_fne_err(pp, nc, VAR_LATIN1, gpg->latin1, gng->latin1);
	errs += gd_fne_err(pp, nc, VAR_LATIN2, gpg->latin2, gng->latin2);
	errs += gd_fne_err(pp, nc, VAR_SPLAT, gpg->splat, gng->splat);
	errs += gd_fne_err(pp, nc, VAR_SPLON, gpg->splon, gng->splon);
    }
    break;
    case GRID_SPACEV:
    {
	gdes_spacev *gng = &gn->spacev;
	gdes_spacev *gpg = &gp->spacev;

	errs += gd_ine_err(pp, nc, VAR_NX, gpg->nx, gng->nx);
	errs += gd_ine_err(pp, nc, VAR_NY, gpg->ny, gng->ny);
	errs += gd_fne_err(pp, nc, VAR_LAP, gpg->lap, gng->lap);
	errs += gd_fne_err(pp, nc, VAR_LOP, gpg->lop, gng->lop);
	errs += gd_fnei_err(pp, nc, VAR_DX, gpg->dx, gng->dx);
	errs += gd_fnei_err(pp, nc, VAR_DY, gpg->dy, gng->dy);
	errs += gd_fne_err(pp, nc, VAR_XP, gpg->xp, gng->xp);
	errs += gd_fne_err(pp, nc, VAR_YP, gpg->yp, gng->yp);
	errs += gd_fne_err(pp, nc, VAR_ORIENTATION, gpg->orient, gng->orient);
	errs += gd_ine_err(pp, nc, VAR_NR, gpg->nr, gng->nr);
	errs += gd_fne_err(pp, nc, VAR_XO, gpg->xo, gng->xo);
	errs += gd_fne_err(pp, nc, VAR_YO, gpg->yo, gng->yo);
    }
    break;
    case GRID_ALBERS:
    case GRID_OLAMBERT:
    case GRID_UTM:
    case GRID_SIMPOL:
    case GRID_MILLER:
    default:
	uerror("%s: can't handle %s grids", nc->ncname,
	       gds_typename(nav->grid_type_code));
	return -1;
    }
    if (errs > 0)
	return -1;
    
    return 0;
}


/*
 * Consistency check between decoded product data and netCDF file to which
 * it will be written.  Returns -1 if a serious inconsistency was found.
 * 
 * If a value is missing in the netCDF file, it should get filled in here.
 * ( *** not implemented yet *** ).
 *
 * The netCDF information is read from cached info rather than from the file
 * for speed, since this check takes place on every product.
 */
static int
check_pp(pp, nc)
    product_data *pp;	/* decoded GRIB product */
    ncfile *nc;		/* netCDF file */
{
    navinfo* nav = nc->nav;
    int found;
    int i;

    found = 0;
    for (i = 0; i < nc->models.n; i++) {
	if (pp->model == nc->models.vals[i]) {
	    found = 1;
	    break;
	}
    }
    if (!found) {
	uerror("%s model %d not in modelid list in %s", pp->header,
	       pp->model, nc->ncname);
	return -1;
    }
    if(pp->center != nav->grid_center) {
	uerror("%s center %d != %d center in %s", pp->header,
	       pp->center, nav->grid_center, nc->ncname);
	return -1;
    }

    found = 0;
    for (i = 0; i < nav->grid_numbers.n; i++) {
	if (pp->grid == nav->grid_numbers.vals[i]) {
	    found = 1;
	    break;
	}
    }
    if (!found) {
	uerror("%s grid %d not in grid_number list in %s", pp->header,
	       pp->grid, nc->ncname);
	return -1;
    }
    
    /* gd->res_flags should match what's in nc, but if not, rewrite it. */
    if (pp->gd->res_flags != nav->rescomp) {
	udebug("%s: rescompflag in %s doesn't match, %d, %d",
	       pp->header, nc->ncname, (int) pp->gd->res_flags, nav->rescomp);
    }
    
    if(check_gd(pp, nc) == -1) {
	return -1;
    }
    return 0;
}


/*
 * Figure out subgrid location from netCDF navigation information and
 * product Grid Description Section information.  Returns 0 on success, -1
 * on failure.
 */
static int
subgrid(nc, pp, ix0, ix1)
    ncfile *nc;
    product_data *pp;
    long *ix0;
    long *ix1;
{
    navinfo* nav = nc->nav;
    gdes_ll *gll;
    float plon;

    *ix0 = 0;
    *ix1 = 0;

    switch (pp->gd->type) {
    case GRID_LL:
    case GRID_RLL:
    case GRID_SLL:
    case GRID_SRLL:

	if(pp->gd->scan_mode & 0x20 == 1 || /*adjacent points in j direction */
	   pp->gd->scan_mode & 0x80 == 1 ) { /* points scan in -i direction */
	    uerror("%s: can't handle scan mode of %x ",
		   pp->header,pp->gd->scan_mode);
	    return -1;
	}

	/* If scanning mode flag indicates North to South scan, we
	   reverse the rows so we can always assume South to North scan */
	if((pp->gd->scan_mode & 0x40) == 0) { /* north to south */
	    int row;
	    int nrows = pp->gd->nrows;
	    int ncols = pp->gd->ncols;
	    float tmp;
	    
	    for (row = 0; row < nrows/2; row++) {
		int col;
		float *upper = pp->data + row * ncols;
		float *lower = pp->data + (nrows - 1 - row) * ncols;
		/* swap row (upper) and nrows-1-row (lower) */
		for (col = 0; col < ncols; col++) {
		    tmp = *upper;
		    *upper = *lower;
		    *lower = tmp;
		    upper++;
		    lower++;
		}
	    }
	    gll = &pp->gd->grid.ll;
	    tmp = gll->la1;
	    gll->la1 = gll->la2;
	    gll->la2 = tmp;
	    pp->gd->scan_mode |= 0x40;
	}	

        /* Compare pp->gdes->grid.ll->la1 to value of La1, La2 netCDF
	   variables and similarly for lo1, lo2. */
	gll = &pp->gd->grid.ll;
	plon = gll->lo1;
	while (plon < nc->nav->grid.ll.lo1)
	    plon += 360.0 ;
        /* handle case where right edge is same as left edge */
	if (plon == nc->nav->grid.ll.lo2 &&
	    nc->nav->grid.ll.lo1 + 360 ==  nc->nav->grid.ll.lo2) {
	    plon = nc->nav->grid.ll.lo1;
	}
	*ix0 = (gll->la1 - nc->nav->grid.ll.la1) /nc->nav->grid.ll.dj + 0.5;
	*ix1 = (plon - nc->nav->grid.ll.lo1) /nc->nav->grid.ll.di + 0.5;
    }
    /* default: */
    return 0;
}


/*
 * Writes decoded GRIB product to netCDF file open for writing.
 * Returns 0 on success, -1 on failure.
 */
int
nc_write(pp, nc)
    product_data *pp;	/* decoded GRIB product to be written */
    ncfile *nc;		/* netCDF file to write */
{
    double reftime, valtime;
    long rec;
    char *varname;
    ncvar *var;
    int varid;
    long lev;			/* level to write, if level dimension */
    char *cp = parmname(nc, pp->param, pp->level_flg); /* var to write */
    int dim=0;			/* which dimension we are dealing with */
#define MAX_PARM_DIMS	5	/* Maximum dimensions for a parameter.
				      X(rec,lev,tri,lat,lon)
				   This would be 5 for a variable with a
				   level dimension and a time-range dimension,
				   4 if just a level or time range dimension,
				   3 if neither a level or time range
				   dimension. */
    long start[MAX_PARM_DIMS];
    long count[MAX_PARM_DIMS];

				/* Check consistency of product with
				 * netCDF file, e.g. model_id, gdes vs.
				 * netCDF navigation */
    if(check_pp(pp, nc) == -1) {
	/* any messages about inconsistency should have already been logged */
	return -1;
    }

    if (!cp) {
	uerror("%s: unrecognized (param,level_flg) combination (%d,%d)",
	       pp->header, pp->param, pp->level_flg);
	return -1;
    }
    varname = estrdup(cp);
				/* locate variable in output netCDF file */
    varid = ncvarid(ncid,varname);
    if (varid == -1) {
	uerror("%s: no variable %s in %s", pp->header, varname,
	       nc->ncname);
	return -1;
    }
    var = nc->vars[varid];

    if (var->dims[0] != nc->recid) { /* no record dimension */
	dim--;
    } else {			/* handle record dimension */
	if (rvhours(pp, nc, &reftime, &valtime) != 0) {
	    uerror("%s: can't get reftime,valtime", pp->header);
	    return -1;
	}

	rec = getrec(nc, reftime, valtime); /* which record to write */
	if (rec == -1) {
	    uerror("%s: can't get record number for variable %s in %s",
		   pp->header, varname, nc->ncname);
	    return -1;
	}
	start[dim] = rec;
	count[dim] = 1;
    }

    /* handle level dimension, if any */
    lev = getlev(pp, nc, var);    
    if (lev == -1)
	return -1;
    if(lev >= 0) {
	dim++;
	start[dim] = lev;
	count[dim] = 1;
    }

    /* handle time-range indicator, if any */
    /* *** write variable contining time-range info *** */

    /* Write varname(rec,lev), varname(rec,tri), or varname(rec) hyperslab,
       but start in the right place if this is only a subgrid of the netCDF
       file domain */
    {
	double slope, intercept;
	long ix0;		/* if subgridded, lat index, otherwise 0 */
	long ix1;		/* if subgridded, lon index, otherwise 0 */
	
	if (subgrid(nc, pp, &ix0, &ix1) == -1) {
	    uerror("%s: can't fit %s variable subgrid into %s",
		       pp->header, varname, nc->ncname);
	    return -1;
	}
	dim++;
	start[dim] = ix0;
	count[dim] = pp->gd->nrows;

	dim++;
	start[dim] = ix1;
	count[dim] = pp->gd->ncols;
	
	if (var->uc) {		/* units conversion */
	    slope = var->uc->slope;
	    intercept = var->uc->intercept;
	} else {
	    slope = 1.0;
	    intercept = 0.0;
	}

	if (float_nc(ncid, varid, start, count,
		      pp->data, slope, intercept, FILL_VAL) == -1) {
	    uerror("%s: error writing %s variable in %s",
		       pp->header, varname, nc->ncname);
	    return -1;
	}
    }

    if (var->dims[0] != nc->recid) { /* no record dimension */
	if (lev < 0) {
	    uinfo("%s: wrote %s(*,*) to %s", pp->header, varname,
		  nc->ncname);
	} else if (lev >= 0) {
	    uinfo("%s: wrote %s(%ld,*,*) to %s", pp->header, varname,
		  lev, nc->ncname);
	}
    } else {
	if (lev < 0) {
	    uinfo("%s: wrote %s(%ld,*,*) to %s", pp->header, varname,
		  rec, nc->ncname);
	} else if (lev >= 0) {
	    uinfo("%s: wrote %s(%ld,%ld,*,*) to %s", pp->header, varname,
		  rec, lev, nc->ncname);
	}
    }

    free(varname);
    return 0;
}
