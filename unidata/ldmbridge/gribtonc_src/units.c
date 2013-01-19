/*
 *	Copyright 1992 University Corporation for Atmospheric Research
 */
/* $Id: units.c,v 1.9 1995/11/22 22:32:43 russ Exp $ */

#include <udunits.h>
#include "ulog.h"
#include "units.h"
#include "timeunits.h"
#include "product_data.h"
#include "nc.h"
#include "params.h"
#include "emalloc.h"

/*
 * Initialize udunits library.  Reads in units table.  Returns a non-zero value
 * on failure, 0 on success.  Only tries to read in units table once; if that
 * fails, it will continue to return failure code on subsequent calls.
 *
 * If the environment variable UDUNITS_PATH exists, that is used as the path
 * of the units table; otherwise whatever was compiled into the installed
 * udunits library is used.
 */
int
init_udunits() {
    static int first = 1;
    static int stat = 0;

    if (first) {
	first = 0;
	stat = utInit((char *) 0);
	switch (stat) {
	  case 0:
	    return stat;
	  case UT_ENOFILE: 
	    uerror("can't find units file--set UDUNITS_PATH env. variable");
	    return UT_ENOFILE;
	  case UT_ESYNTAX:
	    uerror("units file contains a syntax error");
	    return UT_ESYNTAX;
	  case UT_EUNKNOWN:
	    uerror("units file contains an unknown specification");
	    return UT_EUNKNOWN;
	  case UT_EIO:
	    uerror("I/O error occurred while accessing the units file");
	    return UT_EIO;
	  case UT_EALLOC:
	    uerror("memory allocation failure occurred initializing units");
	    return UT_EALLOC;	    
	  default:
	    uerror("unknown error return from utInit");
	    return stat;
	}
    }
    return stat;
}


/*
 * The NUWG conventions require time as a double in units such as
 " "hours since 1992-1-1".  This function converts the GRIB product reference
 * time and forecast time to the reftime and valtime required in the NUWG
 * conventions.  Returns 0 on success, nonzero on failure.
 */
int
rvhours(gp, ncp, reftimep, valtimep)
    product_data *gp;		/* input GRIB product structure */
    ncfile *ncp;		/* input netCDF data structure */
    double *reftimep;		/* output, reference time */
    double *valtimep;		/* output, valid time */
{
    static int init = 0;
    utUnit *rhoursp = ncp->vars[ncp->reftimeid]->bunitp;
    utUnit *vhours = ncp->vars[ncp->valtimeid]->bunitp;
    static utUnit hourunit;
    double tdiff;
    
    int  ref_year = gp->year+(gp->century - (gp->year==0 ? 0 : 1))*100;
    int  ref_month = gp->month;
    int  ref_day = gp->day;
    int  ref_hour = gp->hour;
    int  ref_minute = gp->minute;
    float ref_second = 0;

    if (init == 0) {		/* initialize target units */
	if (utScan("hour", &hourunit) != 0) {
	    uerror("utScan() error, hour");
	    return -3;
	}
	init = 1;
    }
				/* compute reftime in target units */
    (void) utInvCalendar(ref_year, ref_month, ref_day,
			 ref_hour, ref_minute, ref_second,
			 rhoursp, reftimep);

    /* tdiff value is function of tr[0] and tr[1] depending on tr_flg */
    tdiff = frcst_time(gp);

    /* Convert to hours, if tunits(gp->tunit) is not hours already */
    if (gp->tunit != TUNITS_HOUR) {
	utUnit from;
	double slope, intercept;
	
	if (utScan((char *) tunits(gp->tunit), &from) != 0) {
	    uerror("GRIB %s: bad forecast time unit %s",
		   gp->header, tunits(gp->tunit) );
	    return -4;
	}
	if (utConvert(&from, &hourunit, &slope, &intercept) != 0) {
	    uerror("GRIB %s: bad forecast time conversion", gp->header);
	    return -5;
	}
	tdiff = slope * tdiff + intercept;
    }

    /* compute valtime = reftime + tdiff */

    *valtimep = *reftimep + tdiff;

    return 0;
}


/*
 * Get value of units attribute associated with a netCDF variable, if any.
 * Returns 0 if OK, or if no units attribute.
 * Returns -1 if error accessing units attribute.
 */
int
get_units(ncid, varid, bunitp)
    int ncid;			/* netCDF ID */
    int varid;			/* variable ID */
    utUnit **bunitp;		/* returned pointer to malloced binary unit */
{
    nc_type atttype;
    int attlen;
#define MAX_UNIT_LEN 100
    char units[MAX_UNIT_LEN];

    *bunitp = 0;
    
    if (ncattinq(ncid, varid, UNITS_NAME, &atttype, &attlen) == -1) {
	return 0;		/* no units attribute */
    }
    /* else */
    if (atttype != NC_CHAR) {
	uerror("Attribute units not of type char");
	return -1;
    }
    if (attlen+1 > MAX_UNIT_LEN) {
	uerror("units attribute too long: %d", attlen);
	return -1;
    }
    if(ncattget(ncid, varid, UNITS_NAME, units) == -1) {
	uerror("Error getting units attribute");
	return -1;
    }
    units[attlen] = '\0'; /* ncattget doesn't null-terminate */

    *bunitp = (utUnit *)emalloc(sizeof(utUnit));
    /* Don't bother to parse psuedo-units, such as "(allocated by center)" */
    if(units[0] != '(' && utScan(units, *bunitp) != 0) {
	uerror("Error parsing units: %s", units);
	return -1;
    }
    return 0;
}



/*
 * Return units conversion structure (with slope and intercept) for
 * converting from standard GRIB units to specified units for specified
 * netCDF variable corresponding to a GRIB parameter.
 */
unitconv *
uconv(varname, btunitp)
    char *varname;		/* netCDF variable name */
    utUnit *btunitp;		/* netCDF variable units to which GRIB data
				   should be converted */
{
    int param;			/* GRIB parameter code */
    char *funits;		/* units to convert from, if GRIB 1 product */
    double slope=1., intercept=0.;
    utUnit bfunit;
    unitconv *out;

    param = grib_pcode(varname);
    if (param == -1)		/* not a GRIB parameter */
	return 0;
    if (!btunitp)		/* no units to convert to */
	return 0;
    funits = grib_units(param);
    if(!funits)			/* GRIB1 units not available for this parameter */
	return 0;

    if(funits != 0) { 
	if(utScan(funits, &bfunit) != 0) { /* "from" unit */
	    uerror("Error parsing GRIB units `%s' for variable", funits, varname);
	    return 0;
	}
	if(utConvert(&bfunit, btunitp, &slope, &intercept) == UT_ECONVERT) {
	    uerror("GRIB units `%s' not conformable with variable %s:units",
		   funits, varname);
	    return 0;
	}
    } else {
	slope = 1.0;
	intercept = 0.0;
    }
    if (slope == 1.0 && intercept == 0.0) /* trivial conversion */
	return 0;
    out = (unitconv *)emalloc(sizeof(unitconv));
    out->slope = slope;
    out->intercept = intercept;
    return out;
}

