/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ncfloat.c,v 1.6 1995/11/22 18:16:10 russ Exp $ */

/*
 * Harry Edmon's nc_float package, adapted for use in gribtonc with additions
 * that provide a wrapper for ncvarputg calls.
 */
  
#include <limits.h>
#include <string.h>

#include <netcdf.h>
#include <udunits.h>
#include "ulog.h"

#include "units.h"
#include "ncfloat.h"
#include "emalloc.h"

extern int ncerr;

/*
  This routine calculates the slope and intercept necessary to convert from 
  the units of the given netCDF variable to the specified units.  If units
  is blank, the netCDF units are returned and the slope is set to one and 
  the intercept to 0.
  */

int
nc_units(ncid, varid, units, slope, intercept)
    int ncid;			/* input netCDF id */
    int varid;			/* input netCDF variable id */
    char *units;		/* units specifier, returned if blank */
    double *slope;		/* output units conversion factor */
    double *intercept;		/* output units conversion additive offset */
{
    int ulen;
    char nc_units[100];
    utUnit funits, tunits;

    *slope = 1.0; *intercept = 0.0;
    if (units == NULL) return(0);
    if (ncattinq(ncid, varid, UNITS_NAME, NULL, &ulen) == -1 ||
	ncattget(ncid, varid, UNITS_NAME, nc_units) == -1) {
	uerror("Cannot get data units");
	return -1;
    }
    nc_units[ulen] = '\0';
/*
  Prepare for possible units conversion.
  */
    if (units[0]) {
/*
  Prepare udunits.
  */
	if (init_udunits()) {
	    uerror("Cannot initialize udunits library");
	    return -1;
	}
	if (utScan(units, &tunits) == 0 
	    && utScan(nc_units, &funits) == 0) {
	    int ret = utConvert(&funits, &tunits, slope, intercept);
	    if (ret == UT_ECONVERT) {
		uerror("Units `%s' and `%s' are not conformable", units, nc_units);
		return -1;
	    }
	}
    }
    else if (units) {
/*
  No units specified - return units in the netCDF file.
  */
	strcpy(units, nc_units);
    }
    return(0);
}

/*
  This routine is an interface to ncvarget that converts the data from 
  the netCDF to floating point with the given missing value and units.
*/

int
nc_float(ncid, varid, corn, edge, data, slope, intercept, missing)
    int ncid;
    int varid;
    long *corn;
    long *edge;
    float *data;
    float missing;
    double slope;
    double intercept;
{
    int ndims, i, prod=1;
    nc_type datatype;
    ncvoid *dd;
    char *c, *cf;
    short *s, *sf;
    long *l, *lf;
    float *ff;
    double *d, miss;
    
    if (ncvarinq(ncid, varid, NULL, &datatype, &ndims, NULL, NULL) == -1) {
	uerror("Cannot get data type");
	return -1;
    }
    for (i=0; i < ndims; ++i) prod *= edge[i];
/*
  See if a temporary array is necessary.
  */
    if (datatype == NC_FLOAT) dd = data;
    else {
	switch (datatype) {

	case NC_CHAR: 
	case NC_BYTE:
	    i = prod*sizeof(char);
	    break;
	case NC_SHORT:
	    i = prod*sizeof(short);
	    break;
	case NC_LONG:
	    i = prod*sizeof(long);
	    break;
	case NC_DOUBLE:
	    i = prod*sizeof(double);
	}
	dd = (ncvoid *) emalloc(i);
    }
    if (ncvarget(ncid, varid, corn, edge, dd) == -1) {
	if (dd != data) free(dd);
	uerror("ncvarget failed");
	return -1;
    }
/*
  Handle missing data (if present).
  */
    if (ncattget(ncid, varid, _FillValue, &miss) == -1)
	switch (datatype) {
	case NC_CHAR:
	case NC_BYTE:
	    c = (char *) dd;
	    for (i=0; i < prod; ++i) data[i] = c[i];
	    break;

	case NC_SHORT:
	    s = (short *) dd;
	    for (i=0; i < prod; ++i) data[i] = s[i];
	    break;

	case NC_LONG:
	    l = (long *) dd;
	    for (i=0; i < prod; ++i) data[i] = l[i];
	    break;

	case NC_DOUBLE:
	    d = (double *) dd;
	    for (i=0; i < prod; ++i) data[i] = d[i];
	    break;
	}

    else switch (datatype) {
	    
	case NC_FLOAT:	/* Just change missing value */
	    ff = (float *) &miss;
	    for (i=0; i < prod; ++i) {
		if (data[i] == *ff) data[i] = missing;
	    }
	    break;

	case NC_CHAR: 
	case NC_BYTE:
	    cf = (char *) &miss;
	    c = (char *) dd;
	    for (i=0; i < prod; ++i) {
		if (c[i] == *cf) data[i] = missing;
		else data[i] = c[i];
	    }
	    break;
	    
	case NC_SHORT:
	    sf = (short *) &miss;
	    s = (short *) dd;
	    for (i=0; i < prod; ++i) {
		if (s[i] == *sf) data[i] = missing;
		else data[i] = s[i];
	    }
	    break;
	    
	case NC_LONG:
	    lf = (long *) &miss;
	    l = (long *) dd;
	    for (i=0; i < prod; ++i) {
		if (l[i] == *lf) data[i] = missing;
		else data[i] = l[i];
	    }
	    break;
	    
	case NC_DOUBLE:
	    
	    d = (double *) dd;
	    for (i=0; i < prod; ++i) {
		if (d[i] == miss) data[i] = missing;
		else data[i] = d[i];
	    }
	}

    if (dd != data) free(dd);

    if (slope != 1.0) {
	if (intercept != 0.) {
	    for(i=0; i<prod; i++)
		if (data[i] != missing)
		    data[i] = slope * data[i] + intercept;
	}
	else
	    for(i=0; i<prod; i++)
		if (data[i] != missing)
		    data[i] *= slope;
    }
    else if (intercept != 0.)
	for (i=0; i < prod; i++)
	    if (data[i] != missing)
		data[i] += intercept;
    return(0);
}

static
int nint(d)
    double d;
{
    if (d >= 0) return d + .5;
    return d - .5;
}


/*
  This routine is an interface to ncvarput that converts the data to
  the netCDF from floating point with the given missing value and units.
*/
int
float_nc(ncid, varid, corn, edge, data, slope, intercept, missing)
    int ncid;
    int varid;
    long *corn;
    long *edge;
    float *data;
    double slope;
    double intercept;
    float missing;
{
    int ndims, i, prod=1, rangerr=0, check_miss=0;
    nc_type datatype;
    ncvoid *dd;
    char *c, *cf;
    short *s, *sf;
    long *l, *lf;
    float *f, *ff, *datac;
    double *d, miss;
    
    if (ncvarinq(ncid, varid, NULL, &datatype, &ndims, NULL, NULL) == -1) {
	uerror("Cannot get data type");
	return -1;
    }
    for (i=0; i < ndims; ++i) prod *= edge[i];
/*
  Convert the units.
  */
    if (slope == 1.0 && intercept == 0.0) datac = data;
    else {
	datac = (float *) emalloc(prod * sizeof(float));
	if (slope != 1.0) {
	    if (intercept != 0.) {
		for(i=0; i<prod; i++) {
		    if (data[i] == missing) datac[i] = data[i];
		    else datac[i] = slope * data[i] + intercept;
		}
	    }
	    else
		for(i=0; i<prod; i++) {
		    if (data[i] == missing) datac[i] = data[i];
		    else datac[i] = data[i] * slope;
	    }
	}
	else {
	    for (i=0; i < prod; i++) {
		if (data[i] == missing) datac[i] = data[i];
		else datac[i] = data[i] + intercept;
	    }
	}
    }
	
/*
  Create temporary array and store
  */
    if (ncattget(ncid, varid, _FillValue, &miss) == -1) check_miss++;
    switch (datatype) {

    case NC_CHAR: 
    case NC_BYTE:
	cf = (char *) &miss;
	c=(char *) emalloc(prod*sizeof(char));
	for (i=0; i < prod; ++i) {
	    if (check_miss && datac[i] == missing) c[i] = *cf;
	    else if (data[i] < CHAR_MIN || data[i] > CHAR_MAX) {
		rangerr = 1;
		c[i] = *cf;
	    }
	    else c[i] = nint(datac[i]);
	}
	dd = (ncvoid *) c;
	break;
    case NC_SHORT:
	sf = (short *) &miss;
	s=(short *) emalloc(prod*sizeof(short));
	for (i=0; i < prod; ++i) {
	    if (check_miss && datac[i] == missing) s[i] = *sf;
	    else if (data[i] < SHRT_MIN || data[i] > SHRT_MAX) {
		rangerr = 1;
		s[i] = *sf;
	    }
	    else s[i] = nint(datac[i]);
	}
	dd = (ncvoid *) s;
	break;
    case NC_LONG:
	lf = (long *) &miss;
	l=(long *) emalloc(prod*sizeof(long));
	for (i=0; i < prod; ++i) {
	    if (check_miss && datac[i] == missing) l[i] = *lf;
	    else if (data[i] < LONG_MIN || data[i] > LONG_MAX) {
		rangerr = 1;
		l[i] = *lf;
	    }
	    else l[i] = nint(datac[i]);
	}
	dd = (ncvoid *) l;
	break;
    case NC_FLOAT:
	ff = (float *) &miss;
	if (!check_miss || missing == *ff) {
	    dd = (ncvoid *) datac;
	    break;
	}
	f=(float *) emalloc(prod*sizeof(float));
	for (i=0; i < prod; ++i) {
	    if (datac[i] == missing) f[i] = *ff;
	    else f[i] = datac[i];
	}
	dd = (ncvoid *) f;
	break;
    case NC_DOUBLE:
	d=(double *) emalloc(prod*sizeof(double));
	for (i=0; i < prod; ++i) {
	    if (check_miss && datac[i] == missing) d[i] = miss;
	    else d[i] = datac[i];
	}
	dd = (ncvoid *) d;
	break;
    }
    if (data!=datac && (ncvoid *)datac!=dd) free(datac);
    if (rangerr)
	uerror("Range error - out of bounds data set to missing");
    i = ncvarput(ncid, varid, corn, edge, dd);
    if (dd != (ncvoid *) data) free(dd);
    if (i == -1) {
	uerror("ncvarput failed, ncerr=%d, ndims=%d", ncerr, ndims);
	for (i=0; i<ndims; i++)	/* for debugging */
	    udebug("\tcorn[%d] = %ld, edge[%d] = %ld",i, corn[i], i, edge[i]);
	return -1;
    }
    return 0;
}


/*
  This routine is an interface to ncvarputg that converts the data to
  the netCDF from floating point with the given missing value and units.
*/
int
float_ncg(ncid, varid, corn, edge, stride, imap, data, slope, intercept, missing)
    int ncid;
    int varid;
    long *corn;
    long *edge;
    long *stride;
    long *imap;
    float *data;
    double slope;
    double intercept;
    float missing;
{
    int ndims, i, prod=1, rangerr=0, check_miss=0;
    nc_type datatype;
    ncvoid *dd;
    char *c, *cf;
    short *s, *sf;
    long *l, *lf;
    float *f, *ff, *datac;
    double *d, miss;
    
    if (ncvarinq(ncid, varid, NULL, &datatype, &ndims, NULL, NULL) == -1) {
	uerror("Cannot get data type");
	return -1;
    }
    for (i=0; i < ndims; ++i) prod *= edge[i];
/*
  Convert the units.
  */
    if (slope == 1.0 && intercept == 0.0) datac = data;
    else {
	datac = (float *) emalloc(prod * sizeof(float));
	if (slope != 1.0) {
	    if (intercept != 0.) {
		for(i=0; i<prod; i++) {
		    if (data[i] == missing) datac[i] = data[i];
		    else datac[i] = slope * data[i] + intercept;
		}
	    }
	    else
		for(i=0; i<prod; i++) {
		    if (data[i] == missing) datac[i] = data[i];
		    else datac[i] = data[i] * slope;
	    }
	}
	else {
	    for (i=0; i < prod; i++) {
		if (data[i] == missing) datac[i] = data[i];
		else datac[i] = data[i] + intercept;
	    }
	}
    }
	
/*
  Create temporary array and store
  */
    if (ncattget(ncid, varid, _FillValue, &miss) == -1) check_miss++;
    switch (datatype) {

    case NC_CHAR: 
    case NC_BYTE:
	cf = (char *) &miss;
	c=(char *) emalloc(prod*sizeof(char));
	for (i=0; i < prod; ++i) {
	    if (check_miss && datac[i] == missing) c[i] = *cf;
	    else if (data[i] < CHAR_MIN || data[i] > CHAR_MAX) {
		rangerr = 1;
		c[i] = *cf;
	    }
	    else c[i] = nint(datac[i]);
	}
	dd = (ncvoid *) c;
	break;
    case NC_SHORT:
	sf = (short *) &miss;
	s=(short *) emalloc(prod*sizeof(short));
	for (i=0; i < prod; ++i) {
	    if (check_miss && datac[i] == missing) s[i] = *sf;
	    else if (data[i] < SHRT_MIN || data[i] > SHRT_MAX) {
		rangerr = 1;
		s[i] = *sf;
	    }
	    else s[i] = nint(datac[i]);
	}
	dd = (ncvoid *) s;
	break;
    case NC_LONG:
	lf = (long *) &miss;
	l=(long *) emalloc(prod*sizeof(long));
	for (i=0; i < prod; ++i) {
	    if (check_miss && datac[i] == missing) l[i] = *lf;
	    else if (data[i] < LONG_MIN || data[i] > LONG_MAX) {
		rangerr = 1;
		l[i] = *lf;
	    }
	    else l[i] = nint(datac[i]);
	}
	dd = (ncvoid *) l;
	break;
    case NC_FLOAT:
	ff = (float *) &miss;
	if (!check_miss || missing == *ff) {
	    dd = (ncvoid *) datac;
	    break;
	}
	f=(float *) emalloc(prod*sizeof(float));
	for (i=0; i < prod; ++i) {
	    if (datac[i] == missing) f[i] = *ff;
	    else f[i] = datac[i];
	}
	dd = (ncvoid *) f;
	break;
    case NC_DOUBLE:
	d=(double *) emalloc(prod*sizeof(double));
	for (i=0; i < prod; ++i) {
	    if (check_miss && datac[i] == missing) d[i] = miss;
	    else d[i] = datac[i];
	}
	dd = (ncvoid *) d;
	break;
    }
    if (data!=datac && (ncvoid *)datac!=dd) free(datac);
    if (rangerr)
	uerror("Range error - out of bounds data set to missing");
    i = ncvarputg(ncid, varid, corn, edge, stride, imap, dd);
    if (dd != (ncvoid *) data) free(dd);
    if (i == -1) {
	uerror("ncvarputg failed");
	return -1;
    }
    return 0;
}

