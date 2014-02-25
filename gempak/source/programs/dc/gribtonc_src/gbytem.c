/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: gbytem.c,v 1.9 1996/01/10 16:22:00 russ Exp $ */

#include <stdlib.h>			/* for free(), ... */
#include <assert.h>
#include "ulog.h"
#include "emalloc.h"
#include "gds.h"
#include "gdes.h"
#include "gbytem.h"
#include "centers.h"

#ifdef __STDC__
static gbytem* empty_gbytem(int nbytes);
static gbytem* full_gbytem(int nbytes);
static gbytem* unpackbits(int nbits, unsigned char* bits);
static gbytem* unpackbits_first(int nlat, int nlon, unsigned char* bits);
static gbytem* unpackbits_last(int nlat, int nlon, unsigned char* bits);
static gbytem* nmc_grid37(void);
static gbytem* nmc_grid50(void);
static gbytem* grid_noncat(gds * gdsp);
#endif

/*
 * Create an empty bytemap structure with a bytemap array of specified shape,
 * ready for filling in.
 */
static gbytem*
empty_gbytem(nbytes)
    int nbytes;                  /* number of bytes in byte map */
{
    gbytem* bp = (gbytem *) emalloc(sizeof(gbytem));
    bp->nb = nbytes;
    bp->keep = 0;
    bp->map = (char *)emalloc(bp->nb);
    return bp;
}

/*
 * Return a full bytemap of specified length.
 */
static gbytem*
full_gbytem(nbytes)
    int nbytes;                  /* number of bytes in byte map */
{
    gbytem* bp = (gbytem *) emalloc(sizeof(gbytem));
    bp->nb = nbytes;
    bp->keep = 0;
    bp->map = 0;		/* means all 1's */
    return bp;
}


/*
 * Unpack bitmap into array of bytes, one for each bit.
 */
static gbytem*
unpackbits(nbits, bits)
    int nbits;                  /* number of bits to unpack */
    unsigned char *bits;	/* the bits */
{
    gbytem *bp = empty_gbytem(nbits);
    char *dest;
    int i;
    unsigned char *cb = &bits[0];
    unsigned char cur = bits[0];
    int curbit = 0;		/* which bit in byte we are looking at */
    
    dest = bp->map;
    for (i=0; i<nbits; i++) {
	*dest++ = (cur & 0x80) ? PRESENT : MISSING;
	curbit++;
	if (curbit > 7) {
	    curbit = 0;
	    cur = *++cb;
	} else {
	    cur <<= 1;
	}
    }
    return bp;
}


/*
 * Return bytemap for international exchange FOS grids in which
 * the pole value appears first and gets replicated to make the grid
 * rectangular.
 */
static gbytem*
unpackbits_first(nlat, nlon, bits)
    int nlat;			/* no. of latitudes, including pole */
    int nlon;			/* no. of longitudes */
    unsigned char *bits;	/* the bits.  If 0, means all 1's */
{
    gbytem* bp;
    char* dest;
    int i;

    bp = empty_gbytem(nlon * nlat);
    dest = bp->map;
    if(bits) {
	unsigned char *cb = &bits[0];
	unsigned char cur = bits[0];
	int curbit = 0;		/* which bit in byte we are looking at */

	for (i=0; i < 1; i++) { /* the pole value */
	    *dest++ = (cur & 0x80) ? PRESENT : MISSING;
	    curbit++;
	    if (curbit > 7) {
		curbit = 0;
		cur = *++cb;
	    } else {
		cur <<= 1;
	    }
	}
	for (i=1; i < nlon; i++) {	/* replicate pole value */
	    *dest++ = REPLICATED;
	}
	for (i=nlon; i < nlon * nlat; i++) { /* rest of the values */
	    *dest++ = (cur & 0x80) ? PRESENT : MISSING;
	    curbit++;
	    if (curbit > 7) {
		curbit = 0;
		cur = *++cb;
	    } else {
		cur <<= 1;
	    }
	}
    } else {
	for (i=0; i < 1; i++)
	    *dest++ = PRESENT;
	for (i=1; i < nlon; i++)	/* replicate pole value */
	    *dest++ = REPLICATED;
	for (i=nlon; i < nlon * nlat; i++) {
	    *dest++ = PRESENT;
	}
    }
    return bp;
}

/*
 * Return bytemap for international exchange FOS grids in which
 * the pole value appears last and gets replicated to make the grid
 * rectangular.
 */
static gbytem*
unpackbits_last(nlat, nlon, bits)
    int nlat;			/* no. of latitudes, including pole */
    int nlon;			/* no. of longitudes */
    unsigned char *bits;	/* the bits.  If 0, means all 1's */
{
    gbytem* bp;
    char* dest;
    int i;

    bp = empty_gbytem(nlon * nlat);
    dest = bp->map;
    if(bits) {
	unsigned char *cb = &bits[0];
	unsigned char cur = bits[0];
	int curbit = 0;		/* which bit in byte we are looking at */
	
	for (i=0; i < nlon * (nlat - 1); i++) { /* rest of the values */
	    *dest++ = (cur & 0x80) ? PRESENT : MISSING;
	    curbit++;
	    if (curbit > 7) {
		curbit = 0;
		cur = *++cb;
	    } else {
		cur <<= 1;
	    }
	}
	for (i=nlon * (nlat - 1); i < nlon * (nlat - 1) + 1; i++) { /* the pole value */
	    *dest++ = (cur & 0x80) ? PRESENT : MISSING;
	    curbit++;
	    if (curbit > 7) {
		curbit = 0;
		cur = *++cb;
	    } else {
		cur <<= 1;
	    }
	}
	for (i=nlon * (nlat - 1) + 1; i < nlon * nlat; i++) /* replicate pole value */
	    *dest++ = REPLICATED;
    } else {
	for (i=0; i < nlon * (nlat - 1); i++)
	    *dest++ = PRESENT;
	for (i=nlon * (nlat - 1); i < nlon * (nlat - 1) + 1; i++)
	    *dest++ = PRESENT;
	for (i=nlon * (nlat - 1) + 1; i < nlon * nlat; i++)	/* replicate pole value */
	    *dest++ = REPLICATED;
    }
    return bp;
}


/*
 * Return a bytemap for the international exchange FOS grids 37-44, the
 * so-called quasi-regular or "thinned grids".  Filling these grids out to
 * rectangular requires interpolation.
 */
static gbytem*
nmc_grid37()
{
    static gbytem* bp = 0;
    
    if (bp == 0) {              /* gets auto-initialized on first call */
        bp = full_gbytem(1 * 3447); /* quasi-regular */
        if(bp)
            bp->keep = 1;
    }
    return bp;
}


/*
 * Return a bytemap for the international exchange FOS grid 50, 2.5 lon by
 * 1.25 lat.  Grid 50 is a set of points over the contiguous United States
 * and environs on a grid extending from 20N (row 1) to 60N (row 33) in 1.25
 * degree intervals.  The grid increases in longitudinal extent from south
 * to north.
 */
static gbytem*
nmc_grid50()
{
    static gbytem* bp = 0;
    
    if (bp == 0) {              /* gets auto-initialized on first call */
        bp = empty_gbytem(36 * 33);
        if(bp) {
            char *cp;
            int row, i;
            bp->keep = 1;
            cp = bp->map;
            for (row = 1; row < 5; row++) {
                for (i=0; i < 7; i++)
                    *cp++ = MISSING;
                for (i=0; i < 22; i++)
                    *cp++ = PRESENT;
                for (i=0; i < 7; i++)
                    *cp++ = MISSING;
            }
            for (row = 5; row < 9; row++) {
                for (i=0; i < 6; i++)
                    *cp++ = MISSING;
                for (i=0; i < 24; i++)
                    *cp++ = PRESENT;
                for (i=0; i < 6; i++)
                    *cp++ = MISSING;
            }
            for (row = 9; row < 13; row++) {
                for (i=0; i < 5; i++)
                    *cp++ = MISSING;
                for (i=0; i < 26; i++)
                    *cp++ = PRESENT;
                for (i=0; i < 5; i++)
                    *cp++ = MISSING;
            }
            for (row = 13; row < 17; row++) {
                for (i=0; i < 4; i++)
                    *cp++ = MISSING;
                for (i=0; i < 28; i++)
                    *cp++ = PRESENT;
                for (i=0; i < 4; i++)
                    *cp++ = MISSING;
            }
            for (row = 17; row < 21; row++) {
                for (i=0; i < 3; i++)
                    *cp++ = MISSING;
                for (i=0; i < 30; i++)
                    *cp++ = PRESENT;
                for (i=0; i < 3; i++)
                    *cp++ = MISSING;
            }
            for (row = 21; row < 25; row++) {
                for (i=0; i < 2; i++)
                    *cp++ = MISSING;
                for (i=0; i < 32; i++)
                    *cp++ = PRESENT;
                for (i=0; i < 2; i++)
                    *cp++ = MISSING;
            }
            for (row = 25; row < 29; row++) {
                for (i=0; i < 1; i++)
                    *cp++ = MISSING;
                for (i=0; i < 34; i++)
                    *cp++ = PRESENT;
                for (i=0; i < 1; i++)
                    *cp++ = MISSING;
            }
            for (row = 29; row < 34; row++) {
                for (i=0; i < 36; i++)
                    *cp++ = PRESENT;
            }
            assert(bp->nb == cp - bp->map);
        }
    }
    return bp;
}

/*
 * Return a bytemap for a non-cataloged grid, based on the information in its
 * Grid Description Section.
 */
static gbytem*
grid_noncat(gdsp)
    gds *gdsp;
{
    gbytem* bp = 0;
    int ncols, nrows;
    				/* *** When gdes available, use instead *** */
    if (gdsp == 0)              /* No GDS, so fail */
        return 0;
    
    switch(g1i(gdsp->type)) {
    case GRID_LL:
        ncols = g2i(gdsp->grid.ll.ni);
	nrows = g2i(gdsp->grid.ll.nj);
        break;
    case GRID_RLL:
    case 201:
        ncols = g2i(gdsp->grid.rll.ni);
	nrows = g2i(gdsp->grid.rll.nj);
        break;
    case GRID_SLL:
        ncols = g2i(gdsp->grid.sll.ni);
	nrows = g2i(gdsp->grid.sll.nj);
        break;
    case GRID_SRLL:
        ncols = g2i(gdsp->grid.srll.ni);
	nrows = g2i(gdsp->grid.srll.nj);
        break;
    case GRID_GAU:
        ncols = g2i(gdsp->grid.gau.ni);
	nrows = g2i(gdsp->grid.gau.nj);
        break;
    case GRID_RGAU:
        ncols = g2i(gdsp->grid.rgau.ni);
	nrows = g2i(gdsp->grid.rgau.nj);
        break;
    case GRID_SGAU:
        ncols = g2i(gdsp->grid.sgau.ni);
	nrows = g2i(gdsp->grid.srll.nj);
        break;
    case GRID_SRGAU:
        ncols = g2i(gdsp->grid.srgau.ni);
	nrows = g2i(gdsp->grid.srgau.nj);
        break;
    case GRID_MERCAT:
        ncols = g2i(gdsp->grid.mercator.ni);
	nrows = g2i(gdsp->grid.mercator.nj);
        break;
    case GRID_LAMBERT:
        ncols = g2i(gdsp->grid.lambert.nx);
	nrows = g2i(gdsp->grid.lambert.ny);
        break;
    case GRID_POLARS:
        ncols = g2i(gdsp->grid.polars.nx);
	nrows = g2i(gdsp->grid.polars.ny);
        break;
    case GRID_GNOMON:
        ncols = g2i(gdsp->grid.polars.nx);
	nrows = g2i(gdsp->grid.polars.ny);
        break;
    case GRID_ALBERS:
        ncols = g2i(gdsp->grid.albers.nx);
	nrows = g2i(gdsp->grid.albers.ny);
        break;
    case GRID_OLAMBERT:
        ncols = g2i(gdsp->grid.olambert.nx);
	nrows = g2i(gdsp->grid.olambert.ny);
        break;
    case GRID_SPH:
        ncols = g2i(gdsp->grid.sph.j);
	nrows = g2i(gdsp->grid.sph.k);
        break;
    case GRID_RSPH:
        ncols = g2i(gdsp->grid.rsph.j);
	nrows = g2i(gdsp->grid.rsph.k);
        break;
    case GRID_SSPH:
        ncols = g2i(gdsp->grid.ssph.j);
	nrows = g2i(gdsp->grid.ssph.k);
        break;
    case GRID_SRSPH:
        ncols = g2i(gdsp->grid.srsph.j);
	nrows = g2i(gdsp->grid.srsph.k);
        break;
    case GRID_SPACEV:
        ncols = g2i(gdsp->grid.spacev.nx);
	nrows = g2i(gdsp->grid.spacev.ny);
        break;
    case GRID_UTM:
    case GRID_SIMPOL:
    case GRID_MILLER:
    default:
        uerror("unrecognized grid type %d", g1i(gdsp->type));
        return 0;
    }
    bp = full_gbytem(ncols*nrows);
    return bp;
}



/*
 * Make byte map from bit map or generate it from grid tables, if no bit map
 * section exists.  Returns 0 if memory couldn't be allocated.  User should
 * call free_gbytem() on result when done with it.
 */
gbytem*
make_gbytem(bmsp, pdsp, gdsp, npts)
    bms *bmsp;
    pds *pdsp;
    gds *gdsp;
    int npts;
{

#define BMS_OVHD	6	/* overhead bytes in bit map section */
    if (bmsp) {
	/* number of bits in raw GRIB bitmap */
        int bmsp_len = 8*(g3i(bmsp->len) - BMS_OVHD) - g1i(bmsp->nbits);

        if (g2i(bmsp->map_flg)) {
            uerror("Predefined bitmap %d unrecognized",
                   g2i(bmsp->map_flg));
            return 0;
        }
	if (bmsp_len != npts) {	/* grids for which we replicate pole value */
	    switch (g1i(pdsp->grid)) {
	    case 21:
	    case 22:
		return unpackbits_last(37, 37, bmsp->bits);
	    case 23:
	    case 24:
		return unpackbits_first(37, 37, bmsp->bits);
	    case 61:
	    case 62:
		return unpackbits_last(46, 91, bmsp->bits);
	    case 63:
	    case 64:
		return unpackbits_first(46, 91, bmsp->bits);
	    default:
		uerror("bitmap length inconsistency, %d != %d",
		       bmsp_len, npts);
		return 0;
	    }
	} else {
	    return unpackbits(npts, bmsp->bits);
	}
    }
    /* else, no bit map section so generate byte map from predefined grid or
       from information in GDS */

    /* International exchange grids, could come from any center */
    switch (g1i(pdsp->grid)) {
    case 21:
    case 22:
        return unpackbits_last(37, 37, 0);
    case 23:
    case 24:
        return unpackbits_first(37, 37, 0);
    case 25:
        return unpackbits_last(19, 72, 0);
    case 26:
        return unpackbits_first(19, 72, 0);
    case 37:
    case 38:
    case 39:
    case 40:
    case 41:
    case 42:
    case 43:
    case 44:
        return nmc_grid37();
    case 50:
        return nmc_grid50();
    case 61:
    case 62:
	return unpackbits_last(46, 91, 0);
    case 63:
    case 64:
	return unpackbits_first(46, 91, 0);
    case NONCATALOGED_GRID:
        return grid_noncat(gdsp);
    }

    /* else, center-specific grids */
    switch (pdsp->center) {
    case CENTER_NMC:		/* These grid sizes come from NMC Office
				   Note 388 and from a FORTRAN GRIB decoder at
                       ftp://nic.fb4.noaa.gov:/pub/nws/nmc/codes/grib/decode.f
				   */
        switch (g1i(pdsp->grid)) {
        case  1: return full_gbytem(1679);
        case  2: return full_gbytem(10512);
        case  3: return full_gbytem(65160);
        case  4: return full_gbytem(259920);
        case  5: return full_gbytem(3021);
        case  6: return full_gbytem(2385);
        case  27: 
        case  28: return full_gbytem(4225);
        case  29: 
        case  30: return full_gbytem(5365);
        case  33: 
        case  34: return full_gbytem(8326);
        case  45: return full_gbytem(41760);
        case  55: 
        case  56: return full_gbytem(6177);
        case  67: 
        case  68: 
        case  69: 
        case  70: 
        case  71: return full_gbytem(13689);
        case  72: return full_gbytem(406);
        case  73: return full_gbytem(13056);
        case  74: return full_gbytem(10800);
        case  75: 
        case  76: 
        case  77: return full_gbytem(12321);
        case  85: 
        case  86: return full_gbytem(32400);
        case  87: return full_gbytem(5022);
        case  90: return full_gbytem(12902);
        case  91: return full_gbytem(25803);
        case  92: return full_gbytem(24162);
        case  93: return full_gbytem(48323);
        case  94: return full_gbytem(48916);
        case  95: return full_gbytem(97831);
        case  96: return full_gbytem(41630);
        case  98: return full_gbytem(18048);
        case  99: return full_gbytem(45873);
        case  100: return full_gbytem(6889);
        case  101: return full_gbytem(10283);
        case  103: return full_gbytem(3640);
        case  104: return full_gbytem(16170);
        case  105: return full_gbytem(6889);
        case  106: return full_gbytem(19305);
        case  107: return full_gbytem(11040);
        case  126: return full_gbytem(72960);
        case  192: return full_gbytem(46055);
        case  196: return full_gbytem(46055);
        case  201: return full_gbytem(4225);
        case  202: return full_gbytem(2795);
        case  203: return full_gbytem(1755);
        case  204: return full_gbytem(6324);
        case  205: return full_gbytem(1755);
        case  206: return full_gbytem(2091);
        case  207: return full_gbytem(1715);
        case  208: return full_gbytem(783);
        case  209: return full_gbytem(8181);
        case  210: return full_gbytem(625);
        case  211: return full_gbytem(6045);
        case  212: return full_gbytem(23865);
        case  213: return full_gbytem(10965);
        case  214: return full_gbytem(6693);
        default: return grid_noncat(gdsp); /* U.S. non-standard grid,
						   use GDS */
        }
    case CENTER_JMA:
    case CENTER_CMS:
    case CENTER_UKMET:
        return grid_noncat(gdsp); /* use GDS */
    case CENTER_FNOC:
        switch (g1i(pdsp->grid)) {
        case  220:
        case  221: return full_gbytem(3969);
        case  223: return full_gbytem(10512);
        default:
            return grid_noncat(gdsp);
        }
    case CENTER_ECMWF:
        switch (g1i(pdsp->grid)) {
        case  5:
        case  6:
        case  7:
        case  8:
            return full_gbytem(1073);
        case 1:
        case 2:
        case 3:
        case 4:
        case 9:
        case 10:
        case 11:
        case 12:
            return full_gbytem(1369);
        case 13:
        case 14:
        case 15:
        case 16:
            return full_gbytem(361);
        default:
            return grid_noncat(gdsp);
        }
    default:
        return grid_noncat(gdsp);
    }
}

/*
 * Free byte map.
 */
void
free_gbytem(gb)
    gbytem* gb;
{
    if(gb) {
        if(gb->keep)
            return;             /* auto-initialized in static, so don't free */
        if(gb->map)
            free(gb->map);
        free(gb);
    }
}
