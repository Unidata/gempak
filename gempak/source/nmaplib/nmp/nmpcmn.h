/************************************************************************
 * nmpcmn                                                               *
 *                                                                      *
 * This header file contains the global variables and structures for    *
 * the NMP library.                                                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            11/00   Created                                 *
 * M. Li/GSC		12/00	Added mapobj_t				*
 * M. Li/GSC		02/01	Added map, mapfile & mapattr to mapobj  *
 * E. Safford/GSC       05/01   added RAD and SAT string defines        *
 * J. Wu/SAIC		08/01   add default_overlay[]			*
 * J. Wu/SAIC		08/01   add default_map[]			*
 * E. Safford/GSC       01/02   add tru_proj & garea fields to mapobj_t *
 * E. Safford/SAIC	01/08	include proto_nmaplib.h			*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"
#include "xwprm.h"
#include "proto_nmaplib.h"


/*
 *  These two strings are the default map settings for SAT and RAD sources.
 */
#define SAT_STR         "SAT"
#define RAD_STR         "Custom"


/*
 * Data structure for predefined map areas.
 *
 *    This list will be built from the mapinfo.tbl table.
 *    The mapinfo.tbl entries make up the available pre-defined map areas
 *    in the map window.
 */
typedef struct {
    nmpstr_t	name;		/* name of the map which users see */
    nmpstr_t	geog;		/* abreviated garea name */
}maptbl_ent_t;


/*
 * Data structure for overlays from the mapovl.tbl table.
 */
typedef struct {
	int     	ityp;  /* 0 = lat/lon, 1 = map, 2-4 = stnplt */
	nmpovlstr_t	attr;  /* attribute string */
        nmpovlstr_t	gname; /* gempak name */
        nmpovlstr_t	title; /* overlay title name (shown in GUI) */
        Boolean active;        /* on/off flag */
} overlay_t;

typedef struct {
        overlay_t     *mapovl;       /* array of overlays */
        int           novl;          /* number of map overlays */
}mapovl_t;


/*
 * Data info needed to draw a map
 */
typedef	struct {
	char		imgfile[256];	/* image file name		      */
	int		imgtyp;		/* NO_IMG || SAT_IMG || RAD_IMG	      */
	char		mapfile[256];   /* map file string		      */
	char		mapattr[256];	/* map attribute string		      */
	nmpstr_t	map;	    /* name of the predefined map	      */
	nmpstr_t	proj;	    /* map projection string (PROJ)           */
	nmpstr_t	tru_proj;   /* map projection string (PROJ)           */
	nmpstr_t	garea[2];   /* 0 is starting garea, 1 is zoomed garea */
	nmpstr_t	tru_garea[2];/* 0 is starting garea, 1 is zoomed garea */
	int		mode;	    /* graphics mode, normally is 1           */
} mapobj_t;


#ifdef NMP_GLOBAL

    maptbl_ent_t	*map_tbl;
    int			num_maps;

    mapovl_t    overlay[MAX_LOOP];
    mapovl_t    default_overlay[MAX_LOOP];
    mapovl_t    saved_overlay[MAX_LOOP];

    mapobj_t	maps[MAX_LOOP];
    mapobj_t	default_map[MAX_LOOP];
    mapobj_t	saved_map[MAX_LOOP];

#else

    extern      maptbl_ent_t	*map_tbl;
    extern      int		num_maps;

    extern      mapovl_t        overlay[MAX_LOOP];
    extern      mapovl_t        default_overlay[MAX_LOOP];
    extern      mapovl_t        saved_overlay[MAX_LOOP];

    extern	mapobj_t	maps[MAX_LOOP];
    extern	mapobj_t	default_map[MAX_LOOP];
    extern	mapobj_t	saved_map[MAX_LOOP];

#endif

