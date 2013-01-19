/*
 *	Copyright 1988, University Corporation for Atmospheric Research
 *		Not for Resale. All copies to include this notice.
 */
/*	$Id: mc_area.h,v 1.3 1991/10/29 23:57:44 steve Exp $ */

/* Argh, looks like the numbers are in vax byte order */

#define TYPELEN		4 /* Short strings used as identifiers */
#define COMMENTLEN	32	/* longer strings */
/*
 * McIdas AREA DIRECTORY, based on documentation dated 5/87 by R. Dengal
 */
struct area_dir {
/*  1 */ unsigned int	status ;
/*  2 */ unsigned int	type ;
/*  3 */ unsigned int	satid ;
/*  4 */ unsigned int	ndate ; /* YYDDD */
/*  5 */ unsigned int	ntime ; /* HHMMSS */
/*  6 */ unsigned int	lcor ;
/*  7 */ unsigned int	ecor ;
/*  8 */ unsigned int	zcor ;
/*  9 */ unsigned int	lsiz ;
/* 10 */ unsigned int	esiz ;
/* 11 */ unsigned int	zsiz ;
/* 12 */ unsigned int	lres ;
/* 13 */ unsigned int	eres ;
/* 14 */ unsigned int	bands ;
/* 15 */ unsigned int	yzprefix ;
/* 16 */ unsigned int	projnum ;
/* 17 */ unsigned int	cdate ;
/* 18 */ unsigned int	ctime ;
/* 19 */ unsigned int	filtmap ;
/* 20 */ unsigned int	imageid ;
/* 21 */ unsigned int	resvid[4] ;
#define AREA_COMMENTS 24
/* 25 */ char	comments[COMMENTLEN] ;
#define AREA_CALKEY 32
/* 33 */ unsigned int	calkey ;
/* 34 */ unsigned int	navkey ;
/* 35 */ unsigned int	navkey2 ;
/* 36 */ unsigned int	lprefix ;
/* 37 */ unsigned int	pdl[8] ;
/* 45 */ unsigned int	band8 ;
/* 46 */ unsigned int idate ;
/* 47 */ unsigned int itime ;
/* 48 */ unsigned int	startscan ;
/* 49 */ unsigned int	doclen ;
/* 50 */ unsigned int	callen ;
/* 51 */ unsigned int	levlen ;
#define AREA_STYPE 51
/* 52 */ char	stype[TYPELEN] ;
/* 53 */ char	ctype[TYPELEN] ;
/* 54 */ unsigned int	reserved[11] ;
#define AREA_END 63
} ;


/*
 * Structure members common to all McIDAS navigation codicils:
 *
 * navigation type:
 *	'GOES' GOES satellites
 *	'AVHR' AVHRR data
 *	'PS  ' polar stereographic projection
 *	'LAMB' Lambert conformal projection
 *	'MREC' Mercator projection
 *	'MET ' Meteosat images (deformation nav)
 *	'    ' (or binary 0) no navigation for this image
 */
#define MC_NAV_COMMON	char	type[4];


/*
 * Generic McIDAS navigation codicil:
 */
typedef struct mc_nav_any {
    MC_NAV_COMMON
} mc_nav_any;


/*
 * McIdas GOES navigation codicil, based on documentation dated 5/87 by 
 * D. Santek
 */
typedef struct mc_nav_goes {
    MC_NAV_COMMON
    unsigned	iddate;
    unsigned	navtime;
    unsigned	orbtype;
    unsigned	etimy;
    unsigned	etimh;
    unsigned	semima;
    unsigned	eccen;
    unsigned	orbinc;
    unsigned	meana;
    unsigned	perigee;
    unsigned	asnode;
    unsigned	declin;
    unsigned	rascen;
    unsigned	piclin;
    unsigned	spinp;
    unsigned	deglin;
    unsigned	lintot;
    unsigned	degele;
    unsigned	eletot;
    unsigned	pitch;
    unsigned	yaw;
    unsigned	roll;
    unsigned	res1;
    unsigned	iajust;
    unsigned	iajtim;
    unsigned	res2;
    unsigned	iseang;
    unsigned	resskew;
    unsigned	res3;
    unsigned	scanline1;
    unsigned	timeb1;
    unsigned	timec1;
    unsigned	betacount1;
    unsigned	scanline2;
    unsigned	timeb2;
    unsigned	timec2;
    unsigned	betacount2;
    unsigned	gamma;
    unsigned	gammadot;
    char	reserved[80*4] ;
    char	memo[COMMENTLEN] ;
} mc_nav_goes;

/*
 * 4-byte offsets for mc_rarea.c
 */
#define NAV_DATA 1
#define NAV_RESERVED 40


/*
 * McIDAS navigation codicil.
 * Only type 'GOES' used here currently
 */
typedef union mc_nav {
    mc_nav_any	any;
    mc_nav_goes	goes;
} mc_nav;


/*
 * A mcidas area file looks like this:
 *  Area Directory, followed by
 *  Navigation Codicil, followed by
 *  the Image.
 *
 * N.B.
 *	Use of this a template is compiler dependent.
 * Will work most of the time (32 bit architectures) since the
 * arms of the struct are divisible by sizeof(word)
 */
struct mc_area {
	struct area_dir		*dir ;
	mc_nav			*nav ;
	unsigned char		*image ; /* image[imagelen] really */
	void			*private ;
} ;
