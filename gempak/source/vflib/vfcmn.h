
/************************************************************************
 * vfcmn.h                                                              *
 * Contains the structure for a watch box element that is being stored  *
 * or retrieved from a verification file.                               *
 *									*
 **									*
 *Log:									*
 * A. Hardy/GSC		03/00	Modified from spcwtch.h			*
 * A. Hardy/GSC		03/00	Added vfeftm and vfwwcl functions       *
 * A. Hardy/GSC		04/00	Added vfwpwn,vfwoui,vfwawn,vfavcd	*
 *				vfarea, vfclst,	vfvors			*
 * A. Hardy/GSC		05/00	Added vfwwcp				*
 * A. Hardy/GSC		06/00	Added vfsort				*
 * A. Hardy/GSC		12/00	Added pr_knmh                           *
 * A. Hardy/GSC		12/00	Removed prototypes; put in proto_vf.h   *
 * A. Hardy/GSC		 9/01	Increased size of states incl string	*
 * A. Hardy/SAIC        10/01   Added VFCMN_GLOBAL			*
 * R. Tian/SAIC		06/02	Removed ispcwmo flag			*
 * R. Tian/SAIC		06/02	Changed cnty size to be MAXCNTY		*
 * A. Hardy/NCEP	 5/03   Change hailsz from int -> float		*
 * H. Zeng/SAIC		03/06	removed pr_knmh				*
 ***********************************************************************/

#include "pgprm.h"

struct data_file_info { 
    	FILE	*ifp;
	char	filnam[256];
	int	file_len;
};
 struct date_time_info {
        int     year;
	int     month;
	int     day;
	char    hour[5];
};

struct motion_info {
       int     deg;
       int     speed;
};

struct end_info {
       int     dist1;   		/* distance from 1st station */
       char    dirct1[4];		/* direction from 1st station */
       char    stn1[4];	                /* 1st stations 3 char. ID */
       char    stnnam1[33];		/* 1st station's name */
       char    stateid1[3];             /* 1st station's state id */
       char    statnm1[17];             /* 1st stations state name */
       int     dist2;			/* distance from 2nd station */
       char    dirct2[4];		/* direction from 2nd station */
       char    stn2[4];	                /* 2nd stations 3 char. ID */
       char    stnnam2[33];		/* 2nd station's name */
       char    stateid2[3];             /* 2nd station's state id */
       char    statnm2[17];             /* 2nd stations state name */
};

struct attrib_info {
       int     dist;
       char    dirc[16];
};

struct corner_info {
       float   lat;
       float   lon;
       float   newlat;
       float   newlon;
};

struct county_info {
       char    ugc[7];
       char    state[3];
       char    cname[33];
       float   ctylat;
       float   ctylon;
       char    fips[7];
       char    wfo[4];
       char    indnam[33];

};
typedef struct {
       struct data_file_info file_info; /* info on selected data file */
       int      wnum;		        /* watch number */
       char	wtype[32];		/* watch type */
       char	pdsn[8];		/* PDS or Normal */
       struct date_time_info itime;	/* issue time */
       struct date_time_info vtime;	/* valid time */
       struct date_time_info etime;	/* expiration time */
       struct end_info ancrpt;		/* anchor endpoint */
       struct end_info vorrpt;		/* VOR endpoint */
       struct attrib_info ancatt;	/* anchor attribute */
       struct attrib_info voratt;	/* VOR attribute */
       struct corner_info wcpnt1;	/* watch corner point - UR */
       struct corner_info wcpnt2;	/* watch corner point - LR */
       struct corner_info wcpnt3;	/* watch corner point - LL */
       struct corner_info wcpnt4;	/* watch corner point - UL */
       float    hailsz;		        /* hail size */
       int      maxgust;		/* maximum gust (kts) */
       int      maxmph;			/* maximum gust (mph) */
       int      maxtops;		/* maximum tops */
       struct motion_info motion;	/* motion (deg, kts) */
       char     timzone[4];		/* time zone */
       char	replcnm[10][10];	/* replacement watch number */
       int      wwrepnm;	        /* number of replaced watches */
       char     states[256];		/* states included */
       char	status[7];		/* watch status */
       char     frcstr[20];		/* forecaster */
       int	warea;			/* watch area */
       struct county_info cnty[MAXCNTY];/* county names */
       int      total;			/* total number of counties */
       int      sssnum;			/* last digit of watch number */
       char     genday[50];             /* general time of day phrase */
       char     ongoing[128];           /* ongoing watches */
       char     curtim[7];              /* current system time-DDHHMM */
   }SpcInfo_t;

#include "proto_vf.h"
