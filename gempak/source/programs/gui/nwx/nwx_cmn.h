/************************************************************************
 * nwx_cmn.h								*
 *									*
 * This header file includes the common defines and structure        	*
 * declarations for nwx.                                               	*
 ** 									*
 * Log:									*
 * S. Jacobs/NMC	 8/94						*
 * C. Lin/EAI	 	 9/95						*
 * D.W.Plummer/NCEP	 9/96	added another element in smethod_t (OBS)*
 * D.W.Plummer/NCEP	11/96	added ddttm to usrslct_t structure	*
 *                              also increased MAX_PLOT_STN to 3800	*
 * D. Kidwell/NCEP	 8/98	Added watch box information		*
 * D. Kidwell/NCEP	 4/99	Deleted watch box cancel flag		*
 * D. Kidwell/NCEP	 5/99	Replaced MAX_PLOT_STN with LLSTFL       *
 * T. Piper/SAIC	 6/02	Removed unused selct_by variable	*
 * T. Piper/SAIC	 7/02	Moved reportSTD from srcb.c to get fname*
 * T. Piper/SAIC	 7/02	Reinstated the select by state option	*
 * T. Piper/SAIC	 8/02	Increased PRNMAX to support state option*
 * R. Tian/SAIC		 4/03	Added textmode_t and modeflg 		*
 * R. Tian/SAIC		11/03	Added autoMenuBtnW	 		*
 * R. Tian/SAIC		12/03	Added stnindex           		*
 * T. Piper/SAIC	07/04	Increased MAX_REPORTS from 300 -> 1000	*
 * E. Safford/SAIC	09/07	split gui.h into nwx_gui.h & nwx_cmn.h	*
 *				  incorporated nwxcmn.h in nwx_cmn.h	*
 * T. Piper/SAIC	01/08	Added REPMAX from gemprm.h		*
 * X. Guo/CWS           09/11   Increased REPMAX from 150000 -> 250000  *
 ***********************************************************************/

#ifndef NWX_CMN
#define NWX_CMN

#include <sys/utsname.h>
#include "no_xm_geminc.h"
#include "gemprm.h"   	/* MAXTYP, LLSTFL */
#include "ctbcmn.h"
#include "nwx_dstruct.h"

/*---------------------------------------------------------------------*/

#define PRNMAX		( 1000000 )

#define ALL_COL         2
#define ALL_MRK         6
#define SEL_COL         3
#define SEL_MRK         1
#define RES_COL         32
#define MIN_HR          00
#define MAX_PTS		100
#define MAX_CONTOURS	100
#define MAX_REPORTS     1000
#define MAX_WATCHES	200
#define MAX_WPTS	7
#define REPMAX          ( 250000 ) /* Maximum text report size */

/*---------------------------------------------------------------------*/

#define SSTR_LEN 128         /* short string length     */
#define LSTR_LEN 256         /* long  string length     */

#define SRCHSTR_LEN  25     /* search string length     */

/*
 *  Time covered types
 */
#define EVENT		( 0 )
#define SCHEDULED	( 1 )


struct date_time_info {
    	int	year;
	int	month;
	int	day;
	int	hour;
	int	minute;
};

struct srch_strng_info {
	char	search_string[49];
	char	start_of_text[2][9];
	char	end_of_text[2][9];
};

struct data_file_info {
	FILE	*fptr;
	char	filnam[133];
	int	file_len;
};

struct directory_info {
	char	dirpath[73];
	int	nent;
	int	cfilnum;
	char	filnam[NFILES][73];
};

/*
 * structure to hold nwx master table and map table
 */
typedef struct {
        int ndtyp;                              /* # of data types */
        int nmap;                               /* # of maps  */
        struct datatype_list  dtyp_info[MAXTYP];/* data type info of each product*/
        struct maptype_list   map_info[MAXTYP]; /* info of maps */
}nwxtbl_t;

/*
 * structure to hold the info of possible forecast stations
 */
typedef struct {
        int   nstn;                     	/* # of stations */
        char  stnName[LLSTFL][33];      	/* station local names  */
        char  stateId[LLSTFL][3];       	/* state id names  */
        char  counId[LLSTFL][3];        	/* country id names  */
        float lat[LLSTFL];              	/* latitude of the station */
        float lon[LLSTFL];              	/* longitude of the station */
        float elv[LLSTFL];              	/* elevation of the station */
	char  srchstr[LLSTFL][SRCHSTR_LEN];   	/* record search strings */
	char  bulstr[SRCHSTR_LEN];	      	/* bulletion head string */
	int   rptstn[MAX_REPORTS]; 		/* indices of the stations issuing reports */
	float rptstnlat[MAX_REPORTS]; 		/* lat of the stations issuing reports */
	float rptstnlon[MAX_REPORTS]; 		/* lon of the stations issuing reports */
						/* lat, lon info for stations issuing 
						   special reports duplicated to facilitate 
						   easier marker plotting, gtrans, etc.,
						   function calls                     */
	int   nrptstn;		    		/* number of unique stations issueing report(s) */
	int   nreports;		    		/* total number of individual reports */
}stnlist_t; 

/*
 * lat/lon of the map boundaries
 */
typedef struct {
        float  x[2];    /* x-coord of lower left and upper right */
        float  y[2];    /* y-coord of lower left and upper right */
}mapbnd_t; 

/*
 * structure to hold search related information
 */

typedef enum {
	STANDARD, WATCHWARN, OBS
}smethod_t;

typedef struct {
        int     idtyp;                    /* index to data type info */
        struct  directory_info  dir_info; /* info about the data directory */ 
        struct  data_file_info  file_info;/* info about the selected data file */
        char    srchstr[SRCHSTR_LEN];     /* string to search for */
        char    start_of_text[2][9];      /* start mark(s) of a record */
        char    end_of_text[2][9];	  /* end mark(s) of a record */
        struct  date_time_info startd;    /* starting date/time */
        struct  date_time_info endd;      /* endding date/time */
        int     sflag;                    /* -1 = backward */
                                          /*  0 = start over */
                                          /*  1 = foreward */
	smethod_t     smethod;		  /* flag for search method */
	int	current;                  /* pointer to current WW file */
}srchinfo_t;

/*
 * structure to hold user selections
 */

typedef enum {
	STATION, STATE
} selectby_t ;

typedef enum {
	NO_ZOOM, ZOOM
} zoom_t ;

typedef enum {
	REPLACE, APPEND
} textmode_t;

typedef struct {
	int  mapindx;		/* map background index into nwxtbl map_info*/
	selectby_t  selct_by;  	/* 0 -- select by station, 1 -- state */
	zoom_t  zoomflg;	/* 0 -- no zoom, 1 -- zoom */  
	textmode_t  modeflg;	/* 0 -- replace, 1 -- append */
	int  ndttm;     	/* date/time preference */
	int  ddttm;     	/* date/time default */
	struct guid_grp *group; /* address of the selected group info */
	int  prod;      	/* index of the selected product in the group */ 
	int  prvnxt;    	/* flag for previous/next button in text window */
}usrslct_t;

/*
 * structures to hold the plot information
 */

typedef enum {
	EMPTY, STNSELECT, VALUE, GRAPHIC, WATCHBOX 
}plotmode_t;

struct mrkv {
	int          nstn;
	float        lat[LLSTFL];
	float        lon[LLSTFL];
	int          dvalues[LLSTFL];
	int          breaks[LLSTFL];
	int          ncolor;
	int          icolrs[LLSTFL];
	int          marktype; 
	float        marksize;
	int          markwdth;
	int          pltflag;
	int          iposn;
}; /* for plotting values as marker */

struct cntline {
	char  label[7]; /* label for the line */
	int   npt;      /* # of points for the contour*/
	float lat[MAX_PTS];  /* lat for each point */
	float lon[MAX_PTS];  /* lon for each point */
	int   color;	/* color for the contour */
}; /* for each contour line */

struct contour {
	int            nc;	 /* # of contours */
	struct cntline line[MAX_CONTOURS];  
}; /* for collection of contour lines */

struct 	watchinfo {
	char	wtchnum[5];	/* watch number */
	char	valid[DTTMSZ];  	/* watch valid GEMPAK dattim */
	char	expire[DTTMSZ];	/* watch expires GEMPAK dattim */
	int   	npt;      	/* # of watch area vertices */
	float 	lat[MAX_WPTS];  /* lat for each point */
	float   lon[MAX_WPTS];  /* lon for each point */
	int     color;		/* color for the watch box */
}; /* for each watch box */

struct	watchbox {
	int	nb;		/* # of watchboxes */
	struct	watchinfo winfo[MAX_WATCHES];
}; /* for collection of watch boxes */

typedef struct {
	plotmode_t     mode;            /* plot mode */
	union  {
		struct mrkv    markv;
		struct contour cnt;
		struct watchbox wbox;
	}data;				/* plot data */

/*
 * convenient for accessing data
 */
#define plt_mark    data.markv	        /* marker for plot values  */
#define plt_cnt     data.cnt	        /* contours */
#define plt_wbox    data.wbox	        /* watch box */

}plotdata_t;
/*---------------------------------------------------------------------*/

/*
 *	Declare the global variables.
 */

extern int	  _idtyp_save;
extern nwxtbl_t   *nwxTable; 	/* nwx master/map table */
extern stnlist_t  stnList;	/* related station info */
extern usrslct_t  usrSelect;	/* user selections */ 
extern mapbnd_t	  mapBnd;	/* map boundaries */
extern srchinfo_t srchInfo;	/* search information */
extern plotdata_t plotData;	/* data for plotting */

extern char	reportText[];   /* text report */
extern char	printText[];	/* text for print */
extern char	wtchText[MAX_WATCHES][REPMAX]; /* Text for watch boxes */


struct reportSTD {      /* structure to hold info about each bulletin */
    int         position;       /* position */
    int         length;         /* length */
    char        fname[MXFLSZ];     /* location */
};
extern struct reportSTD *_rstd;

#include "proto_nwx.h"

#endif  /* NWX_CMN */
