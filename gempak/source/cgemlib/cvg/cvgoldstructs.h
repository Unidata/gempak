
#define OLD_MAX_CNTY	200

/************************************************************************
 * cvgoldstructs.h							*
 * Contains the old versions of VGF structure, including WATCH BOX     	*
 * VERSION 0, VERSION 1, VERSION 2, and SIGMET VERSION 0		*
 *   									*
 **									*
 * J. Wu/GSC	01/01	Created                                 	*
 * J. Wu/GSC	02/01	Added latest v1 SIGMET into v1/v2 _VG_DBStruct	* 
 * 			& prototypes for old swapping functions     	* 
 * D.W.Plummer/NCEP	 8/01	Added new watch element			*
 * J. Wu/SAIC	06/02	add version 4 structure & swapping function	*
 * H. Zeng/XTRIA        01/03   Added WatchBox version 5                *
 ***********************************************************************/


/************************************************************************
 * v0_vgstruct								*
 * Contains the old VG structure for version 0 SIGMET and WATCH BOX 	*
 ***********************************************************************/

/*
 *  SIGMET VERSION 0
 */
typedef struct v0_sigmetinfo
{
    int         subtype;        /* type of sigmet area (isol,line,area) */
    int         npts;           /* number of points for sigmet area     */
    int         lintyp;         /* line type                            */
    int         linwid;         /* line width                           */
    int         sol;            /* side of line                         */
    char        area[MAX_AREASTR];   /* area(MWO) indicator of unit     */
    char        fir[MAX_FIRSTR];   /* location indicator of FIR unit(s) */
    int         status;         /* 0 = new, 1 = amend, 2 = cancel       */
    float       distance;       /* distance (nautical miles)            */
    char        msgid[MAX_MIDSTR];      /* message id (alfa,bravo,etc.) */
    int         seqnum;                 /* sequence number (1,2,3,...)  */
    char        stime[MAX_TIMESTR];     /* start valid time (ddhhmm)    */
    char        etime[MAX_TIMESTR];     /* end valid time (ddhhmm)      */
    char        remarks[MAX_REMSTR];    /* descriptive remarks          */
    int         sonic;                  /* supersonic indicator (0,1)   */
    char        phenom[MAX_PHENSTR];    /* phenomenon                   */
    char        trend[MAX_TRENDSTR];    /* trend                        */
    char        move[MAX_MOVESTR];      /* movement                     */
    int         obsfcst;        /* observed/forecast indicator (0,1,2)  */
    char        obstime[MAX_TIMESTR];   /* observed/forecast time 
                                           (ddhhmm, UTC)*/
    int         fl;             /* flight level (100s ft)               */
    int         spd;            /* speed of phenomenon (kts)            */
    char        dir[MAX_DIRSTR];/* direction of phenomenon (compass)    */
    char        tops[MAX_TOPSSTR];      /* tops                         */
    char        fcstr[MAX_FCSTRSTR];    /* forecaster name              */
} v0_SigmetInfo;

typedef struct v0_sigmettype
{
    v0_SigmetInfo	info;
    float       	latlon[MAX_SIGMET*2];
} v0_SigmetType;

/*----------------------------------------------------------------------*/
 
/*
 *  WATCH BOX VERSION 0
 */
typedef struct v0_watchboxinfo
{
    int         numpts;
    int         w_style;        /* watch style (WBC or PGRAM)           */
    int         w_type;         /* watch type (TORNADO or SVR T-STM)    */
    int         w_number;       /* watch number                         */
    char        w_file[128];    /* watch filename                       */
    int         w_shape;        /* PGRAM watch shape (EW, NS or ESOL)   */
} v0_WatchBoxInfo;

typedef struct v0_watchboxtype
{
    v0_WatchBoxInfo 	info;
    float       	latlon[MAXPTS*2];
} v0_WatchBoxType;

typedef struct v0_vg_dbstruct
{
    VG_HdrStruct hdr;
    union
    {
        v0_SigmetType    sig;
        v0_WatchBoxType  wbx;
    } elem;
} v0_VG_DBStruct;


/************************************************************************
 * v1_vgstruct								*
 * Contains the old VG structure for version 1 WATCH BOX 		*
 ***********************************************************************/

/*
 *  WATCH BOX VERSION 1
 */
typedef struct v1_watchboxinfo
{
    int         numpts;
    int         w_style;        /* watch style (WBC or PGRAM)           */
    int         w_shape;        /* PGRAM watch shape (EW, NS or ESOL)   */
    /* watch formatting and issuance relevant information               */
    int         w_istat;        /* watch issuing status                 */
    int         w_number;       /* watch number                         */
    char        w_iss_t[20];    /* watch issue time                     */
    char        w_exp_t[20];    /* watch expiration time                */
    int         w_type;         /* watch type (TORNADO or SVR T-STM)    */
    int         w_severity;     /* watch severity                       */
    char        w_timezone[4];  /* watch primary time zone              */
    char        w_hailsz[8];    /* watch max hail size                  */
    char        w_windg[8];     /* watch max wind gust                  */
    char        w_tops[8];      /* watch max tops                       */
    char        w_msmv_d[8];    /* watch mean storm motion vector (dir) */
    char        w_msmv_s[8];    /* watch mean storm motion vector (spd) */
    char        w_states[20];   /* watch states included                */
    char        w_adjarea[20];  /* watch adjacent areas included        */
    char        w_replw[24];    /* watch replacement watch numbers      */
    char        w_fcstr[64];    /* watch issuing forecaster name(s)     */
    char        w_file[128];    /* watch filename                       */
    int         w_issued;       /* watch flag for issuance: =0 N, =1 Y  */
    /* watch status message formatting and issuance relevant info       */
    char        wsm_iss_t[20];  /* wsm issue time                       */
    char        wsm_exp_t[20];  /* wsm expiration time                  */
    char        wsm_ref[32];    /* wsm reference direction              */
    char        wsm_from[128];  /* wsm most recent "from" line          */
    char        wsm_meso[8];    /* wsm mesoscale discussion number      */
    char        wsm_fcstr[64];  /* wsm issuing forecaster name(s)       */

} v1_WatchBoxInfo;

typedef struct v1_watchboxtype
{
    v1_WatchBoxInfo 	info;
    float       	latlon[MAXPTS*2];
} v1_WatchBoxType;

typedef struct v1_vg_dbstruct
{
    VG_HdrStruct hdr;
    union
    {
        SigmetType	sig;
        v1_WatchBoxType	wbx;
    } elem;
} v1_VG_DBStruct;


/************************************************************************
 * v2_vgstruct								*
 * Contains the old VG structure for version 2 WATCH BOX 		*
 ***********************************************************************/

/*
 *  WATCH BOX VERSION 2
 */
typedef struct v2_watchboxinfo
{
    int         numpts;
    int         w_style;        /* watch style (WBC or PGRAM)           */
    int         w_shape;        /* PGRAM watch shape (EW, NS or ESOL)   */
    /* watch formatting and issuance relevant information               */
    int         w_istat;        /* watch issuing status                 */
    int         w_number;       /* watch number                         */
    char        w_iss_t[20];    /* watch issue time                     */
    char        w_exp_t[20];    /* watch expiration time                */
    int         w_type;         /* watch type (TORNADO or SVR T-STM)    */
    int         w_severity;     /* watch severity                       */
    char        w_timezone[4];  /* watch primary time zone              */
    char        w_hailsz[8];    /* watch max hail size                  */
    char        w_windg[8];     /* watch max wind gust                  */
    char        w_tops[8];      /* watch max tops                       */
    char        w_msmv_d[8];    /* watch mean storm motion vector (dir) */
    char        w_msmv_s[8];    /* watch mean storm motion vector (spd) */
    char        w_states[20];   /* watch states included                */
    char        w_adjarea[20];  /* watch adjacent areas included        */
    char        w_replw[24];    /* watch replacement watch numbers      */
    char        w_fcstr[64];    /* watch issuing forecaster name(s)     */
    char        w_file[128];    /* watch filename                       */
    int         w_issued;       /* watch flag for issuance: =0 N, =1 Y  */
    /* watch status message formatting and issuance relevant info       */
    char        wsm_iss_t[20];  /* wsm issue time                       */
    char        wsm_exp_t[20];  /* wsm expiration time                  */
    char        wsm_ref[32];    /* wsm reference direction              */
    char        wsm_from[128];  /* wsm most recent "from" line          */
    char        wsm_meso[8];    /* wsm mesoscale discussion number      */
    char        wsm_fcstr[64];  /* wsm issuing forecaster name(s)       */
    /* county information                                               */
    int         numcnty;                /* number of counties           */
    int         cn_flag;                /* county plot flag             */
    int         cn_stat[OLD_MAX_CNTY];      /* county status                */
    float       cn_ltln[OLD_MAX_CNTY*2];    /* county locations      	*/
} v2_WatchBoxInfo;

typedef struct v2_watchboxtype
{
    v2_WatchBoxInfo 	info;
    float       	latlon[MAXPTS*2];
} v2_WatchBoxType;

typedef struct v2_vg_dbstruct
{
    VG_HdrStruct hdr;
    union
    {
        SigmetType	sig;
        v2_WatchBoxType	wbx;
    } elem;
} v2_VG_DBStruct;


/************************************************************************
 * v3_vgstruct								*
 * Contains the old VG structure for version 3 WATCH BOX 		*
 ***********************************************************************/

/*
 *  WATCH BOX VERSION 3
 */
typedef struct v3_watchboxinfo
{
    int         numpts;
    int         w_style;        /* watch style (WBC or PGRAM)           */
    int         w_shape;        /* PGRAM watch shape (EW, NS or ESOL)   */
    char        w_a0id[8];      /* watch anchor point #0 station id     */
    float       w_a0lt;         /* watch anchor point #0 latitude       */
    float       w_a0ln;         /* watch anchor point #0 longitude      */
    int         w_a0dis;        /* watch anchor point #0 distance (sm)  */
    char        w_a0dir[4];     /* watch anchor point #0 dir (16-pt)    */
    char        w_a1id[8];      /* watch anchor point #1 station id     */
    float       w_a1lt;         /* watch anchor point #1 latitude       */
    float       w_a1ln;         /* watch anchor point #1 longitude      */
    int         w_a1dis;        /* watch anchor point #1 distance (sm)  */
    char        w_a1dir[4];     /* watch anchor point #1 dir (16-pt)    */
    /* watch formatting and issuance relevant information               */
    int         w_istat;        /* watch issuing status                 */
    int         w_number;       /* watch number                         */
    char        w_iss_t[20];    /* watch issue time                     */
    char        w_exp_t[20];    /* watch expiration time                */
    int         w_type;         /* watch type (TORNADO or SVR T-STM)    */
    int         w_severity;     /* watch severity                       */
    char        w_timezone[4];  /* watch primary time zone              */
    char        w_hailsz[8];    /* watch max hail size                  */
    char        w_windg[8];     /* watch max wind gust                  */
    char        w_tops[8];      /* watch max tops                       */
    char        w_msmv_d[8];    /* watch mean storm motion vector (dir) */
    char        w_msmv_s[8];    /* watch mean storm motion vector (spd) */
    char        w_states[20];   /* watch states included                */
    char        w_adjarea[20];  /* watch adjacent areas included        */
    char        w_replw[24];    /* watch replacement watch numbers      */
    char        w_fcstr[64];    /* watch issuing forecaster name(s)     */
    char        w_file[128];    /* watch filename                       */
    int         w_issued;       /* watch flag for issuance: =0 N, =1 Y  */
    /* watch status message formatting and issuance relevant info       */
    char        wsm_iss_t[20];  /* wsm issue time                       */
    char        wsm_exp_t[20];  /* wsm expiration time                  */
    char        wsm_ref[32];    /* wsm reference direction              */
    char        wsm_from[128];  /* wsm most recent "from" line          */
    char        wsm_meso[8];    /* wsm mesoscale discussion number      */
    char        wsm_fcstr[64];  /* wsm issuing forecaster name(s)       */
    /* county information                                               */
    int         numcnty;                /* number of counties           */
    int         cn_flag;                /* county plot flag             */
    int         cn_stat[OLD_MAX_CNTY];      /* county status                */
    float       cn_ltln[OLD_MAX_CNTY*2];    /* county locations      	*/
} v3_WatchBoxInfo;

typedef struct v3_watchboxtype
{
    v3_WatchBoxInfo 	info;
    float       	latlon[MAXPTS*2];
} v3_WatchBoxType;

typedef struct v3_vg_dbstruct
{
    VG_HdrStruct hdr;
    union
    {
        SigmetType	sig;
        v3_WatchBoxType	wbx;
    } elem;
} v3_VG_DBStruct;


/************************************************************************
 * v4_vgstruct								*
 * Contains the old VG structure for version 4 WATCH BOX 		*
 ***********************************************************************/

/*
 *  WATCH BOX VERSION 4
 */
typedef struct v4_watchboxinfo
{
    int 	numpts;
    /* watch definition information					*/
    int		w_style;	/* watch style (WBC or PGRAM)		*/
    int		w_shape;	/* PGRAM watch shape (EW, NS or ESOL)	*/
    char	w_a0id[8];	/* watch anchor point #0 station id	*/
    float	w_a0lt;		/* watch anchor point #0 latitude	*/
    float	w_a0ln;		/* watch anchor point #0 longitude	*/
    int		w_a0dis;	/* watch anchor point #0 distance (sm)	*/
    char	w_a0dir[4];	/* watch anchor point #0 dir (16-pt)	*/
    char	w_a1id[8];	/* watch anchor point #1 station id	*/
    float	w_a1lt;		/* watch anchor point #1 latitude	*/
    float	w_a1ln;		/* watch anchor point #1 longitude	*/
    int 	w_a1dis;	/* watch anchor point #1 distance (sm)	*/
    char	w_a1dir[4];	/* watch anchor point #1 dir (16-pt)	*/
    /* watch formatting and issuance relevant information		*/
    int		w_istat;	/* watch issuing status			*/
    int 	w_number;	/* watch number				*/
    char	w_iss_t[20];	/* watch issue time			*/
    char	w_exp_t[20];	/* watch expiration time		*/
    int		w_type;		/* watch type (TORNADO or SVR T-STM)	*/
    int		w_severity;	/* watch severity			*/
    char	w_timezone[4];	/* watch primary time zone		*/
    char	w_hailsz[8];	/* watch max hail size			*/
    char	w_windg[8];	/* watch max wind gust			*/
    char	w_tops[8];	/* watch max tops     			*/
    char	w_msmv_d[8];	/* watch mean storm motion vector (dir)	*/
    char	w_msmv_s[8];	/* watch mean storm motion vector (spd)	*/
    char	w_states[80];	/* watch states included		*/
    char	w_adjarea[80];	/* watch adjacent areas included	*/
    char	w_replw[MAX_REPLW_LEN];	/* watch replacement watch numbers */
    char	w_fcstr[MAX_FCSTR_LEN];	/* watch issuing forecaster name(s)*/
    char 	w_file[128];	/* watch filename			*/
    int		w_issued;	/* watch flag for issuance: =0 N, =1 Y	*/
    /* watch status message formatting and issuance relevant info	*/
    char	wsm_iss_t[20];	/* wsm issue time			*/
    char	wsm_exp_t[20];	/* wsm expiration time			*/
    char	wsm_ref[32];	/* wsm reference direction		*/
    char	wsm_from[128];	/* wsm most recent "from" line		*/
    char	wsm_meso[8];	/* wsm mesoscale discussion number	*/
    char	wsm_fcstr[MAX_FCSTR_LEN];/* wsm issuing forecaster name(s)*/
    /* county information						*/
    int 	numcnty;		/* number of counties		*/
    int 	cn_flag;		/* county plot flag		*/
    int 	cn_stat[OLD_MAX_CNTY];	/* county status		*/
    int 	cn_fips[OLD_MAX_CNTY];	/* county FIPS code		*/
    float	cn_ltln[OLD_MAX_CNTY*2];	/* county locations	*/
} v4_WatchBoxInfo;

typedef struct v4_watchboxtype
{
    v4_WatchBoxInfo 	info;
    float       	latlon[MAXPTS*2];
} v4_WatchBoxType;

typedef struct v4_vg_dbstruct
{
    VG_HdrStruct hdr;
    union
    {
        SigmetType	sig;
        v4_WatchBoxType	wbx;
    } elem;
} v4_VG_DBStruct;

/*
 *  WATCH BOX VERSION 5
 */
typedef struct v5_watchboxinfo
{
    int 	numpts;
    /* watch definition information					*/
    int		w_style;	/* watch style (WBC or PGRAM)		*/
    int		w_shape;	/* PGRAM watch shape (EW, NS or ESOL)	*/
    char	w_a0id[8];	/* watch anchor point #0 station id	*/
    float	w_a0lt;		/* watch anchor point #0 latitude	*/
    float	w_a0ln;		/* watch anchor point #0 longitude	*/
    int		w_a0dis;	/* watch anchor point #0 distance (sm)	*/
    char	w_a0dir[4];	/* watch anchor point #0 dir (16-pt)	*/
    char	w_a1id[8];	/* watch anchor point #1 station id	*/
    float	w_a1lt;		/* watch anchor point #1 latitude	*/
    float	w_a1ln;		/* watch anchor point #1 longitude	*/
    int 	w_a1dis;	/* watch anchor point #1 distance (sm)	*/
    char	w_a1dir[4];	/* watch anchor point #1 dir (16-pt)	*/
    /* watch formatting and issuance relevant information		*/
    int		w_istat;	/* watch issuing status			*/
    int 	w_number;	/* watch number				*/
    char	w_iss_t[20];	/* watch issue time			*/
    char	w_exp_t[20];	/* watch expiration time		*/
    int		w_type;		/* watch type (TORNADO or SVR T-STM)	*/
    int		w_severity;	/* watch severity			*/
    char	w_timezone[4];	/* watch primary time zone		*/
    char	w_hailsz[8];	/* watch max hail size			*/
    char	w_windg[8];	/* watch max wind gust			*/
    char	w_tops[8];	/* watch max tops     			*/
    char	w_msmv_d[8];	/* watch mean storm motion vector (dir)	*/
    char	w_msmv_s[8];	/* watch mean storm motion vector (spd)	*/
    char	w_states[80];	/* watch states included		*/
    char	w_adjarea[80];	/* watch adjacent areas included	*/
    char	w_replw[MAX_REPLW_LEN];	/* watch replacement watch numbers */
    char	w_fcstr[MAX_FCSTR_LEN];	/* watch issuing forecaster name(s)*/
    char 	w_file[128];	/* watch filename			*/
    int		w_issued;	/* watch flag for issuance: =0 N, =1 Y	*/
    /* watch status message formatting and issuance relevant info	*/
    char	wsm_iss_t[20];	/* wsm issue time			*/
    char	wsm_exp_t[20];	/* wsm expiration time			*/
    char	wsm_ref[32];	/* wsm reference direction		*/
    char	wsm_from[128];	/* wsm most recent "from" line		*/
    char	wsm_meso[8];	/* wsm mesoscale discussion number	*/
    char	wsm_fcstr[MAX_FCSTR_LEN];/* wsm issuing forecaster name(s)*/
    /* county information						*/
    int 	numcnty;		/* number of counties		*/
    int 	cn_flag;		/* county plot flag		*/
    int 	cn_fips[MAX_CNTY];	/* county FIPS code		*/
    float	cn_ltln[MAX_CNTY*2];	/* county locations		*/
} v5_WatchBoxInfo;

typedef struct v5_watchboxtype
{
    v5_WatchBoxInfo 	info;
    float       	latlon[MAXPTS*2];
} v5_WatchBoxType;

typedef struct v5_vg_dbstruct
{
    VG_HdrStruct hdr;
    union
    {
        SigmetType	sig;
        v5_WatchBoxType	wbx;
    } elem;
} v5_VG_DBStruct;

/************************************************************************ 
 * Prototypes of functions which swap old VG elements  		        *
 ***********************************************************************/

void    cvg_swap_v0 (   int             flag, 
                        int             readflg, 
			v0_VG_DBStruct  elold,
                        v0_VG_DBStruct  *elnew, 
			int             *iret );

void    cvg_swap_v1 (   int             flag, 
                        int             readflg, 
			v1_VG_DBStruct  elold,
                        v1_VG_DBStruct  *elnew, 
			int             *iret );

void    cvg_swap_v2 (   int             flag, 
                        int             readflg, 
			v2_VG_DBStruct  elold,
                        v2_VG_DBStruct  *elnew, 
			int             *iret );

void    cvg_swap_v3 (   int             flag, 
                        int             readflg, 
			v3_VG_DBStruct  elold,
                        v3_VG_DBStruct  *elnew, 
			int             *iret );

void    cvg_swap_v4 (   int             flag, 
                        int             readflg, 
			v4_VG_DBStruct  elold,
                        v4_VG_DBStruct  *elnew, 
			int             *iret );

void    cvg_swap_v5 (   int             flag, 
                        int             readflg, 
			v5_VG_DBStruct  elold,
                        v5_VG_DBStruct  *elnew, 
			int             *iret );

/*----------------------------------------------------------------------*/
