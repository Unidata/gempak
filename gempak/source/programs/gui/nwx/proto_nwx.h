/************************************************************************
 * proto_nwx.h								*
 *									*
 * This file contains header files and global variables for use in the	*
 * NWX routines.							*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC          1/01   					*
 * T. Piper/SAIC	 5/02	Added parameter to txtw_dttmSet		*
 * T. Piper/SAIC	 7/03	removed gmpk_init and map_init		*
 * R. Tian/SAIC		 7/03	added mapw_rmappstn			*
 * R. Tian/SAIC		11/03	added auto_start/stopAutoUpdt,dslw_load	*
 * T. Piper/SAIC	01/04	added nwxerr				*
 * T. Piper/SAIC	04/05	CSC for nsfopn				*
 * E. Safford/SAIC	12/07	rm all X/Motif references, add wrapper  *
 *				 function prototypes for gem funcs  	*
 ***********************************************************************/

#ifndef PROTO_NWX
#define	PROTO_NWX


/*
 *  NWX prototypes
 */

int 	dchk_alpha ( char  *str );

int 	dchk_digit ( char  *str );

void 	dir_getflist ( 	struct datatype_list	*dtyp_info,
			int			idtyp,
			struct date_time_info   startdttm,
			struct date_time_info   enddttm,
			struct directory_info	*dir_info,
			int			*iret );

void 	dir_getnextf ( 	struct directory_info	*dir_info,
			int			strtflg,
			struct data_file_info	*file_info,
			int			*iret );

void 	draw_map ( 	int			num,
			struct maptype_list	*map_info,
			int			zoomflg,
			mapbnd_t		*mapb,
			int			*iret );

void 	draw_stnmark ( 	int		numpts,
			float		*slat,
			float		*slon,
			int		jcolr,
			int		imrk,
			int		*iret );

void 	draw_value ( 	struct mrkv 	*markdata );

void 	draw_cntr ( 	struct contour 	*contours );

void 	draw_wbox ( 	struct watchbox *wtchbox,
			int		called_from );

void	dslw_apply(	void );

void	dslw_dttmSensitive( G_Boolean	state );

void	dslw_load(	int	called_from, 
			int	*iret );

void	dslw_srchMaster(int	item,
			int	*iret );

void 	dslw_toggle ( 	void );


void 	dttm_cnvt ( 	char	*dattm,
			int	*iyear,
			int	*imonth,
			int	*iday,
			int	*ihour,
			int	*iret );

int 	dttm_cmp ( 	struct date_time_info 	dttm1,
			struct date_time_info   dttm2 );

void 	dttm_cpy ( 	struct date_time_info 	*dttm1,
			struct date_time_info 	dttm2 );

int 	fosd_txtrd ( 	int	called_from );

void 	fosd_decode ( void );

void 	fosd_plot ( 	int	called_from );

void 	fosd_getdata (  struct datatype_list	*dt_info,
			srchinfo_t		*srch_info,
			int			called_from,
			char			*report,
			int			*iret );

void 	fosd_wbox ( int called_from );


int		idata_init( 			int		*iret );
G_Boolean	idata_getAutoUpdt(		void );

void		idata_setAutoUpdt(		G_Boolean	newValue,
						int		*iret );


void	map_draw ( char *map, char *garea, char *proj, char *latlon, 
		   char *panel, char *text, char *title,
	 	   int *ititl, int *linttl, char *shrttl, int *clear, 
		   int *iret, G_Cardinal, G_Cardinal, G_Cardinal, G_Cardinal, 
		   G_Cardinal, G_Cardinal, G_Cardinal, G_Cardinal );

void	map_mark ( int *nltln, float *rlat, float *rlon, int *ivalue,
		   int *ncolor, int *breaks, int *icolrs, int *mrktyp,
		   float *sizmrk, int *mrkwid, int *pltval, int *iposn,
		   int *iret);


int	mapw_fndStn(		float 		*xin,	
				float 		*yin );
int	mapw_getStation( 	int		idx );
void	mapw_pickStation(	float		xloc,
				float		yloc,
				int		*iret );
int	mapw_isSelected(	int		numsel,
				int		station );
void 	mapw_rmappstn ( 	void ); 
void 	mapw_rmselstn ( 	void );
void	mapw_setStation( 	int		idx,
				int		value,
				int		*iret );
void	mapw_updateMap(		int		mapIndex,
				int		*iret );

int	nwx_guiStart(		int		argc,
				char		*argv[],
				int		*iret );

void	nsfopn	(	char *datatype, 
			char		*filnam,
			int		*iflno,
			int		*iret,
			G_Cardinal,
			G_Cardinal );

void	err_showError	(	int		iret	);

int	nwxtbl_init ( 		void );

int	nwxtbl_sdtyp ( 		char  		*datatype );

void	nwxtbl_getstns ( 	int		inxdt,
			 	stnlist_t	*stns,
			 	int		*iret );

void	pdata_init(		void );

void	pdata_setReportText(	char		*text,
				int		*iret );

void	pdata_getReportText(	char		*report,
				int		*iret );

void	pdata_setDateTime(	char		*text,
				int		*iret );

void	pdata_getDateTime(	char		*date,
				int		*iret );

int	pdata_getTimeCovered(	void );

void	pdata_setTimeCovered(	int		newTimeCovered,
				int		*iret );


void	prf_decode (		char	    	*text,
				stnlist_t   	*stnlist,
				plotdata_t  	*plotdata );

void	qpf_decode (	char	   *text,
			plotdata_t *plotdata );

void	srchb_fosdGetrep ( struct datatype_list	*dtinfo,
			   srchinfo_t		*srchinfo,
			   char			*report,
			   int			*iret );

void	srchb_fosdGetnxt ( srchinfo_t	*srchinfo,
			   char		*report,
			   int		*iret );

void	srchb_repInit ( void );

void	srcho_fosdGetrep ( srchinfo_t   *srchinfo,
			   int		called_from,
			   char		*report,
			   int		*iret );

void	srcho_timChange ( srchinfo_t	*srchinfo,
			  int		minutes,
			  int		*iret );

void	srchw_fosdScan ( srchinfo_t	*srchinfo,
			 int 		called_from,
			 int		*iret );

void	srchw_fosdGetrep ( srchinfo_t	*srchinfo,
			   char		*report,
			   int		*iret );

void	sstruct_stxtmk ( int			idtype,
			 struct datatype_list	*dtypinfo,
			 int			*iret );



void	txtw_prdgrpSet ( 	void );

void	txtw_prnt(		char		*text,
				int		*iret );

void	txtw_dttmSet ( 		char		*filnme );

void	txtw_setPrevNext( 	G_Boolean	prevStatus,
			  	G_Boolean	nextStatus );

void	txtw_dsplyReport(	int		*iret );

void	txtw_dsplyDateTime(	int		*iret );

void	uvi_decode ( 	char       *text,
			stnlist_t  *stnlist,
			plotdata_t *plotdata );

void	wbox_decode (   char 	    *text,
			int	    nwatch,
			plotdata_t  *plotdata );

void	wwcrnr_calc (	float	*side,
			int	*iflag,
			char	locid1[],
			char	locid2[],
			float	dist[],
			float	bear[],
			float	rlat[],
			float	rlon[],
			int	*npt,
			int	*iret );

void	ww_dcod (	char *bultin,
			int *lenbul, int *itype, char *wnum, char *strtim,
			char *stptim, char *tissue, int *icorr, int *icancl, 
			float *rlat, float *rlon, int *npt, int *irepl, char *rnums, 
			int *iret, G_Cardinal, G_Cardinal, G_Cardinal, G_Cardinal, 
			G_Cardinal, G_Cardinal );

void	auto_startAutoUpdt ( void );
void	auto_stopAutoUpdt ( void );

/*
 *  gemplot wrapper functions
 */
void	wgem_er_wmsg(char *errgrp, int *numerr, char *errstr, int *iret );
void	wgem_gclear( int *iret );
void	wgem_geplot( int *iret );
void	wgem_ggtpnt( char *sys, int *ityp, float *x, float *y, int *iret );
void	wgem_gg_ltln( char *ltln, int *iret );
void	wgem_gg_map( char *map, int *iret );
void	wgem_gg_maps(char *proj, char *garea, char *imgfil, int *idrpfl, int *iret );
void	wgem_gg_panl(char *panl, int *iret );
void	wgem_gmark(  char *sys, int *np, float *x, float *y, int *iret ); 
void	wgem_gmesg(  char *messag, int *iret ); 
void	wgem_gg_wlbl(int *np, float *rlat, float *rlon, 
		     float *alat, float *alon, int *iret );
void	wgem_gg_wstr(char *string, int *line, int *iret );
void	wgem_gline(  char *sys, int *np, float *x, float *y, int *iret );
void	wgem_gqline( int *iltyp, int *ilthw, int *iwidth, int *iwhw, int *iret );
void	wgem_gsatim( char *filnam, int *iret ); 
void	wgem_gsmfil( char *mapfil, int *iret );
void	wgem_gqmprj( char *proj, float *angl1, float *angl2, float *angl3, 
		     float *dlatll, float *dlonll, float *dlatur, float *dlonur, 
		     int *iret );
void	wgem_gqtext( int *itxfn, int *itxhw, float *sztext, int *itxwid, 
		     int *brdr, int *irrotn, int *ijust, int *iret );
void	wgem_gscolr( int *icolr, int *iret );
void	wgem_gsline( int *iltyp, int *ilthw, int *iwidth, int *ilwh, int *iret );
void	wgem_gsmrkr( int *imark, int *imkhw, float *szmark, 
		     int *imkwid, int *iret );
void	wgem_gstext( int *itxfn, int *itxhw, float *sztext, int *itxwid, 
		     int *ibrdr, int *irrotn, int *ijust, int *iret );
void	wgem_gtext(  char *sys, float *x, float *y, char *cchar, float *rotat,
		     int *ixoff, int *iyoff, int *iret );
void	wgem_gtextc( char *sys, float *x, float *y, char *cchar, float *rotat,
		     int *ixoff, int *iyoff, int *iret );
void	wgem_gtrans( char *sysin, char *sysout, int *np, float *xin, float *yin, 
		     float *xout, float *yout, int *iret );
void	wgem_in_text(char *text, int *iret );
void	wgem_xxflsh( int *raise, int *iret );



#endif	/* PROTO_NWX */

