extern int     kstart_v72,kend_v72,kenda_v72,keyerM_v72,keyerA_v72,kres_v72,arfd_v72;
extern char    eyetype_v72[4][20],cloudtype_v72[6][20],cbasin_v72[2][9],cmon_v72[12][4];
extern float   tno_v72[83],pres_v72[2][83],wind_v72[83],ebd_v72[11];

extern logical odt_v72,olist_v72,oautomode_v72,override_v72;
extern logical ostartstr_v72,oland_v72,osearch_v72;
extern int     idomain_v72,ixdomain_v72,ifixtype_v72,rmwsizeman_v72;
extern float   osstr_v72;
extern float   spiralband_v72[2][37];
extern float   *fcstlat_v72,*fcstlon_v72;
extern double  *fcsttime_v72,starttime_v72,endtime_v72;

extern char    *hfile_v72,*fixfile_v72;
extern char    *diagnostics_v72;

extern struct  odtdata *odthistoryfirst_v72;
extern struct  odtdata *odtcurrent_v72;
extern struct  ringdata *tcircfirst_v72;
extern struct  datagrid *areadata_v72;
