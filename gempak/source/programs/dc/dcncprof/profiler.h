/* Rrofiler/RASS data structure for FSL NetCDF files */

#ifndef __profiler__
#define __profiler__


typedef struct prof_data {
   float level;
   float u,v,w;
   float sigma_uv,sigma_w;
   int levmode;
   struct prof_data *nextlev;
   } prof_data;

typedef struct rass_data {
   float level;
   float consensus, power, tmpv, specw;
   unsigned char	submode;
   struct rass_data *nextlev;
   } rass_data;

typedef struct sta_struct {
   int wmoStaNum;
   char *staName;
   float staLat,staLon,staElev;
   double timeObs;
   int year,month,day,hour,minute;
   int numlevs;
   int time_interval;
   struct prof_data *pdata;
   struct rass_data *rdata;
   struct sta_struct *next;
   float sfc_sped,sfc_drct,sfc_temp,sfc_relh,sfc_dwpc,sfc_pres;
   float sfc_rain_rate,sfc_rain_amt;
   } sta_struct;

#endif /* __profiler__ */
