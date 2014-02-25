#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <netcdf.h>
#include "ulog.h"
#include "profiler.h"
#include "proto_func.h"

sta_struct *decode_fsl2(int cdfid, long miss, int *iret)
{
int ndims,nvars,natts,nunlim;
int tmpint[20];
int ier,i,j,unlimsiz;
int wmoStaNum_id,wmoStaNum,staName_id;
char staName[20];
int staLat_id,staLon_id,staElev_id,timeObs_id,levels_id;
int uwnd_id,vwnd_id,wwnd_id,uv_qual_id,w_qual_id,levelMode_id;
int sigma_uv_id,sigma_w_id;
int sfc_sped_id,sfc_drct_id,sfc_pres_id,sfc_temp_id,sfc_relh_id,sfc_rain_id;
float staLat,staLon,staElev,level;
float uwnd,vwnd,wwnd,sigma_uv,sigma_w;
int uv_qual,w_qual,levelMode;
float sfc_sped,sfc_drct,sfc_pres,sfc_temp,sfc_relh,sfc_rain;
double timeObs;
nc_type xtype;
int nvdims,nvatts,time_interval;
size_t dimsiz,var_i[5],vc[5],namelen;
float ufill,vfill,wfill;
float pfill,tfill,dfill,sfill,rfill,rrfill;
float fmiss,e;

time_t obs_time;
char timestr[80],*atttext;
int year,month,day,hour,minute;
struct tm *gmt_time=NULL,new_time;
sta_struct *stadat,*head=NULL;
prof_data *plev,*plast;


udebug("decoding fsl2\0");
fmiss = (float)miss;

ier = nc_inq(cdfid,&ndims,&nvars,&natts,&nunlim);

ier = nc_inq_atttype(cdfid,NC_GLOBAL,"avgTimePeriod",&xtype);
if(xtype == NC_CHAR)
   {
   ier = nc_inq_attlen(cdfid,NC_GLOBAL,"avgTimePeriod",&namelen);
   udebug("AvgTimPeriod name len is %d",namelen);
   atttext = (char *)malloc(namelen + 1);
   ier = nc_get_att_text(cdfid,NC_GLOBAL,"avgTimePeriod",atttext);
   sscanf(atttext,"%d",tmpint);
   udebug("AvgTimPeriod type is NC_CHAR %s VAL %d",atttext,tmpint[0]);
   free(atttext);
   }
else
   {
   ier = nc_get_att_int(cdfid,NC_GLOBAL,"avgTimePeriod",tmpint);
   }
udebug("AvgTimPeriod is %d\0",tmpint[0]);
time_interval = tmpint[0];

ier = 0;
ier += nc_inq_varid(cdfid,"wmoStaNum",&wmoStaNum_id);
ier += nc_inq_varid(cdfid,"staName",&staName_id);
ier += nc_inq_varid(cdfid,"staLat",&staLat_id);
ier += nc_inq_varid(cdfid,"staLon",&staLon_id);
ier += nc_inq_varid(cdfid,"staElev",&staElev_id);
ier += nc_inq_varid(cdfid,"timeObs",&timeObs_id);
ier += nc_inq_varid(cdfid,"levels",&levels_id);
ier += nc_inq_varid(cdfid,"uComponent",&uwnd_id);
ier += nc_inq_varid(cdfid,"vComponent",&vwnd_id);
ier += nc_inq_varid(cdfid,"wComponent",&wwnd_id);
ier += nc_get_att_float(cdfid,uwnd_id,"_FillValue",&ufill);
ier += nc_get_att_float(cdfid,vwnd_id,"_FillValue",&vfill);
ier += nc_get_att_float(cdfid,wwnd_id,"_FillValue",&wfill);

ier += nc_inq_varid(cdfid,"uvQualityCode",&uv_qual_id);
ier += nc_inq_varid(cdfid,"wQualityCode",&w_qual_id);
ier += nc_inq_varid(cdfid,"windSpeedStdDev",&sigma_uv_id);
ier += nc_inq_varid(cdfid,"wStdDev",&sigma_w_id);
ier += nc_inq_varid(cdfid,"levelMode",&levelMode_id);

ier += nc_inq_varid(cdfid,"windSpeedSfc",&sfc_sped_id);
ier += nc_inq_varid(cdfid,"windDirSfc",&sfc_drct_id);
ier += nc_inq_varid(cdfid,"pressure",&sfc_pres_id);
ier += nc_inq_varid(cdfid,"temperature",&sfc_temp_id);
ier += nc_inq_varid(cdfid,"relHumidity",&sfc_relh_id);
ier += nc_inq_varid(cdfid,"rainRate",&sfc_rain_id);
ier += nc_get_att_float(cdfid,sfc_sped_id,"_FillValue",&sfill);
ier += nc_get_att_float(cdfid,sfc_drct_id,"_FillValue",&dfill);
ier += nc_get_att_float(cdfid,sfc_pres_id,"_FillValue",&pfill);
ier += nc_get_att_float(cdfid,sfc_temp_id,"_FillValue",&tfill);
ier += nc_get_att_float(cdfid,sfc_relh_id,"_FillValue",&rfill);
ier += nc_get_att_float(cdfid,sfc_rain_id,"_FillValue",&rrfill);

if(ier != 0)
   {
   uerror("could not get station information\0");
   *iret = -1;
   return(NULL);
   }

ier = nc_inq_vardimid(cdfid,staName_id,tmpint);
ier += nc_inq_dimlen(cdfid,tmpint[1],&namelen);

tmpint[0] = 0;tmpint[1] = 0;
ier += nc_inq_var(cdfid,wmoStaNum_id,NULL, &xtype, &nvdims, tmpint, &nvatts);
ier += nc_inq_dimlen(cdfid,tmpint[0],&dimsiz);
if(ier == 0)
   unlimsiz = dimsiz;
   for(i=0;i<unlimsiz;i++)
      {
      var_i[0] = i; var_i[1] = 0; vc[0] = 1; vc[1] = namelen-1;
      memset(staName,'\0',20);
      ier = nc_get_vara_text(cdfid,staName_id,var_i,vc,staName);
      ier = nc_get_var1_int(cdfid,wmoStaNum_id,var_i,&wmoStaNum);
      ier = nc_get_var1_float(cdfid,staLat_id,var_i,&staLat);
      ier = nc_get_var1_float(cdfid,staLon_id,var_i,&staLon);
      ier = nc_get_var1_float(cdfid,staElev_id,var_i,&staElev);
      ier = nc_get_var1_float(cdfid,sfc_sped_id,var_i,&sfc_sped);
      ier = nc_get_var1_float(cdfid,sfc_drct_id,var_i,&sfc_drct);
      ier = nc_get_var1_float(cdfid,sfc_pres_id,var_i,&sfc_pres);
      ier = nc_get_var1_float(cdfid,sfc_temp_id,var_i,&sfc_temp);
      ier = nc_get_var1_float(cdfid,sfc_relh_id,var_i,&sfc_relh);
      ier = nc_get_var1_float(cdfid,sfc_rain_id,var_i,&sfc_rain);
      ier = nc_get_var1_double(cdfid,timeObs_id,var_i,&timeObs);
      obs_time = (time_t) timeObs;
      gmt_time = gmtime(&obs_time);
      new_time = *gmt_time;
      timestr[0] = '\0';
      strftime(timestr,80,"%Y %m %d %H %M",&new_time);
      sscanf(timestr,"%d %d %d %d %d",&year,&month,&day,&hour,&minute);
      udebug("Station %3d %8d %s = %6.2f %7.2f %5.0f %s\0",i,wmoStaNum,
         staName,staLat,staLon,staElev,timestr);

      stadat = (sta_struct *)malloc(sizeof(sta_struct));
      if(stadat == NULL)
         {
         uerror("Could not allocate station data structure\0");
         exit(-2);
         }
      stadat->wmoStaNum = wmoStaNum;
      stadat->staName = (char *)malloc(strlen(staName)+1);
      strcpy(stadat->staName,staName);
      stadat->staLat = staLat;      
      stadat->staLon = staLon;      
      stadat->staElev = staElev;      
      stadat->timeObs = timeObs;      
      stadat->year = year;
      stadat->month = month;
      stadat->day = day;
      stadat->hour = hour;
      stadat->minute = minute;
      stadat->time_interval = time_interval;
      stadat->pdata = NULL;
      stadat->rdata = NULL;
      stadat->sfc_pres = fmiss;
      stadat->sfc_temp = fmiss;
      stadat->sfc_sped = fmiss;
      stadat->sfc_drct = fmiss;
      stadat->sfc_relh = fmiss;
      stadat->sfc_rain_rate = fmiss;
      stadat->sfc_rain_amt = fmiss;
      stadat->sfc_dwpc = fmiss;
      if(sfc_pres != pfill) stadat->sfc_pres = sfc_pres;
      if(sfc_temp != tfill) stadat->sfc_temp = sfc_temp - 273.15;
      if(sfc_sped != sfill) stadat->sfc_sped = sfc_sped;
      if(sfc_drct != dfill) stadat->sfc_drct = sfc_drct;
      if(sfc_relh != rfill) stadat->sfc_relh = sfc_relh;
      if(sfc_rain != rrfill) stadat->sfc_rain_rate = sfc_rain;
      if((stadat->sfc_temp != fmiss)&&(stadat->sfc_relh != fmiss))
         {
         VAPOR_PRES(stadat->sfc_temp+273.15,&e);
         e = e * (stadat->sfc_relh / 100.);
         t_from_e(e,&stadat->sfc_dwpc);
         stadat->sfc_dwpc = stadat->sfc_dwpc - 273.15;
         }

      ier = nc_inq_var(cdfid,levels_id,NULL, &xtype, &nvdims, tmpint, &nvatts);
      if(ier == 0)
         {
         ier = nc_inq_dimlen(cdfid,tmpint[0],&dimsiz);
         stadat->numlevs = dimsiz;
         plast = stadat->pdata;
         for(j=0;j<stadat->numlevs;j++)
            {
            var_i[0] = j;
            ier = nc_get_var1_float(cdfid,levels_id,var_i,&level);
            ier = nc_get_var1_int(cdfid,levelMode_id,var_i,&levelMode);
            var_i[0] = i;
            var_i[1] = j;
            ier = nc_get_var1_float(cdfid,uwnd_id,var_i,&uwnd);
            ier = nc_get_var1_float(cdfid,vwnd_id,var_i,&vwnd);
            ier = nc_get_var1_float(cdfid,wwnd_id,var_i,&wwnd);
            ier = nc_get_var1_int(cdfid,uv_qual_id,var_i,&uv_qual);
            ier = nc_get_var1_int(cdfid,w_qual_id,var_i,&w_qual);
            ier = nc_get_var1_float(cdfid,sigma_uv_id,var_i,&sigma_uv);
            ier = nc_get_var1_float(cdfid,sigma_w_id,var_i,&sigma_w);
            plev = (prof_data *)malloc(sizeof(prof_data));
            if(plev != NULL)
               {
               plev->level = level;
               if(uwnd == ufill) uwnd = fmiss;
               if(vwnd == vfill) vwnd = fmiss;
               if(wwnd == wfill) wwnd = fmiss;
               if(uv_qual != 0) 
                  {
                  uwnd = fmiss;
                  vwnd = fmiss;
                  }
               if(w_qual != 0) wwnd = fmiss;
               if((uwnd == fmiss)||(vwnd == fmiss))
                  sigma_uv = fmiss;
               if(wwnd == fmiss) sigma_w = fmiss;
               plev->u = uwnd; plev->v = vwnd; plev->w = wwnd;
               plev->sigma_uv = sigma_uv;
               plev->sigma_w = sigma_w;
               plev->levmode = levelMode;
               plev->nextlev = NULL;
               if(plast == NULL)
                  stadat->pdata = plev;
               else
                  plast->nextlev = plev;
               plast = plev;
               }
            }
         }
      else
         stadat->numlevs = 0;

      stadat->next = head;
      head = stadat;
      }

return(head);

} 
