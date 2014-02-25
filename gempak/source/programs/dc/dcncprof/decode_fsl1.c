#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>

#include <netcdf.h>
#include "ulog.h"
#include "profiler.h"

#include "proto_func.h"

int flt_E(float a, float b)
{
if((fabs(a) - FLT_EPSILON <= fabs(b))&&
   (fabs(a) + FLT_EPSILON >= fabs(b)))
   return(1);
else
   return(0);
}

sta_struct *decode_fsl1(int cdfid, long miss, int *iret)
{
int ndims,nvars,natts,nunlim;
int tmpint[20];
int ier,i,j,k,unlimsiz,nlow_id,nhigh_id,nlow,nhigh;
int wmoStaNum_id,wmoStaNum,staName_id;
char staName[20];
int staLat_id,staLon_id,staElev_id,level_l_id,level_h_id;
int low_uwnd_id,low_vwnd_id,low_wwnd_id,low_qual_id,low_quals_id;
int high_uwnd_id,high_vwnd_id,high_wwnd_id,high_qual_id,high_quals_id;
int sfc_sped_id,sfc_drct_id,sfc_pres_id,sfc_temp_id,sfc_dwpc_id,sfc_rain_id;
int year_id,month_id,day_id,hour_id,minute_id;
float staLat,staLon,staElev,level;
float uwnd,vwnd,wwnd;
int qual,qual_sum;
float sfc_sped,sfc_drct,sfc_pres,sfc_temp,sfc_relh,sfc_rain,sfc_dwpc;
nc_type xtype;
int nvdims,nvatts;
size_t dimsiz,var_i[5],vc[5],namelen;
float fill,norec;
float fmiss,e,es;
int U[2],V[2],W[2],QUAL[2],QUALS[2],NLEV[2],LEVEL[2];

int year,month,day,hour,minute;
sta_struct *stadat,*head=NULL;
prof_data *plev,*plast;


udebug("decoding fsl1\0");
fmiss = (float)miss;

ier = nc_inq(cdfid,&ndims,&nvars,&natts,&nunlim);
ier += nc_inq_dimid(cdfid,"low_level",&nlow_id);
ier += nc_inq_dimlen(cdfid,nlow_id,&dimsiz);
nlow = dimsiz;
ier += nc_inq_dimid(cdfid,"high_level",&nhigh_id);
ier += nc_inq_dimlen(cdfid,nhigh_id,&dimsiz);
nhigh = dimsiz;

ier = nc_get_att_float(cdfid,NC_GLOBAL,"missing_value",&fill);
ier = nc_get_att_float(cdfid,NC_GLOBAL,"not_recorded",&norec);
udebug("ier %d fill value is %lf %lf nhigh %d nlow %d\0",ier,fill,norec,nhigh,nlow);

ier = 0;
ier += nc_inq_varid(cdfid,"id",&staName_id);
ier += nc_inq_varid(cdfid,"lat",&staLat_id);
ier += nc_inq_varid(cdfid,"lon",&staLon_id);
ier += nc_inq_varid(cdfid,"elev",&staElev_id);
ier += nc_inq_varid(cdfid,"low_level",&level_l_id);
ier += nc_inq_varid(cdfid,"low_u_wind",&low_uwnd_id);
ier += nc_inq_varid(cdfid,"low_v_wind",&low_vwnd_id);
ier += nc_inq_varid(cdfid,"low_w_wind",&low_wwnd_id);
ier += nc_inq_varid(cdfid,"low_qual_indicator",&low_qual_id);
ier += nc_inq_varid(cdfid,"low_qual_summary",&low_quals_id);
ier += nc_inq_varid(cdfid,"high_level",&level_h_id);
ier += nc_inq_varid(cdfid,"high_u_wind",&high_uwnd_id);
ier += nc_inq_varid(cdfid,"high_v_wind",&high_vwnd_id);
ier += nc_inq_varid(cdfid,"high_w_wind",&high_wwnd_id);
ier += nc_inq_varid(cdfid,"high_qual_indicator",&high_qual_id);
ier += nc_inq_varid(cdfid,"high_qual_summary",&high_quals_id);


ier += nc_inq_varid(cdfid,"sfc_spd",&sfc_sped_id);
ier += nc_inq_varid(cdfid,"sfc_dir",&sfc_drct_id);
ier += nc_inq_varid(cdfid,"sfc_p",&sfc_pres_id);
ier += nc_inq_varid(cdfid,"sfc_t",&sfc_temp_id);
ier += nc_inq_varid(cdfid,"sfc_td",&sfc_dwpc_id);
ier += nc_inq_varid(cdfid,"sfc_rain",&sfc_rain_id);
ier += nc_inq_varid(cdfid,"year",&year_id);
ier += nc_inq_varid(cdfid,"month",&month_id);
ier += nc_inq_varid(cdfid,"day",&day_id);
ier += nc_inq_varid(cdfid,"hour",&hour_id);
ier += nc_inq_varid(cdfid,"minute",&minute_id);

if(ier != 0)
   {
   uerror("could not get station information [%d]\0",ier);
   *iret = -1;
   return(NULL);
   }

tmpint[0] = 0;tmpint[1] = 0;
ier = nc_inq_vardimid(cdfid,staName_id,tmpint);
ier += nc_inq_dimlen(cdfid,tmpint[1],&namelen);
ier += nc_inq_dimlen(cdfid,tmpint[0],&dimsiz);

udebug("dimensions %d %d\0",dimsiz,namelen);

if(ier == 0)
   unlimsiz = dimsiz;
   for(i=0;i<unlimsiz;i++)
      {
      var_i[0] = i; var_i[1] = 0; vc[0] = 1; vc[1] = namelen-1;
      memset(staName,'\0',20);
      ier = nc_get_vara_text(cdfid,staName_id,var_i,vc,staName);
      wmoStaNum = 99999;
      ier += nc_get_var1_float(cdfid,staLat_id,var_i,&staLat);
      ier += nc_get_var1_float(cdfid,staLon_id,var_i,&staLon);
      staLon = -staLon;
      ier += nc_get_var1_float(cdfid,staElev_id,var_i,&staElev);
      ier += nc_get_var1_int(cdfid,year_id,var_i,&year);
      ier += nc_get_var1_int(cdfid,month_id,var_i,&month);
      ier += nc_get_var1_int(cdfid,day_id,var_i,&day);
      ier += nc_get_var1_int(cdfid,hour_id,var_i,&hour);
      ier += nc_get_var1_int(cdfid,minute_id,var_i,&minute);
      ier += nc_get_var1_float(cdfid,sfc_sped_id,var_i,&sfc_sped);
      ier += nc_get_var1_float(cdfid,sfc_drct_id,var_i,&sfc_drct);
      ier += nc_get_var1_float(cdfid,sfc_pres_id,var_i,&sfc_pres);
      ier += nc_get_var1_float(cdfid,sfc_temp_id,var_i,&sfc_temp);
      ier += nc_get_var1_float(cdfid,sfc_dwpc_id,var_i,&sfc_dwpc);
      ier += nc_get_var1_float(cdfid,sfc_rain_id,var_i,&sfc_rain);
      udebug("Station %3d %8d %s = %6.2f %7.2f %5.0f\0",i,wmoStaNum,
         staName,staLat,staLon,staElev);

      stadat = (sta_struct *)malloc(sizeof(sta_struct));
      if(stadat == NULL)
         {
         uerror("Could not allocate station data structure\0");
         exit(-2);
         }
      stadat->wmoStaNum = wmoStaNum;
      for(j=0;j<strlen(staName);j++) if(staName[j] == ' ') staName[j] = '\0';
      stadat->staName = (char *)malloc(strlen(staName)+1);
      strcpy(stadat->staName,staName);
      stadat->staLat = staLat;      
      stadat->staLon = staLon;      
      stadat->staElev = staElev;      
      stadat->timeObs = fmiss;      
      stadat->year = year;
      stadat->month = month;
      stadat->day = day;
      stadat->hour = hour;
      stadat->minute = minute;
      stadat->time_interval = 60;
      stadat->pdata = NULL;
      stadat->rdata = NULL;
      stadat->sfc_pres = fmiss;
      stadat->sfc_temp = fmiss;
      stadat->sfc_sped = fmiss;
      stadat->sfc_drct = fmiss;
      stadat->sfc_relh = fmiss;
      stadat->sfc_dwpc = fmiss;
      stadat->sfc_rain_rate = fmiss;
      stadat->sfc_rain_amt = fmiss;

      if((! flt_E(sfc_pres,fill))&& (! flt_E(sfc_pres,norec)))
	 stadat->sfc_pres = sfc_pres;

      if((! flt_E(sfc_temp,fill))&& (! flt_E(sfc_temp,norec)))
	 stadat->sfc_temp = sfc_temp;

      if((! flt_E(sfc_sped,fill))&& (! flt_E(sfc_sped,norec)))
         stadat->sfc_sped = sfc_sped;

      if((! flt_E(sfc_drct,fill))&& (! flt_E(sfc_drct,norec)))
         stadat->sfc_drct = sfc_drct;

      if((! flt_E(sfc_dwpc,fill))&& (! flt_E(sfc_dwpc,norec)))
         stadat->sfc_dwpc = sfc_dwpc;

      if((! flt_E(sfc_rain,fill))&& (! flt_E(sfc_rain,norec)))
         stadat->sfc_rain_amt = sfc_rain;

      if((stadat->sfc_temp != fmiss)&&(stadat->sfc_dwpc != fmiss))
         {
         VAPOR_PRES(stadat->sfc_temp+273.15,&es);
         VAPOR_PRES(stadat->sfc_dwpc+273.15,&e);
         stadat->sfc_relh = (e / es) * 100;
         }

      stadat->numlevs = 0;
      if(ier == 0)
         {
         plast = stadat->pdata;
         U[0] = low_uwnd_id; V[0] = low_vwnd_id; W[0] = low_wwnd_id;
         QUAL[0] = low_qual_id; QUALS[0] = low_quals_id;
         LEVEL[0] = level_l_id; NLEV[0] = nlow;
         U[1] = high_uwnd_id; V[1] = high_vwnd_id; W[1] = high_wwnd_id;
         QUAL[1] = high_qual_id; QUALS[1] = high_quals_id;
         LEVEL[1] = level_h_id; NLEV[1] = nhigh;
         for(k=0;k<2;k++)
            {
         for(j=0;j<NLEV[k];j++)
            {
            var_i[0] = j;
            ier = nc_get_var1_float(cdfid,LEVEL[k],var_i,&level);
            var_i[0] = i;
            var_i[1] = j;
            ier = nc_get_var1_float(cdfid,U[k],var_i,&uwnd);
            ier += nc_get_var1_float(cdfid,V[k],var_i,&vwnd);
            ier += nc_get_var1_float(cdfid,W[k],var_i,&wwnd);
            ier += nc_get_var1_float(cdfid,W[k],var_i,&wwnd);
            ier += nc_get_var1_int(cdfid,QUAL[k],var_i,&qual);
            ier += nc_get_var1_int(cdfid,QUALS[k],var_i,&qual_sum);
            plev = (prof_data *)malloc(sizeof(prof_data));
            if(plev != NULL)
               {
               plev->level = level;
	       if( flt_E(uwnd,fill) || flt_E(uwnd,norec) ) uwnd = fmiss;
	       if( flt_E(vwnd,fill) || flt_E(vwnd,norec) ) vwnd = fmiss;
	       if( flt_E(wwnd,fill) || flt_E(wwnd,norec) ) wwnd = fmiss;
               if(qual != 10) 
                  {
                  uwnd = fmiss;
                  vwnd = fmiss;
                  wwnd = fmiss;
                  }
               if(fabs(wwnd) > 1e36) wwnd = fmiss;
               plev->u = uwnd; plev->v = vwnd; plev->w = wwnd;
               plev->sigma_uv = fmiss;
               plev->sigma_w = fmiss;
               plev->levmode = k+1;
               plev->nextlev = NULL;
               if(plast == NULL)
                  stadat->pdata = plev;
               else
                  plast->nextlev = plev;
               plast = plev;
               }
            stadat->numlevs += 1; 
            }
         }
         }

      stadat->next = head;
      head = stadat;
      }

return(head);

} 
