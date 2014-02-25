#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <netcdf.h>
#include "math.h"
#include "ulog.h"
#include "cosmic.h"

cosmic_struct *decode_nccosmic(int cdfid, long miss, int *iret)
{
int ndims,nvars,natts,nunlim, dimid;
int ier,i;

nc_type xtype;
size_t dimsiz,var_i[5],namelen, attlen;


/******************
** Variable IDs  **
*******************/
int Lat_id, Lon_id, Alt_id, press_id;
int tmpc_id, vap_id, ref_id, refo_id;

/***********************
** Variable fill IDs  **
************************/
float lat_fill, lon_fill, vfill, tfill, zfill, pfill, reffill, refofill;

/***********************
** Variable values    **
************************/
double	timeObs;
float	Lat, Lon;
float	tmpc,vapr,hght,press, refa, refo;

float fmiss;

time_t obs_time, toff;
char timestr[80],*atttext;
int year, month,day, hour, minute, seconds;
float fseconds, start_time, stop_time;
struct tm *gmt_time=NULL, new_time;
cosmic_struct *pdata,*head=NULL;


udebug("decoding nccosmic\0");
fmiss = (float)miss;

ier = nc_inq(cdfid,&ndims,&nvars,&natts,&nunlim);

ier = nc_inq_atttype(cdfid,NC_GLOBAL,"start_time",&xtype);
if(xtype == NC_CHAR) {
   /*
   ier = nc_inq_attlen(cdfid,NC_GLOBAL,"version",&namelen);
   udebug("version name len is %d",namelen);
   atttext = (char *)malloc(namelen + 1);
   ier = nc_get_att_text(cdfid,NC_GLOBAL,"version",atttext);
   atttext[namelen] = '\0';
   sscanf(atttext,"%f",tmpflt);
   udebug("version type is NC_CHAR %s VAL %f",atttext,tmpflt[0]);
   free(atttext);
   */
   uerror("shouldn't get here\0");
   return(NULL);
   }
else {
   ier = nc_get_att_float(cdfid,NC_GLOBAL,"start_time",&start_time);
   udebug("start_time %f\0",start_time);
   ier += nc_get_att_float(cdfid,NC_GLOBAL,"stop_time",&stop_time);
   udebug("stop_time %f\0",stop_time);
   ier += nc_get_att_int(cdfid,NC_GLOBAL,"year",&year);
   udebug("year %d\0",year);
   ier += nc_get_att_int(cdfid,NC_GLOBAL,"month",&month);
   udebug("month %d\0",month);
   ier += nc_get_att_int(cdfid,NC_GLOBAL,"day",&day);
   udebug("day %d\0",day);
   ier += nc_get_att_int(cdfid,NC_GLOBAL,"hour",&hour);
   udebug("hour %d\0",hour);
   ier += nc_get_att_int(cdfid,NC_GLOBAL,"minute",&minute);
   udebug("minute %d\0",minute);
   ier += nc_get_att_float(cdfid,NC_GLOBAL,"second",&fseconds);
   udebug("fseconds %f\0",fseconds);

   if ( ier != 0 ) {
      uerror("couldn't get time\0");
      return(NULL);
      }
   else {
	/* lets get the number of seconds to Jan 6, 00Z 1980 (UTC) */
      new_time.tm_sec = 0;
      new_time.tm_min = 0;
      new_time.tm_hour = 0;
      new_time.tm_mday = 6;
      new_time.tm_mon = 0;
      new_time.tm_year = 80;
      new_time.tm_wday = 0;
      new_time.tm_yday = 6;
      new_time.tm_isdst = -1;
      toff = mktime(&new_time);

      /* get time from seconds since 1970 */
      seconds = (int)fseconds;
      obs_time = (time_t) start_time + toff;
      gmt_time = gmtime(&obs_time);
      new_time = *gmt_time; /* copy the time since gmtime pointer is volotile */
      timestr[0] = '\0';
      strftime(timestr,80,"%Y %m %d %H %M %S",&new_time);
      sscanf(timestr,"%d %d %d %d %d %d",&year,&month,&day,&hour,&minute,&seconds);
      udebug("check time string %s\0",timestr);
      }
   }

/* Find out the ids of variables */
ier = 0;
ier += nc_inq_varid(cdfid,"Lat",&Lat_id);
ier += nc_inq_varid(cdfid,"Lon",&Lon_id);
ier += nc_inq_varid(cdfid,"MSL_alt",&Alt_id);
ier += nc_inq_varid(cdfid,"Pres",&press_id);

ier += nc_inq_varid(cdfid,"Temp",&tmpc_id);
ier += nc_inq_varid(cdfid,"Vp",&vap_id);

ier += nc_inq_varid(cdfid,"Ref",&ref_id);
ier += nc_inq_varid(cdfid,"Ref_obs",&refo_id);

ier += nc_get_att_float(cdfid,Lat_id,"_FillValue",&lat_fill);
ier += nc_get_att_float(cdfid,Lon_id,"_FillValue",&lon_fill);

ier += nc_get_att_float(cdfid,Alt_id,"_FillValue",&zfill);
ier += nc_get_att_float(cdfid,press_id,"_FillValue",&pfill);
ier += nc_get_att_float(cdfid,tmpc_id,"_FillValue",&tfill);
ier += nc_get_att_float(cdfid,vap_id,"_FillValue",&vfill);
ier += nc_get_att_float(cdfid,ref_id,"_FillValue",&reffill);
ier += nc_get_att_float(cdfid,refo_id,"_FillValue",&refofill);

if(ier != 0)
   {
   uerror("could not get variable information\0");
   *iret = -1;
   return(NULL);
   }

ier = nc_inq_dimid(cdfid, "MSL_alt", &dimid);
ier = nc_inq_dimlen(cdfid, dimid, &dimsiz);

for(i=0;i<dimsiz;i++)
   {
   var_i[0] = i; var_i[1] = 0; 

   ier = nc_get_var1_float(cdfid,Lat_id,var_i,&Lat);
   ier += nc_get_var1_float(cdfid,Lon_id,var_i,&Lon);

   if ( ier != 0 || Lat == lat_fill || Lon == lon_fill ) continue;

   ier = nc_get_var1_float(cdfid,Alt_id,var_i,&hght);
   ier += nc_get_var1_float(cdfid,press_id,var_i,&press);
   ier += nc_get_var1_float(cdfid,tmpc_id,var_i,&tmpc);
   ier += nc_get_var1_float(cdfid,vap_id,var_i,&vapr);
   ier += nc_get_var1_float(cdfid,ref_id,var_i,&refa);
   ier += nc_get_var1_float(cdfid,refo_id,var_i,&refo);

   if ( ier != 0 || ( press == pfill && tmpc == tfill && vapr == vfill && 
	refa == reffill && refo == refofill ) ) continue;

   udebug("vars %d  %f %f %f %f %f %f\0", i,
	Lat, Lon, hght, press, tmpc, vapr);


   pdata = (cosmic_struct *)malloc(sizeof(cosmic_struct));
   if(pdata == NULL)
      {
      uerror("Could not allocate station data structure\0");
      exit(2);
      }

   pdata->Lat = Lat;
   pdata->Lon = Lon;
   pdata->timeObs = (double)obs_time;
   pdata->year = year;
   pdata->month = month;
   pdata->day = day;
   pdata->hour = hour;
   pdata->minute = minute;
   pdata->seconds = seconds;
   pdata->Alt = fmiss;
   pdata->press = fmiss;
   pdata->tmpc = fmiss;
   pdata->dwpc = fmiss;
   pdata->refa = fmiss;
   pdata->refo = fmiss;

   if(press != pfill) pdata->press = press;
   if(hght != zfill) pdata->Alt = hght * 1000;
   if(tmpc != tfill) pdata->tmpc = tmpc;
   if(refa != reffill) pdata->refa = refa;
   if(refo != refofill) pdata->refo = refo;

   /* see if we can get dewpoint from vapor pressure */
   if(vapr != vfill) {
      t_from_e(vapr, &pdata->dwpc);
      pdata->dwpc = pdata->dwpc - 273.15;
      }

   pdata->next = head;
   head = pdata;
   }

return(head);



} 
