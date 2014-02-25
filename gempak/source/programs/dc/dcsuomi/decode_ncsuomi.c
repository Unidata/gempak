#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <netcdf.h>
#include "math.h"
#include "ulog.h"
#include "suomi.h"

void *decode_ncsuomi(int cdfid, long miss, int *iret)
{
int ndims,nvars,natts,nunlim;
int ier,i,j;
int tmpint;

nc_type xtype;
size_t var_i[5],vc[5],namelen;


/**************************
** Dimension IDs/Lengths **
***************************/
int stdlen_id, stid_len;
int stdim_id, numstns;
int toffdim_id, numoffs;

/******************
** Variable IDs  **
*******************/
int station_id, timeoff_id, Lat_id, Lon_id, Alt_id, duration_id;
int pwv_id, pwverr_id, wetdel_id, drydel_id, totaldel_id;
int pifact_id, pres_id, tmpc_id, relh_id, dryfdel_id;
int metf_id, pifacttype_id;

/**************************
** Variable fill values  **
***************************/

/*float pwvfill;*/

/***********************
** Variable values    **
************************/
char	stname[20];

double	timeoff;
double	Lat,Lon,hght;

float 	tmpc, relh, pres;
float	durn, pwv, pwv_err;
float   wet_delay, model_dry_delay, total_delay;
float   pifact, final_dry_delay;
char	metf;


float fmiss;

time_t obs_time,ot2;
char *atttext;
struct tm *gmt_time=NULL,new_time;
suomi_struct *pdata,*head=NULL;
suomi_obs *odata;


fmiss = (float)miss;

ier = nc_inq(cdfid,&ndims,&nvars,&natts,&nunlim);
udebug("decoding ncsuomi [%d] %d %d %d %d\0",ier,ndims,nvars,natts,nunlim);

ier = nc_inq_atttype(cdfid,NC_GLOBAL,"start_time",&xtype);
if(xtype == NC_CHAR)
   {
   ier = nc_inq_attlen(cdfid,NC_GLOBAL,"start_time",&namelen);
   udebug("start_time len is %d",namelen);
   atttext = (char *)malloc(namelen + 1);
   ier = nc_get_att_text(cdfid,NC_GLOBAL,"start_time",atttext);
   atttext[namelen] = '\0';
   sscanf(atttext,"%d",&tmpint);
   udebug("starttime type is NC_CHAR %s VAL %d",atttext,tmpint);
   free(atttext);
   }
else
   ier = nc_get_att_int(cdfid,NC_GLOBAL,"start_time",&tmpint);

obs_time = (time_t)tmpint;

ier = nc_inq_attlen(cdfid,NC_GLOBAL,"start_date",&namelen);
atttext = (char *)malloc(namelen + 1);
ier = nc_get_att_text(cdfid,NC_GLOBAL,"start_date",atttext);
atttext[namelen] = '\0';
udebug("start_date is %s",atttext);
free(atttext);

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
ot2 = mktime(&new_time);

obs_time += ot2;



/* Find out the ids of variables */
ier = 0;
ier += nc_inq_varid(cdfid,"station",&station_id);
ier += nc_inq_varid(cdfid,"time_offset",&timeoff_id);
ier += nc_inq_varid(cdfid,"lat",&Lat_id);
ier += nc_inq_varid(cdfid,"lon",&Lon_id);
ier += nc_inq_varid(cdfid,"height",&Alt_id);

if(ier != 0)
   {
   uerror("could not get variable information %d\0",ier);
   *iret = -1;
   return(NULL);
   }

if ( ( ier = nc_inq_varid(cdfid,"duration",&duration_id) ) != 0 ) duration_id = -1;

if ( ( ier = nc_inq_varid(cdfid,"pwv",&pwv_id) ) != 0 ) pwv_id = -1;
if ( ( ier = nc_inq_varid(cdfid,"pwv_err",&pwverr_id) ) != 0 ) pwverr_id = -1;
if ( ( ier = nc_inq_varid(cdfid,"wet_delay",&wetdel_id) ) != 0 ) wetdel_id = -1;
if ( ( ier = nc_inq_varid(cdfid,"model_dry_delay",&drydel_id) ) != 0 ) drydel_id = -1;
if ( ( ier = nc_inq_varid(cdfid,"total_delay",&totaldel_id) ) != 0 ) totaldel_id = -1;
if ( ( ier = nc_inq_varid(cdfid,"pifact",&pifact_id) ) != 0 ) pifact_id = -1;
if ( ( ier = nc_inq_varid(cdfid,"pres",&pres_id) ) != 0 ) pres_id = -1;
if ( ( ier = nc_inq_varid(cdfid,"temperature",&tmpc_id) ) != 0 ) tmpc_id = -1;
if ( ( ier = nc_inq_varid(cdfid,"rh",&relh_id) ) != 0 ) relh_id = -1; 
if ( ( ier = nc_inq_varid(cdfid,"final_dry_delay",&dryfdel_id) ) != 0 ) dryfdel_id = -1; 
if ( ( ier = nc_inq_varid(cdfid,"met_flag",&metf_id) ) != 0 ) metf_id = -1; 
if ( ( ier = nc_inq_varid(cdfid,"pifact_type",&pifacttype_id) ) != 0 ) pifacttype_id = -1; 


/* Get station name information */
ier += nc_inq_dimid(cdfid,"station_name_length", &stdlen_id);
ier += nc_inq_dimlen(cdfid,stdlen_id,&namelen);

if(ier != 0)
   {
   uerror("could not get station name length information %d\0",ier);
   *iret = -1;
   return(NULL);
   }
else
   {
   stid_len = namelen;
   udebug("statid name length is %d\n",stid_len);
   }

/* get missing data values....sorry, right now, these are 
character strings in the CDL, not floating point numbers 
if(nc_get_att_float(cdfid,pwv_id,"missing_value",&pwvfill) != 0)
   {
   printf("error getting missing value %f\n",pwvfill);
   pwvfill = fmiss;
   }
*/

/* Get number of reporting stations */
ier += nc_inq_dimid(cdfid,"station", &stdim_id);
ier += nc_inq_dimlen(cdfid,stdim_id,&namelen);

if(ier != 0)
   {
   uerror("could not get station dimension information %d\0",ier);
   *iret = -1;
   return(NULL);
   }
else
   {
   numstns = namelen;
   udebug("stdim length is %d\n",numstns);
   }

/* Get number of reporting time offsets */
ier += nc_inq_dimid(cdfid,"time_offset", &toffdim_id);
ier += nc_inq_dimlen(cdfid,toffdim_id,&namelen);

if(ier != 0)
   {
   uerror("could not get station dimension information %d\0",ier);
   *iret = -1;
   return(NULL);
   }
else
   {
   numoffs = namelen;
   udebug("time_offset length is %d\n",numoffs);
   }


for(i=0;i<numstns;i++)
   {
   pdata = (suomi_struct *)malloc(sizeof(suomi_struct));
   if(pdata == NULL)
      {
      uerror("Could not allocate station data structure\0");
      exit(-2);
      }

   var_i[0] = i; var_i[1] = 0; vc[0] = 1; vc[1] = stid_len;
   memset(stname,'\0',sizeof(stname));
   ier = nc_get_vara_text(cdfid,station_id,var_i,vc,stname);
   udebug("look stationid %d %d %s\0",i,ier,stname);
   pdata->stid = (char *)malloc(strlen(stname)+1);
   strcpy(pdata->stid,stname);

   ier += nc_get_var1_double(cdfid,Lat_id,var_i,&Lat);
   ier += nc_get_var1_double(cdfid,Lon_id,var_i,&Lon);
   ier += nc_get_var1_double(cdfid,Alt_id,var_i,&hght);
   if(Lat != -999)
      pdata->Lat = Lat;
   else
      pdata->Lat = fmiss;
   if(Lon != -999)
      pdata->Lon = Lon;
   else
      pdata->Lon = fmiss;
   if(hght != -999)
      pdata->Alt = hght;
   else
      pdata->Alt = fmiss;

   pdata->head = NULL;

   for(j=0;j<numoffs;j++)
      {
      /* initialize some missing entries */
      pres = tmpc = relh = durn = pwv = pwv_err = wet_delay = model_dry_delay = total_delay = pifact = final_dry_delay = -999.0;
      metf = 'X';

      vc[0] = j; vc[1] = 0;
      ier += nc_get_var1_double(cdfid,timeoff_id,vc,&timeoff);

      var_i[1] = j;
      if ( pres_id != -1 ) ier += nc_get_var1_float(cdfid,pres_id,var_i,&pres);
      if ( tmpc_id != -1 ) ier += nc_get_var1_float(cdfid,tmpc_id,var_i,&tmpc);
      if ( relh_id != -1 ) ier += nc_get_var1_float(cdfid,relh_id,var_i,&relh);

      if ( duration_id != -1 ) ier += nc_get_var1_float(cdfid,duration_id,var_i,&durn);
      if ( pwv_id != -1 ) ier += nc_get_var1_float(cdfid,pwv_id,var_i,&pwv);
      if ( pwverr_id != -1 ) ier += nc_get_var1_float(cdfid,pwverr_id,var_i,&pwv_err);

      if ( wetdel_id != -1 ) ier += nc_get_var1_float(cdfid,wetdel_id,var_i,&wet_delay);
      if ( drydel_id != -1 ) ier += nc_get_var1_float(cdfid,drydel_id,var_i,&model_dry_delay);
      if ( totaldel_id != -1 ) ier += nc_get_var1_float(cdfid,totaldel_id,var_i,&total_delay);

      if ( pifact_id != -1 ) ier += nc_get_var1_float(cdfid,pifact_id,var_i,&pifact);
      if ( dryfdel_id != -1 ) ier += nc_get_var1_float(cdfid,dryfdel_id,var_i,&final_dry_delay);

      if ( metf_id != -1 ) ier += nc_get_var1_text(cdfid,metf_id,var_i,&metf);

      ot2 = obs_time + timeoff;
      gmt_time = gmtime(&ot2);
      new_time = *gmt_time; /* copy the time since gmtime pointer is volotile */

      odata = (suomi_obs *)malloc(sizeof(suomi_obs));

      odata->seconds = new_time.tm_sec;
      odata->minute = new_time.tm_min;
      odata->hour = new_time.tm_hour;
      odata->day = new_time.tm_mday;
      odata->month = new_time.tm_mon + 1;
      odata->year = 1900 + new_time.tm_year;

      odata->timeObs = ot2;

      if(pres != -999)
         odata->pres = pres;
      else
         odata->pres = fmiss;

      if(tmpc != -999)
         odata->tmpc = tmpc;
      else
         odata->tmpc = fmiss;

      if(relh != -999)
         odata->relh = relh;
      else
         odata->relh = fmiss;

      if(durn != -999)
         odata->duration = durn;
      else
         odata->duration = fmiss;

      if(pwv != -999)
         odata->pwv = pwv;
      else
         odata->pwv = fmiss;

      if(pwv_err != -999)
         odata->pwv_err = pwv_err;
      else
         odata->pwv_err = fmiss;

      if(wet_delay != -999)
         odata->wet_delay = wet_delay;
      else
         odata->wet_delay = fmiss;

      if(model_dry_delay != -999)
         odata->model_dry_delay = model_dry_delay;
      else
         odata->model_dry_delay = fmiss;

      if(total_delay != -999)
         odata->total_delay = total_delay;
      else
         odata->total_delay = fmiss;

      if(pifact != -999)
         odata->pifact = pifact;
      else
         odata->pifact = fmiss;

      if(final_dry_delay != -999)
         odata->final_dry_delay = final_dry_delay;
      else
         odata->final_dry_delay = fmiss;

      odata->met_flag = (int)metf;

      odata->next = pdata->head;
      pdata->head = odata;
      }

   pdata->next = head;
   head = pdata;
   }


/*
pdata = head;
while(pdata != NULL)
   {
   printf("look station %s %f %f %f\n",pdata->stid,
      pdata->Lat,pdata->Lon, pdata->Alt);
   odata = pdata->head;
   while(odata != NULL)
      {
      ot2 = odata->timeObs;
      printf("    time %d %d %d %d\n",odata->year,odata->month,
		odata->day,odata->hour);
      printf("   %f %f %f\n",odata->pres,odata->tmpc,odata->relh);
      printf("   %f %f %f\n",odata->duration, odata->pwv, odata->pwv_err);
      printf("   %f %f %f\n",odata->wet_delay, odata->model_dry_delay, odata->total_delay);
      printf("   %f %f\n",odata->pifact, odata->final_dry_delay);
      odata = odata->next;
      }
   pdata = pdata->next;
   } */


return(head);

} 
