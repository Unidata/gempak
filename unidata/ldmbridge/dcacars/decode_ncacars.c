#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <netcdf.h>
#include "math.h"
#include "ulog.h"
#include "acars.h"

acars_struct *decode_ncacars(int cdfid, long miss, int *iret)
{
int ndims,nvars,natts,nunlim;
int tmpint[20];
float tmpflt[20];
int ier,i,unlimsiz=0;
int ierr;

nc_type xtype;
size_t dimsiz,var_i[5],vc[5],namelen, attlen;

float version;

int tail_id,tail_len;
int aid_len,rpt_len;

/******************
** Variable IDs  **
*******************/
int Lat_id,Lon_id,Alt_id,timeObs_id;
int sped_id,drct_id,tmpk_id,mixr_id,dwpc_id,relh_id,vacc_id,press_id;
int cwmixr_id, drelh_id, s1rh_id, s2rh_id;
int orig_id,dest_id,rpt_id,data_id;
int terr_id,wderr_id,wserr_id,errtyp_id,corr_id,wvqc_id;
int medturb_id,maxturb_id,mach_id,heading_id;

/***********************
** Variable fill IDs  **
************************/
float sfill,dfill,tfill,mrfill,tdfill,rhfill,obsfill,zfill,vafill;
float cwmrfill, drhfill, s1rhfill, s2rhfill;
float headingfill,machfill,maxturbfill,medturbfill;

/***********************
** Variable values    **
************************/
char	tail_number[20],rptsta[20],orig[20],dest[20];
double	timeObs;
float	Lat,Lon;
float	sped,drct,tmpk,relh,s1rh,s2rh,mixr,dwpc,hght,vacc,press;
float	cwmixr, drelh;
int	valid,terr,wderr,wserr,errtyp,corr,wvqc;
float	heading, mach, medturb, maxturb;

float fmiss,e,ftmp;

time_t obs_time;
char timestr[80],*atttext;
int year,month,day,hour,minute,seconds;
struct tm *gmt_time=NULL,new_time;
acars_struct *pdata,*head=NULL;


udebug("decoding ncacars\0");
fmiss = (float)miss;

ier = nc_inq(cdfid,&ndims,&nvars,&natts,&nunlim);

ier = nc_inq_atttype(cdfid,NC_GLOBAL,"version",&xtype);
if(xtype == NC_CHAR)
   {
   ier = nc_inq_attlen(cdfid,NC_GLOBAL,"version",&namelen);
   udebug("version name len is %d",namelen);
   atttext = (char *)malloc(namelen + 1);
   ier = nc_get_att_text(cdfid,NC_GLOBAL,"version",atttext);
   atttext[namelen] = '\0';
   sscanf(atttext,"%f",tmpflt);
   udebug("version type is NC_CHAR %s VAL %f",atttext,tmpflt[0]);
   free(atttext);
   }
else
   {
   ier = nc_get_att_float(cdfid,NC_GLOBAL,"version",tmpflt);
   }
version = tmpflt[0];
udebug("CDF file version is %f\0",version);

ier = nc_inq_attlen(cdfid,NC_GLOBAL,"title",&namelen);
if(ier == 0)
   {
   atttext = (char *)malloc(namelen + 1);
   ier = nc_get_att_text(cdfid,NC_GLOBAL,"title",atttext);
   uinfo(atttext);
   free(atttext);
   }

/* Find out the ids of variables */
ier = 0;
ier += nc_inq_varid(cdfid,"dataDescriptor",&data_id);
ier += nc_inq_varid(cdfid,"latitude",&Lat_id);
ier += nc_inq_varid(cdfid,"longitude",&Lon_id);
ier += nc_inq_varid(cdfid,"altitude",&Alt_id);
ier += nc_inq_varid(cdfid,"timeObs",&timeObs_id);

ier += nc_inq_varid(cdfid,"windSpeed",&sped_id);
ier += nc_inq_varid(cdfid,"windDir",&drct_id);
ier += nc_inq_varid(cdfid,"temperature",&tmpk_id);
ier += nc_inq_varid(cdfid,"waterVaporMR",&mixr_id);
ier += nc_inq_varid(cdfid,"dewpoint",&dwpc_id);
ier += nc_inq_varid(cdfid,"vertAccel",&vacc_id);
ier += nc_inq_varid(cdfid,"tempError",&terr_id);
ier += nc_inq_varid(cdfid,"windDirError",&wderr_id);
ier += nc_inq_varid(cdfid,"windSpeedError",&wserr_id); 
ier += nc_inq_varid(cdfid,"errorType",&errtyp_id); 
ier += nc_inq_varid(cdfid,"correctedFlag",&corr_id); 
ier += nc_inq_varid(cdfid,"waterVaporQC",&wvqc_id); 
ier += nc_inq_varid(cdfid,"medTurbulence",&medturb_id);
ier += nc_inq_varid(cdfid,"maxTurbulence",&maxturb_id);
ier += nc_inq_varid(cdfid,"mach",&mach_id);
ier += nc_inq_varid(cdfid,"heading",&heading_id);
ierr = nc_inq_varid(cdfid,"pressure",&press_id);
if(ierr != 0)
   {
   uinfo("No pressure variable, will calculate\0");
   press_id = -1;
   }

ierr = nc_inq_varid(cdfid,"downlinkedRH",&drelh_id);
if(ierr != 0)
   {
   uinfo("downlinkedRH not present\0");
   drelh_id = -1;
   }

ierr = nc_inq_varid(cdfid,"correctedWVMR",&cwmixr_id);
if(ierr != 0)
   {
   uinfo("correctedWVMR not present\0");
   cwmixr_id = -1;
   }

/* CDL changed for TAMDAR RH version 1.7 */
ierr = nc_inq_varid(cdfid,"rh_probe",&relh_id);
if ( ierr != 0 )
   {
   relh_id = -1;
   udebug("rh_probe not present, will check for TAMDAR\0");
   }


ierr = nc_inq_varid(cdfid,"sensor1RelativeHumidity",&s1rh_id);
if ( ierr != 0 ) s1rh_id = -1;

ierr = nc_inq_varid(cdfid,"sensor2RelativeHumidity",&s2rh_id);
if ( ierr != 0 ) s2rh_id = -1;

ier += nc_get_att_float(cdfid,sped_id,"_FillValue",&sfill);
ier += nc_get_att_float(cdfid,drct_id,"_FillValue",&dfill);
ier += nc_get_att_float(cdfid,tmpk_id,"_FillValue",&tfill);
ier += nc_get_att_float(cdfid,mixr_id,"_FillValue",&mrfill);
ier += nc_get_att_float(cdfid,dwpc_id,"_FillValue",&tdfill);
ier += nc_get_att_float(cdfid,timeObs_id,"_FillValue",&obsfill);
ier += nc_get_att_float(cdfid,Alt_id,"_FillValue",&zfill);
ier += nc_get_att_float(cdfid,vacc_id,"_FillValue",&vafill);

if ( mach_id > 0 )
   {
   ierr = nc_inq_att(cdfid, mach_id, "_FillValue", &xtype, &attlen);
   if ( ierr != 0 ) 
      machfill = fmiss;
   else
      ierr = nc_get_att_float(cdfid, mach_id,"_FillValue",&machfill);
   }
else
   machfill = fmiss;

ier += nc_get_att_float(cdfid,heading_id,"_FillValue",&headingfill);
ier += nc_get_att_float(cdfid,medturb_id,"_FillValue",&medturbfill);
ier += nc_get_att_float(cdfid,maxturb_id,"_FillValue",&maxturbfill);
if(drelh_id != -1)
   ier += nc_get_att_float(cdfid,drelh_id,"_FillValue",&drhfill);
if(cwmixr_id != -1)
   ier += nc_get_att_float(cdfid,cwmixr_id,"_FillValue",&cwmrfill);

if (relh_id != -1 )
   ier += nc_get_att_float(cdfid,relh_id,"_FillValue",&rhfill);
if (s1rh_id != -1 )
   ier += nc_get_att_float(cdfid,s1rh_id,"_FillValue",&s1rhfill);
if (s2rh_id != -1 )
   ier += nc_get_att_float(cdfid,s2rh_id,"_FillValue",&s2rhfill);

/* Get encrypted name information */
ier += nc_inq_varid(cdfid,"en_tailNumber",&tail_id);
ier += nc_inq_vardimid(cdfid,tail_id,tmpint);
ier += nc_inq_dimlen(cdfid,tmpint[1],&namelen);
tail_len = namelen;

udebug("encrypted name length is %d\n",tail_len);
/* Was FSLxxxxx, now is FSLxxxxxxxxx */
if ( tail_len > 8 )
   uerror("Warning: Tail number exceeds 8 characters, will truncate on storage");

ier += nc_inq_varid(cdfid,"origAirport",&orig_id);
ier += nc_inq_varid(cdfid,"destAirport",&dest_id);
ier += nc_inq_vardimid(cdfid,orig_id,tmpint);
ier += nc_inq_dimlen(cdfid,tmpint[1],&namelen);
aid_len = namelen;
ier += nc_inq_varid(cdfid,"rptStation",&rpt_id);
ier += nc_inq_vardimid(cdfid,rpt_id,tmpint);
ier += nc_inq_dimlen(cdfid,tmpint[1],&namelen);
rpt_len = namelen;

if(ier != 0)
   {
   uerror("could not get variable information\0");
   *iret = -1;
   return(NULL);
   }

ier = nc_inq_dimlen(cdfid,nunlim,&dimsiz);
if(ier == 0) unlimsiz = dimsiz;
udebug("unlimited size is %d %d\n",unlimsiz,ier);


for(i=0;i<unlimsiz;i++)
   {
   var_i[0] = i; var_i[1] = 0; vc[0] = 1; vc[1] = tail_len-1;
   memset(tail_number,'\0',sizeof(tail_number));
   ier = nc_get_vara_text(cdfid,tail_id,var_i,vc,tail_number);
   ier += nc_get_var1_double(cdfid,timeObs_id,var_i,&timeObs);
   ier += nc_get_var1_float(cdfid,Lat_id,var_i,&Lat);
   ier += nc_get_var1_float(cdfid,Lon_id,var_i,&Lon);
   ier += nc_get_var1_float(cdfid,Alt_id,var_i,&hght);
   if(press_id > 0) ierr = nc_get_var1_float(cdfid,press_id,var_i,&press);
   ier += nc_get_var1_float(cdfid,sped_id,var_i,&sped);
   ier += nc_get_var1_float(cdfid,drct_id,var_i,&drct);
   ier += nc_get_var1_float(cdfid,tmpk_id,var_i,&tmpk);
   ier += nc_get_var1_float(cdfid,mixr_id,var_i,&mixr);
   if(cwmixr_id > 0) 
      ierr = nc_get_var1_float(cdfid,cwmixr_id,var_i,&cwmixr);
   else
      cwmixr = fmiss;

   if ( relh_id != -1 )
      ier += nc_get_var1_float(cdfid,relh_id,var_i,&relh);
   else
      relh = fmiss;

   if ( s1rh_id != -1 )
      ier += nc_get_var1_float(cdfid,s1rh_id,var_i,&s1rh);
   else
      s1rh = fmiss;

   if ( s2rh_id != -1 )
      ier += nc_get_var1_float(cdfid,s2rh_id,var_i,&s2rh);
   else
      s2rh = fmiss;

   if(drelh_id > 0) 
      ierr = nc_get_var1_float(cdfid,drelh_id,var_i,&drelh);
   else
      drelh = fmiss;

   ier += nc_get_var1_float(cdfid,dwpc_id,var_i,&dwpc);
   ier += nc_get_var1_float(cdfid,vacc_id,var_i,&vacc);
   ier += nc_get_var1_int(cdfid,data_id,var_i,&valid);
   ier += nc_get_var1_int(cdfid,terr_id,var_i,&terr);
   ier += nc_get_var1_int(cdfid,wderr_id,var_i,&wderr);
   ier += nc_get_var1_int(cdfid,wserr_id,var_i,&wserr);
   ier += nc_get_var1_int(cdfid,errtyp_id,var_i,&errtyp);
   ier += nc_get_var1_int(cdfid,corr_id,var_i,&corr);
   ier += nc_get_var1_int(cdfid,wvqc_id,var_i,&wvqc);
   ier += nc_get_var1_float(cdfid,heading_id,var_i,&heading);
   ier += nc_get_var1_float(cdfid,mach_id,var_i,&mach);
   ier += nc_get_var1_float(cdfid,medturb_id,var_i,&medturb);
   ier += nc_get_var1_float(cdfid,maxturb_id,var_i,&maxturb);
   if((valid == 'X')&&(errtyp == 'B')) continue;

   if((wvqc == '0')||(wvqc == '1'))
      {
      udebug("wvqc %c dwpc %f\0",wvqc,dwpc);
      udebug("   look mixr %f cwmixr %f\0",mixr,cwmixr);
      udebug("   look relh %f drelh %f\0",relh,drelh);
      }
   if ( ( s1rh != fmiss ) || ( s2rh != fmiss ) )
      udebug("   look s1rh %f s2rh %f\0",s1rh,s2rh);

   if(corr != 'r')
      udebug("data correction %s %lf %c\n",tail_number,timeObs,corr);

   memset(orig,'\0',sizeof(orig));
   memset(dest,'\0',sizeof(dest));
   vc[1] = aid_len;
   ier = nc_get_vara_text(cdfid,orig_id,var_i,vc,orig);
   ier = nc_get_vara_text(cdfid,dest_id,var_i,vc,dest);
   memset(rptsta,'\0',sizeof(rptsta));
   vc[1] = rpt_len;
   ier = nc_get_vara_text(cdfid,rpt_id,var_i,vc,rptsta);

   /* get time from seconds since 1970 */
   if(timeObs != obsfill)
      {
      obs_time = (time_t) timeObs;
      gmt_time = gmtime(&obs_time);
      new_time = *gmt_time; /* copy the time since gmtime pointer is volotile */
      timestr[0] = '\0';
      strftime(timestr,80,"%Y %m %d %H %M %S",&new_time);
      sscanf(timestr,"%d %d %d %d %d %d",&year,&month,&day,&hour,&minute,&seconds);
      }
   else
      {
      year = miss; month = miss; day = miss; hour = miss; minute = miss; seconds = miss;
      }

   pdata = (acars_struct *)malloc(sizeof(acars_struct));
   if(pdata == NULL)
      {
      uerror("Could not allocate station data structure\0");
      exit(-2);
      }

   /* Changes to cast new 12 charcter IDS to 8 characters */
   if ( strlen(tail_number) > 8 )
      {
      pdata->tailNumber = (char *)malloc(8);
      sprintf(pdata->tailNumber,"F\0");
      strncat(pdata->tailNumber,tail_number+strlen(tail_number) - 7,7); 
      }
   else
      {
      pdata->tailNumber = (char *)malloc(strlen(tail_number)+1);
      memset(pdata->tailNumber,0,sizeof(pdata->tailNumber));
      strcpy(pdata->tailNumber,tail_number);
      }


   pdata->orig = (char *)malloc(strlen(orig)+1);
   strcpy(pdata->orig,orig);
   pdata->dest = (char *)malloc(strlen(dest)+1);
   strcpy(pdata->dest,dest);
   pdata->rptsta = (char *)malloc(strlen(rptsta)+1);
   strcpy(pdata->rptsta,rptsta);
   pdata->Lat = Lat;
   pdata->Lon = Lon;
   pdata->timeObs = timeObs;
   pdata->year = year;
   pdata->month = month;
   pdata->day = day;
   pdata->hour = hour;
   pdata->minute = minute;
   pdata->seconds = seconds;
   pdata->Alt = fmiss;
   pdata->tmpc = fmiss;
   pdata->sped = fmiss;
   pdata->drct = fmiss;
   pdata->relh = fmiss;
   pdata->mixr = fmiss;
   pdata->dwpc = fmiss;
   pdata->vacc = fmiss;
   pdata->mach = fmiss;
   pdata->heading = fmiss;
   pdata->medturb = fmiss;
   pdata->maxturb = fmiss;
   if(hght != zfill) pdata->Alt = hght;
   if((tmpk != tfill)&&(terr == 'p')) pdata->tmpc = tmpk - 273.15;
   if((sped != sfill)&&(wderr == 'p')&&(wserr == 'p')) pdata->sped = sped;
   if((drct != dfill)&&(wderr == 'p')&&(wserr == 'p')) pdata->drct = drct;
   if((wvqc == '0')||(wvqc == '1'))
      {
      if( (relh != fmiss ) && ( relh != rhfill ) )
         if((relh >= 0)&&(relh <= 1.0)) pdata->relh = relh*100;
      if(mixr != mrfill) pdata->mixr = mixr;
      if(dwpc != tdfill) pdata->dwpc = dwpc - 273.15;
      }
   if ( pdata->relh  == fmiss )
      {
      if( (s1rh != fmiss ) && ( s1rh != s1rhfill ) )
	 pdata->relh = s1rh;
      else if( (s2rh != fmiss ) && ( s2rh != s2rhfill ) )
	 pdata->relh = s2rh;
      }
   if(vacc != vafill) pdata->vacc = vacc;

   /* no machfill in cdl yet 
   old mach test when fill value was ommitted --> if((mach >= 0)&&(mach < 2)) pdata->mach = mach; */

   if ( mach != machfill )
      pdata->mach = mach;
   else
      pdata->mach = fmiss;

   if(heading != headingfill) pdata->heading = heading;
   if(medturb != medturbfill) pdata->medturb = medturb;
   if(maxturb != maxturbfill) pdata->maxturb = maxturb;
   
   if(press_id > 0) ftmp = press;
   if((pdata->tmpc != fmiss)&&(pdata->Alt != fmiss))
      {
      press = log(1013.25) - (pdata->Alt * 2.0 * 9.8/ 287.)/(288.15 + tmpk);
      press = exp(press);
      }
   else
      press = fmiss;
   pdata->press = press;
   if(press_id > 0) uinfo("check presure %f [%f]\0",ftmp,press);

   if((pdata->tmpc != fmiss)&&(pdata->dwpc !=fmiss))
      if(pdata->dwpc > pdata->tmpc) 
         printf("%s %04d%02d%02d/%02d%02d DWPC %f greater than TMPC %f\n",
            pdata->tailNumber,pdata->year,pdata->month,pdata->day,
            pdata->hour,pdata->minute,pdata->dwpc,pdata->tmpc);

   /*
   ftmp = fmiss;
   if((pdata->mixr > 0)&&(pdata->press != fmiss))
      {
      e = (mixr / (mixr + .622)) * pdata->press;
      if(pdata->tmpc != fmiss)
         {
         VAPOR_PRES(pdata->tmpc+273.15,&ftmp);
         ftmp = (e/ftmp)*100;
         }
      printf("look %s %d %c %f %f %f %f %f %f\n",
         pdata->tailNumber,pdata->hour,wvqc,pdata->tmpc,pdata->dwpc,pdata->relh,
         mixr,e,ftmp);
      }
   */

   /* see if we can get dewpoint from RH */
   if((pdata->dwpc == fmiss)&&(pdata->relh != fmiss)&&(pdata->tmpc != fmiss))
      {
      udebug("calculating dewpoint for %s\0",pdata->tailNumber);
      VAPOR_PRES(pdata->tmpc+273.15,&e);
      e = e * ( pdata->relh / 100.0 ) ;
      t_from_e(e,&pdata->dwpc);
      pdata->dwpc = pdata->dwpc - 273.15;
      }

   udebug("Record %d tail_number %s time %04d%02d%02d/%02d%02d\n",
          i,tail_number,year,month,day,hour,minute);
   udebug("Lat %f Lon %f  Height %f Pres %f\n",Lat,Lon,hght,press);

   pdata->next = head;
   head = pdata;
   }

return(head);



} 
