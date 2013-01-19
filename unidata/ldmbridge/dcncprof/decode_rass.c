#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#include <netcdf.h>
#include "ulog.h"
#include "profiler.h"
#include "proto_func.h"

void *
decode_rass (int cdfid, long miss, int *iret)
{
  int ndims, nvars, natts, nunlim;
  int tmpint[20];
  int ier, i, j, unlimsiz;
  int wmoStaNum_id, wmoStaNum, staName_id;
  char staName[20];
  int staLat_id, staLon_id, staElev_id, timeObs_id, levels_id;
  int consensus_id, power_id, tmpv_id, specw_id, qual_id, submode_id;
  int sfc_sped_id, sfc_drct_id, sfc_pres_id, sfc_temp_id, sfc_relh_id, sfc_rain_id;
  float staLat, staLon, staElev, level;
  int qual;
  unsigned char submode;
  float sfc_sped, sfc_drct, sfc_pres, sfc_temp, sfc_relh, sfc_rain;
  double timeObs;
  nc_type xtype;
  int nvdims, nvatts, time_interval;
  size_t dimsiz, var_i[5], vc[5], namelen;
  float consensus, power, tmpv, specw;
  float consensus_fill, power_fill, tmpv_fill, specw_fill;
  float pfill, tfill, dfill, sfill, rfill, rrfill;
  float fmiss, e;

  time_t obs_time;
  char timestr[80], *atttext;
  int year, month, day, hour, minute;
  struct tm *gmt_time = NULL, new_time;
  sta_struct *stadat, *head = NULL;
  rass_data *rlev, *rlast;


  udebug ("decoding RASS\0");
  fmiss = (float) miss;

  ier = nc_inq (cdfid, &ndims, &nvars, &natts, &nunlim);

  if ((ier = nc_inq_att (cdfid, NC_GLOBAL, "avgTimePeriod", &xtype, &namelen)) == 0)
     {
     if (xtype == NC_CHAR)
       {
         atttext = (char *) malloc (namelen + 1);
         ier = nc_get_att_text (cdfid, NC_GLOBAL, "avgTimePeriod", atttext);
         sscanf (atttext, "%d", tmpint);
         udebug ("AvgTimPeriod type is NC_CHAR %s VAL %d", atttext, tmpint[0]);
         free (atttext);
       }
     else
       {
         ier = nc_get_att_int (cdfid, NC_GLOBAL, "avgTimePeriod", tmpint);
       }
     } 
  udebug ("AvgTimPeriod is %d\0", tmpint[0]);
  time_interval = tmpint[0];

  ier = 0;
  ier += nc_inq_varid (cdfid, "wmoStaNum", &wmoStaNum_id);
  ier += nc_inq_varid (cdfid, "staName", &staName_id);
  ier += nc_inq_varid (cdfid, "staLat", &staLat_id);
  ier += nc_inq_varid (cdfid, "staLon", &staLon_id);
  ier += nc_inq_varid (cdfid, "staElev", &staElev_id);
  ier += nc_inq_varid (cdfid, "timeObs", &timeObs_id);
  ier += nc_inq_varid (cdfid, "level", &levels_id);
  udebug("test cdl %d",ier);

  ier += nc_inq_varid (cdfid, "consensusNum", &consensus_id);
  ier += nc_inq_varid (cdfid, "peakPower", &power_id);
  ier += nc_inq_varid (cdfid, "virtualTemp", &tmpv_id);
  ier += nc_inq_varid (cdfid, "specWidth", &specw_id);
  ier += nc_get_att_float (cdfid, consensus_id, "_FillValue", &consensus_fill);
  ier += nc_get_att_float (cdfid, power_id, "_FillValue", &power_fill);
  ier += nc_get_att_float (cdfid, tmpv_id, "_FillValue", &tmpv_fill);
  ier += nc_get_att_float (cdfid, specw_id, "_FillValue", &specw_fill);
  udebug("test cdl %d",ier);

  ier += nc_inq_varid (cdfid, "qualityCode", &qual_id);
  ier += nc_inq_varid (cdfid, "submode", &submode_id);
  udebug("test cdl %d",ier);

  ier += nc_inq_varid (cdfid, "windSpeedSfc", &sfc_sped_id);
  ier += nc_inq_varid (cdfid, "windDirSfc", &sfc_drct_id);
  ier += nc_inq_varid (cdfid, "pressure", &sfc_pres_id);
  ier += nc_inq_varid (cdfid, "temperature", &sfc_temp_id);
  ier += nc_inq_varid (cdfid, "relHumidity", &sfc_relh_id);
  ier += nc_inq_varid (cdfid, "rainRate", &sfc_rain_id);
  udebug("test cdl %d",ier);
  ier += nc_get_att_float (cdfid, sfc_sped_id, "_FillValue", &sfill);
  ier += nc_get_att_float (cdfid, sfc_drct_id, "_FillValue", &dfill);
  ier += nc_get_att_float (cdfid, sfc_pres_id, "_FillValue", &pfill);
  ier += nc_get_att_float (cdfid, sfc_temp_id, "_FillValue", &tfill);
  ier += nc_get_att_float (cdfid, sfc_relh_id, "_FillValue", &rfill);
  ier += nc_get_att_float (cdfid, sfc_rain_id, "_FillValue", &rrfill);

  if (ier != 0)
    {
      uerror ("could not get station information %d\0",ier);
      *iret = -1;
      return (NULL);
    }

  ier = nc_inq_vardimid (cdfid, staName_id, tmpint);
  ier += nc_inq_dimlen (cdfid, tmpint[1], &namelen);

  tmpint[0] = 0;
  tmpint[1] = 0;
  ier +=
    nc_inq_var (cdfid, wmoStaNum_id, NULL, &xtype, &nvdims, tmpint, &nvatts);
  ier += nc_inq_dimlen (cdfid, tmpint[0], &dimsiz);

  if (ier == 0)
    unlimsiz = dimsiz;

  for (i = 0; i < unlimsiz; i++)
    {
      var_i[0] = i;
      var_i[1] = 0;
      vc[0] = 1;
      vc[1] = namelen - 1;
      memset (staName, '\0', 20);
      ier = nc_get_vara_text (cdfid, staName_id, var_i, vc, staName);
      ier = nc_get_var1_int (cdfid, wmoStaNum_id, var_i, &wmoStaNum);
      ier = nc_get_var1_float (cdfid, staLat_id, var_i, &staLat);
      ier = nc_get_var1_float (cdfid, staLon_id, var_i, &staLon);
      ier = nc_get_var1_float (cdfid, staElev_id, var_i, &staElev);
      ier = nc_get_var1_float (cdfid, sfc_sped_id, var_i, &sfc_sped);
      ier = nc_get_var1_float (cdfid, sfc_drct_id, var_i, &sfc_drct);
      ier = nc_get_var1_float (cdfid, sfc_pres_id, var_i, &sfc_pres);
      ier = nc_get_var1_float (cdfid, sfc_temp_id, var_i, &sfc_temp);
      ier = nc_get_var1_float (cdfid, sfc_relh_id, var_i, &sfc_relh);
      ier = nc_get_var1_float (cdfid, sfc_rain_id, var_i, &sfc_rain);
      ier = nc_get_var1_double (cdfid, timeObs_id, var_i, &timeObs);
      ier = nc_get_var1_uchar (cdfid, submode_id, var_i, &submode);
      obs_time = (time_t) timeObs;
      gmt_time = gmtime (&obs_time);
      new_time = *gmt_time;
      timestr[0] = '\0';
      strftime (timestr, 80, "%Y %m %d %H %M", &new_time);
      sscanf (timestr, "%d %d %d %d %d", &year, &month, &day, &hour, &minute);
      udebug ("Station %3d %8d %s = %6.2f %7.2f %5.0f %s\0", i, wmoStaNum,
	      staName, staLat, staLon, staElev, timestr);

      stadat = (sta_struct *) malloc (sizeof (sta_struct));
      if (stadat == NULL)
	{
	  uerror ("Could not allocate station data structure\0");
	  exit (-2);
	}
      stadat->wmoStaNum = wmoStaNum;
      stadat->staName = (char *) malloc (strlen (staName) + 1);
      strcpy (stadat->staName, staName);
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
      if (sfc_pres != pfill)
	stadat->sfc_pres = sfc_pres;
      if (sfc_temp != tfill)
	stadat->sfc_temp = sfc_temp - 273.15;
      if (sfc_sped != sfill)
	stadat->sfc_sped = sfc_sped;
      if (sfc_drct != dfill)
	stadat->sfc_drct = sfc_drct;
      if (sfc_relh != rfill)
	stadat->sfc_relh = sfc_relh;
      if (sfc_rain != rrfill)
	stadat->sfc_rain_rate = sfc_rain;
      if ((stadat->sfc_temp != fmiss) && (stadat->sfc_relh != fmiss))
	{
	  VAPOR_PRES (stadat->sfc_temp + 273.15, &e);
	  e = e * (stadat->sfc_relh / 100.);
	  t_from_e (e, &stadat->sfc_dwpc);
	  stadat->sfc_dwpc = stadat->sfc_dwpc - 273.15;
	}

      ier =
	nc_inq_var (cdfid, levels_id, NULL, &xtype, &nvdims, tmpint, &nvatts);
      if (ier == 0)
	{
	  ier = nc_inq_dimlen (cdfid, tmpint[0], &dimsiz);
	  stadat->numlevs = dimsiz;
	  rlast = stadat->rdata;
	  for (j = 0; j < stadat->numlevs; j++)
	    {
	      var_i[0] = j;
	      var_i[1] = 0;
	      ier = nc_get_var1_float (cdfid, levels_id, var_i, &level);
	      var_i[0] = i;
	      var_i[1] = j;
	      ier = nc_get_var1_float (cdfid, consensus_id, var_i, &consensus);
	      ier = nc_get_var1_float (cdfid, power_id, var_i, &power);
	      ier = nc_get_var1_float (cdfid, tmpv_id, var_i, &tmpv);
	      ier = nc_get_var1_float (cdfid, specw_id, var_i, &specw);
	      ier = nc_get_var1_int (cdfid, qual_id, var_i, &qual);
	      udebug ( "level %f qual %d tmpv %f consensus %f power %f specw %f",
		level, qual, tmpv, consensus, power, specw );
	      rlev = (rass_data *) malloc (sizeof (rass_data));
	      if (rlev != NULL)
		{
		  rlev->level = level;
		  if (consensus == consensus_fill)
		    consensus = fmiss;
		  if (power == power_fill)
		    power = fmiss;
		  if (tmpv == tmpv_fill)
		    tmpv = fmiss;
		  if (specw == specw_fill)
		    specw = fmiss;

		  /* 
		     qualityCode:noBitsSet = "Good" ;
                     qualityCode:bit1Set = "Reserved" ;
                     qualityCode:bit2Set = "Test results inconclusive" ;
                     qualityCode:bit3Set = "Test B performed and failed" ;
                     qualityCode:bit4Set = "Test A performed and failed" ;
                     qualityCode:bit5Set = "Reserved" ;
                     qualityCode:bit6Set = "Reserved" ;
                     qualityCode:bit7Set = "Reserved" ;
                     qualityCode:bit8Set = "Reserved" ;
                     qualityCode:bits1To8Set = "Missing" ;
		  */
		  if (qual != 0)
		    {
		      tmpv = fmiss;
		    }
		  rlev->consensus = consensus;
		  rlev->power = power;
		  rlev->tmpv = tmpv;
		  rlev->specw = specw;
		  rlev->submode = submode;
		  rlev->nextlev = NULL;
		  if (rlast == NULL)
		    stadat->rdata = rlev;
		  else
		    rlast->nextlev = rlev;
		  rlast = rlev;
		}
	    }
	}
      else
	stadat->numlevs = 0;

      stadat->next = head;
      head = stadat;
    }

  return (head);

}
