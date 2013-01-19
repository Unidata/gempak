/*
    NASA/TRMM, Code 910.1.
    This is the TRMM Office Radar Software Library.
    Copyright (C) 1996-1999
            John H. Merritt
            Space Applications Corporation
            Vienna, Virginia

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <strings.h>
#define USE_RSL_VARS
#include "rsl.h"
#include "dorade.h"

extern int radar_verbose_flag;

/********************************************************************/
/*                                                                  */
/*                    find_rsl_field_index                          */
/*                                                                  */
/********************************************************************/
int find_rsl_field_index(char *dorade_field_name)
{
  /*  
   * Dorade: VE, DM, SW, DZ, ZDR, PHI, RHO, LDR,  DX,  CH,  AH,  CV,  AV
   *    RSL: VR, DM, SW, DZ,  ZD,  PH,  RH,  LR, *DX, *CH, *AH, *CV, *AV.
   */
  if (strncasecmp(dorade_field_name, "ve", 2) == 0) return VR_INDEX;
  if (strncasecmp(dorade_field_name, "dm", 2) == 0)	return DM_INDEX;
  if (strncasecmp(dorade_field_name, "sw", 2) == 0)	return SW_INDEX;
  if (strncasecmp(dorade_field_name, "dz", 2) == 0)	return DZ_INDEX;
  if (strncasecmp(dorade_field_name, "zdr", 3) == 0) return ZD_INDEX;
  if (strncasecmp(dorade_field_name, "phi", 3) == 0) return PH_INDEX;
  if (strncasecmp(dorade_field_name, "rho", 3) == 0) return RH_INDEX;
  if (strncasecmp(dorade_field_name, "ldr", 3) == 0) return LR_INDEX;
  if (strncasecmp(dorade_field_name, "dx", 2) == 0)	return DX_INDEX;
  if (strncasecmp(dorade_field_name, "ch", 2) == 0)	return CH_INDEX;
  if (strncasecmp(dorade_field_name, "ah", 2) == 0)	return AH_INDEX;
  if (strncasecmp(dorade_field_name, "cv", 2) == 0)	return CV_INDEX;
  if (strncasecmp(dorade_field_name, "av", 2) == 0)	return AV_INDEX;

  fprintf(stderr, "Unknown DORADE type <%s>\n", dorade_field_name);
  return -1;
}

/* Secretly defined in uf_to_radar.c */
Volume *copy_sweeps_into_volume(Volume *new_volume, Volume *old_volume);

Radar *RSL_dorade_to_radar(char *infile)
{
  Radar  *radar;
  Volume *new_volume;
  Sweep  *sweep;
  Ray    *ray;
  int iv, iray, iparam;

  FILE  *fp;
  Volume_desc     *vd;
  Sensor_desc    **sd;
  Sweep_record    *sr;
  Radar_desc      *rd;
  Data_ray        *dray;
  Parameter_data  *pd;

  int nsweep;
  int i;
  char buf[1024];

  int degree, minute;
  float second;

  radar = NULL;
  if (infile == NULL) {
	int save_fd;
	save_fd = dup(0);
	fp = fdopen(save_fd, "r");
  }  else
    if((fp=fopen(infile, "r"))==(FILE *)NULL) {
	  perror(infile);
	  return radar;
    }

  fp = uncompress_pipe(fp); /* Transparently, use gunzip. */

  /**********************************************************************/

  vd = dorade_read_volume_desc(fp);   /* R E A D */
  if (radar_verbose_flag)	dorade_print_volume_desc(vd);  /* P R I N T */

  /* R E A D */
  sd = (Sensor_desc **) calloc(vd->nsensors, sizeof(Sensor_desc *));
  for (i=0; i<vd->nsensors; i++) {
	sd[i] = dorade_read_sensor(fp);
  }

  /* P R I N T */
  if (radar_verbose_flag) {
	for (i=0; i<vd->nsensors; i++) {
	  fprintf(stderr, "============ S E N S O R   # %d =====================\n", i);
	  dorade_print_sensor(sd[i]);
	}
  }
  /* R E A D   sweeps. */
  if (vd->nsensors > 1) {
	fprintf(stderr, "RSL_dorade_to_radar: Unable to process for more than 1 sensor.\n");
	fprintf(stderr, "RSL_dorade_to_radar: Number of sensors is %d\n", vd->nsensors);
	return NULL;
  }

  /* Use sensor 0 for vitals. */
  rd = sd[0]->radar_desc;

  radar = RSL_new_radar(MAX_RADAR_VOLUMES);
  radar->h.month = vd->month;
  radar->h.day   = vd->day;
  radar->h.year  = vd->year;
  radar->h.hour  = vd->hour;
  radar->h.minute = vd->minute;
  radar->h.sec    = vd->second;
  sprintf(radar->h.radar_type, "dorade");
  radar->h.number = 0;
  strncpy(radar->h.name, vd->flight_num, sizeof(radar->h.name));
  strncpy(radar->h.radar_name, rd->radar_name, sizeof(radar->h.radar_name));
  strncpy(radar->h.project, vd->project_name, sizeof(radar->h.project));
  sprintf(radar->h.city, "Unknown");
  strncpy(radar->h.state, "UKN", 3);
  sprintf(radar->h.country, "Unknown");
  /* Convert lat to d:m:s */
  degree = (int)rd->latitude;
  minute = (int)((rd->latitude - degree) * 60);
  second = (rd->latitude - degree - minute/60.0) * 3600.0;
  radar->h.latd = degree;
  radar->h.latm = minute;
  radar->h.lats = second;
  /* Convert lat to d:m:s */
  degree = (int)rd->longitude;
  minute = (int)((rd->longitude - degree) * 60);
  second = (rd->longitude - degree - minute/60.0) * 3600.0;
  radar->h.lond = degree;
  radar->h.lonm = minute;
  radar->h.lons = second;
  radar->h.height = rd->altitude * 1000.0;
  radar->h.spulse = 0; /* FIXME */
  radar->h.lpulse = 0; /* FIXME */
  
  /* Begin volume code. */
  /* We don't know how many sweeps per volume exist, until we read
   * the file.  So allocate a large number of pointers, hope we don't
   * exceed it, and adjust the pointer array at the end.  This is 
   * efficient because we'll be manipulating pointers to the sweeps and
   * not the sweeps themselves.
   */

  if (radar_verbose_flag)
	fprintf(stderr, "Number of parameters: %d\n", rd->nparam_desc);

  /* All the parameters are together, however, their order within
   * the ray is not guarenteed.  For instance, VE could appear after
   * DM.  For this we'll keep a list of parameter names and perform
   * a (linear) search.  The result will be an index into the RSL
   * volume array (radar->v[i]).  It is likely that the order will
   * consistant within a file, therefore, we'll keep track of the index of
   * our previous parameter type and begin the search from there; the next
   * index should be a match.
   *
   * The dorade parameter names and the rsl mapping is:
   *
   * Dorade: VE, DM, SW, DZ, ZDR, PHI, RHO, LDR,  DX,  CH,  AH,  CV,  AV
   *    RSL: VR, DM, SW, DZ,  ZD,  PH,  RH,  LR, *DX, *CH, *AH, *CV, *AV.
   * 
   *    * means this is a new RSL name.
   */

#define DORADE_MAX_SWEEP 20
  nsweep = 0;
  while((sr = dorade_read_sweep(fp, sd))) {
	for(iray = 0; iray < sr->nrays; iray++) {
	  dray = sr->data_ray[iray];

	  /* Now, loop through the parameters and fill the rsl structures. */
	  for (iparam = 0; iparam < dray->nparam; iparam++) {
		pd = dray->parameter_data[iparam];
		iv = find_rsl_field_index(pd->name);
		if (radar->v[iv] == NULL) {
		  radar->v[iv] = RSL_new_volume(DORADE_MAX_SWEEP); /* Expandable */
		} else if (nsweep >= radar->v[iv]->h.nsweeps) {
		  /* Must expand the number of sweeps. */
		  /* Expand by another DORADE_MAX_SWEEP. */
		  if (radar_verbose_flag) {
			fprintf(stderr, "nsweeps (%d) exceeds radar->v[%d]->h.nsweeps (%d).\n", nsweep, iv, radar->v[iv]->h.nsweeps);
			fprintf(stderr, "Increading it to %d sweeps\n", radar->v[iv]->h.nsweeps+DORADE_MAX_SWEEP);
		  }
		  new_volume = RSL_new_volume(radar->v[iv]->h.nsweeps+DORADE_MAX_SWEEP);
		  /* Look in uf_to_radar.c for 'copy_sweeps_into_volume' */
		  new_volume = copy_sweeps_into_volume(new_volume, radar->v[iv]);
		  radar->v[iv] = new_volume;
		}

		/* Allocate the ray and load the parameter data. */
		if ((sweep = radar->v[iv]->sweep[nsweep]) == NULL) {
		  sweep = radar->v[iv]->sweep[nsweep] = RSL_new_sweep(sr->s_info->nrays);
	      sweep->h.sweep_num    = sr->s_info->sweep_num;
		  sweep->h.elev         = sr->s_info->fixed_angle;
          sweep->h.beam_width   = rd->horizontal_beam_width;
		  sweep->h.vert_half_bw = radar->v[iv]->sweep[nsweep]->h.beam_width / 2.0;
		  sweep->h.horz_half_bw = rd->horizontal_beam_width / 2.0;
		  sweep->h.f = RSL_f_list[iv];
		  sweep->h.invf = RSL_invf_list[iv];
		}
		

		if ((ray = sweep->ray[iray]) == NULL) {
		  if (radar_verbose_flag)
			fprintf(stderr, "Allocating %d bins for ray %d\n", dray->data_len[iparam], iray);
		  ray = sweep->ray[iray] = RSL_new_ray(dray->data_len[iparam]);
		}

		/* Copy the ray data into the RSL ray. */

        /* .... fill here .... */
	  }
	}
	nsweep++;
	if (radar_verbose_flag) fprintf(stderr, "______NEW SWEEP__<%d>____\n", nsweep);
	/* Save for loading into volume structure. */
	dorade_free_sweep(sr);
  }

  /* The following avoids a broken pipe message, since a VOLD at the end
   * is not read yet.
   */
  while(fread(buf, sizeof(buf), 1, fp)) continue;  /* Read til EOF */

  rsl_pclose(fp);

  return radar;
}

