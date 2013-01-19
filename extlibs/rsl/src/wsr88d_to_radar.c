/*
    NASA/TRMM, Code 910.1.
    This is the TRMM Office Radar Software Library.
    Copyright (C) 1996, 1997
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
#include <string.h>

#include <stdlib.h>
#include <math.h>

#include "rsl.h"
#include "wsr88d.h"

extern int radar_verbose_flag;
/*
 * These externals can be found in the wsr88d library; secret code.
 */
void print_head(Wsr88d_file_header wsr88d_file_header);
void clear_sweep(Wsr88d_sweep *wsr88d_sweep, int x, int n);
void free_and_clear_sweep(Wsr88d_sweep *wsr88d_sweep, int x, int n);
/*
 * Secretly in uf_to_radar.c
 */
Volume *copy_sweeps_into_volume(Volume *new_volume, Volume *old_volume);

 
void float_to_range(float *x, Range *c, int n, Range (*function)(float x) )
{
  while (n--) {
	if (*x == WSR88D_BADVAL) *c = function(BADVAL);
	else if (*x == WSR88D_RFVAL) *c = function(RFVAL);
	else *c = function(*x);
	c++; x++;
  }
}

/**********************************************************************/
/*                                                                    */
/* done 3/23         wsr88d_load_sweep_into_volume                    */
/*                                                                    */
/*  By: John Merritt                                                  */
/*      Space Applications Corporation                                */
/*      March 3, 1994                                                 */
/**********************************************************************/
int wsr88d_load_sweep_into_volume(Wsr88d_sweep ws,
					   Volume *v, int nsweep, unsigned int vmask)
{
  int i;
  int iray;
  /*float v_data[1000];
  Range  c_data[1000];*/
  /* Increased for super res data */
  float v_data[2000];
  Range  c_data[2000];
  int n;

  int mon, day, year;
  int hh, mm, ss;
  float fsec;
  int vol_cpat;
  float azm_res_spacing, minbeam_width=1.0; /* S. Chiswell added */

  Ray *ray_ptr;
  Range (*invf)(float x);
  float (*f)(Range x);

  /* Allocate memory for MAX_RAYS_IN_SWEEP rays. */
  v->sweep[nsweep] = RSL_new_sweep(MAX_RAYS_IN_SWEEP);
  if (v->sweep[nsweep] == NULL) {
	perror("wsr88d_load_sweep_into_volume: RSL_new_sweep");
	return -1;
  }
	
  v->sweep[nsweep]->h.nrays = 0;
  f = (float (*)(Range x))NULL;
  invf = (Range (*)(float x))NULL;
  if (vmask & WSR88D_DZ) { invf = DZ_INVF; f = DZ_F; }
  if (vmask & WSR88D_VR) { invf = VR_INVF; f = VR_F; }
  if (vmask & WSR88D_SW) { invf = SW_INVF; f = SW_F; }
  
  v->h.invf = invf;
  v->h.f    = f;
  v->sweep[nsweep]->h.invf = invf;
  v->sweep[nsweep]->h.f    = f;

  for (i=0,iray=0; i<MAX_RAYS_IN_SWEEP; i++) {
	if (ws.ray[i] != NULL) {
	  wsr88d_ray_to_float(ws.ray[i], vmask, v_data, &n);
	  float_to_range(v_data, c_data, n, invf);
	  if (n > 0) {
		wsr88d_get_date(ws.ray[i], &mon, &day, &year);
		wsr88d_get_time(ws.ray[i], &hh, &mm, &ss, &fsec);
		/*
	  fprintf(stderr,"n %d, mon %d, day %d, year %d,  hour %d, min %d, sec %d, fsec %f\n",
			n, mon, day, year, hh, mm, ss, fsec);
			*/
		/*
		 * Load the sweep/ray headar information.
		 */
		
		v->sweep[nsweep]->ray[iray] = RSL_new_ray(n);
		/*(Range *)calloc(n, sizeof(Range)); */
		
		ray_ptr = v->sweep[nsweep]->ray[iray]; /* Make code below readable. */
		ray_ptr->h.f        = f;
		ray_ptr->h.invf     = invf;
		ray_ptr->h.month    = mon;
		ray_ptr->h.day      = day;
		ray_ptr->h.year     = year + 1900; /* Yes 1900 makes this year 2000 compliant, due to wsr88d using unix time(). */
		ray_ptr->h.hour     = hh;
		ray_ptr->h.minute   = mm;
		ray_ptr->h.sec      = ss + fsec;
		ray_ptr->h.unam_rng = wsr88d_get_range   (ws.ray[i]);
		ray_ptr->h.azimuth  = wsr88d_get_azimuth (ws.ray[i]);
/* -180 to +180 is converted to 0 to 360 */
		if (ray_ptr->h.azimuth < 0) ray_ptr->h.azimuth += 360;
		ray_ptr->h.ray_num  = wsr88d_get_ray_num( ws.ray[i] );
		ray_ptr->h.elev       = wsr88d_get_elevation_angle(ws.ray[i]);
		ray_ptr->h.elev_num   = wsr88d_get_elev_num ( ws.ray[i] );
		if (vmask & WSR88D_DZ) {
		  ray_ptr->h.range_bin1 = wsr88d_get_refl_rng( ws.ray[i] );
		  ray_ptr->h.gate_size  = wsr88d_get_refl_size( ws.ray[i] );
		} else {
		  ray_ptr->h.range_bin1 = wsr88d_get_dop_rng( ws.ray[i] );
		  ray_ptr->h.gate_size  = wsr88d_get_dop_size( ws.ray[i] );
		}
                /* S. Chiswell */
                azm_res_spacing = wsr88d_get_azm_res( ws.ray[i] );
		if ( azm_res_spacing < minbeam_width ) minbeam_width = azm_res_spacing ;

		ray_ptr->h.vel_res  = wsr88d_get_velocity_resolution(ws.ray[i]);
		vol_cpat = wsr88d_get_volume_coverage(ws.ray[i]);
		switch (vol_cpat) {
		case 11: ray_ptr->h.sweep_rate = 16.0/5.0;  break;
		case 12: ray_ptr->h.sweep_rate = 17.0/4.2;  break;
		case 21: ray_ptr->h.sweep_rate = 11.0/6.0;  break;
		case 31: ray_ptr->h.sweep_rate =  8.0/10.0; break;
		case 32: ray_ptr->h.sweep_rate =  7.0/10.0; break;
		case 121:ray_ptr->h.sweep_rate = 20.0/5.5;  break;
		default: ray_ptr->h.sweep_rate =  0.0; break;
		}

		ray_ptr->h.nyq_vel  = wsr88d_get_nyquist(ws.ray[i]);
		ray_ptr->h.azim_rate   = wsr88d_get_azimuth_rate(ws.ray[i]);
		ray_ptr->h.fix_angle   = wsr88d_get_fix_angle(ws.ray[i]);
		ray_ptr->h.pulse_count = wsr88d_get_pulse_count(ws.ray[i]);
		ray_ptr->h.pulse_width = wsr88d_get_pulse_width(ws.ray[i]);
		/*ray_ptr->h.beam_width  = .95; was hardcoded for old data. S. Chiswell modified below*/
		ray_ptr->h.beam_width  = azm_res_spacing * .95; /* S. Chiswell */
		ray_ptr->h.prf         = wsr88d_get_prf(ws.ray[i]);
		ray_ptr->h.frequency   = wsr88d_get_frequency(ws.ray[i]);
		ray_ptr->h.wavelength = 0.1071; /* Previously called
		                             * wsr88d_get_wavelength(ws.ray[i]).
		                             * See wsr88d.c for explanation.
					     */

		/* It is no coincidence that the 'vmask' and wsr88d datatype
		 * values are the same.  We expect 'vmask' to be one of
		 * REFL_MASK, VEL_MASK, or SW_MASK.  These match WSR88D_DZ,
		 * WSR88D_VR, and WSR88D_SW in the wsr88d library.
		 */
		ray_ptr->h.nbins = n;
		memcpy(ray_ptr->range, c_data, n*sizeof(Range));
		v->sweep[nsweep]->h.nrays = iray+1;
		v->sweep[nsweep]->h.elev += ray_ptr->h.elev;
		v->sweep[nsweep]->h.sweep_num = ray_ptr->h.elev_num;
		iray++;
	  }
	}
  }
  /*v->sweep[nsweep]->h.beam_width = .95;   S. Chiswell Modified below for super res data, not fixed beam width 
  v->sweep[nsweep]->h.vert_half_bw = .475;
  v->sweep[nsweep]->h.horz_half_bw = .475; */
  v->sweep[nsweep]->h.beam_width = minbeam_width * .95;
  v->sweep[nsweep]->h.vert_half_bw = v->sweep[nsweep]->h.beam_width / 2.0;
  v->sweep[nsweep]->h.horz_half_bw = v->sweep[nsweep]->h.beam_width / 2.0; 
  /* Now calculate the mean elevation angle for this sweep. */
  if (v->sweep[nsweep]->h.nrays > 0)
	v->sweep[nsweep]->h.elev /= v->sweep[nsweep]->h.nrays;
  else {
	RSL_free_sweep(v->sweep[nsweep]);
	/* free(v->sweep[nsweep]);  ** No rays loaded, free this sweep. */
	v->sweep[nsweep] = NULL;
  }
  
  return 0;
}

/**********************************************************************/
/*                                                                    */
/*                     RSL_wsr88d_to_radar                            */
/*                                                                    */
/*  By: John Merritt                                                  */
/*      Space Applications Corporation                                */
/*      March 3, 1994                                                 */
/**********************************************************************/

Radar *RSL_wsr88d_to_radar(char *infile, char *call_or_first_tape_file)
/*
 * Gets all volumes from the nexrad file.  Input file is 'infile'.
 * Site information is extracted from 'call_or_first_tape_file'; this
 * is typically a disk file called 'nex.file.1'.
 *
 *  -or-
 *
 * Uses the string in 'call_or_first_tape_file' as the 4 character call sign
 * for the sight.  All UPPERCASE characters.  Normally, this call sign
 * is extracted from the file 'nex.file.1'.
 *
 * Returns a pointer to a Radar structure; that contains the different
 * Volumes of data.
 */
{
  Radar *radar;
  Volume *new_volume;
  Wsr88d_file *wf;
  Wsr88d_sweep wsr88d_sweep;
  Wsr88d_file_header wsr88d_file_header;
  Wsr88d_tape_header wsr88d_tape_header;
  int n;
  int nsweep;
  int iv;
  int nvolumes;
  int volume_mask[] = {WSR88D_DZ, WSR88D_VR, WSR88D_SW};
  char *field_str[] = {"Reflectivity", "Velocity", "Spectrum width"};
  Wsr88d_site_info *sitep;
  char site_id_str[5], failid[]="KFTG";
  char *the_file;

  extern int rsl_qfield[]; /* See RSL_select_fields in volume.c */
  extern int *rsl_qsweep; /* See RSL_read_these_sweeps in volume.c */
  extern int rsl_qsweep_max;

  /* initialize site id to a failsafe value */
 wsr88d_set_station_id(failid, -9999.0, -9999.0, -9999.0);

  sitep = NULL;
/* Determine the site quasi automatically.  Here is the procedure:
 *    1. Determine if we have a call sign.
 *    2. Try reading 'call_or_first_tape_file' from disk.  This is done via
 *       wsr88d_read_tape_header.
 *    3. If no valid site info, abort.
 */
  if (call_or_first_tape_file == NULL) {
	fprintf(stderr, "wsr88d_to_radar: No valid site ID info provided.\n");
	return(NULL);
  }	else if (strlen(call_or_first_tape_file) == 4)
	sitep =  wsr88d_get_site(call_or_first_tape_file);
  else if (strlen(call_or_first_tape_file) < 3) {
	/*fprintf(stderr, "wsr88d_to_radar: No valid site ID info provided, using default.\n");
	strcpy(site_id_str,failid);
        sitep = wsr88d_get_site(site_id_str); -*enough for now. Message 31 contains ICAO and lat/lon */
	/*return(NULL);*/
  }  

  if (sitep == NULL)
	if (wsr88d_read_tape_header(call_or_first_tape_file, &wsr88d_tape_header) > 0) {
	  memcpy(site_id_str, wsr88d_tape_header.site_id, 4);
	  sitep  = wsr88d_get_site(site_id_str);
	}
  /*if (sitep == NULL) {
	  fprintf(stderr,"wsr88d_to_radar: No valid site ID info found.\n");
		return(NULL);
  }
	if (radar_verbose_flag)
	  fprintf(stderr,"SITE: %c%c%c%c\n", sitep->name[0], sitep->name[1],
			 sitep->name[2], sitep->name[3]);*/

  
  memset(&wsr88d_sweep, 0, sizeof(Wsr88d_sweep)); /* Initialize to 0 a 
												   * heavily used variable.
												   */

/* 1. Open the input wsr88d file. */
  if (infile == NULL) the_file = "stdin";  /* wsr88d.c understands this to
											* mean read from stdin.
											*/
  else the_file = infile;

  if ((wf = wsr88d_open(the_file)) == NULL) {
	wsr88d_perror(the_file);
	return NULL;
  }

  nvolumes = 3;
  radar = RSL_new_radar(MAX_RADAR_VOLUMES); /* Allocate all Volume pointers. */
  if (radar == NULL) return NULL;



/* 2. Read wsr88d headers. */
  /* Return # bytes, 0 or neg. on fail. */
  n = wsr88d_read_file_header(wf, &wsr88d_file_header);
  if (n <= 0 ||
	  (strncmp(wsr88d_file_header.title.filename, "ARCHIVE2.", 9) != 0 &&
	   strncmp(wsr88d_file_header.title.filename, "AR2V000", 7) != 0))
  {
	wsr88d_close(wf);
	free(radar);
	return NULL;
  }


  if (radar_verbose_flag)
	print_head(wsr88d_file_header);

/* Clear the sweep pointers. */
  clear_sweep(&wsr88d_sweep, 0, MAX_RAYS_IN_SWEEP);

/* Allocate a maximum of 30 sweeps for the volume. */
/* Order is important.  WSR88D_DZ, WSR88D_VR, WSR88D_SW, is 
 * assigned to the indexes DZ_INDEX, VR_INDEX and SW_INDEX respectively.
 */ 

  for (iv=0; iv<nvolumes; iv++)
	if (rsl_qfield[iv]) radar->v[iv] = RSL_new_volume(20);


/* LOOP until EOF */
  nsweep = 0;
  for (;(n = wsr88d_read_sweep(wf, &wsr88d_sweep)) > 0; nsweep++) {
	if (rsl_qsweep != NULL) {
	  if (nsweep > rsl_qsweep_max) break;
	  if (rsl_qsweep[nsweep] == 0) continue;
	}
	if (radar_verbose_flag)  
	fprintf(stderr,"Processing for SWEEP # %d\n", nsweep);

	  /*  wsr88d_print_sweep_info(&wsr88d_sweep); */
	
	for (iv=0; iv<nvolumes; iv++) {
	  if (rsl_qfield[iv]) {
		if (nsweep >= radar->v[iv]->h.nsweeps) { /* Exceeded sweep limit.
												  * Allocate more sweeps.
												  * Copy all previous sweeps.
												  */
		  if (radar_verbose_flag)
			fprintf(stderr,"Exceeded sweep allocation of %d. Adding 20 more.\n", nsweep);
		  new_volume = RSL_new_volume(radar->v[iv]->h.nsweeps+20);
		  new_volume = copy_sweeps_into_volume(new_volume, radar->v[iv]);
		  radar->v[iv] = new_volume;
		}
		if (wsr88d_load_sweep_into_volume(wsr88d_sweep,
										  radar->v[iv], nsweep,
										  volume_mask[iv] ) != 0) {
		  RSL_free_radar(radar);
		  return NULL;
		}
	  }
	}
	free_and_clear_sweep(&wsr88d_sweep, 0, MAX_RAYS_IN_SWEEP);
  }

  for (iv=0; iv<nvolumes; iv++) {
	if (rsl_qfield[iv]) {
	  radar->v[iv]->h.type_str = strdup(field_str[iv]);
	  radar->v[iv]->h.nsweeps = nsweep;
	}
  }
  wsr88d_close(wf);

/*
 * Here we will assign the Radar_header information.  Take most of it
 * from an existing volume's header.  
 */
  /* set the sitep info from level2 info, message31 if present (TODO) */
  if ( sitep == NULL ) {
     float mess31_lat, mess31_lon, mess31_elev;
     float dms;
     wsr88d_get_station_id(site_id_str, &mess31_lat, &mess31_lon, &mess31_elev);
     sitep = wsr88d_get_site(site_id_str);
     mess31_lon = mess31_lon * -1.0;
     if ( sitep == NULL ) {
        printf("setting site information to ID %s lat %f lon %f elev %f\n",
		site_id_str, mess31_lat, mess31_lon, mess31_elev);
        sitep = (Wsr88d_site_info *)malloc(sizeof(Wsr88d_site_info));
        sitep->number = -9999;
        sprintf(sitep->name,"%s\0",site_id_str); 
        sprintf(sitep->city,"--\0");
        sprintf(sitep->state,"--\0");
        dms = fabs(mess31_lat);
        sitep->latd = floor(dms);
        dms = (dms - (float)sitep->latd)*60;
        sitep->latm = floor(dms);
        dms = (dms - (float)sitep->latm)*60;
        sitep->lats = floor(dms);
        dms = fabs(mess31_lon);
        sitep->lond = floor(dms);
        dms = (dms - (float)sitep->lond)*60;
        sitep->lonm = floor(dms);
        dms = (dms - (float)sitep->lonm)*60;
        sitep->lons = floor(dms);
        if ( mess31_lat < 0 ) {
           sitep->latd *= -1;
           sitep->latm *= -1;
           sitep->lats *= -1;
        }
        if ( mess31_lon < 0 ) {
           sitep->lond *= -1;
           sitep->lonm *= -1;
           sitep->lons *= -1;
        }
        sitep->height = (int)mess31_elev;
        sitep->bwidth = -9999;
	sitep->spulse = 1530;
        sitep->lpulse = 4630;
        
     }
  }

  radar_load_date_time(radar);  /* Magic :-) */

	radar->h.number = sitep->number;
	memcpy(&radar->h.name, sitep->name, sizeof(sitep->name));
	memcpy(&radar->h.radar_name, sitep->name, sizeof(sitep->name)); /* Redundant */
	memcpy(&radar->h.city, sitep->city, sizeof(sitep->city));
	memcpy(&radar->h.state, sitep->state, sizeof(sitep->state));
	strcpy(radar->h.radar_type, "wsr88d");
	radar->h.latd = sitep->latd;
	radar->h.latm = sitep->latm;
	radar->h.lats = sitep->lats;
	if (radar->h.latd < 0) { /* Degree/min/sec  all the same sign */
	  radar->h.latm *= -1;
	  radar->h.lats *= -1;
	}
	radar->h.lond = sitep->lond;
	radar->h.lonm = sitep->lonm;
	radar->h.lons = sitep->lons;
	if (radar->h.lond < 0) { /* Degree/min/sec  all the same sign */
	  radar->h.lonm *= -1;
	  radar->h.lons *= -1;
	}
	radar->h.height = sitep->height;
	radar->h.spulse = sitep->spulse;
	radar->h.lpulse = sitep->lpulse;

	free(sitep);
		
  radar = RSL_prune_radar(radar);
  return radar;
}
