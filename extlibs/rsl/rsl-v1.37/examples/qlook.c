#define TRUE 1
#define FALSE 0
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "rsl.h"

#define ZDR_WIDTH 10
int         verbose;

void rebin_ray(Ray *r, int width);
void rebin_sweep(Sweep *s, int width);
void rebin_volume(Volume *v, int width);
void adjust_gate_size(Radar *radar, float gate_size_adjustment);

void process_args(int argc, char **argv, char *in_file, int *verbose,
				  char *site_id, char *tape_id,
				  int *qc_reflectivity, int *total_reflectivity,
				  int *differential_reflectivity, 
				  int *velocity, int *spectral_width,
				  int *make_gif, int *make_pgm, int *make_bscan, int *make_uf,
				  int *num_sweeps, float *dbz_offset,
				  int *xdim, int *ydim, float *range,
				  float *gate_size_adjustment, int *print_azim);

/***********************************************************************/
/* This program uses the NASA TRMM Office Radar Software Library (RSL) */
/***********************************************************************/
/*** DBW ***************************************************************/
/***********************************************************************/
main(int argc, char **argv) {

	Radar       *radar;
	Volume		*dz_volume, *vr_volume;
	Volume      *dr_volume, *zt_volume;
	Volume		*sw_volume, *qc_volume, *volume;

	Sweep       *sweep;
	Sweep 		*dz_sweep, *qc_sweep;
	Sweep       *dr_sweep, *zt_sweep;
	Sweep       *vr_sweep, *sw_sweep;

	Ray         *ray;
	int         reflectivity, qc_reflectivity, nvolumes;
	int         differential_reflectivity, total_reflectivity;
	int         velocity, spectral_width;
	int         make_catalog, make_gif, make_pgm; 
	int         make_uf, make_bscan;
	int         xdim, ydim, num_sweeps;
	int 		i,j,k,l,n, verbose, kk;
	int			month, day, year, hour, min;
	int         ncbins, width;
	int	    print_azim;
	float       scale, dbz_offset;
	float       latitude, longitude;

	float		sec;
	float       maxr;
	float       nyquist, max_range, gate_size_adjustment;

	char        tape_id[100];
	char		in_file[100], site_id[100];
	char		filename[100], outfile[100], nexfile[100];
	char        command[100], file_prefix[100], file_suffix[3];
	char		dir_string[100], red[120], grn[120], blu[120];
	char        time_string[14], site_string[10],img_base[20];
	FILE		*fp;

/* Set default values */

	strcpy(site_id, "KMLB");
	strcpy(tape_id, "WSR88D");

	verbose         = FALSE;
	reflectivity    = TRUE;
	qc_reflectivity = FALSE;
	total_reflectivity = FALSE;
	differential_reflectivity = FALSE;
	velocity        = FALSE;
	spectral_width  = FALSE;
	make_gif        = FALSE;
	make_pgm        = FALSE;
	make_catalog    = FALSE;
	make_uf         = FALSE;
	print_azim      = FALSE;
	num_sweeps      = 1;
	xdim = ydim     = 400;
	maxr            = 200.;
	dbz_offset      = 0.0;
	gate_size_adjustment = 1.0;
	
/* Process command_line arguments */

	process_args(argc, argv, in_file, &verbose, 
				 site_id, tape_id, 
				 &qc_reflectivity, &total_reflectivity,
				 &differential_reflectivity, 
				 &velocity, &spectral_width,
				 &make_gif, &make_pgm, &make_bscan, &make_uf,  
				 &num_sweeps, &dbz_offset,
				 &xdim, &ydim, &maxr, &gate_size_adjustment,
				 &print_azim);
	
/* Be a chatty Kathy? */

    if(verbose)
       RSL_radar_verbose_on();
    else
       RSL_radar_verbose_off(); 

/* Read data into radar */

	if(verbose) printf("Calling any_format_to_radar\n");
	radar = RSL_anyformat_to_radar(in_file, site_id);
	if(verbose) printf("Called any_format_to_radar\n");
	if(radar==NULL) {
	  if (verbose)
		printf("No radar loaded, bye\n");
	  exit(-1);
	}

/* Print command line parameters */

	if(verbose) {
		printf("Site ID            = %s\n",site_id);
		printf("Tape ID            = %s\n",tape_id);
		printf("Do reflectivity    = %d\n",reflectivity);
		printf("Do qc_reflectivity = %d\n",qc_reflectivity);
		printf("Do differential_reflectivity = %d\n",
			   differential_reflectivity);
		printf("Do total_reflectivity = %d\n",
			   total_reflectivity);
		printf("Do qc_reflectivity = %d\n",qc_reflectivity);
		printf("Do velocity        = %d\n",velocity);
		printf("Do spectral_width  = %d\n",spectral_width);
		printf("Make gif           = %d\n",make_gif);
		printf("Make pgm           = %d\n",make_pgm);
		printf("Make UF file       = %d\n",make_uf);
		printf("dBZ Offset         = %.2f\n",dbz_offset);
		printf("Gate Size Adjust   = %.2f\n",gate_size_adjustment);
		printf("Print Azimuths     = %d\n",print_azim);
	} 


/*
  If Gate Size Adjustment is not unity, then we must change the
  following:
      old_gs = radar->v[i]->sweep[sweepIndex]->ray[rayIndex=]->h.gate_size
	  radar->v[i]->sweep[sweepIndex]->ray[rayIndex=]->h.gate_size = 
	      old_gs*gate_size_adjustment

   Here are some comments from Sandra Yuter on the necessity of this fix.
   > I dug into the revelant code and it looks like we can do a relatively
	> simple workaround for the SIGMET raw product file range bin size
	> errors for the RHB data pulses widths of 0.5 usec and 2.0 usec as follows.
	> 
	> Since we are all converting from sigmet to UF I suggest we resize 
	> the range bin size values in the ray headers in the qlook step
	> where the sigmet to UF conversion occurs.
	> 
	> The resize requires only 1 additional line of code (I have included
	> a few others for context) in qlook.c
	> 
	> rangeToFirstGate = 0.001 *
	> 	         radar->v[i]->sweep[sweepIndex]->ray[rayIndex]->h.range_bin1;
	> 	      gateSize = 0.001 *
	> 	         radar->v[i]->sweep[sweepIndex]->ray[rayIndex]->h.gate_size;
	> 	      radar->v[i]->sweep[sweepIndex]->ray[rayIndex]->h.gate_size=
	> 		gateSize*0.6*1000;
	> 
	> I have used 0.6 adjustment factor since that is 75/125 which corresponds
	> to the error in my 0.5 usec data, for the SUR scans, this adjustment
	> factor is 133.33/125 or 1.067.
	
	The following is from Joe Holmes from SIGMET
	
	> 
	> I think you have experienced a problem with the RVP7 range resolution
	> configuration.  Both in IRIS and in the RVP7 you manually type in
	> the range resolution.  The RVP7 allows a separate resolution for
	> each pulsewidth, while IRIS only allows 1 value.  There is no feedback
	> if these values are not typed in the same.  Based on setup information
	> we have here from the RH Brown from Oct 23, 1998, you had the following
	> configuration:
	> 
	> RVP7:
	> 0   0.50 usec   75.0m
	> 1   0.80 usec  125.0m
	> 2   1.39 usec  125.0m
	> 3   2.00 usec  133.3m
	> 
	> IRIS: 125.0 meters
	> 
	> I think the error at PW#0 was corrected a while back, but
	> the error  in PW#3 was never corrected.  Next time someone is
	> at the ship, they should check this, fix the long pulse, and remake
	> the bandpass filter for the long pulse.
	> 
	> In the short term, you can correct the error by taking all your
	> long pulse data and changing the header to correctly document the
	> range resolution.  We have a program to do this, it is called "change_raw".
	> The source is on any IRIS system, which was installed with the
	> source, headers, and objects turned on.  It is in the
	> ${IRIS_ROOT}utils/examples directory.  We can supply you with
	> a compiled version of this program, if you want.  Available platforms
	> are Linux, HP-UX, and IRIX.

*/
	  
	if(gate_size_adjustment != 1.0) {
	  printf("Adjusting Gate Size by Factor: %.3f\n",gate_size_adjustment);
	  adjust_gate_size(radar, gate_size_adjustment);
	}
	
 /*
   Create the filename prefix. Consists of the Site ID,
   and time string (YYMMDD_hhmm).  The file suffix is 
   like MIME type (e.g, .gif, .pgm, .uf, etc.)
*/
	sprintf(time_string,"%4d/%2.2d%2.2d %2.2d:%2.2d UTC", 
		    radar->h.year, radar->h.month, radar->h.day, 
		    radar->h.hour, radar->h.minute);
/*
  Determine the location (lat/lon) of the radar.
 */
	latitude = radar->h.latd + radar->h.latm/60. + radar->h.lats/3600.;
	longitude = radar->h.lond + radar->h.lonm/60. + radar->h.lons/3600.;

	printf("%s %s %s %.6f %.6f \n",
	       in_file, radar->h.radar_name, time_string, longitude, latitude);

	sprintf(time_string,"%4d_%2.2d%2.2d_%2.2d%2.2d", 
		    radar->h.year, radar->h.month, radar->h.day, 
		    radar->h.hour, radar->h.minute);

/* 
   Print the radar/volume info.
*/

/*
 * DZ     Reflectivity (dBZ), may contain some   DZ_INDEX
 *        signal-processor level QC and/or      
 *        filters. This field would contain 
 *        Darwin's CZ, or WSR88D's standard 
 *        reflectivity. In other words, unless
 *        the field is described otherwise, it
 *        should always go here. In essence, this
 *        is the "cleanest" reflectivity field
 *        for a radar.
 *
 * VR     Radial Velocity (m/s)                  VR_INDEX
 *
 * SW     Spectral Width (m2/s2)                 SW_INDEX
 *
 * CZ     QC Reflectivity (dBZ), contains
 *        post-processed QC'd data               CZ_INDEX
 *
 * ZT     Total Reflectivity (dBZ)               ZT_INDEX
 *        May be uncommon, but important
 *
 * DR     Differential reflectivity              DR_INDEX
 *        DR and LR are for dual-polarization
 *        radars only. Unitless or in dB.
 *
 * LR     Another form of differential ref       LR_INDEX
 *        called LDR, not sure of units
 *
 * ZD     ZDR: Reflectivity Depolarization Ratio ZD_INDEX
 *        ZDR = 10log(ZH/ZV)  (dB)
 *
 * DM     Received power measured by the radar.  DM_INDEX
 *        Units are dBm.
 *
 * RH     Rho : Correlation coefficient          RH_INDEX
 *
 * PH     Phi (MCTEX parameter)                  PH_INDEX
 *
 * XZ     X-band reflectivity                    XZ_INDEX
 *
 * CR     Corrected DR reflectivity (differential) CR_INDEX
 *
 * MZ     DZ mask volume for HDF 1C-51 product.  MZ_INDEX
 *
 * MR     DR mask volume for HDF 1C-51 product.  MR_INDEX
 *
 * ZE     Edited Reflectivity.                   ZE_INDEX
 *
 * VE     Edited Velocity.                       VE_INDEX
 *
 *
                      * 0 = DZ_INDEX = reflectivity.
                      * 1 = VR_INDEX = velocity.
                      * 2 = SW_INDEX = spectrum_width.
                      * 3 = CZ_INDEX = corrected reflectivity.
                      * 4 = ZT_INDEX = uncorrected reflectivity.
                      * 5 = DR_INDEX = differential refl.
                      * 6 = LR_INDEX = another differential refl.
                      * 7 = ZD_INDEX = another differential refl.
                      * 8 = DM_INDEX = received power.
                      * 9 = RH_INDEX = RhoHV: Horz-Vert power corr coeff
                      *10 = PH_INDEX = PhiDP: Differential phase angle
                      *11 = XZ_INDEX = X-band reflectivity.
                      *12 = CR_INDEX = Corrected DR.
                      *13 = MZ_INDEX = DZ mask for 1C-51 HDF.
                      *14 = MR_INDEX = DR mask for 1C-51 HDF.
                      *15 = ZE_INDEX = Edited reflectivity.
                      *16 = VE_INDEX = Edited velocity.
                      *17 = KD_INDEX = KDP deg/km.
                      *18 = TI_INDEX = TIME (unknown)  for MCTEX data.
                      *19 = DX_INDEX
                      *20 = CH_INDEX
                      *21 = AH_INDEX
                      *22 = CV_INDEX
                      *23 = AV_INDEX
*/


	if(verbose) {
	  for(i=0; i< radar->h.nvolumes; i++) {
	    if(radar->v[i] != NULL) {
	      printf("Vol[%2.2d] has %d sweeps\n",i,radar->v[i]->h.nsweeps);
	    } else {
	      printf("Vol[%2.2d] == NULL\n",i);
	    }
	  }
	  printf("--------------------------------------------\n");
	  printf("Number of volumes in radar: %d\n",radar->h.nvolumes);
	}
	
	/* DZ_INDEX */
	if(radar->v[DZ_INDEX] == NULL) {
		printf("DZ_INDEX == NULL\n");
		reflectivity = FALSE;
	} else {
		dz_volume = radar->v[DZ_INDEX];
		if(verbose) printf("Number of sweeps in dz_volume = %d\n",
			   dz_volume->h.nsweeps);
	}
	
	/* CZ_INDEX */
	if(radar->v[CZ_INDEX] == NULL) {
		if(verbose) printf("CZ_INDEX == NULL\n");
		qc_reflectivity = FALSE;
	} else {
		qc_volume = radar->v[CZ_INDEX];
		if(verbose) printf("Number of sweeps in qc_volume = %d\n",
			   qc_volume->h.nsweeps);
	}
	
	/* ZT_INDEX */
	if(radar->v[ZT_INDEX] == NULL) {
		if(verbose) printf("ZT_INDEX == NULL\n");
		total_reflectivity = FALSE;
	} else {
		zt_volume = radar->v[ZT_INDEX];
		if(verbose) printf("Number of sweeps in zt_volume = %d\n", 
			   zt_volume->h.nsweeps);
	}
	/* ZD_INDEX */
	if(radar->v[ZD_INDEX] == NULL) {
		if(verbose) printf("ZD_INDEX == NULL\n");
		differential_reflectivity = FALSE;
	} else {
		dr_volume = radar->v[ZD_INDEX];
		if(verbose) printf("Number of sweeps in dr_volume = %d\n", 
			   dr_volume->h.nsweeps);
	}
	/* VR_INDEX */
	if(radar->v[VR_INDEX] == NULL) {
		if(verbose) printf("VR_INDEX == NULL\n");
		velocity = FALSE;
	} else {
		vr_volume = radar->v[VR_INDEX];
		if(verbose) printf("Number of sweeps in vr_volume = %d\n",
			   vr_volume->h.nsweeps);
	}
		
	/* SW_INDEX */
	if(radar->v[SW_INDEX] == NULL) {
		if(verbose) printf("SW_INDEX == NULL\n");
		spectral_width = FALSE;
	} else {
		sw_volume = radar->v[SW_INDEX];
		if(verbose) printf("Number of sweeps in sw_volume = %d\n",
			   sw_volume->h.nsweeps);
	}
	if(verbose) printf("--------------------------------------------\n");

/*
   Print out the elevation angles
*/
 	if(verbose) {
	  if(reflectivity) {
	    printf("Reflectivity Tilts\n");
	    printf("----------------------\n");
	    if(dz_volume != NULL) {
	      for(i=0; i<dz_volume->h.nsweeps; i++) {
		sweep = dz_volume->sweep[i];
		if(sweep == NULL) {
		  printf("sweep[%d]==NULL\n",i);
		  continue;
		}
		printf("Tilt %2d, Elev=%4.1f\n",i,sweep->h.elev);
	      }
	      printf("----------------------\n");
	    }
	  }
	}
	  /*
	    Print out the values of the rays in each sweep requsted
	  */
	  
	if(print_azim) {
	  printf("Ray angles\n");
	  if(reflectivity) {
	    for(i=0; i<dz_volume->h.nsweeps; i++) {
	      if(dz_volume->sweep[i] != NULL) sweep = dz_volume->sweep[i];
	      printf("Elevation angle: %f\n",sweep->h.elev);
	      printf("Number of rays in sweep[%d] = %d\n",i,sweep->h.nrays);
	      
	      for(j=0; j<sweep->h.nrays-1; j++) {
		if(sweep->ray[j] != NULL) {
		  ray = sweep->ray[j];
		  printf("%d: %7.2f\n  ",j,sweep->ray[j]->h.azimuth);
		}
	      }
	    }
	  }
	}
/* 
   Write out some volume statistics
*/
	if(verbose) {
	  printf("******************* Volume Statistics *******************\n");
	  if(reflectivity) {
		sweep = RSL_get_first_sweep_of_volume(dz_volume);
		if(sweep != NULL) {
		  printf("Number rays        = %d\n", sweep->h.nrays);
		  printf("Beam width         = %.2f deg\n", sweep->h.beam_width);
		  ray = RSL_get_first_ray_of_sweep(sweep);
		  if(ray!= NULL) {
			max_range = ray->h.range_bin1/1000.0 + 
			  ray->h.nbins*ray->h.gate_size/1000.0;
			printf("Number of bins     = %d\n",ray->h.nbins);
			printf("Max range          = %.1f km\n", max_range);
			printf("Range to first bin = %d m\n",ray->h.range_bin1);
			printf("Gate size          = %d m\n",ray->h.gate_size);
			printf("Pulse Rep. Freq.   = %d Hz\n",ray->h.prf);
			printf("Pulse width        = %.2f us\n",ray->h.pulse_width);
			printf("Wavelength         = %.2f m\n",ray->h.wavelength);
			printf("Frequency          = %.2f \n",ray->h.frequency);
		  }
		}
	  }
	}

/*  
	Add a dBZ offset if requested. The offset can be positive or negative.
	if(dbz_offset != 0.0) {
	  printf("Adding dbz_offset to dz_volume: %.2f\n", dbz_offset);
	  RSL_add_dbzoffset_to_volume(dz_volume, dbz_offset);
	  printf("Added dbz_offset to dz_volume: %.2f\n", dbz_offset);
	  exit(0);
	}
*/

/* 
 ****************************************************************
 *  Make images                                                 *
 ****************************************************************
*/


	/* CZ_INDEX */
 	if(qc_reflectivity) {
	  if(verbose) printf("Loading refl colortable...\n");
	  RSL_load_refl_color_table();
	  for(i=0; i<num_sweeps; i++) {
	    sweep = qc_volume->sweep[i];
	    if(sweep == NULL) {
	      printf("sweep[%d]==NULL\n",i);
	      continue;
	    }
	    if(make_pgm) {
	      sprintf(file_suffix,"pgm");
	      sprintf(filename,"qc_%s_%2.2d.%s", time_string,i,file_suffix);
	      printf("Creating: %s\n", filename);
	      RSL_sweep_to_pgm(sweep, filename, xdim, ydim, maxr);
	    }
	    if(make_gif) {
	      sprintf(file_suffix,"gif");
	      sprintf(filename,"qc_%s_%2.2d.%s", time_string,i,file_suffix);
	      printf("Creating: %s\n", filename);
	      RSL_sweep_to_gif(sweep,filename,xdim, ydim, maxr);
	    }
	  }
	}

	/* DZ_INDEX */
	if(reflectivity) {
	  if(verbose) printf("Loading refl colortable...\n");
	  RSL_load_refl_color_table();
	  for(i=0; i<num_sweeps; i++) {
	    sweep = dz_volume->sweep[i];
	    if(sweep == NULL) {
	      printf("sweep[%d]==NULL\n",i);
	      continue;
	    }
	    if(make_pgm) {
	      sprintf(file_suffix,"pgm");
	      sprintf(filename,"dz_%s_%2.2d.%s", 
		      time_string,i,file_suffix);
	      printf("Creating: %s\n", filename);
	      RSL_sweep_to_pgm(sweep, filename, xdim, ydim, maxr);
	    }
	    if(make_gif) {
	      sprintf(file_suffix,"gif");
	      sprintf(filename,"dz_%s_%2.2d.%s", time_string,i,file_suffix); 
/*		  
		  sprintf(filename,"dz_%s.%s.%s", time_string,in_file,file_suffix); 
*/
	      printf("Creating: %s\n", filename);
	      RSL_sweep_to_gif(sweep,filename,xdim, ydim, maxr);
	    }
	  }
	}
	
	/* ZT_INDEX */
	if(total_reflectivity) {
	  if(verbose) printf("Loading refl colortable...\n");
	  RSL_load_refl_color_table();
	  for(i=0; i<num_sweeps; i++) {
		sweep = zt_volume->sweep[i];
		if(sweep == NULL) {
		  printf("sweep[%d]==NULL\n",i);
		  continue;
		}
		if(make_pgm) {
		  sprintf(file_suffix,"pgm");
		  sprintf(filename,"zt_%s_%2.2d.%s", 
				  time_string,i,file_suffix);
		  printf("Creating: %s\n", filename);
		  RSL_sweep_to_pgm(sweep, filename, xdim, ydim, maxr);
		}
		if(make_gif) {
		  sprintf(file_suffix,"gif");
		  sprintf(filename,"zt_%s_%2.2d.%s", 
				  time_string,i,file_suffix);
		  printf("Creating: %s\n", filename);
		  RSL_sweep_to_gif(sweep,filename,xdim, ydim, maxr);
		}
	  }
	}
	
	/* DR_INDEX */
	if(differential_reflectivity) {
		scale = 0.5;
		ncbins = 21;
		width = 10;
		printf("Calling RSL_rebin, %d %d %.2f\n", width);
		RSL_rebin_volume(dr_volume, width);
		if(verbose) printf("Loading zdr colortable...\n");
		RSL_load_zdr_color_table(); 
		for(i=0; i<num_sweeps; i++) {
		  sweep = dr_volume->sweep[i];
		  if(sweep == NULL) {
			printf("sweep[%d]==NULL\n",i);
			continue;
		  }
		  if(make_pgm) {
			sprintf(file_suffix,"pgm");
			sprintf(filename,"dr_%s_%2.2d.%s", 
					time_string,i,file_suffix);
			printf("Creating: %s\n", filename);
			RSL_sweep_to_pgm(sweep, filename, xdim, ydim, maxr);
		  }
		  if(make_gif) {
			sprintf(file_suffix,"gif");
			sprintf(filename,"dr_%s_%2.2d.%s", 
					time_string,i,file_suffix);
			printf("Creating: %s\n", filename);
			RSL_sweep_to_gif(sweep,filename,xdim, ydim, maxr);
		  }
		}
	}

	
	/* VR_INDEX */
 	if(velocity) {
	  if(verbose) printf("Loading vel colortable...\n");
	  RSL_load_vel_color_table();
	  for(i=0; i<num_sweeps; i++) {
		sweep = vr_volume->sweep[i];
		if(sweep == NULL) {
		  printf("sweep[%d]==NULL\n",i);
		  continue;
		}
		if(make_pgm) {
		  sprintf(file_suffix,"pgm");
		  sprintf(filename,"vr_%s_%2.2d.%s", time_string,i,file_suffix);
		  printf("Creating: %s\n", filename);
		  RSL_sweep_to_pgm(sweep, filename, xdim, ydim, maxr);
		}
		if(make_gif) {
		  sprintf(file_suffix,"gif");
		  sprintf(filename,"vr_%s_%2.2d.%s", time_string,i,file_suffix);
		  printf("Creating: %s\n", filename);
		  RSL_sweep_to_gif(sweep,filename,xdim, ydim, maxr);
		}
	  }
	}
	
	/* SW_INDEX */
 	if(spectral_width) {
	  if(verbose) printf("Loading sw colortable...\n");
	  RSL_load_sw_color_table();
	  for(i=0; i<num_sweeps; i++) {
		sweep = sw_volume->sweep[i];
		if(sweep == NULL) {
		  printf("sweep[%d]==NULL\n",i);
		  continue;
		}
		if(make_pgm) {
		  sprintf(file_suffix,"pgm");
		  sprintf(filename,"sw_%s_%2.2d.%s", 
				  time_string,i,file_suffix);
		  printf("Creating: %s\n", filename);
		  RSL_sweep_to_pgm(sweep, filename, xdim, ydim, maxr);
		}
		if(make_gif) {
		  sprintf(file_suffix,"gif");
		  sprintf(filename,"sw_%s_%2.2d.%s", 
				  time_string,i,file_suffix);
		  printf("Creating: %s\n", filename);
		  RSL_sweep_to_gif(sweep,filename,xdim, ydim, maxr);
		}
	  }
	}
	
/*
   Write uf file if requested
*/
 	if(make_uf) {
		sprintf(file_suffix,"uf.gz");
		sprintf(filename,"%s_%s.%s",site_id, time_string,file_suffix);
		printf("Creating UF file: %s\n", filename);
		RSL_radar_to_uf_gzip(radar, filename);
	}

	printf("-->> FIELDS: [ ");
	if(radar->v[0] != NULL) printf("DZ ");
	if(radar->v[1] != NULL) printf("VR ");
	if(radar->v[2] != NULL) printf("SW ");
	if(radar->v[3] != NULL) printf("CZ ");
	if(radar->v[4] != NULL) printf("ZT ");
	if(radar->v[5] != NULL) printf("DR ");
	if(radar->v[6] != NULL) printf("LR ");
	if(radar->v[7] != NULL) printf("ZD ");
	if(radar->v[8] != NULL) printf("DM ");
	if(radar->v[9] != NULL) printf("RH ");
	if(radar->v[10] != NULL) printf("PH ");
	if(radar->v[11] != NULL) printf("XZ ");
	if(radar->v[12] != NULL) printf("CR ");
	if(radar->v[13] != NULL) printf("MZ ");
	if(radar->v[14] != NULL) printf("MR ");
	if(radar->v[15] != NULL) printf("ZE ");
	if(radar->v[16] != NULL) printf("VE ");
	if(radar->v[17] != NULL) printf("KD ");
	if(radar->v[18] != NULL) printf("TI ");
	if(radar->v[19] != NULL) printf("DX ");
	if(radar->v[20] != NULL) printf("CH ");
	if(radar->v[21] != NULL) printf("AH ");
	if(radar->v[22] != NULL) printf("CV ");
	if(radar->v[23] != NULL) printf("AV ");
	if(radar->v[24] != NULL) printf("SQ ");
	printf("] \n\n");
/*
   Wrap it up!
*/

    if(verbose)
	    printf("Finished!\n");
    exit (0);

} /* End of main */

