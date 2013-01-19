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
/*
 *------------------------------------------------------------
 * v1.14 5/12/95
 *------------------------------------------------------------
 *  Procedures:
 *   wsr88d_open
 *   wsr88d_close
 *   wsr88d_read_file_header
 *   wsr88d_read_tape_header
 *   wsr88d_read_sweep
 *   wsr88d_read_ray
 *   wsr88d_perror
 *   wsr88d_ray_to_float
 *
 *  Functions:
 *   wsr88d_get_nyquist
 *   wsr88d_get_atmos_atten_factor
 *   wsr88d_get_velocity_resolution
 *   wsr88d_get_volume_coverage
 *   wsr88d_get_elevation_angle
 *   wsr88d_get_azimuth
 *   wsr88d_get_range
 *   wsr88d_get_data
 *   wsr88d_get_time
 *   wsr88d_get_vcp_info(Wsr88d_ray *ray,int el_num)
 *   wsr88d_get_fix_angle(Wsr88d_ray *ray)
 *   wsr88d_get_pulse_count(Wsr88d_ray *ray)
 *   wsr88d_get_azimuth_rate(Wsr88d_ray *ray)
 *   wsr88d_get_pulse_width(Wsr88d_ray *ray)
 *   wsr88d_get_prt(Wsr88d_ray *ray)
 *   wsr88d_get_prf(Wsr88d_ray *ray)
 *   wsr88d_get_wavelength(Wsr88d_ray *ray)
 *   wsr88d_get_frequency(Wsr88d_ray *ray)
 * 
 *  Misc. routines: (canidates for possible inclusion into the library)
 *   print_head
 *   print_packet_info
 *   free_and_clear_sweep
 *   clear_sweep
 *   print_sweep_info
 *   Cvt_date                    <- From Dan Austin
 *   Cvt_time                    <- From Dan Austin
 *
 */ 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "wsr88d.h"

#include "bzlib.h"

static int little_endian(void)
{
  union {
    unsigned char byte[4];
    int val;
  } word;
  word.val = 0;
  word.byte[3] = 0x1;
  return word.val != 1;
}


static void swap_4_bytes(void *word)
{
  unsigned char *byte;
  unsigned char temp;
  byte    = word;
  temp    = byte[0];
  byte[0] = byte[3];
  byte[3] = temp;
  temp    = byte[1];
  byte[1] = byte[2];
  byte[2] = temp;
}

static void swap_2_bytes(void *word)
{
  unsigned char *byte;
  unsigned char temp;
  byte    = word;
  temp    = byte[0];
  byte[0] = byte[1];
  byte[1] = temp;
}

static void wsr88d_read_bytes(void *output, void *source, int nbytes)
{
  memcpy(output, source, nbytes);
  if ( little_endian() ) {
     switch ( nbytes  ) {
	  case 2:
		  swap_2_bytes(output);
		  break;
	  case 4:
		  swap_4_bytes(output);
		  break;
     }
  }

}
  
/**********************************************************************/
/*   D E B U G G I N G     R O U T I N E S    F O L L O W             */
/**********************************************************************/

extern int radar_verbose_flag;

/************************************************
 * Cvt_date-  convert the date in days since 1/1/70 (Julian) to mm/dd/yy 
 * parameters:
 * long int date_in - input julian date
 * returns: output date 
 * calls from: Cvt_pckt_hdr
 * calls to: none
 ************************************************/

#include <time.h>
int Cvt_date(long int date_in)
{
  int mm, dd, yy;
  time_t itime;
  struct tm *tm_time;
  itime = date_in - 1;
  itime *= 24*60*60; /* Seconds/day * days. */

  tm_time = gmtime(&itime);
  mm = tm_time->tm_mon+1;
  dd = tm_time->tm_mday;
  yy = tm_time->tm_year;

  return 10000.0*yy+100.0*mm+dd;
}

/************************************************
 * Cvt_time- converts 24 hr time in msecs after midnight to hhmmss
 * parameters:
 * long int time_in - input time in msecs after midnight
 * returns: double *time_out - output time
 * calls from: Cvt_pckt_hdr
 * calls to: none
 ************************************************/
float Cvt_time(long int time_in)
{
	double t;
	int hh,mm;

	t = time_in;
    t /= 1000.0;
    hh = t/3600;
    t -= hh*3600;
    mm = t/60;
    t -= mm*60;
	
	return hh*10000 + mm*100 + (float) t;
}
/**********************************************************************/
/*                                                                    */
/*  done 2/28        print_head                                       */
/*                                                                    */
/**********************************************************************/
void print_head(Wsr88d_file_header h)
{
  int i;
  fprintf(stderr,"Filename : ");
  for (i=0;i<9;i++) fprintf(stderr,"%c", h.title.filename[i]);   printf("\n");

  fprintf(stderr,"Extension: ");
  for (i=0;i<3;i++) fprintf(stderr,"%c", h.title.ext[i]);   printf("\n");

  fprintf(stderr,"Julian date: %d\n", Cvt_date(h.title.file_date));
  fprintf(stderr,"       time: %f\n", Cvt_time(h.title.file_time));

  
}

void print_packet_info1(Wsr88d_packet1 *p)
{
  if ( ! radar_verbose_flag ) return;

  fprintf(stderr,"fixed packet: %5hd %5hd %5hd %5hd %5hd %5hd %5hd %10.3f %6d\n",
	p->msg_type, p->id_seq, p->azm, p->ray_num,
	p->ray_status, p->elev, p->elev_num,
	Cvt_time((int)p->ray_time), Cvt_date((int)p->ray_date));
}

void print_packet_info31(Wsr88d_packet31 *ray)
{
  char SEC_STR[]="RRAD";
  int i, istart, pdi, pd0;
  short azm_num, data_block_count;
  float azm_angle;

  if ( ray->compression != 0 ) fprintf(stderr,"Have to implement compression within data blocks\n");
  if ( ! radar_verbose_flag ) return;

  fprintf(stderr,"is compressed %d\n",ray->compression);

  wsr88d_read_bytes(&azm_num, &ray->azm_num, 2 );
  wsr88d_read_bytes(&azm_angle, &ray->azm_angle, 4);
  fprintf(stderr,"ray number %d angle %f\n",azm_num,azm_angle);
  fprintf(stderr,"ray sector num %d index_mode %d status %d space %d\n",ray->sector_num,ray->azm_index_mode,ray->ray_status, ray->azm_res_space);
  wsr88d_read_bytes(&data_block_count, &ray->data_block_count, 2);
  fprintf(stderr,"info message31 num blocks %d\n",data_block_count);

  wsr88d_read_bytes(&pd0, &ray->pdata[0], 4);
  for(i=0; i<data_block_count; i++) {
      wsr88d_read_bytes(&pdi, &ray->pdata[i], 4);
      istart = pdi - pd0;
      fprintf(stderr,"look section istart %d sizeoff %d // %c%c%c%c\n",istart,pdi,
	   ray->data[istart+0], ray->data[istart+1], ray->data[istart+2],
		ray->data[istart+3]);
      if ( memcmp( ray->data+istart, SEC_STR, 4 ) == 0 ) {
	    short nyquist;
	    wsr88d_read_bytes(&nyquist, ray->data+istart+16, 2 );
            fprintf(stderr,"data nyquist %hd\n", nyquist);
      }
  }
}

void print_packet_info_vcp(Wsr88d_packet1 *p)
{
int i;
unsigned char *uc=(unsigned char *)p;
short slen, spat, svcp, snc, sangle, sreswidth;
short pulse_surveillance, pulse_doppler, azm_rate;
unsigned char dopvres, pulsewidth;
float fangle;
size_t COFF=28;
int *vcp_array, arr_len;

wsr88d_read_bytes(&slen, uc+COFF, sizeof(short));
wsr88d_read_bytes(&spat, uc+COFF+2, sizeof(short));
wsr88d_read_bytes(&svcp, uc+COFF+4, sizeof(short));
wsr88d_read_bytes(&snc, uc+COFF+6, sizeof(short));
dopvres = uc[COFF+10];
pulsewidth = uc[COFF+11];
wsr88d_read_bytes(&sreswidth, uc+COFF+10, sizeof(short));

if ( radar_verbose_flag )
    fprintf(stderr,"VCP %d definition len %hd pattern %d num count %d dopvres %d pulsewidth %d reswidth %d\n",
	svcp,slen,spat,snc,(int)dopvres,(int)pulsewidth, (int)sreswidth);

arr_len = snc*3+2;
vcp_array = malloc(sizeof(int)*arr_len);
vcp_array[0] = svcp;
vcp_array[1] = sreswidth; /*2 code*1 values combined high/low order */
for ( i=0; i<(int)snc;i++)
   {
   wsr88d_read_bytes(&sangle,uc+COFF+22+(i*46),sizeof(short));
   fangle = (float)( sangle >> 3 ) * 180.0 / 4096.0;

   wsr88d_read_bytes(&pulse_surveillance,uc+COFF+22+(i*46)+6,sizeof(short));
   wsr88d_read_bytes(&azm_rate,uc+COFF+22+(i*46)+8,sizeof(short));
   wsr88d_read_bytes(&pulse_doppler,uc+COFF+22+(i*46)+26,sizeof(short));
   if ( radar_verbose_flag )
      fprintf(stderr,"vcp angle %d %d %f // %d %d // %d\n",i,(int)sangle,fangle,pulse_surveillance,pulse_doppler,azm_rate);
   vcp_array[2+(i*3)] = sangle;
   vcp_array[3+(i*3)] = pulse_surveillance;
   vcp_array[4+(i*3)] = azm_rate;
   }

wsr88d_set_vcp_info ( (int)svcp, arr_len, vcp_array);

}

void print_packet_info(Wsr88d_ray *p)
{
  if ( p->message_type == 1 )
     print_packet_info1(p->type1);
  else if ( p->message_type == 31 )
     print_packet_info31(p->type31);
  else if ( p->message_type == 5 )
     print_packet_info_vcp ( p->type1 );
  else return;
     /*fprintf(stderr,"Have to create packet_info for type %d\n",p->message_type);*/
}




/**********************************************************************/
/* End of debug routines.                                             */
/**********************************************************************/

void free_and_clear_sweep(Wsr88d_sweep *s, int low, int high)
{
/* Frees and sets the ray pointers to NULL.
 * Assumes that rays pointers have been allocated.
 */
  int i;
  for (i=low; i<high; i++)
	if (s->ray[i] != NULL) {
          if ( s->ray[i]->type1 != NULL ) free(s->ray[i]->type1);
          if ( s->ray[i]->type31 != NULL ) {
	     if ( s->ray[i]->type31->data != NULL ) free ( s->ray[i]->type31->data );
             free(s->ray[i]->type31);
	  }
	  free(s->ray[i]);
	  s->ray[i] = NULL;
	}
}

void clear_sweep(Wsr88d_sweep *s, int low, int high)
{
/*
 * Simply set all sweep pointers to NULL.
 */
  int i;
  for (i=low; i<high; i++) {
        /*if ( s->ray[i]->type1 != NULL ) free(s->ray[i]->type1); 
        if ( s->ray[i]->type31 != NULL ) free(s->ray[i]->type31); */
	s->ray[i] = NULL;
  }
}

void wsr88d_print_sweep_info(Wsr88d_sweep *s)
{
  int i;

  fprintf(stderr,"Mtype    ID  azim  ray# rstat  elev elev#       time   date\n");
  fprintf(stderr,"----- ----- ----- ----- ----- ----- ----- ---------- ------\n");

  for (i=0; i<MAX_RAYS_IN_SWEEP; i++) {
	if (s->ray[i] != NULL) 
	  print_packet_info1((Wsr88d_packet1 *) s->ray[i]);
  }
}

/**********************************************************************/
/*                                                                    */
/*  done 2/28             wsr88d_open                                 */
/*                                                                    */
/**********************************************************************/

Wsr88d_file *wsr88d_open(char *filename)
{
  Wsr88d_file *wf = (Wsr88d_file *)malloc(sizeof(Wsr88d_file));
  int save_fd;

  if ( strcmp(filename, "stdin") == 0 ) {
	save_fd = dup(0);
	wf->fptr = fdopen(save_fd,"r");
  } else {
	wf->fptr = fopen(filename, "r");
  }

  if (wf->fptr == NULL) return NULL;
   /* S. Chiswell...The pipe open/close runs out of file descripters on IRIX64 */
  /*wf->fptr = uncompress_pipe(wf->fptr);*/
#define NEW_BUFSIZ 16384
  setvbuf(wf->fptr,NULL,_IOFBF,(size_t)NEW_BUFSIZ); /* Faster i/o? */
  return wf;
}


/**********************************************************************/
/*                                                                    */
/*  done 2/28             wsr88d_perror                               */
/*                                                                    */
/**********************************************************************/
int wsr88d_perror(char *message)
{
/* 
 * I want to use a global 'wsr88d_errno' and
 * have this routine print an appropriate message.
 */

  /* This is a simple model now. */
  fprintf(stderr, "wsr88d_error: ");
  perror(message);
  return 0;
}

/**********************************************************************/
/*                                                                    */
/*  done 2/28             wsr88d_close                                */
/*                                                                    */
/**********************************************************************/
int wsr88d_close(Wsr88d_file *wf)
{
  int rc=0;
  /* S. Chiswell...don't need pclose since uncompress_pipe is commented out above */
  /*rc = rsl_pclose(wf->fptr);*/
  rc = fclose(wf->fptr);
  free(wf);
  return rc;
}


/**********************************************************************/
/*                                                                    */
/*                     wsr88d_swap_file_header                        */
/*                                                                    */
/**********************************************************************/
void wsr88d_swap_file_header(Wsr88d_file_header *header)
{
  swap_4_bytes(&header->title.file_date);
  swap_4_bytes(&header->title.file_time);
}
  
/**********************************************************************/
/*                                                                    */
/*  done 2/28          wsr88d_read_file_header                        */
/*                                                                    */
/**********************************************************************/
int wsr88d_read_file_header(Wsr88d_file *wf,
							Wsr88d_file_header *wsr88d_file_header)
{
  int n;
  n = fread(&wsr88d_file_header->title,
			sizeof(wsr88d_file_header->title), 1, wf->fptr);
  if (little_endian())
	wsr88d_swap_file_header(wsr88d_file_header);
  return n;
}

/**********************************************************************/
/*                                                                    */
/*  done 8/18          wsr88d_read_tape_header                        */
/*                                                                    */
/**********************************************************************/
int wsr88d_read_tape_header(char *first_file,
							Wsr88d_tape_header *wsr88d_tape_header)
{
  FILE *fp;
  int n;
  char c;

  if ((fp = fopen(first_file, "r")) == NULL) {
	perror(first_file);
	return 0;
  }
  
  n = fread(wsr88d_tape_header, sizeof(Wsr88d_tape_header), 1, fp);
  if (n == 0) {
	fprintf(stderr, "WARNING: %s is smaller than 31616 bytes.  It is not a tape header file.\n", first_file);
  }	else {
	/* Try to read one more character.  If we can, then this is not a 
	 * tape header file.  I suppose that we could look for '.' as the
	 * 9-th character and if it were there, then too this is not a tape
	 * header file.
	 */
	if (fread(&c, sizeof(char), 1, fp) > 0) {
	  fprintf(stderr, "WARNING: %s is larger than 31616 bytes.  It is not a tape header file.\n", first_file);
	  memset(wsr88d_tape_header, 0, sizeof(Wsr88d_tape_header));
	  n = 0;
	} else { /* Ok so far. Now check the first 8 bytes for "ARCHIVE2" */
	  if (strncmp(wsr88d_tape_header->archive2, "ARCHIVE2", 8) != 0) {
		fprintf(stderr, "WARNING: %s is 31616 bytes.  However, the first 8 bytes are not 'ARCHIVE2'.\n", first_file);
		memset(wsr88d_tape_header, 0, sizeof(Wsr88d_tape_header));
		n = 0;
	  }
	}
	
  }
  fclose(fp);
  return n;
}



/**********************************************************************/
/*                                                                    */
/*  not done N/A       wsr88d_read_header                             */
/*                                                                    */
/**********************************************************************/
int wsr88d_read_header(Wsr88d_file *wf, Wsr88d_header *wsr88d_header)
{
  fprintf(stderr,"Routine: wsr88d_read_header\n");
  return 0;
}


/**********************************************************************/
/*                                                                    */
/*  done 3/2           wsr88d_read_sweep                              */
/*                                                                    */
/**********************************************************************/
int wsr88d_read_sweep(Wsr88d_file *wf, Wsr88d_sweep *wsr88d_sweep)
{
  int n;
  Wsr88d_ray wsr88d_ray;
  Wsr88d_packet1 ptype1;
	  int nrec;
	  int end_of_volume;
	  int ray_num;

	/* One sweep is defined as staying at the same RDA elevation number. */
	/* We can read the file and check for that, however, we will need to
	 * buffer our input.  The solution is to read the file and check the
	 * radial status.  If it is '2' then we have reached the END OF ELEVATION.
	 * Here is a complete list of radial status codes:
	 *    0 = Start of new elevation.
	 *    1 = Intermediate radial.
	 *    2 = End of elevation.
	 *    3 = Beginning of volume scan.
	 *    4 = End of volume scan.
	 */

	/* Algorithm steps:
	 *  1. Skip packets until.  Start of new elevation or
	 *     Beginning of Volume scan.  STAT=0 or 3.
	 *  2. Read until End of elevation.  STAT=2 or 4.  Skip message type != 1.
	 */

	  nrec = 0;
	  ray_num = 0;
	  wsr88d_ray.type1 = &ptype1;
	  wsr88d_ray.type31 = NULL;
	  n = wsr88d_read_ray(wf, &wsr88d_ray);
	  /*printf("read ray %d type %d\n",n,wsr88d_ray.message_type);*/

	/* Step 1. */
	  while ( n > 0 ) {
		if ( ( wsr88d_ray.message_type == 1 || wsr88d_ray.message_type == 31 ) &&
			( wsr88d_get_status ( &wsr88d_ray ) == 0 || wsr88d_get_status ( &wsr88d_ray ) == 3 ) )
			break;

		/*fprintf(stderr,"SKIPPING packet: type %d, radial status %d\n",
			   wsr88d_ray.message_type, wsr88d_get_status (&wsr88d_ray) );*/

		if ( wsr88d_ray.message_type == 31 ) {
		   if ( wsr88d_ray.type31 != NULL ) {
		      if ( radar_verbose_flag ) fprintf(stderr,"freeing a type31 status %d\n",wsr88d_get_status (&wsr88d_ray ) );
		      if ( wsr88d_ray.type31->data != NULL ) free ( wsr88d_ray.type31->data);
		      free ( wsr88d_ray.type31);
		      wsr88d_ray.type31 = NULL;
		   }
		}

		n = wsr88d_read_ray(wf, &wsr88d_ray);
	  }
	   
	  if (n <= 0) return n; /* Read failure. */
	  end_of_volume = 0;
	/* Step 2. */
	  while ( ! end_of_volume ) {
		if  ( ( wsr88d_ray.message_type != 1) && 
		      ( wsr88d_ray.message_type != 31 ) ) {
		  /* 
		  fprintf(stderr,"SKIPPING (amid a sweep) packet: type %d\n",
				 wsr88d_ray.message_type );
		  */

		} else {
		  /* Load this ray into the sweep. */
		  ray_num = wsr88d_get_ray_num ( &wsr88d_ray );
		  /* Double check against #  records we've seen. */
		  /* It is possible that a reset occurs and we begin to overwrite
		   * previously loaded rays.  I've seen this occur, rarely, in the
		   * WSR88D data.  I must trust 'ray_num'.
		   */
		 
		  /* 
		  if (nrec+1 != ray_num) {
			fprintf(stderr, "Data says %d is ray_num, but, I've seen %d "
				    "records.\n", ray_num, nrec+1);
		  }
		  */
		 
		  if ( wsr88d_sweep->ray[ray_num] == NULL) {
		     wsr88d_sweep->ray[ray_num] = (Wsr88d_ray *) malloc ( sizeof(Wsr88d_ray) );
		     wsr88d_sweep->ray[ray_num]->type1 = NULL;
		     wsr88d_sweep->ray[ray_num]->type31 = NULL;
		  }
		  wsr88d_sweep->ray[ray_num]->message_type = wsr88d_ray.message_type;
	 
		  if ( wsr88d_ray.message_type == 1 ) {
		     if (wsr88d_sweep->ray[ray_num]->type1 == NULL) 
			wsr88d_sweep->ray[ray_num]->type1 = (Wsr88d_packet1 *) malloc (sizeof(Wsr88d_packet1));
		     memcpy(wsr88d_sweep->ray[ray_num]->type1, wsr88d_ray.type1, sizeof(Wsr88d_packet1));
		  }
		  else if ( wsr88d_ray.message_type == 31 ) {
		     wsr88d_sweep->ray[ray_num]->type31 = wsr88d_ray.type31;
		     wsr88d_ray.type31 = NULL;
		  }
		  else 
		     fprintf(stderr,"have to do data block for message type %d\n",wsr88d_ray.message_type);
		}
		n = wsr88d_read_ray(wf, &wsr88d_ray);
		if (n > 0) nrec++;

		end_of_volume = wsr88d_get_status ( &wsr88d_ray ) == 2 ||
				wsr88d_get_status ( &wsr88d_ray ) == 4 ||
				    n <= 0;
	  }

	  /* Process the last packet of the input data. */
	  if ( wsr88d_get_status ( &wsr88d_ray ) == 2 || wsr88d_get_status ( &wsr88d_ray ) == 4 ) {
		/* Load this ray into the sweep. */
		ray_num = wsr88d_get_ray_num ( &wsr88d_ray );

		if (wsr88d_sweep->ray[ray_num] == NULL) {
		     wsr88d_sweep->ray[ray_num] = (Wsr88d_ray *) malloc ( sizeof(Wsr88d_ray) );
		     wsr88d_sweep->ray[ray_num]->type1 = NULL;
		     wsr88d_sweep->ray[ray_num]->type31 = NULL;
		}
		wsr88d_sweep->ray[ray_num]->message_type = wsr88d_ray.message_type;

		if ( wsr88d_ray.message_type == 1 ) {
		   if (wsr88d_sweep->ray[ray_num]->type1 == NULL) 
		     wsr88d_sweep->ray[ray_num]->type1 = (Wsr88d_packet1 *) malloc (sizeof(Wsr88d_packet1));
		   memcpy(wsr88d_sweep->ray[ray_num]->type1, wsr88d_ray.type1, sizeof(Wsr88d_packet1));
		}
		else if ( wsr88d_ray.message_type == 31 ) {
		     wsr88d_sweep->ray[ray_num]->type31 = wsr88d_ray.type31;
		     wsr88d_ray.type31 = NULL;
		}
		else
		   fprintf(stderr,"have to do data block for message type %d\n",wsr88d_ray.message_type);
	  }

	  /* Just to be safe, clear all ray pointers left in this sweep to
	   * the maximum MAX_RAYS_IN_SWEEP.  This is required when the 
	   * wsr88d_sweep is reused and not cleared.
	   */
	  free_and_clear_sweep(wsr88d_sweep, ray_num+1, MAX_RAYS_IN_SWEEP);
	  
	/*
	  fprintf(stderr,"Processed %d records for elevation number %d\n",
			 nrec+1, wsr88d_ray.elev_num);
	  wsr88d_print_sweep_info(wsr88d_sweep);
	*/
	  return nrec;
	}

	/**********************************************************************/
	/*                                                                    */
	/*                      wsr88d_swap_ray                               */
	/*                                                                    */
	/**********************************************************************/
	void wsr88d_swap_ray1(Wsr88d_packet1 *wsr88d_ray)
	{
	  short *half_word;
	  half_word = (short *)wsr88d_ray;
	  for (; half_word<(short *)&wsr88d_ray->msg_time; half_word++)
		swap_2_bytes(half_word);

	  swap_4_bytes(&wsr88d_ray->msg_time);
	  swap_2_bytes(&wsr88d_ray->num_seg);
	  swap_2_bytes(&wsr88d_ray->seg_num);
	  swap_4_bytes(&wsr88d_ray->ray_time);
	  
	  half_word = (short *) &wsr88d_ray->ray_date;
	  for (; half_word<(short *)&wsr88d_ray->sys_cal; half_word++)
		swap_2_bytes(half_word);

	  swap_4_bytes(&wsr88d_ray->sys_cal);

	  half_word = (short *) &wsr88d_ray->refl_ptr;
	  for (; half_word<(short *)&wsr88d_ray->data[0]; half_word++)
		swap_2_bytes(half_word);

	}

	void wsr88d_swap_ray(Wsr88d_ray *wsr88d_ray)
	{
	  if ( wsr88d_ray->message_type == 1 )
	      wsr88d_swap_ray1 (wsr88d_ray->type1);

          /* now use swap within myltibyte read for packet31 */
	  /*else
	      printf("have to add packet31 swap\n");*/
}

int wsr88d_message_header ( unsigned char *oblock, int nbytes, 
		int *message_type, int *message_size)
{
     short ssize, snum, sseg;

     wsr88d_read_bytes(&ssize,oblock+12,sizeof(short));
     wsr88d_read_bytes(&snum,oblock+24,sizeof(short));
     wsr88d_read_bytes(&sseg,oblock+26,sizeof(short));
     *message_type = (int)oblock[15];
     *message_size = (int)ssize;
     if ( ( ( snum > 1 ) || ( sseg > 1 ) ) 
	&& ( *message_type == 1 || *message_type ==  31 || *message_type ==  5 ) ) 
		fprintf(stderr,"look multiseg %hd %hd // %hd %d\n",
			snum, sseg, *message_size, *message_type);
     return(0);
}


/**********************************************************************/
/*                                                                    */
/*  done 2/28           wsr88d_read_ray                               */
/*                                                                    */
/**********************************************************************/
int wsr88d_read_ray(Wsr88d_file *wf, Wsr88d_ray *wsr88d_ray)
{
  int n;

  int nb, i, length;
  int bzerror;
  unsigned int nout;
  unsigned char raybuf[8];
  static unsigned int nrem=0;
  static int isize=0, osize;
  static off_t optr;
  static unsigned char *iblock=NULL, *oblock = NULL;
  int message_size;
  int message_type;

  /*printf("in read_ray nrem %d optr %d\n",nrem,optr);*/

  if (oblock == NULL)
     {
     oblock = (unsigned char *)malloc(262144);
     osize = 262144;
     }

  if ( nrem > 0 )
     {
     wsr88d_message_header(oblock+optr, 28, &message_type, &message_size);
     if ( nrem < message_size * 2 )
        {
        fprintf(stderr,"Error: less than 1 ray left un-inflated bz [%d]\n",nrem);
        nrem = 0;
        return(0);
        }
     wsr88d_ray->message_type = message_type;
     if ( message_type == 31 ) {
	message_size = message_size * 2 + 12; /* 12 extra bytes of communicated header */
        wsr88d_ray->type31 = (Wsr88d_packet31 *)malloc(sizeof(Wsr88d_packet31));
        wsr88d_ray->type31->data = (unsigned char *)malloc(message_size - (sizeof(Wsr88d_packet31) - sizeof(unsigned char *)));
        memcpy ( wsr88d_ray->type31, oblock + optr, sizeof(Wsr88d_packet31) - sizeof(unsigned char *));
        memcpy ( wsr88d_ray->type31->data, oblock + optr + sizeof(Wsr88d_packet31) - sizeof(unsigned char *), 
		message_size - (sizeof(Wsr88d_packet31) - sizeof(unsigned char *))); 
     }
     else {
        message_size = sizeof(Wsr88d_packet1);
        memcpy ( wsr88d_ray->type1, oblock + optr, message_size );
     }
     optr = optr + message_size;

     if (little_endian())
        wsr88d_swap_ray(wsr88d_ray);

     if ( nrem > message_size )
        nrem -= message_size;
     else if ( nrem < message_size ) {
        fprintf(stderr,"setting nrem=0 was %d %d\n",nrem,message_size);
        nrem = 0;
     }
     else
        nrem = 0;
     print_packet_info(wsr88d_ray);
     return ( message_size );
     }

  nb = fread(raybuf, 1, 8, wf->fptr);
  /* see if this looks like BZIP2 */
  if ( ( nb == 8 ) && ( raybuf[4] == 'B' ) && ( raybuf[5] == 'Z' ) && ( raybuf[6] == 'h' ) )
     {
     /* appears to be BZIP2, seek back to start of BZ */
     fseek ( wf->fptr, (long) -4, SEEK_CUR );
     length = 0;
     for(i=0;i<4;i++)
           length = ( length << 8 ) + (unsigned char)raybuf[i];
     if ( length < 0 ) length = -length;
     if ( isize == 0 )
        {
        if ( (iblock = (unsigned char *)malloc (length)) == NULL )
           {
           fprintf(stderr,"could not allocate input block [%d]\n",length);
           isize = 0;
           return(0);
           }
        isize = length;
        }
     else if ( length > isize )
        {
        if ( ( iblock = (unsigned char *)realloc(iblock,length) ) == NULL )
           {
           fprintf(stderr,"could not realloc input block [%d]\n",length);
           isize = 0;
           return(0);
           }
        else
           isize = length;
        }
     nb = fread ( iblock, 1, length, wf->fptr);
     if ( nb != length )
        {
        fprintf(stderr,"could read bz block %d %d\n",nb,isize);
        return(0);
        }

     nout = osize;
     if ( ( bzerror = BZ2_bzBuffToBuffDecompress((char *)oblock, &nout, (char *)iblock, length, 1, 0) )
                != BZ_OK )
        {
        while ( bzerror == BZ_OUTBUFF_FULL)
           {
           osize += 262144;
           if ((oblock=(unsigned char*) realloc(oblock, osize)) == NULL)
              {
              fprintf(stderr,"Expanding BZ2 output buffer to %d bytes failed\n", osize);
              osize = 0;
              return(osize);
              }
           nout = osize;
	   if ( radar_verbose_flag ) fprintf(stderr,"retrying with buffer increased %d\n",osize);
           bzerror = BZ2_bzBuffToBuffDecompress((char *)oblock, &nout, (char *)iblock, length, 1, 0);
           }
        if ( bzerror != BZ_OK )
           {
           fprintf(stderr,"unable to inflate bz block [%d] length=%d\n",bzerror,length);
           return (0);
           }
        }
     optr = 0; /* set optr at start of oblock just read */

     if ( nout < sizeof(Wsr88d_ray) )
        {
        fprintf(stderr,"read less than 1 ray left un-inflated bz %d %d\n",
                nrem, sizeof(Wsr88d_ray));
        nrem = 0;
        return(0);
        }

     /* 12 bytes data message header, message code, sequence, message length */
     /* 16 bytes Table II message header data */
    
     wsr88d_message_header(oblock, 28, &message_type, &message_size);
     wsr88d_ray->message_type = message_type;
 
     if ( message_type == 31 ) {

	message_size = message_size * 2 + 12; /* 12 extra bytes of communicated header */
        wsr88d_ray->type31 = (Wsr88d_packet31 *)malloc(sizeof(Wsr88d_packet31));
        wsr88d_ray->type31->data = (unsigned char *)malloc(message_size - (sizeof(Wsr88d_packet31) - sizeof(unsigned char *)));
        memcpy ( wsr88d_ray->type31, oblock + optr, sizeof(Wsr88d_packet31) - sizeof(unsigned char *));
        memcpy ( wsr88d_ray->type31->data, oblock  + optr + sizeof(Wsr88d_packet31) - sizeof(unsigned char *), 
		message_size - (sizeof(Wsr88d_packet31) - sizeof(unsigned char *))); 
     }
     else {
        message_size = sizeof(Wsr88d_packet1);
        memcpy ( wsr88d_ray->type1, oblock + optr, sizeof(Wsr88d_packet1) ); /* optr is zero,,,,but easy to follow */
     }
     optr = optr + message_size;
     print_packet_info(wsr88d_ray);

     /*if ( nout > sizeof(Wsr88d_ray) )
        nrem = nout - sizeof(Wsr88d_ray);*/
     if ( nout >= message_size )
        nrem = nout - message_size;
     else
        {
        fprintf(stderr,"Odd bz2 %d %d\n",nout,message_size);
        nrem = 0;
        }
     if (little_endian())
        wsr88d_swap_ray(wsr88d_ray);

     if ( message_type == 31 ) {
        return( message_size);
     }
     else
        return ( sizeof(Wsr88d_packet1) );
     }
  else
     {
     if (nb < 8 )
        {
        /*printf("end of file\n");*/
        return(0);
        }
     fseek ( wf->fptr, (long) -8, SEEK_CUR );

     /* new read in 28 bytes to decide message type */
     optr = 0;
     n = fread(oblock, 28, 1, wf->fptr);
     wsr88d_message_header(oblock+optr, 28, &message_type, &message_size);
     wsr88d_ray->message_type = message_type;
     if ( message_type != 31 ) {
        fseek ( wf->fptr, (long) -28, SEEK_CUR );
	message_size = sizeof(Wsr88d_packet1); 
        n = fread(wsr88d_ray->type1, message_size, 1, wf->fptr);
        if (little_endian() && message_type == 1)
	   wsr88d_swap_ray(wsr88d_ray);
     }
     else if ( message_type == 31 ) {
        fseek ( wf->fptr, (long) -28, SEEK_CUR );
	message_size = message_size * 2 + 12; /* 12 extra bytes of communicated header */

        wsr88d_ray->type31 = (Wsr88d_packet31 *)malloc(sizeof(Wsr88d_packet31));
        n = fread ( wsr88d_ray->type31, 1, sizeof(Wsr88d_packet31) - sizeof(unsigned char *), wf->fptr);
        wsr88d_ray->type31->data = (unsigned char *)malloc(message_size - (sizeof(Wsr88d_packet31) - sizeof(unsigned char *)));
        n += fread ( wsr88d_ray->type31->data, 1, message_size - (sizeof(Wsr88d_packet31) - sizeof(unsigned char *)), wf->fptr);
        if ( n != message_size ) {
           fprintf(stderr,"raw message31 read %d %d\n",message_size - sizeof(unsigned char *),n);
           message_size = 0;
        }
     }
     else
        {
	message_size = 0;
        fprintf(stderr,"shouldn't get here\n");
        }

     if (n > 0) print_packet_info(wsr88d_ray);
     return message_size;
     }

  fprintf(stderr,"should not get here\n");

  return 0;

}

/**********************************************************************/
/*                                                                    */
/*  not done N/A     wsr88d_read_ray_header                           */
/*                                                                    */
/**********************************************************************/
int wsr88d_read_ray_header(Wsr88d_file *wf,
						   Wsr88d_ray_header *wsr88d_ray_header)
{
  fprintf(stderr,"Stub routine: wsr88d_read_ray_header.\n");
  return 0;
}

/**********************************************************************/
/*                                                                    */
/*  done 3/3         wsr88d_ray_to_float                              */
/*                                                                    */
/**********************************************************************/
int wsr88d_ray_to_float_type1(Wsr88d_packet1 *ray,
						int THE_DATA_WANTED, float v[], int *n)
{
/*
 *  Input: *ray             -  WSR-88D packet
 * Output: THE_DATA_WANTED  -  Indicates which field to convert.  Fields:
 *                             WSR88D_DZ, WSR88D_VR, WSR88D_SW
 *         v[]              -  The output vector of float values.         
 *         n                -  Length of the output vector.  0 = no data.
 *
 * Returns n.
 *
 * No allocation of space for the output vector performed here.
 */

/* Code from Dan Austin (cvt_pckt_data.c) was the template for this. */

  /* declarations	*/
  int num_ref_gates,num_vel_gates,num_spec_gates;
  int refl_ptr, vel_ptr,spec_ptr,res_flag;
  int ival;
  int i;
  
  *n = 0;
  num_ref_gates  = ray->num_refl;
  num_vel_gates  = ray->num_dop;
  num_spec_gates = ray->num_dop;  /* 'num_dop', this is not a typo. */

/* The data pointers are specified from the begining of the 
 * 'Digital Radar Data (Message) Header'.  Since we have a structure
 * that defines all the header variables and a member called 'data'.
 * we must subtract the length of the 'message header' from the data
 * pointer.  Hopefully, the reflecivity pointer will be 0 meaning the
 * first element of the 'data' member; ray->data[0];
 */
#define LENGTH_OF_MESSAGE 100
  if (num_ref_gates > 0) refl_ptr = ray->refl_ptr - LENGTH_OF_MESSAGE;
  else refl_ptr = 0;
  
  vel_ptr  = ray->vel_ptr - LENGTH_OF_MESSAGE;
  spec_ptr = ray->spc_ptr - LENGTH_OF_MESSAGE;
  
  res_flag = ray->vel_res;


/*
  fprintf(stderr,"refl_ptr = %d  #g = %d, ", refl_ptr, num_ref_gates);
  fprintf(stderr," vel_ptr = %d  #g = %d, ", vel_ptr, num_vel_gates);
  fprintf(stderr,"spec_ptr = %d  #g = %d, ", spec_ptr, num_spec_gates);
  fprintf(stderr,"res_flag = %d\n", res_flag);
*/

  if (THE_DATA_WANTED == WSR88D_DZ) {
	/* do the reflectivity data  (dbZ)*/
	if (refl_ptr+num_ref_gates > 2300) 
	  fprintf(stderr, "WARNING: # refl index (%d) exceeds maximum (2300)\n",
			  refl_ptr+num_ref_gates);
	else {
	for(i=0; i<num_ref_gates; i++) {
	  ival = ray->data[refl_ptr+i];
	  if(ival > 1)
		  v[i] = (((ival-2.0)/2.0)-32.0);
	  else if (ival == 1) 
		v[i] = WSR88D_RFVAL;
	  else /* ival = 0 */
		v[i] = WSR88D_BADVAL;
	}
	*n = num_ref_gates;
	}

  } else if (THE_DATA_WANTED == WSR88D_VR) {
	/* do the velocity data  (M/S) */
	if (vel_ptr+num_vel_gates > 2300) 
	  fprintf(stderr, "WARNING: # vel index (%d) exceeds maximum (2300)\n",
			  vel_ptr+num_vel_gates);
	else {
	for(i=0; i<num_vel_gates;i++)	{
	  ival = ray->data[vel_ptr+i];
	  if(ival > 1)
		if (res_flag == 2) /* High resolution: 0.5 m/s */
		  v[i] = (((ival-2.0)/2.0)-63.5);
		else
		  v[i] = ((ival-2.0)-127.0);
	  else if (ival == 1) 
		v[i] = WSR88D_RFVAL;
	  else /* ival = 0 */
		v[i] = WSR88D_BADVAL;
	}
	*n = num_vel_gates;
	}
	
  } else if (THE_DATA_WANTED == WSR88D_SW) {
	/* now do the spectrum width data (M/S)*/
	if (spec_ptr+num_spec_gates > 2300) 
	  fprintf(stderr, "WARNING: # spec index (%d) exceeds maximum (2300)\n",
			  spec_ptr+num_spec_gates);
	else {
	for(i=0;i<num_spec_gates;i++) {
	  ival = ray->data[spec_ptr+i];
		if(ival > 1)
		  v[i] = (((ival-2)/2.0)-63.5);
		else if (ival == 1) 
		  v[i] = WSR88D_RFVAL;
		else /* ival = 0 */
		  v[i] = WSR88D_BADVAL;
	}
	*n = num_spec_gates;
	}
  }
  
  return *n;
}

int wsr88d_ray_to_float_type31(Wsr88d_packet31 *ray,
    int THE_DATA_WANTED, float v[], int *n)
{
  /* declarations	*/
  int num_ref_gates,num_vel_gates,num_spec_gates;
  int refl_ptr=-1, vel_ptr=-1, spec_ptr=-1;
  int ival;
  int i, ioff, pdi, pd0;
  int control_flags=0;
  static int prntcnt=0;

  short ng, data_block_count;
  float scale, offset;

  *n = 0;

  wsr88d_read_bytes(&data_block_count, &ray->data_block_count, 2);
  wsr88d_read_bytes(&pd0, &ray->pdata[0], 4);

  if (THE_DATA_WANTED == WSR88D_DZ) {
      char SEC_STR[]="DREF";

      for(i=0; i<data_block_count; i++) {
        wsr88d_read_bytes(&pdi, &ray->pdata[i], 4);
        ioff = pdi - pd0;
        if ( memcmp( ray->data+ioff, SEC_STR, 4 ) == 0 ) {
	   refl_ptr = ioff;
	   break;
        }
      }
      if ( refl_ptr < 0 ) return (*n);

      wsr88d_read_bytes(&ng,ray->data+refl_ptr+8,sizeof(short));
      num_ref_gates = (int)ng;
      wsr88d_read_bytes(&scale,ray->data+refl_ptr+20,sizeof(float));
      wsr88d_read_bytes(&offset,ray->data+refl_ptr+24,sizeof(float));
      control_flags = ray->data[refl_ptr+18];

	for(i=0; i<num_ref_gates; i++) {
	  ival = ray->data[refl_ptr+28+i];
	  if(ival > 1) {
                if ( scale != 0 )
		   v[i] = ( (float)ival - offset ) / scale;
		else
		   v[i] = (float)ival;
	  }
	  else if (ival == 1) {
		v[i] = WSR88D_RFVAL;
	  }
	  else /* ival = 0 */
		v[i] = WSR88D_BADVAL;
	}
	*n = num_ref_gates;
  }
  else if (THE_DATA_WANTED == WSR88D_VR) {
      char SEC_STR[]="DVEL";

      for(i=0; i<data_block_count; i++) {
        wsr88d_read_bytes(&pdi, &ray->pdata[i], 4);
        ioff = pdi - pd0;
        if ( memcmp( ray->data+ioff, SEC_STR, 4 ) == 0 ) {
           vel_ptr = ioff;
           break;
        }
      }
      if ( vel_ptr < 0 ) return (*n);

      wsr88d_read_bytes(&ng,ray->data+vel_ptr+8,sizeof(short));
      num_vel_gates = (int)ng;
      wsr88d_read_bytes(&scale,ray->data+vel_ptr+20,sizeof(float));
      wsr88d_read_bytes(&offset,ray->data+vel_ptr+24,sizeof(float));
      control_flags = ray->data[vel_ptr+18];

      /* do the velocity data  (M/S) */
      for(i=0; i<num_vel_gates;i++)	{
	  ival = ray->data[vel_ptr+28+i];
	  if(ival > 1)
                if ( scale != 0 )
		   v[i] = ( (float)ival - offset ) / scale;
		else
		   v[i] = (float)ival;
	  else if (ival == 1) 
		v[i] = WSR88D_RFVAL;
	  else /* ival = 0 */
		v[i] = WSR88D_BADVAL;
      }
      *n = num_vel_gates;
  }	
  else if (THE_DATA_WANTED == WSR88D_SW) {
      char SEC_STR[]="DSW";

      for(i=0; i<data_block_count; i++) {
        wsr88d_read_bytes(&pdi, &ray->pdata[i], 4);
        ioff = pdi - pd0;
        if ( memcmp( ray->data+ioff, SEC_STR, 3 ) == 0 ) {
           spec_ptr = ioff;
           break;
        }
      }
      if ( spec_ptr < 0 ) return (*n);

      wsr88d_read_bytes(&ng,ray->data+spec_ptr+8,sizeof(short));
      num_spec_gates = (int)ng;
      wsr88d_read_bytes(&scale,ray->data+spec_ptr+20,sizeof(float));
      wsr88d_read_bytes(&offset,ray->data+spec_ptr+24,sizeof(float));
      control_flags = ray->data[spec_ptr+18];

      /* do the spectrum width data  (M/S) */
      for(i=0; i<num_spec_gates;i++)	{
	  ival = ray->data[spec_ptr+28+i];
	  if(ival > 1)
                if ( scale != 0 )
		   v[i] = ( (float)ival - offset ) / scale;
		else
		   v[i] = (float)ival;
	  else if (ival == 1) 
		v[i] = WSR88D_RFVAL;
	  else /* ival = 0 */
		v[i] = WSR88D_BADVAL;
      }
      *n = num_spec_gates;
  }	
  else
	fprintf(stderr,"still have to implement raytofloat %d\n",THE_DATA_WANTED);

  /* only print this message once per volume, if needed */
  if ( (int)ray->ray_status == 3 ) prntcnt = 1;
 
  if ( prntcnt ) {
    switch (control_flags) {
	case 1:
		fprintf(stderr,"recombined azimuthal radials\n");
		prntcnt = 0; 
		break;
	case 2:
		fprintf(stderr,"recombined range gates\n");
		prntcnt = 0; 
		break;
	case 3:
		fprintf(stderr,"recombined radials and range gates to legacy resolution\n");
		prntcnt = 0; 
		break;
    }
  }
  return *n;

}

int wsr88d_ray_to_float(Wsr88d_ray *ray, int THE_DATA_WANTED, float v[], int *n)
{
  if ( ray->message_type == 1 ) {
     return ( wsr88d_ray_to_float_type1(ray->type1, THE_DATA_WANTED, v, n ) );
  }
  else if (  ray->message_type == 31 ) {
     return ( wsr88d_ray_to_float_type31(ray->type31, THE_DATA_WANTED, v, n ) );
  }
  else {
     fprintf(stderr,"have to do ray_to_float_type %d\n",ray->message_type);
     return(0);
  }
}


/**********************************************************************/
/*        Functions that convert some message header values.          */
/**********************************************************************/
/**********************************************************************/
/*                                                                    */
/*  done 3/3   float wsr88d_get_nyquist                               */
/*  done 3/3   float wsr88d_get_atmos_atten_factor                    */
/*  done 3/3   float wsr88d_get_velocity_resolution                   */
/*  done 3/3   int   wsr88d_get_volume_coverage                       */
/*  done 3/3   float wsr88d_get_elevation_angle                       */
/*  done 3/3   float wsr88d_get_azimuth                               */
/*  done 3/3   float wsr88d_get_range                                 */
/*  done 3/3   void  wsr88d_get_date                                  */
/*  done 3/3   void  wsr88d_get_time                                  */
/*  done 5/20  int  *wsr88d_get_vcp_info                              */
/*  done 5/20  float wsr88d_get_fix_angle                             */
/*  done 5/20  int   wsr88d_get_pulse_count                           */
/*  done 5/20  float wsr88d_get_azimuth_rate                          */
/*  done 5/20  float wsr88d_get_pulse_width                           */
/*  done 5/20  float wsr88d_get_prf                                   */
/*  done 5/20  float wsr88d_get_prt                                   */
/*  done 5/20  float wsr88d_get_wavelength                            */
/*  done 5/20  float wsr88d_get_frequency                             */
/*                                                                    */
/**********************************************************************/
float wsr88d_get_nyquist(Wsr88d_ray *ray)
{
  int istart, i, pdi, pd0;
  char SEC_STR[]="RRAD";

  if ( ray->message_type == 1 )
     return ray->type1->nyq_vel/100.0;
  else if ( ray->message_type == 31 ) {
    short nyquist=-9999, data_block_count;
    wsr88d_read_bytes(&data_block_count, &ray->type31->data_block_count, 2);
    wsr88d_read_bytes(&pd0, &ray->type31->pdata[0], 4);
    for(i=0; i<data_block_count; i++) {
        wsr88d_read_bytes(&pdi, &ray->type31->pdata[i], 4);
        istart = pdi - pd0;
        if ( memcmp( ray->type31->data+istart, SEC_STR, 4 ) == 0 ) {
            wsr88d_read_bytes( &nyquist, ray->type31->data+istart+16, sizeof(short) );
            /*printf("data nyquist %f\n", (float)nyquist/100.00);*/
            return ((float)nyquist/100.0);
         }
    }
    return ((float)nyquist);
  }
  else
     return -9999.0;
}

float wsr88d_get_atmos_atten_factor(Wsr88d_ray *ray)
{
  int istart, i, pdi, pd0;
  char SEC_STR[]="RELV";

  if ( ray->message_type == 1 )
     return ray->type1->atm_att/1000.0;
  else if ( ray->message_type == 31 ) {
    short atm_att=-9999, data_block_count;
    wsr88d_read_bytes(&data_block_count, &ray->type31->data_block_count, 2);
    wsr88d_read_bytes(&pd0, &ray->type31->pdata[0], 4);
    for(i=0; i<data_block_count; i++) {
        wsr88d_read_bytes(&pdi, &ray->type31->pdata[i], 4);
        istart = pdi - pd0;
        if ( memcmp( ray->type31->data+istart, SEC_STR, 4 ) == 0 ) {
            wsr88d_read_bytes( &atm_att, ray->type31->data+istart+6, sizeof(short) );
            fprintf(stderr,"data atm_att %f\n", (float)atm_att/1000.0);
            return ((float)atm_att/1000.0);
        }
    }
    return ((float)atm_att);
  }
  else
     return -9999.0;
}

float wsr88d_get_velocity_resolution(Wsr88d_ray *ray)
{
  int istart, i, pdi, pd0;
  char SEC_STR[]="DVEL";

  if ( ray->message_type == 1 ) {
      if (ray->type1->vel_res == 2) 
         return .5;
      else
         return 0.0;
  }
  else if ( ray->message_type == 31 ) {
    float scale=1.0;
    short data_block_count;

    wsr88d_read_bytes(&data_block_count, &ray->type31->data_block_count, 2);
    wsr88d_read_bytes(&pd0, &ray->type31->pdata[0], 4);
    for(i=0; i<data_block_count; i++) {
        wsr88d_read_bytes(&pdi, &ray->type31->pdata[i], 4);
        istart = pdi - pd0;
        if ( memcmp( ray->type31->data+istart, SEC_STR, 4 ) == 0 ) {
            wsr88d_read_bytes( &scale, ray->type31->data+istart+20, sizeof(float) );
            return ((float)scale);
        }
    }
    return ((float)scale);
  }
  else
     return 0.0;
}

char	message_site_icao[5];
float	message_site_lat=-9999.0, message_site_lon=-9999.0, message_site_elev=-9999.0;
void wsr88d_set_station_id(char *icao, float lat, float lon, float elev)
{
   memcpy(message_site_icao,icao,4); message_site_icao[4] = '\0';
   message_site_lat = lat; 
   message_site_lon = lon; 
   message_site_elev = elev; 
}

void wsr88d_get_station_id(char *icao, float *lat, float *lon, float *elev)
{
strcpy(icao,message_site_icao);
*lat = message_site_lat;
*lon = message_site_lon;
*elev = message_site_elev;
}

int   wsr88d_get_volume_coverage(Wsr88d_ray *ray)
{
  int vol_cpat;
  int istart, i, pdi, pd0;
  char SEC_STR[]="RVOL";

 
  if ( ray->message_type == 1 )
      vol_cpat = ray->type1->vol_cpat;
  else if ( ray->message_type == 31 ) {
    short vcp=0, data_block_count;
    wsr88d_read_bytes(&data_block_count, &ray->type31->data_block_count, 2);
    wsr88d_read_bytes(&pd0, &ray->type31->pdata[0], 4);
    for(i=0; i<data_block_count; i++) {
        wsr88d_read_bytes(&pdi, &ray->type31->pdata[i], 4);
        istart = pdi - pd0;
        if ( memcmp( ray->type31->data+istart, SEC_STR, 4 ) == 0 ) {
	    float lat, lon, elev;
	    short site_elev, feedhorn_elev;

            wsr88d_read_bytes( &vcp, ray->type31->data+istart+40, sizeof(short) );
        /* While we are here, set the radar lat/lon */
            wsr88d_read_bytes( &lat, ray->type31->data+istart+8, sizeof(float) );
            wsr88d_read_bytes( &lon, ray->type31->data+istart+12, sizeof(float) );
            wsr88d_read_bytes( &site_elev, ray->type31->data+istart+16, sizeof(short) );
            wsr88d_read_bytes( &feedhorn_elev, ray->type31->data+istart+18, sizeof(short) );
	    elev = (float)site_elev + (float)feedhorn_elev;
            wsr88d_set_station_id((char *)ray->type31+28,lat,lon,elev);
        }
    }
    vol_cpat = (int)vcp;
  }
  else
      vol_cpat = 0;

  /*if (vol_cpat == 11) return 11;
  if (vol_cpat == 12) return 12;
  if (vol_cpat == 21) return 21;
  if (vol_cpat == 31) return 31;
  if (vol_cpat == 32) return 32;
  if (vol_cpat == 121) return 121;*/
  return vol_cpat;
}

float wsr88d_get_elevation_angle(Wsr88d_ray *ray)
{
  if ( ray->message_type == 1 )
     return ray->type1->elev/8.0*(180.0/4096.0);
  else if ( ray->message_type == 31 ) {
     float elev_angle;
     wsr88d_read_bytes(&elev_angle, &ray->type31->elev_angle, 4);
     return elev_angle;
  }
  else
      return -9999.0;
}

float wsr88d_get_azimuth(Wsr88d_ray *ray)
{

  if ( ray->message_type == 1 )
      return ray->type1->azm/8.0*(180.0/4096.0);
  else if ( ray->message_type == 31 )
      {
      float azm_angle;
      wsr88d_read_bytes(&azm_angle, &ray->type31->azm_angle, 4);
      return azm_angle;
      }
  else
      return -9999.0;

}

float wsr88d_get_range(Wsr88d_ray *ray)
{
  int istart, i, pdi, pd0;
  char SEC_STR[]="RRAD";

  if ( ray->message_type == 1 )
      return ray->type1->unam_rng/10.0;
  else if ( ray->message_type == 31 ) {
      short unam_rng, data_block_count;
      wsr88d_read_bytes(&data_block_count, &ray->type31->data_block_count, 2);
      wsr88d_read_bytes(&pd0, &ray->type31->pdata[0], 4);
      for(i=0; i<data_block_count; i++) {
          wsr88d_read_bytes(&pdi, &ray->type31->pdata[i], 4);
          istart = pdi - pd0;
          if ( memcmp( ray->type31->data+istart, SEC_STR, 4 ) == 0 ) {
               wsr88d_read_bytes( &unam_rng, ray->type31->data+istart+6, sizeof(short) );
	       return((float)unam_rng/10.0);
          }
      }
      return -9999.0;
  }
  else
      return -9999.0;
}

#include <time.h>
void wsr88d_get_date(Wsr88d_ray *ray, int *mm, int *dd, int *yy)
{
/*
 * mm (1-12)
 * dd (1-31)
 * yy (ex. 93)
 */
  time_t itime;
  struct tm *tm_time;

  *mm = *dd = *yy = 0; 
  if (ray == NULL) return;

  if ( ray->message_type == 1 )
     itime = ray->type1->ray_date - 1;
  else if ( ray->message_type == 31 ) {
     short stemp;
     wsr88d_read_bytes(&stemp, &ray->type31->msg_date, 2 );
     itime = stemp - 1;
  }
  else
     return;

  itime *= 24*60*60; /* Seconds/day * days. */

  tm_time = gmtime(&itime);
  *mm = tm_time->tm_mon+1;
  *dd = tm_time->tm_mday;
  *yy = tm_time->tm_year;
}

void wsr88d_get_time(Wsr88d_ray *ray, int *hh, int *mm, int *ss, float *fsec)
{
  /*
   * hh (0-23)
   * mm (0-59)
   * ss (0-59)
   * fsec (fraction of second)
   */
  int ray_time;
  double t;
  
  *hh = *mm = *ss = *fsec = 0;
  if (ray == NULL)  return;

  if ( ray->message_type == 1 )
      ray_time = ray->type1->ray_time;
  else if ( ray->message_type == 31 ) {
      wsr88d_read_bytes(&ray_time, &ray->type31->msg_time, 4);
  }
  else
      return;

  t = ray_time;
  t /= 1000.0;
  *hh = t/3600;
  t -= *hh*3600;
  *mm = t/60;
  t -= *mm*60;
  *ss = (int)t;
  *fsec = t - *ss;
}



/*
 * Get_vcp_info - gets info about the volume coverage pattern for this scan
 * parameters:
 * int vcp_num - volume coverage pattern number 
 * int el_num - elevation number w/in vcp
 * returns: int *vcp_info - ptr to array w/vcp info 
 * calls from: Nexrad2uf
 * calls to: none
 */

/* this database contains: 5 volume coverage patterns & associated info */
/* (0)= vcp # (1)=pulse width for vcp  "Id$"                            */
/* line[1-n]: (n,0)= elev. # for vcp (n,1)= (fixed angle)*8*(4096/180)  */
/* (n,2)= pulse count (n,3)= (azimuthal sweep rate)*8*(4096/45)         */

static int vcp11[68] ={11,514,88,17,13600,88,0,14000,264,16,12664,264,0,14000,440,6,11736,608,6,24760,784,6,24760,952,10,12712,1128,10,12720,1368,0,18328,1584,0,18496,1824,0,18512,2184,0,18544,2552,0,18576,3040,0,18640,3552,0,18712};

static int vcp12[53]={12,514,91,15,15401,91,0,18204,164,15,15401,164,0,18204,237,15,15401,237,0,18204,328,3,19297,437,3,20393,564,3,20393,728,3,20393,928,3,20393,1165,0,20680,1456,0,20680,1820,0,21033,2276,0,20929,2840,0,20929,3550,0,20929};

static int vcp21[48]={21,514,88,28,8256,88,0,8272,264,28,8256,264,0,8272,440,8,7888,608,8,7888,784,8,8160,1096,12,8160,1800,0,10640,2656,0,10432,3552,0,10496};

static int vcp31[36]={31,516,88,63,3672,88,0,3688,272,63,3672,272,0,3688,456,63,3672,456,0,3688,640,0,3688,816,0,3688};

static int vcp32[32]={32,514,88,64,3616,88,0,3312,272,64,3616,272,0,3312,456,11,2960,640,11,2960,816,11,2960};

static int vcp121[62]={121,514,91,11,21336,91,0,21696,91,0,19952,91,0,15584,264,11,21336,264,0,21696,264,0,19952,264,0,15584,437,6,13985,437,0,19952,437,0,15584,610,6,15729,610,0,19952,610,0,15584,783,6,11872,783,0,21481,1092,6,14712,1802,0,21481,2658,0,21696,3550,0,21696};

static int vcp300[20]={300,514,88,28,8256,88,0,8272,440,8,8160,1800,0,10384};

static int table_vcp_num=-1;
static int table_vcp_length=0;
static int *table_vcp_array=NULL;
void wsr88d_set_vcp_info ( int vcp_num, int length, int *array)
{
  if ( table_vcp_num > 0 ) {
     /*printf("replacing current VCP info %d with %d\n",table_vcp_num,vcp_num);*/
     if ( table_vcp_array != NULL ) free(table_vcp_array);
  }
  table_vcp_num = vcp_num;
  table_vcp_length = length;
  table_vcp_array = array;
}

/*int *wsr88d_get_vcp_info(int vcp_num,int el_num)*/
int *wsr88d_get_vcp_info(Wsr88d_ray *ray,int el_num)
{
/*
 * This routine from Dan Austin.  Program component of nex2uf.
 */
	static int vcp_info[4];
	int fix_angle;
	int pulse_cnt;
	int az_rate;
	int pulse_width;
	int vcp_num;

	if ( ray->message_type == 1 )
	    vcp_num = ray->type1->vol_cpat;
	else if ( ray->message_type == 31 ) {
	    vcp_num = wsr88d_get_volume_coverage(ray) ;
	}
        else
	    vcp_num = 0;

        if(table_vcp_num == vcp_num ) {
	   if ( (3*el_num)+1 > table_vcp_length ) fprintf(stderr,"VCP table problem %d\n",el_num);
	   fix_angle = table_vcp_array[(3*el_num)-1];
	   pulse_cnt = table_vcp_array[(3*el_num)];
	   az_rate = table_vcp_array[(3*el_num)+1];
	   pulse_width = table_vcp_array[1];
        }
        else { /* see if ythis is predefined */
	   /* case statement to get vcp info */
	   switch(vcp_num) {
	   case 11:
	     fix_angle =   vcp11[(3*el_num)-1];
	     pulse_cnt =   vcp11[(3*el_num)];
	     az_rate =     vcp11[(3*el_num)+1];
	     pulse_width = vcp11[1];
	     break;
	   case 12:
	     fix_angle =   vcp12[(3*el_num)-1];
	     pulse_cnt =   vcp12[(3*el_num)];
	     az_rate =     vcp12[(3*el_num)+1];
	     pulse_width = vcp12[1];
	     break;
	   case 21:
	     fix_angle =   vcp21[(3*el_num)-1];
	     pulse_cnt =   vcp21[(3*el_num)];
	     az_rate =     vcp21[(3*el_num)+1];
	     pulse_width = vcp21[1];
	     break;
	   case 31:
	     fix_angle =   vcp31[(3*el_num)-1];
	     pulse_cnt =   vcp31[(3*el_num)];
	     az_rate =     vcp31[(3*el_num)+1];
	     pulse_width = vcp31[1];
	     break;
	   case 32:
	     fix_angle =   vcp32[(3*el_num)-1];
	     pulse_cnt =   vcp32[(3*el_num)];
	     az_rate =     vcp32[(3*el_num)+1];
	     pulse_width = vcp32[1];
	     break;
	   case 300:
	     fix_angle =   vcp300[(3*el_num)-1];
	     pulse_cnt =   vcp300[(3*el_num)];
	     az_rate =     vcp300[(3*el_num)+1];
	     pulse_width = vcp300[1];
	     break;
	   case 121:
	     fix_angle =   vcp121[(3*el_num)-1];
	     pulse_cnt =   vcp121[(3*el_num)];
	     az_rate =     vcp121[(3*el_num)+1];
	     pulse_width = vcp121[1];
	     break;
	   default:
	     fix_angle  = 0;
	     pulse_cnt  = 0;
	     az_rate    = 0;
	     pulse_width= 0;
	     break;
	   }
	}
	
	/* get array for output	*/
	vcp_info[0]=fix_angle;
	vcp_info[1]=pulse_cnt;
	vcp_info[2]=az_rate;
	vcp_info[3]=pulse_width;
	
	
	/* return the value array	*/
	return(vcp_info);
}


float wsr88d_get_fix_angle(Wsr88d_ray *ray)
{
  int *vcp_info;
  vcp_info = wsr88d_get_vcp_info(ray, wsr88d_get_elev_num( ray ) );
  return vcp_info[0]/8.0*180./4096.0;
}
int wsr88d_get_pulse_count(Wsr88d_ray *ray)
{
  int *vcp_info;
  vcp_info = wsr88d_get_vcp_info(ray, wsr88d_get_elev_num( ray ) );
  return vcp_info[1];
}
float wsr88d_get_azimuth_rate(Wsr88d_ray *ray)
{
  int *vcp_info;
  vcp_info = wsr88d_get_vcp_info(ray, wsr88d_get_elev_num( ray ) );
  return vcp_info[2]/8.0*45./4096.0;
}
float wsr88d_get_pulse_width(Wsr88d_ray *ray)
{
  int *vcp_info;
  vcp_info = wsr88d_get_vcp_info(ray, wsr88d_get_elev_num( ray ) );
  return vcp_info[3]/299.792458;
}

float wsr88d_get_prf(Wsr88d_ray *ray)
{
  float prf;
  float c = 299792458.0;
  float range;

  range = wsr88d_get_range(ray)*1000.0;
  if (range != 0) prf = c/(2*range);
  else prf = 0.0;

  return prf;
}

float wsr88d_get_prt(Wsr88d_ray *ray)
{
  float prf;
  float prt;

  prf = wsr88d_get_prf(ray);
  if (prf != 0) prt = 1.0/prf;
  else prt = 0;
  return prt;
}

/* Note: wsr88d_get_wavelength() below is no longer used because of differences
 * in wavelength for velocity and reflectivity.  The function computes
 * wavelength when Nyquist is present, but returns a constant wavelength
 * otherwise.  Nyquist is present for velocity, but not for reflectivity.  The
 * fact is that WSR-88D radars use a constant wavelength, 10.7 cm., which is
 * the value now used where this function was formerly called in
 * wsr88d_load_sweep_into_volume().
 */

float wsr88d_get_wavelength(Wsr88d_ray *ray)
{
  float wavelength;
  float prf;
  float nyquist;

  prf = wsr88d_get_prf(ray);
  nyquist = wsr88d_get_nyquist(ray);
	/* If required info to determine wavelength does not exist,
		 just use 10 cm. All wsr88d radars are 10cm. MJK */
  if ((prf == 0) || (nyquist == 0.0)) wavelength = 0.10;
  else wavelength = 4*nyquist/prf;
  return wavelength;
}

float wsr88d_get_frequency(Wsr88d_ray *ray)
{
  float freq;
  float c = 299792458.0;

	/* Carrier freq (GHz). Revised 12 Jun 97. MJK */
	freq = (c / wsr88d_get_wavelength(ray)) * 1.0e-9;
  return freq;
}


int wsr88d_get_status ( Wsr88d_ray *wsr88d_ray )
{
  if ( wsr88d_ray->message_type == 1 ) return ( wsr88d_ray->type1->ray_status );
  if ( wsr88d_ray->message_type == 31 ) return ( wsr88d_ray->type31->ray_status );

  /*printf("unknown message type %d\n",wsr88d_ray->message_type);*/
  return(-1);
}

int wsr88d_get_ray_num ( Wsr88d_ray *wsr88d_ray )
{
  /* Ray numbers are 1's based! */
  short azm_num;
  if ( wsr88d_ray->message_type == 1 )
    return ( (int)wsr88d_ray->type1->ray_num - 1);
  else
    {
    wsr88d_read_bytes(&azm_num, &wsr88d_ray->type31->azm_num, 2);
    return ( (int)azm_num - 1);
    }

  fprintf(stderr,"unknown message type %d\n",wsr88d_ray->message_type);

  return(0);
}

int wsr88d_get_elev_num ( Wsr88d_ray *wsr88d_ray )
{
  if ( wsr88d_ray->message_type == 1 )
    return ( (int)wsr88d_ray->type1->elev_num );
  else
    return ( (int)wsr88d_ray->type31->elev_num );

  fprintf(stderr,"unknown message type %d\n",wsr88d_ray->message_type);

  return(0);
}

float wsr88d_get_azm_res ( Wsr88d_ray *wsr88d_ray )
{
float azm_res_space=1.0;
  if ( wsr88d_ray->message_type == 1 )
    azm_res_space = 1.0;
  else if ( wsr88d_ray->message_type == 31 ) {
    if ( (int)wsr88d_ray->type31->azm_res_space == 1 )
       azm_res_space = 0.5;
    else if ( (int)wsr88d_ray->type31->azm_res_space == 2 )
       azm_res_space = 1.0;
    else {
       printf("unexpected resolution spacing %d\n",(int)wsr88d_ray->type31->azm_res_space );
    }
       
  }
  else
     fprintf(stderr,"unknown message type %d\n",wsr88d_ray->message_type);

  return(azm_res_space);
}

int wsr88d_get_refl_rng ( Wsr88d_ray *wsr88d_ray )
{
int i, istart;
char SEC_STR[]="DREF";
  if ( wsr88d_ray->message_type == 1 )
    return ( (int)wsr88d_ray->type1->refl_rng );
  else if ( wsr88d_ray->message_type == 31 ) {
      short refl_rng, data_block_count;
      int pdi, pd0;
      wsr88d_read_bytes(&data_block_count, &wsr88d_ray->type31->data_block_count, 2);
      wsr88d_read_bytes(&pd0, &wsr88d_ray->type31->pdata[0], 4);
      for(i=0; i<data_block_count; i++) {
	  wsr88d_read_bytes(&pdi, &wsr88d_ray->type31->pdata[i], 4);
          istart = pdi - pd0;
          if ( memcmp( wsr88d_ray->type31->data+istart, SEC_STR, 4 ) == 0 ) {
               wsr88d_read_bytes( &refl_rng, wsr88d_ray->type31->data+istart+10, sizeof(short) );
	       /*printf("look refl_rng %d meters\n",refl_rng);*/
	       return((int)refl_rng);
          }
      }
    return -9999;
  }

  fprintf(stderr,"unknown message type %d refl_rng\n",wsr88d_ray->message_type);

  return(0);
}

int wsr88d_get_refl_size ( Wsr88d_ray *wsr88d_ray )
{
int i, istart;
char SEC_STR[]="DREF";

  if ( wsr88d_ray->message_type == 1 )
    return ( (int)wsr88d_ray->type1->refl_size );
  else if ( wsr88d_ray->message_type == 31 ) {
      short refl_size, data_block_count;
      int pdi, pd0;
      wsr88d_read_bytes(&data_block_count, &wsr88d_ray->type31->data_block_count, 2);
      wsr88d_read_bytes(&pd0, &wsr88d_ray->type31->pdata[0], 4);
      for(i=0; i<data_block_count; i++) {
          wsr88d_read_bytes(&pdi, &wsr88d_ray->type31->pdata[i], 4);
          istart = pdi - pd0;
          if ( memcmp( wsr88d_ray->type31->data+istart, SEC_STR, 4 ) == 0 ) {
               wsr88d_read_bytes( &refl_size, wsr88d_ray->type31->data+istart+12, sizeof(short) );
               /*printf("look refl_size %d meters\n",refl_size);*/
               return((int)refl_size);
          }
      }
  }
  else
     fprintf(stderr,"unknown message type %d refl_size\n",wsr88d_ray->message_type);

  return(0);
}

int wsr88d_get_dop_rng ( Wsr88d_ray *wsr88d_ray )
{
int i, istart;
char SEC_STR[]="DVEL";
  if ( wsr88d_ray->message_type == 1 )
    return ( (int)wsr88d_ray->type1->dop_rng );
  else if ( wsr88d_ray->message_type == 31 ) {
      short vel_rng, data_block_count;
      int pdi, pd0;
      wsr88d_read_bytes(&data_block_count, &wsr88d_ray->type31->data_block_count, 2);
      wsr88d_read_bytes(&pd0, &wsr88d_ray->type31->pdata[0], 4);
      for(i=0; i<data_block_count; i++) {
          wsr88d_read_bytes(&pdi, &wsr88d_ray->type31->pdata[i], 4);
          istart = pdi - pd0;
          if ( memcmp( wsr88d_ray->type31->data+istart, SEC_STR, 4 ) == 0 ) {
               wsr88d_read_bytes( &vel_rng, wsr88d_ray->type31->data+istart+10, sizeof(short) );
	       return((int)vel_rng);
          }
      }
  }
  else
    return -9999;

  fprintf(stderr,"unable to determine dop_rng %d\n",wsr88d_ray->message_type);

  return(0);
}

int wsr88d_get_dop_size ( Wsr88d_ray *wsr88d_ray )
{
int i, istart;
char SEC_STR[]="DVEL";
  if ( wsr88d_ray->message_type == 1 )
    return ( (int)wsr88d_ray->type1->dop_size );
  else if ( wsr88d_ray->message_type == 31 ) {
      short dop_size, data_block_count;
      int pdi, pd0;
      wsr88d_read_bytes(&data_block_count, &wsr88d_ray->type31->data_block_count, 2);
      wsr88d_read_bytes(&pd0, &wsr88d_ray->type31->pdata[0], 4);
      for(i=0; i<data_block_count; i++) {
          wsr88d_read_bytes(&pdi, &wsr88d_ray->type31->pdata[i], 4);
          istart = pdi - pd0;
          if ( memcmp( wsr88d_ray->type31->data+istart, SEC_STR, 4 ) == 0 ) {
               wsr88d_read_bytes( &dop_size, wsr88d_ray->type31->data+istart+12, sizeof(short) );
               return((int)dop_size);
          }
      }
  }
  else
    return -9999;

  fprintf(stderr,"unable to determine dop_size %d\n",wsr88d_ray->message_type);

  return(0);
}
