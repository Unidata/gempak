/*
    NASA/TRMM, Code 910.1.
    This is the TRMM Office Radar Software Library.
    Copyright (C) 1998
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
%{
#define USE_RSL_VARS
#include "rapic_routines.h"
#include "rsl.h"
#include <stdlib.h>
#include <math.h>
#include <string.h>

int rapicerror(char *s);
int rapicwrap(char *s);
int rapicwrap(char *s);
int yywrap(char *s);
int rapiclex(void);

int nsweep = 0;
float angres;
Radar *radar, *rapic_radar = NULL;
Volume *volume;
Sweep *sweep;
Ray *ray;

/* Rapic format declarations. */
Rapic_sweep_header rh;
Rapic_sweep *rs;

unsigned char outbuf[2000000];
int outbytes;
float azim, elev;
float save_elev;
int delta_time;
int nray = 0;
int ifield;
int ivolume, isweep, iray;
int station_id;

int sweepcount[5];

extern int radar_verbose_flag;
float rapic_nyquist;
  %}

/* 2/18/98 - John Merritt
 *
 * This grammar parses the NT Rapic radar format.  The grammar
 * is not minimal, however, it is self documenting; it closely matches
 * the brief documentation.
 */

/*-----------------------------------------------------------------
 * NT Rapic Volume Structure:
 * NOTE:   '*' marked tokens are preent only in versions > 10.0
 * 
 *  ImageHeader:
 * 	The header contains a set of tokens followed by data.
 * 	Each token is seperated by '\n' and all the data is
 * 	in ascii.
 *  ScanHeader:
 *         Scan header has the scan details like fieldname,
 * 	date/time azimuth,elevation etc.
 * 	The different sets of Tokens together with the data
 * 	are seperate by '\0'.
 *   ScanData:
 *         This represents the field data in runlehgth code.
 * 	Each Ray data contains azimuth,elevation,delta time
 * 	since start of this scan and the actual data.
 * 	
 * 	Here is a brief explanation of each of the Tokens.
 * 	
 * 
 * ImageHeader:
 * 
 * 1	/IMAGE: 	<seqno> <imgno>
 * 	<seqno>		4 digit number
 * 	<imgno>		10 digit number
 * 	
 * 2	/IMAGESCANS: 	<nscans>
 * 	<nscans>	Number of scans in this volume
 * 		
 * 3	/IMAGESIZE:  	<size>
 * 	<size>		Size of image in bytes
 * 
 * 4	/SCAN       <scanno:> <seqno> <datetime> <???> <elev> <fieldno> <???> <offset> <size>
 * 	<scanno:>   1 - <number of scans> 
 * 	<seqno>	    seqnum as given in /IMAGE:
 * 	<datetime>  yymoddhhmm
 * 	<???>	    Don't care ( I don't know )
 * 	<elev>      Target Elevation in degrees.
 * 	<fieldno>   0-4 
 *          0 Reflectivity     ?? Index? haven't seen this yet. ??
 * 		    1 Velocity         Confirmed this index.
 * 		    2 Width            Confirmed.
 * 	        3 ZDR              ?? Index? haven't seen this yet. ??
 * 		    4 Uncorrected Reflectivity.	Confirmed.
 * 	<???>	    Don't care
 * 	<offset>    Offset to start of scan header
 * 	<size>      Size of this scan
 * 		    add offset to size to get to the next scan
 * 	/SCAN is repeated for each scan in the volume.
 * 	NOTE:
 * 	Unlike other formats each scan need not represent a seperate
 * 	elevation.
 * 	
 * 5	/IMAGEHEADER END:
 * 	Indicates the end of Image header.
 * 
 *  ScanHeader:
 * 
 * 	COUNTRY: 	<code>
 * 	<code>  	is a country code number
 * 	
 * 	NAME:    	<Name>
 * 	<Name>	 	8 char long station name
 * 	
 * 	STNID:		<idno>
 * 	<idno>		A unique number indexing into station details
 * 	
 * 	LATITUDE:	<lat.lat>		*
 * 	<lat.lat>	Latitude in degrees	
 * 	
 * 	LONGITUDE:	<lon.lon>		*
 * 	<lon.lon>	Longitude in degrees
 * 	
 * 	HEIGHT:		<alt>			*
 * 	<alt>		Radar height in meters
 * 	
 * 	DATE:		<datno>
 * 	<datno>		year = datno % 100
 * 			if year > 80 year += 1900
 * 			else year += 2000
 * 			doy  = datno / 100
 * 			Get the julian Number from year,1,1 and add doy -1
 * 			Use Julian date conversion to get calendar date.
 * 			NOTE: As simple as remembering your partner's DOB.
 * 			
 * 	TIME:		<hh.mm>
 * 	<hh.mm>		Hour and minute
 * 	
 * 	TIMESTAMP:	<yyyymoddhhmmss>	*
 * 	<yyyymoddhhmmss> year,month,day,hour,min,sec
 * 	
 * 	VERS:		<versionNumber>
 * 	<versionNumber> 10.01
 * 	
 * 	FREQUENCY:	<freq>			*
 * 	<freq>		Radar Frequency.
 * 	
 * 	PRF:		<prf>
 * 	<prf>		Pulse repetition Frequency
 * 	
 * 	PULSELENGTH:	<len>			*
 * 	<len>		Pulse length
 * 	
 * 	RNGRES:		<gatewidth>
 * 	<gatewidth>	Range between gates
 * 	
 * 	ANGRES:		<angle>
 * 	<angle>		BeamWidth in degrees.
 *
 * 	CLEARAIR:		ON or OFF
 * 	
 * 	VIDRES:		<res>
 * 	<res>		Video Resolution can be 
 * 			16 OR 256.
 * 			
 * 	STARTRNG:	<rng>
 * 	<rng>		Range to First gate in meters.
 * 	
 * 	ENDRNG:		<rng>
 * 	<rng>		Maximum range.
 * 	
 * 	PRODUCT:	<type> <[id]>
 * 	<type>		Ascii text
 * 			VOLUMETRIC
 * 			NORMAL
 * 			
 * 	<[id]>		<id number>
 * 	
 * 	PASS:		<no> of <nscans>
 * 	<no>		Number of this scan
 * 	<nscans>	Max scan no.
 * 	
 * 	IMGFMT:		<type>
 * 	<type>		PPI,RHI etc.
 * 	
 * 	ELEV:		<elev>
 * 	<elev>		Elevation in degrees
 * 	
 * 	VIDEO:		<field>
 * 	<field>		Field Name Vel,Refl,Vel,UnCorRefl,Zdr,Wid
 * 	
 * 	VELLVL:		<level>
 * 	<level>		Velocity Level 
 * 	
 * 	NYQUIST:	<nyq>
 * 	<nyq>		Nyquist Velocity
 * 	
 * 	UNFOLDING:	<ratio>			*
 * 	<ratio>		None or x:y
 * 	
 * 
 * 	@		<data>			*
 * 	<data>		AAA.A,EEE.E,TTT=LEN16D1D1D1NLD1D1D1NLNL
 * 	
 * 	AAA.A		Azimuth in degrees
 * 	EEE.E		Elevation in degress
 * 	TTT		Delta time in seconds since the start of this scan
 * 	LEN16		2 byte long length of radial
 * 	
 * 	D1		Run length coded Data.
 * 	NL		Null The Next Byte is count of NULL data.
 * 	NLNL		End of this Radial
 * 
 *         eg.		There will be no white space in the actual radial data.
 * 				
 * 			@066.1,010.6,004=002082B2817F8C84830048D72D0038
 * 	                                 999C0036202D35FD2C00238A99008AFE920000
 * 			Azimuth = 66.1
 * 			Elev    = 10.6
 * 			Dt	= 4 sec since the start
 * 			0020	= Bytes to follow
 * 			Data    = 82,B2,81,7F,8C,84,83
 * 			0048	= 48H null bytes
 * 			Data    = D7,2D
 * 			0038    = 38H null bytes
 * 			Data	= 99,9C
 * 			0036	= 36H Null bytes
 * 			........................
 * 			0000	= End of Data.
 * 	In  versions before 10.1 			
 * 	@		<data>			
 * 	<data>		AAALEN16D1D1D1NLD1D1D1NLNL
 * 	
 * 	AAA		Azimuth in degrees
 * 	LEN16		2 byte long length of radial
 * 	
 * 	D1		Run length coded Data.
 * 	NL		Null The Next Byte is count of NULL data.
 * 	NLNL		End of this Radial
 * 
 *         eg.		There will be no white space in the actual radial data.
 * 				
 * 			@066002082B2817F8C84830048D72D0038
 * 	                    999C0036202D35FD2C00238A99008AFE920000
 * 			Azimuth = 66
 * 			0020	= Bytes to follow
 * 			Data    = 82,B2,81,7F,8C,84,83
 * 			0048	= 48H null bytes
 * 			Data    = D7,2D
 * 			0038    = 38H null bytes
 * 			Data	= 99,9C
 * 			0036	= 36H Null bytes
 * 			........................
 * 			0000	= End of Data.
 * 			
 * Please see the attached sample.vol to give a overall picture of the data.
 * This is the dump of the volume with white space inserted to make it readable.
 * Radial data dump is in hex.
 */

%token IMAGE IMAGESCANS IMAGESIZE IMAGEEND
%token SCAN IMAGEHEADEREND
%token NUMBER
%token ALPHA
%token FLOATNUMBER
%token BRACKETNUM

%token COUNTRY
%token NAME
%token STNID
%token LATITUDE
%token LONGITUDE
%token HEIGHT
%token DATE
%token TIME
%token TIMESTAMP
%token VERS
%token FREQUENCY
%token PRF
%token PULSELENGTH
%token RNGRES
%token ANGRES
%token ANGLERATE
%token CLEARAIR ON OFF
%token VIDRES
%token STARTRNG
%token ENDRNG
%token PRODUCT
%token PASS
%token IMGFMT
%token ELEV
%token VIDEO
%token VELLVL
%token NYQUIST
%token UNFOLDING
%token AT
%token VOLUMETRIC
%token NORMAL
%token OF
%token REFL VEL UNCORREFL ZDR WID
%token NONE
%token RAYDATA
%token ENDRADARIMAGE

%union {
  Charlen token;
}

%expect 59

%%

rapic_recognized : complete_header sweeps imageend
{
  if (radar_verbose_flag) fprintf(stderr, "SUCCESSFUL parse\n");
  sprintf(radar->h.name, "%s", rh.namestr);
  sprintf(radar->h.radar_name, "%s", rh.namestr);

  radar = fill_header(radar);
  radar = RSL_prune_radar(radar);
  rapic_radar = radar;
  YYACCEPT;
}

sweeps : sweep
       | sweeps sweep

sweep  : sweepheader rays ENDRADARIMAGE
{
  /* Attach the sweep to the volume. */
  if (radar_verbose_flag) fprintf(stderr, "Attach the sweep %d to the volume %d.\n",
		  isweep, ivolume);
  radar->v[ivolume]->sweep[isweep] = sweep;
  radar->v[ivolume]->h.f    = sweep->h.f;
  radar->v[ivolume]->h.invf = sweep->h.invf;
}

sweepheader : scanheader
{
  /*  float c =  RSL_SPEED_OF_LIGHT; */
  if (rh.angle_resolution != 0) 
	sweep = RSL_new_sweep((int)(360.0/rh.angle_resolution+0.5));
  if (fabs(rh.elev - save_elev) > .5) { /* New sweep elevation. */
	isweep++;
	save_elev = rh.elev;
  }
  nray = 0;
  /* rapic_nyquist = c*((float)rh.prf/10.)/(4.*(float)rh.freq*100000.0); */
}

imageend : IMAGEEND seqno imgno

complete_header : imageheader IMAGEHEADEREND
{
  if (radar_verbose_flag) fprintf(stderr, "sweepcount[0] = %d\n", sweepcount[0]);
  if (sweepcount[0] > 0) {
	radar->v[DZ_INDEX] = RSL_new_volume(sweepcount[0]);
	radar->v[DZ_INDEX]->h.type_str = strdup("Reflectivity");
  }
  if (radar_verbose_flag) fprintf(stderr, "sweepcount[1] = %d\n", sweepcount[1]);
  if (sweepcount[1] > 0) {
	volume = radar->v[VR_INDEX] = RSL_new_volume(sweepcount[1]);
	volume->h.type_str = strdup("Velocity");
	volume->h.calibr_const = 0.0;
  }
  if (radar_verbose_flag) fprintf(stderr, "sweepcount[2] = %d\n", sweepcount[2]);
  if (sweepcount[2] > 0) {
	radar->v[SW_INDEX] = RSL_new_volume(sweepcount[2]);
	volume->h.type_str = strdup("Spectral Width");
	volume->h.calibr_const = 0.0;
  }
  if (radar_verbose_flag) fprintf(stderr, "sweepcount[3] = %d\n", sweepcount[3]);
  if (sweepcount[3] > 0) {
	radar->v[ZD_INDEX] = RSL_new_volume(sweepcount[3]);
	volume->h.type_str = strdup("Reflectivity Depolarization Ratio");
	volume->h.calibr_const = 0.0;
  }
  if (radar_verbose_flag) fprintf(stderr, "sweepcount[4] = %d\n", sweepcount[4]);
  if (sweepcount[4] > 0) {
	radar->v[ZT_INDEX] = RSL_new_volume(sweepcount[4]);
	volume->h.type_str = strdup("Total Reflectivity");
	volume->h.calibr_const = 0.0;
  }
  isweep = -1; /* It keeps track of the sweep number across all field
                * types; volumes.  It is immediately bumped to 0 when
                * the sweepheader is parsed.
				*/
  save_elev = 99999;
}
                ;

imageheader : imageheader_item
            | imageheader imageheader_item
            | /* Empty */
            ;

imageheader_item : IMAGE seqno imgno
{
  radar = RSL_new_radar(MAX_RADAR_VOLUMES);
  sweepcount[0] = 0;
  sweepcount[1] = 0;
  sweepcount[2] = 0;
  sweepcount[3] = 0;
  sweepcount[4] = 0;
  radar->h.number = atoi($<token.s>2);
}
 
                 | IMAGESCANS number
                 | IMAGESIZE number
{
  if (atoi($<token.s>2) <= 0) {
	fprintf(stderr, "RAPIC: /IMAGESIZE == %d.  RAPIC ingest returning NULL.\n", atoi($<token.s>2));
	YYERROR;
  }
}
			     | scanlist
                 ;

scanlist : SCAN scanno ':' seqno datetime dc elev fieldno dc offset size
{
  ifield = atoi($<token.s>8);
  sweepcount[ifield]++;
}

/*
 * Now, describe some scan header fields.
 */

rays      : ray
          | rays ray
          | /* EMPTY */
          ;

ray : RAYDATA
 {

   /*   fprintf(stderr, "YACC len=%d text=<", yylval.token.len); */
   /*   binprint(yylval.token.s, yylval.token.len); */
   /*   fprintf(stderr, ">\n"); */

   /* Quiet the compilier, because I only use the rsl_f_list and rsl_invf_list. */
   RSL_ftype[0] = RSL_ftype[0];

   /* Use yylval.token.s and yylval.token.len */
   memset(outbuf, sizeof(outbuf), '\0');
   rapic_decode((unsigned char *)yylval.token.s, yylval.token.len, outbuf, &outbytes,
				&azim, &elev, &delta_time);
   /*   fprintf(stderr, "RAYDATA: ray %d, ivol %d, isweep %d, azim %f, elev %f, dtime %d, size=%d\n", nray, ivolume, isweep, azim, elev, delta_time, outbytes); */

   ray = RSL_new_ray(outbytes);
   rapic_load_ray_header(rh, nray, isweep, elev, azim, &ray->h); /* Mostly from the scanheader (rh). */
   ray->h.azimuth = azim;
   /*    if (39<azim && azim <40) { */
   ray->h.elev = elev;
   ray->h.sec += delta_time;
   ray->h.f    = RSL_f_list[ivolume]; /* Data conversion function. f(x). */
   ray->h.invf = RSL_invf_list[ivolume]; /* invf(x). */

   rapic_fix_time(ray);
   rapic_load_ray_data(outbuf, outbytes, ivolume, ray);
#define DODO
#undef DODO
#ifdef DODO
   if (ray->h.ray_num == 0 && ivolume == 1 && isweep == 0)
	 { int i;
   fprintf(stderr, "RAYDATA: ray %d, ivol %d, isweep %d, azim %f, elev %f, dtime %d, size=%d\n", nray, ivolume, isweep, azim, elev, delta_time, outbytes);
	 for (i=0; i<ray->h.nbins; i++) {
	   fprintf(stderr,"YACCray->range[%d] = %d  %f\n", i, (int)ray->range[i],
			   ray->h.f(ray->range[i]));
	 }
	 }
#endif
   /* Attach the ray to the sweep. */
   sweep->ray[nray]      = ray;
   sweep->h.beam_width   = ray->h.beam_width;
   sweep->h.vert_half_bw = sweep->h.beam_width / 2.0;
   sweep->h.horz_half_bw = sweep->h.beam_width / 2.0;
   sweep->h.sweep_num    = isweep;
   sweep->h.elev         = ray->h.elev;
   sweep->h.f            = ray->h.f;
   sweep->h.invf         = ray->h.invf;
   nray++;
   /*   } */
}

scanheader : scanheaditem
           | scanheader scanheaditem
           | /* EMPTY */
          ;

/* Each of these items are the header for a sweep. */

scanheaditem
: NAME        namestr { memmove(rh.namestr,$<token.s>2,$<token.len>2); }
| COUNTRY     code    { rh.country       = atoi($<token.s>2); }
| STNID		  idno    { rh.station_id_no = atoi($<token.s>2); }
| LATITUDE	  lat     { rh.lat           = atof($<token.s>2); }
| LONGITUDE	  lon     { rh.lon           = atof($<token.s>2); }
| HEIGHT	  alt     { rh.height        = atof($<token.s>2); }
| DATE		  datno   { rh.datno         = atoi($<token.s>2); }
| TIME		  hhmm    { rh.hhmm          = atof($<token.s>2); }
| TIMESTAMP	  yyyymoddhhmmss { memmove(rh.yyyymoddhhmmss,$<token.s>2,$<token.len>2); }
| VERS		  versionNumber  { rh.versionNumber    = atof($<token.s>2); }
| FREQUENCY   freq           { rh.freq             = atoi($<token.s>2); }
| PRF	      prf            { rh.prf              = atoi($<token.s>2); }
| PULSELENGTH len            { rh.pulselen         = atof($<token.s>2); }
| RNGRES	  gatewidth      { rh.range_resolution = atoi($<token.s>2); }
| ANGLERATE	  anglerate      { rh.anglerate        = atof($<token.s>2); }
| CLEARAIR	  clearair       { memmove(rh.clearair,$<token.s>2,$<token.len>2);}
| ANGRES	  angle          { rh.angle_resolution = atof($<token.s>2); }
| VIDRES	  res            { rh.video_resolution = atoi($<token.s>2); }
| STARTRNG	  rng            { rh.start_range      = atoi($<token.s>2); }
| ENDRNG	  rng            { rh.end_range        = atoi($<token.s>2); }
| PRODUCT 	  typeid BRACKETNUM { memmove(rh.product_type,$<token.s>2,$<token.len>2); }
| PRODUCT
| PASS		  noofnscans
| ELEV		  elev  { rh.elev    = atof($<token.s>2); }
| VELLVL	  level { rh.vellvl  = atof($<token.s>2); }
| NYQUIST	  nyq   
{
  rh.nyquist = atof($<token.s>2);
  rapic_nyquist = rh.nyquist;
}
| VIDEO		  field { memmove(rh.video,$<token.s>2,$<token.len>2); }
| IMGFMT	  type  { memmove(rh.imgfmt,$<token.s>2,$<token.len>2); }
| UNFOLDING	  ratio /* Already loaded: rh.ratio1, rh.ratio2 */
;

real     : number
         | FLOATNUMBER

number   : NUMBER

seqno    : number
scanno   : number
imgno    : number
datetime : number
dc       : real
         | ALPHA
elev     : real
fieldno  : number
offset   : number
size     : number
datno    : number

code     : number
namestr  : ALPHA
idno     : number
lat      : real
lon      : real
alt      : real
hhmm     : real

yyyymoddhhmmss : number
versionNumber  : real

freq       : number
prf        : number
len        : real
gatewidth  : number
angle      : real
anglerate  : real
clearair   : ON
           | OFF
res        : number
rng        : number
typeid     :  VOLUMETRIC
           |  NORMAL

noofnscans : no OF nscans
no         : number {rh.scannum = atoi($<token.s>1);}
nscans     : number {rh.ofscans = atoi($<token.s>1);}
type       : ALPHA

field : REFL      {ivolume = DZ_INDEX; volume = radar->v[ivolume];}
      | VEL       {ivolume = VR_INDEX; volume = radar->v[ivolume];}
      | UNCORREFL {ivolume = ZT_INDEX; volume = radar->v[ivolume];}
      | ZDR       {ivolume = ZD_INDEX; volume = radar->v[ivolume];}
      | WID       {ivolume = SW_INDEX; volume = radar->v[ivolume];}

level : real
nyq   : real

ratio : NONE        {rh.ratio1 = 0; rh.ratio2 = 0;}
| number ':' number {rh.ratio1 = atoi($<token.s>1); rh.ratio2 = atoi($<token.s>3);}



%%

int rapicerror(char *s)
{
  fprintf(stderr, "RAPIC ERROR: <%s> on token <", s);
  binprint(yylval.token.s, yylval.token.len);
  fprintf(stderr, ">\n");
  return 1;
}

int rapicwrap(char *s)
{
  yywrap(s);
  return 1;
}
