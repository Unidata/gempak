/***************************************************************
** Dcreanal                                                   **
** This program will convert a NCAR/NCEP reanalysis Netcdf    **
** data set into a GEMPAK gridded data file. COARDS data      **
** conventions are assumed.                                   **
**                                                            **
** Steve Chiswell	Unidata/UCAR	July, 1998            **
***************************************************************/
#include <stdio.h>
#include <math.h>
#include <netcdf.h>
#include <geminc.h>
#include <gemprm.h>

#include "na.h"

#ifdef	UNDERSCORE
#define	open_gemgrid	open_gemgrid_
#endif

/* prototypes */
void	put_gemgrid(const int *iacss, char *varname, int gribid,
                int vcord, int lev1, int lev2,
                int year, int month, int day, int hour,
                int nlat, int nlon, float *grid, int nbits, int *iret);

unsigned long
gregdate_to_julday(year, month, day)
    int         year;   /* Gregorian year */
    int         month;  /* Gregorian month (1-12) */
    int         day;    /* Gregorian day (1-31) */
{
    long                igreg = 15 + 31 * (10 + (12 * 1582));
    int                 iy;     /* signed, origin 0 year */
    int                 ja;     /* Julian century */
    int                 jm;     /* Julian month */
    int                 jy;     /* Julian year */
    unsigned long       julday; /* returned Julian day number */

    /*
     * Because there is no 0 BC or 0 AD, assume the user wants the start of
     * the common era if they specify year 0.
     */
    if (year == 0)
        year = 1;

    iy = year;
    if (year < 0)
        iy++;
    if (month > 2)
    {
        jy = iy;
        jm = month + 1;
    }
    else
    {
        jy = iy - 1;
        jm = month + 13;
    }

    /*
     *  Note: SLIGHTLY STRANGE CONSTRUCTIONS REQUIRED TO AVOID PROBLEMS WITH
     *        OPTIMISATION OR GENERAL ERRORS UNDER VMS!
     */
    julday = day + (int)(30.6001 * jm);
    if (jy >= 0)
    {
        julday += 365 * jy;
        julday += 0.25 * jy;
    }
    else
    {
        double          xi = 365.25 * jy;

        if ((int)xi != xi)
            xi -= 1;
        julday += (int)xi;
    }
    julday += 1720995;

    if (day + (31* (month + (12 * iy))) >= igreg)
    {
        ja = jy/100;
        julday -= ja;
        julday += 2;
        julday += ja/4;
    }

    return julday;
}

void julday_to_gregdate(unsigned long julday, int *year, int *month, int *day)
/*
    unsigned long       julday;          Julian day number to convert 
    int                 *year;           Gregorian year (out) 
    int                 *month;          Gregorian month (1-12) (out) 
    int                 *day;            Gregorian day (1-31) (out) 
*/
{
    long        ja, jb;
    int         jc;
    long        jd;
    int         je, iday, imonth, iyear;
    double      xc;

    if (julday < 2299161)
        ja = julday;
    else
    {
        int     ia = ((julday - 1867216) - 0.25) / 36524.25;

        ja = julday + 1 + ia - (int)(0.25 * ia);
    }

    jb = ja + 1524;
    xc = ((jb - 2439870) - 122.1) / 365.25;
    jc = 6680.0 + xc;
    jd = 365 * jc + (int)(0.25 * jc);
    je = (jb - jd) / 30.6001;

    iday = jb - jd - (int)(30.6001 * je);

    imonth = je - 1;
    if (imonth > 12)
        imonth -= 12;

    iyear = jc - 4715;
    if (imonth > 2)
        iyear -= 1;
    if (iyear <= 0)
        iyear -= 1;

    *year = iyear;
    *month = imonth;
    *day = iday;
}


void usage()
{
printf("Usage: dcreanal netcdffile gempakfile\n");
}

void do_reanal(int cdfid, char *gemfilin, int *ier)
{
int ndims,nvars,natts,nunlim;
size_t dimsiz,var_i[3],latsize,lonsize,levsize,start_i[4],count_i[4];
int time_id,time_dimid,lat_dimid,lon_dimid,lev_dimid,lat_id,lon_id,pres_id=-1;
int i,iret=0,x,y;
double timeval;
unsigned long julian_day,start_day;
int gyear,gmonth,gday,ghour;
int vcord,lev1,lev2=-1,nbits;
float pres_arr[LLMXGD], dmax, dmin, rdif, rbits;
float lat1,lon1,lat2,lon2;
int iflno=-1, nc;
int maxgrid = MMHDRS - 1;
char *gemfil,*gemfil_old,dattim[20],*cpos,idname[256];
/*int attnum;*/
char varname[20],levname[256];
int lev_id,varndims,levcnt,vcid,ivar,gribid;
float scale,offset,missval,vlev;
int time_in_days,time_in_months,mc;

int gribed, center, edition;
char wmotbl[80],ctbl[80],vtbl[80],ntbl[80];

/* initialize grib tables */
na_init (&iret);
center = 60; gribed = 1; edition = 2;
wmotbl[0] = ntbl[0] = vtbl[0] = ctbl[0] = '\0';
strcat(wmotbl,"ncarncep1.tbl"); strcat(ntbl,"ncarncep1.tbl");
strncat(vtbl,"?",1); strncat(ctbl,"?",1);
na_rtbl(&gribed,&center,&edition,wmotbl,ntbl,vtbl,ctbl,&iret);


gemfil = (char *)malloc(strlen(gemfilin) + 1);
gemfil_old = (char *)malloc(strlen(gemfilin) + 1);
gemfil_old[0] = '\0';

iret = nc_inq(cdfid,&ndims,&nvars,&natts,&nunlim);
/*printf("iret %d ndims %d nvars %d natts %d nunlim %d\n",iret,ndims,nvars,natts,nunlim);*/


/* see if vertical coordinate level exists */
iret = nc_inq_varid(cdfid,"level",&lev_id);
iret += nc_inq_dimid(cdfid,"level",&lev_dimid);
if(iret != 0) 
   {
   /* see if we have P(P) or Z(Z) instead of level, eg LDEO files */
   iret = nc_inq_varid(cdfid,"P",&lev_id);
   iret += nc_inq_dimid(cdfid,"P",&lev_dimid);

   if(iret != 0)
      {
      iret = nc_inq_varid(cdfid,"Z",&lev_id);
      iret += nc_inq_dimid(cdfid,"Z",&lev_dimid);
      }
   
   if(iret != 0) lev_id = -1;
   }

iret = nc_inq_varid(cdfid,"time",&time_id);
gyear = 1; gmonth = 1; gday = 1; time_in_days = 0; time_in_months = 0;
if(iret != 0)
   {
   iret = nc_inq_varid(cdfid,"T",&time_id);
   iret = nc_get_att_text(cdfid,time_id,"units",varname);
   if(iret == 0)
      {
      if((cpos = (char *)strstr(varname,"days since")) != NULL)
         {
         sscanf(cpos+10,"%d-%d-%d",&gyear,&gmonth,&gday);
         time_in_days = 1;
         }
      else if((cpos = (char *)strstr(varname,"months since")) != NULL)
         {
         sscanf(cpos+12,"%d-%d-%d",&gyear,&gmonth,&gday);
         time_in_months = 1;
         }
      }
   iret = nc_inq_dimid(cdfid,"T",&time_dimid);
   }

if(nunlim != -1)
   iret = nc_inq_dimlen(cdfid,nunlim,&dimsiz);
else
   nunlim = nc_inq_dimlen(cdfid,time_dimid,&dimsiz);


iret = nc_inq_dimid(cdfid,"lat",&lat_dimid);
iret += nc_inq_dimlen(cdfid,lat_dimid,&latsize);
iret += nc_inq_varid(cdfid,"lat",&lat_id);
if(iret != 0)
   {
   iret = nc_inq_dimid(cdfid,"Y",&lat_dimid);
   iret += nc_inq_dimlen(cdfid,lat_dimid,&latsize);
   iret += nc_inq_varid(cdfid,"Y",&lat_id);
   }

iret = nc_inq_dimid(cdfid,"lon",&lon_dimid);
iret += nc_inq_dimlen(cdfid,lon_dimid,&lonsize);
iret += nc_inq_varid(cdfid,"lon",&lon_id);
if(iret != 0)
   {
   iret = nc_inq_dimid(cdfid,"X",&lon_dimid);
   iret += nc_inq_dimlen(cdfid,lon_dimid,&lonsize);
   iret += nc_inq_varid(cdfid,"X",&lon_id);
   }

printf("\n********************\n");
if(lev_id != -1)
   {
   iret = nc_inq_varname(cdfid,lev_id,idname);
   printf("Vertical coordinate id %s\n",idname);
   }
iret = nc_inq_varname(cdfid,lat_id,idname);
printf("Latitude coordinate %s  size %d\n",idname,latsize);
iret = nc_inq_varname(cdfid,lon_id,idname);
printf("Longitude coordinate %s  size %d\n",idname,lonsize);
iret = nc_inq_varname(cdfid,time_id,idname);
printf("Time coordinate %s\n",idname);


ivar = 0;
while(ivar < nvars)
   {
   if((ivar == lev_id)||(ivar == lat_id)||(ivar == lon_id)||(ivar == time_id))
      {
      ivar++;
      continue;
      }

   iret = nc_inq_varname(cdfid,ivar,idname);
   printf("Variable parameter %s\n********************\n",idname);

/*
 *    see if we can fall back and use the netcdf variable name
 
   iret = nc_inq_attid(cdfid,ivar,"GRIB_name",&attnum);
   if(iret != 0) iret = nc_inq_attid(cdfid,ivar,"grib_name",&attnum);
 */
      
   if(iret == 0) 
      {
      if(lev_id != -1)
         {
         iret = nc_inq_dimlen(cdfid,lev_dimid,&levsize);
         iret = nc_get_att_int(cdfid,lev_id,"GRIB_id",&vcid);
         if(iret != 0)
            iret = nc_get_att_int(cdfid,ivar,"gribleveltype",&vcid);
         }
      else
         levsize = 1;

/*iret += nc_inq_varid(cdfid,"pres",&pres_id);*/
/*printf("number of records %d lats %d lons %d\n",(int)dimsiz,(int)latsize,(int)lonsize);*/


      pres_id = ivar;
      iret = nc_get_att_text(cdfid,pres_id,"GRIB_name",varname);
      if(iret != 0) iret = nc_get_att_text(cdfid,pres_id,"grib_name",varname);
      if(iret != 0)
        {
	cst_lcuc ( idname, varname, &iret);
	if (iret != 0 ) continue;
        cst_lstr ( varname, &nc, &iret);
	if (iret != 0 ) continue;
	/*if ( nc < 4 )
           {
	   for(i=nc; i<4; i++) varname[i]='Z';
	   nc = 4;
	   }*/
	if (nc <= 8)
	   varname[nc] = '\0';
	else
	   varname[8] = '\0';

        printf("Could not get GRIB name, using variable name %s\n", varname);
        /*continue;*/
        }
      gribid = 255; /* use gribid=255 for undefined number */
      iret = nc_get_att_int(cdfid,pres_id,"GRIB_id",&gribid);
      if(iret != 0) iret = nc_get_att_int(cdfid,pres_id,"gribparam",&gribid);

      iret = nc_get_att_float(cdfid,pres_id,"scale_factor",&scale); 
      if(iret != 0) scale = 1;

      iret = nc_get_att_float(cdfid,pres_id,"add_offset",&offset); 
      if(iret != 0) offset = 0;

      iret = nc_get_att_float(cdfid,pres_id,"missing_value",&missval); 
      printf("%s %d scale %f offset %f missing %f\n",varname,gribid,scale,offset,missval);
      }
   else
      {
      ivar++;
      continue;
      }

   /* get grids and write to gempak file */
   iret = nc_inq_varndims(cdfid,pres_id,&varndims);

   start_day = gregdate_to_julday(gyear,gmonth,gday);

   var_i[0] = 0;
   iret = nc_get_var1_float(cdfid,lat_id,var_i,&lat1);
   iret = nc_get_var1_float(cdfid,lon_id,var_i,&lon1);
   var_i[0] = latsize - 1;
   iret = nc_get_var1_float(cdfid,lat_id,var_i,&lat2);
   var_i[0] = lonsize - 1;
   iret = nc_get_var1_float(cdfid,lon_id,var_i,&lon2);


   if((varndims < 3)||(varndims > 4))
      {
      printf("can't handle this many dimensions for a variable %d\n",
         varndims);
      ivar++;
      continue;
      }

   for(i=0;i< (int)dimsiz;i++)
      {
      var_i[0] = i; 
      iret = nc_get_var1_double(cdfid,time_id,var_i,&timeval);
      if((time_in_days != 1)&&(time_in_months != 1))
         {
	 /* assume time in hours */
         if(timeval < 0) 
            {
	    printf("skipping negative time\n");
            continue;
            }
         julian_day = (unsigned long)floor(timeval / 24.0);
         ghour = (int)(timeval - (julian_day * 24) );
         }
      else if(time_in_days == 1)
         {
         julian_day = (unsigned long)timeval;
         ghour = 0;
         }
      else if(time_in_months == 1)
         {
         if(timeval < 0)
            {
            for(mc = 0;mc > timeval;mc--)
               {
               gmonth = gmonth - 1;
               if(gmonth == 0)
                  {
                  gmonth = 12; gyear = gyear - 1;
                  }
               }
            }
         else
            {
            for(mc = 0;mc < (int)timeval;mc++)
               {
               gmonth = gmonth + 1;
               if(gmonth == 13)
                  {
                  gmonth = 1; gyear = gyear + 1;
                  }
               }
            }
	 julian_day = 0;
         ghour = 0;
         start_day = gregdate_to_julday(gyear,gmonth,gday);
         }
      julday_to_gregdate(julian_day+start_day, &gyear, &gmonth, &gday);


      /*printf("   %d year/month/day %d/%d/%d hour %d\n",i,gyear,gmonth,gday,ghour);*/

      levcnt = 0;
      while(levcnt < levsize)
         {
      if(varndims == 3)
         { 
         lev1 = 0;
         start_i[0] = i; start_i[1] = 0; start_i[2] = 0; start_i[3] = 0;
         count_i[0] = 1; count_i[1] = latsize; count_i[2] = lonsize; count_i[3] = 0;
         iret = nc_get_att_text(cdfid,pres_id,"level_desc",levname);
         if(iret != 0)
            {
            /* see if we have a grib id name for constant level surface */
            iret = nc_get_att_int(cdfid,ivar,"gribleveltype",&vcid);
            if(iret == 0)
               vcord = vcid;
            else 
               vcord = 0;
            }
         else
            {
            vcord = 0;
            if(strstr(levname,"Entire Atmosphere") != 0) vcord = 200;
            if(strstr(levname,"Surface") != 0) 
               {
               vcord = 0;
               iret = nc_get_att_text(cdfid,pres_id,"var_desc",levname);
               if((iret == 0)&&(strstr(levname,"10-meter AGL") != 0))
                  {
                  lev1 = 10;
                  vcord = 105;
                  }
               }
            /*printf("level_desc name %d %s\n",vcord,levname);*/
            }
         }
      else if(varndims == 4)
         {
         var_i[0] = levcnt;
         iret = nc_get_var1_float(cdfid,lev_id,var_i,&vlev);
         /*printf("look levsize %d %d %f vcid %d\n",levcnt,levsize,vlev,vcid);*/
         lev1 = (int)vlev;
         start_i[0] = i; start_i[1] = levcnt; start_i[2] = 0; start_i[3] = 0;
         count_i[0] = 1; count_i[1] = 1; count_i[2] = latsize; count_i[3] = lonsize; 
         vcord = vcid;
         }
      else
         {
	 printf("unknown varndims %d\n",varndims);
         }
      iret = nc_get_vara_float(cdfid,pres_id,start_i,count_i,pres_arr);
   
      dmax = dmin = RMISSD;
      for(x=0;x<latsize;x++)
         for(y=0;y<lonsize;y++)
            {
            /*pres_arr[(x*lonsize)+y] = ((pres_arr[(x*lonsize)+y]*10)+367650)/100.0;*/
            if(pres_arr[(x*lonsize)+y] != missval)
	       {
               pres_arr[(x*lonsize)+y] = (pres_arr[(x*lonsize)+y]*scale)+offset;
	       if ( ( pres_arr[(x*lonsize)+y] > dmax ) || ( dmax == RMISSD ) )
		  dmax = pres_arr[(x*lonsize)+y];
	       if ( ( pres_arr[(x*lonsize)+y] < dmin ) || ( dmin == RMISSD ) )
		  dmin = pres_arr[(x*lonsize)+y];
	       }
            else
               pres_arr[(x*lonsize)+y] = -9999.0;
            /*
            var_i[0] = x; 
            iret = nc_get_var1_float(cdfid,lat_id,var_i,&lat);
            var_i[0] = y;
            iret = nc_get_var1_float(cdfid,lon_id,var_i,&lon);
            printf("%d %d %d lat %f lon %f pres %f \n",x,y,(x*lonsize)+y,lat,lon,
               pres_arr[(x*lonsize)+y]); */
            }

      if ( ( dmax != RMISSD ) && ( dmin != RMISSD ) ) {
          rdif = dmax - dmin;
          if ( rdif == 0 ) {
             nbits = 2;
          }
          else {
             rbits = fabs (log (rdif)) / log (2.0);
             nbits = (int) rbits + 1;
             if (nbits > 32) 
	       nbits = 32;
	     else if ( nbits < 2 )
	       nbits = 2;
          }
      }
      else {
          printf("All missing grid\n");
	  nbits = 2;
      }

      /* replace year here since we have a 4 digit year already */
      if ( strstr(gemfilin, "YYYY") != NULL ) {
	 sprintf(dattim,"%04d\0",gyear);
         cst_rpst ( gemfilin, "YYYY", dattim, gemfil, &iret);
      }
      else
         strcpy(gemfil,gemfilin);

      sprintf(dattim,"%02d%02d%02d/%02d00\0",gyear%100,gmonth,gday,ghour); 
      cfl_mnam (dattim,gemfil,gemfil,&iret);


      if((iflno > -1) && (strcmp(gemfil,gemfil_old) != 0))
         {
         gd_clos (&iflno, &iret);
         iflno = -1;
         }
      if(iflno < 0)
         {
         open_gemgrid(gemfil,&latsize,&lonsize,&lat1,&lon1,&lat2,&lon2,&maxgrid,&iflno,strlen(gemfil));
         }
      /*for(nc=0;nc<latsize*lonsize;nc++)
         printf("data val %d %f\n",nc,pres_arr[nc]);*/
      /*old fortran call put_gemgrid(&iflno,varname,&gribid,&vcord,&lev1,&lev2,&gyear,&gmonth,&gday,&ghour,
         &latsize,&lonsize,pres_arr,&nbits,&lat1,&lon1,&lat2,&lon2,strlen(varname));*/
      put_gemgrid(&iflno,varname,gribid,vcord,lev1,lev2,gyear,gmonth,gday,ghour,
         latsize,lonsize,pres_arr,nbits, &iret);

      strcpy(gemfil_old,gemfil);

          levcnt++;
          if(varndims == 3) levcnt = levsize;
          } /* while levcnt < levsize */
      }
   
   if(iflno != -1)
      {
      gd_clos (&iflno, &iret);
      iflno = -1;
      }


   ivar++;
   }

}

int main(int argc, char *argv[])
{
int cdfid,ier;

if(argc != 3)
   {
   usage();
   exit(-1);
   }

ier = nc_open(argv[1],0,&cdfid);
if(ier != 0)
   {
   printf("Could not open %s\n",argv[1]);
   usage();
   exit(-1);
   }

in_bdta (&ier);
do_reanal(cdfid,argv[2],&ier);


ier = nc_close(cdfid);   
}

