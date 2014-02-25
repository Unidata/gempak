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
#include <ulog.h>
#include <geminc.h>
#include <gemprm.h>

#ifdef	UNDERSCORE
#define	open_gemgrid	open_gemgrid_
#define put_gemgrid	put_gemgrid_
#endif

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

julday_to_gregdate(julday, year, month, day)
    unsigned long       julday;         /* Julian day number to convert */
    int                 *year;          /* Gregorian year (out) */
    int                 *month;         /* Gregorian month (1-12) (out) */
    int                 *day;           /* Gregorian day (1-31) (out) */
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

void do_reanal(cdfid,gemfilin,ier)
int cdfid,*ier;
char *gemfilin;
{
int ndims,nvars,natts,nunlim;
size_t dimsiz,var_i[3],latsize,lonsize,levsize,start_i[4],count_i[4];
int time_id,lat_dimid,lon_dimid,lev_dimid,lat_id,lon_id,pres_id=-1;
int i,iret=0,x,y;
double timeval;
unsigned long julian_day,start_day;
int gyear,gmonth,gday,ghour;
int vcord,lev1,lev2=-1,nbits=10;
float pres_arr[97000],lat,lon;
float lat1,lon1,lat2,lon2;
int iflno=-1,iflcount=0;
int maxgrid = 9800;
char *gemfil,*gemfil_old,dattim[20];
int attnum;
char varname[20],levname[256];
int lev_id,varndims,levcnt,vcid,ivar,gribid;
float scale,offset,missval,vlev;

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
/*printf("ndims %d nvars %d natts %d nunlim %d\n",ndims,nvars,natts,nunlim);*/


/* see if vertical coordinate level exists */
iret = nc_inq_varid(cdfid,"level",&lev_id);
iret += nc_inq_dimid(cdfid,"level",&lev_dimid);
if(iret != 0) lev_id = -1;
/*printf("level id = %d\n",lev_id);*/

iret = nc_inq_dimlen(cdfid,nunlim,&dimsiz);
iret += nc_inq_varid(cdfid,"time",&time_id);
iret += nc_inq_dimid(cdfid,"lat",&lat_dimid);
iret += nc_inq_dimid(cdfid,"lon",&lon_dimid);
iret += nc_inq_dimlen(cdfid,lat_dimid,&latsize);
iret += nc_inq_dimlen(cdfid,lon_dimid,&lonsize);
if(lev_id != -1)
   {
   iret += nc_inq_dimlen(cdfid,lev_dimid,&levsize);
   iret += nc_get_att_int(cdfid,lev_id,"GRIB_id",&vcid);
   }
else
   levsize = 1;

iret += nc_inq_varid(cdfid,"lat",&lat_id);
iret += nc_inq_varid(cdfid,"lon",&lon_id);
/*iret += nc_inq_varid(cdfid,"pres",&pres_id);*/
/*printf("number of records %d lats %d lons %d\n",(int)dimsiz,(int)latsize,(int)lonsize);*/

ivar = 0;
while(ivar < nvars)
   {
   if(ivar == lev_id)
      {
      ivar++;
      continue;
      }
   iret = nc_inq_attid(cdfid,ivar,"GRIB_name",&attnum);
   if(iret == 0) 
      {
      pres_id = ivar;
      iret = nc_get_att_text(cdfid,pres_id,"GRIB_name",varname);
      if(iret != 0)
        {
        printf("Could not get GRIB name\n");
        continue;
        }
      gribid = -9999;
      iret += nc_get_att_int(cdfid,pres_id,"GRIB_id",&gribid);
      iret += nc_get_att_float(cdfid,pres_id,"scale_factor",&scale); 
      iret += nc_get_att_float(cdfid,pres_id,"add_offset",&offset); 
      iret += nc_get_att_float(cdfid,pres_id,"missing_value",&missval); 
      /*printf("%s %d scale %f offset %f missing %f\n",varname,gribid,scale,offset,missval);*/
      /*printf("processing %s\n",varname);*/
      }
   else
      {
      ivar++;
      continue;
      }

   /* get grids and write to gempak file */
   iret = nc_inq_varndims(cdfid,pres_id,&varndims);
   gyear = 1; gmonth = 1; gday = 1;
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
      if(timeval < 1) continue;
      julian_day = (unsigned long)floor(timeval / 24.0);
      ghour = (int)(timeval - (julian_day * 24) );
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
            vcord = 0;
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
      if(varndims == 4)
         {
         var_i[0] = levcnt;
         iret = nc_get_var1_float(cdfid,lev_id,var_i,&vlev);
         /*printf("look levsize %d %d %f vcid %d\n",levcnt,levsize,vlev,vcid);*/
         lev1 = (int)vlev;
         start_i[0] = i; start_i[1] = levcnt; start_i[2] = 0; start_i[3] = 0;
         count_i[0] = 1; count_i[1] = 1; count_i[2] = latsize; count_i[3] = lonsize; 
         vcord = vcid;
         }
      iret = nc_get_vara_float(cdfid,pres_id,start_i,count_i,pres_arr);

      for(x=0;x<latsize;x++)
         for(y=0;y<lonsize;y++)
            {
            /*pres_arr[(x*lonsize)+y] = ((pres_arr[(x*lonsize)+y]*10)+367650)/100.0;*/
            if(pres_arr[(x*lonsize)+y] != missval)
               pres_arr[(x*lonsize)+y] = (pres_arr[(x*lonsize)+y]*scale)+offset;
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

      dattim[0] = '\0'; sprintf(dattim,"%02d%02d%02d/%02d00\0",gyear%100,gmonth,gday,ghour); 
      gemfil[0] = '\0'; strcpy(gemfil,gemfilin);
      cfl_mnam (dattim,gemfilin,gemfil,&iret);

      if((iflno > -1) && (strcmp(gemfil,gemfil_old) != 0))
         {
         gd_clos (&iflno, &iret);
         iflno = -1;
         }
      if(iflno < 0)
         {
         printf("opening %s %s\n",gemfil,dattim);
         open_gemgrid(gemfil,&latsize,&lonsize,&lat1,&lon1,&lat2,&lon2,&maxgrid,&iflno,strlen(gemfil));
         }
      put_gemgrid(&iflno,varname,&gribid,&vcord,&lev1,&lev2,&gyear,&gmonth,&gday,&ghour,
         &latsize,&lonsize,pres_arr,&nbits,&lat1,&lon1,&lat2,&lon2,strlen(varname));

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

main(argc,argv)
int argc;
char *argv[];
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
   uerror("Could not open %s\0",argv[1]);
   usage();
   exit(-1);
   }

in_bdta (&ier);
do_reanal(cdfid,argv[2],&ier);


ier = nc_close(cdfid);   
}

