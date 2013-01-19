#include "geminc.h"
#include "gemprm.h"

#include "rcm.h"


static int radinfo_col=0;
static int maxtop_col=0;
static float maxtop_filter = 0.0;
static int cntr_col=0,cntr_wid=1,cntr_type=2;
static float cntr_sz=1.0, cntr_filter=0.0;
static int meso_col=0,meso_mark=2,meso_hw=1,meso_wid=1;
static float meso_sz=1.0;
static float meso_filter = 0.0;
static int tvs_col=0,tvs_mark=1,tvs_hw=1,tvs_wid=1;
static float tvs_sz=1.0;
static float tvs_filter = 0.0;

void set_radinfo(int *i)
{
radinfo_col = *i;
}

void set_maxtop(char *maxtop)
{
int iret;
char *cpos,outstr[80];
cpos = cst_split(maxtop, ';', 79, outstr, &iret);

if ( iret == 0 )
   cst_numb( outstr, &maxtop_col, &iret);

if ( cpos != NULL )
   in_filt( cpos, &maxtop_filter, &iret, strlen(cpos));
}

void set_cntr(char *cntr)
{
int iret,nexp=3,num;
float rdef=0,rval[3];
static char sep[]="/";
char *cpos, outstr[80];


cpos = cst_split(cntr, ';', 79, outstr, &iret);

if(iret == 0)
   {
   st_rlst(outstr,sep,&rdef,&nexp,rval,&num,&iret,strlen(outstr),strlen(sep));

   cntr_col = (int)rval[0];

   if(rval[1] != rdef) cntr_sz = rval[1];
   if(rval[2] != rdef) cntr_wid = (int)rval[2];
   }

if(cpos != NULL)
   {
   in_filt( cpos, &cntr_filter, &iret, strlen(cpos));
   }

}

void set_tvs(char *tvs)
{
int iret;
char *cpos,outstr[80];

cpos = cst_split(tvs, ';', 79, outstr, &iret);
if(iret == 0)
   {
   in_mark(outstr,&tvs_col,&iret, strlen(outstr));
   gqmrkr(&tvs_mark,&tvs_hw,&tvs_sz,&tvs_wid,&iret);
   }

if( cpos != NULL )
   in_filt( cpos, &tvs_filter, &iret, strlen(cpos));
}

void set_meso(char *meso)
{
int iret;
char *cpos,outstr[80];

cpos = cst_split(meso, ';', 79, outstr, &iret);
if(iret == 0)
   {
   in_mark(outstr,&meso_col,&iret,strlen(outstr));
   gqmrkr(&meso_mark,&meso_hw,&meso_sz,&meso_wid,&iret);
   }

if( cpos != NULL )
   in_filt( cpos, &meso_filter, &iret, strlen(cpos));
}

void radar_info(rad_struct RADARS[], int NEXSTNS)
{
int i,j,np=1,iret;
float x,y,sped,drct;
float rotat=0;
float rwmrk, rhmrk, rwtxt, rhtxt, rwbrb, rhbrb;
int ixoff=0,iyoff=0;
char plotstr[8];

float nx[1], ny[1], gx[1], gy[1]; 

char gcord[]="G", ncord[]="N";
char NEstr[] = "NE";
char OMstr[] = "OM";
char NAstr[] = "NA";
cntr_struct *next_cntr;
int *dataptr;

gqsysz ( &rwmrk, &rhmrk, &rwtxt, &rhtxt, &rwbrb, &rhbrb, &iret);

if(radinfo_col > 0) 
   {
   gscolr(&radinfo_col,&iret);
   for(i=0;i<NEXSTNS;i++)
      {
      get_xy(RADARS[i].stnlat,RADARS[i].stnlon,&x,&y);
      if(RADARS[i].mode == MDNE)
         gtext(gcord,&x,&y,NEstr,&rotat,&ixoff,&iyoff,&iret, strlen(gcord),strlen(NEstr));
      else if(RADARS[i].mode == MDOM)
         gtext(gcord,&x,&y,OMstr,&rotat,&ixoff,&iyoff,&iret, strlen(gcord),strlen(OMstr));
      else if(RADARS[i].mode == MDNA)
         gtext(gcord,&x,&y,NAstr,&rotat,&ixoff,&iyoff,&iret, strlen(gcord),strlen(NAstr));
      }
   }

if(cntr_col > 0)
   {
   gscolr(&cntr_col,&iret);
   gsbarb(&cntr_sz,&cntr_wid,&cntr_type,&iret);

   filter_init();

   for(i=0;i<NEXSTNS;i++)
      {
      for(j=0;j<RADARS[i].ncntr;j++)
         {
         get_xy4(RADARS[i].stnlat,RADARS[i].stnlon,RADARS[i].cntr[j].ggg,&x,&y);
         gx[0] = x + .125; gy[0] = y - .125;         
         gtrans ( gcord, ncord, &np, gx, gy, nx, ny, &iret,
            strlen(gcord),strlen(ncord));

	 if((nx[0] >= 0)&&(nx[0] <= 1)&&(ny[0] >= 0)&&(ny[0] <= 1))
	    n_filter (np, nx, ny, rhbrb, rwbrb, cntr_filter, &(RADARS[i].cntr[j]));

         } 
      }

   filter_set_head();
   while(filter_retrieve((void *)(&next_cntr), &x, &y))
      {
      sped = next_cntr->spd;
      drct = next_cntr->dir;
      gbarb(ncord,&np,&x,&y,&sped,&drct,&iret,strlen(ncord));
      } 
   }

if(maxtop_col > 0)
   {
   gscolr(&maxtop_col,&iret);

   filter_init();

   for(i=0;i<NEXSTNS;i++)
      {
      if(RADARS[i].maxtop > 0)
         {
         get_xy4(RADARS[i].stnlat,RADARS[i].stnlon,RADARS[i].maxtop_ggg,&x,&y);

	 gx[0] = x + .125; gy[0] = y - .125;
         gtrans ( gcord, ncord, &np, gx, gy, nx, ny, &iret,
            strlen(gcord),strlen(ncord));
         /* char string is 3 characters */
	 if((nx[0] >= 0)&&(nx[0] <= 1)&&(ny[0] >= 0)&&(ny[0] <= 1))
	    n_filter (np, nx, ny, rhtxt, rwtxt*2.7, maxtop_filter, &(RADARS[i].maxtop) );
         }
      }
   ixoff = 0; iyoff = 0; rotat = 0;
   filter_set_head();
   while(filter_retrieve((void *)(&dataptr), &x, &y))
      {
      sprintf(plotstr,"%03d\0",*dataptr);
      gtextc(ncord,&x,&y,plotstr,&rotat,&ixoff,&iyoff,&iret,
            strlen(ncord),strlen(plotstr));
      }
   }

if(meso_col > 0)
   {
   gscolr(&meso_col,&iret);
   gsmrkr(&meso_mark,&meso_hw,&meso_sz,&meso_wid,&iret);

   filter_init();

   for(i=0;i<NEXSTNS;i++)
      {
      for(j=0;j<RADARS[i].nmeso;j++)
         {
         get_xy4(RADARS[i].stnlat,RADARS[i].stnlon,RADARS[i].meso[j].ggg,&x,&y);

	 gx[0] = x + .125; gy[0] = y - .125;
         gtrans ( gcord, ncord, &np, gx, gy, nx, ny, &iret,
            strlen(gcord),strlen(ncord));
	 if((nx[0] >= 0)&&(nx[0] <= 1)&&(ny[0] >= 0)&&(ny[0] <= 1))
	    n_filter (np, nx, ny, rhmrk, rwmrk, meso_filter, NULL);
         RADARS[i].meso[j].nx = nx[0];
         RADARS[i].meso[j].ny = ny[0];
         }
      }
   filter_set_head();
   while(filter_retrieve((void *)(&dataptr), &x, &y))
      {
      gmark(ncord,&np,&x,&y,&iret,strlen(ncord));
      }
   }

if(tvs_col > 0)
   {
   gscolr(&tvs_col,&iret);
   gsmrkr(&tvs_mark,&tvs_hw,&tvs_sz,&tvs_wid,&iret);

   filter_init();

   for(i=0;i<NEXSTNS;i++)
      {
      for(j=0;j<RADARS[i].ntvs;j++)
         {
         get_xy4(RADARS[i].stnlat,RADARS[i].stnlon,RADARS[i].tvs[j].ggg,&x,&y);
         gx[0] = x + .125; gy[0] = y - .125;
         gtrans ( gcord, ncord, &np, gx, gy, nx, ny, &iret,
            strlen(gcord),strlen(ncord));
	 if((nx[0] >= 0)&&(nx[0] <= 1)&&(ny[0] >= 0)&&(ny[0] <= 1))
            n_filter (np, nx, ny, rhmrk, rwmrk, tvs_filter, NULL);
         RADARS[i].tvs[j].nx = nx[0];
         RADARS[i].tvs[j].ny = ny[0];
         }
      }
   filter_set_head();
   while(filter_retrieve((void *)(&dataptr), &x, &y))
      {
      gmark(ncord,&np,&x,&y,&iret,strlen(ncord));
      }
   }

}
