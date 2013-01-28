#include <geminc.h>
#include <gemprm.h>
#include <clocmn.h>

#include "gpwarn.h"

#ifdef UNDERSCORE
#define gline           gline_
#define gfill           gfill_
#define gscolr          gscolr_
#define gsline          gsline_

#define plot_slsc	plot_slsc_
#endif

extern	int	*outluns, *outnlun;

extern  CLO_t   clo;

void boundry_segs();


void clo_off(char *srchstr, int bindex, int col1, int col2, int ltype, int lwidth)
{
int i,j,k,l,FOUND;

/*printf("clo_off search string %s\n",srchstr);
printf("bounds file %s\n",clo.loc[bindex].bnd.filename);*/

i = 0; FOUND = 0;
while((i<clo.loc[bindex].bnd.nbnd)&&(FOUND == 0))
   {
   if(strstr(clo.loc[bindex].bnd.bound[i].info,srchstr) != 0)
      FOUND++;
   else
      i++;
   }

if(FOUND == 0) return;

for(j=0;j<clo.loc[bindex].bnd.bound[i].nparts;j++)
   {
   boundry_segs(clo.loc[bindex].bnd.bound[i].bndspt[j].strec,
      clo.loc[bindex].bnd.bound[i].bndspt[j].npts,
      clo.loc[bindex].bnd.filename, col1, col2, ltype, lwidth);
   }
}

void bounds_fill(float *X, float *Y, int nsegs, int color)
{
static char navigate[]="M";
float x1[LLMXPT],y1[LLMXPT];
int ltype,lhw,lwidth,whw,segc;
int i,iret;


   ltype = 1;
   lhw = 0; lwidth = 1; whw = 0;
   gsline (&ltype,&lhw,&lwidth,&whw,&iret);
   if(iret == 0)
      {
      gscolr (&color,&iret);
      /* gplt fill routine can only accept up to LLMXPT segments */
      if(nsegs > LLMXPT)
         {
         segc = LLMXPT;
         gfill (navigate,&segc,X,Y,&iret,strlen(navigate));
         while(segc < nsegs)
            {
            x1[0] = X[0]; y1[0] = Y[0];
            x1[1] = X[segc-1]; y1[1] = Y[segc-1];
            i = 2;
            while((i < LLMXPT)&&(segc < nsegs))
               {
               x1[i] = X[segc];
               y1[i] = Y[segc];
               i++;
               segc++;
               }
            gfill (navigate,&i,x1,y1,&iret,strlen(navigate));
            }
         }
      else
         gfill(navigate,&nsegs,X,Y,&iret,strlen(navigate));
      }
}

void bounds_line(float *X, float *Y, int nsegs, int color, int ltype, int lwidth)
{
static char navigate[]="M";
int lhw = 0;
int whw = 0;

int iret;

      gsline (&ltype,&lhw,&lwidth,&whw,&iret);
      gscolr (&color,&iret);
      if(iret != 0) printf("iret 3 %d\n",iret);
      gline (navigate,&nsegs,X,Y,&iret,strlen(navigate));
}


void boundry_segs(long int offset, int npts, char *bndtbl, int col1, int col2, 
			int ltype, int lwidth)
{
FILE *fp;
int iret;
int nsegs,i,j,k;
float mxlat,mnlat,mxlon,mnlon;
float *X,*Y;
float lt[4],ln[4];
char buf[256];
static char bounddir[]="bounds";

fp = (FILE *)cfl_tbop(bndtbl,bounddir,&iret);
if(iret == 0)
   cfl_seek(fp,offset,SEEK_SET,&iret);
else
   {
   printf("open failed %d\n",iret);
   return;
   }

if(iret == 0)
   {
   X = (float *)malloc(npts*sizeof(float));
   Y = (float *)malloc(npts*sizeof(float));
   cfl_rdln(fp,255,buf,&iret);
   /*printf("look cfl_rdln %d %s\n",iret,buf);*/
   i = 0;
   if(iret == 0)
      {
      sscanf(buf,"%d %f %f %f %f %f %f",&nsegs,&mxlat,&mnlat,&mxlon,&mnlon,X,Y);
      i++;
      while((i < nsegs/2)&&(iret == 0))
         {
         cfl_rdln(fp,255,buf,&iret);
         if(iret == 0)
            {
            j = sscanf(buf,"%f %f %f %f %f %f %f %f",
               lt,ln,lt+1,ln+1,lt+2,ln+2,lt+3,ln+3);
            for(k=0;k<j;k+=2)
               {
               X[i] = lt[k/2]; Y[i] = ln[k/2]; i++;
               } 
            }
         }
      }
   bounds_fill(X,Y,nsegs/2,col1);

   bounds_line(X, Y, nsegs/2, col2, ltype, lwidth);
   free(X); free(Y);
   }
cfl_clos(fp,&iret);

}


void plot_ugc(int type, char *zone, int col1, int col2, int ltype, 
		int lwidth, int ftype )

/****************************************************************/
/* int	type	0=Zone, 1=County				*/
/* char	*zone	6 character ugc, xxTiii				*/
/*			xx = state or zone class id		*/
/*			T = type (Zone Z or County C)		*/
/* int col1	Fill color					*/
/* int col2	Outline color					*/
/* int ltype	Outline line type 				*/
/* int lwidth	Outline line width				*/
/* int ftype	Fill pattern					*/
/*								*/
/* Chiz/Unidata							*/
/****************************************************************/
{
int ier,iret;
int FOUND,i;
char srchstr[30];
int bindex,sindex, iftyp;
float szfil=1.0;

clo_init(&ier);

iftyp = ftype;
if(iftyp < 0) iftyp = 1;

gsfill(&szfil,&iftyp,&ier);
if(ier != 0)
   {
   iftyp = 1;
   gsfill(&szfil,&iftyp,&ier);
   }

switch(type)
   {
   case 0:
	/*
	** Lookup FIPS number in county table as search string
	*/
        sindex = clo_which("COUNTY");
        i = 0; FOUND = 0;
        while((i<clo.loc[sindex].stn.nstn)&&(FOUND == 0))
           {
           if(strcmp(clo.loc[sindex].stn.station[i].id,zone) == 0)
              {
              FOUND = 1;
              /*printf("%s %s %d\n",clo.loc[sindex].stn.station[i].id,
                 clo.loc[sindex].stn.station[i].desc,
                 clo.loc[sindex].stn.station[i].nm);*/
              }
           else
              i++;
           }
        if(FOUND != 0)
           {
           bindex = clo_which("CNTY_BNDS");
           sprintf(srchstr,"<FIPS>%d<\0",
              clo.loc[sindex].stn.station[i].nm);
              clo_off(srchstr, bindex, col1, col2, ltype, lwidth);
           }
	break;
   case 1:
	/*
	** Use zone name to search for boundary
	*/
        /*sprintf(srchstr,"ZONE=%s\0",zone);*/
        sprintf(srchstr,"<ZONE>%s\0",zone);
        bindex = clo_which("ZONE_BNDS");
        clo_off(srchstr, bindex, col1, col2, ltype, lwidth);
	break;
   default:
	printf("this type %d not handled\n",type);
   }


}



void plot_slsc ( int *cval, int *nfips)
{
int bindex, ier;
int col1, col2, ltype, lwidth, ifltyp;
float szfil=0.5;
char srchstr[30];
extern gpwarn_config_type gpwarn_config[];

clo_init(&ier);

bindex = clo_which("CNTY_BNDS");
sprintf(srchstr,"<FIPS>%d<\0",*nfips);

col1 = gpwarn_config[*cval].current_fill_color;
col2 = gpwarn_config[*cval].current_line_color;
ltype = gpwarn_config[*cval].current_line_type;
lwidth = gpwarn_config[*cval].current_line_width;
ifltyp = gpwarn_config[*cval].current_fill_type;
gsfill(&szfil,&ifltyp,&ier);
clo_off(srchstr, bindex, col1, col2, ltype, lwidth);
}
