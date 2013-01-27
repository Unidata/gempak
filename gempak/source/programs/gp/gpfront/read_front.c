/*********************************************************
** Collection of subroutines to read a frontal file,	**
** parse the symbols to be plotted and plot them on a 	**
** map. 						**
**							**
** S. Chiswell Unidata	 3/98				**
*********************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <geminc.h>
#include <gemprm.h>

#define MAXP	100
#define SOH	1
#define ETX	3

#ifdef UNDERSCORE
#define read_front	read_front_
#endif

typedef struct features {
   int npoints;
   float lat[MAXP],lon[MAXP];
   float press[MAXP];
   struct features *next;
   } features;

typedef struct wxbull {
   char *valid;
   struct features *low, *high, *warm, *cold, *occluded, *trof, *stationary;
   } wxbull;

float pipscale=1.0;


/* read a bulletin from a file */
char *getbull(fname,offset)
char *fname;
int *offset;
{
struct stat buf;
int fp, ier,FOUND;
char ch;
ssize_t nbyte;
int start,stop,i;
char *rbuf=NULL;

cfl_dopn(fname, &fp, &ier);
if(ier != 0) return(rbuf);

if((ier = fstat(fp,&buf)) != 0)
   {
   perror("Could not stat fronts file\0");
   close(fp);
   return(rbuf);
   }

if(*offset >= buf.st_size)
   {
   printf("offset past end of file\n");
   close(fp);
   return(rbuf);
   }

FOUND = 0;
while((FOUND == 0)&&(*offset < buf.st_size))
   {
   lseek(fp, *offset, SEEK_SET);
   nbyte = read(fp, &ch, 1);
   if(ch == SOH)
      {
      start = *offset;
      FOUND = 1;
      }
   *offset += 1;
   }

while((FOUND == 1)&&(*offset < buf.st_size))
   {
   lseek(fp, *offset, SEEK_SET);
   nbyte = read(fp, &ch, 1);
   if(ch == ETX)
      {
      stop = *offset;
      FOUND = 2;
      }
   *offset += 1;
   }

if(FOUND == 2)
   {
   rbuf = (char *)malloc(stop - start);
   lseek(fp, start+1, SEEK_SET);
   nbyte = read(fp, rbuf ,(stop - start - 2));
   for(i=0;i<(stop - start -3);i++)
      if(rbuf[i] < ' ') rbuf[i] = ' '; /* get rid of ctrl and bull chars! */
   rbuf[stop - start - 2] = '\0';
   *offset = stop + 1;
   }

close(fp);
return(rbuf);
}


/* get frontal location coordinates from a string */
void *getfront(pos)
char **pos;
{
struct features *feat_obj;
char *cpos,*subs,*apos;
double dval;

cpos = *pos;

while((cpos[0] != '\0')&&((cpos[0] < '0')||(cpos[0] > '9'))) cpos += 1;

*pos = cpos;
if(cpos[0] == '\0') return(NULL);

while((cpos[0] != '\0')&&(cpos[0] < 'A')) cpos += 1;

subs = (char *)malloc(cpos - *pos);
subs[0] = '\0';
strncat(subs,*pos,cpos - *pos - 1);

*pos = cpos;

feat_obj = (struct features *)malloc(sizeof(struct features));
feat_obj->npoints = 0;
feat_obj->next = NULL;
cpos = subs;
while(cpos != NULL)
   {
   apos = cpos;
   dval = strtod(apos,&cpos);
   if(cpos != apos) 
      {
      if(dval < 10000)
         {
         feat_obj->lat[feat_obj->npoints] = (float)(((int)dval)/100);
         feat_obj->lon[feat_obj->npoints] = (float)(((int)dval)%100) * (-1.0);
         }
      else
         {
         feat_obj->lat[feat_obj->npoints] = (float)(((int)dval)/1000);
         feat_obj->lon[feat_obj->npoints] = (float)(((int)dval)%1000) * (-1.0);
         }
      feat_obj->npoints += 1;
      apos = cpos;
      }
   else
      cpos = NULL;
   }

free(subs);
return(feat_obj);
}


/* get map features from a list of pressure & latlon points */
void *getfeat(pos)
char *pos;
{
char *cpos;
char *subs;
struct features *feat_obj;
int i;
double dval;

cpos = pos + 5;
while((cpos != NULL)&&(cpos[0] < 'A')) cpos += 1;
subs = (char *)malloc(cpos - pos - 5 + 1);
subs[0] = '\0';

strncat(subs,pos+5,cpos - pos - 5);
feat_obj = (struct features *)malloc(sizeof(struct features));
feat_obj->npoints = 0;
feat_obj->next = NULL;
cpos = subs; i = 0;
while(cpos != NULL)
   {
   pos = cpos;
   dval = strtod(pos,&cpos);
   if(cpos != pos) 
      {
      if(i == 0) 
         {
         feat_obj->press[feat_obj->npoints] = (float)dval;
         }
      else
         {
         if(dval < 10000)
            {
            feat_obj->lat[feat_obj->npoints] = (float)(((int)dval)/100);
            feat_obj->lon[feat_obj->npoints] = (float)(((int)dval)%100) * (-1.0);
            }
         else
            {
            feat_obj->lat[feat_obj->npoints] = (float)(((int)dval)/1000);
            feat_obj->lon[feat_obj->npoints] = (float)(((int)dval)%1000) * (-1.0);
            }
         feat_obj->npoints += 1;
         }
      i = (i + 1) % 2; 
      pos = cpos;
      }
   else
      cpos = NULL;
   }

free(subs);
return(feat_obj);
}


/* parse out highs, lows, and fronts from a bulletin */
void *read_asus1(prod,fhr)
char *prod;
int fhr;
{
struct wxbull *bull=NULL;
struct features *feat_obj;
char *pos,*cpos,*valid,*bullp;
char searchstr[80];
int fcst=0;

if((bull = (void *)malloc(sizeof(struct wxbull))) == NULL)
   return(NULL);

bull->low = NULL; bull->high = NULL;
bull->warm = NULL; bull->cold = NULL;
bull->occluded = NULL; bull->trof = NULL;
bull->stationary = NULL; bull->valid = NULL;

/* see if we are dealing with a forecast front file */
pos = strstr(prod,"PROG VALID");
if(pos != NULL)
   {
   fcst = 1;
   searchstr[0] = '\0';
   sprintf(searchstr,"%02dHR PROG VALID\0",fhr);
   pos = strstr(prod,searchstr);
   if(pos == NULL)
      {
      printf("forecast hour %d not found\n",fhr);
      return(NULL);
      }
   else
      {
      bullp = pos;
      pos = strstr(pos+15,searchstr+2);
      if(pos != NULL) bullp[(pos - bullp)-3] = '\0';
      }
   }
else
   bullp = prod;

pos = strstr(bullp,"VALID");
if(pos != NULL)
   {
   valid = (char *)malloc(30);
   valid[0] = '\0';
   if(fcst != 1)
      {
      strncat(valid,"FRONTS ",7);
      strncat(valid,pos,18); 
      /* dumb check for 3 spaces (\r\r\n since NWS keeps screwing up time*/
      cpos = (char *)strstr(valid,"   ");
      if(cpos != NULL) cpos[0] = '\0';
      }
   else
      {
      strncat(valid,"PROG FRONTS ",12);
      strncat(valid,pos,13);
      }
   bull->valid = valid;
   }

pos = strstr(bullp,"HIGHS");
if(pos != NULL) bull->high = getfeat(pos);

pos = strstr(bullp,"LOWS");
if(pos != NULL) bull->low = getfeat(pos);

pos = strstr(bullp,"COLD");
while(pos != NULL) 
   {
   feat_obj = getfront(&pos);
   if(feat_obj != NULL)
      {
      feat_obj->next = bull->cold;
      bull->cold = feat_obj;
      }
   pos = strstr(pos,"COLD");
   }

pos = strstr(bullp,"WARM");
while(pos != NULL) 
   {
   feat_obj = getfront(&pos);
   if(feat_obj != NULL)
      {
      feat_obj->next = bull->warm;
      bull->warm = feat_obj;
      }
   pos = strstr(pos,"WARM");
   }

pos = strstr(bullp,"STNRY");
while(pos != NULL) 
   {
   feat_obj = getfront(&pos);
   if(feat_obj != NULL)
      {
      feat_obj->next = bull->stationary;
      bull->stationary = feat_obj;
      }
   pos = strstr(pos,"STNRY");
   }

pos = strstr(bullp,"TROF");
while(pos != NULL) 
   {
   feat_obj = getfront(&pos);
   if(feat_obj != NULL)
      {
      feat_obj->next = bull->trof;
      bull->trof = feat_obj;
      }
   pos = strstr(pos,"TROF");
   }

pos = strstr(bullp,"OCFNT");
while(pos != NULL) 
   {
   feat_obj = getfront(&pos);
   if(feat_obj != NULL)
      {
      feat_obj->next = bull->occluded;
      bull->occluded = feat_obj;
      }
   pos = strstr(pos,"OCFNT");
   }

return(bull);
}



/* calculate a spline through the frontal points and
   plot the frontal symbols */
void draw_front(fobj,fnum)
struct features *fobj;
int fnum;
{
int iret;
int ltype, lhw, lwidth, whw, color, clr2;
int ifcod, ipipst, ipipdr;
float pipsz;
int ismtyp=2, ismtyp_old;
float dens=5.0, dens_old;
static char navm[]="M";

gqsmth ( &ismtyp_old, &dens_old, &iret);
gssmth ( &ismtyp, &dens, &iret);

clr2 = -1;
switch(fnum)
   {
   case 1:
	color = 4;
	ltype = 1;
        ifcod = 400;
        break;
   case 2:
	color = 2;
	ltype = 1;
        ifcod = 200;
        break;
   case 3:
	color = 2;
        clr2 = 4;
	ltype = 1;
        ifcod = 006;
        break;
   case 4:
	color = 5;
	ltype = 25;
        ifcod = -1;
        break;
   case 5:
	color = 7;
	ltype = 1;
        ifcod = 600;
        break;
   default:
        color = 6;
        ltype = 1;
   }
lhw = 0; lwidth = 2; whw = 0;
ipipst = 0;
ipipdr = 1;

if ( clr2 > 0 )
   gsclr2(&color,&clr2,&iret);
else
   gscolr(&color,&iret);

if ( ifcod < 1 )
   {
   gsline(&ltype,&lhw,&lwidth,&whw,&iret);
   gline(navm,&(fobj->npoints),fobj->lat,fobj->lon,&iret,strlen(navm));
   }
else
   {
   pipsz = pipscale;
   gsfrnt(&ifcod, &pipsz, &ipipst, &ipipdr, &iret);
   gfrnt(navm,&(fobj->npoints),fobj->lat,fobj->lon,&iret,strlen(navm));
   }

/* reset line type attributes */
gssmth ( &ismtyp_old, &dens_old, &iret);

}

/* subroutine to read a file and plot frontal information */
void read_front(char *wwfil, int *wwlen, char *valtim, int *vallen, char *atts, 
                 int *attlen, char *ashr, int *ashrlen, int *ier)
{

int offset=0, i;
struct wxbull *wxsyms;
struct features *feat_obj;
char *bullp;
char *fname, tmpstr[LLMXLN];

char navigate[2],pstr[10],*pos;
int xioff,yioff,iret;
int color,np,width,fhr,labels=1;
float midx,midy,rotate,spcode;
float size,scale;

fname = (char *)malloc(*wwlen + 1);
st_null (wwfil, fname, &i, &iret, *wwlen, *wwlen+1);

st_null (atts, atts, &i, &iret, *attlen, *attlen); 
if(i == 0) atts[0] = '\0';

/* Get scale for WX symbols */
pos = cst_split(atts,'/',LLMXLN, tmpstr, &iret);
scale = atof(tmpstr);
if(scale < 0) scale = 0;

/* Get Label flag for highs and lows */
if(pos != NULL)
   {
   pos = cst_split(pos,'/',LLMXLN, tmpstr, &iret);
   if ((tmpstr[0] == 'N')||(tmpstr[0] == 'n')) labels=0;
   }

/* Get pipscale, otherwise, same as WXsymbol scale */
if ( pos != NULL )
   {
   pos = cst_split(pos,'/',LLMXLN, tmpstr, &iret);
   pipscale = atof(tmpstr);
   if(pipscale < 0) pipscale = 0;
   }
else
   pipscale = scale;


st_null (ashr, ashr, &i, &iret, *ashrlen, *ashrlen);
if(i == 0) ashr[0] = '\0';
fhr = atol(ashr);
if(fhr < 0) fhr = 12;


/* can be generalized for multiple bulletins in a file later
   if I decide that we need to pass a plot time! */
/*while(offset >=0)  
   {*/
bullp = getbull(fname,&offset);
   /*if(bullp == NULL)
      {
      offset = -1;
      continue;
      }*/
free(fname);

wxsyms = (void *)read_asus1(bullp,fhr);
free(bullp);

if(wxsyms != NULL)
   {
   navigate[0] = 'M';
   navigate[1] = '\0';
   feat_obj = wxsyms->cold;
   while(feat_obj != NULL)
      {
      draw_front(feat_obj,1);
      feat_obj = feat_obj->next;
      }
   
   feat_obj = wxsyms->warm;
   while(feat_obj != NULL)
      {
      draw_front(feat_obj,2);
      feat_obj = feat_obj->next;
      }
   
   feat_obj = wxsyms->stationary;
   while(feat_obj != NULL)
      {
      draw_front(feat_obj,3);
      feat_obj = feat_obj->next;
      }
   
   feat_obj = wxsyms->trof;
   while(feat_obj != NULL)
      {
      draw_front(feat_obj,4);
      feat_obj = feat_obj->next;
      }
   
   feat_obj = wxsyms->occluded;
   while(feat_obj != NULL)
      {
      draw_front(feat_obj,5);
      feat_obj = feat_obj->next;
      }

   if(wxsyms->high != NULL)
      {
      color = 4;
      gscolr(&color,&iret);
      size = 1.5 * scale; width = 2;
      gsspcl(&size,&width,&iret);
      np = 1; spcode = 12;
      for(i=0;i<wxsyms->high->npoints;i++)
         {
         xioff = 0; yioff = 0; rotate = 0;
         midx = wxsyms->high->lat[i];
         midy = wxsyms->high->lon[i];
         gspcl(navigate,&np,&spcode,&midx,&midy,&xioff,&yioff,&iret,strlen(navigate));
         if(labels != 0)
            {
            yioff = (int)(-3*scale); xioff = -3;
            if(wxsyms->high->press[i] < 1000) xioff = -2;
            if(yioff > -2) yioff = -2;
            pstr[0] = '\0'; 
            sprintf(pstr,"%3.0f\0",wxsyms->high->press[i]);
            gtext(navigate,&midx,&midy,pstr,&rotate,&xioff,&yioff,&iret,strlen(navigate),strlen(pstr));
            }
         }
      }
   if(wxsyms->low != NULL)
      {
      color = 2;
      gscolr(&color,&iret);
      size = 1.5 * scale; width = 2;
      gsspcl(&size,&width,&iret);
      np = 1; spcode = 13;
      for(i=0;i<wxsyms->low->npoints;i++)
         {
         xioff = 0; yioff = 0; rotate = 0;
         midx = wxsyms->low->lat[i];
         midy = wxsyms->low->lon[i];
         gspcl(navigate,&np,&spcode,&midx,&midy,&xioff,&yioff,&iret,strlen(navigate));
         if(labels != 0)
            {
            yioff = (int)(-3*scale); xioff = -3;
            if(wxsyms->low->press[i] < 1000) xioff = -2;
            if(yioff > -2) yioff = -2;
            pstr[0] = '\0';
            sprintf(pstr,"%3.0f\0",wxsyms->low->press[i]);
            gtext(navigate,&midx,&midy,pstr,&rotate,&xioff,&yioff,&iret,strlen(navigate),strlen(pstr));
            }
         }
      }
 
   while(wxsyms->cold != NULL)
      {
      feat_obj = wxsyms->cold->next;
      free(wxsyms->cold);
      wxsyms->cold = feat_obj;
      }
   while(wxsyms->warm != NULL)
      {
      feat_obj = wxsyms->warm->next;
      free(wxsyms->warm);
      wxsyms->warm = feat_obj;
      }
   while(wxsyms->trof != NULL)
      {
      feat_obj = wxsyms->trof->next;
      free(wxsyms->trof);
      wxsyms->trof = feat_obj;
      }
   while(wxsyms->occluded != NULL)
      {
      feat_obj = wxsyms->occluded->next;
      free(wxsyms->occluded);
      wxsyms->occluded = feat_obj;
      }
   while(wxsyms->stationary != NULL)
      {
      feat_obj = wxsyms->stationary->next;
      free(wxsyms->stationary);
      wxsyms->stationary = feat_obj;
      }
   if(wxsyms->high != NULL) free(wxsyms->high); 
   if(wxsyms->low != NULL) free(wxsyms->low); 
   if(wxsyms->valid != NULL) 
      {
      strncpy(valtim,wxsyms->valid,*vallen);
      free(wxsyms->valid);
      }
   free(wxsyms); 
   }
   

   /*}*/
/*gflush(&iret);*/
*ier = 0;
return;


}

/* debug stubs */
#ifdef MAIN
int main(int argc, char *argv[])
{
int ier,wwlen,vallen,attlen,ashrlen;
char valtim[72],atts[72],ashr[72];

wwlen = strlen(argv[1]); vallen=71;
strcpy(atts,"     \0"); attlen = strlen(atts);
strcpy(ashr,"     \0"); ashrlen = strlen(ashr);
read_front(argv[1],&wwlen,valtim,&vallen,atts,&attlen,ashr,&ashrlen,&ier);

}
#endif

