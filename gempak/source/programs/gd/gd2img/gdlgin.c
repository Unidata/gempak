#include "geminc.h"
#include "gemprm.h"

#define DEFBAND	25


/*
 * These pointers are defined in the IM library
 */
extern unsigned char *imdata;
extern unsigned char *imndat;

int ncal;

typedef struct calvals {
	float minpx, maxpx, minval, maxval;
	} calvals;

struct calvals *calptr=NULL;

void	freeimg ()
{
free(imdata);
free(imndat);
}

void	gdlgin (float *grid, int *kx, int *ky, 
                int *compress, int *prodlen, int *iret)
{
int ival,i,j,k,ier,lendat,ioff,imax=IMISSD,imin=IMISSD;
float scale, offset, pxoffset;
int num;
static int nbytes=1;
unsigned char *tempdat=NULL;

*iret = 0;

lendat = *kx * *ky;

imndat = (unsigned char *) malloc (lendat * sizeof(unsigned char));
imdata = (unsigned char *) malloc (sizeof(unsigned char));


if((imdata == NULL)||(imndat == NULL))
   {
   if(imdata != NULL) free(imdata);
   if(imndat != NULL) free(imndat);
   printf("failed to allocate data block storage\n");
   *iret = -1;
   return;
   }

if(*compress == 128)
   {
   tempdat = (unsigned char *) malloc (*kx * sizeof(unsigned char));
   if(tempdat == NULL)
      {
      printf("failed to allocate compressed line storage\n");
      free(imdata);
      free(imndat);
      *iret = -1;
      return;
      }
   printf("Starting compression\n");
   png_set_memheap(imndat);
   pngout_init(*kx,*ky);
   }


for(j=*ky-1;j>=0;j--)
   {
   for (i=0;i<*kx;i++)
      {
      k = (j * *kx) + i;
      if(grid[k] == RMISSD)
         ival = 0;
      else
         {
	 scale = 1; offset = 0; pxoffset = 0;
	 if(ncal != 0)
            {
	    num = 0;
	    while((num < ncal) && (grid[k] > calptr[num].maxval)) num++;
	    if(num < ncal)
	       {
	       pxoffset = calptr[num].minpx;
	       offset = calptr[num].minval;
	       scale = (calptr[num].maxpx - calptr[num].minpx)/(calptr[num].maxval - calptr[num].minval);
	       }
	    else
	       printf("error in cal block representing %f\n",grid[k]);
            }
         ival = (int)(((grid[k] - offset)*scale) + pxoffset);
         }

      if((imax == IMISSD)||(ival > imax)) imax = ival;
      if((imin == IMISSD)||(ival < imin)) imin = ival;

      if(*compress == 128)
         {
	 ioff = i;
         mv_itob(&ival,&ioff,&nbytes,tempdat,&ier);
         }
      else
         {
         ioff = ((*ky - 1 - j) * *kx ) + i;
         mv_itob(&ival,&ioff,&nbytes,imndat,&ier);
         }
      }
   if(*compress == 128)
      pngwrite(tempdat);
   }

if(*compress == 128)
   {
   pngout_end();
   free(tempdat);
   *prodlen = png_get_prodlen();
   }
else
   *prodlen = lendat;

/*printf("Product min val %d, max val %d\n",imin,imax);*/
}



void	gdhgin (int *ignhdr, char *lprod, int *lenp, char *wmohdr,
		int *tarr, int *compress, char *calinfo, int *iret)
{
int i,j,ival,icnt,ier,boff;
int FOUND=0;
size_t slen;
unsigned char *barr;
char prodid[80],line[512];
char header[80], head1[7], head2[5], DDHHNN[7]; 
int lenhd = 18;
char defstr[]=" ", *cpos, *spos;
int sat_id = 99, iband,numstr;
float fvals[4];
FILE *fp;
static int nstrings=6, ustrings=3, init=0;
static char **arrptr, **usrptr;
char *calblock;
static char CONFTABLE[]="nex2gini.tbl";
static char ttaaii[]="TTAA00";
static char cccc[]="CCCC";

char headstr[24]; /* Gini expects 20 or 24 bytes, eg PDB starts at byte 21 or 25 */
static char trailer[]={'\r','\r','\n','\0','\0','\0'};

if ( !init) {
   init = !0;

   arrptr = (char **) malloc(sizeof(char *) * nstrings);
   for(i=0; i < nstrings; i++)
      arrptr[i] = (char *) malloc(LLMXLN);

   usrptr = (char **) malloc(sizeof(char *) * ustrings);
   for(i=0; i < ustrings; i++)
      usrptr[i] = (char *) malloc(LLMXLN);
}

iband = DEFBAND;
ncal = 0;
calblock = NULL;
header[0] = '\0';

if(calptr != NULL) 
   {
   free(calptr);
   calptr = NULL;
   }

strncpy(prodid,lprod,*lenp);
prodid[*lenp] = '\0';

fp = cfl_tbop ( CONFTABLE, "unidata", &ier);
if(fp == NULL)
  {
  printf("warning: could not open nex2gini.tbl configuration table\n");
  }
else
   {
   while((FOUND == 0)&&(fgets(line,512,fp)!= NULL))
      {
      if(line[0] == '!') continue;
      slen = strcspn(line," \t"); 
      if(strncmp(prodid,line,slen) == 0)
         {
         FOUND = 1;
         if(line[strlen(line)-1] == '\n') line[strlen(line)-1] = '\0';
         cst_clst(line, ' ', defstr,nstrings,LLMXLN,arrptr, &numstr, &ier);
         sscanf(arrptr[1],"%d",&iband);
         sprintf(header,"%s %s %02d%02d%02d\0",
	    arrptr[4],arrptr[5], tarr[2],tarr[3], tarr[4] );
   	
         /*printf("look lengths %d %d\n",strlen(arrptr[4]),strlen(arrptr[5]));*/
         /*printf("look header %d %s \n",iband,header);*/
	 calblock = arrptr[3];
	 sscanf(arrptr[2],"%d",&ncal);
         }
      }
   fclose(fp);
   }


if ( ( wmohdr != NULL ) && ( strlen(wmohdr) > 0 ) )
   {
   cpos = cst_split(wmohdr, '/', sizeof(head1), head1, &ier);
   if ( strlen(head1) != 6 ) strcpy(head1,ttaaii);

   head2[0] = '\0'; spos = NULL;
   if ( cpos != NULL ) spos = cst_split(cpos, '/', sizeof(head2), head2, &ier);
   
   if ( strlen(head2) != 4 ) strcpy(head2,cccc);

   cpos = spos;
   if ( ( cpos != NULL ) && ( strlen(cpos) >= 6 ) )
      strncpy(DDHHNN, cpos, 6);
   else
      sprintf(DDHHNN,"%02d%02d%02d\0", tarr[2],tarr[3], tarr[4] );

   sprintf(header,"%s %s %s\0",head1,head2,DDHHNN );
   }
else if ( strlen(header) < 1 )
    sprintf(header,"%s %s %02d%02d%02d\0",ttaaii,cccc,tarr[2],tarr[3], tarr[4] );

if ( strlen(calinfo) > 0 ) {
   /* fill in calblock with supplied info */
   /*printf("assign calinfo %s\n",calinfo);*/
   cst_clst(calinfo, '/', defstr,ustrings,LLMXLN,usrptr, &numstr, &ier);
   if ( strcmp(usrptr[0],defstr) != 0 ) {
       if ( sscanf(usrptr[0],"%d",&i) ) sat_id = i;
   }
   if ( strcmp(usrptr[1],defstr) != 0 ) {
       if ( sscanf(usrptr[1],"%d",&i) ) iband = i;
   }
   if ( strcmp(usrptr[2],defstr) != 0 ) {
      ncal = 1;
      for(i=0;i<strlen(usrptr[2]);i++) if ( usrptr[2][i] == ';' ) ncal++;
      calblock = usrptr[2];
   }
   /*printf("ncal = %d\n",ncal);
   for ( i=0; i<numstr; i++)
      printf("look calstr %d %s\n",i,usrptr[i]);*/
}
else if(FOUND == 0) {
    printf("warning: no calibration found in %s configuration table\n",CONFTABLE); 
}

lenhd = strlen(header);
if(lenhd > sizeof(headstr)-6)
   {
   printf("warning header being truncated to %d characters\n",
      sizeof(headstr)-6);
   lenhd = sizeof(headstr);
   }

memset(headstr,' ',sizeof(headstr));
memcpy(headstr,header,lenhd);
memcpy(headstr+sizeof(headstr)-sizeof(trailer),trailer,sizeof(trailer));


/*
 * Use satellite ID=99 and Band as provided (else 25) 
 */
barr = (unsigned char *)ignhdr;
for(i=0;i<sizeof(headstr);i++)
   barr[i] = (unsigned char)headstr[i];

boff = 20; /* assume PDB will start at octet 21 */
barr[boff + 1] = 1;
barr[boff + 2] = sat_id;
barr[boff + 3] = 1;
barr[boff + 4] = iband;

barr[boff + 9] = tarr[0] % 100;
barr[boff + 10] = tarr[1];
barr[boff + 11] = tarr[2];
barr[boff + 12] = tarr[3];
barr[boff + 13] = tarr[4];
barr[boff + 14] = 0;
barr[boff + 15] = 0;

barr[boff + 38] = 0; /* scanning mode, only 0 known */

barr[boff + 42] = 1; /* image resolution */

barr[boff + 43] = *compress; /* compression */

/*
 * Set the size of header to 512 bytes
 */
ival = 512; i = boff + 45; j = 2;
mv_itob(&ival, &i, &j, barr, &ier);

/*
 * set the Calblock flag to 128
 */
barr[boff + 47] = 128;

/*
 * move the Cal block Units
 */
if(calblock != NULL)
   {
   cpos = strchr(calblock,',');
   if(cpos != NULL)
      {
      slen = cpos - calblock;
      for(i=0;i<8;i++)
         {
         if(i < slen)
	    barr[boff+48+i] = calblock[i];
	 else
	    barr[boff+48+i] = ' ';
         }
      barr[boff+56] = ncal;

      calptr = (struct calvals *)malloc(sizeof(struct calvals)*ncal);
      for(icnt=0;icnt<ncal;icnt++)
         {
         cst_rlst ( cpos+1, ',', IMISSD, 4, fvals, &i, &ier );
         ival = (int)(fvals[0]*10000.);
         i = boff + 57 + (16*icnt); j = 4;
         mv_itob(&ival, &i, &j, barr, &ier);
         ival = (int)(fvals[1]*10000.);
         i = boff + 61 + (16*icnt); j = 4;
         mv_itob(&ival, &i, &j, barr, &ier);
         ival = (int)(fvals[2]*10000.);
         i = boff + 65 + (16*icnt); j = 4;
         mv_itob(&ival, &i, &j, barr, &ier);
         ival = (int)(fvals[3]*10000.);
         i = boff + 69 + (16*icnt); j = 4;
         mv_itob(&ival, &i, &j, barr, &ier);
	 cpos = strchr(cpos+1,';');

	 calptr[icnt].minpx = fvals[0];
	 calptr[icnt].maxpx = fvals[1];
	 calptr[icnt].minval = fvals[2];
	 calptr[icnt].maxval = fvals[3];
	 }
      }
   }

*iret = 0;
}
