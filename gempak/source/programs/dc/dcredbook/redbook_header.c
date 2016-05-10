#include <stdio.h>
#include <string.h>

#include <dccmn.h>


int read_char1(x, value)
    unsigned char *x;
    char         *value;
{
    *value = x[0];
    return 1;
}

int read_short1(x,value)
    unsigned char *x;
    short         *value;
{

       *value = (x[0] << 8) | x[1];
       return 1;
}


void get_block(bpos,FF,blen,mode,submode)
char *bpos;
char *FF;
int *blen,*mode,*submode;{
	int iret;
	char chvalue;
	short sval;

	*blen = 0;
	
	iret = read_char1(bpos,&chvalue);
	
	*FF = (chvalue >> 6) ;
	if(*FF == 3)
	   {
	   /*printf("FF flag 3 mode %d\n",chvalue & 0x3F );*/
	   }
	iret = read_short1(bpos,&sval); bpos+=2;
	*blen = sval & 0x3FFF;
	iret = read_char1(bpos,&chvalue); bpos++;
	*mode = chvalue;
	iret = read_char1(bpos,&chvalue); bpos++;
	*submode = chvalue;
	/*printf("FF %d len %d mode %d sub %d\n",*FF,*blen,*mode,*submode);*/
}

char cstr1[10],cstr2[7],dstr[14],cpilid[10];

void block_1_1(buf,len)
char *buf;
int len;
{
int i,ival,iret,year,month,day,hour,minute;
char chvalue;
short sval;
  
 
/*for(i=0;i<len*2;i++) 
   {
   ival = *(buf+i);
   printf("look %d %d",i,ival);
   if((ival > 31)&&(ival < 127)) printf(" %c",ival);
   printf("\n");
   }
*/

memcpy(cstr1,buf+11,9);
cstr1[9] = '\0';

if(len*2 > 31)
   {
   memcpy(cstr2,buf+26,6);
   cstr2[6] = '\0';
   }
   
i = 20;
iret = read_short1(buf+i,&sval); i+=2;
year = sval;
iret = read_char1(buf+i,&chvalue); i++;
month = chvalue;
iret = read_char1(buf+i,&chvalue); i++;
day = chvalue;
iret = read_char1(buf+i,&chvalue); i++;
hour = chvalue;
iret = read_char1(buf+i,&chvalue); i++;
minute = chvalue;
sprintf(dstr,"%04d%02d%02d %02d%02d\0",
        year,month,day,hour,minute);
}
void block_2_2(buf,len)
char *buf;
int len;
{
int i,ival;
int ier;
char errstr[80];

if(len*2 < 21) 
   {
   sprintf(errstr,"mode 2 submode 2 short %d\0",len);
   dc_wclg ( 0, "DCREDBOOK", -1, errstr, &ier );
   return;
   }
memcpy(cpilid,buf+11,9);
cpilid[9] = '\0';
for(i=0;i<9;i++)
   if(cpilid[i] < 32) cpilid[i] = '\0';

/*
for(i=0;i<len*2;i++) 
   {
   ival = *(buf+i);
   printf("look ival %d",ival);
   if((ival > 31)&&(ival < 127)) printf(" %c",ival);
   printf("\n");
   }
*/
}


/* base time & originating model */
void block_1_6(buf,len)
char *buf;
int len;
{
int i,ival;

/*for(i=0;i<len*2;i++)
   {
   ival = *(buf+i);
   printf("look ival %d",ival);
   if((ival > 31)&&(ival < 127)) printf(" %c",ival);
   printf("\n");
   }
*/
}

/* map proj info*/
void block_4_17(buf,len,lat,lon,proj,pnam)
char *buf;
int len;
float *lat,*lon,*proj;
char *pnam;
{
int i,ival,iret,nval;
short sval;
float ullon,ullat,lllon,lllat,urlon,urlat,lrlon,lrlat;
float lat1,lat2,lon1;

/*for(i=0;i<len*2;i++)
   {
   ival = *(buf+i);
   printf("look %d %d",i,ival);
   if((ival > 31)&&(ival < 127)) printf(" %c",ival);
   printf("\n");
   }*/

i = 5;
nval = *(buf+i); i++;
/*printf("icorp %d %d\n",nval,len);*/
if(nval >= 1)
   {
   iret = read_short1(buf+i,&sval); i+=2;
   ullat = sval * .01;
   iret = read_short1(buf+i,&sval); i+=2;
   ullon = sval * -.01;
   }
if(nval >= 2)
   {
   iret = read_short1(buf+i,&sval); i+=2;
   urlat = sval * .01;
   iret = read_short1(buf+i,&sval); i+=2;
   urlon = sval * -.01;
   }
if(nval >= 3)
   {
   iret = read_short1(buf+i,&sval); i+=2;
   lrlat = sval * .01;
   iret = read_short1(buf+i,&sval); i+=2;
   lrlon = sval * -.01;
   }
if(nval >= 4)
   {
   iret = read_short1(buf+i,&sval); i+=2;
   lllat = sval * .01;
   iret = read_short1(buf+i,&sval); i+=2;
   lllon = sval * -.01;
   }

iret = read_short1(buf+i,&sval); i+=2;
lon1 = sval * -.01;

iret = read_short1(buf+i,&sval); i+=2;
lat1 = sval * .01;
/* why is this here?! */
/*if(lat1 >= 0) 
   lat1 = 90;
else
   lat1 = -90;*/

iret = read_short1(buf+i,&sval); i+=2;
if((sval == 9900) || (sval == 9999))
   lat2 = 0;
else
   lat2 = sval * .01;

lat[0] = lllat; lon[0] = lllon;
lat[1] = urlat; lon[1] = urlon;
lat[2] = lrlat; lon[2] = lrlon;
lat[3] = ullat; lon[3] = ullon;

proj[0] = lat1;
proj[1] = lon1;
proj[2] = lat2;
nval = i;
for(i=0;i<6;i++)
   {
   ival = *(buf+nval+i);
   pnam[i] = ival;
   }
pnam[6] = '\0';
}

/* assume tstr is adequately large (41 characters) */
void redbook_header ( char *buf, char *tstr, float *lat, float *lon, float *proj,
			char *pnam, int *ier)
{
int ival;
int i,start;
char FF,chvalue;
int blen,mode,submode,iret;
int year,month,day,hour,minute;
short sval;
int DONE=0,bstart=0;
char errstr[80];

*ier = 0;

tstr[0] = '\0';
cstr1[0] = '\0';
cstr2[0] = '\0';
dstr[0] = '\0';
cpilid[0] = '\0';

if(buf[0] == 1)
   {
   i=11;
   while(*(buf+i) != '\n') i++;
   start = i + 1;
   }
else
   start=45;

while(DONE == 0)
   {
   get_block(buf+start,&FF,&blen,&mode,&submode);
   /*printf("look start %d FF %d %d %d %d\n",start,FF,blen,mode,submode);*/

   if(FF != 1) DONE = 1;
   if((mode == 1)&&(submode == 1)) 
      {
      block_1_1(buf+start,blen);
      bstart++;
      }
   if((mode == 1)&&(submode == 6)) block_1_6(buf+start,blen);
   if((mode == 2)&&(submode == 2)) 
      {
      block_2_2(buf+start,blen);
      /*DONE = 1;*/
      }

   if((mode == 4)&&(submode == 17)) block_4_17(buf+start,blen,lat,lon,proj,pnam);

   if((mode == 1)&&(submode == 2)) DONE = 1; 

   /* don't look forever...since the buf isn't that big!!! */
   if((blen <= 0)||(blen > 128)||(start > 512))
      {
      DONE = 1;
      if(bstart == 0)
         {
	 sprintf(errstr,"error in redbook header %d %d\0",blen,start);
	 dc_wclg ( 0, "DCREDBOOK", -2, errstr, &iret );
	 *ier = -2;
	 return;
         }
      }
   
  
   start = start + blen*2; 
   }

if(bstart > 0) sprintf(tstr,"%s/%s/%s/%s\0",cpilid,cstr2,cstr1,dstr);

}
