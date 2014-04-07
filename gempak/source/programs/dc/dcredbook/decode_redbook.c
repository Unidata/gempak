#include <geminc.h>
#include <gemprm.h>

#include <dccmn.h>
#include <mkdirs_open.h>


#ifdef UNDERSCORE
#define aw_read	aw_read_
#define gqmode	gqmode_
#define	ipredbook ipredbook_
#endif

#define MXREDTBL	3


extern int errno;

void redbook_header ( char *buf, char *tstr, float *lat, float *lon, float *proj,
                        char *pnam, int *ier);
void ipredbook (int *iret);
void aw_read (char *fname, char *dflag, int *iret, size_t, size_t);
void gqmode ( int *mode, int *iret);

void decode_redbook(char *device, char *rawfile, int *iret)
{
int i, ier;
int idrpfl;
int color, itype, iwidth,  arg1=0, arg2=0;
int in_file, out_file;
mode_t omode=S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH;
int START=0;
float lat[4],lon[4],fproj[3];
struct tm redtime;
time_t prodtime;

char *outtemp, *outdev, *tempdev, templstr[15];

char outfilenm[256],outtmpl[256],*pos,*cpos,*tempfil,buf[512],tstr[128],outrstr[512];
char DEV[10],*optparms=NULL;
char pnam[7],ptype[10];
char PIL[10],TITLE[256],prod1[180],prod2[180];
char sysline[512];
char **aryptr;
static char proj[50];
char map[180];
char garea[30];
char gemtime[20];
char errstr[181];
static char imgfls[] = " ";
static char subtok[]="%P";
static char dbug[] = " ";
static char DEFSTR[] = "default";
static char MAPVAR[] = "$MAPFIL";
FILE *fp;

*iret = 0;

aryptr = (char **) malloc(sizeof(char *) * MXREDTBL);
for(i=0;i<MXREDTBL;i++)
   aryptr[i] = (char *)malloc(180);

outfilenm[0] = '\0';
DEV[0] = '\0';

sprintf(map,"31/1/1 \0");


pos = (char *)cst_split(device,'|',9,DEV,&ier);
if(ier != 0) {
   sprintf(errstr, "error determining device driver name %s\0",DEV);
   dc_wclg(0, "DCREDBOOK", -1, errstr, &ier);
   *iret = -1;
   return;
}

if(pos != NULL) {
   optparms = (char *)cst_split(pos,'|',256,outfilenm,&ier);
   if(optparms != NULL) optparms--;
}


in_file = STDIN_FILENO;

if( P_tmpdir[strlen(P_tmpdir) - 1] == '/')
   sprintf (templstr, ".rdbk_XXXXXX\0");
else
   sprintf (templstr, "/.rdbk_XXXXXX\0");   

outtemp = (char *)malloc( strlen(P_tmpdir) + strlen(templstr) + 1);
   
if( outtemp != NULL )
   {
   sprintf(outtemp, "%s%s\0", P_tmpdir, templstr);
   out_file = mkstemp ( outtemp );
   }
else
   out_file = -1;


if(out_file < 0) {
   sprintf(errstr,"could not open temporary file\0");
   dc_wclg(0,"DCREDBOOK",-1,errstr,&ier);
   *iret = -1;
   if(outtemp != NULL) free(outtemp);
   return;
}

tstr[0] = '\0';
while((i = read(in_file,buf,512)) > 0)  {
   write(out_file,buf,i);
   if(START == 0) {
      START++;
      lat[0] = -9999; lon[0] = -9999; fproj[0] = -9999; pnam[0] = '\0';
      redbook_header(buf,tstr,lat,lon,fproj,pnam,&ier);
      if(ier != 0) {
	 close(out_file);
	 unlink(outtemp);
	 free(outtemp);
	 *iret = ier;
	 return;
      }
   }
}
close(out_file);

sprintf(errstr,"created temp input file %s\0",outtemp);
dc_wclg(2,"DCREDBOOK",5,errstr,&ier);

/*
** Set up graphics mode and initialize ip interface for mapfil
*/

ipredbook (&ier);

i=1;
gg_init(&i, &ier);

/* Add call to query mode to load this routine...since gemlib
** routine gp_azdr will try to access this routine later
*/
/*gqmode(&i,&ier);*/

if(lat[0] != -9999)
   {
   sprintf(garea,"%6.2f;%7.2f;%6.2f;%7.2f\0",lat[0],lon[0],lat[1],lon[1]);
   }
else
   sprintf(garea,"AFNA\0");

sprintf(ptype,"STR\0");
if(pnam[0] != '\0')
   {
   if(strcmp(pnam,"PNHE01") == 0)
      sprintf(ptype,"STR\0");
   else if(strcmp(pnam,"PNAM02") == 0)
      sprintf(ptype,"STR\0");
   else
      printf("unknown pnam %s\n",pnam);
   }

if(fproj[0] != -9999)
   sprintf(proj,"%s/%.2f;%.2f;%.2f/NM\0",ptype,fproj[0],fproj[1],fproj[2]);
else
   sprintf(proj,"STR/90;-105;0/NM\0");


PIL[0] = '\0'; TITLE[0] = '\0';
if(tstr[0] != '\0') {
   if((cpos = strstr(tstr,"NMCGPH")) != NULL) {
      strncat(PIL,cpos+6,3);
      fp = (FILE *)cfl_tbop("redbook.tbl","nafos",&ier);
      if(fp != NULL) {
         ier = 0;
         while(ier == 0) {
            cfl_trln(fp,255,buf,&ier);
            if(strncmp(buf,PIL,strlen(PIL)) == 0) {
               cpos = strchr(buf,' ');
               if(cpos == NULL)
                  strcat(TITLE,buf);
               else
                  {
                  strncat(TITLE,buf,cpos-buf);
                  while((cpos[0] != '\n') && isspace(cpos[0])) cpos++;
	          cst_clst(cpos,' ',DEFSTR, MXREDTBL, 80, aryptr, &i, &ier);
		  if ( ( strcmp(aryptr[0],DEFSTR) != 0 ) && 
		       ( strcmp(aryptr[0],"*") != 0 ) )
			   {
			   strcpy(garea,aryptr[0]);
			   sprintf(proj,"DEF\0");
			   }
		  if ( ( strcmp(aryptr[1],DEFSTR) != 0 ) && 
		       ( strcmp(aryptr[1],"*") != 0 ) )
		      {
                      ip_putv ( MAPVAR, aryptr[1], &ier, 
				strlen(MAPVAR), strlen(aryptr[1]));
		      }
		  if ( ( strcmp(aryptr[2],DEFSTR) != 0 ) && 
		       ( strcmp(aryptr[2],"*") != 0 ) )
		      strcpy(map,aryptr[2]);
                  }
	       dc_wclg(1,"DCREDBOOK",1,TITLE,&ier);
               ier = -10;
               }
            }
         cfl_clos(fp,&ier);
	 if(TITLE[0] == '\0')
	    {
            sprintf(errstr,"PIL %s not in redbook.tbl\0",PIL);
            dc_wclg(0,"DCREDBOOK",2,errstr,&ier);
	    sprintf(TITLE,"%s\0",PIL);
	    }
         }
      } 
   else if(tstr[0] == '/') /* use 2nd & third field */
      {
      cst_nocc(tstr+1,'/',2,0,&i,&ier);
      prod1[0] = '\0';
      strncat(prod1,tstr+1,i);
      sprintf(TITLE,"UNKNOWN_%s\0",prod1);
      fp = (FILE *)cfl_tbop("afosgraph.tbl","nafos",&ier);
      if(fp != NULL) {
         ier = 0;
         while(ier == 0) {
            cfl_trln(fp,255,buf,&ier);
            if(strncmp(buf,prod1,strlen(prod1)) == 0) {
               TITLE[0] = '\0';
               cpos = strchr(buf,' ');
               cst_nocc(prod1,'/',1,0,&i,&ier);
	       strncat(TITLE,prod1,i);
               cst_nocc(buf,'-',1,0,&i,&ier);
               if(cpos == NULL) {
                  strcat(TITLE,buf+i);
               } else {
                  strncat(TITLE,buf+i,cpos-buf-i);
                  while((cpos[0] != '\n') && isspace(cpos[0])) cpos++;
                  cst_clst(cpos,' ',garea,1,80,aryptr,&i,&ier);
                  if((i > 0)&&(strcmp(aryptr[0],"*") != 0)) sprintf(garea,"%s\0",aryptr[0]);
               }
               ier = -10;
            }
         }
         cfl_clos(fp,&ier);
      }
      dc_wclg(1,"DCREDBOOK",2,TITLE,&ier);
   } else {
      sprintf(TITLE,"UNKNOWN\0");
      dc_wclg(1,"DCREDBOOK",3,TITLE,&ier);
   }
 
   cst_nocc(tstr,'/',3,0,&i,&ier);
   gemtime[0] = '\0';
   strncat(gemtime,tstr+i+3,6);
   strncat(gemtime,"/",1);
   strncat(gemtime,tstr+i+10,4);
   cfl_mnam(gemtime,outfilenm,outtmpl,&ier);
   strcpy(outfilenm,outtmpl);
}


ier = 0;
while((strstr(outfilenm,subtok) != 0)&&(ier == 0))
   cst_rpst(outfilenm,subtok,TITLE,outfilenm,&ier);

if(strlen(outfilenm) > 0) {
   pos = strrchr(outfilenm,'/');
   if(pos != NULL) {
      char stripped[PATH_MAX+1];
      i = pos - outfilenm;
      memcpy(stripped, outfilenm, i);
      stripped[i] = 0;
      ier = mkdirs(stripped,(omode | 0111));
   }

   tempdev = (char *)malloc( strlen(P_tmpdir) + strlen(templstr) + 1);
   sprintf(tempdev, "%s%s\0",P_tmpdir,templstr);
   mktemp ( tempdev );

   sprintf(outdev,"%s|%s\0",DEV,tempdev);
   if(optparms != NULL) strcat(outdev,optparms);
   gg_sdev(outdev, &ier, strlen(outdev));
} else {
   gg_sdev(device, &ier, strlen(device));
   tempdev = NULL;
}



gg_maps ( proj, garea, imgfls, &idrpfl, &ier, strlen(proj),strlen(garea),strlen(imgfls));

gstanm(&ier);

gg_map(map, &ier, strlen(map));

/*
** TODO: move this to tables
*/
if((strncmp(device,"ps",2) == 0)||
   (strncmp(device,"PS",2) == 0))
   color = 4;
else
   color = 5;

gscolr(&color, &ier);
itype = 1;
iwidth = 1;
gsline(&itype, &arg1, &iwidth, &arg2, &ier );
aw_read(outtemp, dbug, &ier, strlen(outtemp)+1, strlen(dbug));

geplot(&ier);
genanm(&ier);

i=0;
gendp(&i, &ier);

if(strlen(outfilenm) != 0)
   {
   sprintf(sysline,"mv %s %s\0",tempdev,outfilenm);
   if( (ier = system(sysline)) != 0)
      {
      sprintf(errstr,"could not move %s to %s\0",tempdev,outfilenm);
      dc_wclg(0,"DCREDBOOK",-1,errstr,&ier);
      }
   }

if (tempdev != NULL) free(tempdev);


if ( (rawfile[0] == '\0') || ( strcmp(rawfile," ") == 0 ) )
   unlink(outtemp);
else
   {
   chmod ( outtemp, omode);
   ier = 0;
   while((strstr(rawfile,subtok) != 0)&&(ier == 0))
      cst_rpst(rawfile,subtok,TITLE,rawfile,&ier);

   cfl_mnam(gemtime,rawfile,outtmpl,&ier);
   strcpy(rawfile,outtmpl);

   sprintf(sysline,"mv %s %s\0",outtemp,rawfile);
   if( (ier = system(sysline)) != 0)
      {
      sprintf(errstr,"could not move %s to %s\0",outtemp,rawfile);
      dc_wclg(0,"DCREDBOOK",-1,errstr,&ier);
      }

   }

if(outtemp != NULL) free(outtemp);

nbull++;
}
