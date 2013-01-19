/**************************************************************** 
 * RCM.C							* 
 * Chiz/Unidata	10/99						* 
 *								* 
 * Routines for obtaining RCM bulletin and projecting		* 
 * points into 1/16x1/16 LFM grid.				* 
 *								* 
 * void rcm (char *fname, char *wws, char *wwatt, int *idither,	* 
 *        char *meso, char *tvs, char *cntr, char *maxtop,	* 
 *        int *iradinfo, int *imdr, char *map, char *clrbar,	* 
 *        int *nlun, int *luns, int *ier)			* 
 ****************************************************************/
#include "geminc.h"
#include "gemprm.h"

#include "rcm.h"

/* local global variables */
int NEXINDX;
int NEXSTNS;
rad_struct RADARS[MAXSTNS];


int load_stns()
/****************************************************************
 * Subroutine to read in station table information and 		*
 * initialize radar station structire.				*
 *								*
 * int load_stns()						*
 ****************************************************************/
{
int lun,ier;
int eof=0;
char stnid[9],stnnam[33],state[3],coun[3],tbchrs[20];
int isnum,ispri,lenid;
float tlat,tlon,hght;

static char stndir[]="stns";

NEXSTNS = 0;

fl_tbop ( nextbl, stndir, &lun, &ier, strlen(nextbl), strlen(stndir) );
if(ier != 0) return(-1);

while(eof == 0)
   {
   tb_rstn ( &lun, stnid, stnnam, &isnum, state, coun, &tlat, &tlon, &hght, &ispri, tbchrs, &ier,
             sizeof(stnid),sizeof(stnnam),sizeof(state),sizeof(coun),sizeof(tbchrs) );
   if(ier != 0) 
      eof = 1;
   else
      {
      st_lstr(stnid, &lenid, &ier, sizeof(stnid));
      RADARS[NEXSTNS].obs_time = -1;

      RADARS[NEXSTNS].idlst[0] = '\0';
      strncat(RADARS[NEXSTNS].idlst,stnid,lenid);
      RADARS[NEXSTNS].stnlat = tlat;
      RADARS[NEXSTNS].stnlon = tlon;
      RADARS[NEXSTNS].mode = MDNA;
      RADARS[NEXSTNS].maxtop = -1; RADARS[NEXSTNS].maxtop_ggg[0] = '\0';
      RADARS[NEXSTNS].ncntr = 0;
      RADARS[NEXSTNS].nmeso = 0;
      RADARS[NEXSTNS].ntvs = 0;
      
      NEXSTNS++;
      if(NEXSTNS > MAXSTNS)
         {
         printf("too many stations, increase MAXSTNS\n");
         return(-1);
         }
      }
   }

fl_clos(&lun, &ier);

return(0);

}

int get_ndex(char *bultin, int lenbul)
/****************************************************************
 * Subroutine to identify the report ID from the PIL RCMccc.	*
 *								*
 * int get_ndex(char *bultin, int lenbul)			*
 ****************************************************************/
{
char *spos;
char NEXID[4];
int iret,i;

spos = strstr(bultin,"\nRCM") + 4;
NEXID[0] = '\0'; 
strncat(NEXID,spos,3);

for(i=0;i<NEXSTNS;i++)
   if(strcmp(NEXID,RADARS[i].idlst) == 0) return(i);

printf("could not find %s in station table %s\n",NEXID,nextbl);
return(-1);

}

void rcm_process(int fp, time_t tstart, time_t tend)
/****************************************************************
 * RCM driver routine.						*
 *								*
 * void rcm_process(int fp, time_t tstart, time_t tend)		*
 ****************************************************************/
{
int lenbul;
float clat,clon;
int mode, valid, iret;
char bultin[MAXBUL];

while(get_nextbull(fp,bultin,&lenbul) == 0)
   {
   if(isrcm(bultin,lenbul) == 0)
      {
      NEXINDX = get_ndex(bultin,lenbul);
      if(NEXINDX >= 0)
         {
         clat = RADARS[NEXINDX].stnlat; clon = RADARS[NEXINDX].stnlon;
         read_nexaa(bultin, lenbul, RADARS, NEXINDX, tstart, tend, &mode, &valid);
         if(mode > MDNA) RADARS[NEXINDX].mode = mode;
         if((valid == 0)&&(mode > MDNE)) /* should only do precip and clear air */
            read_nexcc(bultin, lenbul, RADARS,NEXINDX, tstart, tend);
         }
      }
   }
}

int rcm_open(char *fname, char *defdir)
/****************************************************************
 * RCM file open routine. Files contain binary data.		*
 *								*
 * int rcm_open(char *fname, char *defdir)			*
 ****************************************************************/
{
int iret, fdes;
long flen;
char newfil[512];

cfl_inqr(fname, defdir, &flen, newfil, &iret);
if(iret != 0) return(-1);

cfl_dopn(newfil, &fdes, &iret);
return(fdes);
}

void templregex (char *instr, char *outstr)
/****************************************************************
 * Regular expression conversions for file templates.		*
 *								*
 * void templregex (char *instr, char *outstr)			*
 ****************************************************************/
{
int ier;
cst_rpst(instr, "YYYY", "[0-9][0-9][0-9][0-9]", outstr, &ier);
cst_rpst(outstr, "YY", "[0-9][0-9]", outstr, &ier);
cst_rpst(outstr, "MMM", "[A-Za-z][A-Za-z][A-Za-z]", outstr, &ier);
cst_rpst(outstr, "MM", "[01][0-9]", outstr, &ier);
cst_rpst(outstr, "DD", "[0-3][0-9]", outstr, &ier);
cst_rpst(outstr, "HH", "[0-2][0-9]", outstr, &ier);
cst_rpst(outstr, "NN", "[0-6][0-9]", outstr, &ier);
cst_rpst(outstr, "DWK", "[A-Za-z][A-Za-z][A-Za-z]", outstr, &ier);
cst_rpst(outstr, "FFF", "[0-9][0-9][0-9]", outstr, &ier);
cst_rpst(outstr, "FF", "[0-9][0-9]", outstr, &ier);
}


void rcm (char *fname, char *wws, char *wwatt, int *idither,
          char *meso, char *tvs, char *cntr, char *maxtop,
          int *iradinfo, int *imdr, char *map, char *clrbar, 
          int *nlun, int *luns, int *ier)
/****************************************************************
 * Main callable routine. Loops through RCM files, most recent 	*
 * first. Older data will not overwrite more recent data.	*
 *								*
 * void templregex (char *instr, char *outstr)			*
 ****************************************************************/
{
int fp,i,iret;
int trange;

time_t tstart,tend;
struct tm new_time;
int itimarr[5], jtimarr[5], ktimarr[5], difstart, difend;

char line[MAXSZ], path[132],tmplt[132], newtmplt[160];
char filnam[512],*postok;
char gemtim[20];
int maxline=MAXSZ, order=-1, itype=1;
int icat, iscat, ifrm, irng, iint, ionoff, plens, tlens, llen, numstr, numscd, hrsbfr, mnsbfr, hraftr, mnaftr, mstrct, dtmmtch;
char zone[7];
char defstr[] = " ";
char defdir[] = " ";
static char **arrptr;
static int init=-1, nstrings = 10;

if(init == -1)
   {
   init = 0;
   arrptr = (char **) malloc(sizeof(char *) * nstrings);
   for(i=0; i < nstrings; i++)
      arrptr[i] = (char *) malloc(MAXTMPL);

   /* All our times will be UTC, don't do any conversions in mktime() */
   putenv("TZ=UTC0");
   tzset();
   }

if(load_stns() != 0)
   {
   printf("could not open %s\n",nextbl);
   return;
   }

set_dither(idither);
set_meso(meso);
set_tvs(tvs);
set_cntr(cntr);
set_maxtop(maxtop);
set_radinfo(iradinfo);
set_mdrplot(imdr);

if(strlen(wws) > 0)
   {
   if((strncmp(wws,"cur",3)==0)||(strncmp(wws,"CUR",3)==0))
      {
      /* get current clock */
      css_date(&itype, &new_time.tm_year, &new_time.tm_mon, &new_time.tm_mday,
		  &new_time.tm_hour, &new_time.tm_min, &new_time.tm_sec,
		  &new_time.tm_yday, zone, &iret);
      itimarr[0] = new_time.tm_year;
      itimarr[1] = new_time.tm_mon;
      itimarr[2] = new_time.tm_mday;
      itimarr[3] = new_time.tm_hour;
      itimarr[4] = new_time.tm_min;
      new_time.tm_year -= 1900;
      new_time.tm_mon -= 1;
      new_time.tm_isdst = -1;
      }
   else
      {
      /*set end time */
      ti_ctoi(wws, itimarr, &iret, strlen(wws));
      new_time.tm_year = itimarr[0] - 1900;
      new_time.tm_mon = itimarr[1] - 1;
      new_time.tm_mday = itimarr[2];
      new_time.tm_hour = itimarr[3];
      new_time.tm_min = itimarr[4];
      new_time.tm_sec = 0;
      new_time.tm_isdst = -1;
      }
   }
else
   {
   /* get current clock */
   css_date(&itype, &new_time.tm_year, &new_time.tm_mon, &new_time.tm_mday,
	       &new_time.tm_hour, &new_time.tm_min, &new_time.tm_sec,
	       &new_time.tm_yday, zone, &iret);
   itimarr[0] = new_time.tm_year;
   itimarr[1] = new_time.tm_mon;
   itimarr[2] = new_time.tm_mday;
   itimarr[3] = new_time.tm_hour;
   itimarr[4] = new_time.tm_min;
   new_time.tm_year -= 1900;
   new_time.tm_mon -= 1;
   new_time.tm_isdst = -1;
   }


/* assume 20 minute range if not specified */
if(strlen(wwatt) > 0)
   {
   iret = sscanf(wwatt,"%d",&trange);
   if(iret < 1) trange = 20;
   }
else
   trange = 20;

ti_subm ( itimarr, &trange, jtimarr, &iret);
tend  = mktime(&new_time);
tstart = tend - (trange * 60);

if(strlen(fname) > 0)
   {
   cst_clst(fname, ';', defstr, nstrings, MAXTMPL, arrptr, &numstr, &iret);
   for(i=0;i<numstr;i++)
      {
      ctb_dtget ( arrptr[i], path, tmplt, &icat, &iscat, &ifrm, &irng, &iint, 
           &ionoff, &hrsbfr, &mnsbfr, &hraftr, &mnaftr, &mstrct, 
	   &dtmmtch, &iret);
      if((iret != 0))
         {
	 if( (fp = rcm_open(arrptr[i],defdir) ) < 0 )
	    {
            printf("error opening %s\n",arrptr[i]);
            *ier = -1;
            return;
            }
	 rcm_process(fp, tstart, tend);
         close(fp);
         }
      else
	 {
         st_null ( path, path, &plens, &iret, sizeof(path), sizeof(path));
         st_null ( tmplt, tmplt, &tlens, &iret, sizeof(tmplt), sizeof(tmplt));
         templregex ( tmplt, newtmplt ); tlens = strlen(newtmplt);
         cfl_scnd (path, &plens, newtmplt, &tlens, ";", &maxline, &order, line, &llen, &numscd, &iret);

         postok = strtok(line,";");
         while(postok != NULL)
            {
	    cfl_mdat(postok, tmplt, "000000/0000", gemtim, &iret);
            ti_ctoi(gemtim, ktimarr, &iret, strlen(gemtim));
	    ti_mdif(ktimarr, jtimarr, &difstart, &iret);
	    ti_mdif(itimarr, ktimarr, &difend, &iret);

	    if((difstart >= 0)&&(difend >= 0)) 
	       { 
               sprintf(filnam,"%s/%s\0",path,postok);
               printf("searching filnam %s\n",filnam);
               if((fp = rcm_open ( filnam, NULL ) ) > 0)
                  {
                  rcm_process(fp, tstart, tend);
                  close(fp);
                  }
	       }
            postok = strtok(NULL,";");
            }
         }
      }
    
   gg_map (map, &iret, strlen(map)); 
   radar_info(RADARS,NEXSTNS);
   radar_stats(RADARS,NEXSTNS,nlun,luns);
   mdr_cbar(clrbar);
   }
else
   {
   printf("Could not open RCM file %s\n",fname);
   *ier = -1;
   }


}

