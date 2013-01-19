#include "geminc.h"
#include "gemprm.h"

#include "rcm.h"

char *radar_mode_tostring(int mode)
{
static char NEstr[] = "NE";
static char OMstr[] = "OM";
static char NAstr[] = "NA";
static char CLRstr[] = "CLEAR AIR";
static char PCPstr[] = "PRECIP";
static char UNKstr[] = "UNKNOWN";

switch (mode)
   {
   case MDNE:
	return(NEstr);
   case MDOM:
	return(OMstr);
   case MDNA:
	return(NAstr);
   case MDCLAR:
	return(CLRstr);
   case MDPCPN:
	return(PCPstr);
   }
return(UNKstr);
}

void radar_stats(rad_struct RADARS[], int NEXSTNS, int *nlun, int *luns)
{
int i,j,ilen,iret;
char timestr[30],outstr[132];
float nx[1],ny[1],lat[1],lon[1];
struct tm *tstruct;
static int np=1;
static char ncord[]="N";
static char mcord[]="M";

sprintf(outstr,"Station  Observation Time       Mode\0");
text_output(nlun,luns,outstr,strlen(outstr));

sprintf(outstr,"____________________________________\0");
text_output(nlun,luns,outstr,strlen(outstr));


for(i=0;i<NEXSTNS;i++)
   {
   if(RADARS[i].obs_time > -1)
      {
      /*ilen =  cftime (timestr,"%Y %m %d: %H%M\0", &(RADARS[i].obs_time)); not supported by compaq */
      tstruct = gmtime (&(RADARS[i].obs_time));
      strftime(timestr, 30, "%Y %m %d: %H%MZ\0", tstruct);
      }
   else
      sprintf(timestr,"Station not found\0");

   sprintf(outstr,"%-8s %-17s %9s\0",RADARS[i].idlst,timestr,radar_mode_tostring(RADARS[i].mode));
   text_output(nlun,luns,outstr,strlen(outstr));

   for(j=0;j<RADARS[i].nmeso;j++)
         {
         nx[0] = RADARS[i].meso[j].nx;
         ny[0] = RADARS[i].meso[j].ny;
         gtrans ( ncord, mcord, &np, nx, ny, lat, lon, &iret,
            strlen(ncord),strlen(mcord));
         sprintf(outstr,"\t\t\t\t\t%s Meso%02d %8.4f %9.4f\0",RADARS[i].idlst,j+1,lat[0],lon[0]);
         text_output(nlun,luns,outstr,strlen(outstr));
         }
   for(j=0;j<RADARS[i].ntvs;j++)
         {
         nx[0] = RADARS[i].tvs[j].nx;
         ny[0] = RADARS[i].tvs[j].ny;
         gtrans ( ncord, mcord, &np, nx, ny, lat, lon, &iret,
            strlen(ncord),strlen(mcord));
         sprintf(outstr,"\t\t\t\t\t%s TVS%02d  %8.4f %9.4f\0",RADARS[i].idlst,j+1,lat[0],lon[0]);
         text_output(nlun,luns,outstr,strlen(outstr));
         }
   }

}
