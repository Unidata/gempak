/* include file containing all AODT library global variables */
#include "../inc/odtlib.h"
/* include file containing all AODT library variable definitions */
#include "../inc/odtlibdefs-x.h"
/* include file containing format statements for shared functions */
#include "../inc/odtlibfuncs.h"

int extern_writegrid(char [],int,float ***,int,int,int,int,float *);

int extern_writegrid(char *gridpath,int gridnum,float ***array,int nx,int ny,int date,int time,float *vcrit) 
{
  FILE *fp;
  int i,j;		                /* index */
  int vmax,spd,dir;
  char *cgridnum;
  char *cgridnum2,*gridpath2;
  char *GRIDNAME;
  char *GRIDNAME2;
  char *cdate;
  float value,lat,lon;
  
  cgridnum=(char *)calloc((size_t)9,sizeof(char));
  GRIDNAME=(char *)calloc((size_t)200,sizeof(char));
  cgridnum2=(char *)calloc((size_t)13,sizeof(char));
  GRIDNAME2=(char *)calloc((size_t)200,sizeof(char));
  gridpath2=(char *)calloc((size_t)200,sizeof(char));
  cdate=(char *)calloc((size_t)12,sizeof(char));

  sprintf(cgridnum,"grid%4.4d",gridnum);
  (void)strcpy((char *)&GRIDNAME[0],gridpath);
  strncat(GRIDNAME,cgridnum,strlen(cgridnum));

  fp=fopen(GRIDNAME,"w+");
  if(fp==0) {
    fclose(fp);
    return -107;
  }

  value=0.0;
  for (i = 0 ; i < nx ; i++) {
    for (j = 0 ; j < ny ; j++) {
      lat=array[i][j][0];
      lon=array[i][j][1];
      value=array[i][j][2];
      if(value<0.0) {
        value = 0.0;               /* outer wind radii */
      }
      fprintf(fp,"%6.2f %7.2f %7.2f\n",lat,lon,value);
    }
  }

  fclose(fp);

  sprintf(cgridnum2,"grid%4.4d-pos",gridnum);
  (void)strcpy((char *)&GRIDNAME2[0],gridpath2);
  strncat(GRIDNAME2,cgridnum2,strlen(cgridnum2));

  fp=fopen(GRIDNAME2,"w+");
  if(fp==0) {
    fclose(fp);
    return -108;
  }

  aodt_julian2cmonth(date,cdate);
  vmax=(int)vcrit[8];
  spd=(int)vcrit[6];
  dir=(int)vcrit[7];
  fprintf(fp,"%6.2f %7.2f %9s %6.6dZ %3.3d %3.3d %3.3d\n",odtcurrent->IR.latitude,odtcurrent->IR.longitude,cdate,time,vmax,spd,dir);

  free(cgridnum);
  free(GRIDNAME);
  free(cgridnum2);
  free(GRIDNAME2);
  free(gridpath2);
  free(cdate);
  fclose(fp);

  return (107);
}
