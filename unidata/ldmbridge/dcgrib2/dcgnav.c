#include <geminc.h>
#include <gemprm.h>

#include <gbcmn.h>

#include "dcgrib.h"

int dcgnav()
{
int found=0;

int lun,ier,iret;

static int numgd=0,nxgd=0,nygd=0;
static float anggd[3],gargd[4],deln,extnd;
static char namgd[8],prjgd[20];

static char grdtbl[]="grdnav.tbl"; 
static char tbltyp[]="grid";

if(numgd != pds.grid_id)
   {
   fl_tbop(grdtbl,tbltyp,&lun,&ier,strlen(grdtbl),strlen(tbltyp));
   if(ier != 0) return(-1);

   while((found == 0)&&(ier == 0))
      {
      tb_grnv(&lun,namgd,&numgd,prjgd,anggd,gargd,&nxgd,&nygd,
              &deln,&extnd,&ier,sizeof(namgd),sizeof(prjgd));
      if(numgd == pds.grid_id) found = 1;
      }
   
   fl_clos(&lun,&iret);
   if(found != 1) 
      {
      printf("Grid id %d not in %s\n",pds.grid_id,grdtbl);
      return(-2);
      }
   }

/* set common block values needed for navigation */
gds.kx = nxgd;
gds.ky = nygd;
gds.latll = gargd[0];
gds.lonll = gargd[1];
gds.latur = gargd[2];
gds.lonur = gargd[3];
gds.angle1 = anggd[0];
gds.angle2 = anggd[1];
gds.angle3 = anggd[2];
if(strncmp(prjgd,"CED",3) == 0)
   gds.grid_proj = 0;
else if (strncmp(prjgd,"MER",3) == 0)
   gds.grid_proj = 1;
else if (strncmp(prjgd,"LCC",3) == 0)
   gds.grid_proj = 3;
else if (strncmp(prjgd,"STR",3) == 0)
   gds.grid_proj = 5;
else
   {
   printf("unknown projection %c%c%c\n",prjgd[0],prjgd[1],prjgd[2]);
   return(-3);
   }

return(0);
}
