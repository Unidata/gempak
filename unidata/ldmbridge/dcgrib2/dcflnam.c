#include <geminc.h>
#include <gemprm.h>

#include <gbcmn.h>

#include "dcgrib.h"

void dcflnam ( char *gemfil, char *filnam, int *maxgrids, int *ier)
{
int itime[3],iaccm;
char dattim[DTTMSZ];

char ftmpl[256];

char *cpos;
int lens, iret;

*ier = 0;


gb_ftim ( itime, &iaccm, &iret);
/*tg_itoc ( itime, dattim, &iret, sizeof(dattim) - 1 );*/

dattim[0] = '\0';
ctg_itoc ( itime, dattim, &iret);
cst_rmbl ( dattim, dattim, &lens, &iret);


if((gemfil != NULL)&&(gemfil[0] != '\0'))
   {
   strcpy(filnam,gemfil);
   cfl_mnam ( dattim, gemfil, filnam, &iret);
   }
else
   {
   tbtmpl(pds.center,pds.izero,pds.process,pds.grid_id,ftmpl,maxgrids,&iret);
   if(iret != 0)
      {
      *ier = -1;
      filnam[0] = '\0';
      return;
      }
   strcpy(filnam,ftmpl);
   cfl_mnam ( dattim, ftmpl, filnam, &iret);
   }

if((cpos = (char *)strstr(filnam,"@@@")) != NULL)
   {
   cpos[0] = 48 + (pds.grid_id / 100);
   cpos[1] = 48 + ((pds.grid_id / 10)%10);
   cpos[2] = 48 + (pds.grid_id % 10);
   }
if((cpos = (char *)strstr(filnam,"###")) != NULL)
   {
   cpos[0] = 48 + (pds.process / 100);
   cpos[1] = 48 + ((pds.process / 10)%10);
   cpos[2] = 48 + (pds.process % 10);
   }
if((cpos = (char *)strstr(filnam,"%%%")) != NULL)
   {
   cpos[0] = 48 + (pds.izero / 100);
   cpos[1] = 48 + ((pds.izero / 10)%10);
   cpos[2] = 48 + (pds.izero % 10);
   }
if((cpos = (char *)strstr(filnam,"%subc%")) != NULL)
   {
   if(pds.izero != 0)
      {
      char *subtemp;
      subtemp = tbsubcenter(&pds.center,&pds.izero);
      cst_rpst(filnam,"%subc%", subtemp, ftmpl, &iret);
      }
   else
      cst_rpst(filnam,"%subc%", "", ftmpl, &iret);

   if(iret == 0)
      sprintf(filnam,"%s\0",ftmpl);
   }
}
