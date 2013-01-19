/*#include <geminc.h>
#include <gemprm.h>*/

#include "gbcmn.h"
#include "gb2def.h"

#include "dcgrib.h"

void dcflnam2 ( char *gemfil, char *filnam, int *maxgrids,
		gribfield *gfld, char *gdattm1, char *gdattm2,
		int *ier)
{
char ftmpl[256],ensnam[20];

char *cpos;
int iret;
int grid_id, center=gfld->idsect[0],subcenter=gfld->idsect[1], process=gfld->ipdtmpl[4];


*ier = 0;

if (gfld->griddef == 0)   /* note... the center defined grid number is not provided */
   grid_id = decode_g2gnum ( gfld );
else
   grid_id = gfld->griddef;


if((gemfil != NULL)&&(gemfil[0] != '\0'))
   {
   strcpy(filnam,gemfil);
   cfl_mnam ( gdattm1, gemfil, filnam, &iret);
   }
else
   {
   tbtmpl(center,subcenter,process,grid_id,ftmpl,maxgrids,&iret);
   if(iret != 0)
      {
      *ier = -1;
      filnam[0] = '\0';
      return;
      }
   strcpy(filnam,ftmpl);
   cfl_mnam ( gdattm1, ftmpl, filnam, &iret);
   }

if((cpos = (char *)strstr(filnam,"@@@")) != NULL)
   {
   cpos[0] = 48 + (grid_id / 100);
   cpos[1] = 48 + ((grid_id / 10)%10);
   cpos[2] = 48 + (grid_id % 10);
   }
if((cpos = (char *)strstr(filnam,"###")) != NULL)
   {
   cpos[0] = 48 + (process / 100);
   cpos[1] = 48 + ((process / 10)%10);
   cpos[2] = 48 + (process % 10);
   }
if((cpos = (char *)strstr(filnam,"%%%")) != NULL)
   {
   cpos[0] = 48 + (subcenter / 100);
   cpos[1] = 48 + ((subcenter / 10)%10);
   cpos[2] = 48 + (subcenter % 10);
   }
if((cpos = (char *)strstr(filnam,"%subc%")) != NULL)
   {
   if(subcenter != 0)
      {
      char *subtemp;
      subtemp = tbsubcenter(&center,&subcenter);
      cst_rpst(filnam,"%subc%", subtemp, ftmpl, &iret);
      }
   else
      cst_rpst(filnam,"%subc%", "", ftmpl, &iret);

   if(iret == 0)
      sprintf(filnam,"%s\0",ftmpl);
   }

if((cpos = (char *)strstr(filnam,"%ens%")) != NULL) 
   {
   ensnam[0] = '\0';
   gb2_ens(gfld,ensnam);
   if ( strlen(ensnam) > 0 ) 
      {
      cst_uclc(ensnam,ensnam,&iret);
      cst_rpst(filnam,"%ens%", ensnam, ftmpl, &iret);
      }
   else
      cst_rpst(filnam,"%ens%", "", ftmpl, &iret);

   if ( iret == 0 ) sprintf(filnam,"%s\0",ftmpl);
   }

}
