#include <geminc.h>
#include <gemprm.h>

#include <gbcmn.h>

#define SGRID_DEF	1

#include "dcsgrid.h"

#include "dcgrib.h"

int dcsgrid()
{
static char sgrid_table[]="dcgrib.tbl";
static char ttype[]="grid";
static int table_read=0;
int iret,ier,lens,lun;
int itbl=0;

if(table_read == 0)
   {
   fl_tbop(sgrid_table,ttype,&lun,&iret,strlen(sgrid_table),strlen(ttype));
   if(lun > 0)
      {
      while (iret == 0)
         {
         tbsgrid(&lun, &gtype[itbl].cntr, &gtype[itbl].gid, &gtype[itbl].cid,
                 gtype[itbl].proj, gtype[itbl].ang, gtype[itbl].gar, 
                 &gtype[itbl].nx, &gtype[itbl].ny, &gtype[itbl].scol, 
                 &gtype[itbl].srow,&iret, sizeof(gtype[itbl].proj)-1);

         if(iret == 0)
            {
	    gtype[itbl].proj[sizeof(gtype[itbl].proj)-1] = '\0';
	    cst_rmbl ( gtype[itbl].proj, gtype[itbl].proj, &lens, &ier);
            itbl++;
            }

         }
      }
   fl_clos(&lun,&iret);
   nsgridvals=itbl;
   sgridval = -1;
   table_read = 1;
   }

/* prevent unnecessary searching */
if((sgridval >= 0)&&(gtype[sgridval].cntr == pds.center)&&
    (gtype[sgridval].gid == pds.grid_id))
   return(gtype[sgridval].cid);

itbl = 0;
while(itbl < nsgridvals)
   {
   if((gtype[itbl].cntr == pds.center)&&(gtype[itbl].gid == pds.grid_id))
      {
      sgridval = itbl;
      return(gtype[sgridval].cid);
      }
   itbl++;
   } 


return(0);
}
