#include <stdio.h>
#include <string.h>
#include "arasubs.h"
#include "mccal.h"

int araopt (int fd, int slot, int nopt, int *opt, int *val)
{
 int opt_name[5];
 int i,j;
 int cur_area = -1,iok;

 /*  'PREC'     PRECISION OF STORED DATA (1,2,OR 4 BYTES) */
 /*  'SPAC'     SPACING OF OUTPUT DATA (1,2,OR 4 BYTES) */
 /*  'UNIT'     OUTPUT UNITS (E.G. 'TEMP','RAD','BRIT' ... ) */
 /*  'SCAL'     SCALE FACTOR TO APPLY TO DATA */
 /*  'CALB'     1-4 CHARACTER NAME OF PACKAGE USED TO CALIBRATE DATA */
 strncpy ((char *)&opt_name[0], "PREC", 4);
 strncpy ((char *)&opt_name[1], "SPAC", 4);
 strncpy ((char *)&opt_name[2], "UNIT", 4);
 strncpy ((char *)&opt_name[3], "SCAL", 4);
 strncpy ((char *)&opt_name[4], "CALB", 4);

 for (i=0; i<=new_area; i++)
  {if (aracom[i].fd == fd) {cur_area = i; break;} }

 for (j=0; j<nopt; j++)
  {for (i=0; i<5; i++)
    {if (opt[j] == opt_name[i])
      {aracom[cur_area].options[i] = val[j];}
    }
  }

 iok = kbprep(slot, &aracom[cur_area]);
 if (iok)
	printf("\n   *** KBPREP ERROR = %d\n",iok);
 return 0;
}
