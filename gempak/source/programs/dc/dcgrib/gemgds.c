#include <stdio.h>
#include "gbcmn.h"

#ifdef	UNDERSCORE
#define	gemggds gemggds_
#endif

void gemgds(gdsloc)
char *gdsloc;
{

gb_gds((unsigned char *)gdsloc);

return;
}

void gemggds(gdsproj,gdslatll,gdslatur,gdslonll,gdslonur,
             gdsang1,gdsang2,gdsang3)
int *gdsproj;
float *gdslatll,*gdslatur,*gdslonll,*gdslonur,*gdsang1,*gdsang2,*gdsang3;
{
*gdsproj = gds.grid_proj;
*gdslatll = (float)gds.latll;
*gdslatur = (float)gds.latur;
*gdslonll = (float)gds.lonll;
*gdslonur = (float)gds.lonur;
*gdsang1 = gds.angle1;
*gdsang2 = gds.angle2;
*gdsang3 = gds.angle3;
}
