#include <stdio.h>
#include "gbcmn.h"

void gempds(char *pdsloc,char *pdsext,int *pdsparm)
{

pds.length = gb_btoi((unsigned char *)pdsloc, 0, 3, FALSE);
gb_pds((unsigned char *)pdsloc);

strcpy(pdsext,pds.extension);
*pdsparm = pds.parameter;

return;
}
