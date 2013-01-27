#include "geminc.h"
#include "gemprm.h"
#include "rsl.h"

#ifdef UNDERSCORE
#define nexr_list_lev	nexr_list_lev_
#endif

/*
 * prototypes
 */
void print_header_for_volume(Volume *v);

void nexr_list_lev ( char *filnam, int *lenfnam,
		char *radparm, int *lenparm, int *ier)

{

int i, nbin, iret;
long    flen, lofset = 20;
char cfil[LLPATH], cprm[LLMXLN];
char stid[]="KFTG", callid[5];

Radar *radar=NULL;
FILE *fp;


*ier = 0;

strncpy(cfil, filnam, *lenfnam);
strncpy(cprm, radparm, *lenparm);
cfil[*lenfnam] = '\0';
cprm[*lenparm] = '\0';

/* get actual file name to use in rsl call */
cfl_inqr ( cfil, NULL, &flen, cfil, &iret);

RSL_select_fields(cprm, NULL);

/*
if ( cprm[0] == 'd' )
        VINDEX = DZ_INDEX;
else if ( cprm[0] == 's' )
        VINDEX = SW_INDEX;
else if ( cprm[0] == 'v' )
        VINDEX = VR_INDEX;
*/

/*
 * see if we can get the station ID from bytes 21-24, otherwise, just use any ID
 */

fp = cfl_ropn ( cfil, NULL, &iret );
if  ( iret != 0 )  {
    printf("failed to open %s\n",cfil);
    *ier = -1;
    return;
    }
else {
    cfl_seek ( fp, lofset, SEEK_SET, &iret );
    if  ( iret != 0 )  {
        *ier = -1;
        cfl_clos ( fp, &iret );
        return;
        }
    }

cfl_read ( fp, 4, (unsigned char *)callid, &nbin, &iret );
cfl_clos ( fp, &iret );

if (( nbin < 4 )||(callid[0] < 'A')||(callid[0] > 'Z')) /* use a safe station ID....we aren't plotting lat/lon anyhow */
   strcpy(callid,stid);
else
   callid[4] = '\0';


/*RSL_radar_verbose_on();*/
RSL_read_these_sweeps("all", NULL);
radar = RSL_wsr88d_to_radar(cfil, callid);
/*RSL_radar_verbose_off();*/

if ( radar == NULL )
   {
   printf("failed to open %s\n",cfil);
   *ier = -1;
   return;
   }

for (i=0; i<radar->h.nvolumes; i++) {
   if (radar->v[i]) {
        printf("Volume %s: Sweeps available %d\n", cprm, radar->v[i]->h.nsweeps);
        print_header_for_volume(radar->v[i]);
      }
   }

RSL_free_radar(radar);


}
