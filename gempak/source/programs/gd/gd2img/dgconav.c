#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define dgconav	dgconav_
#define im_gm2gi im_gm2gi_
#endif

void	dgconav ( float *rnvblk, char *timfnd, char *glevel,
		char *gvcord, char *gfunc, int *kx, int *ky, 
		char *satfil, char *calinfo, char *wmohdr, int *iret )
{
    char pfunc[128], time1[DTTMSZ], time2[DTTMSZ], parm[128];
    int igx, igy, level[2], ivcord;
    int ier;

    int idtarr[5], ignhdr[135];
    int icompress=0;
    int lenp, ilen, offset;
    static char satdef[]="gd2img.gini";

    float *grid=NULL;


    /*printf("got here %s strlen %d\n",timfnd,strlen(timfnd));
    printf("got here %s\n",glevel);
    printf("got here %s\n",gvcord);
    printf("got here %s\n",gfunc);
    printf("got here %d %d\n",*kx,*ky);*/
    dg_onav ( (const float *)rnvblk, iret );

    if ( (grid = (float *)malloc( (*kx) * (*ky) * sizeof(float)) ) == NULL )
        {
        printf("grid allocation failed\n");
        *iret = -1;
        return;
        }

    dgc_grid ( timfnd, glevel, gvcord, gfunc, pfunc, grid,
	&igx, &igy, time1, time2, &level[0], &level[1],
	&ivcord, parm, iret );

    if ( *iret == 0 ) {
        /* write out a gini..... */
	ti_ctoi ((char *)time1, idtarr, &ier, strlen(time1));
	lenp = strlen(parm);
	gdhgin ( ignhdr, parm, &lenp, wmohdr, idtarr, &icompress, calinfo, &ier);

	gdlgin ( grid, &igx, &igy, &icompress, &ilen, &ier);

	lenp = strlen(satfil);
	if ( lenp == 0 ) {
	   strcpy(satfil, satdef);
	   lenp = strlen(satfil);
        }
	im_gm2gi ( satfil, ignhdr, rnvblk, &ier, lenp);

	offset = 21+512;
	im_wgin ( satfil, &lenp, &offset, &ilen, &ier);
    }
    else {
       printf("dgc_grid failed %d\n",*iret);
    }

    free(grid);


}
