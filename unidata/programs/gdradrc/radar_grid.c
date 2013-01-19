/************************************************************************
 * RADAR_GRID.C								* 
 * This module contains routines used to project each NEXRAD image into *
 * the output grid projection and sample the radar data to the grid.	*
 *									*
 * CONTENTS:								*
 *									*
 * radar_grid()			Called by FORTRAN driver		*
 * next_radar()			Called by FORTRAN driver		*
 * radar_boundsinit()		Called by FORTRAN driver		*
 * radar_bounds()		Called by FORTRAN driver		*
 *									* 
 * Chiz/Unidata	02/01							* 
 * Chiz/Unidata 02/02		Removed gqbnd() call for perfomance	*
 ***********************************************************************/
#include <stdlib.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <math.h>

#include <geminc.h>
#include <gemprm.h>
#ifdef UNDERSCORE
#define	radar_grid	radar_grid_
#define next_radar	next_radar_
#endif

void next_radar(char *filpath, char *templ, char *gemtim, char *filnm, 
                int *numc, int *idelt, int *ier)
{
int i, nfile, iret;
int tarr1[5], tarr2[5];
char fname[FILE_FULLSZ],fmnam[80],outtim[20],cgemtim[16];
struct dirent **namelist=NULL;

  *idelt = 0;

  st_null(gemtim,cgemtim,&i,&iret,15,16);

  ti_ctoi(gemtim,tarr1,ier,15); 
  if(*ier != 0) return;

  /* create the name for which we will compare against */
  cfl_mnam(cgemtim, templ, fmnam, &iret);

  /* return list of files reverse sorted (newest time first) */
  cfl_scnt(filpath, templ, -1, &namelist, &nfile, &iret);

  /* find the first file whose time is older than our current time */
  i = 0;
  while ( i < nfile ) {
     strcpy(fname, namelist[i]->d_name);
     cfl_mdat(fname, templ, cgemtim, outtim, &iret);

     ti_ctoi(outtim,tarr2,&iret,strlen(outtim));

     ti_mdif(tarr1, tarr2, idelt, &iret);

     if ( iret == 0 && *idelt >= 0 ) {
       sprintf(filnm,"%s/%s\0",filpath,fname);
       *numc = strlen(filnm);
       break;
     }
     i++;
  }

  if ( i >= nfile ) *ier = -1;

  for ( i=0; i < nfile; i++) {
     free(namelist[i]);
  }
  if ( namelist != NULL ) free(namelist);

}


void radar_grid (char *filnam, int *numc, int *kx, int *ky, float *fdata, float *qmax,
	float *qmin, int *count)
{
int ier, eval, i, j, np;

char line[30];
char gsys[]="G", msys[]="M";
float data[3];
float xout, yout;
int ifound, lenbuf, inchar;
int ptype=-1;
float dbz;
FILE *fp;

*count = 0;

filnam[*numc] = '\0';

fp = cfl_ropn ( filnam, NULL, &ier );

if ( ier != 0 ) return;


/* search for a line starting with "Data " */
ifound = 0;
while ( ! ifound ) {
    if ( fgets (line, 6, fp) != NULL ) {
        if ( strcmp(line,"Data " ) == 0 ) {
            ifound = 1;
        }
	else if ( strcmp(line,"Preci" ) == 0 ) {
	    /* determine mode precipitation type values */
	    i = 4;
            inchar = line[i];
	    while ( ( i < ( sizeof(line) - 2 ) ) && 
		(inchar != EOF) && (inchar != '\n') ) {
                line[i] = inchar; i++;
                inchar = fgetc (fp); 
            }
            line[i] = '\0';
	    if ( strcmp(line, "Precipitation type: rain") == 0 )
		ptype = 1;
	    else
		ptype = 0;
        }
        else {
            lenbuf = strlen (line);
/*
 *          If newline not reached, position file after next newline.
 */
            if ( lenbuf == 5 ) {
		inchar = ' ';
                while ( (inchar != EOF) && (inchar != '\n') ) {
                    inchar = fgetc (fp);
                }
                if ( ferror (fp) ) {
                    cfl_iret ( errno, &eval, &ier );
                } 
            }
        }
   }
   else
      break;
}

if ( ifound == 0 ) {
   printf("faled to find Data line in %s\n",filnam);
   cfl_clos(fp,&ier);
}


while( fscanf(fp,"%f,%f,%f,",&data[0],&data[1],&data[2]) == 3 ) {
    np = 1;
    gtrans(msys, gsys, &np, &data[0], &data[1], &xout, &yout, &ier, strlen(msys), strlen(gsys));
    i = (int)rint(xout);
    j = (int)rint(yout);
    if ( ( i >= 0 ) && ( i <= (*kx-1) ) &&
        ( j >= 0 ) && ( j <= (*ky-1) ) ) {
	switch ( ptype ) {
	    case 1: /* Rain */
		dbz = 10 * log10( 295 * data[2] * 1.43 );
		data[2] = dbz;
		break;
	    case 0: /* Snow */
		dbz = 10 * log10( 1780 * pow(data[2],2.21) );
		data[2] = dbz;
		break;
	    default: /* unknown */
		dbz = RMISSD;
        }
        if ( data[2] > fdata[(*kx * j) + i] ) fdata[(*kx * j) + i] = data[2];
        if ( *qmin == RMISSD ) *qmin = data[2];
        if ( *qmax == RMISSD ) *qmax = data[2];
        if ( data[2] > *qmax ) *qmax = data[2];
        if ( data[2] < *qmin ) *qmin = data[2];
	*count = *count + 1;
    } 
}
cfl_clos(fp,&ier);
}

