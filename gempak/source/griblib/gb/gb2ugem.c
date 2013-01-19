#include "gb2def.h"

#ifdef UNDERSCORE
#define gb2_ugem	gb2_ugem_
#endif

/*void gb2_misjpg (gribfield *gfld, int kx, int ky, float rmsval);*/

void gb2_qlin(int nrows, int ix[], float *idat, int ni, int nj, float *odat);

void gb2_ugem( int idata[], int *iuscal, float *rmsval, int *kx,
		int *ky, int *scan_mode, int *kxky, float grid[], 
		int *iret )
/************************************************************************
 * gb2_ugem								*
 *									*
 * gb2_ugem ( )								*
 *									*
 * Output parameters:							*
 *                                        0 = Successful		*
 *					-31 = Could not allocate memory	*
 **									*
 * Log:									*
 ***********************************************************************/
{
    int 		ier, icnt, negflg, iofst, jerr;
    int 		i, n, ipos=0, seccnt;
    unsigned char 	*outdata=(unsigned char *)idata;
    unsigned char 	*pos1=NULL,*pos2=NULL, *pos3=NULL, *pos4=NULL, 
			*pos5=NULL, *pos6=NULL, *pos7=NULL;

    int 		len1, len2, len3, len4, len5, len6, len7;
    int 		nmiss, *qindx;
    float 		sfact, rmiss[2];

    gribfield 		*lgfld=(gribfield *)malloc(sizeof(gribfield));
    g2int 		*igds;

/*---------------------------------------------------------------------*/
    if ( lgfld == NULL ) {
       *iret = -31;
       return;
    }

    negflg = 0;
    icnt = sizeof(int);

    gbit(outdata,(g2int*)&len1,ipos*8,icnt*8); ipos+= icnt;
    pos1 = outdata + ipos;

    ipos += len1;
    gbit(outdata,(g2int*)&len2,ipos*8,icnt*8); ipos+= icnt;
    if ( len2 > 0 ) pos2 = outdata + ipos;

    ipos += len2;
    gbit(outdata,(g2int*)&len3,ipos*8,icnt*8); ipos+= icnt;
    if ( len3 > 0 ) pos3 = outdata + ipos;

    ipos += len3;
    gbit(outdata,(g2int*)&len4,ipos*8,icnt*8); ipos+= icnt;
    if ( len4 > 0 ) pos4 = outdata + ipos;

    ipos += len4;
    gbit(outdata,(g2int*)&len5,ipos*8,icnt*8); ipos+= icnt;
    if ( len5 > 0 ) pos5 = outdata + ipos;

    ipos += len5;
    gbit(outdata,(g2int*)&len6,ipos*8,icnt*8); ipos+= icnt;
    if ( len6 > 0 ) pos6 = outdata + ipos;

    ipos += len6;
    gbit(outdata,(g2int*)&len7,ipos*8,icnt*8); ipos+= icnt;
    if ( len7 > 0 ) pos7 = outdata + ipos;

    lgfld->locallen=0;
    lgfld->idsect=0;
    lgfld->local=0;
    lgfld->list_opt=0;
    lgfld->igdtmpl=0;
    lgfld->ipdtmpl=0;
    lgfld->idrtmpl=0;
    lgfld->coord_list=0;
    lgfld->bmap=0;
    lgfld->fld=0;

    for ( jerr=0, seccnt=1; jerr == 0 && seccnt <= 7; seccnt++ ) {
       iofst = 0;
       switch ( seccnt ) {
          case 1:
	      if ( pos1 != NULL )
                 jerr=g2_unpack1(pos1,&iofst,&lgfld->idsect,&lgfld->idsectlen);
	      break;
          case 2:
	      if ( pos2 != NULL )
                 jerr = g2_unpack2(pos2,&iofst,&lgfld->locallen,&lgfld->local);
	      break;
          case 3:
	      if ( pos3 != NULL ) {
                 jerr=g2_unpack3(pos3,&iofst,&igds,&lgfld->igdtmpl,
                          &lgfld->igdtlen,&lgfld->list_opt,&lgfld->num_opt);
                 if ( jerr == 0 ) {
                    lgfld->griddef=igds[0];
                    lgfld->ngrdpts=igds[1];
                    lgfld->numoct_opt=igds[2];
                    lgfld->interp_opt=igds[3];
                    lgfld->igdtnum=igds[4];
                 }
              }
	      
	      break;
	  case 4:
	      if ( pos4 != NULL )
	         jerr=g2_unpack4(pos4,&iofst,&lgfld->ipdtnum,
                            &lgfld->ipdtmpl,&lgfld->ipdtlen,&lgfld->coord_list,
                            &lgfld->num_coord);
	      break;
	  case 5:
	      if ( pos5 != NULL )
	         jerr=g2_unpack5(pos5,&iofst,&lgfld->ndpts,&lgfld->idrtnum,
                          &lgfld->idrtmpl,&lgfld->idrtlen);
	      break;
	  case 6:
	      if ( pos6 != NULL )
	         jerr=g2_unpack6(pos6,&iofst,lgfld->ngrdpts,&lgfld->ibmap,
                         &lgfld->bmap);
	      break;
	  case 7:
	      jerr=g2_unpack7(pos7,&iofst,lgfld->igdtnum,lgfld->igdtmpl,
                          lgfld->idrtnum,lgfld->idrtmpl,lgfld->ndpts,
                          &lgfld->fld);
	      break;
       }
    }

    if ( jerr != 0 ) {
       printf("Could not unpack grib2 sections [jerr = %d]\n",jerr);
       *kxky = 0;
       *iret = -31;
    }
    else {

       /*
        *  If a bitmap is included with the field, set all
        *  unmapped grid points to missing val, rmsval.
        */
       if ( lgfld->ibmap != 255 && lgfld->bmap != 0 ) {
	   /*printf("has a bit mask ndata %d npoints %d rmsval %f\n",lgfld->ndpts,lgfld->ngrdpts,*rmsval);*/
           n=0;
           for (i=0;i<lgfld->ngrdpts;i++) {
               if (lgfld->bmap[i]==1) 
		  grid[i] = lgfld->fld[n++];
	       else
		  grid[i] = *rmsval;
           }
	   lgfld->fld = (float *) realloc( lgfld->fld, lgfld->ngrdpts * sizeof(float) );
	   memcpy ( lgfld->fld, grid, lgfld->ngrdpts*sizeof(float) );
	   lgfld->ndpts = lgfld->ngrdpts;
               
       }

       /*
        *  If this is a reduced (Gaussian) grid, create a regular KX * KY grid
	*/
       if ( lgfld->num_opt > 0 ) {
	   n = 0;
 	  /*lgfld->numoct_opt=igds[2];
          lgfld->interp_opt=igds[3]*/

	   /* copy the reduced grid points and reallocate the working grid */
	   memcpy ( grid, lgfld->fld, lgfld->ngrdpts*sizeof(float) ) ;

	   /* qindx is the array of grid offsets */
	   qindx = (int *)malloc((lgfld->num_opt + 1) * sizeof(int));
	   qindx[0] = 0;
	   for ( i = 1; i <= lgfld->num_opt; i++) {
	       qindx[i] = lgfld->list_opt[i-1] + qindx[i-1];
	   }

	   n = (*kx) * (*ky);
	   if ( n > lgfld->ngrdpts )
		lgfld->fld = (float *) realloc( lgfld->fld, n * sizeof(float) );

	   gb2_qlin((int)lgfld->num_opt,qindx,grid,*kx,*ky,lgfld->fld);
	   lgfld->ngrdpts = n;

	   free(qindx);
       }

       /*
        *  If missing value management is used for DRT's 5.2 and 5.3,
        *  set missing grid points to missing val, rmsval.
        */

       if ( (lgfld->idrtnum == 2  || lgfld->idrtnum == 3) &&
          lgfld->idrtmpl[6] != 0  ) {

          gb2_gmis( lgfld, rmiss, &nmiss);

	  printf("missing value management %f %f\n",rmiss[0],rmiss[1]);

          if ( lgfld->idrtmpl[6] == 1 ) {
             for (i=0; i<lgfld->ngrdpts; i++) {
                 if ( G_DIFF (lgfld->fld[i], rmiss[0] ) ) lgfld->fld[i] = *rmsval;
             }
          }

          if ( lgfld->idrtmpl[6] == 2 ) {
             for (i=0; i<lgfld->ngrdpts; i++) {
                 if ( G_DIFF ( lgfld->fld[i], rmiss[0] ) ||
                       G_DIFF ( lgfld->fld[i], rmiss[1] ) ) lgfld->fld[i] = *rmsval;
             }
          }

       }

       /*
	* Check JPEG & PNG missing values
	**
	Apparently not needed now...bitmask now correctly implemented
	if ((lgfld->idrtnum == 40000)
              || (lgfld->idrtnum == 40)
              || (lgfld->idrtnum == 41)
              || (lgfld->idrtnum == 40010))
              gb2_misjpg (lgfld, *kx, *ky, *rmsval);
	*/

       /* Apply user scaling to the grid points after all other
	* data transformation
	*/
       if ( *iuscal != 0 ) {
	   sfact=(float)pow (10.0,(double)(*iuscal));
           for (i=0; i<lgfld->ngrdpts; i++) {
               if ( lgfld->fld[i] != *rmsval ) lgfld->fld[i] = lgfld->fld[i] * sfact;
           }
       }

       /*
        *  Make sure data points are oriented from lower left to upper
        *  right by rows.
        */
       gb2_ornt( *kx, *ky, *scan_mode, lgfld->fld, grid, iret );

       *kxky = lgfld->ngrdpts;

    }
  
    if ( igds != NULL ) free(igds); 
    if ( lgfld != NULL ) {
       if ( lgfld->fld != NULL ) free (lgfld->fld);
       free(lgfld);
    }


    *iret = 0;
    return;
}
