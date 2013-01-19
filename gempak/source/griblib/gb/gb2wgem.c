#include "gb2def.h"

#ifdef UNDERSCORE
#define gd_wpg2	gd_wpg2_
#endif

g2int g2_locfld(unsigned char *cgrib, g2int ifldnum,
        int *len1, unsigned char **pos1,
        int *len2, unsigned char **pos2,
        int *len3, unsigned char **pos3,
        int *len4, unsigned char **pos4,
        int *len5, unsigned char **pos5,
        int *len6, unsigned char **pos6,
        int *len7, unsigned char **pos7);

void gd_wpg2 ( int *iflno, int *outdata, int *nword, int *kx, int *ky,
           int ighdr[], char gdattm[][DTTMSZ], int level[], int *vcord, char *parm,
           int *iuscal, float *rmsval, int *scan_mode, int *replace,
           int *iret, size_t, size_t);

void gb2_wgem( Gribmsg cmsg, Geminfo gem, int n, int iflno, int replace, 
	int ighdr[], int *iret )
/************************************************************************
 * gb2_wgem								*
 *									*
 * gb2_wgem ( cmsg, gem, n, iflno, replace, iret )			*
 *									*
 * Output parameters:							*
 *      *cmsg        struct gribmsg     current GRIB field              *
 *	*ivers		int		GRIB version number		*
 *	*iret		int		Return code			*
 *                                        0 = Successfull               *
 *                                      -34 = Could not process GRIB2   *
 *                                            message                   *
 **									*
 * Log:									*
 ***********************************************************************/
{
    int ier, icnt;
    int len1=0, len2=0, len3=0, len4=0, len5=0, len6=0, len7=0;
    int nbyte, nword, bpad, ipos=0;
    unsigned char *pos1=NULL, *pos2=NULL, *pos3=NULL, *pos4=NULL,
		*pos5=NULL, *pos6=NULL, *pos7=NULL, *outdata;
    char gdattm[2][DTTMSZ];
/*---------------------------------------------------------------------*/

    *iret = g2_locfld( cmsg.cgrib2, (g2int)n, 
	&len1, &pos1, &len2, &pos2, &len3, &pos3,
	&len4, &pos4, &len5, &pos5, &len6, &pos6,
	&len7, &pos7);

    if ( *iret == 0 ) {

       nbyte = ( len1 + len2 + len3 + len4 + len5 + len6 + len7 ) + (7*sizeof(int));

       nword = nbyte / sizeof(int);
       bpad = nbyte % sizeof(int);
       if ( bpad != 0 ) nword++;

       outdata = (unsigned char *)malloc(nword * sizeof(int));

       icnt = sizeof(int);

       sbit(outdata,(g2int *)&len1,(g2int)(ipos*8),icnt*8); ipos += icnt;
       memcpy(outdata+ipos,pos1,len1); ipos += len1;

       sbit(outdata,(g2int *)&len2,(g2int)(ipos*8),icnt*8); ipos += icnt;
       if (len2 > 0) memcpy(outdata+ipos,pos2,len2); ipos += len2;

       sbit(outdata,(g2int *)&len3,(g2int)(ipos*8),icnt*8); ipos += icnt;
       if ( len3 > 0 ) memcpy(outdata+ipos,pos3,len3); ipos += len3;

       sbit(outdata,(g2int *)&len4,(g2int)(ipos*8),icnt*8); ipos += icnt;
       if ( len4 > 0 ) memcpy(outdata+ipos,pos4,len4); ipos += len4;

       sbit(outdata,(g2int *)&len5,(g2int)(ipos*8),icnt*8); ipos += icnt;
       if ( len5 > 0 ) memcpy(outdata+ipos,pos5,len5); ipos += len5;

       sbit(outdata,(g2int *)&len6,(g2int)(ipos*8),icnt*8); ipos += icnt;
       if ( len6 > 0 ) memcpy(outdata+ipos,pos6,len6); ipos += len6;

       sbit(outdata,(g2int *)&len7,(g2int)(ipos*8),icnt*8); ipos += icnt;
       if ( len7 > 0 ) memcpy(outdata+ipos,pos7,len7); ipos += len7;

       if ( bpad != 0 ) memset(outdata+ipos,0,(sizeof(int) - bpad) % 4);

       memcpy(gdattm[0],gem.gdattm1,DTTMSZ);
       memcpy(gdattm[1],gem.gdattm2,DTTMSZ);

       gd_wpg2 ( &iflno, (int *)outdata, &nword, &(cmsg.kx), &(cmsg.ky),
	   ighdr, gdattm, gem.level, &gem.vcord, gem.parm, 
	   &gem.iuscal, &gem.rmsval, &(cmsg.g2scan_mode), &replace, 
	   iret, DTTMSZ, 12);

       free(outdata);

       
    }

}
