#include "geminc.h"
#include "gemprm.h"
#include "color.h"

#ifdef UNDERSCORE
#define im_nids_text_output im_nids_text_output_
#define gqmode	gqmode_
#define gqclrs	gqclrs_
#define gscolb	gscolb_
#define gplutf	gplutf_
#endif

extern float XMAX, XMIN;
extern char wintyp;
extern int graphic_color;
extern int line_color, line_type, _imnids_line_width;
extern int read_int (unsigned char *);
extern short read_short(unsigned char *);

//short read_short (unsigned char *x);
void xytolatlon ( float x, float y, float *lat, float *lon);

void ijtoxy (int i, int j, float *x, float *y)
{
int imode, ier;

gqmode (&imode, &ier);
if ( imode == 1 )
   {
   xytolatlon ( (float)i, (float)j, x, y);
   }
else if ( imode == 2 )
   {
   *x = ((float)i  - XMIN ) / (XMAX - XMIN);
   *y = 1.0 - ( (float)j - XMIN ) / (XMAX - XMIN);
   }
else
   {
   printf("no mode?\n");
   xytolatlon ( (float)i, (float)j, x, y);
   }
}

void dpa_coltbl ( int icbank, char *lutf )
{
int i, ier;
int icolrs[MAXCOLORS], ir[MAXCOLORS], ig[MAXCOLORS], ib[MAXCOLORS];

gplutf ( lutf, &icbank, &ier, strlen(lutf) );
}

int _i1, _j1, _i2, _j2;
float X[2], Y[2], Xv[2], Yv[2], rotat;
int ixoff, iyoff, np;
int imark, imkhw, imkwid;
float szmark;
char *cstr, sys_m[]="M", sys_n[]="N", sys_g[]="G";


int packet_code_2 ( unsigned char *buf )
{
int pcode, plen, i, ier;


  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);
  if (pcode != 2 )
    {
    printf("unexpected code %d, expecting 2\n",pcode);
    return(plen);
    }

  gscolr ( &graphic_color, &ier);

  _i1 = (int)read_short(buf+4);
  _j1 = (int)read_short(buf+6);
  ijtoxy ( _i1, _j1, &X[0], &Y[0]);
  rotat = 0.;
  ixoff = 0;
  iyoff = -1;
  cstr = (char *)(buf+8);

/* todo...handle 8 bit special characters */
  for(i=0;i<plen-4;i++)
      if ( (int)cstr[i] < 0 )
	printf("look packet_2 %d %d\n",i,cstr[i]);

  np = 1;
  gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
          strlen(sys_m),strlen(sys_n));

   /* for STI/MDA products which use symbols !, ", #, $, and % to
	mark storm position */
   if ( ( cstr[1] == ' ' ) && ( ( plen - 4 ) == 2 ) &&
	( cstr[0] >= '!' ) && ( cstr[0] <= '%' ) )
    {
    imkhw = 2;
    szmark = 1.0;
    imkwid = 2;
    switch ( (int)cstr[0] )
       {
       case '!':	
		imark = 17;
		szmark = .8;
		break;
       case '"':	
		imark = 16;
		break;
       case '#':	
		imark = 1;
		break;
       case '$':	
		imark = 20;
		szmark = 1.2;
		break;
       case '%':	
		imark = 5;
		szmark = 1.2;
		break;
       default:
                imark = 16;
       }
    gsmrkr  ( &imark, &imkhw, &szmark, &imkwid, &ier );
    gmark ( sys_n, &np, Xv, Yv, &ier, strlen(sys_n));
    }
  else
     gtext (sys_n, Xv, Yv, cstr, &rotat, &ixoff, &iyoff, &ier,
         strlen(sys_n),plen - 4);
  if(ier != 0) printf("gtext return %d\n",ier);

return(plen);
}

int packet_code_4 ( unsigned char *buf )
{
int pcode, plen, i, ier;
int poff, ilevel;
float drct,spd;

  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);
  if (pcode != 4 )
    {
    printf("unexpected code %d, expecting 4\n",pcode);
    return(plen);
    }

poff = 4;
for(i=0; i<plen; i+=10)
   {
   ilevel = (int)read_short(buf+poff+i); /* RMS */
   vad_setcol ( ilevel);

   _i1 = (int)read_short(buf+poff+i+2); /* X */
   _j1 = (int)read_short(buf+poff+i+4); /* Y */
   ijtoxy ( _i1, _j1, &X[0], &Y[0]);
   drct = (float)read_short(buf+poff+i+6); /* Direction */
   spd = (int)read_short(buf+poff+i+8); /* Sknt */

   np = 1;
   gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
           strlen(sys_m),strlen(sys_n));
   if ( wintyp == 'B' )
      gbarb ( sys_n, &np, Xv, Yv, &spd, &drct, &ier, strlen(sys_n));
   else
      garrw ( sys_n, &np, Xv, Yv, &spd, &drct, &ier, strlen(sys_n));
   }

return(plen);

}


int packet_code_6 ( unsigned char *buf )
{
int pcode, plen, i, ier;

  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);
  if (pcode != 6 )
    {
    printf("unexpected code %d, expecting 6\n",pcode);
    return(plen);
    }

  /*vad_setcol ( 15 );*/
  gscolr ( &line_color, &ier);

  _i1 = (int)read_short(buf+4);
  _j1 = (int)read_short(buf+6);
  ijtoxy ( _i1, _j1, &X[0], &Y[0]);

  for ( i=4; i<plen; i+=4 )
    {
    _i2 = (int)read_short(buf+4+i);
    _j2 = (int)read_short(buf+6+i);
    ijtoxy ( _i2, _j2, &X[1], &Y[1]);

    np = 2;
    gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
            strlen(sys_m),strlen(sys_n));
    gline ( sys_n, &np, Xv, Yv, &ier, strlen(sys_n));
    X[0] = X[1];
    Y[0] = Y[1];
    }

return(plen);
}

int packet_circle ( unsigned char *buf )
{
int pcode, plen, poff, i, mode, ier;

  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);
  /*vad_setcol(15);*/
  gscolr ( &line_color, &ier);

  poff = 4;
  for ( i=0; i<plen; i+=6 )
    {
    _i1 = (int)read_short(buf+poff+i);
    _j1 = (int)read_short(buf+poff+i+2);
    ijtoxy ( _i1, _j1, &X[0], &Y[0]);

    /* radius */
    _i2 = (int)read_short(buf+poff+i+4);
    ijtoxy ( _i1+_i2, _j1, &X[1], &Y[1]);

    np = 2;
    gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
            strlen(sys_m),strlen(sys_n));

    np = 20; /* use 20 points to create circle */
    gcircl ( sys_n, &Xv[0], &Yv[0], &Xv[1], &Yv[1], &np, &ier,
             strlen(sys_n) );
    }

return(plen);
}

int packet_code_25 ( unsigned char *buf )
{
return( packet_circle ( buf ) );
}

int packet_code_3 ( unsigned char *buf )
{
return( packet_circle ( buf ) );
}

int packet_code_11 ( unsigned char *buf )
{
return( packet_circle ( buf ) );
}

int packet_code_8 ( unsigned char *buf, int pcnt )
/* also packet_code_1 */
{
int pcode, plen, ilevel, poff, ier;
int i, maxstr, lenstr;

  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);

  if ( pcode == 8 )
     {
     ilevel = (int)read_short(buf+4); /* text color */
     vad_setcol ( ilevel);
     poff = 6;
     }
  else if ( pcode == 1 )
     {
     gscolr ( &graphic_color, &ier);
     poff = 4;
     }
  else
     {
     printf("unexpected packet code %d expecting 1 or 8\n",pcode);
     return(plen);
     }

  _i1 = (int)read_short(buf+poff);
  _j1 = (int)read_short(buf+poff+2);

  cstr = (char *)(buf+poff+4);
  maxstr = plen - ((int)cstr - (int)buf) + 4;

  if ( ( pcnt < 0 ) ||
	( ( _i1 == 0 ) && ( _j1 == 0 ) && ( maxstr > 80 ) ) )
     {
     /* this is an alpha or tabular page of text */
     for ( i = 0 ; i < maxstr; i += 80 )
	{
	lenstr = maxstr - i;
	if ( lenstr > 80 ) lenstr = 80;
        /*im_nids_text_output ( cstr+i, plen - ((int)cstr - (int)buf) + 4 );*/
        im_nids_text_output ( cstr+i, lenstr );
	}
     }
  else /* plot as graphics */
     {
     ijtoxy ( _i1, _j1 + pcnt*50, &X[0], &Y[0]);
     /*X[0] = ( (float)_i1 - XMIN ) / (XMAX - XMIN);
     Y[0] = 1.0 -  ( (float)_j1 + pcnt*50 - XMIN) / (XMAX - XMIN);*/
     rotat = 0.;
     ixoff = 0;
     iyoff = -1;
     np = 1;
     gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
     strlen(sys_m),strlen(sys_n));
     if ( maxstr > 256 ) maxstr = 256;
     gtext (sys_n, Xv, Yv, cstr, &rotat, &ixoff, &iyoff, &ier, strlen(sys_n),maxstr);
     if(ier != 0) printf("gtext return %d\n",ier);
     }

  return ( plen );

}

int packet_code_10 ( unsigned char *buf, int pcnt )
{
int plen, pcode, ier;
int ilevel, poff, i;

  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);

  if ( pcnt < 0 ) return(plen); /* not output page requested */

  if (pcode != 10 )
     {
     printf("unexpected packet code %d\n",pcode);
     return(plen);
     }

  ilevel = (int)read_short(buf+4);
  vad_setcol ( ilevel);

  poff = 6;

  for ( i=0; i < plen - 2; i+=8 )
    {
    _i1 = (int)read_short(buf+poff+i);
    _j1 = (int)read_short(buf+poff+i+2);
    _i2 = (int)read_short(buf+poff+i+4);
    _j2 = (int)read_short(buf+poff+i+6);
    ijtoxy ( _i1, _j1, &X[0], &Y[0]);
    ijtoxy ( _i2, _j2 + pcnt*50, &X[1], &Y[1]);

    np = 2;
    gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
            strlen(sys_m),strlen(sys_n));
    gline ( sys_n, &np, Xv, Yv, &ier, strlen(sys_n));
    }

  return ( plen );
}

int packet_code_12 ( unsigned char *buf )
{
int plen, pcode, ier;
int ilevel, poff, i;

  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);

  if (pcode != 12)
    {
    printf("unexpected packet code %d expecting 12\n", pcode);
    return(plen);
    }

  /*vad_setcol ( 12 );*/
  gscolr ( &graphic_color, &ier); 
  poff = 4;

  for ( i=0; i<plen; i+=4 )
    {
    _i1 = (int)read_short(buf+poff+i);
    _j1 = (int)read_short(buf+poff+i+2);
    ijtoxy ( _i1, _j1, &X[0], &Y[0]);

    np = 1;
    gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
            strlen(sys_m),strlen(sys_n));

    imark = 16;
    imkhw = 2;
    szmark = 1.0;
    imkwid = 2;
    gsmrkr  ( &imark, &imkhw, &szmark, &imkwid, &ier );
    gmark ( sys_n, &np, Xv, Yv, &ier, strlen(sys_n));
    }
  return ( plen );
}

int packet_code_13 ( unsigned char *buf )
{
int plen, pcode, ier;
int ilevel, poff, i;

  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);

  if (pcode != 13)
    {
    printf("unexpected packet code %d expecting 13\n", pcode);
    return(plen);
    }
  /* vad_setcol ( 13 ); */
  gscolr ( &graphic_color, &ier);
  poff = 4;

  for ( i=0; i<plen; i+=4 )
    {
    _i1 = (int)read_short(buf+poff+i);
    _j1 = (int)read_short(buf+poff+i+2);
    ijtoxy ( _i1, _j1, &X[0], &Y[0]);

    np = 1;
    gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
            strlen(sys_m),strlen(sys_n));
    imark = 18;
    imkhw = 2;
    szmark = 1.0;
    imkwid = 1;
    gsmrkr  ( &imark, &imkhw, &szmark, &imkwid, &ier );
    gmark ( sys_n, &np, Xv, Yv, &ier, strlen(sys_n));
    }

  return ( plen );
}

int packet_code_14 ( unsigned char *buf )
{
int plen, pcode, ier;
int ilevel, poff, i;

  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);

  if (pcode != 14)
    {
    printf("unexpected packet code %d expecting 14\n", pcode);
    return(plen);
    }
  /*vad_setcol ( 14 );*/
  gscolr ( &graphic_color, &ier);

  poff = 4;
  for ( i=0; i<plen; i+=4 )
    {
    _i1 = (int)read_short(buf+poff+i);
    _j1 = (int)read_short(buf+poff+i+2);
    ijtoxy ( _i1, _j1, &X[0], &Y[0]);

    np = 1;
    gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
            strlen(sys_m),strlen(sys_n));
    imark = 4;
    imkhw = 2;
    szmark = 1.0;
    imkwid = 2;
    gsmrkr  ( &imark, &imkhw, &szmark, &imkwid, &ier );
    gmark ( sys_n, &np, Xv, Yv, &ier, strlen(sys_n));
    }

  return ( plen );
}

int packet_code_15 ( unsigned char *buf )
/* Storm ID */
{
int plen, pcode, ier;
int ilevel, poff, i;

  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);

  if (pcode != 15)
    {
    printf("unexpected packet code %d expecting 15\n", pcode);
    return(plen);
    }
  /*vad_setcol ( 15 );*/
  gscolr ( &graphic_color, &ier);

  poff = 4;
  for ( i=0; i<plen; i+=6 )
    {
    _i1 = (int)read_short(buf+poff+i);
    _j1 = (int)read_short(buf+poff+i+2);
    ijtoxy ( _i1, _j1, &X[0], &Y[0]);

    rotat = 0.;
    ixoff = 0;
    iyoff = -1;
    cstr = (char *)(buf + poff + i + 4);

    np = 1;
    gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
            strlen(sys_m),strlen(sys_n));
    gtext (sys_n, Xv, Yv, cstr, &rotat, &ixoff, &iyoff, &ier,
           strlen(sys_n), 2);
    if(ier != 0) printf("gtext return %d packet 15\n",ier);
    }

  return ( plen );
}

int packet_code_16 ( unsigned char *buf )
/* Digital Radial Array */
{
int i, j, poff;
int nrle, lval;
int icbank = SatCid, ncolb, graphic_color; /* use satellite color bank */
float rstart, rdelta, dist;
float x[5], y[5], gx, gy, x1, y1, x2, y2;
int np, ier;
/*static char lutf[]="osf_ref16.tbl";*/
static char lutf[]="upc_radimg.tbl";
extern float radar_clat, radar_clon;


int pcode = (int)read_short(buf); /* must be 16 */
int ifrb = (int)read_short(buf+2);
int inrb = (int)read_short(buf+4);
int icenter = (int)read_short(buf+6);
int jcenter = (int)read_short(buf+8);
float rscale = (float)read_short(buf+10) / 1000.0;
int nrad = (int)read_short(buf+12);

ijtoxy ( icenter, jcenter, &X[0], &Y[0]);

/* get number of colors available */
gqclrs ( &icbank, &ncolb, &ier);
dpa_coltbl ( icbank, lutf );

printf(" first range bin %d numbin %d ic %d jc %d rscale %f nrad %d\n",
	ifrb, inrb, icenter, jcenter, rscale, nrad);

poff = 14;
for ( i=0; i < nrad; i++)
   {
   nrle = (int)read_short(buf+poff);
   if ( ifrb + nrle > inrb ) 
      {
      printf("packet 16, wrong number of range bin, first range bin\n");
      }
   rstart = (float)read_short(buf+poff+2) / 10.0; 
   rdelta = (float)read_short(buf+poff+4) / 10.0; 

   dist = (float)(ifrb + inrb) / ( rscale * 111.11 );
   
   x1 = dist * cos (radar_clat * DTR) * cos ( rstart * DTR );
   y1 = dist * sin ( rstart * DTR );
   x2 = dist * cos (radar_clat * DTR) * cos ( ( rstart + rdelta ) * DTR );
   y2 = dist * sin ( ( rstart + rdelta ) * DTR );
  
   np = 5; 

   printf("look nrle %d rstart %f rdelta %f\n",nrle,rstart,rdelta);
   /*printf("   ");*/
   for ( j = 0; j < nrle; j++ )
      {
      lval = (int)buf[poff+6+j];
      x[0] = X[0] + ( (float)(j + ifrb) / (float)(ifrb + inrb) ) * x1;
      y[0] = Y[0] + ( (float)(j + ifrb) / (float)(ifrb + inrb) ) * y1;
      x[1] = X[0] + ( (float)(j + 1 + ifrb) / (float)(ifrb + inrb) ) * x1;
      y[1] = Y[0] + ( (float)(j + 1 + ifrb) / (float)(ifrb + inrb) ) * y1;
      x[2] = X[0] + ( (float)(j + 1 + ifrb) / (float)(ifrb + inrb) ) * x2;
      y[2] = Y[0] + ( (float)(j + 1 + ifrb) / (float)(ifrb + inrb) ) * y2;
      x[3] = X[0] + ( (float)(j + ifrb) / (float)(ifrb + inrb) ) * x2;
      y[3] = Y[0] + ( (float)(j + ifrb) / (float)(ifrb + inrb) ) * y2;
      x[4] = x[0];
      y[4] = y[0];
      graphic_color = (int)((float)lval  * (float)ncolb / 255. ) + .5;
      gscolb ( &icbank, &graphic_color, &ier);
      gfill ( sys_m, &np, x, y, &ier, strlen(sys_m));
      /*printf("level %d %d   ",j,lval);
      if ( (j + 1)%10 == 0 ) printf("\n   ");*/
      }
   /*printf("\n");*/

   poff = poff + 6 + nrle;   
   }

return(poff);

}

int packet_code_17 ( unsigned char *buf )
/* DPA */
{
int icbank = SatCid, ncolb; /* use satellite color bank */
int plen, ier;
int ilevel, poff, i, j;
int run, np;
float gx, gy, x[5], y[5];
int ulx,uly,irun;

int pcode = (int)read_short(buf);
int nboxes = (int)read_short(buf+6);  /* should be 131 */
int nrow = (int)read_short(buf+8);    /* should be 131 */
static char lutf[]="upc_n1p.tbl";

/* get grid row/col of radar */
radar_ij ( &gx, &gy );
ulx = (int)floor(gx) - ( nboxes/2);
uly = (int)floor(gy) + ( nrow/2);

/*printf(" DPA code %d %d %d\n",pcode,nboxes,nrow);*/

/* get number of colors available */
gqclrs ( &icbank, &ncolb, &ier);
dpa_coltbl ( icbank, lutf );

poff = 10;
for ( i = 0; i < nrow; i++ )
   {
   plen = (int)read_short(buf+poff);
   /* printf("look nbytes %d %d poff %d\n",i,plen,poff); */
   irun = 0;
   for ( j=0; j<plen; j+=2 )
      {
      run = (int)buf[poff+2+j];
      ilevel = (int)buf[poff+3+j];
      x[0] = ulx + irun;
      x[1] = x[0] + run;
      x[2] = x[1];
      x[3] = x[0];
      x[4] = x[0];
      y[0] = uly - i;
      y[1] = y[0];
      y[2] = y[0] - 1;
      y[3] = y[2];
      y[4] = y[0];
      np = 5;
      switch ( ilevel )
	 {
	 case 255:
		graphic_color = 0;
		break;
	 default:
		graphic_color = (int)((float)ilevel  * (float)ncolb / 255. ) + .5;
	 }
      /* printf("   run %d level %d x0 %f y0 %f\n",run,ilevel,x[0],y[0]); */
      gscolb ( &icbank, &graphic_color, &ier);
      gfill ( sys_g, &np, x, y, &ier, strlen(sys_g));
      irun += run;
      }
   if(plen < 2) return(poff);
   poff += plen + 2;
   }

return ( poff );

}

int packet_code_18 ( unsigned char *buf )
/* DPA precipitation rate */
{
int plen, ier;
int ilevel, ilevel1, poff, i, j;
int run, run1, irun;
int ulx, uly;
float gx, gy, x[5], y[5];
int icbank = SatCid, ncolb;

int pcode = (int)read_short(buf);
int nboxes = (int)read_short(buf+6); /* should be 13 */
int nrow = (int)read_short(buf+8); /* should be 13 */
static char lutf[]="osf_ref16.tbl";

/* get grid row/col of radar */
radar_ij ( &gx, &gy );
ulx = (int)floor(gx) - ( nboxes/2) * 10;
uly = (int)floor(gy) + ( nrow/2) * 10;
/*printf(" DPA code %d %d %d\n",pcode,nboxes,nrow);*/

/* get number of colors available */
gqclrs ( &icbank, &ncolb, &ier);
/*dpa_coltbl ( icbank, lutf ); don't plot these for now */

poff = 10;
for ( i = 0; i < nrow; i++ )
   {
   plen = (int)read_short(buf+poff);
   /*printf("look nbytes %d %d poff %d\n",i,plen,poff);*/
   irun = 0;
   for ( j=0; j<plen; j+=2 )
      {
      run = (int)(buf[poff+2+j] >> 4);
      ilevel = (int)(buf[poff+2+j] & 0xf);
      x[0] = ulx + irun;
      x[1] = x[0] + run * 10;
      x[2] = x[1];
      x[3] = x[0];
      x[4] = x[0];
      y[0] = uly - i * 10;
      y[1] = y[0];
      y[2] = y[0] - 1 * 10;
      y[3] = y[2];
      y[4] = y[0];
      np = 5;
      switch ( ilevel )
	 {
	 case 255:
		graphic_color = 0;
		break;
	 default:
		graphic_color = (int)((float)ilevel  * (float)ncolb / 15. ) + .5;
	 }
      /*gscolb ( &icbank, &graphic_color, &ier);
      gfill ( sys_g, &np, x, y, &ier, strlen(sys_g));*/

      irun += run * 10;

      run1 = (int)(buf[poff+3+j] >> 4); /* possibly bits 0000 for last run1 */
      ilevel1 = (int)(buf[poff+3+j] & 0xf);
      if ( run1 > 0 )
         {
         x[0] = ulx + irun;
         x[1] = x[0] + run1 * 10;
         x[2] = x[1];
         x[3] = x[0];
         x[4] = x[0];
         y[0] = uly - i * 10;
         y[1] = y[0];
         y[2] = y[0] - 1 * 10;
         y[3] = y[2];
         y[4] = y[0];
         np = 5;
         switch ( ilevel1 )
	    {
	    case 255:
		   graphic_color = 0;
		   break;
	    default:
		   graphic_color = (int)((float)ilevel1  * (float)ncolb / 15. ) + .5;
	    }
         /*gscolb ( &icbank, &graphic_color, &ier);
         gfill ( sys_g, &np, x, y, &ier, strlen(sys_g));*/

	 irun += run1 * 10;
	 }

      /*printf("   run %d level %d run1 %d level1 %d\n",run,ilevel,
      		run1, ilevel1);*/
      }
   if(plen < 2) return(poff);
   poff += plen + 2;
   }

return ( poff );

}

int packet_code_19 ( unsigned char *buf )
/* Hail Detection Algorithm */
{
int plen, pcode, ier;
int ilevel, poff, i;

  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);

  if (pcode != 19)
    {
    printf("unexpected packet code %d expecting 19\n", pcode);
    return(plen);
    }
  /*vad_setcol ( 14 );*/
  gscolr ( &graphic_color, &ier);

  poff = 4;
  for ( i=0; i<plen; i+=10 )
    {
    _i1 = (int)read_short(buf+poff+i);
    _j1 = (int)read_short(buf+poff+i+2);
    ijtoxy ( _i1, _j1, &X[0], &Y[0]);

    np = 1;
    gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
            strlen(sys_m),strlen(sys_n));

    /* todo, interpret prob. hail (i + 4), prob severe hail (i+6) */
    _i2 = (int)read_short(buf+poff+i+8);

    imkhw = 2;
    imkwid = 2;
    if ( _i2 > 0 )
       {
       imark = 18;
       szmark = (float)_i2;
       }
    else
       {
       imark = 4;
       szmark = (float).5;
       }
    gsmrkr  ( &imark, &imkhw, &szmark, &imkwid, &ier );
    gmark ( sys_n, &np, Xv, Yv, &ier, strlen(sys_n));
    }
  return ( plen );
}

int packet_code_20 ( unsigned char *buf )
{
/* Point feature type/attribute
   1 = mesocyclone, extrapolated
   2 = 3-D correlated shear, extrapolated
   3 = mesocyclone, persistent, new, increasing
   4 = 3-D correlated shear, persistent, new, increasing
   5 = TVS, extrapolated
   6 = ETVS, extrapolated
   7 = TVS, persistent, new, increasing
   8 = ETVS, persistent, new, increasing
   9 = MDA Circulation with Strength Rank >=5, Base <= 1km
  10 = MDA Circulation with Strength Rank >=5, Base > 1km
  11 = MDA Circulation with Strength Rank < 5
*/
int plen, pcode, ier;
int itype, poff, i, icol, iltyp, ilwid, ilthw, ilwhw;
float wtcod;

  pcode = (int)read_short(buf);
  plen = (int)read_short(buf+2);

  if (pcode != 20)
    {
    printf("unexpected packet code %d expecting 20\n", pcode);
    return(plen);
    }
  gscolr ( &graphic_color, &ier);

  poff = 4;
  for ( i=0; i<plen; i+=8 )
    {
    _i1 = (int)read_short(buf+poff+i);
    _j1 = (int)read_short(buf+poff+i+2);
    ijtoxy ( _i1, _j1, &X[0], &Y[0]);

    itype = (int)read_short(buf+poff+i+4);
    _i2 = (int)read_short(buf+poff+i+6);
    /*printf("point feature type %d %d\n",itype,_i2);*/

    if ( ( itype >= 5 ) &&  ( itype <=8 ) ) 
        {
        /* TVS */
        wtcod = 19;
        szmark = (float)1.5;
        imkwid = 2;
        ixoff = 0;
        iyoff = -1;

        np = 1;
        gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
            strlen(sys_m),strlen(sys_n));

        gswthr  ( &szmark, &imkwid, &ier );
        gwthr ( sys_n, &np, &wtcod, Xv, Yv, 
	    &ixoff, &iyoff, &ier, strlen(sys_n));
        }
    else
        {
		line_color, line_type, _imnids_line_width;
	icol = 5;
	gscolr ( &icol, &ier);

        iltyp = 1;
	switch ( itype ) {
            case 1:
            case 3:
            case 9:
            case 10:
	           ilwid = 4;
		   break;
            case 2:
            case 4:
            case 11:
	           ilwid = 4;
		   break;
	    default:
                   printf("unknown (packet %d) point feature type %d %d\n",pcode,itype,_i2);
		   ilwid = 1;
		   
        }
	ilthw = 2; ilwhw = 2;
	gsline ( &iltyp, &ilthw, &ilwid, &ilwhw, &ier);
        /* radius */
        ijtoxy ( _i1+_i2, _j1, &X[1], &Y[1]);

        np = 2;
        gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
            strlen(sys_m),strlen(sys_n));

        np = 20; /* use 20 points to create circle */
        gcircl ( sys_n, &Xv[0], &Yv[0], &Xv[1], &Yv[1], &np, &ier,
             strlen(sys_n) );
        }


    }
  return ( plen );
}

int packet_code_22 ( unsigned char *buf )
{
int pval, plen, poff, pnext,nvol,tptr;
int vtimes[10], i, j;
if ( (int)read_short(buf) != 22 )
   {
   printf("packet code not 22\n");
   return(0);
   }
poff = 2;
/* docs don't match data....no length packet found here 
plen = (int)read_short(buf+poff);
printf("cell trend length %d\n",plen);
poff+=2; */

/*printf("nvols %d latestptr %d\n",(int)buf[poff],(int)buf[poff+1]);*/
nvol = (int)buf[poff]; poff++;
tptr = (int)buf[poff]; poff++;

if ( nvol > 10 )
   {
   printf("packet_22 nvols > 10 error\n");
   return(0);
   }

/* fill in array starting a greater than the tptr-1 position of the latest */
j = nvol - tptr;

for ( i=0; i<nvol;i++)
   {
   vtimes[j] = (int)read_short(buf+poff+(i*2));
   j++;
   if ( j == nvol ) j = 0;
   }

for ( i=0; i<nvol;i++)
   {
   printf("Trend volume time %d %02d%02dZ\n",
      i+1,vtimes[i]/60,vtimes[i]%60);
   }
printf("\n");


poff = poff + 2*nvol;

pval = (int)read_short(buf+poff);
while ( pval == 21)
   {
   pnext = packet_code_21 ( buf+poff, vtimes, nvol );
   if ( pnext == 0 ) break;

   poff = poff + pnext + 4;
   pval = (int)read_short(buf+poff);
   }

/* substitute calculation of plen */
plen = poff;
return(plen);

}

int packet_code_21 ( unsigned char *buf, int vtimes[], int ntimes )
{
int i, j, plen, poff, pval;
int ipos, jpos, trendcode, nvol, tptr, vtrend[10][8];
char cellid[3];
static char *trendstr[8]={"CellTop", "CellBase", 
			  "MaxRefHt", "ProbHail", "ProbSvrH",
			  "CellVIL", "MaxRef", "CentrdHt"};

if ( (int)read_short(buf) != 21  )
   {
   printf("packet code not 21\n");
   return(0);
   }

poff = 2;
plen = (int)read_short(buf+poff);
poff+=2;
cellid[0] = buf[poff]; poff++;
cellid[1] = buf[poff]; poff++;
cellid[2] = '\0';
ipos = (int)read_short(buf+poff); poff +=2;
jpos = (int)read_short(buf+poff); poff +=2;
printf("Cell Trend Data for %s at [%d,%d]\n",cellid,ipos,jpos);

for ( j=0; j<10; j++)
   for ( i=0; i<8; i++)
      vtrend[j][i] = -999;

while ( poff < plen )
   {
   trendcode = (int)read_short(buf+poff); poff +=2;
   if ( ( trendcode < 1 ) || ( trendcode > 8 ) )
      trendcode = -1;
   nvol = buf[poff]; poff++;
   tptr = buf[poff]; poff++;
   /*printf("   %s\n",trendstr[trendcode-1]);*/
   j = nvol - tptr;
   for ( i=0; i<nvol; i++)
      {
      pval = (int)read_short(buf+poff); poff +=2;
      if ( trendcode != -1 )
         vtrend[j][trendcode-1] = pval;
      /*printf("     i %d j %d tptr %d pval %d ",i,j,tptr,pval);
      if ( i + 1 == tptr ) printf(" <<--latest");*/
      j++;
      if ( j == nvol ) j = 0;
      /*printf("\n");*/
      }
   }


printf("Time  ");
for ( i=0; i<8; i++ )
   printf("%9s",trendstr[i]);
printf("\n");


for ( j=0; j < nvol; j++ )
   {
   int imin;
   char chflag;

   imin = vtimes[ntimes - nvol + j];
   printf("%02d%02dZ ",imin/60, imin%60);
   for ( i=0; i<8; i++ )
      switch ( i )
         {
         case 0: /* scale /100, +1000 indicates TOP (BASE) detected at highest (lowest) scan */
	 case 1:
	    if ( i == 0 ) 
               chflag = '+';
            else 
               chflag = '-';
	    if ( vtrend[j][i] > 700 )
	       printf("%8d%c", (vtrend[j][i] - 1000)*100,chflag);
            else
               printf("%9d",vtrend[j][i]*100);
            break;
	 case 2:
	 case 7:
               printf("%9d",vtrend[j][i]*100);
	       break;
	 default:
            printf("%9d",vtrend[j][i]);
         }
   printf("\n");
   }
printf("\n");

return(plen);

}
