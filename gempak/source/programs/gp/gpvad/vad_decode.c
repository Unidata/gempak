#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define vwind	vwind_
#define vad_color_init	vad_color_init_
#define vad_rms_colors	vad_rms_colors_
#define in_wind	in_wind_
#endif

/*
 * Default vad colors as specified by OFCM FMH No. 11
 */ 

int vadcols[]={3, 5, 2, 6, 7, 23, 31, 1, 1, 1, 1, 1, 1, 1, 1, 6};

void vad_color_init ()
{
int ier, bsize=LLMXLN, inum;
char buf[LLMXLN], *cpos;
FILE *fp;

fp = cfl_tbop ( "gpvad.config", "unidata", &ier );
if (fp == NULL) return;

cfl_trln ( fp, bsize, buf, &ier);
while ( ier == 0 )
   {
   if ( strncmp(buf,"COLORS",6) == 0)
      {
      cpos = strchr(buf, ':');
      if ( cpos != NULL )
         {
	 cpos++;
	 cst_ilst ( cpos, ':', 1, 16, vadcols, &inum, &ier);
	 cfl_clos ( fp, &ier );
         return;
         }
      else
	 printf("Invalid color configuration line %s\n",buf);
      }
   cfl_trln ( fp, bsize, buf, &ier);
   }

cfl_clos ( fp, &ier );

}

void vad_rms_colors ( int *NFLVL, int ifcolr[] )
{
int i;
for ( i = 0; i < *NFLVL; i++ )
   ifcolr[i] = vadcols[i];
}

int vad_setcol ( int ival)
{
int ier;
if ( ( ival < 1 )||(ival > 16) )
   {
   printf("unexpected VAD color index %d\n",ival);
   ival = 16;
   }
gscolr ( &vadcols[ival-1], &ier);
}

char wintyp, winuni;
int winclr;

int vwind ( char *wind, int *lens, int *ier)
{
in_wind ( wind, &wintyp, &winuni, &winclr, ier, *lens, sizeof(wintyp), sizeof(winuni));
}

int read_int ( unsigned char *x )
{
int ival = ( ( ( ( ( x[0] << 8 ) | x[1] ) << 8 ) | x[2] ) << 8 ) | x[3];
return (ival);
}

short read_short ( unsigned char *x )
{
short ival = ( x[0] << 8 ) | x[1];
return (ival);
}

void vad_decode ( unsigned char *buf, int *ioff, int *iret)
{
int bflip, i, ij, ier;
signed short *sbytes,*s1,*s2;
int *ibytes;
int ival;
off_t k;

int symbol_off, block_len, nlayers, ilevel;
int packet_code, pblen, poff;
int icnt, boff;

int np, i1,i2,j1,j2, ixoff, iyoff;
float X[2], Y[2], Xv[2], Yv[2], spd, drct, rotat, spcod[2];
int imark, imkhw, imkwid;
float szmark;
char sys_m[]="M", sys_n[]="N", *cstr;


*iret = 0;

/*
** Message header block
 */
#ifdef DEBUG
k = 0;
for(i=0;i<9;i++)
   {
   sbytes = (signed short *)(buf+k);
   printf("sbytes %d %d\n",i+1,(int)(*sbytes));
   k = k + 2;
   }
/*
** Product Description block
 */
for(i=0;i<51;i++)
   {
   sbytes = (signed short *)(buf+k);
   printf("sbytes %d %d %d\n",i+10,(int)k,(int)(*sbytes));
   k = k + 2;
   }
#endif

symbol_off = read_int ( buf + 108 );


k = symbol_off;
block_len = read_int (buf + (symbol_off * 2) + 4);

nlayers = (int)read_short (buf + (symbol_off * 2) + 8);

#ifdef DEBUG
printf("symbol_off %d block_len %d\n",symbol_off,block_len);
printf("nlayers %d\n",nlayers);

for(i=0;i<block_len;i+=2)
   {
   sbytes = (signed short *)(buf+(symbol_off*2)+i);
   printf("sbytes %d %d\n",(symbol_off*2)+i,(int)(*sbytes));
   }
#endif

k = symbol_off*2 + 10;
for(i=0;i<nlayers;i++)
   {
   if ( (int)read_short(buf+k) != -1 )
      {
      printf("Expecting -1, not found\n");
      break;
      }

   ival = read_int(buf + k + 2 );
   icnt = 0;
   while ( icnt < ival ) /*block_len )*/
      {
      boff = k + icnt;
      /*printf("look icnt %d layer_length %d boff %d\n",icnt,ival,boff);*/
      packet_code = (int)read_short(buf + boff + 6 );
      switch (packet_code)
         {
	 case 10:
		pblen = (int)read_short(buf+boff+8);
		ilevel = (int)read_short(buf+boff+10);
		poff = 4;
		vad_setcol ( ilevel);
		while( poff < pblen)
		   {
		   i1 = (int)read_short(buf+boff+8+poff);
		   j1 = (int)read_short(buf+boff+8+poff+2);
		   i2 = (int)read_short(buf+boff+8+poff+4);
		   j2 = (int)read_short(buf+boff+8+poff+6);
		   /*printf("look i1 %d j1 %d i2 %d j2 %d\n",
			i1,j1,i2,j2);*/
		   X[0] = (float)i1 / 511.;
		   X[1] = (float)i2 / 511.;
		   Y[0] = 1.0 - (float)j1 / 511.;
		   Y[1] = 1.0 - (float)j2 / 511.;
		   np = 2;
		   gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
			strlen(sys_m),strlen(sys_n));
		   gline ( sys_n, &np, Xv, Yv, &ier, strlen(sys_n));
		   poff += 8;
		   }
		break;
         case 3: /* mesocyclone info */
         case 11: /* mesocyclone info */
		printf("packet code %d (mesocyclone) found\n",packet_code);
		pblen = (int)read_short(buf+boff+8);
		vad_setcol ( 15 );
		poff = 2;
		i1 = (int)read_short(buf+boff+8+poff); /* X */
		j1 = (int)read_short(buf+boff+8+poff+2); /* Y */
		X[0] = (float)i1 / 511.;
		Y[0] =  1.0 - (float)j1 / 511.;
		/* radius */
		i2 = (int)read_short(buf+boff+8+poff+4); /* radius */
		X[1] = (float)(i1+i2) / 511.;
		Y[1] = Y[0];
		np = 2;
		gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
			strlen(sys_m),strlen(sys_n));

		np = 20; /* use 20 points to create circle */
		gcircl ( sys_n, &Xv[0], &Yv[0], &Xv[1], &Yv[1], &np, &ier,
		   strlen(sys_n) );
		break;
	 case 12: /* TVS */
		printf("packet code %d (TVS) found\n",packet_code);
		pblen = (int)read_short(buf+boff+8);
		vad_setcol ( 12 );
		poff = 2;
		i1 = (int)read_short(buf+boff+8+poff); /* X */
		j1 = (int)read_short(buf+boff+8+poff+2); /* Y */
		X[0] = (float)i1 / 511.;
		Y[0] =  1.0 - (float)j1 / 511.;
		np = 1;
		gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
			strlen(sys_m),strlen(sys_n));
		imark = 16;
		imkhw = 2;
		szmark = 1.0;
		imkwid = 2;
		gsmrkr  ( &imark, &imkhw, &szmark, &imkwid, &ier );
		gmark ( sys_n, &np, Xv, Yv, &ier, strlen(sys_n));
		break;
	 case 13: /* Hail positive */
		printf("packet code %d (Hail) found\n",packet_code);
		pblen = (int)read_short(buf+boff+8);
		vad_setcol ( 13 );
		poff = 2;
		i1 = (int)read_short(buf+boff+8+poff); /* X */
		j1 = (int)read_short(buf+boff+8+poff+2); /* Y */
		X[0] = (float)i1 / 511.;
		Y[0] =  1.0 - (float)j1 / 511.;
		np = 1;
		gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
			strlen(sys_m),strlen(sys_n));
		imark = 18;
		imkhw = 2;
		szmark = 1.0;
		imkwid = 1;
		gsmrkr  ( &imark, &imkhw, &szmark, &imkwid, &ier );
		gmark ( sys_n, &np, Xv, Yv, &ier, strlen(sys_n));
		break;
	 case 14: /* Hail Probable */
		printf("packet code %d (Hail) found\n",packet_code);
		pblen = (int)read_short(buf+boff+8);
		vad_setcol ( 14 );
		poff = 2;
		i1 = (int)read_short(buf+boff+8+poff); /* X */
		j1 = (int)read_short(buf+boff+8+poff+2); /* Y */
		X[0] = (float)i1 / 511.;
		Y[0] =  1.0 - (float)j1 / 511.;
		np = 1;
		gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
			strlen(sys_m),strlen(sys_n));
		imark = 4;
		imkhw = 2;
		szmark = 1.0;
		imkwid = 2;
		gsmrkr  ( &imark, &imkhw, &szmark, &imkwid, &ier );
		gmark ( sys_n, &np, Xv, Yv, &ier, strlen(sys_n));
		break;
         case 4:
		pblen = (int)read_short(buf+boff+8);
		ilevel = (int)read_short(buf+boff+10); /* RMS */
		vad_setcol ( ilevel);
		/*printf("packet code 4 found // len %d level %d\n",pblen,ilevel);*/
		poff = 4;
		i1 = (int)read_short(buf+boff+8+poff); /* X */
		j1 = (int)read_short(buf+boff+8+poff+2); /* Y */
		i2 = (int)read_short(buf+boff+8+poff+4); /* Direction */
		j2 = (int)read_short(buf+boff+8+poff+6); /* Sknt */
		/*printf("vector %d %d Drct %d Sknt %d\n",i1,j1,i2,j2);*/
		X[0] = (float)i1 / 511.;
		Y[0] =  1.0 - (float)j1 / 511.;
		drct = (float)i2;
		spd = (float)j2;
		np = 1;
		gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
			strlen(sys_m),strlen(sys_n));
		if ( wintyp == 'B' )
		   gbarb ( sys_n, &np, Xv, Yv, &spd, &drct, &ier, strlen(sys_n));
		else
		   garrw ( sys_n, &np, Xv, Yv, &spd, &drct, &ier, strlen(sys_n));
		break;
	 case 1:
	 case 8: /* text string */
		pblen = (int)read_short(buf+boff+8);
		if (packet_code == 8 )
		   {
		   ilevel = (int)read_short(buf+boff+10); /* text color */
		   vad_setcol ( ilevel);
		   poff = 4;
		   }
		else
		   {
		   poff = 2;
		   /* temporary... ilevel */
		   vad_setcol ( 15 );
		   }
		i1 = (int)read_short(buf+boff+8+poff);
		j1 = (int)read_short(buf+boff+8+poff+2);
		X[0] = (float)i1 / 511.;
		Y[0] = 1.0 - (float)j1 / 511.;
		rotat = 0.;
		ixoff = 0;
		iyoff = -1;
		cstr = buf+boff+8+poff+4;
		np = 1;
		gtrans (sys_m, sys_n, &np, X, Y, Xv, Yv, &ier,
			strlen(sys_m),strlen(sys_n));
		gtext (sys_n, Xv, Yv, cstr, &rotat, &ixoff, &iyoff, &ier,
			strlen(sys_n),pblen - 6);
		if(ier != 0) printf("gtext return %d\n",ier);
		break;
	 default:
		printf("packet code %d not handled\n",packet_code);
#ifdef DEBUG
		for(ij=icnt;ij<ival;ij+=2)
		   {
		   printf("look vals %d %hd\n",k + ij + 6, (int)read_short(buf + k + ij + 6 ));
		   }
#endif
		pblen = (int)read_short(buf+boff+8);
         }
      icnt = icnt + pblen + 4;
      /*printf("packet code %d look k %d %d %d icnt  %d blocklen %d boff %d\n",
		packet_code,k,ival,k+ival,icnt,pblen,boff);*/
      }
   k = k + ival;
   }

}


