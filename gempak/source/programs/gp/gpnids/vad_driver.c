#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define vwind	vwind_
#define vad_colors	vad_colors_
#define vad_line	vad_line_
#define vad_color_init	vad_color_init_
#define vad_rms_colors	vad_rms_colors_
#define vad_garea	vad_garea_
#define vad_imcbar	vad_imcbar_
#define in_wind	in_wind_
#define gsmode gsmode_
#define gsgmgn	gsgmgn_
#endif

#define MAX_X	2047.0
#define MIN_X	-2048.0

#define MAX_PX	511.
#define MIN_PX  0.

float	XMAX=MAX_X, XMIN=MIN_X;
float	radar_clat, radar_clon;
int graphic_color=1;
int line_color, line_type, line_width;

/*
 * Default vad colors as specified by OFCM FMH No. 11
 */ 

int vadcols[]={3, 5, 2, 6, 7, 1, 23, 31, 1, 1, 1, 1, 1, 1, 1, 6};

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

void vad_colors ( int *icolor, int *ier)
{
*ier = 0;
graphic_color = *icolor;
}

void vad_line ( int *icolor, int *itype, int *iwidth, int *ier)
{
*ier = 0;
line_color = *icolor;
line_type = *itype;
line_width = *iwidth;
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
if ( ( ival < 0 )||(ival > 15) )
   {
   printf("unexpected VAD color index %d\n",ival);
   ival = 15;
   }
gscolr ( &vadcols[ival], &ier);
}

char isub_garea[LLMXLN];

void vad_garea ( char *garea, int *lenstr, int *ier)
{
strncpy(isub_garea, garea, *lenstr);
isub_garea[*lenstr] = '\0';
cst_lcuc ( isub_garea, isub_garea, ier );
return;
}

char image_imcbar[LLMXLN];

void vad_imcbar ( char *imcbar, int *lenstr, int *ier)
{
strncpy(image_imcbar, imcbar, *lenstr);
image_imcbar[*lenstr] = '\0';
*ier = 0;
return;
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

void vad_driver ( unsigned char *buf, int *ioff, int *iret)
{
int bflip, i, ij, ier;
signed short *sbytes,*s1,*s2;
int *ibytes;
int ival;
off_t k;

int block_len, nlayers, ilevel;
int block_offs[3];
int packet_code, pblen, poff;
int icnt, boff;
int product_message_code;

int np, i1,i2,j1,j2, ixoff, iyoff;
float X[2], Y[2], Xv[2], Yv[2], spd, drct, rotat, spcod[2];
float xmin_sav, xmax_sav;
int imark, imkhw, imkwid;
float szmark;
char sys_m[]="M", sys_n[]="N", *cstr;
float rmargin[]={4.0, 4.0, 4.0, 2.0};


*iret = 0;

/*
** Message header block
 */
product_message_code = (int)read_short(buf);
switch (product_message_code)
   {
   case 48:
        i = 2;
	gsmode ( &i, &ier);
        /* set graph margins */
        gsgmgn ( &rmargin[0], &rmargin[1], &rmargin[2], &rmargin[3], &ier );
	XMAX = MAX_PX;
	XMIN = MIN_PX;
	break;
   default:
	XMAX = MAX_X;
	XMIN = MIN_X;
   }

radar_clat = (float)read_int (buf + 20) / 1000.;
radar_clon = (float)read_int (buf + 24) / 1000.;

#ifdef DEBUG
printf("look messagecode %d %f %f\n",product_message_code,XMAX,XMIN);
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

block_offs[0] = read_int ( buf + 108 );
block_offs[1] = read_int ( buf + 112 );
block_offs[2] = read_int ( buf + 116 );

for ( i = 0; i < 3; i++ )
   if ( block_offs[i] != 0 )
      {
      /*printf("look block_offs %d  block1 %d block2 %d\n", i,
         (int)read_short(buf + (block_offs[i] * 2) + 0),
         (int)read_short(buf + (block_offs[i] * 2) + 2) );*/
      switch ( (int)read_short(buf + (block_offs[i] * 2) + 2) )
         {
	 case 1:
	        decode_symbol ( buf, block_offs[i] );
		break;
	 case 2:
		xmin_sav = XMIN; xmax_sav = XMAX;
		XMIN = MIN_PX; XMAX = MAX_PX;
	        decode_alpha_graphic ( buf + (block_offs[i] * 2) );
		XMIN = xmin_sav; XMAX = xmax_sav;
		break;
	 case 3:
	        decode_tabular ( buf + (block_offs[i] * 2) );
		break;
	 default:
		printf("unknown block to decode %d\n",
			(int)read_short(buf + (block_offs[i] * 2) + 2) );
         }
      }


switch (product_message_code)
   {
   case 48:
	   i = 1;
	   gsmode (&i, &ier);
	   break;
   /*default:
	   printf("stay in map mode\n");*/
   }


}


