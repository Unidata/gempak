#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define vwind	vwind_
#define vad_color_init	vad_color_init_
#define vad_rms_colors	vad_rms_colors_
#define in_wind	in_wind_
#define im_isub im_isub_
#define im_cbar	im_cbar_
#define gqmode	gqmode_
#endif

extern float XMAX, XMIN;
extern float radar_clat, radar_clon;
extern char *stashfilnam;
extern char isub_garea[LLMXLN];
extern char image_imcbar[LLMXLN];

char wintyp, winuni;
int winclr;

int vwind ( char *wind, int *lens, int *ier)
{
in_wind ( wind, &wintyp, &winuni, &winclr, ier, *lens, sizeof(wintyp), sizeof(winuni));
}



void decode_symbol ( unsigned char *buf, int symbol_off)
{
int i, ij, ier;
signed short *sbytes,*s1,*s2;
int *ibytes;
int ival;
off_t k;
/*int symbol_off = 0;*/

int block_len, nlayers, ilevel;
int graphic_off, tabular_off, block_offs[3];
int packet_code, pblen, poff;
int pcode,plen;
int icnt, boff;

int mode, np, i1,i2,j1,j2, ixoff, iyoff;
float X[2], Y[2], Xv[2], Yv[2], spd, drct, rotat, spcod[2];
int imark, imkhw, imkwid;
float szmark;
char *cstr;
static char sys_m[]="M", sys_n[]="N", radprj[]="RAD";


k = symbol_off*2;

{
char proj[9], garea[40], nullfile[1];
float angl1, angl2, angl3;
float dlatll, dlonll, dlatur, dlonur;

/* see if we are in map or graph mode */

gqmode ( &mode, &ier );

if ( mode == 1 )
   gqmprj ( proj, &angl1, &angl2, &angl3, &dlatll, &dlonll,
     &dlatur, &dlonur, &ier, sizeof(proj));

/*if ( ( mode == 1 ) && ( ier != 0 ) )*/
   {
   /* if no projection is set for map mode, we set a minimal
   projection in case a raster or radial image isn't specified. */
   sprintf(proj, "CED\0");
   if ( strcmp ( isub_garea, "DSET" ) == 0 )
      {
      dlonll = radar_clon - 2;
      dlonur = radar_clon + 2;
      dlatll = radar_clat - 2;
      dlatur = radar_clon + 2;
      sprintf(garea,"#%4.4f;%4.4f;4;4\0",radar_clat,radar_clon);
      }
   else
      strcpy ( garea, isub_garea);
   gg_maps ( proj, garea, nullfile, &i, &ier,
	  strlen(proj), strlen(garea), sizeof(nullfile) );
   if ( ier != 0 )
      printf("could not set minimal proj %d garea %s\n",ier,garea);
   }

}

/*
values must be block=1, block divider=-1
printf("look vad_symbol block %d %d\n",
   (int)read_short (buf+k),
   (int)read_short (buf+k+2) );*/

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
		packet_code_10 ( buf+boff+6, 0);
		pblen = (int)read_short(buf+boff+8);
		break;
         case 3: /* mesocyclone info */
         case 11: /* mesocyclone info */
         case 25: /* STI circle */
		/*printf("packet code %d (mesocyclone) found len %d\n",packet_code,pblen);*/
		pblen = packet_code_25 ( buf+boff+6);
		break;
	 case 12: /* TVS */
		/*printf("packet code %d (TVS) found\n",packet_code);*/
                pblen = packet_code_12 ( buf+boff+6 );
		break;
	 case 13: /* Hail positive */
		printf("packet code %d (Hail) found\n",packet_code);
                pblen = packet_code_13 ( buf+boff+6 );
		break;
	 case 14: /* Hail Probable */
		printf("packet code %d (Hail) found\n",packet_code);
                pblen = packet_code_14 ( buf+boff+6 );
		break;
         case 19:
		/*printf("packet code %d (Hail) found\n",packet_code);*/
		pblen = packet_code_19 ( buf+boff+6 );
		break;
         case 4:
		pblen = packet_code_4 ( buf+boff+6);
		break;
	 case 1:
	 case 8: /* text string */
		pblen = packet_code_8 ( buf+boff+6, 0 );
		break;
	 case 15: /* storm ID */
		pblen = packet_code_15 ( buf+boff+6 );
		break;
         case 23: /* SCIT past */
         case 24: /* SCIT forecast */
		pblen = (int)read_short(buf+boff+8);
		/* data packets are 2, 6, or 25 */
		poff = 2;
		while ( poff < pblen )
		   {
		   pcode = (int)read_short(buf+boff+8+poff);
		   plen = (int)read_short(buf+boff+8+poff+2);
		   switch ( pcode )
		      {
		      case 2:
				packet_code_2 ( buf+boff+8+poff);
				break;	
		      case 6:
				packet_code_6 ( buf+boff+8+poff);
				break;
		      case 25:
				packet_code_25 ( buf+boff+8+poff);
				break;
		      default:
			   printf("unknown SCIT packet %d\n",pcode);
		      } 

		   poff = poff + 4 + plen;
		   }
		break;
	 case 2:
		pblen = (int)read_short(buf+boff+8);
		packet_code_2 ( buf+boff+6 );
		break;
	 case 17: /* DPA */
		pblen = packet_code_17 ( buf+boff+6 );
		pblen -= 4; /* dpa length plus offset+2 */
		break;
	 case 18: /* DPA */
		pblen = packet_code_18 ( buf+boff+6 );
		pblen -= 4; /* dpa length plus offset+2 */
		break;
	 default:
		if ( ( (unsigned short)packet_code == 0XBA07 ) ||
		     ( (unsigned short)packet_code == 0XBA0F ) ||
		     ( (unsigned short)packet_code == 0XAF1F ) )
		   {
		   /*if ( (unsigned short)packet_code == 0XAF1F )
		      printf("radial product\n");
                   else
		      printf("raster product %s\n",stashfilnam);*/
		   im_simg ( radprj, stashfilnam, &ier,
			strlen(radprj), strlen(stashfilnam) );
		   if (ier == 0 )
		      {
		      im_isub ( isub_garea, &ier, strlen(isub_garea));
		      /*gg_maps ( radprj, "dset", stashfilnam, &i, &ier,
			strlen(radprj), strlen("dset"), strlen(stashfilnam) );*/
		      im_lutf ( "default", &ier, strlen("default"));
		      im_drop ( &ier );
		      im_cbar ( image_imcbar, &ier, strlen(image_imcbar) );
		      }
		   pblen = ival;
		   break;
		   }
		printf("packet code %d not handled [%u %u] %4X\n",packet_code,
			buf[boff + 6], buf[boff + 7], (unsigned short)packet_code);
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
   k = k + ival + 6; /* add layer divider length plus divider */
   }

}


