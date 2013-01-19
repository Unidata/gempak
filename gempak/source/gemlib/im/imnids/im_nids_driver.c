#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define	im_nidsdriver	im_nidsdriver_
#define clz_rfil	clz_rfil_
#define gdctbl	gdctbl_
/*#define gsmode gsmode_*/
#define gqmode gqmode_
#define gsgmgn  gsgmgn_
#define vad_colors	vad_colors_
#define vad_rms_vals	vad_rms_vals_
#define vad_rms_colors	vad_rms_colors_
#define vad_line	vad_line_
#endif

#define MAX_X   2047.0
#define MIN_X   -2048.0

#define MAX_PX  511.
#define MIN_PX  0.

float XMAX = MAX_X, XMIN = MIN_X;
float radar_clat, radar_clon;
int graphic_color = 1;
int line_color, line_type, _imnids_line_width;
char wind_str[] = "bk1";

void
set_gridproj (int num)
{
  int ier, kx, ky;
  float rltln[4], rnvblk[256], anlblk[128];
  char proj[10], gname[20], pnum[5];
  switch (num)
    {
    case 940:
      sprintf (pnum, "#940\0");
      gdctbl (pnum, gname, proj, &kx, &ky, rltln, rnvblk, anlblk,
	      &ier, strlen (pnum), sizeof (gname), sizeof (proj));
      if (ier != 0)
	printf ("failed to set grid proj %d\n", num);
      break;
    default:
      printf ("unknown grid projection\n");
    }
}



int
read_int (unsigned char *x)
{
  int ival = (((((x[0] << 8) | x[1]) << 8) | x[2]) << 8) | x[3];
  return (ival);
}

short
read_short (unsigned char *x)
{
  short ival = (x[0] << 8) | x[1];
  return (ival);
}

/*
 * Default vad colors as specified by OFCM FMH No. 11
 */

int vadcols[] = { 3, 5, 2, 6, 7, 23, 31, 1, 1, 1, 1, 1, 1, 1, 1, 6 };

void vad_color_init ()
{
  int ier, bsize=LLMXLN, inum;
  char buf[LLMXLN], *cpos;
  FILE *fp;

  fp = cfl_tbop ( "gpvad.config", "unidata", &ier );
  if (fp == NULL) 
  {
    printf("Could not open gpvad.config color table, using OFCM FMH-11C defaults\n");
    return;
  }

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


/*
 * default RMS values...will try to replace these with product
 * specific values if found.
 */
#define NRMS	5
int vad_rms_lvls[NRMS] = { 0, 4, 8, 12, 16 };

void
vad_colors (int *icolor, int *ier)
{
  *ier = 0;
  graphic_color = *icolor;
}

void
vad_line (int *icolor, int *itype, int *iwidth, int *ier)
{
  *ier = 0;
  line_color = *icolor;
  line_type = *itype;
  _imnids_line_width = *iwidth;
}

int
vad_setcol (int ival)
{
  int ier;

/* from FCM-H11C-1991, color levels should be 0-4,
   From NCDC nids documentation, ival will be iin range 1-5,
   Therefore, the color selected will be ( ival - 1) */
  if ((ival < 1) || (ival > 16))
    {
      if ( ival != 0 ) printf ("unexpected VAD color index %d\n", ival);
      ival = 16;
    }
  gscolr (&vadcols[ival - 1], &ier);
  return (ier);
}


void vad_rms_colors ( int *NFLVL, int ifcolr[] )
{
int i;
for ( i = 0; i < *NFLVL; i++ )
   ifcolr[i] = vadcols[i];
}

void vad_set_rms ( int i, int ival )
{
if ( ( i >= NRMS ) || (i < 0) )
   {
   printf("Can't set RMS value (%d,%d)\n",i,ival);
   return;
   }
vad_rms_lvls[i] = ival;
}

void vad_rms_vals ( int *NLVL, float flvls[], int *nvals )
{
int i;
for ( i = 0; i < *NLVL &&  i < NRMS; i++ )
   flvls[i] = (float)vad_rms_lvls[i];
if ( *NLVL < NRMS )
   *nvals = *NLVL;
else
   *nvals = NRMS;
}

char isub_garea[LLMXLN];

void
vad_garea (char *garea, int *lenstr, int *ier)
{
  strncpy (isub_garea, garea, *lenstr);
  isub_garea[*lenstr] = '\0';
  cst_lcuc (isub_garea, isub_garea, ier);
  return;
}

void
im_nidsdriver (char *filnam, int *itype, int *ihoff, int *imlenf,
	       int *noff, int block_offs[], int *istart)
{
  FILE *fp;
  char *defdir = NULL, newfil[255];
  int nbytes, block_id, block_len, ier, i, j, bcount;
  long flen, ic;
  int nblocks = 0, ibin, ibout, imnewlen;
  int product_message_code;
  float xmin_sav, xmax_sav;
  /* int nlun = 1, luns[] = { 6 }; */
  char proj[9], projm[9];
  float angl1, angl2, angl3, mangl1, mangl2, mangl3;
  float dlatll, dlonll, dlatur, dlonur, latll, lonll, latur, lonur;


  int kx, ky, saveproj, mier;
  static float rmargin[] = { 4.0, 4.0, 4.0, 2.0 };

  unsigned char *barr = NULL, *prod;

/*
 *	Initialize wind barbs ... if not already done by program!
 */
  i = strlen (wind_str);
  vwind_init (wind_str, &i, &ier);
/*
 * allocate size of image plus the inner CCB, WMO, PIL included in
 * zlib compressed block 
 */
  imnewlen = *imlenf + 128;
  barr = (unsigned char *) malloc (imnewlen);
  if (*itype == IFNEXZ)
    {
      /* read compressed file */
      clz_rfil (filnam, defdir, &nblocks, ihoff, &imnewlen, barr,
		&ibin, &ibout, &ier);
      /* now skip past internal ccb, wmo and pil of zlib product */
      nbytes = 2 * (((barr[0] & 63) << 8) + barr[1]);

      i = 0;
      j = nbytes;
      while ((i < 2) && (j < ibout))
	{
	  if (barr[j] == '\n')
	    i++;
	  j++;
	}
      if (i != 2)
	{
	  printf ("error finding internal wmo/pil of zlib product\n");
	  free (barr);
	  return;
	}
      else
	prod = barr + j;
    }
  else
    {
      /*
       * Obtain the record length of the input file.
       */
      cfl_inqr (filnam, defdir, &flen, newfil, &ier);

      /*
       * Open the input file. The file has already been opened
       * once, so assume this will work.
       */
      fp = cfl_ropn (filnam, defdir, &ier);


      /* seek to start of nids product */
      cfl_seek (fp, *ihoff, SEEK_SET, &ier);
      /*
       *  Read the file.
       */
      cfl_read (fp, *imlenf, barr, &nbytes, &ier);
      prod = barr;

      cfl_clos (fp, &ier);
    }

  radar_clat = (float) read_int (prod + 20) / 1000.;
  radar_clon = (float) read_int (prod + 24) / 1000.;

  /*
   ** Message header block
   */
  product_message_code = (int) read_short (prod);
  switch (product_message_code)
    {
    case 48:
      i = 2;
      gsmode (&i, &ier);
      /* set graph margins */
      gsgmgn (&rmargin[0], &rmargin[1], &rmargin[2], &rmargin[3], &ier);

      /* set the product specific VAD RMS levels */
      for ( i = 0; i < 5; i++)
	 vad_set_rms ( i, (int)read_short(prod+62+(i*2)));
      /* initialize VAD colors */
      vad_color_init ();
      XMAX = MAX_PX;
      XMIN = MIN_PX;
      break;
    case 62:
      { 
      int bc,nblock,bstart,blen;

      if ( block_offs[0] == 0)
         {
         free(barr);
         return;
         }

      bstart = block_offs[0] * 2 + 4;
      nblock = (int) read_short (prod + block_offs[0] * 2 + 2);

      for ( bc=0; bc < nblock; bc++)
          {
	  blen = (int)read_short (prod + bstart);
	  while ( blen > 0 )
             {
	     bstart += 2;
	     for ( ic=0; ic<blen; ic++ ) 
                printf("%c",prod[bstart + ic]);
             printf("\n");
	     bstart = bstart + blen;
	     blen = (int)read_short(prod+bstart);
             }
          bstart += 2;
          }

      if ( block_offs[1] == 0)
         {
         free(barr);
         return;
         }
      
      block_id = (int) read_short (prod + block_offs[1] * 2);
      if ( block_id == 22 )
	  j = packet_code_22 ( prod + block_offs[1] * 2);
      else
          printf("Unexpected block 1 packet %d for product 62\n", block_id);
      
      }
      free (barr);
      return;
    case 81:
      /* need to set grid projection for DPA, so save any current grid projection */
      gqmprj (projm, &mangl1, &mangl2, &mangl3, &latll, &lonll, &latur,
	      &lonur, &mier, sizeof (projm));
      gqgprj (proj, &angl1, &angl2, &angl3, &kx, &ky, &dlatll, &dlonll,
	      &dlatur, &dlonur, &saveproj, sizeof (proj));
      set_gridproj (940);
      mangl3 = 0;		/* seems to be needed? */
      if (mier == 0)
	gsmprj (projm, &mangl1, &mangl2, &mangl3, &latll, &lonll, &latur,
		&lonur, &ier, sizeof (projm));
      break;
    default:
      XMAX = MAX_X;
      XMIN = MIN_X;
    }



  /*for ( i = *istart; i < ( *noff - 2 ) ; i++ ) */
  i = *istart;
  bcount = 0;
  while (bcount < (*noff - 2 - *istart))
    {
      if (i > 2)
	break;			/* sanity check...shouldn't happen, but block_offs[3] currently */

      if ((block_offs[i] != 0) &&
	  ((int) read_short (prod + block_offs[i] * 2) == -1))
	{
	  bcount++;
	  block_id = (int) read_short (prod + block_offs[i] * 2 + 2);
	  block_len = read_int (prod + block_offs[i] * 2 + 4);
	  switch (block_id)
	    {
	    case 1:
	      /*printf("call im_nids_symbol\n"); */
	      im_nids_symbol (prod, block_offs[i]);
	      break;
	    case 2:
	      xmin_sav = XMIN;
	      xmax_sav = XMAX;
	      XMIN = MIN_PX;
	      XMAX = MAX_PX;
	      /*printf ("call im_nids_alpha_graphic\n");*/
	      im_nids_alpha_graphic (prod + (block_offs[i] * 2));
	      XMIN = xmin_sav;
	      XMAX = xmax_sav;
	      break;
	    case 3:
	      /*printf("call im_nids_tabular\n"); */
	      im_nids_tabular (prod + (block_offs[i] * 2));
	      break;
	    default:
	      printf ("unknown block to decode %d %d %d %d %d\n", block_id, block_len,block_offs[0],block_offs[1],block_offs[2]);
	    }
	}
      /*else 
         bcount++;*/
      i++;
    }

  switch (product_message_code)
    {
    case 48:
      i = 1;
      gsmode (&i, &ier);
      break;
    case 81:
      /* restore grid projection if necessary */
      if (saveproj == 0)
	gsgprj (proj, &angl1, &angl2, &angl3, &kx, &ky,
		&dlatll, &dlonll, &dlatur, &dlonur, &ier, sizeof (proj));
      break;
      /*default:
         printf("stay in map mode\n"); */
    }

/*
*/

  free (barr);

}
