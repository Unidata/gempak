#include "gb2def.h"
#include "dccmn.h"		/* for ivrblv global variable */
#include "gbcmn.h"		/* for gds global variable */

#include "dcgrib.h"

extern int ensext;
extern int pkmeth;  /* pkmeth = MDGRB2 defaults to GRIB2 storage, MDGDEC uses old method */

void
decode_grib2 (unsigned char *cgrib, int lenbul,
	      char *gemfil, char *filnam, int maxgrd)
/************************************************************************
* void decode_grib2 ( cgrib, lenbul, gemfil, filnam, naxgrd)		*
*	unsigned char	*cgrib	Input grib product			*
*	int		lenbul	length of input product			*
*	char		*gemfil	User specified output file name		*
*	char		*filnam	final output filename			*
*	int		maxgrd	Maximum number of grids in new file	*
*                                                                       *
* Error Codes                                                           *
* Log:                                                                  *
* Chiz/Unidata                  12/03   Created                         *
* James/Unidata			9/09	Accounted for prmext		*	
*************************************************************************/
{
  int numgrds = maxgrd;
  int i, n, ilen1, ilen2, prmext, ier, iret;
  int iflno;
  g2int unpack = 0, expand = 0;
  g2int listsec0[3], listsec1[13], numlocal;
  Gribmsg curr_g2;
  Geminfo curr_gem;
  char vcoord[20], errstr[133], gdattm[2][DTTMSZ], extstr[20];
  int ighdr[LLGDHD];
  int gdflag = 255, iprec;
  static int tblinit = 0;
  static char errgrp[] = "DECODE_GRIB2";
  static int replace = 1; 

  static char g2tables[5][LLMXLN] = { 0 }, *tbllist[5];

  curr_g2.cgrib2 = cgrib;
  curr_g2.mlength = lenbul;
  curr_g2.gfld = NULL;
  curr_g2.field_tot = 0;

  if (!tblinit)
    {
      for (i = 0; i < 5; i++)
	tbllist[i] = g2tables[i];
      tblinit = !0;
    }

  if ((ier =
       g2_info (curr_g2.cgrib2, listsec0, listsec1, &(curr_g2.field_tot),
		&numlocal)) != 0)
    {
      dc_wclg (0, errgrp, ier, "g2_info failed", &iret);
      return;
    }

  for (n = 0; n < curr_g2.field_tot; n++)
    {
      ier = g2_getfld (curr_g2.cgrib2, n + 1, unpack, expand, &curr_g2.gfld);
  
      /* initialize strings in geminfo structure */
      memset ( curr_gem.cproj, 0, sizeof(curr_gem.cproj));
      memset ( curr_gem.parm, 0, sizeof(curr_gem.parm));
      memset ( curr_gem.gdattm1, 0, sizeof(curr_gem.gdattm1));
      memset ( curr_gem.gdattm2, 0, sizeof(curr_gem.gdattm2));

      gb2_2gem (&curr_g2, &curr_gem, tbllist, ensext, &ier);
      /* make sure the times are null terminated, since the routine above
         strncpy's 1 more character than the fortran routine provides */
      curr_gem.gdattm1[DTTMSZ-1] = '\0';
      curr_gem.gdattm2[DTTMSZ-1] = '\0';

      if (ivrblv >= 4)
	gb2_diag (curr_g2.gfld, gdflag);

      /* TODO: Convert these to GEMPAK weather codes and store as grid data */
      if (ivrblv >= 3)
        {
        if ( ( numlocal > 0 ) && ( curr_g2.gfld->locallen > 0 ) && ( curr_g2.gfld->local != NULL ) )
           {
	   int i2vers = (int)curr_g2.gfld->local[0];
           int i2grp =  gb_btoi ( curr_g2.gfld->local,1,2,0);
	   int i2val =  gb_btoi ( curr_g2.gfld->local,3,4,0);
	   int i2ref = gb_btoi ( curr_g2.gfld->local,7,4,0);
           int i2scale = gb_btoi ( curr_g2.gfld->local,11,2,0);
	   int i2bits = (int)curr_g2.gfld->local[13];
	   int i2type = (int)curr_g2.gfld->local[14];
           g2int *i2out;

           printf("Local Section2 used [length= %d]\n",curr_g2.gfld->locallen);
           printf("check curr_g2.gfld->idsect[3] %d\n",curr_g2.gfld->idsect[3]);
           printf("check type %d\n",i2type);
           printf("section2 local %d %d %d\n",i2vers,i2grp,i2val);

           if(i2type == 1) 
              { /* simple packing using integers */
              i2out = (g2int *)malloc(i2val*(sizeof(int)));
              gbits ( curr_g2.gfld->local, i2out, (g2int)(15*8), (g2int)i2bits, (g2int)0, (g2int)i2val);
                 
	      for(i=0;i<i2val;i++)
                 {
                 if ( i2out[i] == 0 ) 
                    printf("\n");
                 else
                    printf("%c",i2out[i]);
                 }
              printf("\n");
              free(i2out);
              }
           }
        }

      if (ier != 0)
	{
	  sprintf (errstr, "Could not determine parameter name %d %d %d %d [%d]\0",
		   curr_g2.gfld->discipline, curr_g2.gfld->ipdtmpl[0],
		   curr_g2.gfld->ipdtmpl[1],curr_g2.gfld->ipdtnum, curr_gem.vcord);
	  dc_wclg (0, errgrp, ier, errstr, &iret);
	  g2_free (curr_g2.gfld);
	  curr_g2.gfld = NULL;
	  continue;
	}

      if (curr_g2.gfld->griddef == 0)	/* note... the center defined grid number is not provided */
	pds.grid_id = decode_g2gnum (curr_g2.gfld);
      else
	pds.grid_id = curr_g2.gfld->griddef;

      pds.center = curr_g2.gfld->idsect[0];
      pds.izero = curr_g2.gfld->idsect[1];
      pds.process = curr_g2.gfld->ipdtmpl[4];
      gds.kx = curr_g2.kx;
      gds.ky = curr_g2.ky;

      cst_rmbl ( curr_gem.gdattm1, gdattm[0], &i, &iret );
      cst_rmbl ( curr_gem.gdattm2, gdattm[1], &i, &iret );

      dcflnam2 (gemfil, filnam, &numgrds, curr_g2.gfld, gdattm[0], gdattm[1], &ier);

      if (ier == 0)
	{
	  memset (ighdr, 0, sizeof (ighdr));

	  dcogfil2 (filnam, &iflno, numgrds, curr_gem);

	  if ( pkmeth == MDGRB2 )
	    {
	    gb2_wgem(curr_g2, curr_gem, n + 1, iflno, replace, 
		ighdr, &ier);
	    /* method for storing necessary portion of GRIB2 product (Chiz)
	     * sections 3,4,5,6,7. Write template arrays for sect 3,4.
	     * Determine location to start of section 5, and length through section 7.
	     * Write length of data to GEMPAK grid file.
	     * Can be read through g2_unpack#.c routines.
	     */
	    }
          else
	    {
	    if (FGSIZ < (curr_g2.kx * curr_g2.ky))
	      {
	      FGRID =
		(float *) realloc (FGRID,
				   (curr_g2.kx * curr_g2.ky) *
				   sizeof (float));
	      if ( FGRID == NULL ) {
	        sprintf (errstr, "Reallocating grid for %d points [nx %d ny %d] failed\0", 
			curr_g2.kx * curr_g2.ky, curr_g2.kx, curr_g2.ky);
	        dc_wclg (3, errgrp, 0, errstr, &iret);
		g2_free (curr_g2.gfld);
      		curr_g2.gfld = NULL;
		FGSIZ = 0;
		return;
              }
	      FGSIZ = curr_g2.kx * curr_g2.ky;
	      }

	    /*
	     *   Unpack GRIB2 grid
	     */
	    gb2_grid (&curr_g2, curr_gem.iuscal, curr_gem.rmsval,
		    &iprec, FGRID, &iret);

	    /* Check JPEG & PNG missing values -- not needed currently -- 
	    if ((curr_g2.gfld->idrtnum == 40000)
	      || (curr_g2.gfld->idrtnum == 40)
	      || (curr_g2.gfld->idrtnum == 41)
	      || (curr_g2.gfld->idrtnum == 40010))
	      dcmisjpg (curr_g2, FGRID);*/

	    /*
	     *   write grid out to gempak file.
	     */
	    /*gd_wpgd (&iflno, FGRID, &curr_g2.kx, &curr_g2.ky, ighdr,
		   (char *)gdattm, curr_gem.level,
		   &curr_gem.vcord, curr_gem.parm, &replace,
		   &pkmeth, &iprec, &ier, DTTMSZ, 12);*/
	    cgd_wpgd (&iflno, FGRID, &curr_g2.kx, &curr_g2.ky, ighdr,
		   curr_gem.gdattm1, curr_gem.gdattm2, 
		   &curr_gem.level[0], &curr_gem.level[1],
		   &curr_gem.vcord, curr_gem.parm, &replace,
		   &pkmeth, &iprec, &ier );
	    }


	  lv_ccrd (&curr_gem.vcord, vcoord, &iret, sizeof (vcoord) - 1);
	  vcoord[sizeof (vcoord) - 1] = '\0';
          cst_rmbl ( vcoord, vcoord, &i, &iret );

          curr_gem.parm[sizeof(curr_gem.parm)-1] = '\0';
	  cst_rmbl( curr_gem.parm, curr_gem.parm, &i, &iret);

	  sprintf (errstr, "%s [%s] %d:%d %s %d %d\0", curr_gem.parm,
	     gdattm[0], curr_gem.level[0], curr_gem.level[1], 
	     vcoord, curr_g2.kx, curr_g2.ky);
	  if (ier != 0)
	     dc_wclg (0, errgrp, ier, errstr, &iret);
	  else
	     dc_wclg (1, errgrp, ier, errstr, &iret);

	}
      else
	{
	  sprintf (errstr, "decode_g2gds failed\0");
	  dc_wclg (0, errgrp, ier, errstr, &iret);
	}

      g2_free (curr_g2.gfld);
      curr_g2.gfld = NULL;
    }

  if (ivrblv >= 2)
    {
      sprintf (errstr, "numfields %d numlocal %d\0", curr_g2.field_tot,
	       numlocal);
      dc_wclg (2, errgrp, ier, errstr, &iret);
    }

}
