#include "nagrib2.h"
#include "gb2def.h"


int main ( void )
/************************************************************************
 * NAGRIB2                                                              *
 *                                                                      *
 * This program decodes GRIB2 fields and converts the data to a         *
 * GEMPAK grid file.                                                    *
 *                                                                      *
 *  Command line:                                                       *
 *  nagrib2                                                             *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP	09/2004   Based on NAGRIB       	        *
 * m/gamazaychikov/SAIC	09/2005   Added overwr				*
 * S. Gilbert/NCEP	10/2005   modified error checking on gd_wpgd	*
 * T. Piper/SAIC	09/06	  Added gg_sdev and gplt_dev		*
 * R. Tian/SAIC		09/2006   Changed Fortran call to C call    	*
 * S. Gilbert/NCEP	11/2007   Added pdsext to gb2_2gem args     	*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 * S. Jacobs/NCEP	12/09	Moved error check to allow print of	*
 * 				GRIB info even if grid cannot be decoded*
 ***********************************************************************/
{

    int    respond, ret, mode=1, done=0, err, i, skip;
    int    g2filelen, igdfln;
    int    count, ivers, ier, nfld, dgflag;
    int    termflg, fileflg, nfps, prttl;
    int    navsiz, gsflag, igxold, iprec;

    int    g2disc, g2cat, g2id, g2pdt, g2vcrd, g2gdt;
    int    inx, iny, ighdr[LLGDHD], replace=0, pkmeth=MDGDEC;
    int    fill, igrdnm, igx, igy, jx, jy, ksbx[2], ksby[2], subset;
    char   tmpproj[24], outfil[LLMXLN];

    float  rnvblk[LLNNAV], *grid=0;
    char   *gplt_dev="GN";
    G2_input  input;                  /* user input variables */
    Gribmsg   curr_g2;                /* current GRIB2 message and 
                                                data field */
    Geminfo   curr_gem;
    FILE *fps[MMFILE], *fp;
/*-----------------------------------------------------------------------*/
/*
 *  Initialize TAE.
 */
    ip_init ( &respond, &ret );
    if ( ret != 0 ) exit(1);
    ip_idnt ( "NAGRIB2", &done, 7 );

/*
 *  Initialize the GEMPAK plotting package.
 */
    gg_init ( &mode, &ret );
    if ( ret != 0 ) exit(1);

/*
 *  Initialize grid library common area grdcmn.cmn.
 */
     gd_init ( &ret );
     
/*
 *  Set device.
 */
    gg_sdev(gplt_dev, &ret, sizeof(gplt_dev) );

    skip = 1;

/*
 *  Process next request, if user has one.
 */
    while ( done == 0 ) {
        for ( i=0; i<LLGDHD; i++ ) ighdr[i]=0;
/*
 *  Wait for user input.
 */
        if ( skip == 0 ) ip_dynm( &done, &ret );
        skip = 0;
        if ( done != 0 ) break;          /*  Exit out of interactive loop  */
/*
 *  Get user input info.
 */
        nag2in( &input, &ret );
        if ( ret != 0 ) {
            er_wmsg("NAGRIB2",&ret," ",&ret,7,1);
        }
        igdfln=0;
/*
	printf("SAG:%s|%s\n",input.g2file,input.gdoutf);
        printf("SAGcpy:%s\n",input.cpyfil);
        printf("SAGgar:%s\n",input.garea);
        printf("SAGt1:%s:%s:\n",input.tables[0],input.tbllist[0]);
        printf("SAGt2:%s:%s:\n",input.tables[1],input.tbllist[1]);
        printf("SAGt3:%s:%s:\n",input.tables[2],input.tbllist[2]);
        printf("SAGt4:%s:%s:\n",input.tables[3],input.tbllist[3]);
        printf("SAGt5:%s:%s:\n",input.tables[4],input.tbllist[4]);
*/
/*
 *  Open Output devices.
 */
        inc_outt ( input.output, "NAGRIB2.fil", &termflg, &fileflg, outfil,
                   &ret );
        nfps = 0;
        if ( termflg == G_TRUE ) {
            fps[nfps++] = stdout;
        }
        if ( fileflg == G_TRUE ) {
            fp = cfl_wopn ( outfil, &ret );
            fps[nfps++] = fp;
        }
/*
 *  Open input GRIB file.
 */
        g2filelen = strlen( input.g2file );
        gb2_open( input.g2file, &g2filelen, &curr_g2, &ret);
        if ( ret != 0 ) {
            err=-15;
            er_wmsg("NAGRIB2",&err," ",&ret,7,1);
            continue;                   /*  go to next iteration of loop  */
        }
/*
 *  The following loop iterates over each GRIB2 field in 
 *  file input.g2file until EOF or the maximum number of
 *  grids specifiec in MAXGRD is reached.
 */
        count=0;
        nfld=0;
        while ( count < input.maxgrid ) {
/* 
 *  Get next GRIB message.
 */
            gb2_next( &curr_g2, &ivers, &ier );
            if ( ier == -16 ) {         /*   EOF on GRIB2 file  */
               break;
            }
            else if ( ivers != 2 ) {      /*  Not GRIB2  */
               er_wmsg("NAGRIB2",&ier," ",&ret,7,1);
               continue;
            }
            else if ( ier != 0 ) {
               er_wmsg("NAGRIB2",&ier," ",&ret,7,1);
               continue;
            }
            nfld++;
/* 
 *  Convert decoded GRIB2 to gempak info.
 */
            gb2_2gem( &curr_g2, &curr_gem, input.tbllist, input.pdsext, &ier);

/* 
 *  Print decoded GRIB2 diagnostic info, if requested.
 */
            dgflag = input.g2dglst[nfld-1];
            if ( dgflag != 0 )  gb2_diag( curr_g2.gfld, dgflag );

            if ( ier != 0 ) {
               er_wmsg("GB",&ier," ",&ret,2,1);
               continue;
            }
/*
 *  If user variable GDOUTF != 'LIST', process fields
 *  to be written out to the Gempak file.
 */
            if ( input.lstflag != 1 ) {
/*
 *  Open output GEMPAK file, if not done previously.
 */
                 if  ( igdfln == 0 )  {
                     fill=0;
                     na_gcog ( input.gdoutf, input.proj, input.grdarea,
                               input.kxky, input.cpyfil, &fill, 
                               input.garea, curr_gem.gdsarr, &input.maxgrid,
                               &igdfln, rnvblk, &igrdnm, tmpproj,  &igx, &igy,
                               &jx, &jy, ksbx, ksby, &subset, &err );
                     if  ( err != 0 )  {
                         er_wmsg("NAGRIB2",&err," ",&ret,7,1);
                         break;
                     }
                     igxold = rint( rnvblk[4] );
 
                 }
/*  
 *  If output gempak file grid has same navigation
 *  block as the current GRIB2 field, then unpack
 *  field and write to gempak file.
 */
                 navsiz = LLNNAV;
                 grc_cnav ( rnvblk, curr_gem.navblk, &navsiz, &gsflag, &err);
                 /*for(i=0;i<12;i++) printf(" %f",rnvblk[i]);printf("\n");*/
                 /*for(i=0;i<12;i++) printf(" %f",curr_gem.navblk[i]);printf("\n");*/
                 if ( gsflag == 1 ) {
/*
 *  Allocate space for grid.
 */
                     grid = (float *)malloc( igx * igy * sizeof(float) );
                     if ( grid == 0 ) {
                         err=-30;
                         er_wmsg("NAGRIB2",&err," ",&ret,7,1);
                         continue;
                     }
/*
 *  Unpack GRIB2 grid.
 */
                     gb2_grid ( &curr_g2, curr_gem.iuscal, curr_gem.rmsval, 
                                &iprec, grid, &err);
/*
 *  Subset grid, if necessary.
 */
                     inx = igx;
                     iny = igy;
                     if ( subset == 1 ) na_gssg( &igxold, ksbx, ksby,
                                                 grid, &inx, &iny, &err );
/*
 *  Write grid out to gempak file.
 */
                     replace = input.overwr;
                     cgd_wpgd ( &igdfln, grid, &inx, &iny, ighdr,
                                curr_gem.gdattm1, curr_gem.gdattm2,
                                &curr_gem.level[0], &curr_gem.level[1],
                                &curr_gem.vcord, curr_gem.parm, &replace,
                                &pkmeth, &iprec, &err );

                     if ( grid != 0 ) free(grid);
                     grid=0;

                     if ( err != 0 ) {
                         er_wmsg("GD",&err," ",&ret,2,1);
                         if ( err == -11 && replace == 0 ) break;
                         continue;
                     }
                     count++;

                 }  /* end of proper grid area block  */

            }  /*  end of  "not 'LIST'" block  */
/*
 *  Print GRIB field summary to the output devices.
 */
            g2disc = curr_g2.gfld->discipline;
            g2cat = curr_g2.gfld->ipdtmpl[0];
            g2id = curr_g2.gfld->ipdtmpl[1];
            g2pdt = curr_g2.gfld->ipdtnum;
            g2gdt = curr_g2.gfld->igdtnum;
            g2vcrd= curr_g2.gfld->ipdtmpl[9];
            prttl=0;
            if ( nfld % 50  == 1 )  prttl=1;
            grc_wgb2 ( fps, &nfps, &prttl,  &nfld, curr_gem.gdattm1,
                       curr_gem.gdattm2, &curr_gem.level[0], &curr_gem.level[1],
                       &curr_gem.vcord, curr_gem.parm, &g2disc, &g2cat,
                       &g2id, &g2pdt, &g2vcrd, &g2gdt, &err );

        } /*  end while (count... ) loop */
/*
 *  Close output GEMPAK file and print Grid info
 *  to the output devices.
 */
        if  ( igdfln != 0 )  {
            gd_clos ( &igdfln, &err );
            if ( nfps != 0 )  grc_geni ( input.gdoutf, fps, &nfps, &err );
        }
/*
 *  Close input GRIB file.
 */
        gb2_clos( &curr_g2, &ret );
/*
 *  Print summary.
 */
        na_smry ( fps, &nfps, &nfld, &count, input.g2file, input.gdoutf, &err );
/*
 * Close output
 */
        if ( fileflg == G_TRUE ) cfl_clos ( fp, &err );
    }
/*
 *  Exit the GEMPAK user interface.
 */
    ip_exit( &ret );
 
    return(0);
}
