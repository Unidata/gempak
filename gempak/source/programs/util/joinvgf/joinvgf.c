#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

void check_dist ( float *e1_lats, float *e1_lons, int npts1,
                  float *e2_lats, float *e2_lons, int npts2, float tlrnc,
                  int *nomatch, int *contype);

Boolean verify_type ( VG_DBStruct *first_el, VG_DBStruct *second_el );

/************************************************************************
 * joinvgf.c								*
 *									*
 * CONTENTS:								*
 * joinvgf								*
 * check_dist								*
 * verify_type								*
 ***********************************************************************/
 

int main ( int argc, char **argv )
/************************************************************************
 * joinvgf								*
 *                                                                      *
 * Usage:								*
 * joinvgf input1.vgf input2.vgf output.vgf [tolerance]			*
 *                                                                      *
 * Tolerance (optional) should be specified in km.					*
 * The default value for tolerance is 0.5 km				*
 *                                                                      *
 * The following element classes are not processed:			*
 * CLASS_WATCHES, CLASS_TRACKS, CLASS_SIGMETS				*
 *									*
 * It is assumed that two input VGF files are the result of clipping	*
 * of some initial VGF file by program CLIPVGF using "exact" clipping	*
 * algorithm.								*
 *									*
 * main(argc, argv)                                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *  argc   int      number of parameters of command line                *
 *  argv   char**   parameter array of command line                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	02/04	original coding				*
 * m.gamazaychikov/SAIC	03/04	eliminated a boundary point introduced	*
 *				by clipvgf after clipping the original	* 
 *				linear object				*
 * S. Danz/AWC		07/06	Switch to new cvg_writefD() function    *
 ***********************************************************************/
{
int     e1grnum;
int    	ii, loc, ne, np, joffset, ier, icon;
int     infsiz1, infsiz2;
int     iseg1s, iseg1e, iseg2s, iseg2e;
int     jj1beg, jj2beg, jj2, jj1;
int     match, nomatch, contype, found;
int	more, curpos;
int	more1, curpos1, curposl1;
int	more2, curpos2, curposl2;
int     ne1, ne2, iret;
int	ninxarr, inxarr[100];
int     npts, npts1, npts2;
int     wrtflg, pagflg;
char	vg_type1, vg_type2;
char    infile1[128], ifname1[128];
char    infile2[128], ifname2[128];
char    outfile[128], ofname[128];
char	errgrp[8];
char	tolerance[8];
float	e1_lats[LLMXPT], e1_lons[LLMXPT], *ptr1lat, *ptr1lon;
float	e2_lats[LLMXPT], e2_lons[LLMXPT], *ptr2lat, *ptr2lon;
float	e_lats[LLMXPT],  e_lons[LLMXPT];
float   tlrnc, dst;

long	ifilesize;

VG_DBStruct     el, el_1, el_2, el_q;

FILE    *ifptr1, *ifptr2, *ofptr;
/*---------------------------------------------------------------------*/

    /*
     *  First check if number of input arguments is correct.
     */
    if ( argc < 3 )  {
	pagflg = G_FALSE;
	strcpy ( errgrp, "JOINVGF" );
        ip_help ( errgrp, &pagflg, &ier,
                  strlen(errgrp) );
	exit (0);
    }

    /*
     *  First input on command line is input vgf file1 name.
     */
    strcpy ( infile1, argv[1] );
    wrtflg = 0;
    cvg_open ( infile1, wrtflg, &(ifptr1), &ier );
    if ( ier != 0 )  {
	printf("Error opening VGF file %s\n", infile2 );
	exit (0);
    }
    cfl_inqr ( infile1, NULL, &ifilesize, ifname1, &ier );

    /*
     *  Second input on command line is input vgf file2 name.
     */
    strcpy ( infile2, argv[2] );
    wrtflg = 0;
    cvg_open ( infile2, wrtflg, &(ifptr2), &ier );
    if ( ier != 0 )  {
        printf("Error opening VGF file %s\n", infile2 );
        exit (0);
    }
    cfl_inqr ( infile2, NULL, &ifilesize, ifname2, &ier );

    /*
     *  Third input on command line is output vgf file name; create it.
     */
    strcpy ( outfile, argv[3] );
    cvg_crvgf ( outfile, &ier );
    cfl_inqr (  outfile, NULL, &ifilesize, ofname, &ier );

    /*
     *  Forth input on command line is join tolerance in kilometers
     */
    if ( argv[4] != (char *)NULL ) {
      strcpy ( tolerance, argv[4] );
      cst_crnm ( tolerance, &tlrnc, &iret );
    }
    else  {
      tlrnc = 0.5;
      printf("\n No tolerance was provided, continue with default value of %g km\n\n", tlrnc);
    }

    /*
     * Copy input files 1 and 2 into the output file
     */
    cvg_cp ( infile1, outfile, 0, &ier );
    cvg_cp ( infile2, outfile, 0, &ier );
    cfl_clos ( ifptr1, &ier );
    cfl_clos ( ifptr2, &ier );
    /*
     *  Loop through all the elements to set the range records.
     */
    crg_init ( &ier );
    ne = 0;
    more = G_TRUE;
    curpos = 0;
    ofptr = (FILE *) cfl_ropn(ofname, "", &ier);
    while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {
	cvg_rdrecnoc ( ofname, ofptr, curpos, &el, &ier );
	if ( ier < 0 )  {
	    more = G_FALSE;
	}
	else  {
	    crg_set ( &el, curpos, 1, &ier );
	    curpos += el.hdr.recsz;
	    ne++;
	}
    }
    cfl_clos ( ofptr, &ier );
    /*
     *  Loop through all the elements for OUTER SCAN of VG records.
     */
    icon = 0;
    ne1 = 0;
    more1 = G_TRUE;
    curpos1 = 0;
    ofptr = (FILE *) cfl_ropn(ofname, "", &ier);
    while ( ne1 < MAX_EDITABLE_ELEMS && more1 == G_TRUE )  {

        match = FALSE;
	cvg_rdrecnoc( ofname, ofptr, curpos1, &el_1, &ier );

	if ( ier < 0 )  {
	    more1 = G_FALSE;
	}
        else if ( el_1.hdr.recsz > 0 ) { 

	  curpos1 += el_1.hdr.recsz;
	  curposl1 = curpos1 - el_1.hdr.recsz;

          vg_type1  = el_1.hdr.vg_type;

          if ( el_1.hdr.vg_type != FILEHEAD_ELM && 
               el_1.hdr.delete  !=1 && 
               el_1.hdr.closed  != 1 ) {

            switch ( (int)vg_type1 )  {
              case    LINE_ELM:
                npts1 = el_1.elem.lin.info.numpts;
                infsiz1 = sizeof(LineInfo) + npts1*2*sizeof(float);
                ptr1lat = &(el_1.elem.lin.latlon[   0]);
                ptr1lon = &(el_1.elem.lin.latlon[npts1]);
                break;
              case    SPLN_ELM:
                npts1   = el_1.elem.spl.info.numpts;
                infsiz1 = sizeof(SpLineInfo) + npts1*2*sizeof(float);
                ptr1lat = &(el_1.elem.spl.latlon[   0]);
                ptr1lon = &(el_1.elem.spl.latlon[npts1]);
                break;
              case    FRONT_ELM:
                npts1 = el_1.elem.frt.info.numpts;
                infsiz1 = sizeof(FrontInfo) + npts1*2*sizeof(float);
                ptr1lat = &(el_1.elem.frt.latlon[   0]);
                ptr1lon = &(el_1.elem.frt.latlon[npts1]);
                break;
              default:
                npts1 = 1;
                ptr1lat = &(el_1.hdr.range_min_lat);
                ptr1lon = &(el_1.hdr.range_min_lon);
                break;
            }

            memcpy ( e1_lats, ptr1lat, (size_t)npts1*sizeof(float) );
            memcpy ( e1_lons, ptr1lon, (size_t)npts1*sizeof(float) );

            /*
             *  Loop through all the elements for INNER SCAN.
             */
            ne2 = ne1;
            curpos2 = curpos1;
            more2 = G_TRUE;
            while ( ne2 < MAX_EDITABLE_ELEMS && more2 == G_TRUE )  {

               cvg_rdrecnoc( ofname, ofptr, curpos2, &el_2, &ier );

               if ( ier < 0 )  {
                   more2 = G_FALSE;
               }
               else  {

                 crg_gginx( el_2.hdr.grptyp, el_2.hdr.grpnum,
                            sizeof(inxarr)/sizeof(inxarr[0]),
                            inxarr, &ninxarr, &ier );

                 curpos2 += el_2.hdr.recsz;
	         curposl2 = curpos2 - el_2.hdr.recsz;

                 vg_type2  = el_2.hdr.vg_type;

                 if ( el_2.hdr.delete !=1 && el_2.hdr.closed != 1 ) {

                    switch ( (int)vg_type2 )  {
                      case    LINE_ELM:
                       npts2 = el_2.elem.lin.info.numpts;
                       infsiz2 = sizeof(LineInfo) + npts2*2*sizeof(float);
                       ptr2lat = &(el_2.elem.lin.latlon[   0]);
                       ptr2lon = &(el_2.elem.lin.latlon[npts2]);
                       break;
                      case    SPLN_ELM:
                       npts2   = el_2.elem.spl.info.numpts;
                       infsiz2 = sizeof(SpLineInfo) + npts2*2*sizeof(float);
                       ptr2lat = &(el_2.elem.spl.latlon[   0]);
                       ptr2lon = &(el_2.elem.spl.latlon[npts2]);
                       break;
                      case    FRONT_ELM:
                       npts2 = el_2.elem.frt.info.numpts;
                       infsiz2 = sizeof(FrontInfo) + npts2*2*sizeof(float);
                       ptr2lat = &(el_2.elem.frt.latlon[   0]);
                       ptr2lon = &(el_2.elem.frt.latlon[npts2]);
                       break;
                      default:
                       npts2 = 1;
                       ptr2lat = &(el_2.hdr.range_min_lat);
                       ptr2lon = &(el_2.hdr.range_min_lon);
                       break;
                    }

                 memcpy ( e2_lats, ptr2lat, (size_t)npts2*sizeof(float) );
                 memcpy ( e2_lons, ptr2lon, (size_t)npts2*sizeof(float) );

                 /*
                  *  Check if the two elements are of the same type/subtype.
                  */
                 found = FALSE;
                 found = verify_type (&el_1, &el_2);

                 if (found) { 
                    el = el_1;
                    e1grnum = el_1.hdr.grpnum;

                    /*
                     * found two "suspicious" lines, need to examine their
                     * endpoints to see if they meet the criteria
                     * for "closeness"
                     */
                     check_dist (e1_lats, e1_lons, npts1, e2_lats, e2_lons, npts2,
                                 tlrnc, &nomatch, &contype);

                     if (!nomatch) {
                       /*
                        * proceed with connection
                        */
                       match = TRUE;
                       npts = npts1 + npts2-2;
                       switch (contype) {
                        case 1:
                         iseg1s = 0;
                         iseg1e = npts1-1;
                         iseg2s = npts1-1;
                         iseg2e = npts;
                         jj1beg = 0;
                         jj2beg = 1;
                         break;
                        case 2:
                         iseg1s = 0;
                         iseg1e = npts1-1;
                         iseg2s = npts1-1;
                         iseg2e = npts;
                         jj1beg = 0;
                         jj2beg = npts2-2;
                         break;
                        case 3:
                         iseg1s = npts2-1;
                         iseg1e = npts;
                         iseg2s = 0;
                         iseg2e = npts2-1;
                         jj1beg = 1;
                         jj2beg = 0;
                         break;
                        case 4:
                         iseg1s = npts2-1;
                         iseg1e = npts;
                         iseg2s = 0;
                         iseg2e = npts2-1;
                         jj1beg = 1;
                         jj2beg = npts2-1;
                         break;
                     }
                     /*
                      * writing lats and lons of el_1 to a new el
                      */
                     jj1=jj1beg;
                     for ( ii = iseg1s; ii < iseg1e; ii++ )  {
                          e_lats[ii] = e1_lats[jj1];
                          e_lons[ii] = e1_lons[jj1];

                       if ( (jj1beg == 0) | (jj1beg == 1) ) {
                          jj1++;
                       }
                       else {
                          jj1--;
                       }
                     }
                      /*
                       * writing lats and lons of el_2 to a new el
                       */
                     jj2=jj2beg;
                     for ( ii = iseg2s; ii < iseg2e; ii++ )  {
                          e_lats[ii] = e2_lats[jj2];
                          e_lons[ii] = e2_lons[jj2];

                       if ( (jj2beg == 0) | (jj2beg == 1) ) {
                          jj2++;
                       }
                       else {
                          jj2--;
                       }
                     }
                     /*
                      *  now that elements el_1 and el_2 are connected
                      *  proceed with regroupping elements grouped with
                      *  connected element el_2 
                      */
                     if ( el_2.hdr.grptyp > 0 && ninxarr > 0 )  {
                        for ( ii = 0; ii < ninxarr; ii++ )  {
                           crg_goffset ( inxarr[ii], &joffset, &ier );
                           cvg_rdrecnoc( ofname, ofptr, joffset, &el_q, &ier );
                           el_q.hdr.grpnum = e1grnum;
                           cvg_writefD( &el_q, joffset, el_q.hdr.recsz, ofname, &loc, &ier );
                        }
                     }

                     /*
                      * check if the element needs to be closed
                      */
                     if ( (el.hdr.vg_type == LINE_ELM) | 
                          (el.hdr.vg_type == SPLN_ELM) ) {
                         np = 1;
                         clo_dist (&e_lats[0], &e_lons[0], &np,
                                   &e_lats[npts-1], &e_lons[npts-1],
                                   &dst, &ier );
                         if (dst/1000. < tlrnc) {
                            el.hdr.closed=1;
                            npts = npts-1;
                            for ( ii = 0; ii < npts-1; ii++ )  {
                              e_lats[ii] = e_lats[ii+1];
                              e_lons[ii] = e_lons[ii+1];
                            }
                         }
                     }
                     /*
                      *  writing out lats and lons into element structure
                      */
                     if ( el.hdr.vg_type == LINE_ELM ) {
                        for ( ii = 0; ii < npts; ii++ )  {
                           el.elem.lin.latlon[ii]      = e_lats[ii];
                           el.elem.lin.latlon[ii+npts] = e_lons[ii];
                        }
                     }
                     else if (el.hdr.vg_type == SPLN_ELM ) {
                        for ( ii = 0; ii < npts; ii++ )  {
                           el.elem.spl.latlon[ii]      = e_lats[ii];
                           el.elem.spl.latlon[ii+npts] = e_lons[ii];
                        }
                     }
                     else if (el.hdr.vg_type == FRONT_ELM ) {
                        for ( ii = 0; ii < npts; ii++ )  {
                           el.elem.frt.latlon[ii]      = e_lats[ii];
                           el.elem.frt.latlon[ii+npts] = e_lons[ii];
                        }
                     }
                     /*
                      *  bookkeeping  - writing out parts of connected elements 
                      *  el_1 and el_2 with deleted flags
                      */
                     el_1.hdr.delete = 1;
                     el_2.hdr.delete = 1;
                     el_1.hdr.recsz = sizeof(VG_HdrStruct) + infsiz1;
                     el_2.hdr.recsz = sizeof(VG_HdrStruct) + infsiz2;
                     cvg_writefD( &el_1, curposl1, el_1.hdr.recsz, ofname, &loc, &ier );
                     cvg_writefD( &el_2, curposl2, el_2.hdr.recsz, ofname, &loc, &ier );
                     break;
                  }
                 }
                }
               }
               ne2++;
            }
            if (match) {
              /*
               * writing out newly connected element
               */
              if ( el.hdr.vg_type == LINE_ELM ) {
                 el.elem.lin.info.numpts = npts;
                 el.hdr.recsz = sizeof(VG_HdrStruct) + sizeof(LineInfo) + npts*2*sizeof(float);
              }
              else if (el.hdr.vg_type == SPLN_ELM ) {
                 el.elem.spl.info.numpts = npts;
                 el.hdr.recsz = sizeof(VG_HdrStruct) + sizeof(SpLineInfo) + npts*2*sizeof(float);
              }
              else if (el.hdr.vg_type == FRONT_ELM ) {
                 el.elem.frt.info.numpts = npts;
                 el.hdr.recsz = sizeof(VG_HdrStruct) + sizeof(FrontInfo) + npts*2*sizeof(float);
              }
              cvg_writefD( &el, -1, el.hdr.recsz, ofname, &loc, &ier );
              icon++;
            }
         }
	}
	ne1++;
    }

    if ( icon == 0) {
       printf("\nNo lines were connected, try another tolerance --\n\n");
    }

    cfl_clos ( ofptr, &ier );
    return(0);
}

/*=====================================================================*/

void check_dist ( float *e1_lats, float *e1_lons, int npts1,
                  float *e2_lats, float *e2_lons, int npts2, float tlrnc,
                  int *nomatch, int *contype)
/************************************************************************
 * check_dist		                                              	*
 *                                                                      *
 * This function examines the two elements to see if they may be        *
 * connected. If the distance between two points is less that tlrnc 	*
 * then nomatch is returned as FALSE with specific contype		* 
 *                                                                      *
 * check_dist ( e1_lats, e1_lons, int npts1, e2_lats, e2_lons, npts2,	*
 *              tlrnc, nomatch, contype)				*
 *                                                                      *
 * Input parameters:                                                    *
 *  *e1_lats	float	array of latitudes  in element1			*
 *  *e1_lons	float	array of longitudes in element1			*
 *   npts1	int	number of points on element1			*
 *  *e2_lats	float	array of latitudes  in element2			*
 *  *e2_lons	float	array of longitudes in element1			*
 *   npts2	int	number of points on element2			*
 *   tlrnc	float	the "closeness" benchmark			*
 *									*
 * Output parameters:                                                   *
 *  *nomatch	int	flag to connect or not to connect lines		*
 *			(TRUE  - not to connect, FALSE - connect) 	*
 *  *contype	int	an ingeter indicating which points are to 	*
 *			be connected					*
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	02/04						*
 ***********************************************************************/
{
int	    ii, np, ier;
float       dist, epoints[4] [4];
    
/*---------------------------------------------------------------------*/

       *contype = 0;    

       /*
        * contype1 - e1_l with e2_f
        */
       epoints[0][0] = e1_lats[npts1-1];
       epoints[0][1] = e1_lons[npts1-1];
       epoints[0][2] = e2_lats[0];
       epoints[0][3] = e2_lons[0];

       /*
        * contype2 - e1_l with e2_l
        */
       epoints[1][0] = e1_lats[npts1-1];
       epoints[1][1] = e1_lons[npts1-1];
       epoints[1][2] = e2_lats[npts2-1];
       epoints[1][3] = e2_lons[npts2-1];

       /*
        * contype3 - e1_f with e2_l
        */
       epoints[2][0] = e1_lats[0];
       epoints[2][1] = e1_lons[0];
       epoints[2][2] = e2_lats[npts2-1];
       epoints[2][3] = e2_lons[npts2-1];

       /*
        * contype4 - e1_f with e2_f
        */
       epoints[3][0] = e1_lats[0];
       epoints[3][1] = e1_lons[0];
       epoints[3][2] = e2_lats[0];
       epoints[3][3] = e2_lons[0];

       *nomatch = G_TRUE;
       ii = 0;

       while (ii<4 && *nomatch == G_TRUE) {

           np = 1;

           clo_dist ( &epoints[ii][0], &epoints[ii][1], &np, 
                      &epoints[ii][2], &epoints[ii][3],
                      &dist, &ier );
 
           if ( dist/1000.0 < tlrnc ) {
              *nomatch = G_FALSE;
           }
           else {
              *nomatch = G_TRUE;
           }

           switch (*nomatch) {
               case 1:
                  ii++;
                  break;
               case 0:
                  *contype = ii+1;
                  break;
           }
       }
}

Boolean verify_type ( VG_DBStruct *first_el, VG_DBStruct *second_el )
/************************************************************************
 * verify_type                                                    	*
 *                                                                      *
 * This function examines the two elements to see if they may be        *
 * connected.  True is returned if they are the same type/subtype.      *
 *                                                                      *
 * Boolean verify_type ( first_el, second_el ) 			       	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *first_el       VG_DBStruct     first element to be examined    *
 *      *second_el      VG_DBStruct     second element to be examined   *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return:                                                              *
 *                      Boolean         True if the two elements are of *
 *                                       the same type/subtype          *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	02/04   after pgconn_veryfyType			*
 * D.W.Plummer/NCEP	04/04	check entire fcode, not just 100s digit	*
 ***********************************************************************/
{
    Boolean     test = False;
/*---------------------------------------------------------------------*/

    /*
     *  Test to insure that elements are the same type and subtype
     *  where appropriate.
     */
    if (first_el->hdr.vg_class == second_el->hdr.vg_class) {

        if ( first_el->hdr.vg_class == CLASS_FRONTS) {
            if (first_el->elem.frt.info.fcode ==
                        second_el->elem.frt.info.fcode ) {
                test = TRUE;
            }
        }
        else if (first_el->hdr.vg_type == SPLN_ELM &&
                 second_el->hdr.vg_type == SPLN_ELM) {
            if (first_el->elem.spl.info.spltyp ==
                        second_el->elem.spl.info.spltyp) {
                test = TRUE;
            }
        }
        else if (first_el->hdr.vg_type == LINE_ELM &&
                 second_el->hdr.vg_type == LINE_ELM) {
            if (first_el->elem.lin.info.lintyp ==
                        second_el->elem.lin.info.lintyp) {
                test = TRUE;
            }
        }
        else if (first_el->hdr.vg_type == JET_ELM &&
                 second_el->hdr.vg_type == JET_ELM) {
            if (first_el->elem.jet.line.spl.info.spltyp ==
                        second_el->elem.jet.line.spl.info.spltyp) {
                test = TRUE;
            }
        }

    }

    return (test);
}
