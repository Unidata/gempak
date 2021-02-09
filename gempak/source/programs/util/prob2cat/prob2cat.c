#include "p2c.h"


#define PROB2CAT_TIE_DIST_IN_MAP         ( 0.001 ) /* Tie distance for two points in
                                                map coordinate */
/*
 *  Global variables
 */
int          nIn;
char         _Day_out[5];
VG_DBStruct *elIn = NULL;
int          nGroup = 10;

/*
 * Variables for OUTLOK_AREA bounds in map coordinates as closed lines.
 */
char		_boundsType[ 8 ] = "OUTLOOK";
char		_boundsTag [ 14 ] = "<AREA>OUTLOOK";
int		_areaP;
float           _areaX[MAX_BOUND_PT], _areaY[MAX_BOUND_PT];

/************************************************************************
 * prob2cat.c                                                           *
 *                                                                      *
 * CONTENTS:                                                            *
 * 	prob2cat                                                        *
 *                                                                      *
 * Private Functions:							*
 *                                                                      *
 *	p2c_AddGenT	- adds general thunderstorm line to outlook	*
 *			  performs interpolation for 01Z cycle		*
 *	p2c_AdjustLbl   - adjust the positions of the labels to avoid	*
 *			  overlapping					*
 *	p2c_AddOrig2Grp - adds the original outlooks that will be bumped*
 *			  into the P2C_Grp group 			*
 *	p2c_BumpCat	- bumps outlook category to next level		*
 *	p2c_CloseCntr	- closes the contour if it is open		*
 *      p2c_CloseSingleCntr     - closes single contour if it is open   *
 *      p2c_CopyCntr    - creates a copy of a contour                   *
 *      p2c_CopyLbl     - creates a copy of a label                     *
 * 	p2c_CorctCntr   - clips a closed polygon along the SPC Outlook  *
 *			  area boundary					*
 *	p2c_Elms2Grps	- groups elements into categories		*
 *	p2c_Elm2Poly	- writes elements into polygon structures	*
 *      p2c_ExtendCntr  - extends contour endpoints beyond boundary     *
 *	p2c_GetCat 	- returns the risk category of the element	*
 *	p2c_GetGenT	- returns 10%-risk general thunderstorm cntrs	*
 *	p2c_Grp2Out	- calls PolyClip for each risk category		*
 *	p2c_NewPoly	- accepts a line inside a polygon and returns	*
 *			  a new polygon with the line as boundary	*
 *	p2c_OpenCntr	- opens the contours if it is closed		*
 *	p2c_Poly2Out	- writes polygon structures into outlook elms	*
 *	p2c_PolyClip	- performs polygon operations on category cntrs	*
 *	p2c_SetArea	- reads SPC Outlook area bounds into bnds elm	*
 *      p2c_SplitCntr   - splits contour into parts at intersection pts *
 *	p2c_Util	- returns some info of the input element	*
 *									*
 ***********************************************************************/

static void p2c_AddGenT   ( int interp_enh, int nGt, VG_DBStruct *elGt,
                            int *nout, VG_DBStruct **outgt, int *iret );

static void p2c_AdjustLbl ( int nin, VG_DBStruct *el_in, int *iret);

static void p2c_AddOrig2Grp ( int *ngrp, P2C_Grp **p2cGrp, int nbump,
			      VG_DBStruct *origEl, char **origCat, 
			      int *iret );

static void p2c_BumpCat   ( VG_DBStruct *el, int ind, char *newcat, int *done,
                            int *iret);

static void p2c_CloseCntr ( VG_DBStruct *el_in, VG_DBStruct **el_out, int *nOut,
                            int *iret );

static void p2c_CloseSingleCntr ( int *npIn, float *xIn,  float *yIn,
                                  int *nClp, float *xClp, float *yClp,
                                  Boolean *InOut,
                                  int *nOut, float *xOut, float *yOut,
                                  int *iret );

static void p2c_CopyCntr  ( VG_DBStruct *el_in, VG_DBStruct *el_out, 
                            int *iret);

static void p2c_CopyLbl   ( VG_DBStruct *el_in, VG_DBStruct *el_out, 
                            int *iret);

static void p2c_CorctCntr ( VG_DBStruct *el_in, int *iret);

static void p2c_Elms2Grps ( int nin, VG_DBStruct *el_in, int *ngrp,
                            P2C_Grp **p2cGrp, VG_DBStruct *origEl, 
			    int *iret );
                                                                                     
static void p2c_Elm2Poly  ( VG_DBStruct el, gpc_polygon *gpc_poly, 
                            int *iret );
                                                                                    
static void p2c_ExtendCntr ( int *pointFirst, int *pointLast,
                             int *npIn,  float *xIn,  float *yIn,
                             int *npOut, float *xOut, float *yOut,
                             int *iret);

static void p2c_GetCat	  ( VG_DBStruct *el, int prob, char *haz,
                            char *cat, int *nbump, VG_DBStruct **origEl, 
			    char ***origCat, int bumpFlag, int *iret );

static void p2c_GetGenT   ( char *Gt_str, int nInEnh, VG_DBStruct *elEnh,
                            int *nout, VG_DBStruct **el_Gt, int *iret );

static void p2c_Grp2Out   ( int ngrp, P2C_Grp *pc2Grp,
                            VG_DBStruct **outlk, int *nout, int *iret );
#ifdef P2C_NEWPOLY
static void p2c_NewPoly  ( int *np1, float xp1[], float yp1[], int *np2, 
			    float xp2[], float yp2[], int *np3, 
			    float xp3[], float yp3[], int *iret );
#endif
static void p2c_OpenCntr  ( VG_DBStruct *el_in, VG_DBStruct **el_out, 
                            int *nout, int *iret );

static void p2c_Poly2Out  ( gpc_vertex_list contour,  int iclr, 
                            VG_DBStruct *out, int *iret );

static void p2c_PolyClip  ( int ncntrs, VG_DBStruct **cntrs, char *cat,
                            int iclr, int *numOut, VG_DBStruct **outlk, 
                            int *iret );

static void p2c_SetArea   ( int *iret );

static void p2c_SplitCntr ( int *npIn, float *xIn,  float *yIn,
                            int *nClp, Boolean *inOut, float *xClp, float *yClp,
                            int *nSeg, int *segS, int *segE,
                            int *iret );

static void p2c_Util 	  ( VG_DBStruct el, int *np, int *gtype, 
 			    int *gnmbr, float *xout, float *yout);

int main ( int argc, char **argv )
/************************************************************************
 * prob2cat                                                             *
 *                                                                      *
 * This program accepts as input VGF files containing probabilistic     *
 * contours for tornado, wind and hail and generates a VGF file with 	*
 * categorical outlook							*
 *                                                                      *
 * Usage:                                                               *
 *                                                                      *
 * prob2cat VGF_file(s)                 				*
 *                                                                      *
 * 	VGF_Files	-  a list of VGF files separated by whitespace  *
 *                                                                      *
 * main ( argc, argv )                                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *  	argc	int      	number of parameters of command line    *
 *  	argv   	char**   	parameter array of command line         *
 *                                                                      *
 * Output parameters:                                                   *
 *	VGF_file	-  p2c_outlook_DAYn_DDHHMMZ.vgf			* 
 * Return parameters:                                                   *
 *	NONE     		                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 * m.gamazaychikov/SAIC	01/07	Finalized prototype			*
 * m.gamazaychikov/SAIC	05/08	Added calls to p2c_AdjustLbl and 	*
 *				p2c_CorctCntr, grouped labels with cntrs*
 * m.gamazaychikov/SAIC	08/08	Added check and warning for ungrouped   *
 *                              contours				*
 * m.gamazaychikov/CWS  06/09   Increased garea, included checks for	*
 *                              number of preprocessed elements, added	*
 *				copying of original elements		*
 * X.Guo/CWS	  	12/09   Added evaluation tolerance variable in  *
 *                              p2c_CloseSingleCntr and set its value	*
 *                              to 0.001F            			*
 * X.Guo/CWS            03/10   Added codes to handle intersecting point*
 *                              Commented out p2c_NewPoly               *
 * X.Guo/CWS		09/10   Removed p2c_CheckPtOnPoly and added code*
 *                              to reduce multi-points                  *
 * X.Guo/CWS            09/11   Adjusted the number of points needed to *
 *                              be copied from bounds                   *
 * S. Jacobs/NCEP	10/12	Restored previous calculation of the	*
 * 				number of points copied			*
 * B. Yin/SGT		11/14	Fixed issues on bumped outlooks		* 				
 * S. Guan/NCEP         05/16   Modified PROB2CAT_TIE_DIST_IN_MAP and   *
 *                              tol2                                    *
 * S. Guan/NCEP         06/16    Fixed the bug caused because            *
 *                              line is fliped                          *
 ***********************************************************************/
{
    int 	nn, ii, jj, kk, ier, nextEl, curPos, nseg;
    int		nInEnh, nout, loc;
    int         maxch, maxnum, istmax, ipos;
    int         interp_enh=G_FALSE, nout_gent, nGt, *iclozd;
    int         ilblgrp, nElInClosed;
    int         iExc, *excludedGrp;
    long	fileSize;
    long    	ifilesize;
    char    	vgfname[ 128 ], tmstr[10],  Gt_str[4];
    char        outfile[128],  ofname[128];
    char        dumstr[48], datestamp[8], sep[2], defstr[2], **stinarr;

    double      epsilon = 0.000001;

    FILE	*fptr, *ofptr;

    VG_DBStruct el;
    VG_DBStruct *el_enh = NULL, *elGt = NULL;
    VG_DBStruct *outlk  = NULL, *out_gent = NULL, *outlk2 = NULL,
                *elInClosed=NULL, *elInTemp, *origEl = NULL;

    int          np2cgrp;
    P2C_Grp      *p2cGrp = NULL;

    /*
     *  Variables to initialize device and projection
     */
    int		mode = 1, iunit = 1, itype = 1, istat;
     	
    float       xsize = 1500, ysize = 1500;
    float	lllat = 10, lllon = -120, urlat =64 , urlon = -45;
    float	prjang1 = 90, prjang2 = -105, prjang3 = 0;

    char        device[ 3 ] = "GN", dfilnam[ 20 ] = "prob2cat";
    char	proj[ 4 ] = "str";
    float       temp1, temp2;
    int         kt;
/*---------------------------------------------------------------------*/

    printf ( "\n*** prob2cat: Last modified on May. 7, 2019 ***\n" );
    /*
     *  Check if the number of input arguments is correct.
     */
    if ( argc < 2 ) {
       printf ( "\nFOR HELP TYPE: prob2cat h[elp]\n\n" );
       exit ( 0 );
    }

    if ( ( strcmp ( argv[1], "h") == 0 ) || 
         ( strcmp ( argv[1], "help") == 0 ) ||
         ( strcmp ( argv[1], "-h") == 0 ) || 
         ( strcmp ( argv[1], "-help") == 0 ) ) {
         printf("\nUSAGE: prob2cat VGF_file[ VGF_file ]\n\n");
         printf("reads VGF files containing risk of severe weather probabilities\n"
                "for DAY1, DAY2 and DAY3 outlooks and creates output VGF file\n"
                "containing categorical outlooks of severe weather risk.\n"
                "\nDAY1 and DAY2 input files include outlooks for individual hazards:\n"
                "tornado (TORN), hail (HAIL) and high winds (WIND)\n"
                "as well as enhanced thunderstorm outlook file.\n"
                "\nDAY2 and DAY3 input files contain probabilities of:\n"
                "severe weather without separating the risks by hazards.\n\n");
       exit ( 0 );
    }

    /*
     *  Get the output product 
     */
    if ( strstr ( argv[1], "0100") != (char*)NULL ) {
       interp_enh = G_TRUE;
    }
    maxch  = 30;
    maxnum = 5;
    strcpy (sep, "_");
    strcpy (defstr, "");
    strcpy (dumstr, "");
    cst_nocc ( argv[ 1 ], '.', 1, 0, &ipos,  &ier );
    strncpy ( dumstr, argv[ 1 ], ipos-1);
    dumstr [ipos-1] = '\0';
    stinarr = (char **)malloc((size_t)maxnum * sizeof(char *));
    for( ii=0; ii < maxnum; ii++ )
                   stinarr[ii] = (char *)malloc(maxch * sizeof(char));
    cst_clst (dumstr, sep[0], defstr, maxnum, maxch, stinarr, &istmax, &ier);
    strcpy ( outfile, "p2c_outlook_");
    cst_lcuc ( stinarr[ 1 ], tmstr, &ier );
    strcpy ( _Day_out, tmstr);
    strcat ( outfile, tmstr);
    strcat ( outfile, "_");
    strcpy ( datestamp, stinarr[2]);
    strcat ( datestamp, "Z");
    strcat ( outfile, datestamp);
    strcat ( outfile, ".vgf");
    cvg_crvgf ( outfile, &ier );
    cfl_inqr (  outfile, NULL, &ifilesize, ofname, &ier );
    ofptr = cfl_ropn(ofname, "", &ier);
    for ( ii = 0; ii < maxnum; ii++ ) free( stinarr[ii] );
    if( stinarr ) free( (char **)stinarr );
    /*
     *  Set EPSILON for GPC library
     */
    gpc_set_epsilon ( epsilon );

    /* 
     *  Initialize clo, device and projection
     */
    cgr_init ( &ier );
    clo_init ( &ier );

    ginitp ( &mode, &istat, &ier );

    gsdeva ( device, &iunit, dfilnam, &itype, &xsize, &ysize, &ier,
             strlen ( device ), strlen ( dfilnam ) );

    gsmprj ( proj, &prjang1, &prjang2, &prjang3,
             &lllat, &lllon, &urlat, &urlon, &ier, strlen ( proj ) );

    /*
     * Set the lats and lons for the Outlook Area polygon
     */
    _areaP = 0;
    p2c_SetArea ( &ier);

    /*
     *  Get the total number of elements
     */
    nIn = 0;
    nInEnh = 0;

    for ( ii = 1; ii < argc; ii++ ) {

	cfl_inqr ( argv[ ii ], NULL, &fileSize, vgfname, &ier );
        cvg_open ( vgfname, 0, &fptr, &ier );
        if ( ier != 0 )  {
           printf("Error opening VG file %s.\n", vgfname );
           printf("Skipping VG file %s.\n", vgfname );
	   continue;
        }

    	curPos 	= 0;
    	nextEl 	= 0;
    	ier 	= 0;

    	while ( nextEl < MAX_EDITABLE_ELEMS )  {

            cvg_rdrecnoc ( vgfname, fptr, curPos, &el, &ier );

	    if ( ier != 0 ) break;

            if ( el.hdr.recsz > 0 )  {

               curPos += el.hdr.recsz;

               if ( ( (int)el.hdr.vg_type == SPLN_ELM || 
                      (int)el.hdr.vg_type == SPTX_ELM ) && 
		        !el.hdr.delete  &&
                       ( el.hdr.grptyp == 12 ||			/* hail outlook  */
                         el.hdr.grptyp == 15 ||			/* day2 and day3 outlook */
                         el.hdr.grptyp == 13 ||			/* tornado outlook  */
                         el.hdr.grptyp == 14 ||         	/* wind outlook  */
                       ( el.hdr.grptyp == 0 && el.hdr.filled ) ) ) {
	          nIn++;
               }
               else if ( ( (int)el.hdr.vg_type == SPLN_ELM ||
                           (int)el.hdr.vg_type == SPTX_ELM ) &&
                           !el.hdr.delete  && 
		           !el.hdr.filled  &&
                            el.hdr.grptyp == 7 ) {		/* outlook  */
                  nInEnh++;
               }
               else if ( (int)el.hdr.vg_type == SPLN_ELM &&
                         ( el.hdr.grptyp == 0 && !el.hdr.filled ) ) {
                         printf("\n");
                         printf("*** WARNING: found contour of unknown group type ***\n");
                         printf("--> FINAL OUTLOOK may be affected                ***\n");
                         printf("--> Check groupe type of contours in file %s\n", vgfname);
                         printf("\n");
               }
               cvg_freeElPtr ( &el );
            }
            nextEl++;
        }						/* element loop  */
	cvg_clos ( fptr, &ier );
    } 						/* vgf file loop */


    if ( nIn > 0 ) {
     /*
      *  Loop over VG files to read hazard outlook elements
      */
      G_MALLOC ( elInTemp, VG_DBStruct, nIn, "prob2cat: elInTemp" ); 
      G_MALLOC ( elIn, VG_DBStruct, 10*nIn, "prob2cat: elIn" ); 
      G_MALLOC ( iclozd, int,  10*nIn, "prob2cat: iclozd" ); 
      G_MALLOC ( excludedGrp, int,  nIn, "prob2cat: excludedGrp" );
      nn = 0;
      jj = 0;
      for ( ii = 1; ii < argc; ii++ ) {

	cfl_inqr ( argv[ ii ], NULL, &fileSize, vgfname, &ier );
        cvg_open ( vgfname, 0, &fptr, &ier );
        if ( ier != 0 )  {
	   continue;
        }

	/*
	 *  Read outlook elements
	 */
    	curPos 	= 0;
    	nextEl 	= 0;
    	ier 	= 0;

        while ( ( nextEl < MAX_EDITABLE_ELEMS ) && ( jj < nIn ) )  {
                                                                                                                               
            cvg_rdrecnoc ( vgfname, fptr, curPos, &elInTemp[jj], &ier );

            iclozd[jj] = G_FALSE;
                                                                                                                               
            if ( ier != 0 ) break;

            if ( elInTemp[jj].hdr.recsz > 0 )  {

               curPos += elInTemp[jj].hdr.recsz;

               if ( ( (int)elInTemp[jj].hdr.vg_type == SPLN_ELM || 
                    (int)elInTemp[jj].hdr.vg_type == SPTX_ELM ) &&
		        !elInTemp[jj].hdr.delete  &&
                       ( elInTemp[jj].hdr.grptyp == 12 ||		
                         elInTemp[jj].hdr.grptyp == 15 ||		
                         elInTemp[jj].hdr.grptyp == 13 ||		
                         elInTemp[jj].hdr.grptyp == 14 ||
                       ( elInTemp[jj].hdr.grptyp == 0 && elInTemp[jj].hdr.filled ) ) ) {
                 /*
                   Check whether a probabilistic contour is flipped, if it is flipped, flip it again.
                 */ 
                  if ((int)elInTemp[jj].hdr.vg_type == SPLN_ELM && (int)elInTemp[jj].elem.spl.info.spldir == -1  ) {
                    kt =  elInTemp[jj].elem.spl.info.numpts;
                    for ( kk = 0; kk < kt/2; kk++ ) {
                      temp1 = elInTemp[jj].elem.spl.latlon[kk];
                      temp2 = elInTemp[jj].elem.spl.latlon[kk+kt];
                      elInTemp[jj].elem.spl.latlon[kk] = elInTemp[jj].elem.spl.latlon[kt-1-kk];
                      elInTemp[jj].elem.spl.latlon[kt-1-kk] = temp1;
                      elInTemp[jj].elem.spl.latlon[kk+kt]  = elInTemp[jj].elem.spl.latlon[kt+kt-1-kk];
                      elInTemp[jj].elem.spl.latlon[kt+kt-1-kk] = temp2;
                    }        
                    elInTemp[jj].elem.spl.info.spldir = 0;
                  }
                  if ( (int)elInTemp[jj].hdr.vg_type == SPLN_ELM && (!elInTemp[jj].hdr.closed) ) {
                    nElInClosed = 0;
                    p2c_CloseCntr ( &elInTemp[jj], &elInClosed, &nElInClosed, &ier);
                    for ( kk = 0; kk < nElInClosed; kk++ ) {
                      p2c_CopyCntr ( &elInClosed[kk], &elIn[nn], &ier);
                      nn++;
                    }
                    iclozd[jj] = G_TRUE;
                  }
                  if ( ( (int)elInTemp[jj].hdr.vg_type == SPLN_ELM ) && 
                       ( !iclozd[jj] ) && (elInTemp[jj].hdr.closed) ) {
                    p2c_CorctCntr  ( &elInTemp[ jj ], &ier);
                    p2c_CopyCntr ( &elInTemp[jj], &elIn[nn], &ier);
                    nn++;
                  }
                  
                  if ( (int)elInTemp[jj].hdr.vg_type == SPTX_ELM ) {
                    p2c_CopyLbl ( &elInTemp[jj], &elIn[nn], &ier);
                    nn++;
                  }   

                  jj++;

               }
            }
            nextEl++;
        }                                       /* element loop  */
	cvg_clos ( fptr, &ier );
      } 						/* vgf file loop */
    
      for ( ii = 0; ii < nIn; ii++ ) {
        if ( elIn[ ii ].hdr.vg_type == SPLN_ELM && !elIn[ ii ].hdr.filled ) {
            ilblgrp = G_FALSE;
            for ( kk = 0; kk < nIn; kk++ ) {
               if (  elIn[ kk ].hdr.vg_type == SPTX_ELM && 
                    (elIn[ ii ].hdr.grptyp == elIn[ kk ].hdr.grptyp)  &&
                    (elIn[ ii ].hdr.grpnum == elIn[ kk ].hdr.grpnum) ) {
                      ilblgrp = G_TRUE; 
               }
            }
            if ( !ilblgrp ) {
               printf (" not grouped with any labels\n");
               printf("*** WARNING: found ungrouped contour - FINAL OUTLOOK may be affected ***\n");
               printf("--> Check groupings of contours in the input files\n");
            }
        }
      }

    
     /*
      * Conversion prob to cat for individual hazards
      */
      p2c_Elms2Grps ( nn, elIn, &np2cgrp, &p2cGrp, origEl, &ier );

     /*
      * Get the unions of contours for each risk category 
      */
      nout = 0;
      p2c_Grp2Out ( np2cgrp, p2cGrp,  &outlk, &nout, &ier );

     /*
      * Adjust positions of the labels in relation to each other
      */
      p2c_AdjustLbl ( nout, outlk, &ier );

     /*
      * Write out the outlook contours
      */
      iExc =0;
      for ( ii = 0; ii < nout; ii++ ) {
        if ( !outlk[ii].hdr.closed && (int)outlk[ii].hdr.vg_type == SPLN_ELM ) {
          /*
           * Write out open contour
           */
           cvg_writef( &outlk[ii], -1, outlk[ii].hdr.recsz, outfile, FALSE, &loc, &ier );
        }
        else if ( outlk[ii].hdr.closed && (int)outlk[ii].hdr.vg_type == SPLN_ELM ) {
           p2c_OpenCntr ( &outlk[ ii ], &outlk2, &nseg, &ier);
          /*
           *  Exclude labels whose contour are not being written out
           */
           if ( nseg == 0 ) {
              for ( kk = 0; kk < nout; kk++ ) {
                 if ( ( outlk[ ii ].hdr.grpnum == outlk[ kk ].hdr.grpnum ) &&
                    ( (int)outlk[kk].hdr.vg_type == SPTX_ELM ) ) {
                      excludedGrp[iExc] = outlk[ii].hdr.grpnum;
                      iExc++;
                 }
              }
           }
           else {
             /*
              * Write out openED contour
              */
              for ( jj = 0; jj < nseg; jj++ ) {
                 cvg_writef( &outlk2[jj], -1, outlk2[jj].hdr.recsz, outfile, FALSE, &loc, &ier );
              }
           }
        }
      }

     /*
      * Write out the outlook contour labels
      */
      if ( iExc > 0 ) {
         for ( ii = 0; ii < nout; ii++ ) {
            if ( (int)outlk[ii].hdr.vg_type == SPTX_ELM ) {
               for ( jj = 0; jj < iExc; jj++ ) {
                   if ( outlk[ii].hdr.grpnum != excludedGrp[jj])  {
                      cvg_writef( &outlk[ii], -1, outlk[ii].hdr.recsz, outfile, FALSE, &loc, &ier );
                   }
               }
            }
         }
         G_FREE ( excludedGrp, int );
      }
      else {
        for ( ii = 0; ii < nout; ii++ ) {
            if ( (int)outlk[ii].hdr.vg_type == SPTX_ELM ) {
               cvg_writef( &outlk[ii], -1, outlk[ii].hdr.recsz, outfile, FALSE, &loc, &ier );
            }
         }
      }

     /*
      *  Free allocated memory.
      */
      for ( ii = 0; ii < np2cgrp; ii++ ) {
        G_FREE ( p2cGrp[ii].high, VG_DBStruct * );
        G_FREE ( p2cGrp[ii].mdrt, VG_DBStruct * );
        G_FREE ( p2cGrp[ii].enhc, VG_DBStruct * );
        G_FREE ( p2cGrp[ii].slgt, VG_DBStruct * );
        G_FREE ( p2cGrp[ii].mrgl, VG_DBStruct * );
        G_FREE ( p2cGrp[ii].text, VG_DBStruct * );
      }
      G_FREE ( origEl, VG_DBStruct );
      G_FREE ( iclozd, int );
      G_FREE ( p2cGrp, P2C_Grp );
      G_FREE ( elIn, VG_DBStruct );
      G_FREE ( elGt, VG_DBStruct );
      G_FREE ( outlk, VG_DBStruct );
      G_FREE ( outlk2, VG_DBStruct );
    }
    else {
      printf ("*** WARNING: No hazard elements found in the input files ***\n");
    }

    if ( nInEnh > 0 ) {

     /*
      *  Loop over VG files to read enhanced t-storm outlook elements
      */
      G_MALLOC ( el_enh, VG_DBStruct, nInEnh, "prob2cat: el_enh" ); 
      jj = 0;
      for ( ii = 1; ii < argc; ii++ ) {

	cfl_inqr ( argv[ ii ], NULL, &fileSize, vgfname, &ier );
        cvg_open ( vgfname, 0, &fptr, &ier );
        if ( ier != 0 )  {
	   continue;
        }

	/*
	 *  Read enh t-storm outlook elements
	 */

    	curPos 	= 0;
    	nextEl 	= 0;
    	ier 	= 0;

        while ( ( nextEl < MAX_EDITABLE_ELEMS ) && ( jj < nInEnh ) )  {
            cvg_rdrecnoc ( vgfname, fptr, curPos, &el_enh[ jj ], &ier );
            if ( ier != 0 ) break;
            if ( el_enh[ jj ].hdr.recsz > 0 )  {
               curPos += el_enh[ jj ].hdr.recsz;
               if ( ( (int)el_enh[jj].hdr.vg_type == SPLN_ELM || 
                    (int)el_enh[jj].hdr.vg_type == SPTX_ELM ) &&
		        !el_enh[jj].hdr.delete  &&
                         el_enh[jj].hdr.grptyp == 7 ) {		/* outlook  */

                  jj++;
               }
            }
            nextEl++;
        }                                       	/* element loop  */
	cvg_clos ( fptr, &ier );
      } 						/* vgf file loop */
     /*
      * Add "General Thunderstorm" line that corresponds to 
      * "10%" probability of thunderstorm line from 
      * enhanced outlook product VGF.
      */
      strcpy (Gt_str, "10%");
      nGt = 0;
      nout_gent = 0;
     /*
      * Get "10% General Thunderstorm" line
      */
      p2c_GetGenT ( Gt_str, nInEnh, el_enh, &nGt, &elGt, &ier );
     /*
      * Add "General Thunderstorm" line
      */
      p2c_AddGenT ( interp_enh, nGt, elGt, &nout_gent, &out_gent, &ier );
     /*
      * Write out "General Thunderstorm" contours
      */
      for ( ii = 0; ii < nout_gent; ii++ ) {
        cvg_writef( &out_gent[ii], -1, out_gent[ii].hdr.recsz, outfile, FALSE, &loc, &ier );
      }
      cfl_clos ( ofptr, &ier );
     /*
      *  Free allocated memory.
      */
      G_FREE ( el_enh, VG_DBStruct );
      G_FREE ( out_gent, VG_DBStruct );
    }
    else {
      printf ("*** WARNING: No Enhanced Thunderstorm Outlook elements found in the input files ***\n");
    }

    return 0;

}

/*=====================================================================*/

static void p2c_Elms2Grps ( int nin, VG_DBStruct *el_in, int *ngrp, 
		P2C_Grp **p2cGrp, VG_DBStruct *origEl, int *iret )
/************************************************************************
 * p2c_Elms2Grps                                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *nin            int             number of input elements        *
 *      *el_in          VG_DBStruct     array of outlook elements       *
 *                                                                      *
 * Output parameters:                                                   *
 *      *ngrp           int             number of output elements       *
 *      **p2cGrp        P2C_Grp         array of P2C_Grp elements	*
 *      *origEl         VG_DBStruct     array of original elements      *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 * B.Yin/SGT		11/14	Added the original into groups.       	*
 ***********************************************************************/
{
    int                 kk, ii, jj, ier, one = 1, iprob1, iprob2;
    char                hazType1[8], hazType2[8], cat1[6], cat2[6];
    char		**origCat;
    int                 *grouped, nbump;
/*---------------------------------------------------------------------*/
    *iret = 0;

    origCat = NULL;
    nbump = 0;

    /*
     *  Set the flags - each SPLN element should only be grouped once.
     *  Make a copy of all outlooks that will be bumped.
     */
    G_MALLOC ( grouped, int, nin, "p2c_elms2grp: grouped" );
    for ( ii = 0; ii < nin; ii++ ) {
         grouped[ ii ] = G_FALSE;

        iprob1 = 0;
        for ( kk = 0; kk < nin; kk++ ) {
           if (  el_in[ kk ].hdr.vg_type == SPTX_ELM && 
                (el_in[ ii ].hdr.grptyp == el_in[ kk ].hdr.grptyp)  &&
                (el_in[ ii ].hdr.grpnum == el_in[ kk ].hdr.grpnum) ) {
              iprob1 = atoi ( el_in[ kk ].elem.spt.text );
           }
        }

        strcpy (cat1, "");
        if ( el_in[ii].hdr.filled ) continue;
        p2c_GetCat ( &el_in[ ii ], iprob1, hazType1, cat1, &nbump, 
			&origEl, &origCat, 0, &ier );
    }

    /*
     *  Group the outlooks by category.
     */
    *ngrp = 0;
    for ( ii = 0; ii < nin; ii++ ) {
        /*
         *  Skip non-spln elements
         */
        if (  grouped[ii] ) {
            continue;
        }
        else if ( el_in[ ii ].hdr.vg_type != SPLN_ELM || el_in[ ii ].hdr.filled ) {
            continue;
        }
        /*
         *  Add a new P2C_Grp element
         */
        if ( *ngrp == 0 ) {
            G_MALLOC ( (*p2cGrp), P2C_Grp, one, "p2c_elms2grp: p2cGrp" );
        }
        else {
            G_REALLOC ( (*p2cGrp), P2C_Grp, *ngrp + 1, "p2c_elms2grp: p2cGrp" );
        }

        (*p2cGrp)[*ngrp].nhigh = 0;
        (*p2cGrp)[*ngrp].nmdrt = 0;
        (*p2cGrp)[*ngrp].nenhc = 0;
        (*p2cGrp)[*ngrp].nslgt = 0;
        (*p2cGrp)[*ngrp].nmrgl = 0;
        (*p2cGrp)[*ngrp].ntext = 0;
        (*p2cGrp)[*ngrp].high = (VG_DBStruct **)NULL;
        (*p2cGrp)[*ngrp].mdrt = (VG_DBStruct **)NULL;
        (*p2cGrp)[*ngrp].enhc = (VG_DBStruct **)NULL;
        (*p2cGrp)[*ngrp].slgt = (VG_DBStruct **)NULL;
        (*p2cGrp)[*ngrp].mrgl = (VG_DBStruct **)NULL;
        (*p2cGrp)[*ngrp].text = (VG_DBStruct **)NULL;

        iprob1 = 0;
        for ( kk = 0; kk < nin; kk++ ) {
           if (  el_in[ kk ].hdr.vg_type == SPTX_ELM && 
                (el_in[ ii ].hdr.grptyp == el_in[ kk ].hdr.grptyp)  &&
                (el_in[ ii ].hdr.grpnum == el_in[ kk ].hdr.grpnum) ) {
              iprob1 = atoi ( el_in[ kk ].elem.spt.text );
           }
        }

        strcpy (cat1, "");
        p2c_GetCat ( &el_in[ ii ], iprob1, hazType1, cat1, &nbump, 
			&origEl, &origCat, 1, &ier );

        /*
         * Find records of Different Categories
         */
        if      ( strcmp (cat1, "_HIGH") == 0 ) {
            G_MALLOC ( (*p2cGrp)[*ngrp].high, VG_DBStruct *, one,
                                   "p2c_elms2grp: p2cGrp.high" );
            (*p2cGrp)[*ngrp].high[0] = &el_in[ii];
            ( (*p2cGrp)[*ngrp].nhigh )++;
        }
        else if ( strcmp (cat1, "_MDRT") == 0 ) {
            G_MALLOC ( (*p2cGrp)[*ngrp].mdrt, VG_DBStruct *, one,
                                   "p2c_elms2grp: p2cGrp.mdrt" );
            (*p2cGrp)[*ngrp].mdrt[0] = &el_in[ii];
            ( (*p2cGrp)[*ngrp].nmdrt )++;
        }
        else if ( strcmp (cat1, "_ENHC") == 0 ) {
            G_MALLOC ( (*p2cGrp)[*ngrp].enhc, VG_DBStruct *, one,
                                   "p2c_elms2grp: p2cGrp.enhc" );
            (*p2cGrp)[*ngrp].enhc[0] = &el_in[ii];
            ( (*p2cGrp)[*ngrp].nenhc )++;

        }
        else if ( strcmp (cat1, "_SLGT") == 0 ) {
            G_MALLOC ( (*p2cGrp)[*ngrp].slgt, VG_DBStruct *, one,
                                   "p2c_elms2grp: p2cGrp.slgt" );
            (*p2cGrp)[*ngrp].slgt[0] = &el_in[ii];
            ( (*p2cGrp)[*ngrp].nslgt )++;
        }
        else if ( strcmp (cat1, "_MRGL") == 0 ) {
            G_MALLOC ( (*p2cGrp)[*ngrp].mrgl, VG_DBStruct *, one,
                                   "p2c_elms2grp: p2cGrp.mrgl" );
            (*p2cGrp)[*ngrp].mrgl[0] = &el_in[ii];
            ( (*p2cGrp)[*ngrp].nmrgl )++;
        }
        else if ( strcmp (cat1, "_TEXT") == 0 ) {
            G_MALLOC ( (*p2cGrp)[*ngrp].text, VG_DBStruct *, one,
                                   "p2c_elms2grp: p2cGrp.text" );
            (*p2cGrp)[*ngrp].text[0] = &el_in[ii];
            ( (*p2cGrp)[*ngrp].ntext )++;
        }

        /*
         *  Find the rest of P2C_Grp elements with same hazard type & cat
         */
        for ( jj = (ii + 1); jj < nin; jj++ ) {
            if (  el_in[ jj ].hdr.vg_type != SPLN_ELM || el_in[ jj ].hdr.filled ) {
               continue;
            }

            strcpy (cat2, "");
            iprob2 = 0;

            if ( grouped[ jj ] ) {
                continue;
            }

            for ( kk = 0; kk < nin; kk++ ) {
               if (  el_in[ kk ].hdr.vg_type == SPTX_ELM && 
                  (  el_in[ jj ].hdr.grptyp == el_in[ kk ].hdr.grptyp)  &&
                    (el_in[ jj ].hdr.grpnum == el_in[ kk ].hdr.grpnum) ) {
                   iprob2 = atoi ( el_in[ kk ].elem.spt.text );
              }
            }

            p2c_GetCat ( &el_in[ jj ], iprob2, hazType2, cat2, &nbump, 
				&origEl, &origCat, 1, &ier );

            if ( (strcasecmp ( cat1, cat2 ) == 0 ) &&
                 ( cat1[0]  != '\0' ) ) {

               if      ( strcmp (cat2, "_HIGH") == 0 ) {
                   if ( (*p2cGrp)[*ngrp].nhigh == 0 ) {
                     G_MALLOC ( (*p2cGrp)[*ngrp].high, VG_DBStruct *, one,
                                 "p2c_elms2grp: p2cGrp.high" );
                   }
                   else {
                      G_REALLOC ( (*p2cGrp)[*ngrp].high, VG_DBStruct *,
                                (*p2cGrp)[*ngrp].nhigh + 1,
                                 "p2c_elms2grp: p2cGrp.high" );
                   }
                   (*p2cGrp)[*ngrp].high[ (*p2cGrp)[*ngrp].nhigh ] = &el_in[ jj ];
                   ( (*p2cGrp)[*ngrp].nhigh )++;
                   grouped[ jj ] = True;
               }
               else if ( strcmp (cat2, "_MDRT") == 0 ) {
                   if ( (*p2cGrp)[*ngrp].nmdrt == 0 ) {
                     G_MALLOC ( (*p2cGrp)[*ngrp].mdrt, VG_DBStruct *, one,
                                 "p2c_elms2grp: p2cGrp.mdrt" );
                   }
                   else {
                     G_REALLOC ( (*p2cGrp)[*ngrp].mdrt, VG_DBStruct *,
                                 (*p2cGrp)[*ngrp].nmdrt + 1,
                                  "p2c_elms2grp: p2cGrp.mdrt" );
                   }
                   (*p2cGrp)[*ngrp].mdrt[ (*p2cGrp)[*ngrp].nmdrt ] = &el_in[ jj ];
                   ( (*p2cGrp)[*ngrp].nmdrt )++;
                   grouped[ jj ] = True;
               }
               else if ( strcmp (cat2, "_ENHC") == 0 ) {
                   if ( (*p2cGrp)[*ngrp].nenhc == 0 ) {
                     G_MALLOC ( (*p2cGrp)[*ngrp].enhc, VG_DBStruct *, one,
                                "p2c_elms2grp: p2cGrp.enhc" );
                   }
                   else {
                     G_REALLOC ( (*p2cGrp)[*ngrp].enhc, VG_DBStruct *,
                                 (*p2cGrp)[*ngrp].nenhc + 1,
                                  "p2c_elms2grp: p2cGrp.enhc" );
                   }
                   (*p2cGrp)[*ngrp].enhc[ (*p2cGrp)[*ngrp].nenhc ] = &el_in[ jj ];
                    ( (*p2cGrp)[*ngrp].nenhc )++;
                   grouped[ jj ] = True;
               }
               else if ( strcmp (cat2, "_SLGT") == 0 ) {
                   if ( (*p2cGrp)[*ngrp].nslgt == 0 ) {
                     G_MALLOC ( (*p2cGrp)[*ngrp].slgt, VG_DBStruct *, one,
                                "p2c_elms2grp: p2cGrp.slgt" );
                   }
                   else {
                     G_REALLOC ( (*p2cGrp)[*ngrp].slgt, VG_DBStruct *,
                                 (*p2cGrp)[*ngrp].nslgt + 1,
                                  "p2c_elms2grp: p2cGrp.slgt" );
                   }
                   (*p2cGrp)[*ngrp].slgt[ (*p2cGrp)[*ngrp].nslgt ] = &el_in[ jj ];
                    ( (*p2cGrp)[*ngrp].nslgt )++;
                   grouped[ jj ] = True;
               }
               else if ( strcmp (cat2, "_MRGL") == 0 ) {
                   if ( (*p2cGrp)[*ngrp].nmrgl == 0 ) {
                     G_MALLOC ( (*p2cGrp)[*ngrp].mrgl, VG_DBStruct *, one,
                                "p2c_elms2grp: p2cGrp.mrgl" );
                   }
                   else {
                     G_REALLOC ( (*p2cGrp)[*ngrp].mrgl, VG_DBStruct *,
                                 (*p2cGrp)[*ngrp].nmrgl + 1,
                                  "p2c_elms2grp: p2cGrp.mrgl" );
                   }
                   (*p2cGrp)[*ngrp].mrgl[ (*p2cGrp)[*ngrp].nmrgl ] = &el_in[ jj ];
                    ( (*p2cGrp)[*ngrp].nmrgl )++;
                   grouped[ jj ] = True;
               }
               else if ( strcmp (cat2, "_TEXT") == 0 ) {
                   if ( (*p2cGrp)[*ngrp].ntext == 0 ) {
                     G_MALLOC ( (*p2cGrp)[*ngrp].text, VG_DBStruct *, one,
                                "p2c_elms2grp: p2cGrp.text" );
                   }
                   else {
                     G_REALLOC ( (*p2cGrp)[*ngrp].text, VG_DBStruct *,
                                 (*p2cGrp)[*ngrp].ntext + 1,
                                  "p2c_elms2grp: p2cGrp.text" );
                   }
                   (*p2cGrp)[*ngrp].text[ (*p2cGrp)[*ngrp].ntext ] = &el_in[ jj ];
                   ( (*p2cGrp)[*ngrp].ntext )++;
                   grouped[ jj ] = True;
               }
            }
        
        }
        (*ngrp)++;    /* increment the number of output elements */
    }
    G_FREE ( grouped, int );

    /*
     * Added the original outlooks that have been bumped in the group
     */  
    p2c_AddOrig2Grp (ngrp, p2cGrp, nbump, origEl, origCat, &ier );

}
/*=====================================================================*/

static void p2c_AddOrig2Grp ( int *ngrp, P2C_Grp **p2cGrp, int nbump,
			      VG_DBStruct *origEl, char **origCat, int *iret )
/************************************************************************
 * p2c_AddOrig2Grp							*
 *                                                                      *
 * Adds the original outlooks that will be bumped to higher category    *
 * into the P2C_Grp group. (The intersaction between the original and   *
 * the filled area will be bumped in a higher category, but the original*
 * will be treated as one of its original category.)			*
 *                                                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *      nbump           int             number of bumped el        	*
 *      **origEl        VG_DBStruct     original elements of the bumped *
 *      **origCat       char     	original categories of bumped el*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *ngrp           int             number of P2C_Grp        	*
 *      *p2cGrp         P2C_Grp  	array of P2C_Grp         	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SGT		11/14		Created                        	*
 * B. Yin/SGT		02/15		Don't free origEl              	*
 ***********************************************************************/
{
    int 	ii, jj, added, one = 1;
 /*---------------------------------------------------------------------*/

    *iret = 0;
    added = 0;

    if ( nbump != 0 ) { 
       /*
 	* Loop through all original outlooks that will be bumped.
 	*/  
       for ( ii = 0; ii < nbump; ii++ ) {
	 added  = 0;
	 /*
 	  * Add the original into ithe grouip of its category 
 	  */
         for ( jj = 0; jj < *ngrp; jj++ ) {
               if      ( strcmp (origCat[ii], "_HIGH") == 0 ) {
                   if ( (*p2cGrp)[jj].nhigh != 0 ) {
                      G_REALLOC ( (*p2cGrp)[jj].high, VG_DBStruct *,
                                (*p2cGrp)[jj].nhigh + 1,
                                 "p2c_AddOrig2Grp: p2cGrp.high" );
                   (*p2cGrp)[jj].high[ (*p2cGrp)[jj].nhigh ] = &origEl[ ii ];
                   ( (*p2cGrp)[jj].nhigh )++;
		   added = 1;
		  }
               }
               else if ( strcmp (origCat[ii], "_MDRT") == 0 ) {
                   if ( (*p2cGrp)[jj].nmdrt != 0 ) {
                     G_REALLOC ( (*p2cGrp)[jj].mdrt, VG_DBStruct *,
                                 (*p2cGrp)[jj].nmdrt + 1,
                                  "p2c_AddOrig2Grp: p2cGrp.mdrt" );
                   (*p2cGrp)[jj].mdrt[ (*p2cGrp)[jj].nmdrt ] = &origEl[ ii ];
                   ( (*p2cGrp)[jj].nmdrt )++;
		   added = 1;
                   }
               }
               else if ( strcmp (origCat[ii], "_ENHC") == 0 ) {
                   if ( (*p2cGrp)[jj].nenhc != 0 ) {
                     G_REALLOC ( (*p2cGrp)[jj].enhc, VG_DBStruct *,
                                 (*p2cGrp)[jj].nenhc + 1,
                                  "p2c_AddOrig2Grp: p2cGrp.enhc" );
                   (*p2cGrp)[jj].enhc[ (*p2cGrp)[jj].nenhc ] = &origEl[ ii ];
                    ( (*p2cGrp)[jj].nenhc )++;
		   added = 1;
                   }
               }
               else if ( strcmp (origCat[ii], "_SLGT") == 0 ) {
                   if ( (*p2cGrp)[jj].nslgt != 0 ) {
                     G_REALLOC ( (*p2cGrp)[jj].slgt, VG_DBStruct *,
                                 (*p2cGrp)[jj].nslgt + 1,
                                  "p2c_AddOrig2Grp: p2cGrp.slgt" );
                   (*p2cGrp)[jj].slgt[ (*p2cGrp)[jj].nslgt ] = &origEl[ ii ];
                    ( (*p2cGrp)[jj].nslgt )++;
		   added = 1;
                   }
               }
               else if ( strcmp (origCat[ii], "_MRGL") == 0 ) {
                   if ( (*p2cGrp)[jj].nmrgl != 0 ) {
                     G_REALLOC ( (*p2cGrp)[jj].mrgl, VG_DBStruct *,
                                 (*p2cGrp)[jj].nmrgl + 1,
                                  "p2c_AddOrig2Grp: p2cGrp.mrgl" );
                   (*p2cGrp)[jj].mrgl[ (*p2cGrp)[jj].nmrgl ] = &origEl[ ii ];
                    ( (*p2cGrp)[jj].nmrgl )++;
		   added = 1;
		    }
               }
               else if ( strcmp (origCat[ii], "_TEXT") == 0 ) {
                   if ( (*p2cGrp)[jj].ntext != 0 ) {
                     G_REALLOC ( (*p2cGrp)[jj].text, VG_DBStruct *,
                                 (*p2cGrp)[jj].ntext + 1,
                                  "p2c_AddOrig2Grp: p2cGrp.text" );
                   (*p2cGrp)[jj].text[ (*p2cGrp)[jj].ntext ] = &origEl[ ii ];
                   ( (*p2cGrp)[jj].ntext )++;
		   added = 1;
                   }
               }
	}

	if ( added  == 0 ) {
           /*
            *  Add a new P2C_Grp element
            */
           if ( *ngrp == 0 ) {
              G_MALLOC ( (*p2cGrp), P2C_Grp, one, "p2c_AddOrig2Grp: p2cGrp" );
           }  
           else {
              G_REALLOC ( (*p2cGrp), P2C_Grp, *ngrp + 1, "p2c_AddOrig2Grp: p2cGrp" );
           } 

           (*p2cGrp)[*ngrp].nhigh = 0;
           (*p2cGrp)[*ngrp].nmdrt = 0;
           (*p2cGrp)[*ngrp].nenhc = 0;
           (*p2cGrp)[*ngrp].nslgt = 0;
           (*p2cGrp)[*ngrp].nmrgl = 0;
           (*p2cGrp)[*ngrp].ntext = 0;
           (*p2cGrp)[*ngrp].high = (VG_DBStruct **)NULL;
           (*p2cGrp)[*ngrp].mdrt = (VG_DBStruct **)NULL;
           (*p2cGrp)[*ngrp].enhc = (VG_DBStruct **)NULL;
           (*p2cGrp)[*ngrp].slgt = (VG_DBStruct **)NULL;
           (*p2cGrp)[*ngrp].mrgl = (VG_DBStruct **)NULL;
           (*p2cGrp)[*ngrp].text = (VG_DBStruct **)NULL;

           if      ( strcmp (origCat[ii], "_HIGH") == 0 ) {
              G_MALLOC ( (*p2cGrp)[*ngrp].high, VG_DBStruct *, one,
                                   "p2c_AddOrig2Grp: p2cGrp.high" );
              (*p2cGrp)[*ngrp].high[0] = &origEl[ ii ];
              ( (*p2cGrp)[*ngrp].nhigh )++;
           }
           else if ( strcmp (origCat[ii], "_MDRT") == 0 ) {
              G_MALLOC ( (*p2cGrp)[*ngrp].mdrt, VG_DBStruct *, one,
                                   "p2c_AddOrig2Grp: p2cGrp.mdrt" );
              (*p2cGrp)[*ngrp].mdrt[0] = &origEl[ ii ];
              ( (*p2cGrp)[*ngrp].nmdrt )++;
           }
           else if ( strcmp (origCat[ii], "_ENHC") == 0 ) {
              G_MALLOC ( (*p2cGrp)[*ngrp].enhc, VG_DBStruct *, one,
                                   "p2c_AddOrig2Grp: p2cGrp.enhc" );
              (*p2cGrp)[*ngrp].enhc[0] = &origEl[ ii ];
              ( (*p2cGrp)[*ngrp].nenhc )++;
           }
           else if ( strcmp (origCat[ii], "_SLGT") == 0 ) {
              G_MALLOC ( (*p2cGrp)[*ngrp].slgt, VG_DBStruct *, one,
                                   "p2c_AddOrig2Grp: p2cGrp.slgt" );
              (*p2cGrp)[*ngrp].slgt[0] = &origEl[ ii ];
              ( (*p2cGrp)[*ngrp].nslgt )++;
           }
           else if ( strcmp (origCat[ii], "_MRGL") == 0 ) {
              G_MALLOC ( (*p2cGrp)[*ngrp].mrgl, VG_DBStruct *, one,
                                   "p2c_AddOrig2Grp: p2cGrp.mrgl" );
              (*p2cGrp)[*ngrp].mrgl[0] = &origEl[ ii ];
              ( (*p2cGrp)[*ngrp].nmrgl )++;
           }
           else if ( strcmp (origCat[ii], "_TEXT") == 0 ) {
              G_MALLOC ( (*p2cGrp)[*ngrp].text, VG_DBStruct *, one,
                                   "p2c_AddOrig2Grp: p2cGrp.text" );
              (*p2cGrp)[*ngrp].text[0] = &origEl[ ii ];
              ( (*p2cGrp)[*ngrp].ntext )++;
           }
	   (*ngrp)++;

	}

     }
     
     for ( ii = 0; ii < nbump; ii++ ){
    		G_FREE ( origCat[ii], char );
     }
     G_FREE ( origCat, char* );
   }
}

/*=====================================================================*/

static void p2c_Grp2Out ( int ngrp, P2C_Grp *p2cGrp, VG_DBStruct **outlk,
                          int *nout, int *iret )
/************************************************************************
 * p2c_Grp2Out								*
 *                                                                      *
 * Input parameters:                                                    *
 *      ngrp            int             number of P2C_Grp        	*
 *      *p2cGrp         P2C_Grp  	array of P2C_Grp         	*
 *                                                                      *
 * Output parameters:                                                   *
 *      **out           VG_DBStruct     array of outlook elements       *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 ***********************************************************************/
{
    int                 ii, ier, iclr;
    char                cat[9];
 /*---------------------------------------------------------------------*/

    *iret = 0;
    *nout = 0;

    for ( ii = 0; ii < ngrp; ii++ ) {
        if ( p2cGrp[ ii ].high != (VG_DBStruct **)NULL ) {
         strcpy (cat, "HIGH");
         iclr = 7;
         p2c_PolyClip ( p2cGrp[ii].nhigh, p2cGrp[ii].high, cat, iclr, 
                        nout, outlk, &ier );
        }

        if ( p2cGrp[ ii ].mdrt != (VG_DBStruct **)NULL ) {
         strcpy (cat, "MDT");
         iclr = 2;
         p2c_PolyClip ( p2cGrp[ii].nmdrt, p2cGrp[ii].mdrt, cat, iclr,
                        nout, outlk, &ier );
        }

        if ( p2cGrp[ ii ].enhc != (VG_DBStruct **)NULL ) {
         strcpy (cat, "ENH");
         iclr = 17;
         p2c_PolyClip ( p2cGrp[ii].nenhc, p2cGrp[ii].enhc, cat, iclr,
                        nout, outlk, &ier );
        }

        if ( p2cGrp[ ii ].slgt != (VG_DBStruct **)NULL ) {
         strcpy (cat, "SLGT");
         iclr = 23;
         p2c_PolyClip ( p2cGrp[ii].nslgt, p2cGrp[ii].slgt, cat, iclr,
                        nout, outlk, &ier );
        }

        if ( p2cGrp[ ii ].mrgl != (VG_DBStruct **)NULL ) {
         strcpy (cat, "MRGL");
         iclr = 6;
         p2c_PolyClip ( p2cGrp[ii].nmrgl, p2cGrp[ii].mrgl, cat, iclr,
                        nout, outlk, &ier );
        }

        if ( p2cGrp[ ii ].text != (VG_DBStruct **)NULL ) {
         strcpy (cat, "SEE TEXT");
         iclr = 26;
         p2c_PolyClip ( p2cGrp[ii].ntext, p2cGrp[ii].text, cat, iclr,
                        nout, outlk, &ier );
        }
    }
}

/*=====================================================================*/

static void p2c_PolyClip ( int ncntrs, VG_DBStruct **cntrs, char *cat, int iclr,
                           int *numOut, VG_DBStruct **outlk, int *iret )
/************************************************************************
 * p2c_PolyClip                                                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      ncntrs          int             number of contours		*
 *      **cntrs         VG_DBStruct     array of contours		*
 *	cat		char		contour category		*
 *      iclr 	       	int             category color			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *numOut  	int             number of outlook elements      *
 *      **outlk         VG_DBStruct     array of outlook elements	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 * m.gamazaychikov/SAIC	05/08	Set the label group to contour group,	*
 * 				set label offset, cleaned up		*
 * m.gamazaychikov/SAIC	05/08	Changed the label location		*
 * S. Guan/NCEP         06/16   Fixed a logical bug                     *
 ***********************************************************************/
{
    int         jj, kk, ier, lens,nip, one = 1, npu, nuni;
    int		ilablat, ilablon, nint, maxp = 100, found, ii, type1, type2;
    int         bp1 [MAXPTS], ap1[MAXPTS], bp2[MAXPTS], ap2[MAXPTS];
    int 	np1, np2, intpoly[100], *inout, npsmall, npmx = MAXPTS;
    int		hazint[100], hazgrp[100], grup1, grup2;
    float       area_kk, area_jj, xC, yC, tlat, tlon;
    float       ipx[MAXPTS], ipy[MAXPTS];
    float       *xnormal1, *ynormal1, *xnormal2, *ynormal2;
    gpc_polygon gpc_poly_int, gpc_poly_union, gpc_poly_tmp[10],
                gpc_poly_0, gpc_poly_1;
 /*--------------------------------------------------------------------*/
    *iret = 0;
     G_MALLOC ( xnormal1, float, npmx, "p2c_PolyClip: xnormal1" );
     G_MALLOC ( ynormal1, float, npmx, "p2c_PolyClip: ynormal1" );
     G_MALLOC ( xnormal2, float, npmx, "p2c_PolyClip: xnormal2" );
     G_MALLOC ( ynormal2, float, npmx, "p2c_PolyClip: ynormal2" );

   /*
    *  First test if the polygons must be "unified".
    *  Only polygons of the same hazard type can be "unified".
    */
    if ( ncntrs  == 1 ) {
       p2c_Elm2Poly ( *cntrs[0], &gpc_poly_0, &ier );
       p2c_Elm2Poly ( *cntrs[0], &gpc_poly_1, &ier );
       gpc_polygon_clip ( GPC_UNION, &gpc_poly_0, &gpc_poly_1,
                          &gpc_poly_union );
       gpc_free_polygon ( &gpc_poly_0 );
       gpc_free_polygon ( &gpc_poly_1 );
    }
    if ( ncntrs == 2 ) {
     p2c_Util ( *cntrs[0], &np1, &type1, &grup1, xnormal1, ynormal1);
     p2c_Util ( *cntrs[1], &np2, &type2, &grup2, xnormal2, ynormal2);
     if ( type1 == type2 && grup1 == grup2 ) {
       cgr_intersect ( sys_M, &np1, xnormal1, ynormal1,
                       sys_M, &np2, xnormal2, ynormal2,
                       &maxp, sys_M, &nip, ipx, ipy,
                       bp1, ap1, bp2, ap2, &ier ); 
       if ( nip > 0 ) {
         p2c_Elm2Poly ( *cntrs[0], &gpc_poly_0, &ier );
         p2c_Elm2Poly ( *cntrs[1], &gpc_poly_1, &ier );
         gpc_polygon_clip ( GPC_INT, &gpc_poly_0, &gpc_poly_1,
                          &gpc_poly_union );
         gpc_free_polygon ( &gpc_poly_0 );
         gpc_free_polygon ( &gpc_poly_1 );
       }
       else {
         p2c_Elm2Poly ( *cntrs[0], &gpc_poly_0, &ier );
         p2c_Elm2Poly ( *cntrs[1], &gpc_poly_1, &ier );
         gpc_polygon_clip ( GPC_UNION, &gpc_poly_0, &gpc_poly_1,
                            &gpc_poly_union );
         gpc_free_polygon ( &gpc_poly_0 );
         gpc_free_polygon ( &gpc_poly_1 );
       }
     }
     else {
         p2c_Elm2Poly ( *cntrs[0], &gpc_poly_0, &ier );
         p2c_Elm2Poly ( *cntrs[1], &gpc_poly_1, &ier );
         gpc_polygon_clip ( GPC_UNION, &gpc_poly_0, &gpc_poly_1,
                            &gpc_poly_union );
         gpc_free_polygon ( &gpc_poly_0 );
         gpc_free_polygon ( &gpc_poly_1 );
     }
    }

    if ( ncntrs > 2 ) {
       nint = 0;
       for ( kk = 0; kk < 100; kk++ ) {
         intpoly[kk] = 0;
         hazint [kk] = 0;
         hazgrp [kk] = 0;
       }
      /*
       *  Go through each contour to search for intersecting ones 
       *  and set intpoly to -2 for them
       */
       for ( kk = 0; kk < ncntrs; kk++ ) {
         /* 
          * Skip already accounted for contours
          */
          if ( intpoly[kk] == -2 ) continue;

          for ( jj = kk+1; jj < ncntrs; jj++ ) {
           /* 
            * Skip already accounted for contours
            */
            if ( intpoly[jj] == -2 ) continue;

            p2c_Util ( *cntrs[kk], &np1, &type1, &grup1, xnormal1, ynormal1);
            p2c_Util ( *cntrs[jj], &np2, &type2, &grup2, xnormal2, ynormal2);
            if ( type1 == type2 && grup1 == grup2) {
                cgr_intersect ( sys_M, &np1, xnormal1, ynormal1,
                                sys_M, &np2, xnormal2, ynormal2,
                                &maxp, sys_M, &nip, ipx, ipy,
                                bp1, ap1, bp2, ap2, &ier ); 
                if ( nip > 0 ) {
                   cgr_centroid ( xnormal1, ynormal1, &np1,
                                 &xC, &yC,
                                 &area_kk, &ier );
                   cgr_centroid ( xnormal2, ynormal2, &np2, 
                                 &xC, &yC,
                                 &area_jj, &ier );
                   if ( area_kk > area_jj ) {
                        G_MALLOC ( inout, int, np2, "p2c_PolyClip: inout" );
                        cgr_inpoly ( sys_M, &np2, xnormal2, ynormal2, 
                                     sys_M, &np1, xnormal1, ynormal1,
                                     inout, &ier );
                        npsmall = np2;
                   }
                   else if ( area_kk <= area_jj ) {
                        G_MALLOC ( inout, int, np1, "p2c_PolyClip: inout" );
                        cgr_inpoly ( sys_M, &np1, xnormal1, ynormal1, 
                                     sys_M, &np2, xnormal2, ynormal2,
                                     inout, &ier );
                        npsmall = np1;
                   }
                                                                                                           
                   found = G_FALSE;
                   ii = 0;
                   while ( !found && ii < npsmall )  {
                      if ( inout[ii] == 0 )  {
                         found = G_TRUE;
                         break;
                      }
                      ii++;
                   }
                   G_FREE ( inout, int );
                   if ( found )  {
                    /*
                     *  Store intersection hazard type and group number
                     */
                     hazint[nint] = type1;
                     hazgrp[nint] = grup1;
                     intpoly[kk] = -2;
                     intpoly[jj] = -2;
                     p2c_Elm2Poly ( *cntrs[kk], &gpc_poly_0, &ier );
                     p2c_Elm2Poly ( *cntrs[jj], &gpc_poly_1, &ier );
                     gpc_polygon_clip ( GPC_INT, &gpc_poly_0, &gpc_poly_1,
                          &gpc_poly_tmp[nint] );
                     gpc_free_polygon ( &gpc_poly_0 );
                     gpc_free_polygon ( &gpc_poly_1 );
                     if (nint >= 1) {
                        if (hazint[nint] == hazint[nint-1] && hazgrp[nint] == hazgrp[nint-1] ){
                        /*
                         * Should do GPC_INT immediately   
                         */ 
                            gpc_polygon_clip ( GPC_INT, &gpc_poly_tmp[nint], &gpc_poly_tmp[nint-1],
                                 &gpc_poly_tmp[nint-1] );
                            continue;
                        }
                     }
                     nint++;
                     continue;
                   }
                }
             }
          }
       }
       nuni = 0;
       /*
        * finding union of non-intersecting contours - gpc_poly_union
        */
       for ( kk = 0; kk < ncntrs; kk++ ) {
         /* 
          * Skip contours involved in intersections
          */
          if ( intpoly[kk] == -2 ) {
             continue;
          }
         /* 
          * Process the rest of the contours
          */
          else {
            if ( nuni == 0 ) {
              /*
               * This is the starting contour for finding union
               */
              p2c_Elm2Poly ( *cntrs[kk], &gpc_poly_0, &ier );
              gpc_polygon_clip ( GPC_UNION, &gpc_poly_0, &gpc_poly_0,
                               &gpc_poly_union );
              gpc_free_polygon ( &gpc_poly_0 );
              nuni++;
            }
            else if ( nuni > 0 ) {
              /*
               * This is the next contour to the union
               */
              p2c_Elm2Poly ( *cntrs[kk], &gpc_poly_0, &ier );
              gpc_polygon_clip ( GPC_UNION, &gpc_poly_union, &gpc_poly_0,
                                 &gpc_poly_union );
              gpc_free_polygon ( &gpc_poly_0 );
            }
          }
       }
       /*
        * finding union-intersection of intersections - gpc_poly_int
        */
       if ( nint > 0 ) {
          gpc_polygon_clip ( GPC_UNION, &gpc_poly_tmp[0], &gpc_poly_tmp[0],
                            &gpc_poly_int );
          gpc_free_polygon ( &gpc_poly_tmp[0] );
         /*
          * Contour 0 iz being...
          */
          for ( kk = 1; kk < nint; kk++ ) {
             if ( hazint [kk-1] != hazint [kk] ||
                  ( hazint [kk-1] == hazint [kk] &&
                    hazgrp [kk-1] != hazgrp [kk] ) ) {
               /*
                *  ...  unionized with next contour
                */
                 gpc_polygon_clip ( GPC_UNION, &gpc_poly_int, &gpc_poly_tmp[kk],
                              &gpc_poly_int );
             }
             else if ( hazint [kk-1] == hazint [kk] ) {
               /*
                *  ...intersectionized with next contour
                */
                 gpc_polygon_clip ( GPC_INT, &gpc_poly_int, &gpc_poly_tmp[kk],
                              &gpc_poly_int );
             }
             gpc_free_polygon ( &gpc_poly_tmp[kk] );
          }
       }
      /*
       * finding union between the union-intersection of intersections and
       * the union of non-intersecting contours
       */
       if ( nint != 0 && nuni != 0 ) {
           gpc_polygon_clip ( GPC_UNION, &gpc_poly_int, &gpc_poly_union,
                              &gpc_poly_union );
           gpc_free_polygon ( &gpc_poly_int );
       }
       else if ( nint != 0 && nuni == 0 ) {
           gpc_polygon_clip ( GPC_UNION, &gpc_poly_int, &gpc_poly_int,
                              &gpc_poly_union );
           gpc_free_polygon ( &gpc_poly_int );
       }
    }

    nuni = gpc_poly_union.num_contours;

    for ( kk = 0; kk < gpc_poly_union.num_contours; kk++ ) {
         npu     = gpc_poly_union.contour[kk].num_vertices; 
        if ( *numOut == 0 ) {
            G_MALLOC ( (*outlk), VG_DBStruct, one, "p2c_PolyClip: outlk" );
        }
        else {
            G_REALLOC ( (*outlk), VG_DBStruct, (*numOut+1), "p2c_PolyClip: outlk" );
        }
        p2c_Poly2Out ( gpc_poly_union.contour[kk], iclr, &(*outlk)[ *numOut ], &ier);
        (*numOut)++;

        /*  
         * Adding a text label to the contour
         */
         G_REALLOC ( (*outlk), VG_DBStruct, (*numOut+1), "p2c_PolyClip: outlk" );
         npu     = (*outlk)[*numOut-1].elem.spl.info.numpts; 
         ilablat = 1;
         ilablon = npu + 1; 
         tlat = (*outlk)[*numOut-1].elem.spl.latlon[ilablat];
         tlon = (*outlk)[*numOut-1].elem.spl.latlon[ilablon];

         (*outlk)[*numOut].hdr.vg_class = CLASS_TEXT;
         (*outlk)[*numOut].hdr.vg_type = 21;
         (*outlk)[*numOut].hdr.delete = G_FALSE;
         (*outlk)[*numOut].hdr.maj_col  = iclr;
         (*outlk)[*numOut].hdr.min_col  = iclr;
         (*outlk)[*numOut].hdr.smooth  = 0;
         (*outlk)[*numOut].hdr.version  = 0;
         (*outlk)[*numOut].hdr.grptyp  = 7;
         (*outlk)[*numOut].hdr.grpnum  = (*outlk)[*numOut-1].hdr.grpnum;

         (*outlk)[*numOut].elem.spt.info.rotn      = 0.0;
         (*outlk)[*numOut].elem.spt.info.sztext    = 0.8;
         (*outlk)[*numOut].elem.spt.info.sptxtyp   = 5;
         (*outlk)[*numOut].elem.spt.info.turbsym   = 0;
         (*outlk)[*numOut].elem.spt.info.itxfn     = 22;
         (*outlk)[*numOut].elem.spt.info.ithw      = 2;
         (*outlk)[*numOut].elem.spt.info.iwidth    = 1;
         (*outlk)[*numOut].elem.spt.info.txtcol    = iclr;
         (*outlk)[*numOut].elem.spt.info.lincol    = 31;
         (*outlk)[*numOut].elem.spt.info.filcol    = 31;
         (*outlk)[*numOut].elem.spt.info.ialign    = 0;
         (*outlk)[*numOut].elem.spt.info.lat       = tlat;
         (*outlk)[*numOut].elem.spt.info.lon       = tlon;
         (*outlk)[*numOut].elem.spt.info.offset_x  = 2;
         (*outlk)[*numOut].elem.spt.info.offset_y  = 2;
 
         lens = (int)strlen(cat);
         (*outlk)[*numOut].hdr.recsz = sizeof( VG_HdrStruct) +
                         sizeof( SpTextInfo) + (size_t)lens + 1;
         strcpy ( (*outlk)[*numOut].elem.spt.text, cat);
        (*numOut)++;
    }
    gpc_free_polygon ( &gpc_poly_union );
    G_FREE ( xnormal1, float );
    G_FREE ( ynormal1, float );
    G_FREE ( xnormal2, float );
    G_FREE ( ynormal2, float );

}

/*=====================================================================*/

static void p2c_Poly2Out ( gpc_vertex_list contour, int iclr, 
                           VG_DBStruct *out, int *iret )
/************************************************************************
 * p2c_Poly2Out                                                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      contour         gpc_vertex_list GPC polygon contour             *
 *      iclr 		int		category color			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *out            VG_DBStruct     outlook element			*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 * m.gamazaychikov/SAIC	05/08	Set contour group number		*
 * X.Guo/CWS		09/10   Add codes to reduce multi-points        *
 ***********************************************************************/
{
    int          np, ier, ii, jj, nred;
    float       *xnormal, *ynormal;
    float   	xout[MAXPTS], yout[MAXPTS];
    float     	xred[MAXPTS], yred[MAXPTS];

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

    /*
     * Fill the information.
     */
    out->hdr.vg_class = CLASS_LINES;
    out->hdr.vg_type  = 20;
    out->hdr.delete = G_FALSE;
    out->hdr.closed = G_TRUE;
    out->hdr.maj_col = iclr;
    out->hdr.min_col = iclr;
    out->hdr.grptyp = 7;
    out->hdr.grpnum = nGroup;
    out->hdr.version = 0;
    out->hdr.filled = 0;
    out->hdr.smooth = 0;
    np = contour.num_vertices;
    out->hdr.recsz = sizeof(VG_HdrStruct) + sizeof(SpLineInfo) + np*2*sizeof(float);

    nGroup = nGroup+1;

    out->elem.spl.info.numpts = np;
    out->elem.spl.info.spltyp = 4;
    out->elem.spl.info.splstr = 1;
    out->elem.spl.info.spldir = 1;
    out->elem.spl.info.splsiz = 0.5;
    out->elem.spl.info.splwid = 2;

    G_MALLOC ( xnormal, float, np, "p2c_Poly2Out: xnormal" );
    G_MALLOC ( ynormal, float, np, "p2c_Poly2Out: ynormal" );

    gpc_gvlist ( &contour, &np, xnormal, ynormal, &ier );
    gtrans ( sys_N, sys_M, &np, xnormal, ynormal,
             &(out->elem.spl.latlon[   0]),
             &(out->elem.spl.latlon[ np ]),
             &ier, strlen(sys_N), strlen(sys_M) );

    for ( ii = 0 ; ii < np ; ii ++ ) {
         xout[ii] = out->elem.spl.latlon[ii];
         yout[ii] = out->elem.spl.latlon[np+ii ];
    }
    nred = 0;
    xred[ nred ] = xout[ 0 ];
    yred[ nred ] = yout[ 0 ];
    nred ++;
    for ( ii = 1; ii < np; ii++ ) {
        jj = nred -1 ;
   
        if ( ( fabs ( xout[ ii ] - xred[ jj ] ) > PROB2CAT_TIE_DIST_IN_MAP ) ||
             ( fabs ( yout[ ii ] - yred[ jj ] ) > PROB2CAT_TIE_DIST_IN_MAP ) ) {
                xred[ nred ] = xout[ ii ];
                yred[ nred ] = yout[ ii ];
                nred++;
            }
    }
    for ( ii = 0 ; ii < nred ; ii ++ ) {
         out->elem.spl.latlon[ii] = xred[ii];
         out->elem.spl.latlon[nred+ii ] = yred[ii];
    }
    out->elem.spl.info.numpts = nred;
    G_FREE ( xnormal, float );
    G_FREE ( ynormal, float );

}

/*=====================================================================*/

static void p2c_Elm2Poly ( VG_DBStruct el, gpc_polygon *gpc_poly, int *iret )
/************************************************************************
 * p2c_Elm2Poly                                                         *
 *                                                                      *
 * Creates a GPC polygon in normalized coordinate from the outlook.	*
 *                                                                      *
 * Input parameters:                                                    *
 *      el              VG_DBStruct     outlook element                 *
 *                                                                      *
 * Output parameters:                                                   *
 *      *gpc_poly       gpc_polyon      GPC polygon structure           *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 ***********************************************************************/
{
    int                 hole = 0, ier = 0, np;
    float               *xnormal, *ynormal;
    gpc_vertex_list     verts;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * Convert to normalized coordinate first.
     */
    np = el.elem.spl.info.numpts;
    G_MALLOC ( xnormal, float, np, "p2c_Elm2Poly: xnormal" );
    G_MALLOC ( ynormal, float, np, "p2c_Elm2Poly: ynormal" );

    gtrans ( sys_M, sys_N, &np, el.elem.spl.latlon,
             &el.elem.spl.latlon[np], xnormal, ynormal,
             &ier, strlen(sys_M), strlen(sys_N) );

    /*
     * Fill GPC polygon structure with points in the incoming element.
     */
    gpc_poly->num_contours = 0;
    gpc_poly->hole         = (int*)NULL;
    gpc_poly->contour      = (gpc_vertex_list*)NULL;
    verts.vertex          = (gpc_vertex*)NULL;
    verts.num_vertices    = 0;
    np = el.elem.spl.info.numpts;
    gpc_cvlist ( np, xnormal, ynormal, &verts, &ier );
    gpc_poly->num_contours = 0;
    gpc_add_contour ( gpc_poly, &verts, hole );
    free ( verts.vertex );
    G_FREE ( xnormal, float );
    G_FREE ( ynormal, float );
}

/*=====================================================================*/

static void p2c_GetCat ( VG_DBStruct *el, int prob, char *haz, char *cat, 
                         int *nbump, VG_DBStruct **origEl, char ***origCat, 
			 int bumpFlag, int *iret )
/************************************************************************
 * p2c_GetCat                                                           *
 *                                                                      *
 * Input parameters:                                                    *
 *      prob            int             Prob                            *
 *      bumpFlag	int		If the flag is set, it will bump*
 *      				the el. If not, it will make a  *
 *      				copy of the original and put it *
 *      				in the origEl array.		*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *el             VG_DBStruct     Pointer to the VG record        *
 *      *nbump          int     	Pointer to # of bumped El       *
 *	**origEl	VG_DBStruct	Array of original El if bumped	*
 *	***origCat	char		Array of original Cat if bumped	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *haz            char            Hazard type (TORN, WIND, HAIL)	*
 *      *value          char            Category (HIGH, MDRT, ENHC,	*
 *					 	  SLGT, MRGL)		*
 *      *iret           int             Return code                     *
 *                                      -1 - Wrong type                 *
 *                                      -2 - Search failed              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 * B.Yin/SGT            11/14	Added bumpflag and output the originals *
 * 				if bumpflag is not set.			*
 * S. Guan/NCEP         04/19   Made DAY2 input with Hail, Wind and     * 
 *                              Tornado probabilities (similar to DAY1) * 
 *                              work.  	                                *
 ***********************************************************************/
{
    int         *int_ptr, ii, ier, ihgh, ilow, ibmp, idef=100, nexp=2, num;
    int         done, icat, one = 1;
    char        day[6], value[10], tag[25], tblnam[12], dirnam[5], tagtmp[25];
    char        tagbmp[25], bmpcat[6], *cat0;
    char        _Prob_ctgr[6][6] = {"_HIGH", "_MDRT", "_ENHC",
       				    "_SLGT", "_MRGL", "_TEXT"};
/*---------------------------------------------------------------------*/

    *iret = 0;
    done = G_FALSE;

    /*
     *  If it is not a SPLN element return.
     */
    if ( el->hdr.vg_type != SPLN_ELM ){
        strcpy ( cat, "" );
        strcpy ( haz, "" );
        *iret = -1;
        return;
    }

    strcpy ( day, _Day_out );
    strcpy ( tblnam, "p2cdef.tbl" );
    strcpy ( dirnam, "pgen" );
    strcpy ( tag, day );

    /*
     * 
     */
     strcpy ( cat, "" );
     strcpy ( value, "" );
     strcpy ( bmpcat, "" );
     strcpy ( haz, "" );

     if ( el->hdr.grptyp == 12 ) {
        strcpy ( haz, "HAIL" );
        strcat ( tag, "_");
        strcat ( tag, haz );
        strcpy ( tagbmp, tag );
     }
     else if ( el->hdr.grptyp == 13 ) {
        strcpy ( haz, "TORN" );
        strcat ( tag, "_");
        strcat ( tag, haz );
        strcpy ( tagbmp, tag );
     }
     else if ( el->hdr.grptyp == 14 ) {
        strcpy ( haz, "WIND" );
        strcat ( tag, "_");
        strcat ( tag, haz );
        strcpy ( tagbmp, tag );
     }
     else if ( el->hdr.grptyp == 15 ) {
        strcpy ( haz, "D2_3" );
        strcpy ( tagbmp, day );
     }

     strcat ( tagbmp, "_BUMP" );

     if ( strcmp (_Day_out, "DAY1") == 0 ) {
         ilow = 0;
         ihgh = 0;
         ibmp = 0;
         ctb_rdprf ( tblnam, dirnam, tagbmp, value, &ier );
         int_ptr = (int *)malloc((size_t)nexp * sizeof(int));
         cst_ilst( value, ';', idef, nexp, int_ptr, &num, &ier );
         ilow = int_ptr [0];
         ihgh = int_ptr [1];
         if ( int_ptr ) free  ( int_ptr );
         if (  prob == ilow ) ibmp = ilow;
         if (  prob == ihgh ) ibmp = ihgh;
         strcpy ( value, "" );
     }
     else if ( strcmp (_Day_out, "DAY2") == 0 ) {
         ilow = 0;
         ihgh = 0;
         ibmp = 0;
         ctb_rdprf ( tblnam, dirnam, tagbmp, value, &ier );
         int_ptr = (int *)malloc((size_t)nexp * sizeof(int));
         cst_ilst( value, ';', idef, nexp, int_ptr, &num, &ier );
         ilow = int_ptr [0];
         ihgh = int_ptr [1];
         if ( int_ptr ) free  ( int_ptr );
         if (  prob == ilow ) ibmp = ilow;
         if (  prob == ihgh ) ibmp = ihgh;
         strcpy ( value, "" );
     }
     else {
         ctb_rdprf ( tblnam, dirnam, tagbmp, value, &ier );
         ibmp = 0;
         ibmp = atoi (value);
     }

     strcpy ( tagtmp, tag );

     for ( ii = 0; ii < 6; ii++ ) {
         strcpy ( value, "" );
         ilow = 0;
         ihgh = 0;
         strcpy ( tag, tagtmp );
         strcat ( tag, _Prob_ctgr [ii] );
         ctb_rdprf ( tblnam, dirnam, tag, value, &ier );
         int_ptr = (int *)malloc((size_t)nexp * sizeof(int));
         cst_ilst( value, '-', idef, nexp, int_ptr, &num, &ier );
         ilow = int_ptr [0];
         ihgh = int_ptr [1];
         if( int_ptr )
           free( int_ptr );
         if (  prob >= ilow && prob < ihgh ) {
            strcpy ( cat, _Prob_ctgr [ii] );
            icat = ii;
            break;
         }
     }


     if ( prob == ibmp ) {
       done = G_FALSE;
       ii = 0;
       while ( (!done) && (ii < nIn) ) {
          if (
               ( ( (elIn[ ii ].hdr.grptyp == el->hdr.grptyp) &&
                   (elIn[ ii ].hdr.vg_type == SPLN_ELM) &&
                   (elIn[ ii ].hdr.filled) ) )
               ||
               ( ( strcmp (_Day_out, "DAY1") != 0 ) &&
                 ( (elIn[ ii ].hdr.vg_type == SPLN_ELM) &&
                   (el->hdr.grptyp != 12) && (el->hdr.grptyp != 13) &&
                   (el->hdr.grptyp != 14) && (elIn[ ii ].hdr.filled) ) )
             ) {

		if ( bumpFlag ) {
		   /*
 		    * Bump the category
 		    */ 
              	   p2c_BumpCat ( el, ii, bmpcat, &done, &ier);
		}
		else {

	      	   /*
 		    * Make a copy of the original El 
 		    */

            	   cat0 = (char*) malloc( 6*sizeof(char));
              	   strcpy ( cat0, cat );

	  	   if ( *nbump == 0 ) {
            		G_MALLOC ( *origEl, VG_DBStruct, one, "p2c_getCat: origEl" );
            	        *origCat = (char**) malloc( sizeof(char*));
            		//G_MALLOC ( *origCat, char **, one, "p2c_getCat: origCat" );
	  	   } 
	  	   else {
                      G_REALLOC ( *origEl, VG_DBStruct ,
                                *nbump + 1, "p2c_getCat: origEl" );
            	      *origCat = (char**) realloc( *origCat,(*nbump + 1)*sizeof(char*));
            	      //G_REALLOC ( origCat, char **, *nbump + 1, "p2c_getCat: origCat" );
	  	   } 
	  	   memcpy ( &((*origEl)[*nbump]), el, sizeof(VG_DBStruct) );
		   (*origCat)[*nbump] = cat0;
	  	   (*nbump)++;
		   done = G_TRUE;
		}
          }
          ii++;
       }
     }

     if ( done && icat > 0 ) strcpy ( cat, _Prob_ctgr [icat-1] );

}

/*=====================================================================*/

static void p2c_CloseCntr ( VG_DBStruct *el_in, VG_DBStruct **el_out, 
                            int *nOut, int *iret )
/************************************************************************
 * p2c_CloseCntr                                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *el_in          VG_DBStruct	In contour			*
 *                                                                      *
 * Output parameters:                                                   *
 *      **el_out        VG_DBStruct	Out contour array		*
 *      *nOut           int             Number of returned contours     *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 * m.gamazaychikov/SAIC	05/08	Made extention of end points iterative	*
 * m.gamazaychikov/SAIC	07/08	Changed call to cgr_intersect to 	*
 *				cgr_linepoly, removed call to NewPoly,	*
 *				added code to close open contour	*
 * m.gamazaychikov/CWS  06/09   Added calls to p2c_ExtendCntr, CopyCntr,*
 *                              p2c_CloseSingleCntr                     * 
 ***********************************************************************/
{ 
    int         ier, nclosed, npline, npline2, nip, kk;
    int         nppi=2, nclp, ii, firstEndPoint, lastEndPoint;
    int         nSegs, nSegsPossible, *segStart, *segEnd, jj, npSeg, one=1;

    float       dum, linelats[MAXPTS], linelons[MAXPTS];
    float       line_lats[MAXPTS], line_lons[MAXPTS], *ptr1lat, *ptr1lon;
    float       xclosed[MAXPTS], yclosed[MAXPTS];
    float       *xclp, *yclp;

    Boolean     *InOut = NULL;
/*---------------------------------------------------------------------*/

    if ( el_in->hdr.closed == G_TRUE ) {
        return;
    }
    else {
        /* 
         * Need to close the controur 
         */

        /*
         * First find out if the contour intersects the boundary.
         * If it does, truncate the countor at the intersections points.
         * If it does not, extend the contour beyond the boundary,
         * and then truncate at the intersection points.
         */
         npline = el_in->elem.spl.info.numpts;
         ptr1lat = &(el_in->elem.spl.latlon[   0]);
         ptr1lon = &(el_in->elem.spl.latlon[npline]);
         memcpy ( linelats, ptr1lat, (size_t)npline*sizeof(float) );
         memcpy ( linelons, ptr1lon, (size_t)npline*sizeof(float) );

         for ( ii = 0; ii < npline; ii++ ) {
          linelats [ ii ] = rint ( (double)linelats[ ii ] * 1000 ) / 1000;
          linelons [ ii ] = rint ( (double)linelons[ ii ] * 1000 ) / 1000;
         }
 
        /*
         * Special case of lines with only two points - 
         * add point in the middle of the line.
         */
         if ( npline == 2) {
            if ( el_in->elem.spl.info.spldir == -1 ) {
              dum = linelats [0];
              linelats [0] = linelats [1];
              linelats [1] = dum;
              dum = linelons [0];
              linelons [0] = linelons [1];
              linelons [1] = dum;
              el_in->elem.spl.info.spldir = 1;
            }
            linelats [2] = linelats [1];
            linelats [1] = ( linelats [0] + linelats [1] ) / 2.;
            linelons [2] = linelons [1];
            linelons [1] = ( linelons [0] + linelons [1] ) / 2.;
            npline = npline + 1;
         }

        /*
         * Find the intersection of the contour and the boundary
         * inOut: 1==IN; 0==OUT
         */
         cgr_linepoly ( npline, linelats, linelons,
                        _areaP, _areaX, _areaY,
                        &nclp, &xclp, &yclp, &InOut, &ier );
         firstEndPoint = InOut [ 0 ];
         lastEndPoint  = InOut [ nclp-1 ];

        /*
         * Number of intersection points is defined as
         * difference between returned number of points from
         * cgr_linepoly nclp and the original number of points
         * in the line devided by number of points per intersection
         * nppi = 2;
         */
         nip = (nclp-npline)/nppi;
          
        /*
         * Start the closing fo contours depending on the number 
         * of intersections
         */
         if ( nip == 2 ) {
           /*
            * no need for extensions
            */
            for ( kk = 0; kk < npline; kk++) {
               line_lats[kk] = linelats[kk];
               line_lons[kk] = linelons[kk];
            }

           /*
            * Close the contour. 
            */
            p2c_CloseSingleCntr ( &npline,  line_lats, line_lons, 
                                  &nclp,    xclp,      yclp, InOut,
                                  &nclosed, xclosed,   yclosed, &ier );
            *nOut = 1;
            G_MALLOC ( (*el_out), VG_DBStruct, one, "p2c_CloseCntr: el_out" );

           /*
            * Make a copy of the element
            */
            p2c_CopyCntr ( &(*el_in), &(*el_out)[0], &ier);

           /*
            * Overwrite the closed flag, number of points and
            * point coordinates
            */
            (*el_out)[0].hdr.closed = 1;
            (*el_out)[0].hdr.recsz = sizeof(VG_HdrStruct) + sizeof(SpLineInfo)
                                   + nclosed*2*sizeof(float);
            (*el_out)[0].elem.spl.info.numpts = nclosed;

            for ( kk = 0; kk < nclosed; kk++) {
               if ( G_DIFF( xclosed[kk], 0.0F) ||
                    G_DIFF( yclosed[kk], 0.0F)) {
                  (*el_out)[0].elem.spl.latlon[ kk ]          = RMISSD;
                  (*el_out)[0].elem.spl.latlon[ nclosed +kk ] = RMISSD;
               }
               else {
                  (*el_out)[0].elem.spl.latlon[ kk ]           = xclosed[kk];
                  (*el_out)[0].elem.spl.latlon[ kk + nclosed ] = yclosed[kk];
               }
            }

           /*
            *  Free memory allocated in cgr_linepoly
            */
            G_FREE ( xclp, float );
            G_FREE ( yclp, float );
            G_FREE ( InOut, Boolean );
            return;

         }
         else if ( nip < 2 ) {
           /*
            *  Free memory allocated in cgr_linepoly
            */
            G_FREE ( xclp, float );
            G_FREE ( yclp, float );
            G_FREE ( InOut, Boolean );

           /*
            * Need to extend one or both end points
            */
            
           /*
            * Extend the contour so there is two points of intersections 
            */
            p2c_ExtendCntr ( &firstEndPoint, &lastEndPoint, 
                             &npline, linelats, linelons, 
                             &npline2,line_lats, line_lons, &ier );

           /*
            * Find the intersection of the contour and the boundary
            * inOut: 1==IN; 0==OUT
            */
            cgr_linepoly ( npline2, line_lats, line_lons,
                          _areaP, _areaX, _areaY,
                           &nclp, &xclp, &yclp, &InOut, &ier );

           /*
            * Close the contour. 
            */
            p2c_CloseSingleCntr ( &npline,  line_lats, line_lons, 
                                  &nclp,    xclp,      yclp, InOut,
                                  &nclosed, xclosed,   yclosed, &ier );
            *nOut = 1;
            G_MALLOC ( (*el_out), VG_DBStruct, one, "p2c_CloseCntr: el_out" );

           /*
            * Make a copy of the element
            */
            p2c_CopyCntr ( &(*el_in), &(*el_out)[0], &ier);

           /*
            * Overwrite the closed flag, number of points and
            * point coordinates
            */
            (*el_out)[0].hdr.closed = 1;
            (*el_out)[0].hdr.recsz = sizeof(VG_HdrStruct) + sizeof(SpLineInfo)
                                   + nclosed*2*sizeof(float);
            (*el_out)[0].elem.spl.info.numpts = nclosed;

            for ( kk = 0; kk < nclosed; kk++) {
               if ( G_DIFF( xclosed[kk], 0.0F) ||
                    G_DIFF( yclosed[kk], 0.0F)) {
                  (*el_out)[0].elem.spl.latlon[ kk ]          = RMISSD;
                  (*el_out)[0].elem.spl.latlon[ nclosed +kk ] = RMISSD;
               }
               else {
                  (*el_out)[0].elem.spl.latlon[ kk ]           = xclosed[kk];
                  (*el_out)[0].elem.spl.latlon[ kk + nclosed ] = yclosed[kk];
               }
            }

           /*
            *  Free memory allocated in cgr_linepoly
            */
            G_FREE ( xclp, float );
            G_FREE ( yclp, float );
            G_FREE ( InOut, Boolean );
            return;
         }
         else if (nip  > 2 ) {
           /*
            * Multiple intersection points
            */

            if ( !firstEndPoint && !lastEndPoint ) {
               /*
                * both ends of the contour are outside the bounds
                */
               /*
                * Close the contour. 
                */
                nSegsPossible = nip/2;
                G_MALLOC ( segStart, int, nSegsPossible, "p2c_CloseCntr: segStart" );
                G_MALLOC ( segEnd,   int, nSegsPossible, "p2c_CloseCntr: segEnd" );
                p2c_SplitCntr (  &npline, linelats,  linelons, 
                                 &nclp,   InOut, xclp, yclp,
                                 &nSegs,  segStart,  segEnd, &ier );
               /*
                *  Free memory allocated in cgr_linepoly
                */
                G_FREE ( xclp, float );
                G_FREE ( yclp, float );
                G_FREE ( InOut, Boolean );
                G_MALLOC ( (*el_out), VG_DBStruct, nSegs, "p2c_CloseCntr: el_out" );
                for ( ii = 0; ii < nSegs; ii++) {
                   jj = 0;
                   npSeg = segEnd[ii] - segStart[ii] + 1;
                   for ( kk = segStart[ii]; kk < segStart[ii] + npSeg; kk++) {
                     line_lats[jj] = linelats[kk];
                     line_lons[jj] = linelons[kk];
                     jj++;
                   }
                   cgr_linepoly ( npSeg, line_lats, line_lons,
                                 _areaP, _areaX, _areaY,
                                 &nclp, &xclp, &yclp, &InOut, &ier );

                  /*
                   * Close the contour. 
                   */
                   p2c_CloseSingleCntr ( &npSeg,  line_lats, line_lons, 
                                         &nclp,    xclp,      yclp, InOut,
                                         &nclosed, xclosed,   yclosed, &ier );

                  /*
                   * Make a copy of the element
                   */
                   p2c_CopyCntr ( &(*el_in), &(*el_out)[ii], &ier);

                  /*
                   * Overwrite the closed flag, number of points and
                   * point coordinates
                   */
                   (*el_out)[ii].hdr.closed = 1;
                   (*el_out)[ii].hdr.recsz = sizeof(VG_HdrStruct) + sizeof(SpLineInfo)
                                          + nclosed*2*sizeof(float);
                   (*el_out)[ii].elem.spl.info.numpts = nclosed;

                   for ( kk = 0; kk < nclosed; kk++) {
                     if ( G_DIFF( xclosed[kk], 0.0F) ||
                          G_DIFF( yclosed[kk], 0.0F)) {
                           (*el_out)[ii].elem.spl.latlon[ kk ]          = RMISSD;
                           (*el_out)[ii].elem.spl.latlon[ nclosed +kk ] = RMISSD;
                      }
                      else {
                           (*el_out)[ii].elem.spl.latlon[ kk ]           = xclosed[kk];
                           (*el_out)[ii].elem.spl.latlon[ kk + nclosed ] = yclosed[kk];
                      }
                   }

                  /*
                   *  Free memory allocated in cgr_linepoly
                   */
                   G_FREE ( xclp, float );
                   G_FREE ( yclp, float );
                   G_FREE ( InOut, Boolean );
                }
                *nOut = nSegs;
                return;
            }
         }
    }
}
#ifdef P2C_NEWPOLY
/*=====================================================================*/

static void p2c_NewPoly ( int *np1, float xp1[], float yp1[], int *np2, 
			float xp2[], float yp2[], int *np3, 
			float xp3[], float yp3[], int *iret )

/************************************************************************
 * p2c_NewPoly								*
 *									*
 * This function accepts two polygons(one open, one close) as a 	*
 * sequence of points, and builds a new close polygon. The two input   	*
 * polygons intersect with two intersected points, and are in the same	*
 * coordinate system.							* 
 *									*
 * Input parameters:							*
 *	*np1	int	Number of vertices in polygon #1 (open)		*
 *	xp1[]	float	X-Coordinates for polygon #1			*
 *	yp1[]	float	Y-Coordinates for polygon #1			*
 *	*np2	int	Number of vertices in polygon #2 (close)	*
 *	xp2[]	float	X-Coordinates for polygon #2			*
 *	yp2[]	float	Y-Coordinates for polygon #2			*
 *									*
 * Output parameters:							*
 *	*np3    int     Number of vertices in new polygon               *
 *      xp3[]   float   X-Coordinates for new polygon                   *
 *      yp3[]   float   Y-Coordinates for new polygon			*
 *	*iret	int	Return code					*
 *									*
 **									*
 * Log:									*
 * m.gamazaychikov/SAIC 10/06 	From cpcg_newpoly with some changes	*
 ***********************************************************************/
{
int  	ii, na, nc, np, intrsct, nv, ier, more; 
int	bpnt1[MAXPTS], apnt1[MAXPTS], bpnt2[MAXPTS], apnt2[MAXPTS]; 
float	xs1[MAXPTS], ys1[MAXPTS], nx2[MAXPTS], ny2[MAXPTS];
int     indx[MAXPTS];
int	npp2;
/*--------------------------------------------------------------------*/

    *iret = 0;
    *np3 = 0;

    /*
     * Get the intersection points
     */
    intrsct = MAXPTS;

    npp2 = *np2;
    clo_reorder(npp2, xp2, yp2, indx, &ier);

        for (ii = 0; ii < npp2; ii++) {
            nx2[ii] = xp2[indx[ii]];
            ny2[ii] = yp2[indx[ii]];
        }

    cgr_intersect ( sys_M, np1, xp1, yp1, sys_M, np2, nx2, ny2,
                        &intrsct, sys_M, &nv, xs1, ys1, 
                        bpnt1, apnt1, bpnt2, apnt2, &ier ); 

    if (nv < 2) {
	*iret = -5;
	return;
    }

    /*
     * if ( bpnt1[0] == bpnt1[1] && apnt1[0] == apnt1[1] ), re-compute the intersections
     */
    more = G_FALSE;
     if ( bpnt1[0] == bpnt1[1] && apnt1[0] == apnt1[1] ) more = G_TRUE;
    while ( more ) {
	*np1 = *np1 + 1;
	xp1[bpnt1[0]+1] = (xs1[0] + xs1[1]) / 2;
        yp1[bpnt1[0]+1] = (ys1[0] + ys1[1]) / 2;
	for (ii = *np1-1; ii > bpnt1[0]+1; ii--) {
		xp1[ii] = xp1[ii-1];
                yp1[ii] = yp1[ii-1];
	}

	cgr_intersect ( sys_M, np1, xp1, yp1, sys_M, np2, nx2, ny2,
                        &intrsct, sys_M, &nv, xs1, ys1, bpnt1, apnt1,
                        bpnt2, apnt2, &ier );
        if ( bpnt1[0] != bpnt1[1] && apnt1[0] != apnt1[1] )
            more = G_FALSE;
    }

    /*
     * if more then two intersections, get the first and the last 
     */
    if ( nv > 2 ) {
	xs1[1] = xs1[nv-1];
	ys1[1] = ys1[nv-1];
	bpnt1[1] = bpnt1[nv-1];
	apnt1[1] = apnt1[nv-1];
	bpnt2[1] = bpnt2[nv-1];
	apnt2[1] = apnt2[nv-1];
    }

    /*
     * Get points between intersection points in polygon 1
     */
    xp3[0] = xs1[0];
    yp3[0] = ys1[0];
    np = 0;

    if ( G_DIFF ( xs1[0], xp1[apnt1[0]] ) && 
         G_DIFF ( ys1[0], yp1[apnt1[0]] ) ) {
	na = apnt1[0] + 1;
    }
    else {
	na = apnt1[0];
    }

    for (ii = na; ii <= bpnt1[1]; ii++) {
	np++;
	xp3[np] = xp1[ii];
    	yp3[np] = yp1[ii];
    }
 
    if ( !G_DIFF ( xs1[1], xp1[bpnt1[1]] ) || 
         !G_DIFF ( ys1[1], yp1[bpnt1[1]] ) ) {
	np++;
        xp3[np] = xs1[1]; 
        yp3[np] = ys1[1];
    }

    /*
     *  Get points from polygon2 
     */
    if ( G_DIFF ( xs1[1], nx2[apnt2[1]] ) && 
         G_DIFF ( ys1[1], ny2[apnt2[1]] ) ) {
    	nc = apnt2[1] + 1;
    }
    else {
    	nc = apnt2[1];
    }

/*
    if (apnt2[1] < bpnt2[0]) { 
*/
    if (apnt2[1] <= bpnt2[0]) { 
    
    	for (ii = nc; ii <= bpnt2[0]; ii++) {
          
    	    np++;
    	    xp3[np] = nx2[ii];
    	    yp3[np] = ny2[ii];
    	}
    }
    else {
    	for (ii = nc; ii < *np2; ii++) {
    	    np++;
    	    xp3[np] = nx2[ii];
    	    yp3[np] = ny2[ii];
    	}

    	for (ii = 0; ii <= bpnt2[0]; ii++) {
    	    np++;
    	    xp3[np] = nx2[ii];
    	    yp3[np] = ny2[ii];
    	}
    }

    if ( G_DIFF ( xp3[np], xp3[np-1] ) && 
         G_DIFF ( yp3[np], yp3[np-1] ) ) {
    	    np--;
    }    
 

    if (np > 0) {
	*np3 = np+1;
    }
    else {
	*np3 = 0;
	*iret = -5;
    }
} 
#endif
/*=====================================================================*/

static void p2c_OpenCntr ( VG_DBStruct *el_in, VG_DBStruct **el_out, int *nout, 
                           int *iret )
/************************************************************************
 * p2c_OpenCntr                                                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      *el_in          VG_DBStruct     outlook element			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *nout           int             number of output elements       *
 *      **p2cGrp        VG_DBStruct 	array of elements		*
 *                                                                      *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 * m.gamazaychikov/SAIC	05/08	Fixed the bug, that was excluding 1st	*
 *  				point, excluded all bad points instead	*
 *				of setting them to RMISSD		*
 * m.gamazaychikov/SAIC	08/08	Changed how values of inout array are   *
 *                              calculated; changed the alogorythm of   *
 *				splitting a contour into parts          *
 * m.gamazaychikov/CWS	06/09	Changed how values of inout array are   *
 *                              calculated, added call to CopyCntr,	*
 *                              fixed a bug when one point was excluded *
 * m.gamazaychikov/CWS  06/09   Fixed a bug when adding a point		*
 * X.Guo/CWS		03/10   Fixed bugs for ticket 20                * 
 * S. Guan              05/16   Fixed bugs for ticket 7796              *
 ***********************************************************************/
{ 
    int         ii, jj, kk, ier, jst, jfn, npseg, nip, one=1, two=2;
    int         npline, *inout, idum, maxp=10, jdum[MAXPTS], closed=0; 
    int         ninout;
    int         nseg;
    int         inside,ier1;
#ifdef P2C_NEWPOLY
    int 	nclosed, *pointInout, checkAll = 1;
    float	xclosed[MAXPTS], yclosed[MAXPTS];
#endif
    int         izeros [10], iones [10], nones, nzeros, ibegin;
    int         ivertex, iv, ivp1, ivm1;
    int         mj, jAddPoint, rol1, rol2;

    float       line_lats[MAXPTS], line_lons[MAXPTS], *ptr1lat, *ptr1lon;
    float       dum_lats[MAXPTS], dum_lons[MAXPTS];
    float       dum_lats2[MAXPTS], dum_lons2[MAXPTS];
    float       dstnseg, dum, distmark = 0.51;
    float       dstnseg2;
    float       x_int[MAXPTS], y_int[MAXPTS];
    float       *xQrol, *yQrol, tol=0.000001F;
    float       *areaxN, *areayN, *linexN, *lineyN, tol2=0.0002;

    Boolean     don=G_FALSE, found1=G_FALSE, found0=G_FALSE, addOnePoint;
#define LLSCAL  100
/*---------------------------------------------------------------------*/

    if ( el_in->hdr.closed == G_FALSE ) {
       return;
    }
    else {
        *nout = 0;
        /* 
         * Need to open the polygon and return all segments of it
         */
         npline = el_in->elem.spl.info.numpts;
         ptr1lat = &(el_in->elem.spl.latlon[   0]);
         ptr1lon = &(el_in->elem.spl.latlon[npline]);
         memcpy ( line_lats, ptr1lat, (size_t)npline*sizeof(float) );
         memcpy ( line_lons, ptr1lon, (size_t)npline*sizeof(float) );
         for ( ii = 0; ii < npline; ii++ ) {
          dum_lats [ ii ] = RMISSD;
          dum_lons [ ii ] = RMISSD;
          dum_lats2 [ ii ] = RMISSD;
          dum_lons2 [ ii ] = RMISSD;
          line_lats [ ii ] = rint ( (double)line_lats[ ii ] * 1000 ) / 1000;
          line_lons [ ii ] = rint ( (double)line_lons[ ii ] * 1000 ) / 1000;
         }

         nip = 0;
         ninout = 0;
        /*
         *  Calculate the number of intersections
         */
         cgr_intersect ( sys_M, &npline, line_lats, line_lons,
                         sys_M, &_areaP, _areaX, _areaY,
                         &maxp, sys_M, &nip, x_int, y_int, jdum, jdum,
                         jdum, jdum, &ier );

        /*
         *  If no intersections - return the original contour
         */
         if ( nip == 0 ) {
               inside = 0;
               cgr_objint (npline, line_lats, line_lons,NULL,_areaP, _areaX, _areaY,NULL,&inside,&ier1 );
               if ( inside == 0 ) {
                   G_MALLOC ( (*el_out), VG_DBStruct, one, "p2c_OpenCntr: el_out" );
                   *nout = 0;
                   /*
                    * Make a copy of the element
                    */
                   p2c_CopyCntr ( &(*el_in), &(*el_out)[*nout], &ier);
                   (*nout)++;
                   return;
               }
               else {
                   nip = npline;
               }
         }

        /*
         *  This needed for closed contours by "mistake"
         */
         /*
          *After discussed with Michael Gamazaychikov, commented out
          */
#ifdef P2C_NEWPOLY
         G_MALLOC ( pointInout, int, npline, "p2c_OpenCntr: pointInout" );
         cgr_inpolywn ( npline, line_lats, line_lons, _areaP, _areaX, _areaY,
                   checkAll, pointInout, &ier );
                                                                                                                 
         for ( ii = 0; ii < npline; ii++ ) {
           if ( pointInout[ii] == 0 ) ninout++;
         }
         if ( nip == 2 && ninout > 2 ) {
           p2c_NewPoly(&npline, line_lats, line_lons,
                        &_areaP, _areaX, _areaY,
                        &nclosed, xclosed, yclosed,
                        &ier);
           for ( ii = 0; ii < nclosed; ii++ ) {
                line_lats[ ii ] = xclosed [ii];
                line_lons[ ii ] = yclosed [ii];
           }
           npline = nclosed;
         }
#endif
         dstnseg=0.0F;
         cgr_segdist ( &_areaP, _areaX, _areaY, 
                       &line_lats[npline-1], &line_lons[npline-1],
                       &dstnseg, &idum, &idum, &dum, &dum, &ier);

        /*
         *  Start processing contour that needs to be open
         */
         kk = 0;
         nseg = 0;
         for (ii = 0; ii < 10; ii++ ) {
             izeros [ ii ] = 0;
             iones  [ ii ] = 0;
         }
         G_MALLOC ( inout, int, npline, "p2c_OpenCntr: inout" );
    	 for (ii = 0; ii < npline; ii++) {
                     inout[ii] =0;
         }
        /*
         *  Calculate the inout array:
         *  inout = 1 - the point is on the boundary segment     --> discard
         *  inout = 0 - the point is not on the boundary segment --> keep
         */
         G_MALLOC ( areaxN, float, _areaP, "p2c_OpenCntr: areaxN" );
         G_MALLOC ( areayN, float, _areaP, "p2c_OpenCntr: areayN" );
         G_MALLOC ( linexN, float, npline, "p2c_OpenCntr: linexN" );
         G_MALLOC ( lineyN, float, npline, "p2c_OpenCntr: lineyN" );
         G_MALLOC ( xQrol, float, two, "p2c_OpenCntr: xQrol" );
         G_MALLOC ( yQrol, float, two, "p2c_OpenCntr: yQrol" );
         gtrans ( sys_M, sys_N, &_areaP, _areaX, _areaY, areaxN, areayN, &ier, strlen(sys_M), strlen(sys_N) );
         gtrans ( sys_M, sys_N, &npline, line_lats, line_lons, linexN, lineyN, &ier, strlen(sys_M), strlen(sys_N) );

    	 for (ii = 0; ii < npline; ii++) {
            cgr_dist ( _areaP, areaxN, areayN, linexN[ii], lineyN[ii],
                       &dum, &ivertex, &ier);

            if ( ivertex == 0 ) {
                    iv = 0;
                    ivm1 = _areaP-1;
                    ivp1 = iv + 1;
               } else if ( ivertex == _areaP-1) {
                    iv = _areaP-1;
                    ivm1 = _areaP-2;
                    ivp1 = 0;
               } else {
                    iv = ivertex;
                    ivm1 = iv - 1;
                    ivp1 = iv + 1;
            }

            xQrol[0] = areaxN[iv];
            xQrol[1] = areaxN[ivp1];
            yQrol[0] = areayN[iv];
            yQrol[1] = areayN[ivp1];
            cgr_qrol ( &two, xQrol, yQrol, &closed, &linexN[ii], &lineyN[ii], &tol, &rol1, &ier);
            xQrol[0] = areaxN[ivm1];
            xQrol[1] = areaxN[iv];
            yQrol[0] = areayN[ivm1];
            yQrol[1] = areayN[iv];
            cgr_qrol ( &two, xQrol, yQrol, &closed, &linexN[ii], &lineyN[ii], &tol, &rol2, &ier);

            cgr_segdist ( &_areaP, areaxN, areayN, &linexN[ii], &lineyN[ii],
                          &dstnseg2, &idum, &idum, &dum, &dum, &ier);

           /*
            * Decide whether to exclude point ot keep it
            *  inout = 0        - keep it
            *  inout = 1        - loose it
            */
            if ( (rol1 == 0 && rol2 == 0) || G_DIFFT(dstnseg2, 0.0F, tol2 )) {
                inout [ii]=1;
            }
         }

         G_FREE ( areaxN, float );
         G_FREE ( areayN, float );
         G_FREE ( linexN, float );
         G_FREE ( lineyN, float );
         G_FREE ( xQrol, float );
         G_FREE ( yQrol, float );
        /*
         *  Calculate the beginning and end of each inner 
         *  with respect to the boundary segment of the contour
         */
         ibegin = 0;
         nzeros = 0;
         nones = 0;
         while ( !don ) {
             jj = ibegin;
             while ( !found0) {
                 found1 = G_FALSE;
                 if ( !inout [jj] && jj < npline ) {
                    izeros [ nzeros ] = jj;
                    nzeros++;
                    found0 = G_TRUE;
                    break;
                 }
                 if ( jj == npline-1 ) {
                    found0 = G_FALSE;
                    break;
                 }
                 jj++;
             }
             jj = izeros [ nzeros -1 ];
             while ( found0 && !found1) {
                 if ( inout [jj] && jj < npline ) {
                    iones [ nones ] = jj;
                    nones++;
                    found1 = G_TRUE;
                    found0 = G_FALSE;
                    break;
                 }
                 else if ( jj == npline-1 ) {
                    don = G_TRUE;
                    break;
                 }
                 jj++;
             }
             ibegin = iones [ nones - 1 ];
             if ( !found0 && !found1 ) don = G_TRUE;
         }

        /*
         * Check if first and last segments need to be stitched together
         */
         if ( nzeros > nones && !inout[0] )  {
            idum = iones [0];
            for (jj = 0; jj<nzeros-1; jj++) {
               iones  [jj] = iones[jj+1];
               izeros [jj] = izeros[jj+1];
            }
            nzeros = nzeros - 1;
            iones [nzeros-1] = idum;
         }
         else if ( nzeros > nones && inout[0] ) {
            iones[nzeros-1] = 0;
         }

        /*
         *  Start processing each segment to exclude dublicate points
         */ 
         nseg = nzeros;
         *nout = 0;
    	 for (ii = 0; ii < nseg; ii++) {
             addOnePoint = G_FALSE;
             jAddPoint = 0;
             jst = izeros[ii]-1;
             jfn = iones[ii];

             npseg = jfn - jst + 1;
             if ( jst != 0 && jfn == npline-1 && nseg > 1 ) {
                jfn = 0;
                npseg = jfn - jst + 1;
             }
             else if ( jst == -1 ) {
                addOnePoint = G_TRUE;
                jAddPoint = npline-1;
                npseg = jfn + 1;
                jst = 0;
             }

             if ( npseg < 0 ) {
                 /*
                  * Need to connect segments of the line first
                  */
                  npseg = npline - jst + jfn;
                  kk = 0;
                 /*
                  *  1st part
                  */
                  for (jj = jst; jj < npline; jj++) {
                     dum_lats [kk] = line_lats[jj];
                     dum_lons [kk] = line_lons[jj];
                     kk++;
                  }
                 /*
                  *  2nd part
                  */
                  for (jj = 0; jj <= jfn; jj++) {
                     dum_lats [kk] = line_lats[jj];
                     dum_lons [kk] = line_lons[jj];
                     kk++;
                  }
                  jst = 0;
                  jfn = kk-1;
                  for (jj = jst; jj <= jfn; jj++) {
                     line_lats[jj] = dum_lats [jj];
                     line_lons[jj] = dum_lons [jj];
                  }
             }

            /*
             * Exclude identical points
             */
             for (jj = jst; jj < jfn; jj++) {
                  if ( ( G_DIFFT(line_lats[jj], line_lats[jj+1], 0.05F) &&
                         G_DIFFT(line_lons[jj], line_lons[jj+1], 0.05F) ) ) {
                          line_lats[jj]= RMISSD;
                          line_lons[jj]= RMISSD;
                          jj--;
                  }
             }

            /*
             * Check the distance between the ending and starting points
             * exclude them if they are too close
             */
             cgr_dist ( one, &line_lats[jfn], &line_lons[jfn],
                         line_lats[jst], line_lons[jst],
                         &dum, &idum, &ier);

             if ( ( dum < distmark ) && ( nip !=0 ) ) {
                          line_lats[jfn]= RMISSD;
                          line_lons[jfn]= RMISSD;
                          line_lats[jst]= RMISSD;
                          line_lons[jst]= RMISSD;
             }

            /*
             *  Store the unique and valid points
             */
             kk = 0;
             for (jj = jst; jj <= jfn; jj++) {
                  if ( !G_DIFF(line_lats[jj],0.0F) && !G_DIFF(line_lons[jj],0.0F) &&
                       !ERMISS(line_lats[jj]) && !ERMISS(line_lons[jj]) ) {
                     dum_lats [kk] = line_lats[jj];
                     dum_lons [kk] = line_lons[jj];
                     kk++;
                  }
             }
             npseg = kk;

             if ( addOnePoint ) {
                for (kk=0; kk < npseg; kk++) {
                    dum_lats2[kk] = dum_lats[kk];
                    dum_lons2[kk] = dum_lons[kk];
                }
                dum_lats[0] = line_lats[jAddPoint];
                dum_lons[0] = line_lons[jAddPoint];
                mj=1;
                for (kk=0; kk < npseg; kk++) {
                    dum_lats[mj] = dum_lats2[kk];
                    dum_lons[mj] = dum_lons2[kk];
                    mj++;
                }
                npseg=mj;
             }
            /*
             *  Keep only the segments consisting of at least two points 
             */
             if ( npseg > 1 ) {
                 if ( *nout == 0 ) {
                    G_MALLOC ( (*el_out), VG_DBStruct, one, "p2c_OpenCntr: el_out" );
                 }
                 else  {
                    G_REALLOC ( (*el_out), VG_DBStruct, (*nout+1), "p2c_OpenCntr: el_out" );
                 }

                /*
                 * Make a copy of the element
                 */
                 p2c_CopyCntr ( &(*el_in), &(*el_out)[*nout], &ier);

                /*
                 * Overwrite number fo points closed flag, recsz and
                 * point coords
                 */
                 if ( npseg != npline ) {
                     (*el_out)[*nout].hdr.closed = 0;
                     (*el_out)[*nout].elem.spl.info.numpts = npseg;
                     (*el_out)[*nout].hdr.recsz = sizeof(VG_HdrStruct) 
                                                + sizeof(SpLineInfo)
                                                + npseg*2*sizeof(float);
                 }
                 for (jj = 0; jj < npseg; jj++) {
                      (*el_out)[*nout].elem.spl.latlon[ jj ]        = dum_lats[jj];
                      (*el_out)[*nout].elem.spl.latlon[ npseg + jj] = dum_lons[jj];
                 }
                 (*nout)++;
             }
         }
         if ( (*nout) == 1 && dstnseg < distmark ) {
             (*el_out)[*nout-1].hdr.closed = 0;
         }
    }
    G_FREE ( inout, int );
#ifdef P2C_NEWPOLY
    G_FREE ( pointInout, int );
#endif
}

/*=====================================================================*/

static void p2c_Util ( VG_DBStruct el, int *np, int *gtype, int *gnmbr, 
                       float *xout, float *yout)
/************************************************************************
 * p2c_Util                                                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *      el		VG_DBStruct     outlook element			*
 *      *el_in          VG_DBStruct     array of outlook elements       *
 *                                                                      *
 * Output parameters:                                                   *
 *      *np           	int             number of points in contour	*
 *      *gtype        	int             contour group type		*
 *      *gnmbr        	int             contour group number		*
 *      *xout        	float           contour lats			*
 *      *yout        	float           contour lons			*
 *                                                                      *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 ***********************************************************************/
{
    int         npline, ii;
    float       line_lats[MAXPTS], line_lons[MAXPTS], *ptr1lat, *ptr1lon;
/*---------------------------------------------------------------------*/
    npline = el.elem.spl.info.numpts;
    ptr1lat = &(el.elem.spl.latlon[0]);
    ptr1lon = &(el.elem.spl.latlon[npline]);
    memcpy ( line_lats, ptr1lat, (size_t)npline*sizeof(float) );
    memcpy ( line_lons, ptr1lon, (size_t)npline*sizeof(float) );
    for ( ii = 0; ii < npline; ii++) { 
       xout [ ii ] = line_lats [ii];
       yout [ ii ] = line_lons [ii];
    }
    *np = npline;
    *gtype = el.hdr.grptyp;
    *gnmbr = el.hdr.grpnum;
}

/*=====================================================================*/

static void p2c_BumpCat ( VG_DBStruct *el, int ind, char *newcat, int *done,
                          int *iret)
/************************************************************************
 * p2c_BumpCat                                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *       ind          	int             index number			*
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *el          VG_DBStruct     outlook element		        *
 *                                                                      *
 * Output parameters:                                                   *
 *      *newcat          char            new category of the element	*
 *      *done		int		TRUE - element category bumped	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 ***********************************************************************/
{
    float       *xel, *yel, *xsev, *ysev, *xnew, *ynew;
    float       area_el, area_sev, xC, yC;
    int         np_el, np_sev, idum, maxp =100, nip, ier, npnew;
    int         bp1 [MAXPTS], ap1[MAXPTS], bp2[MAXPTS], ap2[MAXPTS];
    int         one = 1, inout, npmx = MAXPTS;
    float       ipx[MAXPTS], ipy[MAXPTS];
    gpc_polygon gpc_poly_union, gpc_poly_0, gpc_poly_1;
/*---------------------------------------------------------------------*/
    *done = G_FALSE;
    strcpy ( newcat, "" );
    G_MALLOC ( xel, float,  npmx, "p2c_BumpCat: xel" );
    G_MALLOC ( yel, float,  npmx, "p2c_BumpCat: yel" );
    G_MALLOC ( xsev, float, npmx, "p2c_BumpCat: xsev" );
    G_MALLOC ( ysev, float, npmx, "p2c_BumpCat: ysev" );
    p2c_Util ( *el, &np_el, &idum, &idum, xel, yel);
    p2c_Util ( elIn[ind], &np_sev, &idum, &idum, xsev, ysev);
    cgr_intersect ( sys_M, &np_el,  xel,  yel,
                    sys_M, &np_sev, xsev, ysev,
                    &maxp, sys_M, &nip, ipx, ipy,
                    bp1, ap1, bp2, ap2, &ier );
    if ( nip == 0 ) {
       cgr_centroid ( xel,  yel,  &np_el,  &xC, &yC, &area_el,  &ier );
       cgr_centroid ( xsev, ysev, &np_sev, &xC, &yC, &area_sev, &ier );
       if ( area_el > area_sev ) {
           cgr_inpoly ( sys_M, &one, &xsev[0], &ysev[0],
                        sys_M, &np_el, xel, yel,
                        &inout, &ier );
       }
       else if ( area_el < area_sev ) {
           cgr_inpoly ( sys_M, &one, &xel[0], &yel[0],
                        sys_M, &np_sev, xsev, ysev,
                        &inout, &ier );
       }
    }
    if ( nip > 0 || inout > 0 ) {
      if ( el->hdr.grptyp == 12 ) strcpy (newcat, "_MDRT");
      if ( el->hdr.grptyp == 14 ) strcpy (newcat, "_HIGH");
      *done = G_TRUE;
      p2c_Elm2Poly ( *el, &gpc_poly_0, &ier );
      p2c_Elm2Poly ( elIn[ind], &gpc_poly_1, &ier );
      gpc_polygon_clip ( GPC_INT, &gpc_poly_0, &gpc_poly_1,
                          &gpc_poly_union );
      gpc_free_polygon ( &gpc_poly_0 );
      gpc_free_polygon ( &gpc_poly_1 );

      npnew = gpc_poly_union.contour[0].num_vertices;

      el->elem.spl.info.numpts = npnew;

      G_MALLOC ( xnew, float, npnew, "p2c_BumpCat: xnew" );
      G_MALLOC ( ynew, float, npnew, "p2c_BumpCat: ynew" );
                                                                                                                      
      gpc_gvlist ( &gpc_poly_union.contour[0], &npnew, xnew, ynew, &ier );

      gtrans ( sys_N, sys_M, &npnew, xnew, ynew,
              &(el->elem.spl.latlon[ 0  ]),
              &(el->elem.spl.latlon[ npnew ]),
              &ier, strlen(sys_N), strlen(sys_M) );

      G_FREE ( xnew, float );
      G_FREE ( ynew, float );
      G_FREE ( xel, float );
      G_FREE ( yel, float );
      G_FREE ( xsev, float );
      G_FREE ( ysev, float );

      return;
    }
}

/*=====================================================================*/

static void p2c_SetArea ( int *iret)
/************************************************************************
 * p2c_SetArea                                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 ***********************************************************************/

{
    int                 minp, maxp, npts, ier, ii;
    float               *xlat, *ylon;
    float               filter;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Set bounds type and tag
     */
    clo_bstype ( _boundsType, &ier );
 
    if ( ier !=  0 ) {
       *iret = -1;
       return;
    }
    clo_bstag  ( _boundsTag, &ier );
    if ( ier !=  0 ) {
       *iret = -2;
       return;
    }

    /*
     * Initialization
     */
    minp   = 0;
    maxp   = MAXPTS;
    filter = 0.0F;

    /*
     * Read each bounds part and put it in a gpc polygon
     */
    G_MALLOC ( xlat, float, maxp, "p2c_SetArea: xlat" );
    G_MALLOC ( ylon, float, maxp, "p2c_SetArea: ylon" );

    clo_bgnext ( &minp, &maxp, &filter, &npts, xlat, ylon, &ier );
    if ( ier !=  0 ) {
       *iret = -3;
       return;
    }

    /*
     *  Save off boundary points  - keep only TWO digits for later use
     *  in cgr_intersect().
     */
    _areaP = npts;
    for ( ii = 0; ii < npts; ii++ ) {
       _areaX [ ii ] = rint ( (double)xlat[ ii ] * 1000 ) / 1000;
       _areaY [ ii ] = rint ( (double)ylon[ ii ] * 1000 ) / 1000;
    }

    G_FREE ( xlat, float );
    G_FREE ( ylon, float );

}

/*=====================================================================*/
                                                                                                                 
static void p2c_AddGenT ( int interp_enh, int nInEnh1, VG_DBStruct *el_enh1,
                          int *nout, VG_DBStruct **outgt, int *iret )
/************************************************************************
 * p2c_AddGenT                                                          *
 *                                                                      *
 * Input parameters:                                                    *
 *	interp_enh	int		flag to interpolate in time	*
 *      nInEnh          int             number of input elements        *
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *el_en          VG_DBStruct     enhanced thunderstorm element   *
 *                                                                      *
 * Output parameters:                                                   *
 *      *nout           int             number of output elements       *
 *      **outgt         VG_DBStruct     array of General Thunder cntrs	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 * m.gamazaychikov/CWS	06/09	Change CS to OpenCntr, made acommadating*
 *                              changes to the code                     *
 ***********************************************************************/
{
    int                 ii, kk, jj, nn, ier, np, nseg, one=1, nInEnh, nCl;
    int                 np1, np2, idum, *grouped;
    int                 maxNewPts       = MAXPTS;
    int                 numMapPts       = 0;
    int                 newPts          = 0;
    float               xNew[MAXPTS], yNew[MAXPTS];
    float               *xMap1 = NULL, *yMap1 = NULL;
    float               *xMap2 = NULL, *yMap2 = NULL;
                                                                                                                 
    float               dist, pct = 0.5;
    float               *x1, *x2, *y1, *y2, tlrnc = 100.;
    VG_DBStruct         *outlk2_enh=NULL, el, *el_enh=NULL, *el_enhCl=NULL;
 /*---------------------------------------------------------------------*/
                                                                                                                 
    *iret = 0;
    *nout = 0;
    G_MALLOC ( x1, float, maxNewPts, "p2c_AddGenT: x1" );
    G_MALLOC ( y1, float, maxNewPts, "p2c_AddGenT: y1" );
    G_MALLOC ( x2, float, maxNewPts, "p2c_AddGenT: x2" );
    G_MALLOC ( y2, float, maxNewPts, "p2c_AddGenT: y2" );
    G_MALLOC ( el_enh, VG_DBStruct, 10*nInEnh1, "p2c_AddGenT: el_enh" );
    nn = 0;
    for ( ii = 0; ii < nInEnh1; ii++ ) {
         if ( !el_enh1[ii].hdr.closed ) {
            p2c_CloseCntr ( &el_enh1[ii], &el_enhCl, &nCl, &ier);
            for ( kk = 0; kk < nCl; kk++ ) {
               p2c_CopyCntr ( &el_enhCl[kk], &el_enh[nn], &ier);
               nn++;
            }
         }
         else {
            p2c_CopyCntr ( &el_enh1[ii], &el_enh[nn], &ier);
            nn++;
         }
    }

    nInEnh = nn;
    G_MALLOC ( grouped, int, nInEnh, "p2c_AddGenT: grouped" );
    for ( ii = 0; ii < nInEnh; ii++ ) {
         grouped[ ii ] = G_FALSE;
    }
    for ( ii = 0; ii < nInEnh; ii++ ) {
       dist = RMISSD;
       if ( !interp_enh ) {
         p2c_OpenCntr ( &el_enh[ii], &outlk2_enh, &nseg, &ier);
       }
       else if ( interp_enh ) {
      /*
       * Need to find and interpolate the contours for 01Z cyle
       */
                                                                                                                 
         /*
          * Skip the element that has already been grouped
          */
          if (  grouped[ii] ) {
            continue;
          }
                                                                                                                 
          for ( kk = ii+1; kk < nInEnh; kk++ ) {
            p2c_Util ( el_enh[ii], &np1, &idum, &idum, x1, y1);
            p2c_Util ( el_enh[kk], &np2, &idum, &idum, x2, y2);
            clo_dist (&x1[0], &y1[0], &one, &x2[0], &y2[0], &dist, &ier);
           /*
            * Matcing contours - interpolate them
            */
            if (dist/1000. < tlrnc) {
              grouped[ kk ] = True;
              /*
               * opened line interpolation
               */
              if ( !el_enh[ii].hdr.closed && !el_enh[kk].hdr.closed) {
                cgr_lineinterp ( &np1, x1, y1, &np2, x2, y2, &pct,
                              &maxNewPts, &newPts, xNew, yNew, &ier );
                el.hdr.closed = G_FALSE;
              }
              /*
               * polygon interpolation
               */
              else if ( el_enh[ii].hdr.closed && el_enh[kk].hdr.closed) {
                cgr_polyinterp ( &np1, x1, y1, &np2, x2, y2, &pct,
                              &numMapPts, xMap1, yMap1, xMap2, yMap2,
                              &maxNewPts, &newPts, xNew, yNew, &ier );
                el.hdr.closed = G_TRUE;
                                                                                                                 
              }
              /*
               * Need to fill out element structure
               */
              el.hdr.vg_class = CLASS_LINES;
              el.hdr.vg_type  = 20;
              el.hdr.delete = G_FALSE;
              el.hdr.maj_col = 18;
              el.hdr.min_col = 18;
              el.hdr.grptyp = 7;
              el.hdr.grpnum = el_enh[ii].hdr.grpnum;
              el.hdr.version = 0;
              el.hdr.filled = 0;
              el.hdr.smooth = 0;
              el.hdr.recsz = sizeof(VG_HdrStruct) + sizeof(SpLineInfo) + newPts*2*sizeof(float);
              el.hdr.delete = G_FALSE;
              el.hdr.closed = el_enh[ii].hdr.closed;
                                                                                                                 
              el.elem.spl.info.numpts = newPts;
              el.elem.spl.info.spltyp = 4;
              el.elem.spl.info.splstr = 1;
              el.elem.spl.info.spldir = 1;
              el.elem.spl.info.splsiz = 0.5;
              el.elem.spl.info.splwid = 5;
              for ( nn = 0; nn < newPts; nn ++ ) {
                el.elem.spl.latlon[nn]        = xNew[nn];
                el.elem.spl.latlon[nn+newPts] = yNew[nn];
              }
              p2c_OpenCntr ( &el, &outlk2_enh, &nseg, &ier);
              cvg_freeElPtr ( &el );
              break;
            }
            /*
             * Unmatching contours - no interpolation
             */
            else dist = RMISSD;
          }                                    /* end of kk inner loop */
          if ( G_DIFF (dist, RMISSD) ) {
            p2c_OpenCntr ( &el_enh[ii], &outlk2_enh, &nseg, &ier);
          }
                                                                                                                 
       }                                    /* end of interp test */
                                                                                                                 
       for ( jj = 0; jj < nseg; jj++ ) {
         if ( *nout == 0 ) {
            G_MALLOC ( (*outgt), VG_DBStruct, one, "p2c_AddGenT: outgt" );
         }
         else {
            G_REALLOC ( (*outgt), VG_DBStruct, (*nout+1), "p2c_AddGenT: outgt" );
         }
         np = outlk2_enh[jj].elem.spl.info.numpts;
                                                                                                                 
         (*outgt)[*nout].hdr.vg_class = CLASS_LINES;
         (*outgt)[*nout].hdr.vg_type  = 20;
         (*outgt)[*nout].hdr.delete = G_FALSE;
         (*outgt)[*nout].hdr.maj_col = 18;
         (*outgt)[*nout].hdr.min_col = 18;
         (*outgt)[*nout].hdr.grptyp = 7;
         (*outgt)[*nout].hdr.grpnum = outlk2_enh[jj].hdr.grpnum;
         (*outgt)[*nout].hdr.version = 0;
         (*outgt)[*nout].hdr.filled = 0;
         (*outgt)[*nout].hdr.smooth = 0;
         (*outgt)[*nout].hdr.recsz = sizeof(VG_HdrStruct) + sizeof(SpLineInfo) + np*2*sizeof(float);
         (*outgt)[*nout].hdr.delete = G_FALSE;
         (*outgt)[*nout].hdr.closed = outlk2_enh[jj].hdr.closed;
                                                                                                                 
         (*outgt)[*nout].elem.spl.info.numpts = np;
         (*outgt)[*nout].elem.spl.info.spltyp = 4;
         (*outgt)[*nout].elem.spl.info.splstr = 1;
         (*outgt)[*nout].elem.spl.info.spldir = 1;
         (*outgt)[*nout].elem.spl.info.splsiz = 0.5;
         (*outgt)[*nout].elem.spl.info.splwid = 2;
         for ( nn = 0; nn < np; nn ++ ) {
           (*outgt)[*nout].elem.spl.latlon[nn]    = outlk2_enh[jj].elem.spl.latlon[nn];
           (*outgt)[*nout].elem.spl.latlon[nn+np] = outlk2_enh[jj].elem.spl.latlon[nn+np];
         }
                                                                                                                 
         (*nout)++;
       }
    }                                    /* end of ii outer loop */
    G_FREE ( el_enh, VG_DBStruct );
    G_FREE ( grouped, int );
    G_FREE ( x1, float );
    G_FREE ( y1, float );
    G_FREE ( x2, float );
    G_FREE ( y2, float );
                                                                                                                 
}

/*=====================================================================*/
                                                                                                                 
static void p2c_GetGenT ( char *Gt_str, int nInEnh, VG_DBStruct *el_enh,
                          int *nout, VG_DBStruct **el_Gt, int *iret )
/************************************************************************
 * p2c_GetGenT                                                          *
 *                                                                      *
 * Input parameters:                                                    *
 * 	Gt_str		char		txt lbl for General Thunder line*
 *      nInEnh          int             number of input elements        *
 *      *el_enh         VG_DBStruct     enhanced thunderstorm element   *
 *                                                                      *
 * Output parameters:                                                   *
 *      *nout           int             number of output elements       *
 *      **el_Gt         VG_DBStruct     array of General Thunder cntrs	*
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	10/06	Created                         	*
 ***********************************************************************/
{
    int                 ii, kk, nn, np, one=1;
/*---------------------------------------------------------------------*/
                                                                                                                 
    *iret = 0;
    *nout = 0;
                                                                                                                 
    for ( ii = 0; ii < nInEnh; ii++ ) {
      if (  el_enh[ ii ].hdr.vg_type == SPLN_ELM ) {
        for ( kk = 0; kk < nInEnh; kk++ ) {
           if (  el_enh[ kk ].hdr.vg_type == SPTX_ELM &&
              (  el_enh[ ii ].hdr.grptyp == el_enh[ kk ].hdr.grptyp)  &&
              (  el_enh[ ii ].hdr.grpnum == el_enh[ kk ].hdr.grpnum)  &&
              (  el_enh[ ii ].hdr.maj_col == el_enh[ kk ].hdr.maj_col)  &&
              ( strcmp (el_enh[ kk ].elem.spt.text, Gt_str) == 0 ) ) {
                if ( *nout == 0 ) {
                  G_MALLOC ( (*el_Gt), VG_DBStruct, one, "p2c_GetGenT: el_Gt" );
                }
                else {
                  G_REALLOC ( (*el_Gt), VG_DBStruct, (*nout+1), "p2c_GetGenT: el_Gt" );
                }

                np = el_enh[ii].elem.spl.info.numpts;
                                                                                                                 
                (*el_Gt)[*nout].hdr.vg_class = CLASS_LINES;
                (*el_Gt)[*nout].hdr.vg_type  = 20;
                (*el_Gt)[*nout].hdr.delete = G_FALSE;
                (*el_Gt)[*nout].hdr.maj_col = 18;
                (*el_Gt)[*nout].hdr.min_col = 18;
                (*el_Gt)[*nout].hdr.grptyp = 7;
                (*el_Gt)[*nout].hdr.grpnum = el_enh[ii].hdr.grpnum;
                (*el_Gt)[*nout].hdr.version = 0;
                (*el_Gt)[*nout].hdr.filled = 0;
                (*el_Gt)[*nout].hdr.smooth = 0;
                (*el_Gt)[*nout].hdr.recsz = sizeof(VG_HdrStruct) + sizeof(SpLineInfo) + np*2*sizeof(float);
                (*el_Gt)[*nout].hdr.delete = G_FALSE;
                (*el_Gt)[*nout].hdr.closed = G_TRUE;
                (*el_Gt)[*nout].hdr.closed = el_enh[ii].hdr.closed;
                                                                                                                 
                (*el_Gt)[*nout].elem.spl.info.numpts = np;
                (*el_Gt)[*nout].elem.spl.info.spltyp = 4;
                (*el_Gt)[*nout].elem.spl.info.splstr = 1;
                (*el_Gt)[*nout].elem.spl.info.spldir = 1;
                (*el_Gt)[*nout].elem.spl.info.splsiz = 0.5;
                (*el_Gt)[*nout].elem.spl.info.splwid = 5;
                for ( nn = 0; nn < np; nn ++ ) {
                  (*el_Gt)[*nout].elem.spl.latlon[nn]    = el_enh[ii].elem.spl.latlon[nn];
                  (*el_Gt)[*nout].elem.spl.latlon[nn+np] = el_enh[ii].elem.spl.latlon[nn+np];
                }
                                                                                                                 
                (*nout)++;
                break;
           }
        }
      }
    }
}

/*=====================================================================*/

static void p2c_CorctCntr ( VG_DBStruct *el_in, int *iret )
/************************************************************************
 * p2c_CorctCntr                                                        *
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *el_in          VG_DBStruct     contour	element			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	05/08	Created                         	*
 * m.gamazaychikov/SAIC	06/09	Change the correction algorithm         *
 ***********************************************************************/
{
    int         hole = 0, nip, ii,  npline, ier, nout;
    int         maxp = MAXPTS;
    int         bp1 [MAXPTS], ap1[MAXPTS], bp2[MAXPTS], ap2[MAXPTS];

    float               line_lats2[MAXPTS], line_lons2[MAXPTS];
    float               x_int[MAXPTS], y_int[MAXPTS];
    float               *line_lats2n, *line_lons2n;
    float               *areaXn, *areaYn;
    gpc_vertex_list     verts;
    gpc_polygon         contour_poly, outlkarea_poly, poly_int;
 /*--------------------------------------------------------------------*/
    *iret = 0;
    npline = el_in->elem.spl.info.numpts;

    for ( ii = 0; ii < npline; ii++ ) {
        line_lats2 [ ii ] = el_in->elem.spl.latlon[   ii];
        line_lons2 [ ii ] = el_in->elem.spl.latlon[   ii + npline ];
        line_lats2 [ ii ] = rint ( (double)line_lats2[ ii ] * 1000 ) / 1000;
        line_lons2 [ ii ] = rint ( (double)line_lons2[ ii ] * 1000 ) / 1000;
    }

    nip = 0;
   /*
    * Find if the contour intersects the SPC Outlook area boundaries
    */
    cgr_intersect ( sys_M, &npline, line_lats2, line_lons2,
                    sys_M, &_areaP, _areaX, _areaY,
                    &maxp, sys_M, &nip, x_int, y_int, 
                    bp1, ap1, bp2, ap2, &ier );
   /*
    * If it intersects - clip it along the boundary
    */
    if ( nip > 0 ) {
        /*
        * Convert OUTLOOK_AREA bounds coordinates to normalized coordinates.
        */
        G_MALLOC ( areaXn, float, _areaP, "p2c_CorctCntr: areaXn" );
        G_MALLOC ( areaYn, float, _areaP, "p2c_CorctCntr: areaYn" );

        gtrans ( sys_M, sys_N, &_areaP, _areaX, _areaY,
                areaXn, areaYn, &ier, strlen(sys_M), strlen(sys_N) );
       /*
        * Convert contour coordinates to normalized coordinates.
        */
        G_MALLOC ( line_lats2n, float, npline, "p2c_CorctCntr: line_lats2n" );
        G_MALLOC ( line_lons2n, float, npline, "p2c_CorctCntr: line_lons2n" );

        gtrans ( sys_M, sys_N, &npline, line_lats2, line_lons2,
                 line_lats2n, line_lons2n, &ier, strlen(sys_M), strlen(sys_N) );

       /*
        * Fill GPC contour polygon structure.
        */
        contour_poly.num_contours = 0;
        contour_poly.hole         = (int*)NULL;
        contour_poly.contour      = (gpc_vertex_list*)NULL;
        verts.vertex          = (gpc_vertex*)NULL;
        verts.num_vertices    = 0;
        gpc_cvlist ( npline, line_lats2n, line_lons2n, &verts, &ier );
        contour_poly.num_contours = 0;
        gpc_add_contour ( &contour_poly, &verts, hole );
        free ( verts.vertex );
        G_FREE ( line_lats2n, float );
        G_FREE ( line_lons2n, float );

       /*
        * Fill GPC contour polygon structure.
        */
        outlkarea_poly.num_contours = 0;
        outlkarea_poly.hole         = (int*)NULL;
        outlkarea_poly.contour      = (gpc_vertex_list*)NULL;
        verts.vertex          = (gpc_vertex*)NULL;
        verts.num_vertices    = 0;
        gpc_cvlist ( _areaP, areaXn, areaYn, &verts, &ier );
        outlkarea_poly.num_contours = 0;
        gpc_add_contour ( &outlkarea_poly, &verts, hole );
        free ( verts.vertex );
        G_FREE ( areaXn, float );
        G_FREE ( areaYn, float );

       /*
        * Find the intersection of two polygons, contour and OUTLOOK_AREA bounds
        */
        gpc_polygon_clip ( GPC_INT, &contour_poly, &outlkarea_poly, &poly_int );
        gpc_free_polygon ( &contour_poly );
        gpc_free_polygon ( &outlkarea_poly );

       /*
        * Transform polygon structure to lat lon
        */
        nout = poly_int.contour[0].num_vertices;;
        G_MALLOC ( line_lats2n, float, nout, "p2c_CorctCntr: line_lats2n" );
        G_MALLOC ( line_lons2n, float, nout, "p2c_CorctCntr: line_lons2n" );

        gpc_gvlist ( &poly_int.contour[0], &nout, line_lats2n, line_lons2n, &ier );
        gtrans ( sys_N, sys_M, &nout, line_lats2n, line_lons2n,
                 &(el_in->elem.spl.latlon[ 0 ]),
                 &(el_in->elem.spl.latlon[ nout ]),
                 &ier, strlen(sys_N), strlen(sys_M) );
        G_FREE ( line_lats2n, float );
        G_FREE ( line_lons2n, float );
        el_in->elem.spl.info.numpts = nout;
        el_in->hdr.closed = 0;
        el_in->hdr.recsz = sizeof(VG_HdrStruct) + sizeof(SpLineInfo) + nout*2*sizeof(float);
    }
}

/*=====================================================================*/

static void p2c_AdjustLbl ( int nin, VG_DBStruct *el_in, int *iret )
/************************************************************************
 * p2c_AdjustLbl                                                        *
 *                                                                      *
 * Input/Output parameters:                                             *
 *      nij             int             number of contours		*
 *      *el_in          VG_DBStruct     array of contours		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	05/08	Created                         	*
 ***********************************************************************/
{
    int         ii, jj, kk,  npline, one = 1, ier;
    float       distance, tooClose=100.0;
 /*--------------------------------------------------------------------*/
    *iret = 0;
    for ( ii = 0; ii < nin; ii++ ) {
        if ( el_in[ii].hdr.vg_type == 21) {
           for ( jj = ii; jj < nin; jj++ ) {
               if ( ( el_in[jj].hdr.vg_type == 21 ) && 
                    ( el_in[jj].hdr.grpnum != el_in[ii].hdr.grpnum ) ) {
                    clo_dist (&el_in[ii].elem.spt.info.lat, 
                              &el_in[ii].elem.spt.info.lon,
                              &one, 
                              &el_in[jj].elem.spt.info.lat, 
                              &el_in[jj].elem.spt.info.lon,
                              &distance, &ier);
                   /*
                    distance = (float) G_DIST (el_in[ii].elem.spt.info.lat, 
                                          el_in[ii].elem.spt.info.lon,
                                          el_in[jj].elem.spt.info.lat, 
                                          el_in[jj].elem.spt.info.lon);
                    * If the distance less than 100 km then put the labels apart 
                    */
                    if ( distance/1000.0 < tooClose ) {
                       for ( kk = 0; kk < nin; kk++ ) {
                           if ( ( el_in[kk].hdr.vg_type == 20 ) && 
                                ( el_in[jj].hdr.grpnum == el_in[kk].hdr.grpnum ) ) {
                              npline = el_in[kk].elem.spl.info.numpts;
                              el_in[jj].elem.spt.info.lat = el_in[kk].elem.spl.latlon[0];
                              el_in[jj].elem.spt.info.lon = el_in[kk].elem.spl.latlon[npline];
                           } 
                           if ( ( el_in[kk].hdr.vg_type == 20 ) &&
                                ( el_in[ii].hdr.grpnum == el_in[kk].hdr.grpnum ) ) {
                              npline = el_in[kk].elem.spl.info.numpts;
                              el_in[ii].elem.spt.info.lat = el_in[kk].elem.spl.latlon[npline-1];
                              el_in[ii].elem.spt.info.lon = el_in[kk].elem.spl.latlon[npline+npline-1];
                           }
                       } 
                    }
               }  
           } 
        } 
    } 
}

/*=====================================================================*/

static void p2c_ExtendCntr ( int *firstPoint, int *lastPoint, 
                             int *npIn,  float *xIn,  float *yIn, 
                             int *npOut, float *xOut, float *yOut, 
                             int *iret)
/************************************************************************
 * p2c_ExtendCntr                                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC	06/09	Created                         	*
 ***********************************************************************/
{ 
    int         ier, kk, nn;
    int		iFirst, iLast, one=1, pointIn, checkAll = 1;

    float       akof=1.0F;
    float	angFirst, latFirst, lonFirst;
    float	angLast, latLast, lonLast;
    float       distFirst, distLast;
/*---------------------------------------------------------------------*/

    *iret = 0;
    if ( *lastPoint && !*firstPoint ) {
              /*
               * Extend the last end point
               */
               cgr_dist ( _areaP, _areaX, _areaY, xIn[*npIn-1], yIn[*npIn-1],
                          &distLast, &iLast, &ier);
         
               cgr_dang ( &xIn[*npIn-2], &yIn[*npIn-2], 
                          &xIn[*npIn-1], &yIn[*npIn-1], &angLast, &ier);
               angLast*=DTR;
               latLast = xIn[*npIn-1] + akof* distLast * cos ( (float) angLast );
               lonLast = yIn[*npIn-1] + akof* distLast * sin ( (float) angLast );
              /*
               * continue extention of the point coords until the point is outside 
               * the SPC Outlook bounds pointIn = 0 while doubling the extention distance
               */
               cgr_inpolywn ( one, &latLast, &lonLast, _areaP, _areaX, _areaY, checkAll,
                              &pointIn, &ier );
               while ( pointIn ) {
                   akof*= 2.0F;
                   latLast = xIn[*npIn-1] + akof* distLast * cos ( (float) angLast );
                   lonLast = yIn[*npIn-1] + akof* distLast * sin ( (float) angLast );
                   cgr_inpolywn ( one, &latLast, &lonLast, _areaP, _areaX, _areaY,checkAll,
                                  &pointIn, &ier );
               }

               for ( kk = 0; kk < *npIn; kk++) {
                  xOut[kk] = xIn[kk];
                  yOut[kk] = yIn[kk];
               }
               xOut[*npIn] = latLast;
               yOut[*npIn] = lonLast;
        
               *npOut = *npIn +1;
    }
    else if ( *firstPoint && !*lastPoint ) {
              /*
               * Extend the first end point
               */
               cgr_dist ( _areaP, _areaX, _areaY, xIn[0], yIn[0],
                      &distFirst, &iFirst, &ier);
               cgr_dang ( &xIn[1], &yIn[1], &xIn[0], &yIn[0], &angFirst, &ier);
               angFirst*=DTR;
               latFirst = xIn[0] + akof* distFirst * cos ( (float) angFirst );
               lonFirst = yIn[0] + akof* distFirst * sin ( (float) angFirst );
              /*
               * continue extention of the point coords until the point is outside 
               * the SPC Outlook bounds pointIn = 0 while doubling the extention distance
               */
               cgr_inpolywn ( one, &latFirst, &lonFirst, _areaP, _areaX, _areaY, checkAll,
                              &pointIn, &ier );
               while ( pointIn ) {
                   akof*= 2.0F;
                   latFirst = xIn[0] + akof* distFirst * cos ( (float) angFirst );
                   lonFirst = yIn[0] + akof* distFirst * sin ( (float) angFirst );
                   cgr_inpolywn ( one, &latFirst, &lonFirst, _areaP, _areaX, _areaY,
                                 checkAll, &pointIn, &ier );
               }
               xOut[0] = latFirst;
               yOut[0] = lonFirst;
               nn = 1;
               for ( kk = 0; kk < *npIn; kk++) {
                 xOut[nn] = xIn[kk];
                 yOut[nn] = yIn[kk];
                 nn++;
               }
               *npOut = *npIn +1;
    }
    else if ( *firstPoint && *lastPoint ) {
           /*
            * Extend iboth the first and the last points.
            */
            cgr_dist ( _areaP, _areaX, _areaY, xIn[0], yIn[0],
                      &distFirst, &iFirst, &ier);
            cgr_dist ( _areaP, _areaX, _areaY, xIn[*npIn-1], yIn[*npIn-1],
                      &distLast, &iLast, &ier);
            cgr_dang ( &xIn[1],        &yIn[1], 
                       &xIn[0],        &yIn[0],        &angFirst, &ier);
            cgr_dang ( &xIn[*npIn-2], &yIn[*npIn-2], 
                       &xIn[*npIn-1], &yIn[*npIn-1], &angLast,  &ier);
            angFirst*=DTR;
            angLast*=DTR;
            latFirst  = xIn[0]        + akof * distFirst * cos ( (float) angFirst );
            lonFirst  = yIn[0]        + akof * distFirst * sin ( (float) angFirst );
            latLast   = xIn[*npIn-1] + akof * distLast  * cos ( (float) angLast );
            lonLast   = yIn[*npIn-1] + akof * distLast  * sin ( (float) angLast );

           /*
            * continue extention of the point coords until the point is outside 
            * the SPC Outlook bounds pointIn = 0 while doubling the extention distance
            *
            * first point:
            */
            cgr_inpolywn ( one, &latFirst, &lonFirst, _areaP, _areaX, _areaY,
                           checkAll, &pointIn, &ier );
            while ( pointIn ) {
                 akof*= 2.0F;
                 latFirst = xIn[0] + akof* distFirst * cos ( (float) angFirst );
                 lonFirst = yIn[0] + akof* distFirst * sin ( (float) angFirst );
                 cgr_inpolywn ( one, &latFirst, &lonFirst, _areaP, _areaX, _areaY,
                                checkAll, &pointIn, &ier );
            }
           /*
            * last point:
            */
            cgr_inpolywn ( one, &latLast, &lonLast, _areaP, _areaX, _areaY,
                           checkAll, &pointIn, &ier );
            while ( pointIn ) {
                 akof*= 2.0F;
                 latLast = xIn[*npIn-1] + akof* distLast * cos ( (float) angLast );
                 lonLast = yIn[*npIn-1] + akof* distLast * sin ( (float) angLast );
                 cgr_inpolywn ( one, &latLast, &lonLast, _areaP, _areaX, _areaY,
                               checkAll, &pointIn, &ier );
            }
            xOut[0] = latFirst;
            yOut[0] = lonFirst;
            nn = 1;
            for ( kk = 0; kk < *npIn; kk++) {
               xOut[nn] = xIn[kk];
               yOut[nn] = yIn[kk];
               nn++;
            }
            xOut[*npIn+1] = latLast;
            yOut[*npIn+1] = lonLast;
        
            *npOut = *npIn+2;
    }
}

/*=====================================================================*/

static void p2c_CloseSingleCntr ( int *npIn, float *xIn,  float *yIn, 
                                  int *nClp, float *xClp, float *yClp, 
                                  Boolean *InOut,
                                  int *nOut, float *xOut, float *yOut,  
                                  int *iret )
/************************************************************************
 * p2c_CloseSingleCntr                                                  *
 *                                                                      *
 * Input/Output parameters:                                             *
 *      *el_out          VG_DBStruct	contour				*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/CWS	06/09	Created                         	*
 * X.Guo/CWS		12/09	Added tolerance variable.Change its	*
 *				value to 0.001F from 0.01F for two	*
 *				points distance checking		*
 * X.Guo/CWS		03/10   Added codes to check intersecting points*
 * X.Guo/CWS   		09/10   Removed p2c_CheckPtOnPoly and check     *
 *                              intersection points in other func       * 
 * X.Guo/CWS		09/11   Adjusted the number of points needed to *
 *                              be copied from bounds                   *
 * S. Jacobs/NCEP	10/12	Restored previous calculation of the	*
 * 				number of points copied			*
 * S. Guan/NCEP         10/18  	Fixed the problem when nint1 = nint2    *
 * S. Guan/NCEP         11/18   Added InOut and adjust xClp and yClp    *  
 *                              to make sure coordinates of             * 
 *                              intersection points right.              * 
 ***********************************************************************/
{ 
    int         ier, ii, jj, ncp, nint1, nint2, npol2, intvrtx;
    int         rol, clsd=0, indx[MAXPTS];
    int         ncp1, ncp2; 

    float       nx2[MAXPTS], ny2[MAXPTS], dstns;
    float       comptol = 0.0F;
    float	tolerance = 0.001F;
    float	temp; 
/*---------------------------------------------------------------------*/

     *iret = 0;
    /*
     * Reorder the boundary points
     */
     clo_reorder(_areaP, _areaX, _areaY, indx, &ier);
     for (ii = 0; ii < _areaP; ii++) {
             nx2[ii] = _areaX[indx[ii]];
             ny2[ii] = _areaY[indx[ii]];
     }
    /*
     * Adjust xClp and yClp to make sure that 
     *  xClp[1]      yClp[1]      - coordinates of intersection point 1
     *  xClp[nClp-2], yClp[nClp-2] - coordinates of intersection point 2
     */
     for (ii = 0; ii < *nClp-1; ii++) {
         if (InOut[ii] == False && InOut[ii+1] == True )  ncp1 = ii; 
         if (InOut[ii] == True  && InOut[ii+1] == False ) ncp2 = ii;
     }
     if ( ncp1 > 1 ) {
         ii=1;
         for ( jj = ncp1; jj < ncp2+2; jj++) {
             xClp[ii] = xClp[jj];
             yClp[ii] = yClp[jj];
             ii++;
         }
     }
     *nClp = ncp2 + 4 - ncp1;

    /*
     *  Find the segments of boundary where the intersection points fall into
     *  xClp[1]      yClp[1]      - coordinates of intersection point 1
     *  xClp[nClp-2], yClp[nClp-2] - coordinates of intersection point 2
     *  intvrtx                    - closest vertex index 
     *  nint1                      - starting index of point in boundary
     *  nint2                      - ending index of point in boundary
     *
     * Intersection 1:
     */
     cgr_dist ( _areaP, nx2, ny2, xClp[1], yClp[1], &dstns, &intvrtx, &ier);

    /*
     *  Find the position of vertex relative to the line and assign nint1
       depending on the value of rol
     */
     cgr_qrol ( &(*npIn), xIn, yIn, &clsd, &nx2[intvrtx], &ny2[intvrtx],
                &comptol, &rol, &ier);
     if ( rol == 1 ) {
              nint1 = intvrtx-1;
     } else if ( rol == -1 ) {
              nint1 = intvrtx;
     } else if ( rol == 0 ) {
              nint1 = intvrtx-1;
     }

     if ( nint1 < 0 ) nint1 = _areaP-2;

    /*
     *  Adjust the intersection point if it lies too close to the boundary
     *  vertex to avoid problems with OpenCntr
     */
     temp = G_DIST (xClp[1], yClp[1], nx2[intvrtx], ny2[intvrtx]);
     if ( temp <= tolerance) {
         xClp[2] = ( nx2[nint1] + nx2[nint1+1] ) / 2.0F;
         yClp[2] = ( ny2[nint1] + ny2[nint1+1] ) / 2.0F;
     }

    /*
     *  Intersection 2: 
     */
     cgr_dist ( _areaP, nx2, ny2, xClp[*nClp-2], yClp[*nClp-2], &dstns, &intvrtx, &ier);

    /*
     *  Find the position of vertex relative to the line and assign nint2
     *  depending on the value of rol
     */

     cgr_qrol ( &(*npIn), xIn, yIn, &clsd, &nx2[intvrtx], &ny2[intvrtx],
                    &comptol, &rol, &ier);
     if ( rol == 1 ) {
              nint2 = intvrtx+1;
     } else if ( rol == -1 ) {
              nint2 = intvrtx;
     } else if ( rol == 0 ) {
              nint2 = intvrtx+1;
     }

     if ( nint2 > _areaP-1 ) nint2 = 0;

    /*
     *  Adjust the intersection point if it lies too close to the boundary
     *  vertex to avoid problems with OpenCntr
     */
     temp = G_DIST (xClp[1], yClp[1], nx2[intvrtx], ny2[intvrtx]);
     if ( temp <= tolerance) {
         xClp[2] = ( nx2[nint1] + nx2[nint1+1] ) / 2.0F;
         yClp[2] = ( ny2[nint1] + ny2[nint1+1] ) / 2.0F;
     }
     temp = G_DIST (xClp[*nClp-2], yClp[*nClp-2], nx2[intvrtx], ny2[intvrtx]);
     if ( temp <= tolerance) {
         xClp[*nClp-3] = ( nx2[nint2] + nx2[nint2+1] ) / 2.0F;
         yClp[*nClp-3] = ( ny2[nint2] + ny2[nint2+1] ) / 2.0F;
     }

    /*
     * Calculate the number of points needed to be copied over from boundary
     */
     jj = nint2;
     if ( nint1 >= nint2 ) {
            npol2 = nint1-nint2 + 1;
     } else {
            npol2 = _areaP-nint2+nint1+1;
	    /* npol2 = nint2-nint1+1; */
     }

    /*
     * Get points from the contour
     */
     xOut[0] = xClp[1];
     yOut[0] = yClp[1];
     ncp = 1;

     for (ii = 2; ii < *nClp-2; ii++) {
             xOut[ncp] = xClp[ii];
             yOut[ncp] = yClp[ii];
             ncp++;
     }

    /*
     *  Get points from boundary
     */
     if ( nint1 == nint2) npol2 = 2;
     for (ii = 0; ii<npol2;ii++) {
             if ( jj == _areaP ) jj=0;
             xOut[ncp] = nx2[jj];
             yOut[ncp] = ny2[jj];
             ncp++;
             jj++;
     }
     *nOut = ncp;
}

/*=====================================================================*/

static void p2c_SplitCntr ( int *npIn, float *xIn,  float *yIn, 
                            int *nClp, Boolean *inOut, float *xClp, float *yClp,
                            int *nSeg, int *segS, int *segE,  
                            int *iret )
/************************************************************************
 * p2c_SplitCntr                                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/CWS	06/09	Created                         	*
 ***********************************************************************/
{ 
    int         ii, jj, istr, iend;
/*---------------------------------------------------------------------*/

     *iret = 0;
     *nSeg = 0;
     istr = 0;
     iend = 0;
     for (ii = 2; ii < *nClp-2; ii++) {
         if ( !inOut[ii-2] && !inOut[ii-1] && inOut[ii] ) {
            for ( jj = 0; jj < *npIn; jj++ ) {
                if ( G_DIFF ( xClp[ii-2], xIn[jj] ) && G_DIFF ( yClp[ii-2], yIn[jj] ) ) {
                   segS[istr]=jj;
                   istr++;
                }
            }
         }
         if ( inOut[ii] && !inOut[ii+1] && !inOut[ii+2] ) {
            for ( jj = 0; jj < *npIn; jj++ ) {
                if ( G_DIFF ( xClp[ii+2], xIn[jj] ) && G_DIFF ( yClp[ii+2], yIn[jj] ) ) {
                   segE[iend]=jj;
                   iend++;
                }
            }
         }
     }
     if ( istr == iend ) {
        *nSeg = (istr + iend) / 2;
     }
}

/*=====================================================================*/

static void p2c_CopyCntr ( VG_DBStruct *el_in, VG_DBStruct *el_out, 
                           int *iret)
/************************************************************************
 * p2c_CopyCntr                                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/CWS	06/09	Created                         	*
 ***********************************************************************/
{
     int          kk;
/*---------------------------------------------------------------------*/
     *iret = 0;
     el_out->hdr.delete = el_in->hdr.delete;
     el_out->hdr.vg_type = el_in->hdr.vg_type;
     el_out->hdr.vg_class = el_in->hdr.vg_class;
     el_out->hdr.filled = el_in->hdr.filled;
     el_out->hdr.smooth = el_in->hdr.smooth;
     el_out->hdr.version = el_in->hdr.version;
     el_out->hdr.grptyp = el_in->hdr.grptyp;
     el_out->hdr.grpnum = el_in->hdr.grpnum;
     el_out->hdr.maj_col = el_in->hdr.maj_col;
     el_out->hdr.min_col = el_in->hdr.min_col;
     el_out->hdr.range_min_lat = el_in->hdr.range_min_lat;
     el_out->hdr.range_min_lon = el_in->hdr.range_min_lon;
     el_out->hdr.range_max_lat = el_in->hdr.range_max_lat;
     el_out->hdr.range_max_lon = el_in->hdr.range_max_lon;
     el_out->hdr.closed = el_in->hdr.closed;
     el_out->hdr.recsz = sizeof(VG_HdrStruct) + sizeof(SpLineInfo)
                     + el_in->elem.spl.info.numpts*2*sizeof(float);
     el_out->elem.spl.info.spltyp = el_in->elem.spl.info.spltyp;
     el_out->elem.spl.info.splstr = el_in->elem.spl.info.splstr;
     el_out->elem.spl.info.spldir = el_in->elem.spl.info.spldir;
     el_out->elem.spl.info.splsiz = el_in->elem.spl.info.splsiz;
     el_out->elem.spl.info.splwid = el_in->elem.spl.info.splwid;
     el_out->elem.spl.info.numpts = el_in->elem.spl.info.numpts;
     for ( kk = 0; kk < el_in->elem.spl.info.numpts; kk++) {
           el_out->elem.spl.latlon[ kk ]           = el_in->elem.spl.latlon[ kk ];
           el_out->elem.spl.latlon[ kk + el_in->elem.spl.info.numpts ] = el_in->elem.spl.latlon[ kk + el_in->elem.spl.info.numpts ];
     }
}

/*=====================================================================*/

static void p2c_CopyLbl ( VG_DBStruct *el_in, VG_DBStruct *el_out, 
                          int *iret)
/************************************************************************
 * p2c_CopyLbl                                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       0: normal return               *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/CWS  06/09   Created                                 *
 ***********************************************************************/
{
     int          lens;
/*---------------------------------------------------------------------*/
     *iret = 0;
     el_out->hdr.delete = el_in->hdr.delete;
     el_out->hdr.vg_type = el_in->hdr.vg_type;
     el_out->hdr.vg_class = el_in->hdr.vg_class;
     el_out->hdr.filled = el_in->hdr.filled;
     el_out->hdr.smooth = el_in->hdr.smooth;
     el_out->hdr.version = el_in->hdr.version;
     el_out->hdr.grptyp = el_in->hdr.grptyp;
     el_out->hdr.grpnum = el_in->hdr.grpnum;
     el_out->hdr.maj_col = el_in->hdr.maj_col;
     el_out->hdr.min_col = el_in->hdr.min_col;
     el_out->hdr.range_min_lat = el_in->hdr.range_min_lat;
     el_out->hdr.range_min_lon = el_in->hdr.range_min_lon;
     el_out->hdr.range_max_lat = el_in->hdr.range_max_lat;
     el_out->hdr.range_max_lon = el_in->hdr.range_max_lon;
     el_out->hdr.closed = el_in->hdr.closed;
     lens = (int)strlen(el_in->elem.spt.text);
     el_out->hdr.recsz = sizeof( VG_HdrStruct) +
                 sizeof( SpTextInfo) + (size_t)lens + 1;
     el_out->elem.spt.info.rotn      = el_out->elem.spt.info.rotn;
     el_out->elem.spt.info.sztext    = el_out->elem.spt.info.sztext;
     el_out->elem.spt.info.sptxtyp   = el_out->elem.spt.info.sptxtyp;
     el_out->elem.spt.info.turbsym   = el_out->elem.spt.info.turbsym;
     el_out->elem.spt.info.itxfn     = el_out->elem.spt.info.itxfn;
     el_out->elem.spt.info.ithw      = el_out->elem.spt.info.ithw;
     el_out->elem.spt.info.iwidth    = el_out->elem.spt.info.iwidth;
     el_out->elem.spt.info.txtcol    = el_out->elem.spt.info.txtcol;
     el_out->elem.spt.info.lincol    = el_out->elem.spt.info.lincol;
     el_out->elem.spt.info.filcol    = el_out->elem.spt.info.filcol;
     el_out->elem.spt.info.ialign    = el_out->elem.spt.info.ialign;
     el_out->elem.spt.info.lat       = el_out->elem.spt.info.lat;
     el_out->elem.spt.info.lon       = el_out->elem.spt.info.lon;
     el_out->elem.spt.info.offset_x  = el_out->elem.spt.info.offset_x;
     el_out->elem.spt.info.offset_y  = el_out->elem.spt.info.offset_y;

     strcpy ( el_out->elem.spt.text, el_in->elem.spt.text);
}

