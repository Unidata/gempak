#include <mel_bufr.h>
#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"
#include "sigbcmn.h"

int main ( int argc, char *argv[] )
/************************************************************************
 * sigbufr                                                              *
 *                                                                      *
 * This program reads and decodes BUFR high and mid level sigwx 	*
 * messages and writes the elements to an ASCII file.                   *
 *                                                                      *
 * command line:                                                        *
 *      sigbufr dattim hh filenames                                     *
 *		dattim	    GEMPAK date/time string                     *
 *		hh	    Valid time (hours) - must be 18 or 24       *
 *		filenames   High lvl + mid lvl output file names (opt)	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP   1/02                                               *
 * A. Hardy/SAIC     2/02	Add readflg to cas_open;errgrp 5 -> 8	*
 * A. Hardy/SAIC     2/02	Added chart level check       		*
 * A. Hardy/NCEP     4/03	Added check if header info was written  *
 * S. Jacobs/NCEP   10/04	Updated for new version of MELBUFR	*
 * M. Li/SAIC	    12/04	Decoded SWM data			*
 * S. Jacobs/NCEP    1/10	Initialized variables; Fixed file close	*
 * 				logic for output files; Fixed cast of	*
 * 				nret value				*
 ***********************************************************************/
{
    BUFR_Info_t BInfo;

/*    int		no_sets;  */
    int		nret, hght_count, header, time_code, descrip;
/*    int	 	idtarr[5]; This array is set but NEVER used ! */
    int 	feature_count, nflg, ihr, leverr, lever2;
    int 	ier, ier1, numerr, nerr1, nerr2;
    int		plen, tlen, maxlen, order, flen, nf;    
    int 	i, num, offset, pagflg, ipos, len;
    char 	BUFR_File[256];  
    char 	dattim[40], fhour [3], ofname[256], ofnamem[256];
    char	alias[256], path[256], templ[256], fname[256];
    char	filstr[400], pathexp[256];
    char	errgrp[8], cc[50], casgrp[4];
    char	center[10], chlvl[10];
    char        **ary_ptr=NULL, tmpstr[50];
    Boolean     readflg, didhdr, didhdrm;
    double	value, tmplvl;
    Status_t	status;

    FILE *ifpout, *ifpoutsav, *ifpoutm;

    BUFR_Val_t bv;
 
    in_bdta ( &ier );
    leverr  = 0;
    lever2  = 2;
    nerr1   = 4;
    nerr2   = 5;
    didhdr  = 0;
    didhdrm = 0;
    ifpout  = (FILE *) NULL;
    ifpoutm = (FILE *) NULL;
    ifpoutsav = (FILE *) NULL;
    tmplvl = 0.0;
    
    readflg = False;
    strcpy ( errgrp, "SIGBUFR" );
    strcpy ( casgrp, "CAS");
    strcpy ( cc, " " );

   /*
    * If GEMPAK date/time string and/or the valid hour are not on the 
    * command line, print help and exit.
    */

    if ( argc < 3 ) {
        pagflg = G_FALSE;
        ip_help ( errgrp, &pagflg, &ier, 
                  strlen(errgrp) );
        numerr = -1;
        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
        exit (1);
    }

    strcpy ( dattim, argv[1] );
    strcpy ( fhour, argv[2] );

   /*
    * Check that forecast hour is 18 or 24.
    */

    cst_numb ( fhour, &ihr, &ier ); 

    if ( ihr != 18 && ihr != 24 ) {
        numerr = -3;
        er_lmsg ( &leverr, errgrp, &numerr, fhour, &ier,
                  strlen(errgrp), strlen(fhour) );
        exit (1);
    }

   /*
    *  Create output file.
    */

    if ( argv[3] != NULL ) {
	cst_nocc ( argv[3], '+', 1, 0, &ipos, &ier );
	if ( ier == 0 ) {
	    cst_ncpy ( ofname, argv[3], ipos, &ier );
	    strcpy ( ofnamem, argv[3]+ipos+1 ); 
	    cst_rxbl ( ofname, ofname, &len, &ier );
	    if ( len <= 0 ) strcpy ( ofname, "SIGWXHI.txt" );
	    cst_rxbl ( ofnamem, ofnamem, &len, &ier );
            if ( len <= 0 ) strcpy ( ofnamem, "SIGWXMID.txt" ); 
	}
	else {
	    strcpy ( ofname, argv[3] );
	    strcpy ( ofnamem, "SIGWXMID.txt" );
	}
    }
    else {
        sprintf ( ofname, "SIGWXHI.txt" );
	strcpy ( ofnamem, "SIGWXMID.txt" );
    }

    feature_count = 0;
 
   /*
    * Get the file name template for this date/time and valid hour.
    */

    strcpy ( alias, "SIGBUF" );
    strcat ( alias, fhour );

    ctb_dtrd ( &ier );
    ctb_dtpath ( alias, path, &ier );
    ctb_dttmpl ( alias, templ, &ier );

    cfl_mnam ( dattim, templ, fname, &ier );

    plen   = (int)strlen ( path );
    tlen   = (int)strlen ( fname );
    maxlen = sizeof ( filstr );
    order  = 1;
    
    cfl_scnd ( path, &plen, fname, &tlen, ";", &maxlen, &order,
               filstr, &flen, &nf, &ier1 );
 
    if ( ier1 >= 0 )  {

        ary_ptr = (char **)malloc((size_t)nf * sizeof(char *));

        for ( i=0; i < nf; i++ )
           ary_ptr[i] = (char *)malloc((size_t)maxlen * sizeof(char));

        cst_clst ( filstr, ';', " ", nf, maxlen, ary_ptr, &num, &ier );

        css_envr ( path, pathexp, &ier );
        strcat ( pathexp, "/" );
    }
    else {
        num = 0;
        nf  = 0;
    }

    if ( num == 0 ) {
        numerr = -2;
        strcpy ( cc, dattim );
        strcat ( cc, "F" );
        strcat ( cc, fhour );
        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
    }

   /*
    * Loop on the high and mid level SIGWX BUFR files.
    */

    for ( i=0; i < num; i++ ) {

     /*
      * Initialize the BUFR message structure and store information
      * about the message in "BInfo." This function sets all of 
      * the default values for using the MELBUFR library.
      */
      if ( BUFR_Info_Init(&BInfo) ) { 
          numerr = -4;
          er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                    strlen(errgrp), strlen(cc) );
      }

     /*
      * Get BUFR_file name.
      */
  
      strcpy ( BUFR_File, pathexp );
      strcat ( BUFR_File, ary_ptr[i] );

      numerr = 1;
      er_lmsg ( &leverr, errgrp, &numerr, BUFR_File, &ier,
                strlen(errgrp), strlen(BUFR_File) );

      if ( FileExists ( BUFR_File ) ) {
/*         BInfo.VersionNumberOfMasterTables = 5; */
/*         BInfo.BUFR_Edition = 3; */
    
        if ( Find_Data_Type ( BUFR_File, &offset ) != 1 ) {
          numerr = -16;
          er_lmsg ( &leverr, errgrp, &numerr, BUFR_File, &ier,
                    strlen(errgrp), strlen(BUFR_File) );
        }

        else if ( BUFR_Init( &BInfo, BUFR_File, DECODING ) ) {
          BUFR_perror( "main" );
          numerr = -5;
          er_lmsg ( &leverr, errgrp, &numerr, BUFR_File, &ier,
                    strlen(errgrp), strlen(BUFR_File) );
        }

        else {

         /*
          * Print the Section header information and the Section 3 fxy 
          * info.  If you do not want the header info to be dumped
          * comment out the next line.
          * 
          * dump_print( 0, 0, stdout, &no_sets );
          */ 
          hght_count = 0;
          header     = 1;
          time_code  = -1;
          nflg       = 0;
          nret       = 0;
	  strcpy ( chlvl, "" );

         /* 
          * Get each value (& any assoc. fields) from the BUFR message.
          */

          while( nflg != BUFR_EOF ) {
            status = BUFR_Get_Value( &bv, 1 );
            nret = (int)status;
            if ( nret != BUFR_EOM && nret != BUFR_EOF ) {
              if ( nret == BUFR_ERROR ) {
                sigberror ("main", &ier );
                numerr = -6;
                er_lmsg ( &leverr, errgrp, &numerr, BUFR_File, &ier,
                          strlen(errgrp), strlen(BUFR_File) );
              }
              else {

               /*
                * Test for IGNORE_FXY, used to indicate that information 
                * that was returned should be ignored.  This happens when
                * a local descriptor that is not defined in Table B is
                * the last descriptor in the data set.
                */
                if ( bv.FXY_Val != (FXY_t)IGNORE_FXY ) {
                    descrip = FXY_Unpack_Dec (bv.FXY_Val);
                    if ( header ) {
                        if ( descrip == D_CENTER_ID ) {
                            numerr = 2;
                            cst_inch ( bv.Val.int_number, center, &ier );
                            er_lmsg ( &lever2, errgrp, &numerr, center,
                                     &ier, strlen(errgrp), strlen(center) );
                        }
                        else if ( descrip == D_TIME_SIGNIF ) {
                            if ( bv.Val.int_number == 16 ) {
                                time_code = 0;
                            }
                            else {
                                time_code = -1;
                            }
                            numerr = 3;
                            cst_inch ( time_code, cc, &ier );
                            er_lmsg ( &lever2, errgrp, &numerr, cc,
                                     &ier, strlen(errgrp), strlen(cc) );
                        }
                        else if ( descrip >= D_YEAR
                               && descrip <= D_MINUTE ) {
/*                            if ( time_code == 0 ) {
                                idtarr [FXY_Y_Value (bv.FXY_Val)-1] = 
                                        bv.Val.int_number;
idtarr NOT used !!! ???                            }
*/
                        }
                        else if ( descrip == D_ALTITUDE ) {
			    if  ( bv.Val_Type == DT_DOUBLE )  {
				value = bv.Val.number;
			    }
			    else {
				value = (double) bv.Val.int_number;
			    }
                            hght_count++;
			    if ( hght_count == 1 ) {
			    	tmplvl = value; 
			    }
			    else if ( hght_count == 2 ) {
				header = 0;
				if ( G_ABS ( HI_BASE - tmplvl ) < 0.5F &&
				     G_ABS ( HI_TOP  - value  ) < 0.5F ) {
				    strcpy ( chlvl, "SWH" );
				}
				else if ( G_ABS ( MID_BASE - tmplvl ) < 0.5F &&
                                          G_ABS ( MID_TOP  - value  ) < 0.5F ) {
                                    strcpy ( chlvl, "SWM" );
                                }
				else if ( G_ABS ( MID_BASE - tmplvl ) < 0.5F &&
                                          G_ABS ( HI_TOP   - value  ) < 0.5F ) {
                                    strcpy ( chlvl, "TWO" );
                                }
				else {
				    numerr = -7;
				    sprintf ( tmpstr, "%f%f", tmplvl, value );
				    er_lmsg ( &leverr, errgrp, &numerr,
                                          tmpstr, &ier, strlen(errgrp), 
                                          strlen(tmpstr) ); 
				}
			    }


			   /*
			    * Write out header files.
			    */
			    
			    if ( strcmp( chlvl, "SWH" ) == 0 || 
				 strcmp( chlvl, "TWO" ) == 0 ) {
				if ( !didhdr ) {
				    ifpout = cas_open ( ofname, readflg, &ier );
				    didhdr = 1;
				    cas_wrmhdr ( ifpout, dattim, fhour, "SWH", center, &ier );
				    ifpoutsav = ifpout;
				}
				ifpout = ifpoutsav;
			    }
			    if ( strcmp( chlvl, "SWM" ) == 0 ||
                                      strcmp( chlvl, "TWO" ) == 0 ) {
                                if ( !didhdrm ) {
                                    ifpoutm = cas_open ( ofnamem, readflg, &ier );
                                    didhdrm = 1;
                                    cas_wrmhdr ( ifpoutm, dattim,  fhour, "SWM", center, &ier );
                                }
				if ( strcmp( chlvl, "SWM" ) == 0 ) ifpout = ifpoutm;
                            }
                        }
                    }
                    else if ( strlen ( chlvl ) > (size_t)0 ) {
                        if ( descrip == D_MET_FEATURE ) {
                           /*
                            * Look for CLOUD, CAT, FRONTS, V_T_S 
                            * (Volcano), and JETS.
                            */
                            if ( bv.Val.int_number == 12 ) {
				if ( strcmp( chlvl, "SWH" ) == 0 ) {

                                   /*
                                    * This is a CLOUD message.
                                    */             
                                    strcpy ( cc, "CLOUD" );
                                    er_lmsg ( &lever2, errgrp, &nerr1, 
                                              cc, &ier, strlen(errgrp), 
                                              strlen(cc) );
                                    feature_count++;
                                    sigbcld ( ifpout, &nflg );
                                    cst_inch ( nflg, cc, &ier );
                                    er_lmsg ( &lever2, errgrp, &nerr2, cc, 
                                              &ier, strlen(errgrp), 
                                              strlen(cc) );
				}
			     	else if ( strcmp( chlvl, "SWM" ) == 0 ) {

                                   /*
                                    * This is a MCLOUD message.
                                    */         
                                    strcpy ( cc, "MCLOUD" );
                                    er_lmsg ( &lever2, errgrp, &nerr1,
                                              cc, &ier, strlen(errgrp),
                                              strlen(cc) );
                                    feature_count++;
                                    sigbmcld ( ifpoutm, &nflg );
                                    cst_inch ( nflg, cc, &ier );
                                    er_lmsg ( &lever2, errgrp, &nerr2, cc,
                                              &ier, strlen(errgrp),
                                              strlen(cc) );
                                }
                            }      
                            else if ( bv.Val.int_number == 13 ) {
                               /*
                                * This is a CAT (Clear Air Turbulence)
                                *  message.
                                */             
				if ( strcmp(chlvl, "SWM") == 0 ) {
                                    strcpy ( cc, "MCAT" );
				}
				else {
				    strcpy ( cc, "CAT" );
				}
                                er_lmsg ( &lever2, errgrp, &nerr1, 
                                          cc, &ier, strlen(errgrp), 
                                          strlen(cc) );
                                feature_count++;
                                sigbturb ( ifpout, chlvl, &nflg );
                                cst_inch ( nflg, cc, &ier );
                                er_lmsg ( &lever2, errgrp, &nerr2, cc, 
                                          &ier, strlen(errgrp), 
                                          strlen(cc) );
                            }      
                            else if ( bv.Val.int_number== 0
                                   || bv.Val.int_number== 2
                                   || bv.Val.int_number== 4 
                                   || bv.Val.int_number== 6
                                   || bv.Val.int_number== 8 
                                   || bv.Val.int_number== 9 ) {
                               /*
                                * This is a FRONTS message.
                                */             
				if ( strcmp(chlvl, "SWM") == 0 ) {
                                    strcpy ( cc, "MFRONTS" );
				}
				else {
				    strcpy ( cc, "FRONTS" );
				}
                                er_lmsg ( &lever2, errgrp, &nerr1, 
                                          cc, &ier, strlen(errgrp), 
                                          strlen(cc) );
                                feature_count++;
                                sigbfronts ( ifpout, bv.Val.int_number,
                                             chlvl, &nflg );
                                cst_inch ( nflg, cc, &ier );
                                er_lmsg ( &lever2, errgrp, &nerr2, cc, 
                                          &ier, strlen(errgrp), 
                                          strlen(cc) );
                            }      
                            else if ( bv.Val.int_number==17 ) {
                               /*
                                * This is a V_T_S (Volcano type) message.
                                */             
                                strcpy ( cc, "Volcano" );
                                er_lmsg ( &lever2, errgrp, &nerr1, 
                                          cc, &ier, strlen(errgrp), 
                                          strlen(cc) );
                                feature_count++;
                                sigbvolcano ( ifpout, ifpoutm, &nflg );
                                cst_inch ( nflg, cc, &ier );
                                er_lmsg ( &lever2, errgrp, &nerr2, cc, 
                                          &ier, strlen(errgrp), 
                                          strlen(cc) );
                            }      
                            else if ( bv.Val.int_number == 10 ) {
                               /*
                                * This is a JETS message.
                                */             
				if ( strcmp(chlvl, "SWM") == 0 ) {
                                    strcpy ( cc, "MJET" );
                                }
                                else {
                                    strcpy ( cc, "JET" );
                                }
                                er_lmsg ( &lever2, errgrp, &nerr1, 
                                          cc, &ier, strlen(errgrp), 
                                          strlen(cc) );
                                feature_count++;
                                sigbjets ( ifpout, chlvl, &nflg );
                                cst_inch ( nflg, cc, &ier );
                                er_lmsg ( &lever2, errgrp, &nerr2, cc, 
                                          &ier, strlen(errgrp), 
                                          strlen(cc) );
                            }      
                        }
                        else if ( descrip == D_VERT_SIGNIF ) {
                           /*
                            * Look for TROP.
                            */
                            if  ( ( bv.missing_flag != 0 ) && 
                                  ( bv.Val.int_number & ( 1 << 4 ) ) ) {   
                               /*
                                * This is a TROP message - bit 3 is on.
                                */                   
				if ( strcmp(chlvl, "SWM") == 0 ) {
                                    strcpy ( cc, "MTROP" );
                                }
                                else {
                                    strcpy ( cc, "TROP" );
                                }
                                er_lmsg ( &lever2, errgrp, &nerr1, 
                                          cc, &ier, strlen(errgrp), 
                                          strlen(cc) );
                                feature_count++;
                                sigbtrop ( ifpout, chlvl, &nflg );  
                                cst_inch ( nflg, cc, &ier );
                                er_lmsg ( &lever2, errgrp, &nerr2, cc, 
                                          &ier, strlen(errgrp), 
                                          strlen(cc) );
                            }
                        }
                        else if ( descrip == D_MET_ATTRIB ) {
                           /*
                            * Look for V_T_S (Storm).
                            */
                            if ( bv.Val.int_number == 1 ) {   
                               /*
                                * This is a V_T_S (Storm type) message.
                                */                   
                                strcpy ( cc, "Storm" );
                                er_lmsg ( &lever2, errgrp, &nerr1, 
                                          cc, &ier, strlen(errgrp), 
                                          strlen(cc) );
                                feature_count++;
                                sigbstorm ( ifpout, ifpoutm, &nflg );  
                                cst_inch ( nflg, cc, &ier );
                                er_lmsg ( &lever2, errgrp, &nerr2, cc, 
                                          &ier, strlen(errgrp), 
                                          strlen(cc) );
                            }
                        }
                        else if ( descrip == D_FACILITY ) {
                           /*
                            * Look for V_T_S (Radiation Leak).
                            */
                            if ( bv.missing_flag != 0 ) {   
                               /*
                                * This is a V_T_S (Radiation Leak type)
                                *  message.
                                */                   
                                strcpy ( cc, "Radiation Leak" );
                                er_lmsg ( &lever2, errgrp, &nerr1, 
                                          cc, &ier, strlen(errgrp), 
                                          strlen(cc) );
                                feature_count++;
                                sigbradtn ( ifpout, ifpoutm, &nflg );  
                                cst_inch ( nflg, cc, &ier );
                                er_lmsg ( &lever2, errgrp, &nerr2, cc, 
                                          &ier, strlen(errgrp), 
                                          strlen(cc) );
                            }
                        }
                    }

                   /*
                    * For debug purposes, uncomment the following code
                    * to print the BUFR descriptor and its value.
                    * 
                    * if ( nflg == BUFR_OK ) {
                    *     BUFR_Val_Print ( bv, 1, 0, stdout );   
                    * }
                    */
                }
                if ( nret == BUFR_EOD ) {  
                   /*
                    * This decoded value is the last one in the dataset.
                    */

                    numerr = -10;
                    er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                              strlen(errgrp), strlen(cc) );
                }
              }
            }
            else {
              nflg = BUFR_EOF;
              sigbeof ( nret, &ier );
              BUFR_Destroy(1);
            }
          }
          if ( nret == BUFR_EOM ) {
            sigbeom ( &nret, &ier );
          }
        }
      } 
      else {
        numerr = -11;
        er_lmsg ( &leverr, errgrp, &numerr, BUFR_File, &ier,
                  strlen(errgrp), strlen(BUFR_File) );
      }
    }

    if ( ier1 >= 0 ) {
        for ( i=0; i < nf; i++ ) {
            free( ary_ptr[i] );
        }

        free( (char **)ary_ptr );

    }

    numerr = 6;
    cst_inch ( feature_count, cc, &ier );
    er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
              strlen(errgrp), strlen(cc) );

    if ( ifpout == ifpoutm ) {
	if ( ifpout != NULL ) {
	    cas_clos ( ifpout, &ier );
	}
    }
    else {
	if ( ifpout != NULL ) {
	    cas_clos ( ifpout, &ier );
	}
	if ( ifpoutm != NULL ) {
	    cas_clos ( ifpoutm, &ier1 );
	}
    }
    
   /*
    * If ASCII file failed to close, write error message.
    */

    if ( ier != 0 ) {
         numerr = -2;
         er_lmsg ( &leverr, casgrp, &numerr, ofname, &ier,
                      strlen(casgrp), strlen(ofname) );
         exit (1);
    }
    if ( ier1 != 0 ) {
         numerr = -2;
         er_lmsg ( &leverr, casgrp, &numerr, ofnamem, &ier,
                      strlen(casgrp), strlen(ofnamem) );
         exit (1);
    }

    
    return(0);
}
